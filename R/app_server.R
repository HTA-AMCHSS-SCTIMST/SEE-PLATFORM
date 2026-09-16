app_server <- function(input, output, session) {
  ping <- mongo_ping()
  if (isTRUE(ping$ok)) {
    ensure_indexes()
    ee_log("info", ping$message, where = "mongo")
  } else {
    ee_log("error", ping$message, where = "mongo")
  }
  ee_log(
    "info",
    sprintf("session started token=%s app_role=%s", session$token %||% "?", ee_app_role()),
    where = "session"
  )
  rv <- shiny::reactiveValues(
    user = NULL,
    page = "login",
    study_id = NULL,
    survey_study = NULL,
    survey_user = NULL,
    survey_page = 1L,
    msg = "",
    err = "",
    seed_info = NULL,
    shelf_result = NULL,
    review_result = NULL,
    review_refresh = 0L,
    review_updated_at = NULL,
    bound_lo = NULL,
    bound_hi = NULL,
    token_tried = FALSE,
    last_activity = Sys.time(),
    oidc_tried = FALSE
  )
  session_timeout <- ee_session_timeout_minutes() * 60
  touch_activity <- function() rv$last_activity <- Sys.time()

  session$onSessionEnded(function() {
    ee_log(
      "info",
      sprintf("session ended user=%s", rv$user$email %||% rv$survey_user$email %||% "anonymous"),
      where = "audit.session"
    )
  })

  shiny::observeEvent(input$ee_activity, {
    touch_activity()
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::invalidateLater(60000, session)
    if (difftime(Sys.time(), rv$last_activity, units = "secs") > session_timeout) {
      if (!is.null(rv$user) || !is.null(rv$survey_user)) {
        ee_log(
          "info",
          sprintf("session timeout user=%s after=%s minutes",
                  rv$user$email %||% rv$survey_user$email %||% "anonymous",
                  ee_session_timeout_minutes()),
          where = "audit.timeout"
        )
      }
      rv$user <- NULL
      rv$survey_user <- NULL
      rv$survey_study <- NULL
      rv$page <- "login"
      rv$err <- "Your session expired due to inactivity. Please sign in again."
      rv$msg <- ""
      rv$last_activity <- Sys.time()
    }
  })

  query_params <- shiny::reactive({
    qs <- session$clientData$url_search
    if (is.null(qs) || !nzchar(qs)) return(list())
    shiny::parseQueryString(qs)
  })

  query_study <- shiny::reactive({
    q <- query_params()
    q$study %||% q$id %||% ""
  })

  query_token <- shiny::reactive({
    q <- query_params()
    q$t %||% ""
  })

  is_survey_mode <- shiny::reactive({
    role <- ee_app_role()
    if (identical(role, "survey")) return(TRUE)
    if (identical(role, "staff")) return(FALSE)
    nzchar(query_study()) || nzchar(query_token())
  })

  survey_pid <- function() {
    u <- rv$survey_user
    u$personId %||% u$id
  }

  shiny::observe({
    if (!ee_auth_dev_mode()) {
      cu <- ee_connect_user(session)
      if (!is.null(cu) && is.null(rv$user) && !is_survey_mode()) {
      tryCatch({
        u <- connect_login(cu)
        rv$user <- user_as_list(u)
        rv$page <- "dash"
        touch_activity()
        ee_log("info", sprintf("Posit Connect login %s as %s", u$email, u$platformRole),
               where = "audit.login")
      }, error = function(e) {
        ee_log_error(e, where = "audit.login")
        rv$err <- conditionMessage(e)
      })
    }
    }
  })

  shiny::observe({
    if (isTRUE(rv$oidc_tried) || !ee_oidc_enabled() || is_survey_mode()) return()
    q <- query_params()
    if (!nzchar(q$code %||% "") && !nzchar(q$error %||% "")) return()
    rv$oidc_tried <- TRUE
    if (nzchar(q$error %||% "")) {
      rv$err <- paste("Sign-in was not completed:", q$error_description %||% q$error)
      return()
    }
    if (!ee_oidc_verify_state(q$state %||% "")) {
      rv$err <- "The sign-in session expired or was invalid. Please try again."
      return()
    }
    tryCatch({
      rv$user <- user_as_list(oidc_login(q$code))
      rv$page <- "dash"
      touch_activity()
      ee_log("info", sprintf("OIDC login %s", rv$user$email), where = "audit.login")
    }, error = function(e) ee_handle(rv, e, "oidc_login"))
  })

  shiny::observeEvent(input$oidc_login, {
    shiny::req(ee_oidc_enabled())
    tryCatch({
      session$sendCustomMessage("ee_oidc_redirect", ee_oidc_authorize_url(ee_oidc_state()))
    }, error = function(e) ee_handle(rv, e, "oidc_redirect"))
  })

  enter_survey <- function(res) {
    u <- user_as_list(res$user)
    if (is.null(u$personId) || !nzchar(as.character(u$personId))) {
      p <- find_person_by_email(u$email)
      u$personId <- doc_id(p)
    }
    rv$survey_user <- u
    rv$survey_study <- res$study
    rv$survey_page <- 1L
    q <- primary_question(study_questions(doc_id(res$study)))
    if (!is.null(q)) {
      rv$bound_lo <- as.numeric(q$lowerBound %||% 0)
      rv$bound_hi <- as.numeric(q$upperBound %||% 1)
    }
    rv$review_result <- if (current_round(res$study) >= 2L) {
      q <- primary_question(study_questions(doc_id(res$study)))
      if (is.null(q)) {
        NULL
      } else {
        prev <- max(1L, current_round(res$study) - 1L)
        aggregation_as_shelf_result(latest_aggregation(doc_id(res$study), doc_id(q), prev)) %||%
          tryCatch(run_shelf_for_question(res$study, q, prev, anonymize = TRUE), error = function(e) NULL)
      }
    } else {
      NULL
    }
    rv$review_updated_at <- if (!is.null(rv$review_result)) Sys.time() else NULL
    rv$review_refresh <- rv$review_refresh + 1L
  }

  shiny::observe({
    if (isTRUE(rv$token_tried) || !is.null(rv$survey_user) || !is_survey_mode()) return()
    tok <- query_token()
    if (!nzchar(tok)) return()
    rv$token_tried <- TRUE
    tryCatch(enter_survey(survey_entry_token(tok)), error = function(e) ee_handle(rv, e, "survey_token"))
  })

  shiny::observe({
    shiny::invalidateLater(15000, session)
    if (is.null(rv$survey_user) || !is_survey_mode() ||
        is.null(rv$survey_study) || current_round(rv$survey_study) < 2L) {
      return()
    }
    st <- find_study(doc_id(rv$survey_study))
    q <- if (is.null(st)) NULL else primary_question(study_questions(doc_id(st)))
    if (is.null(q)) return()
    latest <- aggregation_as_shelf_result(
      latest_aggregation(doc_id(st), doc_id(q), current_round(st) - 1L)
    )
    if (is.null(latest)) return()
    old_id <- rv$review_result$aggregationId %||% ""
    new_id <- latest$aggregationId %||% ""
    if (!identical(as.character(old_id), as.character(new_id))) {
      rv$review_result <- latest
      rv$review_updated_at <- Sys.time()
    }
    rv$review_refresh <- rv$review_refresh + 1L
  })

  shiny::observeEvent(input$login_role, {
    role <- input$login_role
    shiny::updateTextInput(session, "login_email", value = paste0(gsub("_", "", role), "@sctimst.ac.in"))
    shiny::updateTextInput(session, "login_name", value = role_label(role))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$login_go, {
    rv$err <- ""
    tryCatch({
      u <- dev_login(input$login_email, input$login_name, input$login_role)
      rv$user <- user_as_list(u)
      rv$page <- "dash"
      touch_activity()
      ee_log("info", sprintf("dev login %s as %s", u$email, u$platformRole), where = "audit.login")
    }, error = function(e) ee_handle(rv, e, "login"))
  })

  shiny::observeEvent(input$logout, {
    ee_log("info", sprintf("staff logout user=%s", rv$user$email %||% "anonymous"), where = "audit.logout")
    rv$user <- NULL
    rv$page <- "login"
    rv$study_id <- NULL
    rv$msg <- ""
  })

  shiny::observeEvent(input$goto_dash, {
    rv$page <- "dash"
    rv$study_id <- NULL
    rv$shelf_result <- NULL
  })

  shiny::observeEvent(input$goto_people, {
    tryCatch({
      require_role(can_provision_people(rv$user))
      rv$page <- "people"
    }, error = function(e) ee_handle(rv, e, "goto_people"))
  })

  shiny::observeEvent(input$person_add, {
    shiny::req(rv$user)
    rv$err <- ""
    tryCatch({
      p <- add_person(input$person_name, input$person_email, input$person_type, input$person_affil %||% "", rv$user)
      rv$msg <- sprintf("Saved %s as %s.", p$email, p$personType)
    }, error = function(e) ee_handle(rv, e, "person_add"))
  })

  shiny::observeEvent(input$seed_demo, {
    shiny::req(rv$user)
    rv$err <- ""
    tryCatch({
      require_role(can_seed_demo(rv$user), "Only facilitators can seed the demo study.")
      rv$seed_info <- seed_demo(rv$user)
      rv$msg <- sprintf(
        "Demo ready with dummy SHELF judgments (%s). Survey: %s  ·  Experts: %s",
        rv$seed_info$dummyJudgments %||% 0L,
        rv$seed_info$surveyUrl,
        paste(rv$seed_info$expertEmails, collapse = ", ")
      )
      ee_log("info", "seeded HTA demo", where = "seed_demo")
    }, error = function(e) ee_handle(rv, e, "seed_demo"))
  })

  shiny::observeEvent(input$create_study, {
    shiny::req(rv$user, nzchar(trimws(input$new_title %||% "")))
    rv$err <- ""
    methods <- input$new_methods
    if (is.null(methods) || !length(methods)) methods <- c("chips_and_bins", "quantile")
    tryCatch({
      require_role(can_create_study(rv$user), "Only facilitators can create case studies.")
      variable_type <- input$new_variable_type %||% "proportion"
      lower <- as.numeric(input$new_lower)
      upper <- as.numeric(input$new_upper)
      precision <- as.integer(input$new_precision)
      if (!is.finite(lower) || !is.finite(upper) || !(lower < upper)) {
        stop("Lower plausible bound must be less than the upper plausible bound.")
      }
      if (!is.finite(precision) || precision < 0 || precision > 6) {
        stop("Decimal places must be between 0 and 6.")
      }
      if (!nzchar(trimws(input$new_unit %||% ""))) stop("Enter a unit for the quantity.")
      if (!identical(variable_type, "proportion")) {
        methods <- setdiff(methods, "chips_and_bins")
        if (!length(methods)) methods <- "quantile"
      }
      st <- create_study(
        rv$user,
        title = trimws(input$new_title),
        description = input$new_desc %||% "",
        quantity = input$new_qty %||% "",
        methods = methods,
        variable_type = variable_type,
        unit = trimws(input$new_unit),
        lower = lower,
        upper = upper,
        precision = precision,
        preferred_distribution = input$new_distribution %||% "best"
      )
      rv$study_id <- doc_id(st)
      rv$page <- "study"
      rv$msg <- "Survey created."
    }, error = function(e) ee_handle(rv, e, "create_study"))
  })

  shiny::observeEvent(input$open_study, {
    rv$study_id <- input$open_study
    rv$page <- "study"
    rv$msg <- ""
    rv$shelf_result <- NULL
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$goto_responses, {
    st <- find_study(rv$study_id)
    if (!can_view_shelf(rv$user, st)) {
      rv$err <- "You cannot open SHELF fitting while this study is in active deliberation."
      return()
    }
    rv$page <- "responses"
    rv$err <- ""
    rv$shelf_result <- NULL
  })

  shiny::observeEvent(input$invite_go, {
    shiny::req(rv$study_id, nzchar(trimws(input$invite_email %||% "")))
    st <- find_study(rv$study_id)
    tryCatch({
      require_study_manager(rv$user, st, "invite experts to this study")
      p <- invite_expert(st, input$invite_email, input$invite_name, rv$user)
      tok <- issue_invite_token(find_study(rv$study_id), p)
      rv$msg <- sprintf(
        "Added %s (%s). Personal link: %s",
        p$name %||% "", p$email, expert_survey_url(find_study(rv$study_id), p, tok)
      )
      shiny::updateTextInput(session, "invite_email", value = "")
      shiny::updateTextInput(session, "invite_name", value = "")
    }, error = function(e) ee_handle(rv, e, "invite_expert"))
  })

  shiny::observeEvent(input$remove_expert, {
    shiny::req(rv$study_id, nzchar(input$remove_expert %||% ""))
    tryCatch({
      st <- find_study(rv$study_id)
      remove_study_expert(st, input$remove_expert, rv$user)
      rv$msg <- "Expert removed from this survey."
      ee_log("info", sprintf("expert %s removed from study %s", input$remove_expert, rv$study_id),
             where = "audit.study_access")
    }, error = function(e) ee_handle(rv, e, "remove_expert"))
  })

  shiny::observeEvent(input$save_study_details, {
    shiny::req(rv$study_id)
    tryCatch({
      st <- update_study_details(find_study(rv$study_id), input$edit_title,
                                 input$edit_description, rv$user)
      rv$msg <- sprintf("Survey '%s' updated.", st$title)
      ee_log("info", sprintf("study %s updated", rv$study_id), where = "audit.study_edit")
    }, error = function(e) ee_handle(rv, e, "save_study_details"))
  })

  shiny::observeEvent(input$archive_study, {
    shiny::req(rv$study_id)
    tryCatch({
      archive_study(find_study(rv$study_id), rv$user)
      rv$msg <- "Survey archived."
      rv$page <- "dash"
      ee_log("info", sprintf("study %s archived", rv$study_id), where = "audit.study_archive")
    }, error = function(e) ee_handle(rv, e, "archive_study"))
  })

  shiny::observeEvent(input$advance_round, {
    tryCatch({
      st <- find_study(rv$study_id)
      require_study_manager(rv$user, st, "advance this study")
      st <- advance_round(st)
      rv$msg <- sprintf("Now round %s. Experts who reopen the link will see blinded round-1 distributions.", current_round(st))
      ee_log("info", rv$msg, where = "advance_round")
    }, error = function(e) ee_handle(rv, e, "advance_round"))
  })

  shiny::observeEvent(input$complete_study, {
    tryCatch({
      st <- complete_study(find_study(rv$study_id), rv$user)
      rv$msg <- "Study marked complete. Researchers and students can view the consensus."
    }, error = function(e) ee_handle(rv, e, "complete_study"))
  })

  chips_mod <- mod_chips_server("chips", bounds = shiny::reactive({
    list(lo = rv$bound_lo, hi = rv$bound_hi)
  }))

  shiny::observeEvent(input$survey_enter, {
    rv$err <- ""
    tryCatch(enter_survey(survey_entry(query_study(), input$survey_email)), error = function(e) ee_handle(rv, e, "survey_entry"))
  })

  shiny::observeEvent(input$survey_next, {
    rv$survey_page <- rv$survey_page + 1L
  })
  shiny::observeEvent(input$survey_prev, {
    rv$survey_page <- max(1L, rv$survey_page - 1L)
  })

  shiny::observeEvent(input$save_onboarding, {
    tryCatch({
      save_onboarding(
        rv$survey_study, survey_pid(),
        input$coi_financial, input$coi_academic,
        isTRUE(input$tou_accept),
        input$attribution_pref %||% "anonymous"
      )
      rv$err <- ""
      rv$survey_page <- rv$survey_page + 1L
    }, error = function(e) ee_handle(rv, e, "save_onboarding"))
  })

  shiny::observeEvent(input$save_calibration, {
    tryCatch({
      save_calibration(rv$survey_study, survey_pid(), input$practice_p)
      rv$err <- ""
      rv$survey_page <- rv$survey_page + 1L
    }, error = function(e) ee_handle(rv, e, "save_calibration"))
  })

  shiny::observeEvent(input$save_bounds, {
    tryCatch({
      require_onboarding(rv$survey_study, survey_pid())
      b <- save_bounds(rv$survey_study, survey_pid(), input$bound_lo, input$bound_hi, current_round(rv$survey_study))
      rv$bound_lo <- as.numeric(b$lower)
      rv$bound_hi <- as.numeric(b$upper)
      rv$err <- ""
      rv$survey_page <- rv$survey_page + 1L
    }, error = function(e) ee_handle(rv, e, "save_bounds"))
  })

  shiny::observeEvent(input$save_comment, {
    tryCatch({
      q <- primary_question(study_questions(doc_id(rv$survey_study)))
      shiny::req(q)
      save_peer_comment(rv$survey_study, q, survey_pid(), input$peer_comment, current_round(rv$survey_study))
      rv$msg <- "Comment posted."
      rv$err <- ""
      shiny::updateTextAreaInput(session, "peer_comment", value = "")
    }, error = function(e) ee_handle(rv, e, "save_comment"))
  })

  shiny::observeEvent(input$save_chips, {
    shiny::req(rv$survey_study, rv$survey_user)
    qs <- study_questions(doc_id(rv$survey_study))
    pages <- survey_pages(rv$survey_study, qs)
    pg <- pages[[rv$survey_page]]
    shiny::req(identical(pg$kind, "chips"), !is.null(pg$question))
    val <- chips_mod$value()
    if (val$placed != val$totalChips) {
      rv$err <- sprintf("Allocate exactly %s chips (currently %s).", val$totalChips, val$placed)
      ee_log("warn", rv$err, where = "save_chips")
      return()
    }
    tryCatch({
      require_onboarding(rv$survey_study, survey_pid())
      lo <- rv$bound_lo %||% val$lowerBound
      hi <- rv$bound_hi %||% val$upperBound
      if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) stop("Set plausible bounds L < U first.")
      require_rationale_text(input$chips_rationale, isTRUE(pg$question$rationaleRequired %||% TRUE))
      payload <- chips_payload(val$bins, val$totalChips, lo, hi, input$chips_rationale %||% "")
      save_judgment(
        rv$survey_study, pg$question,
        survey_pid(),
        rv$survey_user$displayName,
        payload,
        current_round(rv$survey_study)
      )
      rv$err <- ""
      rv$msg <- "Chips saved."
      rv$survey_page <- rv$survey_page + 1L
      ee_log("info", "chips judgment saved", where = "save_chips")
    }, error = function(e) ee_handle(rv, e, "save_chips"))
  })

  shiny::observeEvent(input$save_quantile, {
    shiny::req(rv$survey_study, rv$survey_user)
    qs <- study_questions(doc_id(rv$survey_study))
    pages <- survey_pages(rv$survey_study, qs)
    pg <- pages[[rv$survey_page]]
    shiny::req(identical(pg$kind, "quantile"), !is.null(pg$question))
    a <- as.numeric(input$p10)
    b <- as.numeric(input$p50)
    c <- as.numeric(input$p90)
    lo <- rv$bound_lo %||% as.numeric(pg$question$lowerBound %||% 0)
    hi <- rv$bound_hi %||% as.numeric(pg$question$upperBound %||% 1)
    tryCatch({
      require_onboarding(rv$survey_study, survey_pid())
      require_rationale_text(input$q_rationale, isTRUE(pg$question$rationaleRequired %||% TRUE))
      mode <- input$q_mode %||% "percentile"
      if (identical(mode, "quartile")) {
        vals <- list(`0.25` = a, `0.5` = b, `0.75` = c)
        validate_strict_quantiles(vals, lo, hi)
        payload <- quartile_payload(a, b, c, input$q_rationale %||% "", lo, hi)
      } else {
        vals <- list(`0.1` = a, `0.5` = b, `0.9` = c)
        validate_strict_quantiles(vals, lo, hi)
        payload <- quantile_payload(a, b, c, input$q_rationale %||% "", lo, hi)
      }
      save_judgment(
        rv$survey_study, pg$question,
        survey_pid(),
        rv$survey_user$displayName,
        payload,
        current_round(rv$survey_study)
      )
      rv$err <- ""
      rv$msg <- "Percentiles saved."
      rv$survey_page <- rv$survey_page + 1L
      ee_log("info", "quantile judgment saved", where = "save_quantile")
    }, error = function(e) ee_handle(rv, e, "save_quantile"))
  })

  shiny::observeEvent(input$run_shelf, {
    shiny::req(rv$study_id, input$resp_question)
    rv$err <- ""
    st <- find_study(rv$study_id)
    qs <- study_questions(doc_id(st))
    q <- Filter(function(x) identical(doc_id(x), input$resp_question), qs)[[1]]
    tryCatch({
      require_role(can_run_shelf(rv$user, st), "Only facilitators can run SHELF.")
      rv$shelf_result <- run_shelf_for_question(
        st, q,
        round_number = current_round(st),
        family = input$fit_family %||% "best"
      )
      rv$msg <- sprintf("SHELF run with %s (%s experts).", rv$shelf_result$fit$engine, rv$shelf_result$nExperts)
    }, error = function(e) ee_handle(rv, e, "run_shelf"))
  })

  shiny::observe({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    qid <- input$resp_question
    if (is.null(qid) || !nzchar(qid)) return()
    if (!is.null(rv$shelf_result) && identical(as.character(rv$shelf_result$questionId), as.character(qid))) return()
    st <- find_study(rv$study_id)
    new_res <- aggregation_as_shelf_result(latest_aggregation(doc_id(st), qid, current_round(st)))
    if (is.null(new_res) && is.null(rv$shelf_result)) return()
    rv$shelf_result <- new_res
  })

  output$login_err <- shiny::renderUI(notice(rv$err, "error"))

  output$root <- shiny::renderUI({
    if (!isTRUE(ping$ok) && !is_survey_mode() && identical(rv$page, "login")) {
      return(shiny::div(
        class = "login-page",
        shiny::div(
          class = "login-card",
          htmltools::tags$h1("MongoDB not connected"),
          htmltools::tags$p(ping$message),
          htmltools::tags$p("Start mongod or set MONGODB_URI in .Renviron. See README.md."),
          htmltools::tags$p(
            class = "team",
            htmltools::tags$a(
              href = "https://hta-amchss-sctimst.github.io/RRC/",
              target = "_blank",
              rel = "noopener noreferrer",
              aria_label = "Achutha Menon Centre for Health Science Studies website",
              "Achutha Menon Centre for Health Science Studies (AMCHSS)"
            )
          )
        )
      ))
    }
    if (is_survey_mode()) return(survey_root_ui(input, rv, query_study(), ping, query_token()))
    if (is.null(rv$user) && ee_auth_dev_mode()) return(login_ui())
    if (is.null(rv$user)) {
      return(shiny::div(class = "login-page", shiny::div(class = "login-card",
        htmltools::tags$h1("Sign in required"),
        htmltools::tags$p(if (ee_oidc_enabled()) "Sign in with your organization account to continue." else
          "This staff app expects Posit Connect login (session user)."),
        htmltools::tags$p(
          class = "team",
          htmltools::tags$a(
            href = "https://hta-amchss-sctimst.github.io/RRC/",
            target = "_blank",
            rel = "noopener noreferrer",
            aria_label = "Achutha Menon Centre for Health Science Studies website",
            "Achutha Menon Centre for Health Science Studies (AMCHSS)"
          )
        )
      )))
    }
    switch(
      rv$page,
      dash = staff_dash_ui(rv),
      study = staff_study_ui(rv),
      responses = staff_responses_ui(rv),
      people = staff_people_ui(rv),
      staff_dash_ui(rv)
    )
  })

  output$shelf_plot <- plotly::renderPlotly({
    shiny::req(rv$shelf_result)
    df <- shelf_plot_df(rv$shelf_result$fit, input$pool_view %||% "both")
    shiny::req(df)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = "Value", y = "Density", colour = NULL, linetype = NULL) +
      ggplot2::theme(legend.position = "bottom")
    plotly::ggplotly(p, tooltip = c("colour", "x", "y"))
  })

  output$review_plot <- plotly::renderPlotly({
    shiny::req(rv$review_result)
    df <- shelf_plot_df(rv$review_result$fit, "both")
    shiny::req(df)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = "Value", y = "Density", colour = NULL, linetype = NULL)
    plotly::ggplotly(p, tooltip = c("colour", "x", "y"))
  })

  output$shelf_params <- shiny::renderTable({
    shiny::req(rv$shelf_result$params)
    params <- rv$shelf_result$params
    pool_view <- input$pool_view %||% "both"
    if (pool_view %in% c("median", "linear")) {
      pool_role <- if (identical(pool_view, "median")) "median_pool" else "linear_pool"
      roles <- tolower(trimws(as.character(params$role %||% "")))
      params <- params[roles == "expert" | roles == pool_role, , drop = FALSE]
    }
    numeric_cols <- c("q_low", "q_mid", "q_high")
    params[numeric_cols] <- lapply(params[numeric_cols], function(x) {
      formatC(as.numeric(x), format = "f", digits = 2)
    })
    names(params) <- c(
      "Expert / result", "Role", "Lower quantile", "Median quantile",
      "Upper quantile", "Distribution", "Best fit from SHELF"
    )
    params
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

  output$dl_csv <- shiny::downloadHandler(
    filename = function() sprintf("shelf-params-%s.csv", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      write_audit_csv(file, rv$shelf_result, st)
    }
  )

  output$dl_pdf <- shiny::downloadHandler(
    filename = function() sprintf("complete-study-audit-%s.pdf", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      qs <- study_questions(doc_id(st))
      q <- Filter(function(x) identical(doc_id(x), input$resp_question), qs)
      q <- if (length(q)) q[[1]] else qs[[1]]
      comments <- list_peer_comments(doc_id(st), doc_id(q), current_round(st))
      write_audit_pdf(file, rv$shelf_result, st, q, current_round(st), comments, input$pool_view %||% "both")
    }
  )

  output$dl_bundle <- shiny::downloadHandler(
    filename = function() sprintf("study-audit-bundle-%s.zip", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      qs <- study_questions(doc_id(st))
      q <- Filter(function(x) identical(doc_id(x), input$resp_question), qs)
      q <- if (length(q)) q[[1]] else qs[[1]]
      comments <- list_peer_comments(doc_id(st), doc_id(q), current_round(st))
      write_audit_bundle(
        file, rv$shelf_result, st, q, current_round(st),
        comments, input$pool_view %||% "both"
      )
    }
  )

  output$dl_quarto <- shiny::downloadHandler(
    filename = function() sprintf("shelf-dossier-%s.html", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      shiny::req(rv$shelf_result)
      qs <- study_questions(doc_id(st))
      q <- Filter(function(x) identical(doc_id(x), input$resp_question), qs)
      q <- if (length(q)) q[[1]] else qs[[1]]
      render_quarto_dossier(file, st, q, current_round(st))
    }
  )
}
