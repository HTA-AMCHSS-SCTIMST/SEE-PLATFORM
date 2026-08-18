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
  session$onSessionEnded(function() {
    ee_log("info", "session ended", where = "session")
  })

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
    shelf_result = NULL
  )

  query_study <- shiny::reactive({
    qs <- session$clientData$url_search
    if (is.null(qs) || !nzchar(qs)) return("")
    q <- shiny::parseQueryString(qs)
    q$study %||% q$id %||% ""
  })

  is_survey_mode <- shiny::reactive({
    role <- ee_app_role()
    if (identical(role, "survey")) return(TRUE)
    if (identical(role, "staff")) return(FALSE)
    nzchar(query_study())
  })

  shiny::observe({
    if (!ee_auth_dev_mode()) {
      cu <- ee_connect_user(session)
      if (!is.null(cu) && is.null(rv$user) && !is_survey_mode()) {
        rv$user <- user_as_list(connect_login(cu))
        rv$page <- "dash"
      }
    }
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
      ee_log("info", sprintf("dev login %s as %s", u$email, u$platformRole), where = "login")
    }, error = function(e) ee_handle(rv, e, "login"))
  })

  shiny::observeEvent(input$logout, {
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

  shiny::observeEvent(input$goto_people, { rv$page <- "people" })

  shiny::observeEvent(input$seed_demo, {
    shiny::req(rv$user)
    rv$err <- ""
    tryCatch({
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
      st <- create_study(
        rv$user,
        title = trimws(input$new_title),
        description = input$new_desc %||% "",
        quantity = input$new_qty %||% "",
        methods = methods
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
    rv$page <- "responses"
    rv$shelf_result <- NULL
  })

  shiny::observeEvent(input$invite_go, {
    shiny::req(rv$study_id, nzchar(trimws(input$invite_email %||% "")))
    st <- find_study(rv$study_id)
    tryCatch({
      p <- invite_expert(st, input$invite_email, input$invite_name, rv$user)
      rv$msg <- sprintf("Added %s (%s). Copy the survey URL below.", p$name %||% "", p$email)
      shiny::updateTextInput(session, "invite_email", value = "")
      shiny::updateTextInput(session, "invite_name", value = "")
    }, error = function(e) ee_handle(rv, e, "invite_expert"))
  })

  shiny::observeEvent(input$advance_round, {
    tryCatch({
      st <- find_study(rv$study_id)
      st <- advance_round(st)
      rv$msg <- sprintf("Now round %s. Experts who reopen the link will see previous-round values.", current_round(st))
      ee_log("info", rv$msg, where = "advance_round")
    }, error = function(e) ee_handle(rv, e, "advance_round"))
  })

  chips_mod <- mod_chips_server("chips")

  shiny::observeEvent(input$survey_enter, {
    rv$err <- ""
    tryCatch({
      res <- survey_entry(query_study(), input$survey_email)
      u <- user_as_list(res$user)
      if (is.null(u$personId) || !nzchar(as.character(u$personId))) {
        p <- find_person_by_email(u$email)
        u$personId <- doc_id(p)
      }
      rv$survey_user <- u
      rv$survey_study <- res$study
      rv$survey_page <- 1L
    }, error = function(e) ee_handle(rv, e, "survey_entry"))
  })

  shiny::observeEvent(input$survey_next, {
    rv$survey_page <- rv$survey_page + 1L
  })
  shiny::observeEvent(input$survey_prev, {
    rv$survey_page <- max(1L, rv$survey_page - 1L)
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
      payload <- chips_payload(val$bins, val$totalChips, val$lowerBound, val$upperBound, input$chips_rationale %||% "")
      save_judgment(
        rv$survey_study, pg$question,
        rv$survey_user$personId %||% rv$survey_user$id,
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
    p10 <- as.numeric(input$p10)
    p50 <- as.numeric(input$p50)
    p90 <- as.numeric(input$p90)
    if (any(!is.finite(c(p10, p50, p90)))) {
      rv$err <- "Enter numeric P10, P50, and P90."
      ee_log("warn", rv$err, where = "save_quantile")
      return()
    }
    if (!(p10 <= p50 && p50 <= p90)) {
      rv$err <- "Quantiles must be non-decreasing: P10 ≤ P50 ≤ P90."
      ee_log("warn", rv$err, where = "save_quantile")
      return()
    }
    tryCatch({
      payload <- quantile_payload(p10, p50, p90, input$q_rationale %||% "")
      save_judgment(
        rv$survey_study, pg$question,
        rv$survey_user$personId %||% rv$survey_user$id,
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
      rv$shelf_result <- run_shelf_for_question(
        st, q,
        round_number = current_round(st),
        family = input$fit_family %||% "best"
      )
      rv$msg <- sprintf("SHELF run with %s (%s experts).", rv$shelf_result$fit$engine, rv$shelf_result$nExperts)
    }, error = function(e) ee_handle(rv, e, "run_shelf"))
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
          htmltools::tags$p("Start mongod or set MONGODB_URI in .Renviron. See README.md.")
        )
      ))
    }
    if (is_survey_mode()) return(survey_root_ui(input, rv, query_study(), ping))
    if (is.null(rv$user) && ee_auth_dev_mode()) return(login_ui())
    if (is.null(rv$user)) {
      return(shiny::div(class = "login-page", shiny::div(class = "login-card",
        htmltools::tags$h1("Sign in required"),
        htmltools::tags$p("This staff app expects Posit Connect login (session user).")
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
    df <- shelf_plot_df(rv$shelf_result$fit)
    shiny::req(df)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = "Value", y = "Density", colour = NULL, linetype = NULL) +
      ggplot2::theme(legend.position = "bottom")
    plotly::ggplotly(p, tooltip = c("colour", "x", "y"))
  })
}

survey_pages <- function(study, questions) {
  pages <- list(list(kind = "welcome", title = "Welcome", subtitle = study$title %||% "Survey"))
  for (q in questions) {
    if (is_chips_question(q)) {
      pages[[length(pages) + 1]] <- list(kind = "chips", title = "Chips-N-Bins", subtitle = q$title, question = q)
    } else {
      pages[[length(pages) + 1]] <- list(kind = "quantile", title = "Low-High-Best", subtitle = q$title, question = q)
    }
  }
  pages[[length(pages) + 1]] <- list(kind = "done", title = "Last page", subtitle = "Ready to submit your responses?")
  pages
}

survey_root_ui <- function(input, rv, study_key, ping) {
  if (!isTRUE(ping$ok)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("MongoDB not connected"), htmltools::tags$p(ping$message)
    )))
  }
  if (!nzchar(study_key)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("Survey"),
      htmltools::tags$p("Open a survey link that includes ?study=<slug>, for example ?study=hta-drug-a-vs-b")
    )))
  }
  st <- find_study(study_key)
  if (is.null(st)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("Case study not found"),
      htmltools::tags$p(paste("No study matches", study_key))
    )))
  }
  if (is.null(rv$survey_user)) {
    welcome <- (st$protocolConfig %||% list())$surveyWelcome %||% list()
    return(shiny::div(
      class = "survey-shell",
      ee_header(),
      shiny::div(
        class = "panel",
        htmltools::tags$h2(st$title),
        htmltools::tags$p(class = "muted", welcome$conductedBy %||% "AMCHSS · SCTIMST"),
        htmltools::tags$p("Enter the email the facilitator invited."),
        shiny::textInput("survey_email", "Email", placeholder = "priya.nair@hospital.org"),
        shiny::actionButton("survey_enter", "Continue", class = "btn-primary"),
        notice(rv$err, "error")
      )
    ))
  }
  qs <- study_questions(doc_id(st))
  pages <- survey_pages(st, qs)
  idx <- max(1L, min(as.integer(rv$survey_page), length(pages)))
  pg <- pages[[idx]]
  progress <- if (length(pages) <= 1) 0 else (idx - 1) / (length(pages) - 1)
  body <- switch(
    pg$kind,
    welcome = shiny::tagList(
      htmltools::tags$h2("Welcome to the survey"),
      htmltools::tags$p((st$protocolConfig$surveyWelcome$why %||% st$description)),
      htmltools::tags$p(class = "muted", st$protocolConfig$surveyWelcome$instructions %||% ""),
      shiny::actionButton("survey_next", "Start", class = "btn-primary")
    ),
    chips = shiny::tagList(
      htmltools::tags$h2(pg$title),
      htmltools::tags$p(pg$question$prompt),
      mod_chips_ui("chips"),
      shiny::textAreaInput("chips_rationale", "Rationale (optional)", rows = 3),
      notice(rv$err, "error"),
      notice(rv$msg, "ok"),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("save_chips", "Save and continue", class = "btn-primary")
      )
    ),
    quantile = shiny::tagList(
      htmltools::tags$h2(pg$title),
      htmltools::tags$p(pg$question$prompt),
      htmltools::tags$p(class = "muted", "P10 = value you think is only 10% likely to be below. P50 = best estimate. P90 = only 10% likely to be above."),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("p10", "P10 (low)", value = NA, min = 0, max = 1, step = 0.01)),
        shiny::column(4, shiny::numericInput("p50", "P50 (best)", value = NA, min = 0, max = 1, step = 0.01)),
        shiny::column(4, shiny::numericInput("p90", "P90 (high)", value = NA, min = 0, max = 1, step = 0.01))
      ),
      shiny::textAreaInput("q_rationale", "Rationale (optional)", rows = 3),
      notice(rv$err, "error"),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("save_quantile", "Save and continue", class = "btn-primary")
      )
    ),
    done = shiny::tagList(
      htmltools::tags$h2("Thank you"),
      htmltools::tags$p("Your judgements have been stored for this round. You may close this window."),
      htmltools::tags$p(class = "muted", paste("Signed in as", rv$survey_user$email))
    )
  )
  shiny::div(
    class = "survey-shell",
    ee_header(rv$survey_user),
    shiny::div(class = "progress-track", shiny::div(class = "progress-bar", style = sprintf("width:%s%%", round(100 * progress)))),
    shiny::div(
      class = "survey-layout",
      shiny::div(
        class = "survey-nav",
        lapply(seq_along(pages), function(i) {
          htmltools::div(
            class = paste("nav-item", if (i == idx) "active"),
            htmltools::tags$strong(paste(i, pages[[i]]$title)),
            htmltools::tags$span(class = "muted", pages[[i]]$subtitle)
          )
        })
      ),
      shiny::div(class = "panel survey-main", body)
    )
  )
}

staff_shell <- function(user, body) {
  extra <- shiny::tagList(
    shiny::actionButton("goto_dash", "Surveys", class = "btn-ghost"),
    if (user$platformRole %in% c("admin", "super_admin")) shiny::actionButton("goto_people", "People", class = "btn-ghost"),
    shiny::actionButton("logout", "Sign out", class = "btn-secondary")
  )
  shiny::div(class = "app-shell", ee_header(user, extra), shiny::div(class = "page-body", body))
}

staff_dash_ui <- function(rv) {
  studies <- list_studies_for_user(rv$user)
  cards <- if (!length(studies)) {
    htmltools::p(class = "muted", "No surveys yet. Seed the HTA demo or create one.")
  } else {
    lapply(studies, function(s) {
      nq <- mongo_count("questions", sprintf('{"studyId": %s, "isActive": true}', json_escape(doc_id(s))))
      ne <- mongo_count("study_access", sprintf('{"studyId": %s, "accessRole": "expert", "status": "active"}', json_escape(doc_id(s))))
      shiny::div(
        class = "study-card",
        htmltools::div(
          class = "study-card-top",
          htmltools::tags$h3(s$title),
          status_pill(s$status %||% "draft")
        ),
        htmltools::p(class = "muted", s$description %||% ""),
        htmltools::p(sprintf("%s experts · %s questions · %s", ne, nq, s$slug)),
        htmltools::tags$button(
          type = "button",
          class = "btn-primary",
          onclick = sprintf(
            "Shiny.setInputValue('open_study', '%s', {priority: 'event'})",
            doc_id(s)
          ),
          "Open"
        )
      )
    })
  }
  staff_shell(rv$user, shiny::tagList(
    htmltools::div(
      class = "page-head",
      htmltools::tags$h2("Surveys"),
      shiny::actionButton("seed_demo", "Seed HTA demo", class = "btn-secondary")
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Your case studies"), cards),
      shiny::div(
        class = "panel",
        htmltools::tags$h3("Create survey"),
        shiny::textInput("new_title", "Title", placeholder = "HTA: Drug A vs Drug B"),
        shiny::textInput("new_qty", "Quantity of interest", placeholder = "5-year progression-free probability"),
        shiny::textAreaInput("new_desc", "Why this elicitation?", rows = 3),
        shiny::checkboxGroupInput(
          "new_methods", "Methods",
          choiceNames = c("Chips-N-Bins", "Low-High-Best (P10/P50/P90)"),
          choiceValues = c("chips_and_bins", "quantile"),
          selected = c("chips_and_bins", "quantile")
        ),
        shiny::actionButton("create_study", "Create", class = "btn-primary")
      )
    )
  ))
}

staff_study_ui <- function(rv) {
  st <- find_study(rv$study_id)
  if (is.null(st)) return(staff_shell(rv$user, htmltools::p("Study not found.")))
  experts <- study_experts(st)
  url <- survey_url(st)
  rows <- if (!length(experts)) {
    htmltools::p(class = "muted", "No experts invited yet.")
  } else {
    htmltools::tags$table(
      class = "data",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Name"), htmltools::tags$th("Email"),
        htmltools::tags$th("Progress"), htmltools::tags$th("Status")
      )),
      htmltools::tags$tbody(lapply(experts, function(ex) {
        htmltools::tags$tr(
          htmltools::tags$td(ex$name),
          htmltools::tags$td(ex$email),
          htmltools::tags$td(sprintf("%s / %s", ex$answeredCount, ex$totalQuestions)),
          htmltools::tags$td(status_pill(ex$status))
        )
      }))
    )
  }
  staff_shell(rv$user, shiny::tagList(
    shiny::actionButton("goto_dash", "← Back to surveys", class = "btn-ghost"),
    htmltools::div(
      class = "page-head",
      htmltools::div(
        htmltools::tags$h2(st$title),
        status_pill(st$status),
        htmltools::span(class = "pill", sprintf("Round %s", current_round(st)))
      ),
      shiny::actionButton("goto_responses", "Responses / SHELF", class = "btn-primary")
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Expert survey link"),
      htmltools::tags$p(class = "muted", "Share this URL. The expert must enter an invited email."),
      shiny::div(class = "url-box", url)
    ),
    shiny::div(
      class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Experts"), rows),
      shiny::div(
        class = "panel",
        htmltools::tags$h3("Invite expert"),
        shiny::textInput("invite_email", "Email"),
        shiny::textInput("invite_name", "Name (optional)"),
        shiny::actionButton("invite_go", "Add expert", class = "btn-primary"),
        htmltools::hr(),
        shiny::actionButton("advance_round", "Advance to next round", class = "btn-secondary")
      )
    )
  ))
}

staff_responses_ui <- function(rv) {
  st <- find_study(rv$study_id)
  if (is.null(st)) return(staff_shell(rv$user, htmltools::p("Study not found.")))
  qs <- study_questions(doc_id(st))
  stt <- shelf_status()
  choices <- stats::setNames(vapply(qs, doc_id, character(1)), vapply(qs, function(q) q$title, character(1)))
  result_ui <- NULL
  if (!is.null(rv$shelf_result)) {
    result_ui <- shiny::tagList(
      htmltools::tags$p(class = "ok", rv$shelf_result$conclusion),
      plotly::plotlyOutput("shelf_plot", height = "420px")
    )
  }
  staff_shell(rv$user, shiny::tagList(
    shiny::actionButton("goto_dash", "← Surveys", class = "btn-ghost"),
    htmltools::tags$h2(paste("Responses —", st$title)),
    htmltools::p(
      class = if (isTRUE(stt$shelfPackage)) "ok" else "error",
      stt$message
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "panel",
      shiny::selectInput("resp_question", "Question", choices = choices),
      shiny::selectInput(
        "fit_family", "Fit family",
        choices = c("Best fitting" = "best", "Beta" = "beta", "Normal" = "normal", "Gamma" = "gamma", "Log normal" = "log_normal"),
        selected = "best"
      ),
      shiny::actionButton("run_shelf", "Run SHELF → charts", class = "btn-primary"),
      result_ui
    )
  ))
}

staff_people_ui <- function(rv) {
  people <- list_people()
  tbl <- htmltools::tags$table(
    class = "data",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Name"), htmltools::tags$th("Email"),
      htmltools::tags$th("Type"), htmltools::tags$th("Affiliation")
    )),
    htmltools::tags$tbody(lapply(people, function(p) {
      htmltools::tags$tr(
        htmltools::tags$td(p$name),
        htmltools::tags$td(p$email),
        htmltools::tags$td(p$personType),
        htmltools::tags$td(p$affiliation %||% "")
      )
    }))
  )
  staff_shell(rv$user, shiny::tagList(
    htmltools::tags$h2("People directory"),
    shiny::div(class = "panel", tbl)
  ))
}
