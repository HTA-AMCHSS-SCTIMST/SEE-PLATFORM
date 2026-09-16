primary_question <- function(questions) {
  if (!length(questions)) return(NULL)
  qn <- Filter(function(q) !is_chips_question(q), questions)
  if (length(qn)) qn[[1]] else questions[[1]]
}

survey_pages <- function(study, questions) {
  rnd <- current_round(study)
  pages <- list(list(kind = "welcome", title = "Welcome", subtitle = study$title %||% "Survey"))
  pages[[length(pages) + 1]] <- list(kind = "onboarding", title = "COI & terms", subtitle = "Declarations")
  pages[[length(pages) + 1]] <- list(kind = "calibration", title = "Practice", subtitle = "Calibration task")
  if (rnd >= 2L) {
    pages[[length(pages) + 1]] <- list(
      kind = "review",
      title = "Round 2 review",
      subtitle = "Blinded peer distributions",
      question = primary_question(questions)
    )
  }
  pages[[length(pages) + 1]] <- list(kind = "bounds", title = "Plausible bounds", subtitle = "L and U first")
  for (q in questions) {
    if (is_chips_question(q)) {
      pages[[length(pages) + 1]] <- list(kind = "chips", title = "Chips-N-Bins", subtitle = q$title, question = q)
    } else {
      pages[[length(pages) + 1]] <- list(kind = "quantile", title = "Quantiles", subtitle = q$title, question = q)
    }
  }
  pages[[length(pages) + 1]] <- list(kind = "done", title = "Last page", subtitle = "Ready to submit your responses?")
  pages
}

survey_root_ui <- function(input, rv, study_key, ping, token_key = "") {
  rv$review_refresh
  if (!isTRUE(ping$ok)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("MongoDB not connected"), htmltools::tags$p(ping$message)
    )))
  }
  if (!nzchar(study_key) && !nzchar(token_key)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("Survey"),
      htmltools::tags$p("Open a survey link that includes ?study=<slug> or a personal invite token.")
    )))
  }
  st <- if (nzchar(study_key)) find_study(study_key) else NULL
  if (is.null(st) && nzchar(token_key)) {
    tok <- find_invite_token(token_key)
    if (!is.null(tok)) st <- find_study(tok$studyId)
  }
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
        htmltools::tags$p("Use your personal invite link, or enter the email the facilitator invited."),
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
  lo0 <- rv$bound_lo %||% 0
  hi0 <- rv$bound_hi %||% 1
  body <- switch(
    pg$kind,
    welcome = shiny::tagList(
      htmltools::tags$h2("Welcome to the survey"),
      htmltools::tags$p((st$protocolConfig$surveyWelcome$why %||% st$description)),
      htmltools::tags$p(class = "muted", st$protocolConfig$surveyWelcome$instructions %||% ""),
      shiny::actionButton("survey_next", "Start", class = "btn-primary")
    ),
    onboarding = shiny::tagList(
      htmltools::tags$h2("Conflict of interest and terms"),
      htmltools::tags$p("Mandatory before any judgments. Write “None” if you have no ties."),
      shiny::textAreaInput("coi_financial", "Financial ties", rows = 3, placeholder = "None"),
      shiny::textAreaInput("coi_academic", "Academic or professional ties", rows = 3, placeholder = "None"),
      shiny::radioButtons(
        "attribution_pref", "Attribution preference",
        choiceNames = c("Anonymous in feedback (Expert A, B, C)", "Attributed (name visible to facilitator reports)"),
        choiceValues = c("anonymous", "attributed"),
        selected = "anonymous"
      ),
      shiny::checkboxInput("tou_accept", "I accept the terms of use for this elicitation.", value = FALSE),
      notice(rv$err, "error"),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("save_onboarding", "Save and continue", class = "btn-primary")
      )
    ),
    calibration = shiny::tagList(
      htmltools::tags$h2("Practice calibration"),
      htmltools::tags$p("This is not part of the case-study quantity. If a fair coin is flipped, what is your probability that it lands heads?"),
      shiny::numericInput("practice_p", "P(heads)", value = 0.5, min = 0, max = 1, step = 0.01),
      notice(rv$err, "error"),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("save_calibration", "Save and continue", class = "btn-primary")
      )
    ),
    review = survey_review_body(rv, pg),
    bounds = shiny::tagList(
      htmltools::tags$h2("Facilitator-defined plausible bounds"),
      htmltools::tags$p(
        "The facilitator set these limits before inviting experts. Keep all judgments strictly within this range."
      ),
      htmltools::tags$p(
        class = "muted",
        sprintf("Lower limit (L): %s · Upper limit (U): %s", lo0, hi0)
      ),
      notice(rv$err, "error"),
      shiny::div(
        class = "btn-row",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("survey_next", "Continue", class = "btn-primary")
      )
    ),
    chips = shiny::tagList(
      htmltools::tags$h2(pg$title),
      htmltools::tags$p(pg$question$prompt),
      mod_chips_ui("chips"),
      shiny::textAreaInput("chips_rationale", "Rationale (required)", rows = 3),
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
      htmltools::tags$p(class = "muted", sprintf("Your bounds: L = %s, U = %s. Values must satisfy L < … < U.", lo0, hi0)),
      shiny::radioButtons(
        "q_mode", "Method",
        choiceNames = c("Percentiles (P10, P50, P90)", "Quartiles (Q1, median, Q3)"),
        choiceValues = c("percentile", "quartile"),
        selected = input$q_mode %||% "percentile",
        inline = TRUE
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("p10", "P10 / Q1", value = NA, min = lo0, max = hi0, step = 0.01)),
        shiny::column(4, shiny::numericInput("p50", "P50 / median", value = NA, min = lo0, max = hi0, step = 0.01)),
        shiny::column(4, shiny::numericInput("p90", "P90 / Q3", value = NA, min = lo0, max = hi0, step = 0.01))
      ),
      shiny::textAreaInput("q_rationale", "Rationale (required)", rows = 3),
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

survey_review_body <- function(rv, pg) {
  fit_ui <- if (!is.null(rv$review_result)) {
    shiny::tagList(
      htmltools::tags$p(class = "ok", rv$review_result$conclusion),
      plotly::plotlyOutput("review_plot", height = "360px"),
      htmltools::tags$h3("Peer rationales (anonymized)"),
      review_rationale_list(rv$review_result),
      htmltools::tags$h3("Blinded commentary"),
      review_comment_list(rv),
      shiny::textAreaInput("peer_comment", "Your comment (no names)", rows = 3),
      shiny::actionButton("save_comment", "Post comment", class = "btn-secondary")
    )
  } else {
    htmltools::tags$p(class = "muted", "No round-1 overlay is available yet. You can still continue.")
  }
  shiny::tagList(
    htmltools::tags$h2("Blinded peer review"),
    htmltools::tags$p("Individual distributions are labelled Expert A, B, C. Comment on the evidence, not on people."),
    if (!is.null(rv$review_updated_at)) {
      htmltools::tags$p(
        class = "muted live-feedback-status",
        sprintf("Live feedback updates automatically · last distribution update %s",
                format(rv$review_updated_at, "%H:%M:%S"))
      )
    },
    fit_ui,
    notice(rv$err, "error"),
    notice(rv$msg, "ok"),
    shiny::div(
      class = "btn-row",
      shiny::actionButton("survey_prev", "Back"),
      shiny::actionButton("survey_next", "Continue to your round-2 judgments", class = "btn-primary")
    )
  )
}

review_rationale_list <- function(shelf_result) {
  items <- lapply(shelf_result$fit$experts %||% list(), function(ex) {
    htmltools::tags$li(
      htmltools::tags$strong(ex$name),
      htmltools::tags$span(ex$rationale %||% "")
    )
  })
  htmltools::tags$ul(items)
}

review_comment_list <- function(rv) {
  st <- rv$survey_study
  q <- primary_question(study_questions(doc_id(st)))
  if (is.null(q)) return(NULL)
  comments <- list_peer_comments(doc_id(st), doc_id(q), current_round(st))
  if (!length(comments)) return(htmltools::tags$p(class = "muted", "No comments yet."))
  js <- current_judgments(doc_id(st), doc_id(q), max(1L, current_round(st) - 1L))
  mapping <- blind_labels_for_ids(c(
    vapply(js, function(j) as.character(j$expertId %||% ""), character(1)),
    vapply(comments, function(c) as.character(c$authorPersonId %||% ""), character(1))
  ))
  htmltools::tags$ul(lapply(comments, function(cmt) {
    htmltools::tags$li(
      htmltools::tags$strong(blind_label(cmt$authorPersonId, mapping)),
      htmltools::tags$span(cmt$body)
    )
  }))
}
