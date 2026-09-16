staff_shell <- function(user, body) {
  extra <- shiny::tagList(
    shiny::actionButton("goto_dash", "Surveys", class = "btn-ghost"),
    if (can_provision_people(user)) shiny::actionButton("goto_people", "People", class = "btn-ghost"),
    shiny::actionButton("logout", "Sign out", class = "btn-secondary")
  )
  shiny::div(class = "app-shell", ee_header(user, extra), shiny::div(class = "page-body", body))
}

staff_dash_ui <- function(rv) {
  user <- rv$user
  if (is_expert_role(user)) {
    return(staff_shell(user, shiny::div(
      class = "panel",
      htmltools::tags$h2("Expert access"),
      htmltools::tags$p("Open the personal survey link from your facilitator. Staff tools are not used for elicitation.")
    )))
  }
  studies <- list_studies_for_user(user)
  cards <- if (!length(studies)) {
    htmltools::p(class = "muted", if (is_viewer(user)) {
      "No completed consensus models are assigned to you yet."
    } else {
      "No surveys yet. Seed the HTA demo or create one."
    })
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
  create_panel <- if (can_create_study(user)) {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Create survey"),
      shiny::textInput("new_title", "Title", placeholder = "HTA: Drug A vs Drug B"),
      shiny::textInput("new_qty", "Quantity of interest", placeholder = "5-year progression-free probability"),
      shiny::textAreaInput("new_desc", "Why this elicitation?", rows = 3),
      shiny::selectInput(
        "new_variable_type", "Variable type",
        choices = c("Proportion / probability" = "proportion", "Continuous" = "continuous", "Count" = "count"),
        selected = "proportion"
      ),
      shiny::textInput("new_unit", "Unit", value = "probability", placeholder = "INR, years, cases, etc."),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("new_lower", "Lower plausible bound", value = 0, step = 0.01)),
        shiny::column(4, shiny::numericInput("new_upper", "Upper plausible bound", value = 1, step = 0.01)),
        shiny::column(4, shiny::numericInput("new_precision", "Decimal places", value = 2, min = 0, max = 6, step = 1))
      ),
      shiny::selectInput(
        "new_distribution", "Preferred distribution",
        choices = c("Best fitting" = "best", "Beta" = "beta", "Normal" = "normal",
                    "Gamma" = "gamma", "Log-normal" = "log_normal"),
        selected = "best"
      ),
      shiny::checkboxGroupInput(
        "new_methods", "Methods",
        choiceNames = c("Chips-N-Bins", "Low-High-Best (P10/P50/P90)"),
        choiceValues = c("chips_and_bins", "quantile"),
        selected = c("chips_and_bins", "quantile")
      ),
      shiny::actionButton("create_study", "Create", class = "btn-primary")
    )
  } else if (is_admin(user)) {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Platform admin"),
      htmltools::tags$p("Provision facilitators in People. Active deliberations are run by study managers; you can open a study only to inspect status, not to enter blinded SHELF fitting.")
    )
  } else {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Read only"),
      htmltools::tags$p("Completed consensus models and audit trails assigned to you.")
    )
  }
  staff_shell(user, shiny::tagList(
    htmltools::div(
      class = "page-head",
      htmltools::tags$h2("Surveys"),
      if (can_seed_demo(user)) shiny::actionButton("seed_demo", "Seed HTA demo", class = "btn-secondary")
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Your case studies"), cards),
      create_panel
    )
  ))
}

staff_study_ui <- function(rv) {
  st <- find_study(rv$study_id)
  user <- rv$user
  if (is.null(st)) return(staff_shell(user, htmltools::p("Study not found.")))
  experts <- study_experts(st)
  url <- survey_url(st)
  rows <- if (!length(experts)) {
    htmltools::p(class = "muted", "No experts invited yet.")
  } else {
    htmltools::tags$table(
      class = "data",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Name"), htmltools::tags$th("Email"),
        htmltools::tags$th("Progress"), htmltools::tags$th("Status"),
        if (can_invite(user, st)) htmltools::tags$th("Invite link"),
        if (can_manage_study(user, st)) htmltools::tags$th("Actions")
      )),
      htmltools::tags$tbody(lapply(experts, function(ex) {
        htmltools::tags$tr(
          htmltools::tags$td(ex$name),
          htmltools::tags$td(ex$email),
          htmltools::tags$td(sprintf("%s / %s", ex$answeredCount, ex$totalQuestions)),
          htmltools::tags$td(status_pill(ex$status)),
          if (can_invite(user, st)) htmltools::tags$td(htmltools::tags$code(ex$surveyUrl %||% "")),
          if (can_manage_study(user, st)) htmltools::tags$td(
            shiny::actionButton(
              paste0("remove_expert_", ex$personId),
              "Remove",
              class = "btn-secondary",
              onclick = sprintf(
                "Shiny.setInputValue('remove_expert', '%s', {priority: 'event'})",
                ex$personId
              )
            )
          )
        )
      }))
    )
  }
  shelf_btn <- if (can_view_shelf(user, st)) {
    shiny::actionButton("goto_responses", "Responses / SHELF", class = "btn-primary")
  } else if (is_admin(user) && study_is_active_deliberation(st)) {
    htmltools::span(class = "muted", "Active deliberation — SHELF is facilitator-only.")
  } else {
    NULL
  }
  invite_panel <- if (can_invite(user, st)) {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Invite expert"),
      shiny::textInput("invite_email", "Email"),
      shiny::textInput("invite_name", "Name (optional)"),
      shiny::actionButton("invite_go", "Add expert", class = "btn-primary"),
      htmltools::hr(),
      if (can_advance_round(user, st)) shiny::actionButton("advance_round", "Advance to next round", class = "btn-secondary"),
      if (can_complete_study(user, st)) shiny::actionButton("complete_study", "Mark elicitation complete", class = "btn-secondary")
    )
  } else {
    shiny::div(class = "panel", htmltools::tags$h3("Access"), htmltools::tags$p(class = "muted", "Read-only."))
  }
  staff_shell(user, shiny::tagList(
    shiny::actionButton("goto_dash", "← Back to surveys", class = "btn-ghost"),
    htmltools::div(
      class = "page-head",
      htmltools::div(
        htmltools::tags$h2(st$title),
        status_pill(st$status),
        htmltools::span(class = "pill", sprintf("Round %s", current_round(st)))
      ),
      shiny::tagList(
        shelf_btn,
        if (can_manage_study(user, st)) shiny::actionButton("archive_study", "Archive survey", class = "btn-secondary")
      )
    ),
    if (can_manage_study(user, st)) shiny::div(
      class = "panel",
      htmltools::tags$h3("Edit survey"),
      shiny::textInput("edit_title", "Title", value = st$title %||% ""),
      shiny::textAreaInput("edit_description", "Description", value = st$description %||% "", rows = 3),
      shiny::actionButton("save_study_details", "Save survey details", class = "btn-primary")
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Expert survey link"),
      htmltools::tags$p(class = "muted", "Share the personal invite link (token) from the table, or the study URL plus invited email."),
      shiny::div(class = "url-box", url)
    ),
    shiny::div(
      class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Experts"), rows),
      invite_panel
    )
  ))
}

staff_responses_ui <- function(rv) {
  st <- find_study(rv$study_id)
  user <- rv$user
  if (is.null(st)) return(staff_shell(user, htmltools::p("Study not found.")))
  if (!can_view_shelf(user, st)) {
    return(staff_shell(user, shiny::tagList(
      shiny::actionButton("goto_dash", "← Surveys", class = "btn-ghost"),
      htmltools::tags$p("You cannot open SHELF results for this study while it is in active deliberation.")
    )))
  }
  qs <- study_questions(doc_id(st))
  stt <- shelf_status()
  choices <- stats::setNames(vapply(qs, doc_id, character(1)), vapply(qs, function(q) q$title, character(1)))
  result_ui <- NULL
  if (!is.null(rv$shelf_result)) {
    comments <- list()
    qid <- rv$shelf_result$questionId %||% input_safe_qid(rv)
    if (!is.null(qid)) comments <- list_peer_comments(doc_id(st), qid, current_round(st))
    result_ui <- shiny::tagList(
      htmltools::tags$p(class = "ok", rv$shelf_result$conclusion),
      plotly::plotlyOutput("shelf_plot", height = "420px"),
      if (is.data.frame(rv$shelf_result$params)) {
        shiny::div(
          class = "panel parameter-panel",
          htmltools::tags$h3("Parameter table"),
          htmltools::tags$p(
            class = "muted",
            "Estimated lower, median, and upper elicited values for each expert and pooled result."
          ),
          shiny::div(class = "table-responsive", shiny::tableOutput("shelf_params"))
        )
      },
      if (length(comments)) {
        mapping <- blind_labels_for_ids(vapply(comments, function(c) as.character(c$authorPersonId), character(1)))
        shiny::div(
          class = "panel",
          htmltools::tags$h3("Blinded comments"),
          htmltools::tags$ul(lapply(comments, function(cmt) {
            htmltools::tags$li(htmltools::tags$strong(blind_label(cmt$authorPersonId, mapping)), cmt$body)
          }))
        )
      }
    )
  }
  run_controls <- if (can_run_shelf(user, st)) {
    preferred_distribution <- (st$protocolConfig %||% list())$preferredDistribution %||% "best"
    shiny::tagList(
      shiny::selectInput(
        "fit_family", "Distribution override",
        choices = c("Best fitting (SHELF)" = "best", "Beta" = "beta", "Normal" = "normal", "Gamma" = "gamma", "Log normal" = "log_normal"),
        selected = preferred_distribution
      ),
      htmltools::tags$p(
        class = "muted",
        "Expert distributions always use SHELF best fits. Choose another family only to override the pooled result."
      ),
      shiny::actionButton("run_shelf", "Run SHELF → charts", class = "btn-primary")
    )
  } else {
    htmltools::tags$p(class = "muted", "Viewing stored consensus. Fitting is facilitator-only.")
  }
  export_ui <- if (can_export_audit(user, st)) {
    shiny::div(
      class = "btn-row",
      shiny::downloadButton("dl_csv", "Download CSV"),
      shiny::downloadButton("dl_pdf", "Download complete PDF")
    )
  } else {
    NULL
  }
  staff_shell(user, shiny::tagList(
    shiny::actionButton("goto_dash", "← Surveys", class = "btn-ghost"),
    htmltools::tags$h2(paste("Responses —", st$title)),
    htmltools::p(class = if (isTRUE(stt$shelfPackage)) "ok" else "error", stt$message),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "panel",
      shiny::selectInput("resp_question", "Question", choices = choices),
      shiny::radioButtons(
        "pool_view", "Pool overlay",
        choiceNames = c("Median and linearPool Fit", "Median pool", "linearPool Fit"),
        choiceValues = c("both", "median", "linear"),
        selected = "both",
        inline = TRUE
      ),
      run_controls,
      export_ui,
      result_ui
    )
  ))
}

input_safe_qid <- function(rv) {
  rv$shelf_result$questionId
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
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "two-col",
      shiny::div(class = "panel", tbl),
      shiny::div(
        class = "panel",
        htmltools::tags$h3("Provision person"),
        shiny::textInput("person_name", "Name"),
        shiny::textInput("person_email", "Email"),
        shiny::textInput("person_affil", "Affiliation"),
        shiny::selectInput(
          "person_type", "Type",
          choices = c("Facilitator" = "facilitator", "Researcher" = "researcher",
                      "Student" = "student", "Expert" = "expert")
        ),
        shiny::actionButton("person_add", "Save", class = "btn-primary"),
        htmltools::tags$p(class = "muted", "Only administrators manage the organization-wide directory. Facilitators manage experts within their own surveys.")
      )
    )
  ))
}
