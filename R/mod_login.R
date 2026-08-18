login_ui <- function() {
  shiny::div(
    class = "login-page",
    shiny::div(
      class = "login-card",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST"),
      htmltools::tags$h1("EXPERT ELICITATION & STATISTICAL PLATFORM"),
      htmltools::tags$p(class = "muted",
        "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"),
      htmltools::tags$p(class = "team", "AMCHSS · SCTIMST"),
      shiny::div(
        class = "muted",
        "Local / Posit-dev sign-in. Facilitators create case studies and invite experts. After seeding, open the survey as ",
        htmltools::tags$code("priya.nair@hospital.org")
      ),
      shiny::radioButtons(
        "login_role",
        "Role",
        choiceNames = c("Admin", "Facilitator", "Researcher", "Expert", "Student"),
        choiceValues = c("admin", "facilitator", "researcher", "expert", "student"),
        selected = "facilitator",
        inline = TRUE
      ),
      shiny::textInput("login_email", "Email", value = "facilitator@sctimst.ac.in"),
      shiny::textInput("login_name", "Display name", value = "Facilitator"),
      shiny::actionButton("login_go", "Sign in", class = "btn-primary"),
      shiny::uiOutput("login_err")
    )
  )
}
