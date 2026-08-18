app_ui <- function() {
  shiny::fluidPage(
    title = "Expert Elicitation · AMCHSS SCTIMST",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@600;700&family=Source+Sans+3:wght@400;600;700&display=swap"
      )
    ),
    shiny::uiOutput("root")
  )
}
