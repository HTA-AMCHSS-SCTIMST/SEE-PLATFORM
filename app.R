# Expert Elicitation & Statistical Platform (Shiny)
# AMCHSS · SCTIMST
#
# Local:  shiny::runApp(".", port = 3938)
# or:     source("run_local.R")

local_dir <- getwd()

renviron <- file.path(local_dir, ".Renviron")
if (file.exists(renviron)) {
  readRenviron(renviron)
} else if (file.exists(file.path(local_dir, ".Renviron.example"))) {
  message("No .Renviron found — copy .Renviron.example to .Renviron and set MONGODB_URI.")
}

r_dir <- file.path(local_dir, "R")
r_files <- c(
  "config.R", "log.R", "ids.R", "mongo.R", "auth.R", "chips.R", "shelf.R",
  "studies.R", "seed.R", "ui_helpers.R", "mod_login.R", "mod_chips.R",
  "app_ui.R", "app_server.R"
)
for (f in r_files) {
  source(file.path(r_dir, f), local = FALSE)
}

needed <- c("shiny", "htmltools", "mongolite", "jsonlite", "ggplot2", "plotly")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Install missing R packages first:\n  install.packages(c(",
    paste(sprintf('\"%s\"', missing), collapse = ", "),
    "))",
    call. = FALSE
  )
}

options(shiny.maxRequestSize = 32 * 1024^2)
ee_setup_shiny_logging()
if (isTRUE(ee_on_posit_connect())) {
  ee_log("info", "Posit Connect runtime detected; AUTH_DEV_MODE defaults off", where = "startup")
}

ui <- app_ui()
server <- app_server
shiny::shinyApp(ui, server)
