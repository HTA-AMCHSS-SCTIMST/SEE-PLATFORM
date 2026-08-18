# One-command local start (staff + survey on the same port).
# In RStudio: Session → Set Working Directory → To Source File Location, then source this file.
# Or from the rwebapp folder:
#   source("run_local.R")

if (file.exists(".Renviron.example") && !file.exists(".Renviron")) {
  file.copy(".Renviron.example", ".Renviron")
  message("Created .Renviron from the example. Edit MONGODB_URI if you use Atlas, then source this file again.")
}

if (file.exists(".Renviron")) readRenviron(".Renviron")

pkgs <- c("shiny", "htmltools", "mongolite", "jsonlite", "ggplot2", "plotly")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
if (!requireNamespace("SHELF", quietly = TRUE)) {
  message("Installing SHELF (official elicitation fits)…")
  try(install.packages("SHELF", repos = "https://cloud.r-project.org"), silent = TRUE)
}

port <- as.integer(Sys.getenv("SHINY_PORT", "3938"))
host <- Sys.getenv("SHINY_HOST", "127.0.0.1")
message("Opening http://", host, ":", port, "  (survey: /?study=hta-drug-a-vs-b after Seed HTA demo)")
shiny::runApp(".", port = port, host = host, launch.browser = TRUE)
