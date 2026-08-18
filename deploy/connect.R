# Deploy this Shiny app to Posit Connect (institute server) or shinyapps.io.
#
# Required env (do not commit secrets):
#   CONNECT_SERVER   e.g. https://posit.sctimst.ac.in
#   CONNECT_API_KEY  from Connect → your name → API Keys
#
# Optional:
#   CONNECT_APP_NAME   default elicitation-platform
#   APP_ROLE           staff | survey | both   (set as Connect Var after first publish)
#
# Usage (PowerShell, from rwebapp):
#   $env:CONNECT_SERVER="https://YOUR-CONNECT-HOST"
#   $env:CONNECT_API_KEY="..."
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" deploy/connect.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."), winslash = "/")
} else {
  getwd()
}
setwd(root)

server <- Sys.getenv("CONNECT_SERVER", "")
api_key <- Sys.getenv("CONNECT_API_KEY", "")
app_name <- Sys.getenv("CONNECT_APP_NAME", "elicitation-platform")

if (!nzchar(server) || !nzchar(api_key)) {
  stop(
    "Set CONNECT_SERVER and CONNECT_API_KEY then re-run.\n",
    "Example:\n",
    "  Sys.setenv(CONNECT_SERVER = 'https://posit.your-institute.edu')\n",
    "  Sys.setenv(CONNECT_API_KEY = '....')\n",
    "  source('deploy/connect.R')\n",
    call. = FALSE
  )
}

server <- sub("/+$", "", server)
message("Registering Connect server ", server)
rsconnect::addConnectServer(url = server, name = "institute", quiet = TRUE)
rsconnect::connectApiUser(server = "institute", apiKey = api_key, quiet = TRUE)

message("Writing manifest…")
rsconnect::writeManifest(appDir = root, appPrimaryDoc = "app.R", appMode = "shiny")

message("Deploying as ", app_name, " …")
rsconnect::deployApp(
  appDir = root,
  appName = app_name,
  appTitle = "Expert Elicitation & Statistical Platform",
  server = "institute",
  launch.browser = TRUE,
  forceUpdate = TRUE,
  logLevel = "verbose"
)

message(
  "After publish, in Connect → this content → Vars set:\n",
  "  MONGODB_URI, MONGODB_DB, AUTH_DEV_MODE=false, APP_ROLE=both,\n",
  "  SURVEY_PUBLIC_URL=<this content's public URL>\n",
  "Access: staff should require login; survey can be Anyone + invited email gate."
)
