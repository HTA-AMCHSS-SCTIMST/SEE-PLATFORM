# App configuration from environment (local .Renviron or Posit Connect vars)

ee_env <- function(name, default = "") {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) default else val
}

ee_on_posit_connect <- function() {
  nzchar(ee_env("CONNECT_SERVER")) ||
    nzchar(ee_env("CONNECT_CONTENT_GUID")) ||
    identical(tolower(ee_env("POSIT_CONNECT", "false")), "true")
}

ee_auth_dev_mode <- function() {
  default <- if (ee_on_posit_connect()) "false" else "true"
  tolower(ee_env("AUTH_DEV_MODE", default)) %in% c("1", "true", "yes")
}

ee_app_role <- function() {
  role <- tolower(ee_env("APP_ROLE", "both"))
  if (!role %in% c("staff", "survey", "both")) "both" else role
}

ee_mongodb_uri <- function() {
  ee_env("MONGODB_URI", "mongodb://127.0.0.1:27017")
}

ee_mongodb_db <- function() {
  ee_env("MONGODB_DB", "expert_elicitation_shiny")
}

ee_survey_public_url <- function() {
  sub("/+$", "", ee_env("SURVEY_PUBLIC_URL", "http://127.0.0.1:3938"))
}

ee_connect_user <- function(session) {
  u <- session$user
  if (is.null(u) || !nzchar(u)) NULL else u
}
