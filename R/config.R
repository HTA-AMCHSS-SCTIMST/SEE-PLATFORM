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
  if (ee_on_posit_connect()) return(FALSE)
  if (ee_oidc_enabled()) return(FALSE)
  tolower(ee_env("AUTH_DEV_MODE", "true")) %in% c("1", "true", "yes")
}

ee_oidc_enabled <- function() {
  all(nzchar(c(
    ee_env("OIDC_ISSUER"),
    ee_env("OIDC_CLIENT_ID"),
    ee_env("OIDC_CLIENT_SECRET"),
    ee_env("OIDC_REDIRECT_URI"),
    ee_env("OIDC_STATE_SECRET")
  )))
}

ee_oidc_issuer <- function() sub("/+$", "", ee_env("OIDC_ISSUER"))

ee_oidc_discovery <- function() {
  if (!ee_oidc_enabled()) stop("OIDC is not configured.", call. = FALSE)
  httr2::request(paste0(ee_oidc_issuer(), "/.well-known/openid-configuration")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

ee_oidc_callback_url <- function() ee_env("OIDC_REDIRECT_URI")

ee_oidc_state <- function() {
  payload <- paste(as.integer(Sys.time()), openssl::base64_encode(openssl::rand_bytes(32)), sep = ".")
  signature <- openssl::sha256(charToRaw(payload), key = charToRaw(ee_env("OIDC_STATE_SECRET")))
  paste(payload, openssl::base64_encode(signature), sep = ".")
}

ee_oidc_verify_state <- function(state) {
  parts <- strsplit(state %||% "", ".", fixed = TRUE)[[1]]
  if (length(parts) != 3L || !grepl("^[0-9]+$", parts[[1]])) return(FALSE)
  age <- as.numeric(Sys.time()) - as.numeric(parts[[1]])
  if (!is.finite(age) || age < 0 || age > 600) return(FALSE)
  payload <- paste(parts[[1]], parts[[2]], sep = ".")
  expected <- openssl::base64_encode(
    openssl::sha256(charToRaw(payload), key = charToRaw(ee_env("OIDC_STATE_SECRET")))
  )
  identical(parts[[3]], expected)
}

ee_oidc_authorize_url <- function(state) {
  discovery <- ee_oidc_discovery()
  params <- c(
    response_type = "code",
    client_id = ee_env("OIDC_CLIENT_ID"),
    redirect_uri = ee_oidc_callback_url(),
    scope = ee_env("OIDC_SCOPE", "openid profile email"),
    state = state
  )
  paste0(
    discovery$authorization_endpoint,
    "?",
    paste(
      vapply(names(params), function(name) {
        paste(utils::URLencode(name, reserved = TRUE),
              utils::URLencode(params[[name]], reserved = TRUE), sep = "=")
      }, character(1)),
      collapse = "&"
    )
  )
}

ee_session_timeout_minutes <- function() {
  value <- suppressWarnings(as.numeric(ee_env("SESSION_TIMEOUT_MINUTES", "30")))
  if (!is.finite(value) || value <= 0) 30 else value
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
