new_invite_secret <- function() {
  paste(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
}

token_expires_iso <- function(days = 90) {
  format(as.POSIXct(Sys.time(), tz = "UTC") + days * 86400, "%Y-%m-%dT%H:%M:%SZ")
}

issue_invite_token <- function(study, person, days = 90) {
  now <- iso_now()
  existing <- mongo_one("invite_tokens", sprintf(
    '{"studyId": %s, "personId": %s, "revoked": false}',
    json_escape(doc_id(study)),
    json_escape(doc_id(person))
  ))
  if (!is.null(existing) && nzchar(existing$token %||% "")) return(existing)
  mongo_insert("invite_tokens", list(
    token = new_invite_secret(),
    studyId = doc_id(study),
    personId = doc_id(person),
    email = person$email,
    revoked = FALSE,
    expiresAt = token_expires_iso(days),
    createdAt = now
  ))
}

find_invite_token <- function(secret) {
  secret <- trimws(secret %||% "")
  if (!nzchar(secret)) return(NULL)
  mongo_one("invite_tokens", sprintf('{"token": %s, "revoked": false}', json_escape(secret)))
}

expert_survey_url <- function(study, person = NULL, token_doc = NULL) {
  base <- sprintf(
    "%s/?study=%s",
    ee_survey_public_url(),
    utils::URLencode(study$slug %||% doc_id(study), reserved = TRUE)
  )
  tok <- token_doc
  if (is.null(tok) && !is.null(person)) tok <- issue_invite_token(study, person)
  if (is.null(tok)) return(base)
  paste0(base, "&t=", utils::URLencode(tok$token, reserved = TRUE))
}

survey_entry_token <- function(secret) {
  tok <- find_invite_token(secret)
  if (is.null(tok)) stop("This invite link is invalid or has been revoked.")
  study <- find_study(tok$studyId)
  if (is.null(study)) stop("Case study not found")
  person <- mongo_one("people", q_id(tok$personId))
  if (is.null(person) || !isTRUE(person$isActive)) {
    stop("This email is not invited. Ask the facilitator to add your email.")
  }
  survey_entry(doc_id(study), person$email)
}
