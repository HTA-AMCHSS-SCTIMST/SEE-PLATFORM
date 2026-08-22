user_role <- function(user) {
  if (is.null(user)) return("")
  as.character(user$platformRole %||% "")
}

is_admin <- function(user) user_role(user) %in% c("admin", "super_admin")
is_facilitator <- function(user) identical(user_role(user), "facilitator")
is_viewer <- function(user) user_role(user) %in% c("researcher", "student")
is_expert_role <- function(user) identical(user_role(user), "expert")

study_status <- function(study) as.character(study$status %||% "draft")

study_is_active_deliberation <- function(study) {
  study_status(study) %in% c("draft", "recruiting", "eliciting", "workshop")
}

study_is_completed <- function(study) identical(study_status(study), "completed")

can_provision_people <- function(user) is_admin(user)
can_create_study <- function(user) is_facilitator(user)
can_seed_demo <- function(user) is_facilitator(user)
can_invite <- function(user) is_facilitator(user)
can_advance_round <- function(user) is_facilitator(user)
can_complete_study <- function(user) is_facilitator(user)

can_run_shelf <- function(user, study = NULL) {
  if (!is_facilitator(user)) return(FALSE)
  TRUE
}

can_view_shelf <- function(user, study) {
  if (is.null(user) || is.null(study)) return(FALSE)
  if (is_facilitator(user)) return(TRUE)
  if (is_admin(user)) return(study_is_completed(study))
  if (is_viewer(user)) return(study_is_completed(study))
  FALSE
}

can_export_audit <- function(user, study) can_view_shelf(user, study)

require_role <- function(ok, msg = "You do not have permission for this action.") {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

add_person <- function(name, email, person_type, affiliation = "", user) {
  require_role(can_provision_people(user))
  email <- tolower(trimws(email))
  if (!nzchar(email) || !grepl("@", email)) stop("Enter a valid email.")
  person_type <- match.arg(person_type, c("facilitator", "researcher", "student", "expert"))
  org <- ensure_org()
  now <- iso_now()
  existing <- find_person_by_email(email)
  if (!is.null(existing)) {
    mongo_update("people", q_id(doc_id(existing)), list(
      name = name %||% existing$name,
      personType = person_type,
      affiliation = affiliation %||% existing$affiliation %||% "",
      isActive = TRUE,
      updatedAt = now
    ))
    return(find_person_by_email(email))
  }
  mongo_insert("people", list(
    orgId = doc_id(org),
    personType = person_type,
    name = name %||% email,
    email = email,
    affiliation = affiliation,
    expertise = list(),
    tags = list(person_type),
    inviteStatus = if (person_type == "facilitator") "active" else "invited",
    isActive = TRUE,
    createdAt = now,
    updatedAt = now,
    createdBy = user$id
  ))
}

complete_study <- function(study, user) {
  require_role(can_complete_study(user))
  mongo_update("studies", q_id(doc_id(study)), list(
    status = "completed",
    updatedAt = iso_now()
  ))
  find_study(doc_id(study))
}
