ROLES <- c("admin", "facilitator", "researcher", "expert", "student")

role_label <- function(role) {
  switch(
    role %||% "",
    admin = "Admin",
    facilitator = "Facilitator",
    researcher = "Researcher",
    expert = "Expert",
    student = "Student",
    super_admin = "Admin",
    "User"
  )
}

staff_roles <- function() c("admin", "super_admin", "facilitator", "researcher")

is_staff_role <- function(role) role %in% staff_roles()

ensure_org <- function() {
  org <- mongo_one("organizations", '{"slug": "sctimst"}')
  if (!is.null(org)) return(org)
  mongo_insert("organizations", list(
    name = "Sree Chitra Tirunal Institute for Medical Sciences & Technology",
    slug = "sctimst",
    settings = list(allowExpertSelfSignup = FALSE, defaultProtocol = "shelf"),
    createdAt = iso_now(),
    updatedAt = iso_now()
  ))
}

find_user_by_email <- function(email) {
  mongo_one("users", q_field("email", tolower(trimws(email))))
}

find_person_by_email <- function(email) {
  mongo_one("people", sprintf(
    '{"email": %s, "isActive": true}',
    json_escape(tolower(trimws(email)))
  ))
}

link_person_to_user <- function(user_id, email) {
  person <- find_person_by_email(email)
  if (is.null(person)) return(NULL)
  now <- iso_now()
  mongo_update("people", q_id(doc_id(person)), list(
    userId = user_id,
    inviteStatus = "accepted",
    updatedAt = now
  ))
  mongo_col("study_access")$update(
    sprintf('{"personId": %s, "status": "active"}', json_escape(doc_id(person))),
    sprintf('{"$set": {"userId": %s, "updatedAt": %s}}', json_escape(user_id), json_escape(now)),
    multiple = TRUE
  )
  mongo_update("users", q_id(user_id), list(personId = doc_id(person), updatedAt = now))
  doc_id(person)
}

dev_login <- function(email, display_name, platform_role) {
  stopifnot(ee_auth_dev_mode())
  email <- tolower(trimws(email))
  role <- if (platform_role %in% ROLES) platform_role else "expert"
  org <- ensure_org()
  now <- iso_now()
  existing <- find_user_by_email(email)
  if (!is.null(existing)) {
    mongo_update("users", q_id(doc_id(existing)), list(
      displayName = display_name,
      platformRole = role,
      lastLoginAt = now,
      updatedAt = now,
      isActive = TRUE
    ))
    user_id <- doc_id(existing)
  } else {
    doc <- mongo_insert("users", list(
      email = email,
      displayName = display_name,
      photoURL = NULL,
      authProvider = "dev",
      platformRole = role,
      orgId = doc_id(org),
      personId = NULL,
      isActive = TRUE,
      lastLoginAt = now,
      createdAt = now,
      updatedAt = now
    ))
    user_id <- doc_id(doc)
  }
  link_person_to_user(user_id, email)
  find_user_by_email(email)
}

connect_login <- function(username) {
  email <- tolower(trimws(username))
  if (!grepl("@", email)) email <- paste0(email, "@sctimst.ac.in")
  existing <- find_user_by_email(email)
  if (!is.null(existing)) return(existing)
  org <- ensure_org()
  person <- find_person_by_email(email)
  role <- if (!is.null(person)) person$personType %||% "expert" else "expert"
  now <- iso_now()
  doc <- mongo_insert("users", list(
    email = email,
    displayName = person$name %||% username,
    authProvider = "posit-connect",
    platformRole = role,
    orgId = doc_id(org),
    personId = if (!is.null(person)) doc_id(person) else NULL,
    isActive = TRUE,
    lastLoginAt = now,
    createdAt = now,
    updatedAt = now
  ))
  link_person_to_user(doc_id(doc), email)
  find_user_by_email(email)
}

survey_entry <- function(study_id_or_slug, email) {
  email <- tolower(trimws(email))
  study <- find_study(study_id_or_slug)
  if (is.null(study)) stop("Case study not found")
  person <- find_person_by_email(email)
  if (is.null(person)) {
    stop("This email is not invited. Ask the facilitator to add your email.")
  }
  access <- mongo_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "status": "active", "accessRole": "expert"}',
    json_escape(doc_id(study)),
    json_escape(doc_id(person))
  ))
  if (is.null(access)) {
    stop("This email is not invited to this case study.")
  }
  now <- iso_now()
  existing <- find_user_by_email(email)
  if (!is.null(existing)) {
    mongo_update("users", q_id(doc_id(existing)), list(
      displayName = person$name %||% email,
      personId = doc_id(person),
      lastLoginAt = now,
      updatedAt = now,
      isActive = TRUE
    ))
    user <- find_user_by_email(email)
  } else {
    user <- mongo_insert("users", list(
      email = email,
      displayName = person$name %||% email,
      authProvider = "survey-invite",
      platformRole = "expert",
      orgId = person$orgId,
      personId = doc_id(person),
      isActive = TRUE,
      lastLoginAt = now,
      createdAt = now,
      updatedAt = now
    ))
  }
  link_person_to_user(doc_id(user), email)
  list(user = find_user_by_email(email), study = study)
}

user_as_list <- function(u) {
  if (is.null(u)) return(NULL)
  list(
    id = doc_id(u),
    email = u$email,
    displayName = u$displayName %||% u$email,
    platformRole = u$platformRole %||% "expert",
    orgId = u$orgId,
    personId = u$personId,
    isActive = isTRUE(u$isActive)
  )
}
