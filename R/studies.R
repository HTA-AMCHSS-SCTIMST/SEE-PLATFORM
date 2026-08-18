current_round <- function(study) {
  cfg <- study$protocolConfig %||% list()
  r <- suppressWarnings(as.integer(cfg$currentRound %||% 1))
  if (is.na(r) || r < 1) 1L else r
}

unique_slug <- function(base) {
  candidate <- base %||% "study"
  n <- 0L
  repeat {
    slug <- if (n == 0L) candidate else paste0(candidate, "-", n)
    if (is.null(mongo_one("studies", q_field("slug", slug)))) return(slug)
    n <- n + 1L
  }
}

create_study <- function(user, title, description = "", quantity = "",
                         methods = c("chips_and_bins", "quantile")) {
  org <- ensure_org()
  now <- iso_now()
  slug <- unique_slug(slugify(title))
  protocol_config <- list(
    methods = as.list(methods),
    currentRound = 1L,
    rounds = 3L,
    anonymizeFeedback = TRUE,
    quantileLevels = list(0.1, 0.5, 0.9),
    surveyWelcome = list(
      conductedBy = "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST, Trivandrum",
      why = description %||% "To capture structured expert judgment for decision support.",
      reason = "SHELF expert elicitation for HTA / clinical decision support.",
      instructions = "Complete chips-and-bins and/or P10/P50/P90 tasks, then submit.",
      quantityOfInterest = quantity,
      contactEmail = user$email
    ),
    caseStudyMeta = list(
      quantityOfInterest = quantity,
      explanation = description
    )
  )
  study <- mongo_insert("studies", list(
    orgId = doc_id(org),
    slug = slug,
    title = title,
    description = description,
    protocolType = "shelf",
    status = "draft",
    ownerId = user$id,
    surveyAccessMode = "invited_only",
    protocolConfig = protocol_config,
    createdAt = now,
    updatedAt = now
  ))
  sid <- doc_id(study)
  person_id <- user$personId
  if (is.null(person_id) || !nzchar(as.character(person_id))) {
    person <- find_person_by_email(user$email)
    if (is.null(person)) {
      person <- mongo_insert("people", list(
        orgId = doc_id(org),
        personType = if (user$platformRole == "facilitator") "facilitator" else "researcher",
        name = user$displayName,
        email = user$email,
        expertise = list(),
        tags = list("facilitator"),
        inviteStatus = "active",
        userId = user$id,
        isActive = TRUE,
        createdAt = now,
        updatedAt = now
      ))
    }
    person_id <- doc_id(person)
    mongo_update("users", q_id(user$id), list(personId = person_id))
  }
  mongo_insert("study_access", list(
    studyId = sid,
    orgId = doc_id(org),
    personId = person_id,
    userId = user$id,
    accessRole = if (user$platformRole == "researcher") "researcher" else "facilitator",
    status = "active",
    notes = "Study owner / facilitator",
    createdAt = now,
    updatedAt = now
  ))
  sort_order <- 0L
  if ("chips_and_bins" %in% methods) {
    mongo_insert("questions", list(
      studyId = sid,
      code = "Q1_CHIPS",
      title = paste("Chips-N-Bins —", quantity %||% title),
      prompt = paste(
        "Allocate all chips across bins for:",
        quantity %||% "the uncertain quantity"
      ),
      variableType = "proportion",
      elicitationMethod = "chips_and_bins",
      unit = "probability",
      lowerBound = 0,
      upperBound = 1,
      judgmentSchema = list(type = "chips_and_bins", binCount = 10L, totalChips = 20L),
      rationaleRequired = TRUE,
      sortOrder = sort_order,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
    sort_order <- sort_order + 1L
  }
  if ("quantile" %in% methods) {
    mongo_insert("questions", list(
      studyId = sid,
      code = if (sort_order > 0) "Q2_QUANTILES" else "Q1_QUANTILES",
      title = paste("Low-High-Best —", quantity %||% title),
      prompt = paste("Provide P10, P50, and P90 for:", quantity %||% "the uncertain quantity"),
      variableType = "proportion",
      elicitationMethod = "quantile",
      unit = "probability",
      lowerBound = 0,
      upperBound = 1,
      judgmentSchema = list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE),
      rationaleRequired = TRUE,
      sortOrder = sort_order,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
  }
  find_study(sid)
}

list_studies_for_user <- function(user) {
  if (user$platformRole %in% c("admin", "super_admin")) {
    return(mongo_all("studies", "{}", sort = '{"updatedAt": -1}'))
  }
  access <- mongo_all("study_access", sprintf(
    '{"status": "active", "$or": [{"userId": %s}, {"personId": %s}]}',
    json_escape(user$id),
    json_escape(user$personId %||% "")
  ))
  ids <- unique(vapply(access, function(a) as.character(a$studyId), character(1)))
  owned <- mongo_all("studies", q_field("ownerId", user$id))
  extra <- lapply(ids, function(id) mongo_one("studies", q_id(id)))
  all_s <- c(owned, Filter(Negate(is.null), extra))
  seen <- character()
  out <- list()
  for (s in all_s) {
    id <- doc_id(s)
    if (id %in% seen) next
    seen <- c(seen, id)
    out[[length(out) + 1]] <- s
  }
  out
}

study_questions <- function(study_id) {
  qs <- mongo_all("questions", sprintf('{"studyId": %s, "isActive": true}', json_escape(study_id)))
  ords <- vapply(qs, function(q) as.integer(q$sortOrder %||% 0), integer(1))
  qs[order(ords)]
}

invite_expert <- function(study, email, name = NULL, user) {
  email <- tolower(trimws(email))
  org <- ensure_org()
  now <- iso_now()
  person <- find_person_by_email(email)
  if (is.null(person)) {
    person <- mongo_insert("people", list(
      orgId = doc_id(org),
      personType = "expert",
      name = name %||% email,
      email = email,
      expertise = list(),
      tags = list(),
      inviteStatus = "invited",
      isActive = TRUE,
      createdAt = now,
      updatedAt = now
    ))
  }
  exists <- mongo_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "expert", "status": "active"}',
    json_escape(doc_id(study)),
    json_escape(doc_id(person))
  ))
  if (is.null(exists)) {
    mongo_insert("study_access", list(
      studyId = doc_id(study),
      orgId = doc_id(org),
      personId = doc_id(person),
      accessRole = "expert",
      status = "active",
      remindersSent = 0,
      remindersTotal = 3,
      notes = "Invited by facilitator",
      grantedBy = user$id,
      createdAt = now,
      updatedAt = now
    ))
  }
  person
}

study_experts <- function(study) {
  sid <- doc_id(study)
  qs <- study_questions(sid)
  nq <- length(qs)
  access <- mongo_all("study_access", sprintf(
    '{"studyId": %s, "accessRole": "expert", "status": "active"}',
    json_escape(sid)
  ))
  lapply(access, function(a) {
    person <- mongo_one("people", q_id(a$personId))
    answered <- 0L
    if (!is.null(person) && nq > 0) {
      answered <- sum(vapply(qs, function(q) {
        mongo_count("judgments", sprintf(
          '{"studyId": %s, "questionId": %s, "expertId": %s, "isCurrent": true, "isConsensus": false}',
          json_escape(sid), json_escape(doc_id(q)), json_escape(doc_id(person))
        )) > 0
      }, logical(1)))
    }
    status <- if (answered <= 0) "not_started" else if (answered >= nq) "submitted" else "ongoing"
    list(
      personId = a$personId,
      name = person$name %||% person$email %||% "Expert",
      email = person$email %||% "",
      answeredCount = answered,
      totalQuestions = nq,
      status = status,
      updatedAt = a$updatedAt
    )
  })
}

save_judgment <- function(study, question, expert_person_id, expert_name, payload, round_number = 1L) {
  now <- iso_now()
  sid <- doc_id(study)
  qid <- doc_id(question)
  mongo_col("judgments")$update(
    sprintf(
      '{"studyId": %s, "questionId": %s, "expertId": %s, "roundNumber": %s, "isConsensus": false}',
      json_escape(sid), json_escape(qid), json_escape(expert_person_id), as.integer(round_number)
    ),
    '{"$set": {"isCurrent": false}}',
    multiple = TRUE
  )
  mongo_insert("judgments", list(
    studyId = sid,
    questionId = qid,
    expertId = expert_person_id,
    expertName = expert_name,
    roundNumber = as.integer(round_number),
    isConsensus = FALSE,
    version = 1L,
    isCurrent = TRUE,
    payload = payload,
    rationale = payload$rationale %||% NULL,
    elicitedAt = now,
    createdAt = now
  ))
}

current_judgments <- function(study_id, question_id, round_number = 1L) {
  mongo_all("judgments", sprintf(
    '{"studyId": %s, "questionId": %s, "roundNumber": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(study_id), json_escape(question_id), as.integer(round_number)
  ))
}

advance_round <- function(study) {
  r <- current_round(study) + 1L
  cfg <- study$protocolConfig %||% list()
  cfg$currentRound <- r
  mongo_update("studies", q_id(doc_id(study)), list(
    protocolConfig = cfg,
    status = "workshop",
    updatedAt = iso_now()
  ))
  find_study(doc_id(study))
}

run_shelf_for_question <- function(study, question, round_number = 1L, family = "best") {
  js <- current_judgments(doc_id(study), doc_id(question), round_number)
  experts <- list()
  for (j in js) {
    qmap <- quantiles_from_payload(j$payload)
    if (is.null(qmap)) next
    experts[[length(experts) + 1]] <- list(
      name = j$expertName %||% j$expertId %||% "Expert",
      quantiles = qmap
    )
  }
  lo <- as.numeric(question$lowerBound %||% 0)
  hi <- as.numeric(question$upperBound %||% 1)
  fit <- run_shelf_fit(experts, lo = lo, hi = hi, preferred = family)
  conclusion <- build_conclusion(fit, question$title %||% question$code, length(experts))
  agg <- mongo_insert("aggregations", list(
    studyId = doc_id(study),
    questionId = doc_id(question),
    roundNumber = as.integer(round_number),
    engine = fit$engine,
    shelfVersion = fit$shelfVersion,
    nExperts = length(experts),
    result = fit,
    conclusion = conclusion,
    createdAt = iso_now()
  ))
  list(fit = fit, conclusion = conclusion, aggregationId = doc_id(agg), nExperts = length(experts))
}

survey_url <- function(study) {
  sprintf("%s/?study=%s", ee_survey_public_url(), utils::URLencode(study$slug %||% doc_id(study), reserved = TRUE))
}
