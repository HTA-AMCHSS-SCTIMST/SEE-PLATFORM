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
                         methods = c("chips_and_bins", "quantile"),
                         variable_type = "proportion", unit = "probability",
                         lower = 0, upper = 1, precision = 2,
                         preferred_distribution = "best") {
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  precision <- as.integer(precision)
  if (!is.finite(lower) || !is.finite(upper) || !(lower < upper)) {
    stop("Lower plausible bound must be less than the upper plausible bound.")
  }
  if (!variable_type %in% c("proportion", "continuous", "count")) {
    stop("Unsupported variable type.")
  }
  if (!nzchar(trimws(unit %||% ""))) stop("A unit is required.")
  if (!is.finite(precision) || precision < 0 || precision > 6) {
    stop("Decimal places must be between 0 and 6.")
  }
  if (!identical(variable_type, "proportion")) methods <- setdiff(methods, "chips_and_bins")
  if (!length(methods)) methods <- "quantile"
  org <- ensure_org()
  now <- iso_now()
  slug <- unique_slug(slugify(title))
  protocol_config <- list(
    methods = as.list(methods),
    currentRound = 1L,
    rounds = 3L,
    anonymizeFeedback = TRUE,
    quantileLevels = list(0.1, 0.5, 0.9),
    variableType = variable_type,
    unit = unit,
    lowerBound = lower,
    upperBound = upper,
    precision = precision,
    preferredDistribution = preferred_distribution,
    surveyWelcome = list(
      conductedBy = "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST, Trivandrum",
      why = description %||% "To capture structured expert judgment for decision support.",
      reason = "SHELF expert elicitation for HTA / clinical decision support.",
      instructions = "Complete chips-and-bins and/or P10/P50/P90 tasks, then submit.",
      quantityOfInterest = quantity,
      variableType = variable_type,
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
      precision = precision,
      preferredDistribution = preferred_distribution,
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
      variableType = variable_type,
      elicitationMethod = "chips_and_bins",
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
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
      variableType = variable_type,
      elicitationMethod = "quantile",
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
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
  if (is_admin(user)) {
    return(mongo_all("studies", "{}", sort = '{"updatedAt": -1}'))
  }
  if (is_expert_role(user)) return(list())
  access <- mongo_all("study_access", sprintf(
    '{"status": "active", "$or": [{"userId": %s}, {"personId": %s}]}',
    json_escape(user$id),
    json_escape(user$personId %||% "")
  ))
  ids <- unique(vapply(access, function(a) as.character(a$studyId), character(1)))
  owned <- if (is_facilitator(user)) mongo_all("studies", q_field("ownerId", user$id)) else list()
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
  if (is_viewer(user)) {
    out <- Filter(study_is_completed, out)
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
  if (identical(study$status %||% "", "draft")) {
    mongo_update("studies", q_id(doc_id(study)), list(status = "recruiting", updatedAt = now))
    study <- find_study(doc_id(study))
  }
  issue_invite_token(study, person)
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
    tok <- if (!is.null(person)) issue_invite_token(study, person) else NULL
    list(
      personId = a$personId,
      name = person$name %||% person$email %||% "Expert",
      email = person$email %||% "",
      answeredCount = answered,
      totalQuestions = nq,
      status = status,
      surveyUrl = if (!is.null(person)) expert_survey_url(study, person, tok) else "",
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

run_shelf_for_question <- function(study, question, round_number = 1L, family = "best",
                                   anonymize = NULL) {
  js <- current_judgments(doc_id(study), doc_id(question), round_number)
  experts <- list()
  ids <- character()
  for (j in js) {
    qmap <- quantiles_from_payload(j$payload)
    if (is.null(qmap)) next
    eid <- as.character(j$expertId %||% j$expertName %||% "expert")
    ids <- c(ids, eid)
    experts[[length(experts) + 1]] <- list(
      id = eid,
      name = j$expertName %||% eid,
      rationale = j$rationale %||% j$payload$rationale %||% "",
      quantiles = qmap
    )
  }
  if (!length(experts)) stop("No current judgments with quantiles for this question.")
  lo <- as.numeric(question$lowerBound %||% 0)
  hi <- as.numeric(question$upperBound %||% 1)
  bds <- lapply(js, function(j) {
    list(lo = j$payload$lowerBound, hi = j$payload$upperBound)
  })
  if (length(bds)) {
    los <- suppressWarnings(as.numeric(vapply(bds, function(b) b$lo %||% NA_real_, numeric(1))))
    his <- suppressWarnings(as.numeric(vapply(bds, function(b) b$hi %||% NA_real_, numeric(1))))
    if (any(is.finite(los))) lo <- min(c(lo, los[is.finite(los)]))
    if (any(is.finite(his))) hi <- max(c(hi, his[is.finite(his)]))
  }
  probs <- payload_probs(js[[1]]$payload)
  do_anon <- if (is.null(anonymize)) isTRUE((study$protocolConfig %||% list())$anonymizeFeedback) else isTRUE(anonymize)
  mapping <- blind_labels_for_ids(ids)
  if (do_anon) {
    for (i in seq_along(experts)) {
      experts[[i]]$realName <- experts[[i]]$name
      experts[[i]]$name <- blind_label(experts[[i]]$id, mapping)
    }
  }
  fit <- run_shelf_fit(experts, lo = lo, hi = hi, probs = probs, preferred = family)
  fit$anonymized <- do_anon
  fit$labelMap <- as.list(mapping)
  fit$questionId <- doc_id(question)
  fit$roundNumber <- as.integer(round_number)
  conclusion <- build_conclusion(fit, question$title %||% question$code, length(experts))
  params <- shelf_params_table(fit)
  agg <- mongo_insert("aggregations", list(
    studyId = doc_id(study),
    questionId = doc_id(question),
    roundNumber = as.integer(round_number),
    engine = fit$engine,
    shelfVersion = fit$shelfVersion,
    nExperts = length(experts),
    result = fit,
    params = params,
    conclusion = conclusion,
    createdAt = iso_now()
  ))
  list(
    fit = fit, conclusion = conclusion, aggregationId = doc_id(agg),
    nExperts = length(experts), params = params, studyId = doc_id(study),
    questionId = doc_id(question), roundNumber = as.integer(round_number)
  )
}

latest_aggregation <- function(study_id, question_id, round_number = NULL) {
  q <- sprintf('{"studyId": %s, "questionId": %s}', json_escape(study_id), json_escape(question_id))
  if (!is.null(round_number)) {
    q <- sprintf(
      '{"studyId": %s, "questionId": %s, "roundNumber": %s}',
      json_escape(study_id), json_escape(question_id), as.integer(round_number)
    )
  }
  docs <- mongo_all("aggregations", q, sort = '{"createdAt": -1}')
  if (!length(docs)) return(NULL)
  docs[[1]]
}

aggregation_as_shelf_result <- function(agg) {
  if (is.null(agg)) return(NULL)
  params <- agg$params
  if (!is.null(params) && !is.data.frame(params)) {
    params <- tryCatch(as.data.frame(params, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(params) && !is.null(agg$result)) {
    params <- tryCatch(shelf_params_table(agg$result), error = function(e) NULL)
  }
  list(
    fit = agg$result,
    conclusion = agg$conclusion,
    aggregationId = doc_id(agg),
    nExperts = agg$nExperts %||% length(agg$result$experts),
    params = params,
    studyId = agg$studyId,
    questionId = agg$questionId,
    roundNumber = agg$roundNumber
  )
}

survey_url <- function(study) {
  sprintf("%s/?study=%s", ee_survey_public_url(), utils::URLencode(study$slug %||% doc_id(study), reserved = TRUE))
}
