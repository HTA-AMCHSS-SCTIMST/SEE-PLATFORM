# MongoDB access via mongolite. Nested documents: always iterate(), never $find()
# flattening to data.frames.

.ee_mongo <- new.env(parent = emptyenv())

mongo_col <- function(name) {
  key <- paste0("c_", name)
  if (is.null(.ee_mongo[[key]])) {
    .ee_mongo[[key]] <- mongolite::mongo(
      collection = name,
      db = ee_mongodb_db(),
      url = ee_mongodb_uri()
    )
  }
  .ee_mongo[[key]]
}

mongo_ping <- function() {
  tryCatch(
    {
      mongo_col("organizations")$count("{}")
      list(ok = TRUE, message = sprintf("Connected to %s", ee_mongodb_db()))
    },
    error = function(e) {
      if (exists("ee_log_error", mode = "function")) ee_log_error(e, "mongo_ping")
      list(ok = FALSE, message = conditionMessage(e))
    }
  )
}

mongo_reset_cache <- function() {
  rm(list = ls(.ee_mongo), envir = .ee_mongo)
}

mongo_all <- function(name, query = "{}", sort = NULL) {
  # mongolite default fields '{"_id":0}' drops ids — always include them.
  it <- if (is.null(sort)) {
    mongo_col(name)$iterate(query, fields = "{}")
  } else {
    mongo_col(name)$iterate(query, fields = "{}", sort = sort)
  }
  docs <- list()
  repeat {
    d <- it$one()
    if (is.null(d)) break
    docs[[length(docs) + 1]] <- d
  }
  docs
}

mongo_one <- function(name, query) {
  mongo_col(name)$iterate(query, fields = "{}")$one()
}

mongo_insert <- function(name, doc) {
  if (is.null(doc[["_id"]])) doc[["_id"]] <- new_oid()
  # mongolite$insert() on a named R list is treated like a data.frame, so nested
  # fields make `_id` an array. Always send one JSON document.
  payload <- jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601")
  mongo_col(name)$insert(payload)
  doc
}

mongo_update <- function(name, query, set_fields) {
  body <- list("$set" = set_fields)
  mongo_col(name)$update(query, jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
}

mongo_count <- function(name, query = "{}") {
  mongo_col(name)$count(query)
}

ensure_indexes <- function() {
  try({
    mongo_col("studies")$index('{"slug": 1}')
    mongo_col("users")$index('{"email": 1}')
    mongo_col("people")$index('{"email": 1}')
    mongo_col("questions")$index('{"studyId": 1}')
    mongo_col("judgments")$index('{"studyId": 1, "questionId": 1, "expertId": 1}')
    mongo_col("study_access")$index('{"studyId": 1, "personId": 1}')
  }, silent = TRUE)
  invisible(TRUE)
}

q_id <- function(id) sprintf('{"_id": %s}', json_escape(id))
q_field <- function(field, value) {
  sprintf("{%s: %s}", jsonlite::toJSON(field, auto_unbox = TRUE), json_escape(value))
}

find_study <- function(id_or_slug) {
  if (!nzchar(id_or_slug %||% "")) return(NULL)
  mongo_one("studies", q_id(id_or_slug)) %||%
    mongo_one("studies", q_field("slug", id_or_slug))
}
