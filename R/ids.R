new_oid <- function() {
  # Must not be 24 hex chars: MongoDB would coerce that to ObjectId and
  # mongolite iterate() then drops `_id`.
  hex <- c(0:9, letters[1:6])
  paste0("ee", paste(sample(hex, 24, replace = TRUE), collapse = ""))
}

doc_id <- function(doc) {
  if (is.null(doc)) return(NULL)
  id <- doc[["_id"]]
  if (is.null(id)) return(NULL)
  if (is.list(id)) {
    if (!is.null(id[["$oid"]])) return(as.character(id[["$oid"]])[[1]])
    if (!is.null(id$oid)) return(as.character(id$oid)[[1]])
    id <- unlist(id, use.names = FALSE)
  }
  id <- as.character(id)
  if (!length(id) || !nzchar(id[[1]])) return(NULL)
  id[[1]]
}

iso_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

slugify <- function(title) {
  s <- tolower(trimws(title %||% "study"))
  s <- gsub("[^a-z0-9]+", "-", s)
  s <- gsub("^-|-$", "", s)
  s <- substr(s, 1, 60)
  if (!nzchar(s)) "study" else s
}

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.atomic(a) && length(a) == 1) {
    if (is.na(a)) return(b)
    if (is.character(a) && !nzchar(a)) return(b)
  }
  a
}

json_escape <- function(x) {
  x <- as.character(x %||% "")
  if (!length(x)) x <- ""
  jsonlite::toJSON(x[[1]], auto_unbox = TRUE)
}

query_eq <- function(field, value) {
  sprintf("{%s: %s}", jsonlite::toJSON(field, auto_unbox = TRUE), json_escape(value))
}
