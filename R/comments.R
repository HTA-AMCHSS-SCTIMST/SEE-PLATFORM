save_peer_comment <- function(study, question, person_id, body, round_number) {
  body <- trimws(body %||% "")
  if (!nzchar(body)) stop("Enter a comment before posting.")
  mongo_insert("peer_comments", list(
    studyId = doc_id(study),
    questionId = doc_id(question),
    authorPersonId = person_id,
    roundNumber = as.integer(round_number),
    body = body,
    createdAt = iso_now()
  ))
}

list_peer_comments <- function(study_id, question_id, round_number) {
  mongo_all("peer_comments", sprintf(
    '{"studyId": %s, "questionId": %s, "roundNumber": %s}',
    json_escape(study_id),
    json_escape(question_id),
    as.integer(round_number)
  ), sort = '{"createdAt": 1}')
}

blind_labels_for_ids <- function(ids) {
  ids <- unique(as.character(ids))
  ids <- ids[nzchar(ids)]
  ids <- sort(ids)
  n <- length(ids)
  labs <- if (n <= 26) paste("Expert", LETTERS[seq_len(n)]) else paste("Expert", seq_len(n))
  stats::setNames(labs, ids)
}

blind_label <- function(id, mapping) {
  key <- as.character(id %||% "")
  if (!nzchar(key) || is.null(mapping[[key]])) return("Expert")
  unname(mapping[[key]])
}
