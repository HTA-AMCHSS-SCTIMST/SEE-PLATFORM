ee_header <- function(user = NULL, extra = NULL) {
  htmltools::tags$header(
    class = "site-header",
    htmltools::div(
      class = "header-inner",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST", class = "brand-logo"),
      htmltools::div(
        class = "brand-text",
        htmltools::tags$h1("EXPERT ELICITATION & STATISTICAL PLATFORM"),
        htmltools::tags$p(
          class = "institute",
          "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"
        ),
        htmltools::tags$p(class = "team", "Achutha Menon Centre for Health Science Studies (AMCHSS)")
      ),
      if (!is.null(user)) {
        htmltools::div(
          class = "header-actions",
          htmltools::div(
            class = "user-chip",
            htmltools::tags$strong(user$displayName),
            htmltools::tags$span(paste(role_label(user$platformRole), "·", user$email))
          ),
          extra
        )
      }
    )
  )
}

status_pill <- function(status) {
  lab <- switch(status,
    draft = "Draft",
    recruiting = "Published",
    eliciting = "Published",
    workshop = "Workshop",
    submitted = "Submitted",
    ongoing = "Ongoing",
    not_started = "Not started",
    status
  )
  htmltools::span(class = paste("pill", status), lab)
}

notice <- function(msg, kind = "info") {
  if (!nzchar(msg %||% "")) return(NULL)
  htmltools::div(class = paste("notice", kind), msg)
}
