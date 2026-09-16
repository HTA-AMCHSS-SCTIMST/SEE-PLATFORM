ee_header <- function(user = NULL, extra = NULL) {
  htmltools::tags$header(
    class = "site-header",
    htmltools::div(
      class = "header-inner",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST", class = "brand-logo"),
      htmltools::div(
        class = "brand-text",
        htmltools::tags$h1("STRUCTURED EXPERT ELICITATION-AMCHSS"),
        htmltools::tags$p(
          class = "institute",
          "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"
        ),
        htmltools::tags$p(
          class = "team",
          htmltools::tags$a(
            href = "https://hta-amchss-sctimst.github.io/RRC/",
            target = "_blank",
            rel = "noopener noreferrer",
            aria_label = "Achutha Menon Centre for Health Science Studies website",
            "Achutha Menon Centre for Health Science Studies (AMCHSS)"
          )
        )
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
  lab <- switch(
    status %||% "",
    draft = "Draft",
    recruiting = "Published",
    eliciting = "Published",
    workshop = "Workshop",
    submitted = "Submitted",
    ongoing = "Ongoing",
    not_started = "Not started",
    completed = "Completed",
    status %||% "Unknown"
  )
  htmltools::span(class = paste("pill", status), lab)
}

notice <- function(msg, kind = "info") {
  if (!nzchar(msg %||% "")) return(NULL)
  htmltools::div(class = paste("notice", kind), msg)
}
