# Console + file logging for the Shiny process (the "server log").

ee_log_path <- function() {
  base <- if (exists("ee_on_posit_connect", mode = "function") && isTRUE(ee_on_posit_connect())) {
    file.path(tempdir(), "elicitation-logs")
  } else {
    file.path(getwd(), "logs")
  }
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)
  file.path(base, "shiny.log")
}

ee_log <- function(level, msg, where = NULL) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  loc <- if (is.null(where) || !nzchar(where)) "" else paste0(" [", where, "]")
  line <- sprintf("%s %-5s%s %s", ts, toupper(as.character(level)), loc, paste(msg, collapse = " "))
  # stderr so it shows in the Rscript / RStudio console that launched the app
  message(line)
  try(cat(line, "\n", file = ee_log_path(), append = TRUE), silent = TRUE)
  invisible(line)
}

ee_log_error <- function(e, where = "app") {
  msg <- conditionMessage(e)
  cls <- paste(class(e), collapse = "/")
  ee_log("error", sprintf("%s (%s)", msg, cls), where = where)
  msg
}

ee_handle <- function(rv, e, where) {
  rv$err <- ee_log_error(e, where)
  invisible(NULL)
}

ee_setup_shiny_logging <- function() {
  options(shiny.fullstacktrace = TRUE, warn = 1)
  options(shiny.error = function() {
    ee_log("error", geterrmessage(), where = "shiny.uncaught")
  })
  ee_log("info", sprintf("logging to console and %s", ee_log_path()), where = "startup")
}
