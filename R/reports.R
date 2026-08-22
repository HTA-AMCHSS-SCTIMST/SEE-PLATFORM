audit_params_df <- function(shelf_result) {
  if (is.null(shelf_result)) return(NULL)
  if (!is.null(shelf_result$params) && is.data.frame(shelf_result$params)) return(shelf_result$params)
  if (!is.null(shelf_result$fit)) return(shelf_params_table(shelf_result$fit))
  NULL
}

audit_judgments_df <- function(study, question, round_number, anonymize = TRUE) {
  js <- current_judgments(doc_id(study), doc_id(question), round_number)
  mapping <- blind_labels_for_ids(vapply(js, function(j) as.character(j$expertId %||% ""), character(1)))
  rows <- lapply(js, function(j) {
    q <- quantiles_from_payload(j$payload)
    trip <- if (is.null(q)) c(NA, NA, NA) else quantile_triple(q)
    lab <- if (anonymize) blind_label(j$expertId, mapping) else (j$expertName %||% j$expertId)
    data.frame(
      series = lab,
      q_low = trip[[1]],
      q_mid = trip[[2]],
      q_high = trip[[3]],
      rationale = as.character(j$rationale %||% j$payload$rationale %||% ""),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

write_audit_csv <- function(path, shelf_result, study = NULL, question = NULL, round_number = 1L) {
  df <- audit_params_df(shelf_result)
  if (is.null(df)) stop("No SHELF results to export. Run SHELF first.")
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}

write_audit_pdf <- function(path, shelf_result, study, question, round_number = 1L,
                            comments = list(), pool_view = "both") {
  if (is.null(shelf_result) || is.null(shelf_result$fit)) {
    stop("No SHELF results to export. Run SHELF first.")
  }
  fit <- shelf_result$fit
  df <- shelf_plot_df(fit, pool_view)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(
      title = question$title %||% question$code,
      subtitle = paste(study$title %||% "", "· round", round_number),
      x = "Value", y = "Density", colour = NULL, linetype = NULL
    )
  params <- audit_params_df(shelf_result)
  jdf <- tryCatch(
    audit_judgments_df(study, question, round_number, anonymize = isTRUE(fit$anonymized)),
    error = function(e) NULL
  )
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  graphics::plot.new()
  graphics::par(mar = c(1, 1, 2, 1))
  lines_txt <- c(
    paste("Audit report —", iso_now()),
    paste("Study:", study$title %||% ""),
    paste("Question:", question$title %||% ""),
    paste("Round:", round_number),
    "",
    shelf_result$conclusion %||% "",
    "",
    "Fitted parameters (q_low / q_mid / q_high):"
  )
  if (!is.null(params)) {
    for (i in seq_len(nrow(params))) {
      lines_txt <- c(lines_txt, sprintf(
        "  %s [%s]: %.3f / %.3f / %.3f  %s",
        params$series[[i]], params$role[[i]],
        params$q_low[[i]], params$q_mid[[i]], params$q_high[[i]],
        params$family[[i]]
      ))
    }
  }
  if (!is.null(jdf)) {
    lines_txt <- c(lines_txt, "", "Rationales:")
    for (i in seq_len(nrow(jdf))) {
      lines_txt <- c(lines_txt, sprintf("  %s: %s", jdf$series[[i]], jdf$rationale[[i]]))
    }
  }
  if (length(comments)) {
    lines_txt <- c(lines_txt, "", "Blinded peer comments:")
    cmap <- blind_labels_for_ids(vapply(comments, function(c) as.character(c$authorPersonId %||% ""), character(1)))
    for (cmt in comments) {
      lines_txt <- c(lines_txt, sprintf("  %s: %s", blind_label(cmt$authorPersonId, cmap), cmt$body))
    }
  }
  graphics::title(main = "Locked SHELF audit record")
  graphics::text(0.02, 0.98, paste(lines_txt, collapse = "\n"), adj = c(0, 1), cex = 0.7, family = "sans")
  invisible(path)
}
