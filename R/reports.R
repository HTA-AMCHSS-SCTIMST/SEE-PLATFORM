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

draw_parameter_table_pdf <- function(params) {
  labels <- c(
    "Expert / result", "Role", "Lower", "Median", "Upper",
    "Distribution", "Best fit"
  )
  values <- if (is.null(params) || !nrow(params)) {
    matrix("No parameter results available.", nrow = 1, ncol = length(labels))
  } else {
    cbind(
      as.character(params$series),
      as.character(params$role),
      formatC(params$q_low, format = "f", digits = 2),
      formatC(params$q_mid, format = "f", digits = 2),
      formatC(params$q_high, format = "f", digits = 2),
      as.character(params$family),
      as.character(params$note)
    )
  }
  n_rows <- nrow(values) + 1L
  widths <- c(0.25, 0.13, 0.11, 0.11, 0.11, 0.14, 0.15)
  x_edges <- c(0, cumsum(widths))
  y_edges <- seq(0, 1, length.out = n_rows + 1L)
  graphics::par(mar = c(0.4, 0.4, 2, 0.4))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  graphics::title(main = "SHELF parameter table", line = 0.5)
  for (row in seq_len(n_rows)) {
    y_bottom <- 1 - y_edges[row + 1]
    y_top <- 1 - y_edges[row]
    fill <- if (row == 1L) "#f7f0e0" else if (row %% 2L == 0L) "#fffdf8" else "#fff8ea"
    graphics::rect(0, y_bottom, 1, y_top, col = fill, border = "#d8c9aa")
    for (col in seq_along(widths)) {
      x_left <- x_edges[col]
      x_right <- x_edges[col + 1L]
      text <- if (row == 1L) labels[[col]] else values[row - 1L, col]
      graphics::text(
        (x_left + x_right) / 2,
        (y_bottom + y_top) / 2,
        text,
        cex = if (row == 1L) 0.72 else 0.68,
        font = if (row == 1L) 2 else 1,
        adj = c(0, 0.5),
        xpd = NA
      )
      graphics::segments(x_left, y_bottom, x_left, y_top, col = "#d8c9aa")
    }
  }
  graphics::box(col = "#b8892d")
}

write_audit_csv <- function(path, shelf_result, study = NULL, question = NULL, round_number = 1L) {
  df <- audit_params_df(shelf_result)
  if (is.null(df)) stop("No SHELF results to export. Run SHELF first.")
  numeric_cols <- intersect(c("q_low", "q_mid", "q_high"), names(df))
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    formatC(as.numeric(x), format = "f", digits = 2)
  })
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
  if (!is.null(params) && pool_view %in% c("median", "linear")) {
    pool_role <- if (identical(pool_view, "median")) "median_pool" else "linear_pool"
    roles <- tolower(trimws(as.character(params$role)))
    params <- params[roles == "expert" | roles == pool_role, , drop = FALSE]
  }
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  draw_parameter_table_pdf(params)
  graphics::plot.new()
  graphics::par(mar = c(1, 1, 2, 1))
  graphics::title(main = "Expert values and rationales")
  lines_txt <- character()
  if (!is.null(jdf)) {
    for (i in seq_len(nrow(jdf))) {
      lines_txt <- c(
        lines_txt,
        sprintf(
          "%s | Lower: %.4f | Median: %.4f | Upper: %.4f",
          jdf$series[[i]], jdf$q_low[[i]], jdf$q_mid[[i]], jdf$q_high[[i]]
        ),
        paste("Rationale:", jdf$rationale[[i]] %||% ""),
        ""
      )
    }
  }
  if (!length(lines_txt)) lines_txt <- "No expert judgments were available."
  if (length(comments)) {
    lines_txt <- c(lines_txt, "Blinded peer comments:")
    cmap <- blind_labels_for_ids(vapply(comments, function(c) as.character(c$authorPersonId %||% ""), character(1)))
    for (cmt in comments) {
      lines_txt <- c(lines_txt, sprintf("  %s: %s", blind_label(cmt$authorPersonId, cmap), cmt$body))
    }
  }
  graphics::text(0.02, 0.98, paste(lines_txt, collapse = "\n"), adj = c(0, 1), cex = 0.75, family = "sans")
  invisible(path)
}

write_audit_bundle <- function(path, shelf_result, study, question, round_number = 1L,
                               comments = list(), pool_view = "both") {
  if (is.null(shelf_result) || is.null(shelf_result$fit)) {
    stop("No SHELF results to export. Run SHELF first.")
  }
  bundle_dir <- tempfile("shelf-bundle-")
  dir.create(bundle_dir)
  on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)

  params <- audit_params_df(shelf_result)
  if (is.null(params)) stop("No parameter table is available.")
  judgments <- audit_judgments_df(
    study, question, round_number,
    anonymize = isTRUE(shelf_result$fit$anonymized)
  )
  metadata <- data.frame(
    field = c("study", "study_id", "question", "question_id", "round",
              "generated_at", "n_experts", "conclusion"),
    value = c(
      study$title %||% "",
      doc_id(study),
      question$title %||% question$code,
      doc_id(question),
      as.character(round_number),
      iso_now(),
      as.character(shelf_result$nExperts %||% NA_integer_),
      shelf_result$conclusion %||% ""
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(params, file.path(bundle_dir, "parameter-table.csv"), row.names = FALSE)
  if (!is.null(judgments)) {
    utils::write.csv(judgments, file.path(bundle_dir, "expert-values-and-rationales.csv"), row.names = FALSE)
  }
  utils::write.csv(metadata, file.path(bundle_dir, "study-metadata.csv"), row.names = FALSE)

  plot_data <- shelf_plot_df(shelf_result$fit, pool_view)
  if (!is.null(plot_data)) {
    plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(
        title = question$title %||% question$code,
        subtitle = paste(study$title %||% "", "· round", round_number),
        x = "Value", y = "Density", colour = "Expert / result", linetype = "Role"
      )
    grDevices::png(file.path(bundle_dir, "distribution-graph.png"), width = 1600, height = 1000, res = 150)
    print(plot)
    grDevices::dev.off()
  }

  write_audit_pdf(
    file.path(bundle_dir, "shelf-audit.pdf"),
    shelf_result, study, question, round_number, comments, pool_view
  )
  old_wd <- setwd(bundle_dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(path, list.files(bundle_dir), flags = "-j")
  invisible(path)
}

quarto_available <- function() {
  nzchar(Sys.which("quarto"))
}

render_quarto_dossier <- function(path, study, question, round_number = 1L) {
  if (!quarto_available()) {
    stop("Quarto is not installed on this server. Install Quarto and restart the Shiny app.")
  }
  root <- normalizePath(".", mustWork = TRUE, winslash = "/")
  qmd <- file.path(root, "reports", "shelf-report.qmd")
  if (!file.exists(qmd)) stop("The Quarto dossier template is missing.")

  output_dir <- tempfile("quarto-dossier-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  output_name <- sprintf("shelf-dossier-%s.html", slugify(study$title %||% "study"))
  args <- c(
    "render", qmd,
    "--to", "html",
    "--output-dir", output_dir,
    "--output", output_name,
    "-P", sprintf("study_id:%s", doc_id(study)),
    "-P", sprintf("question_id:%s", doc_id(question)),
    "-P", sprintf("round_number:%s", as.integer(round_number))
  )
  log_file <- file.path(output_dir, "quarto.log")
  status <- system2("quarto", args = args, stdout = log_file, stderr = log_file)
  rendered <- file.path(output_dir, output_name)
  if (!identical(status, 0L) || !file.exists(rendered)) {
    details <- if (file.exists(log_file)) paste(readLines(log_file, warn = FALSE), collapse = "\n") else ""
    stop("Quarto dossier rendering failed.", if (nzchar(details)) paste0("\n", details) else "")
  }
  if (!file.copy(rendered, path, overwrite = TRUE)) {
    stop("Quarto rendered the dossier but it could not be downloaded.")
  }
  invisible(path)
}
