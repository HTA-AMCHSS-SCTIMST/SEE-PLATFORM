# Native CRAN SHELF::fitdist — same science as the Plumber sidecar in the React app.

shelf_available <- function() {
  requireNamespace("SHELF", quietly = TRUE)
}

shelf_status <- function() {
  ok <- shelf_available()
  ver <- if (ok) as.character(utils::packageVersion("SHELF")) else NA_character_
  list(
    engine = if (ok) "shelf_native" else "none",
    rAvailable = TRUE,
    shelfPackage = ok,
    shelfVersion = ver,
    message = if (ok) paste("CRAN SHELF", ver, "ready") else
      "Install SHELF: install.packages('SHELF')"
  )
}

shelf_family_id <- function(label) {
  alt <- tolower(gsub("[_ ]", ".", label %||% "beta"))
  if (grepl("best", alt)) return("best")
  if (grepl("mirror.*log.*t", alt)) return("mirror_log_t")
  if (grepl("mirror.*log", alt)) return("mirror_log_normal")
  if (grepl("mirror.*gamma", alt)) return("mirror_gamma")
  if (grepl("log.*t", alt)) return("log_t")
  if (grepl("log", alt) && grepl("normal", alt)) return("log_normal")
  if (grepl("skew", alt)) return("skew_normal")
  if (grepl("student", alt) || alt == "t") return("student_t")
  if (grepl("gamma", alt)) return("gamma")
  if (grepl("beta", alt)) return("beta")
  if (grepl("normal", alt)) return("normal")
  "beta"
}

shelf_pdf_grid <- function(fit, expert_index, family_id, lo, hi, n = 81) {
  xs <- seq(lo, hi, length.out = n)
  eps <- (hi - lo) * 1e-6
  x_eval <- pmin(pmax(xs, lo + eps), hi - eps)
  pdf <- rep(0, n)
  if (family_id == "normal") {
    mu <- fit$Normal[expert_index, "mean"]
    sig <- fit$Normal[expert_index, "sd"]
    pdf <- stats::dnorm(x_eval, mu, sig)
  } else if (family_id == "gamma") {
    sh <- fit$Gamma[expert_index, "shape"]
    ra <- fit$Gamma[expert_index, "rate"]
    pdf <- stats::dgamma(pmax(x_eval - lo, 0), shape = sh, rate = ra)
  } else if (family_id == "log_normal") {
    mu <- fit$Log.normal[expert_index, "mean.log.X"]
    sig <- fit$Log.normal[expert_index, "sd.log.X"]
    pdf <- stats::dlnorm(pmax(x_eval, 1e-12), mu, sig)
  } else {
    a <- fit$Beta[expert_index, "shape1"]
    b <- fit$Beta[expert_index, "shape2"]
    u <- (x_eval - lo) / max(hi - lo, 1e-9)
    pdf <- stats::dbeta(pmin(pmax(u, 1e-6), 1 - 1e-6), a, b) / max(hi - lo, 1e-9)
    family_id <- "beta"
  }
  list(x = as.numeric(xs), pdf = as.numeric(pdf), family = family_id)
}

best_fit_label <- function(fit, j) {
  if (is.null(fit$best.fitting)) return("Beta")
  bf <- fit$best.fitting
  if (is.data.frame(bf) || is.matrix(bf)) {
    col <- if ("best.fit" %in% colnames(bf)) "best.fit" else 1
    return(as.character(bf[j, col]))
  }
  as.character(bf[[j]])
}

run_shelf_fit <- function(experts, lo = 0, hi = 1, probs = c(0.1, 0.5, 0.9), preferred = "best") {
  if (!shelf_available()) stop("Package SHELF is not installed")
  n_e <- length(experts)
  if (n_e < 1) stop("No expert judgments to fit")
  vals <- matrix(NA_real_, nrow = length(probs), ncol = n_e)
  names_e <- character(n_e)
  for (j in seq_len(n_e)) {
    ex <- experts[[j]]
    names_e[j] <- ex$name %||% paste0("Expert", j)
    q <- ex$quantiles
    for (i in seq_along(probs)) {
      key <- as.character(probs[i])
      v <- q[[key]]
      if (is.null(v)) v <- q[[sprintf("%.1f", probs[i])]]
      if (is.null(v)) stop(sprintf("Missing quantile %s for %s", key, names_e[j]))
      vals[i, j] <- as.numeric(v)
    }
  }
  colnames(vals) <- names_e
  fit <- SHELF::fitdist(
    vals = vals,
    probs = probs,
    lower = lo,
    upper = hi,
    expertnames = names_e
  )
  preferred_id <- shelf_family_id(preferred)
  expert_outs <- lapply(seq_len(n_e), function(j) {
    best_raw <- best_fit_label(fit, j)
    selected <- if (preferred_id %in% c("best", "")) shelf_family_id(best_raw) else preferred_id
    curves <- tryCatch(
      shelf_pdf_grid(fit, j, selected, lo, hi),
      error = function(e) shelf_pdf_grid(fit, j, "beta", lo, hi)
    )
    list(
      name = names_e[j],
      bestFitting = best_raw,
      family = curves$family,
      quantiles = setNames(as.list(vals[, j]), as.character(probs)),
      x = curves$x,
      pdf = curves$pdf
    )
  })
  pool_vals <- apply(vals, 1, stats::median)
  pool_mat <- matrix(pool_vals, ncol = 1)
  colnames(pool_mat) <- "pool"
  pool_fit <- SHELF::fitdist(
    vals = pool_mat, probs = probs, lower = lo, upper = hi, expertnames = "pool"
  )
  pool_best <- best_fit_label(pool_fit, 1)
  pool_id <- if (preferred_id %in% c("best", "")) shelf_family_id(pool_best) else preferred_id
  pool_curves <- tryCatch(
    shelf_pdf_grid(pool_fit, 1, pool_id, lo, hi),
    error = function(e) shelf_pdf_grid(pool_fit, 1, "beta", lo, hi)
  )
  list(
    engine = "shelf_native",
    shelfVersion = as.character(utils::packageVersion("SHELF")),
    lower = lo,
    upper = hi,
    experts = expert_outs,
    pool = list(
      name = "Median pool",
      family = pool_curves$family,
      quantiles = setNames(as.list(pool_vals), as.character(probs)),
      x = pool_curves$x,
      pdf = pool_curves$pdf
    )
  )
}

build_conclusion <- function(fit_result, question_title, n) {
  pool_q <- fit_result$pool$quantiles
  p10 <- pool_q[["0.1"]] %||% pool_q[[1]]
  p50 <- pool_q[["0.5"]] %||% pool_q[[2]]
  p90 <- pool_q[["0.9"]] %||% pool_q[[3]]
  sprintf(
    "SHELF group summary for “%s” (%d expert%s). Median pool P10 / P50 / P90 = %.3f / %.3f / %.3f (family: %s). Use this as workshop feedback; consensus may be adjusted after discussion.",
    question_title,
    n,
    if (n == 1) "" else "s",
    as.numeric(p10),
    as.numeric(p50),
    as.numeric(p90),
    fit_result$pool$family %||% "beta"
  )
}

shelf_plot_df <- function(fit_result) {
  rows <- list()
  add_series <- function(name, role, x, y) {
    if (is.null(x) || is.null(y)) return()
    rows[[length(rows) + 1]] <<- data.frame(
      name = name, role = role, x = as.numeric(x), pdf = as.numeric(y),
      stringsAsFactors = FALSE
    )
  }
  for (ex in fit_result$experts) add_series(ex$name, "expert", ex$x, ex$pdf)
  add_series(fit_result$pool$name, "pool", fit_result$pool$x, fit_result$pool$pdf)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}
