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

# Experts sometimes enter 0–100 (percent) while the question is bounded 0–1.
shelf_maybe_percent_to_unit <- function(vals, lo, hi) {
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  mx <- max(vals, na.rm = TRUE)
  if (is.finite(hi) && hi <= 1.0001 && is.finite(mx) && mx > hi + 1e-9 && mx <= 100) {
    return(vals / 100)
  }
  vals
}

shelf_validate_vals <- function(vals, lo, hi, names_e = NULL) {
  if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) {
    stop("Invalid plausible limits: the lower limit must be less than the upper limit.")
  }
  for (j in seq_len(ncol(vals))) {
    v <- as.numeric(vals[, j])
    label <- if (!is.null(names_e) && nzchar(names_e[[j]])) names_e[[j]] else paste0("Expert ", j)
    if (any(!is.finite(v))) {
      stop(sprintf("Missing or non-numeric quantile values for %s.", label))
    }
    if (any(v <= lo) || any(v >= hi)) {
      stop(sprintf(
        "Quantile values for %s must be strictly between %.6g and %.6g.",
        label, lo, hi
      ))
    }
    if (any(diff(v) <= 0)) {
      stop(sprintf("Quantiles for %s must be strictly increasing.", label))
    }
  }
  invisible(TRUE)
}

# SHELF::fitdist requires values strictly inside (lower, upper) and increasing.
shelf_interior_vals <- function(vals, lo, hi) {
  span <- max(as.numeric(hi) - as.numeric(lo), 1e-9)
  n <- nrow(vals)
  eps <- span * 1e-4
  lo_i <- as.numeric(lo) + eps
  hi_i <- as.numeric(hi) - eps
  if (lo_i >= hi_i) {
    lo_i <- as.numeric(lo) + span * 1e-6
    hi_i <- as.numeric(hi) - span * 1e-6
  }
  min_step <- min(eps, (hi_i - lo_i) / max(n + 1, 2))
  out <- vals
  for (j in seq_len(ncol(vals))) {
    v <- as.numeric(vals[, j])
    v[!is.finite(v)] <- (lo_i + hi_i) / 2
    v <- pmin(pmax(v, lo_i), hi_i)
    for (i in seq_len(n)) {
      floor_i <- lo_i + (i - 1) * min_step
      ceil_i <- hi_i - (n - i) * min_step
      if (i > 1) floor_i <- max(floor_i, v[i - 1] + min_step)
      v[i] <- min(max(v[i], floor_i), ceil_i)
    }
    out[, j] <- v
  }
  out
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
  colnames(vals) <- make.unique(as.character(names_e), sep = " ")
  names_e <- colnames(vals)
  vals <- shelf_maybe_percent_to_unit(vals, lo, hi)
  shelf_validate_vals(vals, lo, hi, names_e)
  vals <- shelf_interior_vals(vals, lo, hi)
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
    selected <- shelf_family_id(best_raw)
    curves <- tryCatch(
      shelf_pdf_grid(fit, j, selected, lo, hi),
      error = function(e) shelf_pdf_grid(fit, j, "beta", lo, hi)
    )
    list(
      name = names_e[j],
      id = ex$id %||% names_e[j],
      rationale = ex$rationale %||% "",
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
  pool_mat <- shelf_interior_vals(pool_mat, lo, hi)
  pool_fit <- SHELF::fitdist(
    vals = pool_mat, probs = probs, lower = lo, upper = hi, expertnames = "pool"
  )
  pool_best <- best_fit_label(pool_fit, 1)
  pool_id <- if (preferred_id %in% c("best", "")) shelf_family_id(pool_best) else preferred_id
  pool_curves <- tryCatch(
    shelf_pdf_grid(pool_fit, 1, pool_id, lo, hi),
    error = function(e) shelf_pdf_grid(pool_fit, 1, "beta", lo, hi)
  )
  d_arg <- if (preferred_id %in% c("best", "")) "best" else preferred_id
  lp_dens <- tryCatch(
    SHELF::linearPoolDensity(fit, xl = lo, xu = hi, d = d_arg, lpw = 1, nx = 81),
    error = function(e) NULL
  )
  lp_q <- tryCatch(
    as.numeric(SHELF::qlinearpool(fit, q = probs, d = d_arg, w = 1)),
    error = function(e) apply(vals, 1, mean)
  )
  linear_pool <- list(
    name = "linearPool Fit",
    family = d_arg,
    bestFitting = if (preferred_id %in% c("best", "")) pool_best else preferred_id,
    shelfBestFitting = pool_best,
    quantiles = setNames(as.list(lp_q), as.character(probs)),
    x = if (is.null(lp_dens)) pool_curves$x else as.numeric(lp_dens$x),
    pdf = if (is.null(lp_dens)) pool_curves$pdf else as.numeric(lp_dens$f)
  )
  list(
    engine = "shelf_native",
    shelfVersion = as.character(utils::packageVersion("SHELF")),
    lower = lo,
    upper = hi,
    probs = probs,
    experts = expert_outs,
    pool = list(
      name = "Median pool",
      family = pool_curves$family,
      bestFitting = if (preferred_id %in% c("best", "")) pool_best else preferred_id,
      shelfBestFitting = pool_best,
      quantiles = setNames(as.list(pool_vals), as.character(probs)),
      x = pool_curves$x,
      pdf = pool_curves$pdf
    ),
    linearPool = linear_pool
  )
}

quantile_triple <- function(qlist, probs = c(0.1, 0.5, 0.9)) {
  if (is.null(qlist) || !length(qlist)) return(c(NA_real_, NA_real_, NA_real_))
  nms <- names(qlist)
  if (is.null(nms)) nms <- as.character(probs)
  v <- as.numeric(unlist(qlist, use.names = FALSE))
  names(v) <- nms
  c(
    v[["0.1"]] %||% v[["0.25"]] %||% v[[1]],
    v[["0.5"]] %||% v[[min(2, length(v))]],
    v[["0.9"]] %||% v[["0.75"]] %||% v[[length(v)]]
  )
}

build_conclusion <- function(fit_result, question_title, n) {
  med <- quantile_triple(fit_result$pool$quantiles, fit_result$probs)
  lop <- quantile_triple(fit_result$linearPool$quantiles, fit_result$probs)
  sprintf(
    "SHELF group summary for “%s” (%d expert%s). Median pool = %.2f / %.2f / %.2f. linearPool Fit (equal weights) = %.2f / %.2f / %.2f. Use as workshop feedback; consensus may be adjusted after discussion.",
    question_title,
    n,
    if (n == 1) "" else "s",
    med[[1]], med[[2]], med[[3]],
    lop[[1]], lop[[2]], lop[[3]]
  )
}

shelf_plot_df <- function(fit_result, pool_view = "both") {
  rows <- list()
  add_series <- function(name, role, x, y) {
    if (is.null(x) || is.null(y)) return()
    rows[[length(rows) + 1]] <<- data.frame(
      name = name, role = role, x = as.numeric(x), pdf = as.numeric(y),
      stringsAsFactors = FALSE
    )
  }
  for (ex in fit_result$experts) add_series(ex$name, "expert", ex$x, ex$pdf)
  if (pool_view %in% c("both", "median") && !is.null(fit_result$pool)) {
    add_series(fit_result$pool$name, "pool", fit_result$pool$x, fit_result$pool$pdf)
  }
  if (pool_view %in% c("both", "linear") && !is.null(fit_result$linearPool)) {
    add_series(fit_result$linearPool$name, "pool", fit_result$linearPool$x, fit_result$linearPool$pdf)
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

shelf_params_table <- function(fit_result) {
  rows <- list()
  add <- function(name, role, qlist, family, extra = "") {
    trip <- quantile_triple(qlist, fit_result$probs)
    rows[[length(rows) + 1]] <<- data.frame(
      series = name,
      role = role,
      q_low = trip[[1]],
      q_mid = trip[[2]],
      q_high = trip[[3]],
      family = family %||% "",
      note = extra,
      stringsAsFactors = FALSE
    )
  }
  for (ex in fit_result$experts) {
    add(ex$name, "expert", ex$quantiles, ex$family, ex$bestFitting %||% "")
  }
  if (!is.null(fit_result$pool)) {
    add(
      fit_result$pool$name, "median_pool", fit_result$pool$quantiles,
      fit_result$pool$family, fit_result$pool$bestFitting %||% ""
    )
  }
  if (!is.null(fit_result$linearPool)) {
    add(
      fit_result$linearPool$name, "linear_pool", fit_result$linearPool$quantiles,
      fit_result$linearPool$family, fit_result$linearPool$bestFitting %||% ""
    )
  }
  do.call(rbind, rows)
}
