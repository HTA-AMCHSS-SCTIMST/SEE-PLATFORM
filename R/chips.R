build_bins <- function(n, lo, hi, chips = NULL) {
  n <- as.integer(n)
  width <- (hi - lo) / n
  lapply(seq_len(n), function(i) {
    from <- lo + (i - 1) * width
    to <- if (i == n) hi else lo + i * width
    list(
      from = from,
      to = to,
      chips = if (is.null(chips)) 0L else as.integer(chips[[i]] %||% 0L)
    )
  })
}

chips_sum <- function(bins) {
  sum(vapply(bins, function(b) as.integer(b$chips %||% 0), integer(1)))
}

chips_to_quantiles <- function(bins, levels = c(0.1, 0.5, 0.9)) {
  total <- chips_sum(bins)
  if (total <= 0) return(NULL)
  probs <- vapply(bins, function(b) as.integer(b$chips) / total, numeric(1))
  out <- list()
  for (level in levels) {
    cum <- 0
    found <- NULL
    for (i in seq_along(bins)) {
      prev <- cum
      p <- probs[[i]]
      cum <- cum + p
      if (cum >= level - 1e-12) {
        span <- bins[[i]]$to - bins[[i]]$from
        frac <- if (p <= 0) 0 else (level - prev) / p
        frac <- min(max(frac, 0), 1)
        found <- bins[[i]]$from + frac * span
        break
      }
    }
    if (is.null(found)) found <- bins[[length(bins)]]$to
    out[[as.character(level)]] <- as.numeric(found)
  }
  lo <- as.numeric(bins[[1]]$from)
  hi <- as.numeric(bins[[length(bins)]]$to)
  span <- max(hi - lo, 1e-9)
  eps <- span * 1e-4
  for (nm in names(out)) {
    out[[nm]] <- min(max(out[[nm]], lo + eps), hi - eps)
  }
  out
}

chips_mean <- function(bins) {
  total <- chips_sum(bins)
  if (total <= 0) return(NA_real_)
  sum(vapply(bins, function(b) {
    mid <- (as.numeric(b$from) + as.numeric(b$to)) / 2
    as.integer(b$chips) / total * mid
  }, numeric(1)))
}

chips_payload <- function(bins, total_chips, lo, hi, rationale = "") {
  qs <- chips_to_quantiles(bins)
  list(
    method = "chips_and_bins",
    bins = lapply(bins, function(b) list(from = b$from, to = b$to, chips = as.integer(b$chips))),
    totalChips = as.integer(total_chips),
    mean = chips_mean(bins),
    quantiles = qs,
    lowerBound = lo,
    upperBound = hi,
    rationale = rationale
  )
}

quantile_payload <- function(p10, p50, p90, rationale = "", lo = NULL, hi = NULL, mode = "percentile") {
  list(
    method = "quantile",
    mode = mode,
    quantiles = list(`0.1` = as.numeric(p10), `0.5` = as.numeric(p50), `0.9` = as.numeric(p90)),
    lowerBound = lo,
    upperBound = hi,
    rationale = rationale
  )
}

quartile_payload <- function(q1, m, q3, rationale = "", lo = NULL, hi = NULL) {
  list(
    method = "quantile",
    mode = "quartile",
    quantiles = list(`0.25` = as.numeric(q1), `0.5` = as.numeric(m), `0.75` = as.numeric(q3)),
    lowerBound = lo,
    upperBound = hi,
    rationale = rationale
  )
}

quantiles_from_payload <- function(payload) {
  q <- payload$quantiles
  if (!is.null(q)) {
    if (!is.null(q[["0.1"]]) && !is.null(q[["0.5"]]) && !is.null(q[["0.9"]])) {
      return(list(`0.1` = as.numeric(q[["0.1"]]), `0.5` = as.numeric(q[["0.5"]]), `0.9` = as.numeric(q[["0.9"]])))
    }
    if (!is.null(q[["0.25"]]) && !is.null(q[["0.5"]]) && !is.null(q[["0.75"]])) {
      return(list(`0.25` = as.numeric(q[["0.25"]]), `0.5` = as.numeric(q[["0.5"]]), `0.75` = as.numeric(q[["0.75"]])))
    }
  }
  if (!is.null(payload$bins)) return(chips_to_quantiles(payload$bins))
  NULL
}

payload_probs <- function(payload) {
  q <- quantiles_from_payload(payload)
  if (is.null(q)) return(c(0.1, 0.5, 0.9))
  as.numeric(names(q))
}

validate_strict_quantiles <- function(vals, lo, hi) {
  v <- as.numeric(unlist(vals, use.names = FALSE))
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  if (any(!is.finite(c(v, lo, hi)))) stop("Enter numeric bounds and quantiles.")
  if (!(lo < v[[1]])) stop("The first quantile must be greater than the lower plausible limit L.")
  if (!(v[[length(v)]] < hi)) stop("The last quantile must be less than the upper plausible limit U.")
  if (any(diff(v) <= 0)) {
    stop("Quantiles must be strictly increasing (L < P10 < P50 < P90 < U or L < Q1 < M < Q3 < U).")
  }
  invisible(TRUE)
}

require_rationale_text <- function(text, required = TRUE) {
  if (!isTRUE(required)) return(invisible(TRUE))
  if (!nzchar(trimws(text %||% ""))) stop("A written rationale is required.")
  invisible(TRUE)
}

is_chips_question <- function(q) {
  method <- tolower(q$elicitationMethod %||% "")
  typ <- tolower((q$judgmentSchema$type %||% ""))
  method %in% c("chips_and_bins", "roulette", "chips") ||
    typ %in% c("chips_and_bins", "roulette", "chips")
}
