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

quantile_payload <- function(p10, p50, p90, rationale = "") {
  list(
    method = "quantile",
    quantiles = list(`0.1` = as.numeric(p10), `0.5` = as.numeric(p50), `0.9` = as.numeric(p90)),
    rationale = rationale
  )
}

quantiles_from_payload <- function(payload) {
  q <- payload$quantiles
  if (!is.null(q) && !is.null(q[["0.1"]])) {
    return(list(`0.1` = as.numeric(q[["0.1"]]), `0.5` = as.numeric(q[["0.5"]]), `0.9` = as.numeric(q[["0.9"]])))
  }
  if (!is.null(payload$bins)) return(chips_to_quantiles(payload$bins))
  NULL
}

is_chips_question <- function(q) {
  method <- tolower(q$elicitationMethod %||% "")
  typ <- tolower((q$judgmentSchema$type %||% ""))
  method %in% c("chips_and_bins", "roulette", "chips") ||
    typ %in% c("chips_and_bins", "roulette", "chips")
}
