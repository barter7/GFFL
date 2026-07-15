# ============================================================
#  utils.R  (nflmodel)
#  Pure utility functions — no side effects, safe to source
#  independently in tests. Mirrors PropSZN's utils.R.
# ============================================================

# ── Convert American odds to implied probability ─────────────
amer_to_implied <- function(odds) {
  ifelse(is.na(odds) | !is.finite(odds), NA_real_,
         ifelse(odds > 0, 100 / (odds + 100), abs(odds) / (abs(odds) + 100)))
}

# ── Convert probability to fair American odds ────────────────
implied_to_amer <- function(p) {
  ifelse(is.na(p) | p <= 0 | p >= 1, NA_real_,
         ifelse(p >= 0.5, -round(100 * p / (1 - p)), round(100 * (1 - p) / p)))
}

# ── Remove the vig from a two-way market (proportional) ──────
#  Returns the no-vig probability of the OVER side.
devig_two_way <- function(over_odds, under_odds) {
  po <- amer_to_implied(over_odds)
  pu <- amer_to_implied(under_odds)
  ifelse(is.na(po) | is.na(pu), NA_real_, po / (po + pu))
}

#' Log5 matchup rate for probabilities (bounded 0-1)
#'
#' Bill James Log5: combines an offensive player's success rate (P),
#' the defense's allowed rate (D), and the league-average rate (L)
#' into a matchup-specific probability. Used for catch rate and
#' per-opportunity TD rates — anything that is a true probability.
#' Same formula as PropSZN's calc_matchup_k_pct.
calc_matchup_prob <- function(P, D, L) {
  numerator   <- P * D / L
  denominator <- numerator + (1 - P) * (1 - D) / (1 - L)
  out <- numerator / denominator
  ifelse(is.na(P) | is.na(D) | is.na(L) | L <= 0 | L >= 1, P, out)
}

# Ratio adjustment for continuous rate stats (yards per target,
# yards per attempt). Same as PropSZN's calc_matchup_rate.
calc_matchup_rate <- function(P, D, L) {
  ifelse(!is.na(L) & L > 0 & !is.na(D), P * (D / L), P)
}

# ── Empirical-Bayes shrinkage toward 1.0 for defense factors ─
#  factor_hat : raw ratio (def allowed / league)
#  n          : sample size behind the ratio (games)
#  k          : shrinkage prior weight in the same units as n
shrink_factor <- function(factor_hat, n, k) {
  w <- n / (n + k)
  ifelse(is.na(factor_hat), 1.0, 1.0 + w * (factor_hat - 1.0))
}

# ── Exponential recency weights ──────────────────────────────
#  games_ago : integer vector, 0 = most recent
#  half_life : games until weight halves
recency_weights <- function(games_ago, half_life) {
  0.5 ^ (games_ago / half_life)
}

# ── Weighted mean / weighted SD helpers (NA-safe) ────────────
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 3) return(NA_real_)
  m <- sum(x[ok] * w[ok]) / sum(w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}

# ── Discretized Beta weights over a rate grid ────────────────
#  Fits Beta(mean, sd) and returns normalized weights on `grid`.
#  Falls back to a point mass at `mean` when variance is degenerate.
#  Mirrors PropSZN's K-rate discretization in compute_double_mixture.
beta_grid_weights <- function(mean, sd, grid) {
  if (is.na(mean) || mean <= 0 || mean >= 1) return(NULL)
  if (is.na(sd) || sd < 1e-4) {
    w <- as.numeric(abs(grid - mean) == min(abs(grid - mean)))
    return(w / sum(w))
  }
  v     <- sd^2
  ratio <- (mean * (1 - mean)) / v
  if (ratio <= 1) {                    # variance too high for a Beta — clamp
    v     <- mean * (1 - mean) * 0.9
    ratio <- (mean * (1 - mean)) / v
  }
  a <- mean * (ratio - 1)
  b <- (1 - mean) * (ratio - 1)
  w <- stats::dbeta(grid, a, b)
  w[!is.finite(w)] <- 0
  tw <- sum(w)
  if (tw == 0) return(NULL)
  w / tw
}

# ── Team code normalization ─────────────────────────────────
#  hvpkod/ESPN-style codes -> nflverse schedule codes.
normalize_team <- function(x) {
  x <- toupper(trimws(x))
  dplyr::recode(x,
    "LAR" = "LA", "JAC" = "JAX", "WSH" = "WAS", "OAK" = "LV",
    "SD"  = "LAC", "STL" = "LA",
    .default = x
  )
}
