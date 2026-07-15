# ============================================================
#  run_model.R  (nflmodel)
#  Weekly driver — the NFL sibling of PropSZN's build_gamelogs.R
#  daily build.
#
#    1. Refreshes the data cache from nflreadr (skipped if
#       nflreadr isn't installed and a cache already exists)
#    2. Determines the upcoming week from the schedule
#    3. Builds team/defense/player rate tables from all
#       completed games
#    4. Projects all four markets for every eligible player
#    5. Fetches current prop lines (Action Network) and joins
#    6. Saves the weekly card + projection snapshot, updates
#       matched results, renders the HTML report
#
#  Usage:
#    Rscript run_model.R              # upcoming week
#    Rscript run_model.R 2026 3      # specific season/week
# ============================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
})
source("utils.R")
source("data_sources.R")
source("model_core.R")
source("scrape_props.R")
source("results_tracker.R")

message("=== NFL Model Weekly Build ===")
message(sprintf("Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ── Step 1: refresh data ─────────────────────────────────────
message("\n--- Step 1: data cache ---")
tryCatch({
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    refresh_cache()
  } else if (file.exists(GAMELOGS_CACHE)) {
    message("  nflreadr not installed — using existing cache.")
  } else {
    stop("no cache and no nflreadr")
  }
}, error = function(e) {
  message("  WARNING: cache refresh failed — ", conditionMessage(e))
  message("  Build continues with previously cached data.")
})

logs  <- load_gamelogs()
sched <- load_schedules_cache()
tv    <- schedule_team_view(sched)

# ── Step 2: pick the slate week ──────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  SLATE_SEASON <- as.integer(args[1]); SLATE_WEEK <- as.integer(args[2])
} else {
  upcoming <- sched %>%
    filter(is.na(home_score)) %>%
    arrange(season, week) %>%
    slice(1)
  if (nrow(upcoming) == 0)
    stop("No unplayed games in the schedule cache — refresh the cache ",
         "or pass season/week explicitly: Rscript run_model.R 2026 1")
  SLATE_SEASON <- upcoming$season; SLATE_WEEK <- upcoming$week
}
message(sprintf("\n--- Step 2: slate = season %d week %d ---",
                SLATE_SEASON, SLATE_WEEK))

slate <- tv %>% filter(season == SLATE_SEASON, week == SLATE_WEEK)
if (nrow(slate) == 0) stop("No games found for that slate.")

# ── Step 3: model tables (no lookahead) ──────────────────────
message("\n--- Step 3: rates & factors ---")
window <- logs %>%
  filter(season < SLATE_SEASON | (season == SLATE_SEASON & week < SLATE_WEEK))
tw  <- build_team_weeks(window)
lg  <- build_league_baselines(tw)
dfx <- build_defense_factors(tw, lg)
pr  <- build_player_rates(window, tw)
tds <- build_qb_td_shape(window)

# ── Step 4: project the slate ────────────────────────────────
#
#  Eligible players: last seen on a slate team, participated in
#  at least 2 of the team's last 4 games (production proxy for
#  "active and involved" — replace with a depth-chart/injury
#  feed for sharper inclusion near kickoff).
message("\n--- Step 4: projections ---")
recent_cut <- window %>%
  group_by(team) %>%
  summarise(recent_games = list(tail(sort(unique(paste(season, week))), 4)),
            .groups = "drop")

eligible <- window %>%
  mutate(gkey = paste(season, week),
         played = (targets + carries > 0) | (position == "QB" & pass_yards > 0)) %>%
  filter(played) %>%
  inner_join(recent_cut, by = "team") %>%
  rowwise() %>%
  filter(gkey %in% recent_games) %>%
  ungroup() %>%
  count(player_id, team, name = "recent_played") %>%
  filter(recent_played >= 2) %>%
  inner_join(slate %>% select(team, opponent, team_spread, total_line,
                              implied_total),
             by = "team")

proj_rows <- list()
for (i in seq_len(nrow(eligible))) {
  e <- eligible[i, ]
  p <- pr %>% filter(player_id == e$player_id, team == e$team)
  if (nrow(p) == 0 || p$n_games < 3) next
  vd <- build_team_volume_dist(tw, e$team)
  dd <- dfx %>% filter(defense == e$opponent)
  if (nrow(dd) == 0) dd <- default_def_factors()
  gs <- game_script_mult(e$team_spread, e$total_line)

  if (p$position %in% c("RB", "WR", "TE")) {
    pj <- project_receptions(p, vd, dd, gs, lg)
    if (!is.null(pj))
      proj_rows[[length(proj_rows) + 1]] <- tibble(
        season = SLATE_SEASON, week = SLATE_WEEK,
        player_name = p$player_name, position = p$position,
        team = e$team, opponent = e$opponent,
        market = "receptions", proj = pj$exp_rec,
        dist = list(pj$probs))
  }
  if (p$position == "QB" && p$qb_games >= 3) {
    py <- project_pass_yards(p, vd, dd, gs, lg)
    if (!is.null(py))
      proj_rows[[length(proj_rows) + 1]] <- tibble(
        season = SLATE_SEASON, week = SLATE_WEEK,
        player_name = p$player_name, position = p$position,
        team = e$team, opponent = e$opponent,
        market = "pass_yards", proj = py$exp_yds,
        dist = list(py$p_over))
    pt <- project_pass_tds(p, vd, dd, gs, lg, e$implied_total,
                           pass_share = team_pass_td_share(tw, e$team),
                           td_shape = tds)
    if (!is.null(pt))
      proj_rows[[length(proj_rows) + 1]] <- tibble(
        season = SLATE_SEASON, week = SLATE_WEEK,
        player_name = p$player_name, position = p$position,
        team = e$team, opponent = e$opponent,
        market = "pass_tds", proj = pt$exp_td,
        dist = list(pt$probs))
  }
  pa <- project_anytime_td(p, dd, lg, e$implied_total)
  if (!is.null(pa))
    proj_rows[[length(proj_rows) + 1]] <- tibble(
      season = SLATE_SEASON, week = SLATE_WEEK,
      player_name = p$player_name, position = p$position,
      team = e$team, opponent = e$opponent,
      market = "anytime_td", proj = pa$p_anytime,
      dist = list(pa$p_anytime))
}
projections <- bind_rows(proj_rows)
message(sprintf("  %d projections across %d players.",
                nrow(projections), n_distinct(projections$player_name)))

# ── Step 5: props ────────────────────────────────────────────
message("\n--- Step 5: prop lines ---")
props <- tryCatch(fetch_nfl_props(), error = function(e) {
  message("  Props fetch failed: ", conditionMessage(e)); NULL })
best <- if (!is.null(props)) props_best_lines(props) else NULL

# Evaluate P(over) at the posted line (falls back to the nearest
# half-point line to the projection when no line is posted).
line_lookup <- if (!is.null(best)) {
  best %>% filter(side == "over") %>%
    transmute(join_name = norm_name(player_name), market, posted = line)
} else NULL

projections <- projections %>%
  mutate(join_name = norm_name(player_name)) %>%
  { if (!is.null(line_lookup))
      left_join(., line_lookup, by = c("join_name", "market"))
    else mutate(., posted = NA_real_) } %>%
  mutate(
    line_eval = coalesce(posted,
                         ifelse(market == "pass_yards",
                                round(proj / 5) * 5 + 0.5,
                                floor(proj) + 0.5)),
    p_over_line = pmap_dbl(list(market, dist, line_eval), function(m, d, l) {
      if (m == "anytime_td")      as.numeric(d)
      else if (m == "pass_yards") d(l)
      else                        prob_over_discrete(d, l)
    })
  ) %>%
  select(-dist, -posted, -join_name)

# ── Step 6: snapshot, card, report ───────────────────────────
message("\n--- Step 6: outputs ---")
snap <- build_projection_snapshot(SLATE_SEASON, SLATE_WEEK, projections, best)
match_results_to_actuals()

card_path <- file.path("data", "results",
                       sprintf("card_%d_w%02d.csv", SLATE_SEASON, SLATE_WEEK))
write_csv(snap %>% arrange(desc(abs(coalesce(edge, 0)))), card_path)
message("  Card: ", card_path)

if (requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()) {
  tryCatch({
    rmarkdown::render("report.Rmd",
                      params = list(season = SLATE_SEASON, week = SLATE_WEEK),
                      output_file = sprintf("report_%d_w%02d.html",
                                            SLATE_SEASON, SLATE_WEEK),
                      output_dir = file.path("data", "results"),
                      quiet = TRUE)
    message("  Report rendered.")
  }, error = function(e) message("  Report failed: ", conditionMessage(e)))
} else {
  message("  rmarkdown/pandoc unavailable — skipping report.")
}

message(sprintf("\nDone: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
