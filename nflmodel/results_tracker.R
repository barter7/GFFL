# ============================================================
#  results_tracker.R  (nflmodel)
#  Automatic results tracking — the NFL sibling of PropSZN's
#  results_tracker.R.
#
#  Two functions called from run_model.R each week:
#
#    build_projection_snapshot(season, week, projections, props)
#      → data/results/projections_{season}_w{week}.csv
#        One row per player × market: projection, prop line,
#        market odds, no-vig prob, model prob, edge, lean.
#
#    match_results_to_actuals()
#      → data/results/matched_results.csv
#        Joins every snapshot to actual outcomes from the
#        gamelog cache. Only completed games are included.
#
#  Also maintains an opening-odds ledger (first line seen per
#  player/market/week — written once, never updated) for CLV.
#
#  Requires: utils.R, data_sources.R, scrape_props.R sourced.
# ============================================================

library(dplyr)
library(purrr)
library(readr)

RESULTS_DIR       <- file.path("data", "results")
OPENING_ODDS_PATH <- file.path(RESULTS_DIR, "opening_odds.csv")

# ── Opening odds ledger ──────────────────────────────────────
#  One row per player × market × week, written on first sight —
#  handles lines that post Tuesday vs. trickle in Friday.
update_opening_odds_ledger <- function(snap, season, week) {
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
  new_odds <- snap %>%
    filter(!is.na(line) | market == "anytime_td") %>%
    transmute(season, week, player_name, market, line,
              over_odds, under_odds, sportsbook,
              recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (nrow(new_odds) == 0) return(invisible(NULL))

  if (file.exists(OPENING_ODDS_PATH)) {
    existing <- read_csv(OPENING_ODDS_PATH, show_col_types = FALSE)
    new_odds <- new_odds %>%
      anti_join(existing, by = c("season", "week", "player_name", "market"))
    if (nrow(new_odds) > 0)
      write_csv(new_odds, OPENING_ODDS_PATH, append = TRUE)
  } else {
    write_csv(new_odds, OPENING_ODDS_PATH)
  }
  invisible(new_odds)
}

# ── Weekly projection snapshot ───────────────────────────────
#  projections: output of project_week() (run_model.R) — one row
#               per player × market with proj mean + p_over.
#  props:       output of props_best_lines() or NULL (offseason /
#               fetch failure) — snapshot still saved without
#               market columns so the projection record survives.
build_projection_snapshot <- function(season, week, projections,
                                      props = NULL) {
  dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

  snap <- projections %>%
    mutate(join_name = norm_name(player_name))

  if (!is.null(props) && nrow(props) > 0) {
    over <- props %>% filter(side == "over") %>%
      transmute(join_name = norm_name(player_name), market,
                line, over_odds = odds, sportsbook = book_name)
    under <- props %>% filter(side == "under") %>%
      transmute(join_name = norm_name(player_name), market,
                line_u = line, under_odds = odds)
    snap <- snap %>%
      left_join(over,  by = c("join_name", "market")) %>%
      left_join(under, by = c("join_name", "market")) %>%
      # only pair the under when it's the same line as the over
      mutate(under_odds = ifelse(!is.na(line) & !is.na(line_u) &
                                   line == line_u, under_odds, NA)) %>%
      select(-line_u)
  } else {
    snap <- snap %>%
      mutate(line = NA_real_, over_odds = NA_integer_,
             under_odds = NA_integer_, sportsbook = NA_character_)
  }

  snap <- snap %>%
    mutate(
      # model prob of the over at the posted line
      model_p_over = mapply(function(m, l, po) {
        if (is.na(l)) return(NA_real_)
        po        # p_over already evaluated at the posted line upstream
      }, market, line, p_over_line),
      market_p_over = devig_two_way(over_odds, under_odds),
      edge  = model_p_over - market_p_over,
      lean  = case_when(
        is.na(edge)   ~ NA_character_,
        edge >  0.03  ~ "OVER",
        edge < -0.03  ~ "UNDER",
        TRUE          ~ "PASS"
      )
    ) %>%
    select(-join_name)

  path <- file.path(RESULTS_DIR,
                    sprintf("projections_%d_w%02d.csv", season, week))
  write_csv(snap, path)
  update_opening_odds_ledger(snap, season, week)
  message(sprintf("  Snapshot saved: %s (%d rows)", path, nrow(snap)))
  invisible(snap)
}

# ── Match snapshots to actual outcomes ───────────────────────
match_results_to_actuals <- function() {
  files <- list.files(RESULTS_DIR, pattern = "^projections_\\d{4}_w\\d+\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(invisible(NULL))

  logs <- load_gamelogs()
  actuals <- logs %>%
    transmute(season, week, join_name = norm_name(player_name),
              act_receptions = receptions, act_pass_yards = pass_yards,
              act_pass_tds = pass_td,
              act_anytime = as.integer(rush_td + rec_td > 0))

  matched <- map_dfr(files, function(f) {
    snap <- read_csv(f, show_col_types = FALSE)
    if (!all(c("season", "week") %in% names(snap))) return(NULL)
    snap %>%
      # all-NA columns come back typed logical — force the schema
      mutate(line = as.numeric(line), lean = as.character(lean),
             edge = as.numeric(edge),
             join_name = norm_name(player_name)) %>%
      inner_join(actuals, by = c("season", "week", "join_name")) %>%
      mutate(actual = case_when(
        market == "receptions" ~ as.numeric(act_receptions),
        market == "pass_yards" ~ as.numeric(act_pass_yards),
        market == "pass_tds"   ~ as.numeric(act_pass_tds),
        market == "anytime_td" ~ as.numeric(act_anytime)
      )) %>%
      mutate(
        # as.character() guards: ifelse over an all-NA vector stays
        # typed logical, which case_when refuses to combine with chr
        result = case_when(
          is.na(line) | is.na(lean) | lean == "PASS" ~ NA_character_,
          market == "anytime_td" & lean == "OVER" ~
            as.character(ifelse(actual > 0, "WIN", "LOSS")),
          lean == "OVER"  ~ as.character(ifelse(actual > line, "WIN",
                              ifelse(actual == line, "PUSH", "LOSS"))),
          lean == "UNDER" ~ as.character(ifelse(actual < line, "WIN",
                              ifelse(actual == line, "PUSH", "LOSS")))
        )
      ) %>%
      select(-starts_with("act_"), -join_name)
  })

  out <- file.path(RESULTS_DIR, "matched_results.csv")
  write_csv(matched, out)
  message(sprintf("  Matched results: %d rows -> %s", nrow(matched), out))
  invisible(matched)
}
