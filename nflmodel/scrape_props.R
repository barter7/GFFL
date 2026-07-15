# ============================================================
#  scrape_props.R  (nflmodel)
#  Fetches NFL player props from Action Network (no auth
#  required) — the NFL sibling of PropSZN's scrape_props.R.
#
#  ENDPOINT PATTERN (same API as PropSZN's MLB strikeout feed,
#  league 8 → 1 for NFL):
#    https://api.actionnetwork.com/web/v1/leagues/1/props/{market}
#      ?stateCode=NY&date=YYYYMMDD
#
#  MARKET SLUGS: Action Network names prop endpoints
#  "core_bet_type_{id}_{name}" (MLB strikeouts were
#  core_bet_type_37_strikeouts). The NFL ids below are the
#  documented convention but MUST be verified against the live
#  API in-season — run discover_prop_markets() during week 1 and
#  correct any slug that 404s. (Written during the 2026 offseason
#  when the props API returns empty slates, so live verification
#  wasn't possible.)
#
#  RESPONSE STRUCTURE (identical to MLB):
#    result$markets[[1]]$players — list of {id, full_name}
#    result$markets[[1]]$books   — list of {book_id, odds[]}
#    odds entries: {player_id, value (line), money (american)}
#    over/under pairs per player per book.
#
#  OUTPUT (tidy): one row per player × market × book × side
#    cols: player_name, market, book_id, book_name, side, line,
#          odds, is_best_over, is_best_under
#
#  INTEGRATION:
#    source("scrape_props.R"); fetch_nfl_props()
#    Join to projections on norm_name(player_name).
# ============================================================

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# ── Prop markets modeled by model_core.R ─────────────────────
#  Keys are the model's market names; values are AN endpoint slugs.
NFL_MARKETS <- c(
  receptions   = "core_bet_type_75_receptions",
  pass_yards   = "core_bet_type_71_passing_yards",
  pass_tds     = "core_bet_type_73_passing_touchdowns",
  anytime_td   = "core_bet_type_78_touchdown_scorer"
)

# ── Book ID → display name (same AN v1 ids as PropSZN) ──────
BOOK_MAP <- c(
  "15"   = "Consensus",       "30"   = "Open",
  "939"  = "BetMGM",          "972"  = "BetRivers",
  "1005" = "Caesars",         "1006" = "FanDuel",
  "1548" = "DraftKings",      "1902" = "Resorts World",
  "1903" = "Bally Bet",       "2194" = "PrizePicks",
  "2292" = "Fliff",           "2789" = "Fanatics"
)

PROPS_CACHE_FILE      <- file.path("data", "props", "props.csv")
PROPS_CACHE_DATE_FILE <- paste0(PROPS_CACHE_FILE, ".date")
ODDS_LEDGER_FILE      <- file.path("data", "results", "odds_ledger.csv")

# Accent/case-insensitive name key for all props↔projection joins
# (same rule as PropSZN: odds feeds strip diacritics).
norm_name <- function(x) {
  tolower(trimws(iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")))
}

an_headers <- function() {
  httr::add_headers(
    `User-Agent` = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                          "AppleWebKit/537.36 (KHTML, like Gecko) ",
                          "Chrome/124.0 Safari/537.36"),
    Accept   = "application/json",
    Referer  = "https://www.actionnetwork.com/"
  )
}

# ── Discover live prop endpoints for the NFL ─────────────────
#  Lists whatever prop markets AN currently exposes for league 1.
#  Use in week 1 to verify/correct NFL_MARKETS slugs.
discover_prop_markets <- function(state = "NY") {
  url <- sprintf(
    "https://api.actionnetwork.com/web/v1/leagues/1?stateCode=%s", state)
  resp <- tryCatch(httr::GET(url, an_headers(), httr::timeout(20)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("[props] discover failed (HTTP ",
            if (is.null(resp)) "error" else httr::status_code(resp), ")")
    return(NULL)
  }
  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
  # The league config carries a list of prop page slugs.
  slugs <- tryCatch(
    unlist(lapply(parsed$league$prop_bet_types,
                  function(x) x$key %||% x$slug %||% NULL)),
    error = function(e) NULL)
  if (is.null(slugs)) {
    message("[props] response shape changed — inspect `parsed` manually")
    return(parsed)
  }
  slugs
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Fetch one market for one date ────────────────────────────
fetch_market_single_date <- function(market_key, date_str, state = "NY") {
  slug <- NFL_MARKETS[[market_key]]
  url  <- sprintf(
    "https://api.actionnetwork.com/web/v1/leagues/1/props/%s?stateCode=%s&date=%s",
    slug, state, date_str)

  resp <- tryCatch(httr::GET(url, an_headers(), httr::timeout(25)),
                   error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message(sprintf("[props] %s %s: HTTP %s", market_key, date_str,
                    if (is.null(resp)) "error" else httr::status_code(resp)))
    return(NULL)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(parsed)) return(NULL)

  mkts <- parsed$markets
  if (length(mkts) == 0) return(NULL)
  m <- mkts[[1]]

  players <- bind_rows(lapply(m$players, function(p)
    tibble(player_id = p$id, player_name = p$full_name)))
  if (nrow(players) == 0) return(NULL)

  rows <- list()
  for (bk in m$books) {
    bid <- as.character(bk$book_id)
    for (o in bk$odds) {
      side <- o$type %||% (if (isTRUE(o$is_over)) "over" else
                           if (isTRUE(o$is_under)) "under" else NA)
      rows[[length(rows) + 1]] <- tibble(
        player_id = o$player_id,
        book_id   = bid,
        side      = as.character(side),
        line      = as.numeric(o$value %||% NA),
        odds      = as.integer(o$money %||% NA)
      )
    }
  }
  if (length(rows) == 0) return(NULL)

  bind_rows(rows) %>%
    left_join(players, by = "player_id") %>%
    mutate(
      market    = market_key,
      book_name = coalesce(BOOK_MAP[book_id], paste0("Book_", book_id)),
      game_date = date_str
    ) %>%
    filter(!is.na(player_name), !is.na(odds)) %>%
    group_by(player_name, market, side) %>%
    mutate(
      is_best_over  = side == "over"  & odds == max(odds[side == "over"],  -Inf),
      is_best_under = side == "under" & odds == max(odds[side == "under"], -Inf)
    ) %>%
    ungroup()
}

# ── Odds movement ledger ─────────────────────────────────────
#  One row per GENUINE price change per (game_date, player,
#  market, book, side, line) — identical dedupe logic to
#  PropSZN's ledger: consecutive identical fetches collapse,
#  every real tick (open → moves → close) is preserved, and
#  `line` is part of identity (a 4.5 → 3.5 move is a different
#  market, not a price change — matters for CLV).
update_odds_ledger <- function(props_df) {
  if (is.null(props_df) || nrow(props_df) == 0) return(invisible(NULL))
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

  new_entries <- props_df %>%
    filter(!is.na(odds), !is.na(line) | market == "anytime_td") %>%
    transmute(game_date, player_name, market,
              book_id = as.character(book_id), book_name,
              side = as.character(side), line = as.numeric(line),
              odds = as.integer(odds), recorded_at = ts)
  if (nrow(new_entries) == 0) return(invisible(NULL))

  dir.create(dirname(ODDS_LEDGER_FILE), showWarnings = FALSE, recursive = TRUE)
  key_cols <- c("game_date", "player_name", "market", "book_id", "side", "line")

  if (file.exists(ODDS_LEDGER_FILE)) {
    existing <- tryCatch(read_csv(ODDS_LEDGER_FILE, show_col_types = FALSE,
                                  progress = FALSE),
                         error = function(e) NULL)
    if (!is.null(existing) && nrow(existing) > 0) {
      last_odds <- existing %>%
        group_by(across(all_of(key_cols))) %>%
        slice_max(recorded_at, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(all_of(key_cols), last_odds = odds)
      new_entries <- new_entries %>%
        left_join(last_odds, by = key_cols) %>%
        filter(is.na(last_odds) | odds != last_odds) %>%
        select(-last_odds)
    }
    if (nrow(new_entries) > 0)
      write_csv(new_entries, ODDS_LEDGER_FILE, append = TRUE)
  } else {
    write_csv(new_entries, ODDS_LEDGER_FILE)
  }
  invisible(new_entries)
}

# ── Public entry point ───────────────────────────────────────
#  Fetches all modeled markets for the given date (default:
#  today ET) plus the following 6 days — NFL slates span
#  Thu/Sun/Mon, so a Tuesday run captures the whole week.
fetch_nfl_props <- function(date = NULL, state = "NY", use_cache = TRUE) {
  today_et <- as.Date(format(Sys.time(), tz = "America/New_York", "%Y-%m-%d"))
  if (is.null(date)) date <- today_et

  if (use_cache && file.exists(PROPS_CACHE_FILE) &&
      file.exists(PROPS_CACHE_DATE_FILE)) {
    cache_date <- tryCatch(
      as.Date(readLines(PROPS_CACHE_DATE_FILE, warn = FALSE)[1]),
      error = function(e) as.Date("2000-01-01"))
    if (!is.na(cache_date) && cache_date >= today_et) {
      message("[props] Using cached props (written today)")
      return(read_csv(PROPS_CACHE_FILE, show_col_types = FALSE))
    }
  }

  all_dfs <- list()
  for (d_off in 0:6) {
    d_str <- format(date + d_off, "%Y%m%d")
    for (mk in names(NFL_MARKETS)) {
      df_one <- fetch_market_single_date(mk, d_str, state)
      if (!is.null(df_one) && nrow(df_one) > 0) {
        all_dfs[[paste(mk, d_str)]] <- df_one
        update_odds_ledger(df_one)
      }
      Sys.sleep(0.5)                       # be polite to the API
    }
  }

  df <- if (length(all_dfs) > 0) bind_rows(all_dfs) else NULL
  if (is.null(df) || nrow(df) == 0) {
    warning("[props] No props found — offseason, or slugs need ",
            "re-verification via discover_prop_markets()")
    return(NULL)
  }

  dir.create(dirname(PROPS_CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
  write_csv(df, PROPS_CACHE_FILE)
  writeLines(as.character(today_et), PROPS_CACHE_DATE_FILE)
  df
}

# ── Best line per player × market × side ─────────────────────
props_best_lines <- function(props_df) {
  props_df %>%
    filter(!is.na(line) | market == "anytime_td") %>%
    group_by(player_name, market, side) %>%
    slice_max(odds, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_name, market, side, line, odds, book_name)
}
