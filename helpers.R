# helpers.R - Data processing functions for GFFL Dashboard

# =============================================================================
# OWNER MAPPING
# =============================================================================

#' GFFL Owner names mapped by alphabetical index of ESPN user_nickname.
#' When user_nicknames are sorted alphabetically, these are the real names
#' at each position (1-13). Duplicates mean one person had multiple accounts.
OWNER_NAMES_BY_INDEX <- c(
  "Alex",    # index 1
  "Connor",  # index 2
  "Harry",   # index 3
  "Matt",    # index 4
  "Connor",  # index 5 (same person, different ESPN account)
  "Harry",   # index 6 (same person, different ESPN account)
  "Joe",     # index 7
  "Jack",    # index 8
  "Mike",    # index 9
  "Kerley",  # index 10
  "RJ",      # index 11
  "Faz",     # index 12
  "Tom"      # index 13
)

#' Build owner mapping from draft data
#' Uses user_nickname from drafts to identify owners across seasons.
#' Matches the alphabetical index of unique nicknames to OWNER_NAMES_BY_INDEX.
#' @param draft_data Combined draft data with user_nickname column
#' @return Tibble mapping season + franchise_id to owner name
build_owner_map <- function(draft_data) {
  if (is.null(draft_data) || nrow(draft_data) == 0) return(NULL)

  # Build the nickname-to-owner lookup using alphabetical index
  # (same approach as the original GFFL analysis code)
  unique_nicknames <- draft_data |>
    dplyr::distinct(user_nickname) |>
    dplyr::arrange(user_nickname) |>
    dplyr::mutate(index = dplyr::row_number())

  # Map index to real owner name
  owner_lookup <- data.frame(
    index = seq_along(OWNER_NAMES_BY_INDEX),
    owner = OWNER_NAMES_BY_INDEX,
    stringsAsFactors = FALSE
  )

  nickname_to_owner <- dplyr::left_join(unique_nicknames, owner_lookup, by = "index")

  # Log the mapping for debugging
  message("Owner mapping:")
  for (i in seq_len(nrow(nickname_to_owner))) {
    message("  ", nickname_to_owner$user_nickname[i], " -> ",
            nickname_to_owner$owner[i])
  }

  # Fallback: if more nicknames than mapped names, use the nickname itself
  nickname_to_owner$owner <- ifelse(
    is.na(nickname_to_owner$owner),
    nickname_to_owner$user_nickname,
    nickname_to_owner$owner
  )

  # Get unique user_nickname per season/franchise, then attach owner
  nickname_map <- draft_data |>
    dplyr::distinct(season, franchise_id, franchise_name, user_nickname) |>
    dplyr::left_join(
      nickname_to_owner |> dplyr::select(user_nickname, owner),
      by = "user_nickname"
    ) |>
    dplyr::mutate(owner = ifelse(is.na(owner), user_nickname, owner))

  nickname_map
}

#' Attach owner names to schedule data
attach_owners <- function(schedule_data, owner_map) {
  if (is.null(owner_map) || nrow(owner_map) == 0) {
    schedule_data$team_owner <- schedule_data$franchise_name
    schedule_data$opponent_owner <- schedule_data$opponent_name
    return(schedule_data)
  }

  owner_lookup <- owner_map |>
    dplyr::select(season, franchise_id, owner)

  schedule_data |>
    dplyr::left_join(
      owner_lookup |> dplyr::rename(team_owner = owner),
      by = c("season", "franchise_id")
    ) |>
    dplyr::left_join(
      owner_lookup |> dplyr::rename(opponent_owner = owner),
      by = c("season", "opponent_id" = "franchise_id")
    ) |>
    dplyr::mutate(
      team_owner = ifelse(is.na(team_owner), franchise_name, team_owner),
      opponent_owner = ifelse(is.na(opponent_owner), opponent_name, opponent_owner)
    )
}

#' Attach owner names to standings data
attach_owners_standings <- function(standings_data, owner_map) {
  if (is.null(owner_map) || nrow(owner_map) == 0) {
    standings_data$owner <- standings_data$franchise_name
    return(standings_data)
  }

  owner_lookup <- owner_map |> dplyr::select(season, franchise_id, owner)

  standings_data |>
    dplyr::left_join(owner_lookup, by = c("season", "franchise_id")) |>
    dplyr::mutate(owner = ifelse(is.na(owner), franchise_name, owner))
}

# =============================================================================
# GAME TYPE CLASSIFICATION
# =============================================================================

#' Classify matchups as Regular Season or Playoffs
#' Regular season length changed over the years in the NFL/ESPN:
#'   2017-2018: Weeks 1-13
#'   2019-2020: Weeks 1-14
#'   2021+:     Weeks 1-15 (17-game season)
#' @param schedule_data Schedule data with season and week columns
#' @return Schedule data with game_type column added
classify_game_type <- function(schedule_data) {
  schedule_data |>
    dplyr::mutate(
      game_type = dplyr::case_when(
        season %in% c(2017, 2018) & week <= 13 ~ "Regular Season",
        season %in% c(2019, 2020) & week <= 14 ~ "Regular Season",
        season >= 2021 & week <= 15 ~ "Regular Season",
        season < 2017 & week <= 13 ~ "Regular Season",
        TRUE ~ "Playoffs"
      )
    )
}

# =============================================================================
# STANDINGS & RECORDS
# =============================================================================

#' Compute all-time standings aggregated across seasons (by owner)
compute_alltime_standings <- function(standings_data) {
  # Use owner if available, otherwise franchise_name
  name_col <- if ("owner" %in% names(standings_data)) "owner" else "franchise_name"

  standings_data |>
    dplyr::group_by(Team = .data[[name_col]]) |>
    dplyr::summarise(
      Seasons = dplyr::n(),
      W = sum(h2h_wins, na.rm = TRUE),
      L = sum(h2h_losses, na.rm = TRUE),
      `Win%` = ifelse((W + L) > 0, W / (W + L), 0),
      PF = sum(points_for, na.rm = TRUE),
      PA = sum(points_against, na.rm = TRUE),
      `PF/G` = ifelse((W + L) > 0, PF / (W + L), 0),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(W), dplyr::desc(`Win%`))
}

#' Compute the longest winning streak across all matchup data
compute_longest_streak <- function(schedule_data) {
  name_col <- if ("team_owner" %in% names(schedule_data)) "team_owner" else "franchise_name"
  best_team <- ""
  best_streak <- 0

  teams <- unique(schedule_data[[name_col]])

  for (team in teams) {
    team_games <- schedule_data |>
      dplyr::filter(.data[[name_col]] == team) |>
      dplyr::arrange(season, week)

    current_streak <- 0
    max_streak <- 0

    for (i in seq_len(nrow(team_games))) {
      if (!is.na(team_games$result[i]) && team_games$result[i] == "W") {
        current_streak <- current_streak + 1
        max_streak <- max(max_streak, current_streak)
      } else {
        current_streak <- 0
      }
    }

    if (max_streak > best_streak) {
      best_streak <- max_streak
      best_team <- team
    }
  }

  list(team = best_team, streak = best_streak)
}

#' Compute the longest losing streak
compute_longest_losing_streak <- function(schedule_data) {
  name_col <- if ("team_owner" %in% names(schedule_data)) "team_owner" else "franchise_name"
  best_team <- ""
  best_streak <- 0

  teams <- unique(schedule_data[[name_col]])

  for (team in teams) {
    team_games <- schedule_data |>
      dplyr::filter(.data[[name_col]] == team) |>
      dplyr::arrange(season, week)

    current_streak <- 0
    max_streak <- 0

    for (i in seq_len(nrow(team_games))) {
      if (!is.na(team_games$result[i]) && team_games$result[i] == "L") {
        current_streak <- current_streak + 1
        max_streak <- max(max_streak, current_streak)
      } else {
        current_streak <- 0
      }
    }

    if (max_streak > best_streak) {
      best_streak <- max_streak
      best_team <- team
    }
  }

  list(team = best_team, streak = best_streak)
}

# =============================================================================
# OWNER VS OWNER
# =============================================================================

#' Compute head-to-head records between all owners
#' @param schedule_data Schedule data with team_owner and opponent_owner columns
#' @param reg_season_only If TRUE, only count regular season games
#' @return Tibble with owner vs owner records
compute_owner_vs_owner <- function(schedule_data, reg_season_only = TRUE) {
  df <- schedule_data |>
    dplyr::filter(!is.na(result))

  if (reg_season_only && "game_type" %in% names(df)) {
    df <- df |> dplyr::filter(game_type == "Regular Season")
  }

  name_col <- if ("team_owner" %in% names(df)) "team_owner" else "franchise_name"
  opp_col <- if ("opponent_owner" %in% names(df)) "opponent_owner" else "opponent_name"

  df |>
    dplyr::group_by(
      Owner = .data[[name_col]],
      Opponent = .data[[opp_col]]
    ) |>
    dplyr::summarise(
      W = sum(result == "W", na.rm = TRUE),
      L = sum(result == "L", na.rm = TRUE),
      T = sum(result == "T", na.rm = TRUE),
      Games = dplyr::n(),
      `Win%` = round(W / Games, 3),
      PF = round(sum(franchise_score, na.rm = TRUE), 1),
      PA = round(sum(opponent_score, na.rm = TRUE), 1),
      `Avg Margin` = round(mean(franchise_score - opponent_score, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(Owner, dplyr::desc(W))
}

# =============================================================================
# PLAYER OWNERSHIP METRICS
# =============================================================================

#' Compute player ownership metrics per season/owner
#' @param starters_data Combined starters data (includes bench via lineup_slot)
#' @param owner_map Owner mapping from build_owner_map()
#' @return Tibble with one row per player-season-owner with ownership metrics
compute_player_ownership <- function(starters_data, owner_map = NULL) {
  if (is.null(starters_data) || nrow(starters_data) == 0) return(NULL)

  # Attach owner names
  if (!is.null(owner_map) && nrow(owner_map) > 0) {
    starters_data <- starters_data |>
      dplyr::left_join(
        owner_map |> dplyr::select(season, franchise_id, owner),
        by = c("season", "franchise_id")
      ) |>
      dplyr::mutate(owner = ifelse(is.na(owner), franchise_name, owner))
  } else {
    starters_data$owner <- starters_data$franchise_name
  }

  # Number of owners per player per season (computed separately)
  n_owners <- starters_data |>
    dplyr::group_by(season, player_id, player_name) |>
    dplyr::summarise(num_owners = dplyr::n_distinct(franchise_id), .groups = "drop")

  # Main aggregation: per player, per season, per owner
  player_stats <- starters_data |>
    dplyr::group_by(season, player_id, player_name, pos, team, franchise_id, owner) |>
    dplyr::summarise(
      acquired = min(week, na.rm = TRUE),
      weeks_owned = dplyr::n_distinct(week),
      points_owned = round(sum(player_score, na.rm = TRUE), 1),
      points_started = round(sum(
        ifelse(!lineup_slot %in% c("BE", "IR"), player_score, 0),
        na.rm = TRUE
      ), 1),
      weeks_started = sum(!lineup_slot %in% c("BE", "IR")),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_owners, by = c("season", "player_id", "player_name")) |>
    dplyr::arrange(season, owner, dplyr::desc(points_owned))

  player_stats
}

# =============================================================================
# RECORDS BOOK
# =============================================================================

#' Build the all-time records book
compute_records_book <- function(standings_data, schedule_data) {
  records <- list()

  name_col_std <- if ("owner" %in% names(standings_data)) "owner" else "franchise_name"
  name_col_sch <- if ("team_owner" %in% names(schedule_data)) "team_owner" else "franchise_name"

  # Filter to regular season if game_type exists
  reg_schedule <- if ("game_type" %in% names(schedule_data)) {
    schedule_data |> dplyr::filter(game_type == "Regular Season")
  } else {
    schedule_data
  }

  # Most wins in a single season
  best_season <- standings_data |>
    dplyr::arrange(dplyr::desc(h2h_wins)) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Most Wins (Season)",
    Holder = best_season[[name_col_std]],
    Value = as.character(best_season$h2h_wins),
    Season = as.character(best_season$season)
  )))

  # Fewest losses in a season
  fewest_losses <- standings_data |>
    dplyr::arrange(h2h_losses) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Fewest Losses (Season)",
    Holder = fewest_losses[[name_col_std]],
    Value = as.character(fewest_losses$h2h_losses),
    Season = as.character(fewest_losses$season)
  )))

  # Most points in a season
  most_pf <- standings_data |>
    dplyr::arrange(dplyr::desc(points_for)) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Most Points For (Season)",
    Holder = most_pf[[name_col_std]],
    Value = format(round(most_pf$points_for, 1), nsmall = 1),
    Season = as.character(most_pf$season)
  )))

  # Fewest points in a season
  least_pf <- standings_data |>
    dplyr::arrange(points_for) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Fewest Points For (Season)",
    Holder = least_pf[[name_col_std]],
    Value = format(round(least_pf$points_for, 1), nsmall = 1),
    Season = as.character(least_pf$season)
  )))

  if (nrow(reg_schedule) > 0) {
    # Highest single-week score
    high_week <- reg_schedule |>
      dplyr::arrange(dplyr::desc(franchise_score)) |>
      head(1)
    records <- c(records, list(data.frame(
      Record = "Highest Weekly Score",
      Holder = high_week[[name_col_sch]],
      Value = format(round(high_week$franchise_score, 2), nsmall = 2),
      Season = paste0(high_week$season, " W", high_week$week)
    )))

    # Lowest single-week score
    low_week <- reg_schedule |>
      dplyr::filter(franchise_score > 0) |>
      dplyr::arrange(franchise_score) |>
      head(1)
    if (nrow(low_week) > 0) {
      records <- c(records, list(data.frame(
        Record = "Lowest Weekly Score",
        Holder = low_week[[name_col_sch]],
        Value = format(round(low_week$franchise_score, 2), nsmall = 2),
        Season = paste0(low_week$season, " W", low_week$week)
      )))
    }

    # Biggest blowout
    blowout <- reg_schedule |>
      dplyr::mutate(margin = franchise_score - opponent_score) |>
      dplyr::filter(margin > 0) |>
      dplyr::arrange(dplyr::desc(margin)) |>
      head(1)
    if (nrow(blowout) > 0) {
      records <- c(records, list(data.frame(
        Record = "Biggest Blowout",
        Holder = blowout[[name_col_sch]],
        Value = paste0(round(blowout$franchise_score, 1), "-",
                       round(blowout$opponent_score, 1),
                       " (+", round(blowout$margin, 1), ")"),
        Season = paste0(blowout$season, " W", blowout$week)
      )))
    }

    # Closest game
    close_game <- reg_schedule |>
      dplyr::mutate(margin = abs(franchise_score - opponent_score)) |>
      dplyr::filter(margin > 0) |>
      dplyr::arrange(margin) |>
      head(1)
    if (nrow(close_game) > 0) {
      records <- c(records, list(data.frame(
        Record = "Closest Game",
        Holder = close_game[[name_col_sch]],
        Value = paste0(round(close_game$franchise_score, 2), "-",
                       round(close_game$opponent_score, 2),
                       " (", round(close_game$margin, 2), ")"),
        Season = paste0(close_game$season, " W", close_game$week)
      )))
    }
  }

  # Winning streak
  win_streak <- compute_longest_streak(reg_schedule)
  records <- c(records, list(data.frame(
    Record = "Longest Winning Streak",
    Holder = win_streak$team,
    Value = paste0(win_streak$streak, " games"),
    Season = "All-Time"
  )))

  # Losing streak
  lose_streak <- compute_longest_losing_streak(reg_schedule)
  records <- c(records, list(data.frame(
    Record = "Longest Losing Streak",
    Holder = lose_streak$team,
    Value = paste0(lose_streak$streak, " games"),
    Season = "All-Time"
  )))

  # All-time wins leader
  alltime <- compute_alltime_standings(standings_data) |>
    dplyr::arrange(dplyr::desc(W)) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Most All-Time Wins",
    Holder = alltime$Team,
    Value = as.character(alltime$W),
    Season = "All-Time"
  )))

  dplyr::bind_rows(records)
}
