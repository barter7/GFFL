# helpers.R - Data processing functions for GFFL Dashboard

#' Compute all-time standings aggregated across seasons
#' @param standings_data Combined standings data from all seasons
#' @return Tibble with all-time records per team
compute_alltime_standings <- function(standings_data) {
  standings_data |>
    dplyr::group_by(Team = franchise_name) |>
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
#' @param schedule_data Combined schedule data from all seasons
#' @return List with team name and streak length
compute_longest_streak <- function(schedule_data) {
  best_team <- ""
  best_streak <- 0

  teams <- unique(schedule_data$franchise_name)

  for (team in teams) {
    team_games <- schedule_data |>
      dplyr::filter(franchise_name == team) |>
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

#' Compute the longest losing streak for a team
#' @param schedule_data Combined schedule data
#' @return List with team name and streak length
compute_longest_losing_streak <- function(schedule_data) {
  best_team <- ""
  best_streak <- 0

  teams <- unique(schedule_data$franchise_name)

  for (team in teams) {
    team_games <- schedule_data |>
      dplyr::filter(franchise_name == team) |>
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

#' Build the all-time records book
#' @param standings_data Combined standings data
#' @param schedule_data Combined schedule data
#' @return Tibble with record descriptions
compute_records_book <- function(standings_data, schedule_data) {
  records <- list()

  # Most wins in a single season
  best_season <- standings_data |>
    dplyr::arrange(dplyr::desc(h2h_wins)) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Most Wins (Season)",
    Holder = best_season$franchise_name,
    Value = as.character(best_season$h2h_wins),
    Season = as.character(best_season$season)
  )))

  # Fewest losses in a season
  fewest_losses <- standings_data |>
    dplyr::arrange(h2h_losses) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Fewest Losses (Season)",
    Holder = fewest_losses$franchise_name,
    Value = as.character(fewest_losses$h2h_losses),
    Season = as.character(fewest_losses$season)
  )))

  # Most points in a season
  most_pf <- standings_data |>
    dplyr::arrange(dplyr::desc(points_for)) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Most Points For (Season)",
    Holder = most_pf$franchise_name,
    Value = format(round(most_pf$points_for, 1), nsmall = 1),
    Season = as.character(most_pf$season)
  )))

  # Fewest points in a season
  least_pf <- standings_data |>
    dplyr::arrange(points_for) |>
    head(1)
  records <- c(records, list(data.frame(
    Record = "Fewest Points For (Season)",
    Holder = least_pf$franchise_name,
    Value = format(round(least_pf$points_for, 1), nsmall = 1),
    Season = as.character(least_pf$season)
  )))

  # Highest single-week score
  if (nrow(schedule_data) > 0) {
    high_week <- schedule_data |>
      dplyr::arrange(dplyr::desc(franchise_score)) |>
      head(1)
    records <- c(records, list(data.frame(
      Record = "Highest Weekly Score",
      Holder = high_week$franchise_name,
      Value = format(round(high_week$franchise_score, 2), nsmall = 2),
      Season = paste0(high_week$season, " W", high_week$week)
    )))

    # Lowest single-week score
    low_week <- schedule_data |>
      dplyr::filter(franchise_score > 0) |>
      dplyr::arrange(franchise_score) |>
      head(1)
    if (nrow(low_week) > 0) {
      records <- c(records, list(data.frame(
        Record = "Lowest Weekly Score",
        Holder = low_week$franchise_name,
        Value = format(round(low_week$franchise_score, 2), nsmall = 2),
        Season = paste0(low_week$season, " W", low_week$week)
      )))
    }

    # Biggest blowout
    blowout <- schedule_data |>
      dplyr::mutate(margin = franchise_score - opponent_score) |>
      dplyr::filter(margin > 0) |>
      dplyr::arrange(dplyr::desc(margin)) |>
      head(1)
    if (nrow(blowout) > 0) {
      records <- c(records, list(data.frame(
        Record = "Biggest Blowout",
        Holder = blowout$franchise_name,
        Value = paste0(round(blowout$franchise_score, 1), "-",
                       round(blowout$opponent_score, 1),
                       " (+", round(blowout$margin, 1), ")"),
        Season = paste0(blowout$season, " W", blowout$week)
      )))
    }

    # Closest game
    close_game <- schedule_data |>
      dplyr::mutate(margin = abs(franchise_score - opponent_score)) |>
      dplyr::filter(margin > 0) |>
      dplyr::arrange(margin) |>
      head(1)
    if (nrow(close_game) > 0) {
      records <- c(records, list(data.frame(
        Record = "Closest Game",
        Holder = close_game$franchise_name,
        Value = paste0(round(close_game$franchise_score, 2), "-",
                       round(close_game$opponent_score, 2),
                       " (", round(close_game$margin, 2), ")"),
        Season = paste0(close_game$season, " W", close_game$week)
      )))
    }
  }

  # Winning streak
  win_streak <- compute_longest_streak(schedule_data)
  records <- c(records, list(data.frame(
    Record = "Longest Winning Streak",
    Holder = win_streak$team,
    Value = paste0(win_streak$streak, " games"),
    Season = "All-Time"
  )))

  # Losing streak
  lose_streak <- compute_longest_losing_streak(schedule_data)
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
