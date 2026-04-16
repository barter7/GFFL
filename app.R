# GFFL - ESPN Fantasy Football League Historical Dashboard
# Deploy to shinyapps.io

# --- Package Loading ---
library(shiny)
library(bslib)
library(ffscrapr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)
library(purrr)
library(stringr)
library(scales)

# --- Source Helpers ---
source("helpers.R")

# --- League Configuration (for future live data features) ---
LEAGUE_ID <- 570237
ESPN_S2_TOKEN <- Sys.getenv("ESPN_S2", "AEA1%2BjaW2g9NUF4iIXuqdbRZ5qVRGdaxEa3YlbfwdrOPXeKIQvKQZvY7jAiem0XvhcnYczVj2lXY%2FpOTeYUQWSR6tpdRCWrKhUWv%2BvkZ3Q7CiTNr40zp3SZxiPTiyitikEwxmmDA6Dz4dAI1yRU1MWBYQpsRTBjapEBbs0ZKB4mOVPSc0NACSMFxtmb52rdidtJBVyRSAAtI5c9eJUmcsz7bozC96i5p9PAYr18U1Gm56W04Z8XqC3onM3iuqcj0F7dDrwRGKRVYHJSpm3xzJa2TFJH5K6nS4Ps%2B4xSjhsz3ag%3D%3D")
ESPN_SWID_TOKEN <- Sys.getenv("ESPN_SWID", "{5762B088-9519-444D-A2B0-889519E44D16}")

# --- UI ---
ui <- page_navbar(
  title = "GFFL Archives",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#013369",
    secondary = "#D50A0A",
    "navbar-bg" = "#013369"
  ),

  # --- Tab Panels ---

  # HOME / TITLE PAGE
  nav_panel(
    title = "GFFL",
    icon = icon("house"),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Cinzel:wght@700;900&family=Bebas+Neue&display=swap")
    ),
    div(
      style = paste0(
        "min-height:85vh; display:flex; flex-direction:column; align-items:center; justify-content:center; ",
        "background: radial-gradient(ellipse at center, #1a1a2e 0%, #0a0a14 70%, #000 100%); ",
        "padding:40px 20px; border-radius:12px; text-align:center; position:relative; overflow:hidden;"
      ),
      # Subtle gold pattern overlay
      div(style = paste0(
        "position:absolute; inset:0; ",
        "background: radial-gradient(ellipse at top, rgba(212,168,75,0.08), transparent 60%), ",
        "radial-gradient(ellipse at bottom, rgba(212,168,75,0.05), transparent 60%); ",
        "pointer-events:none;"
      )),
      # Top decorative line
      div(style = "height:3px; width:70%; max-width:800px; background:linear-gradient(90deg, transparent, #c9a84c 20%, #f0d675 50%, #c9a84c 80%, transparent); margin-bottom:30px; z-index:2;"),
      # Main title
      h1(style = paste0(
        "font-family:'Cinzel',Georgia,serif; font-weight:900; ",
        "color:#d4a84b; letter-spacing:8px; ",
        "text-shadow: 0 2px 4px rgba(0,0,0,0.8), 0 0 30px rgba(212,168,75,0.4), 0 0 60px rgba(212,168,75,0.2); ",
        "font-size:clamp(32px, 8vw, 80px); line-height:1.1; margin:0; z-index:2; position:relative;"
      ), "GROUPIES"),
      h1(style = paste0(
        "font-family:'Cinzel',Georgia,serif; font-weight:900; ",
        "color:#f0d675; letter-spacing:10px; ",
        "text-shadow: 0 2px 4px rgba(0,0,0,0.8), 0 0 30px rgba(212,168,75,0.4), 0 0 60px rgba(212,168,75,0.2); ",
        "font-size:clamp(40px, 10vw, 100px); line-height:1.1; margin:10px 0; z-index:2; position:relative;"
      ), "FANTASY"),
      h1(style = paste0(
        "font-family:'Cinzel',Georgia,serif; font-weight:900; ",
        "color:#d4a84b; letter-spacing:8px; ",
        "text-shadow: 0 2px 4px rgba(0,0,0,0.8), 0 0 30px rgba(212,168,75,0.4), 0 0 60px rgba(212,168,75,0.2); ",
        "font-size:clamp(32px, 8vw, 80px); line-height:1.1; margin:0; z-index:2; position:relative;"
      ), "FUCKBOI"),
      h1(style = paste0(
        "font-family:'Cinzel',Georgia,serif; font-weight:900; ",
        "color:#d4a84b; letter-spacing:12px; ",
        "text-shadow: 0 2px 4px rgba(0,0,0,0.8), 0 0 30px rgba(212,168,75,0.4), 0 0 60px rgba(212,168,75,0.2); ",
        "font-size:clamp(28px, 7vw, 70px); line-height:1.1; margin:10px 0 0; z-index:2; position:relative;"
      ), "LEAGUE"),
      # Bottom decorative line
      div(style = "height:3px; width:70%; max-width:800px; background:linear-gradient(90deg, transparent, #c9a84c 20%, #f0d675 50%, #c9a84c 80%, transparent); margin-top:30px; z-index:2;"),
      div(style = paste0(
        "color:#8b6914; font-family:Georgia,serif; font-style:italic; ",
        "letter-spacing:4px; margin-top:20px; font-size:clamp(12px, 2vw, 18px); z-index:2; position:relative;"
      ), "EST. 2016")
    )
  ),

  # TROPHY ROOM
  nav_panel(
    title = "Standings",
    icon = icon("trophy"),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          textOutput("standings_title", inline = TRUE),
          div(
            selectInput("standings_season", label = NULL, choices = NULL, width = "140px")
          )
        ),
        DTOutput("alltime_standings_table")
      ),
      card(
        card_header(textOutput("box_plot_title", inline = TRUE)),
        plotlyOutput("season_points_dist_plot", height = "400px")
      )
    )
  ),

  # MATCHUPS
  nav_panel(
    title = "Drafts",
    icon = icon("list-ol"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Draft Board",
          div(
            selectInput("draft_season", label = NULL, choices = NULL, width = "120px")
          )
        ),
        div(style = "overflow-x:auto;", uiOutput("draft_grid"))
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Draft Results (List)"),
        DTOutput("draft_table")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Round 1 Position Breakdown (%)"),
        plotlyOutput("draft_r1_plot", height = "400px")
      ),
      card(
        card_header("Overall Draft Position Breakdown (%)"),
        plotlyOutput("draft_pos_plot", height = "400px")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Biggest Draft Busts"),
        helpText("Players whose actual points fell well short of what their draft slot should have yielded"),
        DTOutput("draft_busts_table")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Biggest Draft Values"),
        helpText("Players who dramatically outscored their draft position's expected value"),
        DTOutput("draft_values_table")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Closest to Draft Value"),
        helpText("Players who finished closest to where they were drafted (min top-40 at position)"),
        DTOutput("draft_accurate_table")
      )
    )
  ),

  # TOP PERFORMANCES
  nav_panel(
    title = "Matchups",
    icon = icon("people-arrows"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Weekly Matchup Results",
          div(
            class = "d-flex gap-2",
            selectInput("matchup_season", label = NULL, choices = NULL, width = "120px"),
            selectInput("matchup_type", label = NULL,
                        choices = c("All", "Regular Season", "Playoffs"),
                        width = "150px")
          )
        ),
        DTOutput("matchups_table")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Highest Scoring Weeks (All-Time)"),
        DTOutput("top_scores_table")
      ),
      card(
        card_header("Biggest Blowouts (All-Time)"),
        DTOutput("blowouts_table")
      )
    )
  ),

  # HEAD-TO-HEAD
  nav_panel(
    title = "Head-to-Head",
    icon = icon("scale-balanced"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Select Matchup"),
        uiOutput("h2h_team_selectors"),
        checkboxInput("h2h_reg_only", "Regular Season Only", value = TRUE),
        actionButton("calc_h2h", "Compare", class = "btn-primary w-100 mt-2")
      ),
      card(
        card_header("Head-to-Head Record"),
        uiOutput("h2h_summary"),
        plotlyOutput("h2h_plot", height = "350px")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Head-to-Head Matchup History"),
        DTOutput("h2h_detail_table")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Owner vs Owner Record Matrix (Regular Season)"),
        DTOutput("owner_vs_owner_table")
      )
    )
  ),

  # DRAFT HISTORY
  nav_panel(
    title = "Trophy Room",
    icon = icon("award"),
    uiOutput("owners_grid")
  ),

  # HALL OF FAME
  nav_panel(
    title = "Hall of Fame",
    icon = icon("landmark"),
    div(
      class = "text-center my-3",
      h2(style = "color:#8B6914; font-family:Georgia,serif; letter-spacing:2px;", "GFFL HALL OF FAME"),
      tags$hr(style = "border-color:#8B6914; width:200px; margin:0 auto;")
    ),
    uiOutput("hof_gallery")
  ),

  # STANDINGS
  nav_panel(
    title = "Records",
    icon = icon("medal"),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IM+Fell+English:ital@0;1&family=Cormorant+Garamond:ital,wght@0,400;0,600;1,400&display=swap")
    ),
    div(
      class = "record-book",
      uiOutput("records_book")
    )
  ),

  # PLAYER RECORDS
  nav_panel(
    title = "Player Records",
    icon = icon("star"),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IM+Fell+English:ital@0;1&family=Cormorant+Garamond:ital,wght@0,400;0,600;1,400&display=swap")
    ),
    div(
      class = "record-book",
      uiOutput("player_records_book")
    )
  ),

  # ACHIEVEMENTS
  nav_panel(
    title = "Achievements",
    icon = icon("trophy"),
    div(
      style = "background:#0a0a0a; padding:20px; border-radius:12px; min-height:80vh;",
      uiOutput("achievements_gallery")
    )
  ),

  nav_panel(
    title = "Top Performances",
    icon = icon("star"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Top Player Performances (All-Time)",
          div(
            class = "d-flex gap-2",
            selectInput("perf_pos", label = NULL,
                        choices = c("All", "QB", "RB", "WR", "TE", "K", "D/ST"),
                        width = "100px"),
            selectInput("perf_season", label = NULL,
                        choices = NULL,
                        width = "120px")
          )
        ),
        uiOutput("top_performances_cards")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Top Performers by Position"),
        DTOutput("top_by_position_table")
      ),
      card(
        card_header("Most Appearances in Top 25"),
        plotlyOutput("top_appearances_plot", height = "400px")
      )
    )
  ),

  # RECAP PHOTOS
  nav_panel(
    title = "Recap Photos",
    icon = icon("images"),
    div(
      class = "text-center my-3",
      h2(style = "color:#013369; font-family:Georgia,serif;", "GFFL Through The Years"),
      tags$hr(style = "border-color:#013369; width:200px; margin:0 auto;")
    ),
    uiOutput("recap_gallery")
  ),

  # COMMISSIONER OF THE YEAR
  nav_panel(
    title = "Commissioner of the Year",
    icon = icon("gavel"),
    div(
      style = paste0(
        "background: linear-gradient(180deg, #0f0f1a 0%, #1a1a2e 50%, #16213e 100%); ",
        "padding:30px 20px; border-radius:12px; text-align:center; min-height:80vh;"
      ),
      h2(style = "color:#d4a84b; font-family:Georgia,serif; letter-spacing:3px; margin-bottom:5px;", "COMMISSIONER OF THE YEAR"),
      tags$hr(style = "border-color:#8b6914; width:200px; margin:0 auto 30px;"),
      uiOutput("commish_gallery")
    )
  ),

  # RECORDS
  nav_spacer(),
  nav_item(
    tags$a(
      icon("github"), "Source",
      href = "https://github.com/barter7/gffl",
      target = "_blank",
      class = "nav-link"
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  # Reactive values to hold fetched data
  rv <- reactiveValues(
    league_data = NULL,
    standings_data = NULL,
    schedule_data = NULL,
    draft_data = NULL,
    starters_data = NULL,
    player_ids = NULL,
    player_stats = NULL,
    owner_map = NULL,
    all_owners = NULL,
    seasons_loaded = NULL
  )

  # --- Load cached data on startup ---
  load_league_data <- function() {
    withProgress(message = "Loading league data...", value = 0.5, {

      # Load pre-cached RDS files (generated by fetch_data.R)
      rv$league_data <- readRDS("data/league_info.rds")
      rv$standings_data <- readRDS("data/standings.rds")
      rv$schedule_data <- readRDS("data/schedule.rds") |> classify_game_type()
      rv$draft_data <- readRDS("data/drafts.rds")
      if (file.exists("data/starters.rds")) {
        rv$starters_data <- readRDS("data/starters.rds")
      }
      if (file.exists("data/player_ids.rds")) {
        rv$player_ids <- readRDS("data/player_ids.rds")
      }
      if (file.exists("data/player_stats.rds")) {
        rv$player_stats <- readRDS("data/player_stats.rds")
      }
      rv$seasons_loaded <- sort(unique(rv$standings_data$season))

      # Build owner mapping from draft data
      if (!is.null(rv$draft_data) && nrow(rv$draft_data) > 0) {
        rv$owner_map <- build_owner_map(rv$draft_data)

        # Attach owner names to schedule and standings
        rv$schedule_data <- attach_owners(rv$schedule_data, rv$owner_map)
        rv$standings_data <- attach_owners_standings(rv$standings_data, rv$owner_map)

        # Attach owner names to draft data
        rv$draft_data <- rv$draft_data |>
          left_join(
            rv$owner_map |> select(season, franchise_id, owner),
            by = c("season", "franchise_id")
          ) |>
          mutate(owner = ifelse(is.na(owner), franchise_name, owner))
      } else {
        rv$schedule_data$team_owner <- rv$schedule_data$franchise_name
        rv$schedule_data$opponent_owner <- rv$schedule_data$opponent_name
        rv$standings_data$owner <- rv$standings_data$franchise_name
      }

      # Extract unique owner names
      if (nrow(rv$standings_data) > 0) {
        rv$all_owners <- sort(unique(rv$standings_data$owner))
      }

      # Update season selectors
      loaded <- rv$seasons_loaded
      if (length(loaded) > 0) {
        updateSelectInput(session, "matchup_season",
                          choices = sort(loaded, decreasing = TRUE))
        updateSelectInput(session, "draft_season",
                          choices = sort(loaded, decreasing = TRUE))
        updateSelectInput(session, "perf_season",
                          choices = c("All-Time", sort(loaded, decreasing = TRUE)))
        updateSelectInput(session, "standings_season",
                          choices = c("All-Time", sort(loaded, decreasing = TRUE)))
      }
    })

    showNotification(
      paste("Loaded", length(rv$seasons_loaded), "seasons successfully!"),
      type = "message", duration = 5
    )
  }

  # Auto-load on startup (must run inside observe for reactive access)
  auto_loaded <- reactiveVal(FALSE)
  observe({
    if (!auto_loaded()) {
      auto_loaded(TRUE)
      load_league_data()
    }
  })

  # Reload button
  observeEvent(input$reload_data, {
    load_league_data()
  })

  # --- League Info Panel ---
  output$league_info_panel <- renderUI({
    req(rv$league_data)
    latest <- rv$league_data[[as.character(max(rv$seasons_loaded))]]
    if (is.null(latest)) return(NULL)

    tagList(
      h6(class = "text-primary", latest$league_name),
      tags$small(
        paste0(
          latest$franchise_count, " teams | ",
          paste(range(rv$seasons_loaded), collapse = "-")
        )
      )
    )
  })

  # ==========================================================================
  # TROPHY ROOM TAB
  # ==========================================================================

  output$owners_grid <- renderUI({
    req(rv$standings_data, rv$schedule_data)

    # Compute stats per owner
    alltime <- compute_alltime_standings(rv$standings_data)

    # Rank owners by win% and total PF for plaque colors
    winpct_ranked <- alltime |> arrange(desc(`Win%`)) |> mutate(winpct_rank = row_number())
    pf_ranked <- alltime |> arrange(desc(PF)) |> mutate(pf_rank = row_number())

    # Per-owner best/worst stats for ranking
    owner_best_records <- rv$standings_data |>
      group_by(owner) |>
      summarise(best_wins = max(h2h_wins), .groups = "drop") |>
      arrange(desc(best_wins)) |> mutate(best_rec_rank = row_number())

    owner_worst_records <- rv$standings_data |>
      group_by(owner) |>
      summarise(worst_wins = min(h2h_wins), .groups = "drop") |>
      arrange(desc(worst_wins)) |> mutate(worst_rec_rank = row_number())

    owner_best_pf_season <- rv$standings_data |>
      group_by(owner) |>
      summarise(best_pf = max(points_for), .groups = "drop") |>
      arrange(desc(best_pf)) |> mutate(best_pf_rank = row_number())

    name_col_sched <- if ("team_owner" %in% names(rv$schedule_data)) "team_owner" else "franchise_name"
    owner_best_week <- rv$schedule_data |>
      group_by(!!sym(name_col_sched)) |>
      summarise(best_week = max(franchise_score, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(best_week)) |> mutate(best_wk_rank = row_number())

    owner_win_streaks <- rv$schedule_data |>
      arrange(!!sym(name_col_sched), season, week) |>
      group_by(!!sym(name_col_sched)) |>
      summarise(max_w = {s<-0;m<-0;for(r in result){if(!is.na(r)&&r=="W"){s<-s+1;m<-max(m,s)}else{s<-0}};m}, .groups = "drop") |>
      arrange(desc(max_w)) |> mutate(wstreak_rank = row_number())

    owner_lose_streaks <- rv$schedule_data |>
      arrange(!!sym(name_col_sched), season, week) |>
      group_by(!!sym(name_col_sched)) |>
      summarise(max_l = {s<-0;m<-0;for(r in result){if(!is.na(r)&&r=="L"){s<-s+1;m<-max(m,s)}else{s<-0}};m}, .groups = "drop") |>
      arrange(max_l) |> mutate(lstreak_rank = row_number())

    # Championships: ESPN league_rank == 1 is the playoff champion
    champs <- rv$standings_data |>
      filter(league_rank == 1) |>
      count(owner, name = "championships")

    # Championship appearances: league_rank 1 or 2 (made the title game)
    champ_appearances <- rv$standings_data |>
      filter(league_rank <= 2) |>
      count(owner, name = "appearances")

    # Sackos: worst regular season record (fewest wins, then lowest PF as tiebreaker)
    sackos <- rv$standings_data |>
      group_by(season) |>
      arrange(h2h_wins, points_for) |>
      slice_head(n = 1) |>
      ungroup() |>
      count(owner, name = "sackos")

    # Playoff appearances: league_rank <= 4 (top 4 make playoffs)
    playoff_data <- rv$standings_data |>
      filter(league_rank <= 4) |>
      select(season, owner) |>
      arrange(owner, season)

    # #1 seed: best regular season record (wins desc, PF tiebreaker)
    oneseed_data <- rv$standings_data |>
      group_by(season) |>
      arrange(desc(h2h_wins), desc(points_for)) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(season, owner)

    # GFFL trophy: most regular season points for
    gffl_pf_data <- rv$standings_data |>
      group_by(season) |>
      arrange(desc(points_for)) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(season, owner)

    owners <- sort(unique(rv$standings_data$owner))

    # Legacy owners (no longer in the league)
    legacy_owners <- c("Joe")
    active_owners <- setdiff(owners, legacy_owners)

    # Build owner stats for sorting
    playoff_counts <- playoff_data |> count(owner, name = "playoff_apps")
    owner_stats <- data.frame(owner = owners, stringsAsFactors = FALSE) |>
      left_join(champs, by = "owner") |>
      left_join(champ_appearances, by = "owner") |>
      left_join(playoff_counts, by = "owner") |>
      left_join(sackos, by = "owner") |>
      mutate(
        championships = ifelse(is.na(championships), 0, championships),
        appearances = ifelse(is.na(appearances), 0, appearances),
        playoff_apps = ifelse(is.na(playoff_apps), 0, playoff_apps),
        sackos = ifelse(is.na(sackos), 0, sackos)
      ) |>
      arrange(desc(championships), desc(appearances), desc(playoff_apps))

    active_sorted <- owner_stats |> filter(owner %in% active_owners) |> pull(owner)
    legacy_sorted <- owner_stats |> filter(owner %in% legacy_owners) |> pull(owner)

    # Function to build a trophy case for one owner
    build_owner_card <- function(o) {
      stats <- alltime |> filter(Team == o)
      n_champs <- if (o %in% champs$owner) champs$championships[champs$owner == o] else 0
      n_appear <- if (o %in% champ_appearances$owner) champ_appearances$appearances[champ_appearances$owner == o] else 0
      n_sackos <- if (o %in% sackos$owner) sackos$sackos[sackos$owner == o] else 0
      playoff_years <- playoff_data |> filter(owner == o) |> pull(season) |> sort()
      oneseed_years <- oneseed_data |> filter(owner == o) |> pull(season) |> sort()
      gffl_pf_years <- gffl_pf_data |> filter(owner == o) |> pull(season) |> sort()

      record <- paste0(stats$W, "-", stats$L)
      pf <- format(round(stats$PF, 0), big.mark = ",")

      # Per-owner season stats
      owner_seasons <- rv$standings_data |> filter(owner == o)
      best_season <- owner_seasons |> arrange(desc(h2h_wins), desc(points_for)) |> head(1)
      worst_season <- owner_seasons |> arrange(h2h_wins, points_for) |> head(1)
      most_pf_season <- owner_seasons |> arrange(desc(points_for)) |> head(1)

      best_record_str <- paste0(best_season$h2h_wins, "-", best_season$h2h_losses)
      worst_record_str <- paste0(worst_season$h2h_wins, "-", worst_season$h2h_losses)
      most_pf_str <- format(round(most_pf_season$points_for, 0), big.mark = ",")

      # Weekly best
      owner_weeks <- rv$schedule_data |> filter(.data[[name_col_sched]] == o)
      best_week <- owner_weeks |> arrange(desc(franchise_score)) |> head(1)
      most_pw_str <- if (nrow(best_week) > 0) paste0(round(best_week$franchise_score, 0)) else "N/A"

      # Streaks
      owner_games <- owner_weeks |> arrange(season, week)
      calc_streak <- function(result_val) {
        max_s <- 0; cur <- 0
        for (r in owner_games$result) {
          if (!is.na(r) && r == result_val) { cur <- cur + 1; max_s <- max(max_s, cur) }
          else { cur <- 0 }
        }
        max_s
      }
      win_streak_str <- as.character(calc_streak("W"))
      lose_streak_str <- as.character(calc_streak("L"))

      # Get ranks for each stat
      get_rank <- function(df, col, val) {
        r <- df[[col]][df[[1]] == val]
        if (length(r) == 0) 99 else r
      }
      best_rec_rank <- get_rank(owner_best_records, "best_rec_rank", o)
      worst_rec_rank <- get_rank(owner_worst_records, "worst_rec_rank", o)
      best_pf_rank <- get_rank(owner_best_pf_season, "best_pf_rank", o)
      best_wk_rank <- get_rank(owner_best_week, "best_wk_rank", o)
      wstreak_rank <- get_rank(owner_win_streaks, "wstreak_rank", o)
      lstreak_rank <- get_rank(owner_lose_streaks, "lstreak_rank", o)

      # Plaque colors based on ranking
      wp_rank <- winpct_ranked$winpct_rank[winpct_ranked$Team == o]
      pf_rank <- pf_ranked$pf_rank[pf_ranked$Team == o]

      get_plaque_style <- function(rank) {
        if (length(rank) == 0) rank <- 99
        if (rank <= 3) {
          # Gold
          list(
            bg = "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
            border = "#8b6914",
            text = "#3d2b0a",
            shadow = "rgba(212,168,75,0.3)"
          )
        } else if (rank <= 6) {
          # Silver
          list(
            bg = "linear-gradient(180deg, #a8a8a8, #d8d8d8, #b0b0b0, #888888)",
            border = "#666",
            text = "#2a2a2a",
            shadow = "rgba(150,150,150,0.3)"
          )
        } else {
          # Bronze
          list(
            bg = "linear-gradient(180deg, #a0714a, #cd8c5c, #b07848, #7a5430)",
            border = "#5c3a20",
            text = "#2a1a0a",
            shadow = "rgba(160,113,74,0.3)"
          )
        }
      }

      wp_style <- get_plaque_style(wp_rank)
      pf_style <- get_plaque_style(pf_rank)

      build_plaque <- function(label, value, style, wide = FALSE) {
        div(
          class = if (wide) "plaque-wide" else "plaque-stat",
          style = paste0(
            "padding:2px; margin:1px; min-width:0; ",
            if (wide) "width:100%; display:inline-block; " else "display:inline-block; ",
            "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
            "border-radius:4px; box-shadow: 0 2px 5px rgba(0,0,0,0.4);"
          ),
          div(
            style = paste0(
              "background:", style$bg, "; ",
              "border:1px solid ", style$border, "; ",
              "border-radius:3px; padding:4px 6px; text-align:center; min-width:0; ",
              "box-shadow: inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2), ",
              "0 1px 0 ", style$shadow, ";"
            ),
            if (!is.null(label)) div(
              class = "plaque-label",
              style = paste0(
                "font-family:Georgia,serif; text-transform:uppercase; ",
                "letter-spacing:0.5px; color:", style$text, "; opacity:0.75;"
              ), label),
            div(
              class = "plaque-value",
              style = paste0(
                "font-family:Georgia,serif; font-weight:bold; ",
                "color:", style$text, "; white-space:nowrap; ",
                "text-shadow: 0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2);"
              ), value)
          )
        )
      }

      # Name plaque style (always gold, matches photo width)
      name_style <- list(
        bg = "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
        border = "#8b6914", text = "#3d2b0a", shadow = "rgba(212,168,75,0.3)"
      )

      # Helper to get season info for tooltip
      get_season_tooltip <- function(yr, prefix = "") {
        s <- owner_seasons |> filter(season == yr)
        if (nrow(s) == 0) return("")
        rec <- paste0(s$h2h_wins, "-", s$h2h_losses)
        pos <- paste0("#", s$league_rank)
        paste0(prefix, yr, " | ", rec, " | ", pos)
      }

      # Championship trophies with tooltips
      champ_years <- rv$standings_data |> filter(owner == o, league_rank == 1) |> pull(season) |> sort()
      # Add pre-data championships
      if (o == "Connor") champ_years <- sort(unique(c(2016, champ_years)))
      lombardi_imgs <- if (length(champ_years) > 0) {
        paste(sapply(champ_years, function(yr) {
          tip <- get_season_tooltip(yr, "Champion ")
          paste0("<img src='photos/champion_ring.png' class='trophy-img lombardi-img' title='", tip, "'>")
        }), collapse = "")
      } else ""

      # Hunt trophies (title game appearances) with tooltips
      appear_years <- rv$standings_data |> filter(owner == o, league_rank <= 2) |> pull(season) |> sort()
      hunt_imgs <- if (length(appear_years) > 0) {
        paste(sapply(appear_years, function(yr) {
          tip <- get_season_tooltip(yr, "Title Game ")
          paste0("<img src='photos/Hunt.png' class='trophy-img hunt-img' title='", tip, "'>")
        }), collapse = "")
      } else ""

      # Sacko trophies with tooltips
      sacko_years <- rv$standings_data |>
        group_by(season) |> arrange(h2h_wins, points_for) |> slice_head(n = 1) |>
        ungroup() |> filter(owner == o) |> pull(season) |> sort()
      # Add pre-data sackos
      if (o == "Kerley") sacko_years <- sort(unique(c(2016, sacko_years)))
      sacko_imgs <- if (length(sacko_years) > 0 && file.exists("www/photos/sacko.png")) {
        paste(sapply(sacko_years, function(yr) {
          tip <- get_season_tooltip(yr, "Sacko ")
          paste0("<img src='photos/sacko.png' class='trophy-img sacko-img' title='", tip, "'>")
        }), collapse = "")
      } else ""

      # Combined banners with tooltips
      all_banner_years <- sort(unique(c(playoff_years, oneseed_years)))
      combined_banners <- ""
      if (length(all_banner_years) > 0) {
        banner_list <- sapply(all_banner_years, function(yr) {
          tip <- get_season_tooltip(yr, if (yr %in% oneseed_years) "#1 Seed " else "Playoffs ")
          if (yr %in% oneseed_years) {
            for (ext in c(".PNG", ".png", ".jpg", ".jpeg")) {
              f <- paste0("www/photos/oneseed_", yr, ext)
              if (file.exists(f)) return(paste0("<img src='photos/oneseed_", yr, ext, "' class='trophy-img banner-img' title='", tip, "'>"))
            }
          } else {
            for (ext in c(".PNG", ".png", ".jpg", ".jpeg")) {
              f <- paste0("www/photos/playoffs_", yr, ext)
              if (file.exists(f)) return(paste0("<img src='photos/playoffs_", yr, ext, "' class='trophy-img banner-img' title='", tip, "'>"))
            }
          }
          return("")
        })
        combined_banners <- paste(banner_list, collapse = "")
      }

      # GFFL trophies with tooltips
      gffl_imgs <- ""
      if (length(gffl_pf_years) > 0) {
        gffl_imgs <- paste(sapply(gffl_pf_years, function(yr) {
          tip <- get_season_tooltip(yr, "Most PF ")
          paste0("<img src='photos/GFFL.png' class='trophy-img gffl-img' title='", tip, "'>")
        }), collapse = "")
      }

      # MVP trophies (goes to #1 seed each year) - single mvp.png repeated per year
      mvp_imgs <- ""
      if (length(oneseed_years) > 0) {
        mvp_imgs <- paste(sapply(oneseed_years, function(yr) {
          tip <- get_season_tooltip(yr, "MVP ")
          paste0("<img src='photos/mvp.png' class='trophy-img mvp-img' title='", tip, "'>")
        }), collapse = "")
      }

      # Jersey image lookup
      jersey_file <- NULL
      for (name_variant in c(tolower(o), o)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          f <- file.path("www", "photos", paste0(name_variant, "_jersey", ext))
          if (file.exists(f)) {
            jersey_file <- paste0("photos/", name_variant, "_jersey", ext)
            break
          }
        }
        if (!is.null(jersey_file)) break
      }

      photo_file <- NULL
      for (name_variant in c(paste0(tolower(o), "_headshot"), tolower(o), o)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          if (file.exists(file.path("www", "photos", paste0(name_variant, ext)))) {
            photo_file <- paste0("photos/", name_variant, ext)
            break
          }
        }
        if (!is.null(photo_file)) break
      }

      shelf_style <- paste0(
        "border-bottom:3px solid rgba(255,255,255,0.15); ",
        "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
        "min-height:70px; display:flex; align-items:flex-end; justify-content:center; ",
        "flex-wrap:wrap; padding:4px 2px 6px;"
      )

      div(
        style = paste0(
          "background: linear-gradient(180deg, #1a1210 0%, #2a1f18 30%, #1a1210 100%); ",
          "border:3px solid #3d2b1a; ",
          "border-radius:6px; ",
          "box-shadow: inset 0 0 30px rgba(0,0,0,0.5), 0 4px 15px rgba(0,0,0,0.5); ",
          "overflow:hidden; position:relative;"
        ),

        div(style = paste0(
          "position:absolute; top:0; left:0; right:0; bottom:0; ",
          "background: linear-gradient(135deg, rgba(255,255,255,0.06) 0%, transparent 50%, rgba(255,255,255,0.03) 100%); ",
          "pointer-events:none; z-index:1;"
        )),

        # Top shelf: photo+nameplate (left) | jersey (right)
        div(
          class = "photo-shelf-row",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "padding:10px 6px 8px;"
          ),
          # Left: photo + name plaque
          div(
            style = "flex:1; display:flex; flex-direction:column; align-items:center; justify-content:center;",
            if (!is.null(photo_file)) {
              div(
                style = "position:relative; flex-shrink:0;",
                class = "owner-photo-frame",
                div(
                  style = "position:absolute; top:10%; left:10%; width:80%; height:80%; overflow:hidden;",
                  tags$img(src = photo_file,
                           style = paste0("width:100%; height:100%; object-fit:cover; object-position:",
                                          if (o == "Joe") "50% 20%" else "top", ";"))
                ),
                tags$img(src = "photos/frame.PNG",
                         style = "position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none;")
              )
            } else {
              div(
                class = "owner-photo-frame",
                style = "display:flex; align-items:center; justify-content:center; flex-shrink:0; background:#2a1f18;",
                tags$span(style = "color:#555; font-size:3rem;", icon("user"))
              )
            },
            div(
              style = "width:100%; max-width:140px; margin-top:4px;",
              build_plaque(NULL, o, name_style, wide = TRUE)
            )
          ),
          # Right: jersey
          div(
            style = "flex:1; display:flex; align-items:center; justify-content:center;",
            if (!is.null(jersey_file)) {
              tags$img(src = jersey_file, class = "jersey-img")
            } else {
              div(
                style = "color:#555; font-size:11px; text-align:center; font-style:italic;",
                "Jersey",
                tags$br(),
                "coming soon"
              )
            }
          )
        ),

        # Lombardi / Hunt split shelf
        div(
          class = "trophy-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; padding:4px 2px 6px; overflow:hidden; position:relative;"
          ),
          div(class = "shelf-label shelf-label-left", "CHAMPIONSHIPS"),
          div(class = "shelf-label shelf-label-right", "APPEARANCES"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:nowrap; overflow:hidden; position:relative; z-index:2;",
            HTML(lombardi_imgs)
          ),
          div(style = "width:1px; background:rgba(255,255,255,0.12); align-self:stretch; margin:2px 1px; flex-shrink:0; z-index:2;"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:nowrap; overflow:hidden; position:relative; z-index:2;",
            HTML(hunt_imgs)
          )
        ),

        # MVP (left) | GFFL (right) split shelf
        div(
          class = "trophy-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; padding:4px 4px 6px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-left", "BEST RECORD"),
          div(class = "shelf-label shelf-label-right", "MOST POINTS"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; position:relative; z-index:2;",
            HTML(mvp_imgs)
          ),
          div(style = "width:2px; background:rgba(255,255,255,0.12); align-self:stretch; margin:4px 2px; z-index:2;"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; position:relative; z-index:2;",
            HTML(gffl_imgs)
          )
        ),

        # Playoff banners shelf (full width, no divider)
        div(
          class = "banner-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:center; justify-content:center; flex-wrap:wrap; padding:4px 4px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-full", "PLAYOFF APPEARANCES"),
          div(style = "position:relative; z-index:2; display:flex; flex-wrap:wrap; justify-content:center;",
              HTML(combined_banners))
        ),

        # Sackos shelf (full width)
        div(
          class = "sacko-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; padding:4px 4px 6px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-full", "SACKO LAPOS"),
          div(style = "position:relative; z-index:2; display:flex; flex-wrap:wrap; justify-content:center; align-items:flex-end;",
              HTML(sacko_imgs))
        ),

        # Plaques shelf (bottom, full width - always 6 in one row)
        div(
          class = "plaque-shelf",
          style = paste0(
            "background: linear-gradient(180deg, #1a1210 0%, #2a1f18 50%, #1a1210 100%); ",
            "display:flex; align-items:center; justify-content:space-around; flex-wrap:nowrap; ",
            "padding:6px 2px; border-top:2px solid #8b6914; overflow:hidden;"
          ),
          build_plaque("Record", record, wp_style),
          build_plaque("Points", pf, pf_style),
          build_plaque("Best", best_record_str, get_plaque_style(best_rec_rank)),
          build_plaque("Pts (S)", most_pf_str, get_plaque_style(best_pf_rank)),
          build_plaque("Pts (W)", most_pw_str, get_plaque_style(best_wk_rank)),
          build_plaque("Streak", win_streak_str, get_plaque_style(wstreak_rank))
        )
      )
    }

    # Ghost card - for owners without ESPN data (Sean, Kenny, Xing Wei)
    # fill_all = TRUE fills every shelf with fill_image (for Honorary owners)
    build_ghost_card <- function(o, fill_all = FALSE, fill_image = NULL) {
      # Define plaque helpers locally (they're scoped inside build_owner_card)
      build_plaque <- function(label, value, style, wide = FALSE) {
        div(
          class = if (wide) "plaque-wide" else "plaque-stat",
          style = paste0(
            "padding:2px; margin:1px; min-width:0; ",
            if (wide) "width:100%; display:inline-block; " else "display:inline-block; ",
            "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
            "border-radius:4px; box-shadow: 0 2px 5px rgba(0,0,0,0.4);"
          ),
          div(
            style = paste0(
              "background:", style$bg, "; ",
              "border:1px solid ", style$border, "; ",
              "border-radius:3px; padding:4px 6px; text-align:center; min-width:0; ",
              "box-shadow: inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2), ",
              "0 1px 0 ", style$shadow, ";"
            ),
            if (!is.null(label)) div(
              class = "plaque-label",
              style = paste0(
                "font-family:Georgia,serif; text-transform:uppercase; ",
                "letter-spacing:0.5px; color:", style$text, "; opacity:0.75;"
              ), label),
            div(
              class = "plaque-value",
              style = paste0(
                "font-family:Georgia,serif; font-weight:bold; ",
                "color:", style$text, "; white-space:nowrap; ",
                "text-shadow: 0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2);"
              ), value)
          )
        )
      }
      name_style <- list(
        bg = "linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828)",
        border = "#8b6914", text = "#3d2b0a", shadow = "rgba(212,168,75,0.3)"
      )
      bronze_style <- list(
        bg = "linear-gradient(180deg, #a0714a, #cd8c5c, #b07848, #7a5430)",
        border = "#5c3a20", text = "#2a1a0a", shadow = "rgba(160,113,74,0.3)"
      )
      question_plaque <- function(label) build_plaque(label, "???", bronze_style)

      # Find headshot (try with and without spaces)
      o_clean <- gsub(" ", "", tolower(o))
      photo_file <- NULL
      for (name_variant in c(paste0(o_clean, "_headshot"), paste0(tolower(o), "_headshot"), o_clean, tolower(o), o)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          if (file.exists(file.path("www", "photos", paste0(name_variant, ext)))) {
            photo_file <- paste0("photos/", name_variant, ext)
            break
          }
        }
        if (!is.null(photo_file)) break
      }

      # Find jersey (try with and without spaces)
      jersey_file <- NULL
      for (name_variant in c(o_clean, tolower(o), o)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          f <- file.path("www", "photos", paste0(name_variant, "_jersey", ext))
          if (file.exists(f)) {
            jersey_file <- paste0("photos/", name_variant, "_jersey", ext)
            break
          }
        }
        if (!is.null(jersey_file)) break
      }

      # Fill images for Honorary cards
      fill_html <- ""
      if (fill_all && !is.null(fill_image)) {
        fill_html <- paste(rep(paste0("<img src='", fill_image, "' class='trophy-img lombardi-img'>"), 3), collapse = "")
      }

      div(
        style = paste0(
          "background: linear-gradient(180deg, #1a1210 0%, #2a1f18 30%, #1a1210 100%); ",
          "border:3px solid #3d2b1a; ",
          "border-radius:6px; ",
          "box-shadow: inset 0 0 30px rgba(0,0,0,0.5), 0 4px 15px rgba(0,0,0,0.5); ",
          "overflow:hidden; position:relative;"
        ),
        div(style = paste0(
          "position:absolute; top:0; left:0; right:0; bottom:0; ",
          "background: linear-gradient(135deg, rgba(255,255,255,0.06) 0%, transparent 50%, rgba(255,255,255,0.03) 100%); ",
          "pointer-events:none; z-index:1;"
        )),

        # Top shelf: photo + jersey
        div(
          class = "photo-shelf-row",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "padding:10px 6px 8px;"
          ),
          div(
            style = "flex:1; display:flex; flex-direction:column; align-items:center; justify-content:center;",
            if (!is.null(photo_file)) {
              div(
                style = "position:relative; flex-shrink:0;",
                class = "owner-photo-frame",
                div(
                  style = "position:absolute; top:10%; left:10%; width:80%; height:80%; overflow:hidden;",
                  tags$img(src = photo_file,
                           style = "width:100%; height:100%; object-fit:cover; object-position:top;")
                ),
                tags$img(src = "photos/frame.PNG",
                         style = "position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none;")
              )
            } else {
              div(
                class = "owner-photo-frame",
                style = "display:flex; align-items:center; justify-content:center; flex-shrink:0; background:#2a1f18;",
                tags$span(style = "color:#555; font-size:3rem;", icon("user"))
              )
            },
            div(
              style = "width:100%; max-width:140px; margin-top:4px;",
              build_plaque(NULL, o, name_style, wide = TRUE)
            )
          ),
          div(
            style = "flex:1; display:flex; align-items:center; justify-content:center;",
            if (!is.null(jersey_file)) {
              tags$img(src = jersey_file, class = "jersey-img")
            } else {
              div(
                style = "color:#555; font-size:11px; text-align:center; font-style:italic;",
                "Jersey", tags$br(), "coming soon"
              )
            }
          )
        ),

        # Lombardi / Hunt split shelf
        div(
          class = "trophy-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; padding:4px 2px 6px; overflow:hidden; position:relative;"
          ),
          div(class = "shelf-label shelf-label-left", "CHAMPIONSHIPS"),
          div(class = "shelf-label shelf-label-right", "APPEARANCES"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:nowrap; overflow:hidden; position:relative; z-index:2;",
            HTML(fill_html)
          ),
          div(style = "width:1px; background:rgba(255,255,255,0.12); align-self:stretch; margin:2px 1px; flex-shrink:0; z-index:2;"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:nowrap; overflow:hidden; position:relative; z-index:2;",
            HTML(fill_html)
          )
        ),

        # MVP / GFFL split shelf
        div(
          class = "trophy-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; padding:4px 4px 6px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-left", "BEST RECORD"),
          div(class = "shelf-label shelf-label-right", "MOST POINTS"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; position:relative; z-index:2;",
            HTML(fill_html)
          ),
          div(style = "width:2px; background:rgba(255,255,255,0.12); align-self:stretch; margin:4px 2px; z-index:2;"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; position:relative; z-index:2;",
            HTML(fill_html)
          )
        ),

        # Playoff banners shelf
        div(
          class = "banner-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:center; justify-content:center; flex-wrap:wrap; padding:4px 4px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-full", "PLAYOFF APPEARANCES"),
          div(style = "position:relative; z-index:2;",
              HTML(fill_html))
        ),

        # Sacko shelf
        div(
          class = "sacko-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; padding:4px 4px 6px; position:relative;"
          ),
          div(class = "shelf-label shelf-label-full", "SACKO LAPOS"),
          div(style = "position:relative; z-index:2;",
              HTML(fill_html))
        ),

        # Plaques - all ???
        div(
          class = "plaque-shelf",
          style = paste0(
            "background: linear-gradient(180deg, #1a1210 0%, #2a1f18 50%, #1a1210 100%); ",
            "display:flex; align-items:center; justify-content:space-around; flex-wrap:nowrap; ",
            "padding:6px 2px; border-top:2px solid #8b6914; overflow:hidden;"
          ),
          question_plaque("Record"),
          question_plaque("Points"),
          question_plaque("Best"),
          question_plaque("Pts (S)"),
          question_plaque("Pts (W)"),
          question_plaque("Streak")
        )
      )
    }

    # Build active owner cards (sorted by championships desc, sackos asc)
    active_cards <- lapply(active_sorted, build_owner_card)

    # Build legacy owner cards (existing + ghost cards for Sean/Kenny)
    legacy_cards <- c(
      lapply(legacy_sorted, build_owner_card),
      list(build_ghost_card("Sean")),
      list(build_ghost_card("Kenny"))
    )

    # Build honorary cards (Xing Wei with centurion trophies)
    honorary_cards <- list(
      build_ghost_card("Xing Wei", fill_all = TRUE, fill_image = "photos/centurion_trophy.png")
    )

    div(
      style = "background:#e8e0d4; padding:20px; border-radius:12px;",
      tags$style("
        .trophy-grid { display:grid; grid-template-columns: repeat(3, 1fr); gap:12px; }
        @media (max-width:768px) { .trophy-grid { grid-template-columns: 1fr; gap:12px; } }
        .photo-shelf-row { display:flex; align-items:center; justify-content:space-around; }

        /* Background shelf labels - subtle engraved look at top of shelf */
        .shelf-label {
          position:absolute;
          top:2px;
          font-family:Georgia,serif;
          font-weight:bold;
          letter-spacing:2px;
          color:rgba(100,100,110,0.25);
          text-shadow: 0 1px 0 rgba(255,255,255,0.05), 0 -1px 0 rgba(0,0,0,0.5);
          text-transform:uppercase;
          pointer-events:none;
          z-index:1;
          white-space:nowrap;
          overflow:hidden;
          text-overflow:clip;
          font-size:9px;
        }
        .shelf-label-left { left:2px; right:50%; text-align:center; padding:0 2px; }
        .shelf-label-right { right:2px; left:50%; text-align:center; padding:0 2px; }
        .shelf-label-full { left:0; right:0; text-align:center; padding:0 2px; }

        @media (min-width:769px) {
          .shelf-label { font-size:12px; letter-spacing:3px; }
        }

        /* Mobile sizes */
        .trophy-img { object-fit:contain; }
        .lombardi-img { height:52px; margin:0 5px; }
        .hunt-img { height:40px; margin:0 4px; }
        .sacko-img { height:40px; width:32px; margin:0 1px; }
        .banner-img { height:50px; margin:2px; }
        .gffl-img { height:45px; margin:0 5px; }
        .mvp-img { height:30px; margin:0 2px; }
        .jersey-img { max-width:100%; max-height:150px; object-fit:contain; }

        .trophy-shelf { height:65px; }
        .banner-shelf { height:65px; }
        .sacko-shelf { height:65px; }
        .plaque-shelf { min-height:40px; padding:4px; }
        .owner-photo-frame { width:110px; height:130px; }

        /* Plaques scale to fit */
        .plaque-stat { flex:1 1 0; min-width:0; }
        .plaque-stat > div { padding:3px 3px !important; }
        .plaque-label { font-size:7px; }
        .plaque-value { font-size:11px; }

        /* Desktop sizes */
        @media (min-width:769px) {
          .lombardi-img { height:60px; margin:0 5px; }
          .hunt-img { height:50px; margin:0 5px; }
          .sacko-img { height:65px; width:50px; margin:0 3px; }
          .banner-img { height:75px; margin:3px; }
          .gffl-img { height:65px; margin:0 6px; }
          .mvp-img { height:42px; margin:0 3px; }
          .jersey-img { max-height:180px; }

          .trophy-shelf { height:90px; }
          .banner-shelf { height:95px; }
          .sacko-shelf { height:90px; }
          .owner-photo-frame { width:140px; height:170px; }

          .plaque-stat > div { padding:5px 6px !important; }
          .plaque-label { font-size:9px; }
          .plaque-value { font-size:14px; }
        }
      "),
      div(
        class = "trophy-grid",
        active_cards
      ),
      if (length(legacy_cards) > 0) {
        tagList(
          tags$hr(style = "border-color:#aaa;"),
          h4(style = "color:#666; text-align:center; margin-top:16px; margin-bottom:12px;",
             icon("clock-rotate-left"), " Legacy Owners"),
          div(
            class = "trophy-grid",
            legacy_cards
          )
        )
      },
      if (length(honorary_cards) > 0) {
        tagList(
          tags$hr(style = "border-color:#aaa;"),
          h4(style = "color:#666; text-align:center; margin-top:16px; margin-bottom:12px;",
             icon("award"), " Honorary"),
          div(
            class = "trophy-grid",
            honorary_cards
          )
        )
      }
    )
  })

  # ==========================================================================
  # HALL OF FAME TAB
  # ==========================================================================

  output$hof_gallery <- renderUI({
    req(rv$standings_data)

    # Get champion for each season using league_rank == 1
    champ_rows <- rv$standings_data |>
      filter(league_rank == 1) |>
      arrange(season)

    # Get championship rosters if starters data available
    champ_rosters <- NULL
    if (!is.null(rv$starters_data) && nrow(rv$starters_data) > 0) {
      champ_rosters <- get_championship_rosters(
        rv$starters_data, rv$standings_data,
        if (!is.null(rv$schedule_data)) rv$schedule_data else readRDS("data/schedule.rds")
      )
    }

    bust_cards <- lapply(seq_len(nrow(champ_rows)), function(i) {
      row <- champ_rows[i, ]
      owner <- row$owner
      yr <- row$season
      team_name <- trimws(row$franchise_name)
      record <- paste0(row$h2h_wins, "-", row$h2h_losses)

      # Check for bust image (bust2 first, then bust, lowercase and capitalized)
      bust_file <- NULL
      photo_file <- NULL
      for (bust_suffix in c("_bust2", "_bust")) {
        for (name_variant in c(tolower(owner), owner)) {
          for (bust_ext in c(".PNG", ".png", ".jpg", ".jpeg")) {
            if (file.exists(file.path("www", "photos", paste0(name_variant, bust_suffix, bust_ext)))) {
              bust_file <- paste0("photos/", name_variant, bust_suffix, bust_ext)
              break
            }
          }
          if (!is.null(bust_file)) break
        }
        if (!is.null(bust_file)) break
      }
      # Check for regular photo (headshot first, then name)
      for (name_variant in c(paste0(tolower(owner), "_headshot"), tolower(owner), owner)) {
        for (ext in c(".png", ".jpg", ".jpeg")) {
          if (file.exists(file.path("www", "photos", paste0(name_variant, ext)))) {
            photo_file <- paste0("photos/", name_variant, ext)
            break
          }
        }
        if (!is.null(photo_file)) break
      }

      has_bust <- !is.null(bust_file)

      # Build championship roster dropdown
      roster_html <- NULL
      if (!is.null(champ_rosters)) {
        roster <- champ_rosters |>
          filter(season == yr) |>
          arrange(match(pos, c("QB", "RB", "WR", "TE", "K", "DST", "D/ST", "DEF")), desc(player_score))

        if (nrow(roster) > 0) {
          roster_rows <- lapply(seq_len(nrow(roster)), function(j) {
            p <- roster[j, ]
            headshot_url <- NULL
            if (!is.null(rv$player_ids)) {
              match_row <- rv$player_ids |>
                filter(tolower(name) == tolower(p$player_name)) |>
                head(1)
              if (nrow(match_row) > 0 && "espn_id" %in% names(match_row) && !is.na(match_row$espn_id)) {
                headshot_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",
                                       match_row$espn_id, ".png&w=96&h=70&cb=1")
              }
            }

            tags$tr(
              tags$td(
                style = "width:40px; padding:3px;",
                if (!is.null(headshot_url)) {
                  tags$img(src = headshot_url,
                           style = paste0(
                             "width:32px; height:32px; border-radius:50%; object-fit:cover; ",
                             "filter: sepia(0.7) saturate(0.5) brightness(0.9) contrast(1.1); ",
                             "border:2px solid #8b6914;"
                           ),
                           onerror = "this.style.display='none'")
                }
              ),
              tags$td(
                style = "color:#d4a84b; font-size:11px; text-align:left; padding:3px;",
                tags$span(style = "font-weight:bold;", p$player_name),
                tags$br(),
                tags$span(style = "color:#8b6914; font-size:9px;",
                          paste0(p$pos, " - ", p$team))
              ),
              tags$td(
                style = "color:#d4a84b; font-weight:bold; font-size:12px; text-align:right; padding:3px;",
                round(p$player_score, 0)
              )
            )
          })

          total_score <- round(sum(roster$player_score, na.rm = TRUE), 0)

          roster_html <- div(
            style = "margin-top:6px;",
            tags$details(
              style = "cursor:pointer;",
              tags$summary(
                style = "color:#d4a84b; font-family:Georgia,serif; font-size:11px; text-align:center; list-style:none; padding:4px;",
                tags$span(style = "border-bottom:1px solid #c9a84c;", "View Roster")
              ),
              div(
                style = paste0(
                  "margin-top:4px; padding:4px; background:#0a0a1a; ",
                  "border:1px solid #c9a84c; border-radius:4px;"
                ),
                tags$table(
                  style = "width:100%; border-collapse:collapse;",
                  roster_rows,
                  tags$tr(
                    style = "border-top:1px solid #c9a84c;",
                    tags$td(style = "padding:3px;"),
                    tags$td(style = "color:#d4a84b; font-weight:bold; font-size:11px; text-align:left; padding:3px;",
                            "TOTAL"),
                    tags$td(style = "color:#d4a84b; font-weight:bold; font-size:13px; text-align:right; padding:3px;",
                            total_score)
                  )
                )
              )
            )
          )
        }
      }

      # Draft results dropdown
      draft_html <- NULL
      champ_fid <- row$franchise_id
      if (!is.null(rv$draft_data) && !is.null(rv$starters_data)) {
        champ_draft <- rv$draft_data |>
          filter(season == yr, franchise_id == champ_fid) |>
          arrange(round, pick)

        if (nrow(champ_draft) > 0) {
          # Compute starter stats per drafted player
          owner_starters <- rv$starters_data |>
            filter(season == yr, franchise_id == champ_fid,
                   !lineup_slot %in% c("BE", "IR"))

          champ_week <- max(rv$schedule_data |> filter(season == yr) |> pull(week), na.rm = TRUE)

          draft_rows <- lapply(seq_len(nrow(champ_draft)), function(j) {
            d <- champ_draft[j, ]
            player_starts <- owner_starters |> filter(player_name == d$player_name)
            weeks_started <- nrow(player_starts)
            total_pts <- round(sum(player_starts$player_score, na.rm = TRUE), 0)
            champ_game_pts <- round(sum(player_starts$player_score[player_starts$week == champ_week], na.rm = TRUE), 0)

            headshot_url <- NULL
            if (!is.null(rv$player_ids)) {
              match_row <- rv$player_ids |>
                filter(tolower(name) == tolower(d$player_name)) |>
                head(1)
              if (nrow(match_row) > 0 && "espn_id" %in% names(match_row) && !is.na(match_row$espn_id)) {
                headshot_url <- paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",
                                       match_row$espn_id, ".png&w=96&h=70&cb=1")
              }
            }

            tags$tr(
              tags$td(
                style = "color:#8b6914; font-size:9px; padding:3px; width:22px;",
                paste0("R", d$round)
              ),
              tags$td(
                style = "width:36px; padding:3px;",
                if (!is.null(headshot_url)) {
                  tags$img(src = headshot_url,
                           style = paste0(
                             "width:28px; height:28px; border-radius:50%; object-fit:cover; ",
                             "filter: sepia(0.7) saturate(0.5) brightness(0.9) contrast(1.1); ",
                             "border:2px solid #8b6914;"
                           ),
                           onerror = "this.style.display='none'")
                }
              ),
              tags$td(
                style = "color:#d4a84b; font-size:10px; text-align:left; padding:3px;",
                tags$span(style = "font-weight:bold;", d$player_name),
                tags$br(),
                tags$span(style = "color:#8b6914; font-size:8px;",
                          paste0(d$pos, " - ", d$team))
              ),
              tags$td(
                style = "color:#d4a84b; font-size:10px; text-align:center; padding:3px;",
                weeks_started
              ),
              tags$td(
                style = "color:#d4a84b; font-size:10px; text-align:right; padding:3px;",
                total_pts
              ),
              tags$td(
                style = "color:#d4a84b; font-size:10px; text-align:right; padding:3px;",
                if (champ_game_pts > 0) champ_game_pts else "-"
              )
            )
          })

          draft_html <- div(
            style = "margin-top:4px;",
            tags$details(
              style = "cursor:pointer;",
              tags$summary(
                style = "color:#d4a84b; font-family:Georgia,serif; font-size:11px; text-align:center; list-style:none; padding:4px;",
                tags$span(style = "border-bottom:1px solid #c9a84c;", "View Draft")
              ),
              div(
                style = paste0(
                  "margin-top:4px; padding:4px; background:#0a0a1a; ",
                  "border:1px solid #c9a84c; border-radius:4px;"
                ),
                tags$table(
                  style = "width:100%; border-collapse:collapse;",
                  tags$thead(
                    tags$tr(
                      style = "border-bottom:1px solid #c9a84c;",
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:left;", "Rd"),
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:left;", ""),
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:left;", "Player"),
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:center;", "Wks"),
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:right;", "Pts"),
                      tags$th(style = "color:#8b6914; font-size:8px; padding:2px; text-align:right;", "Chp")
                    )
                  ),
                  tags$tbody(draft_rows)
                )
              )
            )
          )
        }
      }

      div(
        class = "d-inline-block text-center mb-4",
        style = "width:48%; min-width:160px; max-width:220px; vertical-align:top; margin:0 1%;",

        # Elegant Canton alcove with warm spotlight
        div(
          style = paste0(
            "width:190px; margin:0 auto; ",
            "background: radial-gradient(ellipse at 50% 0%, #3a2a1a 0%, #1a1008 40%, #0a0804 100%); ",
            "border-radius:50% 50% 0 0 / 20% 20% 0 0; ",
            "padding:20px 10px 0; ",
            "box-shadow: inset 0 20px 60px rgba(212,168,75,0.12), ",
            "inset 0 -10px 20px rgba(0,0,0,0.8); ",
            "border:1px solid #2a1f14; border-bottom:none;"
          ),

          # Spotlight glow behind bust
          div(
            style = paste0(
              "position:relative; ",
              "background: radial-gradient(ellipse at 50% 40%, rgba(212,168,75,0.1) 0%, transparent 70%); ",
              "padding:10px 0;"
            ),

            if (has_bust) {
              tags$img(
                src = bust_file,
                style = paste0(
                  "width:160px; margin:0 auto; display:block; ",
                  "filter: drop-shadow(0 5px 20px rgba(212,168,75,0.25)) ",
                  "drop-shadow(0 2px 6px rgba(0,0,0,0.5));"
                )
              )
            } else if (!is.null(photo_file)) {
              div(
                style = paste0(
                  "width:120px; height:150px; margin:0 auto; ",
                  "border-radius:50% 50% 45% 45% / 55% 55% 45% 45%; ",
                  "overflow:hidden; padding:5px; ",
                  "background: radial-gradient(ellipse at 30% 30%, #d4a84b, #8b6914, #5c4413); ",
                  "box-shadow: 0 5px 20px rgba(212,168,75,0.2);"
                ),
                tags$img(
                  src = photo_file,
                  style = paste0(
                    "width:100%; height:100%; ",
                    "object-fit:cover; object-position:top; ",
                    "border-radius:50% 50% 40% 40% / 55% 55% 45% 45%; ",
                    "filter: sepia(0.8) saturate(0.5) brightness(0.85) contrast(1.1);"
                  )
                )
              )
            } else {
              div(
                style = "width:120px; height:150px; margin:0 auto; display:flex; align-items:center; justify-content:center;",
                tags$span(style = "color:#3d2b1a; font-size:4rem;", icon("user"))
              )
            }
          )
        ),

        # Marble pedestal with plaques
        div(
          style = paste0(
            "width:190px; margin:0 auto; ",
            "background: linear-gradient(180deg, #2a2018, #1a1510 30%, #0f0c08 70%, #1a1510); ",
            "border:1px solid #2a1f14; border-top:3px solid #8b6914; ",
            "border-radius:0 0 6px 6px; ",
            "padding:10px 8px; ",
            "box-shadow: 0 8px 25px rgba(0,0,0,0.6);"
          ),
          # Name plaque - metallic gold with black engraved text
          div(
            style = paste0(
              "padding:2px; margin-bottom:5px; ",
              "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
              "border-radius:4px; box-shadow: 0 2px 5px rgba(0,0,0,0.4);"
            ),
            div(
              style = paste0(
                "background: linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828); ",
                "border:1px solid #8b6914; border-radius:3px; ",
                "padding:6px 10px; text-align:center; ",
                "box-shadow: inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2);"
              ),
              div(
                style = "color:#3d2b0a; font-family:Georgia,serif; font-weight:bold; font-size:15px; letter-spacing:2px; text-transform:uppercase; text-shadow: 0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2);",
                owner
              )
            )
          ),
          # Year plaque - metallic gold with black engraved text
          div(
            style = paste0(
              "padding:2px; ",
              "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
              "border-radius:4px; box-shadow: 0 2px 5px rgba(0,0,0,0.4);"
            ),
            div(
              style = paste0(
                "background: linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828); ",
                "border:1px solid #8b6914; border-radius:3px; ",
                "padding:5px 10px; text-align:center; ",
                "box-shadow: inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2);"
              ),
              div(
                style = "color:#3d2b0a; font-family:Georgia,serif; font-weight:bold; font-size:18px; letter-spacing:3px; text-shadow: 0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2);",
                yr
              )
            )
          )
        ),

        # Championship roster dropdown
        roster_html,
        # Draft results dropdown
        draft_html
      )
    })

    div(
      style = paste0(
        "padding:20px; background: linear-gradient(180deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%); ",
        "border-radius:12px; min-height:400px; ",
        "display:flex; flex-wrap:wrap; justify-content:center; align-items:flex-start; gap:16px;"
      ),
      bust_cards
    )
  })

  # ==========================================================================
  # STANDINGS TAB
  # ==========================================================================

  # Dynamic title
  output$standings_title <- renderText({
    if (is.null(input$standings_season) || input$standings_season == "All-Time") {
      "All-Time Standings"
    } else {
      paste0(input$standings_season, " Standings")
    }
  })

  output$box_plot_title <- renderText({
    if (is.null(input$standings_season) || input$standings_season == "All-Time") {
      "All-Time Weekly Score Distribution"
    } else {
      paste0(input$standings_season, " Weekly Score Distribution")
    }
  })

  output$alltime_standings_table <- renderDT({
    req(rv$standings_data)

    # Filter by season if applicable
    if (!is.null(input$standings_season) && input$standings_season != "All-Time") {
      yr <- as.integer(input$standings_season)
      season_data <- rv$standings_data |> filter(season == yr) |> arrange(desc(h2h_wins), desc(points_for))
      df <- season_data |>
        mutate(Rank = row_number(),
               `Win%` = h2h_wins / (h2h_wins + h2h_losses + ifelse(is.na(h2h_ties), 0, h2h_ties)),
               `PF/G` = points_for / (h2h_wins + h2h_losses + ifelse(is.na(h2h_ties), 0, h2h_ties))) |>
        select(Rank, Team = owner, W = h2h_wins, L = h2h_losses,
               `Win%`, PF = points_for, PA = points_against, `PF/G`)
    } else {
      df <- compute_alltime_standings(rv$standings_data)
    }

    # Color breaks for conditional formatting
    w_breaks <- quantile(df$W, probs = c(0.33, 0.66), na.rm = TRUE)
    pf_breaks <- quantile(df$PF, probs = c(0.33, 0.66), na.rm = TRUE)
    pa_breaks <- quantile(df$PA, probs = c(0.33, 0.66), na.rm = TRUE)
    ppg_breaks <- quantile(df$`PF/G`, probs = c(0.33, 0.66), na.rm = TRUE)

    # Handle case where all values are same (ties in breaks)
    safe_breaks <- function(b) {
      if (length(unique(b)) < 2) c(b[1] - 0.001, b[1] + 0.001) else b
    }
    w_breaks <- safe_breaks(w_breaks)
    pf_breaks <- safe_breaks(pf_breaks)
    pa_breaks <- safe_breaks(pa_breaks)
    ppg_breaks <- safe_breaks(ppg_breaks)

    dt <- datatable(
      df,
      options = list(
        pageLength = 20,
        dom = "t",
        order = list(list(2, "desc")),
        columnDefs = list(
          list(width = "70px", targets = 0),
          list(className = "dt-center", targets = 1:(ncol(df)-1))
        )
      ),
      rownames = FALSE
    ) |>
      formatPercentage("Win%", digits = 1) |>
      formatRound(c("PF", "PA", "PF/G"), 1) |>
      formatStyle("W",
        background = styleInterval(w_breaks,
          c("rgba(220,53,69,0.2)", "rgba(255,193,7,0.2)", "rgba(40,167,69,0.2)"))) |>
      formatStyle("PF",
        background = styleInterval(pf_breaks,
          c("rgba(220,53,69,0.2)", "rgba(255,193,7,0.2)", "rgba(40,167,69,0.2)"))) |>
      formatStyle("PA",
        background = styleInterval(pa_breaks,
          c("rgba(40,167,69,0.2)", "rgba(255,193,7,0.2)", "rgba(220,53,69,0.2)"))) |>
      formatStyle("PF/G",
        background = styleInterval(ppg_breaks,
          c("rgba(220,53,69,0.2)", "rgba(255,193,7,0.2)", "rgba(40,167,69,0.2)")))

    dt
  })

  output$wins_by_season_plot <- renderPlotly({
    req(rv$standings_data)
    p <- rv$standings_data |>
      ggplot(aes(x = factor(season), y = h2h_wins, fill = owner)) +
      geom_col(position = "dodge") +
      labs(x = "Season", y = "Wins", fill = "Owner") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
    ggplotly(p, tooltip = c("fill", "y")) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$points_by_season_plot <- renderPlotly({
    req(rv$standings_data)
    p <- rv$standings_data |>
      ggplot(aes(x = factor(season), y = points_for, fill = owner)) +
      geom_col(position = "dodge") +
      labs(x = "Season", y = "Points For", fill = "Owner") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
    ggplotly(p, tooltip = c("fill", "y")) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # ==========================================================================
  # MATCHUPS TAB
  # ==========================================================================

  filtered_schedule <- reactive({
    req(rv$schedule_data, input$matchup_season)
    df <- rv$schedule_data |>
      filter(season == as.integer(input$matchup_season))
    if (input$matchup_type != "All" && "game_type" %in% names(df)) {
      df <- df |> filter(game_type == input$matchup_type)
    }
    df
  })

  output$matchups_table <- renderDT({
    sched <- filtered_schedule() |>
      select(
        Week = week,
        Type = game_type,
        Owner = team_owner,
        Score = franchise_score,
        Opponent = opponent_owner,
        `Opp Score` = opponent_score,
        Result = result
      ) |>
      arrange(Week, Owner)

    datatable(sched, options = list(pageLength = 25, dom = "ftp"),
              rownames = FALSE, filter = "top") |>
      formatRound(c("Score", "Opp Score"), 2)
  })

  output$top_scores_table <- renderDT({
    req(rv$schedule_data)
    top <- rv$schedule_data |>
      arrange(desc(franchise_score)) |>
      head(25) |>
      select(Season = season, Week = week, Owner = team_owner,
             Score = franchise_score, Opponent = opponent_owner)
    datatable(top, options = list(pageLength = 10, dom = "tp"), rownames = FALSE) |>
      formatRound("Score", 2)
  })

  output$blowouts_table <- renderDT({
    req(rv$schedule_data)
    blowouts <- rv$schedule_data |>
      mutate(margin = franchise_score - opponent_score) |>
      filter(margin > 0) |>
      arrange(desc(margin)) |>
      head(25) |>
      select(Season = season, Week = week, Winner = team_owner,
             `Win Score` = franchise_score, Loser = opponent_owner,
             `Lose Score` = opponent_score, Margin = margin)
    datatable(blowouts, options = list(pageLength = 10, dom = "tp"), rownames = FALSE) |>
      formatRound(c("Win Score", "Lose Score", "Margin"), 2)
  })

  # ==========================================================================
  # HEAD-TO-HEAD TAB
  # ==========================================================================

  output$h2h_team_selectors <- renderUI({
    req(rv$all_owners)
    owners <- rv$all_owners
    tagList(
      selectInput("h2h_team1", "Owner 1", choices = owners, selected = owners[1]),
      selectInput("h2h_team2", "Owner 2", choices = owners,
                  selected = owners[min(2, length(owners))])
    )
  })

  h2h_data <- eventReactive(input$calc_h2h, {
    req(rv$schedule_data, input$h2h_team1, input$h2h_team2)
    df <- rv$schedule_data |>
      filter(team_owner == input$h2h_team1, opponent_owner == input$h2h_team2)
    if (input$h2h_reg_only && "game_type" %in% names(df)) {
      df <- df |> filter(game_type == "Regular Season")
    }
    df
  })

  output$h2h_summary <- renderUI({
    req(h2h_data())
    df <- h2h_data()
    if (nrow(df) == 0) return(h5("No matchups found between these owners."))

    wins <- sum(df$result == "W", na.rm = TRUE)
    losses <- sum(df$result == "L", na.rm = TRUE)
    ties <- sum(df$result == "T", na.rm = TRUE)
    avg_score <- round(mean(df$franchise_score, na.rm = TRUE), 1)
    avg_opp <- round(mean(df$opponent_score, na.rm = TRUE), 1)

    div(
      class = "text-center mb-3",
      h4(paste(input$h2h_team1, "vs", input$h2h_team2)),
      h5(paste0(wins, "W - ", losses, "L",
                if (ties > 0) paste0(" - ", ties, "T") else "")),
      tags$small(paste0("Avg Score: ", avg_score, " - ", avg_opp))
    )
  })

  output$h2h_plot <- renderPlotly({
    req(h2h_data())
    df <- h2h_data()
    if (nrow(df) == 0) return(NULL)

    df <- df |>
      mutate(
        game_label = paste0(season, " W", week),
        margin = franchise_score - opponent_score,
        color = ifelse(margin >= 0, "Win", "Loss")
      )

    p <- ggplot(df, aes(x = game_label, y = margin, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("Win" = "#28a745", "Loss" = "#dc3545")) +
      labs(x = NULL, y = "Score Margin", fill = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            legend.position = "none")
    ggplotly(p, tooltip = c("y"))
  })

  output$h2h_detail_table <- renderDT({
    req(h2h_data())
    df <- h2h_data() |>
      mutate(Margin = franchise_score - opponent_score) |>
      select(Season = season, Week = week,
             !!input$h2h_team1 := franchise_score,
             !!input$h2h_team2 := opponent_score,
             Margin, Result = result) |>
      arrange(desc(Season), desc(Week))
    datatable(df, options = list(pageLength = 20, dom = "tp"), rownames = FALSE) |>
      formatRound(c(input$h2h_team1, input$h2h_team2, "Margin"), 2)
  })

  output$owner_vs_owner_table <- renderDT({
    req(rv$schedule_data)
    ovo <- compute_owner_vs_owner(rv$schedule_data, reg_season_only = TRUE) |>
      select(Owner, Opponent, W, L, Games, `Win%`, `Avg Margin`)
    datatable(ovo, options = list(pageLength = 25, dom = "ftp"),
              rownames = FALSE, filter = "top") |>
      formatRound("Avg Margin", 1)
  })

  # ==========================================================================
  # DRAFTS TAB
  # ==========================================================================

  # Draft Grid
  output$draft_grid <- renderUI({
    req(rv$draft_data, input$draft_season)
    yr <- as.integer(input$draft_season)

    draft <- rv$draft_data |> filter(season == yr)
    if (nrow(draft) == 0) return(h5(class = "text-muted text-center", "No draft data."))

    # Ensure owner column exists (may already be attached in load_league_data)
    if (!"owner" %in% names(draft) && !is.null(rv$owner_map)) {
      draft <- draft |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
    }

    # Get season points - prefer full nflreadr stats, fall back to rostered
    season_pts <- NULL
    if (!is.null(rv$player_stats)) {
      # Use full NFL season fantasy points (includes unrostered players)
      # Half-PPR = (standard + full PPR) / 2
      ps <- rv$player_stats |> filter(season == yr)
      has_ppr <- "fantasy_points_ppr" %in% names(ps)
      has_std <- "fantasy_points" %in% names(ps)

      if ("week" %in% names(ps)) {
        # Weekly data - aggregate
        agg <- ps |> group_by(player_display_name, position)
        if (has_ppr && has_std) {
          agg <- agg |> summarise(total_pts = round(sum((fantasy_points + fantasy_points_ppr) / 2, na.rm = TRUE), 0), .groups = "drop")
        } else if (has_ppr) {
          agg <- agg |> summarise(total_pts = round(sum(fantasy_points_ppr, na.rm = TRUE), 0), .groups = "drop")
        } else {
          agg <- agg |> summarise(total_pts = round(sum(fantasy_points, na.rm = TRUE), 0), .groups = "drop")
        }
        season_pts <- agg |> rename(player_name = player_display_name, pos = position)
      } else {
        season_pts <- ps |>
          mutate(total_pts = if (has_ppr && has_std) round((fantasy_points + fantasy_points_ppr) / 2, 0)
                             else if (has_ppr) round(fantasy_points_ppr, 0)
                             else round(fantasy_points, 0)) |>
          select(player_name = player_display_name, pos = position, total_pts)
      }
    } else if (!is.null(rv$starters_data)) {
      # Fall back to rostered points from starters data
      season_pts <- rv$starters_data |>
        filter(season == yr) |>
        group_by(player_name, pos) |>
        summarise(total_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop")
    }

    # Compute position rank among all drafted players
    if (!is.null(season_pts)) {
      draft <- draft |>
        left_join(season_pts, by = c("player_name", "pos")) |>
        mutate(total_pts = ifelse(is.na(total_pts), 0, total_pts)) |>
        group_by(pos) |>
        mutate(pos_rank = rank(-total_pts, ties.method = "min")) |>
        ungroup()
    }

    # Get unique owners in draft order
    owners <- draft |>
      filter(round == 1) |>
      arrange(pick) |>
      pull(owner) |>
      unique()

    max_round <- max(draft$round, na.rm = TRUE)

    # Color by position
    pos_colors <- c(QB = "#FF6B6B", RB = "#4ECDC4", WR = "#45B7D1",
                    TE = "#FFA07A", K = "#C9C9C9", DST = "#A0A0A0")

    # Build header row
    header_cells <- c(
      list(tags$th(style = "padding:4px 6px; background:#222; color:#fff; font-size:11px; position:sticky; left:0; z-index:3;", "RD")),
      lapply(owners, function(o) {
        tags$th(style = "padding:4px 6px; background:#013369; color:#fff; font-size:10px; text-align:center; min-width:100px; white-space:nowrap;", o)
      })
    )

    # Build rows by round
    body_rows <- lapply(1:max_round, function(rd) {
      cells <- list(
        tags$td(style = "padding:4px 6px; font-weight:bold; background:#f0f0f0; font-size:12px; position:sticky; left:0; z-index:2;", paste0("RD ", rd))
      )
      for (o in owners) {
        pick <- draft |> filter(round == rd, owner == o)
        if (nrow(pick) > 0) {
          p <- pick[1, ]
          bg <- ifelse(p$pos %in% names(pos_colors), pos_colors[p$pos], "#eee")
          has_pts <- "total_pts" %in% names(p) && !is.na(p$total_pts)
          has_rank <- "pos_rank" %in% names(p) && !is.na(p$pos_rank)
          pts_text <- if (has_pts) paste0(p$total_pts, " pts") else ""
          rank_text <- if (has_rank) paste0(p$pos, p$pos_rank) else p$pos

          cell_content <- list(
            div(style = "font-weight:bold; font-size:11px; white-space:nowrap;", p$player_name),
            div(style = "color:#666; font-size:9px;", paste0(p$pos, " - ", p$team))
          )
          if (nchar(pts_text) > 0) cell_content <- c(cell_content, list(div(style = "font-weight:bold; font-size:10px; color:#013369; margin-top:2px;", pts_text)))
          if (nchar(rank_text) > 0) cell_content <- c(cell_content, list(div(style = "font-size:9px; color:#888;", rank_text)))

          cells <- c(cells, list(
            do.call(tags$td, c(
              list(style = paste0("padding:4px 6px; background:", bg, "33; border:1px solid #ddd; font-size:10px; text-align:center; vertical-align:top;")),
              cell_content
            ))
          ))
        } else {
          cells <- c(cells, list(tags$td(style = "padding:4px 6px; background:#f8f8f8; border:1px solid #ddd;", "")))
        }
      }
      do.call(tags$tr, cells)
    })

    div(
      style = "overflow-x:auto; max-width:100%;",
      tags$table(
        style = "border-collapse:collapse; width:auto;",
        tags$thead(do.call(tags$tr, header_cells)),
        tags$tbody(body_rows)
      )
    )
  })

  output$draft_table <- renderDT({
    req(rv$draft_data, input$draft_season)
    df <- rv$draft_data |>
      filter(season == as.integer(input$draft_season)) |>
      select(Round = round, Pick = pick, Overall = overall,
             Owner = owner, Player = player_name,
             Pos = pos, Team = team) |>
      arrange(Overall)
    datatable(df, options = list(pageLength = 25, dom = "ftp"),
              rownames = FALSE, filter = "top")
  })

  # Compute draft value analysis (busts, values, accurate)
  draft_value_data <- reactive({
    req(rv$draft_data)

    drafts <- rv$draft_data |>
      filter(pos %in% c("QB", "RB", "WR", "TE"))

    # Attach owner if not present
    if (!"owner" %in% names(drafts) && !is.null(rv$owner_map)) {
      drafts <- drafts |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
    }

    # Compute drafted position rank per season (e.g., 1st RB taken = 1)
    drafts <- drafts |>
      group_by(season, pos) |>
      arrange(overall) |>
      mutate(drafted_pos_rank = row_number()) |>
      ungroup()

    # Get season fantasy points (full stats if available, else rostered)
    get_season_pts <- function(yr) {
      if (!is.null(rv$player_stats)) {
        ps <- rv$player_stats |> filter(season == yr)
        has_ppr <- "fantasy_points_ppr" %in% names(ps)
        has_std <- "fantasy_points" %in% names(ps)
        if ("week" %in% names(ps)) {
          if (has_ppr && has_std) {
            ps |> group_by(player_display_name, position) |>
              summarise(total_pts = sum((fantasy_points + fantasy_points_ppr) / 2, na.rm = TRUE), .groups = "drop") |>
              rename(player_name = player_display_name, pos = position)
          } else {
            col <- if (has_ppr) "fantasy_points_ppr" else "fantasy_points"
            ps |> group_by(player_display_name, position) |>
              summarise(total_pts = sum(.data[[col]], na.rm = TRUE), .groups = "drop") |>
              rename(player_name = player_display_name, pos = position)
          }
        } else {
          col <- if (has_ppr && has_std) NA else if (has_ppr) "fantasy_points_ppr" else "fantasy_points"
          if (is.na(col)) {
            ps |> mutate(total_pts = (fantasy_points + fantasy_points_ppr) / 2) |>
              select(player_name = player_display_name, pos = position, total_pts)
          } else {
            ps |> mutate(total_pts = .data[[col]]) |>
              select(player_name = player_display_name, pos = position, total_pts)
          }
        }
      } else if (!is.null(rv$starters_data)) {
        rv$starters_data |>
          filter(season == yr) |>
          group_by(player_name, pos) |>
          summarise(total_pts = sum(player_score, na.rm = TRUE), .groups = "drop")
      } else NULL
    }

    # Process each season
    all_years <- sort(unique(drafts$season))
    results <- list()
    for (yr in all_years) {
      sp <- get_season_pts(yr)
      if (is.null(sp) || nrow(sp) == 0) next

      # Compute actual position rank per position
      sp <- sp |>
        filter(pos %in% c("QB", "RB", "WR", "TE")) |>
        group_by(pos) |>
        mutate(actual_pos_rank = rank(-total_pts, ties.method = "min")) |>
        ungroup()

      yr_drafts <- drafts |> filter(season == yr)
      joined <- yr_drafts |>
        left_join(sp, by = c("player_name", "pos")) |>
        mutate(total_pts = ifelse(is.na(total_pts), 0, total_pts),
               actual_pos_rank = ifelse(is.na(actual_pos_rank),
                                        max(sp$actual_pos_rank, na.rm = TRUE) + 1,
                                        actual_pos_rank))

      # Compute "expected points" = points of the actual player at that draft rank
      # e.g., if drafted as 1st RB, expected = actual RB1 points that season
      pos_rank_pts <- sp |>
        group_by(pos) |>
        arrange(actual_pos_rank) |>
        mutate(rank_for_draft = row_number()) |>
        ungroup() |>
        select(pos, rank_for_draft, expected_pts = total_pts)

      joined <- joined |>
        left_join(pos_rank_pts, by = c("pos" = "pos", "drafted_pos_rank" = "rank_for_draft")) |>
        mutate(value_diff = total_pts - expected_pts,
               rank_diff = actual_pos_rank - drafted_pos_rank)

      results[[as.character(yr)]] <- joined
    }

    bind_rows(results)
  })

  output$draft_busts_table <- renderDT({
    req(draft_value_data())
    df <- draft_value_data() |>
      filter(!is.na(value_diff), !is.na(expected_pts), expected_pts > 0) |>
      arrange(value_diff) |> head(25) |>
      transmute(
        Season = season,
        Owner = owner,
        Player = player_name,
        Pos = pos,
        `Drafted As` = paste0(pos, drafted_pos_rank),
        `Finished As` = paste0(pos, actual_pos_rank),
        Points = round(total_pts, 0),
        `Expected Pts` = round(expected_pts, 0),
        `Pts Lost` = round(value_diff, 0)
      )
    datatable(df, options = list(pageLength = 15, dom = "tp"),
              rownames = FALSE)
  })

  output$draft_values_table <- renderDT({
    req(draft_value_data())
    df <- draft_value_data() |>
      filter(!is.na(value_diff), !is.na(expected_pts)) |>
      arrange(desc(value_diff)) |> head(25) |>
      transmute(
        Season = season,
        Owner = owner,
        Player = player_name,
        Pos = pos,
        `Drafted As` = paste0(pos, drafted_pos_rank),
        `Finished As` = paste0(pos, actual_pos_rank),
        Points = round(total_pts, 0),
        `Expected Pts` = round(expected_pts, 0),
        `Pts Gained` = round(value_diff, 0)
      )
    datatable(df, options = list(pageLength = 15, dom = "tp"),
              rownames = FALSE)
  })

  output$draft_accurate_table <- renderDT({
    req(draft_value_data())
    df <- draft_value_data() |>
      filter(!is.na(value_diff), !is.na(expected_pts),
             drafted_pos_rank <= 40, actual_pos_rank <= 40) |>
      mutate(abs_diff = abs(value_diff)) |>
      arrange(abs_diff) |> head(25) |>
      transmute(
        Season = season,
        Owner = owner,
        Player = player_name,
        Pos = pos,
        `Drafted As` = paste0(pos, drafted_pos_rank),
        `Finished As` = paste0(pos, actual_pos_rank),
        Points = round(total_pts, 0),
        `Expected Pts` = round(expected_pts, 0),
        `Pts Diff` = round(value_diff, 0)
      )
    datatable(df, options = list(pageLength = 15, dom = "tp"),
              rownames = FALSE)
  })

  output$draft_r1_plot <- renderPlotly({
    req(rv$draft_data)
    r1 <- rv$draft_data |>
      filter(round == 1) |>
      count(owner, pos, name = "n") |>
      group_by(owner) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup()

    # Order owners alphabetically
    r1$owner <- factor(r1$owner, levels = sort(unique(r1$owner)))
    # Order positions with clear priority
    pos_order <- c("QB", "RB", "WR", "TE", "K", "DST")
    r1$pos <- factor(r1$pos, levels = pos_order[pos_order %in% unique(r1$pos)])

    p <- ggplot(r1, aes(x = owner, y = pct, fill = pos,
                        text = paste0(owner, "<br>", pos, ": ", pct, "% (", n, " picks)"))) +
      geom_col(position = "stack") +
      labs(x = NULL, y = "% of Round 1 Picks", fill = "Position") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$draft_pos_plot <- renderPlotly({
    req(rv$draft_data)
    pos_counts <- rv$draft_data |>
      count(owner, pos, name = "n") |>
      group_by(owner) |>
      mutate(pct = round(n / sum(n) * 100, 1)) |>
      ungroup()

    pos_counts$owner <- factor(pos_counts$owner, levels = sort(unique(pos_counts$owner)))
    pos_order <- c("QB", "RB", "WR", "TE", "K", "DST")
    pos_counts$pos <- factor(pos_counts$pos, levels = pos_order[pos_order %in% unique(pos_counts$pos)])

    p <- ggplot(pos_counts, aes(x = owner, y = pct, fill = pos,
                                text = paste0(owner, "<br>", pos, ": ", pct, "% (", n, " picks)"))) +
      geom_col(position = "stack") +
      labs(x = NULL, y = "% of All Picks", fill = "Position") +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # ==========================================================================
  # TOP PERFORMANCES TAB
  # ==========================================================================

  # Helper to get player headshot URL from player_ids
  get_headshot_url <- function(player_name, player_ids_df) {
    if (is.null(player_ids_df)) return(NULL)
    # Try matching by name
    match <- player_ids_df |>
      filter(tolower(name) == tolower(player_name)) |>
      head(1)
    if (nrow(match) > 0 && "espn_id" %in% names(match)) {
      espn_id <- match$espn_id
      if (!is.na(espn_id)) {
        return(paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/", espn_id, ".png&w=96&h=70&cb=1"))
      }
    }
    NULL
  }

  # Filtered starters for top performances
  filtered_starters <- reactive({
    req(rv$starters_data)
    df <- rv$starters_data |>
      filter(lineup_slot != "BE", lineup_slot != "IR")

    # Season filter
    if (!is.null(input$perf_season) && input$perf_season != "All-Time") {
      df <- df |> filter(season == as.integer(input$perf_season))
    }

    # Position filter
    if (!is.null(input$perf_pos) && input$perf_pos != "All") {
      if (input$perf_pos == "D/ST") {
        df <- df |> filter(pos %in% c("DST", "D/ST", "DEF"))
      } else {
        df <- df |> filter(pos == input$perf_pos)
      }
    }

    df |> arrange(desc(player_score))
  })

  # Top performances cards with headshots
  output$top_performances_cards <- renderUI({
    req(filtered_starters())
    df <- filtered_starters() |> head(25)

    if (nrow(df) == 0) return(h5(class = "text-muted text-center", "No data available. Run fetch_data.R to cache starters data."))

    # Attach owner names if available
    if (!is.null(rv$owner_map)) {
      df <- df |>
        left_join(
          rv$owner_map |> select(season, franchise_id, owner),
          by = c("season", "franchise_id")
        ) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
    } else {
      df$owner <- df$franchise_name
    }

    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]

      # Get headshot
      headshot_url <- get_headshot_url(row$player_name, rv$player_ids)

      headshot_img <- if (!is.null(headshot_url)) {
        tags$img(src = headshot_url,
                 style = "width:70px; height:70px; object-fit:cover; border-radius:50%; border:3px solid #013369; background:#eee;",
                 onerror = "this.style.display='none'; this.nextElementSibling.style.display='flex';")
      } else {
        NULL
      }

      placeholder <- div(
        style = paste0(
          "width:70px; height:70px; border-radius:50%; background:#e9ecef; ",
          "border:3px solid #013369; align-items:center; justify-content:center; ",
          "display:", if (is.null(headshot_url)) "flex;" else "none;",
          " color:#6c757d; font-size:1.5rem;"
        ),
        icon("football")
      )

      rank_color <- if (i == 1) "#FFD700" else if (i == 2) "#C0C0C0" else if (i == 3) "#CD7F32" else "#6c757d"

      div(
        class = "d-flex align-items-center p-2 mb-1",
        style = paste0("border-bottom:1px solid #eee;", if (i <= 3) " background:#f8f9fa;" else ""),

        # Rank
        div(style = paste0("width:40px; font-size:20px; font-weight:bold; color:", rank_color, "; text-align:center;"),
            paste0("#", i)),

        # Headshot
        div(style = "width:80px; display:flex; justify-content:center;",
            headshot_img, placeholder),

        # Player info
        div(style = "flex:1; margin-left:12px;",
          div(style = "font-weight:bold; font-size:16px;", row$player_name),
          div(style = "color:#666; font-size:13px;",
              paste0(row$pos, " - ", row$team, " | ",
                     row$season, " Week ", row$week, " | ",
                     "Owner: ", row$owner))
        ),

        # Score
        div(style = "width:80px; text-align:right; font-size:22px; font-weight:bold; color:#013369;",
            round(row$player_score, 1))
      )
    })

    div(style = "max-height:700px; overflow-y:auto;", rows)
  })

  # Top by position table
  output$top_by_position_table <- renderDT({
    req(rv$starters_data)
    df <- rv$starters_data |>
      filter(lineup_slot != "BE", lineup_slot != "IR") |>
      group_by(pos) |>
      slice_max(player_score, n = 1) |>
      ungroup() |>
      select(Pos = pos, Player = player_name, Team = team,
             Score = player_score, Season = season, Week = week) |>
      arrange(desc(Score))
    datatable(df, options = list(pageLength = 15, dom = "t"), rownames = FALSE) |>
      formatRound("Score", 1)
  })

  # Most appearances in top 25
  output$top_appearances_plot <- renderPlotly({
    req(rv$starters_data)
    top25 <- rv$starters_data |>
      filter(lineup_slot != "BE", lineup_slot != "IR") |>
      arrange(desc(player_score)) |>
      head(100)

    appearances <- top25 |>
      count(player_name, sort = TRUE) |>
      head(15)

    appearances$player_name <- factor(appearances$player_name,
                                       levels = rev(appearances$player_name))

    p <- ggplot(appearances, aes(x = player_name, y = n, fill = player_name)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Appearances in Top 100 Scores") +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  # ==========================================================================
  # SEASON RECAPS TAB
  # ==========================================================================

  output$season_points_dist_plot <- renderPlotly({
    req(rv$standings_data)
    df <- rv$standings_data

    # Filter by season if selected
    if (!is.null(input$standings_season) && input$standings_season != "All-Time") {
      df <- df |> filter(season == as.integer(input$standings_season))
    }

    # Order owners by all-time total PF (descending = top scorer at top)
    owner_order <- df |>
      group_by(owner) |>
      summarise(total_pf = sum(points_for, na.rm = TRUE), .groups = "drop") |>
      arrange(total_pf) |> pull(owner)
    df$owner <- factor(df$owner, levels = owner_order)

    p <- ggplot(df, aes(x = owner, y = points_for,
                        text = paste0(owner, "<br>", season, ": ", round(points_for, 0), " pts"))) +
      geom_boxplot(fill = "#013369", color = "#8b6914", alpha = 0.7,
                   outlier.shape = NA) +
      geom_jitter(width = 0.15, color = "#d4a84b", alpha = 0.9, size = 2.5) +
      coord_flip() +
      labs(x = NULL, y = "Season Total Points") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text")
  })

  # ==========================================================================
  # RECAP PHOTOS TAB
  # ==========================================================================

  output$recap_gallery <- renderUI({
    # Find all images in www/recaps/
    recap_files <- list.files("www/recaps", pattern = "\\.(jpg|jpeg|png|gif|JPG|JPEG|PNG|GIF)$",
                              full.names = FALSE)
    recap_files <- setdiff(recap_files, "README.txt")

    if (length(recap_files) == 0) {
      return(h4(class = "text-muted text-center", "No recap photos uploaded yet."))
    }

    # Shuffle for a fun collage feel
    set.seed(42)
    recap_files <- sample(recap_files)

    # Build masonry-style collage using CSS columns
    photo_tags <- lapply(recap_files, function(f) {
      div(
        style = "break-inside:avoid; margin-bottom:8px;",
        tags$img(
          src = paste0("recaps/", f),
          style = paste0(
            "width:100%; border-radius:6px; ",
            "box-shadow: 0 2px 8px rgba(0,0,0,0.3); ",
            "cursor:pointer; transition: transform 0.2s;"
          ),
          onmouseover = "this.style.transform='scale(1.03)'",
          onmouseout = "this.style.transform='scale(1)'"
        )
      )
    })

    div(
      style = paste0(
        "column-count:3; column-gap:8px; padding:12px; ",
        "background:#1a1a2e; border-radius:12px;"
      ),
      # CSS override for mobile: 2 columns
      tags$style(".recap-collage { column-count:3; }
                  @media (max-width:768px) { .recap-collage { column-count:2; } }"),
      div(class = "recap-collage",
          style = "column-gap:8px;",
          photo_tags)
    )
  })

  # ==========================================================================
  # COMMISSIONER OF THE YEAR TAB
  # ==========================================================================

  output$commish_gallery <- renderUI({
    commissioners <- data.frame(
      year = 2016:2025,
      name = c(rep("Harry", 9), "Tom"),
      stringsAsFactors = FALSE
    )

    cards <- lapply(seq_len(nrow(commissioners)), function(i) {
      yr <- commissioners$year[i]
      name <- commissioners$name[i]

      # Find headshot
      photo_file <- NULL
      for (name_variant in c(paste0(tolower(name), "_headshot"), tolower(name), name)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          if (file.exists(file.path("www", "photos", paste0(name_variant, ext)))) {
            photo_file <- paste0("photos/", name_variant, ext)
            break
          }
        }
        if (!is.null(photo_file)) break
      }

      div(
        style = "display:inline-block; text-align:center; margin:10px; vertical-align:top; width:140px;",
        # Photo
        if (!is.null(photo_file)) {
          div(
            style = "position:relative; width:120px; height:140px; margin:0 auto;",
            div(
              style = "position:absolute; top:10%; left:10%; width:80%; height:80%; overflow:hidden;",
              tags$img(src = photo_file,
                       style = "width:100%; height:100%; object-fit:cover; object-position:top;")
            ),
            tags$img(src = "photos/frame.PNG",
                     style = "position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none;")
          )
        } else {
          div(
            style = "width:120px; height:140px; margin:0 auto; display:flex; align-items:center; justify-content:center; background:#2a1f18; border-radius:4px;",
            tags$span(style = "color:#555; font-size:2.5rem;", icon("user"))
          )
        },
        # Gold name plaque
        div(
          style = paste0(
            "margin-top:6px; padding:2px; ",
            "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
            "border-radius:4px;"
          ),
          div(
            style = paste0(
              "background: linear-gradient(180deg, #d4a84b, #f0d675, #c9a84c, #a07828); ",
              "border:1px solid #8b6914; border-radius:3px; padding:4px 8px; text-align:center;"
            ),
            div(style = "color:#3d2b0a; font-family:Georgia,serif; font-weight:bold; font-size:13px; text-transform:uppercase; text-shadow: 0 1px 0 rgba(255,255,255,0.3);",
                name),
            div(style = "color:#3d2b0a; font-family:Georgia,serif; font-weight:bold; font-size:16px; text-shadow: 0 1px 0 rgba(255,255,255,0.3);",
                yr)
          )
        )
      )
    })

    div(
      style = "display:flex; flex-wrap:wrap; justify-content:center; gap:10px;",
      cards
    )
  })

  # ==========================================================================
  # ACHIEVEMENTS TAB
  # ==========================================================================

  output$achievements_gallery <- renderUI({
    tryCatch({
      req(rv$standings_data, rv$schedule_data)

    standings <- rv$standings_data
    schedule <- rv$schedule_data

    # Define all 20 achievements with icons and criteria
    achievements <- list(
      list(id = "first_win", name = "First Blood", desc = "Win a regular season game", icon = "medal"),
      list(id = "champion", name = "Champion", desc = "Win a championship", icon = "trophy"),
      list(id = "dynasty", name = "Dynasty", desc = "Win 2+ championships", icon = "crown"),
      list(id = "title_game", name = "Finalist", desc = "Make the championship game", icon = "flag-checkered"),
      list(id = "one_seed", name = "Top Dog", desc = "Earn the #1 seed", icon = "medal"),
      list(id = "playoff_bound", name = "Playoff Bound", desc = "Make the playoffs", icon = "award"),
      list(id = "mvp", name = "MVP", desc = "Best regular season record", icon = "star"),
      list(id = "opoy", name = "OPOY", desc = "Most regular season points in a season", icon = "fire"),
      list(id = "century", name = "Century Club", desc = "Score 100+ in a single week", icon = "100"),
      list(id = "one_fifty", name = "Elite Performer", desc = "Score 150+ in a single week", icon = "star"),
      list(id = "two_hundred", name = "Unstoppable", desc = "Score 200+ in a single week", icon = "bolt"),
      list(id = "winning_record", name = "Winner", desc = "All-time winning record", icon = "chart-line"),
      list(id = "veteran", name = "Veteran", desc = "Play 5+ seasons", icon = "shield"),
      list(id = "decade", name = "Decade of Play", desc = "Play 9+ seasons", icon = "hourglass-end"),
      list(id = "undefeated", name = "Perfect Season", desc = "Go undefeated in regular season", icon = "certificate"),
      list(id = "sacko", name = "Sacko Holder", desc = "Finish last in the regular season", icon = "poo"),
      list(id = "double_sacko", name = "Repeat Offender", desc = "Win the Sacko 2+ times", icon = "dumpster-fire"),
      list(id = "runner_up", name = "So Close", desc = "Finish 2nd in the championship", icon = "medal"),
      list(id = "hundred_wins", name = "Centurion", desc = "Win 100+ career games", icon = "check-double"),
      list(id = "fourteen_k", name = "14K Club", desc = "Score 14,000+ career points", icon = "coins"),
      list(id = "big_blowout", name = "Demolition", desc = "Win a game by 75+ points", icon = "bomb"),
      list(id = "homer", name = "Homer", desc = "Draft 3+ players from your favorite team", icon = "home"),
      list(id = "double_zero", name = "00 Agent", desc = "Have two started players score 0 pts in a week", icon = "user-ninja"),
      list(id = "game_of_inches", name = "Game of Inches", desc = "Win a matchup by 1 point or less", icon = "ruler"),
      list(id = "gauntlet", name = "The Gauntlet", desc = "Defeat every team at least once in a season", icon = "fist-raised"),
      list(id = "bad_beat", name = "Bad Beat", desc = "Lose a week as 2nd-highest scoring team", icon = "heart-crack"),
      list(id = "really_bad_beat", name = "Really Bad Beat", desc = "Miss playoffs as 2nd-highest PF in reg season", icon = "skull"),
      list(id = "heating_up", name = "Heating Up", desc = "Win 5 weeks in a row (same season)", icon = "fire-flame-curved"),
      list(id = "on_fire", name = "On Fire", desc = "Win 10 weeks in a row (same season)", icon = "fire-flame-simple"),
      list(id = "soft_schedule", name = "Soft Schedule", desc = "Lowest points against in a season", icon = "feather"),
      list(id = "ninja", name = "Ninja", desc = "Sneak into playoffs with bottom 3 PF", icon = "mask"),
      list(id = "blowout", name = "Blowout", desc = "Win a week by 50+ points", icon = "explosion"),
      list(id = "everyone_eats", name = "Everyone Eats", desc = "Every starter scores 6+ pts in a week", icon = "utensils"),
      list(id = "double_ds", name = "Double D's", desc = "Every starter scores 10+ pts (incl DST/K)", icon = "dice-two"),
      list(id = "why", name = "Why?", desc = "Have 3+ kickers on roster at once", icon = "question"),
      list(id = "ban_kickers", name = "Ban Kickers", desc = "Kicker is your highest scoring starter", icon = "futbol"),
      list(id = "wins_champs", name = "Wins Championships", desc = "Defense is your highest scoring starter", icon = "shield-halved"),
      list(id = "kingslayer", name = "Kingslayer", desc = "End the win streak of a team that started 5-0+", icon = "khanda"),
      list(id = "rock_bottom", name = "Rock Bottom", desc = "Have the lowest scoring week of an entire season", icon = "arrow-down"),
      list(id = "top_qb", name = "Top QB", desc = "Highest scoring single QB starter in a season", icon = "football"),
      list(id = "top_rb", name = "Top RB", desc = "Highest scoring single RB starter in a season", icon = "person-running"),
      list(id = "top_wr", name = "Top WR", desc = "Highest scoring single WR starter in a season", icon = "hand"),
      list(id = "top_te", name = "Top TE", desc = "Highest scoring single TE starter in a season", icon = "user-large"),
      list(id = "top_k", name = "Top K", desc = "Highest scoring single K starter in a season", icon = "futbol"),
      list(id = "top_dst", name = "Top D/ST", desc = "Highest scoring single D/ST starter in a season", icon = "shield"),
      list(id = "best_qb", name = "Best QB Group", desc = "Most total QB starter points in a season", icon = "football"),
      list(id = "best_rb", name = "Best RB Group", desc = "Most total RB starter points in a season", icon = "person-running"),
      list(id = "best_wr", name = "Best WR Group", desc = "Most total WR starter points in a season", icon = "hand"),
      list(id = "best_te", name = "Best TE Group", desc = "Most total TE starter points in a season", icon = "user-large"),
      list(id = "best_k", name = "Best K Group", desc = "Most total K starter points in a season", icon = "futbol"),
      list(id = "best_dst", name = "Best D/ST Group", desc = "Most total D/ST starter points in a season", icon = "shield")
    )

    # Compute which achievements each owner has unlocked
    owners <- sort(unique(standings$owner))
    legacy <- c("Joe")
    active <- setdiff(owners, legacy)
    all_owners <- c(active, legacy)

    # Team-to-owner favorite team map for Homer achievement
    favorite_teams <- list(
      Tom = "NYJ", RJ = "NYJ", Matt = "NYJ",
      Kerley = "NYG", Jack = "NYG", Mike = "NYG",
      Faz = "PIT", Alex = "PIT", Harry = "PIT", Connor = "PIT"
    )

    # Pre-compute season-wide data for achievements
    # Soft Schedule: lowest PA per season winner
    soft_schedule_seasons <- standings |>
      group_by(season) |>
      arrange(points_against) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(season, owner)

    # MVP: best regular season record per season
    mvp_seasons <- standings |>
      group_by(season) |>
      arrange(desc(h2h_wins), desc(points_for)) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(season, owner)

    # Ninja: bottom 3 PF but made playoffs (top 4)
    ninja_seasons <- standings |>
      group_by(season) |>
      mutate(pf_rank = rank(-points_for)) |>
      ungroup() |>
      filter(league_rank <= 4, pf_rank >= 8) |>
      select(season, owner)

    # Really Bad Beat: 2nd highest PF but missed playoffs
    rbb_seasons <- standings |>
      group_by(season) |>
      mutate(pf_rank = rank(-points_for)) |>
      ungroup() |>
      filter(pf_rank == 2, league_rank > 4) |>
      select(season, owner)

    # Rock Bottom: lowest scoring week of a season
    name_col_sch <- if ("team_owner" %in% names(schedule)) "team_owner" else "franchise_name"
    rock_bottom_seasons <- schedule |>
      filter(!is.na(franchise_score)) |>
      group_by(season) |>
      arrange(franchise_score) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(season, owner = !!name_col_sch)

    # Kingslayer: ended a 5-0+ start streak
    kingslayer_owners <- character(0)
    if ("game_type" %in% names(schedule)) {
      reg_all <- schedule |> filter(game_type == "Regular Season", !is.na(result))
    } else {
      reg_all <- schedule |> filter(!is.na(result))
    }
    for (yr in unique(reg_all$season)) {
      yr_data <- reg_all |> filter(season == yr) |> arrange(week)
      for (team in unique(yr_data[[name_col_sch]])) {
        team_games <- yr_data |> filter(.data[[name_col_sch]] == team) |> arrange(week)
        if (nrow(team_games) >= 6) {
          # Check if first 5 games are wins
          first_5 <- head(team_games, 5)$result
          if (all(!is.na(first_5)) && all(first_5 == "W")) {
            # Find first loss after week 5
            after_5 <- team_games[team_games$week > 5, ]
            first_loss <- after_5 |> filter(result == "L") |> head(1)
            if (nrow(first_loss) > 0) {
              # The winner of that game is the kingslayer
              opp_col_sch <- if ("opponent_owner" %in% names(reg_all)) "opponent_owner" else "opponent_name"
              kingslayer <- first_loss[[opp_col_sch]]
              kingslayer_owners <- c(kingslayer_owners, kingslayer)
            }
          }
        }
      }
    }
    kingslayer_owners <- unique(kingslayer_owners)

    # Top scorer at each position per season (single best player)
    top_pos_seasons <- list(QB = c(), RB = c(), WR = c(), TE = c(), K = c(), DST = c())
    # Best total at each position per season (sum of starter points)
    best_pos_seasons <- list(QB = c(), RB = c(), WR = c(), TE = c(), K = c(), DST = c())

    if (!is.null(rv$starters_data) && !is.null(rv$owner_map)) {
      st_with_owner <- rv$starters_data |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner)) |>
        filter(!lineup_slot %in% c("BE", "IR"))

      # Normalize position (DST/D/ST/DEF -> DST)
      st_with_owner <- st_with_owner |>
        mutate(pos_norm = ifelse(pos %in% c("DST", "D/ST", "DEF"), "DST", pos))

      for (position in c("QB", "RB", "WR", "TE", "K", "DST")) {
        pos_data <- st_with_owner |> filter(pos_norm == position)

        # Single highest-scoring player by season
        if (nrow(pos_data) > 0) {
          single <- pos_data |>
            group_by(season, owner, player_name) |>
            summarise(pts = sum(player_score, na.rm = TRUE), .groups = "drop") |>
            group_by(season) |>
            arrange(desc(pts)) |>
            slice_head(n = 1) |>
            ungroup()
          top_pos_seasons[[position]] <- unique(single$owner)

          # Total at position by season
          totals <- pos_data |>
            group_by(season, owner) |>
            summarise(pts = sum(player_score, na.rm = TRUE), .groups = "drop") |>
            group_by(season) |>
            arrange(desc(pts)) |>
            slice_head(n = 1) |>
            ungroup()
          best_pos_seasons[[position]] <- unique(totals$owner)
        }
      }
    }

    compute_achievements <- function(o) {
      owner_st <- standings |> filter(owner == o)
      owner_sc <- schedule |> filter(!is.na(result))
      name_col <- if ("team_owner" %in% names(owner_sc)) "team_owner" else "franchise_name"
      opp_col <- if ("opponent_owner" %in% names(owner_sc)) "opponent_owner" else "opponent_name"

      owner_sc_all <- owner_sc |> filter(.data[[name_col]] == o)
      reg_sc <- if ("game_type" %in% names(owner_sc_all)) owner_sc_all |> filter(game_type == "Regular Season") else owner_sc_all

      # Most PF seasons
      most_pf_seasons <- standings |>
        group_by(season) |>
        arrange(desc(points_for)) |>
        slice_head(n = 1) |>
        ungroup() |>
        filter(owner == o)

      # Sackos
      sacko_seasons <- standings |>
        group_by(season) |>
        arrange(h2h_wins, points_for) |>
        slice_head(n = 1) |>
        ungroup() |>
        filter(owner == o)

      career_wins <- sum(owner_st$h2h_wins, na.rm = TRUE)
      career_pf <- sum(owner_st$points_for, na.rm = TRUE)
      total_seasons <- nrow(owner_st)
      max_week <- if (nrow(reg_sc) > 0) max(reg_sc$franchise_score, na.rm = TRUE) else 0
      max_margin <- if (nrow(reg_sc) > 0) max(reg_sc$franchise_score - reg_sc$opponent_score, na.rm = TRUE) else 0
      undefeated <- any(owner_st$h2h_losses == 0 & owner_st$h2h_wins > 0)

      # Game of Inches: win by 1 pt or less
      game_of_inches <- any((reg_sc$franchise_score - reg_sc$opponent_score) > 0 &
                            (reg_sc$franchise_score - reg_sc$opponent_score) <= 1, na.rm = TRUE)

      # Blowout: win by 50+
      blowout_50 <- any((reg_sc$franchise_score - reg_sc$opponent_score) >= 50, na.rm = TRUE)

      # Gauntlet: beat every owner in one season
      gauntlet <- FALSE
      for (yr in unique(reg_sc$season)) {
        beaten <- reg_sc |> filter(season == yr, result == "W") |> pull(.data[[opp_col]]) |> unique()
        all_opps_yr <- reg_sc |> filter(season == yr) |> pull(.data[[opp_col]]) |> unique()
        if (length(all_opps_yr) > 0 && all(all_opps_yr %in% beaten)) {
          gauntlet <- TRUE; break
        }
      }

      # Bad Beat: lose as 2nd highest scorer that week
      bad_beat <- FALSE
      losses <- reg_sc |> filter(!is.na(result) & result == "L")
      for (i in seq_len(nrow(losses))) {
        row <- losses[i, ]
        week_all <- schedule |> filter(season == row$season, week == row$week) |>
          distinct(.data[[name_col]], .keep_all = TRUE) |>
          arrange(desc(franchise_score))
        if (nrow(week_all) >= 2 && week_all[[name_col]][2] == o) {
          bad_beat <- TRUE; break
        }
      }

      # Winning streaks
      owner_games_sorted <- reg_sc |> arrange(season, week)
      calc_max_streak_within_season <- function(result_val) {
        max_s <- 0
        for (yr in unique(owner_games_sorted$season)) {
          yr_games <- owner_games_sorted |> filter(season == yr)
          cur <- 0; m <- 0
          for (r in yr_games$result) {
            if (!is.na(r) && r == result_val) { cur <- cur + 1; m <- max(m, cur) } else cur <- 0
          }
          max_s <- max(max_s, m)
        }
        max_s
      }
      max_win_streak <- calc_max_streak_within_season("W")

      # Starter-based achievements (00 Agent, Double D's, Everyone Eats, kicker stuff)
      double_zero <- FALSE
      double_ds <- FALSE
      everyone_eats <- FALSE
      why <- FALSE
      ban_kickers <- FALSE
      wins_champs <- FALSE

      if (!is.null(rv$starters_data)) {
        starters <- rv$starters_data
        own_st <- starters |>
          left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
          mutate(owner = ifelse(is.na(owner), franchise_name, owner)) |>
          filter(owner == o)

        # Starters (non-bench/IR)
        starting <- own_st |> filter(!lineup_slot %in% c("BE", "IR"))

        # 00 Agent: 2+ starters with 0 in same week
        zero_wks <- starting |> filter(player_score == 0) |>
          count(season, week, name = "n") |> filter(n >= 2)
        double_zero <- nrow(zero_wks) > 0

        # Double D's: every starter 10+
        weekly_min <- starting |>
          group_by(season, week) |>
          summarise(min_score = min(player_score, na.rm = TRUE),
                    n_start = n(), .groups = "drop") |>
          filter(n_start >= 8, min_score >= 10)
        double_ds <- nrow(weekly_min) > 0

        # Everyone Eats: every starter 6+ (proxy for TD)
        weekly_six <- starting |>
          group_by(season, week) |>
          summarise(min_score = min(player_score, na.rm = TRUE),
                    n_start = n(), .groups = "drop") |>
          filter(n_start >= 8, min_score >= 6)
        everyone_eats <- nrow(weekly_six) > 0

        # Why?: 3+ kickers on roster at same time
        kicker_weeks <- own_st |> filter(pos == "K") |>
          count(season, week, name = "n") |> filter(n >= 3)
        why <- nrow(kicker_weeks) > 0

        # Ban Kickers: kicker is top scoring starter
        top_starter_pos <- starting |>
          group_by(season, week) |>
          slice_max(player_score, n = 1, with_ties = FALSE) |>
          ungroup()
        ban_kickers <- any(top_starter_pos$pos == "K", na.rm = TRUE)

        # Wins Championships: defense is top scoring starter
        wins_champs <- any(top_starter_pos$pos %in% c("DST", "D/ST", "DEF"), na.rm = TRUE)
      }

      # Homer: 3+ players from favorite NFL team in a draft
      homer <- FALSE
      fav_team <- favorite_teams[[o]]
      if (!is.null(fav_team) && !is.null(rv$draft_data)) {
        own_draft <- rv$draft_data |> filter(owner == o) |>
          count(season, team, name = "n") |>
          filter(team == fav_team, n >= 3)
        homer <- nrow(own_draft) > 0
      }

      # Soft Schedule: lowest PA
      soft_schedule <- o %in% soft_schedule_seasons$owner
      # MVP: best reg season record
      mvp <- o %in% mvp_seasons$owner
      # Ninja: made playoffs with bottom 3 PF
      ninja <- o %in% ninja_seasons$owner
      # Really Bad Beat
      really_bad_beat <- o %in% rbb_seasons$owner

      list(
        first_win = career_wins >= 1,
        champion = any(owner_st$league_rank == 1),
        dynasty = sum(owner_st$league_rank == 1, na.rm = TRUE) >= 2,
        title_game = any(owner_st$league_rank <= 2, na.rm = TRUE),
        one_seed = any(standings |> group_by(season) |> arrange(desc(h2h_wins), desc(points_for)) |> slice_head(n = 1) |> ungroup() |> filter(owner == o) |> nrow() > 0),
        playoff_bound = any(owner_st$league_rank <= 4, na.rm = TRUE),
        mvp = mvp,
        opoy = nrow(most_pf_seasons) > 0,
        century = max_week >= 100,
        one_fifty = max_week >= 150,
        two_hundred = max_week >= 200,
        winning_record = career_wins > sum(owner_st$h2h_losses, na.rm = TRUE),
        veteran = total_seasons >= 5,
        decade = total_seasons >= 9,
        undefeated = undefeated,
        sacko = nrow(sacko_seasons) > 0,
        double_sacko = nrow(sacko_seasons) >= 2,
        runner_up = any(owner_st$league_rank == 2, na.rm = TRUE),
        hundred_wins = career_wins >= 100,
        fourteen_k = career_pf >= 14000,
        big_blowout = max_margin >= 75,
        homer = homer,
        double_zero = double_zero,
        game_of_inches = game_of_inches,
        gauntlet = gauntlet,
        bad_beat = bad_beat,
        really_bad_beat = really_bad_beat,
        heating_up = max_win_streak >= 5,
        on_fire = max_win_streak >= 10,
        soft_schedule = soft_schedule,
        ninja = ninja,
        blowout = blowout_50,
        everyone_eats = everyone_eats,
        double_ds = double_ds,
        why = why,
        ban_kickers = ban_kickers,
        wins_champs = wins_champs,
        kingslayer = o %in% kingslayer_owners,
        rock_bottom = o %in% rock_bottom_seasons$owner,
        top_qb = o %in% top_pos_seasons$QB,
        top_rb = o %in% top_pos_seasons$RB,
        top_wr = o %in% top_pos_seasons$WR,
        top_te = o %in% top_pos_seasons$TE,
        top_k = o %in% top_pos_seasons$K,
        top_dst = o %in% top_pos_seasons$DST,
        best_qb = o %in% best_pos_seasons$QB,
        best_rb = o %in% best_pos_seasons$RB,
        best_wr = o %in% best_pos_seasons$WR,
        best_te = o %in% best_pos_seasons$TE,
        best_k = o %in% best_pos_seasons$K,
        best_dst = o %in% best_pos_seasons$DST
      )
    }

    # Helper: build one Xbox 360 style achievement badge
    build_badge <- function(ach, unlocked) {
      # Xbox 360 logo: circle with 4 segments split by horizontal and vertical gaps
      # Center circle with icon
      icon_color <- if (unlocked) "#a1c943" else "#444"
      ring_color <- if (unlocked) "#a1c943" else "#2a2a2a"
      bg_color <- if (unlocked) "#1a1a1a" else "#0a0a0a"
      text_color <- if (unlocked) "#fff" else "#666"

      div(
        title = paste0(ach$name, ": ", ach$desc),
        style = "display:flex; flex-direction:column; align-items:center; margin:8px; width:85px;",

        # Xbox 360 style ring
        div(
          style = paste0(
            "position:relative; width:70px; height:70px; display:flex; align-items:center; justify-content:center; ",
            # 4 quarter ring using conic-gradient with gaps
            "background: conic-gradient(from 3deg, ", ring_color, " 0deg 84deg, transparent 84deg 96deg, ",
            ring_color, " 96deg 174deg, transparent 174deg 186deg, ",
            ring_color, " 186deg 264deg, transparent 264deg 276deg, ",
            ring_color, " 276deg 354deg, transparent 354deg 360deg); ",
            "border-radius:50%; ",
            # Make it a ring by cutting center with mask
            "-webkit-mask: radial-gradient(circle, transparent 50%, #000 51%); ",
            "mask: radial-gradient(circle, transparent 50%, #000 51%);"
          )
        ),
        # Center circle with icon
        div(
          style = paste0(
            "position:absolute; margin-top:14px; width:42px; height:42px; border-radius:50%; ",
            "background:", bg_color, "; border:1px solid ", ring_color, "; ",
            "display:flex; align-items:center; justify-content:center; ",
            "box-shadow: inset 0 0 8px rgba(0,0,0,0.5);"
          ),
          tags$span(style = paste0("color:", icon_color, "; font-size:20px;"),
                    icon(ach$icon))
        ),
        # Label below
        div(
          style = paste0(
            "margin-top:8px; font-size:10px; text-align:center; color:", text_color, "; ",
            "font-family:Arial,sans-serif; font-weight:bold; line-height:1.2;"
          ),
          ach$name
        )
      )
    }

    # Build owner section
    build_owner_section <- function(o) {
      status <- compute_achievements(o)
      badges <- lapply(achievements, function(ach) {
        build_badge(ach, isTRUE(status[[ach$id]]))
      })

      # Owner photo
      photo_file <- NULL
      o_clean <- gsub(" ", "", tolower(o))
      for (name_variant in c(paste0(o_clean, "_headshot"), paste0(tolower(o), "_headshot"), tolower(o), o)) {
        for (ext in c(".png", ".jpg", ".jpeg", ".PNG")) {
          if (file.exists(file.path("www", "photos", paste0(name_variant, ext)))) {
            photo_file <- paste0("photos/", name_variant, ext)
            break
          }
        }
        if (!is.null(photo_file)) break
      }

      unlocked_count <- sum(unlist(status), na.rm = TRUE)
      # Gamerscore: most are 100G, special achievements worth more
      # Champion 1000, Finalist 500, Playoff Bound 250, all others 100
      ach_value <- function(id) {
        if (id == "champion") 1000
        else if (id == "title_game") 500
        else if (id == "playoff_bound") 250
        else 100
      }
      gamerscore <- sum(sapply(achievements, function(a) {
        if (isTRUE(status[[a$id]])) ach_value(a$id) else 0
      }), na.rm = TRUE)
      # Games = career matchups
      owner_st <- standings |> filter(owner == o)
      career_games <- sum(owner_st$h2h_wins + owner_st$h2h_losses + dplyr::coalesce(owner_st$h2h_ties, 0L), na.rm = TRUE)

      div(
        style = paste0(
          "background: linear-gradient(180deg, #1a1a1a 0%, #0a0a0a 100%); ",
          "border:2px solid #a1c943; border-radius:8px; ",
          "padding:15px; margin-bottom:20px; ",
          "box-shadow: 0 4px 12px rgba(0,0,0,0.5), 0 0 20px rgba(161,201,67,0.15);"
        ),
        # Xbox 360 style gamer card header
        div(
          style = "display:flex; align-items:center; margin-bottom:15px; padding-bottom:12px; border-bottom:2px solid #a1c943;",
          # Pixelated photo (square, large)
          if (!is.null(photo_file)) {
            tags$img(src = photo_file,
                     style = paste0(
                       "width:110px; height:110px; object-fit:cover; object-position:top; ",
                       "image-rendering: pixelated; image-rendering: -moz-crisp-edges; image-rendering: crisp-edges; ",
                       "border:3px solid #a1c943; margin-right:20px; ",
                       "box-shadow: 0 0 12px rgba(161,201,67,0.4); ",
                       "filter: contrast(1.1) saturate(1.2);"
                     ))
          } else {
            div(style = "width:110px; height:110px; background:#333; border:3px solid #a1c943; margin-right:20px; display:flex; align-items:center; justify-content:center;",
                tags$span(style = "color:#a1c943; font-size:40px;", icon("user")))
          },
          # Gamer card stats
          div(
            style = "flex:1;",
            # Gamertag
            tags$div(
              style = paste0(
                "color:#a1c943; font-family:'Courier New',monospace; font-weight:bold; ",
                "font-size:28px; text-shadow: 2px 2px 0 #000; letter-spacing:2px; margin-bottom:8px;"
              ),
              toupper(o)
            ),
            # Stats table
            tags$table(
              style = "font-family:'Courier New',monospace; font-size:16px; border-collapse:collapse;",
              tags$tr(
                tags$td(style = "color:#a1c943; padding:2px 20px 2px 0; font-weight:bold;", "Games"),
                tags$td(style = "color:#fff; padding:2px 0; font-weight:bold; text-align:right;", career_games)
              ),
              tags$tr(
                tags$td(style = "color:#a1c943; padding:2px 20px 2px 0; font-weight:bold;", "Gamerscore"),
                tags$td(style = "color:#fff; padding:2px 0; font-weight:bold; text-align:right;",
                        tagList(format(gamerscore, big.mark = ","), tags$span(style = "color:#a1c943; margin-left:4px;", "G")))
              ),
              tags$tr(
                tags$td(style = "color:#a1c943; padding:2px 20px 2px 0; font-weight:bold;", "Achievements"),
                tags$td(style = "color:#fff; padding:2px 0; font-weight:bold; text-align:right;",
                        paste0(unlocked_count, " / ", length(achievements)))
              )
            )
          )
        ),
        # Badge grid
        div(
          style = "display:flex; flex-wrap:wrap; justify-content:flex-start;",
          badges
        )
      )
    }

    # Build all sections
    tagList(
      h2(style = "color:#a1c943; font-family:Arial,sans-serif; text-align:center; letter-spacing:3px; margin-bottom:20px;",
         "GFFL ACHIEVEMENTS"),
      lapply(all_owners, build_owner_section)
    )
    }, error = function(e) {
      div(style = "color:#fff; padding:20px; background:#3d0000; border-radius:8px;",
          h3(style = "color:#ff6b6b;", "Error loading achievements:"),
          tags$pre(style = "color:#fff; font-size:12px; white-space:pre-wrap;", conditionMessage(e)))
    })
  })

  # ==========================================================================
  # PLAYER RECORDS TAB
  # ==========================================================================

  output$player_records_book <- renderUI({
    req(rv$starters_data)

    starters <- rv$starters_data |> filter(!pos %in% c("DST", "D/ST", "DEF", "K"))
    started <- starters |> filter(!lineup_slot %in% c("BE", "IR"))
    benched <- starters |> filter(lineup_slot == "BE")

    # Attach owner names
    if (!is.null(rv$owner_map)) {
      starters <- starters |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
      started <- started |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
      benched <- benched |>
        left_join(rv$owner_map |> select(season, franchise_id, owner), by = c("season", "franchise_id")) |>
        mutate(owner = ifelse(is.na(owner), franchise_name, owner))
    }

    # Helper: get ESPN headshot URL
    get_hs <- function(pname) {
      if (is.null(rv$player_ids)) return(NULL)
      m <- rv$player_ids |> filter(tolower(name) == tolower(pname)) |> head(1)
      if (nrow(m) > 0 && "espn_id" %in% names(m) && !is.na(m$espn_id)) {
        return(paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/", m$espn_id, ".png&w=96&h=70&cb=1"))
      }
      NULL
    }

    # Build a top-5 record section with headshots
    build_top5 <- function(title, df, value_col, extra_col = NULL) {
      rows <- lapply(seq_len(min(nrow(df), 20)), function(i) {
        r <- df[i, ]
        hs_url <- get_hs(r$player_name)
        tags$tr(
          tags$td(style = "color:#8b6914; font-size:14px; padding:4px 6px; width:30px; vertical-align:middle;",
                  paste0("#", i)),
          tags$td(style = "width:40px; padding:4px;",
            if (!is.null(hs_url)) tags$img(src = hs_url, style = "width:32px; height:32px; border-radius:50%; object-fit:cover; border:2px solid #8b6914;", onerror = "this.style.display='none'")
          ),
          tags$td(style = "color:#3d2a10; font-family:'Cormorant Garamond',Georgia,serif; font-size:16px; font-weight:600; padding:4px; vertical-align:middle;",
                  r$player_name,
                  tags$span(style = "color:#8b6914; font-size:12px; margin-left:4px;", paste0(r$pos, " - ", r$team))),
          tags$td(style = "color:#3d2a10; font-family:'Cormorant Garamond',Georgia,serif; font-size:18px; font-weight:bold; text-align:right; padding:4px; vertical-align:middle;",
                  r[[value_col]]),
          if (!is.null(extra_col)) tags$td(style = "color:#5c3a10; font-size:13px; font-style:italic; text-align:right; padding:4px; vertical-align:middle;", r[[extra_col]])
        )
      })

      tagList(
        div(style = "margin:25px 20px 10px;",
          div(style = "font-family:'IM Fell English',Georgia,serif; font-size:20px; color:#3d2a10; font-weight:bold; letter-spacing:2px; text-transform:uppercase; border-bottom:2px solid #5c3a10; padding-bottom:4px;",
              title)),
        tags$table(style = "width:calc(100% - 40px); margin:0 20px; border-collapse:collapse;",
          tags$tbody(rows)),
        tags$hr(style = "border:none; border-top:1px dashed rgba(92,58,16,0.25); margin:10px 20px;")
      )
    }

    # Build a single-record entry (same style as Records tab)
    build_single <- function(title, player, value, detail = "") {
      hs_url <- get_hs(player)
      div(style = "margin:15px 20px;",
        div(style = "font-family:'IM Fell English',Georgia,serif; font-size:18px; color:#3d2a10; font-weight:bold; letter-spacing:1px; text-transform:uppercase;",
            title),
        div(style = "display:flex; align-items:center; padding-left:20px; margin-top:4px;",
          if (!is.null(hs_url)) tags$img(src = hs_url, style = "width:36px; height:36px; border-radius:50%; object-fit:cover; border:2px solid #8b6914; margin-right:10px;", onerror = "this.style.display='none'"),
          div(
            tags$span(style = "font-family:'Cormorant Garamond',Georgia,serif; font-size:18px; font-weight:600; color:#3d2a10;", value),
            tags$span(style = "color:#8b6914; font-size:14px; margin-left:6px;", paste0("— ", player)),
            if (nchar(detail) > 0) tags$span(style = "color:#5c3a10; font-size:13px; margin-left:6px; font-style:italic;", paste0("(", detail, ")"))
          )
        ),
        tags$hr(style = "border:none; border-top:1px dashed rgba(92,58,16,0.25); margin:8px 0 0;")
      )
    }

    # ==================== COMPUTE ALL RECORDS ====================

    # 1. Highest single-game starter score - top 5
    top_scores <- started |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 2. Most games scoring 50+ points
    above50 <- started |> filter(player_score >= 50) |>
      count(player_name, pos, team, name = "games_50plus") |> arrange(desc(games_50plus)) |> head(10)

    # 3. Most games scoring 40+ points
    above40 <- started |> filter(player_score >= 40) |>
      count(player_name, pos, team, name = "games_40plus") |> arrange(desc(games_40plus)) |> head(10)

    # 4. Most games scoring 30+ points
    above30 <- started |> filter(player_score >= 30) |>
      count(player_name, pos, team, name = "games_30plus") |> arrange(desc(games_30plus)) |> head(10)

    # 5. Highest single-game bench score (left on bench)
    top_bench <- benched |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts on bench"),
             detail = paste0(owner, " - ", season, " W", week))

    # 6. Most weeks on someone's bench
    bench_kings <- benched |>
      count(player_name, pos, team, name = "weeks_benched") |>
      arrange(desc(weeks_benched)) |> head(10)

    # 7. Most seasons owned by the same owner
    loyalty <- starters |>
      group_by(player_name, pos, team, owner) |>
      summarise(seasons = n_distinct(season), .groups = "drop") |>
      arrange(desc(seasons)) |> head(10) |>
      mutate(display = paste0(seasons, " seasons"),
             detail = owner)

    # 8. Player on most different owners' teams
    nomads <- starters |>
      group_by(player_name, pos, team) |>
      summarise(num_owners = n_distinct(franchise_id), .groups = "drop") |>
      arrange(desc(num_owners)) |> head(10)

    # 9. Most total starts across all seasons
    most_starts <- started |>
      count(player_name, pos, team, name = "total_starts") |>
      arrange(desc(total_starts)) |> head(10)

    # 10. Most total fantasy points scored as a starter
    most_total_pts <- started |>
      group_by(player_name, pos, team) |>
      summarise(total_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
      arrange(desc(total_pts)) |> head(10)

    # 11. Highest average score per start (min 10 starts)
    best_avg <- started |>
      group_by(player_name, pos, team) |>
      summarise(starts = n(), avg_pts = round(mean(player_score, na.rm = TRUE), 1), .groups = "drop") |>
      filter(starts >= 10) |>
      arrange(desc(avg_pts)) |> head(10)

    # 12. Biggest bust: highest projected with lowest actual
    if ("projected_score" %in% names(started)) {
      busts <- started |>
        mutate(bust_margin = projected_score - player_score) |>
        filter(!is.na(bust_margin)) |>
        arrange(desc(bust_margin)) |> head(10) |>
        mutate(display = paste0("Projected ", round(projected_score, 0), ", scored ", round(player_score, 1)),
               detail = paste0(owner, " - ", season, " W", week))
    } else { busts <- NULL }

    # 13. Biggest overperformer: lowest projected with highest actual
    if ("projected_score" %in% names(started)) {
      booms <- started |>
        mutate(boom_margin = player_score - projected_score) |>
        filter(!is.na(boom_margin)) |>
        arrange(desc(boom_margin)) |> head(10) |>
        mutate(display = paste0("Projected ", round(projected_score, 0), ", scored ", round(player_score, 1)),
               detail = paste0(owner, " - ", season, " W", week))
    } else { booms <- NULL }

    # 14. Most goose eggs (0 points as a starter)
    goose_eggs <- started |> filter(player_score == 0) |>
      count(player_name, pos, team, name = "goose_eggs") |>
      arrange(desc(goose_eggs)) |> head(10)

    # 15. Most different teams (NFL) a player appeared on
    team_hoppers <- starters |>
      group_by(player_name, pos) |>
      summarise(nfl_teams = n_distinct(team), .groups = "drop") |>
      filter(nfl_teams > 1) |>
      arrange(desc(nfl_teams)) |> head(10) |>
      mutate(team = "Multiple")

    # 16. Most games scoring 20+ points
    above20 <- started |> filter(player_score >= 20) |>
      count(player_name, pos, team, name = "games_20plus") |> arrange(desc(games_20plus)) |> head(10)

    # 17. Most games scoring under 5 points (started)
    under5 <- started |> filter(player_score < 5) |>
      count(player_name, pos, team, name = "games_under5") |> arrange(desc(games_under5)) |> head(10)

    # 18. Highest scoring QB in a single game
    top_qb <- started |> filter(pos == "QB") |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 19. Highest scoring RB in a single game
    top_rb <- started |> filter(pos == "RB") |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 20. Highest scoring WR in a single game
    top_wr <- started |> filter(pos == "WR") |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 21. Highest scoring TE in a single game
    top_te <- started |> filter(pos == "TE") |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 22. Most consistent: lowest standard deviation (min 20 starts)
    most_consistent <- started |>
      group_by(player_name, pos, team) |>
      summarise(starts = n(), sd_pts = round(sd(player_score, na.rm = TRUE), 2),
                avg = round(mean(player_score, na.rm = TRUE), 1), .groups = "drop") |>
      filter(starts >= 20) |>
      arrange(sd_pts) |> head(10) |>
      mutate(display = paste0("SD: ", sd_pts, " (avg ", avg, ")"))

    # 23. Most boom-or-bust: highest standard deviation (min 20 starts)
    most_volatile <- started |>
      group_by(player_name, pos, team) |>
      summarise(starts = n(), sd_pts = round(sd(player_score, na.rm = TRUE), 2),
                avg = round(mean(player_score, na.rm = TRUE), 1), .groups = "drop") |>
      filter(starts >= 20) |>
      arrange(desc(sd_pts)) |> head(10) |>
      mutate(display = paste0("SD: ", sd_pts, " (avg ", avg, ")"))

    # 24. Biggest single-game improvement over previous week
    week_jumps <- started |>
      arrange(player_name, season, week) |>
      group_by(player_name, pos, team) |>
      mutate(prev_score = lag(player_score),
             jump = player_score - prev_score) |>
      ungroup() |>
      filter(!is.na(jump)) |>
      arrange(desc(jump)) |> head(10) |>
      mutate(display = paste0("+", round(jump, 1), " pts (", round(prev_score, 0), " -> ", round(player_score, 0), ")"),
             detail = paste0(owner, " - ", season, " W", week))

    # 25. Biggest single-game drop from previous week
    week_drops <- started |>
      arrange(player_name, season, week) |>
      group_by(player_name, pos, team) |>
      mutate(prev_score = lag(player_score),
             drop = prev_score - player_score) |>
      ungroup() |>
      filter(!is.na(drop)) |>
      arrange(desc(drop)) |> head(10) |>
      mutate(display = paste0("-", round(drop, 1), " pts (", round(prev_score, 0), " -> ", round(player_score, 0), ")"),
             detail = paste0(owner, " - ", season, " W", week))

    # 26. Most points scored in a single season for one owner
    season_leaders <- started |>
      group_by(player_name, pos, team, season, owner) |>
      summarise(season_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
      arrange(desc(season_pts)) |> head(10) |>
      mutate(display = paste0(season_pts, " pts"),
             detail = paste0(owner, " - ", season))

    # 27. Most games started across all owners (most popular starter)
    most_popular <- started |>
      count(player_name, pos, team, name = "times_started") |>
      arrange(desc(times_started)) |> head(10)

    # 28. Most career bench points
    bench_pts <- benched |>
      group_by(player_name, pos, team) |>
      summarise(bench_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
      arrange(desc(bench_pts)) |> head(10)

    # 29. Non-QB: Highest single-game score
    non_qb_started <- started |> filter(pos != "QB")
    top_non_qb <- non_qb_started |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts"),
             detail = paste0(owner, " - ", season, " W", week))

    # 30. Non-QB: Highest score left on bench
    non_qb_benched <- benched |> filter(pos != "QB")
    top_non_qb_bench <- non_qb_benched |> arrange(desc(player_score)) |> head(10) |>
      mutate(display = paste0(round(player_score, 1), " pts on bench"),
             detail = paste0(owner, " - ", season, " W", week))

    # 31. Non-QB: Most games scoring 30+ points
    non_qb_above30 <- non_qb_started |> filter(player_score >= 30) |>
      count(player_name, pos, team, name = "games_30plus") |> arrange(desc(games_30plus)) |> head(10)

    # 32. Non-QB: Most games scoring 20+ points
    non_qb_above20 <- non_qb_started |> filter(player_score >= 20) |>
      count(player_name, pos, team, name = "games_20plus") |> arrange(desc(games_20plus)) |> head(10)

    # 33. Non-QB: Highest average per start (min 10)
    non_qb_avg <- non_qb_started |>
      group_by(player_name, pos, team) |>
      summarise(starts = n(), avg_pts = round(mean(player_score, na.rm = TRUE), 1), .groups = "drop") |>
      filter(starts >= 10) |>
      arrange(desc(avg_pts)) |> head(10)

    # 34. Non-QB: Most total career points
    non_qb_total <- non_qb_started |>
      group_by(player_name, pos, team) |>
      summarise(total_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
      arrange(desc(total_pts)) |> head(10)

    # 35. Non-QB: Most points in a single season
    non_qb_season <- non_qb_started |>
      group_by(player_name, pos, team, season, owner) |>
      summarise(season_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
      arrange(desc(season_pts)) |> head(10) |>
      mutate(display = paste0(season_pts, " pts"),
             detail = paste0(owner, " - ", season))

    # 36-39. Most points in a single season BY POSITION (top 20)
    pos_season_records <- function(position) {
      started |>
        filter(pos == position) |>
        group_by(player_name, pos, team, season, owner) |>
        summarise(season_pts = round(sum(player_score, na.rm = TRUE), 0), .groups = "drop") |>
        arrange(desc(season_pts)) |> head(20) |>
        mutate(display = paste0(season_pts, " pts"),
               detail = paste0(owner, " - ", season))
    }

    qb_season <- pos_season_records("QB")
    rb_season <- pos_season_records("RB")
    wr_season <- pos_season_records("WR")
    te_season <- pos_season_records("TE")

    # ==================== BUILD UI ====================

    tagList(
      tags$style("
        .record-book .player-records th { font-family:'IM Fell English',Georgia,serif; font-size:14px; color:#5c3a10; }
      "),
      h1("The GFFL Player Records"),
      div(class = "subtitle", "A Catalogue of Heroes, Villains & Benchwarming Legends"),
      div(class = "fleuron", HTML("&#10086; &#10086; &#10086;")),

      build_top5("Highest Single-Game Score", top_scores, "display", "detail"),
      build_top5("Most Games Scoring 50+ Points", above50, "games_50plus"),
      build_top5("Most Games Scoring 40+ Points", above40, "games_40plus"),
      build_top5("Most Games Scoring 30+ Points", above30, "games_30plus"),
      build_top5("Most Total Fantasy Points (Career)", most_total_pts, "total_pts"),
      build_top5("Highest Avg Points Per Start (min 10)", best_avg, "avg_pts"),
      build_top5("Most Starts Across All Seasons", most_starts, "total_starts"),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Highest Score Left on Bench", top_bench, "display", "detail"),
      build_top5("Most Weeks Spent on the Bench", bench_kings, "weeks_benched"),
      build_top5("Most Goose Eggs (0 pts as starter)", goose_eggs, "goose_eggs"),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Most Loyal: Seasons with Same Owner", loyalty, "display", "detail"),
      build_top5("Most Nomadic: Different Owners", nomads, "num_owners"),
      build_top5("Most NFL Teams Played For", team_hoppers, "nfl_teams"),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Highest Scoring QB (Single Game)", top_qb, "display", "detail"),
      build_top5("Highest Scoring RB (Single Game)", top_rb, "display", "detail"),
      build_top5("Highest Scoring WR (Single Game)", top_wr, "display", "detail"),
      build_top5("Highest Scoring TE (Single Game)", top_te, "display", "detail"),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Most Games Scoring 20+ Points", above20, "games_20plus"),
      build_top5("Most Games Under 5 Points (as starter)", under5, "games_under5"),
      build_top5("Most Points in a Single Season", season_leaders, "display", "detail"),
      build_top5("Most Popular Starter (Times Started)", most_popular, "times_started"),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Most Consistent (Lowest Variability, min 20 starts)", most_consistent, "display"),
      build_top5("Most Boom-or-Bust (Highest Variability, min 20 starts)", most_volatile, "display"),
      build_top5("Biggest Week-to-Week Jump", week_jumps, "display", "detail"),
      build_top5("Biggest Week-to-Week Drop", week_drops, "display", "detail"),

      if (!is.null(busts)) tagList(
        div(class = "fleuron", HTML("&#10086;")),
        build_top5("Biggest Busts (Projected vs Actual)", busts, "display", "detail"),
        build_top5("Biggest Booms (Over-Performed Projection)", booms, "display", "detail")
      ),

      div(class = "fleuron", HTML("&#10086;")),

      build_top5("Most Career Bench Points", bench_pts, "bench_pts"),
      build_top5("Highest Non-QB Single-Game Score", top_non_qb, "display", "detail"),
      build_top5("Highest Non-QB Score Left on Bench", top_non_qb_bench, "display", "detail"),
      build_top5("Non-QB: Most Games Scoring 30+", non_qb_above30, "games_30plus"),
      build_top5("Non-QB: Most Games Scoring 20+", non_qb_above20, "games_20plus"),
      build_top5("Non-QB: Highest Avg Per Start (min 10)", non_qb_avg, "avg_pts"),
      build_top5("Non-QB: Most Career Starter Points", non_qb_total, "total_pts"),
      build_top5("Non-QB: Most Points in a Season", non_qb_season, "display", "detail"),

      div(class = "fleuron", HTML("&#10086;")),

      h2(style = "font-family:'IM Fell English',Georgia,serif; color:#3d2a10; text-align:center; letter-spacing:3px; margin:20px 0 10px;",
         "SEASONAL RECORDS BY POSITION"),

      build_top5("QB: Most Points in a Season (Top 20)", qb_season, "display", "detail"),
      build_top5("RB: Most Points in a Season (Top 20)", rb_season, "display", "detail"),
      build_top5("WR: Most Points in a Season (Top 20)", wr_season, "display", "detail"),
      build_top5("TE: Most Points in a Season (Top 20)", te_season, "display", "detail"),

      div(class = "fleuron", HTML("&#10086; &#10086; &#10086;"))
    )
  })

  # ==========================================================================
  # RECORDS TAB - Old Book Style
  # ==========================================================================

  output$records_book <- renderUI({
    req(rv$standings_data, rv$schedule_data)
    records <- compute_records_book(rv$standings_data, rv$schedule_data)

    # Build record rows as table
    entries <- lapply(seq_len(nrow(records)), function(i) {
      r <- records[i, ]
      tags$tr(
        tags$td(class = "cell-record", r$Record),
        tags$td(class = "cell-owner", r$Holder),
        tags$td(class = "cell-metric", r$Value),
        tags$td(class = "cell-season", r$Season)
      )
    })

    tagList(
      tags$style("
        .record-book {
          background: #f4e8d0;
          background-image:
            radial-gradient(ellipse at top, rgba(139,105,20,0.12), transparent 60%),
            radial-gradient(ellipse at bottom, rgba(139,105,20,0.15), transparent 60%);
          padding: 40px 30px;
          min-height: 80vh;
          border: 8px double #5c3a10;
          box-shadow: inset 0 0 80px rgba(92,58,16,0.2), 0 10px 40px rgba(0,0,0,0.3);
          border-radius: 4px;
          position: relative;
        }
        .record-book::before, .record-book::after {
          content: '';
          position: absolute;
          top: 20px;
          bottom: 20px;
          width: 1px;
          background: rgba(92,58,16,0.3);
        }
        .record-book::before { left: 12px; }
        .record-book::after { right: 12px; }
        .record-book h1 {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 42px;
          text-align: center;
          color: #3d2a10;
          text-shadow: 1px 1px 0 rgba(255,255,255,0.4), 2px 2px 4px rgba(0,0,0,0.2);
          letter-spacing: 4px;
          margin: 0 0 10px;
          font-weight: normal;
        }
        .record-book .subtitle {
          font-family: 'Cormorant Garamond', Georgia, serif;
          font-style: italic;
          text-align: center;
          color: #5c3a10;
          font-size: 18px;
          margin-bottom: 20px;
          letter-spacing: 2px;
        }
        .record-book .fleuron {
          text-align: center;
          color: #8b6914;
          font-size: 24px;
          margin: 10px 0 20px;
          letter-spacing: 20px;
        }
        .record-table {
          width: 100%;
          border-collapse: collapse;
          font-family: 'Cormorant Garamond', Georgia, serif;
          color: #2a1a08;
          margin: 0 auto;
        }
        .record-table th {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 16px;
          color: #3d2a10;
          text-transform: uppercase;
          letter-spacing: 2px;
          text-align: left;
          padding: 8px 12px;
          border-bottom: 2px solid #5c3a10;
        }
        .record-table td {
          padding: 10px 12px;
          border-bottom: 1px dashed rgba(92,58,16,0.25);
          vertical-align: middle;
        }
        .record-table tr:nth-child(even) td {
          background: rgba(139,105,20,0.05);
        }
        .cell-record {
          font-family: 'IM Fell English', Georgia, serif;
          font-size: 16px;
          color: #3d2a10;
          font-weight: bold;
          text-transform: uppercase;
          letter-spacing: 1px;
          width: 30%;
        }
        .cell-owner {
          font-size: 18px;
          font-style: italic;
          color: #5c3a10;
          width: 20%;
        }
        .cell-metric {
          font-size: 18px;
          font-weight: 600;
          color: #3d2a10;
          width: 25%;
        }
        .cell-season {
          font-size: 16px;
          color: #8b6914;
          width: 25%;
        }
        @media (max-width:768px) {
          .record-book { padding: 20px 10px; }
          .record-book h1 { font-size: 28px; letter-spacing: 2px; }
          .record-book .subtitle { font-size: 14px; }
          .record-table th { font-size: 12px; padding: 6px 4px; }
          .record-table td { padding: 8px 4px; }
          .cell-record { font-size: 13px; }
          .cell-owner { font-size: 14px; }
          .cell-metric { font-size: 14px; }
          .cell-season { font-size: 12px; }
        }
      "),
      h1("The GFFL Records Book"),
      div(class = "subtitle", "A Chronicle of Glorious Deeds & Dreadful Follies"),
      div(class = "fleuron", HTML("&#10086; &#10086; &#10086;")),
      tags$table(
        class = "record-table",
        tags$thead(
          tags$tr(
            tags$th("Record"),
            tags$th("Owner"),
            tags$th("Metric"),
            tags$th("Season")
          )
        ),
        tags$tbody(entries)
      ),
      div(class = "fleuron", HTML("&#10086; &#10086; &#10086;"))
    )
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)
