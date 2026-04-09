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
  title = "GFFL Historical Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#013369",
    secondary = "#D50A0A",
    "navbar-bg" = "#013369"
  ),
  sidebar = sidebar(
    width = 280,
    title = "GFFL",
    uiOutput("league_info_panel"),
    hr(),
    actionButton("reload_data", "Reload Data", class = "btn-outline-primary w-100",
                 icon = icon("rotate"))
  ),

  # --- Tab Panels ---

  # TROPHY ROOM
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
    title = "Standings",
    icon = icon("trophy"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("All-Time Standings"),
        DTOutput("alltime_standings_table")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Wins by Season"),
        plotlyOutput("wins_by_season_plot", height = "450px")
      ),
      card(
        card_header("Points For by Season"),
        plotlyOutput("points_by_season_plot", height = "450px")
      )
    )
  ),

  # MATCHUPS
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
    title = "Drafts",
    icon = icon("list-ol"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Draft Results",
          div(
            selectInput("draft_season", label = NULL, choices = NULL, width = "120px")
          )
        ),
        DTOutput("draft_table")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Round 1 Pick Distribution by Owner"),
        plotlyOutput("draft_r1_plot", height = "400px")
      ),
      card(
        card_header("Most Drafted Positions by Owner"),
        plotlyOutput("draft_pos_plot", height = "400px")
      )
    )
  ),

  # TOP PERFORMANCES
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

  # SEASON RECAPS
  nav_panel(
    title = "Season Recaps",
    icon = icon("calendar"),
    layout_columns(
      col_widths = c(12),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Season Recap",
          div(
            selectInput("recap_season", label = NULL, choices = NULL, width = "120px")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput("season_standings_plot", height = "400px"),
          plotlyOutput("season_points_dist_plot", height = "400px")
        )
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Season Standings Detail"),
        DTOutput("season_detail_table")
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

  # RECORDS
  nav_panel(
    title = "Records",
    icon = icon("medal"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("All-Time Win Percentage Leaders"),
        plotlyOutput("win_pct_plot", height = "400px")
      ),
      card(
        card_header("All-Time Points Per Game Leaders"),
        plotlyOutput("ppg_plot", height = "400px")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Most All-Time Wins",
        value = textOutput("record_most_wins"),
        theme = "primary",
        showcase = icon("crown")
      ),
      value_box(
        title = "Highest Single-Week Score",
        value = textOutput("record_high_score"),
        theme = "success",
        showcase = icon("arrow-up")
      ),
      value_box(
        title = "Longest Win Streak",
        value = textOutput("record_win_streak"),
        theme = "warning",
        showcase = icon("fire")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("All-Time Records Book"),
        DTOutput("records_table")
      )
    )
  ),

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
        updateSelectInput(session, "recap_season",
                          choices = sort(loaded, decreasing = TRUE))
        updateSelectInput(session, "draft_season",
                          choices = sort(loaded, decreasing = TRUE))
        updateSelectInput(session, "perf_season",
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
    owner_stats <- data.frame(owner = owners, stringsAsFactors = FALSE) |>
      left_join(champs, by = "owner") |>
      left_join(sackos, by = "owner") |>
      mutate(
        championships = ifelse(is.na(championships), 0, championships),
        sackos = ifelse(is.na(sackos), 0, sackos)
      ) |>
      arrange(desc(championships), sackos)

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

      build_plaque <- function(label, value, style) {
        div(
          style = paste0(
            "flex:1; margin:2px; padding:3px; ",
            "background: linear-gradient(135deg, #5c4413, #3d2b1a, #5c4413); ",
            "border-radius:4px; box-shadow: 0 2px 5px rgba(0,0,0,0.4);"
          ),
          div(
            style = paste0(
              "background:", style$bg, "; ",
              "border:1px solid ", style$border, "; ",
              "border-radius:3px; padding:6px 4px; text-align:center; ",
              "box-shadow: inset 0 1px 3px rgba(255,255,255,0.4), inset 0 -1px 3px rgba(0,0,0,0.2), ",
              "0 1px 0 ", style$shadow, ";"
            ),
            div(style = paste0(
              "font-family:Georgia,serif; font-size:9px; text-transform:uppercase; ",
              "letter-spacing:1px; color:", style$text, "; opacity:0.7;"
            ), label),
            div(style = paste0(
              "font-family:Georgia,serif; font-weight:bold; font-size:14px; ",
              "color:", style$text, "; ",
              "text-shadow: 0 1px 0 rgba(255,255,255,0.3), 0 -1px 0 rgba(0,0,0,0.2);"
            ), value)
          )
        )
      }

      lombardi_imgs <- if (n_champs > 0) {
        paste(rep("<img src='photos/lombardi.png' class='trophy-img lombardi-img'>", n_champs), collapse = "")
      } else ""

      hunt_imgs <- if (n_appear > 0) {
        paste(rep("<img src='photos/Hunt.png' class='trophy-img hunt-img'>", n_appear), collapse = "")
      } else ""

      sacko_imgs <- if (n_sackos > 0 && file.exists("www/photos/sacko-trophy.png")) {
        paste(rep("<img src='photos/sacko-trophy.png' class='trophy-img sacko-img'>", n_sackos), collapse = "")
      } else ""

      playoff_imgs <- ""
      if (length(playoff_years) > 0) {
        banners <- sapply(playoff_years, function(yr) {
          for (ext in c(".PNG", ".png", ".jpg", ".jpeg")) {
            f <- paste0("www/photos/playoffs_", yr, ext)
            if (file.exists(f)) return(paste0("<img src='photos/playoffs_", yr, ext, "' class='trophy-img banner-img'>"))
          }
          return("")
        })
        playoff_imgs <- paste(banners, collapse = "")
      }

      # #1 seed banners
      oneseed_imgs <- ""
      if (length(oneseed_years) > 0) {
        banners <- sapply(oneseed_years, function(yr) {
          for (ext in c(".PNG", ".png", ".jpg", ".jpeg")) {
            f <- paste0("www/photos/oneseed_", yr, ext)
            if (file.exists(f)) return(paste0("<img src='photos/oneseed_", yr, ext, "' class='trophy-img banner-img'>"))
          }
          return("")
        })
        oneseed_imgs <- paste(banners, collapse = "")
      }

      # GFFL trophy (most PF) - single image repeated per year won
      gffl_imgs <- ""
      if (length(gffl_pf_years) > 0) {
        gffl_imgs <- paste(rep("<img src='photos/GFFL.png' class='trophy-img banner-img'>", length(gffl_pf_years)), collapse = "")
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

        div(
          style = paste0(
            "background: linear-gradient(90deg, #3d2b1a, #5c4413, #3d2b1a); ",
            "padding:8px; text-align:center; ",
            "border-bottom:2px solid #8b6914;"
          ),
          tags$span(style = "color:#d4a84b; font-family:Georgia,serif; font-weight:bold; font-size:16px; letter-spacing:1px; text-transform:uppercase;", o)
        ),

        # Photo shelf with plaques on left and right
        div(
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:center; justify-content:center; ",
            "padding:8px 4px 6px;"
          ),
          # Record plaque (left)
          build_plaque("Record", record, wp_style),
          # Photo (center)
          if (!is.null(photo_file)) {
            div(
              style = "position:relative; margin:0 4px; flex-shrink:0;",
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
              style = "display:flex; align-items:center; justify-content:center; margin:0 4px; flex-shrink:0; background:#2a1f18;",
              tags$span(style = "color:#555; font-size:3rem;", icon("user"))
            )
          },
          # Points plaque (right)
          build_plaque("Points", pf, pf_style)
        ),

        # Lombardi / Hunt split shelf
        div(
          class = "trophy-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; padding:4px 4px 6px; overflow:hidden;"
          ),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap;",
            HTML(lombardi_imgs)
          ),
          div(style = "width:2px; background:rgba(255,255,255,0.12); align-self:stretch; margin:4px 2px;"),
          div(
            style = "flex:1; display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap;",
            HTML(hunt_imgs)
          )
        ),

        # GFFL trophies (left) | #1 seed banners (right) - split shelf
        div(
          class = "banner-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:center; padding:4px 4px 6px;"
          ),
          div(
            style = "flex:1; display:flex; align-items:center; justify-content:center; flex-wrap:wrap;",
            HTML(gffl_imgs)
          ),
          div(style = "width:2px; background:rgba(255,255,255,0.12); align-self:stretch; margin:4px 2px;"),
          div(
            style = "flex:1; display:flex; align-items:center; justify-content:center; flex-wrap:wrap;",
            HTML(oneseed_imgs)
          )
        ),

        # Playoff banners shelf (3rd row)
        div(
          class = "banner-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:center; justify-content:center; ",
            "flex-wrap:wrap; padding:4px 2px; overflow:hidden;"
          ),
          HTML(playoff_imgs)
        ),

        # Sacko shelf (4th row)
        div(
          class = "sacko-shelf",
          style = paste0(
            "border-bottom:3px solid rgba(255,255,255,0.15); ",
            "background: linear-gradient(180deg, transparent 85%, rgba(255,255,255,0.05) 100%); ",
            "display:flex; align-items:flex-end; justify-content:center; ",
            "flex-wrap:wrap; padding:4px 2px 6px; overflow:hidden;"
          ),
          HTML(sacko_imgs)
        ),

        # Bottom trim
        div(
          style = paste0(
            "background: linear-gradient(90deg, #3d2b1a, #5c4413, #3d2b1a); ",
            "height:8px; border-top:2px solid #8b6914;"
          )
        )
      )
    }

    # Build active owner cards (sorted by championships desc, sackos asc)
    active_cards <- lapply(active_sorted, build_owner_card)

    # Build legacy owner cards
    legacy_cards <- lapply(legacy_sorted, build_owner_card)

    div(
      style = "background:#e8e0d4; padding:20px; border-radius:12px;",
      tags$style("
        .trophy-grid { display:grid; grid-template-columns: repeat(2, 1fr); gap:16px; }
        @media (max-width:768px) { .trophy-grid { grid-template-columns: 1fr; } }

        /* Mobile sizes */
        .trophy-img { object-fit:contain; }
        .lombardi-img { height:55px; margin:0 4px; }
        .hunt-img { height:42px; margin:0 2px; }
        .sacko-img { height:60px; width:48px; margin:0 2px; }
        .banner-img { height:80px; margin:2px; }

        .trophy-shelf { height:80px; }
        .banner-shelf { height:110px; }
        .sacko-shelf { height:80px; }
        .owner-photo-frame { width:130px; height:155px; }

        /* Desktop sizes - much bigger */
        @media (min-width:769px) {
          .lombardi-img { height:90px; margin:0 8px; }
          .hunt-img { height:70px; margin:0 5px; }
          .sacko-img { height:90px; width:70px; margin:0 4px; }
          .banner-img { height:140px; margin:5px; }

          .trophy-shelf { height:110px; }
          .banner-shelf { height:170px; }
          .sacko-shelf { height:110px; }
          .owner-photo-frame { width:160px; height:190px; }
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
                round(p$player_score, 1)
              )
            )
          })

          total_score <- round(sum(roster$player_score, na.rm = TRUE), 1)

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
          # Name plaque
          div(
            style = paste0(
              "background: linear-gradient(180deg, #151515, #0a0a0a); ",
              "border:2px solid #c9a84c; border-radius:3px; ",
              "padding:6px 10px; margin-bottom:5px; ",
              "box-shadow: inset 0 1px 3px rgba(0,0,0,0.5), 0 1px 0 rgba(201,168,76,0.3);"
            ),
            div(
              style = "color:#d4a84b; font-family:Georgia,serif; font-weight:bold; font-size:15px; letter-spacing:2px; text-transform:uppercase; text-shadow: 0 1px 2px rgba(0,0,0,0.5);",
              owner
            )
          ),
          # Year plaque
          div(
            style = paste0(
              "background: linear-gradient(180deg, #151515, #0a0a0a); ",
              "border:2px solid #c9a84c; border-radius:3px; ",
              "padding:5px 10px; ",
              "box-shadow: inset 0 1px 3px rgba(0,0,0,0.5), 0 1px 0 rgba(201,168,76,0.3);"
            ),
            div(
              style = "color:#d4a84b; font-family:Georgia,serif; font-weight:bold; font-size:18px; letter-spacing:3px; text-shadow: 0 1px 2px rgba(0,0,0,0.5);",
              yr
            )
          )
        ),

        # Championship roster dropdown
        roster_html
      )
    })

    div(
      class = "text-center",
      style = "padding:20px; background: linear-gradient(180deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%); border-radius:12px; min-height:400px;",
      bust_cards
    )
  })

  # ==========================================================================
  # STANDINGS TAB
  # ==========================================================================

  output$alltime_standings_table <- renderDT({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data)
    datatable(
      alltime,
      options = list(pageLength = 20, dom = "t", order = list(list(2, "desc"))),
      rownames = FALSE
    ) |>
      formatPercentage("Win%", digits = 1) |>
      formatRound(c("PF", "PA", "PF/G"), 1)
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

  output$draft_r1_plot <- renderPlotly({
    req(rv$draft_data)
    r1 <- rv$draft_data |>
      filter(round == 1) |>
      count(owner, name = "First_Round_Picks") |>
      arrange(desc(First_Round_Picks))
    r1$owner <- factor(r1$owner, levels = rev(r1$owner))

    p <- ggplot(r1, aes(x = owner, y = First_Round_Picks, fill = owner)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Round 1 Picks (All Seasons)") +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$draft_pos_plot <- renderPlotly({
    req(rv$draft_data)
    pos_counts <- rv$draft_data |>
      filter(round <= 5) |>
      count(owner, pos) |>
      arrange(owner, desc(n))

    p <- ggplot(pos_counts, aes(x = owner, y = n, fill = pos)) +
      geom_col(position = "stack") +
      coord_flip() +
      labs(x = NULL, y = "Picks (Rounds 1-5)", fill = "Position") +
      theme_minimal()
    ggplotly(p, tooltip = c("fill", "y"))
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

  output$season_standings_plot <- renderPlotly({
    req(rv$standings_data, input$recap_season)
    df <- rv$standings_data |>
      filter(season == as.integer(input$recap_season)) |>
      arrange(desc(h2h_wins))
    df$owner <- factor(df$owner, levels = rev(df$owner))

    p <- ggplot(df, aes(x = owner, y = h2h_wins, fill = owner)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Wins", title = paste(input$recap_season, "Standings")) +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$season_points_dist_plot <- renderPlotly({
    req(rv$schedule_data, input$recap_season)
    df <- rv$schedule_data |>
      filter(season == as.integer(input$recap_season),
             game_type == "Regular Season")

    p <- ggplot(df, aes(x = team_owner, y = franchise_score, fill = team_owner)) +
      geom_boxplot(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Weekly Score",
           title = paste(input$recap_season, "Score Distribution")) +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$season_detail_table <- renderDT({
    req(rv$standings_data, input$recap_season)
    df <- rv$standings_data |>
      filter(season == as.integer(input$recap_season)) |>
      arrange(desc(h2h_wins), desc(points_for)) |>
      mutate(Rank = row_number()) |>
      select(Rank, Owner = owner, Team = franchise_name,
             W = h2h_wins, L = h2h_losses,
             PF = points_for, PA = points_against)
    datatable(df, options = list(pageLength = 20, dom = "t"), rownames = FALSE) |>
      formatRound(c("PF", "PA"), 1)
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
  # RECORDS TAB
  # ==========================================================================

  output$win_pct_plot <- renderPlotly({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data) |> arrange(desc(`Win%`))
    alltime$Team <- factor(alltime$Team, levels = rev(alltime$Team))

    p <- ggplot(alltime, aes(x = Team, y = `Win%`, fill = Team)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Win %") + theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$ppg_plot <- renderPlotly({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data) |> arrange(desc(`PF/G`))
    alltime$Team <- factor(alltime$Team, levels = rev(alltime$Team))

    p <- ggplot(alltime, aes(x = Team, y = `PF/G`, fill = Team)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = NULL, y = "Points Per Game") + theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$record_most_wins <- renderText({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data)
    top <- alltime |> arrange(desc(W)) |> head(1)
    paste0(top$Team, " (", top$W, ")")
  })

  output$record_high_score <- renderText({
    req(rv$schedule_data)
    top <- rv$schedule_data |> arrange(desc(franchise_score)) |> head(1)
    paste0(top$team_owner, " (", round(top$franchise_score, 1), ")")
  })

  output$record_win_streak <- renderText({
    req(rv$schedule_data)
    streak <- compute_longest_streak(rv$schedule_data)
    paste0(streak$team, " (", streak$streak, ")")
  })

  output$records_table <- renderDT({
    req(rv$standings_data, rv$schedule_data)
    records <- compute_records_book(rv$standings_data, rv$schedule_data)
    datatable(records, options = list(pageLength = 20, dom = "t"), rownames = FALSE)
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)
