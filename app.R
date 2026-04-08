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

  # OWNERS
  nav_panel(
    title = "Owners",
    icon = icon("users"),
    uiOutput("owners_grid")
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
  # OWNERS TAB
  # ==========================================================================

  output$owners_grid <- renderUI({
    req(rv$standings_data, rv$schedule_data)

    # Compute stats per owner
    alltime <- compute_alltime_standings(rv$standings_data)

    # Championships: who finished 1st in standings each season
    champs <- rv$standings_data |>
      group_by(season) |>
      filter(h2h_wins == max(h2h_wins)) |>
      slice_max(points_for, n = 1) |>
      ungroup() |>
      count(owner, name = "championships")

    # Sackos: who finished last in standings each season
    sackos <- rv$standings_data |>
      group_by(season) |>
      filter(h2h_wins == min(h2h_wins)) |>
      slice_min(points_for, n = 1) |>
      ungroup() |>
      count(owner, name = "sackos")

    owners <- sort(unique(rv$standings_data$owner))

    # Build a card for each owner
    owner_cards <- lapply(owners, function(o) {
      stats <- alltime |> filter(Team == o)
      n_champs <- if (o %in% champs$owner) champs$championships[champs$owner == o] else 0
      n_sackos <- if (o %in% sackos$owner) sackos$sackos[sackos$owner == o] else 0

      record <- paste0(stats$W, "-", stats$L)
      pf <- format(round(stats$PF, 1), nsmall = 1, big.mark = ",")

      # Lombardi trophies as repeated icons
      trophy_html <- if (n_champs > 0) {
        paste(rep("<i class='fas fa-trophy' style='color:#FFD700; font-size:28px; margin-right:4px;'></i>", n_champs), collapse = "")
      } else {
        "<span style='color:#999;'>None</span>"
      }

      # Check for owner photo (www/photos/Name.jpg or .png)
      photo_file <- NULL
      for (ext in c(".jpg", ".jpeg", ".png")) {
        if (file.exists(file.path("www", "photos", paste0(o, ext)))) {
          photo_file <- paste0("photos/", o, ext)
          break
        }
      }

      card(
        class = "text-center",
        card_header(class = "fw-bold fs-5", o),
        card_body(
          # Owner photo or placeholder
          if (!is.null(photo_file)) {
            tags$img(src = photo_file,
                     style = "width:120px; height:150px; object-fit:cover; border-radius:8px; margin:0 auto 12px; display:block;")
          } else {
            div(
              style = "width:120px; height:150px; background:#e9ecef; border:2px dashed #adb5bd; border-radius:8px; margin:0 auto 12px; display:flex; align-items:center; justify-content:center;",
              tags$span(style = "color:#6c757d; font-size:2rem;",
                        icon("user"))
            )
          },
          # Championships
          div(class = "mb-2",
            tags$strong("Championships"),
            div(style = "min-height:40px; display:flex; align-items:center; justify-content:center;",
                HTML(trophy_html))
          ),
          # Sackos
          div(class = "mb-2",
            tags$strong("Sackos: "),
            tags$span(class = if (n_sackos > 0) "text-danger fw-bold" else "",
                      n_sackos)
          ),
          # All-Time Record
          div(class = "mb-2",
            tags$strong("All-Time Record: "),
            tags$span(record)
          ),
          # Points For
          div(class = "mb-1",
            tags$strong("Points For: "),
            tags$span(pf)
          )
        )
      )
    })

    # Arrange in a responsive grid (4 per row on large screens)
    layout_column_wrap(
      width = "250px",
      !!!owner_cards
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
