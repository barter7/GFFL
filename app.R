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
# nflplotR is optional - provides NFL team logos in plots
if (requireNamespace("nflplotR", quietly = TRUE)) library(nflplotR)

# --- Source Helpers ---
source("helpers.R")

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
    width = 300,
    title = "League Settings",
    textInput(
      "league_id",
      "ESPN League ID",
      value = "570237"
    ),
    helpText("Find your league ID in the URL of your ESPN fantasy league page."),
    sliderInput(
      "season_range",
      "Seasons",
      min = 2004,
      max = 2025,
      value = c(2004, 2025),
      step = 1,
      sep = ""
    ),
    hr(),
    h6("Private League Auth (optional)"),
    passwordInput("espn_s2", "ESPN_S2 Cookie", placeholder = "Leave blank for public leagues"),
    passwordInput("swid", "SWID Cookie", placeholder = "Leave blank for public leagues"),
    helpText("Required only for private leagues. Find these in your browser cookies at espn.com."),
    hr(),
    actionButton("fetch_data", "Load League Data", class = "btn-primary w-100", icon = icon("football")),
    hr(),
    uiOutput("league_info_panel")
  ),

  # --- Tab Panels ---
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
            selectInput(
              "matchup_season",
              label = NULL,
              choices = NULL,
              width = "120px"
            )
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

  nav_panel(
    title = "Head-to-Head",
    icon = icon("scale-balanced"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Select Matchup"),
        uiOutput("h2h_team_selectors"),
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
    )
  ),

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
            selectInput(
              "recap_season",
              label = NULL,
              choices = NULL,
              width = "120px"
            )
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
    all_teams = NULL,
    seasons_loaded = NULL
  )

  # --- Fetch Data ---
  observeEvent(input$fetch_data, {
    req(input$league_id)

    league_id <- as.integer(input$league_id)
    seasons <- seq(input$season_range[1], input$season_range[2])

    # Auth params (empty string = NULL for ffscrapr)
    s2 <- if (nchar(input$espn_s2) > 0) input$espn_s2 else NULL
    sw <- if (nchar(input$swid) > 0) input$swid else NULL

    withProgress(message = "Fetching league data...", value = 0, {
      n_seasons <- length(seasons)

      all_standings <- list()
      all_schedules <- list()
      all_league <- list()

      for (i in seq_along(seasons)) {
        s <- seasons[i]
        incProgress(1 / n_seasons, detail = paste("Season", s))

        tryCatch({
          conn <- espn_connect(
            season = s,
            league_id = league_id,
            espn_s2 = s2,
            swid = sw
          )

          league_info <- ff_league(conn)
          standings <- ff_standings(conn)
          schedule <- ff_schedule(conn)

          # Add season column if not present
          if (!"season" %in% names(standings)) standings$season <- s
          if (!"season" %in% names(schedule)) schedule$season <- s

          all_league[[as.character(s)]] <- league_info
          all_standings[[as.character(s)]] <- standings
          all_schedules[[as.character(s)]] <- schedule

        }, error = function(e) {
          showNotification(
            paste("Error loading season", s, ":", e$message),
            type = "warning",
            duration = 8
          )
        })
      }

      # Combine all data
      rv$league_data <- all_league
      rv$standings_data <- bind_rows(all_standings)
      rv$schedule_data <- bind_rows(all_schedules)
      rv$seasons_loaded <- seasons[as.character(seasons) %in% names(all_standings)]

      # Extract unique team names
      if (nrow(rv$standings_data) > 0) {
        rv$all_teams <- sort(unique(rv$standings_data$franchise_name))
      }

      # Update season selectors
      loaded <- rv$seasons_loaded
      if (length(loaded) > 0) {
        updateSelectInput(session, "matchup_season",
                          choices = sort(loaded, decreasing = TRUE))
        updateSelectInput(session, "recap_season",
                          choices = sort(loaded, decreasing = TRUE))
      }
    })

    showNotification(
      paste("Loaded", length(rv$seasons_loaded), "seasons successfully!"),
      type = "message",
      duration = 5
    )
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
  # STANDINGS TAB
  # ==========================================================================

  output$alltime_standings_table <- renderDT({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data)
    datatable(
      alltime,
      options = list(
        pageLength = 20,
        dom = "t",
        order = list(list(2, "desc"))
      ),
      rownames = FALSE
    ) |>
      formatPercentage("Win%", digits = 1) |>
      formatRound(c("PF", "PA", "PF/G"), 1)
  })

  output$wins_by_season_plot <- renderPlotly({
    req(rv$standings_data)
    p <- rv$standings_data |>
      ggplot(aes(x = factor(season), y = h2h_wins, fill = franchise_name)) +
      geom_col(position = "dodge") +
      labs(x = "Season", y = "Wins", fill = "Team") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
    ggplotly(p, tooltip = c("fill", "y")) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$points_by_season_plot <- renderPlotly({
    req(rv$standings_data)
    p <- rv$standings_data |>
      ggplot(aes(x = factor(season), y = points_for, fill = franchise_name)) +
      geom_col(position = "dodge") +
      labs(x = "Season", y = "Points For", fill = "Team") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
    ggplotly(p, tooltip = c("fill", "y")) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # ==========================================================================
  # MATCHUPS TAB
  # ==========================================================================

  output$matchups_table <- renderDT({
    req(rv$schedule_data, input$matchup_season)
    sched <- rv$schedule_data |>
      filter(season == as.integer(input$matchup_season)) |>
      select(
        Week = week,
        Team = franchise_name,
        Score = franchise_score,
        Opponent = opponent_name,
        `Opp Score` = opponent_score,
        Result = result
      ) |>
      arrange(Week, Team)

    datatable(
      sched,
      options = list(pageLength = 25, dom = "ftp"),
      rownames = FALSE,
      filter = "top"
    ) |>
      formatRound(c("Score", "Opp Score"), 2)
  })

  output$top_scores_table <- renderDT({
    req(rv$schedule_data)
    top <- rv$schedule_data |>
      arrange(desc(franchise_score)) |>
      head(25) |>
      select(
        Season = season,
        Week = week,
        Team = franchise_name,
        Score = franchise_score,
        Opponent = opponent_name
      )
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
      select(
        Season = season,
        Week = week,
        Winner = franchise_name,
        `Win Score` = franchise_score,
        Loser = opponent_name,
        `Lose Score` = opponent_score,
        Margin = margin
      )
    datatable(blowouts, options = list(pageLength = 10, dom = "tp"), rownames = FALSE) |>
      formatRound(c("Win Score", "Lose Score", "Margin"), 2)
  })

  # ==========================================================================
  # HEAD-TO-HEAD TAB
  # ==========================================================================

  output$h2h_team_selectors <- renderUI({
    req(rv$all_teams)
    teams <- rv$all_teams
    tagList(
      selectInput("h2h_team1", "Team 1", choices = teams, selected = teams[1]),
      selectInput("h2h_team2", "Team 2", choices = teams, selected = teams[min(2, length(teams))])
    )
  })

  h2h_data <- eventReactive(input$calc_h2h, {
    req(rv$schedule_data, input$h2h_team1, input$h2h_team2)
    rv$schedule_data |>
      filter(
        franchise_name == input$h2h_team1,
        opponent_name == input$h2h_team2
      )
  })

  output$h2h_summary <- renderUI({
    req(h2h_data())
    df <- h2h_data()
    if (nrow(df) == 0) return(h5("No matchups found between these teams."))

    wins <- sum(df$result == "W", na.rm = TRUE)
    losses <- sum(df$result == "L", na.rm = TRUE)
    ties <- sum(df$result == "T", na.rm = TRUE)
    avg_score <- round(mean(df$franchise_score, na.rm = TRUE), 1)
    avg_opp <- round(mean(df$opponent_score, na.rm = TRUE), 1)

    div(
      class = "text-center mb-3",
      h4(paste(input$h2h_team1, "vs", input$h2h_team2)),
      h5(paste0(wins, "W - ", losses, "L", if (ties > 0) paste0(" - ", ties, "T") else "")),
      tags$small(paste0("Avg Score: ", avg_score, " - ", avg_opp))
    )
  })

  output$h2h_plot <- renderPlotly({
    req(h2h_data())
    df <- h2h_data()
    if (nrow(df) == 0) return(NULL)

    df <- df |>
      mutate(
        game_label = paste("S", season, "W", week),
        margin = franchise_score - opponent_score,
        color = ifelse(margin >= 0, "Win", "Loss")
      )

    p <- ggplot(df, aes(x = game_label, y = margin, fill = color)) +
      geom_col() +
      scale_fill_manual(values = c("Win" = "#28a745", "Loss" = "#dc3545")) +
      labs(x = NULL, y = "Score Margin", fill = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "none"
      )
    ggplotly(p, tooltip = c("y"))
  })

  output$h2h_detail_table <- renderDT({
    req(h2h_data())
    df <- h2h_data() |>
      mutate(Margin = franchise_score - opponent_score) |>
      select(
        Season = season,
        Week = week,
        !!input$h2h_team1 := franchise_score,
        !!input$h2h_team2 := opponent_score,
        Margin,
        Result = result
      ) |>
      arrange(desc(Season), desc(Week))

    datatable(df, options = list(pageLength = 20, dom = "tp"), rownames = FALSE) |>
      formatRound(c(input$h2h_team1, input$h2h_team2, "Margin"), 2)
  })

  # ==========================================================================
  # SEASON RECAPS TAB
  # ==========================================================================

  output$season_standings_plot <- renderPlotly({
    req(rv$standings_data, input$recap_season)
    df <- rv$standings_data |>
      filter(season == as.integer(input$recap_season)) |>
      arrange(desc(h2h_wins))

    df$franchise_name <- factor(df$franchise_name, levels = rev(df$franchise_name))

    p <- ggplot(df, aes(x = franchise_name, y = h2h_wins, fill = franchise_name)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Wins", title = paste(input$recap_season, "Standings")) +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$season_points_dist_plot <- renderPlotly({
    req(rv$schedule_data, input$recap_season)
    df <- rv$schedule_data |>
      filter(season == as.integer(input$recap_season))

    p <- ggplot(df, aes(x = franchise_name, y = franchise_score, fill = franchise_name)) +
      geom_boxplot(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Weekly Score", title = paste(input$recap_season, "Score Distribution")) +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$season_detail_table <- renderDT({
    req(rv$standings_data, input$recap_season)
    df <- rv$standings_data |>
      filter(season == as.integer(input$recap_season)) |>
      arrange(desc(h2h_wins), desc(points_for)) |>
      mutate(Rank = row_number()) |>
      select(
        Rank,
        Team = franchise_name,
        W = h2h_wins,
        L = h2h_losses,
        PF = points_for,
        PA = points_against
      )
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
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Win %") +
      theme_minimal()
    ggplotly(p, tooltip = c("y"))
  })

  output$ppg_plot <- renderPlotly({
    req(rv$standings_data)
    alltime <- compute_alltime_standings(rv$standings_data) |> arrange(desc(`PF/G`))
    alltime$Team <- factor(alltime$Team, levels = rev(alltime$Team))

    p <- ggplot(alltime, aes(x = Team, y = `PF/G`, fill = Team)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Points Per Game") +
      theme_minimal()
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
    paste0(top$franchise_name, " (", round(top$franchise_score, 1), ")")
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
