library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(lubridate)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Brighton, Hove & District Snooker",
    titleWidth = 350),
  dashboardSidebar(
    width = 350,
    collapsed = FALSE,
    sidebarMenu(
      menuItem(
        "Stat leaders", tabName = "stat-leaders", icon = icon("dashboard")
        ),
      menuItem(
        "Player dashboard", tabName = "player", icon = icon("dashboard")
        ),
      menuItem(
        "Team dashboard", tabName = "team", icon = icon("dashboard")
        ),
      menuItem(
        "Missing scorecards", tabName = "missing", icon = icon("window-restore")
        ),
      menuItem(
        "Handicap calculator", tabName = "handicaps", icon = icon("calculator")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "stat-leaders",
        fluidRow(
          column(
            width = 4,
            sliderInput(
              "min_frames", min = 0, max = 100, value = 20,
              label = "Filter out players playing fewer frames than:")
          ),
          column(
            width = 4,
            sliderInput(
              "last_played", min = 2010, max = year(Sys.Date()),
              value = year(Sys.Date()) - 2, sep = "",
              label = "Filter out players who haven't played since:")
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              dataTableOutput("ratings_table")
            )
          )
        ),
        fluidRow(
          tabBox(
            id = "player_stats",
            tabPanel(
              title = "Overall Win %",
              dataTableOutput("overall_win_pct")
            ),
            tabPanel(
              title = "Head to Head",
              dataTableOutput("head_to_head")
            ),
            tabPanel(
              title = "Best Players By Division",
              pickerInput("chosen_division", multiple = FALSE,
                          choices = c(
                            "1st Division",
                            "2nd Division",
                            "3rd Division",
                            "4th Division",
                            "5th Division",
                            "6th Division"
                          )
              ),
              dataTableOutput("best_players_by_division")
            )
          )
        )
      ),
      tabItem(
        tabName = "player",
        fluidRow(
          selectizeInput("choose_player",
                         choices = NULL,
                         label = "Choose player to display:")
        ),
        fluidRow(
          column(
            width = 4,
            valueBoxOutput(
              "current_player_ranking",
              width = 12)
          ),
          column(
            width = 4,
            valueBoxOutput(
              "best_player_rating",
              width = 12),
            valueBoxOutput(
              "current_player_rating",
              width = 12)
          ),
          column(
            width = 4,
            valueBoxOutput(
              "overall_player_win_pct",
              width = 12),
            valueBoxOutput(
              "current_player_win_pct",
              width = 12)
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              dataTableOutput("frame_history")
            )
          )
        )
      ),
      tabItem(
        tabName = "team",
        fluidRow(
          column(
            width = 6,
            uiOutput("home_team_selector"),
            dataTableOutput("home_team_ratings")
            ),
          column(
            width = 6,
            uiOutput("away_team_selector"),
            dataTableOutput("away_team_ratings")
          )
        ),
        fluidRow(
          dataTableOutput("team_head_to_head")  
        )
      ),
      tabItem(
        tabName = "missing",
        fluidRow(
          column(
            width = 7,
            box(
              width = NULL,
              tableOutput("missing_summary_normal")
            )
          ),
          column(
            width = 5,
            checkboxGroupButtons(
              inputId = "seasons",
              label = "Choose seasons to include: ",
              choices = c(2010:2018),
              selected = c(2010:2018),
              justified = TRUE,
              status = "primary"
            ),
            checkboxGroupButtons(
              inputId = "divisions",
              label = "Choose divisions to include: ",
              choices = c(1:6),
              selected = c(1:6),
              justified = TRUE,
              status = "primary"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              dataTableOutput("missing_scorecards")
            )
          )
        )
      ),
      tabItem(
        tabName = "handicaps",
        fluidRow(
          column(
            width = 4,
            numericInput(
              "lower_bound", label = "Handicap of best player:",
              value = -7
            )
          ),
          column(
            width = 4,
            numericInput(
              "upper_bound", label = "Handicap of worst player:",
              value = 77
            )
          ),
          column(
            width = 4,
            numericInput(
              "increment", label = "Gap between each handicap level:",
              value = 7
              )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              dataTableOutput("calculated_handicaps"),
              downloadButton("download_handicaps", "Download")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  withProgress(message = "Progress:", value = 0, {
    num_steps <- 5
    # Update progress bar
    incProgress(1 / num_steps, detail = "Loading current player ratings")
    # Read in the CSV file of the latest player ratings
    player_current_ratings <-
      read_csv(
        paste0("https://www.dropbox.com/s/biiuxon7wxsjopl/",
               "Player-ratings-output.csv?dl=1")
      )
    # Update progress bar
    incProgress(1 / num_steps, detail = "Loading full frame history")
    # Read in the CSV file of the week-by-week player ratings
    player_ratings_archive <-
      read_csv(
        paste0("https://www.dropbox.com/s/uf8adydoz4bfoyw/",
               "Frame-scores.csv?dl=1")
      )
    # Update progress bar
    incProgress(1 / num_steps, detail = "Loading missing scorecards info")
    # Read in the CSV file of the missing scorecards details
    missing_scorecards <-
      read_csv(
        paste0("https://www.dropbox.com/s/c7gdnrfr60im2vt/",
               "missing-scorecards.csv?dl=1")
      )
    # Update progress bar
    incProgress(1 / num_steps, detail = "Loading player season summaries")
    # Read in the CSV file of the player record summary details
    player_record_summary <-
      read_csv(
        paste0("https://www.dropbox.com/s/sawkbzbboccuihq/",
               "player-record-summary.csv?dl=1")
      )
    # Update progress bar
    incProgress(1 / num_steps, detail = "Loading head to head records")
    # Read in the CSV file of the head to head summary details
    head_to_head_summary <-
      read_csv(
        paste0("https://www.dropbox.com/s/kff1skpll5bgo61/",
               "head-to-head-summary.csv?dl=1")
      )
  })
  # Create a vector of the current teams this season
  current_season_teams <- player_ratings_archive %>%
    filter(season == max(season)) %>%
    distinct(home_team) %>%
    arrange(home_team)
  current_season_teams <- current_season_teams[["home_team"]]
  # Create a cross tab of missing scorecards by division and season
  crosstab <- missing_scorecards %>%
    count(season, division) %>%
    rename(missing = n) %>%
    spread(division, missing) %>%
    arrange(desc(season))
  colnames(crosstab) <-
    c("season", paste("Division", colnames(crosstab)[2:ncol(crosstab)]))
  crosstab[is.na(crosstab)] <- 0
  # Create a dataframe of the form over the last 5 matches
  player_form <- player_ratings_archive %>%
    mutate(result = ifelse(home_score > away_score, "W",
                           ifelse(home_score == away_score, "D", "L"))) %>%
    rename(player_id = home_player_id, player_name = home_player_name) %>%
    bind_rows(player_ratings_archive %>%
                mutate(result = ifelse(home_score < away_score, "W",
                                       ifelse(home_score == away_score,
                                              "D", "L"))) %>%
                rename(player_id = away_player_id,
                       player_name = away_player_name)) %>%
    group_by(player_id, player_name) %>%
    top_n(5, fixture_date) %>%
    arrange(player_name, fixture_date) %>%
    summarise(form = paste(result, collapse = ""))
  # Create a reactive container to store dataframes that are generated based on
  # user input
  rv <- reactiveValues()
  # Stash the players names and IDs into a dataframe for use across the app
  player_df <- player_current_ratings %>%
    distinct(name, id) %>%
    arrange(name)
  # Create a reactive dataframe of the players who have played enough frames
  # and recently enough to show in the player dashboard
  filtered_in_players <- reactive({
    df <- player_current_ratings %>%
      filter(frames_played >= input$min_frames &
               year(latest_match_date) > input$last_played)
  })
  # Transform the player_ratings_archive and store output in reactiveValues
  # to give a player-centric view of frame history  
  player_frames <- reactive({
    # Lookup the player ID for the chosen player name
    player_id <- player_df %>%
      filter(name == input$choose_player) %>%
      select(id) %>%
      as.integer()
    # Generate player-centric dataframe
    df <- player_ratings_archive %>%
      filter(home_player_id == player_id) %>%
      rename(pts_for = home_score, pts_against = away_score,
             opp_id = away_player_id, rating = post_match_home_rating) %>%
      mutate(home_away = "H", opp_team = away_team,
             won_lost = ifelse(pts_for > pts_against, "W", "L"),
             opponent = paste0(away_player_name, " (",
                               round(post_match_away_rating, 0), ")")) %>%
      select(fixture_date, division, opponent, pts_for, pts_against, won_lost,
             home_away, opp_team, rating, opp_id) %>%
      rbind(player_ratings_archive %>%
              filter(away_player_id == player_id) %>%
              rename(pts_for = away_score, pts_against = home_score,
                     opp_id = home_player_id,
                     rating = post_match_away_rating) %>%
              mutate(home_away = "A", opp_team = home_team,
                     won_lost = ifelse(pts_for > pts_against, "W", "L"),
                     opponent = paste0(home_player_name, " (",
                                       round(post_match_home_rating, 0),
                                       ")")) %>%
              select(fixture_date, division, opponent, pts_for, pts_against,
                     won_lost, home_away, opp_team, rating, opp_id)) %>%
      arrange(desc(fixture_date))
    df
  })
  # Update the choices that appear in the dropdown on the player dashboard tab
  updateSelectizeInput(session = getDefaultReactiveDomain(),
                       inputId = "choose_player",
                       choices = player_df$name,
                       server = TRUE)
  
  # Create a datatable containing the player ratings leaderboard
  output$ratings_table <- DT::renderDataTable({
    df <- filtered_in_players() %>%
      select(name, latest_rating, latest_match_date, frames_played) %>%
      arrange(desc(latest_rating))
    DT::datatable(
      df,
      colnames = c("Name", "Current Rating",
                   "Last Played", "Frames Played")) %>%
      formatRound("latest_rating", 0)
  })
  # Create a datatable containing the player ratings leaderboard
  output$calculated_handicaps <- DT::renderDataTable({
    df <- filtered_in_players() %>%
      select(name, latest_rating, latest_match_date, frames_played) %>%
      arrange(desc(latest_rating))
    best_player <- max(df$latest_rating)
    worst_player <- min(df$latest_rating)
    df$calculated_handicap <-
      input$upper_bound + input$increment -
      (input$upper_bound - input$lower_bound) /
      (best_player - worst_player) * df$latest_rating
    df$calculated_handicap <-
      round(df$calculated_handicap / input$increment, digits = 0) *
      input$increment
    df <- df %>%
      select(name, calculated_handicap)
    rv[["calculated_handicaps"]] <- df
    DT::datatable(
      df,
      colnames = c("Name", "Calculated Handicap")) %>%
      formatRound("calculated_handicap", 0)
  })
  # Downloadable csv of handicaps
  output$download_handicaps <- downloadHandler(
    filename = function() {
      "calculated_handicaps.csv"
    },
    content = function(file) {
      write.csv(rv[["calculated_handicaps"]], file, row.names = FALSE)
    }
  )
  # Create a datatable containing the player's frame history
  output$frame_history <- DT::renderDataTable({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- player_frames() %>%
      select(1:9)
    DT::datatable(
      df,
      colnames = c("Date", "Division", "Opponent", "For", "Against", "Result",
                   "Home/Away", "Opponent's Team", "Rating"),
      rownames = FALSE) %>%
      formatRound("rating", 0)
  })
  # Create a normal table showing the missing scorecards per season and division
  output$missing_summary_normal <- renderTable(
    crosstab,
    bordered = TRUE,
    striped = TRUE,
    digits = 0
  )
  # Create a datatable showing the missing scorecards per selected season and
  #division
  output$missing_scorecards <- DT::renderDataTable({
    df <- missing_scorecards %>%
      mutate(season = season + 2000) %>%
      filter(division %in% input$divisions) %>%
      filter(season %in% input$seasons)
    DT::datatable(
      df,
      caption = "Missing scorecards for selected seasons and divisions",
      colnames = c("Date", "Season", "Division", "Home Team",
                   "Away Team", "Home Score", "Away Score"),
      rownames = FALSE)
  })
  # Create a datatable showing the overall win percentage
  output$overall_win_pct <- DT::renderDataTable({
    df <- player_record_summary %>%
      inner_join(filtered_in_players(), by = c("player_id" = "id")) %>%
      group_by(player_name) %>%
      summarise_at(vars(played, wins), funs(sum)) %>%
      mutate(win_pct = wins / played) %>%
      select(player_name, win_pct, played) %>%
      arrange(desc(win_pct))
    DT::datatable(
      df,
      caption = paste("Overall winning percentage for players who have played",
                      "at least", input$min_frames, "frames and played at",
                      "least one frame since", input$last_played),
      colnames = c("Name", "Win %", "Frames Played"),
      rownames = FALSE) %>%
      formatPercentage("win_pct", digits = 1)
  })
  # Create a datatable showing the head to head summary
  output$head_to_head <- DT::renderDataTable({
    df <- head_to_head_summary %>%
      mutate(record = paste(wins_left, "-", wins_right)) %>%
      select(played, player_name, record, opponent_name)
    DT::datatable(
      df,
      caption = paste("Head to head for all matchups played more than once"),
      colnames = c("Frames Played", "Player 1", "Record", "Player 2"),
      rownames = FALSE)
  })
  # Create a value box for the selected player's current ranking
  output$current_player_ranking <- renderValueBox({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- filtered_in_players() %>%
      arrange(desc(latest_rating))
    current_ranking <- which(df$name == input$choose_player)
    
    valueBox(
      paste0("#", current_ranking, " / ", nrow(df)), "Current Ranking",
      icon = icon("line-chart"), color = "yellow"
    )
  })
  # Create a value box for the selected player's current rating
  output$best_player_rating <- renderValueBox({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- player_frames()
    best_rating <- df %>%
      mutate(best_rating = max(rating))
    best_rating <- best_rating$best_rating
    valueBox(
      formatC(best_rating, big.mark = ",", digits = 0, format = "f"),
      "Best Rating", icon = icon("line-chart"), color = "yellow"
    )
  })
  # Create a value box for the selected player's current rating
  output$current_player_rating <- renderValueBox({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- filtered_in_players()
    current_rating <- df[which(df$name == input$choose_player), ]$latest_rating
    df <- player_frames()
    best_rating <- df %>%
      mutate(best_rating = max(rating))
    best_rating <- best_rating$best_rating
    valueBox(
      formatC(current_rating, big.mark = ",", digits = 0, format = "f"),
      "Current Rating", icon = icon("line-chart"),
      color = ifelse(current_rating >= best_rating, "green", "red")
    )
  })
  # Create a value box for the selected player's overall win percentage
  output$overall_player_win_pct <- renderValueBox({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- player_record_summary %>%
      filter(player_name == input$choose_player) %>%
      group_by(player_name) %>%
      summarise_at(vars(played, wins), funs(sum)) %>%
      mutate(win_pct = wins / played * 100) %>%
      select(win_pct)
    valueBox(
      paste0(formatC(df$win_pct, digits = 1, format = "f"),
             "%"),
      "Overall Win %", icon = icon("line-chart"), color = "yellow"
    )
  })
  # Create a value box for the selected player's overall win percentage
  output$current_player_win_pct <- renderValueBox({
    # Wait until a player has been chosen
    req(input$choose_player)
    df <- player_record_summary %>%
      filter(player_name == input$choose_player) %>%
      group_by(player_name) %>%
      summarise_at(vars(played, wins), funs(sum)) %>%
      mutate(win_pct = wins / played * 100) %>%
      select(win_pct)
    overall_win_pct <- df$win_pct
    current_season <- max(player_record_summary$season)
    df <- player_record_summary %>%
      filter(player_name == input$choose_player,
             season == current_season) %>%
      group_by(player_name) %>%
      summarise_at(vars(played, wins), funs(sum)) %>%
      mutate(win_pct = wins / played * 100) %>%
      select(win_pct)
    current_win_pct <- ifelse(nrow(df) == 0, NA, df$win_pct)
    valueBox(
      ifelse(is.na(current_win_pct), "NA",
             paste0(formatC(current_win_pct, digits = 1, format = "f"),
                    "%")),
      "Current Season Win %", icon = icon("line-chart"),
      color = ifelse(is.na(current_win_pct), "yellow",
                     ifelse(current_win_pct >= overall_win_pct, "green", "red"))
    )
  })
  # Create a table of win % per player by division
  output$best_players_by_division <- DT::renderDataTable({
    req(input$chosen_division)
    chosen_division <- substr(input$chosen_division, 1, 1) %>%
      as.integer()
    df <- player_record_summary %>%
      filter(division == chosen_division) %>%
      inner_join(filtered_in_players(), by = c("player_id" = "id")) %>%
      group_by(player_name, division) %>%
      summarise_at(vars(played, wins), funs(sum)) %>%
      mutate(win_pct = wins / played) %>%
      select(player_name, division, win_pct, played) %>%
      arrange(desc(win_pct))
    DT::datatable(
      df,
      caption =
        paste("Overall winning percentage for players who have played",
              "at least", input$min_frames, "frames (in total) and played at",
              "least one frame since", input$last_played),
      colnames = c("Name", "Division", "Win %", "Frames Played"),
      rownames = FALSE) %>%
      formatPercentage("win_pct", digits = 1)
  })
  # Create a selectizeInput to choose the home team
  output$home_team_selector <- renderUI({
    selectizeInput(
      inputId = "chosen_home_team",
      label = "Pick the home team:",
      choices = current_season_teams,
      selected = current_season_teams[1])
  })
  # Create a selectizeInput to choose the away team
  output$away_team_selector <- renderUI({
    selectizeInput(
      inputId = "chosen_away_team",
      label = "Pick the away team:",
      choices = current_season_teams,
      selected = current_season_teams[length(current_season_teams)])
  })
  # Create a dataframe of the current season results
  current_season_frames <- player_ratings_archive %>%
    filter(season == max(season))
  # Create a reactive dataframe of the players in the home team who have played
  # so far this season
  home_team_players <- reactive({
    current_season_frames %>%
      filter(home_team == input$chosen_home_team) %>%
      rename(player_id = home_player_id, player_name = home_player_name) %>%
      select(player_id, player_name) %>%
      bind_rows(df %>%
                  filter(away_team == input$chosen_home_team) %>%
                  rename(player_id = away_player_id,
                         player_name = away_player_name) %>%
                  select(player_id, player_name)) %>%
      distinct(player_id, player_name) %>%
      inner_join(player_current_ratings,
                 by = c("player_id" = "id", "player_name" = "name")) %>%
      arrange(desc(latest_rating), player_name) %>%
      select(player_id, player_name, latest_rating)
  })
  # Create a reactive dataframe of the players in the away team who have played
  # so far this season
  away_team_players <- reactive({
    current_season_frames %>%
      filter(home_team == input$chosen_away_team) %>%
      rename(player_id = home_player_id, player_name = home_player_name) %>%
      select(player_id, player_name) %>%
      bind_rows(df %>%
                  filter(away_team == input$chosen_away_team) %>%
                  rename(player_id = away_player_id,
                         player_name = away_player_name) %>%
                  select(player_id, player_name)) %>%
      distinct(player_id, player_name) %>%
      inner_join(player_current_ratings,
                 by = c("player_id" = "id", "player_name" = "name")) %>%
      arrange(desc(latest_rating), player_name) %>%
      select(player_id, player_name, latest_rating)
  })
  # Create a DT for the home team rankings
  output$home_team_ratings <- renderDataTable({
    req(input$chosen_home_team)
    df3 <- home_team_players() %>%
      left_join(player_form,
                by = c("player_id", "player_name")) %>%
      select(player_name, latest_rating, form)
    DT::datatable(
      df3,
      caption = "Home team ratings",
      rownames = FALSE,
      colnames = c("Name", "Current rating", "Form")
    ) %>%
      formatRound("latest_rating", digits = 0)
  })
  # Create a DT for the away team rankings
  output$away_team_ratings <- renderDataTable({
    req(input$chosen_away_team)
    df3 <- away_team_players() %>%
      left_join(player_form,
                by = c("player_id", "player_name")) %>%
      ungroup() %>%
      select(player_name, latest_rating, form)
    DT::datatable(
      df3,
      caption = "Home team ratings",
      rownames = FALSE,
      colnames = c("Name", "Current rating", "Form")
    ) %>%
      formatRound("latest_rating", digits = 0)
  })
  # Create a head to head table for all the players involved in both teams
  output$team_head_to_head <- renderDataTable({
    head_to_head_summary_flip <- head_to_head_summary
    original_colnames <- colnames(head_to_head_summary)
    flipped_colnames <- original_colnames[c(3:4, 1:2, 5, 7, 6)]
    colnames(head_to_head_summary_flip) <- flipped_colnames
    relevant_head_to_head <- head_to_head_summary %>%
      bind_rows(head_to_head_summary_flip) %>%
      filter(player_id %in% home_team_players()$player_id,
               opponent_id %in% away_team_players()$player_id) %>%
      arrange(desc(played), player_name) %>%
      mutate(record = paste(wins_left, wins_right, sep = "-")) %>%
      select(played, player_name, record, opponent_name)
    DT::datatable(
      relevant_head_to_head,
      caption = "Head to head records",
      rownames = FALSE,
      colnames = c("Frames played", "Player 1", "Record", "Player 2")
    )
  })
}

shinyApp(ui, server)