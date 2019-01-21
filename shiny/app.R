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
              value = year(Sys.Date()) - 1, sep = "",
              label = "Filter out players who haven't played since before:")
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
              width = 12),
            valueBoxOutput(
              "current_player_rating",
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
        h2("Team tab content")
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
      )
    )
  )
)

server <- function(input, output) {
  # Read in the CSV file of the latest player ratings
  player_current_ratings <-
    read_csv(
      paste0("https://www.dropbox.com/s/biiuxon7wxsjopl/",
             "Player-ratings-output.csv?dl=1")
    )
  # Read in the CSV file of the week-by-week player ratings
  player_ratings_archive <-
    read_csv(
      paste0("https://www.dropbox.com/s/uf8adydoz4bfoyw/",
             "Frame-scores.csv?dl=1")
    )
  # Read in the CSV file of the missing scorecards details
  missing_scorecards <-
    read_csv(
      paste0("https://www.dropbox.com/s/c7gdnrfr60im2vt/",
             "missing-scorecards.csv?dl=1")
    )
  # Read in the CSV file of the player record summary details
  player_record_summary <-
    read_csv(
      paste0("https://www.dropbox.com/s/sawkbzbboccuihq/",
             "player-record-summary.csv?dl=1")
    )
  # Read in the CSV file of the head to head summary details
  head_to_head_summary <-
    read_csv(
      paste0("https://www.dropbox.com/s/kff1skpll5bgo61/",
             "head-to-head-summary.csv?dl=1")
    )
  # Create a cross tab of missing scorecards by division and season
  crosstab <- missing_scorecards %>%
    count(season, division) %>%
    rename(missing = n) %>%
    spread(division, missing) %>%
    arrange(desc(season))
  colnames(crosstab) <-
    c("season", paste("Division", colnames(crosstab)[2:ncol(crosstab)]))
  crosstab[is.na(crosstab)] <- 0
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
               year(latest_match_date) >= input$last_played)
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
    df <- filtered_in_players() %>%
      arrange(desc(latest_rating))
    current_ranking <- which(df$name == input$choose_player)
    valueBox(
      paste0("#", current_ranking, " / ", nrow(df)), "Current Ranking",
      icon = icon("line-chart"), color = "yellow"
    )
  })
  # Create a value box for the selected player's current rating
  output$current_player_rating <- renderValueBox({
    df <- filtered_in_players()
    current_rating <- df[which(df$name == input$choose_player), ]$latest_rating
    valueBox(
      formatC(current_rating, big.mark = ",", digits = 0, format = "f"),
      "Current Rating", icon = icon("line-chart"), color = "blue"
    )
  })
}

shinyApp(ui, server)