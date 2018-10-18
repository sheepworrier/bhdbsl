library(shiny)
library(shinydashboard)
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
        h2("Missing scorecards tab content")
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
  # Derive the missing scorecards
  missing_scorecards <- 1
  # Create a reactive container to store dataframes that are generated based on
  # user input
  rv <- reactiveValues()
  # Stash the players names and IDs into a dataframe for use across the app
  player_df <- player_current_ratings %>%
    distinct(name, id) %>%
    arrange(name)
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
    df <- player_current_ratings %>%
      filter(frames_played >= input$min_frames &
               year(latest_match_date) >= input$last_played) %>%
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
}

shinyApp(ui, server)