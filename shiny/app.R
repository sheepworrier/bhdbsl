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
    collapsed = TRUE,
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
                         label = "Choose player to display:",
                         options = list(placeholder = "select a player"))
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
  
  updateSelectizeInput(session = getDefaultReactiveDomain(),
                       inputId = "choose_player",
                       choices = unique(player_current_ratings$name),
                       server = TRUE)
  
  # Create a reactive container to store dataframes that are generated based on
  # user input
  rv <- reactiveValues()
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
}

shinyApp(ui, server)