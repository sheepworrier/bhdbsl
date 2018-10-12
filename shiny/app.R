library(shiny)
library(DT)

ui <- navbarPage(
  title = "BHDBSL Results",
  tabPanel("Player ratings (minimum 20 frames)", DT::dataTableOutput('playerRatings')),
  tabPanel("Player breakdown",
           sidebarLayout(
             sidebarPanel(
               uiOutput("choose_player")
             ),
             mainPanel(
               plotOutput("plot"),
               hr(),
               DT::dataTableOutput('playerResults')
             )
           )
  )
  #,tabPanel("Player breakdown", DT::dataTableOutput('playerRatings'))
)

server <- function(input, output) {

  playerRatings <- read.csv("https://www.dropbox.com/s/biiuxon7wxsjopl/Player-ratings-output.csv?dl=1")
  playerRatingsOuptut <- data.frame(playerRatings$name,
                                    playerRatings$latest_rating,
                                    playerRatings$latest_match_date,
                                    playerRatings$frames_played)
  playerRatingsOuptut <- subset(playerRatingsOuptut, playerRatings.frames_played >= 20)
  colnames(playerRatingsOuptut) <- c("name", "Rating", "Most Recent Match", "Total Frames Played")
  playerRatingsOuptut$`Most Recent Match` <- as.Date(playerRatingsOuptut$`Most Recent Match`, origin = "1970-01-01")
  rating.order <- order(playerRatingsOuptut$Rating, decreasing = TRUE)
  playerRatingsOuptut$Rating <- round(playerRatingsOuptut$Rating)
  name.order <- order(playerRatingsOuptut$name, decreasing = FALSE)
  playerNames <- playerRatingsOuptut[name.order, ]
  
  playerWeeklyRatings <- read.csv("https://www.dropbox.com/s/uf8adydoz4bfoyw/Frame-scores.csv?dl=1")
  playerWeeklyRatings$fixture_date <- as.Date(playerWeeklyRatings$fixture_date, origin = "1970-01-01")
  date.order <- order(playerWeeklyRatings$fixture_date, decreasing = FALSE)
  sortedWeeklyRatings <- playerWeeklyRatings[date.order, ]
  
    # display 25 rows initially
  output$playerRatings <- DT::renderDataTable(
    DT::datatable(playerRatingsOuptut[rating.order, ], options = list(pageLength = 25), rownames = FALSE)
  )
  
  ratingLinePlot <- reactive({
    dataToPlotHome <- subset(sortedWeeklyRatings, home_player_name == input$player)
    dataToPlotHome$rating <- dataToPlotHome$post_match_home_rating
    dataToPlotAway <- subset(sortedWeeklyRatings, away_player_name == input$player)
    dataToPlotAway$rating <- dataToPlotAway$post_match_away_rating
    dataToPlot <- rbind(dataToPlotHome, dataToPlotAway)
    date.order <- order(dataToPlot$fixture_date, decreasing = FALSE)
    dataToPlot <- dataToPlot[date.order, ]
    plot(dataToPlot$fixture_date, dataToPlot$rating, type="n", main="Player rating over time", 
         xlab = "Fixture Date", ylab = "Rating") 
    lines(dataToPlot$fixture_date, dataToPlot$rating, type="l") 
  })
  
  output$choose_player <- renderUI({
    selectInput("player", "Choose player", as.list(playerNames$name))
    })
  output$plot <- renderPlot({
    ratingLinePlot()
  })
  
  resultsTable <- reactive({
    dataToPlot <- subset(sortedWeeklyRatings, home_player_name == input$player | away_player_name == input$player)
    dataToPlot <- dataToPlot[c(-6, -9)]
  })

  output$playerResults <- DT::renderDataTable(
    DT::datatable(resultsTable(), options = list(pageLength = 25), rownames = FALSE)
  )
  
}

shinyApp(ui, server)
