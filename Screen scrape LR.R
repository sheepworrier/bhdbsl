library(rvest)
library(httr)
library(tidyverse)
library(purrr)
# Import the reference data containing the results URLs per season per division
ref_data <- read_csv("Snooker-Results-pages-per-season.csv")

get_season_division_results <- function(season, division, url) {
  # Use httr to load the results page 1 of X
  results_1st_page <- read_html(GET(url, add_headers('user-agent' = 'r')))
  # Read in the number of results pages
  num_pages_nodes <- results_1st_page %>%
    html_node("#hide-container li:nth-child(5) a")
  # Handle the case where the season is incomplete and we only have 1 page of
  # results
  if (length(num_pages_nodes) > 0) {
    num_pages <- num_pages_nodes %>%
      html_text() %>%
      as.integer()
  } else {
    num_pages <- 1
  }
  print(paste("There are", num_pages, "pages of results"))
  # Create an argument list to pass through to get_single_results_page
  arg_list <- list(rep(substr(url, 1, str_length(url) - 5), num_pages),
                   rep(season, num_pages),
                   rep(division, num_pages),
                   seq(1, num_pages))
  # Run get_single_results_page successively to populate the result_table
  result_table <- pmap_dfr(arg_list, get_single_results_page)
  result_table
}

get_single_results_page <- function(base_url, season, division, page_number) {
  # Print to console to track progress
  print(paste("Scraping page", page_number, "for division", division,
              "and season", season))
  # Contruct URL for the results page number in question
  results_page_url <- paste0(base_url, "/", page_number, ".html")
  print(results_page_url)
  # Use httr to load the results page X
  results_page <- read_html(GET(results_page_url,
                                add_headers('user-agent' = 'r')))
  # Use rvest to extract the table of up to 20 results into a dataframe
  results_table <- results_page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill=TRUE)
  # The final column of the above table contains a URL for the match details
  match_detail_urls <- results_page %>%
    html_nodes("td:nth-child(9) a") %>%
    html_attr("href")
  # Convert the  datetime format to DD/MM/YYYY
  results_table$`Date Time` <- paste0(substr(results_table$`Date Time`, 1, 6),
                                      "20",
                                      substr(results_table$`Date Time`, 7, 8))
  
  # Remove empty columns and process strings to give final results table
  final_results_table <-
    data.frame(fixture_date = results_table$`Date Time`,
               season = season,
               division = division,
               home_team = results_table$`Home Team`,
               away_team = results_table$`Away Team`,
               home_score = as.integer(substr(results_table$Score, 1, 1)),
               away_score = as.integer(substr(results_table$Score, 5, 5)),
               stringsAsFactors = FALSE)
  # Set absolute URL of match details pages
  final_results_table$URLs <-
    paste0("http://brightonhovedistrictsnooker.leaguerepublic.com",
           match_detail_urls)
  final_results_table
}
# Grab every match result and the link to the match details page
# results <- pmap_dfr(unname(ref_data), get_season_division_results)

results <- get_season_division_results(ref_data$Season[[15]],
                                       ref_data$Division[[15]],
                                       ref_data$`Results URL`[[15]])

# scrapeMatchPage <- function()
# print(paste("Scraping page", pageNum))
# 
# matchPageURL <-
#   # "http://brightonhovedistrictsnooker.leaguerepublic.com/l/match/18529450.html"
#   paste0("http://brightonhovedistrictsnooker.leaguerepublic.com/l/match/18526176.html")
# matchPage <- read_html(GET(matchPageURL, add_headers('user-agent' = 'r')))
# potentialFrameTable <- matchPage %>%
#   html_nodes(".divider-x2 table")
# if(length(potentialFrameTable) > 0) {
#   frameTable <- potentialFrameTable %>%
#     .[[1]] %>%
#     html_table(fill=TRUE)
#   colnames(frameTable) <- c("homePlayerName", "homeScore", "awayPlayerName", "awayScore")
#   frameTable$fixtureDate <- paste0(substr())
# }

