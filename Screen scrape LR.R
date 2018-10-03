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
    html_nodes(".pagination a")
  # Handles the case where the season is incomplete or we have less than 5 pages
  # of results
  if (length(num_pages_nodes) == 0) {
    num_pages <- 1
  } else {
    num_pages <- num_pages_nodes %>%
      html_text() %>%
      as.integer() %>%
      max(na.rm = TRUE)
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
results <- read_csv("New-website-match-scores.csv")

scrape_match_page <-
  function(fixture_date, season, division, home_team, away_team, home_score,
           away_score, url) {
    print(paste("Scraping", url))
    # Use httr ro get the web page containing the match details
    match_page <- read_html(GET(url, add_headers('user-agent' = 'r')))
    # Look for the table of frame scores which may or may not exist
    potential_frame_table <- match_page %>%
      html_nodes(".divider-x2 table")
    # Check whether table exists
    if(length(potential_frame_table) > 0) {
      # If it exists then read into a dataset
      frame_table <- potential_frame_table %>%
        .[[1]] %>%
        html_table(fill=TRUE)
      # Read in the player URLs so that we can parse to get the player ID
      player_urls <- potential_frame_table %>%
        .[[1]] %>%
        html_nodes("a") %>%
        html_attr("href")
      # Parse URLs to exrtact the player IDs
      player_ids <- sub("^.+/", "", sub(".html$", "", player_urls))
      player_id_matrix <-
        t(matrix(player_ids, nrow = 2, ncol = length(player_ids) / 2))
      # Rename column names (as those on the website aren't that helpful)
      colnames(frame_table) <-
        c("home_player_name", "home_score", "away_score", "away_player_name")
      # Construct (almost) final output table.  Just need to add in player IDs
      frame_scores <-
        data.frame(fixture_date,
                   season,
                   division,
                   home_team,
                   away_team,
                   home_player_id = player_id_matrix[, 1],
                   home_player_name = frame_table$home_player_name,
                   home_score = frame_table$home_score,
                   away_player_id = player_id_matrix[, 2],
                   away_player_name = frame_table$away_player_name,
                   away_score = frame_table$away_score)
      frame_scores
    }
  }

# Cloudflare blocks IPs when too many are attempted in quick succession, so
# will break them down by season
results_14 <- results %>% filter(season == 14)
results_15 <- results %>% filter(season == 15)
results_16 <- results %>% filter(season == 16)
results_17 <- results %>% filter(season == 17)
results_18 <- results %>% filter(season == 18)

# Scrape the frame scores from all of the match pages
frame_scores_14 <- pmap_dfr(unname(results_14), scrape_match_page)
frame_scores_15 <- pmap_dfr(unname(results_15), scrape_match_page)
frame_scores_16 <- pmap_dfr(unname(results_16), scrape_match_page)
frame_scores_17 <- pmap_dfr(unname(results_17), scrape_match_page)
frame_scores_18 <- pmap_dfr(unname(results_18), scrape_match_page)

# Combine and filter out BYEs
frame_scores_total <- rbind(frame_scores_14
                            , frame_scores_15
                            , frame_scores_16
                            , frame_scores_17
                            , frame_scores_18
                            )
frame_scores_total <- frame_scores_total %>%
  filter(home_player_id != "" & away_player_id != "")

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "New-website-frame-scores.csv")
write_csv(results, "New-website-match-scores.csv")
