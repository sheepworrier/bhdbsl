library(rvest)
library(httr)
library(tidyverse)
library(purrr)
library(RSelenium)
library(stringr)

# Open a selenium session
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)
remDr$open()

get_season_division_results <- function(season, division, url, sport,
                                        league = "Brighton") {
  # Create session
  remDr$navigate(url)
  session <- remDr$getPageSource()[[1]] %>%
    read_html()
  # Read in the number of results pages
  num_pages_nodes <- session %>%
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
  arg_list <- list(rep(substr(url, 1, str_length(url) - 7), num_pages),
                   rep(season, num_pages),
                   rep(division, num_pages),
                   seq(1, num_pages),
                   rep(sport, num_pages),
                   rep(league, num_pages))
  # Run get_single_results_page successively to populate the result_table
  result_table <- pmap_dfr(arg_list, get_single_results_page)
  result_table
}

get_single_results_page <- function(base_url, season, division, page_number,
                                    sport, league = "Brighton") {
  # Print to console to track progress
  print(paste("Scraping page", page_number, "for division", division,
              "and season", season))
  # Contruct URL for the results page number in question
  results_page_url <- paste0(base_url, "/", page_number, ".html")
  # Create session
  remDr$navigate(results_page_url)
  session <- remDr$getPageSource()[[1]] %>%
    read_html()
  # Use rvest to extract the table of up to 20 results into a dataframe
  results_table <- session %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill=TRUE)
  # Remove any notes that have been applied to any of the match results
  # if (sport != "Snooker comp") {
  #   if (is.na(results_table[1, 1])) {
  #     results_table <- results_table[is.na(results_table[, 1]), ]    
  #   } else {
  #     results_table <- results_table[nchar(results_table[, 1]) == 0, ]
  #   }
  # }
  # The final column of the above table contains a URL for the match details
  match_detail_urls <- session %>%
    html_nodes("tbody tr") %>%
    html_attr("data-match-href")

  # Construct final results table for this match - slightly different format if
  # snooker or billiards
  if (sport == "Snooker") {
    print("Gathering Snooker results")
    # Remove empty columns and process strings to give final results table
    final_results_table <-
      data.frame(fixture_date = as.Date(results_table$`Date Time`,
                                        format = "%d/%m/%y"),
                 season = season,
                 division = division,
                 home_team = results_table$`Home Team`,
                 away_team = results_table$`Away Team`,
                 home_score = as.integer(substr(results_table$Score, 1, 1)),
                 away_score = as.integer(substr(results_table$Score, 5, 5)),
                 URLs = paste0("http://brightonhovedistrictsnooker.",
                               "leaguerepublic.com",
                               match_detail_urls),
                 stringsAsFactors = FALSE) %>%
      filter(!is.na(home_score))
  } else if (sport == "Billiards") {
    print("Gathering Billiards results")
    # Rename the columns without a header
    colnames(results_table) <- c("Date Time", "Home Team", "Score", "Away Team",
                                 "Division")
    if(league == "Brighton") {
      # Reformat the scores column to split between home and away, scoring and
      # overall
      results_table <- results_table %>%
        mutate(Score = str_remove_all(Score, "\\(")) %>%
        mutate(Score = str_remove_all(Score, "\\)")) %>%
        separate(Score, c("home_score", "away_score"), sep = " - ") %>%
        separate(home_score, c("home_sp", "home_op"), sep = "-") %>%
        separate(away_score, c("away_sp", "away_op"), sep = "-") %>%
        mutate_at(vars(home_sp:away_op), list(as.integer))
      
      final_results_table <- results_table %>%
        mutate(fixture_date = as.Date(results_table$`Date Time`,
                                      format = "%d/%m/%y"),
               season = season,
               division = division,
               url =
                 paste0("http://brightonhovedistrictbilliards.leaguerepublic.com",
                        match_detail_urls)) %>%
        select(fixture_date, season, division, home_team = `Home Team`,
               away_team = `Away Team`, home_sp, away_sp, home_op, away_op, url)  %>%
        filter(!is.na(home_sp))
    } else if (league == "Worthing") {
      # Reformat the scores column to split between home and away, scoring and
      # overall
      results_table <- results_table %>%
        mutate(Score = str_remove_all(Score, "\\(")) %>%
        mutate(Score = str_remove_all(Score, "\\)")) %>%
        separate(Score, c("home_score", "away_score"), sep = " - ") %>%
        mutate_at(vars(home_score:away_score), list(as.integer))
      
      final_results_table <- results_table %>%
        mutate(fixture_date = as.Date(results_table$`Date Time`,
                                      format = "%d/%m/%y"),
               season = season,
               division = division,
               url =
                 paste0("http://brightonhovedistrictbilliards.leaguerepublic.com",
                        match_detail_urls)) %>%
        select(fixture_date, season, division, home_team = `Home Team`,
               away_team = `Away Team`, home_sp, away_sp, home_op, away_op, url)  %>%
        filter(!is.na(home_score))
    }
  } else {
    print("Gathering Snooker competition results")
    # Remove empty columns
    # Filter out any walkovers
    final_results_table <- results_table %>%
      mutate(fixture_date = as.Date(results_table$`Date Time`,
                                    format = "%d/%m/%y"),
             season = season,
             division = division) %>%
      select(fixture_date, season, division, home_team = `Home Team`,
             away_team = `Away Team`, home_score, away_score) %>%
      filter(!is.na(home_score))
  }
  final_results_table
}

scrape_match_page <-
  function(fixture_date, season, division, home_team, away_team, url,
           sport = "Snooker", league = "Brighton") {
    print(paste("Scraping", url))
    # Create session
    remDr$navigate(url)
    session <- remDr$getPageSource()[[1]] %>%
      read_html()
    # Look for the table of frame scores which may or may not exist
    potential_frame_table <- session %>%
      html_nodes(".spacer-bottom table")
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
                   away_score = frame_table$away_score,
                   stringsAsFactors = FALSE)
      frame_scores$fixture_date <-
        as.Date(frame_scores$fixture_date, origin = "1970-01-01")
      # For Billiards look at the subsequent tables on scoring points to get the
      # handicaps used for this match
      if (sport == "Billiards") {
        # Scrape the 2-3 stats tables at the bottom of the match page
        stats_tables <- session %>%
          html_nodes(".table-scroll table")
        # Turn the 1st scoring points table into a dataframe
        sp_home_table <- stats_tables %>%
          .[[2]] %>%
          html_table(fill=TRUE) %>%
          mutate(home_player_name = str_extract(Player, "[^\\[]+") %>%
                   trimws("right"),
                 home_player_handicap =
                   str_match(Player, "\\[(-?\\d+)\\]")[,2] %>%
                   as.numeric()) %>%
          select(home_player_name, home_player_handicap, home_player_sp = SPF)
        # Turn the 2nd scoring points table into a dataframe
        sp_away_table <- stats_tables %>%
          .[[3]] %>%
          html_table(fill=TRUE) %>%
          mutate(away_player_name = str_extract(Player, "[^\\[]+") %>%
                   trimws("right"),
                 away_player_handicap =
                   str_match(Player, "\\[(-?\\d+)\\]")[,2] %>%
                   as.numeric()) %>%
          select(away_player_name, away_player_handicap, away_player_sp = SPF)
        # Combine into the main frame scores table
        frame_scores <- frame_scores %>%
          left_join(sp_home_table) %>%
          left_join(sp_away_table)
      }
    } else {
      frame_scores <-
        data.frame(fixture_date = as.Date(character()),
                   season = numeric(),
                   division = numeric(),
                   home_team = character(),
                   away_team = character(),
                   home_player_id = character(),
                   home_player_name = character(),
                   home_score = numeric(),
                   away_player_id = character(),
                   away_player_name = character(),
                   away_score = numeric(),
                   stringsAsFactors = FALSE)
    }
    # Look for the table of breaks which may or may not exist
    potential_break_tables <- session %>%
      html_nodes("table")
    if (length(potential_break_tables) > 2) {
      # First and last tables are not what we are looking for, so process
      # only the one or two in the middle
      for (i in 2:(length(potential_break_tables) - 1)) {
        # If it exists then read into a dataframe
        breaks_table <- potential_break_tables[[i]] %>%
          html_table(fill=TRUE)
        # Check if the column heading is High Break
        if (colnames(breaks_table)[1] == "High Break") {
          colnames(breaks_table) <- c("player_name", "high_break")
          # Remove the handicap from the player name
          breaks_table$player_name <-
            str_replace(breaks_table$player_name, " \\[[0-9]+\\]", "")
          # Read in the player URLs so that we can parse to get the player ID
          player_urls <- potential_break_tables[[i]] %>%
            html_nodes("a") %>%
            html_attr("href")
          # Parse URLs to exrtact the player IDs
          player_ids <- sub("^.+/", "", sub(".html$", "", player_urls))
          # Construct (almost) final output table.  Just need to add in player
          # IDs
          breaks <-
            data.frame(fixture_date,
                       season,
                       division,
                       player_id = player_ids,
                       player_name = breaks_table$player_name,
                       high_break = breaks_table$high_break,
                       stringsAsFactors = FALSE)
          breaks$fixture_date <-
            as.Date(breaks$fixture_date, origin = "1970-01-01")
          breaks_new <<- bind_rows(breaks_new, breaks) 
        }
      }
    }
    sleep_time <- rnorm(1, 6, 2) %>% round()
    if(sleep_time < 2) { sleep_time <- 5}
    print(paste("Sleeping for", sleep_time, "seconds"))
    Sys.sleep(sleep_time)
    frame_scores
  }

end_of_season_adjustments <-
  function(last_season, next_season) {
  # Derive the most recent ranking per division per player
  most_recent_rating_per_player_per_div <-
    player_seasons_majority %>%
    filter(season <= last_season) %>%
    group_by(player_id, majority_division) %>%
    arrange(player_id, majority_division, desc(season)) %>%
    slice(1) %>%
    ungroup()
  # Calculate the change in division (promotion or relegation) and set
  # adjustment to be 1/2 of the gap between divisions.  Positive for promotion,
  # negative for relegation
  players_to_adjust <- player_seasons_majority %>%
    filter(season == last_season) %>%
    inner_join(player_seasons_majority %>%
                 filter(season == next_season),
               by = c("player_id")) %>%
    filter(majority_division.x != majority_division.y) %>%
    left_join(division_ratings_range,
              by = c("majority_division.y" = "majority_division")) %>%
    mutate(latest_rating = if_else(majority_division.x > majority_division.y,
                                   max(lowest_rating, eos_rating.x),
                                   min(highest_rating, eos_rating.x))) %>%
    rename(player_name = player_name.x) %>%
    select(player_id, player_name, latest_rating)
  players_to_adjust
}

save_off_end_of_season_ratings <-
  function(last_season, player_seasons_majority) {
  # Anti join with the 
  df <- player_seasons_majority %>%
      inner_join(player_ratings %>%
                   select(player_id, latest_rating), by = "player_id") %>%
    filter(season == last_season) %>%
    mutate(eos_rating = latest_rating) %>%
    select(-latest_rating)
  # Return the results
  df
  }
