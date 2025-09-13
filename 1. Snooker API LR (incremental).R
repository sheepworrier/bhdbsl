library(httr)
library(jsonlite)
library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(purrr)

# Get the seasons from the LeagueRepublic API ####
seasons_resp <-
  GET("https://api.leaguerepublic.com/json/getSeasonsForLeague/976459074.json")
seasons_cont <- content(seasons_resp)
seasons_df <-tibble(data = seasons_cont) |>
  unnest_wider(data)

## Get the current season ID ####
current_season <- 24
current_season_id <- seasons_df |>
  filter(str_detect(seasonName, paste0("20", current_season))) |>
  pull(seasonID)

# Grab every match result and the link to the match details page ####
matches_resp <-
  GET(paste0("https://api.leaguerepublic.com/json/getFixturesForSeason/",
             current_season_id, ".json"))
matches_cont <- content(matches_resp)
matches_df <- tibble(data = matches_cont) |>
  unnest_wider(data)

## Convert to match the format used in the CSV ####
results_new <- matches_df |>
  filter(fixtureGroupDesc %in% c("Premier Division", "Division 1")) |>
  mutate(fixture_date = as.POSIXct(fixtureDateInMilliseconds / 1000,
                                   origin = "1970-01-01",
                                   tz = "Europe/London") |>
           as.Date(),
         season = current_season,
         division = if_else(fixtureGroupDesc == "Premier Division", 1, 2),
         home_score = as.integer(homeScore),
         away_score = as.integer(roadScore)) |>
  select(fixture_date, season, division, home_team = homeTeamName,
         away_team = roadTeamName, home_score, away_score, fixtureID)

# Read in the old results from the CSV ####
results_old <- read_csv("New-website-match-scores.csv",
                        col_types = cols(
                          fixture_date = col_date(),
                          season = col_integer(),
                          division = col_integer(),
                          home_team = col_character(),
                          away_team = col_character(),
                          home_score = col_integer(),
                          away_score = col_integer(),
                          URLs = col_character())) %>%
  filter(!is.na(home_score))

# Read in the formerly scraped frame scores ####
frame_scores_old <- read_csv("New-website-frame-scores.csv",
                             col_types = cols(
                               fixture_date = col_date(),
                               season = col_integer(),
                               division = col_integer(),
                               home_team = col_character(),
                               away_team = col_character(),
                               home_player_id = col_integer(),
                               home_player_name = col_character(),
                               home_score = col_integer(),
                               away_player_id = col_integer(),
                               away_player_name = col_character(),
                               away_score = col_integer()))
# Calculate which old results have no frame scores ####
results_to_scrape <- results_new |>
  anti_join(frame_scores_old, by = c("fixture_date", "season", "division",
                                     "home_team", "away_team")) |>
  select(-c(home_score, away_score))

get_match_details_from_api <- function(fixture_id, fixture_date, season,
                                       division, home_team, away_team) {
  print(paste("Processing fixture", fixture_id))
  # Get match details from API ####
  match_details_resp <-
    GET(paste0("https://api.leaguerepublic.com/json/getFullFixtureDetails/",
               fixture_id, ".json"))
  match_details_cont <- content(match_details_resp)
  ## Get details of available home and away players ####
  av_home_players <- tibble(data = list(match_details_cont)) |>
    hoist(data,
          availableHomePlayers = "availableHomePlayers") |>
    unnest_longer(availableHomePlayers) |>
    unnest_wider(availableHomePlayers) |>
    rename(home_firstName = firstName, home_lastName = lastName,
           home_player_id = personID) |>
    select(-data, -handicapValue)
  av_away_players <- tibble(data = list(match_details_cont)) |>
    hoist(data,
          availableRoadPlayers = "availableRoadPlayers") |>
    unnest_longer(availableRoadPlayers) |>
    unnest_wider(availableRoadPlayers) |>
    rename(road_firstName = firstName, road_lastName = lastName,
           away_player_id = personID) |>
    select(-data, -handicapValue)
  ## Get frame scores ####
  frame_scores <- tibble(data = list(match_details_cont)) |>
    hoist(data,
          gameGroups = c("gameFormat", "gameGroups")) |>
    unnest_longer(gameGroups) |>
    unnest_wider(gameGroups) |>
    unnest_longer(games) |>
    hoist(games,
          home_score = "homeScoreLevel1",
          away_score = "roadScoreLevel1",
          homePlayers = "homePlayers",
          roadPlayers = "roadPlayers") |>
    unnest_longer(homePlayers) |>
    unnest_longer(roadPlayers) |>
    hoist(homePlayers,
          home_firstName = "firstName",
          home_lastName = "lastName") |>
    hoist(roadPlayers,
          road_firstName = "firstName",
          road_lastName = "lastName") |>
    select(-data, -gameGroupDesc) |>
    left_join(av_home_players,
              join_by(home_firstName, home_lastName)) |>
    left_join(av_away_players,
              join_by(road_firstName, road_lastName)) |>
    mutate(fixture_date = fixture_date, season = season, division = division,
           home_team = home_team, away_team = away_team, fixtureID = fixture_id,
           home_player_name = paste(home_firstName, home_lastName),
           away_player_name = paste(road_firstName, road_lastName),
           home_score = as.integer(home_score),
           away_score = as.integer(away_score)) |>
    select(fixture_date, season, division, home_team, away_team, home_player_id,
           home_player_name, home_score, away_player_id, away_player_name,
           away_score, fixtureID)
  ## Get break details ####
  breaks <- bind_rows(
    ### Home team break details ####
    if(is.null(match_details_cont$homeLeagueStatTypes)) {
      data.frame()
    } else {
      tibble(data = list(match_details_cont)) |>
        hoist(data,
              homeLeagueStatTypes = "homeLeagueStatTypes") |>
        unnest_longer(homeLeagueStatTypes) |>
        hoist(homeLeagueStatTypes,
              home_statistics = "statistics") |>
        unnest_longer(home_statistics) |>
        unnest_wider(home_statistics) |>
        mutate(player_name = paste(firstName, lastName),
               high_break = statisticValue) |>
        left_join(av_home_players,
                  join_by(firstName == home_firstName,
                          lastName == home_lastName)) |>
        select(player_id = home_player_id, player_name, high_break) |>
        mutate(high_break = as.integer(high_break),
               fixtureID = fixture_id)
    },
    ### Away team break details ####
    if(is.null(match_details_cont$roadLeagueStatTypes)) {
      data.frame()
    } else {
      tibble(data = list(match_details_cont)) |>
        hoist(data,
              roadLeagueStatTypes = "roadLeagueStatTypes") |>
        unnest_longer(roadLeagueStatTypes) |>
        hoist(roadLeagueStatTypes,
              road_statistics = "statistics") |>
        unnest_longer(road_statistics) |>
        unnest_wider(road_statistics) |>
        mutate(player_name = paste(firstName, lastName),
               high_break = statisticValue) |>
        left_join(av_away_players,
                  join_by(firstName == road_firstName,
                          lastName == road_lastName)) |>
        select(player_id = away_player_id, player_name, high_break) |>
        mutate(high_break = as.integer(high_break),
               fixtureID = fixture_id)
    }
  )
  ### Pause for 1s to avoid throttling ####
  Sys.sleep(1)
  
  return(list(fs = frame_scores, b = breaks))
}

# Run the UDF for all new matches ####
matches_list <-
  pmap(results_to_scrape |>
         select(fixtureID, fixture_date, season, division, home_team,
                away_team) |>
         unname(),
       get_match_details_from_api)

# FOR DEBUGGING
# ii <- 195
# foo <- get_match_details_from_api(
#   results_to_scrape$fixtureID[ii], results_to_scrape$fixture_date[ii],
#   results_to_scrape$season[ii], results_to_scrape$division[ii],
#   results_to_scrape$home_team[ii], results_to_scrape$away_team[ii])
# fixture_id <- results_to_scrape$fixtureID[ii]
# fixture_date <- results_to_scrape$fixture_date[ii]
# season <- results_to_scrape$season[ii]
# division <- results_to_scrape$division[ii]
# home_team <- results_to_scrape$home_team[ii]
# away_team <- results_to_scrape$away_team[ii]
# foo$fs

# Save the new frame scores ####
match_details_list <- map(matches_list, "fs")
frame_scores_new <- bind_rows(match_details_list)

# Save the new frame scores ####
breaks_list <- map(matches_list, "b")
breaks_new <- bind_rows(breaks_list)

# Combine old and new ####
frame_scores_total <- frame_scores_old |>
  anti_join(frame_scores_new,
            join_by(fixture_date, season, division, home_team, away_team)) |>
  bind_rows(frame_scores_new)
results_total <- results_old |>
  anti_join(results_new,
            join_by(fixture_date, season, division, home_team, away_team)) |>
  bind_rows(results_new)

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "New-website-frame-scores.csv")
write_csv(results_total, "New-website-match-scores.csv")
#write_csv(breaks_new, "New-website-breaks.csv", append = TRUE)
