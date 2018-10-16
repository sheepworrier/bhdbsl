library(tidyverse)
library(stringdist)
library(zoo)
source("common-functions.R")
# Read in the mappings already defined between players on the old and new
# wesbite
player_id_map <- read_csv("player-id-map.csv")
# Read in the players from the new website we have already decided don't have an
# equivalent on the old website
definitely_unmapped <- read_csv("New-website-players-unmapped.csv")
# Load in the frame scores from the old Trory website 2010-2013
old_frame_scores <- read_csv("Old-website-frame-scores.csv")
# Load in the frame scores from the LR website 2014 onwards
new_frame_scores <- read_csv("New-website-frame-scores.csv")
# Check for any new players that don't appear on the player_id_map and see if
# they resemble any old website player names
unmapped_new_players <- new_frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name) %>%
  distinct(player_id, player_name) %>%
  rbind(new_frame_scores %>%
          rename(player_id = away_player_id, player_name = away_player_name) %>%
          distinct(player_id, player_name)) %>%
  distinct(player_id, player_name) %>%
  anti_join(player_id_map, by = c("player_id" = "new_player_id")) %>%
  anti_join(definitely_unmapped, by = "player_id")
# Identify players from the old website who aren't yet mapped to a new website
# player
unmapped_old_players <- player_id_map %>%
  filter(player_id == new_player_id)
# Check if there are any new players to map
if(nrow(unmapped_new_players) > 0) {
  # Calculate  Jaro Winckler string distance between old names and new
  dist <- stringdistmatrix(unmapped_old_players$full_name,
                           unmapped_new_players$player_name,
                           method = "jw")
  row.names(dist) <- as.character(unmapped_old_players$full_name)
  colnames(dist) <- as.character(unmapped_new_players$player_name)
  # Select the closest match for each (old) player
  output <-
    data.frame(
      unmapped_old_players,
      word_close = unmapped_new_players[as.numeric(apply(dist,
                                                         1, which.min)),
                                        "player_name"],
      dist_min = apply(dist, 1, min, na.rm = TRUE))
  colnames(output) <- c("player_id", "first_name", "last_name", "full_name",
                        "new_full_name", "distance")
  # Manually add matches to player-id.csv, then reload into a dataframe
  player_id_map <- read_csv("player-id-map.csv")
  # Any players on the new website that are not already mapped can be discounted
  # next time
  still_unmapped_new_players <- unmapped_new_players %>%
    anti_join(player_id_map, by = c("player_id" = "new_player_id"))
  # Write to CSV file for use next iteration to exclude from fuzzy matching
  write_csv(still_unmapped_new_players, "New-website-players-unmapped.csv") 
}
# Update the home / away player_ids and names for the old website frame_scores
old_frame_scores <- old_frame_scores %>%
  inner_join(player_id_map, by = c("home_player_id" = "player_id")) %>%
  mutate(home_player_id = new_player_id, home_player_name = full_name) %>%
  select(1:11) %>%
  inner_join(player_id_map, by = c("away_player_id" = "player_id")) %>%
  mutate(away_player_id = new_player_id, away_player_name = full_name) %>%
  select(1:11)
# Combine frame scores from the old and the new websites
frame_scores <- rbind(old_frame_scores, new_frame_scores)
frame_scores$fixture_date <- as.Date(frame_scores$fixture_date, "%d/%m/%Y")
frame_scores[, c("post_match_home_rating", "post_match_away_rating")] <-NA
frame_scores$home_player_id <- as.character(frame_scores$home_player_id)
frame_scores$away_player_id <- as.character(frame_scores$away_player_id)
# Get number of matches played in each division per season per player
summary1 <- frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name) %>%
  select(player_id, player_name, season, division) %>%
  rbind(frame_scores %>%
          rename(player_id = away_player_id, player_name = away_player_name) %>%
          select(player_id, player_name, season, division)) %>%
  count(player_id, player_name, season, division) %>%
  rename(num_frames = n)
# Pick the main division per season per player based on where they played the
# majority of their matches.  In case of a tie, choose the hgher division
summary2 <- summary1 %>%
  select(player_id, player_name, season, division, num_frames) %>%
  inner_join(summary1 %>%
               group_by(player_id, season) %>%
               summarise_at(vars(num_frames), max),
             by = c("num_frames", "player_id", "season")) %>%
  group_by(player_id, player_name, season) %>%
  summarise_at(vars(division), min) %>%
  rename(majority_division = division)
# Finally summarise further to find the first season a player played a frame and
# which division they played the majority of them in
summary3 <- summary2 %>%
  inner_join(summary2 %>%
               group_by(player_id) %>%
               summarise_at(vars(season), min),
             by = c("player_id", "season"))

# Based on analysis top Division is 5.78 times stronger than 6th top and so on.
# If we start at 1500 in the top division calculate the starting rating for the
# other divisions and assign to each player
starting_ranking <-
  data.frame(division = c(1:6),
             rel_div_strength = c(5.78, 2.81, 2.4, 1.96, 1.57, 1))
starting_ranking$value <-
  1500 / starting_ranking$rel_div_strength[1] *
  starting_ranking$rel_div_strength
player_ratings <- summary3 %>%
  inner_join(starting_ranking, by = c("majority_division" = "division"))
player_ratings$latest_rating <- player_ratings$value
player_ratings$latest_fixture_date <- as.Date("2010-01-01")
player_ratings[, c("frames_played")] <- 0
player_ratings <- as.data.frame(player_ratings)
rownames(player_ratings) <- player_ratings$player_id
# Sort the frame_scores by date, division and home_team
## Possible issue here where Premier division players can play twice on the same
## night, but it *seems* to preserve scorecard order
frame_scores <- frame_scores %>%
  arrange(fixture_date, division, home_team)
# Iterate through frame_scores, retrieve the player rating before each frame and
# update with the player rating for both players after each frame
weight_value <- 20
last_season <- 10
# Create a dataframe to handle players taking whole seasons off
seasons <- data.frame(season = unique(summary2$season), join_condition = 1)
players <- summary2 %>%
  distinct(player_id, player_name) %>%
  mutate(join_condition = 1)
player_seasons <- players %>%
  inner_join(seasons, by = "join_condition") %>%
  select(player_id, player_name, season)
player_seasons_majority <- player_seasons %>%
  left_join(summary2, by = c("player_id", "player_name", "season")) %>%
  group_by(player_id, player_name) %>%
  mutate_at(vars(majority_division), funs(na.locf(., na.rm = FALSE))) %>%
  filter(!is.na(majority_division))
# Loop through all frame scores
for(i in 1:nrow(frame_scores)) {
  row <- frame_scores[i, ]
  print(paste("Iteration", i, "for season", row$season))
  # At the change in seasons, apply adjustments to account for promotions and
  # relegations
  if (row$season != last_season) {
    print(paste("Making adjustments at end of season", last_season))
    players_to_adjust <- end_of_season_adjustments(last_season, row$season)
    player_ratings <- player_ratings %>%
      left_join(players_to_adjust, by = "player_id") %>%
      mutate_if(is.numeric, funs(ifelse(is.na(.), latest_rating, .))) %>%
      mutate(latest_rating = new_rating) %>%
      select(1:9)
    rownames(player_ratings) <- player_ratings$player_id
    last_season <- row$season
  }
  # Set up temporary dataframes for home and away player from the player_ratings
  # dataframe
  home_player <- player_ratings %>%
    filter(player_id == row$home_player_id)
  away_player <- player_ratings %>%
    filter(player_id == row$away_player_id)
  # Home / away player rankings are either the latest if they have played a
  # frame or their starting values
  home_player_ranking <- home_player$latest_rating
  away_player_ranking <- away_player$latest_rating
  # Determine the winner of the frame - home or away
  home_frames_won <- ifelse(row$home_score > row$away_score, 1, 0)
  away_frames_won <- 1 - home_frames_won
  # Calculate new ranking for home / away players
  home_player_new_ranking <-
    home_player_ranking +
    weight_value * (home_frames_won - home_player_ranking /
                      (home_player_ranking + away_player_ranking))
  away_player_new_ranking <-
    away_player_ranking +
    weight_value * (away_frames_won - away_player_ranking /
                      (home_player_ranking + away_player_ranking))
  # Update new ranking alongside frame scores
  frame_scores[i, 12] <- home_player_new_ranking
  frame_scores[i, 13] <- away_player_new_ranking
  # Update player rating table ready for nect iteration involving this player
  player_ratings[home_player$player_id, 7] <- home_player_new_ranking
  player_ratings[home_player$player_id, 8] <- row$fixture_date
  player_ratings[home_player$player_id, 9] <- home_player$frames_played + 1
  player_ratings[away_player$player_id, 7] <- away_player_new_ranking
  player_ratings[away_player$player_id, 8] <- row$fixture_date
  player_ratings[away_player$player_id, 9] <- away_player$frames_played + 1
}

# Tidy up output for CSV
player_ratings_output <-
  player_ratings[, c(3:4, 1:2, 6:9)]
colnames(player_ratings_output) <-
  c("debut_season", "debut_division", "id", "name",
    "initial_rating", "latest_rating", "latest_match_date", "frames_played")
player_ratings_output$debut_season <- player_ratings_output$debut_season + 2000

write_csv(player_ratings_output, "Player-ratings-output.csv")
write_csv(frame_scores, "Frame-scores.csv")