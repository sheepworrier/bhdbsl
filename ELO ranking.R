library(tidyverse)
frame_scores <- read_csv("Old-website-frame-scores.csv")

# Create a unique list of player IDs to use to map the results from the LR
# website from 2014 onwards
player_id_map <- frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name) %>%
  select(player_id, player_name) %>%
  rbind(frame_scores %>%
          rename(player_id = away_player_id, player_name = away_player_name) %>%
          select(player_id, player_name)) %>%
  distinct() %>%
  mutate(full_name = player_name) %>%
  extract(player_name, c("first_name", "last_name"), "([^ ]+) (.*)") %>%
  arrange(last_name, first_name)
# Add in any new players since 2014
new_players <-
  data.frame(player_id = c(534:536),
             first_name = c("Sean", "Brett", "John"),
             last_name = c("Payne", "Williams", "Derret"))
new_players$full_name <- paste(new_players$first_name, new_players$last_name)
player_id_map <- rbind(player_id_map, new_players)

# Load in the frame scores from the LR website 2014 onwards
new_frame_scores <- read_csv("New-website-frame-scores.csv")
# Merge with the player_id_map datasets to get the home and away player ids
new_frame_scores_with_ids <- new_frame_scores %>%
  left_join(player_id_map %>%
              select(player_id, full_name),
            by = c("home_player_name" = "full_name")) %>%
  rename(home_player_id = player_id) %>%
  left_join(player_id_map %>%
              select(player_id, full_name),
            by = c("away_player_name" = "full_name")) %>%
  rename(away_player_id = player_id)

frame_scores <- rbind(frame_scores, new_frame_scores_with_ids)
frame_scores$fixture_date <- as.Date(frame_scores$fixture_date, "%d/%m/%Y")
frame_scores[, c("post_match_home_rating", "post_match_away_rating")] <-NA

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
player_ratings[, c("latest_rating", "latest_fixture_date")] <- NA
player_ratings[, c("frames_played")] <- 0
rownames(player_ratings) <- player_ratings$player_id

# Iterate through frame_scores, retrieve the player rating before each frame and
# update with the player rating for both players after each frame
weight_value <- 10
for(i in 1:nrow(frame_scores)) {
  print(paste("Iteration", i))
  row <- frame_scores[i, ]
  home_player = subset(player_ratings, player_id == row$home_player_id)
  away_player = subset(player_ratings, player_id == row$away_player_id)
  if (is.na(home_player$latest_rating)) {
    home_player_ranking <- home_player$value
  } else {
    home_player_ranking <- home_player$latest_rating
  }
  if (row$home_score > row$away_score) {
    home_frames_won <- 1
    away_frames_won <- 0
  } else {
    home_frames_won <- 0
    away_frames_won <- 1
  }
  if (is.na(away_player$latest_rating)) {
    away_player_ranking <- away_player$value
  } else {
    away_player_ranking <- away_player$latest_rating
  }
  home_player_new_ranking <- home_player_ranking + weight_value * (home_frames_won - home_player_ranking /
                                                                      (home_player_ranking + away_player_ranking))
  away_player_new_ranking <- away_player_ranking + weight_value * (away_frames_won - away_player_ranking /
                                                                      (home_player_ranking + away_player_ranking))
  frame_scores[i, 12] <- home_player_new_ranking
  frame_scores[i, 13] <- away_player_new_ranking
  player_ratings[player_ratings$player_id == home_player$player_id, 7] <-
    home_player_new_ranking
  player_ratings[player_ratings$player_id == home_player$player_id, 8] <-
    row$fixture_date
  player_ratings[player_ratings$player_id == home_player$player_id, 9] <-
    home_player$frames_played + 1
  player_ratings[player_ratings$player_id == away_player$player_id, 7] <-
    away_player_new_ranking
  player_ratings[player_ratings$player_id == away_player$player_id, 8] <-
    row$fixture_date
  player_ratings[player_ratings$player_id == away_player$player_id, 9] <-
    away_player$frames_played + 1
}

player_ratings_output <- data.frame(player_ratings$season,
                                    player_ratings$majority_division,
                                    player_ratings$player_id,
                                    player_ratings$player_name,
                                    player_ratings$value,
                                    player_ratings$latest_rating,
                                    player_ratings$latest_fixture_date,
                                    player_ratings$frames_played)
colnames(player_ratings_output) <- c("debut_season", "debut_division", "id", "name",
                                   "initial_rating", "latest_rating", "latest_match_date", "frames_played")
player_ratings_output$debut_season <- player_ratings_output$debut_season + 2000

write_csv(player_ratingsOutput, "Player-ratings-output.csv")
write_csv(frame_scores, "Frame-scores.csv")