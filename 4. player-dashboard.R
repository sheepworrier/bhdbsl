source("common-functions.R")
# Load in frame scores
frame_scores <- read_csv("Frame-scores.csv")
# Get unique player list
players <- frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name) %>%
  select(player_id, player_name) %>%
  rbind(frame_scores %>%
          rename(player_id = away_player_id, player_name = away_player_name) %>%
          select(player_id, player_name)) %>%
  distinct()
# Convert the frame scores to be from both the home and away player prespective
player_record <- frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name,
         player_rating = post_match_home_rating,
         opponent_id = away_player_id, opponent_name = away_player_name,
         opponent_rating = post_match_away_rating,
         pts_for = home_score, pts_against = away_score) %>%
  mutate(home_away = "H") %>%
  rbind(frame_scores %>%
          rename(player_id = away_player_id, player_name = away_player_name,
                 player_rating = post_match_away_rating,
                 opponent_id = home_player_id, opponent_name = home_player_name,
                 opponent_rating = post_match_home_rating,
                 pts_for = away_score, pts_against = home_score) %>%
          mutate(home_away = "A")) %>%
  mutate(wins = ifelse(pts_for > pts_against, 1, 0))
# Calculate player record by season and division
player_record_summary <- player_record %>%
  group_by(player_id, player_name, season, division) %>%
  summarise(played = n(), win_pct = mean(wins), wins = sum(wins),
            pts_for = sum(pts_for), pts_against = sum(pts_against),
            min_rating = min(player_rating),
            max_rating = max(player_rating)) %>%
  mutate(losses = played - wins)
# Calculate head-to-head stats
head_to_head_summary <- player_record %>%
  filter(player_id < opponent_id) %>%
  group_by(player_id, player_name, opponent_id, opponent_name) %>%
  summarise(played = n(), wins_left = sum(wins)) %>%
  filter(played > 1) %>%
  mutate(wins_right = played - wins_left) %>%
  arrange(desc(played))
# Write out to CSVs
write_csv(player_record_summary, "player-record-summary.csv")
write_csv(head_to_head_summary, "head-to-head-summary.csv")