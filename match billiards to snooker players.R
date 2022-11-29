library(readr)
library(tidylog)
library(dplyr)
library(stringdist)
# Get details of all players who have played league billiards this season
billiards_frames <- read_csv("Billiards-frame-scores.csv")
current_season_billiards_frames <- billiards_frames %>%
  slice_max(season)
current_season_billiards_players <- current_season_billiards_frames %>%
  select(player_id = home_player_id, player_name = home_player_name,
         team = home_team) %>%
  bind_rows(current_season_billiards_frames %>%
              select(player_id = away_player_id, player_name = away_player_name,
                     team = away_team)) %>%
  count(player_id, player_name, team, name = "matches_played")
# Get details of all players who have played league snooker this season
snooker_frames <- read_csv("Frame-scores.csv")
current_season_snooker_frames <- snooker_frames %>%
  slice_max(season)
current_season_snooker_players <- current_season_snooker_frames %>%
  select(player_id = home_player_id, player_name = home_player_name,
         team = home_team) %>%
  bind_rows(current_season_snooker_frames %>%
              select(player_id = away_player_id, player_name = away_player_name,
                     team = away_team)) %>%
  count(player_id, player_name, team, name = "matches_played") %>%
  group_by(player_id, player_name) %>%
  mutate(team = paste(team, collapse = ", ")) %>%
  ungroup() %>%
  group_by(player_id, player_name, team) %>%
  summarise(matches_played = sum(matches_played, na.rm = TRUE)) %>%
  ungroup()
# Read matched players from CSV
billiards_and_snooker_players <- read_csv("billiards-snooker-player-map.csv")
# Add any new players to the CSV and reload
current_season_billiards_players %>%
  anti_join(billiards_and_snooker_players,
            by = c("player_id" = "player_id.billiards"))
# Produce overall list of players
overall_list <- current_season_snooker_players %>%
  left_join(billiards_and_snooker_players %>%
              select(player_id.snooker, player_id.billiards, team.billiards),
            by = c("player_id" = "player_id.snooker")) %>%
  left_join(current_season_billiards_players %>%
              select(player_id.billiards = player_id,
                     billiards_frames_played = matches_played),
            by = "player_id.billiards") %>%
  rename(snooker_team = team, snooker_frames_played = matches_played,
         billiards_team = team.billiards) %>%
  select(-starts_with("player_id")) %>%
  bind_rows(current_season_billiards_players %>%
              anti_join(billiards_and_snooker_players,
                        by = c("player_id" = "player_id.billiards")) %>%
              select(player_name, billiards_team = team,
                     billiards_frames_played = matches_played))
write_csv(overall_list, "all players current season.csv")
