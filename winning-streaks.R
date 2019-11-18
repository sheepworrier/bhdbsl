frame_scores <- read_csv("Frame-scores.csv")

all_scores <- frame_scores %>%
  rename(player_id = home_player_id, player_name = home_player_name) %>%
  mutate(win = home_score > away_score) %>%
  select(fixture_date, player_id, player_name, win) %>%
  bind_rows(frame_scores %>%
              rename(player_id = away_player_id,
                     player_name = away_player_name) %>%
              mutate(win = home_score < away_score) %>%
              select(fixture_date, player_id, player_name, win)) %>%
  arrange(player_id, fixture_date)

rle(all_scores$win == TRUE)


labelled_scores <- all_scores %>%
  group_by(run = data.table::rleid(player_id, win), player_id)

win_lose_streaks <- labelled_scores %>%
  summarise(win_lose_streak = n())

labelled_scores %>%
  inner_join(win_lose_streaks, by = c("run", "player_id")) %>%
  group_by(player_id, player_name, win, run) %>%
  summarise(start_date = min(fixture_date), end_date = max(fixture_date),
         streak = mean(win_lose_streak)) %>%
  arrange(desc(streak))