library(purrr)

# Get actual results from the season
current_season_frame_scores <- frame_scores_total |>
  filter(season == 24)
# Get the starting handicap for each player
handicap_changes <- looker_output |>
  filter(season == 24) |>
  slice_min(fixture_date, n = 1, by = c(player_id, player_name),
            with_ties = FALSE) |>
  select(player_id, player_name, player_handicap, fixture_date)
# UDF to replay a single result with new handicaps and then recalulate score +
# update handicap table
replay_match <- function(current_result_index) {
  current_result <- current_season_frame_scores[current_result_index, ]
  # Get latest handicap
  home_handicap <- handicap_changes |>
    filter(player_id == current_result$home_player_id) |>
    slice_max(fixture_date, n = 1, with_ties = FALSE) |>
    pull(player_handicap)
  away_handicap <- handicap_changes |>
    filter(player_id == current_result$away_player_id) |>
    slice_max(fixture_date, n = 1, with_ties = FALSE) |>
    pull(player_handicap)
  # Recalculate score with updated handicap
  updated_result <- current_result |>
    mutate(scoring_rate = (home_score - home_player_handicap) /
             (away_score - away_player_handicap),
           home_handicap_new = home_handicap,
           away_handicap_new = away_handicap,
           home_score_new = if_else((200 - home_handicap_new) / scoring_rate <
                                      (200 - away_handicap_new),
                                    200,
                                    ((200 - away_handicap_new) * scoring_rate +
                                      home_handicap_new) |>
                                      floor() |>
                                      as.integer()),
           away_score_new = if_else((200 - home_handicap_new) / scoring_rate <
                                      (200 - away_handicap_new),
                                    ((200 - home_handicap_new) / scoring_rate +
                                      away_handicap_new) |>
                                      floor() |>
                                      as.integer(),
                                    200))
  # assert_that(abs(updated_result$home_score - updated_result$home_score_new) < 1 &
  #               abs(updated_result$away_score - updated_result$away_score_new) < 1,
  #             msg = "Scores don't match")
  # foo <<- updated_result
  # updated_result |> View()
  # Update handicap
  handicap_changes_to_add <-
    tibble(player_id = c(updated_result$home_player_id,
                         updated_result$away_player_id),
           player_name = c(updated_result$home_player_name,
                           updated_result$away_player_name),
           player_handicap = c(if_else(updated_result$home_score_new > 
                                         updated_result$away_score_new,
                                       home_handicap - 5,
                                       home_handicap + 5),
                               if_else(updated_result$home_score_new <
                                         updated_result$away_score_new,
                                       away_handicap - 5,
                                       away_handicap + 5)),
           fixture_date = updated_result$fixture_date + 1)
  handicap_changes <<- bind_rows(handicap_changes, handicap_changes_to_add)
  # Return updated_result
  updated_result
}
# replay_match(24)
# Call the UDF over each result in turn
updated_results <- map_dfr(1:72, replay_match)
# Analyse changes
player_results <-
updated_results |>
  mutate(player_win = if_else(home_score == 200, 1, 0),
         player_win_new = if_else(home_score_new == 200, 1, 0),
         player_sp_new = floor((home_score_new - home_handicap_new) /
           (200 - home_handicap_new) * 5),
         opponent_sp_new = floor((away_score_new - away_handicap_new) /
                                 (200 - away_handicap_new) * 5)) |>
  select(fixture_date, player_name = home_player_name,
         opponent_name = away_player_name, player_score = home_score,
         opponent_score = away_score, player_win, player_sp = home_player_sp,
         opponent_sp = away_player_sp, player_score_new = home_score_new,
         opponent_score_new = away_score_new, player_win_new, player_sp_new,
         opponent_sp_new, handicap_new = home_handicap_new) |>
  bind_rows(updated_results |>
              mutate(player_win = if_else(away_score == 200, 1, 0),
                     player_win_new = if_else(away_score_new == 200, 1, 0),
                     player_sp_new =
                       floor((away_score_new - away_handicap_new) /
                               (200 - away_handicap_new) * 5),
                     opponent_sp_new =
                       floor((home_score_new - home_handicap_new) /
                               (200 - home_handicap_new) * 5)) |>
              select(fixture_date, player_name = away_player_name,
                     opponent_name = home_player_name,
                     player_score = away_score,
                     opponent_score = home_score, player_win,
                     player_sp = away_player_sp, opponent_sp = home_player_sp,
                     player_score_new = away_score_new,
                     opponent_score_new = home_score_new, player_win_new,
                     player_sp_new, opponent_sp_new,
                     handicap_new = away_handicap_new)) |>
  # filter(player_win_new != player_win) |>
  # filter(opponent_sp_new != opponent_sp | player_sp_new != player_sp) |>
  arrange(player_name, fixture_date)
player_results |>
  summarise(played = n(),
            won = mean(player_win),
            won_new = mean(player_win_new),
            avg_sp = mean(player_sp),
            avg_opponent_sp = mean(opponent_sp),
            avg_sp_new = mean(player_sp_new),
            avg_opponent_sp_new = mean(opponent_sp_new),
            .by = player_name) |>
  write_csv("proposal.csv")
write_csv(handicap_changes, "proposal_handicaps.csv")
