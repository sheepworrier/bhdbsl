# Run this first in the terminal
# sudo docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
source("common-functions.R")
library(assertthat)
current_season <- 23
# Import the reference data containing the results URLs per season per division
# Only needs updating when a new season is added
ref_data <- read_csv("Billiards-Results-pages-per-season.csv") %>%
  mutate(sport = "Billiards") %>%
  filter(Season == current_season)

# Grab every match result and the link to the match details page
results_new <- pmap_dfr(unname(ref_data), get_season_division_results)
results_old <- read_csv("Billiards-match-scores.csv",
                        col_types = cols(
                          fixture_date = col_date(),
                          season = col_integer(),
                          division = col_integer(),
                          home_team = col_character(),
                          away_team = col_character(),
                          home_sp = col_integer(),
                          away_sp = col_integer(),
                          home_op = col_integer(),
                          away_op = col_integer(),
                          url = col_character()))
# Update results_new to include any results from result_old that are not from
# the current season
results_new <- results_new %>%
  bind_rows(results_old %>%
              filter(!season %in% results_new$season))

# Will definitely scrape any new results, plus any old results that we didn't
# have frame details for
new_results_to_check <- results_new %>%
  anti_join(results_old,
            by = c("fixture_date", "season", "division", "home_team",
                   "away_team", "home_sp", "away_sp", "home_op", "away_op",
                   "url")) %>%
  select(fixture_date, season, division, home_team, away_team,
         home_score = home_op, away_score = away_op, url)
new_results_to_scrape <- new_results_to_check %>%
  select(-c(home_score, away_score)) %>%
  mutate(sport = "Billiards")
new_results_to_check <- new_results_to_check %>%
  select(-c(season, division, url)) %>%
  arrange(fixture_date, home_team, away_team)
# Read in the formerly scraped frame scores
frame_scores_old <- read_csv("Billiards-frame-scores.csv",
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
# Calculate which old results have no frame scores
old_results_to_scrape <- results_old %>%
  anti_join(frame_scores_old, by = c("fixture_date", "season", "division",
                                     "home_team", "away_team")) %>%
  filter(season == current_season) %>%
  select(fixture_date, season, division, home_team, away_team, url) %>%
  mutate(sport = "Billiards")
# Create an empty dataframe
breaks_new = data.frame(fixture_date = as.Date(character()),
                    season = numeric(),
                    division = numeric(),
                    player_id = character(),
                    player_name = character(),
                    high_break = integer(),
                    stringsAsFactors = FALSE)
# Scrape the likely fixtures that will now have match scores
frame_scores_new <-
  pmap_dfr(unname(new_results_to_scrape %>%
                    bind_rows(old_results_to_scrape)),
           scrape_match_page)
# Sum the frame scores for comparing with the match scores
summed_frame_scores <- frame_scores_new %>%
  group_by(fixture_date, home_team, away_team) %>%
  summarise(home_score = sum(home_score),
            away_score = sum(away_score)) %>%
  ungroup() %>%
  arrange(fixture_date, home_team, away_team)
# Check that the sum of the frame scores equals the overall match score
assert_that(all.equal(summed_frame_scores, new_results_to_check),
            msg = "ERROR: scores don't add up")
# Combine and filter out BYEs
frame_scores_total <- rbind(frame_scores_old, frame_scores_new)
frame_scores_total <- frame_scores_total %>%
  filter(home_player_id != "" & away_player_id != "")

# Close Selenium session opened in common functions
remDr$close()

# Create a new output for Looker Studio
looker_output <- frame_scores_total %>%
  select(fixture_date, season, player_name = home_player_name,
         player_id = home_player_id, player_handicap = home_player_handicap,
         player_score = home_score, player_sp = home_player_sp,
         opponent_name = away_player_name,
         opponent_handicap = away_player_handicap, opponent_score = away_score,
         opponent_sp = away_player_sp, player_team = home_team,
         opponent_team = away_team) %>%
  bind_rows(frame_scores_total %>%
              select(fixture_date, season, player_name = away_player_name,
                     player_id = away_player_id,
                     player_handicap = away_player_handicap,
                     player_score = away_score, player_sp = away_player_sp,
                     opponent_name = home_player_name,
                     opponent_handicap = home_player_handicap,
                     opponent_score = home_score, opponent_sp = home_player_sp,
                     player_team = away_team, opoonent_team = home_team)) %>%
  arrange(player_id, fixture_date) %>%
  filter(!is.na(player_handicap)) %>%
  mutate(one = 1,
         handicap_period = cumsum(player_id != lag(player_id, default = "") |
                             player_handicap != lag(player_handicap, default = 200))) %>%
  group_by(handicap_period) %>%
  mutate(avg_points_scored_in_period = cumsum(player_score) / cumsum(one)) %>%
  ungroup() %>%
  mutate(latest_fixture = max(fixture_date), .by = player_id) %>%
  mutate(`Latest Match` = if_else(fixture_date == latest_fixture,
                                  "TRUE", "")) %>%
  select(-one, -handicap_period, -latest_fixture)

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "Billiards-frame-scores.csv")
write_csv(results_new, "Billiards-match-scores.csv")
write_csv(breaks_new, "Billiards-breaks.csv", append = TRUE)
write_csv(looker_output,
          "Google Data Studio Outputs/Billiards-player-frame-scores.csv")
