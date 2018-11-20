source("common-functions.R")
# Import the reference data containing the results URLs per season per division
# Only needs updating when a new season is added
ref_data <- read_csv("Snooker-Results-pages-per-season.csv")

# Grab every match result and the link to the match details page
results_new <- pmap_dfr(unname(ref_data), get_season_division_results)
results_old <- read_csv("New-website-match-scores.csv",
                        col_types = cols(
                          fixture_date = col_character(),
                          season = col_integer(),
                          division = col_integer(),
                          home_team = col_character(),
                          away_team = col_character(),
                          home_score = col_integer(),
                          away_score = col_integer(),
                          URLs = col_character()))
# Will definitely scrape any new results, plus any old results that we didn't
# have frame details for
# new_results_to_scrape <- setdiff(results_new, results_old)
new_results_to_scrape <- results_new %>%
  anti_join(results_old, by = c("fixture_date", "season", "division",
                                "home_team", "away_team", "home_score",
                                "away_score", "URLs"))
# Read in the formerly scraped frame scores
frame_scores_old <- read_csv("New-website-frame-scores.csv",
                             col_types = cols(
                               fixture_date = col_character(),
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
                                     "home_team", "away_team"))
# Old result cards will not regularly be entered into the system, so generally
# just scrape new results for frame scores
frame_scores_new <- pmap_dfr(unname(new_results_to_scrape), scrape_match_page)

# Combine and filter out BYEs
frame_scores_total <- rbind(frame_scores_old, frame_scores_new)
frame_scores_total <- frame_scores_total %>%
  filter(home_player_id != "" & away_player_id != "")

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "New-website-frame-scores.csv")
write_csv(results_new, "New-website-match-scores.csv")