source("common-functions.R")
# Import the reference data containing the results URLs per season per division
# Only needs updating when a new season is added
ref_data <- read_csv("Billiards-Results-pages-per-season.csv") %>%
  mutate(sport = "Billiards")

# Grab every match result and the link to the match details page
results_new <- pmap_dfr(unname(ref_data), get_season_division_results)

new_results_to_scrape <- results_new %>%
  select(fixture_date, season, division, home_team, away_team, url)

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
  pmap_dfr(unname(new_results_to_scrape[30:34, ]),
           scrape_match_page)

# Filter out BYEs
frame_scores_total <- frame_scores_new %>%
  filter(home_player_id != "" & away_player_id != "")

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "Billiard-frame-scores.csv")
write_csv(results_new, "Billiard-match-scores.csv")
write_csv(breaks_new, "Billiard-breaks.csv")
