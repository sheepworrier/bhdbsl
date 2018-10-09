source("common-functions.R")
# Import the reference data containing the results URLs per season per division
ref_data <- read_csv("Snooker-Results-pages-per-season.csv")

# Grab every match result and the link to the match details page
# results <- pmap_dfr(unname(ref_data), get_season_division_results)
results <- read_csv("New-website-match-scores.csv")

# Cloudflare blocks IPs when too many are attempted in quick succession, so
# will break them down by season
results_14 <- results %>% filter(season == 14)
results_15 <- results %>% filter(season == 15)
results_16 <- results %>% filter(season == 16)
results_17 <- results %>% filter(season == 17)
results_18 <- results %>% filter(season == 18)

# Scrape the frame scores from all of the match pages
frame_scores_14 <- pmap_dfr(unname(results_14), scrape_match_page)
frame_scores_15 <- pmap_dfr(unname(results_15), scrape_match_page)
frame_scores_16 <- pmap_dfr(unname(results_16), scrape_match_page)
frame_scores_17 <- pmap_dfr(unname(results_17), scrape_match_page)
frame_scores_18 <- pmap_dfr(unname(results_18), scrape_match_page)

# Combine and filter out BYEs
frame_scores_total <- rbind(frame_scores_14
                            , frame_scores_15
                            , frame_scores_16
                            , frame_scores_17
                            , frame_scores_18
                            )
frame_scores_total <- frame_scores_total %>%
  filter(home_player_id != "" & away_player_id != "")

# Write the results to a CSV file for use in the ELO ranking
write_csv(frame_scores_total, "New-website-frame-scores.csv")
write_csv(results, "New-website-match-scores.csv")
