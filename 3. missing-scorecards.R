source("common-functions.R")
# Read in the match and frame scores from the saved CSVs
match_scores <-
  rbind(read_csv("Old-website-match-scores.csv") %>%
          rename(home_team = `Home Team`, away_team = `Away Team`,
                 home_score = `Home Score`, away_score = `Away Score`,
                 season = Season) %>%
          mutate(division = as.integer(substring(Division, 10, 10)),
                 fixture_date = as.Date(`Date dd/mm/yyyy`, "%d/%m/%Y")) %>%
          select(fixture_date, season, division, home_team,
                 away_team, home_score, away_score),
        read_csv("New-website-match-scores.csv") %>%
          mutate(fixture_date = as.Date(fixture_date, "%d/%m/%Y")) %>%
          select(-URLs, -fixtureID))
frame_scores <- read_csv("Frame-scores.csv") %>%
  mutate(fixture_date = as.Date(fixture_date))
# Perform an anti-join to derive missing scorecards
missing_scorecards <- match_scores %>%
  anti_join(frame_scores, by = c("fixture_date", "season", "division",
                                 "home_team", "away_team"))
# Write output to CSV file
write_csv(missing_scorecards, "missing-scorecards.csv")
