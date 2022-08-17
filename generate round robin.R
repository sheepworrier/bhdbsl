library(TouRnament)
library(dplyr)
library(readr)
# List of Premier Division teams.  Pretend there are 8 teams to spread out the
# fixtures with some BYEs and more closely align with Div 1
prem_teams <- c("St. Matthias A", "St. Matthias C", "St. Matthias D",
                "Castle Club C", "Castle Club G", "N.A.R.C A", "BYE 1", "BYE 2")
# Generate an 8-team single round robin
prem_fixtures <-
  roundrobin(prem_teams, randomize = FALSE, second_round = FALSE, seed = 1234)
# List of Div 1 teams
div1_teams <-
  c("St. Matthias B", "N.A.R.C B", "Portslade Legion", "Portslade Sports A",
    "Portslade Sports B", "Hove Deep Sea Anglers", "Moulsecoomb Club A",
    "Moulsecoomb Club B", "Preston Club", "Champion House",
    "Southwick Sports A", "Southwick Sports B", "Southwick Sports C")
# Generate a 13-team single round robin
div1_fixtures <-
  roundrobin(div1_teams, randomize = FALSE, second_round = FALSE, seed = 1234)
# UDF to greate a mirror image of a round of fixtures
reverse_fixtures <- function(fixtures_df, round) {
  fixtures_df %>%
    rename(Home = Away, Away = Home) %>%
    mutate(Matchday = Matchday + (round - 1) * max(Matchday))
}
# Create the full Premier Division season: 4 rounds, removing BYEs
full_prem <- prem_fixtures %>%
  bind_rows(reverse_fixtures(prem_fixtures, 2),
            prem_fixtures %>%
              mutate(Matchday = Matchday + 2 * max(Matchday)),
            reverse_fixtures(prem_fixtures, 4)) %>%
  filter(!Home %in% c("BYE 1", "BYE 2"),
         !Away %in% c("BYE 1", "BYE 2")) %>%
  mutate(Division = "Premier Division")
# Create the full Div 1 season: 2 rounds with a 1-week break after 7 weeks
# to align mid-season with Christmas & New Year break
full_div1 <- div1_fixtures %>%
  bind_rows(reverse_fixtures(div1_fixtures, 2)) %>%
  mutate(Division = "Division 1",
         Matchday = if_else(Matchday > 7, Matchday + 1, Matchday))
# Create a dataframe of dates for each match day
matchdays <- data.frame(Date = c(seq.Date(as.Date('2022-09-12'),
                                          as.Date('2022-11-28'),
                                          by = "weeks"),
                                 as.Date("2022-12-12"), as.Date("2022-12-19"),
                                 seq.Date(as.Date('2023-01-09'),
                                          as.Date('2023-04-03'),
                                          by = "weeks"),
                                 as.Date('2023-04-17'))) %>%
  mutate(Matchday = row_number())
# Update the fixtures with the match day and add a dummy Fixture ID variable to
# align with LeagueRepublic downloads
all_fixtures <- bind_rows(full_prem, full_div1) %>%
  inner_join(matchdays) %>%
  mutate(`Fixture ID` = row_number()) %>%
  rename(`Home Team` = Home, `Away Team` = Away)
# Write to a CSV file for assessment in Schedule scoring report.qmd and
# potential modification 
write_csv(all_fixtures, "fixtures_2022-23 DP v3.csv")
