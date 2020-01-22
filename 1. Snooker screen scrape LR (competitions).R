source("common-functions.R")
# Import the reference data containing the results URLs per season per
# competition
# Only needs updating when a new season is added
ref_data <- read_csv("Snooker-Competitions-Results-pages-per-season.csv") %>%
  mutate(sport = "Snooker comp") %>%
  filter(Season == 19)

# Grab every match result and the link to the match details page
results_new <- pmap_dfr(unname(ref_data), get_season_division_results)

# Adjust names to match handicap sheet
# results_final <- results_new %>%
#   mutate(home_team =
#            case_when(home_team == "M Dumbleton" ~ "Matt Dumbleton",
#                      TRUE ~ home_team),
#          away_team =
#            case_when(away_team == "M Dumbleton" ~ "Matt Dumbleton",
#                      TRUE ~ away_team))

# Get handicaps from Google Sheets
library(googledrive)
library(googlesheets4)
drive_auth(use_oob = TRUE)
# drive_auth()
sheets_auth(token = drive_token())

#drive_files <- drive_ls()
# sheets_get("1KFOLH60yg3leijxgC3lPZUkCokTloTlcKNA8V_T0yvw")
handicaps <-
  sheets_read("1KFOLH60yg3leijxgC3lPZUkCokTloTlcKNA8V_T0yvw",
              "2019/20 singles contacts") %>%
  mutate(Name = case_when(Name == "M. Dumbleton" ~ "M Dumbleton",
                          TRUE ~ Name))
# Viusalise the number of matches where the better player won
# Viusalise the number of matches that went to a decider vs. not
features_final <- results_new %>%
  left_join(handicaps %>%
              select(-`Phone number`),
            by = c("home_team" = "Name")) %>%
  rename(home_handicap = `Per frame handicap`) %>%
  left_join(handicaps %>%
              select(-`Phone number`),
            by = c("away_team" = "Name")) %>%
  rename(away_handicap = `Per frame handicap`) %>%
  mutate(best_player_won =
           case_when(home_handicap == away_handicap ~ NA,
                     home_handicap < away_handicap &
                       home_score > away_score ~ TRUE,
                     home_handicap > away_handicap &
                       home_score < away_score ~ TRUE,
                     home_handicap < away_handicap &
                       home_score < away_score ~ FALSE,
                     home_handicap > away_handicap &
                       home_score > away_score ~ FALSE),
         decider = abs(home_score - away_score) == 1)

features_final %>%
  count(decider, best_player_won)
  
