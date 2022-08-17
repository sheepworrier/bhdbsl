library(readr)
# Read in the round robin fixtures that have been generated
fixtures_raw <- read_csv("fixtures_2022-23 DP v4.csv")
# Read in the short codes to transform the team names
short_codes <- read_csv("LR team short codes.csv", na = "") %>%
  select(`Short Code`, Team)
# Transform to a dataframe in the correct format for uploading
fixtures_upload <- fixtures_raw %>%
  inner_join(short_codes,
             by = c("Home Team" = "Team")) %>%
  inner_join(short_codes,
             by = c("Away Team" = "Team")) %>%
  mutate(`Date dd/mm/yyyy` = format(Date, "%d/%m/%Y"),
         `Time HH:MM` = "19:00",
         `Home Team` = `Short Code.x`,
         `Away Team` = `Short Code.y`,
         Venue = "",
         Pitch = "",
         `Home Score` = "",
         `Away Score` = "") %>%
  select(`Date dd/mm/yyyy`, `Time HH:MM`, Division, `Home Team`, `Away Team`,
         Venue, Pitch, `Home Score`, `Away Score`)
# Write to CSV files for uploading to LeagueRepublic
write_csv(fixtures_upload %>%
            filter(Division == "Premier Division"),
          "fixtureupload_Premier.csv",
          quote = "none")
write_csv(fixtures_upload %>%
            filter(Division == "Division 1"),
          "fixtureupload_Div1.csv",
          quote = "none")
