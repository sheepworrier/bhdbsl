library(gmailr)
library(tableHTML)
library(readr)
library(tidyr)
library(dplyr)

snooker_week <- 14
# test_to_address <- "djp42@cantab.net"
email_addresses <- read_csv("team_email_addresses.csv")
divisions <- data.frame(division = seq(1, 4),
                        div_text = c("Premier", "Division 1", "Division 2",
                                     "Division 3"),
                        stringsAsFactors = FALSE)

match_scores <- read_csv("New-website-match-scores.csv") %>%
  filter(season == 19)

breaks <- read_csv("New-website-breaks.csv")

snooker_weeks <- match_scores %>%
  filter(weekdays(fixture_date) == "Monday") %>%
  distinct(fixture_date) %>%
  arrange(fixture_date) %>%
  mutate(week_number = row_number())

final_scores <- snooker_weeks %>%
  inner_join(match_scores, by = "fixture_date") %>%
  filter(week_number == snooker_week) %>%
  inner_join(divisions, by = "division") %>%
  arrange(division, home_team) %>%
  select(div_text, home_team, home_score, away_score, away_team)

final_breaks <- breaks %>%
  inner_join(snooker_weeks, by = "fixture_date")  %>%
  filter(week_number == snooker_week) %>%
  inner_join(divisions, by = "division") %>%
  arrange(division, desc(high_break)) %>%
  select(div_text, player_name, high_break)

highest_breaks_per_division <- breaks %>%
  filter(season == 19) %>%
  group_by(division) %>%
  summarise(high_break = max(high_break)) %>%
  inner_join(breaks %>%
               filter(season == 19)) %>%
  inner_join(divisions, by = "division") %>%
  arrange(division, fixture_date) %>%
  select(div_text, player_name, fixture_date, high_break)

msg <- paste0("<u><b>Snooker results for week ", snooker_week, "</b></u>",
              "<br><br>",
              tableHTML(final_scores, rownames = FALSE,
                        headers = c("Division", "Home Team", "Home Score",
                                    "Away Score", "Away Team")),
              "<br><br><u><b>Snooker breaks this week</b></u><br><br>",
              tableHTML(final_breaks, rownames = FALSE,
                        headers = c("Division", "Name", "High Break")),
              "<br<br><u><b>Top snooker breaks this season</b></u><br><br>",
              tableHTML(highest_breaks_per_division, rownames = FALSE,
                        headers = c("Division", "Name", "Date", "High Break")),
              "<br><br>",
              "Kind regards,<br>",
              "Dean Perry")

weekly_email <-
  gm_mime() %>%
  gm_to(email_addresses$`Email Address`) %>%
  gm_from("deanjohnperry@gmail.com") %>%
  gm_subject(paste("Snooker Results for Week", snooker_week)) %>%
  gm_html_body(msg)

# Verify it looks correct
gm_create_draft(weekly_email)

# If all is good with your draft, then you can send it
gm_send_message(weekly_email)
