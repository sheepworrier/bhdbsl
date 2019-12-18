library(gmailr)
library(tableHTML)
library(readr)
library(tidyr)
library(dplyr)

snooker_week <- 6
test_to_address <- "djp42@cantab.net"
divisions <- data.frame(division = seq(1, 4),
                        div_text = c("Premier", "Division 1", "Division 2",
                                     "Division 3"),
                        stringsAsFactors = FALSE)

match_scores <- read_csv("New-website-match-scores.csv") %>%
  filter(season == 19)

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

msg <- paste0("<u><b>Snooker results for week ", snooker_week, "</b></u>",
              "<br><br>",
              tableHTML(final_scores, rownames = FALSE,
                        headers = c("Division", "Home Team", "Home Score",
                                    "Away Score", "Away Team")),
              "<br><br>",
              "Kind regards,<br>",
              "Dean Perry")

weekly_email <-
  gm_mime() %>%
  gm_to(test_to_address) %>%
  gm_from("deanjohnperry@gmail.com") %>%
  gm_subject(paste("Snooker Results for Week", snooker_week)) %>%
  gm_html_body(msg)

# Verify it looks correct
gm_create_draft(weekly_email)

# If all is good with your draft, then you can send it
gm_send_message(weekly_email)
