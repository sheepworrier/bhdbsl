library(gmailr)
library(tableHTML)
library(readr)
library(tidyr)
library(dplyr)

snooker_week <- 24
billiards_week <- 15
# test_to_address <- "djp42@cantab.net"
email_addresses <- read_csv("team_email_addresses.csv")
snooker_divisions <- data.frame(division = seq(1, 4),
                                div_text = c("Premier", "Division 1", 
                                             "Division 2", "Division 3"),
                                stringsAsFactors = FALSE)

snooker_match_scores <- read_csv("New-website-match-scores.csv") %>%
  filter(season == 19)
billiards_match_scores <- read_csv("Billiards-match-scores.csv") %>%
  filter(season == 19)

snooker_breaks <- read_csv("New-website-breaks.csv")
billiards_breaks <- read_csv("Billiards-breaks.csv")

snooker_weeks <- snooker_match_scores %>%
  filter(weekdays(fixture_date) == "Monday") %>%
  distinct(fixture_date) %>%
  arrange(fixture_date) %>%
  mutate(week_number = row_number())
billiards_weeks <- billiards_match_scores %>%
  filter(weekdays(fixture_date) == "Wednesday") %>%
  distinct(fixture_date) %>%
  arrange(fixture_date) %>%
  mutate(week_number = row_number())

final_snooker_scores <- snooker_weeks %>%
  inner_join(snooker_match_scores, by = "fixture_date") %>%
  filter(week_number == snooker_week) %>%
  inner_join(snooker_divisions, by = "division") %>%
  arrange(division, home_team) %>%
  select(div_text, home_team, home_score, away_score, away_team)
final_billiards_scores <- billiards_weeks %>%
  inner_join(billiards_match_scores, by = "fixture_date") %>%
  filter(week_number == billiards_week) %>%
  mutate(div_text = "Division 1") %>%
  arrange(division, home_team) %>%
  select(div_text, home_team, home_op, home_sp, away_sp, away_op, away_team)

final_snooker_breaks <- snooker_breaks %>%
  inner_join(snooker_weeks, by = "fixture_date")  %>%
  filter(week_number == snooker_week) %>%
  inner_join(snooker_divisions, by = "division") %>%
  arrange(division, desc(high_break)) %>%
  select(div_text, player_name, high_break)
final_billiards_breaks <- billiards_breaks %>%
  inner_join(billiards_weeks, by = "fixture_date")  %>%
  filter(week_number == billiards_week) %>%
  mutate(div_text = "Division 1") %>%
  arrange(division, desc(high_break)) %>%
  select(div_text, player_name, high_break)

highest_snooker_breaks_per_division <- snooker_breaks %>%
  filter(season == 19) %>%
  group_by(division) %>%
  summarise(high_break = max(high_break)) %>%
  inner_join(snooker_breaks %>%
               filter(season == 19)) %>%
  inner_join(snooker_divisions, by = "division") %>%
  arrange(division, fixture_date) %>%
  select(div_text, player_name, fixture_date, high_break)
highest_billiards_breaks_per_division <- billiards_breaks %>%
  filter(season == 19) %>%
  group_by(division) %>%
  summarise(high_break = max(high_break)) %>%
  inner_join(billiards_breaks %>%
               filter(season == 19)) %>%
  mutate(div_text = "Division 1") %>%
  arrange(division, fixture_date) %>%
  select(div_text, player_name, fixture_date, high_break)

msg <- paste0("<u><b>Snooker results for week ", snooker_week, "</b></u>",
              "<br><br>",
              tableHTML(final_snooker_scores, rownames = FALSE,
                        headers = c("Division", "Home Team", "Home Score",
                                    "Away Score", "Away Team")),
              "<br><br>",
              "<u><b>Snooker breaks this week</b></u>",
              "<br><br>",
              tableHTML(final_snooker_breaks, rownames = FALSE,
                        headers = c("Division", "Name", "High Break")),
              "<br<br>",
              "<u><b>Top snooker breaks this season</b></u>",
              "<br><br>",
              tableHTML(highest_snooker_breaks_per_division, rownames = FALSE,
                        headers = c("Division", "Name", "Date", "High Break")),
              "<br><br>",
              "<u><b>Billiards results for week ", billiards_week, "</b></u>",
              "<br><br>",
              tableHTML(final_billiards_scores, rownames = FALSE,
                        headers = c("Division", "Home Team", "Home OP",
                                    "Home SP", "Away SP", "Away OP",
                                    "Away Team")),
              "<br><br>",
              "<u><b>Billiards breaks this week</b></u>",
              "<br><br>",
              tableHTML(final_billiards_breaks, rownames = FALSE,
                        headers = c("Division", "Name", "High Break")),
              "<br<br>",
              "<u><b>Top billiards breaks this season</b></u>",
              "<br><br>",
              tableHTML(highest_billiards_breaks_per_division, rownames = FALSE,
                        headers = c("Division", "Name", "Date", "High Break")),
              "<br><br>",
              "Kind regards,<br>",
              "Dean Perry",
              "<br><br>",
              "P.S. if anyone would like to be removed from this list or you ",
              "would like me to add anyone then please let me know.")

weekly_email <-
  gm_mime() %>%
  gm_to(email_addresses$`Email Address`) %>%
  gm_from("deanjohnperry@gmail.com") %>%
  gm_subject(paste("Snooker Results for Week", snooker_week,
                   "and Billiards Results for Week", billiards_week)) %>%
  gm_html_body(msg)

# Verify it looks correct
gm_create_draft(weekly_email)

# If all is good with your draft, then you can send it
gm_send_message(weekly_email)
