library(TouRnament)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(assertthat)

# Create a UDF to generate fixtures for any reasonable number of teams
generate_abstract_divisions <- function(num_teams) {
  # List of abstract lettered teams
  teams <- letters[1:num_teams]
  # Generate an 8-team single round robin
  fixtures <-
    roundrobin(teams, randomize = FALSE, second_round = FALSE, seed = 1234)
  # Cross-tab view
  fixtures %>%
    rename(Team = Home) %>%
    mutate(Location = "Home") %>%
    select(Matchday, Team, Location) %>%
    bind_rows(fixtures %>%
                rename(Team = Away) %>%
                mutate(Location = "Away") %>%
                select(Matchday, Team, Location)) %>%
    pivot_wider(names_from = Matchday, values_from = Location) %>%
    arrange(Team) %>%
    write_csv(paste0(num_teams, "-team-grid.csv"))
  # Fixtures view
  fixtures %>%
    write_csv(paste0(num_teams, "-team-fixtures.csv"))  
}
# Run the above for 8, 10 and 12
walk(c(6, 8, 10, 12), generate_abstract_divisions)

# Assign the lettered teams to actual entered teams
billiards_teams <- c("MCA", "MCB", "SMA", "SMB")
prem_snooker <- c("SMA", "SMD", "SMC", "SME", "SMF", "PSA", "SSA", "CCA", "CCB",
                  "CCC", "PC")
div1_snooker <- c("CH", "HDS", "MCA", "MCB", "MCC", "SMB", "PSB", "SSB", "PSC",
                  "SSC")
# Create 1st round of fixtures without dates
create_fixtures_round <- function(short_codes, name) {
  # Calculate division size (even numbers)
  fixture_size <- ceiling(length(short_codes) / 2) * 2
  # Read in the earlier generated fixtures
  base_fixtures <- read_csv(paste0(fixture_size,
                                   "-team-fixtures.csv"))
  # Create a df of team short codes
  teams_df <- data.frame(letter = letters[1:length(short_codes)],
                         short_code = short_codes)
  # Join together
  fixtures <- base_fixtures |>
    left_join(teams_df |>
                rename(home = short_code),
              join_by(Home == letter)) |>
    left_join(teams_df |>
                rename(away = short_code),
              join_by(Away == letter)) |>
    select(-c(Home, Away)) |>
    filter(!is.na(home), !is.na(away))
  # Store in environment
  assign(paste0(name, "_fixtures"), fixtures, .GlobalEnv)
}
create_fixtures_round(billiards_teams, "billiards")
create_fixtures_round(prem_snooker, "prem")
create_fixtures_round(div1_snooker, "div1")
# Assign dates to the matchdays
billiards_dates <- c(as.Date('2024-09-25'),
                     as.Date('2024-10-02'),
                     as.Date('2024-10-23'),
                     as.Date('2024-10-30'),
                     as.Date('2024-11-20'),
                     as.Date('2024-11-27'),
                     as.Date('2025-01-08'),
                     as.Date('2025-01-15'),
                     as.Date('2025-02-05'),
                     as.Date('2025-02-12'),
                     as.Date('2025-03-05'),
                     as.Date('2025-03-12'))
prem_dates <- c(seq.Date(as.Date('2024-09-23'),
                         as.Date('2024-10-28'),
                         by = "weeks"),
                seq.Date(as.Date('2024-11-11'),
                         as.Date('2024-12-02'),
                         by = "weeks"),
                as.Date('2024-12-16'),
                seq.Date(as.Date('2025-01-06'),
                         as.Date('2025-01-27'),
                         by = "weeks"),
                seq.Date(as.Date('2025-02-10'),
                         as.Date('2025-03-10'),
                         by = "weeks"),
                as.Date('2025-03-24'),
                as.Date('2025-03-31'))
div1_dates <- data.frame(fixtures = prem_dates) |>
  filter(!fixtures %in% c(as.Date('2024-12-02'), as.Date('2024-12-16'),
                          as.Date('2025-03-24'), as.Date('2025-03-31'))) %>%
  pull(fixtures)
# UDF to greate a mirror image of a round of fixtures
reverse_fixtures <- function(fixtures_df, round) {
  fixtures_df %>%
    rename(home = away, away = home) %>%
    mutate(Matchday = Matchday + (round - 1) * max(Matchday))
}
# Create output for uploading to LR using short codes
create_full_division_fixtures <- function(name, num_rounds) {
  # Load the appropriate team fixture assignments from memory
  team_fixtures <- get(paste0(name, "_fixtures"), .GlobalEnv)
  # Load the appropriate date fixture assignments from memory
  date_fixtures <- data.frame(Date = get(paste0(name, "_dates"), .GlobalEnv)) |>
    mutate(Matchday = row_number())
  # Check there are no duplicates
  assert_that(date_fixtures |>
                count(Date) |>
                filter(n > 1) |>
                nrow() == 0,
              msg = "Duplicate dates")
  # Check that they are all on the same day of week
  wday_check <- date_fixtures |>
    mutate(wday = format(Date, "%a"))
  assert_that(wday_check  |>
                count(wday) |>
                nrow() == 1,
              msg = paste("Wrong dow for ",
                          paste(wday_check |>
                                  mutate(dow_cnt = n(), .by = wday) |>
                                  slice_min(dow_cnt, n = 1) |>
                                  pull(Date),
                                collapse = ", ")))
  # Check they are all in ascending date order
  assert_that(date_fixtures |>
                mutate(days_between = as.numeric(Date - lag(Date))) |>
                filter(days_between < 0) |>
                nrow() == 0,
              msg = "Dates are not in ascending order")
  # Generate full season fixtures by matchday
  full_season <- team_fixtures |>
    bind_rows(reverse_fixtures(team_fixtures, 2))
  if(num_rounds == 4) {
    full_season <- full_season |>
      bind_rows(full_season |>
                  mutate(Matchday = Matchday + max(full_season$Matchday)))
  } else if(num_rounds == 2) {
    # continue
  } else {
    stop(paste("No if branch to handle", num_rounds, "of fixtures"))
  }
  # Generate the full season fixtures with dates
  full_season |>
    inner_join(date_fixtures,
               join_by(Matchday)) |>
    mutate(`Date dd/mm/yyyy` = format(Date, "%d/%m/%Y"),
           `Time HH:MM` = "19:00",
           Division = case_when(name == "prem" ~ "Premier Division",
                                .default = "Division 1"),
           Venue = "", Pitch = "", `Home Score` = "", `Away Score` = "") |>
    select(`Date dd/mm/yyyy`, `Time HH:MM`, Division, `Home Team` = home,
           `Away Team` = away, Venue, Pitch, `Home Score`, `Away Score`) |>
    write_csv(paste0(name, "_upload.csv"))
}
create_full_division_fixtures("billiards", 4)
create_full_division_fixtures("prem", 2)
create_full_division_fixtures("div1", 2)
# Check matches per venue
library(stringr)
fixtures <- read_csv("prem_upload.csv") |>
  bind_rows(read_csv("div1_upload.csv"))
fixtures |>
  mutate(venue = str_sub(`Home Team`, 1, 2),
         date = as.Date(`Date dd/mm/yyyy`, format = "%d/%m/%Y")) |>
  count(date, venue) |>
  mutate(issue = 
           case_when(venue == "SM" & n <= 3 ~ FALSE,
                     venue == "SS" & n <= 2 ~ FALSE,
                     venue == "PS" & n <= 2 ~ FALSE,
                     venue == "MC" & n <= 2 ~ FALSE,
                     venue == "CC" & n <= 8 ~ FALSE,
                     .default = TRUE)) |>
  filter(n > 1, issue)
# Create CSV for importing into Google Calendar
fixtures |>
  filter(`Home Team` == "SMD" | `Away Team` == "SMD") |>
  mutate(Opponent = if_else(`Home Team` == "SMD", `Away Team`, `Home Team`),
         Opponent = str_replace(Opponent, "SM", "St. Matthias ") |>
           str_replace("PC", "Preston Club") |>
           str_replace("PS", "Portslade Sports ") |>
           str_replace("CC", "Castle Club ") |>
           str_replace("SS", "Southwick Sports "),
         Subject = paste(Opponent, if_else(`Home Team` == "SMD", "(H)", "(A)")),
         Date = as.Date(`Date dd/mm/yyyy`, format = "%d/%m/%Y") |>
           format("%Y-%m-%d"),
         `End time` = "22:00") |>
  select(Subject, `Start date` = Date, `Start time` = `Time HH:MM`,
         `End date` = Date, `End time`) |>
  write_csv("snooker_gcal_upload.csv")
read_csv("billiards_upload.csv") |>
  filter(`Home Team` == "SMB" | `Away Team` == "SMB") |>
  mutate(Opponent = if_else(`Home Team` == "SMB", `Away Team`, `Home Team`),
         Opponent = str_replace(Opponent, "SM", "St. Matthias ") |>
           str_replace("MC", "Moulsecoomb "),
         Subject = paste(Opponent, if_else(`Home Team` == "SMB", "(H)", "(A)")),
         Date = as.Date(`Date dd/mm/yyyy`, format = "%d/%m/%Y") |>
           format("%Y-%m-%d"),
         `End time` = "22:00") |>
  select(Subject, `Start date` = Date, `Start time` = `Time HH:MM`,
         `End date` = Date, `End time`) |>
  write_csv("billiards_gcal_upload.csv")
