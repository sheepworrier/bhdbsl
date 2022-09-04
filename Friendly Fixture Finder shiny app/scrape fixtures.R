library(rvest)
library(dplyr)
library(stringr)
library(purrr)
# UDF to process a single page of fixtures
get_single_fixtures_page <- function(page_number, base_url) {
  print(paste("Processing page", page_number))
  # Construct URL for the results page number in question
  fixtures_page_url <- paste0(base_url, "/", page_number, ".html")
  # Read in the above URL
  fixture_page <- read_html(fixtures_page_url)
  # Use rvest to get the competition details from the table
  competition_name <- fixture_page %>%
    html_nodes("td:nth-child(1)") %>%
    html_attr("title") %>%
    str_remove_all("[\\r\\n\\t]|<strong>|</strong>") %>%
    str_trim()
  # Use rvest to extract the table of up to 20 fixtures into a dataframe
  fixtures_table <- fixture_page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble(.name_repair = "unique") %>%
    select(2, 3, 5, 6) %>%
    mutate(competition = competition_name)
  fixtures_table
}
# URL of initial page of fixtures
url <- paste0("https://brightonhovedistrictsnooker.leaguerepublic.com/",
              "matches/511611623/-1_-1/-1/-1/-1.html")
# Load initial page showing all outstanding fixtures for this season
fixture_start_page <- read_html(url)
# Get in the number of pages of fixtures
num_pages_nodes <- fixture_start_page %>%
  html_nodes("#hide-container .flex a")
# Handles the case where there is less than 2 pages of fixtures
if (length(num_pages_nodes) == 0) {
  num_pages <- 1
} else {
  num_pages <- num_pages_nodes %>%
    html_text() %>%
    as.integer() %>%
    max(na.rm = TRUE)
}
print(paste("There are", num_pages, "pages of results"))
# Run get_single_results_page successively to populate the fixture_table
fixture_table <- pmap_dfr(list(seq(1, num_pages),
                               substr(url, 1, str_length(url) - 5)),
                          get_single_fixtures_page)
fixture_table