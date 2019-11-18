library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
matchScores <- read_csv("Old-website-match-scores.csv")
teams <- unique(matchScores$`Home Team`)
View(teams)
matchScores$homeClub <- sub(" [A-K]$","", matchScores$`Home Team`)
uniqueTeamsPerSeason <- matchScores %>%
  group_by(Season, homeClub, `Home Team`) %>%
  summarise(matchCount = n())
teamsPerClubPerSeason <- uniqueTeamsPerSeason %>%
  group_by(Season, homeClub) %>%
  summarise(teamCount = n())
tidyTeams <- spread(teamsPerClubPerSeason, key = Season, value = teamCount)
tidyTeams[is.na(tidyTeams)] <- 0
write_csv(tidyTeams, "Teams-per-club-per-season.csv")
# Allow user to add teams per club data for recent seasons
tidyTeams <- read_csv("Teams-per-club-per-season-final.csv")
teamsPerClubPerSeason <- gather(tidyTeams, key = "season", value ="teamCount", 4:11)
teamsPerClubPerSeason$homeClubFactor <- factor(teamsPerClubPerSeason$homeClub, 
                                         levels = unique(teamsPerClubPerSeason$homeClub[order(rev(teamsPerClubPerSeason$season),
                                                                                              teamsPerClubPerSeason$teamCount)]),
                                         ordered = TRUE)
ggplot(teamsPerClubPerSeason,
       aes(x = season, y = teamCount, fill = homeClubFactor)) + geom_bar(colour = "black", stat = "identity")

tidyTeams$change <- tidyTeams$`17` - tidyTeams$`10`
tidyTeams$polarity <- tidyTeams$change / abs(tidyTeams$change)
tidyTeams$polarity[is.nan(tidyTeams$polarity)] <- 0
tidyTeams$polarity <- factor(tidyTeams$polarity)
tidyTeams$change <- factor(abs(tidyTeams$change))
brightonMap <- qplot(Long, Lat, data = tidyTeams, size = change, col = polarity,
                     alpha = I(0.5),
                     xlab = "Longitude", ylab = "Latitude",
              main = "Growth / Decline of teams per club since 2010")
brightonMap + scale_colour_manual("Growth or decline", values = c("red", "black", "green"))
