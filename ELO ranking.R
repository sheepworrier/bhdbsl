library(sqldf)
setwd("~/Documents/R Code/snooker/")
frameScores <- read.csv("Old-website-frame-scores.csv")
frameScores$fixtureDate <- as.Date(frameScores$fixtureDate, "%d/%m/%Y")
frameScores[, c("postMatchHomeRating", "postMatchAwayRating")] <-NA

# Get number of matches played in each division per season per player
summary1 <- sqldf("SELECT playerID, playerName, season, division, count(*) as numFrames
                  FROM (SELECT homePlayerID AS playerID, homePlayerName AS playerName, season, division
                  FROM frameScores
                  UNION ALL
                  SELECT awayPlayerID AS playerID, awayPlayerName AS playerName, season, division
                  FROM frameScores)
                  GROUP BY playerID, playerName, season, division")

# Pick the main division per season per player based on where they played the majority of their matches.  In case of a tie,
# choose the hgher division
summary2 <- sqldf("SELECT playerID, playerName, season, min(division) AS majorityDivision
                  FROM (SELECT a.playerID, a.playerName, a.season, a.division, a.numFrames
                  FROM summary1 AS a
                  INNER JOIN (SELECT playerID, season, max(numFrames) AS mostFramesPlayedInDivision
                  FROM summary1
                  GROUP BY playerID, season) AS b
                  ON a.numFrames = b.mostFramesPlayedInDivision
                  AND a.playerID = b.playerID
                  AND a.season = b.season)
                  GROUP BY playerID, playerName, season")

# Finally summarise further to find the first season a player played a frame and which division they played the majority of
# them in
summary3 <- sqldf("SELECT a.playerID, a.playerName, a.season, a.majorityDivision
                  FROM summary2 AS a
                  INNER JOIN (SELECT playerID, min(season) AS firstSeason
                  FROM summary2
                  GROUP BY playerID) AS b
                  ON a.playerID = b.playerID
                  AND a.season = b.firstSeason")

# Based on analysis top Division is 5.78 times stronger than 6th top and so on.  If we start at 1500 in the top division
# calculate the starting rating for the other divisions and assign to each player
relDivStrength <- c(5.78, 2.81, 2.4, 1.96, 1.57, 1)
division <- c(1:6)
startingRanking <- data.frame(division,
                              relDivStrength)
startingRanking$value <- 1500 / relDivStrength[1] * startingRanking$relDivStrength
playerRatings <- merge(summary3, startingRanking, by.x = "majorityDivision", by.y = "division")
playerRatings[, c("latestRating", "latestFixtureDate")] <- NA
playerRatings[, c("framesPlayed")] <- 0

# Iterate through frameScores, retrieve the player rating before each frame and update with the player rating for both
# players after each frame
weightValue <- 10
for(i in 1:nrow(frameScores)) {
  row <- frameScores[i,]
  homePlayer = subset(playerRatings, playerID == row$homePlayerID)
  awayPlayer = subset(playerRatings, playerID == row$awayPlayerID)
  if (is.na(homePlayer$latestRating)) {
    homePlayerRanking <- homePlayer$value
  } else {
    homePlayerRanking <- homePlayer$latestRating
  }
  if (row$homeScore > row$awayScore) {
    homeFramesWon <- 1
    awayFramesWon <- 0
  } else {
    homeFramesWon <- 0
    awayFramesWon <- 1
  }
  if (is.na(awayPlayer$latestRating)) {
    awayPlayerRanking <- awayPlayer$value
  } else {
    awayPlayerRanking <- awayPlayer$latestRating
  }
  homePlayerNewRanking <- homePlayerRanking + weightValue * (homeFramesWon - homePlayerRanking /
                                                                      (homePlayerRanking + awayPlayerRanking))
  awayPlayerNewRanking <- awayPlayerRanking + weightValue * (awayFramesWon - awayPlayerRanking /
                                                                      (homePlayerRanking + awayPlayerRanking))
  frameScores[i, 12] <- homePlayerNewRanking
  frameScores[i, 13] <- awayPlayerNewRanking
  playerRatings[as.numeric(rownames(homePlayer)), 7] <- homePlayerNewRanking
  playerRatings[as.numeric(rownames(homePlayer)), 8] <- row$fixtureDate
  playerRatings[as.numeric(rownames(homePlayer)), 9] <- homePlayer$framesPlayed + 1
  playerRatings[as.numeric(rownames(awayPlayer)), 7] <- awayPlayerNewRanking
  playerRatings[as.numeric(rownames(awayPlayer)), 8] <- row$fixtureDate
  playerRatings[as.numeric(rownames(awayPlayer)), 9] <- awayPlayer$framesPlayed + 1
  #print(paste(homePlayerRanking, as.numeric(rownames(homePlayer)), homePlayerNewRanking,
  #            "vs.",
  #            awayPlayerRanking, as.numeric(rownames(awayPlayer)), awayPlayerNewRanking))
}

playerRatingsOutput <- data.frame(playerRatings$season,
                                  playerRatings$majorityDivision,
                                  playerRatings$playerID,
                                  playerRatings$playerName,
                                  playerRatings$value,
                                  playerRatings$latestRating,
                                  playerRatings$latestFixtureDate,
                                  playerRatings$framesPlayed)
colnames(playerRatingsOutput) <- c("DebutSeason", "DebutDivision", "ID", "Name",
                                   "InitialRating", "LatestRating", "LatestMatchDate", "FramesPlayed")
playerRatingsOutput$DebutSeason <- playerRatingsOutput$DebutSeason + 2000

write.csv(playerRatingsOutput, "Player-ratings-output.csv", row.names = FALSE)
write.csv(frameScores, "Frame-scores.csv", row.names = FALSE)