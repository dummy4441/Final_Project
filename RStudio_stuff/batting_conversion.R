library(tidyverse)
library(Lahman)

getwd()
setwd("~/Stats336/Final Project/data")


# Total tables in Lahman
view(LahmanData)

#view(battingStats)
#importing the tables i want and others i might use

apperencesTable <- Appearances
peopleTable <- People
battingTable <- Batting
pitchingTable <- Pitching
teamsTable <- Teams

write.csv(apperencesTable, file = "Appearances.csv", row.names = FALSE)
write.csv(peopleTable, file = "People.csv", row.names = FALSE)
write.csv(battingTable, file = "Batting.csv", row.names = FALSE)
write.csv(pitchingTable, file = "Pitching.csv", row.names = FALSE)
write.csv(teamsTable, file = "Teams.csv", row.names = FALSE)

# I moved them myself to 