library(tidyverse)

#  lahman stuff

library()


#  1.  How many times since 1970 has a team lost 100 or more games in a season?

q1 <- Teams |> 
  filter(yearID >= 1970, L >= 100)

#  2.  Among all players who have stolen 200 or more bases in their career, who has the most
#      career home runs?  Find the top ten.

#view(Batting)

q2a <- Batting |> 
  group_by(playerID) |>
  mutate(sum_sb = sum(SB, na.rm = TRUE)) |> 
  mutate(sum_hr = sum(HR, na.rm = TRUE)) |> 
  left_join(People, by="playerID") |> 
  filter(sum_sb >= 200) |> 
  arrange(desc(sum_hr)) |> 
  distinct(playerID, .keep_all = TRUE) |>
  select(playerID, nameFirst, nameLast, sum_hr, sum_sb)


#  2b. make a scatter plot with career HRs on x and career SB on y.  Put all players with more than 
#      3000 career AB on your chart.  

q2b <- Batting |> 
  group_by(playerID) |>
  mutate(sum_sb = sum(SB, na.rm = TRUE)) |> 
  mutate(sum_hr = sum(HR, na.rm = TRUE)) |> 
  mutate(sum_ab = sum(AB, na.rm = TRUE)) |> 
  filter(sum_sb >= 200, sum_ab > 3000) |> 
  left_join(People, by="playerID") |> 
  arrange(desc(sum_hr)) |> 
  distinct(playerID, .keep_all = TRUE) |>
  select(playerID, nameFirst, nameLast, sum_hr, sum_sb)

ggplot(data = q2b, aes(x = sum_hr, y = sum_sb)) + geom_point()

#  3.  How many players have won the Cy Young award in both the AL and the NL?  Make a nice table with
#      player names rather than just IDs.

write.csv(AwardsPlayers, file = "AwardsPlayers.csv", row.names = FALSE)

AwardsPlayers

cy_young <- awards %>% filter(awardID == "Cy Young Award")


q3a <- AwardsPlayers |> 
  filter(awardID == "Cy Young Award")

q3b <- q3a |> 
  group_by(playerID, lgID) |> 
  left_join(People, by = "playerID") |>
  filter(lgID == 'AL' lgID == 'NL') |> 
  distinct()

library(nflfastR)
library(dplyr)

#  4.  In the 2003 regular season, how many times did a team convert 3rd and 15 or more?

pbp2003 <- load_pbp(2003)


q4 <- pbp2003 |> 
  filter(down == 3, ydstogo >= 15, play_type == "pass" | play_type == "run") |> 
  count(posteam, name = "conversions")

#  5.  Make a table that shows each team's longest 3rd-down conversion of the 2003 regular season.
#      (for example, the longest 3rd down that Arizona successfully converted in 2003 was 20 yards.)

q5 <- pbp2003 |> 
  filter(down == 3, play_type == "pass" | play_type == "run") |> 
  group_by(posteam) |> 
  summarise(longest_conversion = max(ydstogo, na.rm = TRUE))

#  5b. make a bar chart out of this data


ggplot(q5, aes(x = fct_reorder( posteam, longest_conversion ), y = longest_conversion)) +
  geom_bar(stat = "identity") +
  labs(x = "Team", y = "Longest 3rd Down Conversion (Yards)", 
       title = "Longest 3rd Down Conversion by Team in 2003")

#  6.  During the 2003 regular season, find all instances where a team had three or more plays of
#      40+ yards in one week and then had zero plays of 40 or more yards in the next week.

q6 <- pbp2003 |> 
  filter(!is.na(yards_gained), week <= 17) |> 
  group_by(posteam, week) |> 
  summarise(plays_40_plus = sum(yards_gained >= 40, na.rm = TRUE)) |> 
  mutate(next_week_plays_40_plus = lead(plays_40_plus)) |> 
  filter(plays_40_plus >= 3, next_week_plays_40_plus == 0) |> 
  select(posteam, week, plays_40_plus, next_week_plays_40_plus)


library(wehoop)



# start with this to get all WCBB team game data from the 2021-2024 seasons

wcbb_data <- load_wbb_team_box(seasons=2021:2024)

#  7.  are there any instances in the data where the team_id is the same but the team_location is
#      different?  Hint: yes!  Make a table with team_id in one column, and a list of different
#      team locations in another column.  Fake example:

#          team_id     names
#             9999     Sewanee, University of the South
#             8888     Oklahoma State, Okla St
#             ...      ...


q7 <- wcbb_data |> 
  group_by(team_id) |> 
  summarise(names = toString(unique(team_location))) |> 
  filter(grepl(",", names))

#  8.  make a table with every team and their winning percentage for the entire 4-season period.
#      sort from highest to lowest.
#      

q8 <- wcbb_data |> 
  group_by(team_id, team_location) |> 
  summarise(win_pct = mean(team_winner, na.rm = TRUE)) |>
  filter(win_pct != 1.0 | 0.0) |> 
  arrange(desc(win_pct))


#  9.  what team had the biggest drop between their win total in one season and their win total in the
#      following season?  For example:  Sacramento State had 25 wins in the 2023 season and only
#      6 wins in 2024, for a drop of 19 wins.  A couple teams had bigger drops.

q9 <- wcbb_data |> 
  group_by(team_id, team_location, season) |> 
  summarise(wins = sum(team_winner, na.rm = TRUE)) |> 
  mutate(next_wins = lead(wins), drop = wins - next_wins) |> 
  filter(!is.na(drop)) |> 
  arrange(desc(drop))


