install.packages('tidyverse')
library('tidyverse')
install.packages('janitor')
library('janitor')

summary(era_plus_pennant_winners)
# Average wins for a pennant winner is 96.  
# Average ERA is 3.36
# Average WHIP is 1.27
# Aveerage SO/BB is 1.774
# Average ERA+ is 113.8

install.packages('sqldf')
library('sqldf')

Wild_card_era <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year >= 1995')
# The average ERA+ for pennant winners since 1995 is 113.6 which is on pace with the all time number
dead_ball_era <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1903 AND 1919')
# During the DeadBall era the avg ERA+ was 118.9, a higher number than the wildcard era or the overall number
Between_wars <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1920 AND 1941')
# During the between wars era it was 114.3
After_war <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1946 AND 1959')
# The number here is a bit below average at 112.4
The_1960s <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1960 AND 1968')
# A very low number during a pitching dominate era of 109.7
Pre_Divisions <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1946 AND 1968')
# The era between the end of WWII and the start of the playoffs was 111.3, below average
Division_Era <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1969 AND 1993')
# In the Four Division era it was 111.04.  Also below average.  
From_46_to_93 <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 1946 AND 1993')
# Average is 111.2...For a nearly 50 year stint the ERA plus was below the all time mean

Extra_WC <- sqldf('SELECT avg(ERA_Plus) FROM era_plus_pennant_winners WHERE Year BETWEEN 2012 AND 2021')
# Average ERA plus in the expanded wildcard era was higher than the all time mean is 114.6
# Pitching for pennant winners is above the all time mean in recent seasons.  Relievers and more pitchers?

# Query info on how bad the worst teams have been each season.

# WC(95-21) avg for ERA Plus from WC era is 111.25
# Div Era (69-93) for ERA Plus from that era is 109.79

Top_Twenty_Five_Pitching_Staffs <- sqldf('SELECT Tm, Year, ERA_Plus FROM era_plus_pennant_winners LIMIT 25')
view(Top_Twenty_Five_Pitching_Staffs)

# Grouping the answsers from the data together so we can make visuals
All_Era_Stats <- c(dead_ball_era, Between_wars, After_war, The_1960s, Division_Era, From_46_to_93, wild_card_era, Extra_WC )
AllEraWords <- c("1903-1919", "1920-1941", "1946 to 1959", "1960-1968", "1969-1993", "1946-1993", "1995-Present", "2012-Present")
ERA_Plus_By_Era <- data.frame(AllEraWords, All_Era_Stats)
view(ERA_Plus_By_Era)

install.packages('ggplot')

view(All_Era_Stats)

# Chart for ERA + numbers by era
ggplot(data = ERA_Plus_By_Era, aes(x = All_Era_Stats, y = AllEraWords))+ geom_bar(stat = "identity", width=0.7,
                                                                                  color="blue", fill = "red")+
  coord_flip()

barplot(ERA_Plus_By_Era$y, ylim = c(95, 110))


# See if I can change the 0-100 scale and order by graph height

summary(OPS_Plus_)
# Avg pennant winning team has hit 127.3 HR a season. 
# The mean OPS is .745
# The mean AVG is .270
# The mean OBP is .339
# The mean SLG is .406

# The mean OPS Plus is only 103.4 (The lowest 83 the highest 127)

Top_25_OPS <- sqldf('SELECT Tm, Year, OPS_Plus FROM OPS_Plus_ LIMIT 25')
# Only 8 of the top 25 were teams prior to WW2
# 10 of the top 25 have been in the wild card era (since 1995)
# SIX of them have occurred since 2013
# EIGHT since 2003
# 3 Teams from the Dead Ball era
# 5 From the 20s and 30s

 sqldf('SELECT Tm, Year, OPS_Plus FROM OPS_Plus_ WHERE Year BETWEEN 1903 AND 1919')
 Dead_Ball_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1903 AND 1919')
# AVG WAS 103
Between_Wars_OPS <- sqldf('SELECT Tm, Year, OPS_Plus FROM OPS_Plus_ WHERE Year BETWEEN 1920 AND 1941')
Between_Wars_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1920 AND 1941')
# 103.3
After_War_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1946 AND 1959')
# 104.1
The_1960s_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1960 AND 1968')
# 101
Divisional_Era_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1969 AND 1993')
# 103.9
Wild_Card_Era_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 1995 AND 2021')
# 104.0
Extra_WC_OPS <- sqldf('SELECT avg(OPS_Plus) FROM OPS_Plus_ WHERE Year BETWEEN 2012 AND 2021')
# 106.2

All_Era_Stats_OPS <- c(Dead_Ball_OPS, Between_Wars_OPS, After_War_OPS, The_1960s_OPS, Divisional_Era_OPS,
                       Wild_Card_Era_OPS, Extra_WC_OPS)
OPSWords <- c("Dead Ball OPS", "Between Wars OPS", "After War OPS", "The 1960s OPS", "Divisional Era OPS",
              "Wild Card Era OPS", "Extra Wild Card OPS")
OPS_Plus_By_Era <- data.frame(All_Era_Stats_OPS, OPSWords)

ggplot(data = OPS_Plus_By_Era, aes(x = All_Era_Stats_OPS, y = OPSWords))+ geom_bar(stat = "identity", width=0.9,
                                                                                  color="blue", fill = "lightsteelblue")+coord_flip()

ggplot(OPS_Plus_By_Era, aes(All_Era_Stats, OPSWords)) + geom_bar()

OPS_By_Era_G <- ggplot(data = OPS_Plus_By_Era, aes(x = All_Era_Stats_OPS, y = factor(OPSWords) ))

OPS_By_Era_G + geom_bar(stat = "identity")



ggplot(data = OPS_Plus_By_Era, aes(x = All_Era_Stats_OPS, y = OPSWords))+ geom_col()+
  geom_text(aes(label = All_Era_Stats_OPS), vjust = 1.5, colour = "lightsteelblue")

# Pennant winners have had solid to above average offenses and very good pitching.
# Some of the recent pennant winners have had very high OPS and ERA Plus stats.  
# The avg OPS + for postseason teams from 1969-2021 is 103.  Same splits for Divisional and WC Eras
# Therefore there average offensive team in the WS and postseason have the same OPS PLus


view(Top_25_OPS)
view(Top_Twenty_Five_Pitching_Staffs)

# There are three teams that are in the top 25 in both categories: 2020 Dodgers, 2019 Ass, 1910 A's, 1948 Indians.
# 1995 Indians missed it by one ERA Plus number.


Teams_in_top_25_hit_pitch <- c("1910 Athletics", "1948 Indians", "2019 Astros", "2020 Dodgers")
Final_Four_OPS <- c(113, 113, 119, 119)
Final_Four_ERA <- c(133, 127, 127, 146)

Final_Four <- data.frame(Teams_in_top_25_hit_pitch, Final_Four_OPS, Final_Four_ERA)
view(Final_Four)








