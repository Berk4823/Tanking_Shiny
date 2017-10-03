library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(dtplyr)
library(markdown)
library(openxlsx)
# loading packages

##########################################################################################
##########################################################################################
################################# Defining Functions #####################################
##########################################################################################
##########################################################################################

Simulate_NBA_current = function(){
  Team = c("Boston Celtics", "Phoenix Suns", "Los Angeles Lakers", "Philadelphia 76ers", 
           "Orlando Magic", "Minnesota Timberwolves", "New York Knicks", "Sacramento Kings", 
           "Dallas Mavericks", "New Orleans Pelicans", "Charlotte Hornets", "Detroit Pistons", 
           "Denver Nuggets", "Miami Heat")
  # list of NBA teams
  
  Balls = c(250, 199, 156, 119, 88, 63, 43, 28, 17, 11, 8, 7, 6, 5)
  # probability for these respective teams (they draw balls)
  
  SimVector = rep(Team, Balls)
  # creating a vector with the proper counts for each team based on the ball counts
  
  z = c()
  i = 1
  while(length(z) <= 2){ # repeat the loop 3 times
    x = sample(SimVector, 1, replace = T)
    # pick a team 
    SimVector = SimVector[SimVector != x]
    # reassign SimVector to all teams that were NOT picked
    z[i] = x
    # storing the result in the empty vector created earlier
    i = i + 1
  }
  z[4:14] = Team[!(Team %in% z)]
  # adding in the remaining teams in reverse order of standings
  return(z)
}

Simulate_NBA_proposed = function(){
  Team = c("Boston Celtics", "Phoenix Suns", "Los Angeles Lakers", "Philadelphia 76ers", 
           "Orlando Magic", "Minnesota Timberwolves", "New York Knicks", "Sacramento Kings", 
           "Dallas Mavericks", "New Orleans Pelicans", "Charlotte Hornets", "Detroit Pistons", 
           "Denver Nuggets", "Miami Heat")
  # list of NBA teams
  newlottery_bottom7 = Team[1:7]
  # bottom 7 teams
  newlottery_top7 = Team[8:14]
  # top 7 teams
  x = sample(newlottery_bottom7, 3, replace = F)
  # picking 3 teams for the top 3 picks in the bottom7 bracket 
  x[4:7] = newlottery_bottom7[!(newlottery_bottom7 %in% x)]
  # adding in the bottom 4 teams in this bracket
  y = sample(newlottery_top7, 3, replace = F)
  # picking 3 teams for the top 3 picks in the top7 bracket 
  y[4:7] = newlottery_top7[!(newlottery_top7 %in% y)]
  # adding in the bottom 4 teams in this bracket
  z = c(x, y)
  # concatenating
  return(z)
}

nba_switch = function(x){
  switch(x, 
         "Boston Celtics" = 2,
         "Phoenix Suns" = 3,
         "Los Angeles Lakers" = 4,
         "Philadelphia 76ers" = 5, 
         "Orlando Magic" = 6,
         "Minnesota Timberwolves" = 7,
         "New York Knicks" = 8,
         "Sacramento Kings" = 9, 
         "Dallas Mavericks" = 10,
         "New Orleans Pelicans" = 11,
         "Charlotte Hornets" = 12,
         "Detroit Pistons" = 13, 
         "Denver Nuggets" = 14, 
         "Miami Heat" = 15
  )
}
# the reason this starts at 2 and not 1 is that the numbers are used to reference columms in the data frame NBAProb, where
# column 1 is the pick number and the teams begin with column 2. 

nba_switch2 = function(x){
  switch(x, 
         "1" = "Boston Celtics",
         "2" = "Phoenix Suns",
         "3" = "Los Angeles Lakers",
         "4" = "Philadelphia 76ers",
         "5" = "Orlando Magic",
         "6" = "Minnesota Timberwolves",
         "7" = "New York Knicks",
         "8" = "Sacramento Kings",
         "9" = "Dallas Mavericks",
         "10" = "New Orleans Pelicans",
         "11" = "Charlotte Hornets",
         "12" = "Detroit Pistons",  
         "13" = "Denver Nuggets",
         "14" = "Miami Heat")
}

##########################################################################################
##########################################################################################
################################# Data Cleaning ##########################################
##########################################################################################
##########################################################################################

setwd("~/")
# to avoid error for " cannot change working directory" that sometimes occurs with relative path 
setwd("./Shiny Apps/Tanking3")
NBA_final = list.files(".", pattern = "Sportsref[0-9]+")
# creating a vector with the files I want to read in
NBA_final = lapply(NBA_final, function(x) read.xlsx(x, 
                                                    sheet = 1, 
                                                    colNames = T))
# applying read.xlsx function to the vector of files, output is a list, each element being a data frame
NBA_final = do.call(rbind, NBA_final)
# rbinding the list of data frames together 
colnames(NBA_final) = NBA_final %>% slice(1)
# renaming the columns
NBA_final = NBA_final[2:945, ]
# omitting the first column which was the column names
NBA_final = NBA_final %>% 
  rename(WS_48 = `WS/48`) %>% 
  arrange(Year) %>% 
  filter(Year != 2017)
# filtering out 2017 because these players were just drafted-- they have no NBA statistics yet. 
NBA_final = NBA_final[!is.na(NBA_final$WS_48), ]
NBA_final = NBA_final %>% slice(1:892)
# the last few rows were column headers from the individual data frames, I am filtering them out
NBA_final$WS_48 = as.numeric(NBA_final$WS_48)
# changing to numeric
NBA_final$Pk = as.numeric(NBA_final$Pk)
# changing to numeric

NBA_final_wsgraph = NBA_final %>% 
  group_by(Pk) %>% 
  summarise(Avg_WS = mean(WS_48)) %>% 
  arrange(Pk)
# creating the data frame for Win Shares

NBAProb = read.csv("NBA Lottery Probabilities.csv", stringsAsFactors = F)
NBAProb = NBAProb %>% 
  mutate(Pk = c(1:14)) %>% 
  select(15, 1:14)
# reading in the current lottery probability csv, mutating PK and reordering the columns

NBAProb2 = read.csv("NBA Lottery Probabilities2.csv", stringsAsFactors = F)
NBAProb2 = NBAProb2 %>% 
  slice(1:14) %>% 
  select(-16) %>% 
  rename(Pk = Pick)
# reading in the proposed lottery probability csv

NBAProb3 = left_join(NBAProb, NBAProb2, by = "Pk")
# joining the two data frames
colnames(NBAProb3) = c("Pk", paste0(1:14, ".x"), paste0(1:14, ".y"))
# renaming the columns





####################### FOR THE CHAMPIONS DATA #########################

nbachamp = list.files(".", pattern = "Champ")
# creating a vector with the files I want to read in
nbachamp = lapply(nbachamp, function(x) read.xlsx(x, sheet = 1))
nbachamp = do.call(rbind, nbachamp)
# colnames(nbachamp) = nbachamp %>% slice(1)
colnames(nbachamp) = colnames(nbachamp) = c("Rk", "Player", "Age", "G", "GS", "MP", "FG", "FGA", "3P", "3PA", "FT", 
                                            "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "FGper", 
                                            "3Pper", "FTper", "MP2", "PTS2", "TRB2", "AST2", "STL2", "BLK2")
nbachamp = nbachamp %>% slice(2:455)
# filtering out the column row (not identical to the names manually typed above)
nbachamp = nbachamp[!is.na(nbachamp$Rk),]
# filtering out NAs
nbachamp = nbachamp %>% filter(Rk != "Rk")
nbachamp$Rk = as.numeric(nbachamp$Rk)
# making numeric
nbachamp2 = nbachamp %>% 
  group_by(Player) %>% 
  summarise(Number_Champ = n())
NBA_final = left_join(NBA_final, nbachamp2, by = "Player")
# joining the nba champion data to NBA_final 
NBA_final$Number_Champ[is.na(NBA_final$Number_Champ)] = 0
# setting NAs to 0 (NA in this case means no championships)

Champ_by_pick = NBA_final %>% 
  group_by(Pk) %>% 
  summarise(Total_Champ = sum(Number_Champ))
# Sum of champions by pick- for use in plots later






####################### FOR THE ALL STAR DATA #########################

nba_allstar = read.csv("NBA_All_Star_Rosters.csv", stringsAsFactors = F)
# reading in the csv 
colnames(nba_allstar) = c("Player", "Team", "Year", "Is_AllStar")
# changing column names
nba_allstar = nba_allstar %>% 
  group_by(Player) %>% 
  mutate(Number_AllStar = n()) %>% 
  select(Player, Is_AllStar, Number_AllStar)
# adding a new column for number of All Star Appearances

# Below I am fixing some inconsistencies between the data frames. I obtained the data from separate sources, so
# there were sometimes differences in spellings like Lebron vs LeBron. 
nba_allstar$Player[nba_allstar$Player == "Lebron James"] = "LeBron James"
nba_allstar$Player[nba_allstar$Player == "Isiah Thomas"] = "Isaiah Thomas"
nba_allstar$Player[nba_allstar$Player == "Amar'e Stoudemire"] = "Amare Stoudemire"
nba_allstar = unique(nba_allstar)
NBA_final$Player[464] = "Amare Stoudemire"
nba_allstar = nba_allstar[c(1:34, 36:229), ]

NBA_final = left_join(NBA_final, nba_allstar, by = "Player")
# joining the All Star data into NBA_final
NBA_final$Is_AllStar[is.na(NBA_final$Is_AllStar)] = "No"
# setting NAs to be "No" for the Is_AllStar column 
NBA_final$Number_AllStar[is.na(NBA_final$Number_AllStar)] = 0
# setting Nas to be 0 for the Number_AllStar column

n1 = NBA_final %>% 
  group_by(Pk, Is_AllStar) %>% 
  summarise(n = n()) %>% 
  mutate(Proportion_AllStar = n / sum(n))
# summarizing and getting the propotion

nba_prob = NBAProb2 - NBAProb
# subtracting the probabilities to get the differential
nba_prob$Pk = 1:14
# adding a column Pk for the pick number
colnames(nba_prob) = c("Pk", paste0("Team", 1:14))
# renaming the columns
