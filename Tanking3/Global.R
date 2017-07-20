library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(dtplyr)
library(markdown)
library(openxlsx)

Simulate_NBA = function(){
  Team = c("Boston Celtics", "Phoenix Suns", "Los Angeles Lakers", "Philadelphia 76ers", 
           "Orlando Magic", "Minnesota Timberwolves", "New York Knicks", "Sacramento Kings", 
           "Dallas Mavericks", "New Orleans Pelicans", "Charlotte Hornets", "Detroit Pistons", 
           "Denver Nuggets", "Miami Heat")
  Balls = c(250, 199, 156, 119, 88, 63, 43, 28, 17, 11, 8, 7, 6, 5)
  SimVector = rep(Team, Balls)
  z = c()
  i = 1
  x = sample(SimVector, 1, replace = T)
  y = SimVector[SimVector != x]
  z[1] = x
  while(length(z) <= 2){
    x = sample(y, 1, replace = T)
    y = y[y != x]
    i = i + 1
    z[i] = x
  }
  q = z
  a = Team[Team %in% z == F]
  q[4:14] = a 
  return(q)
}

Simulate_NBA2 = function(){
  Team = c("Boston Celtics", "Phoenix Suns", "Los Angeles Lakers", "Philadelphia 76ers", 
           "Orlando Magic", "Minnesota Timberwolves", "New York Knicks", "Sacramento Kings", 
           "Dallas Mavericks", "New Orleans Pelicans", "Charlotte Hornets", "Detroit Pistons", 
           "Denver Nuggets", "Miami Heat")
  newlottery_bottom7 = Team[1:7]
  newlottery_top7 = Team[8:14]
  y = sample(newlottery_bottom7, 3, replace = F)
  x = newlottery_bottom7[newlottery_bottom7 %in% y == F]
  a = sample(newlottery_top7, 3, replace = F)
  b = newlottery_top7[newlottery_top7 %in% a == F]
  z = c(y, x, a, b)
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

# This is where EVERYTHING will be saved for re-doing my data munging

NBA_final = list.files(pattern = "Sportsref[0-9]+")
NBA_final = lapply(NBA_final, function(x) read.xlsx(x, sheet = 1, colNames = T))
NBA_final = do.call(rbind, NBA_final)
colnames(NBA_final) = NBA_final %>% slice(1)
NBA_final = NBA_final[2:945, ]
NBA_final = NBA_final %>% rename(WS_48 = `WS/48`)
NBA_final = NBA_final %>% arrange(Year)
NBA_final = NBA_final %>% filter(Year != 2017)
NBA_final = NBA_final[(is.na(NBA_final$WS_48) == F), ]
NBA_final = NBA_final %>% slice(1:905)
NBA_final = NBA_final[is.na(NBA_final$WS_48) == F, ]
NBA_final = NBA_final %>% slice(1:892)
NBA_final$WS_48 = as.numeric(NBA_final$WS_48)
NBA_final$Pk = as.numeric(NBA_final$Pk)

NBA_final_wsgraph = NBA_final %>% group_by(Pk) %>% summarise(Avg_WS = mean(WS_48)) %>% 
  arrange (Pk)
NBA_final_wsgraph = NBA_final_wsgraph %>% arrange(Pk)

NBAProb = read.csv("NBA Lottery Probabilities.csv", stringsAsFactors = F)
NBAProb = NBAProb %>% mutate(Pk = c(1:14))
NBAProb = NBAProb %>% select(15, 1:14)

NBAProb2 = read.csv("NBA Lottery Probabilities2.csv", stringsAsFactors = F)
NBAProb2 = NBAProb2 %>% slice(1:14)
NBAProb2 = NBAProb2 %>% select(-16)
NBAProb2 = NBAProb2 %>% rename(Pk = Pick)

NBAProb3 = left_join(NBAProb, NBAProb2, by = "Pk")



# for the Champions Data 

nbachamp = list.files(pattern = "Champ")
nbachamp = lapply(list.files(pattern = "Champ.*.xlsx"), function(x) read.xlsx(x, sheet = 1))
nbachamp = do.call(rbind, nbachamp)
colnames(nbachamp) = nbachamp %>% slice(1)
colnames(nbachamp) = colnames(nbachamp) = c("Rk", "Player", "Age", "G", "GS", "MP", "FG", "FGA", "3P", "3PA", "FT", "FTA",    "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "FGper", "3Pper", "FTper", "MP2", "PTS2", "TRB2", "AST2", "STL2", "BLK2")
nbachamp = nbachamp %>% slice(2:455)
nbachamp = nbachamp[is.na(nbachamp$Rk) == F ,]
nbachamp = nbachamp %>% filter(Rk != "Rk")
nbachamp$Rk = as.numeric(nbachamp$Rk)
nbachamp %>% group_by(Rk) %>% summarise(Number_Champs = n())

# for joining nbachamp with nba_final

nbachamp2 = nbachamp %>% group_by(Player) %>% summarise(Number_Champ = n())
NBA_final = left_join(NBA_final, nbachamp2, by = "Player")
# NBA_final now has Champion data added to it. Need to clean up the NAs
NBA_final$Number_Champ[is.na(NBA_final$Number_Champ)] = 0
sum(is.na(NBA_final$Number_Champ)) # returns 0. No NAs in Number_Champ now

Champ_by_pick = NBA_final %>% group_by(Pk) %>% summarise(Total_Champ = sum(Number_Champ))

# for the NBA All Star Data 

nba_allstar = read.csv("NBA_All_Star_Rosters.csv", stringsAsFactors = F)

#Now going to add this to NBA_final again
nba_allstar = nba_allstar %>% group_by(Player) %>% mutate(Number_AllStar = n()) 
colnames(nba_allstar) = c("Player", "Team", "Year", "Is_AllStar", "Number_AllStar")
nba_allstar2 = nba_allstar %>% select(Player, Is_AllStar, Number_AllStar)
nba_allstar2$Player[nba_allstar2$Player == "Lebron James"]
nba_allstar2$Player[nba_allstar2$Player == "Lebron James"] = "LeBron James"
nba_allstar2$Player[nba_allstar2$Player == "Isiah Thomas"] = "Isaiah Thomas"
nba_allstar2$Player[nba_allstar2$Player == "Amar'e Stoudemire"] = "Amare Stoudemire"
nba_allstar2 = unique(nba_allstar2)
NBA_final$Player[464] = "Amare Stoudemire"
nba_allstar2 = nba_allstar2[c(1:34, 36:229), ]

NBA_final = left_join(NBA_final, nba_allstar2, by = "Player")
NBA_final$Is_AllStar[is.na(NBA_final$Is_AllStar)] = "No"
NBA_final$Number_AllStar[is.na(NBA_final$Number_AllStar)] = 0

n1 = NBA_final %>% group_by(Pk, Is_AllStar) %>% summarise(n = n())
n1= n1 %>% mutate(Proportion_AllStar = n / sum(n))
ggplot(data = n1, aes(x = Pk, y = Proportion_AllStar)) + geom_line(aes(color = Is_AllStar))

paste0(1:14, ".x")
paste0(1:14, ".y")
colnames(NBAProb3) = c("Pk", paste0(1:14, ".x"), paste0(1:14, ".y"))

# Exploring new graphs
nba_range = NBA_final %>% filter(G >=82) %>% group_by(Pk) %>% summarise(Range = max(WS_48) - min(WS_48))
ggplot(data = nba_range, aes(x = Pk, y = Range)) + geom_point()

nba_prob = NBAProb2 - NBAProb
nba_prob["Pk"] = 1:14
ggplot(data = nba_prob, aes(x = Pk, y = BOS)) + geom_col()

paste0("Team", 1:14)
colnames(nba_prob) = c("Pk", paste0("Team", 1:14))
