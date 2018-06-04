# Build expected goals model with kaggle data
library(ggplot2)
library(RcppRoll)
library(reshape2)


setwd("C:\\Users\\Andrew\\Desktop\\Data\\football-events")

events = read.csv("events.csv",header = T)
games = read.csv("ginf.csv",header = T)

#add country column to events data
events = sqldf("
               SELECT A.*,B.country, B.date
               FROM events A left join games B on A.id_odsp = B.id_odsp
               ")

names(events)
names(games)

min(as.Date(games$date))
max(as.Date(games$date))

# Some preprocessing
# Remove penalties - 'shots from the penalty spot'
# 
events_sub = events[events$event_type == 1,]
events_sub = events_sub[events_sub$location != 14,]

fit = glm(is_goal ~
            time + side + location + bodypart + assist_method + situation + fast_break, data = events_sub,family = "binomial")

events_sub$chance_rating = fit$fitted.values

# add game total xG to game info dataset

hxg = aggregate(events_sub$chance_rating[events_sub$side == 1],by = list(events_sub$id_odsp[events_sub$side == 1],events_sub$event_team[events_sub$side == 1]), FUN = sum)
axg = aggregate(events_sub$chance_rating[events_sub$side == 2],by = list(events_sub$id_odsp[events_sub$side == 2],events_sub$event_team[events_sub$side == 2]), FUN = sum)  


games = merge(games,hxg,by.x = "id_odsp",by.y = "Group.1")
names(games)[20] = "home_xg"
games = merge(games,axg,by.x = "id_odsp",by.y = "Group.1")
names(games)[22] = "away_xg"


games$fttg = games$fthg + games$ftag
games$total_xg = games$home_xg + games$away_xg




# Create moving averag and visualise game totals
n = 50
steps = seq(1,nrow(games),n)
total_goals_ma = c()
total_xg_ma = c()
for (i in steps){
  print(i)
  total_goals_ma = c(total_goals_ma,mean(games$fttg[i:(i+n-1)]))  
  total_xg_ma = c(total_xg_ma,mean(games$total_xg[i:(i+n-1)]))
  }


ma = cbind(total_goals_ma,total_xg_ma)
ma = melt(ma)
ggplot(data = ma,aes(x = Var1,y = value,group = Var2, colour = Var2)) + geom_line(size = 1.2)





