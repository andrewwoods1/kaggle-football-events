#  Look at xG per game for all players
# and
# create post shot xg
library(sqldf)
library(ggplot2)
library(ggrepel)

events_sub$shot_place[is.na(events_sub$shot_place)] = 'Other'
events_sub$shot_place = as.factor(events_sub$shot_place)

fit = glm(is_goal ~
            time + side + shot_place + location + bodypart + assist_method + situation + fast_break, data = events_sub,family = "binomial")

events_sub$chance_rating_post = fit$fitted.values
events_sub$rating_differential = events_sub$chance_rating_post - events_sub$chance_rating


# Pull in number of games played for each player
no_games = sqldf("
                 select player, count (distinct id_odsp) as no_games
                 from events_sub
                 group by player
                 ")


diff = sqldf(
  "
  select player, avg(rating_differential) as av_diff
  from events_sub
  where shot_place <> 'Other'
  group by player
  "
)


xgpg = sqldf(
  "
  SELECT id_odsp, player, sum(chance_rating) as xg
  from events_sub
  where shot_place <> 'Other'
  group by id_odsp, player
  "
  )

xgpg = aggregate(xgpg$xg,
                 by = list(xgpg$player),
                 FUN = mean)



merged = merge(xgpg,diff,by.x = "Group.1",by.y = "player")
merged = merge(merged,no_games,by.x = "Group.1",by.y = "player")

###########################################################
# Now subset accordingly

sub = subset(merged,merged$av_diff >  0 & merged$no_games > 80 & merged$x > 0.3)
p = ggplot(data = sub, aes(x = sub$x,y = sub$av_diff,label = sub$Group.1)) + geom_point()
p + geom_label_repel()

p1 = ggplot(data = sub, aes(x = Group.1, y = av_diff)) + geom_bar()
p1


