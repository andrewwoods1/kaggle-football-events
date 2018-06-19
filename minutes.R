library(ggplot2)
library(MASS)
library(dplyr)


df = read.csv(file.choose())
names(df)

#Home shots and time between them
soth = subset(df,df$event_type == 1 & df$side == 1)
soth = soth %>%
  arrange(id_odsp)

wait_time = aggregate(soth$time,by = list(soth$id_odsp,soth$side),FUN = diff)
wait_time = wait_time %>%
  arrange(Group.1)


wait_diff = c()
for (x in wait_time$Group.1){
  wait_diff = c(wait_diff,'first',wait_time$x[wait_time$Group.1 == x])
}
wait_diff = unlist(wait_diff)

soth$wait_diff = wait_diff
soth$wait_diff[soth$wait_diff < 0] = 0


soth$wait_diff[soth$wait_diff == "first"] = soth$time[soth$wait_diff == "first"]
soth$wait_diff = as.numeric(soth$wait_diff)

summary(soth$wait_diff)


# Away shots
sota = subset(df,df$event_type == 1 & df$side == 2)
sota = sota %>%
  arrange(id_odsp)

wait_time = aggregate(sota$time,by = list(sota$id_odsp,sota$side),FUN = diff)
wait_time = wait_time %>%
  arrange(Group.1)

wait_diff = c()
for (x in wait_time$Group.1){
  wait_diff = c(wait_diff,'first',wait_time$x[wait_time$Group.1 == x])
}
wait_diff = unlist(wait_diff)

sota$wait_diff = wait_diff
sota$wait_diff[sota$wait_diff < 0] = 0


sota$wait_diff[sota$wait_diff == "first"] = sota$time[sota$wait_diff == "first"]
sota$wait_diff = as.numeric(sota$wait_diff)

summary(sota$wait_diff)
# Combine the two
sot = rbind(soth,sota)
summary(sot$wait_diff)






# Analyse

#Look at correlation and distribution of times in each category

mean(sot$wait_diff[sot$is_goal == 1])
mean(sot$wait_diff[sot$is_goal == 0])

sot$bin = cut(sot$wait_diff,breaks = c(0,10,20,30,40,50,100),right = FALSE)
conv = sot %>%
  group_by(bin) %>%
  summarise(Conversion_Rate = mean(is_goal), count = n())

ggplot(data = conv, aes(x = bin,y = conv$Conversion_Rate)) +
  geom_bar(stat = "identity",width = 0.7, fill = "#0B75B6") + coord_flip() +
  ylab("Shot Conversion Rate") + xlab("Time since previous shot") +
  geom_text(aes(label=count), vjust=0.3,hjust =1.2)

t.test(sot$is_goal[sot$wait_diff > 20],sot$is_goal[sot$wait_diff <= 20],
       alternative = "g")

