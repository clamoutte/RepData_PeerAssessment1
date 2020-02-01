

library(tidyr)
library(dplyr)
library(ggplot2)

dirName <- "data"
dirPath <- paste0("./",dirName)
activityFile <- "activity.csv"
if(!file.exists(dirName)){
  dir.create(dirName)
}
unzip("activity.zip", exdir=dirPath)

activities <- read.csv(paste0(dirPath,"/",activityFile))

mm <- activities %>% filter(!is.na(steps))

##What is mean total number of steps taken per day?

activityDailySummary <- activitiesPP %>% 
                      group_by(date) %>% 
                      summarise(total = sum(steps), mean=mean(steps), median = median(steps))

hist(activityDailySummary$total, main= "Total # of Steps per Day", xlab= "Total Steps", col = "grey")

##What is the average daily activity pattern?
activityIntervalSummary <- activitiesPP %>% 
  group_by(interval) %>% 
  summarise(total = sum(steps), mean=mean(steps), median = median(steps))

plot(activityIntervalSummary$interval, activityIntervalSummary$mean, type = "l", xlab = "Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern")

activityIntervalSummary[which.max(activityIntervalSummary$mean),]


##Inputing missing values

sum(is.na(activities))

##Fill in Missing Values with average for the day

activitiesNew <- merge(activities, as.data.frame(activityIntervalSummary), by="interval")

activitiesNew %>% filter(is.na(steps))

activitiesNA <- activitiesNew %>% filter(is.na(steps)) %>% mutate(steps = mean)
activityImputedDailySummary <- activitiesImputed %>% 
  group_by(date) %>% 
  summarise(total = sum(steps), mean=mean(steps), median = median(steps))

hist(activityImputedDailySummary$total, main= "Total # of Steps per Day (Imputed)", xlab= "Total Steps", col = "grey")

mean(activityImputedDailySummary$total)
median(activityImputedDailySummary$total)


##Are there differences in activity patterns between weekdays and weekends?
activitiesImputed$dayOfWeek <- sapply(as.Date(activitiesImputed$date), function(x) {  if(weekdays(x) == "Saturday" | weekdays(x)== "Sunday")  "weekend" else "weekday"} )

activityImputedIntervalSummary <- activitiesImputed %>% 
  group_by(interval,dayOfWeek) %>% 
  summarise(total = sum(steps), mean=mean(steps), median = median(steps))


ggplot(activityImputedIntervalSummary, aes(interval, mean)) + 
  geom_line() + 
  facet_grid(dayOfWeek ~ .) +
  xlab("Interval") + 
  ylab("Average # of steps")