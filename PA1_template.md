---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

The following is a project assignement for Coursera's Reproducible Research course. The assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Loading and preprocessing the data

We will use the following libraries to explora and analyze the data:


```r
library(tidyr)
library(dplyr)
library(ggplot2)
```

A zip file containing the data has already been loaded in the project directory. It needs to be unzipped.

1. Load the data

```r
dirName <- "data"
dirPath <- paste0("./",dirName)
activityFile <- "activity.csv"
if(!file.exists(dirName)){
  dir.create(dirName)
}
unzip("activity.zip", exdir=dirPath)

activities <- read.csv(paste0(dirPath,"/",activityFile))
```

2. Transform the data
We are creating a subset will all na values removed.


```r
activitiesPP <- activities %>% filter(!is.na(steps))
```

## What is the mean total number of steps taken per day?

We will remove NA values before making any calculations.

1. Calculate the total number of steps taken per day


```r
activityDailySummary <- activitiesPP %>% 
                      group_by(date) %>% 
                      summarise(total = sum(steps), mean=mean(steps), median = median(steps))


activityDailySummary
```

```
## # A tibble: 53 x 4
##    date       total   mean median
##    <fct>      <int>  <dbl>  <dbl>
##  1 2012-10-02   126  0.438      0
##  2 2012-10-03 11352 39.4        0
##  3 2012-10-04 12116 42.1        0
##  4 2012-10-05 13294 46.2        0
##  5 2012-10-06 15420 53.5        0
##  6 2012-10-07 11015 38.2        0
##  7 2012-10-09 12811 44.5        0
##  8 2012-10-10  9900 34.4        0
##  9 2012-10-11 10304 35.8        0
## 10 2012-10-12 17382 60.4        0
## # ... with 43 more rows
```

2. Make a histogram of the total number of steps taken each day


```r
hist(activityDailySummary$total, main= "Total # of Steps per Day", xlab= "Total Steps", col = "grey")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(activityDailySummary$total)
```

```
## [1] 10766.19
```

```r
median(activityDailySummary$total)
```

```
## [1] 10765
```

The mean of the total number of steps taken per day is **10,766.19**. 
The median is **10,765**.


## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activityIntervalSummary <- activitiesPP %>% 
  group_by(interval) %>% 
  summarise(total = sum(steps), mean=mean(steps), median = median(steps))

plot(activityIntervalSummary$interval, activityIntervalSummary$mean, type = "l", xlab = "Interval", ylab = "Average Steps", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activityIntervalSummary[which.max(activityIntervalSummary$mean),]
```

```
## # A tibble: 1 x 4
##   interval total  mean median
##      <int> <int> <dbl>  <int>
## 1      835 10927  206.     19
```

The **8:35** minute interval has the maximum number of daily steps.

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(activities))
```

```
## [1] 2304
```

We want to confirm the **NA** values are only present in steps  points and that we do not have any missing date or interval values in the data set.


```r
sum(is.na(activities$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activities$date))
```

```
## [1] 0
```

```r
sum(is.na(activities$interval))
```

```
## [1] 0
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

As suggested in the assignment, I will fill in missing values based on the mean for that day.

First, add daily total steps, mean and median to the original data set.
 
 
 ```r
 activitiesNew <- merge(activities, as.data.frame(activityDailySummary), by="date")
 
 head(activitiesNew)
 ```
 
 ```
 ##         date steps interval total   mean median
 ## 1 2012-10-02     0     1740   126 0.4375      0
 ## 2 2012-10-02     0     1715   126 0.4375      0
 ## 3 2012-10-02     0     1725   126 0.4375      0
 ## 4 2012-10-02     0     1710   126 0.4375      0
 ## 5 2012-10-02     0     1735   126 0.4375      0
 ## 6 2012-10-02     0     1855   126 0.4375      0
 ```
 
 ```r
 activitiesNew %>% filter(is.na(steps))
 ```
 
 ```
 ## [1] date     steps    interval total    mean     median  
 ## <0 rows> (or 0-length row.names)
 ```
 
After discovering that some dates simply do not have any interval values, I decide to fill in NA values with the mean for the interval.
 
 
 ```r
  activitiesNew <- merge(activities, as.data.frame(activityIntervalSummary), by="interval")
   head(activitiesNew)
 ```
 
 ```
 ##   interval steps       date total     mean median
 ## 1        0    NA 2012-10-01    91 1.716981      0
 ## 2        0     0 2012-11-23    91 1.716981      0
 ## 3        0     0 2012-10-28    91 1.716981      0
 ## 4        0     0 2012-11-06    91 1.716981      0
 ## 5        0     0 2012-11-24    91 1.716981      0
 ## 6        0     0 2012-11-15    91 1.716981      0
 ```
 
 ```r
 head(activitiesNew %>% filter(is.na(steps)))
 ```
 
 ```
 ##   interval steps       date total     mean median
 ## 1        0    NA 2012-10-01    91 1.716981      0
 ## 2        0    NA 2012-11-04    91 1.716981      0
 ## 3        0    NA 2012-11-30    91 1.716981      0
 ## 4        0    NA 2012-11-14    91 1.716981      0
 ## 5        0    NA 2012-11-09    91 1.716981      0
 ## 6        0    NA 2012-11-01    91 1.716981      0
 ```

Then, fill in **NA** values in the steps column, with the mean for the interval. 


```r
activitiesNA <- activitiesNew %>% filter(is.na(steps)) %>% mutate(steps = mean)

head(activitiesNA)
```

```
##   interval    steps       date total     mean median
## 1        0 1.716981 2012-10-01    91 1.716981      0
## 2        0 1.716981 2012-11-04    91 1.716981      0
## 3        0 1.716981 2012-11-30    91 1.716981      0
## 4        0 1.716981 2012-11-14    91 1.716981      0
## 5        0 1.716981 2012-11-09    91 1.716981      0
## 6        0 1.716981 2012-11-01    91 1.716981      0
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
 activitiesImputed <- rbind(activitiesNew %>% filter(!is.na(steps)), activitiesNA) %>% arrange(date)
 head(activitiesImputed)
```

```
##   interval     steps       date total      mean median
## 1        0 1.7169811 2012-10-01    91 1.7169811      0
## 2        5 0.3396226 2012-10-01    18 0.3396226      0
## 3       10 0.1320755 2012-10-01     7 0.1320755      0
## 4       15 0.1509434 2012-10-01     8 0.1509434      0
## 5       20 0.0754717 2012-10-01     4 0.0754717      0
## 6       25 2.0943396 2012-10-01   111 2.0943396      0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
   

```r
activityImputedDailySummary <- activitiesImputed %>% 
                      group_by(date) %>% 
                      summarise(total = sum(steps), mean=mean(steps), median = median(steps))

hist(activityImputedDailySummary$total, main= "Total # of Steps per Day (Imputed)", xlab= "Total Steps", col = "grey")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
mean(activityImputedDailySummary$total)
```

```
## [1] 10766.19
```

```r
median(activityImputedDailySummary$total)
```

```
## [1] 10766.19
```

Imputing missing values based on the mean for the interval does not have a significant impact on any of the total sum, mean or median of steps.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activitiesImputed$dayOfWeek <- sapply(as.Date(activitiesImputed$date), function(x) {  if(weekdays(x) == "Saturday" | weekdays(x)== "Sunday")  "weekend" else "weekday"} )

head(activitiesImputed)
```

```
##   interval     steps       date total      mean median dayOfWeek
## 1        0 1.7169811 2012-10-01    91 1.7169811      0   weekday
## 2        5 0.3396226 2012-10-01    18 0.3396226      0   weekday
## 3       10 0.1320755 2012-10-01     7 0.1320755      0   weekday
## 4       15 0.1509434 2012-10-01     8 0.1509434      0   weekday
## 5       20 0.0754717 2012-10-01     4 0.0754717      0   weekday
## 6       25 2.0943396 2012-10-01   111 2.0943396      0   weekday
```
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

First calculate the average # of steps per interval and type of day.


```r
activityImputedIntervalSummary <- activitiesImputed %>% 
  group_by(interval,dayOfWeek) %>% 
  summarise(total = sum(steps), mean=mean(steps), median = median(steps))
```

Plot the averages to compare weekdays and weekday estimates.


```r
ggplot(activityImputedIntervalSummary, aes(interval, mean)) + 
  geom_line() + 
  facet_grid(dayOfWeek ~ .) +
  xlab("Interval") + 
  ylab("Average # of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
  
The average number of steps is generally higher during the weekends than on weekends. However the maxu=imum number of steps is achieved during weekdays.
