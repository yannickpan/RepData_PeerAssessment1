---
title: "Reproducible Research Peer Assignment 1"
author: "Yangchen Pan"
date: "Friday, October 09, 2015"
output: html_document
---
#What is mean total number of steps taken per day?
##Calculate the total number of steps taken per day

```r
activitydata = read.csv("C:\\Users\\pyc66_000\\RepData_PeerAssessment1\\activity\\activity.csv")
act.stepsperday = aggregate(data = activitydata, steps ~ date, FUN = sum)
head(act.stepsperday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

##If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(steps,data=act.stepsperday,xlab='Total steps per day', ylab='Frequency using binwith 600',binwidth=600)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

##Calculate and report the mean and median of the total number of steps taken per day

```r
stepsMean = mean(act.stepsperday$steps)
stepsMedian = median(act.stepsperday$steps)
```

#What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsbyintvl = list(activitydata$steps)
intervals = list(activitydata$interval)
steps5intvl = aggregate(x = stepsbyintvl, by = intervals, FUN = mean,na.rm=TRUE)
colnames(steps5intvl)[1] = "intervals"
colnames(steps5intvl)[2] = "meansteps"
ggplot(data = steps5intvl, aes(x=intervals, y=meansteps))+geom_line() + xlab("5 minutes interval") + ylab("mean steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps = which.max(steps5intvl$meansteps)
steps5intvl[maxsteps,]
```

```
##     intervals meansteps
## 104       835  206.1698
```
The interval with max steps is: 835

#Imputing missing values
##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
num.missing = length(is.na(activitydata$steps))
```
The number of missing values in 'steps' column is: 17568

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
##Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
library(Hmisc)
activity.imputed = activitydata
activity.imputed$steps = impute(activitydata$steps, fun=mean)
```

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activity.impute.stepsbyday = aggregate(data=activity.imputed,steps~date,FUN = sum)
qplot(activity.impute.stepsbyday$steps,xlab='steps per day (imputed)', ylab='Frequency with binwith = 600', binwidth=600)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
imputedmeansteps = mean(activity.imputed$steps)
imputedmediansteps = median(activity.imputed$steps)
```
Imputed steps mean: 37.3825996
Imputed steps median: 0

#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity.imputed$date = as.Date(activity.imputed$date)
activity.imputed$datetype = ifelse(as.POSIXlt(activity.imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
activity.imputed = transform(activity.imputed, datetype = factor(datetype))
levels(activity.imputed$datetype)
```

```
## [1] "weekday" "weekend"
```

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activity.stepsbydatetype = aggregate(steps ~ interval + datetype, data=activity.imputed, mean)
ggplot(activity.stepsbydatetype, aes(interval, steps)) +
geom_line() +
facet_grid(datetype ~ .) +
xlab("5-minute interval") +
ylab("mean steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
