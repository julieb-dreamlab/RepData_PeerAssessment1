---
title: "Reproducible Research: Peer Assessment 1"
author: "julieb"
git_repo: "https://github.com/julieb-dreamlab/RepData_PeerAssessment1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
actdata <- read.csv("../data/activity.csv")
```


## What is mean total number of steps taken per day?
The total number of steps each day is calculated.
Report the summary of the daily totals


```r
#The total number of steps each day is calculated.
sm0<- with(actdata, tapply(steps,date,sum, na.rm=TRUE))
sumsdf<-data.frame(sm0)
names(sumsdf)<- c("sums")
c<-ggplot(sumsdf, aes(sums))
d<- c+ geom_histogram(binwidth = 1000)
d<- d+ labs(x= "Total Steps in a Day")
d<- d+ labs(y= "Count of Days")
d<- d+ labs(title= "Histogram of Total Steps in a Day")
print(d)
```

![](PA1_julieb_files/figure-html/sums-1.png)<!-- -->

```r
# report the summary of the daily totals
summary(sumsdf)
```

```
##       sums      
##  Min.   :    0  
##  1st Qu.: 6778  
##  Median :10395  
##  Mean   : 9354  
##  3rd Qu.:12811  
##  Max.   :21194
```

## What is the average daily activity pattern?
Sum the number of steps at each 5-minute interval across all days to see high activity periods.
Make a time series plot of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis).

```r
#The steps per 5-minute interval are calculated.
sm1<- with(actdata, tapply(steps,interval,sum, na.rm=TRUE))
intsdf<-data.frame(sm1)
names(intsdf)<- c("sums")
intsdf<- mutate(intsdf,minutes = 5*(1:nrow(intsdf)),interval = as.numeric(rownames(intsdf)))
```

```
## Warning: package 'bindrcpp' was built under R version 3.5.1
```

```r
c<-ggplot(intsdf, aes(minutes,sums))
d<- c+ geom_line()
d<- d+ labs(x= "Time of Day, minutes")
d<- d+ labs(y= "Total Steps during an Interval")
d<- d+ labs(title= "Daily Activity")
print(d)
```

![](PA1_julieb_files/figure-html/intervals-1.png)<!-- -->

```r
maxmin<-with(intsdf, intsdf[which(sums == max(sums)),2])
maxint<-with(intsdf, intsdf[which(sums == max(sums)),3])
```
The 5-minute interval where the maximum average number of steps are taken is Interval: 835, (520 minutes).

## Imputing missing values
The presence of missing days may introduce bias into some
calculations or summaries of the data.  
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs).

```r
sumNAs <-sum(is.na(actdata$steps))
meanNAs<-mean(is.na(actdata$steps))
perNAs<- meanNAs*100
```
Answer: The total number of missing step values is: 2304.  
This is 13.1147541% of the values in the dataset.

2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# Find the mean value for steps at each interval
sm2<- with(actdata, tapply(steps,interval,mean, na.rm=TRUE))
# Save these values in a data frame with the corresponding interval value
fsdf<-data.frame(sm2)
names(fsdf)<- c("means")
fsdf<- mutate(fsdf,interval = as.numeric(rownames(fsdf)))
```
3. Create a new dataset that is equal to the original dataset but with the
missing data filled in

```r
# Add two columns (needFill and filled) to the activity data frame 
actdata<- mutate(actdata,needFill=is.na(actdata$steps), filled=actdata$steps)
# Use the mean interval steps for the replacement values of NA
for(i in which(actdata$needFill==TRUE)){
  actdata$filled[i] <- fsdf$means[fsdf$interval==actdata$interval[i]]
}
```
4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do
these values differ from the estimates from the first part of the assignment?
What is the impact of imputing missing data on the estimates of the total
daily number of steps?


```r
#The total number of steps each day is calculated.
sm3<- with(actdata, tapply(filled,date,sum, na.rm=TRUE))
sumsdf<-data.frame(sm3)
names(sumsdf)<- c("sums")
c<-ggplot(sumsdf, aes(sums))
d<- c+ geom_histogram(binwidth = 1000)
d<- d+ labs(x= "Total Steps in a Day")
d<- d+ labs(y= "Count of Days")
d<- d+ labs(title= "Histogram of Total Steps in a Day (NAs replaced with Interval Mean Value")
print(d)
```

![](PA1_julieb_files/figure-html/sumsFilled-1.png)<!-- -->

```r
# report the summary of the daily totals
summary(sumsdf)
```

```
##       sums      
##  Min.   :   41  
##  1st Qu.: 9819  
##  Median :10766  
##  Mean   :10766  
##  3rd Qu.:12811  
##  Max.   :21194
```
Answer: The substituted mean interval values do not change the mean and median values of the totals.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.  

```r
# New column dayType for factor: weekday, weekend
# First, add a day column
actdata<-mutate(actdata,day = weekdays(as.Date(actdata$date)))
# Second, define the dayType levels
wf<- factor(actdata$day,levels = c("Sunday","Monday","Tuesday","Wednesday",
                                   "Thursday","Friday","Saturday"),
            labels = c("weekend","weekday","weekday","weekday","weekday",
                       "weekday","weekend"))
# Third, add the dayType column
actdata<-mutate(actdata,dayType = wf)
```
2. Make a panel plot containing a time series plot of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).

```r
#The steps per 5-minute interval are calculated.
library(lattice)
a1<- actdata %>% group_by(dayType,interval)%>%summarize(avg_steps=mean(filled))
c<- xyplot(avg_steps~interval | dayType, data=a1)
print(c)
```

![](PA1_julieb_files/figure-html/intervalsBydayType-1.png)<!-- -->
