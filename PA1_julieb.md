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
maxint<-with(intsdf, intsdf[which(sums == max(sums)),2])
```
The 5-minute interval where the maximum average number of steps are taken is 520.

## Imputing missing values
The presence of missing days may introduce bias into some
calculations or summaries of the data.  
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs).

```r
sumNAs <-sum(is.na(actdata$steps))
```
Answer: The total number of missing step values is: 2304.


## Are there differences in activity patterns between weekdays and weekends?
