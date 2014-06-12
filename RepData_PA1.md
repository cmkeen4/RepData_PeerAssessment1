# Reproducible Research: Peer Assessment 1
by Chris Keen
========================================================

This is the R Markdown document I am submitting for Peer Assessment 1. 

## Loading and preprocessing the data
I used the unzip() function to unzip the file. Then created the table 'ActivityData' from the read.csv function.


```r
unzip("activity.zip")
ActivityData <- read.csv("activity.csv")

head(ActivityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
dim(ActivityData)
```

```
## [1] 17568     3
```

```r
summary(ActivityData)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

## What is mean total number of steps taken per day?

I used tapply to find the total number(sum) of steps taken each day.


```r
sumAD <- with(ActivityData, tapply(steps, date, sum, na.rm=TRUE))
```

Then I created a histogram using the results.

```r
hist(sumAD, ylim=range(0:30), main="Daily Total Steps Taken", 
     xlab="Total of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Finally, I calculated the Mean and Median for the total steps taken each day.

```r
mnADsum <- mean(sumAD)
medADsum <- median(sumAD)

print(paste("ActivityData Mean: ", mnADsum))
```

```
## [1] "ActivityData Mean:  9354.22950819672"
```

```r
print(paste("ActivityData Median: ", medADsum))
```

```
## [1] "ActivityData Median:  10395"
```

## What is the average daily activity pattern?
I calculated the Mean of steps for each interval across all the days.


```r
mnADintervals <- with(ActivityData, tapply(steps, interval, mean, na.rm=TRUE))
```
Next, I created a time series plot of the average number of steps taken at each 5-minute interval each day.


```r
plot(mnADintervals, type="l", xlab="5-Minute Intervals(24hr clock)", 
     ylab="Avg Number of Steps", main="Avg. Number of Steps per Interval", 
     xaxt="n")
    axis(side=1, at=seq(0,288, 12), labels=c(0:24))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Finally, I searched for the interval with the maximum number of steps on average for each day.


```r
maxADint <- max(mnADintervals)
names(mnADintervals)[which(mnADintervals == maxADint)]
```

```
## [1] "835"
```


## Imputing missing values
Calculate the number of 'NA' values in the dataset.

```r
sumADna <- sum(is.na(ActivityData))
print(sumADna)
```

```
## [1] 2304
```
I copied the ActivityData data frame to a new data frame, completeAD, in order to replace the 'NA' values. Then used a for loop to replace the 'NA' values with the means for each of the 5-minute intervals.  

```r
completeAD <- ActivityData

for (i in which(sapply(completeAD, is.numeric))) {
    completeAD[is.na(completeAD[, i]), i] <- 
        with(completeAD, tapply(steps, interval, mean, na.rm=TRUE))
}
```

I used tapply to find the total number(sum) of steps taken each day for the new dataset 'completeAD'.

```r
compADsteps <- with(completeAD, tapply(steps, date, sum, na.rm=TRUE))
```
Here is a comparison of the two datasets 'ActivityData', which has the NA values included and 'completeAD', which has the NA values replaced with the means for each of the 5-minute intervals.  
The first comparison is in the form of histograms

```r
par(mfcol=c(1,2))
hist(sumAD, ylim=range(0:40), 
     main="Daily Total Steps Taken\n (w/ NA's)", 
     xlab="Total of Steps")
hist(compADsteps, ylim=range(0:40), 
     main="Daily Total Steps Taken\n (NA's replaced)", 
     xlab="Total of Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

Finally, I created a data frame with the Mean and Median for each data set.

```r
mncompAD <- mean(compADsteps)
medcompAD <- median(compADsteps)

bothMN <- c(mnADsum, mncompAD)
bothMED <- c(medADsum, medcompAD)
difMNMED <- c((mncompAD-mnADsum), (medcompAD-medADsum))
MNMEDdf <- data.frame(bothMN, bothMED)
MNMEDdf <- rbind(MNMEDdf, difMNMED)
rownames(MNMEDdf) <- c("w/ NA's", "NA's replaced", "Difference")
colnames(MNMEDdf) <- c("Mean", "Median")
print(MNMEDdf)
```

```
##                Mean  Median
## w/ NA's        9354 10395.0
## NA's replaced 10766 10766.2
## Difference     1412   371.2
```

## Are there differences in activity patterns between weekdays and weekends?
Created a new column in completeAD dataset for two level factor "weekday" and "weekend".


```r
completeAD$date <- as.Date(completeAD$date)
completeAD$weekdays <- as.factor(weekdays(completeAD$date))

levels(completeAD$weekdays) <- c("weekday", "weekday", "weekday", "weekday", 
                                "weekday", "weekend", "weekend")

str(completeAD)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekdays: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Then I calculated the Mean of steps for each interval across the two factors.
Here I split the dataset 'completeAD' into two separate datasets; 'weekdayDF' and 'weekendDF'.  


```r
weekdayDF <- subset(completeAD, completeAD$weekdays == "weekday")
weekendDF <- subset(completeAD, completeAD$weekdays == "weekend")
```

This is the mean of steps for each interval for the "weekday" factor.

```r
mnWeekdayInts <- with(weekdayDF, tapply(steps, interval, mean, na.rm=TRUE))
```
This is the mean of steps for each interval for the "weekend" factor.

```r
mnWeekendInts <- with(weekendDF, tapply(steps, interval, mean, na.rm=TRUE))
```

Finally, I created two time plots to show comparison between the "Weekday" and "Weekend" Mean of steps for each interval.

```r
par(mfrow = c(2,1))
plot(mnWeekdayInts, type="l", xlab="5-Minute Intervals(24hr clock)", 
     ylab="Avg Number of Steps", main="Avg. Number of Steps per Interval\n Weekdays", 
     xaxt="n")
axis(side=1, at=seq(0,288, 12), labels=c(0:24))
plot(mnWeekendInts, type="l", xlab="5-Minute Intervals(24hr clock)", 
     ylab="Avg Number of Steps", main="Avg. Number of Steps per Interval\n Weekends", 
     xaxt="n")
axis(side=1, at=seq(0,288, 12), labels=c(0:24))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 
