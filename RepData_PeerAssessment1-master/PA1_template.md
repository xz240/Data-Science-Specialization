# Course Project 1
Andrew Mendonca  
July 25, 2017  

## Loading and preprocessing the data

The first thing to do is unzip the file and load the data by read.csv.


```r
unzip(zipfile = "C:/Users/amend/Documents/GitHub/RepData_PeerAssessment1/activity.zip")
filedata <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA",
                     colClasses = cbind("numeric", "character", "integer"))
head(filedata)
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
The next thing to do is process/transform the data into a suitable format
by correcting the dates and removing rows with missing values.


```r
filedata$date <- as.Date(filedata$date, format = "%Y-%m-%d")
filedata_ignore <- subset(filedata, !is.na(filedata$steps))
```

## What is mean total number of steps taken per day?

First, calculate the total number of steps taken per day.


```r
stepsTotal <- tapply(filedata_ignore$steps, filedata_ignore$date, sum,
                     na.rm = TRUE)
```
Next, make a histogram of the total number of steps taken per day.


```r
hist(stepsTotal, breaks = 40, main = "The Distribution of Daily Total Steps 
     (missing data ignored)", xlab = "Total Steps Taken Per Day", 
     ylab = "Frequency", col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Next, calculate and report the mean and median of the total number of steps
taken per day.


```r
mean(stepsTotal)
```

```
## [1] 10766.19
```

```r
median(stepsTotal)
```

```
## [1] 10765
```
Therefore, the mean is 10766 steps and the median is 10765 steps.


## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis).


```r
average_pattern <- tapply(filedata_ignore$steps, filedata_ignore$interval,
                          mean, na.rm = TRUE)
filedata_ap <- data.frame(interval = as.integer(names(average_pattern)),
                          avg = average_pattern)
plot(filedata_ap$interval, filedata_ap$avg, type = "l", col = "Red",
     main = "Average Daily Activity Pattern", xlab = "5-minute interval",
     ylab = "Average Number of Steps Across All Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Next, determine which 5-minute interval, on average across all the days 
in the dataset, contains the maximum number of steps.


```r
filedata_ap[filedata_ap$avg == max(filedata_ap$avg),]
```

```
##     interval      avg
## 835      835 206.1698
```
It appears that the interval 835 contains a maximum of 206 steps.


## Imputing missing values

First, calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with NAs).


```r
sum(is.na(filedata$steps))
```

```
## [1] 2304
```
So the total number of rows with NAs is 2304.

A good strategy for filling in the missing values is to use the mean
for any 5-minute interval with a missing value.

Create a dataset filedata_imp that is equal to the original dataset 
but with the missing data filled in.


```r
filedata_imp <- filedata
new_data <- is.na(filedata_imp$steps)
average_pattern <- tapply(filedata_ignore$steps, filedata_ignore$interval,
                          mean, na.rm = TRUE)
filedata_imp$steps[new_data] <- average_pattern[as.character(filedata_imp$interval[new_data])]
head(filedata_imp)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Make a histogram of the total number of steps taken each day and calculate
and report the mean and median total number of steps taken per day.


```r
new_total <- tapply(filedata_imp$steps, filedata_imp$date, sum, na.rm = TRUE)
hist(new_total, breaks = 40, main = "The Distribution of Daily Total Steps 
     (with missing data imputed)", xlab = "Total Steps Taken Per Day", 
     ylab = "Frequency", col = "Blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(new_total)
```

```
## [1] 10766.19
```

```r
median(new_total)
```

```
## [1] 10766.19
```
The new mean is 10766 and the new median is 10766. Compared with the original mean 10766 
and the original median 10765, the mean stays the same and the median slightly changes.
It appears that the new median is identical to the mean because when filling out all the missing
data for the intervals, means are being used, so this results in more data being close or
identical to the intervals, and thus, the median becomes identical to the mean.

The impact of imputing data on the estimates of the total daily number of steps results 
in higher frequencies in the center region of the histogram, which is close to the mean.


## Are there differences in activity patterns between weekdays and weekends?

First, create a new factor variable "week" in the dataset with two
levels - "weekday" and "weekend" indicating whether a given date is a
weekday or weekend day.


```r
validWeekday <- function(days) {
  ifelse(weekdays(days) == "Saturday" | weekdays(days) == "Sunday", 
         "weekend", "weekday")
}
filedata_imp$week <- as.factor(sapply(filedata_imp$date, validWeekday))
head(filedata_imp)
```

```
##       steps       date interval    week
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```
Next, make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged
across all weekday days or weekend days (y-axis).


```r
library(lattice)
filedata_week <- aggregate(steps ~ week + interval, filedata_imp, mean)
xyplot(steps ~ interval | factor(week), layout = cbind(1, 2), type = "l",
       lty = 1, xlab = "Interval", ylab = "Number of steps", filedata_week)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Based on the panel plot, it appears that people are more active earlier in
the day during weekdays than weekends, but they are more active throughout 
the rest of the weekends than weekdays.
