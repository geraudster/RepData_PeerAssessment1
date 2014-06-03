# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Load the data from the zip file:

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

Use the date format for the date column:

```r
data$date <- as.Date(data$date, format="%Y-%m-%d")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
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

Here is some stats on the dataset:

```r
sum(is.na(data$steps))/length(data$steps)
```

```
## [1] 0.1311
```

## What is mean total number of steps taken per day?

Let's plot the total number of steps:

```r
library(ggplot2)
library(plyr)
meanByDay <- ddply(data, .(date), summarize, sum = sum(steps), mean = mean(steps, na.rm=TRUE), median = median(steps, na.rm=TRUE))
ggplot(data=meanByDay, aes(x=date)) + geom_histogram(aes(y=sum), stat="identity", fill = "blue")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
ggplot(data=meanByDay, aes(x=date)) + geom_histogram(aes(y=mean), stat="identity", fill = "blue")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```r
ggplot(data=meanByDay, aes(x=date)) + geom_histogram(aes(y=median), stat="identity", fill = "blue")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) 



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
