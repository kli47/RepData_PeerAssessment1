---
title: Reproducible Research Course Project 1 - kli47
---

## Loading and preprocessing the data  

Show any code that is needed to

### 1. Load the data


```r
unzip("activity.zip")
dataset <- read.csv("activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(tidyr)
dataset$date <- as.Date(dataset$date)
z <- dataset %>% drop_na()
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

### 1. Calculate the total number of steps taken per day


```r
library(dplyr)
```
Table of total number of steps per day

```r
y <- z %>% group_by(date) %>% mutate(total.steps = sum(steps)) %>% select(date, total.steps) %>% distinct %>% print(n = Inf)
```

```
## # A tibble: 53 x 2
## # Groups:   date [53]
##    date       total.steps
##    <date>           <int>
##  1 2012-10-02         126
##  2 2012-10-03       11352
##  3 2012-10-04       12116
##  4 2012-10-05       13294
##  5 2012-10-06       15420
##  6 2012-10-07       11015
##  7 2012-10-09       12811
##  8 2012-10-10        9900
##  9 2012-10-11       10304
## 10 2012-10-12       17382
## 11 2012-10-13       12426
## 12 2012-10-14       15098
## 13 2012-10-15       10139
## 14 2012-10-16       15084
## 15 2012-10-17       13452
## 16 2012-10-18       10056
## 17 2012-10-19       11829
## 18 2012-10-20       10395
## 19 2012-10-21        8821
## 20 2012-10-22       13460
## 21 2012-10-23        8918
## 22 2012-10-24        8355
## 23 2012-10-25        2492
## 24 2012-10-26        6778
## 25 2012-10-27       10119
## 26 2012-10-28       11458
## 27 2012-10-29        5018
## 28 2012-10-30        9819
## 29 2012-10-31       15414
## 30 2012-11-02       10600
## 31 2012-11-03       10571
## 32 2012-11-05       10439
## 33 2012-11-06        8334
## 34 2012-11-07       12883
## 35 2012-11-08        3219
## 36 2012-11-11       12608
## 37 2012-11-12       10765
## 38 2012-11-13        7336
## 39 2012-11-15          41
## 40 2012-11-16        5441
## 41 2012-11-17       14339
## 42 2012-11-18       15110
## 43 2012-11-19        8841
## 44 2012-11-20        4472
## 45 2012-11-21       12787
## 46 2012-11-22       20427
## 47 2012-11-23       21194
## 48 2012-11-24       14478
## 49 2012-11-25       11834
## 50 2012-11-26       11162
## 51 2012-11-27       13646
## 52 2012-11-28       10183
## 53 2012-11-29        7047
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
plot1 <- ggplot(y, aes(x = date,  y = total.steps)) + geom_bar(stat = "identity", fill="steelblue") + labs(title="Histogram of Total Steps (Excluding Missing Values)", x = "Date", y = "Total Steps") + theme_minimal()
plot1
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

### 3. Calculate and report the mean and median of the total number of steps taken per day

Utilizing the summary feature.


```r
j <- summary(y)
j
```

```
##       date             total.steps   
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```
The mean amount of total steps is 10766
The median amount of total steps is 10765

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
a <- z %>% group_by(interval) %>% summarize(average.steps = mean(steps))

ggplot(a, aes(x=interval, y=average.steps)) + geom_line() + xlab("5-Minute Intervals") + ylab("Steps") + ggtitle("Average Steps Across All Days for Each 5-Minute Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
a[which.max(a$average.steps),]
```

```
## # A tibble: 1 x 2
##   interval average.steps
##      <int>         <dbl>
## 1      835          206.
```

```r
print(paste("The 5-minute interval with the maximum number of steps on average across all the days is the", paste(as.character(a[which.max(a$average.steps),1]), "th", sep=""), "interval"))
```

```
## [1] "The 5-minute interval with the maximum number of steps on average across all the days is the 835th interval"
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs). 


```r
print(paste("There are", (sum(is.na(dataset))), "missing values"))
```

```
## [1] "There are 2304 missing values"
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: Will fill in the missing values with the mean for that 5-minutes interval (as averaged across all the days), see variable "a" above. 

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
b <- a %>% rename(steps = average.steps)

c <- dataset %>% inner_join(b, by = "interval") %>% mutate(steps = coalesce(steps.x, steps.y)) %>% select(date, interval, steps)
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
d <- c %>% group_by(date) %>% mutate(total.steps = sum(steps)) %>% select(date, total.steps) %>% distinct

plot2 <- ggplot(d, aes(x = date,  y = total.steps)) + geom_bar(stat = "identity", fill="steelblue") + labs(title="Histogram of Total Steps (Missing Values Filled In)", x = "Date", y = "Total Steps") + theme_minimal()
plot2
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
Once again using the summary feature to calculate mean and median of total steps

```r
e <- summary(d)
```

Now comparing the two summaries
With missing values removed:

```r
j
```

```
##       date             total.steps   
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```
With missing values filled in:

```r
e
```

```
##       date             total.steps   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

The median and means do not differ grossly. There is a similar mean value and median value is only 1 step off.

We can compare the two graphs of total steps

```r
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 1)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)
  
Grossly, filling in the missing values added some bars in the October and November. Did not change the overal shape of our histogram.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
c$day <- factor((weekdays(c$date) %in% weekdays), levels = c(FALSE, TRUE), labels=c("Weekend", "Weekday"))
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
k <- c %>% group_by(day, interval) %>% summarize(average.steps = mean(steps))
```

```
## `summarise()` has grouped output by 'day'. You can override using the `.groups` argument.
```

```r
library(lattice)
xyplot(average.steps ~ interval | day, data = k, layout = c(1,2), type = "l", main = "Weekday vs. Weekend Total Steps per 5-Minute Interval", xlab = "5-Minute Intervals", ylab = "Steps")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)
