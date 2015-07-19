# Reproducible Research: Peer Assessment 1

### Enable Code Reading and Load Libraries

```r
echo = TRUE
library(ggplot2)
```

### Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv",header = TRUE,colClasses = c("integer","Date","factor"))
#Remove missing values
data_noNA = na.omit(data)
```


### What is mean total number of steps taken per day?

- Make a histogram of the total number of steps taken each day

```r
ggplot(data_noNA, aes(date, steps)) + geom_bar(stat = "identity", colour = "yellow", fill = "red") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalSteps <- aggregate(data_noNA$steps, list(Date = data_noNA$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

- Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days


```r
average_steps <- aggregate(data_noNA$steps, list(interval = as.numeric(as.character(data_noNA$interval))), FUN = "mean")
names(average_steps)[2] <- "mean_steps"

ggplot(average_steps, aes(interval, mean_steps)) + geom_line(color = "black") + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

- 5-minute interval with maximum number of steps


```r
average_steps[average_steps$mean_steps == max(average_steps$mean_steps), ]
```

```
##     interval mean_steps
## 104      835   206.1698
```

### Imputing missing values

- The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

- Filling in all of the missing values in the dataset. 

Using the mean for the 5-minute interval to fill NA values

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_NA <- data 
for (i in 1:nrow(data_NA)) {
    if (is.na(data_NA$steps[i])) {
        data_NA$steps[i] <- average_steps[which(data_NA$interval[i] == average_steps$interval), ]$mean_steps
    }
}
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(data_NA, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "yellow",
                                             fill = "red") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no NA)", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
total_steps_NA <- aggregate(data_NA$steps,list(Date = data_NA$date), FUN = "sum")$x
mean_NA <- mean(total_steps_NA)
mean_NA
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median_NA <- median(total_steps_NA)
median_NA
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
mean_noNA <- mean(totalSteps)
median_noNA <- median(totalSteps)
mean_NA - mean_noNA
```

```
## [1] 0
```

```r
median_NA - median_noNA
```

```
## [1] 1.188679
```

After going through the imputation technique, and substituting missing values with the mean of 5 minute interval NA values, we observe the following:
The new mean of total steps taken per day is the same as that of the original mean. 
The new median of total steps taken per day is greater than that of the original median.

### Are there differences in activity patterns between weekdays and weekends?

- Creating a factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data_NA$weekdays <- factor(format(data_NA$date, "%A"))
levels(data_NA$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(data_NA$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(data_NA$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(data_NA$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

- Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
average_steps <- aggregate(data_NA$steps, 
                      list(interval = as.numeric(as.character(data_NA$interval)), 
                           weekdays = data_NA$weekdays),
                      FUN = "mean")
names(average_steps)[3] <- "mean_steps"
library(lattice)
xyplot(average_steps$mean_steps ~ average_steps$interval | average_steps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 
