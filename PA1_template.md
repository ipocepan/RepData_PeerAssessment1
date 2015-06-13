# Reproducible Research: Peer Assessment 1
The assignment is to write a report that answers the questions detailed below. 
Ultimately, the entire assignment needs to be completed in a single R markdown 
document that can be processed by *knitr* and transformed into an *HTML* file.

## Loading and preprocessing the data

```r
url <- "https://github.com/ipocepan/RepData_PeerAssessment1/blob/master/activity.zip"
download.file(url, destfile = "activity.zip", method = "curl")
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
activity_data <- read.csv("activity.csv", sep = ",", header = T, 
                          colClasses = c("numeric", "Date", "numeric"))
activity_data$wdayID <- format(activity_data$date, "%u")
activity_data$wday[activity_data$wdayID %in% c("6","7")] <- "weekend"
activity_data$wday[activity_data$wdayID %in% c("1","2","3","4","5")] <- "weekday"
head(activity_data)
```

```
##   steps       date interval wdayID    wday
## 1    NA 2012-10-01        0      1 weekday
## 2    NA 2012-10-01        5      1 weekday
## 3    NA 2012-10-01       10      1 weekday
## 4    NA 2012-10-01       15      1 weekday
## 5    NA 2012-10-01       20      1 weekday
## 6    NA 2012-10-01       25      1 weekday
```

```r
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  $ wdayID  : chr  "1" "1" "1" "1" ...
##  $ wday    : chr  "weekday" "weekday" "weekday" "weekday" ...
```

## What is mean total number of steps taken per day?
Calculate the total number of steps per day:

```r
library(ggplot2)
steps_data <- aggregate(steps ~ date, data=activity_data, FUN = sum, na.rm = T)

ggplot(data = steps_data, aes(steps_data$steps)) +
       geom_histogram(binwidth = 1000, fill = "red3", 
                      col = "darkred", alpha = 0.8) +
       geom_vline(aes(xintercept = mean(steps_data$steps)),
                  color = "cadetblue", linetype = "dashed", size = 1) +
       labs(title = "Total number of steps per day",
            x = "Number of steps", y = "Frequency") + 
       scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
       scale_x_continuous(breaks = seq(0, 22000, by = 2000))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(steps_data$steps)
```

```
## [1] 10766.19
```

```r
median(steps_data$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
library(ggplot2)
mean_steps_data <- aggregate(steps ~ interval, data = activity_data, FUN = mean)
max_int <- mean_steps_data[which.max(mean_steps_data$steps),]$interval

ggplot(data = mean_steps_data, aes(x = interval, y = steps)) +
        geom_line(aes(group = 1, col = steps), size = 0.8) +
        geom_vline(aes(xintercept = max_int),
               color = "red3", linetype = "dashed", size = 1) +
        labs(title = "Average number of steps per 5-min interval",
             x = "Interval ID",
             y = "Average number of steps") + 
        scale_x_continuous(breaks = seq(0, 2355, by = 250)) + 
        scale_y_continuous(breaks = seq(0, 210, by = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
dim(activity_data[!complete.cases(activity_data),])
```

```
## [1] 2304    5
```

```r
library(plyr)
imputed_data <- ddply(activity_data, .(interval), mutate, new_steps=median(steps, na.rm=T))
imputed_data$steps[is.na(imputed_data$steps)] <- imputed_data$new_steps[is.na(imputed_data$steps)]

steps_data <- aggregate(steps ~ date, data = imputed_data, FUN=sum)

ggplot(data = steps_data, aes(steps_data$steps)) +
       geom_histogram(binwidth = 1000, fill = "red3", 
                      col = "darkred", alpha = 0.8) +
       geom_vline(aes(xintercept = mean(steps_data$steps)),
                  color = "cadetblue", linetype = "dashed", size = 1) +
       labs(title = "Total number of steps per day",
            x = "Number of steps", y = "Frequency") + 
       scale_y_continuous(breaks = seq(0, 10, by = 1)) + 
       scale_x_continuous(breaks = seq(0, 22000, by = 2000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean(steps_data$steps)
```

```
## [1] 9503.869
```

```r
median(steps_data$steps)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?

```r
imputed_data$wday <- as.factor(imputed_data$wday) 
str(imputed_data)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ steps    : num  0 0 0 47 0 0 0 0 0 34 ...
##  $ date     : Date, format: "2012-10-01" "2012-10-02" ...
##  $ interval : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ wdayID   : chr  "1" "2" "3" "4" ...
##  $ wday     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 2 2 1 1 1 ...
##  $ new_steps: num  0 0 0 0 0 0 0 0 0 0 ...
```

```r
library(ggplot2)
mean_data_wday <- aggregate(steps ~ interval + wday, data = imputed_data, FUN = mean)

ggplot(data = mean_data_wday, aes(x = interval, y = steps)) +
        geom_line(aes(group = 1, col = steps), size = 0.8) +
        facet_grid(wday ~ .) +
        labs(title = "Average number of steps per 5-min interval",
             x = "Interval ID",
             y = "Average number of steps") + 
        scale_x_continuous(breaks = seq(0, 2355, by = 250)) + 
        scale_y_continuous(breaks = seq(0, 210, by = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
