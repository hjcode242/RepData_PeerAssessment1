# Reproducable Research: Assignment 1

##Loading the data \##### Load the data and dependencies

``` r
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)

fulldata <- read.csv("activity.csv")
```

------------------------------------------------------------------------

##What is the mean total number of steps taken per day?

##### Calculate the steps per day

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
dailysteps <- tapply(fulldata$steps, fulldata$date, sum, na.rm = TRUE)
```

##### Create a histogram

``` r
library(ggplot2)

qplot(dailysteps, xlab = 'Total number of steps per day', ylab = 'Frequency using binwidth 500')
```

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

##### Mean and median calculations

``` r
mean_steps <- mean(dailysteps, na.rm = TRUE)

## [1] 9354.23
```

``` r
median_steps <- median(dailysteps, na.rm = TRUE)

##[1] 10395
```

## What is the average daily pattern?

``` r
avgdailypattern <- aggregate(x=list(steps = fulldata$steps), by =  list(interval = fulldata$interval), FUN = mean, na.rm = TRUE)

ggplot(data = avgdailypattern, aes(x = interval, y = steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

##### On average across all of the days in the dataset, the 5 minute interval contains the maximum number of steps?

``` r
maxsteps <- filter(avgdailypattern, steps == max(steps))
```

``` r
##     interval    steps
##      835    206.1698
```

##Imputing missing values

``` r
missing_values <- sum(is.na(fulldata$steps))

##[1] 2304
```

``` r
fulldataImputed <- fulldata


mean_steps <- mean(fulldata$steps, na.rm = TRUE)


fulldataImputed$steps[is.na(fulldataImputed$steps)] <- mean_steps
```

``` r
steps_imputed_by_day <- tapply(fulldataImputed$steps, fulldataImputed$date, sum)

qplot(steps_imputed_by_day, xlab = 'Total Num. Imputed Steps', ylab = 'Frequency Using Binwith 500', binwidth = 500)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
meanImputed <- mean(steps_imputed_by_day)

medianImputed <- median(steps_imputed_by_day)

##[1] 10766.19
##[1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
fulldataImputed <- fulldata


fulldataImputed$date <- as.POSIXlt(fulldataImputed$date)

fulldataImputed$dateType <- ifelse(weekdays(fulldataImputed$date) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')
```

``` r
averagedfulldataImputed <- aggregate(steps ~ interval + dateType, data = fulldataImputed, mean)

ggplot(averagedfulldataImputed, aes(interval, steps)) +
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-14-1.png)
