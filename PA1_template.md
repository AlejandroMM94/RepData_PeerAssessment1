---
title: "PA1_template"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course [web site](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

## Loading and preprocessing the data

Establishing the working directory and importing the CSV file 

```{r cars}
getwd()
unzip("./activity.zip", exdir = "./activity")
activity <- read.csv("./activity/activity.csv")
head(activity)
```

## What is mean total number of steps taken per day?

Making a histogram of the total number of steps taken each day

```{r}
activity_per_day <- aggregate(steps ~ date, data = activity, sum)
hist(activity_per_day$steps, main = "Total Steps per Day", xlab = "Number of Steps", ylim = c(0, 30), col = "red")
```

Calculating and reporting the mean and median of the total number of steps taken per day

```{r}
c(Median = median(activity_per_day$steps), Mean = mean(activity_per_day$steps))
```

## What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
activity_per_interval <- aggregate(steps ~ interval, data = activity, mean)
with(activity_per_interval, plot(interval, steps, type = "l", main = "Average Daily Activity Pattern", 
                                 xlab = "Average Number of Steps", ylab = "Number of Steps", col = "blue"))
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
max_interval <- activity_per_interval[order(-activity_per_interval$steps), ]
max_interval[1, ]
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset

```{r}
NA_values <-  table(is.na(activity))
NA_values[2]
```

Filling in all of the missing values in the new dataset with the mean of the steps

```{r}
activity_NA <- activity
activity_NA$steps[is.na(activity_NA$steps)] = mean(activity_NA$steps, na.rm=TRUE)
```

Making a histogram of the total number of steps taken each day in the new dataset

```{r}
activity_NA_per_day <- aggregate(steps ~ date, data = activity_NA, sum)
hist(activity_NA_per_day$steps, main = "Total Steps per Day", xlab = "Number of Steps", ylim = c(0, 40), col = "red")
```

Calculating and reporting the mean and median of the total number of steps taken per day in the new dataset

```{r}
c(Median = median(activity_NA_per_day$steps), Mean = mean(activity_NA_per_day$steps))
```

## Are there differences in activity patterns between weekdays and weekends?

Creating two new factors: weekdays and weekends

```{r}
activiyty_days <- activity_NA
activiyty_days$date <- as.Date(activiyty_days$date)
activiyty_days$week <- as.factor(weekdays(activiyty_days$date))
laboral_days <- c("lunes", "martes", "miércoles", "jueves", "viernes")
weekend_days <- c("sábado", "domingo")
levels(activiyty_days$week) <- list("Weekend"= weekend_days, "Weekday"= laboral_days)
head(activiyty_days)
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
activity_days_per_day <- aggregate(steps ~ interval + week, data = activiyty_days, mean)
library(lattice)
xyplot(activity_days_per_day$steps ~ activity_days_per_day$interval|activity_days_per_day$week, 
       main="Average Steps per Day by kind of week day",xlab="Interval", ylab="Number of Steps",layout=c(1,2), type="l")
```