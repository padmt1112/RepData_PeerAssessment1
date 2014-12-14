---
title: 'Coursera - Reproducible Research: Peer Assignment 1'
author: "Padmanabhan T"
date: "Sunday, December 14, 2014"
output: html_document
---



###Loading and preprocessing the data:

1. Load data from the working directory
2. Preprocess the data (if required)

```{r}

setwd("C:/Data Scientist/Reproducible Results/Week 2/repdata-data-activity")

data <- read.csv("activity.csv")
```
###What is mean total number of steps taken per day?


1.Make a histogram of the total number of steps taken each day

```{r}

steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

```


2. Calculate and report the mean and median total number of steps taken per day
```{r}
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```
The mean is **`r rmean`** and the median is **`r rmedian`**

###What is the average daily activity pattern?


1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of steps per Day by Interval")

```


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}

max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is **`r max_interval`**.

###Imputing missing values


1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))

```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data.    

3.Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r}

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```


4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r}

steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)

```

**Calculate new mean and median for imputed data**


```{r}

rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```



**Calculate difference between imputed and non-imputed data**



```{r}

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

**Calculate total difference**


```{r}

total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)

```
The imputed data mean is **`r rmean.i`**  
The imputed data median is **`r rmedian.i`**  
The difference between the non-imputed mean and imputed mean is **`r mean_diff`**  
The difference between the non-imputed mean and imputed mean is **`r med_diff`**  
The difference between total number of steps between imputed and non-imputed data is **`r total_diff`**.   
**Thus, there were `r total_diff` more steps in the imputed data**.


###Are there differences in activity patterns between weekdays and weekends?


1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r}
library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

```
