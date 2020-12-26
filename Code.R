## Loading and preprocessing data.
## Using lubridate to convert class of the date variable.

library(lubridate)
activity <- read.csv("activity.csv")
activity <- transform(activity, date = ymd(date))

## Total steps per day is calculated using tapply().
## Histogram is plotted using barplot() but answers the question in hand.

totalspd <- tapply(activity$steps, activity$date, sum)
barplot(totalspd, col = "yellow", xlab = "Date", ylab = "Total steps per day")
title("Total number of steps taken each day")

## Mean and median is calculated and reported.

meanspd <- mean(totalspd, na.rm = TRUE)
medianspd <- median(totalspd, na.rm = TRUE)
print(meanspd)
print(medianspd)

## The dataset is first organized such that the average number of steps averaged across all days is included in the dataset. 
## It is done so by using the dplyr pipeline

## Then the time series plot is created using the ggplot by using the processed dataset.

library(dplyr)
library(ggplot2)
avg <- activity %>% group_by(interval) %>% 
          summarise(avg = mean(steps, na.rm = TRUE))
plot1 <- ggplot(avg) + 
          geom_line(aes(interval,avg), color = "blue", size = 1, alpha = 0.5) +
          ggtitle("Average daily activity pattern") + xlab("Interval") + 
          ylab("Average number of steps")
print(plot1)

## The 5 min interval that has the maximum average number of steps avaraged across all days is found by using grep.

avg[grep(max(avg$avg), avg$avg),]

## The total number of NA values is calculated by using the simple code.

sum(as.numeric(is.na(activity$steps)))

## An object is created which contains all dates having missing values. 
## (It can be easily deduced that the values are missing for particular dates).

library(lubridate)
na <- totalspd[is.na(totalspd)]
naa <- names(na)
naa <- ymd(naa)
avg2 <- avg
names(avg2)[2] <- "steps"

## The object is matched such that only those dates which has missing values will be imputed with the average number of steps averaged across all days.
## All the dates with missing values are looped and the average values are merged with the dataset.
## A new dataset is used because each merge loop will delete the previous merge made.

i = 1
while (i <= length(naa)) {
  k = 1
  avg2$date <- naa[i]
  act <- merge(activity, avg2, by = c("date","interval"), all.x = TRUE)
  if (i==1) {
    act2 <- act
    act2$steps <- na
  }
  for (j in act$steps.x) {
    if (is.na(act2$steps[k]) == FALSE) {
      k <- k+1
      next
    }
    if (is.na(j) == TRUE) {
      act2$steps[k] <- act$steps.y[k]
    }
    else if (is.na(j) == FALSE){
      act2$steps[k] <- act$steps.x[k]
    }
    k <- k+1
  }
  i = i+1
}

## Processing is done such that it resembles the original dataset with missing values filled.

newactivity <- select(act2, steps, date, interval)

## Total number of steps per each day is calculated and the histogram is plotted for the new dataset.

totalspd2 <- tapply(newactivity$steps, newactivity$date, sum)
barplot(totalspd2, col = "orange", xlab = "Date", ylab = "Total steps per day")
title("Total number of steps taken each day \n after imputing missing values")

## Mean and median calculation.

meanspd2 <- mean(totalspd2)
medianspd2 <- median(totalspd2)

## Dataset factored into weekdays and weekends using a loop.

l <- 1
while (l <= length(newactivity$date)) {
  if (weekdays(newactivity$date[l]) == "Sunday") {
    newactivity$factor[l] <- "weekend"
  }
  else {
    newactivity$factor[l] <- "weekday"
  }
  l <- l+1
}

newactivity <- transform(newactivity, factor = factor(factor))

## Organizing the dataset for average calculation using dyplr pipeline.

library(dplyr)
library(ggplot2)
newavg <- newactivity %>% group_by(interval, factor) %>% 
            summarise(avg = mean(steps, na.rm = TRUE))

## Plotting a panel plot for the new dataset that is factored accordingly.

plot2 <- ggplot(newavg) + 
          geom_line(aes(interval,avg, color = factor), size = 1, alpha = 0.5) + 
          facet_grid(factor ~ .) +
          ggtitle("Activity patterns between weekdays and weekends") + 
          xlab("Interval") + ylab("Average number of steps")
print(plot2)
