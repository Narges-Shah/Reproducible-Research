library(ggplot2)
library(scales)

### Loading and preprocessing the data
setwd("~/Desktop/Data Science/Reproducible Research/RepData_PeerAssessment1")
if(!file.exists('activity.csv'))
  unzip('activity.zip')

data <- read.csv('activity.csv')

### What is mean total number of steps taken per day?
totalSteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(totalSteps, binwidth = 1000, xlab="Total number of steps taken every day")

meanSteps <- mean(totalSteps, na.rm = TRUE)
medianSteps <- median(totalSteps, na.rm = TRUE)

### What is the average daily activity pattern?
averageSteps <- aggregate(x=list(steps = data$steps), by = list(interval = data$interval), FUN = mean, na.rm = TRUE)
ggplot(data = averageSteps, aes(x = interval, y = steps)) +
                                                  geom_line() +
                                                  xlab("Every 5-MIN") +
                                                  ylab("Average Number of Steps")

averageSteps[which.max(averageSteps$steps),]

### Imputing missing values
missing <- is.na(data$steps)
table(missing)

newData <- data
newData <- transform(data, steps = ifelse(is.na(data$steps), averageSteps$steps[match(data$interval, averageSteps$interval)], data$steps))

totalSteps <- tapply(newData$steps, newData$date, FUN=sum)
qplot(totalSteps, binwidth=1000, xlab = "Total number of steps taken each day")
mean(totalSteps)
median(totalSteps)

### Are there differences in activity patterns between weekdays and weekends?
weekdayFactor <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
newData$date <- as.Date(newData$date)
newData$day <- sapply(newData$date, FUN = weekdayFactor)

averageSteps <- aggregate(steps ~ interval + day, data=newData, mean)
ggplot(averageSteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
