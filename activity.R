# |*****************************************************************************
# | Dwayne Macadangdang 9/14/2016
# | Coursera: Exploratory Data Analysis
# | Week 2 Programming Assignment 1

activity <- function()
{
  library(dplyr)
  library(lattice)
  
  setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Repro_Res/")
  
  # read data from file
  activity.file.data <- read.csv("activity.csv")
  
  # calc distrubtion of steps
  steps.data <- aggregate(steps ~ date, activity.file.data, sum)
  # hist(steps.data$steps, main = paste("Distribution: Total Steps Per Day"), col = "purple", xlab = "Number of Steps")
  
  #histogram with ggplot
  histogram.steps.data <- ggplot(data = steps.data, aes(steps.data$steps)) 
  histogram.steps.data + geom_histogram(color = "black", fill = "pink", bins = 10, binwidth = 6000) + labs(x = "Steps", y = "Count", title = "Distribution: Total Steps")
  # histogram.steps.data + geom_histogram(color = "purple", bins = 10, binwidth = 6000) + labs(x = "Steps", y = "Count", title = "Distribution: Total Steps")
  
  # mean & median
  mean.steps.data <- mean(steps.data$steps)
  median.steps.data <- median(steps.data$steps)
  
  # calc mean of steps/interval
  interval.steps.data <- aggregate(steps ~ interval, activity.file.data, mean)
  # plot(interval.steps.data$interval, interval.steps.data$steps, type = "l", col = "orange", xlab = "Interval", ylab = "Number of Steps", main = "Average Number of Steps per Day by Interval")
  
  # alternative histogram using ggplot
   graph.data <- ggplot(interval.steps.data, aes(interval, steps))
   graph.data + geom_line(color = "purple") + labs(x = "Interval", y = "Steps", title = "Steps Per Interval")
  # g + geom_point() + facet_grid(drv ~ cyl, margins = TRUE) + geom_smooth(method = "lm", se = FALSE, size = 2, color = "black") + labs(x = "Displacement", y = "Highway Mileage", title = "Swirl Rules!")
  
  # max interval
  max.interval.steps.data <- interval.steps.data[which.max(interval.steps.data$steps), 1]
  
  # missing values
  incomplete.data <- sum(!complete.cases(activity.file.data))
  # testing! imputed.data <- imputed.data[!complete.cases(activity.file.data), ]
  imputed.data <- transform(activity.file.data, steps = ifelse(is.na(activity.file.data$steps), interval.steps.data$steps[match(activity.file.data$interval, interval.steps.data$interval)], activity.file.data$steps))

  imputed.data[as.character(imputed.data$date) == "2012-10-01", 1] <- 0
  
  imputed.steps.data <- aggregate(steps ~ date, imputed.data, sum)
  hist(imputed.steps.data$steps, main = paste("Total Steps Each Day"), col = "purple", xlab = "Number of Steps")
  
  hist(steps.data$steps, main = paste("Total Steps Each Day"), col = "orange", xlab = "Number of Steps", add = TRUE)
  legend("topright", c("Imputed", "Non-imputed"), col = c("purple", "orange"), lwd = 10)
  
  # imputed mean & median
  mean.imputed.steps.data <- mean(imputed.steps.data$steps)
  median.imputed.steps.data <- median(imputed.steps.data$steps)
  
  # difference
  mean.difference <- mean.imputed.steps.data - mean.steps.data
  median.difference <- median.imputed.steps.data - median.steps.data
  
  total.difference <- sum(imputed.steps.data$steps) - sum(steps.data$steps)
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  imputed.data$weekday = as.factor(ifelse(is.element(weekdays(as.Date(imputed.data$date)), weekdays), "Weekday", "Weekend"))
  
  imputed.steps.interval.data <- aggregate(steps ~ interval + weekday, imputed.data, mean)
  
  xyplot(imputed.steps.interval.data$steps ~ imputed.steps.interval.data$interval|imputed.steps.interval.data$weekday, main = "Average Steps per Day by Interval", xlab = "Interval", ylab = "Steps", layout = c(1,2), type = "l")
  
  
  }
