setwd("/Users/elissachasen/Google Drive/coursera/reproducible research/project 1/")

unzip("activity.zip")
act <- read.csv("activity.csv")
str(act)

#What is mean total number of steps taken per day?
#ignore the missing values in the dataset.

#Calculate the total number of steps taken per day
steps_per_day <- act %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps),
                mean_steps = mean(steps, na.rm = TRUE),
                med_steps = median(steps, na.rm = TRUE)
        )
steps_per_day$total_steps        

#Make a histogram of the total number of steps taken each day
hist(steps_per_day$total_steps)

#Calculate and report the mean and median of the total number of steps taken per day
steps_per_day$mean_steps
steps_per_day$med_steps
#or 
mn <- tapply(act$steps, act$date, mean)
med <- tapply(act$steps, act$date, median)

#What is the average daily activity pattern?

# Make a time series plot of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all
# days (y-axis)
act$interval.factor <- as.factor(act$interval)
with(act, plot(interval, steps, type = "l"))
by_interval <- act %>%
        group_by(interval) %>%
        summarise(mn_stp = mean(steps, na.rm = TRUE))

with(by_interval, plot(interval, mn_stp, type = "l"))

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
which.max(by_interval$mn_stp)
