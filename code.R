setwd("/Users/elissachasen/Google Drive/coursera/reproducible research/project 1/")

unzip("activity.zip")
act <- read.csv("activity.csv")
str(act)
library(dplyr)
library(ggplot2)
library(cowplot)
library(timeDate)

#What is mean total number of steps taken per day?
#ignore the missing values in the dataset.

#Calculate the total number of steps taken per day
steps_per_day <- act %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm = TRUE),
                mean_steps = mean(steps, na.rm = TRUE),
                med_steps = median(steps, na.rm = TRUE)
        )

steps_per_day$total_steps        

#Make a histogram of the total number of steps taken each day
hist(steps_per_day$total_steps, breaks = 20,
     xlab = "Total steps per day", main = "Histogram of total steps per day")

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

by_interval <- act %>%
        group_by(interval) %>%
        summarise(mn_stp = mean(steps, na.rm = TRUE))

with(by_interval, plot(interval, mn_stp, type = "l", 
                       ylab = "Average number of steps",
                       xlab = "Interval of day", 
                       main = "Average no. of steps across all days by 5-minute interval"))

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
which.max(by_interval$mn_stp)

#Imputing missing values

#Calculate and report the total number of missing values in the dataset
sum(is.na(act))

#Devise a strategy for filling in all of the missing values in the dataset. 
# use the mean for that 5-minute interval.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
#replacer <- function(data, col1, col2){
  #print(mn_col <- paste0(data$col1, data$col2, mean, na.rm = TRUE))
  #data$col1 <- replace(data$col1, is.na(data$col1), mn_col)
  #newData <<- data.frame(data)
#}
#replace("act", "steps", "interval")


mn_int <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
act$step.narm <- replace(act$steps, is.na(act$steps), mn_int)


# Make a histogram of the total number of steps taken each day and 

steps_per_day_narm <- act %>%
  group_by(date) %>%
  summarize(total_steps = sum(step.narm),
            mean_steps = mean(step.narm),
            med_steps = median(step.narm)
  )

par(mfrow = c(2, 1))
hist(steps_per_day_narm$total_steps, breaks = 20, 
     xlab = "Steps per day (removed NAs)",
     main = "Histogram with NAs imputed by mean per interval")
hist(steps_per_day$total_steps, breaks = 20,
     xlab = "Steps per day (including NAs)",
     main = "Historgram includes NAs")

# Calculate and report the mean and median total number of steps taken per day. 
steps_per_day_narm$mean_steps
steps_per_day_narm$med_steps

# Do these values differ from the estimates from the first part of the assignment? 
summary(steps_per_day$mean_steps)
summary(steps_per_day_narm$mean_steps)

summary(steps_per_day$med_steps)
summary(steps_per_day_narm$med_steps)

# What is the impact of imputing missing data on the estimates of the total daily number of steps?
p1 <- ggplot(steps_per_day, aes(as.numeric(date), mean_steps)) +
    geom_line() +
    labs(x = "Day", y = "Mean number of steps", title = "Avg steps per day (including NAs)")

p2 <- ggplot(steps_per_day_narm, aes(as.numeric(date), mean_steps)) +
    geom_line() + 
    labs(x = "Day", y = "Mean number of steps", title = "Avg steps per day (NAs imputed)")

p3 <- ggplot(steps_per_day, aes(as.numeric(date), med_steps)) +
    geom_line() +
    labs(x = "Day", y = "Median number of steps", title = "Median steps per day (including NAs)")

p4 <- ggplot(steps_per_day_narm, aes(as.numeric(date), med_steps)) +
    geom_line() +
    labs(x = "Day", y = "Median number of steps", title = "Median steps per day (NAs imputed)")

plot_grid(p1, p2, p3, p4, ncol = 2)

# Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.
act$date <- as.Date(act$date)  # convert date variable to data format
act$day <- weekdays(act$date)

# Create a new factor variable in the dataset with two levels -
# "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

wkends <- act %>%
  filter(day == "Sunday"| day == "Saturday") %>%
  mutate(wkday = "weekend")

wkdys <- act %>%
  filter(day == "Monday"| day == "Tuesday" | day == "Wednesday" | 
           day == "Thursday" | day == "Friday") %>%
  mutate(wkday = "weekday")

all_dat <- bind_rows(wkends, wkdys) %>%
        arrange(date)

# Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 

by_int_wkdy <- wkdys %>%
  group_by(interval) %>%
  summarise(mn_stp = mean(step.narm))

by_int_wkend <- wkends %>%
  group_by(interval) %>%
  summarise(mn_stp = mean(step.narm))

par(mfrow = c(2, 1))

with(by_int_wkdy, plot(interval, mn_stp, type = "l", 
                       ylab = "Average number of steps",
                       xlab = "Interval of day", 
                       main = "Average no. of steps across weekdays by 5-minute interval"))
with(by_int_wkend, plot(interval, mn_stp, type = "l", 
                       ylab = "Average number of steps",
                       xlab = "Interval of day", 
                       main = "Average no. of steps across weekend by 5-minute interval"))
