# Clean workspace
rm(list=ls())
# load library 
library (lattice)
library(zoo)
# download the zip file and unzip
file.url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(file.url,destfile='activity.data.zip')
activity.datafile <- unzip('activity.data.zip',overwrite=TRUE)
activity.data <- read.csv(activity.datafile, sep=",")
activity.data$date <- as.Date(activity.data$date,"%Y-%m-%d")
head(activity.data,n=5)

#the dataset includes three variable:
#1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
#2. date: The date on which the measurement was taken in YYYY-MM-DD format
#3. interval: Identifier for the 5-minute interval in which measurement was taken?


# Calculate the total number of steps taken per day
totalnumberofstepperday <- tapply(activity.data$steps,activity.data$date,sum)
# Histogram of the total number of steps per day
hist(totalnumberofstepperday,main = paste("Histogram of total number of steps per day"),xlab="Number of steps per day")
# Calculate the mean and median of the total number of steps taken per day
totalnumberofstepperday.mean <- mean(totalnumberofstepperday,na.rm = TRUE)
totalnumberofstepperday.median <- median(totalnumberofstepperday,na.rm = TRUE)

#What is the average daily activity pattern?
library(zoo)
average <- tapply(activity.data$steps,activity.data$interval,mean,na.rm = TRUE)
interval <- activity.data$interval
interval.average <- zoo(average,interval)
plot(interval.average,type='l', xlab="5-minute interval", ylab="average number of steps across all days ", main = "average number of steps taken across all days per 5-minute interval", col='red')

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
names(which.max(interval.average))

# Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalnumberofNA <- sum(is.na(activity.data$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

modified.activity.data <- activity.data # a new dataset that is equal to activity.data but the missing data will be filled in
# Here, I am replacing the missing values with the mean for the corresponding 5-minute interval
colnames <- names(average)
for (i in 1:nrow(modified.activity.data) )
{
  if (is.na(modified.activity.data$steps[i]))
  {modified.activity.data$steps[i] = average[[colnames=as.character(modified.activity.data$interval[i])]]}
}

# Calculate the total number of steps taken per day for the modified dataset
modified.totalnumberofstepperday <- tapply(modified.activity.data$steps,(modified.activity.data$date),sum)
# Histogram of the total number of steps per day
hist(modified.totalnumberofstepperday,main = paste("Histogram of total number of steps per day"),xlab="Number of steps per day")
# Calculate the mean and median of the total number of steps taken per day
modified.totalnumberofstepperday.mean <- mean(modified.totalnumberofstepperday,na.rm = TRUE)
modified.totalnumberofstepperday.median <- median(modified.totalnumberofstepperday,na.rm = TRUE)
modified.totalnumberofstepperday.mean- totalnumberofstepperday.mean
modified.totalnumberofstepperday.median- totalnumberofstepperday.median


# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
daytype <- weekdays(as.Date(modified.activity.data$date))
 for (i in 1:nrow(modified.activity.data))
 {
 if (daytype[i]=="Saturday" | daytype[i]=="Sunday") 
   {daytype[i]="weekend"}
   else {daytype[i] = "weekday"}
 }
modified.activity.data$daytype=factor(daytype)

# Are there differences in activity patterns between weekdays and weekends?
stepsperday <- aggregate(steps ~ interval + daytype, data = modified.activity.data, mean)
names(stepsperday) <- c("interval", "daytype", "steps")
xyplot(steps ~ interval | daytype, stepsperday, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
