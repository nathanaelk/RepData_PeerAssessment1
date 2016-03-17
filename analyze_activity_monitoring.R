
dataUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
zipFile <- 'activity.zip'
csvFile <- 'activity.csv'

# Download the file only if necessary
if ((file.exists(zipFile) == FALSE) & (file.exists(csvFile) == FALSE)) {
  download.file(dataUrl, zipFile)
}

# Unzip the zip file only if necessary.
if (file.exists(csvFile) == FALSE){
  unzip(zipFile)
}

# Load data from file
activityData <- read.csv(csvFile, na.strings = 'NA')

# Get the list of all the unique dates the data were collected
alldays <- unique(activityData$date)

total_steps <- c()

# Build a list of total steps per day
for (day in alldays) {
  activityDataPerDay <- subset(activityData, date == day)
  total_steps <- c(total_steps, sum(activityDataPerDay$steps))
}

# Create an histogram in a png file for the total steps per day
png(filename = 'histogram_total_steps_per_day.png')
plot(as.Date(alldays), total_steps, type = 'h', xlab='Days', ylab='Total steps',
     main='Total steps per day')
dev.off()

print('Open average_steps_per_day.png for time series plot of average number of steps taken')

index_max_number_of_steps <- which(activityData$steps == max(activityData$steps, na.rm = TRUE))
print('The 5 min interval with the maximum number of steps is:')
print(activityData$interval[index_max_number_of_steps])

# Impute missing data (NA)
# Since missing data are NA, we want to subset all the data where the steps is not NA
clean_activityData <- subset(activityData, is.na(activityData$steps) == FALSE)

# Refresh the list of all the unique dates the data were collected, with missing data removed
alldays <- unique(clean_activityData$date)

total_steps <- c()
mean_steps <- c()
median_steps <- c()

# Build 3 vectors of respectively mean, median and total steps per day, with missing data removed
for (day in alldays) {
  activityDataPerDay <- subset(clean_activityData, date == day)
  mean_steps <- c(mean_steps, mean(activityDataPerDay$steps))
  median_steps <- c(median_steps, median(activityDataPerDay$steps))
  total_steps <- c(total_steps, sum(activityDataPerDay$steps))
}

mean_and_mediandf <- data.frame('Mean_Steps'= mean_steps, 'Median_Steps'= median_steps, 'date' = unique(clean_activityData$date))
mean_and_mediandf$date <- as.Date(mean_and_mediandf$date)

print('Mean and median steps per day:')
print(mean_and_mediandf)

# Plot into a png file the average steps per day and the median steps per day
png(filename = 'average_steps_per_day.png')
with(mean_and_mediandf, plot(date, Mean_Steps, type = 'l', xlab = 'Days', ylab = 'Steps'))
points(mean_and_mediandf$Median_Steps, type = 'l', col='red')
dev.off()

# Create an histogram in a png file for the total steps per day
png(filename = 'histogram_total_steps_per_day_without_missing_data.png')
with(mean_and_mediandf, plot(date, total_steps, type = 'h', xlab = 'Days', ylab = 'Total steps',
                             main = 'Total steps per day (missing values ignored)'))
dev.off()

# Define a function that returns 'weekend' when the date is a weekend (Sunday or Saturday);
# otherwise 'weekday'

convert_to_weekday_weekend <- function(d){
  if (weekdays(d) == 'Sunday' | weekdays(d) == 'Saturday'){
    return ('weekend')
  } else {
    return ('weekday')
  }
}

# Create a vector that says the type of day (weekday or weekend) and add that vector as a column of
# the data frame
typeOfDay <- lapply(clean_activityData$date, convert_to_weekday_weekend)

library('dplyr')
clean_activityData <- mutate(clean_activityData, dayType = typeOfDay)

print(head(clean_activityData), 10)

# Get the list of all unique intervals  
allintervals <- unique(clean_activityData$interval)
average_steps_weekday <- c()
average_steps_weekend <- c()

# Calculate the total average of steps per interval per type of day
for (curr_interval in allintervals) {
  activityDataPerIntervalOnWeekday <- subset(clean_activityData, interval == curr_interval & dayType == 'weekday')
  activityDataPerIntervalOnWeekend <- subset(clean_activityData, interval == curr_interval & dayType != 'weekend')
  average_steps_weekday <- c(average_steps_weekday, mean(activityDataPerIntervalOnWeekday$steps))
  average_steps_weekend <- c(average_steps_weekend, mean(activityDataPerIntervalOnWeekend$steps))
}

# Plot the average steps per interval on the weekdays and on the weekends
png(filename = 'average_steps_per_interval_pertypeofDay.png')
plot(allintervals, average_steps_weekday, type = 'l', xlab = 'Intervals', ylab = 'Average Steps', col = 'blue',
     main = 'Average steps per intervals')
points(average_steps_weekend, type = 'l', col='red')
legend('topright', pch=c('-','-'), lty=c(1,1), col=c('blue', 'red'), 
       legend=c('Weekdays', 'Weekends'))
dev.off()
