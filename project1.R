
library(ggplot2)
Sys.setlocale("LC_TIME", "English") 

fileUrl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName = "activitydata.zip"

if(!file.exists(fileName)){
        download.file(fileUrl, fileName, mode = "wb")    
}
unzip(fileName, exdir = "data")

activity = read.csv("data/activity.csv")

# mean total number of steps taken each day
totalStepsByDay = aggregate(steps ~ date, data = activity, FUN = sum)
qplot(totalStepsByDay$steps, binwidth=1000, ylab="Frequency", xlab="total steps each day")

mean(totalStepsByDay$steps, na.rm = TRUE)
median(totalStepsByDay$steps, na.rm = TRUE)

# average daily activity pattern
stepsByInterval <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(stepsByInterval$interval, stepsByInterval$steps, geom = "line", 
      xlab = "5 minute interval", ylab = "average number of steps")

stepsByInterval[which.max(stepsByInterval$steps),1]


# imputing missing values by using avg values for interval

sum(!complete.cases(activity))
imputed <- transform(activity, steps = ifelse(is.na(activity$steps),
        stepsByInterval$steps[match(activity$interval, stepsByInterval$interval)],
        activity$steps))

# hist, mean and median of imputed data
imputedStepsByDay <- aggregate (steps ~date, data = imputed, FUN = sum)
qplot(imputedStepsByDay$steps, binwidth=1000, ylab="Frequency", xlab="total steps each day")

mean(imputedStepsByDay$steps)
median(imputedStepsByDay$steps)

# differences in activity patterns between weekends and weekdays
weekendDays <- c("Saturday", "Sunday")
imputed$weekday <- as.factor(ifelse(is.element(weekdays(as.Date(imputed$date)),
        weekendDays),"Weekend", "Weekday" ))


imputedStepsByInterval <- aggregate(steps ~ interval + weekday,data = imputed,FUN = mean)

ggplot(imputedStepsByInterval, aes(interval, steps)) + geom_line() + facet_wrap(~weekday)+
        xlab("5 minute interval") +ylab("average number of steps")
