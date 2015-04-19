# Reproducible Research: Peer Assessment 1

To begin the project, I like to first load all the libraries that I use.


```r
suppressWarnings(suppressPackageStartupMessages(library(plyr)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(stringr)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(gridExtra)))
```


bit <- read.csv("C:/RFiles/Coursera/Reproducible_Research/activity.csv")
bit$date <- as.Date(bit$date, format = "%Y-%m-%d") 

new.bit <- filter(bit, steps != "NA") # get rid of rows where steps is NA


day.steps <- ddply(new.bit, "date", summarise, steps.per.day = sum(steps))


step.hist <- ggplot(day.steps, aes(x = steps.per.day))   # plot the blank frame

        step.hist + geom_histogram(bin=300, 
                                   fill ="white", 
                                   color = "black")

cat("The mean number of steps taken per day is ", mean(day.steps$steps.per.day), "\n")

cat("The median number of steps taken per day is ", median(day.steps$steps.per.day), "\n")


interval.steps.avg <- group_by(new.bit, interval) %>%
        summarise(avg.steps = mean(steps))
        
# time series plot
time.steps <- ggplot(interval.steps.avg, aes(x = interval))
        time.steps + geom_line(aes(y = avg.steps)) +
                     labs(x = "Interval", y = "Average steps per Interval") + 
                     ggtitle("Average Steps Per interval - Oct-1-2012 to Nov-30-2012")


max.avg <- filter(interval.steps.avg, avg.steps == max(avg.steps))  # 1 row with int ad max




missing.data <- sum(is.na(bit$steps))
cat("There are", missing.data, "rows with missing data", "\n")


# to fill in the missing data in bit, I will use the mean per interval, already determined in interval.steps.avg
# do this with mutate -make a 4th column

fix.bit <- mutate(bit, fix.steps = ifelse(!is.na(steps), steps, 
                                          interval.steps.avg$avg.steps))

fix.bit <- select(fix.bit, -steps) # get rid of the original steps data

fix.day.steps <- group_by(fix.bit, date) %>%
                          summarise(sum.steps = sum(fix.steps))
        
fix.hist <- ggplot(fix.day.steps, aes(x = sum.steps))   # plot the blank frame
fix.hist + geom_histogram(bin=300, 
                           fill ="white", 
                           color = "black")

cat("The mean number of steps taken per day using the imputed data is ", mean(fix.day.steps$sum.steps), "\n")

cat("The median number of steps taken per day using the imputed data is ", median(fix.day.steps$sum.steps), "\n")

# create a new factor in the data frame showing weekend or weekday

wk.new.bit <- mutate(fix.bit, day = weekdays(date))
wk.new.bit$day <- as.factor(wk.new.bit$day)

wk.new.bit <- mutate(wk.new.bit, day.type = ifelse(day == "Saturday"|day == "Sunday", "weekend", "weekday"))

#day %in% c("Saturday", "Sunday") # this is a good method for the or function above too

#creates the interval data for weekend and weekdays

week.day <- filter(wk.new.bit, day.type =="weekday") %>%
                        group_by(interval) %>%        
                        summarise(avg.steps = mean(fix.steps))

week.end <- filter(wk.new.bit, day.type =="weekend") %>%
                        group_by(interval) %>%        
                        summarise(avg.steps = mean(fix.steps))

plot.weekday <- ggplot(week.day, aes(x = interval)) +
                        geom_line(aes(y = avg.steps)) +
                        labs(x = "Interval", y = "Average steps per Interval") + 
                        ggtitle("Average Steps Per interval - Oct-1-2012 to Nov-30-2012 - On Weekdays")


plot.weekend <- ggplot(week.end, aes(x = interval)) +
                        geom_line(aes(y = avg.steps)) +
                        labs(x = "Interval", y = "Average steps per Interval") + 
                        ggtitle("Average Steps Per interval - Oct-1-2012 to Nov-30-2012 - On Weekends")

grid.arrange(plot.weekday, plot.weekend)

       
#grid.arrange(p1,p2,p3,p4, ncol = 2, main = "Main title")



## Loading and preprocessing the data



## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
