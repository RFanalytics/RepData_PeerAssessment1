# Reproducible Research: Peer Assessment 1

### Since this is a learing assignment, I will explain my actions as the report goes along as well as develop a respectable looking report that answers the questions

To begin the project, I like to first load all the libraries that I use.


```r
suppressWarnings(suppressPackageStartupMessages(library(plyr)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(stringr)))
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
suppressWarnings(suppressPackageStartupMessages(library(gridExtra)))
```

## Preparation - Loading and preprocessing the data  

The following code is used to read in the activity data from the working directory in R.  The date information is read in as factor and is coerced to date format.  


```r
        bit <-      read.csv("C:/RFiles/Coursera/Reproducible_Research/activity.csv")
        bit$date <- as.Date(bit$date, format = "%Y-%m-%d")
```


The following code removes rows where the number of steps = NA to prepare the data for further analysis.  


```r
        new.bit <-   filter(bit, steps != "NA")
```

## Question #1 - What is mean total number of steps taken per day?  

1.      Calculate the total number of steps taken per day.  


```r
        day.steps <- ddply(new.bit, "date", summarise, steps.per.day = sum(steps))
```

2.      Make a histogram of the total number of steps taken each day.  
 

```r
        step.hist <- ggplot(day.steps, aes(x = steps.per.day))

        step.hist + geom_histogram(bin=300, 
                                   fill ="white", 
                                   color = "black")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

3.      Calculate and report the mean and median of the total number of steps taken per day.  

I have chosen to use cat to print the report below the code for this part.  It shows the code and result in the HTML output, albeit not as clean as it could be.  For questions 2 and 3 I will use the better method of embedding directly to in line R code - but you don't see the code in HTML unless you referece the markdown documents.


```r
        cat("The mean number of steps taken per day is ", mean(day.steps$steps.per.day), "\n")
```

```
## The mean number of steps taken per day is  10766.19
```

```r
        cat("The median number of steps taken per day is ", median(day.steps$steps.per.day), "\n")
```

```
## The median number of steps taken per day is  10765
```
  
###  **Question 1 answer:**  
#### The mean number of steps taken per day is 10766.
  

## Question #2 - What is the average daily activity pattern?  

1.      Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  


```r
        interval.steps.avg <- group_by(new.bit, interval) %>%
                                      summarise(avg.steps = mean(steps))
        
        time.steps <- ggplot(interval.steps.avg, aes(x = interval))

        time.steps + geom_line(aes(y = avg.steps)) +
                     labs(x = "Interval", y = "Average steps per Interval") + 
                     ggtitle("Average Steps Per Interval  Oct-1-2012 to Nov-30-2012")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

2.      Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?   




```r
        max.avg <- filter(interval.steps.avg, avg.steps == max(avg.steps))
        max.interval <- max.avg$interval
```

###  **Question 2 answer:**  
#### The maximum number of steps occurs at interval 835

## Question #3 - Imputing missing values.  

1.      Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).  


```r
        missing.data <- sum(is.na(bit$steps))
```

**There are 2304 rows with missing data**

2.      Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.   

The strategey that I chose was to use the mean per interval, that had already been caluculated in interval.steps.avg.  I like to use the dplyr package for speed and readability.  For this case it requires the use of the mutate function.


```r
        fix.bit <- mutate(bit, fix.steps = ifelse(!is.na(steps), steps, interval.steps.avg$avg.steps))
        fix.bit <- select(fix.bit, -steps) 
```

3.      Create a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
        fix.day.steps <- group_by(fix.bit, date) %>%
                          summarise(sum.steps = sum(fix.steps))
```

4.      Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 

I am rounding the steps to integers as part of this report.


```r
        fix.hist <- ggplot(fix.day.steps, aes(x = sum.steps))
        fix.hist + geom_histogram(bin=300, 
                                  fill ="white", 
                                  color = "black")
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

**The mean number of steps taken per day using the imputed is 10766.**  

**The median number of steps taken per day using the imputed is 10766.**

+ **Do these values differ from the estimates from the first part of the assignment?**  The values do not differ significantly since the counts are weighted toward the middle of the grouping.

+ **What is the impact of imputing missing data on the estimates of the total daily number of steps?**  By imputing data with the interval mean, the mean and median approach each other and provide similar results.    


## Question #4 - Are there differences in activity patterns between weekdays and weekends?  

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  

1.      Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  



```r
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
```

2.      Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
plot.weekday <- ggplot(week.day, aes(x = interval)) +
                        geom_line(aes(y = avg.steps)) +
                        labs(x = "Interval", y = "Average steps per Interval") + 
                        ggtitle("Average Steps Per Interval  Oct-1-2012 to Nov-30-2012 - On Weekdays")


plot.weekend <- ggplot(week.end, aes(x = interval)) +
                        geom_line(aes(y = avg.steps)) +
                        labs(x = "Interval", y = "Average steps per Interval") + 
                        ggtitle("Average Steps Per Interval  Oct-1-2012 to Nov-30-2012 - On Weekends")

grid.arrange(plot.weekday, plot.weekend)
```

![](./PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

###  **Question 4 answer:**  
#### There are differences between the activity patterns of weekdays and weekends.  The weekday data shows a larger average number of steps at the begining part of the day, with some lower average number of steps sustained during daytime hours.  The weekend data shows more instances of higher averages during the daytime hours.  One explanation could be that this person is perhaps an office worker.  There would be some activity getting ready for the work day in the morning with some moderate movement during the work day.  Some spurts in activity during the day could be explained as going out to lunch and stopping after work to pick up grocery items.  On the weekends, this person may have more activities such as shopping, yard work, or participattion in hobbies which could explain the difference.  Both data sets show little activity during normal sleep hours and this result is expected.  
