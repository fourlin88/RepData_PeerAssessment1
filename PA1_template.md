# Reproducible Research: Peer Assessment 1
Created by fourlin88, 11 June 2015

### Loading and preprocessing the data

Here is the code to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data into a format suitable for analysis


```r
library(plyr)
unzip("activity.zip")
Data<-read.csv("activity.csv")
DataGroupedByDate <- ddply(Data, c("date"), summarise,
               sum = sum(steps)
)
```

### What is mean total number of steps taken per day?

For this part of the assignment, we can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day



```r
library(ggplot2)
ggplot(DataGroupedByDate,aes(date,sum)) + 
  geom_bar(stat="identity") +
  labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day.



```r
#print("original mean")
OriginalMean<-mean(DataGroupedByDate$sum,na.rm=TRUE)
print(OriginalMean)
```

```
## [1] 10766.19
```

```r
#print("original median")
OriginalMedian<-median(DataGroupedByDate$sum,na.rm=TRUE)
print(OriginalMedian)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
DataGroupedByInterval <- ddply(Data, c("interval"), summarise,
               mean = mean(steps,na.rm=TRUE)
)

ggplot(DataGroupedByInterval,aes(interval,mean))+
  geom_line(colour="red")+
  labs(title="Time Series Plot of steps every 5 min interval", x="5-min Interval", y="Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
DataGroupedByInterval[DataGroupedByInterval$mean == max(DataGroupedByInterval$mean), ]
```

```
##     interval     mean
## 104      835 206.1698
```



### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
# Total number of missing value in the dataset. 
sum(is.na(Data))
```

```
## [1] 2304
```

```r
# They are all related to steps subset
sum(is.na(Data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

#####*My strategy is to use the mean for the same interval across the dataset*

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

#####*Here is the code to create a new dataset.*


```r
#Copy the original dataset
ImputingData<- Data
#replace each step which is NA with the mean of that interval across the original dataset

for (i in 1:nrow(ImputingData)) {
    if (is.na(ImputingData$steps[i])) {
        ImputingData$steps[i] <- DataGroupedByInterval[which(ImputingData$interval[i] == DataGroupedByInterval$interval), ]$mean
    }
}

sum(is.na(ImputingData))
```

```
## [1] 0
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
ImputingDataGroupedByDate <- ddply(ImputingData, c("date"), summarise,
               sum = sum(steps)
)

ggplot(ImputingDataGroupedByDate,aes(date,sum)) + 
  geom_bar(stat="identity") +
  labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
#print("new mean")
ImputingMean<-mean(ImputingDataGroupedByDate$sum)
Compare<-list("Original Mean",(OriginalMean), "Imputing Mean",(ImputingMean),"The Difference",(OriginalMean-ImputingMean))
print.table(Compare)
```

```
## [1] Original Mean  10766.19       Imputing Mean  10766.19      
## [5] The Difference 0
```

```r
#print("new median")
ImputingMedian<-median(ImputingDataGroupedByDate$sum)
Compare<-list("Original Median",(OriginalMedian), "Imputing Median",(ImputingMedian),"The Difference",(OriginalMedian-ImputingMedian))
print.table(Compare)
```

```
## [1] Original Median 10765           Imputing Median 10766.19       
## [5] The Difference  -1.188679
```

####*Although there is no impact to the mean value of steps, the impact of imputing missing data is causing the new median value to be different with the original median. *



### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.



```r
ImputingData$weekdays <- factor(format(as.Date(ImputingData$date), "%A"))
levels(ImputingData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(ImputingData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(ImputingData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(ImputingData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```


2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



```r
ImputingDataGroupedByInterval <- ddply(ImputingData, c("interval","weekdays"), summarise,
               mean = mean(steps)
)

library(lattice)
xyplot(ImputingDataGroupedByInterval$mean ~ ImputingDataGroupedByInterval$interval | ImputingDataGroupedByInterval$weekdays, 
       layout = c(1, 2), type = "l",
       xlab = "Interval", ylab = "Average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

####*From the plot, there was a slight difference in the pattern between weekends and weekdays. The peak happened during weekdays.*
