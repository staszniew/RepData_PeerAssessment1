# Reproducible Research: Peer Assessment 1
_Author: Stanislaw Szostak_

The aim of this paper is to present a brief analysis of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
A pre-requisite to running the code presented in this paper is to load _dplyr_ and _lattice_ libraries:

```r
library(dplyr)
library(lattice)
```

First of all, let's load the csv file into the "RawData" variable and convert the date column to the appropriate format. The raw data file has been deployed in the _repres_ subfolder of the default working directory.

```r
RawData <- read.csv("D:/R/WD/repres/activity.csv", header = T, sep = ",", stringsAsFactors = F)
RawData$date <- as.Date(RawData$date, format = "%Y-%m-%d")
```

The "RawData" variable now contains 17568 rows and 3 columns:  
- "steps": number of steps done,  
- "date": date in YYYY-MM-DD format,  
- "interval": the 5 minute interval value.

```r
str(RawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
In order to be able to answer the question above, let's make use of the _dplyr_ package to transform our "RawData" data frame. The output will be saved to a new data frame called "AggData":

```r
AggData <- RawData %>% 
       select(1:3) %>%  
       group_by(date) %>%
       summarise(TotalSteps = sum(steps, na.rm = T))
```

The "AggData" variable contains 61 rows and 2 columns:  
- "date",  
- "TotalSteps": total number of steps per day,  


```r
str(AggData)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	61 obs. of  2 variables:
##  $ date      : Date, format: "2012-10-01" "2012-10-02" ...
##  $ TotalSteps: int  0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  - attr(*, "drop")= logi TRUE
```

Here is a histogram of the distribution of the number of steps per day for the analysed period, as well as a summary of the TotalSteps variable:  

```r
par(mfrow=c(1,1))
with(AggData, hist(TotalSteps, breaks = 10, col = "red", xlab = "Number of steps", main = "Histogram of the total number of steps per day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
summary(AggData$TotalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

As we can deduct from the above-presented table, **the median number of steps amounts to 10400 and the average is 9354 steps/day**. This gives a hint that we might encounter skewness to the right, as the median is superior to the mean.

## What is the average daily activity pattern?
In order to display the average daily activity pattern, we need to transform the raw data frame into another one, grouped by intervals:

```r
avDay <- RawData %>%
         select(interval,steps) %>%
         group_by(interval) %>%
         summarise(Average = mean(steps, na.rm = T))
```

The activity throughout the day is as follows:

```r
with(avDay, plot(interval, Average, type = "l",  xlab = "Interval", ylab = "Number of steps", main = "Average number of steps per interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

We can easily discover the existence of a morning activity peak (between approx. 8-9 AM), then the number of steps varies during the afternoon and drops gradually starting from 7 PM. **The highest average number of steps occurs at 8:35 AM**, as can be seen below.

```r
print(avDay[avDay$Average ==max(avDay$Average),])
```

```
## Source: local data frame [1 x 2]
## 
##   interval  Average
## 1      835 206.1698
```

## Imputing missing values
In the raw dataset, there are 2304 missing values:

```r
print(sum(is.na(RawData$steps)))
```

```
## [1] 2304
```

The NAs significantly bias this analysis. New values will be imputed with the average value for the given interval:

```r
xMerged <- merge(avDay,RawData,by.x = "interval", by.y = "interval", all=TRUE)
xMerged <- xMerged[order(xMerged$date,xMerged$interval),]

# updating NAs with average interval value
for(i in 1:nrow(xMerged)){
  if(is.na(xMerged$steps[i])==TRUE){
    xMerged$steps[i] = xMerged$Average[i]
  }
  
}
xMerged <- xMerged %>% select(steps,date,interval)
summary(xMerged)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

The outcome of this transformation has been saved in the "xMerged" variable. The formerly-displayed "AggData" has been transformed to the "AggDataNARP" variable, containing no more missing values. This is how the comparison of the original and imputed data looks like in terms of frequency:

```r
AggDataNARP <- xMerged %>% 
  group_by(date) %>%
  summarise(TotalStepsNARP = sum(steps, na.rm = T))

# creating plots for comparison
par(mfrow=c(1,2))
with(AggData, hist(TotalSteps, breaks = 10, col = "red", xlab = "Number of steps", main = "Total steps per day"))
with(AggDataNARP, hist(TotalStepsNARP, breaks = 10, col = "red", xlab = "Number of steps", main = "Total steps per day, NA replaced"))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

```r
summary(AggDataNARP$TotalStepsNARP)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

It is easily noticeable that the number of occurrences with low numbers of steps have significantly decreased. In the same time, we observe a surge in occurrences with >10k steps.  
What's more, **the median and mean both are equal to 10770**. It means that we have obtained a much more symmetric distribution.


## Are there differences in activity patterns between weekdays and weekends?

For further analysis, a data frame called "xMergedWD" is created:

```r
# new variable created, Weekday column added
xMergedWD <- xMerged
xMergedWD$Weekday = "Weekday"

# Weekday column value updated
for(i in 1:nrow(xMergedWD)){
  if(weekdays(xMergedWD$date[i], abbreviate = T) %in% c("Sun","Sat")){
    xMergedWD$Weekday[i] = "Weekend"
    }
}

# data frame transformation (factor variable) and mean calculation
xMergedWD$Weekday <- factor(xMergedWD$Weekday, levels = c("Weekday","Weekend"))
xMergedWD <- xMergedWD %>% 
             group_by(Weekday,interval) %>%
             summarise(AvgSteps = mean(steps,na.rm=T))
```

This new data frame contains 576 rows (288 rows for each interval x 2 factor levels: "Weekday" and "Weekend"). Using the _lattice_ package, the difference in activity patterns between weekdays and weekends looks as follows:

```r
library(lattice)
xyplot(AvgSteps ~ interval | Weekday, data = xMergedWD, type = "l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 

The most striking conclusion is that weekday activity is much more concentrated around the morning peak than on weekends. However, activity on weekends seems to be distributed in a more even manner.
