# Course Project 1 - Fit Bit Data Analysis
title: "readme.MD"
author: "TaylorBurrows"
date: "August 31, 2016"
output: html_document


## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

##Steps Taken in Analysis/Submittal 

### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
setwd("~/datasciencecoursera/3Reproducible Research/Project1")

if(!file.exists("./Activity_Dataset")) {
  dir.create("./Activity_Dataset")
}
    fileUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl,destfile = "./Activity_Dataset/activity.csv", method="curl")
    unzip(zipfile = "./Activity_Dataset/activity.zip", exdir="./Activity_Dataset.csv")
    
data<-read.csv("activity.csv")
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day
```r
avg_steps <- aggregate(steps ~ date, act_data, sum)
hist(avg_steps$steps, main = paste("Total Number of Steps Taken Each Day"), 
     col="red", 
     xlab="Total Number of Steps")
dev.copy(png,file="hist1.png", height=480,width=480)
dev.off()
```
![plot of chunk unnamed-chunk-2](/Users/tmoniz/datasciencecoursera/3Reproducible Research/Project1/hist1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
rmean <- mean(avg_steps$steps)
rmedian <- median(avg_steps$steps)
```

Answer: The `mean` is 10766.19 steps and the `median` is 10765.


### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
five_min <- aggregate(steps ~ interval, act_data, mean)

plot(five_min$interval,five_min$steps, type="l", xlab="Intervals", ylab="Total Number of Steps",main="Average Number of Steps per Day by Five Minute Intervals")
```

![plot of chunk unnamed-chunk-3](./Users/tmoniz/datasciencecoursera/3Reproducible Research/Project1/timeSeries.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps_int <- five_min[which.max(five_min$steps),1]
```
Answer: The interval that contains the maximum amount of steps is 835

### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)
```r
missing_value <- sum(is.na(act_data))
```
Answer: There are 2304 missing values in this data set.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
act_data_fill <- transform(act_data, steps = ifelse(is.na(steps), round(mean(steps, na.rm=TRUE)), steps))
missing_final <- sum(is.na(act_data_fill))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
act_data_noMiss <- aggregate(steps ~ date, act_data_fill, sum)
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(act_data_noMiss$steps, main = paste("Steps Each Day"), xlab="Total Number of Steps", ylab="Count", legend("topright", c("mean", "median"), col=c("blue", "red"))

abline(v = mean(act_data_noMiss$steps), col = "red", lwd = 2)
abline(v = median(act_data_noMiss$steps), col = "blue", lwd=2)

dev.copy(png,file="hist2.png", height=480,width=480)
dev.off()
```

![plot of chunk unnamed-chunk-4](/Users/tmoniz/datasciencecoursera/3Reproducible Research/Project1/hist2.png)

```r
rmean2 <- mean(act_data_noMiss$steps)
rmedian2 <- median(act_data_noMiss$steps)
```

Answer: The mean and median without missing values is different than it was before. The new mean is 10751.74 and the new median is 10565.  However, the difference is not significant enough to affect the analysis. See exact difference below:

```r
diff_mean <- rmean2 - rmean
diff_median <- rmedian2 - rmedian
total_diff <- sum(act_data_noMiss$steps) - sum(act_data_noMiss$steps)
```
Difference of means = -14.45
Difference of medians = -109
Total difference in Steps = 0

### Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


```r
wkDay <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
act_data_fill$dow = as.factor(ifelse(is.element(weekdays(as.Date(act_data_fill$date)),wkDay), "Weekday", "Weekend"))

act_data_noMiss <- aggregate(steps ~ interval + dow, act_data_fill, mean)

library(lattice)

xyplot(act_data_noMiss$steps ~ act_data_noMiss$interval|act_data_noMiss$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
dev.copy(png,file="panel.png", height=500,width=500)
dev.off()
```
![plot of chunk unnamed-chunk-5](/Users/tmoniz/datasciencecoursera/3Reproducible Research/Project1/panel.png)

## Submitting the Assignment

To submit the assignment:

1. Commit your completed `PA1_template.Rmd` file to the `master` branch of your git repository (you should already be on the `master` branch unless you created new ones)

2. Commit your `PA1_template.md` and `PA1_template.html` files produced by processing your R markdown file with the `knit2html()` function in R (from the **knitr** package)

3. If your document has figures included (it should) then they should have been placed in the `figure/` directory by default (unless you overrode the default). Add and commit the `figure/` directory to your git repository.

4. Push your `master` branch to GitHub.

5. Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will
need to submit the 40 character SHA-1 hash (as string of numbers from
0-9 and letters from a-f) that identifies the repository commit that
contains the version of the files you want to submit. You can do this
in GitHub by doing the following:

1. Go into your GitHub repository web page for this assignment

2. Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

3. You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.

A valid submission will look something like (this is just an **example**!)

```r
https://github.com/rdpeng/RepData_PeerAssessment1

7c376cc5447f11537f8740af8e07d6facc3d9645
```
