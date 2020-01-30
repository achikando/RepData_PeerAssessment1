---
output: 
  html_document: 
    keep_md: yes
---
R Markdown file for Reproducible research project 1
===================================================

Aristide Chikando
Course project 1

Start by creating work directory, downloading and loading data


```r
if(!file.exists("./coursework")){dir.create("./coursework")}
furl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(furl, destfile = "./coursework/dat")
setwd("./coursework")
# unzip and read in data
unzip("dat")
dat = read.csv("activity.csv")
```

Load libraries, get summary of "steps" data and an of assessment of missing values 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
names(dat)
```

```
## [1] "steps"    "date"     "interval"
```

```r
# Checking out the data
summary(dat$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
# See what fraction of the data has missing values
mean(is.na(dat$steps))
```

```
## [1] 0.1311475
```


here is the computation and output on number of steps taken per day


```r
dat2 = subset(dat, steps != "NA")  # Ommit rows with missing values

# group data by dates and compute daily step totals
totalSteps = dat2 %>% mutate(steps = as.numeric(steps)) %>% group_by(date) %>% 
  summarise(steps = sum(steps))
totalSteps$date = as.Date(totalSteps$date)
# plot
hist(totalSteps$steps, main = "Histogram of Daily Steps Totals", xlab = "Daily steps totals")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, file="StepHistogram.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# run summary statistics on summed-up steps column to display mean and median 
summary(totalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

Here is computation and output on the average daily activity pattern 

```r
# group data by intervals points and compute avarage steps at each intervals
meanSteps = dat2 %>% mutate(steps = as.numeric(steps)) %>% group_by(interval) %>% 
  summarise(steps = mean(steps))
# plotting
plot(meanSteps$interval, meanSteps$steps, type = "l", lwd = 2, xlab = "5-minute interval", 
     ylab="Mean number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, file="ActivityPattern.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# run summary statistics on averaged steps column to display mean and median
summary(meanSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.113  37.383  52.835 206.170
```

The number of rows with missing values (NA) is

```r
dat3 = subset(dat, is.na(steps))  # get rows with missing values
dim(dat3)[1] # output number of rows with missing values
```

```
## [1] 2304
```

Filling in missing values, resummarizing and replotting daily activity pattern

```r
# group rows with missing values by interval
nvSteps = data.frame(dat3 %>% mutate(steps = as.numeric(steps)) %>% group_by(interval))
a = data.frame(meanSteps$steps)
tmp = rbind(a, a, a, a, a, a, a, a) # generate column with mean steps at each corresponding interval
ndat = cbind(tmp, nvSteps$date, nvSteps$interval) # replacing "NA" column with computed averages
names(ndat) = names(dat)
new_data = rbind(ndat, data.frame(dat2)) # merge dataset with replaced "NAs" to the remaining dataset

#compute new total daily steps
ntotalSteps = new_data %>% mutate(steps = as.numeric(steps)) %>% group_by(date) %>% 
  summarise(steps = sum(steps))
# histogram of new daily total step
hist(ntotalSteps$steps, main = "Daily Steps Totals (with supplemented data)", xlab = "Daily steps totals")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
dev.copy(png, file="StepHistogram2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

Here is a boxplot of the 2 daily steps totals (without NAs and with replaced NAs) for comparison


```r
boxplot(totalSteps$steps, ntotalSteps$steps, main="Daily step totals")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
dev.copy(png, file="StepBoxPlot.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

Here are summary statistics showing replacing NAs with interval averages has no signifcicant impact on daily steps totals computed in the first part of the assignment 

```r
summary(totalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

```r
summary(ntotalSteps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

Here is a plot showing the activity pattern between weekdays and weekends


```r
new_data$date = as.Date(new_data$date)
wkd = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new_data$wkDay = c('weekend', 'weekday')[(weekdays(new_data$date) %in% wkd)+1L]

# transform data for plotting
new_dataMean = new_data %>% mutate(wkDay=as.factor(wkDay), steps = as.numeric(steps)) %>% 
  group_by(wkDay,interval) %>% summarize(steps = mean(steps))
library(lattice)
free.y<-list(y=list(relation="free"))
xyplot(steps~interval|wkDay,data=new_dataMean,type="l", xlab = "Interval", 
       ylab = "Mean Number of Steps Taken", scales=free.y, layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dev.copy(png, file="ActivityPattern2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


