---
title: "Reproducible Research Wk2 Assignment"
output:
  pdf_document: default
  html_document:
    keep_md: yes
---




##Data used for this report
The data used for this report is taken from the Dataset :Activity.zip Data found on the website
[https://github.com/rdpeng/RepData_PeerAssessment1]

The data comes from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:  
**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
**date**: The date on which the measurement was taken in YYYY-MM-DD format  
**interval**: Identifier for the 5-minute interval in which measurement was taken  
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

###Step 1 - Read the data having saved the data in your working directory into a folder called Coursera

```r
rawdata<-read.csv("~/Coursera/activity.csv", stringsAsFactors = FALSE)
str(rawdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(rawdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(rawdata)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

As can be seen from the output, there are NAs in the Steps fields, and also the Date field is a character. 
Transform the date field and investigate NAs. 

Add the lubridate package first then create a new variable in date format

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```




```r
procdata<-(rawdata)
procdata$newdate<-ymd(procdata$date)

str(procdata)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ newdate : Date, format: "2012-10-01" "2012-10-01" ...
```



##What is the Mean total number of steps per day
First - sum the data up to a daily total and plot a histogram of the data

```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
daysteps<-ddply(procdata, .(newdate),summarise, stepsperday=sum(steps))

hist(daysteps$stepsperday,main="Histogram of total number of steps taken each day",ylab="number of days", xlab="Number of steps")
```

![](PA1_template_files/figure-latex/Summarise data to daily total using plyr-1.pdf)<!-- --> 
  
**Mean number of steps per day ignoring missings**

```r
meanstepsperday<-as.integer(round(mean(daysteps$stepsperday,na.rm=TRUE),digits=0))
```

Mean number of steps per day ignoring missings is 10766.

**Median number of steps per day ignoring missings**

```r
medianstepsperday<-as.integer(median(daysteps$stepsperday,na.rm=TRUE))
```
 
Median number of steps per day ignoring missings is 10765

##What is the average daily activity pattern

First lets average the steps per 5 minute interval
 - remove NAs first as these mess up the plotting

```r
procdata2<-na.omit(procdata)
minsteps<-ddply(procdata2, .(interval),summarise, stepsperint=mean(steps))
plot(minsteps$stepsperint,type="l",main="Time series plot of average number of steps per interval",ylab="Average number of steps", xlab="5 minute interval", ylim=c(0,250), lwd=5,col="red")
```

![](PA1_template_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 
  
##Missing VAlues

###What is the interval with on average the most steps 


```r
intmax<-which(minsteps$stepsperint==max(minsteps$stepsperint))
x<-minsteps$interval[intmax]
x
```

```
## [1] 835
```
  
The interval with on average the most steps is 835.



###Inputing missing values  
Add the average number of steps per interval to all missing records.   
Make a new dataset procdata3


```r
procdata3<-merge(procdata,minsteps, all=TRUE)
procdata3$adjsteps<-ifelse(is.na(procdata3$steps),procdata3$stepsperint,procdata3$steps)
str(procdata3)
```

```
## 'data.frame':	17568 obs. of  6 variables:
##  $ interval   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps      : int  NA 0 0 0 0 0 0 0 0 0 ...
##  $ date       : chr  "2012-10-01" "2012-11-23" "2012-10-28" "2012-11-06" ...
##  $ newdate    : Date, format: "2012-10-01" "2012-11-23" ...
##  $ stepsperint: num  1.72 1.72 1.72 1.72 1.72 ...
##  $ adjsteps   : num  1.72 0 0 0 0 ...
```

```r
head(procdata3)
```

```
##   interval steps       date    newdate stepsperint adjsteps
## 1        0    NA 2012-10-01 2012-10-01    1.716981 1.716981
## 2        0     0 2012-11-23 2012-11-23    1.716981 0.000000
## 3        0     0 2012-10-28 2012-10-28    1.716981 0.000000
## 4        0     0 2012-11-06 2012-11-06    1.716981 0.000000
## 5        0     0 2012-11-24 2012-11-24    1.716981 0.000000
## 6        0     0 2012-11-15 2012-11-15    1.716981 0.000000
```

```r
tail(procdata3)
```

```
##       interval steps       date    newdate stepsperint adjsteps
## 17563     2355     0 2012-10-16 2012-10-16    1.075472 0.000000
## 17564     2355     0 2012-10-07 2012-10-07    1.075472 0.000000
## 17565     2355     0 2012-10-25 2012-10-25    1.075472 0.000000
## 17566     2355     0 2012-11-03 2012-11-03    1.075472 0.000000
## 17567     2355    NA 2012-10-08 2012-10-08    1.075472 1.075472
## 17568     2355    NA 2012-11-30 2012-11-30    1.075472 1.075472
```


```r
daysteps3<-ddply(procdata3, .(newdate),summarise, stepsperday3=sum(adjsteps))

hist(daysteps3$stepsperday3,main="Histogram of total number of steps taken each day -with missings imputed",ylab="number of days", xlab="Number of steps")
```

![](PA1_template_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 

```r
tail(daysteps3)
```

```
##       newdate stepsperday3
## 56 2012-11-25     11834.00
## 57 2012-11-26     11162.00
## 58 2012-11-27     13646.00
## 59 2012-11-28     10183.00
## 60 2012-11-29      7047.00
## 61 2012-11-30     10766.19
```

**Mean number of steps per day inputing missings**

```r
meanstepsperday3<-as.integer(round(mean(daysteps3$stepsperday3),digits=0))
```

Mean number of steps per day ignoring missings is 10766.  
Mean number of steps per day inputing missings is 10766.

**Median number of steps per day ignoring missings**

```r
medianstepsperday3<-as.integer(median(daysteps3$stepsperday))
```
 
Median number of steps per day ignoring missings is 10765  
Median number of steps per day inputing missings is 10766
  
There is no difference in the means including or excluding missing values, but the median steps has increased by 1


##Create a split by weekend or weekday and compare the two


```r
procdata3$day<-as.factor(ifelse(weekdays(procdata3$newdate) %in% c("Saturday","Sunday"),"Weekend","Weekday"))
avgstepsint<-ddply(procdata3, .( interval,day),summarise,avgsteps2=mean(adjsteps))
head(avgstepsint)
```

```
##   interval     day  avgsteps2
## 1        0 Weekday 2.25115304
## 2        0 Weekend 0.21462264
## 3        5 Weekday 0.44528302
## 4        5 Weekend 0.04245283
## 5       10 Weekday 0.17316562
## 6       10 Weekend 0.01650943
```

```r
tail(avgstepsint)
```

```
##     interval     day  avgsteps2
## 571     2345 Weekday 0.26331237
## 572     2345 Weekend 1.70518868
## 573     2350 Weekday 0.29685535
## 574     2350 Weekend 0.02830189
## 575     2355 Weekday 1.41006289
## 576     2355 Weekend 0.13443396
```



```r
library(lattice)
xyplot(avgsteps2~interval|day,data=avgstepsint,
       type='l',layout=c(1,2),
       xlab='5 Minute Interval',ylab='Average Number of Steps')
```

![](PA1_template_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 


