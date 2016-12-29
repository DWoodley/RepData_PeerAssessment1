## Reproducible Research Graded Project 1

## 1. Read in Dataset and process data.
    #File name activity.csv
    if(exists("activity")) remove("activity")
    if(exists("imputed.activity")) remove("imputed.activity")
    if(exists("t.avg.steps")) remove("t.avg.steps")
    if(exists("avg.steps.by.time")) remove("avg.steps.by.time")
    if(exists("avg.steps.weekday.t")) remove("avg.steps.weekday.t")
    if(exists("avg.steps.weekend.t")) remove("avg.steps.weekend.t")
    
    par(mfrow=c(1,1))
    
    library(lubridate)
    library(stringr)
    library(data.table)
    library(chron)

    unzip("activity.zip")
    filename <- "activity.csv"

    activity <- read.csv(filename,header = TRUE)
    
    # Create datetime column from date and interval columns (needed????)
    activity$datetime <- str_c(as.character(activity$date),
        str_c(str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=1,end=2),
          str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=3,end=4),"00",
          sep=":"),sep=" ")
    
    activity$datetime <- as_datetime(activity$datetime,tz=Sys.timezone())

    # Create factors from intervals
    activity$interval.f <- factor(str_c(str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=1,end=2),
                                 str_sub(str_pad(as.character(activity$interval),4,side="left",pad="0"),start=3,end=4),
                                 sep=":"),ordered=TRUE)

## 2. Create histogram of number of steps per day.
    hist(tapply(activity$steps,activity$date,sum,na.rm=TRUE),breaks=50,
         xlab = "Number of steps",
         ylab = "Frequency",
         main = "Frequency of Total Steps Per Day \nOver 61 Day Period (Raw Data)")

## 3. Calculate mean and median of number of steps per day.
    mean.steps <- round(mean(tapply(activity$steps,activity$date,sum,na.rm=TRUE)))
    median.steps <- median(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
    
    print(str_c("Mean number of steps per day:",as.character(mean.steps),sep= " "))
    print(str_c("Median number of steps per day:",as.character(median.steps),sep= " "))
    

## 4. Create time series plot of the average number of steps taken.
    # This is a time series plot of the average number of steps taken
    # by 5 minute intervals.
    avg.steps.by.time <- tapply(activity$steps,activity$interval,mean,na.rm=TRUE,ordered=TRUE)
    
    t.avg.steps <- as.data.table(avg.steps.by.time)
    t.avg.steps$avg.steps.by.time <- round(t.avg.steps$avg.steps.by.time)
    t.avg.steps$interval <- row.names(avg.steps.by.time)
    t.avg.steps$interval <- str_pad(t.avg.steps$interval,4,side="left",pad="0")
    t.avg.steps$interval <- str_c(str_sub(t.avg.steps$interval,1,2),":",str_sub(t.avg.steps$interval,3,4))
    t.avg.steps$interval <- factor(t.avg.steps$interval,ordered = TRUE)
    
    plot(x = t.avg.steps$interval,
         y= t.avg.steps$avg.steps.by.time,
         main="Average Number of Steps by 5 Minute Intervals",
         ylab = "Number of Steps",
         xlab = "Time of Day",
         type = "l")
    lines(t.avg.steps$avg.steps.by.time,col="blue")    
    
## 5. Find the 5 minute interval that, on average, contains the maximum
##    number of steps.
    max.interval <- t.avg.steps[t.avg.steps$avg.steps.by.time 
                                == max(t.avg.steps$avg.steps.by.time)]$interval

    print(str_c("Start of 5 minute interval with maximum average number of steps:",as.character(max.interval),sep= " "))

    
## 7. Calculate and report the total number of missing values in the dataset 
    # (i.e. the total number of rows with NAs)
    num.missing.vals <- sum(is.na(activity$steps))
    print(str_c("Count of missing steps values:",as.character(num.missing.vals),sep= " "))
    
    
## 6. Create code to describe and show a strategy for imputing missing data.
    # Will imput missing data where the value of the steps column is NA.
    # Will assume that 0 values of the steps are legitimate data points.
    
    imputed.activity <- activity
    
    if (num.missing.vals > 0)
    {
        for (x in t.avg.steps$interval) {
            imputed.activity[imputed.activity$interval.f==x & is.na(imputed.activity$steps),]$steps <- 
                round(t.avg.steps[t.avg.steps$interval==x,]$avg.steps.by.time)
        }
    }


## 7. Create histogram of the total number of steps taken each day AFTER 
##    missing values are imputed. (Compare to histogram of raw data)
    xpar <- par(mfrow = c(2,1))
    
    hist(tapply(imputed.activity$steps,imputed.activity$date,sum),breaks=50,
        xlab = "Number of steps per day",
        ylab = "Frequency",
        main = "Frequency of Total Steps Per Day \nOver 61 Day Period (Imputed Data)")

    hist(tapply(activity$steps,activity$date,sum,na.rm=TRUE),breaks=50,
         xlab = "Number of steps per day",
         ylab = "Frequency",
         main = "Frequency of Total Steps Per Day \nOver 61 Day Period (Raw Data)")
    
    par(xpar)
    
    mean.steps.imputed <- round(mean(tapply(imputed.activity$steps,imputed.activity$date,sum,na.rm=TRUE)))
    median.steps.imputed <- median(tapply(imputed.activity$steps,imputed.activity$date,sum,na.rm=TRUE))
    
    print(str_c("Mean number of steps per day (raw data):",as.character(mean.steps),sep= " "))
    print(str_c("Mean number of steps per day (imputed data):",as.character(mean.steps.imputed),sep= " "))
    print(str_c("Median number of steps per day (raw data):",as.character(median.steps),sep= " "))
    print(str_c("Median number of steps per day (imputed data):",as.character(median.steps.imputed),sep= " "))
    
## 8. Create a panel plot comparing the average number of steps taken per
##    per 5-minute interval across weekdays and weekends.
    
    
    imputed.activity$daytype <- factor("Weekday",levels=c("Weekday","Weekend"))
    imputed.activity[is.weekend(imputed.activity$datetime),]$daytype <- factor("Weekend")
    
    avg.steps.weekend <- tapply(imputed.activity[imputed.activity$daytype=="Weekend",]$steps,
                            imputed.activity[imputed.activity$daytype=="Weekend",]$interval.f,
                            mean,na.rm=TRUE,ordered=TRUE)
    avg.steps.weekend.t <- as.data.table(avg.steps.weekend)
    avg.steps.weekend.t$interval <- row.names(avg.steps.weekend)
    
    avg.steps.weekday <- tapply(imputed.activity[imputed.activity$daytype=="Weekday",]$steps,
                                imputed.activity[imputed.activity$daytype=="Weekday",]$interval.f,
                                mean,na.rm=TRUE,ordered=TRUE)
    
    avg.steps.weekday.t <- as.data.table(avg.steps.weekday)
    avg.steps.weekday.t$interval <- row.names(avg.steps.weekday)
    
    avg.steps.weekday.t$daytype <- factor("Weekday",levels=c("Weekday","Weekend"))
    avg.steps.weekend.t$daytype <- factor("Weekend",levels=c("Weekday","Weekend"))
    colnames(avg.steps.weekday.t) <- c("avg.steps","interval","daytype")
    colnames(avg.steps.weekend.t) <- c("avg.steps","interval","daytype")
    
    
    xsteps <- rbind(avg.steps.weekend.t,avg.steps.weekday.t)
    coplot(avg.steps ~ factor(interval)|daytype,data=xsteps,type = "l",
           ylab="Steps",xlab="Interval")
    
    
    
    
## 9. Include all the R code needed to reproduce the results (numbers, plots,
##    etc) in the report.