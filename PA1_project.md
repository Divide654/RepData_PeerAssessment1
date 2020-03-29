Loading in data from provided csv file

    ###Loading Data
    library(lattice)
    x=read.csv("activity.csv",stringsAsF=F)

------------------------------------------------------------------------

Determine the average steps taken per day and plot this distribution as
a histogram. This subject usually takes around 10,000 steps a day.

    ### What is mean total number of steps taken per day?

    steps_per_day=tapply(x$steps,x$date,sum,na.rm=T)
    hist(steps_per_day, breaks=16, main="Histogram of Steps Per Day",xlab="Total Steps",col="steel blue")

![](html1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    print(paste("Mean: ",mean(steps_per_day)))

    ## [1] "Mean:  9354.22950819672"

    print(paste("Median: ",median(steps_per_day)))

    ## [1] "Median:  10395"

------------------------------------------------------------------------

Determine the average steps taken per time interval and plot to
visualize effect of daily schedule on activity. The subject clearly has
very low activity levels at night while they are presumably sleeping.
They have a high spike in activity at around the 800 time interval. This
could be a scheduled workout or a commute.

    ###What is the average daily activity pattern?

    mStepsPerInt=tapply(x$steps,x$interval,mean,na.rm=T)#Average steps per Interval
    xyplot(mStepsPerInt~as.numeric(names(mStepsPerInt)),type="l",xlab="Time Interval",ylab="Average Steps")

![](html1_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    print(paste("Interval with most average steps: ",names(which.max(mStepsPerInt)))) #Interval with highest Mean

    ## [1] "Interval with most average steps:  835"

------------------------------------------------------------------------

Impute missing values in data using the average value from the
corresponding time interval. This notably removes the group of days that
had around 0 steps in the previous histogram.

    ###Imputing missing values

    sum(is.na(x$steps)) #number of na values

    ## [1] 2304

    for(i in seq_along(x$steps)){ #Replace missing values with mean of interval
      if(is.na(x$steps[i])){
        x$steps[i]=mStepsPerInt[which(x[i,3]==names(mStepsPerInt))]
      }
    }
    sumPerDay2=tapply(x$steps,x$date,sum,na.rm=T)
    hist(sumPerDay2,            #plot hist of sum per day again
         main="Histogram of Steps Per Day (w/Imputing)",xlab="Total Steps",col="steel blue",breaks=16)

![](html1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    print(paste("Mean Per Day: ",mean(sumPerDay2))) #average day

    ## [1] "Mean Per Day:  10766.1886792453"

    print(paste("Median Per Day: ",median(sumPerDay2))) #median day  

    ## [1] "Median Per Day:  10766.1886792453"

------------------------------------------------------------------------

Plot the average steps taken per time interval, separating weekends and
weekdays. The subject has a different activity schedule on weekends.
Most notably, activity is much lower at the beginning of the day on
weekends.

    ###Are there differences in activity patterns between weekdays and weekends?

    x$weekday=weekdays(as.Date(x$date)) #record weekday of each date

    x$dayType[x$weekday=="Sunday"|x$weekday=="Saturday"]="weekend"
    x$dayType[x$weekday!="Sunday"&x$weekday!="Saturday"]="weekday"
    x$dayType=factor(x$dayType,levels=c("weekend","weekday"))

    wkAg=aggregate(steps~interval+dayType,x,mean)
    with(wkAg,xyplot(steps~interval|dayType,type="l",lwd=1,layout=c(1,2)))

![](html1_files/figure-markdown_strict/unnamed-chunk-5-1.png)
