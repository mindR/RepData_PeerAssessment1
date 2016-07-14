Loading and Preprocessing the data

    actdata <- read.csv("activity.csv")

    #loading the data and storing it in actdata variable

No transformation required from the basic data analysis

Q1. What is the total number of steps taken per day?

Step 1: Calculating total number of steps taken each day

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    plotdata <- actdata %>% group_by(date) %>% summarise(sum(steps, na.rm=TRUE))

    colnames(plotdata) <- c("date", "TotalSteps")

Step 2: Creating Histogram of steps each day

    hist(plotdata$TotalSteps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)<!-- -->

Step 3: calculating mean and meadian value of steps data

    actmean <- mean(actdata$steps, na.rm = TRUE)
    actmedian <- median(actdata$steps, na.rm = TRUE)

    #mean value is 37.3826
    #median value is 0

What is the average daily activity pattern?

Step 1: Get the daily average for each interval

    plotdata2 <- actdata %>% group_by(interval) %>% summarise(mean(steps, na.rm=TRUE))

    #Stores the intervals and mean steps in the plotdata dataframe

Step 2: Time series plot for the steps data averages across days for
each interval

    plot(plotdata2, type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)<!-- -->

    #plots the data and the average steps for each time interval can be seen

Step 3: To find the interval with maximum average steps

    #can be found using max function

    #changing the column names of plotdata dataframe to make it more usable
    colnames(plotdata2) <- c("interval", "meansteps")

    plotdata2[plotdata2$meansteps==max(plotdata2$meansteps),]

    ## Source: local data frame [1 x 2]
    ## 
    ##   interval meansteps
    ##      (int)     (dbl)
    ## 1      835  206.1698

    # interval with max average steps is 835

Imputing missing values

Step 1: calculate and report the total missing values

    checkdata <- is.na (actdata$steps)
    table(checkdata)

    ## checkdata
    ## FALSE  TRUE 
    ## 15264  2304

    #total missing values 2304

Step 2: Strategy to fill in the missing steps values

Replacing the NAs with the equivalent mean values for each intervals

Step 3: Creating a new dataset with filled NAs for each intervals

    #function to replace the NA values and assigning it to new dataset #ModData

    rep_NA <- function() {
      i <- 0
      tempData <- actdata
      for (i in which(is.na(tempData$steps))) {
        tempData$steps[i] <- plotdata2$meansteps[which(plotdata2$interval==tempData$interval[i])]
      }
      tempData
    }

    ModData <- rep_NA()

Step 4: Histogram of new dataset and checking the mean and median total
number of steps and comparing it with old data

    plotdata3 <- ModData %>% group_by(date) %>% summarise(sum(steps, na.rm=TRUE))

    colnames(plotdata3) <- c("date", "TotalSteps")

    hist(plotdata3$TotalSteps)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)<!-- -->

    #histogram seems different from the one with original data
    #frequency changed for certain values because of additional data for 
    #blank values

    mean(ModData$steps)

    ## [1] 37.3826

    median(ModData$steps)

    ## [1] 0

    #mean 37.3826
    #median 0
    #same as the original data, since missing values replaced by mean itself

Are there any differences in activity patterns between weekdays and
weekends?

Step 1: adding a weekday column to the modified data

    #converting the date field into date class
    ModData$date <- as.Date(ModData$date)

    weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

    #adding a Day column 
    ModData$Day <- factor((weekdays(ModData$date) %in% weekdays1), levels = c(FALSE,TRUE), labels = c("weekend", "weekday"))

Step 2: creating a time series plot for average number of steps averaged
across weekdays and weekends

    #creating the dataframe for plotting interval vs mean steps
    plotdata4 <- ModData %>% group_by(interval, Day) %>% summarise(mean(steps, na.rm=TRUE))

    colnames(plotdata4) <- c("interval", "Day", "meansteps")

    #creating the timesharing plot
    library(lattice)
    xyplot(meansteps ~ interval | Day, data = plotdata4, type = "l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)<!-- -->