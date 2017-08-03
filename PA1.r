##-- loading data
data1 <- read.csv("activity.csv")
totalsteps <- tapply(data1$steps,data1$date,FUN=sum, na.rm=TRUE)

##-plotting histogram of total number of steps per day
library(ggplot2)
qplot(totalsteps,binwidth=1000,xlab="Total number of steps per day")
mean(totalsteps,na.rm = TRUE)
median(totalsteps, na.rm = TRUE)

##--plotting time series per 5-minute interval
avgsteps <- aggregate(x=list(steps=data1$steps),by=list(interval=data1$interval),FUN=mean,na.rm=TRUE)
ggplot(data=avgsteps,aes(x=interval,y=steps)) + geom_line() + 
  xlab("5-minute interval") + ylab("Average steps")

avgsteps(which.max(avgsteps$steps))

##--calculating missing values
sum(is.na(data1$steps))


##--replace missing value by the mean of its 5-minute interval
imputevalue <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (avgsteps[avgsteps$interval==interval, "steps"])
  return(filled)
  
}
impute <- data1
impute$steps <- mapply(imputevalue, impute$steps, impute$interval)


## plotting imputed data

Totalsteps <- tapply(impute$steps, impute$date, FUN=sum)

qplot(Totalsteps, binwidth=1000, xlab="total number of steps taken each day")

mean(Totalsteps)
median(Totalsteps)
