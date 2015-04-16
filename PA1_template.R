#created 4-13-15 by Steven Watson for Coursera Reproduceable Research

#Load and process the data
activitydata <- read.csv("./activity.csv") #reads in the data
library(dplyr) #loads dplyr

#Summarize by day
daysummary<-group_by(activitydata,date) #grouping by day
stepsbyday<- summarize(daysummary,sum(steps)) #summing steps by day
hist(stepsbyday[[2]]) #plotting the histogram of steps by day
meansteps<-mean(stepsbyday[[2]], na.rm=TRUE) #shows mean steps by day
mediansteps<-median(stepsbyday[[2]], na.rm=TRUE) #shows median steps by day

#Summarize by time
intervalsummary<-group_by(activitydata,interval) #grouping by interval
intervalsummary2<-na.omit(intervalsummary) #removing NAs
stepsbyinterval<- summarize(intervalsummary2,mean(steps)) #averaging by interval
names(stepsbyinterval)<-sub("\\(","",names(stepsbyinterval)) #removing parenthesis
names(stepsbyinterval)<-sub("\\)","",names(stepsbyinterval))#removing parenthesis
with(stepsbyinterval,plot(interval,meansteps),type="l") #time plot
maxsteps<-max(stepsbyinterval$meansteps) #returns max value
maxtime<-stepsbyinterval$interval[which.max(stepsbyinterval$meansteps)] #returns max time

#Impute missing values
missingvalues<- length(which(is.na(activitydata))) #counts NAs
activitydatanew<-activitydata #making a new data frame with no NAs

#for all values, if a value is NA, replace it with the average step value
for(i in 1:17568) {  
    if(is.na(activitydata$steps[i])==TRUE) {
    activitydatanew$steps[i]= mean(activitydata$steps,na.rm=TRUE) 
    }
}

#Summarize new data by day
daysummarynew<-group_by(activitydatanew,date) #grouping by day
stepsbydaynew<- summarize(daysummarynew,sum(steps)) #summing steps by day
hist(stepsbydaynew[[2]]) #plotting the histogram of steps by day
meanstepsnew<-mean(stepsbydaynew[[2]], na.rm=TRUE) #shows mean steps by day
medianstepsnew<-median(stepsbydaynew[[2]], na.rm=TRUE) #shows median steps by day

weekdaydatanew<-activitydatanew #making a new data frame for weekdays
weekdaydatanew$date<-as.Date(weekdaydatanew$date) #converting to dates
weekdaydatanew<- mutate(weekdaydatanew, weekday=weekdays(date)) #adding day of week

#converting to weekend/weekday
weekdaydatanew$weekday<- sub("Monday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Tuesday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Wednesday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Thursday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Friday","Weekday",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Saturday","Weekend",weekdaydatanew$weekday)
weekdaydatanew$weekday<- sub("Sunday","Weekend",weekdaydatanew$weekday)

weekdaysummarynew<-group_by(weekdaydatanew,interval) #grouping by interval
avstepsbyweekdaynew<-weekdaysummarynew #new datafram
avstepsbyweekdaynew$mean <- ave(avstepsbyweekdaynew$steps, avstepsbyweekdaynew$interval, FUN=mean) #adding in means

library(lattice) #load lattice

xyplot(mean ~ interval | weekday, avstepsbyweekdaynew) #plotting mean steps by period for weekday and weekend