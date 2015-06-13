#setwd("C:/Tetra/2015/online-course-2015/reproducible-2015")
#unzip("repdata-data-activity.zip")
library(plyr)
library(dplyr)
data<-read.csv("activity.csv")
cdata <- ddply(data, c("date"), summarise,
               N    = length(steps),
               sum = sum(steps),
               mean = mean(steps),
               sd   = sd(steps),
               se   = sd / sqrt(N)
)
#plot(cdata$sum)
#hist(cdata$sum)
dow <- function(x) format(as.Date(x), "%A")

cdata$day <- dow(cdata$date)

type_of_day <- function(x) {
    if (x == "Saturday" || x == "Sunday")
    {
      tmp_1 <- "weekends"
    }
    else {
      tmp_1 <- "weekday"
    }
    print(x)
    print(tmp_1)
    return(tmp_1)
  
}

#mutate(cdata, new = type_of_day(day))

#levels(cdata$day)<- list(weekday=c("Monday","Tuesday","Wednesday","Thursday","Friday"),
  #                       weekend=c("Saturday","Sunday"))

#library(lubridate)
#df$date <- as.Date(df$date)
#wday(df$date, label=TRUE)

#tapply

#ccdata<-ddply(cdata,c=("cdata$day$weekday"), summarise,
#  sum(steps))


library(ggplot2)

ggplot(cdata,aes(date,sum)) + geom_bar(stat="identity")

### For line:

##### need to figure it out
##ggplot(data,aes(interval,steps)) + geom_line(colour="red") 
## ??????

##### ?????
idata <- ddply(data, c("interval"), summarise,
               sum = sum(steps,na.rm=TRUE),
               mean = mean(steps,na.rm=TRUE)
)

ggplot(idata,aes(interval,mean))+geom_line(colour="red")

#idata<-group_by(data,interval)

