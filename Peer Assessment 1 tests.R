
setwd("C:/Users/Cristobal/Dropbox/Data Science Coursera/5 Reproducible Research/RepData_PeerAssessment1")

unzip("activity.zip")
data <- read.csv("activity.csv")
Sys.setlocale("LC_TIME", "English")
summary(data)
str(data)
names(data)
head(data)
tail(data)
install.packages("ggplot2")
class(data$date)

day <- as.Date(date, %d)

library(ggplot2)
plot(data, aes(x=mean(steps), y=factor(date))) + geom_bar()

plot(data$date, data$steps, type="l")
library(dplyr)
install.packages("dplyr")

data2 <- group_by(data, date) %>%
        filter(!is.na(steps)) %>%
        summarise(total_steps = sum(steps))
qplot(date, total_steps, data=data2)
qplot(total_steps, binwidth = 5000, col="green",data=data2)
hist(data2$total_steps, col="red")
data2 <- group_by(data, date) %>%
        summarise(totalsteps = sum(steps))
data2

mean(data2$totalsteps, na.rm=TRUE)
median(data2$totalsteps, na.rm=TRUE)
?median
mean(data2$totalsteps)

data2$date <- as.character(data2$date)
data2$date <- as.Date(data2$date, "%d%m%y")
data2
class(data2$date)
da
data3 <- mutate(data2, date2 = as.Date(date))
data4 <- mutate(data3, Date = as.Date(date2, "%d%m%y"))       

data4

?as.Date
))
class(data3$date2)

)
       qplot(totalsteps, date, data=data2)


?qplot()
?group_by
library(ggplot2)

Sys.setlocale()

## What is the average daily activity pattern? ##

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

data3 <- group_by(data, interval) %>%
        filter(!is.na(steps)) %>%
        summarise(avg_steps = mean(steps))
qplot(avg_steps, data=data3)
qplot(interval, avg_steps, data=data3, type="l")
plot(data3$interval, data3$avg_steps, type="l", col="red", main="Daily Activity Pattern", xlab="5-minute Interval", ylab="Average of Steps")
?plot

with(data3, interval[avg_steps == 206.1698])
data3[which.max(data3$avg_steps), 1]

data3[which.na(data3$avg_steps)]
sum(is.na(data$steps))
?n()

## Imputar NA

dataNA <- data
        
dataNA$steps[dataNA$steps==NA] <- group_by(data, interval) %>%
        summarise(avg_steps = mean(steps, na.rm=TRUE))
        
dataNA$steps[dataNA$steps=="NA"] <- mean(dataNA$steps, na.rm=TRUE)        

?impute
sum(is.na(dataNA$steps))
impute
data$steps[data$steps==NA] <- data
?ddply

library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dataNA2 <- ddply(dataNA, ~ interval, transform, steps = impute.mean(steps))
dataNA2 <- dataNA2[order(dataNA2$date), ]
dataNA2 <- select(dataNA2, 1:3)
?order
dataNA2[1,1]
names(dataNA2)
?select
sum(is.na(dataNA$steps))

hist(dataNA$steps, col="red")
mean(dataNA$total_steps)
median(dataNA$total_steps)

class(dataNA$date)
dataNA2 <- group_by(dataNA, date) %>%
        summarise(total_steps = sum(steps))

?weekdays()

dataweek <- weekdays(dataNA3$date)
class(dataNA3$date)
)

Sys.setlocale("LC_TIME", "English")
class(dataNA$date)
dataNA$date <- as.character(dataNA$date)
class(dataNA$date)
dataNA$date <- as.Date(dataNA$date)
class(dataNA$date)
class(datadays$weekdays)
?factor

datadays <- mutate(dataNA, weekdays = weekdays(dataNA$date))
datadays$weekdays2[datadays$weekdays=="lunes"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="martes"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="miércoles"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="jueves"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="viernes"] <- "Weekday"

datadays$weekdays2[datadays$weekdays=="sábado"] <- "Weekend"
datadays$weekdays2[datadays$weekdays=="domingo"] <- "Weekend"

tail(datadays)

## sintaxis final weekends

Sys.setlocale("LC_TIME", "English")
dataNA$date <- as.character(dataNA$date)
dataNA$date <- as.Date(dataNA$date)

datadays <- mutate(dataNA, weekdays = weekdays(dataNA$date))
datadays$weekdays2[datadays$weekdays=="monday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="tuesday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="wednesday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="thursday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="friday"] <- "Weekday"

datadays$weekdays2[datadays$weekdays=="sturday"] <- "Weekend"
datadays$weekdays2[datadays$weekdays=="sunday"] <- "Weekend"

datadays$weekdays2 <- as.factor(datadays$weekdays2)

group_by(datadays, interval, weekdays2) %>%
        summarise(avg_steps = mean(steps))

?as.factor
