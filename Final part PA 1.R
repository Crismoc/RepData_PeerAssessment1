Sys.setlocale("LC_TIME", "English")
dataNA$date <- as.character(dataNA$date)
dataNA$date <- as.Date(dataNA$date)

datadays <- mutate(dataNA, weekdays = weekdays(dataNA$date))
datadays$weekdays2[datadays$weekdays=="Monday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="Tuesday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="Wednesday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="Thursday"] <- "Weekday"
datadays$weekdays2[datadays$weekdays=="Friday"] <- "Weekday"

datadays$weekdays2[datadays$weekdays=="Saturday"] <- "Weekend"
datadays$weekdays2[datadays$weekdays=="Sunday"] <- "Weekend"

datadays$weekdays2 <- as.factor(datadays$weekdays2)

datadays2 <- group_by(datadays, interval, weekdays2) %>%
        summarise(avg_steps = mean(steps))

library(lattice)
xyplot(avg_steps ~ interval | weekdays2, data = datadays2, layout = c(1,2))

xyplot(avg_steps ~ interval | weekdays2, data = datadays2, layout = c(1,2), type="l")
