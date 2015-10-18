setwd("C:/Users/Ythie/Documents/Data Science/Reproducible Research")
act <- read.csv('activity.csv', header = T)

acttot <- aggregate(act$steps ~ act$date, act, sum, na.action = na.pass)
hist(acttot$`act$steps`)
actavg <- aggregate(acttot$`act$steps` ~ acttot$`act$date`, acttot, mean, na.action = na.pass)
actmed <- aggregate(acttot$`act$steps` ~ acttot$`act$date`, acttot, median, na.action = na.pass)

actint <- aggregate(act$steps ~ act$interval, act, mean, na.action = na.omit)
plot(actint$`act$interval`, actint$`act$steps`, type = 'l')
maxstep <- subset(actint, 'act$steps' == max('act$steps'))
maxstep$'act$interval'

actna <- act[!complete.cases(act),]
dim(actna)
actmean <- aggregate(act$steps ~ act$date, act, mean, na.action = na.pass)
actmean$`act$steps`[is.na(actmean$`act$steps`)] <- mean(actmean$`act$steps`,na.rm = T)
names(actmean) <- c('date', 'steps')
actmer <- merge(act, actmean, by = 'date')
actmer$steps.x[is.na(actmer$steps.x)] <- actmer$steps.y[is.na(actmer$steps.x)]
names(actmer) <- c('date', 'steps', 'interval')
actmer <- actmer[,c('date', 'steps', 'interval')]
acttot2 <- aggregate(actmer$steps ~ actmer$date, actmer, sum)
hist(acttot2$`actmer$steps`)
actavg2 <- aggregate(acttot2$`actmer$steps` ~ acttot2$`actmer$date`, acttot2, mean)
actmed2 <- aggregate(acttot2$`actmer$steps` ~ acttot2$`actmer$date`, acttot2, median)

day <- weekdays(as.Date(act$date))
day[grepl('^S', day)] <- 'Weekend'
day[!grepl('^S', day)] <- 'Weekday'
day <- factor(day)
actmer$day <- day
actwd <- subset(actmer, day == 'Weekday')
actwe <- subset(actmer, day == 'Weekend')
actintwd <- aggregate(actwd$steps ~ actwd$interval, actwd, mean)
actintwe <- aggregate(actwe$steps ~ actwe$interval, actwe, mean)
par(mfrow = c(1,2))
plot(actintwd$`actwd$interval`, actintwd$`actwd$steps`, type = 'l')
plot(actintwe$`actwe$interval`, actintwe$`actwe$steps`, type = 'l')
