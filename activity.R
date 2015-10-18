setwd("C:/Users/Ythie/Documents/Github/RepData_PeerAssessment1")
act <- read.csv('activity.csv', header = T)

acttot <- aggregate(act$steps ~ act$date, act, sum, na.action = na.pass)
hist(acttot$`act$steps`, xlab = 'Total Number of Steps per Day', main = 
       'Histogram of Total Number of Steps per Day', col = 'Red')

actavg <- aggregate(acttot$`act$steps` ~ acttot$`act$date`, acttot, mean,
                    na.action = na.pass)
head(actavg)
actmed <- aggregate(acttot$`act$steps` ~ acttot$`act$date`, acttot, median,
                    na.action = na.pass)
head(actmed)

actint <- aggregate(act$steps ~ act$interval, act, mean, na.action = na.omit)
plot(actint$`act$interval`, actint$`act$steps`, type = 'l', 
     xlab = '5 Minute Interval', ylab = 'Average Number of Steps',
     main = 'Average Daily Activity Pattern')

maxstep <- subset(actint, actint$'act$steps' == max(actint$'act$steps'))
print('Interval with Maximum Number of Steps = ')
print(maxstep$`act$interval`)

actna <- act[!complete.cases(act),]
print('Total Number of Missing Value = ')
dim(actna)

print('Imputing Strategy: Replace each missing value with mean of total average steps taken per day')
actmean <- aggregate(act$steps ~ act$date, act, mean, na.action = na.pass)
actmean$`act$steps`[is.na(actmean$`act$steps`)] <- mean(actmean$`act$steps`,na.rm = T)
names(actmean) <- c('date', 'steps')
actmer <- merge(act, actmean, by = 'date')
actmer$steps.x[is.na(actmer$steps.x)] <- actmer$steps.y[is.na(actmer$steps.x)]
names(actmer) <- c('date', 'steps', 'interval')
actmer <- actmer[,c('date', 'steps', 'interval')]
acttot2 <- aggregate(actmer$steps ~ actmer$date, actmer, sum)
hist(acttot2$`actmer$steps`, xlab = 'Total Number of Steps per Day',
     main = 'Histogram of Total Number of Steps per Day', col = 'Blue')
actavg2 <- aggregate(acttot2$`actmer$steps` ~ acttot2$`actmer$date`, acttot2, mean)
head(actavg2)
actmed2 <- aggregate(acttot2$`actmer$steps` ~ acttot2$`actmer$date`, acttot2, median)
head(actmed2)

day <- weekdays(as.Date(act$date))
day[!grepl('^S', day)] <- 'Weekday'
day[grepl('^S', day)] <- 'Weekend'
day <- factor(day)
actmer$day <- day
actwd <- subset(actmer, day == 'Weekday')
actwe <- subset(actmer, day == 'Weekend')
actintwd <- aggregate(actwd$steps ~ actwd$interval, actwd, mean)
actintwe <- aggregate(actwe$steps ~ actwe$interval, actwe, mean)
par(mfrow = c(1,2))
plot(actintwd$`actwd$interval`, actintwd$`actwd$steps`, type = 'l',
     xlab = '5 Minute Interval', ylab = 'Average Number of Steps',
     main = 'Average Weekday Activity Pattern')
plot(actintwe$`actwe$interval`, actintwe$`actwe$steps`, type = 'l',
     xlab = '5 Minute Interval', ylab = 'Average Number of Steps',
     main = 'Average Weekend Activity Pattern')