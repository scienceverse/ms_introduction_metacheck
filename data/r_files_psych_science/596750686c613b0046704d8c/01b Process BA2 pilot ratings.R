library(foreign)
library(stringr)
library(reshape)
library(psych)
library(Hmisc)
library(lubridate)

jobs <- read.spss("/Users/jaapdenissen/Documents/JobRatings_09302015.sav", to.data.frame=T, use.value.labels = F)

labels(jobs)

#[October 14, 1582 is Day 1 of the Gregorian calendar, see https://github.com/cran/oce/blob/master/R/moon.R]
jobs$startdate <- ISOdatetime(1582,10,14, 0, 0, 0)  + jobs$STARTED

#create a time frame (exclude others besides that)

date1 <- as.POSIXct("2015-09-16 00:00:01")
date2 <- as.POSIXct("2015-12-31 16:30:00")
int <- new_interval(date1, date2)

jobs <- jobs[jobs$startdate %within% int,]

jobs$duration_mins <- jobs$TIME_SUM / 60

molten <- melt(jobs[, c(1, 52:426)], id=("CASE"))

molten$jobnr <- paste("job", substr(molten$variable, 3, 4), sep = "")
molten$itemnr <- paste("item", substr(molten$variable, 6, 7), sep = "")

mycast <- cast(molten, jobnr~itemnr+CASE)

test <- as.data.frame(rbind(
  c("alpha.1" , alpha(mycast[,2:3])$total$raw_alpha), 
  c("alpha.2" , alpha(mycast[,4:5])$total$raw_alpha),
  c("alpha.3" , alpha(mycast[,6:7])$total$raw_alpha),
  c("alpha.4" , alpha(mycast[,8:9])$total$raw_alpha),
  c("alpha.5" , alpha(mycast[,10:11])$total$raw_alpha),
  c("alpha.6" , alpha(mycast[,12:13])$total$raw_alpha),
  c("alpha.7" , alpha(mycast[,14:15])$total$raw_alpha),
  c("alpha.8" , alpha(mycast[,16:17])$total$raw_alpha),
  c("alpha.9" , alpha(mycast[,18:19])$total$raw_alpha),
  c("alpha.10" , alpha(mycast[,20:21])$total$raw_alpha),
  c("alpha.11" , alpha(mycast[,22:23])$total$raw_alpha),
  c("alpha.12" , alpha(mycast[,24:25])$total$raw_alpha),
  c("alpha.13" , alpha(mycast[,26:27])$total$raw_alpha),
  c("alpha.14" , alpha(mycast[,28:29])$total$raw_alpha),
  c("alpha.15" , alpha(mycast[,30:31])$total$raw_alpha)
  ))

mycast$item01 <- rowMeans(mycast[,2:3])
mycast$item02 <- rowMeans(mycast[,4:5])
mycast$item03 <- rowMeans(mycast[,6:7])
mycast$item04 <- rowMeans(mycast[,8:9])
mycast$item05 <- rowMeans(mycast[,10:11])
mycast$item06 <- rowMeans(mycast[,12:13])
mycast$item07 <- rowMeans(mycast[,14:15])
mycast$item08 <- rowMeans(mycast[,16:17])
mycast$item09 <- rowMeans(mycast[,18:19])
mycast$item10 <- rowMeans(mycast[,20:21])
mycast$item11 <- rowMeans(mycast[,22:23])
mycast$item12 <- rowMeans(mycast[,24:25])
mycast$item13 <- rowMeans(mycast[,26:27])
mycast$item14 <- rowMeans(mycast[,28:29])
mycast$item15 <- rowMeans(mycast[,30:31])

mycast$item03 <- 8 - mycast$item03
mycast$item06 <- 8 - mycast$item06
mycast$item08 <- 8 - mycast$item08
mycast$item15 <- 8 - mycast$item15

alpha(mycast[,c("item01","item08","item12")])
alpha(mycast[,c("item02","item06","item09")])
#warning: item 13 loads very heavily, which is because 3 and 7 are slightly negatively correlated
cor(mycast$item03, mycast$item07)
alpha(mycast[,c("item03","item07","item13")])
alpha(mycast[,c("item04","item10","item14")])
alpha(mycast[,c("item05","item11","item15")])

mycast$con <- rowMeans(mycast[,c("item01","item08","item12")])
mycast$ext <- rowMeans(mycast[,c("item02","item06","item09")])
mycast$agr <- rowMeans(mycast[,c("item03","item07","item13")])
mycast$ope <- rowMeans(mycast[,c("item04","item10","item14")])
mycast$neu <- rowMeans(mycast[,c("item05","item11","item15")])

