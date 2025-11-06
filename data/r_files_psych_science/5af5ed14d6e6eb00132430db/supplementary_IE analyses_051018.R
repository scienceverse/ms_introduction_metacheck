## Charlesworth & Banaji. Patterns of Implicit and Explicit Attitudes I. Long-term trends from 2007 to 2016.
## Script version date: April 12th, 2018

####################################################################################################################################

## This file examines: 
# (1) the trajectories of IE correlations over time
# (2) the cross-temporal implicit and explicit trends: does implicit predict explicit or vice versa?
# (3) differences in IE correlations as a function of generational cohort
# (4) the cross-temporal implicit and explicit changes as a function of Google searches

install.packages("plyr", dependencies = TRUE)
install.packages("imputeTS", dependencies = TRUE)
install.packages("forecast", dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("lmtest", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
install.packages("car", dependencies = TRUE)

require(plyr)
require(imputeTS)
require(forecast) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
require(lmtest)
require(reshape2)
require(car)

load("timeseries_allattitudes_011918.RData")
load("TSvectors_2016data_011918.RData")

####################################################################################################################################

## IE correlation trajectories across time -----------------
age.month.cor <- ddply(timeseriesAge, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
dis.month.cor <- ddply(timeseriesDis, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
race.month.cor <- ddply(timeseriesRace, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
skin.month.cor <- ddply(timeseriesSkin, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
sex.month.cor <- ddply(timeseriesSex, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
weight.month.cor <- ddply(timeseriesWeightBody, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))
weightF.month.cor <- ddply(timeseriesWeightFace, .(month.cent), summarize, ie.corr = cor(dscore, att.center1))

## For body weight/skin-tone perform imputation
## skin-tone
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$ie.corr <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "ie.corr")
skin.month.cor <- rbind(skin.month.extra, skin.month.cor)
skin.month.cor <- skin.month.cor[order(skin.month.cor$month.cent),]
skin.month.cor

skin.cor.ts <- ts(skin.month.cor$ie.corr, frequency = 12, start = c(2007, 1))
# impute with seasonal decomposition and interpolation
skin.cor.ts <- na.seadec(skin.cor.ts, algorithm = "interpolation")
skin.cor.ts
plot(skin.cor.ts)

## body-weight
# add in NAs for month values 125:131
weight.month.cor$month.cent[47:53] # gives us months 125 - 131
weight.month.cor$ie.corr[47:53] <- NA

# add add in NAs for missing months
weight.month.cor.extra <- as.data.frame(c(105:107, 132))
weight.month.cor.extra$ie.corr <- rep(NA, 4)
names(weight.month.cor.extra) <- c( "month.cent", "ie.corr")
weight.month.cor <- rbind(weight.month.cor.extra, weight.month.cor)
weight.month.cor <- weight.month.cor[order(weight.month.cor$month.cent),]
weight.month.cor

## Add on 2007 (month 37) through to 1003 (month 75)
weightF.month.cor <- weightF.month.cor[weightF.month.cor$month.cent >= 37 & weightF.month.cor$month.cent <= 75,]
weight.month.cor <- rbind(weightF.month.cor, weight.month.cor)

weight.cor.ts <- ts(weight.month.cor$ie.corr, frequency = 12, start = c(2007, 1))
weight.cor.ts <- na.seadec(weight.cor.ts, algorithm = "interpolation")
weight.cor.ts
plot(weight.cor.ts)


## complete for the remaining attitudes
age.cor.ts <- ts(age.month.cor$ie.corr, frequency = 12, start = c(2007, 1))
dis.cor.ts <- ts(dis.month.cor$ie.corr, frequency = 12, start = c(2007, 1))
race.cor.ts <- ts(race.month.cor$ie.corr, frequency = 12, start = c(2007, 1))
sex.cor.ts <- ts(sex.month.cor$ie.corr, frequency = 12, start = c(2007, 1))


## Percentage change from decomposed trends -------
decomp.age.cor <- decompose(age.cor.ts)
decomp.dis.cor <- decompose(dis.cor.ts)
decomp.race.cor <- decompose(race.cor.ts)
decomp.skin.cor <- decompose(skin.cor.ts)
decomp.sex.cor <- decompose(sex.cor.ts)
decomp.weight.cor <- decompose(weight.cor.ts)

# age
(decomp.age.cor$trend[7] - decomp.age.cor$trend[114])/decomp.age.cor$trend[7]

# dis
(decomp.dis.cor$trend[7] - decomp.dis.cor$trend[114])/decomp.dis.cor$trend[7]

# race
(decomp.race.cor$trend[7] - decomp.race.cor$trend[114])/decomp.race.cor$trend[7]

# skin
(decomp.skin.cor$trend[7] - decomp.skin.cor$trend[114])/decomp.skin.cor$trend[7]

# sex
(decomp.sex.cor$trend[7] - decomp.sex.cor$trend[114])/decomp.sex.cor$trend[7]

# weight
(decomp.weight.cor$trend[7] - decomp.weight.cor$trend[114])/decomp.weight.cor$trend[7]

# Initial and final values --------
age.cor.ts[1]; age.cor.ts[120]
dis.cor.ts[1]; dis.cor.ts[120]
race.cor.ts[1]; race.cor.ts[120]
sex.cor.ts[1]; sex.cor.ts[120]
skin.cor.ts[1]; skin.cor.ts[120]
weight.cor.ts[1]; weight.cor.ts[120]

## Plotting ------------
op <- par(mfrow = c(2,3))
plot(age.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "AGE", cex.main = 3)
lines(decomp.age.cor$trend, lwd = 3)

plot(dis.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "DISABILITY", cex.main = 3)
lines(decomp.dis.cor$trend, lwd = 3)

plot(weight.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "BODY-WEIGHT", cex.main = 3)
lines(decomp.weight.cor$trend, lwd = 3)

plot(skin.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "SKIN-TONE", cex.main = 3)
lines(decomp.skin.cor$trend, lwd = 3)

plot(race.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "RACE", cex.main = 3)
lines(decomp.race.cor$trend, lwd = 3)

plot(sex.cor.ts, ylab = "IE correlation", xlab = "", col = "darkgrey", ylim = c(0,0.5), main = "SEXUALITY", cex.main = 3)
lines(decomp.sex.cor$trend, lwd = 3)

par(op)


## ARIMAs on IE trajectories ---------
age.ie.arima <- auto.arima(age.cor.ts)
forecast.age.ie <- forecast(age.ie.arima, h = 60)
plot(forecast.age.ie, ylim = c(0,0.5))

dis.ie.arima <- auto.arima(dis.cor.ts)
forecast.dis.ie <- forecast(dis.ie.arima, h = 60)
plot(forecast.dis.ie, ylim = c(0,0.5))

weight.ie.arima <- auto.arima(weight.cor.ts)
forecast.weight.ie <- forecast(weight.ie.arima, h = 60)
plot(forecast.weight.ie, ylim = c(0,0.5))

skin.ie.arima <- auto.arima(skin.cor.ts)
forecast.skin.ie <- forecast(skin.ie.arima, h = 60)
plot(forecast.skin.ie, ylim = c(0,0.5))

race.ie.arima <- auto.arima(race.cor.ts)
forecast.race.ie <- forecast(race.ie.arima, h = 60)
plot(forecast.race.ie, ylim = c(0,0.5))

sex.ie.arima <- auto.arima(sex.cor.ts)
forecast.sex.ie <- forecast(sex.ie.arima, h = 60)
plot(forecast.sex.ie, ylim = c(0,0.5))

## PLotting with forecasts ---------
op <- par(mfrow = c(2,3))
plot(forecast.sex.ie, main = "SEXUALITY", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.sex.cor$trend, col = "black", lwd = 3)
plot(forecast.race.ie, main = "RACE", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.race.cor$trend, col = "black", lwd = 3)
plot(forecast.skin.ie, main = "SKIN-TONE", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.skin.cor$trend, col = "black", lwd = 3)
plot(forecast.age.ie, main = "AGE", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.age.cor$trend, col = "black", lwd = 3)
plot(forecast.dis.ie, main = "DISABILITY", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.dis.cor$trend, col = "black", lwd = 3)
plot(forecast.weight.ie, main = "BODY-WEIGHT", ylab = "Implicit-Explicit Correlation", ylim = c(0, 0.5), cex.main = 2,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.weight.cor$trend, col = "black", lwd = 3)
par(op)

## IE correlation across cohorts --------------------
## AGE
timeseriesAge$year <- substr(timeseriesAge$month, start = 1, stop = 2)
timeseriesAge$year <- factor(timeseriesAge$year, labels = c(2007:2016))
table(timeseriesAge$year)
timeseriesAge$yob <- as.numeric(as.character(timeseriesAge$year)) - timeseriesAge$age.new
table(timeseriesAge$yob)
timeseriesAge$yobcats <- cut(timeseriesAge$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesAge$yobcats) <- c("silent","boomers", "genX", "millennials",
                                   "genZ")
table(timeseriesAge$yobcats)
levels(timeseriesAge$yobcats)

## DISABILITY
timeseriesDis$year <- substr(timeseriesDis$month, start = 1, stop = 2)
timeseriesDis$year <- factor(timeseriesDis$year, labels = c(2007:2016))
table(timeseriesDis$year)
timeseriesDis$yob <- as.numeric(as.character(timeseriesDis$year)) - timeseriesDis$age.new
table(timeseriesDis$yob)
timeseriesDis$yobcats <- cut(timeseriesDis$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesDis$yobcats) <- c("silent","boomers", "genX", "millennials",
                                   "genZ")
table(timeseriesDis$yobcats)


## SEX
timeseriesSex$year <- substr(timeseriesSex$month, start = 1, stop = 2)
timeseriesSex$year <- factor(timeseriesSex$year, labels = c(2007:2016))
table(timeseriesSex$year)
timeseriesSex$yob <- as.numeric(as.character(timeseriesSex$year)) - timeseriesSex$age.new
table(timeseriesSex$yob)
timeseriesSex$yobcats <- cut(timeseriesSex$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesSex$yobcats) <- c("silent","boomers", "genX", "millennials",
                                   "genZ")
table(timeseriesSex$yobcats)


## RACE
timeseriesRace$year <- substr(timeseriesRace$month, start = 1, stop = 2)
timeseriesRace$year <- factor(timeseriesRace$year, labels = c(2007:2016))
table(timeseriesRace$year)
timeseriesRace$yob <- as.numeric(as.character(timeseriesRace$year)) - timeseriesRace$age.new
table(timeseriesRace$yob)
timeseriesRace$yobcats <- cut(timeseriesRace$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesRace$yobcats) <- c("silent","boomers", "genX", "millennials",
                                    "genZ")
table(timeseriesRace$yobcats)

## SKIN
timeseriesSkin$year <- substr(timeseriesSkin$month, start = 1, stop = 2)
timeseriesSkin$year <- factor(timeseriesSkin$year, labels = c(2007:2016))
table(timeseriesSkin$year)
timeseriesSkin$yob <- as.numeric(as.character(timeseriesSkin$year)) - timeseriesSkin$age.new
table(timeseriesSkin$yob)
timeseriesSkin$yobcats <- cut(timeseriesSkin$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesSkin$yobcats) <- c("silent","boomers", "genX", "millennials",
                                    "genZ")
table(timeseriesSkin$yobcats)

## BODY WEIGHT (BODY)
timeseriesWeightBody$year <- substr(timeseriesWeightBody$month, start = 1, stop = 2)
timeseriesWeightBody$year <- factor(timeseriesWeightBody$year, labels = c(2010:2016))
table(timeseriesWeightBody$year)
timeseriesWeightBody$yob <- as.numeric(as.character(timeseriesWeightBody$year)) - timeseriesWeightBody$age.new
table(timeseriesWeightBody$yob)
timeseriesWeightBody$yobcats <- cut(timeseriesWeightBody$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesWeightBody$yobcats) <- c("silent","boomers", "genX", "millennials",
                                          "genZ")
table(timeseriesWeightBody$yobcats)

## BODY WEIGHT (FACE)
timeseriesWeightFace$year <- substr(timeseriesWeightFace$month, start = 1, stop = 2)
timeseriesWeightFace$year <- factor(timeseriesWeightFace$year, labels = c(2004:2011))
table(timeseriesWeightFace$year)
timeseriesWeightFace$yob <- as.numeric(as.character(timeseriesWeightFace$year)) - timeseriesWeightFace$age.new
table(timeseriesWeightFace$yob)
timeseriesWeightFace$yobcats <- cut(timeseriesWeightFace$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesWeightFace$yobcats) <- c("silent","boomers", "genX", "millennials",
                                          "genZ")
table(timeseriesWeightFace$yobcats)

## Plotting -------------
age.coh.cor <- ddply(timeseriesAge, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
dis.coh.cor <- ddply(timeseriesDis, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
race.coh.cor <- ddply(timeseriesRace, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
sex.coh.cor <- ddply(timeseriesSex, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
skin.coh.cor <- ddply(timeseriesSkin, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
weight.coh.cor <- ddply(timeseriesWeightBody, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))
weightF.coh.cor <- ddply(timeseriesWeightFace, .(yobcats), summarize, ie.corr = cor(dscore, att.center1))

age.coh.cor2 <- ddply(timeseriesAge, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                      ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
dis.coh.cor2 <- ddply(timeseriesDis, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                      ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
race.coh.cor2 <- ddply(timeseriesRace, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                       ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
skin.coh.cor2 <- ddply(timeseriesSkin, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                       ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
sex.coh.cor2 <- ddply(timeseriesSex, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                      ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
weight.coh.cor2 <- ddply(timeseriesWeightBody, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                         ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])
weightF.coh.cor2 <- ddply(timeseriesWeightFace, .(yobcats[is.na(yobcats)!=TRUE]), summarize, ie.corr.r = cor.test(dscore, att.center1)$estimate, 
                          ie.corr.lb = cor.test(dscore, att.center1)$conf.int[1], ie.corr.ub = cor.test(dscore, att.center1)$conf.int[2])

names(age.coh.cor2)[1] <- c("yobcats")
names(dis.coh.cor2)[1] <- c("yobcats")
names(race.coh.cor2)[1] <- c("yobcats")
names(skin.coh.cor2)[1] <- c("yobcats")
names(sex.coh.cor2)[1] <- c("yobcats")
names(weight.coh.cor2)[1] <- c("yobcats")
names(weightF.coh.cor2)[1] <- c("yobcats")

age.coh.cor2 <- age.coh.cor2[age.coh.cor2$yobcats!="silent",]
dis.coh.cor2 <- dis.coh.cor2[dis.coh.cor2$yobcats!="silent",]
race.coh.cor2 <- race.coh.cor2[race.coh.cor2$yobcats!="silent",]
skin.coh.cor2 <- skin.coh.cor2[skin.coh.cor2$yobcats!="silent",]
sex.coh.cor2 <- sex.coh.cor2[sex.coh.cor2$yobcats!="silent",]
weight.coh.cor2 <- weight.coh.cor2[weight.coh.cor2$yobcats!="silent",]
weightF.coh.cor2 <- weightF.coh.cor2[weightF.coh.cor2$yobcats!="silent",]

age.coh.cor2$yobcats <- factor(age.coh.cor2$yobcats)
dis.coh.cor2$yobcats <- factor(dis.coh.cor2$yobcats)
race.coh.cor2$yobcats <- factor(race.coh.cor2$yobcats)
skin.coh.cor2$yobcats <- factor(skin.coh.cor2$yobcats)
sex.coh.cor2$yobcats <- factor(sex.coh.cor2$yobcats)
weight.coh.cor2$yobcats <- factor(weight.coh.cor2$yobcats)
weightF.coh.cor2$yobcats <- factor(weightF.coh.cor2$yobcats)


op <- par(mfrow = c(2,3))
plot(age.coh.cor$yobcats, age.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "AGE", cex.main = 3)

plot(dis.coh.cor$yobcats, dis.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "DISABILITY", cex.main = 3)

plot(weight.coh.cor$yobcats, weight.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "BODY-WEIGHT", cex.main = 3)

plot(skin.coh.cor$yobcats, skin.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "SKIN-TONE", cex.main = 3)

plot(race.coh.cor$yobcats, race.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "RACE", cex.main = 3)

plot(sex.coh.cor$yobcats, sex.coh.cor$ie.corr, ylim = c(0, 0.5), 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "SEXUALITY", cex.main = 3)
par(op)


## with error bars
op <- par(mfrow = c(2,3))
plot(age.coh.cor2$yobcats, age.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "AGE", cex.main = 3)
points(age.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = age.coh.cor2$ie.corr.lb, y1 = age.coh.cor2$ie.corr.ub, lwd = 2)

plot(dis.coh.cor2$yobcats, dis.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "DISABILITY", cex.main = 3)
points(dis.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = dis.coh.cor2$ie.corr.lb, y1 = dis.coh.cor2$ie.corr.ub, lwd = 2)

plot(weight.coh.cor2$yobcats, weight.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "BODY-WEIGHT", cex.main = 3)
points(weight.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = weight.coh.cor2$ie.corr.lb, y1 = weight.coh.cor2$ie.corr.ub, lwd = 2)

plot(skin.coh.cor2$yobcats, skin.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "SKIN-TONE", cex.main = 3)
points(skin.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = skin.coh.cor2$ie.corr.lb, y1 = skin.coh.cor2$ie.corr.ub, lwd = 2)

plot(race.coh.cor2$yobcats, race.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "RACE", cex.main = 3)
points(race.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = race.coh.cor2$ie.corr.lb, y1 = race.coh.cor2$ie.corr.ub, lwd = 2)

plot(sex.coh.cor2$yobcats, sex.coh.cor2$ie.corr.r, ylim = c(0, 0.5), lty = 0, 
     ylab = "Implicit-Explicit Correlation", xlab = "Cohort", main = "SEXUALITY", cex.main = 3)
points(sex.coh.cor2$ie.corr.r, pch = 20, cex =2)
segments(x0 = 1:4, y0 = sex.coh.cor2$ie.corr.lb, y1 = sex.coh.cor2$ie.corr.ub, lwd = 2)
par(op)




## Predicting Implicit from Explicit -----------------
## AGE (on trended series)
grangertest(age.Wts, age.WEts, order = 1)
grangertest(age.Wts, age.WEts, order = 3)
grangertest(age.Wts, age.WEts, order = 6)
grangertest(age.Wts, age.WEts, order = 9)

grangertest(age.WEts, age.Wts, order = 1)
grangertest(age.WEts, age.Wts, order = 3)
grangertest(age.WEts, age.Wts, order = 6)
grangertest(age.WEts, age.Wts, order = 9)

## AGE (on detrended series)
age.Wts.detrend <- age.Wts - decompose(age.Wts)$trend
age.WEts.detrend <- age.WEts - decompose(age.WEts)$trend
grangertest(age.Wts.detrend, age.WEts.detrend, order = 1)
grangertest(age.Wts.detrend, age.WEts.detrend, order = 6)

grangertest(age.WEts.detrend, age.Wts.detrend, order = 1)
grangertest(age.WEts.detrend, age.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(age.WEts, age.Wts)

## correlating over time, detrended
cor.test(age.WEts.detrend, age.Wts.detrend)

# plot overlayed trends with z-transformations
z.age.WEts <- ts(as.vector(scale(age.WEts)), freq = 12, start = c(2007, 1))
z.age.Wts <- ts(as.vector(scale(age.Wts)), freq = 12, start = c(2007, 1))

plot(decompose(z.age.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.age.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise overlayed
plot(z.age.WEts - decompose(z.age.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
par(new = TRUE)
plot(z.age.Wts - decompose(z.age.Wts)$trend, ylab = "", axes = FALSE, col = "red", 
     ylim = c(-2,1.5))


## DISABILITY (on trended series)
grangertest(dis.Wts, dis.WEts, order = 1)
grangertest(dis.Wts, dis.WEts, order = 3)
grangertest(dis.Wts, dis.WEts, order = 6)
grangertest(dis.Wts, dis.WEts, order = 9)

grangertest(dis.WEts, dis.Wts, order = 1)
grangertest(dis.WEts, dis.Wts, order = 3)
grangertest(dis.WEts, dis.Wts, order = 6)
grangertest(dis.WEts, dis.Wts, order = 9)

## dis (on detrended series)
dis.Wts.detrend <- dis.Wts - decompose(dis.Wts)$trend
dis.WEts.detrend <- dis.WEts - decompose(dis.WEts)$trend
grangertest(dis.Wts.detrend, dis.WEts.detrend, order = 1)
grangertest(dis.Wts.detrend, dis.WEts.detrend, order = 6)

grangertest(dis.WEts.detrend, dis.Wts.detrend, order = 1)
grangertest(dis.WEts.detrend, dis.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(dis.WEts, dis.Wts)

## correlating over time, detrended
cor.test(dis.WEts.detrend, dis.Wts.detrend)

# plot overlayed trends with z-transformations
z.dis.WEts <- ts(as.vector(scale(dis.WEts)), freq = 12, start = c(2007, 1))
z.dis.Wts <- ts(as.vector(scale(dis.Wts)), freq = 12, start = c(2007, 1))

plot(decompose(z.dis.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.dis.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise overlayed
plot(z.dis.WEts - decompose(z.dis.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
par(new = TRUE)
plot(z.dis.Wts - decompose(z.dis.Wts)$trend, ylab = "", axes = FALSE, col = "red", 
     ylim = c(-2,1.5))


## RACE (on trended series)
grangertest(race.Wts, race.WEts, order = 1)
grangertest(race.Wts, race.WEts, order = 3)
grangertest(race.Wts, race.WEts, order = 6)
grangertest(race.Wts, race.WEts, order = 9)

grangertest(race.WEts, race.Wts, order = 1)
grangertest(race.WEts, race.Wts, order = 3)
grangertest(race.WEts, race.Wts, order = 6)
grangertest(race.WEts, race.Wts, order = 9)

## race (on detrended series)
race.Wts.detrend <- race.Wts - decompose(race.Wts)$trend
race.WEts.detrend <- race.WEts - decompose(race.WEts)$trend
grangertest(race.Wts.detrend, race.WEts.detrend, order = 1)
grangertest(race.Wts.detrend, race.WEts.detrend, order = 6)

grangertest(race.WEts.detrend, race.Wts.detrend, order = 1)
grangertest(race.WEts.detrend, race.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(race.WEts, race.Wts)

## correlating over time, detrended
cor.test(race.WEts.detrend, race.Wts.detrend)

# plot overlayed trends with z-transformations
z.race.WEts <- ts(as.vector(scale(race.WEts)), freq = 12, start = c(2007, 1))
z.race.Wts <- ts(as.vector(scale(race.Wts)), freq = 12, start = c(2007, 1))

plot(decompose(z.race.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.race.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise
plot(z.race.WEts - decompose(z.race.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
par(new = TRUE)
plot(z.race.Wts - decompose(z.race.Wts)$trend, ylab = "", axes = FALSE, col = "red", 
     ylim = c(-2,1.5))


## SKIN TONE (on trended series)
grangertest(skin.Wts, skin.WEts, order = 1)
grangertest(skin.Wts, skin.WEts, order = 3)
grangertest(skin.Wts, skin.WEts, order = 6)
grangertest(skin.Wts, skin.WEts, order = 9)

grangertest(skin.WEts, skin.Wts, order = 1)
grangertest(skin.WEts, skin.Wts, order = 3)
grangertest(skin.WEts, skin.Wts, order = 6)
grangertest(skin.WEts, skin.Wts, order = 9)

## skin (on detrended series)
skin.Wts.detrend <- skin.Wts - decompose(skin.Wts)$trend
skin.WEts.detrend <- skin.WEts - decompose(skin.WEts)$trend
grangertest(skin.Wts.detrend, skin.WEts.detrend, order = 1)
grangertest(skin.Wts.detrend, skin.WEts.detrend, order = 6)

grangertest(skin.WEts.detrend, skin.Wts.detrend, order = 1)
grangertest(skin.WEts.detrend, skin.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(skin.WEts, skin.Wts)

## correlating over time, detrended
cor.test(skin.WEts.detrend, skin.Wts.detrend)

# plot overlayed trends with z-transformations
z.skin.WEts <- ts(as.vector(scale(skin.WEts)), freq = 12, start = c(2007, 1))
z.skin.Wts <- ts(as.vector(scale(skin.Wts)), freq = 12, start = c(2007, 1))

plot(decompose(z.skin.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.skin.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise
plot(z.skin.WEts - decompose(z.skin.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
par(new = TRUE)
plot(z.skin.Wts - decompose(z.skin.Wts)$trend, ylab = "", axes = FALSE, col = "red", 
     ylim = c(-2,1.5))


## SEXUALITY (on trended series)
grangertest(sex.Wts, sex.WEts, order = 1)
grangertest(sex.Wts, sex.WEts, order = 3)
grangertest(sex.Wts, sex.WEts, order = 6)
grangertest(sex.Wts, sex.WEts, order = 9)

grangertest(sex.WEts, sex.Wts, order = 1)
grangertest(sex.WEts, sex.Wts, order = 3)
grangertest(sex.WEts, sex.Wts, order = 6)
grangertest(sex.WEts, sex.Wts, order = 9)

## sex (on detrended series)
sex.Wts.detrend <- sex.Wts - decompose(sex.Wts)$trend
sex.WEts.detrend <- sex.WEts - decompose(sex.WEts)$trend
grangertest(sex.Wts.detrend, sex.WEts.detrend, order = 1)
grangertest(sex.Wts.detrend, sex.WEts.detrend, order = 6)

grangertest(sex.WEts.detrend, sex.Wts.detrend, order = 1)
grangertest(sex.WEts.detrend, sex.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(sex.WEts, sex.Wts)

## correlating over time, detrended
cor.test(sex.WEts.detrend, sex.Wts.detrend)

# plot overlayed trends with z-transformations
z.sex.WEts <- ts(as.vector(scale(sex.WEts)), freq = 12, start = c(2007, 1))
z.sex.Wts <- ts(as.vector(scale(sex.Wts)), freq = 12, start = c(2007, 1))

plot(decompose(z.sex.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.sex.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise
plot(z.sex.WEts - decompose(z.sex.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
par(new = TRUE)
plot(z.sex.Wts - decompose(z.sex.Wts)$trend, ylab = "", axes = FALSE, col = "red", 
     ylim = c(-2,1.5))


## BODY WEIGHT - figure (on trended series)
grangertest(weight.Wts, weight.WEts, order = 1)
grangertest(weight.Wts, weight.WEts, order = 3)
grangertest(weight.Wts, weight.WEts, order = 6)
grangertest(weight.Wts, weight.WEts, order = 9)

grangertest(weight.WEts, weight.Wts, order = 1)
grangertest(weight.WEts, weight.Wts, order = 3)
grangertest(weight.WEts, weight.Wts, order = 6)
grangertest(weight.WEts, weight.Wts, order = 9)

## weight (on detrended series)
weight.Wts.detrend <- weight.Wts - decompose(weight.Wts)$trend
weight.WEts.detrend <- weight.WEts - decompose(weight.WEts)$trend
grangertest(weight.Wts.detrend, weight.WEts.detrend, order = 1)
grangertest(weight.Wts.detrend, weight.WEts.detrend, order = 6)

grangertest(weight.WEts.detrend, weight.Wts.detrend, order = 1)
grangertest(weight.WEts.detrend, weight.Wts.detrend, order = 6)

## correlating over time, trended
cor.test(weight.WEts[40:120], weight.Wts)

## correlating over time, detrended
cor.test(weight.WEts.detrend[40:120], weight.Wts.detrend)

# plot overlayed trends with z-transformations
z.weight.WEts <- ts(as.vector(scale(weight.WEts)), freq = 12, start = c(2007, 1))
z.weight.Wts <- ts(as.vector(scale(weight.Wts)), freq = 12, start = c(2010, 4))

plot(decompose(z.weight.WEts)$trend, ylab = "Z-transformed preference scores", ylim = c(-1.5, 1))
lines(decompose(z.weight.Wts)$trend, col = "red")
abline(h = 0, lty = 2)

# noise
plot(z.weight.WEts - decompose(z.weight.WEts)$trend, ylab = "Z-transformed preference scores", 
     ylim = c(-2,1.5))
lines(z.weight.Wts - decompose(z.weight.Wts)$trend, col = "red")



## BODY WEIGHT - face (on trended series)
grangertest(weight.WtsFace, weight.WEts, order = 1)
grangertest(weight.WtsFace, weight.WEts, order = 3)
grangertest(weight.WtsFace, weight.WEts, order = 6)
grangertest(weight.WtsFace, weight.WEts, order = 9)

grangertest(weight.WEts, weight.WtsFace, order = 1)
grangertest(weight.WEts, weight.WtsFace, order = 3)
grangertest(weight.WEts, weight.WtsFace, order = 6)
grangertest(weight.WEts, weight.WtsFace, order = 9)

## weight (on detrended series)
weight.WtsFace.detrend <- weight.WtsFace - decompose(weight.WtsFace)$trend
weight.WEts.detrend <- weight.WEts - decompose(weight.WEts)$trend
grangertest(weight.WtsFace.detrend, weight.WEts.detrend, order = 1)
grangertest(weight.WtsFace.detrend, weight.WEts.detrend, order = 6)

grangertest(weight.WEts.detrend, weight.WtsFace.detrend, order = 1)
grangertest(weight.WEts.detrend, weight.WtsFace.detrend, order = 6)




## Google search trends ----------------
ism.trends.d <- read.csv("ism comparisons_dependent trends_041318.csv")
anti.trends.d <- read.csv("anti comparisons_dependent trends_051118.csv")
rights.trends.d <- read.csv("rights comparisons_dependent trends_051118.csv")


## Total searches (dependent) for -ism
ism.trends.d$sizeism <- as.character(ism.trends.d$sizeism)
ism.trends.d$ableism <- as.character(ism.trends.d$ableism)
ism.trends.dlong <- melt(ism.trends.d[,-1], 
                         measure.vars = c("racism", "homophobia", "ageism", "ableism", "sizeism"))
ism.trends.dlong$value <-recode(ism.trends.dlong$value, "'<1' = '0'")

ddply(ism.trends.dlong, .(variable), summarize, mean = mean(value), sd = sd(value), min = min(value), max = max(value))
summary(lm(value ~ variable, data = ism.trends.dlong))

op <- par(mar = c(6,6,2,2))
plot(ism.trends.dlong$variable, ism.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Racism", "Homophobia", "Ageism", "Ableism", "Sizeism"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)
par(op)

## Total searches (dependent) for -anti
anti.trends.d$anti.Black <- as.character(anti.trends.d$anti.Black)
anti.trends.d$anti.fat <- as.character(anti.trends.d$anti.fat)

anti.trends.d$anti.Black <- recode(anti.trends.d$anti.Black, "'<1' = '0'")
anti.trends.d$anti.fat <- recode(anti.trends.d$anti.fat, "'<1' = '0'")


anti.trends.dlong <- melt(anti.trends.d[,-1], 
                         measure.vars = c("anti.Black", "anti.gay", "anti.old", "anti.disabled", "anti.fat"))

ddply(anti.trends.dlong, .(variable), summarize, mean = mean(value), sd = sd(value), min = min(value), max = max(value))
summary(lm(value ~ variable, data = anti.trends.dlong))

op <- par(mar = c(6,6,2,2))
plot(anti.trends.dlong$variable, anti.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Anti-Black", "Anti-Gay", "Anti-Old", "Anti-Disabled", "Anti-Fat"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)
par(op)

## Total searches (dependent) for -rights
rights.trends.d$fat.acceptance <- as.character(rights.trends.d$fat.acceptance)
rights.trends.d$fat.acceptance <- recode(rights.trends.d$fat.acceptance, "'<1' = '0'")

rights.trends.dlong <- melt(rights.trends.d[,-1], 
                          measure.vars = c("black.rights", "gay.rights", "elder.rights", "disability.rights", "fat.acceptance"))

ddply(rights.trends.dlong, .(variable), summarize, mean = mean(value), sd = sd(value), min = min(value), max = max(value))
summary(lm(value ~ variable, data = rights.trends.dlong))

op <- par(mar = c(6,6,2,2))
plot(rights.trends.dlong$variable, rights.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Black-Rights", "Gay-Rights", "Elder-Rights", "Disability-Rights", "Fat-Acceptance"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)
par(op)

## Take the averages across the terms
all.trends.d <- rbind(ism.trends.dlong, anti.trends.dlong, rights.trends.dlong)

# Race
stat.desc(all.trends.d$value[which(all.trends.d$variable=="racism" | all.trends.d$variable== "anti.Black" | all.trends.d$variable== "black.rights")])
stat.desc(all.trends.d$value[which(all.trends.d$variable=="homophobia" | all.trends.d$variable== "anti.gay" | all.trends.d$variable== "gay.rights")])
stat.desc(all.trends.d$value[which(all.trends.d$variable=="ageism" | all.trends.d$variable== "anti.old" | all.trends.d$variable== "elder.rights")])
stat.desc(all.trends.d$value[which(all.trends.d$variable=="ableism" | all.trends.d$variable== "anti.disabled" | all.trends.d$variable== "disability.rights")])
stat.desc(all.trends.d$value[which(all.trends.d$variable=="sizeism" | all.trends.d$variable== "anti.fat" | all.trends.d$variable== "fat.acceptance")])


## Plot altogether

op <- par(mfrow = c(3,1), mar = c(6,6,2,2))
plot(ism.trends.dlong$variable, ism.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Racism", "Homophobia", "Ageism", "Ableism", "Sizeism"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)

plot(anti.trends.dlong$variable, anti.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Anti-Black", "Anti-Gay", "Anti-Old", "Anti-Disabled", "Anti-Fat"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)

plot(rights.trends.dlong$variable, rights.trends.dlong$value, axes = FALSE,
     ylab = "Relative Frequency\n(Out of possible 100)",
     xlab = "Search term", cex.lab = 1.5)
box()
axis(1, at = c(1:5), labels = c("Black-Rights", "Gay-Rights", "Elder-Rights", "Disability-Rights", "Fat-Acceptance"), cex.axis = 1.5)
axis(2, at = seq(0, 100, by = 20), seq(0, 100, by = 20), las = 2, cex.axis = 1.5)
par(op)



