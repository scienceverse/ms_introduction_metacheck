## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 21st, 2018

####################################################################################################################################

## Timeseries by novel, repeat, and all IAT takers

####################################################################################################################################

## Load data
load("timeseries_allattitudes_011918.RData")

# Install packages
install.packages("forecast", dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("tseries", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("imputeTS", dependencies = TRUE)
install.packages("lsr", dependencies = TRUE)

# Load packages
require(forecast) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
require(tseries)
require(lmtest)
require(plyr)
require(imputeTS)
require(lsr)

## Check num across all attitudes --------------
table(timeseriesAge$num.new, timeseriesAge$month.cent)
table(timeseriesDis$num.new, timeseriesDis$month.cent)
table(timeseriesRace$num.new, timeseriesRace$month.cent)
table(timeseriesSex$num.new, timeseriesSex$month.cent)
table(timeseriesSkin$num.new, timeseriesSkin$month.cent) ## 106 and 107
table(timeseriesWeightBody$num.new, timeseriesWeightBody$month.cent) ## not collected from 1004 to 1411
table(timeseriesWeightFace$num.new, timeseriesWeightFace$month.cent) ## only collected until 76 (1004)


## IMPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
age.tsnovel <- ddply(timeseriesAge[timeseriesAge$num.new=="0",],
               .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wtsnovel <- ts(age.tsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.Wtsnovel
plot(age.Wtsnovel)

age.arimanovel <- auto.arima(age.Wtsnovel, stepwise = FALSE, approximation = FALSE)
age.arimanovel 

forecast.agenovel <- forecast(age.arimanovel, h = 60)
plot(forecast.agenovel)

forecast.agenovelLONG <- forecast(age.arimanovel, h = 1800) # 150 years
plot(forecast.agenovelLONG)
abline(h = 0)

min(which(forecast.agenovelLONG$lower[,"95%"]<=0))/12 
min(which(forecast.agenovelLONG$mean<=0))/12 
min(which(forecast.agenovelLONG$upper[,"95%"]<=0))/12 

## percent change
decomp.agenovel <- decompose(age.Wtsnovel)
(decomp.agenovel$trend[7]-decomp.agenovel$trend[114])/decomp.agenovel$trend[7]
# initial value
age.Wtsnovel[1]
# final value
age.Wtsnovel[120]


## Fit to repeats
age.tsrepeat <- ddply(timeseriesAge[timeseriesAge$num.new!="0",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wtsrepeat <- ts(age.tsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.Wtsrepeat
plot(age.Wtsrepeat)

age.arimarepeat <- auto.arima(age.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
age.arimarepeat

forecast.agerepeat <- forecast(age.arimarepeat, h = 60)
plot(forecast.agerepeat)

forecast.agerepeatLONG <- forecast(age.arimarepeat, h = 1800) # 150 years
plot(forecast.agerepeatLONG)
abline(h = 0)

min(which(forecast.agerepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.agerepeatLONG$mean<=0))/12 
min(which(forecast.agerepeatLONG$upper[,"95%"]<=0))/12
min(which(forecast.agerepeatLONG$upper[,"95%"]>=2*age.Wtsrepeat[1]))/12

## percent change
decomp.agerepeat <- decompose(age.Wtsrepeat)
(decomp.agerepeat$trend[7]-decomp.agerepeat$trend[114])/decomp.agerepeat$trend[7] 
# initial value
age.Wtsrepeat[1] 
# final value
age.Wtsrepeat[120] 

## Fit to all
age.tsall <- ddply(timeseriesAge,
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wtsall <- ts(age.tsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.Wtsall
plot(age.Wtsall)

age.arimaall <- auto.arima(age.Wtsall, stepwise = FALSE, approximation = FALSE)
age.arimaall

forecast.ageall <- forecast(age.arimaall, h = 60)
plot(forecast.ageall)

forecast.ageallLONG <- forecast(age.arimaall, h = 1800) # 150 years
plot(forecast.ageallLONG)
abline(h = 0)

min(which(forecast.ageallLONG$lower[,"95%"]<=0))/12 
min(which(forecast.ageallLONG$mean<=0))/12
min(which(forecast.ageallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.ageallLONG$upper[,"95%"]>=2*age.Wtsall[1]))/12 

## percent change
decomp.ageall <- decompose(age.Wtsall)
(decomp.ageall$trend[7]-decomp.ageall$trend[114])/decomp.ageall$trend[7] 
# initial value
age.Wtsall[1] 
# final value
age.Wtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.agenovel, main = "Novel", ylim = c(0.3, 0.57))
plot(forecast.agerepeat, main = "Repeat", ylim = c(0.3, 0.57))
plot(forecast.ageall, main = "All", ylim = c(0.3, 0.57))
par(op)



## Overlapping plots
plot(forecast.agenovel, ylim = c(0.3, 0.57), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agerepeat, ylim = c(0.3, 0.57), main = "Implicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.ageall, ylim = c(0.3, 0.57), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 


## DIS  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
dis.tsnovel <- ddply(timeseriesDis[timeseriesDis$num.new=="0",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.Wtsnovel <- ts(dis.tsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.Wtsnovel
plot(dis.Wtsnovel)

dis.arimanovel <- auto.arima(dis.Wtsnovel, stepwise = FALSE, approximation = FALSE)
dis.arimanovel 

forecast.disnovel <- forecast(dis.arimanovel, h = 60)
plot(forecast.disnovel)

forecast.disnovelLONG <- forecast(dis.arimanovel, h = 1800) # 150 years
plot(forecast.disnovelLONG)
abline(h = 0)

min(which(forecast.disnovelLONG$lower[,"95%"]<=0))/12 
min(which(forecast.disnovelLONG$mean<=0))/12 
min(which(forecast.disnovelLONG$upper[,"95%"]<=0))/12 

## percent change
decomp.disnovel <- decompose(dis.Wtsnovel)
(decomp.disnovel$trend[7]-decomp.disnovel$trend[114])/decomp.disnovel$trend[7]
# initial value
dis.Wtsnovel[1]
# final value
dis.Wtsnovel[120] 


## Fit to repeats
dis.tsrepeat <- ddply(timeseriesDis[timeseriesDis$num.new!="0",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.Wtsrepeat <- ts(dis.tsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.Wtsrepeat
plot(dis.Wtsrepeat)

dis.arimarepeat <- auto.arima(dis.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
dis.arimarepeat 

forecast.disrepeat <- forecast(dis.arimarepeat, h = 60)
plot(forecast.disrepeat)

forecast.disrepeatLONG <- forecast(dis.arimarepeat, h = 1800) # 150 years
plot(forecast.disrepeatLONG)
abline(h = 0)

min(which(forecast.disrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.disrepeatLONG$mean<=0))/12 
min(which(forecast.disrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecast.disrepeatLONG$upper[,"95%"]>=2*dis.Wtsrepeat[1]))/12 

## percent change
decomp.disrepeat <- decompose(dis.Wtsrepeat)
(decomp.disrepeat$trend[7]-decomp.disrepeat$trend[114])/decomp.disrepeat$trend[7] 
# initial value
dis.Wtsrepeat[1] 
# final value
dis.Wtsrepeat[120]

## Fit to all
dis.tsall <- ddply(timeseriesDis,
                   .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.Wtsall <- ts(dis.tsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.Wtsall
plot(dis.Wtsall)

dis.arimaall <- auto.arima(dis.Wtsall, stepwise = FALSE, approximation = FALSE)
dis.arimaall

forecast.disall <- forecast(dis.arimaall, h = 60)
plot(forecast.disall)

forecast.disallLONG <- forecast(dis.arimaall, h = 1800) # 150 years
plot(forecast.disallLONG)
abline(h = 0)

min(which(forecast.disallLONG$lower[,"95%"]<=0))/12 
min(which(forecast.disallLONG$mean<=0))/12 
min(which(forecast.disallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.disallLONG$upper[,"95%"]>=2*dis.Wtsall[1]))/12 

## percent change
decomp.disall <- decompose(dis.Wtsall)
(decomp.disall$trend[7]-decomp.disall$trend[114])/decomp.disall$trend[7] 
# initial value
dis.Wtsall[1] 
# final value
dis.Wtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.disnovel, main = "Novel", ylim = c(0.35, 0.7))
plot(forecast.disrepeat, main = "Repeat", ylim = c(0.35, 0.7))
plot(forecast.disall, main = "All", ylim = c(0.35, 0.7))
par(op)



## Overlapping plots
plot(forecast.disnovel, ylim = c(0.35, 0.7), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disrepeat, ylim = c(0.35, 0.7), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disall, ylim = c(0.35, 0.7), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## RACE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
race.tsnovel <- ddply(timeseriesRace[timeseriesRace$num.new=="0",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
tail(race.tsnovel)
race.Wtsnovel <- ts(race.tsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.Wtsnovel
plot(race.Wtsnovel)

race.arimanovel <- auto.arima(race.Wtsnovel, stepwise = FALSE, approximation = FALSE)
race.arimanovel 

forecast.racenovel <- forecast(race.arimanovel, h = 60)
plot(forecast.racenovel)

forecast.racenovelLONG <- forecast(race.arimanovel, h = 1800) # 150 years
plot(forecast.racenovelLONG)
abline(h = 0)

min(which(forecast.racenovelLONG$lower[,"95%"]<=0))/12
min(which(forecast.racenovelLONG$mean<=0))/12 
min(which(forecast.racenovelLONG$upper[,"95%"]<=0))/12
min(which(forecast.racenovelLONG$upper[,"95%"]>=2*race.Wtsnovel[1]))/12 

## percent change
decomp.racenovel <- decompose(race.Wtsnovel)
(decomp.racenovel$trend[7]-decomp.racenovel$trend[114])/decomp.racenovel$trend[7] 
# initial value
race.Wtsnovel[1] 
# final value
race.Wtsnovel[120] 


## Fit to repeats
race.tsrepeat <- ddply(timeseriesRace[timeseriesRace$num.new!="0",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.Wtsrepeat <- ts(race.tsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.Wtsrepeat
plot(race.Wtsrepeat)

race.arimarepeat <- auto.arima(race.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
race.arimarepeat 

forecast.racerepeat <- forecast(race.arimarepeat, h = 60)
plot(forecast.racerepeat)

forecast.racerepeatLONG <- forecast(race.arimarepeat, h = 1800) # 150 years
plot(forecast.racerepeatLONG)
abline(h = 0)

min(which(forecast.racerepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.racerepeatLONG$mean<=0))/12 
min(which(forecast.racerepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecast.racerepeatLONG$upper[,"95%"]>=2*race.Wtsrepeat[1]))/12 

## percent change
decomp.racerepeat <- decompose(race.Wtsrepeat)
(decomp.racerepeat$trend[7]-decomp.racerepeat$trend[114])/decomp.racerepeat$trend[7] 
# initial value
race.Wtsrepeat[1] 
# final value
race.Wtsrepeat[120] 

## Fit to all
race.tsall <- ddply(timeseriesRace,
                   .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.Wtsall <- ts(race.tsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.Wtsall
plot(race.Wtsall)

race.arimaall <- auto.arima(race.Wtsall, stepwise = FALSE, approximation = FALSE)
race.arimaall 

forecast.raceall <- forecast(race.arimaall, h = 60)
plot(forecast.raceall)

forecast.raceallLONG <- forecast(race.arimaall, h = 1800) # 150 years
plot(forecast.raceallLONG)
abline(h = 0)

min(which(forecast.raceallLONG$lower[,"95%"]<=0))/12 
min(which(forecast.raceallLONG$mean<=0))/12 
min(which(forecast.raceallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.raceallLONG$upper[,"95%"]>=2*race.Wtsall[1]))/12 

## percent change
decomp.raceall <- decompose(race.Wtsall)
(decomp.raceall$trend[7]-decomp.raceall$trend[114])/decomp.raceall$trend[7] 
# initial value
race.Wtsall[1] 
# final value
race.Wtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.racenovel, main = "Novel", ylim = c(0.1, 0.4))
plot(forecast.racerepeat, main = "Repeat", ylim = c(0.1, 0.4))
plot(forecast.raceall, main = "All", ylim = c(0.1, 0.4))
par(op)



## Overlapping plots
plot(forecast.racenovel, ylim = c(0.1, 0.4), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racerepeat, ylim = c(0.1, 0.4), main = "Implicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.raceall, ylim = c(0.1, 0.4), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## SEXUALITY  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
sex.tsnovel <- ddply(timeseriesSex[timeseriesSex$num.new=="0",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.Wtsnovel <- ts(sex.tsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.Wtsnovel
plot(sex.Wtsnovel)

sex.arimanovel <- auto.arima(sex.Wtsnovel, stepwise = FALSE, approximation = FALSE)
sex.arimanovel

forecast.sexnovel <- forecast(sex.arimanovel, h = 60)
plot(forecast.sexnovel)

forecast.sexnovelLONG <- forecast(sex.arimanovel, h = 1800) # 150 years
plot(forecast.sexnovelLONG)
abline(h = 0)

min(which(forecast.sexnovelLONG$lower[,"95%"]<=0))/12 
min(which(forecast.sexnovelLONG$mean<=0))/12 
min(which(forecast.sexnovelLONG$upper[,"95%"]<=0))/12 
min(which(forecast.sexnovelLONG$upper[,"95%"]>=2*sex.Wtsnovel[1]))/12 

## percent change
decomp.sexnovel <- decompose(sex.Wtsnovel)
(decomp.sexnovel$trend[7]-decomp.sexnovel$trend[114])/decomp.sexnovel$trend[7] 
# initial value
sex.Wtsnovel[1] 
# final value
sex.Wtsnovel[120] 


## Fit to repeats
sex.tsrepeat <- ddply(timeseriesSex[timeseriesSex$num.new!="0",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.Wtsrepeat <- ts(sex.tsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.Wtsrepeat
plot(sex.Wtsrepeat)

sex.arimarepeat <- auto.arima(sex.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
sex.arimarepeat 

forecast.sexrepeat <- forecast(sex.arimarepeat, h = 60)
plot(forecast.sexrepeat)

forecast.sexrepeatLONG <- forecast(sex.arimarepeat, h = 1800) # 150 years
plot(forecast.sexrepeatLONG)
abline(h = 0)

min(which(forecast.sexrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.sexrepeatLONG$mean<=0))/12 
min(which(forecast.sexrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecast.sexrepeatLONG$upper[,"95%"]>=2*sex.Wtsrepeat[1]))/12 

## percent change
decomp.sexrepeat <- decompose(sex.Wtsrepeat)
(decomp.sexrepeat$trend[7]-decomp.sexrepeat$trend[114])/decomp.sexrepeat$trend[7] 
# initial value
sex.Wtsrepeat[1] 
# final value
sex.Wtsrepeat[120] 

## Fit to all
sex.tsall <- ddply(timeseriesSex,
                   .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.Wtsall <- ts(sex.tsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.Wtsall
plot(sex.Wtsall)

sex.arimaall <- auto.arima(sex.Wtsall, stepwise = FALSE, approximation = FALSE)
sex.arimaall 

forecast.sexall <- forecast(sex.arimaall, h = 60)
plot(forecast.sexall)

forecast.sexallLONG <- forecast(sex.arimaall, h = 1800) # 150 years
plot(forecast.sexallLONG)
abline(h = 0)

min(which(forecast.sexallLONG$lower[,"95%"]<=0))/12 
min(which(forecast.sexallLONG$mean<=0))/12
min(which(forecast.sexallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.sexallLONG$upper[,"95%"]>=2*sex.Wtsall[1]))/12 

## percent change
decomp.sexall <- decompose(sex.Wtsall)
(decomp.sexall$trend[7]-decomp.sexall$trend[114])/decomp.sexall$trend[7] 
# initial value
sex.Wtsall[1] 
# final value
sex.Wtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.sexnovel, main = "Novel", ylim = c(0, 0.42))
plot(forecast.sexrepeat, main = "Repeat", ylim = c(0, 0.42))
plot(forecast.sexall, main = "All", ylim = c(0, 0.42))
par(op)



## Overlapping plots
plot(forecast.sexnovel, ylim = c(0, 0.42), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexrepeat, ylim = c(0, 0.42), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexall, ylim = c(0, 0.42), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
abline(h = 0, lty = 2)
legend("bottomleft", c("Novel", "Repeat", "All"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## SKIN TONE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
skin.tsnovel <- ddply(timeseriesSkin[timeseriesSkin$num.new=="0",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))


# impute missing values
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$cohens <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")

skin.tsnovel <- rbind(skin.month.extra, skin.tsnovel)
skin.tsnovel <- skin.tsnovel[order(skin.tsnovel$month.cent),]
skin.tsnovel

skin.Wtsnovel <- ts(skin.tsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.Wtsnovel
plot(skin.Wtsnovel)

# impute with seasonal decomposition and interpolation
skin.Wtsnovel <- na.seadec(skin.Wtsnovel, algorithm = "interpolation")
skin.Wtsnovel
plot(skin.Wtsnovel)

skin.arimanovel <- auto.arima(skin.Wtsnovel, stepwise = FALSE, approximation = FALSE)
skin.arimanovel 

forecast.skinnovel <- forecast(skin.arimanovel, h = 60)
plot(forecast.skinnovel)

forecast.skinnovelLONG <- forecast(skin.arimanovel, h = 1800) # 150 years
plot(forecast.skinnovelLONG)
abline(h = 0)

min(which(forecast.skinnovelLONG$lower[,"95%"]<=0))/12
min(which(forecast.skinnovelLONG$mean<=0))/12 
min(which(forecast.skinnovelLONG$upper[,"95%"]<=0))/12 
min(which(forecast.skinnovelLONG$upper[,"95%"]>=2*skin.Wtsnovel[1]))/12 

## percent change
decomp.skinnovel <- decompose(skin.Wtsnovel)
(decomp.skinnovel$trend[7]-decomp.skinnovel$trend[114])/decomp.skinnovel$trend[7]
# initial value
skin.Wtsnovel[1]
# final value
skin.Wtsnovel[120]


## Fit to repeats
skin.tsrepeat <- ddply(timeseriesSkin[timeseriesSkin$num.new!="0",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsrepeat <- rbind(skin.month.extra, skin.tsrepeat)
skin.tsrepeat <- skin.tsrepeat[order(skin.tsrepeat$month.cent),]
skin.tsrepeat

skin.Wtsrepeat <- ts(skin.tsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.Wtsrepeat
plot(skin.Wtsrepeat)

# impute with seasonal decomposition and interpolation
skin.Wtsrepeat <- na.seadec(skin.Wtsrepeat, algorithm = "interpolation")
skin.Wtsrepeat
plot(skin.Wtsrepeat)

skin.arimarepeat <- auto.arima(skin.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
skin.arimarepeat

forecast.skinrepeat <- forecast(skin.arimarepeat, h = 60)
plot(forecast.skinrepeat)

forecast.skinrepeatLONG <- forecast(skin.arimarepeat, h = 1800) # 150 years
plot(forecast.skinrepeatLONG)
abline(h = 0)

min(which(forecast.skinrepeatLONG$lower[,"95%"]<=0))/12
min(which(forecast.skinrepeatLONG$mean<=0))/12 
min(which(forecast.skinrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecast.skinrepeatLONG$upper[,"95%"]>=2*skin.Wtsrepeat[1]))/12

## percent change
decomp.skinrepeat <- decompose(skin.Wtsrepeat)
(decomp.skinrepeat$trend[7]-decomp.skinrepeat$trend[114])/decomp.skinrepeat$trend[7] 
# initial value
skin.Wtsrepeat[1] 
# final value
skin.Wtsrepeat[120] 

## Fit to all
skin.tsall <- ddply(timeseriesSkin,
                   .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsall <- rbind(skin.month.extra, skin.tsall)
skin.tsall <- skin.tsall[order(skin.tsall$month.cent),]
skin.tsall

skin.Wtsall <- ts(skin.tsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.Wtsall
plot(skin.Wtsall)

# impute with seasonal decomposition and interpolation
skin.Wtsall <- na.seadec(skin.Wtsall, algorithm = "interpolation")
skin.Wtsall
plot(skin.Wtsall)

skin.arimaall <- auto.arima(skin.Wtsall, stepwise = FALSE, approximation = FALSE)
skin.arimaall

forecast.skinall <- forecast(skin.arimaall, h = 60)
plot(forecast.skinall)

forecast.skinallLONG <- forecast(skin.arimaall, h = 1800) # 150 years
plot(forecast.skinallLONG)
abline(h = 0)

min(which(forecast.skinallLONG$lower[,"95%"]<=0))/12 
min(which(forecast.skinallLONG$mean<=0))/12 
min(which(forecast.skinallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.skinallLONG$upper[,"95%"]>=2*skin.Wtsall[1]))/12 

## percent change
decomp.skinall <- decompose(skin.Wtsall)
(decomp.skinall$trend[7]-decomp.skinall$trend[114])/decomp.skinall$trend[7] 
# initial value
skin.Wtsall[1] 
# final value
skin.Wtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.skinnovel, main = "Novel", ylim = c(0.15, 0.4))
plot(forecast.skinrepeat, main = "Repeat", ylim = c(0.15, 0.4))
plot(forecast.skinall, main = "All", ylim = c(0.15, 0.4))
par(op)



## Overlapping plots
plot(forecast.skinnovel, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinrepeat, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinall, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "All"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## BODY WEIGHT (BODY) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$num.new, timeseriesWeightBody$month) 
# not available prior to 1501, so can only look at two years of data

## Fit to Novels
weightBody.tsnovel <- ddply(timeseriesWeightBody[timeseriesWeightBody$num.new=="0",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

weightBody.Wtsnovel <- ts(weightBody.tsnovel$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.Wtsnovel
plot(weightBody.Wtsnovel)

weightBody.arimanovel <- auto.arima(weightBody.Wtsnovel, stepwise = FALSE, approximation = FALSE)
weightBody.arimanovel 

forecast.weightBodynovel <- forecast(weightBody.arimanovel, h = 60)
plot(forecast.weightBodynovel)

forecast.weightBodynovelLONG <- forecast(weightBody.arimanovel, h = 1800) # 150 years
plot(forecast.weightBodynovelLONG)
abline(h = 0)

min(which(forecast.weightBodynovelLONG$lower[,"95%"]<=0))/12 
min(which(forecast.weightBodynovelLONG$mean<=0))/12 
min(which(forecast.weightBodynovelLONG$upper[,"95%"]<=0))/12 
min(which(forecast.weightBodynovelLONG$upper[,"95%"]>=2*weightBody.Wtsnovel[1]))/12 

## percent change
decomp.weightBodynovel <- decompose(weightBody.Wtsnovel)
decomp.weightBodynovel$trend
(decomp.weightBodynovel$trend[7]-decomp.weightBodynovel$trend[18])/decomp.weightBodynovel$trend[7] 
plot(decomp.weightBodynovel)
# initial value
weightBody.Wtsnovel[1]
# final value
weightBody.Wtsnovel[24] 

## Fit to Repeats
weightBody.tsrepeat <- ddply(timeseriesWeightBody[timeseriesWeightBody$num.new!="0",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

weightBody.Wtsrepeat <- ts(weightBody.tsrepeat$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.Wtsrepeat
plot(weightBody.Wtsrepeat)

weightBody.arimarepeat <- auto.arima(weightBody.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
weightBody.arimarepeat

forecast.weightBodyrepeat <- forecast(weightBody.arimarepeat, h = 60)
plot(forecast.weightBodyrepeat)

forecast.weightBodyrepeatLONG <- forecast(weightBody.arimarepeat, h = 1800) # 150 years
plot(forecast.weightBodyrepeatLONG)
abline(h = 0)

min(which(forecast.weightBodyrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.weightBodyrepeatLONG$mean<=0))/12 
min(which(forecast.weightBodyrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecast.weightBodyrepeatLONG$upper[,"95%"]>=2*weightBody.Wtsrepeat[1]))/12 

## percent change
decomp.weightBodyrepeat <- decompose(weightBody.Wtsrepeat)
(decomp.weightBodyrepeat$trend[7]-decomp.weightBodyrepeat$trend[18])/decomp.weightBodyrepeat$trend[7] 
plot(decomp.weightBodyrepeat)
# initial value
weightBody.Wtsrepeat[1]
# final value
weightBody.Wtsrepeat[24] 

## Fit to All
weightBody.tsall <- ddply(timeseriesWeightBody[timeseriesWeightBody$month.cent>=133,],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

weightBody.Wtsall <- ts(weightBody.tsall$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.Wtsall
plot(weightBody.Wtsall)

weightBody.arimaall <- auto.arima(weightBody.Wtsall, stepwise = FALSE, approximation = FALSE)
weightBody.arimaall 

forecast.weightBodyall <- forecast(weightBody.arimaall, h = 60)
plot(forecast.weightBodyall)

forecast.weightBodyallLONG <- forecast(weightBody.arimaall, h = 1800) # 150 years
plot(forecast.weightBodyallLONG)
abline(h = 0)

min(which(forecast.weightBodyallLONG$lower[,"95%"]<=0))/12
min(which(forecast.weightBodyallLONG$mean<=0))/12 
min(which(forecast.weightBodyallLONG$upper[,"95%"]<=0))/12 
min(which(forecast.weightBodyallLONG$upper[,"95%"]>=2*weightBody.Wtsall[1]))/12 

## percent change
decomp.weightBodyall <- decompose(weightBody.Wtsall)
(decomp.weightBodyall$trend[7]-decomp.weightBodyall$trend[18])/decomp.weightBodyall$trend[7] 
plot(decomp.weightBodyall)
# initial value
weightBody.Wtsall[1] 
# final value
weightBody.Wtsall[24] 



## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.weightBodynovel, main = "Novel", ylim = c(0.2, 0.85))
plot(forecast.weightBodyrepeat, main = "Repeat", ylim = c(0.2, 0.85))
plot(forecast.weightBodyall, main = "All", ylim = c(0.2, 0.85))
par(op)



## Overlapping plots
plot(forecast.weightBodynovel, ylim = c(0.2, 0.85), main = "Implicit Weight (Body) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodyall, ylim = c(0.2, 0.85), main = "Implicit Weight (Body) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodyrepeat, ylim = c(0.2, 0.85), main = "Implicit Weight (Body) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Repeat", "All", "Novel"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## BODY WEIGHT (FACE) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightFace$num.new, timeseriesWeightFace$month) # until 1004

## Fit to Novels
weightFace.tsnovel <- ddply(timeseriesWeightFace[timeseriesWeightFace$num.new=="0",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsnovel$month.cent)

weightFace.Wtsnovel <- ts(weightFace.tsnovel$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010, 4)) # ** ends early
weightFace.Wtsnovel
plot(weightFace.Wtsnovel)

weightFace.arimanovel <- auto.arima(weightFace.Wtsnovel, stepwise = FALSE, approximation = FALSE)
weightFace.arimanovel 

forecast.weightFacenovel <- forecast(weightFace.arimanovel, h = 60)
plot(forecast.weightFacenovel)

forecast.weightFacenovelLONG <- forecast(weightFace.arimanovel, h = 1800) # 150 years
plot(forecast.weightFacenovelLONG)
abline(h = 0)

min(which(forecast.weightFacenovelLONG$lower[,"95%"]<=0))/12 
min(which(forecast.weightFacenovelLONG$mean<=0))/12 
min(which(forecast.weightFacenovelLONG$upper[,"95%"]>=2*weightFace.Wtsnovel[1]))/12 
## note these numbers are from April, 2010.

## percent change
decomp.weightFacenovel <- decompose(weightFace.Wtsnovel)
(decomp.weightFacenovel$trend[7]-decomp.weightFacenovel$trend[66])/decomp.weightFacenovel$trend[7] 
plot(decomp.weightFacenovel)
# initial value
weightFace.Wtsnovel[1] 
# final value
weightFace.Wtsnovel[72] 


## Fit to Repeats
weightFace.tsrepeat <- ddply(timeseriesWeightFace[timeseriesWeightFace$num.new!="0",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsrepeat$month.cent)

weightFace.Wtsrepeat <- ts(weightFace.tsrepeat$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010,4)) ## ends early **
weightFace.Wtsrepeat
plot(weightFace.Wtsrepeat)

weightFace.arimarepeat <- auto.arima(weightFace.Wtsrepeat, stepwise = FALSE, approximation = FALSE)
weightFace.arimarepeat 

forecast.weightFacerepeat <- forecast(weightFace.arimarepeat, h = 60)
plot(forecast.weightFacerepeat)

forecast.weightFacerepeatLONG <- forecast(weightFace.arimarepeat, h = 1800) # 150 years
plot(forecast.weightFacerepeatLONG)
abline(h = 0)

min(which(forecast.weightFacerepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecast.weightFacerepeatLONG$mean<=0))/12 
min(which(forecast.weightFacerepeatLONG$upper[,"95%"]>=2*weightFace.Wtsrepeat[1]))/12

## percent change
decomp.weightFacerepeat <- decompose(weightFace.Wtsrepeat)
(decomp.weightFacerepeat$trend[7]-decomp.weightFacerepeat$trend[66])/decomp.weightFacerepeat$trend[7] 
plot(decomp.weightFacerepeat)
# initial value
weightFace.Wtsrepeat[1] 
# final value
weightFace.Wtsrepeat[72] 


## Fit to All
weightFace.tsall <- ddply(timeseriesWeightFace[timeseriesWeightFace$month.cent<=76,],
                                 .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsall$month.cent)

weightFace.Wtsall <- ts(weightFace.tsall$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010,4))
weightFace.Wtsall
plot(weightFace.Wtsall)

weightFace.arimaall <- auto.arima(weightFace.Wtsall, stepwise = FALSE, approximation = FALSE)
weightFace.arimaall 

forecast.weightFaceall <- forecast(weightFace.arimaall, h = 60)
plot(forecast.weightFaceall)

forecast.weightFaceallLONG <- forecast(weightFace.arimaall, h = 1800) # 150 years
plot(forecast.weightFaceallLONG)
abline(h = 0)

min(which(forecast.weightFaceallLONG$lower[,"95%"]>=2*weightFace.Wtsall[1]))/12 
min(which(forecast.weightFaceallLONG$mean>=2*weightFace.Wtsall[1]))/12 
min(which(forecast.weightFaceallLONG$upper[,"95%"]>=2*weightFace.Wtsall[1]))/12 

## percent change
decomp.weightFaceall <- decompose(weightFace.Wtsall)
(decomp.weightFaceall$trend[7]-decomp.weightFaceall$trend[66])/decomp.weightFaceall$trend[7] 
plot(decomp.weightFaceall)
# initial value
weightFace.Wtsall[1] 
# final value
weightFace.Wtsall[72] 

## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.weightFacenovel, main = "Novel", ylim = c(0.2, 0.72))
plot(forecast.weightFacerepeat, main = "Repeat", ylim = c(0.2, 0.72))
plot(forecast.weightFaceall, main = "All", ylim = c(0.2, 0.72))
par(op)



## Overlapping plots
plot(forecast.weightFacenovel, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightFaceall, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightFacerepeat, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("topleft", c("Repeat", "All", "Novel"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


### PLOTTED SUMMARIES ------------------------------------------

op <- par(mfrow = c(2,3))
## Age
plot(forecast.agenovel, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.ageall, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agerepeat, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Disability
plot(forecast.disnovel, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disall, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disrepeat, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Body Weight (Body)
plot(forecast.weightBodynovel, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes", xlim = c(2007, 2022),
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodyall, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes",xlim = c(2007, 2022),
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodyrepeat, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes",xlim = c(2007, 2022),
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Race
plot(forecast.racenovel, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.raceall, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racerepeat, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Skin-Tone
plot(forecast.skinnovel, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinall, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinrepeat, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Sexuality
plot(forecast.sexnovel, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexall, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexrepeat, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

par(op)



##############################################################################################################################
##############################################################################################################################
## EXPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
age.Etsnovel <- ddply(timeseriesAge[timeseriesAge$num.new=="0",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WEtsnovel <- ts(age.Etsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WEtsnovel
plot(age.WEtsnovel)

age.arimanovelE <- auto.arima(age.WEtsnovel, stepwise = FALSE, approximation = FALSE)
age.arimanovelE 

forecastE.agenovel <- forecast(age.arimanovelE, h = 60)
plot(forecastE.agenovel)

forecastE.agenovelLONG <- forecast(age.arimanovelE, h = 1800) # 150 years
plot(forecastE.agenovelLONG)
abline(h = 0)

min(which(forecastE.agenovelLONG$lower[,"95%"]<=0))/12
min(which(forecastE.agenovelLONG$mean<=0))/12 
min(which(forecastE.agenovelLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.agenovelLONG$upper[,"95%"]>=2*age.WEtsnovel[1]))/12 

## percent change
decompE.agenovel <- decompose(age.WEtsnovel)
(decompE.agenovel$trend[7]-decompE.agenovel$trend[114])/decompE.agenovel$trend[7] 
# initial value
age.WEtsnovel[1] 
# final value
age.WEtsnovel[120] 


## Fit to repeats
age.Etsrepeat <- ddply(timeseriesAge[timeseriesAge$num.new!="0",],
                      .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WEtsrepeat <- ts(age.Etsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WEtsrepeat
plot(age.WEtsrepeat)

age.arimarepeatE <- auto.arima(age.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
age.arimarepeatE 

forecastE.agerepeat <- forecast(age.arimarepeatE, h = 60)
plot(forecastE.agerepeat)

forecastE.agerepeatLONG <- forecast(age.arimarepeatE, h = 1800) # 150 years
plot(forecastE.agerepeatLONG)
abline(h = 0)

min(which(forecastE.agerepeatLONG$lower[,"95%"]<=0))/12
min(which(forecastE.agerepeatLONG$mean<=0))/12 
min(which(forecastE.agerepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.agerepeatLONG$upper[,"95%"]>=2*age.WEtsrepeat[1]))/12 

## percent change
decompE.agerepeat <- decompose(age.WEtsrepeat)
(decompE.agerepeat$trend[7]-decompE.agerepeat$trend[114])/decompE.agerepeat$trend[7] 
# initial value
age.WEtsrepeat[1] 
# final value
age.WEtsrepeat[120] 

## Fit to all
age.Etsall <- ddply(timeseriesAge,
                   .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WEtsall <- ts(age.Etsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WEtsall
plot(age.WEtsall)

age.arimaallE <- auto.arima(age.WEtsall, stepwise = FALSE, approximation = FALSE)
age.arimaallE 

forecastE.ageall <- forecast(age.arimaallE, h = 60)
plot(forecastE.ageall)

forecastE.ageallLONG <- forecast(age.arimaallE, h = 1800) # 150 years
plot(forecastE.ageallLONG)
abline(h = 0)

min(which(forecastE.ageallLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.ageallLONG$mean<=0))/12 
min(which(forecastE.ageallLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.ageallLONG$upper[,"95%"]>=2*age.WEtsall[1]))/12

## percent change
decompE.ageall <- decompose(age.WEtsall)
(decompE.ageall$trend[7]-decompE.ageall$trend[114])/decompE.ageall$trend[7] 
# initial value
age.WEtsall[1]
# final value
age.WEtsall[120]


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastE.agenovel, main = "Novel", ylim = c(0.1, 0.72))
plot(forecastE.agerepeat, main = "Repeat", ylim = c(0.1, 0.72))
plot(forecastE.ageall, main = "All", ylim = c(0.1, 0.72))
par(op)



## Overlapping plots
plot(forecastE.agenovel, ylim = c(0.1, 0.72), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.agerepeat, ylim = c(0.1, 0.72), main = "Explicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.ageall, ylim = c(0.1, 0.72), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 


## DISABILITY  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
dis.Etsnovel <- ddply(timeseriesDis[timeseriesDis$num.new=="0",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WEtsnovel <- ts(dis.Etsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WEtsnovel
plot(dis.WEtsnovel)

dis.arimanovelE <- auto.arima(dis.WEtsnovel, stepwise = FALSE, approximation = FALSE)
dis.arimanovelE 

forecastE.disnovel <- forecast(dis.arimanovelE, h = 60)
plot(forecastE.disnovel)

forecastE.disnovelLONG <- forecast(dis.arimanovelE, h = 1800) # 150 years
plot(forecastE.disnovelLONG)
abline(h = 0)

min(which(forecastE.disnovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.disnovelLONG$mean<=0))/12 
min(which(forecastE.disnovelLONG$upper[,"95%"]<=0))/12 

## percent change
decompE.disnovel <- decompose(dis.WEtsnovel)
(decompE.disnovel$trend[7]-decompE.disnovel$trend[114])/decompE.disnovel$trend[7]
# initial value
dis.WEtsnovel[1]
# final value
dis.WEtsnovel[120]


## Fit to repeats
dis.Etsrepeat <- ddply(timeseriesDis[timeseriesDis$num.new!="0",],
                      .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WEtsrepeat <- ts(dis.Etsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WEtsrepeat
plot(dis.WEtsrepeat)

dis.arimarepeatE <- auto.arima(dis.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
dis.arimarepeatE 

forecastE.disrepeat <- forecast(dis.arimarepeatE, h = 60)
plot(forecastE.disrepeat)

forecastE.disrepeatLONG <- forecast(dis.arimarepeatE, h = 1800) # 150 years
plot(forecastE.disrepeatLONG)
abline(h = 0)

min(which(forecastE.disrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.disrepeatLONG$mean<=0))/12 
min(which(forecastE.disrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.disrepeatLONG$upper[,"95%"]>=2*dis.WEtsrepeat[1]))/12 

## percent change
decompE.disrepeat <- decompose(dis.WEtsrepeat)
(decompE.disrepeat$trend[7]-decompE.disrepeat$trend[114])/decompE.disrepeat$trend[7] 
# initial value
dis.WEtsrepeat[1] 
# final value
dis.WEtsrepeat[120] 

## Fit to all
dis.Etsall <- ddply(timeseriesDis,
                   .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WEtsall <- ts(dis.Etsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WEtsall
plot(dis.WEtsall)

dis.arimaallE <- auto.arima(dis.WEtsall, stepwise = FALSE, approximation = FALSE)
dis.arimaallE 

forecastE.disall <- forecast(dis.arimaallE, h = 60)
plot(forecastE.disall)

forecastE.disallLONG <- forecast(dis.arimaallE, h = 1800) # 150 years
plot(forecastE.disallLONG)
abline(h = 0)

min(which(forecastE.disallLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.disallLONG$mean<=0))/12 
min(which(forecastE.disallLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.disallLONG$upper[,"95%"]>=2*dis.WEtsall[1]))/12 

## percent change
decompE.disall <- decompose(dis.WEtsall)
(decompE.disall$trend[7]-decompE.disall$trend[114])/decompE.disall$trend[7] 
# initial value
dis.WEtsall[1] 
# final value
dis.WEtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastE.disnovel, main = "Novel", ylim = c(0.15, 0.7))
plot(forecastE.disrepeat, main = "Repeat", ylim = c(0.15, 0.7))
plot(forecastE.disall, main = "All", ylim = c(0.15, 0.7))
par(op)



## Overlapping plots
plot(forecastE.disnovel, ylim = c(0.15, 0.7), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.disrepeat, ylim = c(0.15, 0.7), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.disall, ylim = c(0.15, 0.7), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## RACE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
race.Etsnovel <- ddply(timeseriesRace[timeseriesRace$num.new=="0",],
                      .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
tail(race.Etsnovel)
race.WEtsnovel <- ts(race.Etsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WEtsnovel
plot(race.WEtsnovel)

race.arimanovelE <- auto.arima(race.WEtsnovel, stepwise = FALSE, approximation = FALSE)
race.arimanovelE 

forecastE.racenovel <- forecast(race.arimanovelE, h = 60)
plot(forecastE.racenovel)

forecastE.racenovelLONG <- forecast(race.arimanovelE, h = 1800) # 150 years
plot(forecastE.racenovelLONG)
abline(h = 0)

min(which(forecastE.racenovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.racenovelLONG$mean<=0))/12 
min(which(forecastE.racenovelLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.racenovelLONG$upper[,"95%"]>=2*race.WEtsnovel[1]))/12 

## percent change
decompE.racenovel <- decompose(race.WEtsnovel)
(decompE.racenovel$trend[7]-decompE.racenovel$trend[114])/decompE.racenovel$trend[7]
# initial value
race.WEtsnovel[1] 
# final value
race.WEtsnovel[120]


## Fit to repeats
race.Etsrepeat <- ddply(timeseriesRace[timeseriesRace$num.new!="0",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WEtsrepeat <- ts(race.Etsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WEtsrepeat
plot(race.WEtsrepeat)

race.arimarepeatE <- auto.arima(race.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
race.arimarepeatE 

forecastE.racerepeat <- forecast(race.arimarepeatE, h = 60)
plot(forecastE.racerepeat)

forecastE.racerepeatLONG <- forecast(race.arimarepeatE, h = 1800) # 150 years
plot(forecastE.racerepeatLONG)
abline(h = 0)

min(which(forecastE.racerepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.racerepeatLONG$mean<=0))/12 
min(which(forecastE.racerepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.racerepeatLONG$upper[,"95%"]>=2*race.WEtsrepeat[1]))/12 

## percent change
decompE.racerepeat <- decompose(race.WEtsrepeat)
(decompE.racerepeat$trend[7]-decompE.racerepeat$trend[114])/decompE.racerepeat$trend[7] 
# initial value
race.WEtsrepeat[1]
# final value
race.WEtsrepeat[120]

## Fit to all
race.Etsall <- ddply(timeseriesRace,
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WEtsall <- ts(race.Etsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WEtsall
plot(race.WEtsall)

race.arimaallE <- auto.arima(race.WEtsall, stepwise = FALSE, approximation = FALSE)
race.arimaallE 

forecastE.raceall <- forecast(race.arimaallE, h = 60)
plot(forecastE.raceall)

forecastE.raceallLONG <- forecast(race.arimaallE, h = 1800) # 150 years
plot(forecastE.raceallLONG)
abline(h = 0)

min(which(forecastE.raceallLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.raceallLONG$mean<=0))/12 
min(which(forecastE.raceallLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.raceallLONG$upper[,"95%"]>=2*race.WEtsall[1]))/12 

## percent change
decompE.raceall <- decompose(race.WEtsall)
(decompE.raceall$trend[7]-decompE.raceall$trend[114])/decompE.raceall$trend[7] 
# initial value
race.WEtsall[1]
# final value
race.WEtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastE.racenovel, main = "Novel", ylim = c(-0.1, 0.5))
abline(h = 0, lty = 2)
plot(forecastE.racerepeat, main = "Repeat", ylim = c(-0.1, 0.5))
abline(h = 0, lty = 2)
plot(forecastE.raceall, main = "All", ylim = c(-0.1, 0.5))
abline(h = 0, lty = 2)
par(op)



## Overlapping plots
plot(forecastE.racenovel, ylim = c(-0.1, 0.5), main = "Explicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.racerepeat, ylim = c(-0.1, 0.5), main = "Explicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.raceall, ylim = c(-0.1, 0.5), main = "Explicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
abline(h = 0, lty = 2)
legend("bottomleft", c("All", "Repeat", "Novel"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## SEXUALITY  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
sex.Etsnovel <- ddply(timeseriesSex[timeseriesSex$num.new=="0",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WEtsnovel <- ts(sex.Etsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WEtsnovel
plot(sex.WEtsnovel)

sex.arimanovelE <- auto.arima(sex.WEtsnovel, stepwise = FALSE, approximation = FALSE)
sex.arimanovelE 

forecastE.sexnovel <- forecast(sex.arimanovelE, h = 60)
plot(forecastE.sexnovel)

forecastE.sexnovelLONG <- forecast(sex.arimanovelE, h = 1800) # 150 years
plot(forecastE.sexnovelLONG)
abline(h = 0)

min(which(forecastE.sexnovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.sexnovelLONG$mean<=0))/12 
min(which(forecastE.sexnovelLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.sexnovelLONG$upper[,"95%"]>=2*sex.WEtsnovel[1]))/12 

## percent change
decompE.sexnovel <- decompose(sex.WEtsnovel)
(decompE.sexnovel$trend[7]-decompE.sexnovel$trend[114])/decompE.sexnovel$trend[7] 
# initial value
sex.WEtsnovel[1] 
# final value
sex.WEtsnovel[120] 


## Fit to repeats
sex.Etsrepeat <- ddply(timeseriesSex[timeseriesSex$num.new!="0",],
                      .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WEtsrepeat <- ts(sex.Etsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WEtsrepeat
plot(sex.WEtsrepeat)

sex.arimarepeatE <- auto.arima(sex.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
sex.arimarepeatE 

forecastE.sexrepeat <- forecast(sex.arimarepeatE, h = 60)
plot(forecastE.sexrepeat)

forecastE.sexrepeatLONG <- forecast(sex.arimarepeatE, h = 1800) # 150 years
plot(forecastE.sexrepeatLONG)
abline(h = 0)

min(which(forecastE.sexrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.sexrepeatLONG$mean<=0))/12 
min(which(forecastE.sexrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.sexrepeatLONG$upper[,"95%"]>=2*sex.WEtsrepeat[1]))/12 

## percent change
decompE.sexrepeat <- decompose(sex.WEtsrepeat)
(decompE.sexrepeat$trend[7]-decompE.sexrepeat$trend[114])/decompE.sexrepeat$trend[7] 
# initial value
sex.WEtsrepeat[1] 
# final value
sex.WEtsrepeat[120] 

## Fit to all
sex.Etsall <- ddply(timeseriesSex,
                   .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WEtsall <- ts(sex.Etsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WEtsall
plot(sex.WEtsall)

sex.arimaallE <- auto.arima(sex.WEtsall, stepwise = FALSE, approximation = FALSE)
sex.arimaallE

forecastE.sexall <- forecast(sex.arimaallE, h = 60)
plot(forecastE.sexall)

forecastE.sexallLONG <- forecast(sex.arimaallE, h = 1800) # 150 years
plot(forecastE.sexallLONG)
abline(h = 0)

min(which(forecastE.sexallLONG$lower[,"95%"]<=0))/12
min(which(forecastE.sexallLONG$mean<=0))/12 
min(which(forecastE.sexallLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.sexallLONG$upper[,"95%"]>=2*sex.WEtsall[1]))/12 

## percent change
decompE.sexall <- decompose(sex.WEtsall)
(decompE.sexall$trend[7]-decompE.sexall$trend[114])/decompE.sexall$trend[7] 
# initial value
sex.WEtsall[1] 
# final value
sex.WEtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastE.sexnovel, main = "Novel", ylim = c(0, 0.8))
plot(forecastE.sexrepeat, main = "Repeat", ylim = c(0, 0.8))
plot(forecastE.sexall, main = "All", ylim = c(0, 0.8))
par(op)



## Overlapping plots
plot(forecastE.sexnovel, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.sexrepeat, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.sexall, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
abline(h = 0, lty = 2)
legend("bottomleft", c("Novel", "Repeat", "All"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 




## SKIN TONE  --------------------------------------------------------------------------------------------------------
## Fit to Num 0s
skin.Etsnovel <- ddply(timeseriesSkin[timeseriesSkin$num.new=="0",],
                      .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.Etsnovel <- rbind(skin.month.extra, skin.Etsnovel)
skin.Etsnovel <- skin.Etsnovel[order(skin.Etsnovel$month.cent),]
skin.Etsnovel

skin.WEtsnovel <- ts(skin.Etsnovel$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WEtsnovel
plot(skin.WEtsnovel)

# impute with seasonal decomposition and interpolation
skin.WEtsnovel <- na.seadec(skin.WEtsnovel, algorithm = "interpolation")
skin.WEtsnovel
plot(skin.WEtsnovel)

skin.arimanovelE <- auto.arima(skin.WEtsnovel, stepwise = FALSE, approximation = FALSE)
skin.arimanovelE

forecastE.skinnovel <- forecast(skin.arimanovelE, h = 60)
plot(forecastE.skinnovel)

forecastE.skinnovelLONG <- forecast(skin.arimanovelE, h = 1800) # 150 years
plot(forecastE.skinnovelLONG)
abline(h = 0)

min(which(forecastE.skinnovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.skinnovelLONG$mean<=0))/12 
min(which(forecastE.skinnovelLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.skinnovelLONG$upper[,"95%"]>=2*skin.WEtsnovel[1]))/12 

## percent change
decompE.skinnovel <- decompose(skin.WEtsnovel)
(decompE.skinnovel$trend[7]-decompE.skinnovel$trend[114])/decompE.skinnovel$trend[7] 
# initial value
skin.WEtsnovel[1] 
# final value
skin.WEtsnovel[120] 


## Fit to repeats
skin.Etsrepeat <- ddply(timeseriesSkin[timeseriesSkin$num.new!="0",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.Etsrepeat <- rbind(skin.month.extra, skin.Etsrepeat)
skin.Etsrepeat <- skin.Etsrepeat[order(skin.Etsrepeat$month.cent),]
skin.Etsrepeat

skin.WEtsrepeat <- ts(skin.Etsrepeat$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WEtsrepeat
plot(skin.WEtsrepeat)

# impute with seasonal decomposition and interpolation
skin.WEtsrepeat <- na.seadec(skin.WEtsrepeat, algorithm = "interpolation")
skin.WEtsrepeat
plot(skin.WEtsrepeat)

skin.arimarepeatE <- auto.arima(skin.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
skin.arimarepeatE 

forecastE.skinrepeat <- forecast(skin.arimarepeatE, h = 60)
plot(forecastE.skinrepeat)

forecastE.skinrepeatLONG <- forecast(skin.arimarepeatE, h = 1800) # 150 years
plot(forecastE.skinrepeatLONG)
abline(h = 0)

min(which(forecastE.skinrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.skinrepeatLONG$mean<=0))/12 
min(which(forecastE.skinrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.skinrepeatLONG$upper[,"95%"]>=2*skin.WEtsrepeat[1]))/12 

## percent change
decompE.skinrepeat <- decompose(skin.WEtsrepeat)
(decompE.skinrepeat$trend[7]-decompE.skinrepeat$trend[114])/decompE.skinrepeat$trend[7] 
# initial value
skin.WEtsrepeat[1] 
# final value
skin.WEtsrepeat[120] 

## Fit to all
skin.Etsall <- ddply(timeseriesSkin,
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.Etsall <- rbind(skin.month.extra, skin.Etsall)
skin.Etsall <- skin.Etsall[order(skin.Etsall$month.cent),]
skin.Etsall

skin.WEtsall <- ts(skin.Etsall$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WEtsall
plot(skin.WEtsall)

# impute with seasonal decomposition and interpolation
skin.WEtsall <- na.seadec(skin.WEtsall, algorithm = "interpolation")
skin.WEtsall
plot(skin.WEtsall)

skin.arimaallE <- auto.arima(skin.WEtsall, stepwise = FALSE, approximation = FALSE)
skin.arimaallE 

forecastE.skinall <- forecast(skin.arimaallE, h = 60)
plot(forecastE.skinall)

forecastE.skinallLONG <- forecast(skin.arimaallE, h = 1800) # 150 years
plot(forecastE.skinallLONG)
abline(h = 0)

min(which(forecastE.skinallLONG$lower[,"95%"]<=0))/12 
min(which(forecastE.skinallLONG$mean<=0))/12 
min(which(forecastE.skinallLONG$upper[,"95%"]<=0))/12 
min(which(forecastE.skinallLONG$upper[,"95%"]>=2*skin.WEtsall[1]))/12 

## percent change
decompE.skinall <- decompose(skin.WEtsall)
(decompE.skinall$trend[7]-decompE.skinall$trend[114])/decompE.skinall$trend[7] 
# initial value
skin.WEtsall[1] 
# final value
skin.WEtsall[120] 


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastE.skinnovel, main = "Novel", ylim = c(0.15, 0.4))
plot(forecastE.skinrepeat, main = "Repeat", ylim = c(0.15, 0.4))
plot(forecastE.skinall, main = "All", ylim = c(0.15, 0.4))
par(op)



## Overlapping plots
plot(forecastE.skinnovel, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.skinrepeat, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.skinall, ylim = c(0.15, 0.4), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("All", "Repeat", "All"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 


## BODY WEIGHT (BODY) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$num.new, timeseriesWeightBody$month) 
# not available prior to 1501.

## Fit to Novels
weightBody.Etsnovel <- ddply(timeseriesWeightBody[timeseriesWeightBody$num.new=="0",],
                            .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

weightBody.WEtsnovel <- ts(weightBody.Etsnovel$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.WEtsnovel
plot(weightBody.WEtsnovel)

weightBody.arimaEnovel <- auto.arima(weightBody.WEtsnovel, stepwise = FALSE, approximation = FALSE)
weightBody.arimaEnovel 

forecastEweightBodynovel <- forecast(weightBody.arimaEnovel, h = 60)
plot(forecastEweightBodynovel)

forecastEweightBodynovelLONG <- forecast(weightBody.arimaEnovel, h = 1800) # 150 years
plot(forecastEweightBodynovelLONG)
abline(h = 0)

min(which(forecastEweightBodynovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastEweightBodynovelLONG$mean<=0))/12 
min(which(forecastEweightBodynovelLONG$upper[,"95%"]<=0))/12 
min(which(forecastEweightBodynovelLONG$upper[,"95%"]>=2*weightBody.WEtsnovel[1]))/12 

## percent change
decompE.weightBodynovel <- decompose(weightBody.WEtsnovel)
decompE.weightBodynovel$trend
(decompE.weightBodynovel$trend[7]-decompE.weightBodynovel$trend[18])/decompE.weightBodynovel$trend[7] 
plot(decompE.weightBodynovel)
# initial value
weightBody.WEtsnovel[1]
# final value
weightBody.WEtsnovel[24] 

## Fit to Repeats
weightBody.Etsrepeat <- ddply(timeseriesWeightBody[timeseriesWeightBody$num.new!="0",],
                             .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

weightBody.WEtsrepeat <- ts(weightBody.Etsrepeat$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.WEtsrepeat
plot(weightBody.WEtsrepeat)

weightBody.arimaErepeat <- auto.arima(weightBody.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
weightBody.arimaErepeat 

forecastEweightBodyrepeat <- forecast(weightBody.arimaErepeat, h = 60)
plot(forecastEweightBodyrepeat)

forecastEweightBodyrepeatLONG <- forecast(weightBody.arimaErepeat, h = 1800) # 150 years
plot(forecastEweightBodyrepeatLONG)
abline(h = 0)

min(which(forecastEweightBodyrepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastEweightBodyrepeatLONG$mean<=0))/12 
min(which(forecastEweightBodyrepeatLONG$upper[,"95%"]<=0))/12 
min(which(forecastEweightBodyrepeatLONG$upper[,"95%"]>=2*weightBody.WEtsrepeat[1]))/12 

## percent change
decompE.weightBodyrepeat <- decompose(weightBody.WEtsrepeat)
(decompE.weightBodyrepeat$trend[7]-decompE.weightBodyrepeat$trend[18])/decompE.weightBodyrepeat$trend[7] 
plot(decompE.weightBodyrepeat)
# initial value
weightBody.WEtsrepeat[1] 
# final value
weightBody.WEtsrepeat[24]

## Fit to All
weightBody.Etsall <- ddply(timeseriesWeightBody[timeseriesWeightBody$month.cent>=133,],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

weightBody.WEtsall <- ts(weightBody.Etsall$weight.mean, frequency = 12, start = c(2015, 1), end = c(2016,12))
weightBody.WEtsall
plot(weightBody.WEtsall)

weightBody.arimaEall <- auto.arima(weightBody.WEtsall, stepwise = FALSE, approximation = FALSE)
weightBody.arimaEall

forecastEweightBodyall <- forecast(weightBody.arimaEall, h = 60)
plot(forecastEweightBodyall)

forecastEweightBodyallLONG <- forecast(weightBody.arimaEall, h = 1800) # 150 years
plot(forecastEweightBodyallLONG)
abline(h = 0)

min(which(forecastEweightBodyallLONG$lower[,"95%"]<=0))/12 
min(which(forecastEweightBodyallLONG$mean<=0))/12 
min(which(forecastEweightBodyallLONG$upper[,"95%"]<=0))/12
min(which(forecastEweightBodyallLONG$upper[,"95%"]>=2*weightBody.WEtsall[1]))/12 

## percent change
decompE.weightBodyall <- decompose(weightBody.WEtsall)
(decompE.weightBodyall$trend[7]-decompE.weightBodyall$trend[18])/decompE.weightBodyall$trend[7] 
plot(decompE.weightBodyall)
# initial value
weightBody.WEtsall[1] 
# final value
weightBody.WEtsall[24] 



## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastEweightBodynovel, main = "Novel", ylim = c(0.2, 1.1))
plot(forecastEweightBodyrepeat, main = "Repeat", ylim = c(0.2, 1.1))
plot(forecastEweightBodyall, main = "All", ylim = c(0.2, 1.1))
par(op)



## Overlapping plots
plot(forecastEweightBodynovel, ylim = c(0.2, 1.1), main = "Explicit Weight (Body) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastEweightBodyall, ylim = c(0.2, 1.1), main = "Explicit Weight (Body) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastEweightBodyrepeat, ylim = c(0.2, 1.1), main = "Explicit Weight (Body) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Repeat", "All", "Novel"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## BODY WEIGHT (FACE) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightFace$num.new, timeseriesWeightFace$month) 
# till 1004

## Fit to Novels
weightFace.Etsnovel <- ddply(timeseriesWeightFace[timeseriesWeightFace$num.new=="0",],
                            .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.Etsnovel$month.cent)

weightFace.WEtsnovel <- ts(weightFace.Etsnovel$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010, 4)) 
# ** ends early
weightFace.WEtsnovel
plot(weightFace.WEtsnovel)

weightFace.arimaEnovel <- auto.arima(weightFace.WEtsnovel, stepwise = FALSE, approximation = FALSE)
weightFace.arimaEnovel 

forecastEweightFacenovel <- forecast(weightFace.arimaEnovel, h = 60)
plot(forecastEweightFacenovel)

forecastEweightFacenovelLONG <- forecast(weightFace.arimaEnovel, h = 1800) # 150 years
plot(forecastEweightFacenovelLONG)
abline(h = 0)

min(which(forecastEweightFacenovelLONG$lower[,"95%"]<=0))/12 
min(which(forecastEweightFacenovelLONG$mean<=0))/12 
min(which(forecastEweightFacenovelLONG$upper[,"95%"]>=2*weightFace.WEtsnovel[1]))/12 
## note these numbers are from April, 2004.

## percent change
decompE.weightFacenovel <- decompose(weightFace.WEtsnovel)
(decompE.weightFacenovel$trend[7]-decompE.weightFacenovel$trend[66])/decompE.weightFacenovel$trend[7] 
plot(decompE.weightFacenovel)
# initial value
weightFace.WEtsnovel[1] 
# final value
weightFace.WEtsnovel[72] 


## Fit to Repeats
weightFace.Etsrepeat <- ddply(timeseriesWeightFace[timeseriesWeightFace$num.new!="0",],
                             .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.Etsrepeat$month.cent)

weightFace.WEtsrepeat <- ts(weightFace.Etsrepeat$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010,4)) ## ends early **
weightFace.WEtsrepeat
plot(weightFace.WEtsrepeat)

weightFace.arimaErepeat <- auto.arima(weightFace.WEtsrepeat, stepwise = FALSE, approximation = FALSE)
weightFace.arimaErepeat 

forecastEweightFacerepeat <- forecast(weightFace.arimaErepeat, h = 60)
plot(forecastEweightFacerepeat)

forecastEweightFacerepeatLONG <- forecast(weightFace.arimaErepeat, h = 1800) # 150 years
plot(forecastEweightFacerepeatLONG)
abline(h = 0)

min(which(forecastEweightFacerepeatLONG$lower[,"95%"]<=0))/12 
min(which(forecastEweightFacerepeatLONG$mean<=0))/12 
min(which(forecastEweightFacerepeatLONG$upper[,"95%"]>=2*weightFace.WEtsrepeat[1]))/12 

## percent change
decompE.weightFacerepeat <- decompose(weightFace.WEtsrepeat)
(decompE.weightFacerepeat$trend[7]-decompE.weightFacerepeat$trend[66])/decompE.weightFacerepeat$trend[7] 
plot(decompE.weightFacerepeat)
# initial value
weightFace.WEtsrepeat[1] 
# final value
weightFace.WEtsrepeat[72]


## Fit to All
weightFace.Etsall <- ddply(timeseriesWeightFace[timeseriesWeightFace$month.cent<=76,],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.Etsall$month.cent)

weightFace.WEtsall <- ts(weightFace.Etsall$weight.mean, frequency = 12, start = c(2004, 5), end = c(2010,4))
weightFace.WEtsall
plot(weightFace.WEtsall)

weightFace.arimaEall <- auto.arima(weightFace.WEtsall, stepwise = FALSE, approximation = FALSE)
weightFace.arimaEall 

forecastEweightFaceall <- forecast(weightFace.arimaEall, h = 60)
plot(forecastEweightFaceall)

forecastEweightFaceallLONG <- forecast(weightFace.arimaEall, h = 1800) # 150 years
plot(forecastEweightFaceallLONG)
abline(h = 0)

min(which(forecastEweightFaceallLONG$lower[,"95%"]<=0))/12
min(which(forecastEweightFaceallLONG$mean<=0))/12 
min(which(forecastEweightFaceallLONG$upper[,"95%"]>=2*weightFace.WEtsall[1]))/12

## percent change
decompE.weightFaceall <- decompose(weightFace.WEtsall)
(decompE.weightFaceall$trend[7]-decompE.weightFaceall$trend[66])/decompE.weightFaceall$trend[7]
plot(decompE.weightFaceall)
# initial value
weightFace.WEtsall[1]
# final value
weightFace.WEtsall[72]

## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecastEweightFacenovel, main = "Novel", ylim = c(0.5, 1.5))
plot(forecastEweightFacerepeat, main = "Repeat", ylim = c(0.5, 1.5))
plot(forecastEweightFaceall, main = "All", ylim = c(0.5, 1.5))
par(op)



## Overlapping plots
plot(forecastEweightFacenovel, ylim = c(0.5, 1.5), main = "Explicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastEweightFaceall, ylim = c(0.5, 1.5), main = "Explicit Weight (Face) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastEweightFacerepeat, ylim = c(0.5, 1.5), main = "Explicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("topleft", c("Repeat", "All", "Novel"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## SUMMARIES -----------------
## (1) Model parameters ---------
forecast.agenovel$model # Age Implicit (0,1,1) + drift
forecastE.agenovel$model # Age Explicit (0,1,2)(1,0,0)
forecast.disnovel$model # Disability Implicit (0,1,1) 
forecastE.disnovel$model # Disability Explicit (0,1,1)(0,0,1) + drift
forecast.racenovel$model # Race Implicit (2,1,3)
forecastE.racenovel$model # Race Explicit (0,1,1)(2,0,0)
forecast.sexnovel$model # Sexuality Implicit (0,1,3)(2,0,0)
forecastE.sexnovel$model # Sexuality Explicit (1,1,1)(2,0,0)
forecast.skinnovel$model # Skin-Tone Implicit (0,1,2) 
forecastE.skinnovel$model # Skin-Tone Explicit (1,0,1)(1,0,1)
forecast.weightBodynovel$model # Weight Implicit (Body) (0,1,0)
forecast.weightFacenovel$model # Weight Implicit (Face) (0,1,1)
forecastEweightBodynovel$model # Weight Explicit (Body) (0,1,1)
forecastEweightFacenovel$model # Weight Explicit (Face) (0,1,1)

## (2) Forecasts to neutrality ----------
## EXPLICIT --------
## AGE
min(which(forecastE.agenovelLONG$lower[,"95%"]<=0))/12 # 11.75 (11 yrs, 9 months)
min(which(forecastE.agenovelLONG$mean<=0))/12 # NA
min(which(forecastE.agenovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.agenovelLONG$upper[,"95%"]>=2*age.WEtsnovel[1]))/12 # 32.91667 (32 yrs, 11 mos)

## DISABILITY
min(which(forecastE.disnovelLONG$lower[,"95%"]<=0))/12 # 17.833 (17 yrs, 10 mos)
min(which(forecastE.disnovelLONG$mean<=0))/12 # 25.667 (25 yrs, 8 mos)
min(which(forecastE.disnovelLONG$upper[,"95%"]<=0))/12 # 34.5833 (34 yrs, 7 mos)
min(which(forecastE.disnovelLONG$upper[,"95%"]>=2*dis.WEtsnovel[1]))/12 # NA

## RACE
min(which(forecastE.racenovelLONG$lower[,"95%"]<=0))/12 # 4.5 (4 yr, 6 mos)
min(which(forecastE.racenovelLONG$mean<=0))/12 # NA
min(which(forecastE.racenovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.racenovelLONG$upper[,"95%"]>=2*race.WEtsnovel[1]))/12 # 34.0833 (34 yrs, 1 mos)

## SEXUALITY
min(which(forecastE.sexnovelLONG$lower[,"95%"]<=0))/12 # 6.41667 (6 yrs, 5 mos)
min(which(forecastE.sexnovelLONG$mean<=0))/12 # NA
min(which(forecastE.sexnovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.sexnovelLONG$upper[,"95%"]>=2*sex.WEtsnovel[1]))/12 # 30.0833 (30 yrs, 1 mos)

## SKIN
min(which(forecastE.skinnovelLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecastE.skinnovelLONG$mean<=0))/12 # NA
min(which(forecastE.skinnovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.skinnovelLONG$upper[,"95%"]>=2*skin.WEtsnovel[1]))/12 # NA

## BODY WEIGHT
min(which(forecastEweightBodynovelLONG$lower[,"95%"]<=0))/12 #  46 yrs
min(which(forecastEweightBodynovelLONG$mean<=0))/12 # NA
min(which(forecastEweightBodynovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastEweightBodynovelLONG$upper[,"95%"]>=2*weightBody.WEtsnovel[1]))/12 # 43.833

## BODY WEIGHT (Face)
min(which(forecastEweightFacenovelLONG$lower[,"95%"]<=0))/12 #  33.75 (33 yrs, 9 mos)
min(which(forecastEweightFacenovelLONG$mean<=0))/12 # NA
min(which(forecastEweightFacenovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastEweightFacenovelLONG$upper[,"95%"]>=2*weightFace.WEtsnovel[1]))/12 # 60.33


## IMPLICIT --------
## AGE
min(which(forecast.agenovelLONG$lower[,"95%"]<=0))/12 # 77
min(which(forecast.agenovelLONG$mean<=0))/12 # 106.4167 (106 yrs, 5 mos)
min(which(forecast.agenovelLONG$upper[,"95%"]<=0))/12 # 146 yrs, 9 mos
min(which(forecast.agenovelLONG$upper[,"95%"]>=2*age.Wtsnovel[1]))/12 # NA

## DISABILITY
min(which(forecast.disnovelLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disnovelLONG$mean<=0))/12 # NA
min(which(forecast.disnovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disnovelLONG$upper[,"95%"]>=2*dis.Wtsnovel[1]))/12 # NA

## RACE
min(which(forecast.racenovelLONG$lower[,"95%"]<=0))/12 # 128 yrs, 6 mos
min(which(forecast.racenovelLONG$mean<=0))/12 # NA
min(which(forecast.racenovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.racenovelLONG$upper[,"95%"]>=2*race.Wtsnovel[1]))/12 # NA

## SEXUALITY
min(which(forecast.sexnovelLONG$lower[,"95%"]<=0))/12 # 11.91667 (11 yrs, 11 mos)
min(which(forecast.sexnovelLONG$mean<=0))/12 # NA
min(which(forecast.sexnovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexnovelLONG$upper[,"95%"]>=2*sex.Wtsnovel[1]))/12 # 50 yrs, 3 mos

## SKIN
min(which(forecast.skinnovelLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinnovelLONG$mean<=0))/12 # NA
min(which(forecast.skinnovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinnovelLONG$upper[,"95%"]>=2*skin.Wtsnovel[1]))/12 # NA

## BODY WEIGHT (BODY)
min(which(forecast.weightBodynovelLONG$lower[,"95%"]<=0))/12 # 20.0833 (20 yrs, 1 mos)
min(which(forecast.weightBodynovelLONG$mean<=0))/12 # NA
min(which(forecast.weightBodynovelLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodynovelLONG$upper[,"95%"]>=2*weightBody.Wtsnovel[1]))/12 # 13 yrs, 2 mos

## BODY WEIGHT (FACE) ** from Apr 2010
min(which(forecast.weightFacenovelLONG$lower[,"95%"]<=0))/12 # 103 yrs, 2 mos
min(which(forecast.weightFacenovelLONG$mean<=0))/12 # NA
min(which(forecast.weightFacenovelLONG$upper[,"95%"]>=2*weightFace.Wtsnovel[1]))/12 # 54 yrs, 6 mos

## (3) Percentage changes from trend values ----------
## EXPLICIT -------
## AGE
(decompE.agenovel$trend[7] - decompE.agenovel$trend[114])/decompE.agenovel$trend[7] # 29.55% decrease
## DIS
(decompE.disnovel$trend[7] - decompE.disnovel$trend[114])/decompE.disnovel$trend[7] # 22.59% decrease
## RACE
(decompE.racenovel$trend[7] - decompE.racenovel$trend[114])/decompE.racenovel$trend[7] # 33.73% decrease
## SEX
(decompE.sexnovel$trend[7] - decompE.sexnovel$trend[114])/decompE.sexnovel$trend[7] # 44.82% decrease
## SKIN
(decompE.skinnovel$trend[7] - decompE.skinnovel$trend[114])/decompE.skinnovel$trend[7] # 20.49% decrease
## WEIGHT (Body)
(decompE.weightBodynovel$trend[7] - decompE.weightBodynovel$trend[18])/decompE.weightBodynovel$trend[7] # 7.34% decrease
## WEIGHT (Face)
(decompE.weightFacenovel$trend[7] - decompE.weightFacenovel$trend[66])/decompE.weightFacenovel$trend[7] # 18.71% decrease


## IMPLICIT -----
## AGE
(decomp.agenovel$trend[7] - decomp.agenovel$trend[114])/decomp.agenovel$trend[7] # 7.60% decrease
## DIS
(decomp.disnovel$trend[7] - decomp.disnovel$trend[114])/decomp.disnovel$trend[7] # 3.70% decrease
## RACE
(decomp.racenovel$trend[7] - decomp.racenovel$trend[114])/decomp.racenovel$trend[7] # 15.74% decrease
## SEX
(decomp.sexnovel$trend[7] - decomp.sexnovel$trend[114])/decomp.sexnovel$trend[7] # 31.71% decrease
## SKIN
(decomp.skinnovel$trend[7] - decomp.skinnovel$trend[114])/decomp.skinnovel$trend[7] # 14.08% decrease
## WEIGHT
# Body
(decomp.weightBodynovel$trend[7] - decomp.weightBodynovel$trend[18])/decomp.weightBodynovel$trend[7] # increased by 11.60%
# Face
(decomp.weightFacenovel$trend[7] - decomp.weightFacenovel$trend[66])/decomp.weightFacenovel$trend[7] # increased by 15.28%

## (4) First and Last raw values ----------
## EXPLICIT -------
## AGE
age.WEtsnovel[1]; age.WEtsnovel[120] # 0.50, 0.43
## DIS
dis.WEtsnovel[1]; dis.WEtsnovel[120] # 0.71, 0.44
## RACE
race.WEtsnovel[1]; race.WEtsnovel[120] # 0.30, 0.19
## SEX
sex.WEtsnovel[1]; sex.WEtsnovel[120] # 0.57, 0.38
## SKIN
skin.WEtsnovel[1]; skin.WEtsnovel[120] # 0.33, 0.20
## WEIGHT
weightBody.WEtsnovel[1]; weightBody.WEtsnovel[24] # 0.83, 0.83
weightFace.WEtsnovel[1]; weightFace.WEtsnovel[72] # 1.19, 1.07

## IMPLICIT -----
## AGE
age.Wtsnovel[1]; age.Wtsnovel[120] # 0.53, 0.48
## DIS
dis.Wtsnovel[1]; dis.Wtsnovel[120] # 0.61, 0.62
## RACE
race.Wtsnovel[1]; race.Wtsnovel[120] # 0.35, 0.33
## SEX
sex.Wtsnovel[1]; sex.Wtsnovel[120] # 0.34, 0.21
## SKIN
skin.Wtsnovel[1]; skin.Wtsnovel[120] # 0.36, 0.34
## WEIGHT
# Body
weightBody.Wtsnovel[1]; weightBody.Wtsnovel[24] # 0.51, 0.56
# Face
weightFace.Wtsnovel[1]; weightFace.Wtsnovel[72] # 0.44, 0.50
