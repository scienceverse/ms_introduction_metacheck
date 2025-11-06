## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 20th, 2018

####################################################################################################################################

## Timeseries by demographic sub-groups

####################################################################################################################################

## Load data object
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


## IMPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE (by age groups) --------------------------------------------------------------------------------------------------------
# first, create age groups for subsetting the data
timeseriesAge$agecats <- cut(timeseriesAge$age.new, c(0,25,35,45,55,100))
levels(timeseriesAge$agecats) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5599")
table(timeseriesAge$agecats, timeseriesAge$month.cent) # now all age categories for each year
table(timeseriesAge$agecats)

## Fit to 0 - 25
age.ts0024 <- ddply(timeseriesAge[timeseriesAge$agecats=="age0024",],
               .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wts0024 <- ts(age.ts0024$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts0024
plot(age.Wts0024)

age.arima0024 <- auto.arima(age.Wts0024, stepwise = FALSE, approximation = FALSE)
age.arima0024 # parameters (0,1,1)(2,0,0)[12] 

forecast.age0024 <- forecast(age.arima0024, h = 60)
plot(forecast.age0024)

forecast.age0024LONG <- forecast(age.arima0024, h = 1800) # 150 years
plot(forecast.age0024LONG)
abline(h = 0)

min(which(forecast.age0024LONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.age0024LONG$mean<=0))/12 # NA
min(which(forecast.age0024LONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.age0024 <- decompose(age.Wts0024)
(decomp.age0024$trend[7]-decomp.age0024$trend[114])/decomp.age0024$trend[7] # 6.54%
# initial value
age.Wts0024[1] # 0.44
# final value
age.Wts0024[120] # 0.42


## Fit to 25 - 34
age.ts2534 <- ddply(timeseriesAge[timeseriesAge$agecats=="age2534",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wts2534 <- ts(age.ts2534$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts2534
plot(age.Wts2534)

age.arima2534 <- auto.arima(age.Wts2534, stepwise = FALSE, approximation = FALSE)
age.arima2534 # parameters (0,1,1)(1,0,0)

forecast.age2534 <- forecast(age.arima2534, h = 60)
plot(forecast.age2534)

forecast.age2534LONG <- forecast(age.arima2534, h = 1800) # 150 years
plot(forecast.age2534LONG)
abline(h = 0)

min(which(forecast.age2534LONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.age2534LONG$mean<=0))/12 # NA
min(which(forecast.age2534LONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.age2534 <- decompose(age.Wts2534)
(decomp.age2534$trend[7]-decomp.age2534$trend[114])/decomp.age2534$trend[7] # 6.79%
# initial value
age.Wts2534[1] # 0.49
# final value
age.Wts2534[120] # 0.39

## Fit to 35 - 44
age.ts3544 <- ddply(timeseriesAge[timeseriesAge$agecats=="age3544",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wts3544 <- ts(age.ts3544$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts3544
plot(age.Wts3544)

age.arima3544 <- auto.arima(age.Wts3544, stepwise = FALSE, approximation = FALSE)
age.arima3544 # parameters (0,0,0) with non-zero mean 

forecast.age3544 <- forecast(age.arima3544, h = 60)
plot(forecast.age3544)

forecast.age3544LONG <- forecast(age.arima3544, h = 1800) # 150 years
plot(forecast.age3544LONG)
abline(h = 0)

min(which(forecast.age3544LONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.age3544LONG$mean<=0))/12 # NA
min(which(forecast.age3544LONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.age3544 <- decompose(age.Wts3544)
(decomp.age3544$trend[7]-decomp.age3544$trend[114])/decomp.age3544$trend[7] # 3.17%
# initial value
age.Wts3544[1] # 0.45
# final value
age.Wts3544[120] # 0.47

## Fit to 45 - 54
age.ts4554 <- ddply(timeseriesAge[timeseriesAge$agecats=="age4554",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wts4554 <- ts(age.ts4554$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts4554
plot(age.Wts4554)

age.arima4554 <- auto.arima(age.Wts4554, stepwise = FALSE, approximation = FALSE)
age.arima4554 # parameters (0,1,1)(1,0,1)

forecast.age4554 <- forecast(age.arima4554, h = 60)
plot(forecast.age4554)


forecast.age4554LONG <- forecast(age.arima4554, h = 1800) # 150 years
plot(forecast.age4554LONG)
abline(h = 0)

min(which(forecast.age4554LONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.age4554LONG$mean<=0))/12 # NA
min(which(forecast.age4554LONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.age4554 <- decompose(age.Wts4554)
(decomp.age4554$trend[7]-decomp.age4554$trend[114])/decomp.age4554$trend[7] # 1.76%
# initial value
age.Wts4554[1] # 0.45
# final value
age.Wts4554[120] # 0.43


## Fit to 55 +
age.ts5599 <- ddply(timeseriesAge[timeseriesAge$agecats=="age5599",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wts5599 <- ts(age.ts5599$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts5599
plot(age.Wts5599)

age.arima5599 <- auto.arima(age.Wts5599, stepwise = FALSE, approximation = FALSE)
age.arima5599 # parameters (0,0,0) with non-zero mean 

forecast.age5599 <- forecast(age.arima5599, h = 60)
plot(forecast.age5599)

forecast.age5599LONG <- forecast(age.arima5599, h = 1800) # 150 years
plot(forecast.age5599LONG)
abline(h = 0)

min(which(forecast.age5599LONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.age5599LONG$mean<=0))/12 # NA
min(which(forecast.age5599LONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.age5599 <- decompose(age.Wts5599)
(decomp.age5599$trend[7]-decomp.age5599$trend[114])/decomp.age5599$trend[7] # +2.53%
# initial value
age.Wts5599[1] # 0.47
# final value
age.Wts5599[120] # 0.44



## Summary with plots
op <- par(mfrow = c(2,3))
plot(forecast.age0024, main = "24 years and Under", ylim = c(0.3, 0.6))
plot(forecast.age2534, main = "25 - 34 years", ylim = c(0.3, 0.6))
plot(forecast.age3544, main = "35 - 44 years", ylim = c(0.3, 0.6))
plot(forecast.age4554, main = "45 - 54 years", ylim = c(0.3, 0.6))
plot(forecast.age5599, main = "55 years and Over", ylim = c(0.3, 0.6))
par(op)


## Overlapping plots
plot(forecast.age5599, ylim = c(0.3, 0.6), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.age0024, ylim = c(0.3, 0.6), main = "Implicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.age3544, ylim = c(0.3, 0.6), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

legend("topleft", c("35 - 44", "Under 24", "Over 55"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 





## DISABILITY (by disability status) --------------------------------------------------------------------------------------------------------
table(timeseriesDis$dis.new, timeseriesDis$month.cent) # have sufficient data from 2007 onward
table(timeseriesDis$dis.new)

## Fit to Disabled
dis.tsDis <- ddply(timeseriesDis[timeseriesDis$dis.new=="Yes",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.WtsDis <- ts(dis.tsDis$weight.mean, frequency = 12, start = c(2007, 1))
dis.WtsDis
plot(dis.WtsDis)

dis.arimaDis <- auto.arima(dis.WtsDis, stepwise = FALSE, approximation = FALSE)
dis.arimaDis # parameters (0,1,2) 

forecast.disDis <- forecast(dis.arimaDis, h = 60)
plot(forecast.disDis)

forecast.disDisLONG <- forecast(dis.arimaDis, h = 1800) # 150 years
plot(forecast.disDisLONG)
abline(h = 0)

min(which(forecast.disDisLONG$lower[,"95%"]<=0)) # NA
min(which(forecast.disDisLONG$mean<=0)) # NA
min(which(forecast.disDisLONG$upper[,"95%"]<=0)) # NA

## percent change
decomp.disDis <- decompose(dis.WtsDis)
(decomp.disDis$trend[7]-decomp.disDis$trend[114])/decomp.disDis$trend[7] # 9.08%
# initial value
dis.WtsDis[1] # 0.42
# final value
dis.WtsDis[120] # 0.43


## Fit to Not disabled
dis.tsNdis <- ddply(timeseriesDis[timeseriesDis$dis.new=="No",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.WtsNdis <- ts(dis.tsNdis$weight.mean, frequency = 12, start = c(2007, 1))
dis.WtsNdis
plot(dis.WtsNdis)

dis.arimaNdis <- auto.arima(dis.WtsNdis, stepwise = FALSE, approximation = FALSE)
dis.arimaNdis # parameters (1,1,1)

forecast.disNdis <- forecast(dis.arimaNdis, h = 60)
plot(forecast.disNdis)

forecast.disNdisLONG <- forecast(dis.arimaNdis, h = 1800) # 150 years
plot(forecast.disNdisLONG)
abline(h = 0)

min(which(forecast.disNdisLONG$lower[,"95%"]<=0)) # NA
min(which(forecast.disNdisLONG$mean<=0)) # NA
min(which(forecast.disNdisLONG$upper[,"95%"]<=0)) # NA

## percent change
decomp.disNdis <- decompose(dis.WtsNdis)
(decomp.disNdis$trend[7]-decomp.disNdis$trend[114])/decomp.disNdis$trend[7] # 0.69%
# initial value
dis.WtsNdis[1] # 0.53
# final value
dis.WtsNdis[120] # 0.54


## Summary with plots
op <- par(mfrow = c(1,2))
plot(forecast.disDis, main = "Disabled", ylim = c(0.25, 0.6))
plot(forecast.disNdis, main = "Not disabled", ylim = c(0.25, 0.6))
par(op)


## Overlapping plots
plot(forecast.disNdis, ylim = c(0.25, 0.6), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disDis, ylim = c(0.25, 0.6), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
legend("bottomleft", c("Disabled", "Not disabled"), col = c(rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 




## RACE (by race groups) --------------------------------------------------------------------------------------------------------
table(timeseriesRace$ethnic.race.new, timeseriesRace$month.cent) # have sufficient data from 2007 onward
table(timeseriesRace$ethnic.race.new)

## Fit to White
race.tsWhite <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="White",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.WtsWhite <- ts(race.tsWhite$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsWhite
plot(race.WtsWhite)

race.arimaWhite <- auto.arima(race.WtsWhite, stepwise = FALSE, approximation = FALSE)
race.arimaWhite # parameters (0,1,2)

forecast.raceWhite <- forecast(race.arimaWhite, h = 60)
plot(forecast.raceWhite)

forecast.raceWhiteLONG <- forecast(race.arimaWhite, h = 1800) # 150 years
plot(forecast.raceWhiteLONG)
abline(h = 0)

min(which(forecast.raceWhiteLONG$lower[,"95%"]<=0))/12 # 123.5833
min(which(forecast.raceWhiteLONG$mean<=0))/12 # NA
min(which(forecast.raceWhiteLONG$upper[,"95%"]<=0)) # NA
min(which(forecast.raceWhiteLONG$upper[,"95%"]>=2*race.WtsWhite[1]))/12 # >150

## percent change
decomp.raceWhite <- decompose(race.WtsWhite)
(decomp.raceWhite$trend[7]-decomp.raceWhite$trend[114])/decomp.raceWhite$trend[7] # 15.27%
# initial value
race.WtsWhite[1] # 0.40
# final value
race.WtsWhite[120] # 0.36


## Fit to Asian
race.tsAsian <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="Asian",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.WtsAsian <- ts(race.tsAsian$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsAsian
plot(race.WtsAsian)

decomp.race.Asian <- decompose(race.WtsAsian)

race.arimaAsian <- auto.arima(race.WtsAsian, stepwise = FALSE, approximation = FALSE)
race.arimaAsian # parameters (0,1,1)

forecast.raceAsian <- forecast(race.arimaAsian, h = 60)
plot(forecast.raceAsian)

forecast.raceAsianLONG <- forecast(race.arimaAsian, h = 1800) # 150 years
plot(forecast.raceAsianLONG)
abline(h = 0)

min(which(forecast.raceAsianLONG$lower[,"95%"]<=0))/12 # 135.5
min(which(forecast.raceAsianLONG$mean<=0)) # NA
min(which(forecast.raceAsianLONG$upper[,"95%"]<=0)) # NA
min(which(forecast.raceAsianLONG$upper[,"95%"]>=2*race.WtsAsian[1]))/12 # >150

## percent change
decomp.raceAsian <- decompose(race.WtsAsian)
(decomp.raceAsian$trend[7]-decomp.raceAsian$trend[114])/decomp.raceAsian$trend[7] # 14.83%
# initial value
race.WtsAsian[1] # 0.32
# final value
race.WtsAsian[120] # 0.31



## Fit to Black/African American
race.tsBlack <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="Black or African American",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.WtsBlack <- ts(race.tsBlack$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsBlack
plot(race.WtsBlack)

decomp.race.Black <- decompose(race.WtsBlack)

race.arimaBlack <- auto.arima(race.WtsBlack, stepwise = FALSE, approximation = FALSE)
race.arimaBlack # parameters (0,0,1)(1,0,0)[12] with non-zero mean 

forecast.raceBlack <- forecast(race.arimaBlack, h = 60)
plot(forecast.raceBlack)

forecast.raceBlackLONG <- forecast(race.arimaBlack, h = 1800) # 150 years
plot(forecast.raceBlackLONG)
abline(h = 0)

min(which(forecast.raceBlackLONG$lower[,"95%"]<=0)) #NA
min(which(forecast.raceBlackLONG$mean<=0)) # NA
min(which(forecast.raceBlackLONG$upper[,"95%"]<=0))/12 # 1 mo
min(which(forecast.raceBlackLONG$upper[,"95%"]>=2*race.WtsBlack[1])) # NA

## percent change
decomp.raceBlack <- decompose(race.WtsBlack)
(decomp.raceBlack$trend[7]-decomp.raceBlack$trend[114])/decomp.raceBlack$trend[7] # 3.61%
# initial value
race.WtsBlack[1] # -0.089
# final value
race.WtsBlack[120] # 0.0093



## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.raceWhite, main = "White", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
plot(forecast.raceBlack, main = "Black", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
plot(forecast.raceAsian, main = "Asian", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
par(op)


## Overlapping plots
op <- par(mar=c(2.5,7,2.5,2.5))
plot(forecast.raceWhite, ylim = c(-0.15, 0.5), xlim = c(2007,2018),
     main = "",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
box()
axis(1, at = c(2007:2018), labels = c(2007:2018), cex.axis = 1.5)
axis(2, at = seq(-0.15, 0.5, by = 0.1), labels = seq(-0.15, 0.5, by = 0.1), las = 2, cex.axis = 1.5)
title(ylab="IAT D-Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.raceBlack, ylim = c(-0.15, 0.5), xlim = c(2007, 2018), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
legend("left", c("Black", "White"), col = c(rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 
par(op)

## Overlapping plots
op <- par(mar=c(2.5,7,2.5,2.5))
plot(forecast.raceWhite, ylim = c(-0.15, 0.5), xlim = c(2007,2018),
     main = "",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.raceWhite$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2018), labels = c(2007:2018), cex.axis = 1.5)
axis(2, at = seq(-0.15, 0.5, by = 0.1), labels = seq(-0.15, 0.5, by = 0.1), las = 2, cex.axis = 1.5)
title(ylab="IAT D-Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.raceBlack, ylim = c(-0.15, 0.5), xlim = c(2007, 2018), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.race.Black$trend, col =  rgb(1,0.3,0), lwd = 3)
par(new = TRUE)
plot(forecast.raceAsian, ylim = c(-0.15, 0.5), main = "", xlim = c(2007, 2018), axes = FALSE,
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = "darkgrey", fcol = rgb(0,0,1, alpha = 0.2))
lines(decomp.race.Asian$trend, col =  rgb(0,0.3,1), lwd = 3)
abline(h = 0, lty = 2)
text(2010, 0.2, labels = "White", cex = 1.3)
text(2010, 0.15, labels = "Asian", cex = 1.3)
text(2010, 0.1, labels = "Black", cex = 1.3)
lines(x = c(2008, 2009), y = c(0.2, 0.2), lwd = 4, col = rgb(0,0.5,0.3, alpha = 0.8))
lines(x = c(2008, 2009), y = c(0.15, 0.15), lwd = 4, col = rgb(0,0.3,1, alpha = 0.8))
lines(x = c(2008, 2009), y = c(0.1, 0.1), lwd = 4, col = rgb(1,0.3,0, alpha = 0.8))







## SEXUALITY (by sexual orientation) --------------------------------------------------------------------------------------------------------
table(timeseriesSex$sexuality.new, timeseriesSex$month.cent) # have sufficient data from 2007 onward
table(timeseriesSex$sexuality.new)

## Fit to Straight
sex.tsStraight <- ddply(timeseriesSex[timeseriesSex$sexuality.new=="Straight",],
                   .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.WtsStraight <- ts(sex.tsStraight$weight.mean, frequency = 12, start = c(2007, 1))
sex.WtsStraight
plot(sex.WtsStraight)

decomp.sex.Straight <- decompose(sex.WtsStraight)

sex.arimaStraight <- auto.arima(sex.WtsStraight, stepwise = FALSE, approximation = FALSE)
sex.arimaStraight # parameters (0,1,1)(2,0,0) with drift

forecast.sexStraight <- forecast(sex.arimaStraight, h = 60)
plot(forecast.sexStraight)

forecast.sexStraightLONG <- forecast(sex.arimaStraight, h = 1800) # 150 years
plot(forecast.sexStraightLONG)
abline(h = 0)

min(which(forecast.sexStraightLONG$lower[,"95%"]<=0))/12 # 10.1667
min(which(forecast.sexStraightLONG$mean<=0))/12 # 20.25
min(which(forecast.sexStraightLONG$upper[,"95%"]<=0))/12 # 41.91667

## percent change
decomp.sexStraight <- decompose(sex.WtsStraight)
(decomp.sexStraight$trend[7]-decomp.sexStraight$trend[114])/decomp.sexStraight$trend[7] # 24.91%
# initial value
sex.WtsStraight[1] # 0.43
# final value
sex.WtsStraight[120] # 0.27




## Fit to Gay/Lesbian
sex.tsLG <- ddply(timeseriesSex[timeseriesSex$sexuality.new=="Gay/Lesbian",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.WtsLG <- ts(sex.tsLG$weight.mean, frequency = 12, start = c(2007, 1))
sex.WtsLG
plot(sex.WtsLG)

decomp.sex.LG <- decompose(sex.WtsLG)

sex.arimaLG <- auto.arima(sex.WtsLG, stepwise = FALSE, approximation = FALSE)
sex.arimaLG # parameters (2,1,3) with drift

forecast.sexLG <- forecast(sex.arimaLG, h = 60)
plot(forecast.sexLG)

forecast.sexLGLONG <- forecast(sex.arimaLG, h = 1800) # 150 years
plot(forecast.sexLGLONG)
abline(h = 0)
abline(h = 2*sex.WtsLG[1])

min(which(forecast.sexLGLONG$lower[,"95%"]<=2*sex.WtsLG[1]))/12 # 3.5833
min(which(forecast.sexLGLONG$mean<=2*sex.WtsLG[1]))/12 # 10.25
min(which(forecast.sexLGLONG$upper[,"95%"]<=2*sex.WtsLG[1]))/12 # 18.8333

## percent change
decomp.sexLG <- decompose(sex.WtsLG)
(decomp.sexLG$trend[7]-decomp.sexLG$trend[114])/decomp.sexLG$trend[7] # 80.73%
# initial value
sex.WtsLG[1] # -0.15
# final value
sex.WtsLG[120] # -0.27




## Summary with plots 
op <- par(mfrow = c(1,2))
plot(forecast.sexStraight, main = "Straight", ylim = c(-0.25, 0.5))
abline(h = 0, lty = 2)
plot(forecast.sexLG, main = "Gay/Lesbian", ylim = c(-0.25, 0.5))
abline(h = 0, lty = 2)
par(op)



## Overlapping plots
plot(forecast.sexStraight, ylim = c(-0.25, 0.5), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), 
     fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexLG, ylim = c(-0.25, 0.5), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
abline(h = 0, lty = 2)
legend(2006.5, 0.4, c("Lesbian/Gay", "Straight"), col = c(rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## Overlapping plots
op <- par(mar=c(2.5,7,2.5,2.5))
plot(forecast.sexStraight, ylim = c(-0.25, 0.5), xlim = c(2007,2018),
     main = "",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.sex.Straight$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2018), labels = c(2007:2018), cex.axis = 1.5)
axis(2, at = seq(-0.2, 0.5, by = 0.1), labels = seq(-0.2, 0.5, by = 0.1), las = 2, cex.axis = 1.5)
title(ylab="IAT D-Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.sexLG, ylim = c(-0.25, 0.5), xlim = c(2007, 2018), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.sex.LG$trend, col =  rgb(1,0.3,0), lwd = 3)
text(2010, 0.3, labels = "Straight", cex = 1.3)
text(2010, 0.2, labels = "Lesbian/Gay", cex = 1.3)
lines(x = c(2008, 2009), y = c(0.3, 0.3), lwd = 4, col = rgb(0,0.5,0.3, alpha = 0.8))
lines(x = c(2008, 2009), y = c(0.2, 0.2), lwd = 4, col = rgb(1,0.3,0, alpha = 0.8))
par(op)


## Race and Sexuality plotted together
op <- par(mar=c(2.5,7,4,2.5), mfrow = c(1, 2))
plot(forecast.sexStraight, ylim = c(-0.3, 0.5), xlim = c(2007,2020),
     main = "Implicit Sexuality Attitudes", cex.main = 2,
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.sex.Straight$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2020), labels = c(2007:2020), cex.axis = 1.5)
axis(2, at = seq(-0.3, 0.5, by = 0.1), labels = round(seq(-0.3, 0.5, by = 0.1), 2), las = 2, cex.axis = 1.5)
title(ylab="IAT D-Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.sexLG, ylim = c(-0.3, 0.5), xlim = c(2007, 2020), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.sex.LG$trend, col =  rgb(1,0.3,0), lwd = 3)
text(2008, 0.47, labels = "Straight", cex = 1.3, adj = 0, col = rgb(0,0.5,0))
text(2008, -0.22, labels = "Lesbian/Gay", cex = 1.3, adj = 0, col = rgb(1,0.3,0))

plot(forecast.raceWhite, ylim = c(-0.15, 0.5), xlim = c(2007,2020),
     main = "Implicit Race Attitudes", cex.main = 2,
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.raceWhite$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2020), labels = c(2007:2020), cex.axis = 1.5)
axis(2, at = seq(-0.15, 0.5, by = 0.1), labels = seq(-0.15, 0.5, by = 0.1), las = 2, cex.axis = 1.5)
title(ylab="IAT D-Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.raceBlack, ylim = c(-0.15, 0.5), xlim = c(2007, 2020), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.race.Black$trend, col =  rgb(1,0.3,0), lwd = 3)
par(new = TRUE)
plot(forecast.raceAsian, ylim = c(-0.15, 0.5), main = "", xlim = c(2007, 2020), axes = FALSE,
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = "darkgrey", fcol = rgb(0,0,1, alpha = 0.2))
lines(decomp.race.Asian$trend, col =  rgb(0,0.3,1), lwd = 3)
abline(h = 0, lty = 2)
text(2008, 0.45, labels = "White", cex = 1.3, adj = 0, col = rgb(0,0.5,0.3))
text(2008, 0.27, labels = "Asian", cex = 1.3, adj = 0, col = rgb(0,0.3,1))
text(2008, -0.1, labels = "Black", cex = 1.3, adj = 0, col = rgb(1,0.3,0))
par(op)




## SKIN TONE (by race groups) --------------------------------------------------------------------------------------------------------
table(timeseriesSkin$ethnic.race.new, timeseriesSkin$month.cent) # have sufficient data from 2007 onward
table(timeseriesSkin$ethnic.race.new)

## Fit to White
skin.tsWhite <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="White",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(skin.tsWhite$month.cent)

# impute missing values
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$cohens <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")

skin.tsWhite <- rbind(skin.month.extra, skin.tsWhite)
skin.tsWhite <- skin.tsWhite[order(skin.tsWhite$month.cent),]
skin.tsWhite

skin.WtsWhite <- ts(skin.tsWhite$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsWhite
plot(skin.WtsWhite)

# impute with seasonal decomposition and interpolation
skin.WtsWhite <- na.seadec(skin.WtsWhite, algorithm = "interpolation")
skin.WtsWhite
plot(skin.WtsWhite)

skin.arimaWhite <- auto.arima(skin.WtsWhite, stepwise = FALSE, approximation = FALSE)
skin.arimaWhite # parameters (0,1,1)

forecast.skinWhite <- forecast(skin.arimaWhite, h = 60)
plot(forecast.skinWhite)

forecast.skinWhiteLONG <- forecast(skin.arimaWhite, h = 1800) # 150 years
plot(forecast.skinWhiteLONG)
abline(h = 0)

min(which(forecast.skinWhiteLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinWhiteLONG$mean<=0))/12 # NA
min(which(forecast.skinWhiteLONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.skinWhite <- decompose(skin.WtsWhite)
(decomp.skinWhite$trend[7]-decomp.skinWhite$trend[114])/decomp.skinWhite$trend[7] # 11.45%
# initial value
skin.WtsWhite[1] # 0.40
# final value
skin.WtsWhite[120] # 0.36


## Fit to Asian
skin.tsAsian <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="Asian",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsAsian <- rbind(skin.month.extra, skin.tsAsian)
skin.tsAsian <- skin.tsAsian[order(skin.tsAsian$month.cent),]
skin.tsAsian

skin.WtsAsian <- ts(skin.tsAsian$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsAsian
plot(skin.WtsAsian)

# impute with seasonal decomposition and interpolation
skin.WtsAsian <- na.seadec(skin.WtsAsian, algorithm = "interpolation")
skin.WtsAsian
plot(skin.WtsAsian)

skin.arimaAsian <- auto.arima(skin.WtsAsian, stepwise = FALSE, approximation = FALSE)
skin.arimaAsian # parameters (0,0,1) with non-zero mean  

forecast.skinAsian <- forecast(skin.arimaAsian, h = 60)
plot(forecast.skinAsian)

forecast.skinAsianLONG <- forecast(skin.arimaAsian, h = 1800) # 150 years
plot(forecast.skinAsianLONG)
abline(h = 0)

min(which(forecast.skinAsianLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinAsianLONG$mean<=0))/12 # NA
min(which(forecast.skinAsianLONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.skinAsian <- decompose(skin.WtsAsian)
(decomp.skinAsian$trend[7]-decomp.skinAsian$trend[114])/decomp.skinAsian$trend[7] # 10.07%
# initial value
skin.WtsAsian[1] # 0.30
# final value
skin.WtsAsian[120] # 0.27

## Fit to Black/African American
skin.tsBlack <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="Black or African American",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsBlack <- rbind(skin.month.extra, skin.tsBlack)
skin.tsBlack <- skin.tsBlack[order(skin.tsBlack$month.cent),]
skin.tsBlack

skin.WtsBlack <- ts(skin.tsBlack$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsBlack
plot(skin.WtsBlack)

# impute with seasonal decomposition and interpolation
skin.WtsBlack <- na.seadec(skin.WtsBlack, algorithm = "interpolation")
skin.WtsBlack
plot(skin.WtsBlack)

skin.arimaBlack <- auto.arima(skin.WtsBlack, stepwise = FALSE, approximation = FALSE)
skin.arimaBlack # parameters (0,1,3)

forecast.skinBlack <- forecast(skin.arimaBlack, h = 60)
plot(forecast.skinBlack)

forecast.skinBlackLONG <- forecast(skin.arimaBlack, h = 1800) # 150 years
plot(forecast.skinBlackLONG)
abline(h = 0)

min(which(forecast.skinBlackLONG$lower[,"95%"]<=0))/12 # 3.0833 yrs
min(which(forecast.skinBlackLONG$mean<=0))/12 # NA
min(which(forecast.skinBlackLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinBlackLONG$upper[,"95%"]>=2*skin.WtsBlack[1]))/12 # 32.0833

## percent change
decomp.skinBlack <- decompose(skin.WtsBlack)
(decomp.skinBlack$trend[7]-decomp.skinBlack$trend[114])/decomp.skinBlack$trend[7] # -40.17%
plot(decomp.skinBlack)
# initial value
skin.WtsBlack[1] # 0.095
# final value
skin.WtsBlack[120] # 0.10


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.skinWhite, main = "White", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
plot(forecast.skinBlack, main = "Black", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
plot(forecast.skinBWbiracial, main = "Black-White biracial", ylim = c(-.15, .5))
abline(h = 0, lty = 2)
par(op)





## Overlapping plots
plot(forecast.skinWhite, ylim = c(-0.05, 0.5), main = "Implicit Skin-Tone Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinBlack, ylim = c(-0.05, 0.5), main = "Implicit Skin-Tone Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinAsian, ylim = c(-0.05, 0.5), main = "Implicit Skin-Tone Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
abline(h = 0, lty = 2)
legend("left", c("Asian", "Black", "White"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## WEIGHT (BODY, by weight groups) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$iam.new, timeseriesWeightBody$month.cent) # have sufficient data from 2007 onward
timeseriesWeightBody$weightcuts <- as.numeric(timeseriesWeightBody$iam.new)
timeseriesWeightBody$weightcuts <- cut(timeseriesWeightBody$weightcuts, c(0,3,4,7))
levels(timeseriesWeightBody$weightcuts) <- c("under", "neither","over")
table(timeseriesWeightBody$weightcuts, timeseriesWeightBody$month.cent) # months 125 - 131 have low data

table(timeseriesWeightBody$weightcuts)

## Fit to under
weight.tsunder <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="under",],
                      .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsunder$month.cent)

# add in NAs for month values 125:131
which(weight.tsunder$month.cent==125) # gives us index 47
weight.tsunder$month.cent[47:48] # gives us month 125, 130
weight.tsunder$mean[47:48] <- NA
weight.tsunder$weight.mean[47:48] <- NA
weight.tsunder$cohens[47:48] <- NA
weight.tsunder
weight.tsunder <- weight.tsunder[is.na(weight.tsunder$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 126:129, 131:132))
weight.month.extra$mean <- rep(NA, 9)
weight.month.extra$weight.mean <- rep(NA, 9)
weight.month.extra$cohens <- rep(NA, 9)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsunder <- rbind(weight.month.extra, weight.tsunder)
weight.tsunder <- weight.tsunder[order(weight.tsunder$month.cent),]
weight.tsunder

weight.Wtsunder <- ts(weight.tsunder$weight.mean, frequency = 12, start = c(2010, 4))
weight.Wtsunder
plot(weight.Wtsunder)

# impute with seasonal decomposition and interpolation
weight.Wtsunder <- na.seadec(weight.Wtsunder, algorithm = "interpolation")
weight.Wtsunder
plot(weight.Wtsunder)

weight.arimaunder <- auto.arima(weight.Wtsunder, stepwise = FALSE, approximation = FALSE)
weight.arimaunder # parameters (3,0,0)(1,1,1)

forecast.weightunder <- forecast(weight.arimaunder, h = 60)
plot(forecast.weightunder)

forecast.weightunderLONG <- forecast(weight.arimaunder, h = 1800) # 150 years
plot(forecast.weightunderLONG)
abline(h = 0)
abline(h = 2*weight.Wtsunder[1])

min(which(forecast.weightunderLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightunderLONG$mean<=0))/12 # NA
min(which(forecast.weightunderLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightunderLONG$upper[,"95%"]>=2*weight.Wtsunder[1]))/12 # NA


## percent change
decomp.weightunder <- decompose(weight.Wtsunder)
(decomp.weightunder$trend[7]-decomp.weightunder$trend[75])/decomp.weightunder$trend[7] # +3.20%
# initial value
weight.Wtsunder[1] # 0.50
# final value
weight.Wtsunder[81] # 0.50


## Fit to neither
weight.tsneither <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="neither",],
                        .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsneither$month.cent)

# add in NAs for month values 125:131
weight.tsneither$month.cent[47:50] # gives us months 125, 126, 129, 130
weight.tsneither$mean[47:50] <- NA
weight.tsneither$weight.mean[47:50] <- NA
weight.tsneither$cohens[47:50] <- NA
weight.tsneither
weight.tsneither <- weight.tsneither[is.na(weight.tsneither$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 127:128, 131:132))
weight.month.extra$mean <- rep(NA, 7)
weight.month.extra$weight.mean <- rep(NA, 7)
weight.month.extra$cohens <- rep(NA, 7)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsneither <- rbind(weight.month.extra, weight.tsneither)
weight.tsneither <- weight.tsneither[order(weight.tsneither$month.cent),]
weight.tsneither

weight.Wtsneither <- ts(weight.tsneither$weight.mean, frequency = 12, start = c(2010, 4))
weight.Wtsneither
plot(weight.Wtsneither)

# impute with seasonal decomposition and interpolation
weight.Wtsneither <- na.seadec(weight.Wtsneither, algorithm = "interpolation")
weight.Wtsneither
plot(weight.Wtsneither)

weight.arimaneither <- auto.arima(weight.Wtsneither, stepwise = FALSE, approximation = FALSE)
weight.arimaneither # parameters (3,0,0)

forecast.weightneither <- forecast(weight.arimaneither, h = 60)
plot(forecast.weightneither)

forecast.weightneitherLONG <- forecast(weight.arimaneither, h = 1800) # 150 years
plot(forecast.weightneitherLONG)
abline(h = 0)
abline(h = 2*weight.Wtsneither[1])

min(which(forecast.weightneitherLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightneitherLONG$mean<=0)) # NA
min(which(forecast.weightneitherLONG$upper[,"95%"]<=0)) # NA
min(which(forecast.weightneitherLONG$upper[,"95%"]>=2*weight.Wtsneither[1]))/12 # NA

## percent change
decomp.weightneither <- decompose(weight.Wtsneither)
(decomp.weightneither$trend[7]-decomp.weightneither$trend[75])/decomp.weightneither$trend[7] # +4.87%
# initial value
weight.Wtsneither[1] # 0.52
# final value
weight.Wtsneither[81] # 0.52


## Fit to over
weight.tsover <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="over",],
                        .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsover$month.cent)

# add in NAs for month values 125:131
weight.tsover$month.cent[47:53] # gives us months 125:131
weight.tsover$mean[47:53] <- NA
weight.tsover$weight.mean[47:53] <- NA
weight.tsover$cohens[47:53] <- NA
weight.tsover
weight.tsover <- weight.tsover[is.na(weight.tsover$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 132))
weight.month.extra$mean <- rep(NA, 4)
weight.month.extra$weight.mean <- rep(NA, 4)
weight.month.extra$cohens <- rep(NA, 4)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsover <- rbind(weight.month.extra, weight.tsover)
weight.tsover <- weight.tsover[order(weight.tsover$month.cent),]
weight.tsover

weight.Wtsover <- ts(weight.tsover$weight.mean, frequency = 12, start = c(2010, 4))
weight.Wtsover
plot(weight.Wtsover)

# impute with seasonal decomposition and interpolation
weight.Wtsover <- na.seadec(weight.Wtsover, algorithm = "interpolation")
weight.Wtsover
plot(weight.Wtsover)

weight.arimaover <- auto.arima(weight.Wtsover, stepwise = FALSE, approximation = FALSE)
weight.arimaover # parameters (1,0,0)(2,0,0)

forecast.weightover <- forecast(weight.arimaover, h = 60)
plot(forecast.weightover)

forecast.weightoverLONG <- forecast(weight.arimaover, h = 1800) # 150 years
plot(forecast.weightoverLONG)
abline(h = 0)
abline(h = 2*weight.Wtsover[1])

min(which(forecast.weightoverLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightoverLONG$mean<=0))/12 # NA
min(which(forecast.weightoverLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightoverLONG$upper[,"95%"]>=2*weight.Wtsover[1]))/12 # NA

## percent change
decomp.weightover <- decompose(weight.Wtsover)
(decomp.weightover$trend[7]-decomp.weightover$trend[75])/decomp.weightover$trend[7] # +5.17%
# initial value
weight.Wtsover[1] # 0.42
# final value
weight.Wtsover[81] # 0.44

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.weightunder, main = "Underweight", ylim = c(0.25, 0.7))
plot(forecast.weightneither, main = "Neither under nor over", ylim = c(0.25, 0.7))
plot(forecast.weightover, main = "Overweight", ylim = c(0.25, 0.7))
par(op)


## Overlapping plots
plot(forecast.weightneither, ylim = c(0.15, 0.75), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightunder, ylim = c(0.15, 0.75), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightover, ylim = c(0.15, 0.75), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
legend("bottomleft", c("Average weight", "Overweight", "Underweight"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## WEIGHT (FACE, by weight groups) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightFace$weight.COMB, timeseriesWeightFace$month.cent)
timeseriesWeightFace$weightcuts <- as.numeric(timeseriesWeightFace$weight.COMB)
timeseriesWeightFace$weightcuts <- cut(timeseriesWeightFace$weightcuts, c(0,3,4,7))
levels(timeseriesWeightFace$weightcuts) <- c("under", "neither","over")
table(timeseriesWeightFace$weightcuts, timeseriesWeightFace$month.cent) # all months have sufficient data
table(timeseriesWeightFace$weightcuts)

## Fit to under
weight.tsFaceunder <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="under",],
                        .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsFaceunder$month.cent)

weight.WtsFaceunder <- ts(weight.tsFaceunder$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011,10))
weight.WtsFaceunder
plot(weight.WtsFaceunder)

weight.arimaunderFace <- auto.arima(weight.WtsFaceunder, stepwise = FALSE, approximation = FALSE)
weight.arimaunderFace # parameters (3,1,0)

forecast.weightunderFace <- forecast(weight.arimaunderFace, h = 60)
plot(forecast.weightunderFace)

forecast.weightunderFaceLONG <- forecast(weight.arimaunderFace, h = 1800) # 150 years
plot(forecast.weightunderFaceLONG)
abline(h = 0)
abline(h = 2*weight.WtsFaceunder[1])

min(which(forecast.weightunderFaceLONG$lower[,"95%"]<=0))/12 # 28.41667
min(which(forecast.weightunderFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightunderFaceLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightunderFaceLONG$upper[,"95%"]>=2*weight.WtsFaceunder[1]))/12 # 0.91667


## percent change
decomp.weightunderFace <- decompose(weight.WtsFaceunder)
(decomp.weightunderFace$trend[7]-decomp.weightunderFace$trend[75])/decomp.weightunderFace$trend[7] # +26.47%
# initial value
weight.WtsFaceunder[1] # 0.29
# final value
weight.WtsFaceunder[81] # 0.43


## Fit to neither
weight.tsFaceneither <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="neither",],
                          .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsFaceneither$month.cent)

weight.WtsFaceneither <- ts(weight.tsFaceneither$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011, 10))
weight.WtsFaceneither
plot(weight.WtsFaceneither)

weight.arimaneitherFace <- auto.arima(weight.WtsFaceneither, stepwise = FALSE, approximation = FALSE)
weight.arimaneitherFace # parameters (0,1,1) with drift

forecast.weightneitherFace <- forecast(weight.arimaneitherFace, h = 60)
plot(forecast.weightneitherFace)

forecast.weightneitherFaceLONG <- forecast(weight.arimaneitherFace, h = 1800) # 150 years
plot(forecast.weightneitherFaceLONG)
abline(h = 0)
abline(h = 2*weight.WtsFaceneither[1])

min(which(forecast.weightneitherFaceLONG$lower[,"95%"]>=2*weight.WtsFaceneither[1]))/12 # 29.25
min(which(forecast.weightneitherFaceLONG$mean>=2*weight.WtsFaceneither[1]))/12 # 12.1667
min(which(forecast.weightneitherFaceLONG$upper[,"95%"]>=2*weight.WtsFaceneither[1]))/12 # 4.833

## percent change
decomp.weightneitherFace <- decompose(weight.WtsFaceneither)
(decomp.weightneitherFace$trend[7]-decomp.weightneitherFace$trend[75])/decomp.weightneitherFace$trend[7] # +41.19%
# initial value
weight.WtsFaceneither[1] # 0.34
# final value
weight.WtsFaceneither[81] # 0.46


## Fit to over
weight.tsFaceover <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="over",],
                       .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weight.tsFaceover$month.cent)

weight.WtsFaceover <- ts(weight.tsFaceover$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011, 10))
weight.WtsFaceover
plot(weight.WtsFaceover)

weight.arimaoverFace <- auto.arima(weight.WtsFaceover, stepwise = FALSE, approximation = FALSE)
weight.arimaoverFace # parameters (0,1,1) with drift

forecast.weightoverFace <- forecast(weight.arimaoverFace, h = 60)
plot(forecast.weightoverFace)

forecast.weightoverFaceLONG <- forecast(weight.arimaoverFace, h = 1800) # 150 years
plot(forecast.weightoverFaceLONG)
abline(h = 0)
abline(h = 2*weight.WtsFaceover[1])

min(which(forecast.weightoverFaceLONG$lower[,"95%"]>=2*weight.WtsFaceover[1]))/12 # 28
min(which(forecast.weightoverFaceLONG$mean>=2*weight.WtsFaceover[1]))/12 # 9.8333
min(which(forecast.weightoverFaceLONG$upper[,"95%"]>=2*weight.WtsFaceover[1]))/12 # 3.333

## percent change
decomp.weightoverFace <- decompose(weight.WtsFaceover)
(decomp.weightoverFace$trend[7]-decomp.weightoverFace$trend[75])/decomp.weightoverFace$trend[7] # +48.25%
# initial value
weight.WtsFaceover[1] # 0.26
# final value
weight.WtsFaceover[81] # 0.38

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.weightunderFace, main = "Underweight", ylim = c(0.25, 0.7))
plot(forecast.weightneitherFace, main = "Neither under nor over", ylim = c(0.25, 0.7))
plot(forecast.weightoverFace, main = "Overweight", ylim = c(0.25, 0.7))
par(op)


## Overlapping plots
plot(forecast.weightneitherFace, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightunderFace, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightoverFace, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
legend("topleft", c("Average weight", "Overweight", "Underweight"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 




## EXPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE (by age groups) --------------------------------------------------------------------------------------------------------
# first, create age groups for subsetting the data
timeseriesAge$agecats <- cut(timeseriesAge$age.new, c(0,25,35,45,55,100))
levels(timeseriesAge$agecats) <- c("age0024", "age2534", "age3544",
                                   "age4554", "age5599")
table(timeseriesAge$agecats, timeseriesAge$month.cent) # now all age categories for each year

## Fit to 0 - 25
age.tsE0024 <- ddply(timeseriesAge[timeseriesAge$agecats=="age0024",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsE0024 <- ts(age.tsE0024$weight.mean, frequency = 12, start = c(2007, 1))
age.WtsE0024
plot(age.WtsE0024)

age.WtsE0024 <- window(age.WtsE0024, start = c(2007,1))
plot(age.WtsE0024)

age.arimaE0024 <- auto.arima(age.WtsE0024, stepwise = FALSE, approximation = FALSE)
age.arimaE0024 # parameters (0,1,1)(2,0,0)

forecast.ageE0024 <- forecast(age.arimaE0024, h = 60)
plot(forecast.ageE0024)

forecast.ageE0024LONG <- forecast(age.arimaE0024, h = 1800) # 150 years
plot(forecast.ageE0024LONG)
abline(h = 0)

min(which(forecast.ageE0024LONG$lower[,"95%"]<=0))/12 # 11.25
min(which(forecast.ageE0024LONG$mean<=0)) # NA
min(which(forecast.ageE0024LONG$upper[,"95%"]<=0)) # NA
min(which(forecast.ageE0024LONG$upper[,"95%"]>=2*age.WtsE0024[1]))/12 # 19.5833

## percent change
decomp.ageE0024 <- decompose(age.WtsE0024)
(decomp.ageE0024$trend[7]-decomp.ageE0024$trend[114])/decomp.ageE0024$trend[7] # -27.15%
# initial value
age.WtsE0024[1] # 0.69
# final value
age.WtsE0024[120] # 0.56



## Fit to 25 - 34
age.tsE2534 <- ddply(timeseriesAge[timeseriesAge$agecats=="age2534",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsE2534 <- ts(age.tsE2534$weight.mean, frequency = 12, start = c(2007, 1))
age.WtsE2534
plot(age.WtsE2534)

age.WtsE2534 <- window(age.WtsE2534, start = c(2007,1))
plot(age.WtsE2534)
age.arimaE2534 <- auto.arima(age.WtsE2534, stepwise = FALSE, approximation = FALSE)
age.arimaE2534 # parameters (1,1,2)(1,0,0)

forecast.ageE2534 <- forecast(age.arimaE2534, h = 60)
plot(forecast.ageE2534)
abline(h = 0)

forecast.ageE2534LONG <- forecast(age.arimaE2534, h = 1800) # 150 years
plot(forecast.ageE2534LONG)
abline(h = 0)

min(which(forecast.ageE2534LONG$lower[,"95%"]<=0))/12 # 1 month, already neutral
min(which(forecast.ageE2534LONG$mean<=0)) # NA
min(which(forecast.ageE2534LONG$upper[,"95%"]<=0)) # NA
min(which(forecast.ageE2534LONG$upper[,"95%"]>=2*age.WtsE2534[1]))/12 # 28.0883

## percent change
decomp.ageE2534 <- decompose(age.WtsE2534)
(decomp.ageE2534$trend[7]-decomp.ageE2534$trend[114])/decomp.ageE2534$trend[7] # -54.30%
# initial value
age.WtsE2534[1] # 0.32
# final value
age.WtsE2534[120] # 0.12


## Fit to 35 - 44
age.tsE3544 <- ddply(timeseriesAge[timeseriesAge$agecats=="age3544",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsE3544 <- ts(age.tsE3544$weight.mean, frequency = 12, start = c(2007, 1))
age.WtsE3544
plot(age.WtsE3544)

age.WtsE3544 <- window(age.WtsE3544, start = c(2007,1))
plot(age.WtsE3544)
age.arimaE3544 <- auto.arima(age.WtsE3544, stepwise = FALSE, approximation = FALSE)
age.arimaE3544 # parameters (0,1,5)

forecast.ageE3544 <- forecast(age.arimaE3544, h = 60)
plot(forecast.ageE3544)
abline(h = 0)

forecast.ageE3544LONG <- forecast(age.arimaE3544, h = 1800) # 150 years
plot(forecast.ageE3544LONG)
abline(h = 0)

min(which(forecast.ageE3544LONG$lower[,"95%"]<=0))/12 # 1 month, already neutral
min(which(forecast.ageE3544LONG$mean<=0))/12 # 2 months, then asymptotes
min(which(forecast.ageE3544LONG$upper[,"95%"]>=2*age.WtsE3544[1]))/12 # 1 month, already doubled

## percent change
decomp.ageE3544 <- decompose(age.WtsE3544)
(decomp.ageE3544$trend[7]-decomp.ageE3544$trend[114])/decomp.ageE3544$trend[7] # -89.26%
# initial value
age.WtsE3544[1] # 0.094
# final value
age.WtsE3544[120] # 0.082



## Fit to 45 - 54
age.tsE4554 <- ddply(timeseriesAge[timeseriesAge$agecats=="age4554",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsE4554 <- ts(age.tsE4554$weight.mean, frequency = 12, start = c(2007, 1))
age.WtsE4554
plot(age.WtsE4554)

age.WtsE4554 <- window(age.WtsE4554, start = c(2007,1))
age.arimaE4554 <- auto.arima(age.WtsE4554, stepwise = FALSE, approximation = FALSE)
age.arimaE4554 # parameters (0,1,1)(1,0,2)[12]  

forecast.ageE4554 <- forecast(age.arimaE4554, h = 60)
plot(forecast.ageE4554)
abline(h = 0)

forecast.ageE4554LONG <- forecast(age.arimaE4554, h = 1800) # 150 years
plot(forecast.ageE4554LONG)
abline(h = 0)

min(which(forecast.ageE4554LONG$lower[,"95%"]<=0))/12 # 1 month, already neutral
min(which(forecast.ageE4554LONG$mean<=0))/12 # 3 months, already neutral
min(which(forecast.ageE4554LONG$upper[,"95%"]>=2*age.WtsE4554[1]))/12 # 9.5

## percent change
decomp.ageE4554 <- decompose(age.WtsE4554)
(decomp.ageE4554$trend[7]-decomp.ageE4554$trend[114])/decomp.ageE4554$trend[7] # -88.95%
# initial value
age.WtsE4554[1] # 0.13
# final value
age.WtsE4554[120] # 0.0092


## Fit to 55 +
age.tsE5599 <- ddply(timeseriesAge[timeseriesAge$agecats=="age5599",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsE5599 <- ts(age.tsE5599$weight.mean, frequency = 12, start = c(2007, 1))
age.WtsE5599
plot(age.WtsE5599)

age.WtsE5599 <- window(age.WtsE5599, start = c(2007,1))
plot(age.WtsE5599)
age.arimaE5599 <- auto.arima(age.WtsE5599, stepwise = FALSE, approximation = FALSE)
age.arimaE5599 # parameters (2,1,3)

forecast.ageE5599 <- forecast(age.arimaE5599, h = 60)
plot(forecast.ageE5599)
abline(h = 0)

forecast.ageE5599LONG <- forecast(age.arimaE5599, h = 1800) # 150 years
plot(forecast.ageE5599LONG)
abline(h = 0)

min(which(forecast.ageE5599LONG$lower[,"95%"]<=0))/12 # 1 month, already neutral
min(which(forecast.ageE5599LONG$mean<=0))/12 # 1 month, already neutral
min(which(forecast.ageE5599LONG$upper[,"95%"]>=2*age.WtsE5599[1]))/12 # 1 month, already doubled

## percent change
decomp.ageE5599 <- decompose(age.WtsE5599)
(decomp.ageE5599$trend[7]-decomp.ageE5599$trend[114])/decomp.ageE5599$trend[7] # -129.61%
# initial value
age.WtsE5599[1] # 0.080
# final value
age.WtsE5599[120] # 0.048



## Summary with plots
op <- par(mfrow = c(2,3))
plot(forecast.ageE0024, main = "24 years and Under", ylim = c(-0.25, 0.8))
abline(h = 0)
plot(forecast.ageE2534, main = "25 - 34 years", ylim = c(-0.25, 0.8))
abline(h = 0)
plot(forecast.ageE3544, main = "35 - 44 years", ylim = c(-0.25, 0.8))
abline(h = 0)
plot(forecast.ageE4554, main = "45 - 54 years", ylim = c(-0.25, 0.8))
abline(h = 0)
plot(forecast.ageE5599, main = "55 years and Over", ylim = c(-0.25, 0.8))
abline(h = 0)
par(op)


## Overlapping plots
plot(forecast.ageE0024, ylim = c(-0.3, 0.8), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.ageE5599, ylim = c(-0.3, 0.8), main = "Explicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.ageE2534, ylim = c(-0.3, 0.8), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
abline(h = 0, lty = 2)
legend("bottomleft", c( "Under 25", "25 - 34", "Over 55"), col = c( rgb(0,0.5,0.3), rgb(0,0.3,1), rgb(1,0.3,0)), bty = "n", lty = 1, lwd = 2) 




## DISABILITY (by disability status) --------------------------------------------------------------------------------------------------------
table(timeseriesDis$dis.new, timeseriesDis$month.cent) # have sufficient data from 2007 onward

## Fit to Disabled
dis.tsEDis <- ddply(timeseriesDis[timeseriesDis$dis.new=="Yes",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WtsEDis <- ts(dis.tsEDis$weight.mean, frequency = 12, start = c(2007, 1))
dis.WtsEDis
plot(dis.WtsEDis)

dis.WtsEDis <- window(dis.WtsEDis, start = c(2007,1))
plot(dis.WtsEDis)
dis.arimaEDis <- auto.arima(dis.WtsEDis, stepwise = FALSE, approximation = FALSE)
dis.arimaEDis # parameters (0,1,1) 

forecast.disEDis <- forecast(dis.arimaEDis, h = 60)
plot(forecast.disEDis)
abline(h = 0)

forecast.disDisELONG <- forecast(dis.arimaEDis, h = 1800) # 150 years
plot(forecast.disDisELONG)
abline(h = 0)

min(which(forecast.disDisELONG$lower[,"95%"]<=0))/12 # 1 months
min(which(forecast.disDisELONG$mean<=0)) # NA
min(which(forecast.disDisELONG$upper[,"95%"]<=0)) # NA
min(which(forecast.disDisELONG$upper[,"95%"]>=2*dis.WtsEDis[1]))/12 # 74.833 yrs

## percent change
decomp.disEDis <- decompose(dis.WtsEDis)
(decomp.disEDis$trend[7]-decomp.disEDis$trend[114])/decomp.disEDis$trend[7] # -26.88%
# initial value
dis.WtsEDis[1] # 0.35
# final value
dis.WtsEDis[120] # 0.27


## Fit to Not disabled
dis.tsENdis <- ddply(timeseriesDis[timeseriesDis$dis.new=="No",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WtsENdis <- ts(dis.tsENdis$weight.mean, frequency = 12, start = c(2007, 1))
dis.WtsENdis
plot(dis.WtsENdis)

dis.WtsENdis <- window(dis.WtsENdis, start = c(2007,1))
dis.arimaENdis <- auto.arima(dis.WtsENdis, stepwise = FALSE, approximation = FALSE)
dis.arimaENdis # parameters (0,1,1)(1,0,0)[12]  

forecast.disENdis <- forecast(dis.arimaENdis, h = 60)
plot(forecast.disENdis)

forecast.disNdisELONG <- forecast(dis.arimaENdis, h = 1800) # 150 years
plot(forecast.disNdisELONG)
abline(h = 0)
abline(h = 2*dis.WtsENdis[1])

min(which(forecast.disNdisELONG$lower[,"95%"]<=0))/12 # 61.25
min(which(forecast.disNdisELONG$mean<=0)) # NA
min(which(forecast.disNdisELONG$upper[,"95%"]<=0)) # NA
min(which(forecast.disNdisELONG$upper[,"95%"]>=2*dis.WtsENdis[1])) # NA

## percent change
decomp.disENdis <- decompose(dis.WtsENdis)
(decomp.disENdis$trend[7]-decomp.disENdis$trend[114])/decomp.disENdis$trend[7] # -24.01%
# initial value
dis.WtsENdis[1] # 0.69
# final value
dis.WtsENdis[120] # 0.47



## Summary with plots
op <- par(mfrow = c(1,2))
plot(forecast.disEDis, main = "Disabled", ylim = c(-0.1, 0.7))
abline(h = 0, lty = 2)
plot(forecast.disENdis, main = "Not disabled", ylim = c(-0.1, 0.7))
abline(h = 0, lty = 2)
par(op)


## Overlapping plots
plot(forecast.disENdis, ylim = c(-0.1, 0.7), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disEDis, ylim = c(-0.1, 0.7), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
abline(h = 0, lty = 2)
legend("bottomleft", c("Disabled", "Not disabled"), col = c(rgb(1,0.3,0), rgb(0,0.5,0.3)), lty = 1, lwd = 2, cex = 1.2) 




## RACE (by race groups) --------------------------------------------------------------------------------------------------------
table(timeseriesRace$ethnic.race.new, timeseriesRace$month.cent) # have sufficient data from 2007 onward

## Fit to White
race.tsEWhite <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="White",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEWhite <- ts(race.tsEWhite$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsEWhite
plot(race.WtsEWhite)

decomp.race.WhiteE <- decompose(race.WtsEWhite)

race.WtsEWhite <- window(race.WtsEWhite, start = c(2007,1))
plot(race.WtsEWhite)

race.arimaEWhite <- auto.arima(race.WtsEWhite, stepwise = FALSE, approximation = FALSE)
race.arimaEWhite # parameters (1,1,1)(0,0,2)

forecast.raceEWhite <- forecast(race.arimaEWhite, h = 60)
plot(forecast.raceEWhite)

forecast.raceWhiteELONG <- forecast(race.arimaEWhite, h = 1800) # 150 years
plot(forecast.raceWhiteELONG)
abline(h = 0)
abline(h = 2*race.WtsEWhite[1])

min(which(forecast.raceWhiteELONG$lower[,"95%"]<=0))/12 # 12.5833
min(which(forecast.raceWhiteELONG$mean<=0))/12 # NA
min(which(forecast.raceWhiteELONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceWhiteELONG$upper[,"95%"]>=2*race.WtsEWhite[1]))/12 # 47.833

## percent change
decomp.raceEWhite <- decompose(race.WtsEWhite)
(decomp.raceEWhite$trend[7]-decomp.raceEWhite$trend[114])/decomp.raceEWhite$trend[7] # -30.91%
# initial value
race.WtsEWhite[1] # 0.56
# final value
race.WtsEWhite[120] # 0.37


## Fit to Asian
race.tsEAsian <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="Asian",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEAsian <- ts(race.tsEAsian$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsEAsian
plot(race.WtsEAsian)

decomp.race.AsianE <- decompose(race.WtsEAsian)

race.WtsEAsian <- window(race.WtsEAsian, start = c(2007,1))
race.arimaEAsian <- auto.arima(race.WtsEAsian, stepwise = FALSE, approximation = FALSE)
race.arimaEAsian # parameters (0,1,1)  with drift

forecast.raceEAsian <- forecast(race.arimaEAsian, h = 60)
plot(forecast.raceEAsian)

forecast.raceAsianELONG <- forecast(race.arimaEAsian, h = 1800) # 150 years
plot(forecast.raceAsianELONG)
abline(h = 0)

min(which(forecast.raceAsianELONG$lower[,"95%"]<=0))/12 # 2.0833 yrs
min(which(forecast.raceAsianELONG$mean<=0))/12 # 9 yrs, 6 mos
min(which(forecast.raceAsianELONG$upper[,"95%"]<=0))/12 # 27 yrs, 7 mos.
min(which(forecast.raceAsianELONG$upper[,"95%"]>=2*race.WtsEAsian[1]))/12 # NA

## percent change
decomp.raceEAsian <- decompose(race.WtsEAsian)
(decomp.raceEAsian$trend[7]-decomp.raceEAsian$trend[114])/decomp.raceEAsian$trend[7] # -46.85%
# initial value
race.WtsEAsian[1] # 0.40
# final value
race.WtsEAsian[120] # 0.20


## Fit to Black/African American
race.tsEBlack <- ddply(timeseriesRace[timeseriesRace$ethnic.race.new=="Black or African American",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEBlack <- ts(race.tsEBlack$weight.mean, frequency = 12, start = c(2007, 1))
race.WtsEBlack
plot(race.WtsEBlack)

decomp.race.BlackE <- decompose(race.WtsEBlack)

race.WtsEBlack <- window(race.WtsEBlack, start = c(2007,1))
race.arimaEBlack <- auto.arima(race.WtsEBlack, stepwise = FALSE, approximation = FALSE)
race.arimaEBlack # parameters (1,1,2)(0,0,1)

forecast.raceEBlack <- forecast(race.arimaEBlack, h = 60)
plot(forecast.raceEBlack)

forecast.raceBlackELONG <- forecast(race.arimaEBlack, h = 1800) # 150 years
plot(forecast.raceBlackELONG)
abline(h = 0)

min(which(forecast.raceBlackELONG$lower[,"95%"]<=2*race.WtsEBlack[1]))/12 # 6 yrs, 2 mos.
min(which(forecast.raceBlackELONG$mean<=2*race.WtsEBlack[1]))/12 # NA
min(which(forecast.raceBlackELONG$upper[,"95%"]>=0))/12 # 5 yrs, 6 mos.

## percent change
decomp.raceEBlack <- decompose(race.WtsEBlack)
(decomp.raceEBlack$trend[7]-decomp.raceEBlack$trend[114])/decomp.raceEBlack$trend[7] # +29.09%
# initial value
race.WtsEBlack[1] # -1.18
# final value
race.WtsEBlack[120] # -0.87



## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.raceEWhite, main = "White", ylim = c(-1.25, .65))
abline(h = 0, lty = 2)
plot(forecast.raceEBlack, main = "Black", ylim = c(-1.25, .65))
abline(h = 0, lty = 2)
plot(forecast.raceEAsian, main = "Asian", ylim = c(-1.25, .65))
abline(h = 0, lty = 2)
par(op)




## Overlapping plots
plot(forecast.raceEAsian, ylim = c(-1.25, 0.65), main = "Explicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.1), rgb(0,0.3,1, alpha = 0.1)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.1))
par(new = TRUE)
plot(forecast.raceEWhite, ylim = c(-1.25, 0.65), main = "Explicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.raceEBlack, ylim = c(-1.25, 0.65), main = "Explicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
abline(h = 0, lty = 2)
legend("left", c("Asian", "Black", "White"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## SEXUALITY (by sexual orientation) --------------------------------------------------------------------------------------------------------
table(timeseriesSex$sexuality.new, timeseriesSex$month.cent) # have sufficient data from 2007 onward

## Fit to Straight
sex.tsEStraight <- ddply(timeseriesSex[timeseriesSex$sexuality.new=="Straight",],
                         .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsEStraight <- ts(sex.tsEStraight$weight.mean, frequency = 12, start = c(2007, 1))
sex.WtsEStraight
plot(sex.WtsEStraight)

decomp.sex.StraightE <- decompose(sex.WtsEStraight)

sex.WtsEStraight <- window(sex.WtsEStraight, start = c(2007,1))
sex.arimaStraightE <- auto.arima(sex.WtsEStraight, stepwise = FALSE, approximation = FALSE)
sex.arimaStraightE # parameters (0,1,2)(2,0,1)[12] with drift 

forecast.sexStraightE <- forecast(sex.arimaStraightE, h = 60)
plot(forecast.sexStraightE)

forecast.sexStraightELONG <- forecast(sex.arimaStraightE, h = 1800) # 150 years
plot(forecast.sexStraightELONG)
abline(h = 0)

min(which(forecast.sexStraightELONG$lower[,"95%"]<=0))/12 # 10.75 yrs
min(which(forecast.sexStraightELONG$mean<=0))/12 # 15.0833
min(which(forecast.sexStraightELONG$upper[,"95%"]<=0))/12 # 20.75

## percent change
decomp.sexEStraight <- decompose(sex.WtsEStraight)
(decomp.sexEStraight$trend[7]-decomp.sexEStraight$trend[114])/decomp.sexEStraight$trend[7] # -37.35%
# initial value
sex.WtsEStraight[1] # 0.98
# final value
sex.WtsEStraight[120] # 0.60


## Fit to Gay/Lesbian
sex.tsELG <- ddply(timeseriesSex[timeseriesSex$sexuality.new=="Gay/Lesbian",],
                   .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsELG <- ts(sex.tsELG$weight.mean, frequency = 12, start = c(2007, 1))
sex.WtsELG
plot(sex.WtsELG)

decomp.sex.LGE <- decompose(sex.WtsELG)

sex.WtsELG <- window(sex.WtsELG, start = c(2007,1))
sex.arimaLGE <- auto.arima(sex.WtsELG, stepwise = FALSE, approximation = FALSE)
sex.arimaLGE # parameters (1,0,1) with non-zero mean 

forecast.sexLGE <- forecast(sex.arimaLGE, h = 60)
plot(forecast.sexLGE)

forecast.sexLGELONG <- forecast(sex.arimaLGE, h = 1800) # 150 years
plot(forecast.sexLGELONG)
abline(h = 0)
abline(h = 2*sex.WtsELG[1])

min(which(forecast.sexLGELONG$lower[,"95%"]<=2*sex.WtsELG[1])) # NA
min(which(forecast.sexLGELONG$mean<=2*sex.WtsELG[1])) # NA
min(which(forecast.sexLGELONG$upper[,"95%"]<=2*sex.WtsELG[1])) # NA

## percent change
decomp.sexELG <- decompose(sex.WtsELG)
(decomp.sexELG$trend[7]-decomp.sexELG$trend[114])/decomp.sexELG$trend[7] # -8.29%
# initial value
sex.WtsELG[1] # -0.74
# final value
sex.WtsELG[120] # -0.91



## Summary with plots 
op <- par(mfrow = c(1,2))
plot(forecast.sexStraightE, main = "Straight", ylim = c(-1, 1))
abline(h = 0, lty = 2)
plot(forecast.sexLGE, main = "Gay/Lesbian", ylim = c(-1, 1))
abline(h = 0, lty = 2)
par(op)



## Overlapping plots
plot(forecast.sexStraightE, ylim = c(-1, 1), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexLGE, ylim = c(-1, 1), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
abline(h = 0, lty = 2)
legend(2006.5, 0.8, c("Lesbian/Gay", "Straight"), col = c(rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 

## Race and Sexuality plotted together
op <- par(mar=c(2.5,7,4,2.5), mfrow = c(1, 2))
plot(forecast.sexStraightE, ylim = c(-1, 1), xlim = c(2007,2020),
     main = "Explicit Sexuality Attitudes", cex.main = 2,
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.sex.StraightE$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2020), labels = c(2007:2020), cex.axis = 1.5)
axis(2, at = seq(-1, 1, by = 0.25), labels = seq(-1, 1, by = 0.25), las = 2, cex.axis = 1.5)
title(ylab="Explicit Preference Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.sexLGE, ylim = c(-1, 1), xlim = c(2007, 2020), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.sex.LGE$trend, col =  rgb(1,0.3,0), lwd = 3)
text(2008, 0.75, labels = "Straight", cex = 1.3, adj = 0, col = rgb(0,0.5,0.3))
text(2008, -0.5, labels = "Lesbian/Gay", cex = 1.3, adj = 0, col = rgb(1,0.3,0))

plot(forecast.raceEWhite, ylim = c(-1.6, 0.7), xlim = c(2007,2020),
     main = "Explicit Race Attitudes", cex.main = 2,
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = "darkgrey", 
     fcol = rgb(0,0.5,0, alpha = 0.2), axes = FALSE)
lines(decomp.race.WhiteE$trend, col = rgb(0,0.5,0), lwd = 3)
box()
axis(1, at = c(2007:2020), labels = c(2007:2020), cex.axis = 1.5)
axis(2, at = seq(-1.6, 0.6, by = 0.2), labels = round(seq(-1.6, 0.6, by = 0.2), 2), las = 2, cex.axis = 1.5)
title(ylab="Explicit Preference Score",
      cex.lab=2, line = 5)
abline(h = 0, lty = 2)
par(new = TRUE)
plot(forecast.raceEBlack, ylim = c(-1.6, 0.7), xlim = c(2007, 2020), main = "", axes = FALSE,
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = "darkgrey", fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.race.BlackE$trend, col =  rgb(1,0.3,0), lwd = 3)
par(new = TRUE)
plot(forecast.raceEAsian, ylim = c(-1.6, 0.7), main = "", xlim = c(2007, 2020), axes = FALSE,
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = "darkgrey", fcol = rgb(0,0,1, alpha = 0.2))
lines(decomp.race.AsianE$trend, col =  rgb(0,0.3,1), lwd = 3)
text(2008, 0.65, labels = "White", cex = 1.3, adj = 0, col = rgb(0,0.5,0.3))
text(2008, 0.35, labels = "Asian", cex = 1.3, adj = 0, col = rgb(0,0.3,1))
text(2008, -0.85, labels = "Black", cex = 1.3, adj = 0, col = rgb(1,0.3,0))
par(op)





## SKIN TONE (by race groups) --------------------------------------------------------------------------------------------------------
table(timeseriesSkin$ethnic.race.new, timeseriesSkin$month.cent) # have sufficient data from 2007 onward

## Fit to White
skin.tsEWhite <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="White",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(skin.tsEWhite$month.cent)

# impute missing values
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$cohens <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")

skin.tsEWhite <- rbind(skin.month.extra, skin.tsEWhite)
skin.tsEWhite <- skin.tsEWhite[order(skin.tsEWhite$month.cent),]
skin.tsEWhite

skin.WtsEWhite <- ts(skin.tsEWhite$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsEWhite
plot(skin.WtsEWhite)

# impute with seasonal decomposition and interpolation
skin.WtsEWhite <- na.seadec(skin.WtsEWhite, algorithm = "interpolation")
skin.WtsEWhite
plot(skin.WtsEWhite)

skin.WtsEWhite <- window(skin.WtsEWhite, start = c(2007,1))
skin.arimaWhiteE <- auto.arima(skin.WtsEWhite, stepwise = FALSE, approximation = FALSE)
skin.arimaWhiteE # parameters (0,1,3)(1,0,1)

forecast.skinWhiteE <- forecast(skin.arimaWhiteE, h = 60)
plot(forecast.skinWhiteE)

forecast.skinWhiteELONG <- forecast(skin.arimaWhiteE, h = 1800) # 150 years
plot(forecast.skinWhiteELONG)
abline(h = 0)

min(which(forecast.skinWhiteELONG$lower[,"95%"]<=0))/12 #12 yrs
min(which(forecast.skinWhiteELONG$mean<=0)) # NA
min(which(forecast.skinWhiteELONG$upper[,"95%"]>=2*skin.WtsEWhite[1]))/12 # 49 yrs, 3 mos.

## percent change
decomp.skinEWhite <- decompose(skin.WtsEWhite)
(decomp.skinEWhite$trend[7]-decomp.skinEWhite$trend[114])/decomp.skinEWhite$trend[7] # -18.19%
# initial value
skin.WtsEWhite[1] # 0.46
# final value
skin.WtsEWhite[120] # 0.32


## Fit to Asian
skin.tsEAsian <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="Asian",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.tsEAsian <- rbind(skin.month.extra, skin.tsEAsian)
skin.tsEAsian <- skin.tsEAsian[order(skin.tsEAsian$month.cent),]
skin.tsEAsian

skin.WtsEAsian <- ts(skin.tsEAsian$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsEAsian
plot(skin.WtsEAsian)

# impute with seasonal decomposition and interpolation
skin.WtsEAsian <- na.seadec(skin.WtsEAsian, algorithm = "interpolation")
skin.WtsEAsian
plot(skin.WtsEAsian)

skin.WtsEAsian <- window(skin.WtsEAsian, start = c(2007,1))
skin.arimaAsianE <- auto.arima(skin.WtsEAsian, stepwise = FALSE, approximation = FALSE)
skin.arimaAsianE # parameters (1,0,0) with non-zero mean  

forecast.skinAsianE <- forecast(skin.arimaAsianE, h = 60)
plot(forecast.skinAsianE)

forecast.skinAsianELONG <- forecast(skin.arimaAsianE, h = 1800) # 150 years
plot(forecast.skinAsianELONG)
abline(h = 0)

## percent change
decomp.skinEAsian <- decompose(skin.WtsEAsian)
(decomp.skinEAsian$trend[7]-decomp.skinEAsian$trend[114])/decomp.skinEAsian$trend[7] # +7.70%, from curvilinear trend pattern.
plot(decomp.skinEAsian)
# initial value
skin.WtsEAsian[1] # 0.39
# final value
skin.WtsEAsian[120] # 0.24



## Fit to Black/African American
skin.tsEBlack <- ddply(timeseriesSkin[timeseriesSkin$ethnic.race.new=="Black or African American",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.tsEBlack <- rbind(skin.month.extra, skin.tsEBlack)
skin.tsEBlack <- skin.tsEBlack[order(skin.tsEBlack$month.cent),]
skin.tsEBlack

skin.WtsEBlack <- ts(skin.tsEBlack$weight.mean, frequency = 12, start = c(2007, 1))
skin.WtsEBlack
plot(skin.WtsEBlack)

# impute with seasonal decomposition and interpolation
skin.WtsEBlack <- na.seadec(skin.WtsEBlack, algorithm = "interpolation")
skin.WtsEBlack
plot(skin.WtsEBlack)

skin.WtsEBlack <- window(skin.WtsEBlack, start = c(2007,1))
skin.arimaBlackE <- auto.arima(skin.WtsEBlack, stepwise = FALSE, approximation = FALSE)
skin.arimaBlackE # parameters (1,0,1)

forecast.skinBlackE <- forecast(skin.arimaBlackE, h = 60)
plot(forecast.skinBlackE)
abline(h = 0)
abline(h = 2*skin.WtsEBlack[1])

forecast.skinBlackELONG <- forecast(skin.arimaBlackE, h = 1800) # 150 years
plot(forecast.skinBlackELONG)
abline(h = 0)
abline(h = 2*skin.WtsEBlack[1])

min(which(forecast.skinBlackELONG$upper[,"95%"]>=0))/12 # 2 yrs, 3 mos.
min(which(forecast.skinBlackELONG$mean>=0)) # NA
min(which(forecast.skinBlackELONG$lower[,"95%"]<=2*skin.WtsEBlack[1]))/12 # 1 mos.

## percent change
decomp.skinEBlack <- decompose(skin.WtsEBlack)
(decomp.skinEBlack$trend[7]-decomp.skinEBlack$trend[114])/decomp.skinEBlack$trend[7] # -7.66%, again curvilinear
plot(decomp.skinEBlack)
# initial value
skin.WtsEBlack[1] # -0.16
# final value
skin.WtsEBlack[120] # -0.34


## Summary with plots
op <- par(mfrow = c(2,3))
plot(forecast.skinWhiteE, main = "White", ylim = c(-.3, .8))
abline(h = 0, lty = 2)
plot(forecast.skinBlackE, main = "Black", ylim = c(-.3, .8))
abline(h = 0, lty = 2)
plot(forecast.skinAsianE, main = "Asian", ylim = c(-.3, .8))
abline(h = 0, lty = 2)
par(op)




## Overlapping plots
plot(forecast.skinAsianE, ylim = c(-0.3, 0.75), main = "Explicit Skin-Tone Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.1), rgb(0,0.3,1, alpha = 0.1)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.1))
par(new = TRUE)
plot(forecast.skinWhiteE, ylim = c(-0.3, 0.75), main = "Explicit Skin-Tone Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinBlackE, ylim = c(-0.3, 0.75), main = "Explicit Skin-Tone Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
abline(h = 0, lty = 2)
legend("topleft", c("Asian", "Black", "White"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 




## WEIGHT (by weight groups) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$iam.new, timeseriesWeightBody$month.cent) # have sufficient data from 2007 onward
timeseriesWeightBody$weightcuts <- as.numeric(timeseriesWeightBody$iam.new)
timeseriesWeightBody$weightcuts <- cut(timeseriesWeightBody$weightcuts, c(0,3,4,7))
levels(timeseriesWeightBody$weightcuts) <- c("under", "neither","over")
table(timeseriesWeightBody$weightcuts, timeseriesWeightBody$month.cent) # months 125 - 131 have low data

table(timeseriesWeightFace$weight.COMB, timeseriesWeightFace$month.cent) # have sufficient data from 2007 onward
timeseriesWeightFace$weightcuts <- as.numeric(timeseriesWeightFace$weight.COMB)
timeseriesWeightFace$weightcuts <- cut(timeseriesWeightFace$weightcuts, c(0,3,4,7))
levels(timeseriesWeightFace$weightcuts) <- c("under", "neither","over")
table(timeseriesWeightFace$weightcuts, timeseriesWeightFace$month.cent) # all months have sufficient data


## Fit to under
weight.tsunderE <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="under",],
                        .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsunderE$month.cent)

# add in NAs for month values 125:131
which(weight.tsunderE$month.cent==125) # gives us index 47
weight.tsunderE$month.cent[47:48] # gives us month 125, 130
weight.tsunderE$mean[47:48] <- NA
weight.tsunderE$weight.mean[47:48] <- NA
weight.tsunderE$cohens[47:48] <- NA
weight.tsunderE
weight.tsunderE <- weight.tsunderE[is.na(weight.tsunderE$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 126:129, 131:132))
weight.month.extra$mean <- rep(NA, 9)
weight.month.extra$weight.mean <- rep(NA, 9)
weight.month.extra$cohens <- rep(NA, 9)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsunderE <- rbind(weight.month.extra, weight.tsunderE)
weight.tsunderE <- weight.tsunderE[order(weight.tsunderE$month.cent),]
weight.tsunderE

weight.tsunderE.face <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="under",],
                         .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsunderE$month.cent)
# keep only months 37 through to 75 to match the explicit attitudes of other domains.
weight.tsunderE.face <- weight.tsunderE.face[weight.tsunderE.face$month.cent >= 37 & weight.tsunderE.face$month.cent <= 75,]
weight.tsunderE.face <- weight.tsunderE.face[is.na(weight.tsunderE.face$month.cent)==FALSE,]
weight.tsunderEcomb <- rbind(weight.tsunderE.face, weight.tsunderE)

## make into a ts object
weight.WtsunderE <- ts(weight.tsunderEcomb$weight.mean, frequency = 12, start = c(2007, 1))
weight.WtsunderE
plot(weight.WtsunderE)

# impute with seasonal decomposition and interpolation
weight.WtsunderE <- na.seadec(weight.WtsunderE, algorithm = "interpolation")
weight.WtsunderE
plot(weight.WtsunderE)

weight.arimaunderE <- auto.arima(weight.WtsunderE, stepwise = FALSE, approximation = FALSE)
weight.arimaunderE # parameters (0,1,1)

forecast.weightunderE <- forecast(weight.arimaunderE, h = 60)
plot(forecast.weightunderE)

forecast.weightunderELONG <- forecast(weight.arimaunderE, h = 1800) # 150 years
plot(forecast.weightunderELONG)
abline(h = 0)
abline(h = 2*weight.WtsunderE[1])

min(which(forecast.weightunderELONG$lower[,"95%"]<=0))/12 # 69.667
min(which(forecast.weightunderELONG$mean<=0))/12 # NA
min(which(forecast.weightunderELONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightunderELONG$upper[,"95%"]>=2*weight.WtsunderE[1]))/12 # 112

## percent change
decomp.weightunderE <- decompose(weight.WtsunderE)
(decomp.weightunderE$trend[7]-decomp.weightunderE$trend[114])/decomp.weightunderE$trend[7] # -15.94%
# initial value
weight.WtsunderE[1] # 1.19
# final value
weight.WtsunderE[120] # 0.96



## Fit to neither under nor over
weight.tsneitherE <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="neither",],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsneitherE$month.cent)

# add in NAs for month values 125:131
weight.tsneitherE$month.cent[47:50] # gives us months 125, 126, 129, 130
weight.tsneitherE$mean[47:50] <- NA
weight.tsneitherE$weight.mean[47:50] <- NA
weight.tsneitherE$cohens[47:50] <- NA
weight.tsneitherE
weight.tsneitherE <- weight.tsneitherE[is.na(weight.tsneitherE$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 127:128, 131:132))
weight.month.extra$mean <- rep(NA, 7)
weight.month.extra$weight.mean <- rep(NA, 7)
weight.month.extra$cohens <- rep(NA, 7)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsneitherE <- rbind(weight.month.extra, weight.tsneitherE)
weight.tsneitherE <- weight.tsneitherE[order(weight.tsneitherE$month.cent),]
weight.tsneitherE

weight.tsneitherE.face <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="neither",],
                              .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsneitherE$month.cent)
# keep only months 37 through to 75 to match the explicit attitudes of other domains.
weight.tsneitherE.face <- weight.tsneitherE.face[weight.tsneitherE.face$month.cent >= 37 & weight.tsneitherE.face$month.cent <= 75,]
weight.tsneitherE.face <- weight.tsneitherE.face[is.na(weight.tsneitherE.face$month.cent)==FALSE,]
weight.tsneitherEcomb <- rbind(weight.tsneitherE.face, weight.tsneitherE)

## make into a ts object
weight.WtsneitherE <- ts(weight.tsneitherEcomb$weight.mean, frequency = 12, start = c(2007, 1))
weight.WtsneitherE
plot(weight.WtsneitherE)

# impute with seasonal decomposition and interpolation
weight.WtsneitherE <- na.seadec(weight.WtsneitherE, algorithm = "interpolation")
weight.WtsneitherE
plot(weight.WtsneitherE)

weight.arimaneitherE <- auto.arima(weight.WtsneitherE, stepwise = FALSE, approximation = FALSE)
weight.arimaneitherE # parameters (0,1,1)(1,0,0)

forecast.weightneitherE <- forecast(weight.arimaneitherE, h = 60)
plot(forecast.weightneitherE)

forecast.weightneitherELONG <- forecast(weight.arimaneitherE, h = 1800) # 150 years
plot(forecast.weightneitherELONG)
abline(h = 0)
abline(h = 2*weight.WtsneitherE[1])

min(which(forecast.weightneitherELONG$lower[,"95%"]<=0))/12 # 40.58333
min(which(forecast.weightneitherELONG$mean<=0)) # NA
min(which(forecast.weightneitherELONG$upper[,"95%"]<=0)) # NA
min(which(forecast.weightneitherELONG$upper[,"95%"]>=2*weight.WtsneitherE[1]))/12 # 66

## percent change
decomp.weightneitherE <- decompose(weight.WtsneitherE)
(decomp.weightneitherE$trend[7]-decomp.weightneitherE$trend[114])/decomp.weightneitherE$trend[7] # -9.93%
# initial value
weight.WtsneitherE[1] # 1.07
# final value
weight.WtsneitherE[120] # 0.97


## Fit to over
weight.tsoverE <- ddply(timeseriesWeightBody[timeseriesWeightBody$weightcuts=="over",],
                       .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsoverE$month.cent)

# add in NAs for month values 125:131
weight.tsoverE$month.cent[47:53] # gives us months 125:131
weight.tsoverE$mean[47:53] <- NA
weight.tsoverE$weight.mean[47:53] <- NA
weight.tsoverE$cohens[47:53] <- NA
weight.tsoverE
weight.tsoverE <- weight.tsoverE[is.na(weight.tsoverE$month.cent)==FALSE,]

# add add in NAs for missing months
weight.month.extra <- as.data.frame(c(105:107, 132))
weight.month.extra$mean <- rep(NA, 4)
weight.month.extra$weight.mean <- rep(NA, 4)
weight.month.extra$cohens <- rep(NA, 4)
names(weight.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weight.tsoverE <- rbind(weight.month.extra, weight.tsoverE)
weight.tsoverE <- weight.tsoverE[order(weight.tsoverE$month.cent),]
weight.tsoverE

weight.tsoverE.face <- ddply(timeseriesWeightFace[timeseriesWeightFace$weightcuts=="over",],
                              .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weight.tsoverE.face$month.cent)
# keep only months 37 through to 75 to match the explicit attitudes of other domains.
weight.tsoverE.face <- weight.tsoverE.face[weight.tsoverE.face$month.cent >= 37 & weight.tsoverE.face$month.cent <= 75,]
weight.tsoverE.face <- weight.tsoverE.face[is.na(weight.tsoverE.face$month.cent)==FALSE,]
weight.tsoverEcomb <- rbind(weight.tsoverE.face, weight.tsoverE)

## make into a ts object
weight.WtsoverE <- ts(weight.tsoverEcomb$weight.mean, frequency = 12, start = c(2007, 1))
weight.WtsoverE
plot(weight.WtsoverE)

# impute with seasonal decomposition and interpolation
weight.WtsoverE <- na.seadec(weight.WtsoverE, algorithm = "interpolation")
weight.WtsoverE
plot(weight.WtsoverE)

weight.arimaoverE <- auto.arima(weight.WtsoverE, stepwise = FALSE, approximation = FALSE)
weight.arimaoverE # parameters (1,1,1)

forecast.weightoverE <- forecast(weight.arimaoverE, h = 60)
plot(forecast.weightoverE)

forecast.weightoverELONG <- forecast(weight.arimaoverE, h = 1800) # 150 years
plot(forecast.weightoverELONG)
abline(h = 0)
abline(h = 2*weight.WtsoverE[1])

min(which(forecast.weightoverELONG$lower[,"95%"]<=0))/12 # 36.91667
min(which(forecast.weightoverELONG$mean<=0))/12 # NA
min(which(forecast.weightoverELONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightoverELONG$upper[,"95%"]>=2*weight.WtsoverE[1]))/12 # 70.5833

## percent change
decomp.weightoverE <- decompose(weight.WtsoverE)
(decomp.weightoverE$trend[7]-decomp.weightoverE$trend[114])/decomp.weightoverE$trend[7] # 22.83%
# initial value
weight.WtsoverE[1] # 0.65
# final value
weight.WtsoverE[120] # 0.56


## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.weightunderE, main = "Underweight", ylim = c(0.3, 1.5))
plot(forecast.weightneitherE, main = "Average Weight", ylim = c(0.3, 1.5))
plot(forecast.weightoverE, main = "Overweight", ylim = c(0.3,1.5))
par(op)

## Overlapping plots
plot(forecast.weightneitherE, ylim = c(0.3,1.5), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightunderE, ylim = c(0.3,1.5), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightoverE, ylim = c(0.3,1.5), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
legend("bottomleft", c("Average weight", "Overweight", "Underweight"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


