## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 20th, 2018

####################################################################################################################################

## Timeseries by cohort groups

####################################################################################################################################

## Load data object
load("timeseries_allattitudes_011918.RData")

# Install packages
install.packages("forecast" , dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
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


### Create year of birth variable across datasets ----------------
## And subset as "cohorts"

## AGE
timeseriesAge$year <- substr(timeseriesAge$month, start = 1, stop = 2)
timeseriesAge$year <- factor(timeseriesAge$year, labels = c(2007:2016))
table(timeseriesAge$year)
timeseriesAge$yob <- as.numeric(as.character(timeseriesAge$year)) - timeseriesAge$age.new
table(timeseriesAge$yob)

timeseriesAge$yobcats <- cut(timeseriesAge$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesAge$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesAge$yobcats)
table(timeseriesAge$yobcats, timeseriesAge$month.cent) 
# now all age categories for each year, with GenZ only relevant from 2011 onwards 
# (when the earliest born would be at least 6 yrs)


## DISABILITY
timeseriesDis$year <- substr(timeseriesDis$month, start = 1, stop = 2)
timeseriesDis$year <- factor(timeseriesDis$year, labels = c(2007:2016))
table(timeseriesDis$year)
timeseriesDis$yob <- as.numeric(as.character(timeseriesDis$year)) - timeseriesDis$age.new
table(timeseriesDis$yob)

# first, create year of birth groups for subsetting the data
timeseriesDis$yobcats <- cut(timeseriesDis$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesDis$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesDis$yobcats)
table(timeseriesDis$yobcats, timeseriesDis$month.cent) # now all age categories for each year


## SEX
timeseriesSex$year <- substr(timeseriesSex$month, start = 1, stop = 2)
timeseriesSex$year <- factor(timeseriesSex$year, labels = c(2007:2016))
table(timeseriesSex$year)
timeseriesSex$yob <- as.numeric(as.character(timeseriesSex$year)) - timeseriesSex$age.new
table(timeseriesSex$yob)

# first, create year of birth groups for subsetting the data
timeseriesSex$yobcats <- cut(timeseriesSex$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesSex$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesSex$yobcats, timeseriesSex$month.cent) # now all age categories for each year
table(timeseriesSex$yobcats)

## RACE
timeseriesRace$year <- substr(timeseriesRace$month, start = 1, stop = 2)
timeseriesRace$year <- factor(timeseriesRace$year, labels = c(2007:2016))
table(timeseriesRace$year)
timeseriesRace$yob <- as.numeric(as.character(timeseriesRace$year)) - timeseriesRace$age.new
table(timeseriesRace$yob)

# first, create year of birth groups for subsetting the data
timeseriesRace$yobcats <- cut(timeseriesRace$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesRace$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesRace$yobcats, timeseriesRace$month.cent) # now all age categories for each year
table(timeseriesRace$yobcats)

## SKIN
timeseriesSkin$year <- substr(timeseriesSkin$month, start = 1, stop = 2)
timeseriesSkin$year <- factor(timeseriesSkin$year, labels = c(2007:2016))
table(timeseriesSkin$year)
timeseriesSkin$yob <- as.numeric(as.character(timeseriesSkin$year)) - timeseriesSkin$age.new
table(timeseriesSkin$yob)

# first, create year of birth groups for subsetting the data
timeseriesSkin$yobcats <- cut(timeseriesSkin$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesSkin$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesSkin$yobcats, timeseriesSkin$month.cent) # now all age categories for each year
table(timeseriesSkin$yobcats)

## BODY WEIGHT (BODY)
timeseriesWeightBody$year <- substr(timeseriesWeightBody$month, start = 1, stop = 2)
timeseriesWeightBody$year <- factor(timeseriesWeightBody$year, labels = c(2010:2016))
table(timeseriesWeightBody$year)
timeseriesWeightBody$yob <- as.numeric(as.character(timeseriesWeightBody$year)) - timeseriesWeightBody$age.new
table(timeseriesWeightBody$yob)

# first, create year of birth groups for subsetting the data
timeseriesWeightBody$yobcats <- cut(timeseriesWeightBody$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesWeightBody$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesWeightBody$yobcats, timeseriesWeightBody$month.cent) # now all age categories for each year
table(timeseriesWeightBody$yobcats)

## BODY WEIGHT (FACE)
timeseriesWeightFace$year <- substr(timeseriesWeightFace$month, start = 1, stop = 2)
timeseriesWeightFace$year <- factor(timeseriesWeightFace$year, labels = c(2004:2011))
table(timeseriesWeightFace$year)
timeseriesWeightFace$yob <- as.numeric(as.character(timeseriesWeightFace$year)) - timeseriesWeightFace$age.new
table(timeseriesWeightFace$yob)

# first, create year of birth groups for subsetting the data
timeseriesWeightFace$yobcats <- cut(timeseriesWeightFace$yob, c(1918,1945,1964,1976,1995, 2009))
levels(timeseriesWeightFace$yobcats) <- c("silent","boomers", "genX", "millenials",
                                    "genZ")
table(timeseriesWeightFace$yobcats, timeseriesWeightFace$month.cent) # now all age categories for each year
table(timeseriesWeightFace$yobcats)


## IMPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE  --------------------------------------------------------------------------------------------------------
## Fit to Boomers
age.tsboom <- ddply(timeseriesAge[timeseriesAge$yobcats=="boomers",],
               .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wtsboom <- ts(age.tsboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.Wtsboom
plot(age.Wtsboom)

age.arimaboom <- auto.arima(age.Wtsboom, stepwise = FALSE, approximation = FALSE)
age.arimaboom # parameters (0,0,2)

forecast.ageboom <- forecast(age.arimaboom, h = 60)
plot(forecast.ageboom)

forecast.ageboomLONG <- forecast(age.arimaboom, h = 1800) # 150 years
plot(forecast.ageboomLONG)
abline(h = 0)

min(which(forecast.ageboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageboomLONG$mean<=0))/12 # NA
min(which(forecast.ageboomLONG$upper[,"95%"]<=0))/12 # NA

## percent change
decomp.ageboom <- decompose(age.Wtsboom)
(decomp.ageboom$trend[7]-decomp.ageboom$trend[114])/decomp.ageboom$trend[7] # +1.24%
# initial value
age.Wtsboom[1] # 0.46
# final value
age.Wtsboom[120] # 0.45


## Fit to Gen X
age.tsgenX <- ddply(timeseriesAge[timeseriesAge$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.WtsgenX <- ts(age.tsgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsgenX
plot(age.WtsgenX)

age.arimagenX <- auto.arima(age.WtsgenX, stepwise = FALSE, approximation = FALSE)
age.arimagenX # parameters (2,1,1)

forecast.agegenX <- forecast(age.arimagenX, h = 60)
plot(forecast.agegenX)

forecast.agegenXLONG <- forecast(age.arimagenX, h = 1800) # 150 years
plot(forecast.agegenXLONG)
abline(h = 0)

min(which(forecast.agegenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.agegenXLONG$mean<=0))/12 # NA
min(which(forecast.agegenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.agegenXLONG$upper[,"95%"]>=2*age.WtsgenX[1]))/12 # NA

## percent change
decomp.agegenX <- decompose(age.WtsgenX)
(decomp.agegenX$trend[7]-decomp.agegenX$trend[114])/decomp.agegenX$trend[7] # -4.63%
# initial value
age.WtsgenX[1] # 0.46
# final value
age.WtsgenX[120] # 0.45

## Fit to Millenials
age.tsmillenials <- ddply(timeseriesAge[timeseriesAge$yobcats=="millenials",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.Wtsmillenials <- ts(age.tsmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.Wtsmillenials
plot(age.Wtsmillenials)

age.arimamillenials <- auto.arima(age.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
age.arimamillenials # parameters (0,1,2)(2,0,0)

forecast.agemillenials <- forecast(age.arimamillenials, h = 60)
plot(forecast.agemillenials)

forecast.agemillenialsLONG <- forecast(age.arimamillenials, h = 1800) # 150 years
plot(forecast.agemillenialsLONG)
abline(h = 0)

min(which(forecast.agemillenialsLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.agemillenialsLONG$mean<=0))/12 # NA
min(which(forecast.agemillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.agemillenialsLONG$upper[,"95%"]>=2*age.Wtsmillenials[1]))/12 # NA

## percent change
decomp.agemillenials <- decompose(age.Wtsmillenials)
(decomp.agemillenials$trend[7]-decomp.agemillenials$trend[114])/decomp.agemillenials$trend[7] # -3.92%
# initial value
age.Wtsmillenials[1] # 0.45
# final value
age.Wtsmillenials[120] # 0.43


## Fit to Gen Z
age.tsgenZ <- ddply(timeseriesAge[timeseriesAge$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
age.WtsgenZ <- ts(age.tsgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsgenZ
plot(age.WtsgenZ)
age.WtsgenZ <- window(age.WtsgenZ, start = c(2011, 1), end = c(2016, 12))
plot(age.WtsgenZ)

age.arimagenZ <- auto.arima(age.WtsgenZ, stepwise = FALSE, approximation = FALSE)
age.arimagenZ # parameters (0,1,1)

forecast.agegenZ <- forecast(age.arimagenZ, h = 60)
plot(forecast.agegenZ)

forecast.agegenZLONG <- forecast(age.arimagenZ, h = 1800) # 150 years
plot(forecast.agegenZLONG)
abline(h = 0)

min(which(forecast.agegenZLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.agegenZLONG$mean<=0))/12 # NA
min(which(forecast.agegenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.agegenZLONG$upper[,"95%"]>=2*age.WtsgenZ[1]))/12 # NA

## percent change
decomp.agegenZ <- decompose(age.WtsgenZ)
(decomp.agegenZ$trend[7]-decomp.agegenZ$trend[66])/decomp.agegenZ$trend[7] # +10.59%
# initial value
age.WtsgenZ[1] # 0.38
# final value
age.WtsgenZ[72] # 0.41

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.ageboom, main = "Boomers", ylim = c(0.3, 0.52))
plot(forecast.agegenX, main = "Gen X", ylim = c(0.3, 0.52))
plot(forecast.agemillenials, main = "Millenials", ylim = c(0.3, 0.52))
plot(forecast.agegenZ, main = "Gen Z", ylim = c(0.3, 0.52), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecast.ageboom, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agemillenials, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agegenX, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 



## DISABILITY --------------------------------------------------------------------------------------------------------
table(timeseriesDis$yobcats, timeseriesDis$month.cent) # now all age categories for each year

## Fit to Boomers
dis.tsboom <- ddply(timeseriesDis[timeseriesDis$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                    cohens = cohensD(dscore))
dis.Wtsboom <- ts(dis.tsboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.Wtsboom
plot(dis.Wtsboom)

dis.arimaboom <- auto.arima(dis.Wtsboom, stepwise = FALSE, approximation = FALSE) 
dis.arimaboom # parameters (0,1,1)

forecast.disboom <- forecast(dis.arimaboom, h = 60)
plot(forecast.disboom)

forecast.disboomLONG <- forecast(dis.arimaboom, h = 1800) # 150 years
plot(forecast.disboomLONG)
abline(h = 0)

min(which(forecast.disboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disboomLONG$mean<=0))/12 # NA
min(which(forecast.disboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disboomLONG$upper[,"95%"]>=2*dis.Wtsboom[1]))/12 # NA

## percent change
decomp.disboom <- decompose(dis.Wtsboom)
(decomp.disboom$trend[7]-decomp.disboom$trend[114])/decomp.disboom$trend[7] # +9.18%
# initial value
dis.Wtsboom[1] # 0.59
# final value
dis.Wtsboom[120] # 0.73


## Fit to Gen X
dis.tsgenX <- ddply(timeseriesDis[timeseriesDis$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.WtsgenX <- ts(dis.tsgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WtsgenX
plot(dis.WtsgenX)

dis.arimdisnX <- auto.arima(dis.WtsgenX, stepwise = FALSE, approximation = FALSE)
dis.arimdisnX # parameters (0,1,3)

forecast.disgenX <- forecast(dis.arimdisnX, h = 60)
plot(forecast.disgenX)

forecast.disgenXLONG <- forecast(dis.arimdisnX, h = 1800) # 150 years
plot(forecast.disgenXLONG)
abline(h = 0)

min(which(forecast.disgenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disgenXLONG$mean<=0))/12 # NA
min(which(forecast.disgenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disgenXLONG$upper[,"95%"]>=2*dis.WtsgenX[1]))/12 # NA

## percent change
decomp.disgenX <- decompose(dis.WtsgenX)
(decomp.disgenX$trend[7]-decomp.disgenX$trend[114])/decomp.disgenX$trend[7] # +12.40%
# initial value
dis.WtsgenX[1] # 0.50
# final value
dis.WtsgenX[120] # 0.57

## Fit to Millenials
dis.tsmillenials <- ddply(timeseriesDis[timeseriesDis$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.Wtsmillenials <- ts(dis.tsmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.Wtsmillenials
plot(dis.Wtsmillenials)

dis.arimamillenials <- auto.arima(dis.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
dis.arimamillenials # parameters (0,0,0)

forecast.dismillenials <- forecast(dis.arimamillenials, h = 60)
plot(forecast.dismillenials)

forecast.dismillenialsLONG <- forecast(dis.arimamillenials, h = 1800) # 150 years
plot(forecast.dismillenialsLONG)
abline(h = 0)

min(which(forecast.dismillenialsLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.dismillenialsLONG$mean<=0))/12 # NA
min(which(forecast.dismillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.dismillenialsLONG$upper[,"95%"]>=2*dis.Wtsmillenials[1]))/12 # NA

## percent change
decomp.dismillenials <- decompose(dis.Wtsmillenials)
(decomp.dismillenials$trend[7]-decomp.dismillenials$trend[114])/decomp.dismillenials$trend[7] # +0.67%
# initial value
dis.Wtsmillenials[1] # 0.50
# final value
dis.Wtsmillenials[120] # 0.49


## Fit to Gen Z
dis.tsgenZ <- ddply(timeseriesDis[timeseriesDis$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
dis.tsgenZ <- dis.tsgenZ[dis.tsgenZ$month.cent >= 85,]

dis.WtsgenZ <- ts(dis.tsgenZ$weight.mean, frequency = 12, start = c(2011, 1))
dis.WtsgenZ
plot(dis.WtsgenZ)

dis.arimdisnZ <- auto.arima(dis.WtsgenZ, stepwise = FALSE, approximation = FALSE)
dis.arimdisnZ # parameters (0,0,1)

forecast.disgenZ <- forecast(dis.arimdisnZ, h = 60)
plot(forecast.disgenZ)

forecast.disgenZLONG <- forecast(dis.arimdisnZ, h = 1800) # 150 years
plot(forecast.disgenZLONG)
abline(h = 0)

min(which(forecast.disgenZLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disgenZLONG$mean<=0))/12 # NA
min(which(forecast.disgenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disgenZLONG$upper[,"95%"]>=2*dis.WtsgenZ[1]))/12 # NA

## percent change
decomp.disgenZ <- decompose(dis.WtsgenZ)
(decomp.disgenZ$trend[7]-decomp.disgenZ$trend[66])/decomp.disgenZ$trend[7] # +27.55%
# initial value
dis.WtsgenZ[1] # 0.42
# final value
dis.WtsgenZ[72] # 0.47

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.disboom, main = "Boomers", ylim = c(0.2, 0.76))
plot(forecast.disgenX, main = "Gen X", ylim = c(0.2, 0.76))
plot(forecast.dismillenials, main = "Millenials", ylim = c(0.2, 0.76))
plot(forecast.disgenZ, main = "Gen Z", ylim = c(0.2, 0.76))
par(op)



## Overlapping plots
plot(forecast.disboom, ylim = c(0.35, 0.76), main = "Implicit Dis Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.dismillenials, ylim = c(0.35, 0.76), main = "Implicit Dis Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disgenX, ylim = c(0.35, 0.76), main = "Implicit Dis Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2, ncol = 3) 



## RACE --------------------------------------------------------------------------------------------------------
table(timeseriesRace$yobcats, timeseriesRace$month.cent) # now all age categories for each year

## Fit to Boomers
race.tsboom <- ddply(timeseriesRace[timeseriesRace$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.Wtsboom <- ts(race.tsboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.Wtsboom
plot(race.Wtsboom)

race.arimaboom <- auto.arima(race.Wtsboom, stepwise = FALSE, approximation = FALSE)
race.arimaboom # parameters (0,0,4)

forecast.raceboom <- forecast(race.arimaboom, h = 60)
plot(forecast.raceboom)

forecast.raceboomLONG <- forecast(race.arimaboom, h = 1800) # 150 years
plot(forecast.raceboomLONG)
abline(h = 0)

min(which(forecast.raceboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.raceboomLONG$mean<=0))/12 # NA
min(which(forecast.raceboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceboomLONG$upper[,"95%"]>=2*race.Wtsboom[1]))/12 # NA

## percent change
decomp.raceboom <- decompose(race.Wtsboom)
(decomp.raceboom$trend[7]-decomp.raceboom$trend[114])/decomp.raceboom$trend[7] # -1.47%
# initial value
race.Wtsboom[1] # 0.33
# final value
race.Wtsboom[120] # 0.33


## Fit to Gen X
race.tsgenX <- ddply(timeseriesRace[timeseriesRace$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.WtsgenX <- ts(race.tsgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsgenX
plot(race.WtsgenX)

race.arimracenX <- auto.arima(race.WtsgenX, stepwise = FALSE, approximation = FALSE)
race.arimracenX # parameters (0,0,2)

forecast.racegenX <- forecast(race.arimracenX, h = 60)
plot(forecast.racegenX)

forecast.racegenXLONG <- forecast(race.arimracenX, h = 1800) # 150 years
plot(forecast.racegenXLONG)
abline(h = 0)

min(which(forecast.racegenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.racegenXLONG$mean<=0))/12 # NA
min(which(forecast.racegenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.racegenXLONG$upper[,"95%"]>=2*race.WtsgenX[1]))/12 # NA

## percent change
decomp.racegenX <- decompose(race.WtsgenX)
(decomp.racegenX$trend[7]-decomp.racegenX$trend[114])/decomp.racegenX$trend[7] # -13.83%
# initial value
race.WtsgenX[1] # 0.29
# final value
race.WtsgenX[120] # 0.28

## Fit to Millenials
race.tsmillenials <- ddply(timeseriesRace[timeseriesRace$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.Wtsmillenials <- ts(race.tsmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.Wtsmillenials
plot(race.Wtsmillenials)

race.arimamillenials <- auto.arima(race.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
race.arimamillenials # parameters (1,1,1)(1,0,1)

forecast.racemillenials <- forecast(race.arimamillenials, h = 60)
plot(forecast.racemillenials)

forecast.racemillenialsLONG <- forecast(race.arimamillenials, h = 1800) # 150 years
plot(forecast.racemillenialsLONG)
abline(h = 0)

min(which(forecast.racemillenialsLONG$lower[,"95%"]<=0))/12 # 19
min(which(forecast.racemillenialsLONG$mean<=0))/12 # NA
min(which(forecast.racemillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.racemillenialsLONG$upper[,"95%"]>=2*race.Wtsmillenials[1]))/12 # 57.0833

## percent change
decomp.racemillenials <- decompose(race.Wtsmillenials)
(decomp.racemillenials$trend[7]-decomp.racemillenials$trend[114])/decomp.racemillenials$trend[7] # -19.98%
# initial value
race.Wtsmillenials[1] # 0.34
# final value
race.Wtsmillenials[120] # 0.29


## Fit to Gen Z
race.tsgenZ <- ddply(timeseriesRace[timeseriesRace$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
race.WtsgenZ <- ts(race.tsgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsgenZ
plot(race.WtsgenZ)
race.WtsgenZ <- window(race.WtsgenZ, start = c(2011, 1), end = c(2016, 12))
plot(race.WtsgenZ)

race.arimracenZ <- auto.arima(race.WtsgenZ, stepwise = FALSE, approximation = FALSE)
race.arimracenZ # parameters (0,1,1)

forecast.racegenZ <- forecast(race.arimracenZ, h = 60)
plot(forecast.racegenZ)

forecast.racegenZLONG <- forecast(race.arimracenZ, h = 1800) # 150 years
plot(forecast.racegenZLONG)
abline(h = 0)

min(which(forecast.racegenZLONG$lower[,"95%"]<=0))/12 # 23
min(which(forecast.racegenZLONG$mean<=0))/12 # NA
min(which(forecast.racegenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.racegenZLONG$upper[,"95%"]>=2*race.WtsgenZ[1]))/12 # 19.91667

## percent change
decomp.racegenZ <- decompose(race.WtsgenZ)
(decomp.racegenZ$trend[7]-decomp.racegenZ$trend[66])/decomp.racegenZ$trend[7] # -5.62%
# initial value
race.WtsgenZ[1] # 0.29
# final value
race.WtsgenZ[72] # 0.31

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.raceboom, main = "Boomers", ylim = c(0, 0.5))
plot(forecast.racegenX, main = "Gen X", ylim = c(0, 0.5))
plot(forecast.racemillenials, main = "Millenials", ylim = c(0, 0.5))
plot(forecast.racegenZ, main = "Gen Z", ylim = c(0, 0.5), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecast.raceboom, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racemillenials, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racegenX, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## SEXUALITY  --------------------------------------------------------------------------------------------------------
table(timeseriesSex$yobcats, timeseriesSex$month.cent) # now all age categories for each year

## Fit to Boomers
sex.tsboom <- ddply(timeseriesSex[timeseriesSex$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.Wtsboom <- ts(sex.tsboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.Wtsboom
plot(sex.Wtsboom)

sex.arimaboom <- auto.arima(sex.Wtsboom, stepwise = FALSE, approximation = FALSE)
sex.arimaboom # parameters (0,1,1)

forecast.sexboom <- forecast(sex.arimaboom, h = 60)
plot(forecast.sexboom)

forecast.sexboomLONG <- forecast(sex.arimaboom, h = 1800) # 150 years
plot(forecast.sexboomLONG)
abline(h = 0)

min(which(forecast.sexboomLONG$lower[,"95%"]<=0))/12 # 144.0833
min(which(forecast.sexboomLONG$mean<=0))/12 # NA
min(which(forecast.sexboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexboomLONG$upper[,"95%"]>=2*sex.Wtsboom[1]))/12 # 137.25

## percent change
decomp.sexboom <- decompose(sex.Wtsboom)
(decomp.sexboom$trend[7]-decomp.sexboom$trend[114])/decomp.sexboom$trend[7] # -11.27%
# initial value
sex.Wtsboom[1] # 0.28
# final value
sex.Wtsboom[120] # 0.21


## Fit to Gen X
sex.tsgenX <- ddply(timeseriesSex[timeseriesSex$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.WtsgenX <- ts(sex.tsgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsgenX
plot(sex.WtsgenX)

sex.arimsexnX <- auto.arima(sex.WtsgenX, stepwise = FALSE, approximation = FALSE)
sex.arimsexnX # parameters (0,1,1)

forecast.sexgenX <- forecast(sex.arimsexnX, h = 60)
plot(forecast.sexgenX)

forecast.sexgenXLONG <- forecast(sex.arimsexnX, h = 1800) # 150 years
plot(forecast.sexgenXLONG)
abline(h = 0)

min(which(forecast.sexgenXLONG$lower[,"95%"]<=0))/12 # 29.833
min(which(forecast.sexgenXLONG$mean<=0))/12 # NA
min(which(forecast.sexgenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexgenXLONG$upper[,"95%"]>=2*sex.WtsgenX[1]))/12 # 57.1667

## percent change
decomp.sexgenX <- decompose(sex.WtsgenX)
(decomp.sexgenX$trend[7]-decomp.sexgenX$trend[114])/decomp.sexgenX$trend[7] # -12.57%
# initial value
sex.WtsgenX[1] # 0.30
# final value
sex.WtsgenX[120] # 0.21

## Fit to Millenials
sex.tsmillenials <- ddply(timeseriesSex[timeseriesSex$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.Wtsmillenials <- ts(sex.tsmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.Wtsmillenials
plot(sex.Wtsmillenials)

sex.arimamillenials <- auto.arima(sex.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
sex.arimamillenials # parameters (1,1,1)(2,0,0)

forecast.sexmillenials <- forecast(sex.arimamillenials, h = 60)
plot(forecast.sexmillenials)

forecast.sexmillenialsLONG <- forecast(sex.arimamillenials, h = 1800) # 150 years
plot(forecast.sexmillenialsLONG)
abline(h = 0)

min(which(forecast.sexmillenialsLONG$lower[,"95%"]<=0))/12 # 9.5833
min(which(forecast.sexmillenialsLONG$mean<=0))/12 # NA
min(which(forecast.sexmillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexmillenialsLONG$upper[,"95%"]>=2*sex.Wtsmillenials[1]))/12 # 53.41667

## percent change
decomp.sexmillenials <- decompose(sex.Wtsmillenials)
(decomp.sexmillenials$trend[7]-decomp.sexmillenials$trend[114])/decomp.sexmillenials$trend[7] # -31.57%
# initial value
sex.Wtsmillenials[1] # 0.34
# final value
sex.Wtsmillenials[120] # 0.16


## Fit to Gen Z
sex.tsgenZ <- ddply(timeseriesSex[timeseriesSex$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
sex.WtsgenZ <- ts(sex.tsgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsgenZ
plot(sex.WtsgenZ)
sex.WtsgenZ <- window(sex.WtsgenZ, start = c(2011, 1), end = c(2016, 12))
plot(sex.WtsgenZ)

sex.arimsexnZ <- auto.arima(sex.WtsgenZ, stepwise = FALSE, approximation = FALSE)
sex.arimsexnZ # parameters (1,1,3)(1,0,0)

forecast.sexgenZ <- forecast(sex.arimsexnZ, h = 60)
plot(forecast.sexgenZ)

forecast.sexgenZLONG <- forecast(sex.arimsexnZ, h = 1800) # 150 years
plot(forecast.sexgenZLONG)
abline(h = 0)

min(which(forecast.sexgenZLONG$lower[,"95%"]<=0))/12 # 43.667
min(which(forecast.sexgenZLONG$mean<=0))/12 # NA
min(which(forecast.sexgenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexgenZLONG$upper[,"95%"]>=2*sex.WtsgenZ[1]))/12 # 90.1667

## percent change
decomp.sexgenZ <- decompose(sex.WtsgenZ)
(decomp.sexgenZ$trend[7]-decomp.sexgenZ$trend[66])/decomp.sexgenZ$trend[7] # -21.18%
# initial value
sex.WtsgenZ[1] # 0.25
# final value
sex.WtsgenZ[72] # 0.26

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.sexboom, main = "Boomers", ylim = c(0, 0.52))
plot(forecast.sexgenX, main = "Gen X", ylim = c(0, 0.52))
plot(forecast.sexmillenials, main = "Millenials", ylim = c(0, 0.52))
plot(forecast.sexgenZ, main = "Gen Z", ylim = c(0, 0.52), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecast.sexboom, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.1), rgb(0,0.5,0.3, alpha = 0.1)), col = rgb(0,0.5,0.3, alpha = 0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
lines(decomp.sexboom$trend, col = rgb(0,0.5,0.3), lwd = 3)
par(new = TRUE)
plot(forecast.sexmillenials, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.1), rgb(1,0.3,0, alpha = 0.1)), col = rgb(1,0.3,0, alpha = 0.3), fcol = rgb(1,0,0, alpha = 0.2))
lines(decomp.sexmillenials$trend, col = rgb(1,0.3,0), lwd = 3)
par(new = TRUE)
plot(forecast.sexgenX, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.1), rgb(0,0.3,1, alpha = 0.1)), col = rgb(0,0.3,1, alpha = 0.3), fcol = rgb(0,0,1, alpha = 0.2))
lines(decomp.sexgenX$trend, col = rgb(0,0.3,1), lwd = 3)
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## SKIN TONE  --------------------------------------------------------------------------------------------------------
table(timeseriesSkin$yobcats, timeseriesSkin$month.cent) # now all age categories for each year

## Fit to Boomers
skin.tsboom <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

# impute missing values
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$cohens <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")

skin.tsboom <- rbind(skin.month.extra, skin.tsboom)
skin.tsboom <- skin.tsboom[order(skin.tsboom$month.cent),]
skin.tsboom

skin.Wtsboom <- ts(skin.tsboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.Wtsboom
plot(skin.Wtsboom)

# impute with seasonal decomposition and interpolation
skin.Wtsboom <- na.seadec(skin.Wtsboom, algorithm = "interpolation")
skin.Wtsboom
plot(skin.Wtsboom)

skin.arimaboom <- auto.arima(skin.Wtsboom, stepwise = FALSE, approximation = FALSE)
skin.arimaboom # parameters (0,0,2)

forecast.skinboom <- forecast(skin.arimaboom, h = 60)
plot(forecast.skinboom)

forecast.skinboomLONG <- forecast(skin.arimaboom, h = 1800) # 150 years
plot(forecast.skinboomLONG)
abline(h = 0)

min(which(forecast.skinboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinboomLONG$mean<=0))/12 # NA
min(which(forecast.skinboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinboomLONG$upper[,"95%"]>=2*skin.Wtsboom[1]))/12 # NA

## percent change
decomp.skinboom <- decompose(skin.Wtsboom)
(decomp.skinboom$trend[7]-decomp.skinboom$trend[114])/decomp.skinboom$trend[7] # +4.20%
plot(decomp.skinboom$trend)
# initial value
skin.Wtsboom[1] # 0.35
# final value
skin.Wtsboom[120] # 0.42


## Fit to Gen X
skin.tsgenX <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsgenX <- rbind(skin.month.extra, skin.tsgenX)
skin.tsgenX <- skin.tsgenX[order(skin.tsgenX$month.cent),]
skin.tsgenX

skin.WtsgenX <- ts(skin.tsgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsgenX
plot(skin.WtsgenX)

# impute with seasonal decomposition and interpolation
skin.WtsgenX <- na.seadec(skin.WtsgenX, algorithm = "interpolation")
skin.WtsgenX
plot(skin.WtsgenX)

skin.arimagenX <- auto.arima(skin.WtsgenX, stepwise = FALSE, approximation = FALSE)
skin.arimagenX # parameters (3,0,1)

forecast.skingenX <- forecast(skin.arimagenX, h = 60)
plot(forecast.skingenX)

forecast.skingenXLONG <- forecast(skin.arimagenX, h = 1800) # 150 years
plot(forecast.skingenXLONG)
abline(h = 0)

min(which(forecast.skingenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skingenXLONG$mean<=0))/12 # NA
min(which(forecast.skingenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skingenXLONG$upper[,"95%"]>=2*skin.WtsgenX[1]))/12 # NA

## percent change
decomp.skingenX <- decompose(skin.WtsgenX)
(decomp.skingenX$trend[7]-decomp.skingenX$trend[114])/decomp.skingenX$trend[7] # +3.10%
plot(decomp.skingenX$trend)
# initial value
skin.WtsgenX[1] # 0.32
# final value
skin.WtsgenX[120] # 0.35


## Fit to Millenials
skin.tsmillenials <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="millenials",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsmillenials <- rbind(skin.month.extra, skin.tsmillenials)
skin.tsmillenials <- skin.tsmillenials[order(skin.tsmillenials$month.cent),]
skin.tsmillenials

skin.Wtsmillenials <- ts(skin.tsmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.Wtsmillenials
plot(skin.Wtsmillenials)

# impute with seasonal decomposition and interpolation
skin.Wtsmillenials <- na.seadec(skin.Wtsmillenials, algorithm = "interpolation")
skin.Wtsmillenials
plot(skin.Wtsmillenials)

skin.arimamillenials <- auto.arima(skin.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
skin.arimamillenials # parameters (2,1,3)

forecast.skinmillenials <- forecast(skin.arimamillenials, h = 60)
plot(forecast.skinmillenials)

forecast.skinmillenialsLONG <- forecast(skin.arimamillenials, h = 1800) # 150 years
plot(forecast.skinmillenialsLONG)
abline(h = 0)

min(which(forecast.skinmillenialsLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinmillenialsLONG$mean<=0))/12 # NA
min(which(forecast.skinmillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinmillenialsLONG$upper[,"95%"]>=2*skin.Wtsmillenials[1]))/12 # NA

## percent change
decomp.skinmillenials <- decompose(skin.Wtsmillenials)
(decomp.skinmillenials$trend[7]-decomp.skinmillenials$trend[114])/decomp.skinmillenials$trend[7] # -12.40%
# initial value
skin.Wtsmillenials[1] # 0.32
# final value
skin.Wtsmillenials[120] # 0.27



## Fit to Gen Z
skin.tsgenZ <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))

skin.tsgenZ <- rbind(skin.month.extra, skin.tsgenZ)
skin.tsgenZ <- skin.tsgenZ[order(skin.tsgenZ$month.cent),]
skin.tsgenZ

skin.WtsgenZ <- ts(skin.tsgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsgenZ
plot(skin.WtsgenZ)

skin.WtsgenZ <- window(skin.WtsgenZ, start = c(2011, 1), end = c(2016, 12))
plot(skin.WtsgenZ)

# impute with seasonal decomposition and interpolation
skin.WtsgenZ <- na.seadec(skin.WtsgenZ, algorithm = "interpolation")
skin.WtsgenZ
plot(skin.WtsgenZ)

skin.arimskingenZ <- auto.arima(skin.WtsgenZ, stepwise = FALSE, approximation = FALSE)
skin.arimskingenZ # parameters (0,1,2)

forecast.skingenZ <- forecast(skin.arimskingenZ, h = 60)
plot(forecast.skingenZ)

forecast.skingenZLONG <- forecast(skin.arimskingenZ, h = 1800) # 150 years
plot(forecast.skingenZLONG)
abline(h = 0)

min(which(forecast.skingenZLONG$lower[,"95%"]<=0))/12 # 45.91667
min(which(forecast.skingenZLONG$mean<=0))/12 # NA
min(which(forecast.skingenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skingenZLONG$upper[,"95%"]>=2*skin.WtsgenZ[1]))/12 # 130.75

## percent change
decomp.skingenZ <- decompose(skin.WtsgenZ)
(decomp.skingenZ$trend[7]-decomp.skingenZ$trend[66])/decomp.skingenZ$trend[7] # -14.12%
# initial value
skin.WtsgenZ[1] # 0.33
# final value
skin.WtsgenZ[72] # 0.26

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.skinboom, main = "Boomers", ylim = c(0.14, 0.45))
plot(forecast.skingenX, main = "Gen X", ylim = c(0.14, 0.45))
plot(forecast.skinmillenials, main = "Millenials", ylim = c(0.14, 0.45))
plot(forecast.skingenZ, main = "Gen Z", ylim = c(0.14, 0.45), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecast.skinboom, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinmillenials, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skingenX, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 





## BODY WEIGHT (BODY) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$yobcats, timeseriesWeightBody$month.cent) # now all age categories for each year

## Fit to Boomers
weightBody.tsboom <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="boomers",],
                     .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightBody.tsboom$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsboom$month.cent[47:51] # gives us months 125:130, missing 128
weightBody.tsboom$mean[47:51] <- NA
weightBody.tsboom$weight.mean[47:51] <- NA
weightBody.tsboom$cohens[47:51] <- NA
weightBody.tsboom

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 128, 131:132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsboom <- rbind(weightBody.month.extra, weightBody.tsboom)
weightBody.tsboom <- weightBody.tsboom[order(weightBody.tsboom$month.cent),]
weightBody.tsboom

weightBody.Wtsboom <- ts(weightBody.tsboom$weight.mean, frequency = 12, start = c(2010, 4), end = c(2016,12))
weightBody.Wtsboom
plot(weightBody.Wtsboom)

# impute with seasonal decomposition and interpolation
weightBody.Wtsboom <- na.seadec(weightBody.Wtsboom, algorithm = "interpolation")
weightBody.Wtsboom
plot(weightBody.Wtsboom)

weightBody.arimaboom <- auto.arima(weightBody.Wtsboom, stepwise = FALSE, approximation = FALSE)
weightBody.arimaboom # parameters (2,1,1)

forecast.weightBodyboom <- forecast(weightBody.arimaboom, h = 60)
plot(forecast.weightBodyboom)

forecast.weightBodyboomLONG <- forecast(weightBody.arimaboom, h = 1800) # 150 years
plot(forecast.weightBodyboomLONG)
abline(h = 0)

min(which(forecast.weightBodyboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyboomLONG$mean<=0))/12 # NA
min(which(forecast.weightBodyboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyboomLONG$upper[,"95%"]>=2*weightBody.Wtsboom[1]))/12 # NA

## percent change
decomp.weightBodyboom <- decompose(weightBody.Wtsboom)
(decomp.weightBodyboom$trend[7]-decomp.weightBodyboom$trend[75])/decomp.weightBodyboom$trend[7] # +4.34%
plot(decomp.weightBodyboom)
# initial value
weightBody.Wtsboom[1] # 0.50
# final value
weightBody.Wtsboom[81] # 0.53

## Fit to Gen X
weightBody.tsgenX <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="genX",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightBody.tsgenX$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsgenX$month.cent[47:51] # gives us months 125:130, missing 127:128
weightBody.tsgenX$mean[47:51] <- NA
weightBody.tsgenX$weight.mean[47:51] <- NA
weightBody.tsgenX$cohens[47:51] <- NA
weightBody.tsgenX

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 127:128, 132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsgenX <- rbind(weightBody.month.extra, weightBody.tsgenX)
weightBody.tsgenX <- weightBody.tsgenX[order(weightBody.tsgenX$month.cent),]
weightBody.tsgenX

weightBody.WtsgenX <- ts(weightBody.tsgenX$weight.mean, frequency = 12, start = c(2010, 4), end = c(2016,12))
weightBody.WtsgenX
plot(weightBody.WtsgenX)

# impute with seasonal decomposition and interpolation
weightBody.WtsgenX <- na.seadec(weightBody.WtsgenX, algorithm = "interpolation")
weightBody.WtsgenX
plot(weightBody.WtsgenX)

weightBody.arimagenX <- auto.arima(weightBody.WtsgenX, stepwise = FALSE, approximation = FALSE)
weightBody.arimagenX # parameters (1,0,0)

forecast.weightBodygenX <- forecast(weightBody.arimagenX, h = 60)
plot(forecast.weightBodygenX)

forecast.weightBodygenXLONG <- forecast(weightBody.arimagenX, h = 1800) # 150 years
plot(forecast.weightBodygenXLONG)
abline(h = 0)

min(which(forecast.weightBodygenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodygenXLONG$mean<=0))/12 # NA
min(which(forecast.weightBodygenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodygenXLONG$upper[,"95%"]>=2*weightBody.WtsgenX[1]))/12 # NA

## percent change
decomp.weightBodygenX <- decompose(weightBody.WtsgenX)
(decomp.weightBodygenX$trend[7]-decomp.weightBodygenX$trend[75])/decomp.weightBodygenX$trend[7] # +7.69%
plot(decomp.weightBodygenX)
# initial value
weightBody.WtsgenX[1] # 0.55
# final value
weightBody.WtsgenX[81] # 0.51

## Fit to Millenials
weightBody.tsmillenials <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="millenials",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightBody.tsmillenials$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsmillenials$month.cent[47:51] # gives us months 125:130, missing 127
weightBody.tsmillenials$mean[47:51] <- NA
weightBody.tsmillenials$weight.mean[47:51] <- NA
weightBody.tsmillenials$cohens[47:51] <- NA
weightBody.tsmillenials

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 127, 131:132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsmillenials <- rbind(weightBody.month.extra, weightBody.tsmillenials)
weightBody.tsmillenials <- weightBody.tsmillenials[order(weightBody.tsmillenials$month.cent),]
weightBody.tsmillenials

weightBody.Wtsmillenials <- ts(weightBody.tsmillenials$weight.mean, frequency = 12, start = c(2010, 4), end = c(2016,12))
weightBody.Wtsmillenials
plot(weightBody.Wtsmillenials)

# impute with seasonal decomposition and interpolation
weightBody.Wtsmillenials <- na.seadec(weightBody.Wtsmillenials, algorithm = "interpolation")
weightBody.Wtsmillenials
plot(weightBody.Wtsmillenials)

weightBody.arimamillenials <- auto.arima(weightBody.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
weightBody.arimamillenials # parameters (1,0,0)(1,0,2)

forecast.weightBodymillenials <- forecast(weightBody.arimamillenials, h = 60)
plot(forecast.weightBodymillenials)

forecast.weightBodymillenialsLONG <- forecast(weightBody.arimamillenials, h = 1800) # 150 years
plot(forecast.weightBodymillenialsLONG)
abline(h = 0)

min(which(forecast.weightBodymillenialsLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodymillenialsLONG$mean<=0))/12 # NA
min(which(forecast.weightBodymillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodymillenialsLONG$upper[,"95%"]>=2*weightBody.Wtsmillenials[1]))/12 # NA

## percent change
decomp.weightBodymillenials <- decompose(weightBody.Wtsmillenials)
(decomp.weightBodymillenials$trend[7]-decomp.weightBodymillenials$trend[75])/decomp.weightBodymillenials$trend[7] # +6.50%
plot(decomp.weightBodymillenials)
# initial value
weightBody.Wtsmillenials[1] # 0.46
# final value
weightBody.Wtsmillenials[81] # 0.47


## Fit to Gen Z
weightBody.tsgenZ <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="genZ",],
                                 .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightBody.tsgenZ$month.cent)

# impute missing values
# add in NAs for month values 125:131 (all missing anyways)
weightBody.month.extra <- as.data.frame(c(105:107, 125:132))
weightBody.month.extra$mean <- rep(NA, 11)
weightBody.month.extra$weight.mean <- rep(NA, 11)
weightBody.month.extra$cohens <- rep(NA, 11)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsgenZ <- rbind(weightBody.month.extra, weightBody.tsgenZ)
weightBody.tsgenZ <- weightBody.tsgenZ[order(weightBody.tsgenZ$month.cent),]
weightBody.tsgenZ

weightBody.WtsgenZ <- ts(weightBody.tsgenZ$weight.mean, frequency = 12, start = c(2010, 4), end = c(2016,12))
weightBody.WtsgenZ
plot(weightBody.WtsgenZ)

weightBody.WtsgenZ <- window(weightBody.WtsgenZ, start = c(2011, 1), end = c(2016,12))
weightBody.WtsgenZ
plot(weightBody.WtsgenZ)

# impute with seasonal decomposition and interpolation
weightBody.WtsgenZ <- na.seadec(weightBody.WtsgenZ, algorithm = "interpolation")
weightBody.WtsgenZ
plot(weightBody.WtsgenZ)

weightBody.arimagenZ <- auto.arima(weightBody.WtsgenZ, stepwise = FALSE, approximation = FALSE)
weightBody.arimagenZ # parameters (4,1,0)

forecast.weightBodygenZ <- forecast(weightBody.arimagenZ, h = 60)
plot(forecast.weightBodygenZ)

forecast.weightBodygenZLONG <- forecast(weightBody.arimagenZ, h = 1800) # 150 years
plot(forecast.weightBodygenZLONG)
abline(h = 0)
abline(h = 2*weightBody.WtsgenZ[1])

min(which(forecast.weightBodygenZLONG$lower[,"95%"]<=0))/12 # 39.667
min(which(forecast.weightBodygenZLONG$mean<=0))/12 # NA
min(which(forecast.weightBodygenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodygenZLONG$upper[,"95%"]>=2*weightBody.WtsgenZ[1]))/12 # 6.667

## percent change
decomp.weightBodygenZ <- decompose(weightBody.WtsgenZ)
(decomp.weightBodygenZ$trend[7]-decomp.weightBodygenZ$trend[66])/decomp.weightBodygenZ$trend[7] # +23.15%
plot(decomp.weightBodygenZ)
# initial value
weightBody.WtsgenZ[1] # 0.35
# final value
weightBody.WtsgenZ[72] # 0.48



## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecast.weightBodyboom, main = "Boomers", ylim = c(0.2, 0.65))
plot(forecast.weightBodygenX, main = "Gen X", ylim = c(0.2, 0.65))
plot(forecast.weightBodymillenials, main = "Millenials", ylim = c(0.2, 0.65))
plot(forecast.weightBodygenZ, main = "Gen Z", ylim = c(0.2, 0.65), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecast.weightBodyboom, ylim = c(0.2, 0.65), main = "Implicit WeightBody Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodymillenials, ylim = c(0.2, 0.65), main = "Implicit WeightBody Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodygenX, ylim = c(0.2, 0.65), main = "Implicit WeightBody Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


## BODY WEIGHT (FACE) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightFace$yobcats, timeseriesWeightFace$month.cent) 
# only boomers, genX and millenials make sense (no genZ)

## Fit to Boomers
weightFace.tsboom <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="boomers",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsboom$month.cent)

weightFace.Wtsboom <- ts(weightFace.tsboom$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011, 10))
weightFace.Wtsboom
plot(weightFace.Wtsboom)

weightFace.arimaboom <- auto.arima(weightFace.Wtsboom, stepwise = FALSE, approximation = FALSE)
weightFace.arimaboom # parameters (0,1,1) with drift

forecast.weightFaceboom <- forecast(weightFace.arimaboom, h = 60)
plot(forecast.weightFaceboom)

forecast.weightFaceboomLONG <- forecast(weightFace.arimaboom, h = 1800) # 150 years
plot(forecast.weightFaceboomLONG)
abline(h = 0)

min(which(forecast.weightFaceboomLONG$lower[,"95%"]>=2*weightFace.Wtsboom[1]))/12 # 10.5833
min(which(forecast.weightFaceboomLONG$mean>=2*weightFace.Wtsboom[1]))/12 # 3.1667
min(which(forecast.weightFaceboomLONG$upper[,"95%"]>=2*weightFace.Wtsboom[1]))/12 # 0.41667
## note these numbers are from Oct, 2011.

## percent change
decomp.weightFaceboom <- decompose(weightFace.Wtsboom)
(decomp.weightFaceboom$trend[7]-decomp.weightFaceboom$trend[75])/decomp.weightFaceboom$trend[7] # +60.91%
plot(decomp.weightFaceboom)
# initial value
weightFace.Wtsboom[1] # 0.27
# final value
weightFace.Wtsboom[81] # 0.41


## Fit to Gen X
weightFace.tsgenX <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="genX",],
                           .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsgenX$month.cent)

weightFace.WtsgenX <- ts(weightFace.tsgenX$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011,10))
weightFace.WtsgenX
plot(weightFace.WtsgenX)

weightFace.arimagenX <- auto.arima(weightFace.WtsgenX, stepwise = FALSE, approximation = FALSE)
weightFace.arimagenX # parameters (0,1,4) with drift

forecast.weightFacegenX <- forecast(weightFace.arimagenX, h = 60)
plot(forecast.weightFacegenX)

forecast.weightFacegenXLONG <- forecast(weightFace.arimagenX, h = 1800) # 150 years
plot(forecast.weightFacegenXLONG)
abline(h = 0)

min(which(forecast.weightFacegenXLONG$lower[,"95%"]>=2*weightFace.WtsgenX[1]))/12 # 11.1667
min(which(forecast.weightFacegenXLONG$mean>=2*weightFace.WtsgenX[1]))/12 # 6.25
min(which(forecast.weightFacegenXLONG$upper[,"95%"]>=2*weightFace.WtsgenX[1]))/12 # 2.1667

## percent change
decomp.weightFacegenX <- decompose(weightFace.WtsgenX)
(decomp.weightFacegenX$trend[7]-decomp.weightFacegenX$trend[75])/decomp.weightFacegenX$trend[7] # +47.40%
plot(decomp.weightFacegenX)
# initial value
weightFace.WtsgenX[1] # 0.31
# final value
weightFace.WtsgenX[81] # 0.45


## Fit to Millenials
weightFace.tsmillenials <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="millenials",],
                                 .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), cohens = cohensD(dscore))
table(weightFace.tsmillenials$month.cent)

weightFace.Wtsmillenials <- ts(weightFace.tsmillenials$weight.mean, frequency = 12, start = c(2004, 5), end = c(2011,10))
weightFace.Wtsmillenials
plot(weightFace.Wtsmillenials)

weightFace.arimamillenials <- auto.arima(weightFace.Wtsmillenials, stepwise = FALSE, approximation = FALSE)
weightFace.arimamillenials # parameters (0,1,1) with drift

forecast.weightFacemillenials <- forecast(weightFace.arimamillenials, h = 60)
plot(forecast.weightFacemillenials)

forecast.weightFacemillenialsLONG <- forecast(weightFace.arimamillenials, h = 1800) # 150 years
plot(forecast.weightFacemillenialsLONG)
abline(h = 0)

min(which(forecast.weightFacemillenialsLONG$lower[,"95%"]>=2*weightFace.Wtsmillenials[1]))/12 # 27.5
min(which(forecast.weightFacemillenialsLONG$mean>=2*weightFace.Wtsmillenials[1]))/12 # 11.0833
min(which(forecast.weightFacemillenialsLONG$upper[,"95%"]>=2*weightFace.Wtsmillenials[1]))/12 # 4.333

## percent change
decomp.weightFacemillenials <- decompose(weightFace.Wtsmillenials)
(decomp.weightFacemillenials$trend[7]-decomp.weightFacemillenials$trend[75])/decomp.weightFacemillenials$trend[7] # +38.13%
plot(decomp.weightFacemillenials)
# initial value
weightFace.Wtsmillenials[1] # 0.31
# final value
weightFace.Wtsmillenials[81] # 0.42

## Summary with plots
op <- par(mfrow = c(1,3))
plot(forecast.weightFaceboom, main = "Boomers", ylim = c(0.2, 0.72))
plot(forecast.weightFacegenX, main = "Gen X", ylim = c(0.2, 0.72))
plot(forecast.weightFacemillenials, main = "Millenials", ylim = c(0.2, 0.72))
par(op)



## Overlapping plots
plot(forecast.weightFaceboom, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightFacemillenials, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightFacegenX, ylim = c(0.2, 0.7), main = "Implicit Weight (Face) Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("topleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


### COHORT SUMMARIES ------------------------------------------
## red = millenials
## blue = gen X
## green = baby boomers.

op <- par(mfrow = c(2,3))
## Age
plot(forecast.ageboom, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agemillenials, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.agegenX, ylim = c(0.35, 0.52), main = "Implicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Disability
plot(forecast.disboom, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.dismillenials, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.disgenX, ylim = c(0.35, 0.76), main = "Implicit Disability Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Body Weight (Body)
plot(forecast.weightBodyboom, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes", xlim = c(2007, 2022),
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodymillenials, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes",xlim = c(2007, 2022),
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.weightBodygenX, ylim = c(0.35, 0.65), main = "Implicit WeightBody Attitudes",xlim = c(2007, 2022),
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Race
plot(forecast.raceboom, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racemillenials, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.racegenX, ylim = c(0, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Skin-Tone
plot(forecast.skinboom, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skinmillenials, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.skingenX, ylim = c(0.14, 0.45), main = "Implicit Skin Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

## Sexuality
plot(forecast.sexboom, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexmillenials, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecast.sexgenX, ylim = c(0, 0.52), main = "Implicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))

par(op)



##############################################################################################################################
##############################################################################################################################
## EXPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## AGE  --------------------------------------------------------------------------------------------------------
## Fit to Boomers
age.tsEboom <- ddply(timeseriesAge[timeseriesAge$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsEboom <- ts(age.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsEboom
plot(age.WtsEboom)

age.arimaEboom <- auto.arima(age.WtsEboom, stepwise = FALSE, approximation = FALSE)
age.arimaEboom # parameters (2,1,3)

forecastE.ageboom <- forecast(age.arimaEboom, h = 60)
plot(forecastE.ageboom)

forecastE.ageboomLONG <- forecast(age.arimaEboom, h = 1800) # 150 years
plot(forecastE.ageboomLONG)
abline(h = 0)

min(which(forecastE.ageboomLONG$lower[,"95%"]<=0))/12 # Already neutral
min(which(forecastE.ageboomLONG$mean<=0))/12 # NA
min(which(forecastE.ageboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.ageboomLONG$upper[,"95%"]>=2*age.WtsEboom[1]))/12 # 7.333

## percent change
decompE.ageboom <- decompose(age.WtsEboom)
(decompE.ageboom$trend[7]-decompE.ageboom$trend[114])/decompE.ageboom$trend[7] # -90.73%
# initial value
age.WtsEboom[1] # 0.10
# final value
age.WtsEboom[120] # 0.025


## Fit to Gen X
age.tsEgenX <- ddply(timeseriesAge[timeseriesAge$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsEgenX <- ts(age.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsEgenX
plot(age.WtsEgenX)

age.arimaEgenX <- auto.arima(age.WtsEgenX, stepwise = FALSE, approximation = FALSE)
age.arimaEgenX # parameters (0,1,1)(1,0,0) with drift

forecastE.agegenX <- forecast(age.arimaEgenX, h = 60)
plot(forecastE.agegenX)

forecastE.agegenXLONG <- forecast(age.arimaEgenX, h = 1800) # 150 years
plot(forecastE.agegenXLONG)
abline(h = 0)

min(which(forecastE.agegenXLONG$lower[,"95%"]<=0))/12 # Already neutral
min(which(forecastE.agegenXLONG$mean<=0))/12 # Already neutral
min(which(forecastE.agegenXLONG$upper[,"95%"]<=0))/12 # 6 yrs, 2 mos
min(which(forecastE.agegenXLONG$upper[,"95%"]>=2*age.WtsEgenX[1]))/12 # NA

## percent change
decompE.agegenX <- decompose(age.WtsEgenX)
(decompE.agegenX$trend[7]-decompE.agegenX$trend[114])/decompE.agegenX$trend[7] # -104.69%
# initial value
age.WtsEgenX[1] # 0.15
# final value
age.WtsEgenX[120] # 0.040

## Fit to Millenials
age.tsEmillenials <- ddply(timeseriesAge[timeseriesAge$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsEmillenials <- ts(age.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsEmillenials
plot(age.WtsEmillenials)

age.arimaEmillenials <- auto.arima(age.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
age.arimaEmillenials # parameters (0,1,2)(1,0,2)

forecastE.agemillenials <- forecast(age.arimaEmillenials, h = 60)
plot(forecastE.agemillenials)

forecastE.agemillenialsLONG <- forecast(age.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.agemillenialsLONG)
abline(h = 0)

min(which(forecastE.agemillenialsLONG$lower[,"95%"]<=0))/12 # 2 yrs 1 mos
min(which(forecastE.agemillenialsLONG$mean<=0))/12 # NA
min(which(forecastE.agemillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.agemillenialsLONG$upper[,"95%"]>=2*age.WtsEmillenials[1]))/12 # 26 yrs, 6 mos

## percent change
decompE.agemillenials <- decompose(age.WtsEmillenials)
(decompE.agemillenials$trend[7]-decompE.agemillenials$trend[114])/decompE.agemillenials$trend[7] # -57.66%
# initial value
age.WtsEmillenials[1] # 0.65
# final value
age.WtsEmillenials[120] # 0.30


## Fit to Gen Z
age.tsEgenZ <- ddply(timeseriesAge[timeseriesAge$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
age.WtsEgenZ <- ts(age.tsEgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
age.WtsEgenZ
plot(age.WtsEgenZ)
age.WtsEgenZ <- window(age.WtsEgenZ, start = c(2011, 1), end = c(2016, 12))
plot(age.WtsEgenZ)

age.arimaEgenZ <- auto.arima(age.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
age.arimaEgenZ # parameters (0,1,2)(1,0,0) with drift

forecastE.agegenZ <- forecast(age.arimaEgenZ, h = 60)
plot(forecastE.agegenZ)

forecastE.agegenZLONG <- forecast(age.arimaEgenZ, h = 1800) # 150 years
plot(forecastE.agegenZLONG)
abline(h = 0)

min(which(forecastE.agegenZLONG$lower[,"95%"]<=0))/12 # 5 yrs, 6 mos
min(which(forecastE.agegenZLONG$mean<=0))/12 # 11 yrs 4 mos
min(which(forecastE.agegenZLONG$upper[,"95%"]<=0))/12 # 22 yrs, 4 mos
min(which(forecastE.agegenZLONG$upper[,"95%"]>=2*age.WtsEgenZ[1]))/12 # NA

## percent change
decompE.agegenZ <- decompose(age.WtsEgenZ)
(decompE.agegenZ$trend[7]-decompE.agegenZ$trend[66])/decompE.agegenZ$trend[7] # -33.30%
# initial value
age.WtsEgenZ[1] # 1.04
# final value
age.WtsEgenZ[72] # 0.58

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecastE.ageboom, main = "Boomers", ylim = c(-0.3, 1.1))
plot(forecastE.agegenX, main = "Gen X", ylim = c(-0.3, 1.1))
plot(forecastE.agemillenials, main = "Millenials", ylim = c(-0.3, 1.1))
plot(forecastE.agegenZ, main = "Gen Z", ylim = c(-0.3, 1.1), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.ageboom, ylim = c(-0.3, 1.1), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.agemillenials, ylim = c(-0.3, 1.1), main = "Explicit Age Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.agegenX, ylim = c(-0.3, 1.1), main = "Explicit Age Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("topright", c("Gen X", "Millenials", "Baby Boomers"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## DISABILITY --------------------------------------------------------------------------------------------------------
table(timeseriesDis$yobcats, timeseriesDis$month.cent) # now all age categories for each year

## Fit to Boomers
dis.tsEboom <- ddply(timeseriesDis[timeseriesDis$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), 
                    cohens = cohensD(att.center1))
dis.WtsEboom <- ts(dis.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WtsEboom
plot(dis.WtsEboom)

dis.arimaEboom <- auto.arima(dis.WtsEboom, stepwise = FALSE, approximation = FALSE) 
dis.arimaEboom # parameters (0,1,1)

forecastE.disboom <- forecast(dis.arimaEboom, h = 60)
plot(forecastE.disboom)

forecastE.disboomLONG <- forecast(dis.arimaEboom, h = 1800) # 150 years
plot(forecastE.disboomLONG)
abline(h = 0)

min(which(forecastE.disboomLONG$lower[,"95%"]<=0))/12 # 96 yrs, 1 mos
min(which(forecastE.disboomLONG$mean<=0))/12 # NA
min(which(forecastE.disboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.disboomLONG$upper[,"95%"]>=2*dis.WtsEboom[1]))/12 # NA

## percent change
decompE.disboom <- decompose(dis.WtsEboom)
(decompE.disboom$trend[7]-decompE.disboom$trend[114])/decompE.disboom$trend[7] # -15.86%
# initial value
dis.WtsEboom[1] # 0.62
# final value
dis.WtsEboom[120] # 0.41


## Fit to Gen X
dis.tsEgenX <- ddply(timeseriesDis[timeseriesDis$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WtsEgenX <- ts(dis.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WtsEgenX
plot(dis.WtsEgenX)

dis.arimdisnXE <- auto.arima(dis.WtsEgenX, stepwise = FALSE, approximation = FALSE)
dis.arimdisnXE # parameters (0,1,1)

forecastE.disgenX <- forecast(dis.arimdisnXE, h = 60)
plot(forecastE.disgenX)

forecastE.disgenXLONG <- forecast(dis.arimdisnXE, h = 1800) # 150 years
plot(forecastE.disgenXLONG)
abline(h = 0)

min(which(forecastE.disgenXLONG$lower[,"95%"]<=0))/12 # 47 yrs, 7 mos
min(which(forecastE.disgenXLONG$mean<=0))/12 # NA
min(which(forecastE.disgenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.disgenXLONG$upper[,"95%"]>=2*dis.WtsEgenX[1]))/12 # 125 yrs, 11 mos

## percent change
decompE.disgenX <- decompose(dis.WtsEgenX)
(decompE.disgenX$trend[7]-decompE.disgenX$trend[114])/decompE.disgenX$trend[7] # -6.06%
# initial value
dis.WtsEgenX[1] # 0.48
# final value
dis.WtsEgenX[120] # 0.42

## Fit to Millenials
dis.tsEmillenials <- ddply(timeseriesDis[timeseriesDis$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.WtsEmillenials <- ts(dis.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
dis.WtsEmillenials
plot(dis.WtsEmillenials)

dis.arimaEmillenials <- auto.arima(dis.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
dis.arimaEmillenials # parameters (0,1,3)(1,0,0)

forecastE.dismillenials <- forecast(dis.arimaEmillenials, h = 60)
plot(forecastE.dismillenials)

forecastE.dismillenialsLONG <- forecast(dis.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.dismillenialsLONG)
abline(h = 0)

min(which(forecastE.dismillenialsLONG$lower[,"95%"]<=0))/12 # 25 yrs, 11 mos
min(which(forecastE.dismillenialsLONG$mean<=0))/12 # NA
min(which(forecastE.dismillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.dismillenialsLONG$upper[,"95%"]>=2*dis.WtsEmillenials[1]))/12 # NA

## percent change
decompE.dismillenials <- decompose(dis.WtsEmillenials)
(decompE.dismillenials$trend[7]-decompE.dismillenials$trend[114])/decompE.dismillenials$trend[7] # -31.77%
# initial value
dis.WtsEmillenials[1] # 0.67
# final value
dis.WtsEmillenials[120] # 0.42


## Fit to Gen Z
dis.tsEgenZ <- ddply(timeseriesDis[timeseriesDis$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(att.center1), 
                    weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
dis.tsEgenZ <- dis.tsEgenZ[dis.tsEgenZ$month.cent >= 85,]
dis.WtsEgenZ <- ts(dis.tsEgenZ$weight.mean, frequency = 12, start = c(2011, 1))
dis.WtsEgenZ
plot(dis.WtsEgenZ)

dis.arimdisnZE <- auto.arima(dis.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
dis.arimdisnZE # parameters (2,1,3)

forecastE.disgenZ <- forecast(dis.arimdisnZE, h = 60)
plot(forecastE.disgenZ)

forecastE.disgenZLONG <- forecast(dis.arimdisnZE, h = 1800) # 150 years
plot(forecastE.disgenZLONG)
abline(h = 0)

min(which(forecastE.disgenZLONG$lower[,"95%"]<=0))/12 # 1 yr, 3 mos
min(which(forecastE.disgenZLONG$mean<=0))/12 # NA
min(which(forecastE.disgenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.disgenZLONG$upper[,"95%"]>=2*dis.WtsEgenZ[1]))/12 # 28 yrs, 4 mos.

## percent change
decompE.disgenZ <- decompose(dis.WtsEgenZ)
(decompE.disgenZ$trend[7]-decompE.disgenZ$trend[66])/decompE.disgenZ$trend[7] # -20.74%
# initial value
dis.WtsEgenZ[1] # 1.11
# final value
dis.WtsEgenZ[72] # 0.47

## Summary with plots 
op <- par(mfrow = c(2,2))
plot(forecastE.disboom, main = "Boomers", ylim = c(-0.3, 1.1))
plot(forecastE.disgenX, main = "Gen X", ylim = c(-0.3, 1.1))
plot(forecastE.dismillenials, main = "Millenials", ylim = c(-0.3, 1.1))
plot(forecastE.disgenZ, main = "Gen Z", ylim = c(-0.3, 1.1), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.disboom, ylim = c(-0.3, 1.1), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.dismillenials, ylim = c(-0.3, 1.1), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.disgenX, ylim = c(-0.3, 1.1), main = "Explicit Disability Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), 
       col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## RACE --------------------------------------------------------------------------------------------------------
table(timeseriesRace$yobcats, timeseriesRace$month.cent) 
# now all age categories for each year (low frequencies late 2015)

## Fit to Boomers
race.tsEboom <- ddply(timeseriesRace[timeseriesRace$yobcats=="boomers",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEboom <- ts(race.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsEboom
plot(race.WtsEboom)

race.arimaEboom <- auto.arima(race.WtsEboom, stepwise = FALSE, approximation = FALSE)
race.arimaEboom # parameters (0,1,1)

forecastE.raceboom <- forecast(race.arimaEboom, h = 60)
plot(forecastE.raceboom)

forecastE.raceboomLONG <- forecast(race.arimaEboom, h = 1800) # 150 years
plot(forecastE.raceboomLONG)
abline(h = 0)

min(which(forecastE.raceboomLONG$lower[,"95%"]<=0))/12 # 49.25
min(which(forecastE.raceboomLONG$mean<=0))/12 # NA
min(which(forecastE.raceboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.raceboomLONG$upper[,"95%"]>=2*race.WtsEboom[1]))/12 # NA

## percent change
decompE.raceboom <- decompose(race.WtsEboom)
(decompE.raceboom$trend[7]-decompE.raceboom$trend[114])/decompE.raceboom$trend[7] # -8.56%
# initial value
race.WtsEboom[1] # 0.33
# final value
race.WtsEboom[120] # 0.25


## Fit to Gen X
race.tsEgenX <- ddply(timeseriesRace[timeseriesRace$yobcats=="genX",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEgenX <- ts(race.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsEgenX
plot(race.WtsEgenX)

race.arimracenX <- auto.arima(race.WtsEgenX, stepwise = FALSE, approximation = FALSE)
race.arimracenX # parameters (0,0,2)

forecastE.racegenX <- forecast(race.arimracenX, h = 60)
plot(forecastE.racegenX)
abline(h = 0)
abline(h = 2*race.WtsEgenX[1])

forecastE.racegenXLONG <- forecast(race.arimracenX, h = 1800) # 150 years
plot(forecastE.racegenXLONG)
abline(h = 0)

min(which(forecastE.racegenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecastE.racegenXLONG$mean<=0))/12 # NA
min(which(forecastE.racegenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.racegenXLONG$upper[,"95%"]>=2*race.WtsEgenX[1]))/12 # 0.25, 3 mos.

## percent change
decompE.racegenX <- decompose(race.WtsEgenX)
(decompE.racegenX$trend[7]-decompE.racegenX$trend[114])/decompE.racegenX$trend[7] # -4.23%
# initial value
race.WtsEgenX[1] # 0.16
# final value
race.WtsEgenX[120] # 0.11

## Fit to Millenials
race.tsEmillenials <- ddply(timeseriesRace[timeseriesRace$yobcats=="millenials",],
                           .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEmillenials <- ts(race.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsEmillenials
plot(race.WtsEmillenials)

race.arimaEmillenials <- auto.arima(race.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
race.arimaEmillenials # parameters (0,1,1)(2,0,2) with drift

forecastE.racemillenials <- forecast(race.arimaEmillenials, h = 60)
plot(forecastE.racemillenials)

forecastE.racemillenialsLONG <- forecast(race.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.racemillenialsLONG)
abline(h = 0)

min(which(forecastE.racemillenialsLONG$lower[,"95%"]<=0))/12 # 1.8333
min(which(forecastE.racemillenialsLONG$mean<=0))/12 # 4.91667
min(which(forecastE.racemillenialsLONG$upper[,"95%"]<=0))/12 # 20.333
min(which(forecastE.racemillenialsLONG$upper[,"95%"]>=2*race.WtsEmillenials[1]))/12 # NA

## percent change
decompE.racemillenials <- decompose(race.WtsEmillenials)
(decompE.racemillenials$trend[7]-decompE.racemillenials$trend[114])/decompE.racemillenials$trend[7] # -42.71%
# initial value
race.WtsEmillenials[1] # 0.32
# final value
race.WtsEmillenials[120] # 0.14


## Fit to Gen Z
race.tsEgenZ <- ddply(timeseriesRace[timeseriesRace$yobcats=="genZ",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
race.WtsEgenZ <- ts(race.tsEgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
race.WtsEgenZ
plot(race.WtsEgenZ)
race.WtsEgenZ <- window(race.WtsEgenZ, start = c(2011, 1), end = c(2016, 12))
plot(race.WtsEgenZ)

race.arimracenZE <- auto.arima(race.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
race.arimracenZE # parameters (0,1,1)(0,0,2)

forecastE.racegenZ <- forecast(race.arimracenZE, h = 60)
plot(forecastE.racegenZ)

forecastE.racegenZLONG <- forecast(race.arimracenZE, h = 1800) # 150 years
plot(forecastE.racegenZLONG)
abline(h = 0)

min(which(forecastE.racegenZLONG$lower[,"95%"]<=0))/12 # 2 mos
min(which(forecastE.racegenZLONG$mean<=0))/12 # NA
min(which(forecastE.racegenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.racegenZLONG$upper[,"95%"]>=2*race.WtsEgenZ[1]))/12 # 7 yrs

## percent change
decompE.racegenZ <- decompose(race.WtsEgenZ)
(decompE.racegenZ$trend[7]-decompE.racegenZ$trend[66])/decompE.racegenZ$trend[7] # -44.95%
# initial value
race.WtsEgenZ[1] # 0.35
# final value
race.WtsEgenZ[72] # 0.19

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecastE.raceboom, main = "Boomers", ylim = c(-0.3, 0.5))
plot(forecastE.racegenX, main = "Gen X", ylim = c(-0.3, 0.5))
plot(forecastE.racemillenials, main = "Millenials", ylim = c(-0.3, 0.5))
plot(forecastE.racegenZ, main = "Gen Z", ylim = c(-0.3, 0.5), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.raceboom, ylim = c(-0.3, 0.5), main = "Explicit Race Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.racemillenials, ylim = c(-0.3, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.racegenX, ylim = c(-0.3, 0.5), main = "Implicit Race Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## SEXUALITY  --------------------------------------------------------------------------------------------------------
table(timeseriesSex$yobcats, timeseriesSex$month.cent) # now all age categories for each year

## Fit to Boomers
sex.tsEboom <- ddply(timeseriesSex[timeseriesSex$yobcats=="boomers",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsEboom <- ts(sex.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsEboom
plot(sex.WtsEboom) # Note: December 2012 is an outlier

sex.arimaEboom <- auto.arima(sex.WtsEboom, stepwise = FALSE, approximation = FALSE)
sex.arimaEboom # parameters (0,1,2)

forecastE.sexboom <- forecast(sex.arimaEboom, h = 60)
plot(forecastE.sexboom)

forecastE.sexboomLONG <- forecast(sex.arimaEboom, h = 1800) # 150 years
plot(forecastE.sexboomLONG)
abline(h = 0)

min(which(forecastE.sexboomLONG$lower[,"95%"]<=0))/12 # 38.75
min(which(forecastE.sexboomLONG$mean<=0))/12 # NA
min(which(forecastE.sexboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.sexboomLONG$upper[,"95%"]>=2*sex.WtsEboom[1]))/12 # NA

## percent change
decompE.sexboom <- decompose(sex.WtsEboom)
(decompE.sexboom$trend[7]-decompE.sexboom$trend[114])/decompE.sexboom$trend[7] # -23.38%
# initial value
sex.WtsEboom[1] # 0.63
# final value
sex.WtsEboom[120] # 0.28


## Fit to Gen X
sex.tsEgenX <- ddply(timeseriesSex[timeseriesSex$yobcats=="genX",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsEgenX <- ts(sex.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsEgenX
plot(sex.WtsEgenX) # again, outlier in December 2012 (not due to frequencies or known coding changes)

sex.arimsexnXE <- auto.arima(sex.WtsEgenX, stepwise = FALSE, approximation = FALSE)
sex.arimsexnXE # parameters (0,1,1)

forecastE.sexgenX <- forecast(sex.arimsexnXE, h = 60)
plot(forecastE.sexgenX)

forecastE.sexgenXLONG <- forecast(sex.arimsexnXE, h = 1800) # 150 years
plot(forecastE.sexgenXLONG)
abline(h = 0)

min(which(forecastE.sexgenXLONG$lower[,"95%"]<=0))/12 # 19.0833
min(which(forecastE.sexgenXLONG$mean<=0))/12 # NA
min(which(forecastE.sexgenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.sexgenXLONG$upper[,"95%"]>=2*sex.WtsEgenX[1]))/12 # 61.25

## percent change
decompE.sexgenX <- decompose(sex.WtsEgenX)
(decompE.sexgenX$trend[7]-decompE.sexgenX$trend[114])/decompE.sexgenX$trend[7] # -28.94%
# initial value
sex.WtsEgenX[1] # 0.53
# final value
sex.WtsEgenX[120] # 0.30

## Fit to Millenials
sex.tsEmillenials <- ddply(timeseriesSex[timeseriesSex$yobcats=="millenials",],
                          .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsEmillenials <- ts(sex.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsEmillenials
plot(sex.WtsEmillenials)

sex.arimaEmillenials <- auto.arima(sex.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
sex.arimaEmillenials # parameters (0,1,1)(2,0,0)

forecastE.sexmillenials <- forecast(sex.arimaEmillenials, h = 60)
plot(forecastE.sexmillenials)

forecastE.sexmillenialsLONG <- forecast(sex.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.sexmillenialsLONG)
abline(h = 0)

min(which(forecastE.sexmillenialsLONG$lower[,"95%"]<=0))/12 # 3.5833
min(which(forecastE.sexmillenialsLONG$mean<=0))/12 # NA
min(which(forecastE.sexmillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.sexmillenialsLONG$upper[,"95%"]>=2*sex.WtsEmillenials[1]))/12 # 42.333

## percent change
decompE.sexmillenials <- decompose(sex.WtsEmillenials)
(decompE.sexmillenials$trend[7]-decompE.sexmillenials$trend[114])/decompE.sexmillenials$trend[7] # -53.11%
# initial value
sex.WtsEmillenials[1] # 0.70
# final value
sex.WtsEmillenials[120] # 0.27


## Fit to Gen Z
sex.tsEgenZ <- ddply(timeseriesSex[timeseriesSex$yobcats=="genZ",],
                    .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
sex.WtsEgenZ <- ts(sex.tsEgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
sex.WtsEgenZ
plot(sex.WtsEgenZ)
sex.WtsEgenZ <- window(sex.WtsEgenZ, start = c(2011, 1), end = c(2016, 11)) ## last month is a strange anomaly... inspect frequencies **
plot(sex.WtsEgenZ)

sex.arimsexnZE <- auto.arima(sex.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
sex.arimsexnZE # parameters (2,1,2)(1,0,0)

forecastE.sexgenZ <- forecast(sex.arimsexnZE, h = 60)
plot(forecastE.sexgenZ)

forecastE.sexgenZLONG <- forecast(sex.arimsexnZE, h = 1800) # 150 years
plot(forecastE.sexgenZLONG)
abline(h = 0)

min(which(forecastE.sexgenZLONG$lower[,"95%"]<=0))/12 # 0.5
min(which(forecastE.sexgenZLONG$mean<=0))/12 # NA
min(which(forecastE.sexgenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.sexgenZLONG$upper[,"95%"]>=2*sex.WtsEgenZ[1]))/12 # 9.833

## percent change
decompE.sexgenZ <- decompose(sex.WtsEgenZ)
(decompE.sexgenZ$trend[7]-decompE.sexgenZ$trend[65])/decompE.sexgenZ$trend[7] # -37.31%
# initial value
sex.WtsEgenZ[1] # 0.44
# final value
sex.WtsEgenZ[71] # 0.42

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecastE.sexboom, main = "Boomers", ylim = c(0, 0.8))
plot(forecastE.sexgenX, main = "Gen X", ylim = c(0, 0.8))
plot(forecastE.sexmillenials, main = "Millenials", ylim = c(0, 0.8))
plot(forecastE.sexgenZ, main = "Gen Z", ylim = c(0, 0.8), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.sexboom, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.sexmillenials, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.sexgenX, ylim = c(0, 0.8), main = "Explicit Sexuality Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 



## SKIN TONE  --------------------------------------------------------------------------------------------------------
table(timeseriesSkin$yobcats, timeseriesSkin$month.cent) # now all age categories for each year

## Fit to Boomers
skin.tsEboom <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="boomers",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

# impute missing values
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$cohens <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")

skin.tsEboom <- rbind(skin.month.extra, skin.tsEboom)
skin.tsEboom <- skin.tsEboom[order(skin.tsEboom$month.cent),]
skin.tsEboom

skin.WtsEboom <- ts(skin.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsEboom
plot(skin.WtsEboom) # October 2012 is an outlier (not for frequencies or known coding)

# impute with seasonal decomposition and interpolation
skin.WtsEboom <- na.seadec(skin.WtsEboom, algorithm = "interpolation")
skin.WtsEboom
plot(skin.WtsEboom)

skin.arimaEboom <- auto.arima(skin.WtsEboom, stepwise = FALSE, approximation = FALSE)
skin.arimaEboom # parameters (0,0,2)(1,0,0)

forecastE.skinboom <- forecast(skin.arimaEboom, h = 60)
plot(forecastE.skinboom)

forecastE.skinboomLONG <- forecast(skin.arimaEboom, h = 1800) # 150 years
plot(forecastE.skinboomLONG)
abline(h = 0)

min(which(forecastE.skinboomLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecastE.skinboomLONG$mean<=0))/12 # NA
min(which(forecastE.skinboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.skinboomLONG$upper[,"95%"]>=2*skin.WtsEboom[1]))/12 # NA

## percent change
decompE.skinboom <- decompose(skin.WtsEboom)
(decompE.skinboom$trend[7]-decompE.skinboom$trend[114])/decompE.skinboom$trend[7] # -14.82%
# initial value
skin.WtsEboom[1] # 0.31
# final value
skin.WtsEboom[120] # 0.20


## Fit to Gen X
skin.tsEgenX <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="genX",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.tsEgenX <- rbind(skin.month.extra, skin.tsEgenX)
skin.tsEgenX <- skin.tsEgenX[order(skin.tsEgenX$month.cent),]
skin.tsEgenX

skin.WtsEgenX <- ts(skin.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsEgenX
plot(skin.WtsEgenX)

# impute with seasonal decomposition and interpolation
skin.WtsEgenX <- na.seadec(skin.WtsEgenX, algorithm = "interpolation")
skin.WtsEgenX
plot(skin.WtsEgenX)

skin.arimaEgenX <- auto.arima(skin.WtsEgenX, stepwise = FALSE, approximation = FALSE)
skin.arimaEgenX # parameters (0,0,0)

forecastE.skingenX <- forecast(skin.arimaEgenX, h = 60)
plot(forecastE.skingenX)

forecastE.skingenXLONG <- forecast(skin.arimaEgenX, h = 1800) # 150 years
plot(forecastE.skingenXLONG)
abline(h = 0)

min(which(forecastE.skingenXLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecastE.skingenXLONG$mean<=0))/12 # NA
min(which(forecastE.skingenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.skingenXLONG$upper[,"95%"]>=2*skin.WtsEgenX[1]))/12 # NA

## percent change
decompE.skingenX <- decompose(skin.WtsEgenX)
(decompE.skingenX$trend[7]-decompE.skingenX$trend[114])/decompE.skingenX$trend[7] # +5.04%
# initial value
skin.WtsEgenX[1] # 0.24
# final value
skin.WtsEgenX[120] # 0.11


## Fit to Millenials
skin.tsEmillenials <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="millenials",],
                           .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.tsEmillenials <- rbind(skin.month.extra, skin.tsEmillenials)
skin.tsEmillenials <- skin.tsEmillenials[order(skin.tsEmillenials$month.cent),]
skin.tsEmillenials

skin.WtsEmillenials <- ts(skin.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsEmillenials
plot(skin.WtsEmillenials)

# impute with seasonal decomposition and interpolation
skin.WtsEmillenials <- na.seadec(skin.WtsEmillenials, algorithm = "interpolation")
skin.WtsEmillenials
plot(skin.WtsEmillenials)

skin.arimaEmillenials <- auto.arima(skin.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
skin.arimaEmillenials # parameters (0,1,1)(2,0,0)

forecastE.skinmillenials <- forecast(skin.arimaEmillenials, h = 60)
plot(forecastE.skinmillenials)

forecastE.skinmillenialsLONG <- forecast(skin.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.skinmillenialsLONG)
abline(h = 0)

min(which(forecastE.skinmillenialsLONG$lower[,"95%"]<=0))/12 # 1.5833
min(which(forecastE.skinmillenialsLONG$mean<=0))/12 # NA
min(which(forecastE.skinmillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.skinmillenialsLONG$upper[,"95%"]>=2*skin.WtsEmillenials[1]))/12 # 23.91667

## percent change
decompE.skinmillenials <- decompose(skin.WtsEmillenials)
(decompE.skinmillenials$trend[7]-decompE.skinmillenials$trend[114])/decompE.skinmillenials$trend[7] # -31.45%
# initial value
skin.WtsEmillenials[1] # 0.31
# final value
skin.WtsEmillenials[120] # 0.13



## Fit to Gen Z
skin.tsEgenZ <- ddply(timeseriesSkin[timeseriesSkin$yobcats=="genZ",],
                     .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))

skin.tsEgenZ <- rbind(skin.month.extra, skin.tsEgenZ)
skin.tsEgenZ <- skin.tsEgenZ[order(skin.tsEgenZ$month.cent),]
skin.tsEgenZ

skin.WtsEgenZ <- ts(skin.tsEgenZ$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
skin.WtsEgenZ
plot(skin.WtsEgenZ)

skin.WtsEgenZ <- window(skin.WtsEgenZ, start = c(2011, 1), end = c(2016, 12))
plot(skin.WtsEgenZ)

# impute with seasonal decomposition and interpolation
skin.WtsEgenZ <- na.seadec(skin.WtsEgenZ, algorithm = "interpolation")
skin.WtsEgenZ
plot(skin.WtsEgenZ)

skin.arimskingenZE <- auto.arima(skin.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
skin.arimskingenZE # parameters (0,1,2)

forecastE.skingenZ <- forecast(skin.arimskingenZE, h = 60)
plot(forecastE.skingenZ)

forecastE.skingenZLONG <- forecast(skin.arimskingenZE, h = 1800) # 150 years
plot(forecastE.skingenZLONG)
abline(h = 0)

min(which(forecastE.skingenZLONG$lower[,"95%"]<=0))/12 # 0.41667
min(which(forecastE.skingenZLONG$mean<=0))/12 # NA
min(which(forecastE.skingenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.skingenZLONG$upper[,"95%"]>=2*skin.WtsEgenZ[1]))/12 # 19.833

## percent change
decompE.skingenZ <- decompose(skin.WtsEgenZ)
(decompE.skingenZ$trend[7]-decompE.skingenZ$trend[66])/decompE.skingenZ$trend[7] # -36.57%
# initial value
skin.WtsEgenZ[1] # 0.43
# final value
skin.WtsEgenZ[72] # 0.16

## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecastE.skinboom, main = "Boomers", ylim = c(0, 0.5))
plot(forecastE.skingenX, main = "Gen X", ylim = c(0, 0.5))
plot(forecastE.skinmillenials, main = "Millenials", ylim = c(0, 0.5))
plot(forecastE.skingenZ, main = "Gen Z", ylim = c(0, 0.5), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.skinboom, ylim = c(-0.2, 0.5), main = "Explicit Skin Tone Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.skinmillenials, ylim = c(-0.2, 0.5), main = "Explicit Skin Tone Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.skingenX, ylim = c(-0.2, 0.5), main = "Explicit Skin Tone Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 





## BODY WEIGHT (BOTH) --------------------------------------------------------------------------------------------------------
table(timeseriesWeightBody$yobcats, timeseriesWeightBody$month.cent) # now all age categories for each year

## Fit to Boomers
weightBody.tsEboom <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="boomers",],
                           .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightBody.tsEboom$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsEboom$month.cent[47:51] # gives us months 125:130, missing 128
weightBody.tsEboom$mean[47:51] <- NA
weightBody.tsEboom$weight.mean[47:51] <- NA
weightBody.tsEboom$cohens[47:51] <- NA
weightBody.tsEboom

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 128, 131:132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsEboom <- rbind(weightBody.month.extra, weightBody.tsEboom)
weightBody.tsEboom <- weightBody.tsEboom[order(weightBody.tsEboom$month.cent),]
weightBody.tsEboom


weightFace.tsEboom <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="boomers",],
                            .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.tsEboom$month.cent)
weightFace.tsEboom <- weightFace.tsEboom[weightFace.tsEboom$month.cent >= 37 & weightFace.tsEboom$month.cent <= 75,]

weightComb.tsEboom <- rbind(weightFace.tsEboom, weightBody.tsEboom)
weightComb.tsEboom <- weightComb.tsEboom[is.na(weightComb.tsEboom$month.cent)==FALSE,]
weightComb.tsEboom


weightComb.WtsEboom <- ts(weightComb.tsEboom$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
weightComb.WtsEboom
plot(weightBody.WtsEboom)

# impute with seasonal decomposition and interpolation
weightComb.WtsEboom <- na.seadec(weightComb.WtsEboom, algorithm = "interpolation")
weightComb.WtsEboom
plot(weightComb.WtsEboom)

weightComb.arimaEboom <- auto.arima(weightComb.WtsEboom, stepwise = FALSE, approximation = FALSE)
weightComb.arimaEboom # parameters (0,1,1)

forecastE.weightCombboom <- forecast(weightComb.arimaEboom, h = 60)
plot(forecastE.weightCombboom)

forecastE.weightCombboomLONG <- forecast(weightComb.arimaEboom, h = 1800) # 150 years
plot(forecastE.weightCombboomLONG)
abline(h = 0)

min(which(forecastE.weightCombboomLONG$lower[,"95%"]<=0))/12 # 10.1667
min(which(forecastE.weightCombboomLONG$mean<=0))/12 # NA
min(which(forecastE.weightCombboomLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.weightCombboomLONG$upper[,"95%"]>=2*weightComb.WtsEboom[1]))/12 # 18

## percent change
decompE.weightCombboom <- decompose(weightComb.WtsEboom)
(decompE.weightCombboom$trend[7]-decompE.weightCombboom$trend[114])/decompE.weightCombboom$trend[7] # -18.66%
plot(decompE.weightCombboom)
# initial value
weightComb.WtsEboom[1] # 0.82
# final value
weightComb.WtsEboom[120] # 0.72



## Fit to Gen X
weightBody.tsEgenX <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="genX",],
                           .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightBody.tsEgenX$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsEgenX$month.cent[47:51] # gives us months 125:130, missing 127:128
weightBody.tsEgenX$mean[47:51] <- NA
weightBody.tsEgenX$weight.mean[47:51] <- NA
weightBody.tsEgenX$cohens[47:51] <- NA
weightBody.tsEgenX

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 127:128, 132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsEgenX <- rbind(weightBody.month.extra, weightBody.tsEgenX)
weightBody.tsEgenX <- weightBody.tsEgenX[order(weightBody.tsEgenX$month.cent),]
weightBody.tsEgenX

weightFace.tsEgenX <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="genX",],
                            .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.tsEgenX$month.cent)
weightFace.tsEgenX <- weightFace.tsEgenX[weightFace.tsEgenX$month.cent >= 37 & weightFace.tsEgenX$month.cent <= 75,]

weightComb.tsEgenX <- rbind(weightFace.tsEgenX, weightBody.tsEgenX)
weightComb.tsEgenX <- weightComb.tsEgenX[is.na(weightComb.tsEgenX$month.cent)==FALSE,]
weightComb.tsEgenX

weightComb.WtsEgenX <- ts(weightComb.tsEgenX$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
weightComb.WtsEgenX
plot(weightComb.WtsEgenX)

# impute with seasonal decomposition and interpolation
weightComb.WtsEgenX <- na.seadec(weightComb.WtsEgenX, algorithm = "interpolation")
weightComb.WtsEgenX
plot(weightComb.WtsEgenX)

weightComb.arimaEgenX <- auto.arima(weightComb.WtsEgenX, stepwise = FALSE, approximation = FALSE)
weightComb.arimaEgenX # parameters (0,1,1)

forecastE.weightCombgenX <- forecast(weightComb.arimaEgenX, h = 60)
plot(forecastE.weightCombgenX)

forecastE.weightCombgenXLONG <- forecast(weightComb.arimaEgenX, h = 1800) # 150 years
plot(forecastE.weightCombgenXLONG)
abline(h = 0)

min(which(forecastE.weightCombgenXLONG$lower[,"95%"]<=0))/12 # 30
min(which(forecastE.weightCombgenXLONG$mean<=0))/12 # NA
min(which(forecastE.weightCombgenXLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.weightCombgenXLONG$upper[,"95%"]>=2*weightComb.WtsEgenX[1]))/12 # 48.75

## percent change
decompE.weightCombgenX <- decompose(weightComb.WtsEgenX)
(decompE.weightCombgenX$trend[7]-decompE.weightCombgenX$trend[114])/decompE.weightCombgenX$trend[7] # -19.11%
plot(decompE.weightCombgenX)
# initial value
weightComb.WtsEgenX[1] # 0.75
# final value
weightComb.WtsEgenX[120] # 0.61



## Fit to Millenials
weightBody.tsEmillenials <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="millenials",],
                                 .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightBody.tsEmillenials$month.cent)

# impute missing values
# add in NAs for month values 125:131
weightBody.tsEmillenials$month.cent[47:51] # gives us months 125:130, missing 127
weightBody.tsEmillenials$mean[47:51] <- NA
weightBody.tsEmillenials$weight.mean[47:51] <- NA
weightBody.tsEmillenials$cohens[47:51] <- NA
weightBody.tsEmillenials

# add add in NAs for missing months
weightBody.month.extra <- as.data.frame(c(105:107, 127, 131:132))
weightBody.month.extra$mean <- rep(NA, 6)
weightBody.month.extra$weight.mean <- rep(NA, 6)
weightBody.month.extra$cohens <- rep(NA, 6)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsEmillenials <- rbind(weightBody.month.extra, weightBody.tsEmillenials)
weightBody.tsEmillenials <- weightBody.tsEmillenials[order(weightBody.tsEmillenials$month.cent),]
weightBody.tsEmillenials

weightFace.tsEmillenials <- ddply(timeseriesWeightFace[timeseriesWeightFace$yobcats=="millenials",],
                            .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightFace.tsEmillenials$month.cent)
weightFace.tsEmillenials <- weightFace.tsEmillenials[weightFace.tsEmillenials$month.cent >= 37 & weightFace.tsEmillenials$month.cent <= 75,]

weightComb.tsEmillenials <- rbind(weightFace.tsEmillenials, weightBody.tsEmillenials)
weightComb.tsEmillenials <- weightComb.tsEmillenials[is.na(weightComb.tsEmillenials$month.cent)==FALSE,]
weightComb.tsEmillenials

weightComb.WtsEmillenials <- ts(weightComb.tsEmillenials$weight.mean, frequency = 12, start = c(2007, 1), end = c(2016,12))
weightComb.WtsEmillenials
plot(weightComb.WtsEmillenials)

# impute with seasonal decomposition and interpolation
weightComb.WtsEmillenials <- na.seadec(weightComb.WtsEmillenials, algorithm = "interpolation")
weightComb.WtsEmillenials
plot(weightComb.WtsEmillenials)

weightComb.arimaEmillenials <- auto.arima(weightComb.WtsEmillenials, stepwise = FALSE, approximation = FALSE)
weightComb.arimaEmillenials # parameters (1,1,1)(0,0,2)

forecastE.weightCombmillenials <- forecast(weightComb.arimaEmillenials, h = 60)
plot(forecastE.weightCombmillenials)

forecastE.weightCombmillenialsLONG <- forecast(weightComb.arimaEmillenials, h = 1800) # 150 years
plot(forecastE.weightCombmillenialsLONG)
abline(h = 0)

min(which(forecastE.weightCombmillenialsLONG$lower[,"95%"]<=0))/12 # 65.667
min(which(forecastE.weightCombmillenialsLONG$mean<=0))/12 # NA
min(which(forecastE.weightCombmillenialsLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.weightCombmillenialsLONG$upper[,"95%"]>=2*weightComb.WtsEmillenials[1]))/12 # 108.1667

## percent change
decompE.weightCombmillenials <- decompose(weightComb.WtsEmillenials)
(decompE.weightCombmillenials$trend[7]-decompE.weightCombmillenials$trend[114])/decompE.weightCombmillenials$trend[7] # -14.56%
plot(decompE.weightCombmillenials)
# initial value
weightComb.WtsEmillenials[1] # 0.93
# final value
weightComb.WtsEmillenials[120] # 0.84


## Fit to Gen Z
weightBody.tsEgenZ <- ddply(timeseriesWeightBody[timeseriesWeightBody$yobcats=="genZ",],
                           .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), cohens = cohensD(att.center1))
table(weightBody.tsEgenZ$month.cent)

# impute missing values
# add in NAs for month values 125:131 (all missing anyways)
weightBody.month.extra <- as.data.frame(c(105:107, 125:132))
weightBody.month.extra$mean <- rep(NA, 11)
weightBody.month.extra$weight.mean <- rep(NA, 11)
weightBody.month.extra$cohens <- rep(NA, 11)
names(weightBody.month.extra) <- c( "month.cent", "mean", "weight.mean", "cohens")
weightBody.tsEgenZ <- rbind(weightBody.month.extra, weightBody.tsEgenZ)
weightBody.tsEgenZ <- weightBody.tsEgenZ[order(weightBody.tsEgenZ$month.cent),]
weightBody.tsEgenZ

weightBody.WtsEgenZ <- ts(weightBody.tsEgenZ$weight.mean, frequency = 12, start = c(2010, 4), end = c(2016,12))
weightBody.WtsEgenZ

weightBody.WtsEgenZ <- window(weightBody.WtsEgenZ, start = c(2011, 1), end = c(2016,12))
plot(weightBody.WtsEgenZ)

# impute with seasonal decomposition and interpolation
weightBody.WtsEgenZ <- na.seadec(weightBody.WtsEgenZ, algorithm = "interpolation")
weightBody.WtsEgenZ
plot(weightBody.WtsEgenZ)

weightBody.arimaEgenZ <- auto.arima(weightBody.WtsEgenZ, stepwise = FALSE, approximation = FALSE)
weightBody.arimaEgenZ # parameters (0,1,2)

forecastE.weightBodygenZ <- forecast(weightBody.arimaEgenZ, h = 60)
plot(forecastE.weightBodygenZ)

forecastE.weightBodygenZLONG <- forecast(weightBody.arimaEgenZ, h = 1800) # 150 years
plot(forecastE.weightBodygenZLONG)
abline(h = 0)

min(which(forecastE.weightBodygenZLONG$lower[,"95%"]<=0))/12 # 13.0833
min(which(forecastE.weightBodygenZLONG$mean<=0))/12 # NA
min(which(forecastE.weightBodygenZLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecastE.weightBodygenZLONG$upper[,"95%"]>=2*weightBody.WtsEgenZ[1]))/12 # 42

## percent change
decompE.weightBodygenZ <- decompose(weightBody.WtsEgenZ)
(decompE.weightBodygenZ$trend[7]-decompE.weightBodygenZ$trend[66])/decompE.weightBodygenZ$trend[7] # -12.76%
plot(decompE.weightBodygenZ)
# initial value
weightBody.WtsEgenZ[1] # 1.11
# final value
weightBody.WtsEgenZ[72] # 0.81



## Summary with plots
op <- par(mfrow = c(2,2))
plot(forecastE.weightCombboom, main = "Boomers", ylim = c(0.2, 1.4))
plot(forecastE.weightCombgenX, main = "Gen X", ylim = c(0.2, 1.4))
plot(forecastE.weightCombmillenials, main = "Millenials", ylim = c(0.2, 1.4))
plot(forecastE.weightBodygenZ, main = "Gen Z", ylim = c(0.2, 1.4), xlim = c(2007, 2022))
par(op)



## Overlapping plots
plot(forecastE.weightCombboom, ylim = c(0.2, 1.2), main = "Explicit Body Weight Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.2), rgb(0,0.5,0.3, alpha = 0.2)), col = rgb(0,0.5,0.3), fcol = rgb(0,0.5,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.weightCombmillenials, ylim = c(0.2, 1.2), main = "Explicit Body Weight Attitudes",
     shadecols = c(rgb(1,0,0, alpha = 0.2), rgb(1,0.3,0, alpha = 0.2)), col = rgb(1,0.3,0), fcol = rgb(1,0,0, alpha = 0.2))
par(new = TRUE)
plot(forecastE.weightCombgenX, ylim = c(0.2, 1.2), main = "Explicit Body Weight Attitudes",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)), col = rgb(0,0.3,1), fcol = rgb(0,0,1, alpha = 0.2))
legend("bottomleft", c("Gen X", "Millenials", "Baby Boomers"), col = c(rgb(0,0.3,1), rgb(1,0.3,0), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 2) 


