## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 21st, 2018
####################################################################################################################################

## This file performs ARIMA time series analysis on the weighted time series vectors from 2007-2014.
# Then uses forecasts from these models in comparison to the actual 2015 and 2016 data to evaluate forecast accuracy.

## This serves to validate the overall model class, and demonstrate the functionality of time series analysis.
# Note that the models from 2007 - 2014 data may vary from the 2007 - 2016 data models.

## Data used is the TSvectors.RData, which includes the vectors (both weighted and unweighted), from 2007 onward
# with seasonal decomposition and linear interpolation (for 2 months in skin-tone and 7 months in weight)


####################################################################################################################################

## Load data
load("TSvectors_2016data_011918.RData")

# Install packages
install.packages("forecast", dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("lmtest", dependencies = TRUE)

# Load packages
require(forecast) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
require(lmtest)


##### IMPLICIT ATTITUDES ------------------
## AGE 
## first subset to TEST/TRAIN
age.WtsTRAIN <- window(age.Wts, start = c(2007,1), end = c(2014, 12))
age.WtsTRAIN

age.WtsTEST <- window(age.Wts, start = c(2015, 1))
age.WtsTEST

age.arimaTRAIN <- auto.arima(age.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
age.arimaTRAIN 

forecast.ageTRAIN <- forecast(age.arimaTRAIN, h = 24)

plot(forecast.ageTRAIN, ylim = c(0.35, 0.48), main = "Implicit Age Attitudes")
lines(age.WtsTEST, col = "red")

accuracy(forecast.ageTRAIN, age.WtsTEST)


## DISABILITY
## first subset to TEST/TRAIN
dis.WtsTRAIN <- window(dis.Wts, start = c(2007,1), end = c(2014, 12))
dis.WtsTRAIN

dis.WtsTEST <- window(dis.Wts, start = c(2015, 1))
dis.WtsTEST

dis.arimaTRAIN <- auto.arima(dis.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
dis.arimaTRAIN 

forecast.disTRAIN <- forecast(dis.arimaTRAIN, h = 24)

plot(forecast.disTRAIN)
lines(dis.WtsTEST, col = "red")

accuracy(forecast.disTRAIN, dis.WtsTEST)



## RACE
## first subset to TEST/TRAIN
race.WtsTRAIN <- window(race.Wts, start = c(2007,1), end = c(2014, 12))
race.WtsTRAIN

race.WtsTEST <- window(race.Wts, start = c(2015, 1))
race.WtsTEST

race.arimaTRAIN <- auto.arima(race.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
race.arimaTRAIN 

forecast.raceTRAIN <- forecast(race.arimaTRAIN, h = 24)

plot(forecast.raceTRAIN)
lines(race.WtsTEST, col = "red")

accuracy(forecast.raceTRAIN, race.WtsTEST)



## SEXUALITY
## first subset to TEST/TRAIN
sex.WtsTRAIN <- window(sex.Wts, start = c(2007,1), end = c(2014, 12))
sex.WtsTRAIN

sex.WtsTEST <- window(sex.Wts, start = c(2015, 1))
sex.WtsTEST

sex.arimaTRAIN <- auto.arima(sex.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
sex.arimaTRAIN 

forecast.sexTRAIN <- forecast(sex.arimaTRAIN, h = 24)

plot(forecast.sexTRAIN)
lines(sex.WtsTEST, col = "red")

accuracy(forecast.sexTRAIN, sex.WtsTEST)



## SKIN TONE
## first subset to TEST/TRAIN
skin.WtsTRAIN <- window(skin.Wts, start = c(2007,1), end = c(2014, 12))
skin.WtsTRAIN

skin.WtsTEST <- window(skin.Wts, start = c(2015, 1))
skin.WtsTEST

skin.arimaTRAIN <- auto.arima(skin.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
skin.arimaTRAIN 

forecast.skinTRAIN <- forecast(skin.arimaTRAIN, h = 24)

plot(forecast.skinTRAIN)
lines(skin.WtsTEST, col = "red")

accuracy(forecast.skinTRAIN, skin.WtsTEST)


## BODY WEIGHT
## Body figure *note that there is really not much data to go off of...
## first subset to TEST/TRAIN
weight.WtsTRAIN <- window(weight.Wts, start = c(2010,4), end = c(2014, 12))
weight.WtsTRAIN

weight.WtsTEST <- window(weight.Wts, start = c(2015, 1))
weight.WtsTEST

weight.arimaTRAIN <- auto.arima(weight.WtsTRAIN, stepwise = FALSE, approximation = FALSE)
weight.arimaTRAIN 

forecast.weightTRAIN <- forecast(weight.arimaTRAIN, h = 24)

plot(forecast.weightTRAIN)
lines(weight.WtsTEST, col = "red")

accuracy(forecast.weightTRAIN, weight.WtsTEST)



####### EXPLICIT ATTITUDES ----------
## AGE
## first subset to TEST/TRAIN
age.WEtsTRAIN <- window(age.WEts, start = c(2007,1), end = c(2014, 12))
age.WEtsTRAIN

age.WEtsTEST <- window(age.WEts, start = c(2015, 1))
age.WEtsTEST

age.arimaTRAINE <- auto.arima(age.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
age.arimaTRAINE

forecast.ageTRAINE <- forecast(age.arimaTRAINE, h = 24)

plot(forecast.ageTRAINE)
lines(age.WEtsTEST, col = "red")

accuracy(forecast.ageTRAINE, age.WEtsTEST)



## DISABILITY
## first subset to TEST/TRAIN
dis.WEtsTRAIN <- window(dis.WEts, start = c(2007,1), end = c(2014, 12))
dis.WEtsTRAIN

dis.WEtsTEST <- window(dis.WEts, start = c(2015, 1))
dis.WEtsTEST

dis.arimaTRAINE <- auto.arima(dis.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
dis.arimaTRAINE 

forecast.disTRAINE <- forecast(dis.arimaTRAINE, h = 24)

plot(forecast.disTRAINE)
lines(dis.WEtsTEST, col = "red")

accuracy(forecast.disTRAINE, dis.WEtsTEST)



## RACE
## first subset to TEST/TRAIN
race.WEtsTRAIN <- window(race.WEts, start = c(2007,1), end = c(2014, 12))
race.WEtsTRAIN

race.WEtsTEST <- window(race.WEts, start = c(2015, 1))
race.WEtsTEST

race.arimaTRAINE <- auto.arima(race.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
race.arimaTRAINE 

forecast.raceTRAINE <- forecast(race.arimaTRAINE, h = 24)

plot(forecast.raceTRAINE)
lines(race.WEtsTEST, col = "red")

accuracy(forecast.raceTRAINE, race.WEtsTEST)



## SEXUALITY
## first subset to TEST/TRAIN
sex.WEtsTRAIN <- window(sex.WEts, start = c(2007,1), end = c(2014, 12))
sex.WEtsTRAIN

sex.WEtsTEST <- window(sex.WEts, start = c(2015, 1))
sex.WEtsTEST

sex.arimaTRAINE <- auto.arima(sex.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
sex.arimaTRAINE 

forecast.sexTRAINE <- forecast(sex.arimaTRAINE, h = 24)

plot(forecast.sexTRAINE)
lines(sex.WEtsTEST, col = "red")

accuracy(forecast.sexTRAINE, sex.WEtsTEST)



## SKIN TONE
## first subset to TEST/TRAIN
skin.WEtsTRAIN <- window(skin.WEts, start = c(2007,1), end = c(2014, 12))
skin.WEtsTRAIN

skin.WEtsTEST <- window(skin.WEts, start = c(2015, 1))
skin.WEtsTEST

skin.arimaTRAINE <- auto.arima(skin.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
skin.arimaTRAINE 

forecast.skinTRAINE <- forecast(skin.arimaTRAINE, h = 24)

plot(forecast.skinTRAINE)
lines(skin.WEtsTEST, col = "red")

accuracy(forecast.skinTRAINE, skin.WEtsTEST)


## BODY WEIGHT
## first subset to TEST/TRAIN
weight.WEtsTRAIN <- window(weight.WEts, start = c(2007,1), end = c(2014, 12))
weight.WEtsTRAIN

weight.WEtsTEST <- window(weight.WEts, start = c(2015, 1))
weight.WEtsTEST

weight.arimaTRAINE <- auto.arima(weight.WEtsTRAIN, stepwise = FALSE, approximation = FALSE)
weight.arimaTRAINE 

forecast.weightTRAINE <- forecast(weight.arimaTRAINE, h = 24)

plot(forecast.weightTRAINE)
lines(weight.WEtsTEST, col = "red")

accuracy(forecast.weightTRAINE, weight.WEtsTEST)


## Summaries ---------------
# (1) Accuracy -----------
## Implicit attitudes ---------
accuracy(forecast.ageTRAIN, age.WtsTEST)[2,c(3,4)]
accuracy(forecast.disTRAIN, dis.WtsTEST)[2,c(3,4)]
accuracy(forecast.raceTRAIN, race.WtsTEST)[2,c(3,4)]
accuracy(forecast.skinTRAIN, skin.WtsTEST)[2,c(3,4)]
accuracy(forecast.sexTRAIN, sex.WtsTEST)[2,c(3,4)]
accuracy(forecast.weightTRAIN, weight.WtsTEST)[2,c(3,4)]

## Explicit attitudes ---------
accuracy(forecast.ageTRAINE, age.WEtsTEST)[2,c(3,4)]
accuracy(forecast.disTRAINE, dis.WEtsTEST)[2,c(3,4)]
accuracy(forecast.raceTRAINE, race.WEtsTEST)[2,c(3,4)]
accuracy(forecast.skinTRAINE, skin.WEtsTEST)[2,c(3,4)]
accuracy(forecast.sexTRAINE, sex.WEtsTEST)[2,c(3,4)]
accuracy(forecast.weightTRAINE, weight.WEtsTEST)[2,c(3,4)]

# (2) Plots --------------
## Implicit Attitudes ----------
op <- par(mfrow = c(2,3))
plot(forecast.ageTRAIN, main = "Implicit Age Attitudes", ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(age.WtsTEST, col = "red")
plot(forecast.disTRAIN, main = "Implicit Disability Attitudes", ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(dis.WtsTEST, col = "red")
plot(forecast.raceTRAIN, main = "Implicit Race Attitudes", ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(race.WtsTEST, col = "red")
plot(forecast.sexTRAIN, main = "Implicit Sexuality Attitudes", ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(sex.WtsTEST, col = "red")
plot(forecast.skinTRAIN, main = "Implicit Skin-Tone Attitudes", ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(skin.WtsTEST, col = "red")
plot(forecast.weightTRAIN, main = "Implicit Body-Weight Attitudes", xlim = c(2007, 2017), ylim = c(0.1, 0.6), ylab = "IAT D-score")
lines(weight.WtsTEST, col = "red")
par(op)


## Explicit attitudes --------
op <- par(mfrow = c(2,3))
plot(forecast.ageTRAINE, main = "Explicit Age Attitudes", ylim = c(0.1, 0.8), ylab = "Explicit Preference")
lines(age.WEtsTEST, col = "red", lwd = 2)
plot(forecast.disTRAINE, main = "Explicit Disability Attitudes", ylim = c(0.1, 0.8), ylab = "Explicit Preference")
lines(dis.WEtsTEST, col = "red", lwd = 2)
plot(forecast.raceTRAINE, main = "Explicit Race Attitudes", ylim = c(0.1, 0.8), ylab = "Explicit Preference")
lines(race.WEtsTEST, col = "red", lwd = 2)
plot(forecast.sexTRAINE, main = "Explicit Sexuality Attitudes", ylim = c(0.1, 0.8), ylab = "Explicit Preference")
lines(sex.WEtsTEST, col = "red", lwd = 2)
plot(forecast.skinTRAINE, main = "Explicit Skin-Tone Attitudes", ylim = c(0.1, 0.8), ylab = "Explicit Preference")
lines(skin.WEtsTEST, col = "red", lwd = 2)
plot(forecast.weightTRAINE, main = "Explicit Body-Weight Attitudes", ylim = c(0.5, 1.2), ylab = "Explicit Preference")
lines(weight.WEtsTEST, col = "red", lwd = 2)
par(op)

