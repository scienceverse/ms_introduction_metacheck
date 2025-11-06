## Charlesworth, & Banaji Patterns of Change I.
## Script version date: January 19th, 2018
####################################################################################################################################

## This file performs ARIMA time series analysis on the weighted time series vectors from 2007 onward (to 2016).

## Data used is the TSvectors.RData, which includes the vectors (both weighted and unweighted), from 2007 onward
# with seasonal decomposition and linear interpolation (for 2 months in skin-tone and 7 months in weightBody)

####################################################################################################################################

## Load data object
load("TSvectors_2016data_011918.RData")

# Install packages
install.packages("forecast", dependencies = TRUE) 
## ensure this is forecast version 8.2 (see reproducing instructions for further details).
# To install version 8.2, use the following code:
# install.packages("devtools"); require(devtools)
# install_version("forecast", version = "8.2")

install.packages("tseries", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("strucchange", dependencies = TRUE)

# Load packages
require(forecast) ## ensure this is forecast version 8.2
require(tseries)
require(lmtest)
require(strucchange)



####################################################################################################################################

#### WEIGHTED TIME SERIES ---------------------------------------------------------------------------------------------------------
## IMPLICIT ATTITUDES -------------------------------------------------------------------------------------------------------------

## AGE -------------------------------------------------------------------------------------------------------------------------
## visualize
plot(age.Wts)

## fit the model
age.arima <- auto.arima(age.Wts, stepwise = FALSE, approximation = FALSE)
age.arima # parameters (0,1,2)(2,0,0)

# Check diagnostics and accuracy of final model
tsdiag(age.arima)
plot(age.arima$residuals)
Box.test(age.arima$residuals)
accuracy(age.arima)
coeftest(age.arima)

## ARIMA forecasts
forecast.age <- forecast(age.arima, h = 60) # 5 years
plot(forecast.age)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality or doubling
forecast.ageLONG <- forecast(age.arima, h = 1800) # 150 years
plot(forecast.ageLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$mean<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]>=2*age.Wts[1]))/12 # NA

## percentage change from trends
decomp.age <- decompose(age.Wts)
decomp.age$trend
## starting value
decomp.age$trend[7] # 0.45
## ending value
decomp.age$trend[114] # 0.43
## percentage change
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7] # 5.36% decrease.


## DISABILITY -----------
plot(dis.Wts) 
## Fit model
dis.arima <- auto.arima(dis.Wts, stepwise = FALSE, approximation = FALSE)
dis.arima # parameters (0,1,2)

# Check model accuracy and diagnostics of our final model
tsdiag(dis.arima)
Box.test(dis.arima$residuals)
plot(dis.arima$residuals)
accuracy(dis.arima)
coeftest(dis.arima)

## ARIMA forecasts
forecast.dis <- forecast(dis.arima, h = 60) # 2 years
plot(forecast.dis)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality or doubling
forecast.disLONG <- forecast(dis.arima, h = 1800) # 150 years
plot(forecast.disLONG, ylim = c(0, 1))   # 80%, 95% prediction interval
abline(h = 0)
abline(h = 2*dis.Wts[1])

min(which(forecast.disLONG$lower[,"95%"]<=0))/12 #  NA
min(which(forecast.disLONG$mean<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]>=2*dis.Wts[1]))/12 # NA

## starting value
decomp.dis <- decompose(dis.Wts)
decomp.dis$trend[7] # 0.50
## ending value
decomp.dis$trend[114] # 0.49
## percentage change
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7] # 1.66% decrease




## RACE ---------------
plot(race.Wts)
## fit model
race.arima <- auto.arima(race.Wts, stepwise = FALSE, approximation = FALSE)
race.arima # parameters (1,1,1)(2,0,0)

# Check diagnostics and accuracy
tsdiag(race.arima)
plot(race.arima$residuals)
Box.test(race.arima$residuals)
accuracy(race.arima)
coeftest(race.arima)

## ARIMA forecasts
forecast.race <- forecast(race.arima, h = 60) # 2 years
plot(forecast.race)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality or doubling
forecast.raceLONG <- forecast(race.arima, h = 1800) # 150 years
plot(forecast.raceLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 # 57.667 years, months
min(which(forecast.raceLONG$mean<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.Wts[1]))/12 # 107.0833 years, months


## starting value
decomp.race <- decompose(race.Wts)
decomp.race$trend[7] # 0.33
## ending value
decomp.race$trend[114] # 0.27
## percentage change
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7] # 16.81% decrease


## SEXUALITY ------------
plot(sex.Wts)
## fit model
sex.arima <- auto.arima(sex.Wts, stepwise = FALSE, approximation = FALSE)
sex.arima # (0,1,2)(2,0,0)[12] with drift 

# Check the diagnostics of model
tsdiag(sex.arima) 
plot(sex.arima$residuals)
Box.test(sex.arima$residuals)
accuracy(sex.arima)
coeftest(sex.arima)

## ARIMA forecasts
forecast.sex <- forecast(sex.arima, h = 60) # 5 years
plot(forecast.sex)   # 80%, 95% prediction interval

## year for confidence interval to reach neutrality or doubling
forecast.sexLONG <- forecast(sex.arima, h = 1800) # 150 years
plot(forecast.sexLONG)
abline(h = 0)
abline(h = 2*sex.Wts[1])

min(which(forecast.sexLONG$lower[,"95%"]<=0))/12 # 9 years, months
min(which(forecast.sexLONG$mean<=0))/12 # 16.5 years, months
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 # 29.75 years, months


## starting value
decomp.sex <- decompose(sex.Wts)
decomp.sex$trend[7] # 0.33
## ending value
decomp.sex$trend[114] # 0.22
## percentage change
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7] # 33.46% decrease


## SKIN-TONE --------------
plot(skin.Wts)
## fit model
skin.arima <- auto.arima(skin.Wts, stepwise = FALSE, approximation = FALSE)
skin.arima # parameters (0,1,1)

# Check the diagnostics and accuracy
tsdiag(skin.arima) 
plot(skin.arima$residuals)
Box.test(skin.arima$residuals) 
accuracy(skin.arima) 
coeftest(skin.arima)

## ARIMA forecasts
forecast.skin <- forecast(skin.arima, h = 60) # 5 years
plot(forecast.skin)   # 80%, 95% prediction interval

## year for confidence interval to pass neutral or doubling
forecast.skinLONG <- forecast(skin.arima, h = 1800) # 150 years
plot(forecast.skinLONG)
abline(h = 0)

min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 # 138.58 years, months
min(which(forecast.skinLONG$mean<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.Wts[1]))/12 # NA

## starting value
decomp.skin <- decompose(skin.Wts)
decomp.skin$trend[7] # 0.33
## ending value
decomp.skin$trend[114] # 0.28
## percentage change
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7] # 14.59% decrease



## WEIGHT -----------
## (1) Body ---------
plot(weight.Wts)
# fit model
weight.arimaBody <- auto.arima(weight.Wts, stepwise = FALSE, approximation = FALSE)
weight.arimaBody # parameters (1,0,0)

## Check diagnostics
tsdiag(weight.arimaBody)
plot(weight.arimaBody$residuals)
Box.test(weight.arimaBody$residuals)
accuracy(weight.arimaBody)
coeftest(weight.arimaBody)

## ARIMA forecasts
forecast.weightBody <- forecast(weight.arimaBody, h = 60) 
plot(forecast.weightBody)   # 80%, 95% prediction interval

## year for confidence interval to reach neutrality or doubling
forecast.weightBodyLONG <- forecast(weight.arimaBody, h = 1800) # 200 years
plot(forecast.weightBodyLONG)

min(which(forecast.weightBodyLONG$lower[,"95%"]<=0)) # NA
min(which(forecast.weightBodyLONG$mean<=0)) # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]<=0)) # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.Wts[1])) # NA

# percentage change
decomp.weight <- decompose(weight.Wts)
## starting value
decomp.weight$trend[7] # 0.47
## ending value
decomp.weight$trend[75] # 0.49
## percentage change
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7] # increased by 4.67%



## (1) Face ---------
plot(weight.WtsFace) 
# fit model
weight.arimaFace <- auto.arima(weight.WtsFace, stepwise = FALSE, approximation = FALSE)
weight.arimaFace # parameters (0,1,1)(1,0,0)

## Check diagnostics
tsdiag(weight.arimaFace)
plot(weight.arimaFace$residuals)
Box.test(weight.arimaFace$residuals)
accuracy(weight.arimaFace)
coeftest(weight.arimaFace)

## ARIMA forecasts
forecast.weightFace <- forecast(weight.arimaFace, h = 98) # up to 2020
plot(forecast.weightFace)   # 80%, 95% prediction interval

## year for confidence interval to reach neutrality or doubling
forecast.weightFaceLONG <- forecast(weight.arimaFace, h = 1800) # 200 years
plot(forecast.weightFaceLONG)

min(which(forecast.weightFaceLONG$lower[,"95%"]<=0))/12 # years, 31.75 months
min(which(forecast.weightFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weight.WtsFace[1]))/12 # 6.6667 years, months
## note these numbers are from October, 2011.

# percentage change
decomp.weightFace <- decompose(weight.WtsFace)
## starting value
decomp.weightFace$trend[7] # 0.31
## ending value
decomp.weightFace$trend[84] # 0.44
## percentage change
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7] # increased by 40.10%


## (3) Compare body and face
## Overlapping plots
plot(forecast.weightBody, ylim = c(0.28, 0.6), xlim = c(2004,2020), main = "Implicit Body Weight Attitudes", ylab = "IAT D-score",
     shadecols = c(rgb(0,0,1, alpha = 0.2), rgb(0,0.3,1, alpha = 0.2)),
     col = rgb(0,0.3,1, alpha = 0.5), fcol = rgb(0,0,1, alpha = 0.5), flwd = 2)
lines(decomp.weight$trend, col = rgb(0,0.3,1), lwd = 3)
par(new = TRUE)
plot(forecast.weightFace, ylim = c(0.28, 0.6), xlim = c(2004,2020), main = "Implicit Body Weight Attitudes",
     shadecols = c(rgb(0,0.5,0, alpha = 0.1), rgb(0,0.5,0.3, alpha = 0.1)),
     col = rgb(0,0.5,0.3, alpha = 0.5), fcol = rgb(0,0.5,0, alpha = 0.5), flwd = 2)
lines(decomp.weightFace$trend, col = rgb(0,0.5,0.3), lwd = 3)
legend("bottom", c("Body Figure", "Face"), col = c(rgb(0,0.3,1), rgb(0,0.5,0.3)), bty = "n", lty = 1, lwd = 3) 



##############################################################################################################################
##############################################################################################################################

### EXPLICIT ATTITUDES ------------------------------------------------------------------------------------------------------------
## WEIGHTED ------------------------------------------------------------------------------------------------------------------------

## Age ------------------------------------------------------------------------------------------------------------------------

plot(age.WEts) 
# Fit the model
age.arimaE <- auto.arima(age.WEts, stepwise = FALSE, approximation = FALSE)
age.arimaE # parameters (0,1,2)(2,0,0)[12] 

## check model accuracy and residuals
tsdiag(age.arimaE) 
Box.test(age.arimaE$residuals) 
plot(age.arimaE$residuals) 
accuracy(age.arimaE) 
coeftest(age.arimaE)

## ARIMA forecasts
forecast.ageE <- forecast(age.arimaE, h = 60) # 5 years
plot(forecast.ageE)   # 80%, 95% prediction interval

# month for the confidence interval to reach neutrality or doubling
forecast.ageLONGE <- forecast(age.arimaE, h = 1800) # 150 years
plot(forecast.ageLONGE)
abline(h = 0)
abline(h = 2*age.WEts[1])


min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 # 8.41667 years, months
min(which(forecast.ageLONGE$mean<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.WEts[1]))/12 # 25.0833 years, months

## starting value
decomp.ageE <- decompose(age.WEts)
decomp.ageE$trend[7] # 0.52
## ending value
decomp.ageE$trend[114] # 0.34
## percentage change
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7] # 33.65% decrease


## Disability -----------
plot(dis.WEts) 
# fit model
dis.arimaE <- auto.arima(dis.WEts, stepwise = FALSE, approximation = FALSE)
dis.arimaE # parameters (1,1,1)(2,0,0) with drift
## will produce warning message, because the AR value is close to stationary. This can be ignored.

# Check the diagnostics
tsdiag(dis.arimaE)
Box.test(dis.arimaE$residuals)
plot(dis.arimaE$residuals)
accuracy(dis.arimaE)
coeftest(dis.arimaE)

## ARIMA forecasts
forecast.disE <- forecast(dis.arimaE, h = 60) # 5 years
plot(forecast.disE)   # 80%, 95% prediction interval

# month for the confidence interval to reach neutrality or doubling
forecast.disLONGE <- forecast(dis.arimaE, h = 2400) # 150 years
plot(forecast.disLONGE)
abline(h = 0)
abline(h = 2*dis.WEts[1])

min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 # 17.67 years, months
min(which(forecast.disLONGE$mean<=0))/12 # 23.92 years, months
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 # 30.92 years, months
min(which(forecast.disLONGE$upper[,"95%"]>=(2*dis.WEts[1])))/12 # NA

## starting value
decomp.disE <- decompose(dis.WEts)
decomp.disE$trend[7] # 0.54
## ending value
decomp.disE$trend[114] # 0.41
## percentdis change
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7] # 23.97% decrease



## Race ---------------
plot(race.WEts)
# fit the model
race.arimaE <- auto.arima(race.WEts, approximation = FALSE, stepwise = FALSE)
race.arimaE # (0,1,1)(2,0,2)[12] with drift 

## Check diagnostics
tsdiag(race.arimaE) 
accuracy(race.arimaE)
plot(race.arimaE$residuals)
Box.test(race.arimaE$residuals)
coeftest(race.arimaE) # all are significant

## ARIMA forecasts
forecast.raceE <- forecast(race.arimaE, h = 60) # 5 years
plot(forecast.raceE)   # 80%, 95% prediction interval

# month for the confidence interval to reach neutral or doubling
forecast.raceLONGE <- forecast(race.arimaE, h = 1800) # 150 years
plot(forecast.raceLONGE)
abline(h = 0)
abline(h = 2*race.WEts[1])

min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12 # 1.91667 years, months
min(which(forecast.raceLONGE$mean<=0))/12 # 7 years, months
min(which(forecast.raceLONGE$upper[,"95%"]<=0))/12 # 21.5 years, months
min(which(forecast.raceLONGE$upper[,"95%"]>=2*race.WEts[1]))/12 # NA

## starting value
decomp.raceE <- decompose(race.WEts)
decomp.raceE$trend[7] # 0.32
## ending value
decomp.raceE$trend[114] # 0.20
## percentrace change
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7] # 36.74% decrease



## Sexuality ------------
plot(sex.WEts) 
# fit the model
sex.arimaE <- auto.arima(sex.WEts, stepwise = FALSE, approximation = FALSE)
sex.arimaE # parameters (0,1,2)(2,0,0)[12] 

## Check the diagnostics
tsdiag(sex.arimaE) 
Box.test(sex.arimaE$residuals)
plot(sex.arimaE$residuals)
accuracy(sex.arimaE)
coeftest(sex.arimaE)

## ARIMA forecasts
forecast.sexE <- forecast(sex.arimaE, h = 60) # 5 years
plot(forecast.sexE)   # 80%, 95% prediction interval

# month for the confidence interval to reach neutrality or doubling
forecast.sexLONGE <- forecast(sex.arimaE, h = 1800) # 150 years
plot(forecast.sexLONGE)
abline(h = 0)
abline(h = 2*sex.WEts[1])

min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12 # 5.5 years, months
min(which(forecast.sexLONGE$mean<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.WEts[1]))/12 # 42.75 years, months

## starting value
decomp.sexE <- decompose(sex.WEts)
decomp.sexE$trend[7] ## 0.68
## ending value
decomp.sexE$trend[114] ## 0.35
## percentsex change
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7] # 48.59% decrease



## Skin-Tone --------------
plot(skin.WEts) 
# fit model
skin.arimaE <- auto.arima(skin.WEts, stepwise = FALSE, approximation = FALSE)
skin.arimaE # parameters (0,1,3)(1,0,1)[12] 

# Check diagnostics
tsdiag(skin.arimaE)
plot(skin.arimaE$residuals)
Box.test(skin.arimaE$residuals)
accuracy(skin.arimaE)
coeftest(skin.arimaE)

## ARIMA forecasts
forecast.skinE <- forecast(skin.arimaE, h = 60) # 5 years
plot(forecast.skinE)   # 80%, 95% prediction interval

forecast.skinLONGE <- forecast(skin.arimaE, h = 1800) # 150 years
plot(forecast.skinLONGE)
abline(h = 0)
abline(h = 2*skin.WEts[1])

min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 # 2.58333 years, months
min(which(forecast.skinLONGE$mean<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.WEts[1]))/12 # 23.667 years, months


## starting value
decomp.skinE <- decompose(skin.WEts)
decomp.skinE$trend[7] # 0.27
## ending value
decomp.skinE$trend[114] # 0.21
## percentskin change
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7] # 21.04% decrease




## Weight -----------
plot(weight.WEts) 
# fit the model
weight.arimaE <- auto.arima(weight.WEts, stepwise = FALSE, approximation = FALSE)
weight.arimaE # parameters (0,1,2)(2,0,0)[12] 

## Check diagnostics
tsdiag(weight.arimaE)
plot(weight.arimaE$residuals)
Box.test(weight.arimaE$residuals)
accuracy(weight.arimaE)
coeftest(weight.arimaE)

## ARIMA forecasts
forecast.weightE <- forecast(weight.arimaE, h = 60) # 5 years
plot(forecast.weightE)   # 80%, 95% prediction interval

# month for the confidence interval to reach neutrality or doubling
forecast.weightLONGE <- forecast(weight.arimaE, h = 1800) # 150 years
plot(forecast.weightLONGE)
abline(h = 0)
abline(h = 2*weight.WEts[1])


min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12 # 34.25 years, months
min(which(forecast.weightLONGE$mean<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.WEts[1]))/12 # 67.5 years, months


## starting value
decomp.weightE <- decompose(weight.WEts)
decomp.weightE$trend[7] ## 0.96
## ending value
decomp.weightE$trend[114] ## 0.82
## percentage change
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7] # 14.81% decrease


##############################################################################################################################
##############################################################################################################################

## SUMMARIES -----------------
## (1) Model parameters ---------
forecast.age$model # Age Implicit (0,1,2)(2,0,0)
forecast.ageE$model # Age Explicit (0,1,2)(2,0,0)
forecast.dis$model # Disability Implicit (0,1,2) 
forecast.disE$model # Disability Explicit (1,1,1)(2,0,0) + drift
forecast.race$model # Race Implicit (1,1,1)(2,0,0)
forecast.raceE$model # Race Explicit (0,1,1)(2,0,2) + drift 
forecast.sex$model # Sexuality Implicit (0,1,2)(2,0,0) + drift 
forecast.sexE$model # Sexuality Explicit (0,1,2)(2,0,0)
forecast.skin$model # Skin-Tone Implicit (0,1,1) 
forecast.skinE$model # Skin-Tone Explicit (0,1,3)(1,0,1)
forecast.weightBody$model # Weight Implicit (Body) (1,0,0)
forecast.weightFace$model # Weight Implicit (Face) (0,1,1)(1,0,0)
forecast.weightE$model # Weight Explicit (0,1,2)(2,0,0)

## (2) Forecasts to neutrality ----------
## EXPLICIT --------
## AGE
min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 # 8.41667 (8 yrs, 5 mos)
min(which(forecast.ageLONGE$mean<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.WEts[1]))/12 # 26.08333 (26 yrs, 1 mos)

## DISABILITY
min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 # 17.5 (17 yrs, 6 mos)
min(which(forecast.disLONGE$mean<=0))/12 # 23.91667 (23 yrs, 11 mos)
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 # 30.91667 (30 yrs, 11 mos)
min(which(forecast.disLONGE$upper[,"95%"]>=2*dis.WEts[1]))/12 # NA

## RACE
min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12 # 1.91667 (1 yr, 11 mos)
min(which(forecast.raceLONGE$mean<=0))/12 # 7 yrs
min(which(forecast.raceLONGE$upper[,"95%"]<=0))/12 # 21.5 (21 yrs, 6 mos)

## SEXUALITY
min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12 # 5.5 (5 yrs, 6 mos)
min(which(forecast.sexLONGE$mean<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.WEts[1]))/12 # 42.75 (42 yrs, 9 mos)

## SKIN
min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 # 2.58333 (2 yrs, 7 mos)
min(which(forecast.skinLONGE$mean<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.WEts[1]))/12 # 23.667 (23 yrs, 8 mos)

## BODY WEIGHT
min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12 # 34.25 (34 yrs, 3 mos)
min(which(forecast.weightLONGE$mean<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.WEts[1]))/12 # 67.5 (67 yrs, 6 mos)

## IMPLICIT --------
## AGE
min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$mean<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]>=2*age.Wts[1]))/12 # NA

## DISABILITY
min(which(forecast.disLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$mean<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]>=2*dis.Wts[1]))/12 # NA

## RACE
min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 # 57.667 (57 yrs, 8 mos)
min(which(forecast.raceLONG$mean<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.Wts[1]))/12 # 107.0833 (107 yrs, 1 mos)

## SEXUALITY
min(which(forecast.sexLONG$lower[,"95%"]<=0))/12 # 9 yrs
min(which(forecast.sexLONG$mean<=0))/12 # 16.5 (16 yrs, 6 mos)
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 # 29.75 (29 yrs, 9 mos)
min(which(forecast.sexLONG$upper[,"95%"]>=2*sex.Wts[1]))/12 # NA

## SKIN
min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 # 138.5833 (138 yrs, 7 mos)
min(which(forecast.skinLONG$mean<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.Wts[1]))/12 # NA

## BODY WEIGHT (BODY)
min(which(forecast.weightBodyLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$mean<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.Wts[1]))/12 #NA

## BODY WEIGHT (FACE) ** from Oct 2011
min(which(forecast.weightFaceLONG$lower[,"95%"]<=0))/12 # 31.75 (31 yrs, 9 mos) = July, 2043
min(which(forecast.weightFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weight.WtsFace[1]))/12 # 6.667 (6 yrs, 8 mos) = June, 2018

## (3) Percentage changes from trend values ----------
## EXPLICIT -------
## AGE
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7] # 33.65% decrease
## DIS
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7] # 23.97% decrease
## RACE
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7] # 36.74% decrease
## SEX
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7] # 48.59% decrease
## SKIN
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7] # 21.04% decrease
## WEIGHT
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7] # 14.81% decrease

## IMPLICIT -----
## AGE
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7] # 5.36% decrease
## DIS
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7] # 1.66% decrease
## RACE
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7] # 16.81% decrease
## SEX
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7] # 33.46% decrease
## SKIN
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7] # 14.59% decrease
## WEIGHT
# Body
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7] # increased by 4.67%
# Face
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7] # increased by 40.10%

## (4) First and Last raw values ----------
## EXPLICIT -------
## AGE
age.WEts[1]; age.WEts[120] # 0.52, 0.40
## DIS
dis.WEts[1]; dis.WEts[120] # 0.64, 0.44
## RACE
race.WEts[1]; race.WEts[120] # 0.30, 0.17
## SEX
sex.WEts[1]; sex.WEts[120] # 0.67, 0.35
## SKIN
skin.WEts[1]; skin.WEts[120] # 0.30, 0.15
## WEIGHT
weight.WEts[1]; weight.WEts[120] # 0.90, 0.80

## IMPLICIT -----
## AGE
age.Wts[1]; age.Wts[120] # 0.45, 0.42
## DIS
dis.Wts[1]; dis.Wts[120] # 0.51, 0.52
## RACE
race.Wts[1]; race.Wts[120] # 0.33, 0.30
## SEX
sex.Wts[1]; sex.Wts[120] # 0.33, 0.17
## SKIN
skin.Wts[1]; skin.Wts[120] # 0.33, 0.29
## WEIGHT
# Body
weight.Wts[1]; weight.Wts[81] # 0.48, 0.48
# Face
weight.WtsFace[1]; weight.WtsFace[90] # 0.30, 0.41

# (2) Plots ---------
pdf("changeIATfinalplot.pdf", width = 12, height = 16)
op <- par(mfrow = c(4,3))
plot(forecast.sex, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.sex$trend, col = "black", lwd = 3)
text(2017, 0.11, "neutral:\n9 yrs", cex = 0.95, adj = 0)
text(2017, 0.3, "neutral:\n29 yrs, 9 mos", cex = 0.95, adj = 0)

plot(forecast.race, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.race$trend, col = "black", lwd = 3)
text(2017, 0.2, "neutral:\n57 yrs, 8 mos", cex = 0.95, adj = 0)
text(2017, 0.36, "double:\n107 yrs, 1 mos", cex = 0.95, adj = 0)

plot(forecast.skin, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.skin$trend, col = "black", lwd = 3)
text(2017, 0.23, "neutral:\n138 yrs, 7 mos", cex = 0.95, adj = 0)
text(2017, 0.35, "double:\n>150 yrs", cex = 0.95, adj = 0)

plot(forecast.age, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.age$trend, col = "black", lwd = 3)
text(2017, 0.37, "neutral:\n>150 yrs", cex = 0.95, adj = 0)
text(2017, 0.48, "double:\n>150 yrs", cex = 0.95, adj = 0)

plot(forecast.dis, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.dis$trend, col = "black", lwd = 3)
text(2017, 0.43, "neutral:\n>150 yrs", cex = 0.95, adj = 0)
text(2017, 0.56, "double:\n>150 yrs", cex = 0.95, adj = 0)

plot(forecast.weightFace, main = "Implicit Body-Weight Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "lightgrey", 
     xlim = c(2004, 2020), fcol = "darkgrey", flty = 3,
     shadecols = c("gray90", "gray96"))
lines(decomp.weightFace$trend, col = "darkgrey", lwd = 3, lty = 2)
par(new = TRUE)
plot(forecast.weightBody, main = "Implicit Body-Weight Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2004, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"),
     axes = FALSE)
lines(decomp.weight$trend, col = "black", lwd = 3)
text(2017, 0.53, "not changing", cex = 0.95, adj = 0)
text(2009, 0.35, "Face stimuli", cex = 1, adj = 0)
text(2009, 0.32, "Figure stimuli", cex = 1, adj = 0)
lines(x = c(2007, 2008.5), y = c(0.35, 0.35), lty = 2, lwd = 3, col = "darkgrey")
lines(x = c(2007, 2008.5), y = c(0.32, 0.32), lwd = 3, col = "black")


plot(forecast.sexE, main = "Explicit Sexuality Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.sexE$trend, col = "black", lwd = 3)
text(2017, 0.05, "neutral:\n5 yrs, 6 mos", cex = 0.95, adj = 0)
text(2017, 0.62, "double:\n42 yrs, 9 mos", cex = 0.95, adj = 0)
plot(forecast.raceE, main = "Explicit Race Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.raceE$trend, col = "black", lwd = 3)
text(2017, -0.05, "neutral:\n1 yr, 11 mos", cex = 0.95, adj = 0)
text(2017, 0.36, "neutral:\n21 yrs, 6 mos", cex = 0.95, adj = 0)
plot(forecast.skinE, main = "Explicit Skin-Tone Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.skinE$trend, col = "black", lwd = 3)
text(2017, -0.05, "neutral:\n2 yrs, 7 mos", cex = 0.95, adj = 0)
text(2017, 0.38, "double:\n23 yrs, 8 mos", cex = 0.95, adj = 0)

plot(forecast.ageE, main = "Explicit Age Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.ageE$trend, col = "black", lwd = 3)
text(2017, 0.09, "neutral:\n8 yrs, 5 mos", cex = 0.95, adj = 0)
text(2017, 0.58, "double:\n26 yrs, 1 mos", cex = 0.95, adj = 0)
plot(forecast.disE, main = "Explicit Disability Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.disE$trend, col = "black", lwd = 3)
text(2017, 0.24, "neutral:\n17 yrs, 6 mos", cex = 0.95, adj = 0)
text(2017, 0.52, "neutral:\n30 yrs, 11 mos", cex = 0.95, adj = 0)
plot(forecast.weightE, main = "Explicit Body-Weight Attitudes", ylab = "Explicit preference score", ylim = c(0.5, 1.3), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.weightE$trend, col = "black", lwd = 3)
text(2017, 0.55, "neutral:\n34 yrs, 3 mos", cex = 0.95, adj = 0)
text(2017, 1.03, "double:\n67 yrs, 6 mos", cex = 0.95, adj = 0)
par(op)
dev.off()



## (3) Model fit ----------
## IMPLICIT
## Age
plot(forecast.age$residuals)
Box.test(forecast.age$residuals)
accuracy(forecast.age)

## Disability
plot(forecast.dis$residuals)
Box.test(forecast.dis$residuals)
accuracy(forecast.dis)

## Race
plot(forecast.race$residuals)
Box.test(forecast.race$residuals)
accuracy(forecast.race)

## Sexuality
plot(forecast.sex$residuals)
Box.test(forecast.sex$residuals)
accuracy(forecast.sex)

## Skin
plot(forecast.skin$residuals)
Box.test(forecast.skin$residuals)
accuracy(forecast.skin)

## Weight
plot(forecast.weightBody$residuals)
Box.test(forecast.weightBody$residuals)
accuracy(forecast.weightBody)


## EXPLICIT
## Age
plot(forecast.ageE$residuals)
Box.test(forecast.ageE$residuals)
accuracy(forecast.ageE)

## Disability
plot(forecast.disE$residuals)
Box.test(forecast.disE$residuals)
accuracy(forecast.disE)

## Race
plot(forecast.raceE$residuals)
Box.test(forecast.raceE$residuals)
accuracy(forecast.raceE)

## Sexuality
plot(forecast.sexE$residuals)
Box.test(forecast.sexE$residuals)
accuracy(forecast.sexE)

## Skin
plot(forecast.skinE$residuals)
Box.test(forecast.skinE$residuals)
accuracy(forecast.skinE)

## Weight
plot(forecast.weightE$residuals)
Box.test(forecast.weightE$residuals)
accuracy(forecast.weightE)


## (4) Average autocorrelation -------
(Acf(age.Wts)$acf[2]+
   Acf(age.WEts)$acf[2]+
   Acf(dis.Wts)$acf[2]+
   Acf(dis.WEts)$acf[2]+
   Acf(race.Wts)$acf[2]+
   Acf(race.WEts)$acf[2]+
   Acf(sex.Wts)$acf[2]+
   Acf(sex.WEts)$acf[2]+
   Acf(skin.Wts)$acf[2]+
   Acf(skin.WEts)$acf[2]+
   Acf(weight.Wts)$acf[2]+
   Acf(weight.WEts)$acf[2])/12

median(c(Acf(age.Wts)$acf[2],
         Acf(age.WEts)$acf[2],
         Acf(dis.Wts)$acf[2],
         Acf(dis.WEts)$acf[2],
         Acf(race.Wts)$acf[2],
         Acf(race.WEts)$acf[2],
         Acf(sex.Wts)$acf[2],
         Acf(sex.WEts)$acf[2],
         Acf(skin.Wts)$acf[2],
         Acf(skin.WEts)$acf[2],
         Acf(weight.Wts)$acf[2],
         Acf(weight.WEts)$acf[2]))

range(c(Acf(age.Wts)$acf[2],
        Acf(age.WEts)$acf[2],
        Acf(dis.Wts)$acf[2],
        Acf(dis.WEts)$acf[2],
        Acf(race.Wts)$acf[2],
        Acf(race.WEts)$acf[2],
        Acf(sex.Wts)$acf[2],
        Acf(sex.WEts)$acf[2],
        Acf(skin.Wts)$acf[2],
        Acf(skin.WEts)$acf[2],
        Acf(weight.Wts)$acf[2],
        Acf(weight.WEts)$acf[2]))
