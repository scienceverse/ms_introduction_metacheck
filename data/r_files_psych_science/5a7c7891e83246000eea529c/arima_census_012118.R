## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 21st, 2018
####################################################################################################################################

## This file performs ARIMA time series analysis on the CENSUS weighted time series vectors from 2007 onward (to 2016).

## Data used is the TSvectors for CENSUS, which includes the vectors (both weighted and unweighted), from 2007 onward
# with seasonal decomposition and linear interpolation (for 2 months in skin-tone and 7 months in weightBody)

####################################################################################################################################

# Install packages
install.packages("forecast") ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("tseries")
install.packages("lmtest")
install.packages("strucchange")

# Load packages
require(forecast) ## ensure this is forecast package v8.2
require(tseries)
require(lmtest)
require(strucchange)

load("TSvectors_census2016_012218.RData")


####################################################################################################################################

#### WEIGHTED TIME SERIES ---------------------------------------------------------------------------------------------------------
## IMPLICIT ATTITUDES -------------------------------------------------------------------------------------------------------------

## AGE -------------------------------------------------------------------------------------------------------------------------
## visualize and simple model
plot(age.CWts)
age.CTS <- tslm(age.CWts ~ trend + season)
dwtest(age.CTS)
acf.age <- Acf(age.CWts)
plot(acf.age)
acf.age$acf[2]

## fit the model
age.arima <- auto.arima(age.CWts, stepwise = FALSE, approximation = FALSE)
age.arima

# Check diagnostics and accuracy of final model
tsdiag(age.arima)
plot(age.arima$residuals)
Box.test(age.arima$residuals)
accuracy(age.arima)
coeftest(age.arima)

## ARIMA forecasts
forecast.age <- forecast(age.arima, h = 60) # 5 years
plot(forecast.age)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.ageLONG <- forecast(age.arima, h = 1800) # 150 years
plot(forecast.ageLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 
min(which(forecast.ageLONG$mean<=0))/12 
min(which(forecast.ageLONG$upper[,"95%"]<=0))/12 

## percentage change from trends
decomp.age <- decompose(age.CWts)
decomp.age$trend
## starting value
decomp.age$trend[7]
## ending value
decomp.age$trend[114]
## percentage change
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7]


## DISABILITY -----------
plot(dis.CWts) # autocorrelations
dis.Cts <- tslm(dis.CWts ~ trend + season)
dwtest(dis.Cts)
acf.dis <- Acf(dis.CWts)
plot(acf.dis)
acf.dis$acf[2]

# Fit model
dis.arima <- auto.arima(dis.CWts, stepwise = FALSE, approximation = FALSE)
dis.arima 

# Check model accuracy and diagnostics of our final model
tsdiag(dis.arima)
Box.test(dis.arima$residuals)
plot(dis.arima$residuals)
accuracy(dis.arima)
coeftest(dis.arima)

## ARIMA forecasts
forecast.dis <- forecast(dis.arima, h = 60) # 2 years
plot(forecast.dis)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.disLONG <- forecast(dis.arima, h = 1800) # 150 years
plot(forecast.disLONG, ylim = c(0, 1))   # 80%, 95% prediction interval
abline(h = 0)
abline(h = 2*dis.CWts[1])

min(which(forecast.disLONG$lower[,"95%"]<=0))/12 
min(which(forecast.disLONG$mean<=0))/12 
min(which(forecast.disLONG$upper[,"95%"]<=0))/12 

## starting value
decomp.dis <- decompose(dis.CWts)
decomp.dis$trend[7]
## ending value
decomp.dis$trend[114]
## percentage change
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7]


## RACE ---------------
plot(race.CWts)
race.Cts <- tslm(race.CWts ~ trend + season)
dwtest(race.Cts) 
acf.race <- Acf(race.CWts)
plot(acf.race)
acf.race$acf[2] 

## Fit the model
race.arima <- auto.arima(race.CWts, stepwise = FALSE, approximation = FALSE)
race.arima

fitted(race.arima)
plot(race.CWts)
lines(fitted(race.arima), col = "red")

# Check diagnostics and accuracy
tsdiag(race.arima)
plot(race.arima$residuals)
Box.test(race.arima$residuals)
accuracy(race.arima)
coeftest(race.arima)

## ARIMA forecasts
forecast.race <- forecast(race.arima, h = 60) # 2 years
plot(forecast.race)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.raceLONG <- forecast(race.arima, h = 1800) # 150 years
plot(forecast.raceLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 
min(which(forecast.raceLONG$mean<=0))/12 
min(which(forecast.raceLONG$upper[,"95%"]<=0))/12
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.CWts[1]))/12 


## starting value
decomp.race <- decompose(race.CWts)
decomp.race$trend[7]
## ending value
decomp.race$trend[114]
## percentage change
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7]


## SEXUALITY ------------

plot(sex.CWts)
sex.Cts <- tslm(sex.CWts ~ trend + season)
dwtest(sex.Cts) 
acf.sex <- Acf(sex.CWts)
plot(acf.sex)
acf.sex$acf[2]


# Fit the model
sex.arima <- auto.arima(sex.CWts, stepwise = FALSE, approximation = FALSE)
sex.arima

# Check the diagnostics of model
tsdiag(sex.arima) 
plot(sex.arima$residuals)
Box.test(sex.arima$residuals)
accuracy(sex.arima)
coeftest(sex.arima)

## ARIMA forecasts
forecast.sex <- forecast(sex.arima, h = 60) # 5 years
plot(forecast.sex)   # 80%, 95% prediction interval

## year for confidence interval to pass 0
forecast.sexLONG <- forecast(sex.arima, h = 1800) # 150 years
plot(forecast.sexLONG)
abline(h = 0)
abline(h = 2*sex.CWts[1])

min(which(forecast.sexLONG$lower[,"95%"]<=0))/12
min(which(forecast.sexLONG$mean<=0))/12 
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 
min(which(forecast.sexLONG$upper[,"95%"]>=2*sex.CWts[1]))/12


## starting value
decomp.sex <- decompose(sex.CWts)
decomp.sex$trend[7]
## ending value
decomp.sex$trend[114]
## percentage change
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7]




## SKIN-TONE --------------
plot(skin.CWts)
skin.Cts <- tslm(skin.CWts ~ trend + season)
dwtest(skin.Cts) 
acf.skin <- Acf(skin.CWts)
plot(acf.skin)
acf.skin$acf[2] 

# Fit the model
skin.arima <- auto.arima(skin.CWts, stepwise = FALSE, approximation = FALSE)
skin.arima 

# Compare the diagnostics
tsdiag(skin.arima) 
plot(skin.arima$residuals)
Box.test(skin.arima$residuals) 
accuracy(skin.arima) 
coeftest(skin.arima)

## ARIMA forecasts
forecast.skin <- forecast(skin.arima, h = 60) # 5 years
plot(forecast.skin)   # 80%, 95% prediction interval

## year for confidence interval to pass 0
forecast.skinLONG <- forecast(skin.arima, h = 1800) # 150 years
plot(forecast.skinLONG)
abline(h = 0)

min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 
min(which(forecast.skinLONG$mean<=0))/12 
min(which(forecast.skinLONG$upper[,"95%"]<=0))/12 
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.CWts[1]))/12 

## starting value
decomp.skin <- decompose(skin.CWts)
decomp.skin$trend[7]
## ending value
decomp.skin$trend[114]
## percentage change
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7]



## WEIGHT -----------
## (1) Body ---------
plot(weight.CWts) # autocorrelations
weight.Cts <- tslm(weight.CWts ~ trend + season)
dwtest(weight.Cts) 
acf.weight <- Acf(weight.CWts)
plot(acf.weight)
acf.weight$acf[2] # 

# Fit the model
weight.arimaBody <- auto.arima(weight.CWts, stepwise = FALSE, approximation = FALSE)
weight.arimaBody

## Check diagnostics of chosen model
tsdiag(weight.arimaBody)
plot(weight.arimaBody$residuals)
Box.test(weight.arimaBody$residuals)
accuracy(weight.arimaBody)

## Check coeffiecients
coeftest(weight.arimaBody)

## ARIMA forecasts
forecast.weightBody <- forecast(weight.arimaBody, h = 48) # 4 years
plot(forecast.weightBody)   # 80%, 95% prediction interval

## year for confidence interval to pass 0 or doubling point
forecast.weightBodyLONG <- forecast(weight.arimaBody, h = 1800) # 200 years
plot(forecast.weightBodyLONG)

min(which(forecast.weightBodyLONG$lower[,"95%"]<=0)) # NA
min(which(forecast.weightBodyLONG$mean<=0)) # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]<=0)) # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.CWts[1])) # NA

# percentage change from the trend
decomp.weight <- decompose(weight.CWts)
decomp.weight$trend
## starting value
decomp.weight$trend[7] # first value
## ending value
decomp.weight$trend[75] # last value
## percentage change
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7]



## (2) Face ---------
plot(weightFace.CWts)
weightFace.Cts <- tslm(weightFace.CWts ~ trend + season)
dwtest(weightFace.Cts) 
acf.weightFace <- Acf(weightFace.CWts)
plot(acf.weightFace)
acf.weightFace$acf[2]

# Fit the model
weight.arimaFace <- auto.arima(weightFace.CWts, stepwise = FALSE, approximation = FALSE)
weight.arimaFace

## Check diagnostics of chosen model
tsdiag(weight.arimaFace)
plot(weight.arimaFace$residuals)
Box.test(weight.arimaFace$residuals)
accuracy(weight.arimaFace)
coeftest(weight.arimaFace)

## ARIMA forecasts
forecast.weightFace <- forecast(weight.arimaFace, h = 98) # up to 2020
plot(forecast.weightFace)   # 80%, 95% prediction interval

## year for confidence interval to pass 0 or doubling point
forecast.weightFaceLONG <- forecast(weight.arimaFace, h = 1800)
plot(forecast.weightFaceLONG)

min(which(forecast.weightFaceLONG$lower[,"95%"]>=2*weightFace.CWts[1]))/12 
min(which(forecast.weightFaceLONG$mean>=2*weightFace.CWts[1]))/12 
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weightFace.CWts[1]))/12 
## note these numbers are from October, 2011.

# percentage change from the trend
decomp.weightFace <- decompose(weightFace.CWts)
decomp.weightFace$trend
## starting value
decomp.weightFace$trend[7] # first value
## ending value
decomp.weightFace$trend[84] # last value
## percentage change
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7]


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
plot(age.CWEts) 
age.CEts <- tslm(age.CWEts ~ trend + season)
age.CEts
dwtest(age.CEts) 
acf.ageE <- Acf(age.CWEts)
plot(acf.ageE)
acf.ageE$acf[2] 

# Fit the model
age.arimaE <- auto.arima(age.CWEts, stepwise = FALSE, approximation = FALSE)
age.arimaE

## check model accuracy and residuals
tsdiag(age.arimaE) 
Box.test(age.arimaE$residuals) 
plot(age.arimaE$residuals) 
accuracy(age.arimaE) 
coeftest(age.arimaE) 

## ARIMA forecasts
forecast.ageE <- forecast(age.arimaE, h = 60) # 5 years
plot(forecast.ageE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.ageLONGE <- forecast(age.arimaE, h = 1800) # 150 years
plot(forecast.ageLONGE)
abline(h = 0)
abline(h = 2*age.CWEts[1])


min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 
min(which(forecast.ageLONGE$mean<=0))/12 
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.CWEts[1]))/12 

## starting value
decomp.ageE <- decompose(age.CWEts)
decomp.ageE$trend[7]
## ending value
decomp.ageE$trend[114]
## percentage change
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7]





## Disability -----------

plot(dis.CWEts)
dis.CEts <- tslm(dis.CWEts ~ trend + season)
dwtest(dis.CEts) 
acf.disE <- Acf(dis.CWEts)
plot(acf.disE)
acf.disE$acf[2] 

# Fit model:
dis.arimaE <- auto.arima(dis.CWEts, stepwise = FALSE, approximation = FALSE)
dis.arimaE

tsdiag(dis.arimaE)
Box.test(dis.arimaE$residuals) 
plot(dis.arimaE$residuals) 
accuracy(dis.arimaE)
coeftest(dis.arimaE)

## ARIMA forecasts
forecast.disE <- forecast(dis.arimaE, h = 60) # 5 years
plot(forecast.disE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.disLONGE <- forecast(dis.arimaE, h = 1800) # 150 years
plot(forecast.disLONGE)
abline(h = 0)
abline(h = 2*dis.CWEts[1])


min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 
min(which(forecast.disLONGE$mean<=0))/12 
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 
min(which(forecast.disLONGE$upper[,"95%"]>=(2*dis.CWEts[1])))/12

## starting value
decomp.disE <- decompose(dis.CWEts)
decomp.disE$trend[7]
## ending value
decomp.disE$trend[114]
## percentdis change
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7]



## Race ---------------
plot(race.CWEts) 
race.CEts <- tslm(race.CWEts ~ trend + season)
dwtest(race.CEts) 
acf.raceE <- Acf(race.CWEts)
plot(acf.raceE)
acf.raceE$acf[2] 

# Fit the model
race.arimaE <- auto.arima(race.CWEts, approximation = FALSE, stepwise = FALSE)
race.arimaE 

## check diagnostics of model
tsdiag(race.arimaE) 
accuracy(race.arimaE)
plot(race.arimaE$residuals)
Box.test(race.arimaE$residuals)
coeftest(race.arimaE) 

## ARIMA forecasts
forecast.raceE <- forecast(race.arimaE, h = 60) # 5 years
plot(forecast.raceE)   # 80%, 95% prediction interval


# month for the confidence interval to pass 0
forecast.raceLONGE <- forecast(race.arimaE, h = 1800) # 150 years
plot(forecast.raceLONGE)
abline(h = 0)
abline(h = 2*race.CWEts[1])


min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12
min(which(forecast.raceLONGE$mean<=0))/12 
min(which(forecast.raceLONGE$upper[,"95%"]<=0))/12 
min(which(forecast.raceLONGE$upper[,"95%"]>=2*race.CWEts[1]))/12 

## starting value
decomp.raceE <- decompose(race.CWEts)
decomp.raceE$trend[7]
## ending value
decomp.raceE$trend[114]
## percentrace change
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7]




## Sexuality ------------
plot(sex.CWEts)
sex.CEts <- tslm(sex.CWEts ~ trend + season)
dwtest(sex.CEts) 
acf.sexE <- Acf(sex.CWEts)
plot(acf.sexE)
acf.sexE$acf[2] 

# Fit the model
sex.arimaE <- auto.arima(sex.CWEts, stepwise = FALSE, approximation = FALSE)
sex.arimaE 

## check the diagnostics
tsdiag(sex.arimaE) 
Box.test(sex.arimaE$residuals)
plot(sex.arimaE$residuals)
accuracy(sex.arimaE)
coeftest(sex.arimaE) 

## ARIMA forecasts
forecast.sexE <- forecast(sex.arimaE, h = 60) # 5 years
plot(forecast.sexE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.sexLONGE <- forecast(sex.arimaE, h = 1800) # 150 years
plot(forecast.sexLONGE)
abline(h = 0)
abline(h = 2*sex.CWEts[1])


min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12
min(which(forecast.sexLONGE$mean<=0))/12 
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.CWEts[1]))/12 

## starting value
decomp.sexE <- decompose(sex.CWEts)
decomp.sexE$trend[7]
## ending value
decomp.sexE$trend[114]
## percentsex change
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7]



## Skin-Tone --------------
plot(skin.CWEts) 
skin.CEts <- tslm(skin.CWEts ~ trend + season)
skin.CEts
dwtest(skin.CEts) 
acf.skinE <- Acf(skin.CWEts)
plot(acf.skinE)
acf.skinE$acf[2] 

# Fit the model
skin.arimaE <- auto.arima(skin.CWEts, stepwise = FALSE, approximation = FALSE)
skin.arimaE 

# check diagnostics
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
abline(h = 2*skin.CWEts[1])

min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 
min(which(forecast.skinLONGE$mean<=0))/12 
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.CWEts[1]))/12

## starting value
decomp.skinE <- decompose(skin.CWEts)
decomp.skinE$trend[7]
## ending value
decomp.skinE$trend[114]
## percentskin change
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7]




## Weight -----------
plot(weight.CWEts) 
weight.CETS <- tslm(weight.CWEts ~ trend + season)
dwtest(weight.CETS)
acf.weightE <- Acf(weight.CWEts)
plot(acf.weightE)
acf.weightE$acf[2] 


# Fit the model
weight.arimaE <- auto.arima(weight.CWEts, stepwise = FALSE, approximation = FALSE)
weight.arimaE

## Check accuracy and residuals
tsdiag(weight.arimaE)
plot(weight.arimaE$residuals)
Box.test(weight.arimaE$residuals)
accuracy(weight.arimaE)
coeftest(weight.arimaE)

## ARIMA forecasts
forecast.weightE <- forecast(weight.arimaE, h = 60) # 5 years
plot(forecast.weightE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.weightLONGE <- forecast(weight.arimaE, h = 1800) # 150 years
plot(forecast.weightLONGE)
abline(h = 0)
abline(h = 2*weight.CWEts[1])


min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12
min(which(forecast.weightLONGE$mean<=0))/12
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.CWEts[1]))/12 


## starting value
decomp.weightE <- decompose(weight.CWEts)
decomp.weightE$trend[7]
## ending value
decomp.weightE$trend[114]
## percentage change
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7]


##############################################################################################################################
##############################################################################################################################

## SUMMARIES -----------------
## (1) Model parameters ---------
forecast.age$model # Age Implicit (0,1,1)(1,0,0)
forecast.ageE$model # Age Explicit (0,1,1)(2,0,0)
forecast.dis$model # Disability Implicit (1,0,0) 
forecast.disE$model # Disability Explicit (0,1,1)(1,0,0)
forecast.race$model # Race Implicit (0,1,5)
forecast.raceE$model # Race Explicit (0,1,2)(1,0,0)
forecast.sex$model # Sexuality Implicit (0,1,1)(2,0,0)
forecast.sexE$model # Sexuality Explicit (1,1,2)(2,0,0)
forecast.skin$model # Skin-Tone Implicit (0,1,1)(0,0,1)
forecast.skinE$model # Skin-Tone Explicit (0,1,1)(1,0,1)
forecast.weightBody$model # Weight Implicit (Body) (1,0,0)
forecast.weightFace$model # Weight Implicit (Face) (0,1,1)(1,0,0)
forecast.weightE$model # Weight Explicit (0,1,1)(2,0,0)

## (2) Forecasts to neutrality ----------
## EXPLICIT --------
## AGE
min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 # 7.41667 (7 yrs, 5 mos)
min(which(forecast.ageLONGE$mean<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.CWEts[1]))/12 # 16 yrs

## DISABILITY
min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 #39.667 (39 yrs, 8 mos)
min(which(forecast.disLONGE$mean<=0))/12 # NA
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disLONGE$upper[,"95%"]>=2*dis.CWEts[1]))/12 # NA

## RACE
min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12 # 41.25 (41 yrs, 3 mos)
min(which(forecast.raceLONGE$mean<=0))/12 # NA
min(which(forecast.raceLONGE$upper[,"95%"]>=2*race.CWEts[1]))/12 # 109.33 (109 yrs, 4 mos)

## SEXUALITY
min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12 # 5.41667 (5 yrs, 5 mos)
min(which(forecast.sexLONGE$mean<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.CWEts[1]))/12 # 9.1667 (9 yrs, 2 mos)

## SKIN
min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 # 5.8333 (5 yrs, 10 mos)
min(which(forecast.skinLONGE$mean<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.CWEts[1]))/12 # 36.667, (36 yrs, 8 mos)

## BODY WEIGHT
min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12 # 8.5 (8 yrs, 6 mos)
min(which(forecast.weightLONGE$mean<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.CWEts[1]))/12 # 28

## IMPLICIT --------
## AGE
min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$mean<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]>=2*age.CWts[1]))/12 # NA

## DISABILITY
min(which(forecast.disLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$mean<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]>=2*dis.CWts[1]))/12 # NA

## RACE
min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONG$mean<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.CWts[1]))/12 # NA

## SEXUALITY
min(which(forecast.sexLONG$lower[,"95%"]<=0))/12 # 10
min(which(forecast.sexLONG$mean<=0))/12 # NA
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONG$upper[,"95%"]>=2*sex.CWts[1]))/12 # 40 yrs, 7 mos

## SKIN
min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONG$mean<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.CWts[1]))/12 # NA

## BODY WEIGHT (BODY)
min(which(forecast.weightBodyLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$mean<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.CWts[1]))/12 #NA

## BODY WEIGHT (FACE) ** from Oct 2011
min(which(forecast.weightFaceLONG$lower[,"95%"]<=0))/12 # 17.5833 (17 yrs, 7 mos)
min(which(forecast.weightFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weightFace.CWts[1]))/12 # 3.333 (3 yrs, 4 mos)

## (3) Percentage changes from trend values ----------
## EXPLICIT -------
## AGE
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7] # 29.15% decrease
## DIS
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7] # 23.03% decrease
## RACE
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7] # 24.46% decrease
## SEX
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7] # 40.59% decrease
## SKIN
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7] # 16.42% decrease
## WEIGHT
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7] # 19.23% decrease

## IMPLICIT -----
## AGE
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7] # 1.29% decrease
## DIS
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7] # 4.61% increase
## RACE
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7] # 12.27% decrease
## SEX
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7] # 33.60% decrease
## SKIN
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7] # 9.36% decrease
## WEIGHT
# Body
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7] # 6.31% increase
# Face
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7] # 44.31% increase

## (4) First and Last raw values ----------
## EXPLICIT -------
## AGE
age.CWEts[1]; age.CWEts[120] # 0.64, 0.48
## DIS
dis.CWEts[1]; dis.CWEts[120] # 0.80, 0.56
## RACE
race.CWEts[1]; race.CWEts[120] # 0.36, 0.23
## SEX
sex.CWEts[1]; sex.CWEts[120] # 1.05, 0.70
## SKIN
skin.CWEts[1]; skin.CWEts[120] # 0.44, 0.25
## WEIGHT
weight.CWEts[1]; weight.CWEts[120] # 0.92, 0.78

## IMPLICIT -----
## AGE
age.CWts[1]; age.CWts[120] # 0.42, 0.41
## DIS
dis.CWts[1]; dis.CWts[120] # 0.51, 0.56
## RACE
race.CWts[1]; race.CWts[120] # 0.34, 0.33
## SEX
sex.CWts[1]; sex.CWts[120] # 0.39, 0.24
## SKIN
skin.CWts[1]; skin.CWts[120] # 0.34, 0.32
## WEIGHT
# Body
weight.CWts[1]; weight.CWts[81] # 0.51, 0.52
# Face
weightFace.CWts[1]; weightFace.CWts[89] # 0.29, 0.46

# (2) Plots ---------
## EXPLICIT --------
## plotting all weighted explicit together
plot(forecast.weightE$fitted, xlim = c(2007, 2022),
     ylim = c(0, 1.3), main = "Changes in Explicit Attitudes, 2007 - 2022",
     ylab = "Explicit preference score", xlab = "Year", col = "darkorange", lwd = 2)
lines(forecast.weightE$mean, col = "darkorange", lwd = 2, lty = 3)
lines(forecast.sexE$fitted, col = "darkred", lwd = 2)
lines(forecast.sexE$mean, col = "darkred", lwd = 2, lty = 3)
lines(forecast.raceE$fitted, col = "darkblue", lwd = 2)
lines(forecast.raceE$mean, col = "darkblue", lwd = 2, lty = 3)
lines(forecast.disE$fitted, col = "purple", lwd = 2)
lines(forecast.disE$mean, col = "purple", lwd = 2, lty = 3)
lines(forecast.skinE$fitted, col = "lightblue", lwd = 2)
lines(forecast.skinE$mean, col = "lightblue", lwd = 2, lty = 3)
lines(forecast.ageE$fitted, col = "darkgreen", lwd = 2)
lines(forecast.ageE$mean, col = "darkgreen", lwd = 2, lty = 3)
legend("topright", c("Disability", "Weight", "Age","Skin-Tone", "Race", "Sexuality"),
       col = c("purple", "darkorange", "darkgreen", "lightblue", "darkblue", "darkred"),
       lty = 1, bty = "n", cex = 0.8)


## plotting all weighted explicit forecasts with confidence intervals and model parameters, together
op <- par(mfrow = c(2,3))
plot(forecast.ageE, main = "Explicit Age Attitudes", ylab = "Explicit preference score", ylim = c(0,1))
abline(h = 0, lty = 2)
plot(forecast.disE, main = "Explicit Disability Attitudes", ylab = "Explicit preference score", ylim = c(0,1))
abline(h = 0, lty = 2)
plot(forecast.weightE, main = "Explicit Weight Attitudes", ylab = "Explicit preference score", ylim = c(0,1.2),
     xlim = c(2007, 2021))
abline(h = 0, lty = 2)
plot(forecast.raceE, main = "Explicit Race Attitudes", ylab = "Explicit preference score", ylim = c(0,1)) 
abline(h = 0, lty = 2)
plot(forecast.skinE, main = "Explicit Skin-Tone Attitudes", ylab = "Explicit preference score", ylim = c(0,1)) 
abline(h = 0, lty = 2)
plot(forecast.sexE, main = "Explicit Sexuality Attitudes", ylab = "Explicit preference score", ylim = c(0,1.2))
abline(h = 0, lty = 2)
par(op)


## plotting all weighted EXPLICIT forecasts with confidence intervals and no model parameters, together
# with decomposed trend lines as well
op <- par(mfrow = c(2,3))
plot(forecast.ageE, main = "Explicit Age Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.ageE$trend, col = "black", lwd = 3)
plot(forecast.disE, main = "Explicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.disE$trend, col = "black", lwd = 3)
plot(forecast.weightE, main = "Explicit Weight Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.weightE$trend, col = "black", lwd = 3)
plot(forecast.raceE, main = "Explicit Race Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.raceE$trend, col = "black", lwd = 3)
plot(forecast.skinE, main = "Explicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.skinE$trend, col = "black", lwd = 3)
plot(forecast.sexE, main = "Explicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0, 1.2), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.sexE$trend, col = "black", lwd = 3)
par(op)



## IMPLICIT ----------
## plotting all weighted implicit forecasts with confidence intervals and model parameters, together
op <- par(mfrow = c(2,3))
plot(forecast.age, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7))
abline(h = 0, lty = 2)
plot(forecast.dis, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7))
abline(h = 0, lty = 2)
plot(forecast.weightBody, main = "Implicit Weight Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7), xlim = c(2007, 2021))
abline(h = 0, lty = 2)
plot(forecast.race, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7)) 
abline(h = 0, lty = 2)
plot(forecast.skin, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7)) 
abline(h = 0, lty = 2)
plot(forecast.sex, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7))
abline(h = 0, lty = 2)
par(op)



## plotting all weighted implicit forecasts with confidence intervals and no model parameters, together
# with decomposed trend lines as well
op <- par(mfrow = c(2,3))
plot(forecast.age, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.age$trend, col = "black", lwd = 3)
plot(forecast.dis, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.dis$trend, col = "black", lwd = 3)
plot(forecast.weightBody, main = "Implicit Weight Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.weight$trend, col = "black", lwd = 3)
plot(forecast.race, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.race$trend, col = "black", lwd = 3)
plot(forecast.skin, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.skin$trend, col = "black", lwd = 3)
plot(forecast.sex, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.7), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.sex$trend, col = "black", lwd = 3)
par(op)




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
(acf.age$acf[2]+
   acf.ageE$acf[2]+
   acf.dis$acf[2]+
   acf.disE$acf[2]+
   acf.race$acf[2]+
   acf.raceE$acf[2]+
   acf.sex$acf[2]+
   acf.sexE$acf[2]+
   acf.skin$acf[2]+
   acf.skinE$acf[2]+
   acf.weight$acf[2]+
   acf.weightE$acf[2])/12

median(c(acf.age$acf[2],
        acf.ageE$acf[2],
        acf.dis$acf[2],
        acf.disE$acf[2],
        acf.race$acf[2],
        acf.raceE$acf[2],
        acf.sex$acf[2],
        acf.sexE$acf[2],
        acf.skin$acf[2],
        acf.skinE$acf[2],
        acf.weight$acf[2],
        acf.weightE$acf[2]))

range(c(acf.age$acf[2],
        acf.ageE$acf[2],
        acf.dis$acf[2],
        acf.disE$acf[2],
        acf.race$acf[2],
        acf.raceE$acf[2],
        acf.sex$acf[2],
        acf.sexE$acf[2],
        acf.skin$acf[2],
        acf.skinE$acf[2],
        acf.weight$acf[2],
        acf.weightE$acf[2]))
