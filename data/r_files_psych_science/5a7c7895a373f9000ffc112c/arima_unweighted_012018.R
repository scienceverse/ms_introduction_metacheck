## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 20th, 2018
####################################################################################################################################

## This file performs ARIMA time series analysis on the UNWEIGHTED time series vectors from 2007 onward (to 2016).

## Also provides supplemental diagnostics of the time series trajectories

## Data used is the TSvectors.RData, which includes the vectors (both weighted and unweighted), from 2007 onward
# with seasonal decomposition and linear interpolation (for 2 months in skin-tone and 7 months in weightBody)

####################################################################################################################################

# Install packages
install.packages("forecast", dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("tseries", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("strucchange", dependencies = TRUE)

# Load packages
require(forecast) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
require(tseries)
require(lmtest)
require(strucchange)

load("TSvectors_2016data_011918.RData")


####################################################################################################################################

## IMPLICIT ATTITUDES -------------------------------------------------------------------------------------------------------------

## AGE -------------------------------------------------------------------------------------------------------------------------
## visualize and simple model
plot(age.ts)
age.arima <- auto.arima(age.ts, stepwise = FALSE, approximation = FALSE)
age.arima # parameters (2,1,1)(2,0,0)

# Check diagnostics and accuracy of final model
tsdiag(age.arima) 
plot(age.arima$residuals)
Box.test(age.arima$residuals) 
accuracy(age.arima)

# Check coefficients
coeftest(age.arima)

## ARIMA forecasts
forecast.age <- forecast(age.arima, h = 60) # 5 years
plot(forecast.age)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.ageLONG <- forecast(age.arima, h = 1800) # 150 years
plot(forecast.ageLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$mean<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]<=0))/12 # NA

## percentage change from trends
decomp.age <- decompose(age.ts)
decomp.age$trend
## starting value
decomp.age$trend[7] # 0.45
## ending value
decomp.age$trend[114] # 0.43
## percentage change
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7] # 4.47%


## DISABILITY -----------
plot(dis.ts) 
dis.arima <- auto.arima(dis.ts, stepwise = FALSE, approximation = FALSE)
dis.arima # parameters (0,1,2)

# Check model accuracy and diagnostics of our final model
tsdiag(dis.arima)
Box.test(dis.arima$residuals)
plot(dis.arima$residuals)
accuracy(dis.arima)

# Check coefficients
coeftest(dis.arima)

## ARIMA forecasts
forecast.dis <- forecast(dis.arima, h = 60) # 2 years
plot(forecast.dis)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.disLONG <- forecast(dis.arima, h = 1800) # 150 years
plot(forecast.disLONG, ylim = c(0, 1))   # 80%, 95% prediction interval
abline(h = 0)
abline(h = 2*dis.ts[1])

min(which(forecast.disLONG$lower[,"95%"]<=0))/12 #  NA
min(which(forecast.disLONG$mean<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]<=0))/12 # NA

## starting value
decomp.dis <- decompose(dis.ts)
decomp.dis$trend[7] # 0.50
## ending value
decomp.dis$trend[114] # 0.49
## percentage change
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7] # 2.16%




## RACE ---------------
plot(race.ts)
race.arima <- auto.arima(race.ts, stepwise = FALSE, approximation = FALSE)
race.arima # parameters (1,1,1)(2,0,0)

fitted(race.arima)
plot(race.ts)
lines(fitted(race.arima), col = "red")

# Check diagnostics and accuracy
tsdiag(race.arima)
plot(race.arima$residuals)
Box.test(race.arima$residuals)
accuracy(race.arima)

## Test coefficients:
coeftest(race.arima)
# all are significant

## ARIMA forecasts
forecast.race <- forecast(race.arima, h = 60) # 2 years
plot(forecast.race)   # 80%, 95% prediction interval

## Year for confidence intervals and mean to reach neutrality requires MUCH longer estimation period
forecast.raceLONG <- forecast(race.arima, h = 1800) # 150 years
plot(forecast.raceLONG)   # 80%, 95% prediction interval
abline(h = 0)

min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 # 56.667
min(which(forecast.raceLONG$mean<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.ts[1]))/12 # 112.1667


## starting value
decomp.race <- decompose(race.ts)
decomp.race$trend[7] # 0.33
## ending value
decomp.race$trend[114] # 0.27
## percentage change
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7] # 16.90%


## SEXUALITY ------------
plot(sex.ts)
sex.arima <- auto.arima(sex.ts, stepwise = FALSE, approximation = FALSE)
sex.arima # (0,1,2)(2,0,0)

# Check the diagnostics of model
tsdiag(sex.arima) 
plot(sex.arima$residuals)
Box.test(sex.arima$residuals)
accuracy(sex.arima)

# Check coefficient significance
coeftest(sex.arima)

## ARIMA forecasts
forecast.sex <- forecast(sex.arima, h = 60) # 5 years
plot(forecast.sex)   # 80%, 95% prediction interval

## year for confidence interval to pass 0
forecast.sexLONG <- forecast(sex.arima, h = 1800) # 150 years
plot(forecast.sexLONG)
abline(h = 0)
abline(h = 2*sex.ts[1])

min(which(forecast.sexLONG$lower[,"95%"]<=0))/12 # 4
min(which(forecast.sexLONG$mean<=0))/12 # NA
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONG$upper[,"95%"]>=2*sex.ts[1]))/12 # 32.25


## starting value
decomp.sex <- decompose(sex.ts)
decomp.sex$trend[7] # 0.34
## ending value
decomp.sex$trend[114] #0.19
## percentage change
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7] # 43.87%




## SKIN-TONE --------------
plot(skin.ts)
skin.arima <- auto.arima(skin.ts, stepwise = FALSE, approximation = FALSE)
skin.arima # parameters (0,1,1) with drift

# Compare the diagnostics
tsdiag(skin.arima) 
plot(skin.arima$residuals)
Box.test(skin.arima$residuals) 
accuracy(skin.arima) 

## Check the coefficients
coeftest(skin.arima)

## ARIMA forecasts
forecast.skin <- forecast(skin.arima, h = 60) # 5 years
plot(forecast.skin)   # 80%, 95% prediction interval


## year for confidence interval to pass 0
forecast.skinLONG <- forecast(skin.arima, h = 1800) # 150 years
plot(forecast.skinLONG)
abline(h = 0)

min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 # 34.25
min(which(forecast.skinLONG$mean<=0))/12 # 52.25
min(which(forecast.skinLONG$upper[,"95%"]<=0))/12 # 79.0833
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.ts[1]))/12 # NA

## starting value
decomp.skin <- decompose(skin.ts)
decomp.skin$trend[7] # 0.33
## ending value
decomp.skin$trend[114] # 0.28
## percentage change
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7] # 16.38



## WEIGHT -----------
## (1) Body ---------
plot(weight.ts) 
weight.arimaBody <- auto.arima(weight.ts, stepwise = FALSE, approximation = FALSE)
weight.arimaBody # parameters (1,0,0)(2,0,0)

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
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.ts[1])) # NA

# percentage change from the trend
decomp.weight <- decompose(weight.ts)
decomp.weight$trend
## starting value
decomp.weight$trend[7] # first value = 0.47
## ending value
decomp.weight$trend[75] # last value = 0.49
## percentage change
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7] # 3.70%



## (2) Face ---------
plot(weight.tsFace) 
weight.arimaFace <- auto.arima(weight.tsFace, stepwise = FALSE, approximation = FALSE)
weight.arimaFace # parameters (0,1,1)(1,0,0)

## Check diagnostics of chosen model
tsdiag(weight.arimaFace)
plot(weight.arimaFace$residuals)
Box.test(weight.arimaFace$residuals)
accuracy(weight.arimaFace)

## Check coeffiecients
coeftest(weight.arimaFace)

## ARIMA forecasts
forecast.weightFace <- forecast(weight.arimaFace, h = 98) # up to 2020
plot(forecast.weightFace)   # 80%, 95% prediction interval

## year for confidence interval to pass 0 or doubling point
forecast.weightFaceLONG <- forecast(weight.arimaFace, h = 1800) # 200 years
plot(forecast.weightFaceLONG)

min(which(forecast.weightFaceLONG$lower[,"95%"]<=0))/12 # 35.83
min(which(forecast.weightFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weight.tsFace[1]))/12 # 8.583
## note these numbers are from October, 2011.

# percentage change from the trend
decomp.weightFace <- decompose(weight.tsFace)
decomp.weightFace$trend
## starting value
decomp.weightFace$trend[7] # first value = 0.31
## ending value
decomp.weightFace$trend[84] # last value = 0.44
## percentage change
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7] # 42.01%


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
## Age ------------------------------------------------------------------------------------------------------------------------
plot(age.Ets) 
age.arimaE <- auto.arima(age.Ets, stepwise = FALSE, approximation = FALSE)
age.arimaE # parameters (0,1,2)(2,0,0)

## check model accuracy and residuals
tsdiag(age.arimaE) 
Box.test(age.arimaE$residuals) 
plot(age.arimaE$residuals) 
accuracy(age.arimaE) 


## ARIMA forecasts
forecast.ageE <- forecast(age.arimaE, h = 60) # 5 years
plot(forecast.ageE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.ageLONGE <- forecast(age.arimaE, h = 1800) # 150 years
plot(forecast.ageLONGE)
abline(h = 0)
abline(h = 2*age.Ets[1])


min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 # 5.25
min(which(forecast.ageLONGE$mean<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.Ets[1]))/12 # 11.667

## starting value
decomp.ageE <- decompose(age.Ets)
decomp.ageE$trend[7]
## ending value
decomp.ageE$trend[114]
## percentage change
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7]





## Disability -----------

plot(dis.Ets) 
dis.arimaE <- auto.arima(dis.Ets, stepwise = FALSE, approximation = FALSE)
dis.arimaE # parameters (0,1,1)(1,0,0) with drift

tsdiag(dis.arimaE)
Box.test(dis.arimaE$residuals) # both not significant
plot(dis.arimaE$residuals) # a little bit more like white noise
accuracy(dis.arimaE)
coeftest(dis.arimaE)

## ARIMA forecasts
forecast.disE <- forecast(dis.arimaE, h = 60) # 5 years
plot(forecast.disE)   # 80%, 95% prediction interval


# month for the confidence interval to pass 0
forecast.disLONGE <- forecast(dis.arimaE, h = 1800) # 150 years
plot(forecast.disLONGE)
abline(h = 0)
abline(h = 2*dis.Ets[1])


min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 # 18.5
min(which(forecast.disLONGE$mean<=0))/12 # 26.333
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 # 36.0833
min(which(forecast.disLONGE$upper[,"95%"]>=(2*dis.Ets[1])))/12 # NA

## starting value
decomp.disE <- decompose(dis.Ets)
decomp.disE$trend[7]
## ending value
decomp.disE$trend[114]
## percentage change
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7]



## Race ---------------
plot(race.Ets) 
race.arimaE <- auto.arima(race.Ets, approximation = FALSE, stepwise = FALSE)
race.arimaE # (1,1,2)(1,0,1)

plot(fitted(race.arimaE), col = "red")
lines(race.Ets)

## check diagnostics of model
tsdiag(race.arimaE) 
accuracy(race.arimaE)
plot(race.arimaE$residuals)
Box.test(race.arimaE$residuals)

## check the coefficients
coeftest(race.arimaE) # all are significant

## ARIMA forecasts
forecast.raceE <- forecast(race.arimaE, h = 60) # 5 years
plot(forecast.raceE)   # 80%, 95% prediction interval


# month for the confidence interval to pass 0
forecast.raceLONGE <- forecast(race.arimaE, h = 1800) # 150 years
plot(forecast.raceLONGE)
abline(h = 0)
abline(h = 2*race.Ets[1])


min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12 # 1.667
min(which(forecast.raceLONGE$mean<=0))/12 # NA
min(which(forecast.raceLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONGE$upper[,"95%"]>=2*race.Ets[1]))/12 # 16.667

## starting value
decomp.raceE <- decompose(race.Ets)
decomp.raceE$trend[7]
## ending value
decomp.raceE$trend[114]
## percentrace change
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7]





## Sexuality ------------
plot(sex.Ets) 
sex.arimaE <- auto.arima(sex.Ets, stepwise = FALSE, approximation = FALSE)
sex.arimaE # parameters (0,1,2)(2,0,0)[12] 

## check the diagnostics
tsdiag(sex.arimaE) # passes all necessary tests
Box.test(sex.arimaE$residuals)
plot(sex.arimaE$residuals)
accuracy(sex.arimaE)

# Check coefficient significance
coeftest(sex.arimaE) # all parameters are significant

## ARIMA forecasts
forecast.sexE <- forecast(sex.arimaE, h = 60) # 5 years
plot(forecast.sexE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.sexLONGE <- forecast(sex.arimaE, h = 1800) # 150 years
plot(forecast.sexLONGE)
abline(h = 0)
abline(h = 2*sex.Ets[1])


min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12 # 1.5833
min(which(forecast.sexLONGE$mean<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.Ets[1]))/12 # 16.667

## starting value
decomp.sexE <- decompose(sex.Ets)
decomp.sexE$trend[7]
## ending value
decomp.sexE$trend[114]
## percentsex change
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7]



## Skin-Tone --------------

plot(skin.Ets) 
skin.arimaE <- auto.arima(skin.Ets, stepwise = FALSE, approximation = FALSE)
skin.arimaE # parameters (2,0,0)(2,0,0)[12] 

# check diagnostics
tsdiag(skin.arimaE)
plot(skin.arimaE$residuals)
Box.test(skin.arimaE$residuals)
accuracy(skin.arimaE)

# check coefficients
coeftest(skin.arimaE)

## ARIMA forecasts
forecast.skinE <- forecast(skin.arimaE, h = 60) # 5 years
plot(forecast.skinE)   # 80%, 95% prediction interval

forecast.skinLONGE <- forecast(skin.arimaE, h = 1800) # 150 years
plot(forecast.skinLONGE)
abline(h = 0)
abline(h = 2*skin.Ets[1])

min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$mean<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.Ets[1]))/12 # NA

## starting value
decomp.skinE <- decompose(skin.Ets)
decomp.skinE$trend[7]
## ending value
decomp.skinE$trend[114]
## percentskin change
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7]




## Weight -----------
plot(weight.Ets) 
weight.arimaE <- auto.arima(weight.Ets, stepwise = FALSE, approximation = FALSE)
weight.arimaE # parameters (1,1,1)(0,0,2)

## Check accuracy and residuals
tsdiag(weight.arimaE)
plot(weight.arimaE$residuals)
Box.test(weight.arimaE$residuals)
accuracy(weight.arimaE)

# Check coefficients
coeftest(weight.arimaE)

## ARIMA forecasts
forecast.weightE <- forecast(weight.arimaE, h = 60) # 5 years
plot(forecast.weightE)   # 80%, 95% prediction interval

# month for the confidence interval to pass 0
forecast.weightLONGE <- forecast(weight.arimaE, h = 1800) # 150 years
plot(forecast.weightLONGE)
abline(h = 0)
abline(h = 2*weight.Ets[1])


min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12 # 50.0833
min(which(forecast.weightLONGE$mean<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.Ets[1]))/12 # 72.333


## starting value
decomp.weightE <- decompose(weight.Ets)
decomp.weightE$trend[7]
## ending value
decomp.weightE$trend[114]
## percentage change
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7]


##############################################################################################################################
##############################################################################################################################


## Plots: Weighted and unweighted together -----
## Explicit
op <- par(mfrow = c(2,3))
plot(age.Ets, main = "Explicit Age Attitudes", ylab = "Explicit Preference", ylim = c(0.15, 0.7), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(age.WEts, col = "cadetblue", lwd = 3)
plot(dis.Ets, main = "Explicit Disability Attitudes", ylab = "Explicit Preference", ylim = c(0.35, 0.65), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(dis.WEts, col = "cadetblue", lwd = 3)
plot(weight.Ets, main = "Explicit Weight Attitudes", ylab = "Explicit Preference", ylim = c(0.75, 1.1), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(weight.WEts, col = "cadetblue", lwd = 3)
plot(race.Ets, main = "Explicit Race Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 0.45),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(race.WEts, col = "cadetblue", lwd = 3)
plot(skin.Ets, main = "Explicit Skin-Tone Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 0.45),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(skin.WEts, col = "cadetblue", lwd = 3)
plot(sex.Ets, main = "Explicit Sexuality Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 0.8),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(sex.WEts, col = "cadetblue", lwd = 3)
par(op)


## Implicit
op <- par(mfrow = c(2,3))
plot(age.ts, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.4, 0.48), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(age.Wts, col = "cadetblue", lwd = 3)
plot(dis.ts, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.44, 0.55), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(dis.Wts, col = "cadetblue", lwd = 3)
plot(weight.tsFace, main = "Implicit Weight Attitudes", ylab = "IAT D-score", ylim = c(0.28, 0.55), cex.main = 1.5,
     col = "coral", lwd = 3, xlim = c(2004, 2017), lty = 3)
lines(weight.ts, col = "coral", lwd = 3)
lines(weight.Wts, col = "cadetblue", lwd = 3)
lines(weight.WtsFace, col = "cadetblue", lwd = 3, lty = 3)
plot(race.ts, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.23, 0.37),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(race.Wts, col = "cadetblue", lwd = 3)
plot(skin.ts, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.23, 0.37),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(skin.Wts, col = "cadetblue", lwd = 3)
plot(sex.ts, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.12, 0.4),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(sex.Wts, col = "cadetblue", lwd = 3)
par(op)




