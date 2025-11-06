## Charlesworth & Banaji. Patterns of Change I.
## Script version date: April 12th, 2018

####################################################################################################################################

## Redoes imputation and timeseries preparation for full data samples...

####################################################################################################################################

## Load data
load("fullsample_timeseries_043018.RData")

## Install packages
install.packages("imputeTS", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("lsr", dependencies = TRUE)

## Load packages
require(imputeTS)
require(plyr)
require(lsr)


####################################################################################################################################
## IMPLICIT ATTITUDES  ------------------------
age.month <- ddply(timeseriesAge2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
dis.month <- ddply(timeseriesDis2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
race.month <- ddply(timeseriesRace2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
sex.month <- ddply(timeseriesSex2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
skin.month <- ddply(timeseriesSkin2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
weight.month.Body <- ddply(timeseriesWeightBody2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
weight.month.Face <- ddply(timeseriesWeightFace2, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))


## Age --------------
table(timeseriesAge2$month.cent)
age.ts <- ts(age.month$mean, frequency = 12, start = c(2007, 1))
age.ts
plot(age.ts)

age.Wts <- ts(age.month$weight.mean, frequency = 12, start = c(2007, 1))
age.Wts
plot(age.Wts)
lines(age.ts, col = "cadetblue")

## Disability -----------
table(timeseriesDis2$month.cent)
dis.ts <- ts(dis.month$mean, frequency = 12, start = c(2007, 1))
dis.ts
plot(dis.ts)

dis.Wts <- ts(dis.month$weight.mean, frequency = 12, start = c(2007, 1))
dis.Wts
plot(dis.Wts)
lines(dis.ts, col = "cadetblue")

## Race ------------
table(timeseriesRace2$month.cent)
race.ts <- ts(race.month$mean, frequency = 12, start = c(2007, 1))
race.ts
plot(race.ts)

race.Wts <- ts(race.month$weight.mean, frequency = 12, start = c(2007, 1))
race.Wts
plot(race.Wts)
lines(race.ts, col = "cadetblue")


## Sexuality  ----------
table(timeseriesSex2$month.cent)
sex.ts <- ts(sex.month$mean, frequency = 12, start = c(2007, 1))
sex.ts
plot(sex.ts)

sex.Wts <- ts(sex.month$weight.mean, frequency = 12, start = c(2007, 1))
sex.Wts
plot(sex.Wts)
lines(sex.ts, col = "cadetblue")


## Skin Tone ------------
table(timeseriesSkin2$month.cent) 

# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$d <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "d")
skin.month <- rbind(skin.month.extra, skin.month)
skin.month <- skin.month[order(skin.month$month.cent),]
skin.month

skin.ts <- ts(skin.month$mean, frequency = 12, start = c(2007, 1))
skin.ts
# impute with seasonal decomposition and interpolation
skin.ts <- na.seadec(skin.ts, algorithm = "interpolation")
skin.ts
plot(skin.ts)

skin.Wts <- ts(skin.month$weight.mean, frequency = 12, start = c(2007, 1))
skin.Wts
plot(skin.Wts)
# go with seasonal decomposition and interpolation
skin.Wts <- na.seadec(skin.Wts, algorithm = "interpolation")
skin.Wts
plot(skin.Wts)
lines(skin.ts, col = "cadetblue")

## Weight ---------------
# (1) Body ------------
# Missing data for months 105:107 and 132, with low data for 125 - 131
table(timeseriesWeightBody2$month.cent)
table(timeseriesWeightBody2$month)

weight.month.Body
# add in NAs for month values 125:131
weight.month.Body$month.cent[47:53] # gives us months 125 - 131
weight.month.Body$mean[47:53] <- NA
weight.month.Body$weight.mean[47:53] <- NA
weight.month.Body$d[47:53] <- NA
weight.month.Body

# add add in NAs for missing months
weight.month.Body.extra <- as.data.frame(c(105:107, 132))
weight.month.Body.extra$mean <- rep(NA, 4)
weight.month.Body.extra$weight.mean <- rep(NA, 4)
weight.month.Body.extra$d <- rep(NA, 4)
names(weight.month.Body.extra) <- c( "month.cent", "mean", "weight.mean", "d")
weight.month.Body <- rbind(weight.month.Body.extra, weight.month.Body)
weight.month.Body <- weight.month.Body[order(weight.month.Body$month.cent),]
weight.month.Body

weight.ts <- ts(weight.month.Body$mean, frequency = 12, start = c(2010, 4))
weight.ts

# impute with seasonal decomposition and interpolation
weight.ts <- na.seadec(weight.ts, algorithm = "interpolation")
weight.ts
plot(weight.ts)

weight.Wts <- ts(weight.month.Body$weight.mean, frequency = 12, start = c(2010, 4))
weight.Wts
plot(weight.Wts)

# go with seasonal decomposition and interpolation
weight.Wts <- na.seadec(weight.Wts, algorithm = "interpolation")
weight.Wts
plot(weight.Wts)
lines(weight.ts, col = "cadetblue")

# (2) Face -------------
table(timeseriesWeightFace2$month)
## complete data for this test.

weight.tsFace <- ts(weight.month.Face$mean, frequency = 12, start = c(2004, 5))
weight.tsFace
plot(weight.tsFace)

weight.WtsFace <- ts(weight.month.Face$weight.mean, frequency = 12, start = c(2004, 5))
weight.WtsFace
plot(weight.WtsFace)
lines(weight.tsFace, col = "cadetblue")

## Plotting two tests together
plot(weight.WtsFace, xlim = c(2004, 2017), ylim = c(0.27, 0.55), ylab = "IAT D-score", lwd = 2, main = "Implicit Body Weight Attitudes")
lines(weight.Wts, col = "coral", lwd = 2)
legend("bottomright", col = c("black", "coral"), c("Face Stimuli", "Body Stimuli"), lty = 1, lwd = 2, bty = "n")


####################################################################################################################################
## EXPLICIT ATTITUDES ------------------------------------------------------------------------------------------------------------
age.month.e <- ddply(timeseriesAge2, .(month.cent), summarize, mean = mean(att.center1, na.rm = TRUE), weight.mean = weighted.mean(att.center1, weightvec, na.rm = TRUE), d = cohensD(att.center1))
dis.month.e <- ddply(timeseriesDis2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
race.month.e <- ddply(timeseriesRace2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
skin.month.e <- ddply(timeseriesSkin2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
sex.month.e <- ddply(timeseriesSex2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
weight.month.e <- ddply(timeseriesWeightBody2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
weight.month.eFace <- ddply(timeseriesWeightFace2, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))


## Age ------
age.Ets <- ts(age.month.e$mean, frequency = 12, start = c(2007, 1))
age.Ets
plot(age.Ets)

age.WEts <- ts(age.month.e$weight.mean, frequency = 12, start = c(2007, 1))
age.WEts
plot(age.WEts)
lines(age.Ets, col = "cadetblue")

## Disability ------
dis.Ets <- ts(dis.month.e$mean, frequency = 12, start = c(2007, 1))
dis.Ets
plot(dis.Ets)

dis.WEts <- ts(dis.month.e$weight.mean, frequency = 12, start = c(2007, 1))
dis.WEts
plot(dis.WEts)
lines(dis.Ets, col = "cadetblue")

## Race -------
race.Ets <- ts(race.month.e$mean, frequency = 12, start = c(2007, 1))
race.Ets
plot(race.Ets)

race.WEts <- ts(race.month.e$weight.mean, frequency = 12, start = c(2007, 1))
race.WEts
plot(race.WEts)
lines(race.Ets, col = "cadetblue")

## Sexuality -------
sex.Ets <- ts(sex.month.e$mean, frequency = 12, start = c(2007, 1))
sex.Ets
plot(sex.Ets)

sex.WEts <- ts(sex.month.e$weight.mean, frequency = 12, start = c(2007, 1))
sex.WEts
plot(sex.WEts)
lines(sex.Ets, col = "cadetblue")

## Skin Tone -----
# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$d <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "d")
skin.month.e <- rbind(skin.month.extra, skin.month.e)
skin.month.e <- skin.month.e[order(skin.month.e$month.cent),]
skin.month.e

skin.Ets <- ts(skin.month.e$mean, frequency = 12, start = c(2007, 1))
skin.Ets
plot(skin.Ets)
# go with seasonal decomposition and interpolation
skin.Ets <- na.seadec(skin.Ets, algorithm = "interpolation")
skin.Ets
plot(skin.Ets)

skin.WEts <- ts(skin.month.e$weight.mean, frequency = 12, start = c(2007, 1))
skin.WEts
plot(skin.WEts)
# go with seasonal decomposition and interpolation
skin.WEts <- na.seadec(skin.WEts, algorithm = "interpolation")
skin.WEts
plot(skin.WEts)
lines(skin.Ets, col = "cadetblue")

## Weight ---------
## Compile data across tests
# Missing data for months 105:107 and 132, with low data for 125 - 131
weight.month.e
# add in NAs for month values 125:131
weight.month.e$month.cent[47:53] # gives us months 125 - 131
weight.month.e$mean[47:53] <- NA
weight.month.e$weight.mean[47:53] <- NA
weight.month.e$d[47:53] <- NA
weight.month.e

# add add in NAs for missing months
weight.month.e.extra <- as.data.frame(c(105:107, 132))
weight.month.e.extra$mean <- rep(NA, 4)
weight.month.e.extra$weight.mean <- rep(NA, 4)
weight.month.e.extra$d <- rep(NA, 4)
names(weight.month.e.extra) <- c( "month.cent", "mean", "weight.mean", "d")
weight.month.e <- rbind(weight.month.e.extra, weight.month.e)
weight.month.e <- weight.month.e[order(weight.month.e$month.cent),]
weight.month.e

## Add on 2007 (month 37) through to 1003 (month 75)
weight.month.eFace <- weight.month.eFace[weight.month.eFace$month.cent >= 37 & weight.month.eFace$month.cent <= 75,]

weight.month.ecomb <- rbind(weight.month.eFace, weight.month.e)

## unweighted
weight.Ets <- ts(weight.month.ecomb$mean, frequency = 12, start = c(2007, 1))
weight.Ets
plot(weight.Ets)

#  use seasonal decomposition and interpolation
weight.Ets <- na.seadec(weight.Ets, algorithm = "interpolation")
weight.Ets
plot(weight.Ets)

## WEIGHTED
weight.WEts <- ts(weight.month.ecomb$weight.mean, frequency = 12, start = c(2007, 1))
weight.WEts
plot(weight.WEts)

# and impute with seasonal decomposition and interpolation
weight.WEts <- na.seadec(weight.WEts, algorithm = "interpolation")
weight.WEts
plot(weight.WEts)
lines(weight.Ets, col = "cadetblue")

