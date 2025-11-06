## Charlesworth & Banaji Patterns of Change I.
## Script version date: January 21st, 2018

####################################################################################################################################

## Takes vectors of month values (from census) and imputes missing values 
# using seasonal decomposition and interpolation for those months with NA
## Performs this imputation for all attitudes from 2007 - 2016
# (basic monthly averages, followed by weighted monthly averages for implicit and explicit)

## Data objects required: timeseries dataframes with weighted vectors for CENSUS data

## The final imputed time series values are calculated using (2) seasonal decomposition and interpolation and are output as:
# (1) attitude.Cts for unweighted implicit
# (2) attitude.CEts for unweighted explicit
# (3) attitude.CWts for weighted implicit
# (4) attitude.CWEts for weighted explicit

####################################################################################################################################

## Install packages
install.packages("imputeTS", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("lsr", dependencies = TRUE)

## Load packages
require(imputeTS)
require(plyr)
require(lsr)

## Load data
load("timeseries_censusweights_012118.RData")


####################################################################################################################################
## IMPLICIT ATTITUDES  ------------------------
age.month <- ddply(timeseriesAgeCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
dis.month <- ddply(timeseriesDisCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
race.month <- ddply(timeseriesRaceCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
sex.month <- ddply(timeseriesSexCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
skin.month <- ddply(timeseriesSkinCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
weight.month.Body <- ddply(timeseriesWeightBodyCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
weight.month.Face <- ddply(timeseriesWeightFaceCensus, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))


## Age --------------
table(timeseriesAgeCensus$month.cent)
age.Cts <- ts(age.month$mean, frequency = 12, start = c(2007, 1))
age.Cts
plot(age.Cts)

age.CWts <- ts(age.month$weight.mean, frequency = 12, start = c(2007, 1))
age.CWts
plot(age.CWts)
lines(age.Cts, col = "cadetblue")

## Disability -----------
table(timeseriesDisCensus$month.cent)
dis.Cts <- ts(dis.month$mean, frequency = 12, start = c(2007, 1))
dis.Cts
plot(dis.Cts)

dis.CWts <- ts(dis.month$weight.mean, frequency = 12, start = c(2007, 1))
dis.CWts
plot(dis.CWts)
lines(dis.Cts, col = "cadetblue")

## Race ------------
table(timeseriesRaceCensus$month.cent)
race.Cts <- ts(race.month$mean, frequency = 12, start = c(2007, 1))
race.Cts
plot(race.Cts)

race.CWts <- ts(race.month$weight.mean, frequency = 12, start = c(2007, 1))
race.CWts
plot(race.CWts)
lines(race.Cts, col = "cadetblue")


## Sexuality  ----------
table(timeseriesSexCensus$month.cent)
sex.Cts <- ts(sex.month$mean, frequency = 12, start = c(2007, 1))
sex.Cts
plot(sex.Cts)

sex.CWts <- ts(sex.month$weight.mean, frequency = 12, start = c(2007, 1))
sex.CWts
plot(sex.CWts)
lines(sex.Cts, col = "cadetblue")


## SkinCensus Tone ------------
table(timeseriesSkinCensus$month.cent) 

# add in NAs for month values 106, 107
skin.month.extra <- as.data.frame(c(106, 107))
skin.month.extra$mean <- rep(NA, 2)
skin.month.extra$weight.mean <- rep(NA, 2)
skin.month.extra$d <- rep(NA, 2)
names(skin.month.extra) <- c( "month.cent", "mean", "weight.mean", "d")
skin.month <- rbind(skin.month.extra, skin.month)
skin.month <- skin.month[order(skin.month$month.cent),]
skin.month

skin.Cts <- ts(skin.month$mean, frequency = 12, start = c(2007, 1))
skin.Cts
# impute with seasonal decomposition and interpolation
skin.Cts <- na.seadec(skin.Cts, algorithm = "interpolation")
skin.Cts
plot(skin.Cts)

skin.CWts <- ts(skin.month$weight.mean, frequency = 12, start = c(2007, 1))
skin.CWts
plot(skin.CWts)
# go with seasonal decomposition and interpolation
skin.CWts <- na.seadec(skin.CWts, algorithm = "interpolation")
skin.CWts
plot(skin.CWts)
lines(skin.Cts, col = "cadetblue")

## Weight ---------------
# (1) Body ------------
# Missing data for months 105:107 and 132, with low data for 125 - 131
table(timeseriesWeightBodyCensus$month.cent)
table(timeseriesWeightBodyCensus$month)

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

weight.Cts <- ts(weight.month.Body$mean, frequency = 12, start = c(2010, 4))
weight.Cts

# impute with seasonal decomposition and interpolation
weight.Cts <- na.seadec(weight.Cts, algorithm = "interpolation")
weight.Cts
plot(weight.Cts)

weight.CWts <- ts(weight.month.Body$weight.mean, frequency = 12, start = c(2010, 4))
weight.CWts
plot(weight.CWts)

# go with seasonal decomposition and interpolation
weight.CWts <- na.seadec(weight.CWts, algorithm = "interpolation")
weight.CWts
plot(weight.CWts)
lines(weight.Cts, col = "cadetblue")

# (2) Face ------------
# No missing data
table(timeseriesWeightFaceCensus$month.cent)
table(timeseriesWeightFaceCensus$month)

weightFace.Cts <- ts(weight.month.Face$mean, frequency = 12, start = c(2004, 5))
weightFace.Cts

weightFace.CWts <- ts(weight.month.Face$weight.mean, frequency = 12, start = c(2004, 5))
weightFace.CWts

plot(weightFace.CWts)
lines(weightFace.Cts, col = "cadetblue")



####################################################################################################################################
## EXPLICIT ATTITUDES ------------------------------------------------------------------------------------------------------------
age.month.e <- ddply(timeseriesAgeCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
dis.month.e <- ddply(timeseriesDisCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
race.month.e <- ddply(timeseriesRaceCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
skin.month.e <- ddply(timeseriesSkinCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
sex.month.e <- ddply(timeseriesSexCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
weight.month.e <- ddply(timeseriesWeightBodyCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))
weight.month.eFace <- ddply(timeseriesWeightFaceCensus, .(month.cent), summarize, mean = mean(att.center1), weight.mean = weighted.mean(att.center1, weightvec), d = cohensD(att.center1))


## Age ------
age.CEts <- ts(age.month.e$mean, frequency = 12, start = c(2007, 1))
age.CEts
plot(age.CEts)

age.CWEts <- ts(age.month.e$weight.mean, frequency = 12, start = c(2007, 1))
age.CWEts
plot(age.CWEts)
lines(age.CEts, col = "cadetblue")

## Disability ------
dis.CEts <- ts(dis.month.e$mean, frequency = 12, start = c(2007, 1))
dis.CEts
plot(dis.CEts)

dis.CWEts <- ts(dis.month.e$weight.mean, frequency = 12, start = c(2007, 1))
dis.CWEts
plot(dis.CWEts)
lines(dis.CEts, col = "cadetblue")

## Race -------
race.CEts <- ts(race.month.e$mean, frequency = 12, start = c(2007, 1))
race.CEts
plot(race.CEts)

race.CWEts <- ts(race.month.e$weight.mean, frequency = 12, start = c(2007, 1))
race.CWEts
plot(race.CWEts)
lines(race.CEts, col = "cadetblue")

## Sexuality -------
sex.CEts <- ts(sex.month.e$mean, frequency = 12, start = c(2007, 1))
sex.CEts
plot(sex.CEts)

sex.CWEts <- ts(sex.month.e$weight.mean, frequency = 12, start = c(2007, 1))
sex.CWEts
plot(sex.CWEts)
lines(sex.CEts, col = "cadetblue")

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

skin.CEts <- ts(skin.month.e$mean, frequency = 12, start = c(2007, 1))
skin.CEts
plot(skin.CEts)
# go with seasonal decomposition and interpolation
skin.CEts <- na.seadec(skin.CEts, algorithm = "interpolation")
skin.CEts
plot(skin.CEts)

skin.CWEts <- ts(skin.month.e$weight.mean, frequency = 12, start = c(2007, 1))
skin.CWEts
plot(skin.CWEts)
# go with seasonal decomposition and interpolation
skin.CWEts <- na.seadec(skin.CWEts, algorithm = "interpolation")
skin.CWEts
plot(skin.CWEts)
lines(skin.CEts, col = "cadetblue")

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

weight.month.eFace <- weight.month.eFace[weight.month.eFace$month.cent>=37 & weight.month.eFace$month.cent <= 75,]
weight.month.eFace

weight.month.eComb <- rbind(weight.month.eFace, weight.month.e)
weight.month.eComb

## unweighted
weight.CEts <- ts(weight.month.eComb$mean, frequency = 12, start = c(2007, 1))
weight.CEts
plot(weight.CEts)

#  use seasonal decomposition and interpolation
weight.CEts <- na.seadec(weight.CEts, algorithm = "interpolation")
weight.CEts
plot(weight.CEts)

## WEIGHTED
weight.CWEts <- ts(weight.month.eComb$weight.mean, frequency = 12, start = c(2007, 1))
weight.CWEts
plot(weight.CWEts)

# and impute with seasonal decomposition and interpolation
weight.CWEts <- na.seadec(weight.CWEts, algorithm = "interpolation")
weight.CWEts
plot(weight.CWEts)
lines(weight.CEts, col = "cadetblue")

##############################################################################################################################
##############################################################################################################################


## Plots: Weighted and unweighted together -----
## Explicit
op <- par(mfrow = c(2,3))
plot(age.CEts, main = "Explicit Age Attitudes", ylab = "Explicit Preference", ylim = c(0.15, 0.8), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(age.CWEts, col = "cadetblue", lwd = 3)
plot(dis.CEts, main = "Explicit Disability Attitudes", ylab = "Explicit Preference", ylim = c(0.35, 0.85), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(dis.CWEts, col = "cadetblue", lwd = 3)
plot(weight.CEts, main = "Explicit Weight Attitudes", ylab = "Explicit Preference", ylim = c(0.7, 1.2), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(weight.CWEts, col = "cadetblue", lwd = 3)
plot(race.CEts, main = "Explicit Race Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 0.6),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(race.CWEts, col = "cadetblue", lwd = 3)
plot(skin.CEts, main = "Explicit Skin-Tone Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 0.6),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(skin.CWEts, col = "cadetblue", lwd = 3)
plot(sex.CEts, main = "Explicit Sexuality Attitudes", ylab = "Explicit Preference", ylim = c(0.1, 1.3),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(sex.CWEts, col = "cadetblue", lwd = 3)
par(op)


## Implicit
op <- par(mfrow = c(2,3))
plot(age.Cts, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.38, 0.5), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(age.CWts, col = "cadetblue", lwd = 3)
plot(dis.Cts, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.4, 0.6), cex.main = 1.5,
     col = "coral", lwd = 3)
lines(dis.CWts, col = "cadetblue", lwd = 3)
plot(weightFace.Cts, main = "Implicit Weight Attitudes", ylab = "IAT D-score", ylim = c(0.25, 0.56), cex.main = 1.5,
     col = "coral", lwd = 3, xlim = c(2004, 2017), lty = 3)
lines(weight.Cts, col = "coral", lwd = 3)
lines(weight.CWts, col = "cadetblue", lwd = 3)
lines(weightFace.CWts, col = "cadetblue", lwd = 3, lty = 3)
plot(race.Cts, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.23, 0.44),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(race.CWts, col = "cadetblue", lwd = 3)
plot(skin.Cts, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.23, 0.44),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(skin.CWts, col = "cadetblue", lwd = 3)
plot(sex.Cts, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.12, 0.5),cex.main = 1.5,
     col = "coral", lwd = 3)
lines(sex.CWts, col = "cadetblue", lwd = 3)
par(op)





