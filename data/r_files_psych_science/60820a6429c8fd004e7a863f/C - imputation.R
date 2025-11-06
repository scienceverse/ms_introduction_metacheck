
## Patterns of Implicit and Explicit Attitudes IV. 
## Script version date: September 1st, 2021

####################################################################################################################################

## Creates the TS vectors for use in the ARIMA modelling

## Data objects required: timeseries dataframes with weighted vectors

## The final time series are output as:
# (1) attitude.ts for unweighted implicit
# (2) attitude.Ets for unweighted explicit
# (3) attitude.Wts for weighted implicit
# (4) attitude.WEts for weighted explicit

####################################################################################################################################

# setwd("**") # set working directory if necessary

## Load data
load("timeseries_NEWDATA.RData")
load("ALL_demogdataframe.RData")

## Install packages
if (!require(plyr)) {install.packages("plyr", dependencies = TRUE); require(plyr)}
if (!require(lsr)) {install.packages("lsr", dependencies = TRUE); require(lsr)}

####################################################################################################################################

## **** OVERALL TS TRAJECTORIES ---------

## IMPLICIT ATTITUDES  ------------------------
race.month <- ddply(timeseriesRace_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
age.month <- ddply(timeseriesAge_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
dis.month <- ddply(timeseriesDis_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
sex.month <- ddply(timeseriesSex_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
skin.month <- ddply(timeseriesSkin_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))
weight.month <- ddply(timeseriesWeight_NEW, .(month.cent), summarize, mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), d = cohensD(dscore))

## Race ------------
table(timeseriesRace_NEW$month.cent)
race.tsNEW <- ts(race.month$mean, frequency = 12, start = c(2017, 1))
race.tsNEW
plot(race.tsNEW)

race.WtsNEW <- ts(race.month$weight.mean, frequency = 12, start = c(2017, 1))
race.WtsNEW
plot(race.WtsNEW)
lines(race.tsNEW, col = "cadetblue")


## Age --------------
table(timeseriesAge_NEW$month.cent)
age.tsNEW <- ts(age.month$mean, frequency = 12, start = c(2017, 1))
age.tsNEW
plot(age.tsNEW)

age.WtsNEW <- ts(age.month$weight.mean, frequency = 12, start = c(2017, 1))
age.WtsNEW
plot(age.WtsNEW)
lines(age.tsNEW, col = "cadetblue")

## Disability -----------
table(timeseriesDis_NEW$month.cent)
dis.tsNEW <- ts(dis.month$mean, frequency = 12, start = c(2017, 1))
dis.tsNEW
plot(dis.tsNEW)

dis.WtsNEW <- ts(dis.month$weight.mean, frequency = 12, start = c(2017, 1))
dis.WtsNEW
plot(dis.WtsNEW)
lines(dis.tsNEW, col = "cadetblue")

## Sexuality  ----------
table(timeseriesSex_NEW$month.cent)
sex.tsNEW <- ts(sex.month$mean, frequency = 12, start = c(2017, 1))
sex.tsNEW
plot(sex.tsNEW)

sex.WtsNEW <- ts(sex.month$weight.mean, frequency = 12, start = c(2017, 1))
sex.WtsNEW
plot(sex.WtsNEW)
lines(sex.tsNEW, col = "cadetblue")


## Skin Tone ------------
table(timeseriesSkin_NEW$month.cent) 

skin.tsNEW <- ts(skin.month$mean, frequency = 12, start = c(2017, 1))
skin.tsNEW
plot(skin.tsNEW)

skin.WtsNEW <- ts(skin.month$weight.mean, frequency = 12, start = c(2017, 1))
skin.WtsNEW
plot(skin.WtsNEW)
lines(skin.tsNEW, col = "cadetblue")

## Weight ---------------
table(timeseriesWeight_NEW$month)
weight.tsNEW <- ts(weight.month$mean, frequency = 12, start = c(2017, 1))
weight.tsNEW
plot(weight.tsNEW)

weight.WtsNEW <- ts(weight.month$weight.mean, frequency = 12, start = c(2017, 1))
weight.WtsNEW
plot(weight.WtsNEW)
lines(weight.tsNEW, col = "cadetblue")


####################################################################################################################################
## EXPLICIT ATTITUDES ------------------------------------------------------------------------------------------------------------
race.month.e <- ddply(timeseriesRace_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))
age.month.e <- ddply(timeseriesAge_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))
dis.month.e <- ddply(timeseriesDis_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))
skin.month.e <- ddply(timeseriesSkin_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))
sex.month.e <- ddply(timeseriesSex_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))
weight.month.e <- ddply(timeseriesWeight_NEW, .(month.cent), summarize, mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), d = cohensD(attnumeric))


## Race -------
race.EtsNEW <- ts(race.month.e$mean, frequency = 12, start = c(2017, 1))
race.EtsNEW
plot(race.EtsNEW)

race.WEtsNEW <- ts(race.month.e$weight.mean, frequency = 12, start = c(2017, 1))
race.WEtsNEW
plot(race.WEtsNEW)
lines(race.EtsNEW, col = "cadetblue")


## Age ------
age.EtsNEW <- ts(age.month.e$mean, frequency = 12, start = c(2017, 1))
age.EtsNEW
plot(age.EtsNEW)

age.WEtsNEW <- ts(age.month.e$weight.mean, frequency = 12, start = c(2017, 1))
age.WEtsNEW
plot(age.WEtsNEW)
lines(age.EtsNEW, col = "cadetblue")

## Disability ------
dis.EtsNEW <- ts(dis.month.e$mean, frequency = 12, start = c(2017, 1))
dis.EtsNEW
plot(dis.EtsNEW)

dis.WEtsNEW <- ts(dis.month.e$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEtsNEW
plot(dis.WEtsNEW)
lines(dis.EtsNEW, col = "cadetblue")


## Sexuality -------
sex.EtsNEW <- ts(sex.month.e$mean, frequency = 12, start = c(2017, 1))
sex.EtsNEW
plot(sex.EtsNEW)

sex.WEtsNEW <- ts(sex.month.e$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEtsNEW
plot(sex.WEtsNEW)
lines(sex.EtsNEW, col = "cadetblue")

## Skin Tone -----
skin.EtsNEW <- ts(skin.month.e$mean, frequency = 12, start = c(2017, 1))
skin.EtsNEW
plot(skin.EtsNEW)

skin.WEtsNEW <- ts(skin.month.e$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEtsNEW
plot(skin.WEtsNEW)
lines(skin.EtsNEW, col = "cadetblue")

## Weight ---------
weight.EtsNEW <- ts(weight.month.e$mean, frequency = 12, start = c(2017, 1))
weight.EtsNEW
plot(weight.EtsNEW)

weight.WEtsNEW <- ts(weight.month.e$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEtsNEW
plot(weight.WEtsNEW)
lines(weight.EtsNEW, col = "cadetblue")


## Save out ----
save(list = c("weight.WtsNEW", "weight.tsNEW", "weight.WEtsNEW", "weight.EtsNEW",
              "race.WtsNEW", "race.tsNEW", "race.WEtsNEW", "race.EtsNEW",
              "skin.WtsNEW", "skin.tsNEW", "skin.WEtsNEW", "skin.EtsNEW",
              "sex.WtsNEW", "sex.tsNEW", "sex.WEtsNEW", "sex.EtsNEW",
              "age.WtsNEW", "age.tsNEW", "age.WEtsNEW", "age.EtsNEW",
              "dis.WtsNEW", "dis.tsNEW", "dis.WEtsNEW", "dis.EtsNEW"),
     file = "TSvectors_NEWDATA.RData")



####################################################################################################################################

## **** DEMOGRAPHIC TS TRAJECTORIES ----
## NOTE:
# x.ts.x (e.g., race.ts.lib) -- means that it is the unweighted time series for that group (e.g., liberals)
# x.Wts.x (e.g., race.Wts.lib) -- means that it is weighted to OVERALL target sample weights (set across all time)
# x.Wts.X.x (e.g., race.Wts.Pol.lib) -- means that it is weighted to DEMOGRAPHIC covariate sample weights


### IMPLICIT Race ----
## Politics:
# Liberals
race.liberals <- race.libcons[as.numeric(race.libcons$politicalid.new) > 4,]
race.conserv <- race.libcons[as.numeric(race.libcons$politicalid.new) < 4,]
race.month.lib <- ddply(race.liberals, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))

race.ts.libNEW <- ts(race.month.lib$mean, frequency = 12, start = c(2017, 1))
race.ts.libNEW
race.Wts.Pol.libNEW <- ts(race.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
race.Wts.Pol.libNEW
race.Wts.libNEW <- ts(race.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.libNEW
plot(race.Wts.libNEW)
lines(race.Wts.Pol.libNEW, col = "cadetblue")
lines(race.ts.libNEW, col = "coral")
# Conservatives
race.month.cons <- ddply(race.conserv, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.pol.mean = weighted.mean(dscore, weightvec.pol))
race.ts.consNEW <- ts(race.month.cons$mean, frequency = 12, start = c(2017, 1))
race.ts.consNEW
race.Wts.Pol.consNEW <- ts(race.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
race.Wts.Pol.consNEW
race.Wts.consNEW <- ts(race.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.consNEW
plot(race.Wts.consNEW)
lines(race.Wts.Pol.consNEW, col = "cadetblue")
lines(race.ts.consNEW, col = "coral")

## Gender:
# Female
race.fem <- race.femmal[race.femmal$genderidentity_simp == "female",]
race.mal <- race.femmal[race.femmal$genderidentity_simp == "male",]
race.month.fem <- ddply(race.fem, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
race.ts.femNEW <- ts(race.month.fem$mean, frequency = 12, start = c(2017, 1))
race.ts.femNEW
race.Wts.Gen.femNEW <- ts(race.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
race.Wts.Gen.femNEW
race.Wts.femNEW <- ts(race.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.femNEW
plot(race.Wts.femNEW)
lines(race.Wts.Gen.femNEW, col = "cadetblue")
lines(race.ts.femNEW, col = "coral")

# Males
race.month.mal <- ddply(race.mal, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
race.ts.malNEW <- ts(race.month.mal$mean, frequency = 12, start = c(2017, 1))
race.ts.malNEW
race.Wts.Gen.malNEW <- ts(race.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
race.Wts.Gen.malNEW
race.Wts.malNEW <- ts(race.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.malNEW
plot(race.Wts.malNEW)
lines(race.Wts.Gen.malNEW, col = "cadetblue")
lines(race.ts.malNEW, col = "coral")

## Education:
# < College
race.ncol <- race.ncolcol[race.ncolcol$edu.grp == "nocollege",]
race.col <- race.ncolcol[race.ncolcol$edu.grp == "college",]
race.month.ncol <- ddply(race.ncol, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.edu.mean = weighted.mean(dscore, weightvec.edu))

race.ts.ncolNEW <- ts(race.month.ncol$mean, frequency = 12, start = c(2017, 1))
race.ts.ncolNEW
race.Wts.Edu.ncolNEW <- ts(race.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
race.Wts.Edu.ncolNEW
race.Wts.ncolNEW <- ts(race.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.ncolNEW
plot(race.Wts.ncolNEW)
lines(race.Wts.Edu.ncolNEW, col = "cadetblue")
lines(race.ts.ncolNEW, col = "coral")
# > College
race.month.col <- ddply(race.col, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

race.ts.colNEW <- ts(race.month.col$mean, frequency = 12, start = c(2017, 1))
race.ts.colNEW
race.Wts.Edu.colNEW <- ts(race.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
race.Wts.Edu.colNEW
race.Wts.colNEW <- ts(race.month.col$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.colNEW
plot(race.Wts.colNEW)
lines(race.Wts.Edu.colNEW, col = "cadetblue")
lines(race.ts.colNEW, col = "coral")

## Race:
# White
race.white <- race.whblkasi[race.whblkasi$racesimple == "White",]
race.black <- race.whblkasi[race.whblkasi$racesimple == "Black",]
race.asian <- race.whblkasi[race.whblkasi$racesimple == "Asian",]

race.month.white <- ddply(race.white, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

race.ts.whiteNEW <- ts(race.month.white$mean, frequency = 12, start = c(2017, 1))
race.ts.whiteNEW
race.Wts.Race.whiteNEW <- ts(race.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
race.Wts.Race.whiteNEW
race.Wts.whiteNEW <- ts(race.month.white$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.whiteNEW
plot(race.Wts.whiteNEW)
lines(race.Wts.Race.whiteNEW, col = "cadetblue")
lines(race.ts.whiteNEW, col = "coral")

# Black
race.month.black <- ddply(race.black, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

race.ts.blackNEW <- ts(race.month.black$mean, frequency = 12, start = c(2017, 1))
race.ts.blackNEW
race.Wts.Race.blackNEW <- ts(race.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
race.Wts.Race.blackNEW
race.Wts.blackNEW <- ts(race.month.black$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.blackNEW
plot(race.Wts.blackNEW)
lines(race.Wts.Race.blackNEW, col = "cadetblue")
lines(race.ts.blackNEW, col = "coral")

# Asian
race.month.asian <- ddply(race.asian, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

race.ts.asianNEW <- ts(race.month.asian$mean, frequency = 12, start = c(2017, 1))
race.ts.asianNEW
race.Wts.Race.asianNEW <- ts(race.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
race.Wts.Race.asianNEW
race.Wts.asianNEW <- ts(race.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.asianNEW
plot(race.Wts.asianNEW)
lines(race.Wts.Race.asianNEW, col = "cadetblue")
lines(race.ts.asianNEW, col = "coral")


## Age:
race.ygmidold$weightvec.age <- as.numeric(race.ygmidold$weightvec.age)
# Young
race.young <- race.ygmidold[race.ygmidold$age.grp == "young",]
race.middle <- race.ygmidold[race.ygmidold$age.grp == "middle",]
race.old <- race.ygmidold[race.ygmidold$age.grp == "old",]

race.month.young <- ddply(race.young, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

race.ts.youngNEW <- ts(race.month.young$mean, frequency = 12, start = c(2017, 1))
race.ts.youngNEW
race.Wts.Age.youngNEW <- ts(race.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
race.Wts.Age.youngNEW
race.Wts.youngNEW <- ts(race.month.young$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.youngNEW
plot(race.Wts.youngNEW)
lines(race.Wts.Age.youngNEW, col = "cadetblue")
lines(race.ts.youngNEW, col = "coral")

# Middle
race.month.middle <- ddply(race.middle, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.age.mean = weighted.mean(dscore, weightvec.age))

race.ts.middleNEW <- ts(race.month.middle$mean, frequency = 12, start = c(2017, 1))
race.ts.middleNEW
race.Wts.Age.middleNEW <- ts(race.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
race.Wts.Age.middleNEW
race.Wts.middleNEW <- ts(race.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.middleNEW
plot(race.Wts.middleNEW)
lines(race.Wts.Age.middleNEW, col = "cadetblue")
lines(race.ts.middleNEW, col = "coral")

# Old
race.month.old <- ddply(race.old, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.age.mean = weighted.mean(dscore, weightvec.age))

race.ts.oldNEW <- ts(race.month.old$mean, frequency = 12, start = c(2017, 1))
race.ts.oldNEW
race.Wts.Age.oldNEW <- ts(race.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
race.Wts.Age.oldNEW
race.Wts.oldNEW <- ts(race.month.old$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.oldNEW
plot(race.Wts.oldNEW)
lines(race.Wts.Age.oldNEW, col = "cadetblue")
lines(race.ts.oldNEW, col = "coral")

## Religion:
race.allreligion$relisimple <- car::recode(race.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
race.christ <- race.allreligion[race.allreligion$relisimple == "christian",]
race.other <- race.allreligion[race.allreligion$relisimple == "other",]
race.nonrel <- race.allreligion[race.allreligion$relisimple == "nonrel",]
race.jew <- race.allreligion[race.allreligion$relisimple == "jewish",]

race.month.christian <- ddply(race.christ, .(month.cent), summarize, 
                              mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                              weight.rel.mean = weighted.mean(dscore, weightvec.religion))

race.ts.christianNEW <- ts(race.month.christian$mean, frequency = 12, start = c(2017, 1))
race.ts.christianNEW
race.Wts.Rel.christianNEW <- ts(race.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.Wts.Rel.christianNEW
race.Wts.christianNEW <- ts(race.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.christianNEW
plot(race.Wts.christianNEW)
lines(race.Wts.Rel.christianNEW, col = "cadetblue")
lines(race.ts.christianNEW, col = "coral")

# other
race.month.other <- ddply(race.other, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))

race.ts.otherNEW <- ts(race.month.other$mean, frequency = 12, start = c(2017, 1))
race.ts.otherNEW
race.Wts.Rel.otherNEW <- ts(race.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.Wts.Rel.otherNEW
race.Wts.otherNEW <- ts(race.month.other$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.otherNEW
plot(race.Wts.otherNEW)
lines(race.Wts.Rel.otherNEW, col = "cadetblue")
lines(race.ts.otherNEW, col = "coral")

# non
race.month.non <- ddply(race.nonrel, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.rel.mean = weighted.mean(dscore, weightvec.religion))

race.ts.nonNEW <- ts(race.month.non$mean, frequency = 12, start = c(2017, 1))
race.ts.nonNEW
race.Wts.Rel.nonNEW <- ts(race.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.Wts.Rel.nonNEW
race.Wts.nonNEW <- ts(race.month.non$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.nonNEW
plot(race.Wts.nonNEW)
lines(race.Wts.Rel.nonNEW, col = "cadetblue")
lines(race.ts.nonNEW, col = "coral")

# jewish
race.month.jewish <- ddply(race.jew, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.rel.mean = weighted.mean(dscore, weightvec.religion))
race.ts.jewishNEW <- ts(race.month.jewish$mean, frequency = 12, start = c(2017, 1))
race.ts.jewishNEW
race.Wts.Rel.jewishNEW <- ts(race.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.Wts.Rel.jewishNEW
race.Wts.jewishNEW <- ts(race.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
race.Wts.jewishNEW
plot(race.Wts.jewishNEW)
lines(race.Wts.Rel.jewishNEW, col = "cadetblue")
lines(race.ts.jewishNEW, col = "coral")


## EXPLICIT Race ----
## Politics:
# Liberals
race.month.E.lib <- ddply(race.liberals, .(month.cent), summarize, 
                        mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                        weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

race.Ets.libNEW <- ts(race.month.E.lib$mean, frequency = 12, start = c(2017, 1))
race.Ets.libNEW
race.WEts.Pol.libNEW <- ts(race.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
race.WEts.Pol.libNEW
race.WEts.libNEW <- ts(race.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.libNEW
plot(race.WEts.libNEW)
lines(race.WEts.Pol.libNEW, col = "cadetblue")
lines(race.Ets.libNEW, col = "coral")
# Conservatives
race.month.E.cons <- ddply(race.conserv, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
race.Ets.consNEW <- ts(race.month.E.cons$mean, frequency = 12, start = c(2017, 1))
race.Ets.consNEW
race.WEts.Pol.consNEW <- ts(race.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
race.WEts.Pol.consNEW
race.WEts.consNEW <- ts(race.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.consNEW
plot(race.WEts.consNEW)
lines(race.WEts.Pol.consNEW, col = "cadetblue")
lines(race.Ets.consNEW, col = "coral")

## Gender:
# Female
race.month.E.fem <- ddply(race.fem, .(month.cent), summarize, 
                        mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                        weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
race.Ets.femNEW <- ts(race.month.E.fem$mean, frequency = 12, start = c(2017, 1))
race.Ets.femNEW
race.WEts.Gen.femNEW <- ts(race.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
race.WEts.Gen.femNEW
race.WEts.femNEW <- ts(race.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.femNEW
plot(race.WEts.femNEW)
lines(race.WEts.Gen.femNEW, col = "cadetblue")
lines(race.Ets.femNEW, col = "coral")

# Males
race.month.E.mal <- ddply(race.mal, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
race.Ets.malNEW <- ts(race.month.E.mal$mean, frequency = 12, start = c(2017, 1))
race.Ets.malNEW
race.WEts.Gen.malNEW <- ts(race.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
race.WEts.Gen.malNEW
race.WEts.malNEW <- ts(race.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.malNEW
plot(race.WEts.malNEW)
lines(race.WEts.Gen.malNEW, col = "cadetblue")
lines(race.Ets.malNEW, col = "coral")

## Education:
# < College
race.month.E.ncol <- ddply(race.ncol, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

race.Ets.ncolNEW <- ts(race.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
race.Ets.ncolNEW
race.WEts.Edu.ncolNEW <- ts(race.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
race.WEts.Edu.ncolNEW
race.WEts.ncolNEW <- ts(race.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.ncolNEW
plot(race.WEts.ncolNEW)
lines(race.WEts.Edu.ncolNEW, col = "cadetblue")
lines(race.Ets.ncolNEW, col = "coral")
# > College
race.month.E.col <- ddply(race.col, .(month.cent), summarize, 
                        mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                        weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

race.Ets.colNEW <- ts(race.month.E.col$mean, frequency = 12, start = c(2017, 1))
race.Ets.colNEW
race.WEts.Edu.colNEW <- ts(race.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
race.WEts.Edu.colNEW
race.WEts.colNEW <- ts(race.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.colNEW
plot(race.WEts.colNEW)
lines(race.WEts.Edu.colNEW, col = "cadetblue")
lines(race.Ets.colNEW, col = "coral")

## Race:
# White
race.month.E.white <- ddply(race.white, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.race.mean = weighted.mean(attnumeric, weightvec.race))

race.Ets.whiteNEW <- ts(race.month.E.white$mean, frequency = 12, start = c(2017, 1))
race.Ets.whiteNEW
race.WEts.Race.whiteNEW <- ts(race.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
race.WEts.Race.whiteNEW
race.WEts.whiteNEW <- ts(race.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.whiteNEW
plot(race.WEts.whiteNEW)
lines(race.WEts.Race.whiteNEW, col = "cadetblue")
lines(race.Ets.whiteNEW, col = "coral")

# Black
race.month.E.black <- ddply(race.black, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.race.mean = weighted.mean(attnumeric, weightvec.race))

race.Ets.blackNEW <- ts(race.month.E.black$mean, frequency = 12, start = c(2017, 1))
race.Ets.blackNEW
race.WEts.Race.blackNEW <- ts(race.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
race.WEts.Race.blackNEW
race.WEts.blackNEW <- ts(race.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.blackNEW
plot(race.WEts.blackNEW)
lines(race.WEts.Race.blackNEW, col = "cadetblue")
lines(race.Ets.blackNEW, col = "coral")

# Asian
race.month.E.asian <- ddply(race.asian, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.race.mean = weighted.mean(attnumeric, weightvec.race))

race.Ets.asianNEW <- ts(race.month.E.asian$mean, frequency = 12, start = c(2017, 1))
race.Ets.asianNEW
race.WEts.Race.asianNEW <- ts(race.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
race.WEts.Race.asianNEW
race.WEts.asianNEW <- ts(race.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.asianNEW
plot(race.WEts.asianNEW)
lines(race.WEts.Race.asianNEW, col = "cadetblue")
lines(race.Ets.asianNEW, col = "coral")


## Age:
# Young
race.month.E.young <- ddply(race.young, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))

race.Ets.youngNEW <- ts(race.month.E.young$mean, frequency = 12, start = c(2017, 1))
race.Ets.youngNEW
race.WEts.Age.youngNEW <- ts(race.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
race.WEts.Age.youngNEW
race.WEts.youngNEW <- ts(race.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.youngNEW
plot(race.WEts.youngNEW)
lines(race.WEts.Age.youngNEW, col = "cadetblue")
lines(race.Ets.youngNEW, col = "coral")

# Middle
race.month.E.middle <- ddply(race.middle, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.age.mean = weighted.mean(attnumeric, weightvec.age))

race.Ets.middleNEW <- ts(race.month.E.middle$mean, frequency = 12, start = c(2017, 1))
race.Ets.middleNEW
race.WEts.Age.middleNEW <- ts(race.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
race.WEts.Age.middleNEW
race.WEts.middleNEW <- ts(race.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.middleNEW
plot(race.WEts.middleNEW)
lines(race.WEts.Age.middleNEW, col = "cadetblue")
lines(race.Ets.middleNEW, col = "coral")

# Old
race.month.E.old <- ddply(race.old, .(month.cent), summarize, 
                        mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                        weight.age.mean = weighted.mean(attnumeric, weightvec.age))

race.Ets.oldNEW <- ts(race.month.E.old$mean, frequency = 12, start = c(2017, 1))
race.Ets.oldNEW
race.WEts.Age.oldNEW <- ts(race.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
race.WEts.Age.oldNEW
race.WEts.oldNEW <- ts(race.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.oldNEW
plot(race.WEts.oldNEW)
lines(race.WEts.Age.oldNEW, col = "cadetblue")
lines(race.Ets.oldNEW, col = "coral")

## Religion:
# christian
race.month.E.christian <- ddply(race.christ, .(month.cent), summarize, 
                              mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                              weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

race.Ets.christianNEW <- ts(race.month.E.christian$mean, frequency = 12, start = c(2017, 1))
race.Ets.christianNEW
race.WEts.Rel.christianNEW <- ts(race.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.WEts.Rel.christianNEW
race.WEts.christianNEW <- ts(race.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.christianNEW
plot(race.WEts.christianNEW)
lines(race.WEts.Rel.christianNEW, col = "cadetblue")
lines(race.Ets.christianNEW, col = "coral")

# other
race.month.E.other <- ddply(race.other, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

race.Ets.otherNEW <- ts(race.month.E.other$mean, frequency = 12, start = c(2017, 1))
race.Ets.otherNEW
race.WEts.Rel.otherNEW <- ts(race.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.WEts.Rel.otherNEW
race.WEts.otherNEW <- ts(race.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.otherNEW
plot(race.WEts.otherNEW)
lines(race.WEts.Rel.otherNEW, col = "cadetblue")
lines(race.Ets.otherNEW, col = "coral")

# non
race.month.E.non <- ddply(race.nonrel, .(month.cent), summarize, 
                        mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                        weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

race.Ets.nonNEW <- ts(race.month.E.non$mean, frequency = 12, start = c(2017, 1))
race.Ets.nonNEW
race.WEts.Rel.nonNEW <- ts(race.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.WEts.Rel.nonNEW
race.WEts.nonNEW <- ts(race.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.nonNEW
plot(race.WEts.nonNEW)
lines(race.WEts.Rel.nonNEW, col = "cadetblue")
lines(race.Ets.nonNEW, col = "coral")

# jewish
race.month.E.jewish <- ddply(race.jew, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
race.Ets.jewishNEW <- ts(race.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
race.Ets.jewishNEW
race.WEts.Rel.jewishNEW <- ts(race.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
race.WEts.Rel.jewishNEW
race.WEts.jewishNEW <- ts(race.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
race.WEts.jewishNEW
plot(race.WEts.jewishNEW)
lines(race.WEts.Rel.jewishNEW, col = "cadetblue")
lines(race.Ets.jewishNEW, col = "coral")

### IMPLICIT Skin-Tone ----
## Politics:
# Liberals
skin.liberals <- skin.libcons[as.numeric(skin.libcons$politicalid.new) > 4,]
skin.conserv <- skin.libcons[as.numeric(skin.libcons$politicalid.new) < 4,]

skin.month.lib <- ddply(skin.liberals, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.pol.mean = weighted.mean(dscore, weightvec.pol))

skin.ts.libNEW <- ts(skin.month.lib$mean, frequency = 12, start = c(2017, 1))
skin.ts.libNEW
skin.Wts.Pol.libNEW <- ts(skin.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Pol.libNEW
skin.Wts.libNEW <- ts(skin.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.libNEW
plot(skin.Wts.libNEW)
lines(skin.Wts.Pol.libNEW, col = "cadetblue")
lines(skin.ts.libNEW, col = "coral")
# Conservatives
skin.month.cons <- ddply(skin.conserv, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))
skin.ts.consNEW <- ts(skin.month.cons$mean, frequency = 12, start = c(2017, 1))
skin.ts.consNEW
skin.Wts.Pol.consNEW <- ts(skin.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Pol.consNEW
skin.Wts.consNEW <- ts(skin.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.consNEW
plot(skin.Wts.consNEW)
lines(skin.Wts.Pol.consNEW, col = "cadetblue")
lines(skin.ts.consNEW, col = "coral")

## Gender:
# Female
skin.fem <- skin.femmal[skin.femmal$genderidentity_simp == "female",]
skin.mal <- skin.femmal[skin.femmal$genderidentity_simp == "male",]
skin.month.fem <- ddply(skin.fem, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.gen.mean = weighted.mean(dscore, weightvec.gender))
skin.ts.femNEW <- ts(skin.month.fem$mean, frequency = 12, start = c(2017, 1))
skin.ts.femNEW
skin.Wts.Gen.femNEW <- ts(skin.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Gen.femNEW
skin.Wts.femNEW <- ts(skin.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.femNEW
plot(skin.Wts.femNEW)
lines(skin.Wts.Gen.femNEW, col = "cadetblue")
lines(skin.ts.femNEW, col = "coral")

# Males
skin.month.mal <- ddply(skin.mal, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.gen.mean = weighted.mean(dscore, weightvec.gender))
skin.ts.malNEW <- ts(skin.month.mal$mean, frequency = 12, start = c(2017, 1))
skin.ts.malNEW
skin.Wts.Gen.malNEW <- ts(skin.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Gen.malNEW
skin.Wts.malNEW <- ts(skin.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.malNEW
plot(skin.Wts.malNEW)
lines(skin.Wts.Gen.malNEW, col = "cadetblue")
lines(skin.ts.malNEW, col = "coral")

## Education:
# < College
skin.ncol <- skin.ncolcol[skin.ncolcol$edu.grp == "nocollege",]
skin.col <- skin.ncolcol[skin.ncolcol$edu.grp == "college",]
skin.month.ncol <- ddply(skin.ncol, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

skin.ts.ncolNEW <- ts(skin.month.ncol$mean, frequency = 12, start = c(2017, 1))
skin.ts.ncolNEW
skin.Wts.Edu.ncolNEW <- ts(skin.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Edu.ncolNEW
skin.Wts.ncolNEW <- ts(skin.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.ncolNEW
plot(skin.Wts.ncolNEW)
lines(skin.Wts.Edu.ncolNEW, col = "cadetblue")
lines(skin.ts.ncolNEW, col = "coral")
# > College
skin.month.col <- ddply(skin.col, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.edu.mean = weighted.mean(dscore, weightvec.edu))

skin.ts.colNEW <- ts(skin.month.col$mean, frequency = 12, start = c(2017, 1))
skin.ts.colNEW
skin.Wts.Edu.colNEW <- ts(skin.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Edu.colNEW
skin.Wts.colNEW <- ts(skin.month.col$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.colNEW
plot(skin.Wts.colNEW)
lines(skin.Wts.Edu.colNEW, col = "cadetblue")
lines(skin.ts.colNEW, col = "coral")

## Race:
# White
skin.white <- skin.whblkasi[skin.whblkasi$racesimple == "White",]
skin.black <- skin.whblkasi[skin.whblkasi$racesimple == "Black",]
skin.asian <- skin.whblkasi[skin.whblkasi$racesimple == "Asian",]

skin.month.white <- ddply(skin.white, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

skin.ts.whiteNEW <- ts(skin.month.white$mean, frequency = 12, start = c(2017, 1))
skin.ts.whiteNEW
skin.Wts.Race.whiteNEW <- ts(skin.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Race.whiteNEW
skin.Wts.whiteNEW <- ts(skin.month.white$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.whiteNEW
plot(skin.Wts.whiteNEW)
lines(skin.Wts.Race.whiteNEW, col = "cadetblue")
lines(skin.ts.whiteNEW, col = "coral")

# Black
skin.month.black <- ddply(skin.black, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

skin.ts.blackNEW <- ts(skin.month.black$mean, frequency = 12, start = c(2017, 1))
skin.ts.blackNEW
skin.Wts.Race.blackNEW <- ts(skin.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Race.blackNEW
skin.Wts.blackNEW <- ts(skin.month.black$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.blackNEW
plot(skin.Wts.blackNEW)
lines(skin.Wts.Race.blackNEW, col = "cadetblue")
lines(skin.ts.blackNEW, col = "coral")

# Asian
skin.month.asian <- ddply(skin.asian, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

skin.ts.asianNEW <- ts(skin.month.asian$mean, frequency = 12, start = c(2017, 1))
skin.ts.asianNEW
skin.Wts.Race.asianNEW <- ts(skin.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Race.asianNEW
skin.Wts.asianNEW <- ts(skin.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.asianNEW
plot(skin.Wts.asianNEW)
lines(skin.Wts.Race.asianNEW, col = "cadetblue")
lines(skin.ts.asianNEW, col = "coral")


## Age:
skin.ygmidold$weightvec.age <- as.numeric(skin.ygmidold$weightvec.age)
# Young
skin.young <- skin.ygmidold[skin.ygmidold$age.grp == "young",]
skin.middle <- skin.ygmidold[skin.ygmidold$age.grp == "middle",]
skin.old <- skin.ygmidold[skin.ygmidold$age.grp == "old",]

skin.month.young <- ddply(skin.young, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.age.mean = weighted.mean(dscore, weightvec.age))

skin.ts.youngNEW <- ts(skin.month.young$mean, frequency = 12, start = c(2017, 1))
skin.ts.youngNEW
skin.Wts.Age.youngNEW <- ts(skin.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Age.youngNEW
skin.Wts.youngNEW <- ts(skin.month.young$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.youngNEW
plot(skin.Wts.youngNEW)
lines(skin.Wts.Age.youngNEW, col = "cadetblue")
lines(skin.ts.youngNEW, col = "coral")

# Middle
skin.month.middle <- ddply(skin.middle, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

skin.ts.middleNEW <- ts(skin.month.middle$mean, frequency = 12, start = c(2017, 1))
skin.ts.middleNEW
skin.Wts.Age.middleNEW <- ts(skin.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Age.middleNEW
skin.Wts.middleNEW <- ts(skin.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.middleNEW
plot(skin.Wts.middleNEW)
lines(skin.Wts.Age.middleNEW, col = "cadetblue")
lines(skin.ts.middleNEW, col = "coral")

# Old
skin.month.old <- ddply(skin.old, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.age.mean = weighted.mean(dscore, weightvec.age))

skin.ts.oldNEW <- ts(skin.month.old$mean, frequency = 12, start = c(2017, 1))
skin.ts.oldNEW
skin.Wts.Age.oldNEW <- ts(skin.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Age.oldNEW
skin.Wts.oldNEW <- ts(skin.month.old$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.oldNEW
plot(skin.Wts.oldNEW)
lines(skin.Wts.Age.oldNEW, col = "cadetblue")
lines(skin.ts.oldNEW, col = "coral")

## Religion:
skin.allreligion$relisimple <- car::recode(skin.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
skin.christ <- skin.allreligion[skin.allreligion$relisimple == "christian",]
skin.other <- skin.allreligion[skin.allreligion$relisimple == "other",]
skin.nonrel <- skin.allreligion[skin.allreligion$relisimple == "nonrel",]
skin.jew <- skin.allreligion[skin.allreligion$relisimple == "jewish",]

skin.month.christian <- ddply(skin.christ, .(month.cent), summarize, 
                             mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                             weight.rel.mean = weighted.mean(dscore, weightvec.religion))

skin.ts.christianNEW <- ts(skin.month.christian$mean, frequency = 12, start = c(2017, 1))
skin.ts.christianNEW
skin.Wts.Rel.christianNEW <- ts(skin.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Rel.christianNEW
skin.Wts.christianNEW <- ts(skin.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.christianNEW
plot(skin.Wts.christianNEW)
lines(skin.Wts.Rel.christianNEW, col = "cadetblue")
lines(skin.ts.christianNEW, col = "coral")

# other
skin.month.other <- ddply(skin.other, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.rel.mean = weighted.mean(dscore, weightvec.religion))

skin.ts.otherNEW <- ts(skin.month.other$mean, frequency = 12, start = c(2017, 1))
skin.ts.otherNEW
skin.Wts.Rel.otherNEW <- ts(skin.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Rel.otherNEW
skin.Wts.otherNEW <- ts(skin.month.other$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.otherNEW
plot(skin.Wts.otherNEW)
lines(skin.Wts.Rel.otherNEW, col = "cadetblue")
lines(skin.ts.otherNEW, col = "coral")

# non
skin.month.non <- ddply(skin.nonrel, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.rel.mean = weighted.mean(dscore, weightvec.religion))

skin.ts.nonNEW <- ts(skin.month.non$mean, frequency = 12, start = c(2017, 1))
skin.ts.nonNEW
skin.Wts.Rel.nonNEW <- ts(skin.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Rel.nonNEW
skin.Wts.nonNEW <- ts(skin.month.non$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.nonNEW
plot(skin.Wts.nonNEW)
lines(skin.Wts.Rel.nonNEW, col = "cadetblue")
lines(skin.ts.nonNEW, col = "coral")

# jewish
skin.month.jewish <- ddply(skin.jew, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))
skin.ts.jewishNEW <- ts(skin.month.jewish$mean, frequency = 12, start = c(2017, 1))
skin.ts.jewishNEW
skin.Wts.Rel.jewishNEW <- ts(skin.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.Wts.Rel.jewishNEW
skin.Wts.jewishNEW <- ts(skin.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
skin.Wts.jewishNEW
plot(skin.Wts.jewishNEW)
lines(skin.Wts.Rel.jewishNEW, col = "cadetblue")
lines(skin.ts.jewishNEW, col = "coral")


## EXPLICIT Skin-Tone ----
## Politics:
# Liberals
skin.month.E.lib <- ddply(skin.liberals, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

skin.Ets.libNEW <- ts(skin.month.E.lib$mean, frequency = 12, start = c(2017, 1))
skin.Ets.libNEW
skin.WEts.Pol.libNEW <- ts(skin.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Pol.libNEW
skin.WEts.libNEW <- ts(skin.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.libNEW
plot(skin.WEts.libNEW)
lines(skin.WEts.Pol.libNEW, col = "cadetblue")
lines(skin.Ets.libNEW, col = "coral")
# Conservatives
skin.month.E.cons <- ddply(skin.conserv, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
skin.Ets.consNEW <- ts(skin.month.E.cons$mean, frequency = 12, start = c(2017, 1))
skin.Ets.consNEW
skin.WEts.Pol.consNEW <- ts(skin.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Pol.consNEW
skin.WEts.consNEW <- ts(skin.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.consNEW
plot(skin.WEts.consNEW)
lines(skin.WEts.Pol.consNEW, col = "cadetblue")
lines(skin.Ets.consNEW, col = "coral")

## Gender:
# Female
skin.month.E.fem <- ddply(skin.fem, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
skin.Ets.femNEW <- ts(skin.month.E.fem$mean, frequency = 12, start = c(2017, 1))
skin.Ets.femNEW
skin.WEts.Gen.femNEW <- ts(skin.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Gen.femNEW
skin.WEts.femNEW <- ts(skin.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.femNEW
plot(skin.WEts.femNEW)
lines(skin.WEts.Gen.femNEW, col = "cadetblue")
lines(skin.Ets.femNEW, col = "coral")

# Males
skin.month.E.mal <- ddply(skin.mal, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
skin.Ets.malNEW <- ts(skin.month.E.mal$mean, frequency = 12, start = c(2017, 1))
skin.Ets.malNEW
skin.WEts.Gen.malNEW <- ts(skin.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Gen.malNEW
skin.WEts.malNEW <- ts(skin.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.malNEW
plot(skin.WEts.malNEW)
lines(skin.WEts.Gen.malNEW, col = "cadetblue")
lines(skin.Ets.malNEW, col = "coral")

## Education:
# < College
skin.month.E.ncol <- ddply(skin.ncol, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

skin.Ets.ncolNEW <- ts(skin.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
skin.Ets.ncolNEW
skin.WEts.Edu.ncolNEW <- ts(skin.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Edu.ncolNEW
skin.WEts.ncolNEW <- ts(skin.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.ncolNEW
plot(skin.WEts.ncolNEW)
lines(skin.WEts.Edu.ncolNEW, col = "cadetblue")
lines(skin.Ets.ncolNEW, col = "coral")
# > College
skin.month.E.col <- ddply(skin.col, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

skin.Ets.colNEW <- ts(skin.month.E.col$mean, frequency = 12, start = c(2017, 1))
skin.Ets.colNEW
skin.WEts.Edu.colNEW <- ts(skin.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Edu.colNEW
skin.WEts.colNEW <- ts(skin.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.colNEW
plot(skin.WEts.colNEW)
lines(skin.WEts.Edu.colNEW, col = "cadetblue")
lines(skin.Ets.colNEW, col = "coral")

## Race:
# White
skin.month.E.white <- ddply(skin.white, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

skin.Ets.whiteNEW <- ts(skin.month.E.white$mean, frequency = 12, start = c(2017, 1))
skin.Ets.whiteNEW
skin.WEts.Race.whiteNEW <- ts(skin.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Race.whiteNEW
skin.WEts.whiteNEW <- ts(skin.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.whiteNEW
plot(skin.WEts.whiteNEW)
lines(skin.WEts.Race.whiteNEW, col = "cadetblue")
lines(skin.Ets.whiteNEW, col = "coral")

# Black
skin.month.E.black <- ddply(skin.black, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

skin.Ets.blackNEW <- ts(skin.month.E.black$mean, frequency = 12, start = c(2017, 1))
skin.Ets.blackNEW
skin.WEts.Race.blackNEW <- ts(skin.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Race.blackNEW
skin.WEts.blackNEW <- ts(skin.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.blackNEW
plot(skin.WEts.blackNEW)
lines(skin.WEts.Race.blackNEW, col = "cadetblue")
lines(skin.Ets.blackNEW, col = "coral")

# Asian
skin.month.E.asian <- ddply(skin.asian, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

skin.Ets.asianNEW <- ts(skin.month.E.asian$mean, frequency = 12, start = c(2017, 1))
skin.Ets.asianNEW
skin.WEts.Race.asianNEW <- ts(skin.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Race.asianNEW
skin.WEts.asianNEW <- ts(skin.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.asianNEW
plot(skin.WEts.asianNEW)
lines(skin.WEts.Race.asianNEW, col = "cadetblue")
lines(skin.Ets.asianNEW, col = "coral")


## Age:
# Young
skin.month.E.young <- ddply(skin.young, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.age.mean = weighted.mean(attnumeric, weightvec.age))

skin.Ets.youngNEW <- ts(skin.month.E.young$mean, frequency = 12, start = c(2017, 1))
skin.Ets.youngNEW
skin.WEts.Age.youngNEW <- ts(skin.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Age.youngNEW
skin.WEts.youngNEW <- ts(skin.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.youngNEW
plot(skin.WEts.youngNEW)
lines(skin.WEts.Age.youngNEW, col = "cadetblue")
lines(skin.Ets.youngNEW, col = "coral")

# Middle
skin.month.E.middle <- ddply(skin.middle, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.age))

skin.Ets.middleNEW <- ts(skin.month.E.middle$mean, frequency = 12, start = c(2017, 1))
skin.Ets.middleNEW
skin.WEts.Age.middleNEW <- ts(skin.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Age.middleNEW
skin.WEts.middleNEW <- ts(skin.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.middleNEW
plot(skin.WEts.middleNEW)
lines(skin.WEts.Age.middleNEW, col = "cadetblue")
lines(skin.Ets.middleNEW, col = "coral")

# Old
skin.month.E.old <- ddply(skin.old, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.age.mean = weighted.mean(attnumeric, weightvec.age))

skin.Ets.oldNEW <- ts(skin.month.E.old$mean, frequency = 12, start = c(2017, 1))
skin.Ets.oldNEW
skin.WEts.Age.oldNEW <- ts(skin.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Age.oldNEW
skin.WEts.oldNEW <- ts(skin.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.oldNEW
plot(skin.WEts.oldNEW)
lines(skin.WEts.Age.oldNEW, col = "cadetblue")
lines(skin.Ets.oldNEW, col = "coral")

## Religion:
# christian
skin.month.E.christian <- ddply(skin.christ, .(month.cent), summarize, 
                               mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                               weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

skin.Ets.christianNEW <- ts(skin.month.E.christian$mean, frequency = 12, start = c(2017, 1))
skin.Ets.christianNEW
skin.WEts.Rel.christianNEW <- ts(skin.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Rel.christianNEW
skin.WEts.christianNEW <- ts(skin.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.christianNEW
plot(skin.WEts.christianNEW)
lines(skin.WEts.Rel.christianNEW, col = "cadetblue")
lines(skin.Ets.christianNEW, col = "coral")

# other
skin.month.E.other <- ddply(skin.other, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

skin.Ets.otherNEW <- ts(skin.month.E.other$mean, frequency = 12, start = c(2017, 1))
skin.Ets.otherNEW
skin.WEts.Rel.otherNEW <- ts(skin.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Rel.otherNEW
skin.WEts.otherNEW <- ts(skin.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.otherNEW
plot(skin.WEts.otherNEW)
lines(skin.WEts.Rel.otherNEW, col = "cadetblue")
lines(skin.Ets.otherNEW, col = "coral")

# non
skin.month.E.non <- ddply(skin.nonrel, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

skin.Ets.nonNEW <- ts(skin.month.E.non$mean, frequency = 12, start = c(2017, 1))
skin.Ets.nonNEW
skin.WEts.Rel.nonNEW <- ts(skin.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Rel.nonNEW
skin.WEts.nonNEW <- ts(skin.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.nonNEW
plot(skin.WEts.nonNEW)
lines(skin.WEts.Rel.nonNEW, col = "cadetblue")
lines(skin.Ets.nonNEW, col = "coral")

# jewish
skin.month.E.jewish <- ddply(skin.jew, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
skin.Ets.jewishNEW <- ts(skin.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
skin.Ets.jewishNEW
skin.WEts.Rel.jewishNEW <- ts(skin.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
skin.WEts.Rel.jewishNEW
skin.WEts.jewishNEW <- ts(skin.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
skin.WEts.jewishNEW
plot(skin.WEts.jewishNEW)
lines(skin.WEts.Rel.jewishNEW, col = "cadetblue")
lines(skin.Ets.jewishNEW, col = "coral")




### IMPLICIT Sexuality ----
## Politics:
# Liberals
sex.liberals <- sex.libcons[as.numeric(sex.libcons$politicalid.new) > 4,]
sex.conserv <- sex.libcons[as.numeric(sex.libcons$politicalid.new) < 4,]

sex.month.lib <- ddply(sex.liberals, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))

sex.ts.libNEW <- ts(sex.month.lib$mean, frequency = 12, start = c(2017, 1))
sex.ts.libNEW
sex.Wts.Pol.libNEW <- ts(sex.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Pol.libNEW
sex.Wts.libNEW <- ts(sex.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.libNEW
plot(sex.Wts.libNEW)
lines(sex.Wts.Pol.libNEW, col = "cadetblue")
lines(sex.ts.libNEW, col = "coral")
# Conservatives
sex.month.cons <- ddply(sex.conserv, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.pol.mean = weighted.mean(dscore, weightvec.pol))
sex.ts.consNEW <- ts(sex.month.cons$mean, frequency = 12, start = c(2017, 1))
sex.ts.consNEW
sex.Wts.Pol.consNEW <- ts(sex.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Pol.consNEW
sex.Wts.consNEW <- ts(sex.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.consNEW
plot(sex.Wts.consNEW)
lines(sex.Wts.Pol.consNEW, col = "cadetblue")
lines(sex.ts.consNEW, col = "coral")

## Gender:
# Female
sex.fem <- sex.femmal[sex.femmal$genderidentity_simp == "female",]
sex.mal <- sex.femmal[sex.femmal$genderidentity_simp == "male",]
sex.month.fem <- ddply(sex.fem, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
sex.ts.femNEW <- ts(sex.month.fem$mean, frequency = 12, start = c(2017, 1))
sex.ts.femNEW
sex.Wts.Gen.femNEW <- ts(sex.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Gen.femNEW
sex.Wts.femNEW <- ts(sex.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.femNEW
plot(sex.Wts.femNEW)
lines(sex.Wts.Gen.femNEW, col = "cadetblue")
lines(sex.ts.femNEW, col = "coral")

# Males
sex.month.mal <- ddply(sex.mal, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
sex.ts.malNEW <- ts(sex.month.mal$mean, frequency = 12, start = c(2017, 1))
sex.ts.malNEW
sex.Wts.Gen.malNEW <- ts(sex.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Gen.malNEW
sex.Wts.malNEW <- ts(sex.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.malNEW
plot(sex.Wts.malNEW)
lines(sex.Wts.Gen.malNEW, col = "cadetblue")
lines(sex.ts.malNEW, col = "coral")

## Education:
# < College
sex.ncol <- sex.ncolcol[sex.ncolcol$edu.grp == "nocollege",]
sex.col <- sex.ncolcol[sex.ncolcol$edu.grp == "college",]
sex.month.ncol <- ddply(sex.ncol, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.edu.mean = weighted.mean(dscore, weightvec.edu))

sex.ts.ncolNEW <- ts(sex.month.ncol$mean, frequency = 12, start = c(2017, 1))
sex.ts.ncolNEW
sex.Wts.Edu.ncolNEW <- ts(sex.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Edu.ncolNEW
sex.Wts.ncolNEW <- ts(sex.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.ncolNEW
plot(sex.Wts.ncolNEW)
lines(sex.Wts.Edu.ncolNEW, col = "cadetblue")
lines(sex.ts.ncolNEW, col = "coral")
# > College
sex.month.col <- ddply(sex.col, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

sex.ts.colNEW <- ts(sex.month.col$mean, frequency = 12, start = c(2017, 1))
sex.ts.colNEW
sex.Wts.Edu.colNEW <- ts(sex.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Edu.colNEW
sex.Wts.colNEW <- ts(sex.month.col$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.colNEW
plot(sex.Wts.colNEW)
lines(sex.Wts.Edu.colNEW, col = "cadetblue")
lines(sex.ts.colNEW, col = "coral")

## Race:
# White
sex.white <- sex.whblkasi[sex.whblkasi$racesimple == "White",]
sex.black <- sex.whblkasi[sex.whblkasi$racesimple == "Black",]
sex.asian <- sex.whblkasi[sex.whblkasi$racesimple == "Asian",]

sex.month.white <- ddply(sex.white, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

sex.ts.whiteNEW <- ts(sex.month.white$mean, frequency = 12, start = c(2017, 1))
sex.ts.whiteNEW
sex.Wts.Race.whiteNEW <- ts(sex.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Race.whiteNEW
sex.Wts.whiteNEW <- ts(sex.month.white$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.whiteNEW
plot(sex.Wts.whiteNEW)
lines(sex.Wts.Race.whiteNEW, col = "cadetblue")
lines(sex.ts.whiteNEW, col = "coral")

# Black
sex.month.black <- ddply(sex.black, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

sex.ts.blackNEW <- ts(sex.month.black$mean, frequency = 12, start = c(2017, 1))
sex.ts.blackNEW
sex.Wts.Race.blackNEW <- ts(sex.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Race.blackNEW
sex.Wts.blackNEW <- ts(sex.month.black$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.blackNEW
plot(sex.Wts.blackNEW)
lines(sex.Wts.Race.blackNEW, col = "cadetblue")
lines(sex.ts.blackNEW, col = "coral")

# Asian
sex.month.asian <- ddply(sex.asian, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

sex.ts.asianNEW <- ts(sex.month.asian$mean, frequency = 12, start = c(2017, 1))
sex.ts.asianNEW
sex.Wts.Race.asianNEW <- ts(sex.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Race.asianNEW
sex.Wts.asianNEW <- ts(sex.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.asianNEW
plot(sex.Wts.asianNEW)
lines(sex.Wts.Race.asianNEW, col = "cadetblue")
lines(sex.ts.asianNEW, col = "coral")


## Age:
sex.ygmidold$weightvec.age <- as.numeric(sex.ygmidold$weightvec.age)
# Young
sex.young <- sex.ygmidold[sex.ygmidold$age.grp == "young",]
sex.middle <- sex.ygmidold[sex.ygmidold$age.grp == "middle",]
sex.old <- sex.ygmidold[sex.ygmidold$age.grp == "old",]

sex.month.young <- ddply(sex.young, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

sex.ts.youngNEW <- ts(sex.month.young$mean, frequency = 12, start = c(2017, 1))
sex.ts.youngNEW
sex.Wts.Age.youngNEW <- ts(sex.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Age.youngNEW
sex.Wts.youngNEW <- ts(sex.month.young$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.youngNEW
plot(sex.Wts.youngNEW)
lines(sex.Wts.Age.youngNEW, col = "cadetblue")
lines(sex.ts.youngNEW, col = "coral")

# Middle
sex.month.middle <- ddply(sex.middle, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.age.mean = weighted.mean(dscore, weightvec.age))

sex.ts.middleNEW <- ts(sex.month.middle$mean, frequency = 12, start = c(2017, 1))
sex.ts.middleNEW
sex.Wts.Age.middleNEW <- ts(sex.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Age.middleNEW
sex.Wts.middleNEW <- ts(sex.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.middleNEW
plot(sex.Wts.middleNEW)
lines(sex.Wts.Age.middleNEW, col = "cadetblue")
lines(sex.ts.middleNEW, col = "coral")

# Old
sex.month.old <- ddply(sex.old, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.age.mean = weighted.mean(dscore, weightvec.age))

sex.ts.oldNEW <- ts(sex.month.old$mean, frequency = 12, start = c(2017, 1))
sex.ts.oldNEW
sex.Wts.Age.oldNEW <- ts(sex.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Age.oldNEW
sex.Wts.oldNEW <- ts(sex.month.old$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.oldNEW
plot(sex.Wts.oldNEW)
lines(sex.Wts.Age.oldNEW, col = "cadetblue")
lines(sex.ts.oldNEW, col = "coral")

## Religion:
sex.allreligion$relisimple <- car::recode(sex.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
sex.christ <- sex.allreligion[sex.allreligion$relisimple == "christian",]
sex.other <- sex.allreligion[sex.allreligion$relisimple == "other",]
sex.nonrel <- sex.allreligion[sex.allreligion$relisimple == "nonrel",]
sex.jew <- sex.allreligion[sex.allreligion$relisimple == "jewish",]

sex.month.christian <- ddply(sex.christ, .(month.cent), summarize, 
                              mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                              weight.rel.mean = weighted.mean(dscore, weightvec.religion))

sex.ts.christianNEW <- ts(sex.month.christian$mean, frequency = 12, start = c(2017, 1))
sex.ts.christianNEW
sex.Wts.Rel.christianNEW <- ts(sex.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Rel.christianNEW
sex.Wts.christianNEW <- ts(sex.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.christianNEW
plot(sex.Wts.christianNEW)
lines(sex.Wts.Rel.christianNEW, col = "cadetblue")
lines(sex.ts.christianNEW, col = "coral")

# other
sex.month.other <- ddply(sex.other, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))

sex.ts.otherNEW <- ts(sex.month.other$mean, frequency = 12, start = c(2017, 1))
sex.ts.otherNEW
sex.Wts.Rel.otherNEW <- ts(sex.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Rel.otherNEW
sex.Wts.otherNEW <- ts(sex.month.other$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.otherNEW
plot(sex.Wts.otherNEW)
lines(sex.Wts.Rel.otherNEW, col = "cadetblue")
lines(sex.ts.otherNEW, col = "coral")

# non
sex.month.non <- ddply(sex.nonrel, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.rel.mean = weighted.mean(dscore, weightvec.religion))

sex.ts.nonNEW <- ts(sex.month.non$mean, frequency = 12, start = c(2017, 1))
sex.ts.nonNEW
sex.Wts.Rel.nonNEW <- ts(sex.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Rel.nonNEW
sex.Wts.nonNEW <- ts(sex.month.non$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.nonNEW
plot(sex.Wts.nonNEW)
lines(sex.Wts.Rel.nonNEW, col = "cadetblue")
lines(sex.ts.nonNEW, col = "coral")

# jewish
sex.month.jewish <- ddply(sex.jew, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.rel.mean = weighted.mean(dscore, weightvec.religion))
sex.ts.jewishNEW <- ts(sex.month.jewish$mean, frequency = 12, start = c(2017, 1))
sex.ts.jewishNEW
sex.Wts.Rel.jewishNEW <- ts(sex.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Rel.jewishNEW
sex.Wts.jewishNEW <- ts(sex.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.jewishNEW
plot(sex.Wts.jewishNEW)
lines(sex.Wts.Rel.jewishNEW, col = "cadetblue")
lines(sex.ts.jewishNEW, col = "coral")

## Sexuality
table(sex.sexuality$sexuality_simp)
sex.straight <- sex.sexuality[sex.sexuality$sexuality_simp == "Straight",]
sex.lesgay <- sex.sexuality[sex.sexuality$sexuality_simp == "Lesbian or Gay",]
sex.other <- sex.sexuality[sex.sexuality$sexuality_simp == "Other",]

# Straight
sex.month.straight <- ddply(sex.straight, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.age.mean = weighted.mean(dscore, weightvec.sexori))

sex.ts.straightNEW <- ts(sex.month.straight$mean, frequency = 12, start = c(2017, 1))
sex.ts.straightNEW
sex.Wts.Sex.straightNEW <- ts(sex.month.straight$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Sex.straightNEW
sex.Wts.straightNEW <- ts(sex.month.straight$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.straightNEW
plot(sex.Wts.straightNEW)
lines(sex.Wts.Sex.straightNEW, col = "cadetblue")
lines(sex.ts.straightNEW, col = "coral")

# Lesbian/Gay
sex.month.lesgay <- ddply(sex.lesgay, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.sexori))

sex.ts.lesgayNEW <- ts(sex.month.lesgay$mean, frequency = 12, start = c(2017, 1))
sex.ts.lesgayNEW
sex.Wts.Sex.lesgayNEW <- ts(sex.month.lesgay$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Sex.lesgayNEW
sex.Wts.lesgayNEW <- ts(sex.month.lesgay$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.lesgayNEW
plot(sex.Wts.lesgayNEW)
lines(sex.Wts.Sex.lesgayNEW, col = "cadetblue")
lines(sex.ts.lesgayNEW, col = "coral")

# Other
sex.month.other <- ddply(sex.other, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.age.mean = weighted.mean(dscore, weightvec.sexori))

sex.ts.otherNEW <- ts(sex.month.other$mean, frequency = 12, start = c(2017, 1))
sex.ts.otherNEW
sex.Wts.Sex.otherNEW <- ts(sex.month.other$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.Wts.Sex.otherNEW
sex.Wts.otherNEW <- ts(sex.month.other$weight.mean, frequency = 12, start = c(2017, 1))
sex.Wts.otherNEW
plot(sex.Wts.otherNEW)
lines(sex.Wts.Sex.otherNEW, col = "cadetblue")
lines(sex.ts.otherNEW, col = "coral")




## EXPLICIT Sexuality ----
## Politics:
# Liberals
sex.month.E.lib <- ddply(sex.liberals, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

sex.Ets.libNEW <- ts(sex.month.E.lib$mean, frequency = 12, start = c(2017, 1))
sex.Ets.libNEW
sex.WEts.Pol.libNEW <- ts(sex.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Pol.libNEW
sex.WEts.libNEW <- ts(sex.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.libNEW
plot(sex.WEts.libNEW)
lines(sex.WEts.Pol.libNEW, col = "cadetblue")
lines(sex.Ets.libNEW, col = "coral")
# Conservatives
sex.month.E.cons <- ddply(sex.conserv, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
sex.Ets.consNEW <- ts(sex.month.E.cons$mean, frequency = 12, start = c(2017, 1))
sex.Ets.consNEW
sex.WEts.Pol.consNEW <- ts(sex.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Pol.consNEW
sex.WEts.consNEW <- ts(sex.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.consNEW
plot(sex.WEts.consNEW)
lines(sex.WEts.Pol.consNEW, col = "cadetblue")
lines(sex.Ets.consNEW, col = "coral")

## Gender:
# Female
sex.month.E.fem <- ddply(sex.fem, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
sex.Ets.femNEW <- ts(sex.month.E.fem$mean, frequency = 12, start = c(2017, 1))
sex.Ets.femNEW
sex.WEts.Gen.femNEW <- ts(sex.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Gen.femNEW
sex.WEts.femNEW <- ts(sex.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.femNEW
plot(sex.WEts.femNEW)
lines(sex.WEts.Gen.femNEW, col = "cadetblue")
lines(sex.Ets.femNEW, col = "coral")

# Males
sex.month.E.mal <- ddply(sex.mal, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
sex.Ets.malNEW <- ts(sex.month.E.mal$mean, frequency = 12, start = c(2017, 1))
sex.Ets.malNEW
sex.WEts.Gen.malNEW <- ts(sex.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Gen.malNEW
sex.WEts.malNEW <- ts(sex.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.malNEW
plot(sex.WEts.malNEW)
lines(sex.WEts.Gen.malNEW, col = "cadetblue")
lines(sex.Ets.malNEW, col = "coral")

## Education:
# < College
sex.month.E.ncol <- ddply(sex.ncol, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

sex.Ets.ncolNEW <- ts(sex.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
sex.Ets.ncolNEW
sex.WEts.Edu.ncolNEW <- ts(sex.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Edu.ncolNEW
sex.WEts.ncolNEW <- ts(sex.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.ncolNEW
plot(sex.WEts.ncolNEW)
lines(sex.WEts.Edu.ncolNEW, col = "cadetblue")
lines(sex.Ets.ncolNEW, col = "coral")
# > College
sex.month.E.col <- ddply(sex.col, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

sex.Ets.colNEW <- ts(sex.month.E.col$mean, frequency = 12, start = c(2017, 1))
sex.Ets.colNEW
sex.WEts.Edu.colNEW <- ts(sex.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Edu.colNEW
sex.WEts.colNEW <- ts(sex.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.colNEW
plot(sex.WEts.colNEW)
lines(sex.WEts.Edu.colNEW, col = "cadetblue")
lines(sex.Ets.colNEW, col = "coral")

## Race:
# White
sex.month.E.white <- ddply(sex.white, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

sex.Ets.whiteNEW <- ts(sex.month.E.white$mean, frequency = 12, start = c(2017, 1))
sex.Ets.whiteNEW
sex.WEts.Race.whiteNEW <- ts(sex.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Race.whiteNEW
sex.WEts.whiteNEW <- ts(sex.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.whiteNEW
plot(sex.WEts.whiteNEW)
lines(sex.WEts.Race.whiteNEW, col = "cadetblue")
lines(sex.Ets.whiteNEW, col = "coral")

# Black
sex.month.E.black <- ddply(sex.black, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

sex.Ets.blackNEW <- ts(sex.month.E.black$mean, frequency = 12, start = c(2017, 1))
sex.Ets.blackNEW
sex.WEts.Race.blackNEW <- ts(sex.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Race.blackNEW
sex.WEts.blackNEW <- ts(sex.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.blackNEW
plot(sex.WEts.blackNEW)
lines(sex.WEts.Race.blackNEW, col = "cadetblue")
lines(sex.Ets.blackNEW, col = "coral")

# Asian
sex.month.E.asian <- ddply(sex.asian, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

sex.Ets.asianNEW <- ts(sex.month.E.asian$mean, frequency = 12, start = c(2017, 1))
sex.Ets.asianNEW
sex.WEts.Race.asianNEW <- ts(sex.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Race.asianNEW
sex.WEts.asianNEW <- ts(sex.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.asianNEW
plot(sex.WEts.asianNEW)
lines(sex.WEts.Race.asianNEW, col = "cadetblue")
lines(sex.Ets.asianNEW, col = "coral")


## Age:
# Young
sex.month.E.young <- ddply(sex.young, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.age))

sex.Ets.youngNEW <- ts(sex.month.E.young$mean, frequency = 12, start = c(2017, 1))
sex.Ets.youngNEW
sex.WEts.Age.youngNEW <- ts(sex.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Age.youngNEW
sex.WEts.youngNEW <- ts(sex.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.youngNEW
plot(sex.WEts.youngNEW)
lines(sex.WEts.Age.youngNEW, col = "cadetblue")
lines(sex.Ets.youngNEW, col = "coral")

# Middle
sex.month.E.middle <- ddply(sex.middle, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.age.mean = weighted.mean(attnumeric, weightvec.age))

sex.Ets.middleNEW <- ts(sex.month.E.middle$mean, frequency = 12, start = c(2017, 1))
sex.Ets.middleNEW
sex.WEts.Age.middleNEW <- ts(sex.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Age.middleNEW
sex.WEts.middleNEW <- ts(sex.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.middleNEW
plot(sex.WEts.middleNEW)
lines(sex.WEts.Age.middleNEW, col = "cadetblue")
lines(sex.Ets.middleNEW, col = "coral")

# Old
sex.month.E.old <- ddply(sex.old, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))

sex.Ets.oldNEW <- ts(sex.month.E.old$mean, frequency = 12, start = c(2017, 1))
sex.Ets.oldNEW
sex.WEts.Age.oldNEW <- ts(sex.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Age.oldNEW
sex.WEts.oldNEW <- ts(sex.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.oldNEW
plot(sex.WEts.oldNEW)
lines(sex.WEts.Age.oldNEW, col = "cadetblue")
lines(sex.Ets.oldNEW, col = "coral")

## Religion:
# christian
sex.month.E.christian <- ddply(sex.christ, .(month.cent), summarize, 
                                mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                                weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

sex.Ets.christianNEW <- ts(sex.month.E.christian$mean, frequency = 12, start = c(2017, 1))
sex.Ets.christianNEW
sex.WEts.Rel.christianNEW <- ts(sex.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Rel.christianNEW
sex.WEts.christianNEW <- ts(sex.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.christianNEW
plot(sex.WEts.christianNEW)
lines(sex.WEts.Rel.christianNEW, col = "cadetblue")
lines(sex.Ets.christianNEW, col = "coral")

# other
sex.month.E.other <- ddply(sex.other, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

sex.Ets.otherNEW <- ts(sex.month.E.other$mean, frequency = 12, start = c(2017, 1))
sex.Ets.otherNEW
sex.WEts.Rel.otherNEW <- ts(sex.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Rel.otherNEW
sex.WEts.otherNEW <- ts(sex.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.otherNEW
plot(sex.WEts.otherNEW)
lines(sex.WEts.Rel.otherNEW, col = "cadetblue")
lines(sex.Ets.otherNEW, col = "coral")

# non
sex.month.E.non <- ddply(sex.nonrel, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

sex.Ets.nonNEW <- ts(sex.month.E.non$mean, frequency = 12, start = c(2017, 1))
sex.Ets.nonNEW
sex.WEts.Rel.nonNEW <- ts(sex.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Rel.nonNEW
sex.WEts.nonNEW <- ts(sex.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.nonNEW
plot(sex.WEts.nonNEW)
lines(sex.WEts.Rel.nonNEW, col = "cadetblue")
lines(sex.Ets.nonNEW, col = "coral")

# jewish
sex.month.E.jewish <- ddply(sex.jew, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
sex.Ets.jewishNEW <- ts(sex.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
sex.Ets.jewishNEW
sex.WEts.Rel.jewishNEW <- ts(sex.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Rel.jewishNEW
sex.WEts.jewishNEW <- ts(sex.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.jewishNEW
plot(sex.WEts.jewishNEW)
lines(sex.WEts.Rel.jewishNEW, col = "cadetblue")
lines(sex.Ets.jewishNEW, col = "coral")

## Sexuality
# Straight
sex.month.E.straight <- ddply(sex.straight, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.sexori))

sex.Ets.straightNEW <- ts(sex.month.E.straight$mean, frequency = 12, start = c(2017, 1))
sex.Ets.straightNEW
sex.WEts.Sex.straightNEW <- ts(sex.month.E.straight$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Sex.straightNEW
sex.WEts.straightNEW <- ts(sex.month.E.straight$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.straightNEW
plot(sex.WEts.straightNEW)
lines(sex.WEts.Sex.straightNEW, col = "cadetblue")
lines(sex.Ets.straightNEW, col = "coral")

# Lesbian/Gay
sex.month.E.lesgay <- ddply(sex.lesgay, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.sexori))

sex.Ets.lesgayNEW <- ts(sex.month.E.lesgay$mean, frequency = 12, start = c(2017, 1))
sex.Ets.lesgayNEW
sex.WEts.Sex.lesgayNEW <- ts(sex.month.E.lesgay$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Sex.lesgayNEW
sex.WEts.lesgayNEW <- ts(sex.month.E.lesgay$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.lesgayNEW
plot(sex.WEts.lesgayNEW)
lines(sex.WEts.Sex.lesgayNEW, col = "cadetblue")
lines(sex.Ets.lesgayNEW, col = "coral")

# Other
sex.month.E.other <- ddply(sex.other, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.age.mean = weighted.mean(attnumeric, weightvec.sexori))

sex.Ets.otherNEW <- ts(sex.month.E.other$mean, frequency = 12, start = c(2017, 1))
sex.Ets.otherNEW
sex.WEts.Sex.otherNEW <- ts(sex.month.E.other$weight.age.mean, frequency = 12, start = c(2017, 1))
sex.WEts.Sex.otherNEW
sex.WEts.otherNEW <- ts(sex.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
sex.WEts.otherNEW
plot(sex.WEts.otherNEW)
lines(sex.WEts.Sex.otherNEW, col = "cadetblue")
lines(sex.Ets.otherNEW, col = "coral")



### IMPLICIT Age ----
## Politics:
# Liberals
age.liberals <- age.libcons[as.numeric(age.libcons$politicalid.new) > 4,]
age.conserv <- age.libcons[as.numeric(age.libcons$politicalid.new) < 4,]

age.month.lib <- ddply(age.liberals, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.pol.mean = weighted.mean(dscore, weightvec.pol))

age.ts.libNEW <- ts(age.month.lib$mean, frequency = 12, start = c(2017, 1))
age.ts.libNEW
age.Wts.Pol.libNEW <- ts(age.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.Wts.Pol.libNEW
age.Wts.libNEW <- ts(age.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.libNEW
plot(age.Wts.libNEW)
lines(age.Wts.Pol.libNEW, col = "cadetblue")
lines(age.ts.libNEW, col = "coral")
# Conservatives
age.month.cons <- ddply(age.conserv, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))
age.ts.consNEW <- ts(age.month.cons$mean, frequency = 12, start = c(2017, 1))
age.ts.consNEW
age.Wts.Pol.consNEW <- ts(age.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.Wts.Pol.consNEW
age.Wts.consNEW <- ts(age.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.consNEW
plot(age.Wts.consNEW)
lines(age.Wts.Pol.consNEW, col = "cadetblue")
lines(age.ts.consNEW, col = "coral")

## Gender:
# Female
age.fem <- age.femmal[age.femmal$genderidentity_simp == "female",]
age.mal <- age.femmal[age.femmal$genderidentity_simp == "male",]
age.month.fem <- ddply(age.fem, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.gen.mean = weighted.mean(dscore, weightvec.gender))
age.ts.femNEW <- ts(age.month.fem$mean, frequency = 12, start = c(2017, 1))
age.ts.femNEW
age.Wts.Gen.femNEW <- ts(age.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
age.Wts.Gen.femNEW
age.Wts.femNEW <- ts(age.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.femNEW
plot(age.Wts.femNEW)
lines(age.Wts.Gen.femNEW, col = "cadetblue")
lines(age.ts.femNEW, col = "coral")

# Males
age.month.mal <- ddply(age.mal, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.gen.mean = weighted.mean(dscore, weightvec.gender))
age.ts.malNEW <- ts(age.month.mal$mean, frequency = 12, start = c(2017, 1))
age.ts.malNEW
age.Wts.Gen.malNEW <- ts(age.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
age.Wts.Gen.malNEW
age.Wts.malNEW <- ts(age.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.malNEW
plot(age.Wts.malNEW)
lines(age.Wts.Gen.malNEW, col = "cadetblue")
lines(age.ts.malNEW, col = "coral")

## Education:
# < College
age.ncol <- age.ncolcol[age.ncolcol$edu.grp == "nocollege",]
age.col <- age.ncolcol[age.ncolcol$edu.grp == "college",]
age.month.ncol <- ddply(age.ncol, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

age.ts.ncolNEW <- ts(age.month.ncol$mean, frequency = 12, start = c(2017, 1))
age.ts.ncolNEW
age.Wts.Edu.ncolNEW <- ts(age.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
age.Wts.Edu.ncolNEW
age.Wts.ncolNEW <- ts(age.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.ncolNEW
plot(age.Wts.ncolNEW)
lines(age.Wts.Edu.ncolNEW, col = "cadetblue")
lines(age.ts.ncolNEW, col = "coral")
# > College
age.month.col <- ddply(age.col, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.edu.mean = weighted.mean(dscore, weightvec.edu))

age.ts.colNEW <- ts(age.month.col$mean, frequency = 12, start = c(2017, 1))
age.ts.colNEW
age.Wts.Edu.colNEW <- ts(age.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
age.Wts.Edu.colNEW
age.Wts.colNEW <- ts(age.month.col$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.colNEW
plot(age.Wts.colNEW)
lines(age.Wts.Edu.colNEW, col = "cadetblue")
lines(age.ts.colNEW, col = "coral")

## Race:
# White
table(age.whblkasi$racesimple)
age.white <- age.whblkasi[age.whblkasi$racesimple == "White",]
age.black <- age.whblkasi[age.whblkasi$racesimple == "Black",]
age.asian <- age.whblkasi[age.whblkasi$racesimple == "Asian",]

age.month.white <- ddply(age.white, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

age.ts.whiteNEW <- ts(age.month.white$mean, frequency = 12, start = c(2017, 1))
age.ts.whiteNEW
age.Wts.Race.whiteNEW <- ts(age.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
age.Wts.Race.whiteNEW
age.Wts.whiteNEW <- ts(age.month.white$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.whiteNEW
plot(age.Wts.whiteNEW)
lines(age.Wts.Race.whiteNEW, col = "cadetblue")
lines(age.ts.whiteNEW, col = "coral")

# Black
age.month.black <- ddply(age.black, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

age.ts.blackNEW <- ts(age.month.black$mean, frequency = 12, start = c(2017, 1))
age.ts.blackNEW
age.Wts.Race.blackNEW <- ts(age.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
age.Wts.Race.blackNEW
age.Wts.blackNEW <- ts(age.month.black$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.blackNEW
plot(age.Wts.blackNEW)
lines(age.Wts.Race.blackNEW, col = "cadetblue")
lines(age.ts.blackNEW, col = "coral")

# Asian
age.month.asian <- ddply(age.asian, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.race.mean = weighted.mean(dscore, weightvec.race))

age.ts.asianNEW <- ts(age.month.asian$mean, frequency = 12, start = c(2017, 1))
age.ts.asianNEW
age.Wts.Race.asianNEW <- ts(age.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
age.Wts.Race.asianNEW
age.Wts.asianNEW <- ts(age.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.asianNEW
plot(age.Wts.asianNEW)
lines(age.Wts.Race.asianNEW, col = "cadetblue")
lines(age.ts.asianNEW, col = "coral")


## Age:
age.ygmidold$weightvec.age <- as.numeric(age.ygmidold$weightvec.age)
# Young
age.young <- age.ygmidold[age.ygmidold$age.grp == "young",]
age.middle <- age.ygmidold[age.ygmidold$age.grp == "middle",]
age.old <- age.ygmidold[age.ygmidold$age.grp == "old",]

age.month.young <- ddply(age.young, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.age.mean = weighted.mean(dscore, weightvec.age))

age.ts.youngNEW <- ts(age.month.young$mean, frequency = 12, start = c(2017, 1))
age.ts.youngNEW
age.Wts.Age.youngNEW <- ts(age.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
age.Wts.Age.youngNEW
age.Wts.youngNEW <- ts(age.month.young$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.youngNEW
plot(age.Wts.youngNEW)
lines(age.Wts.Age.youngNEW, col = "cadetblue")
lines(age.ts.youngNEW, col = "coral")

# Middle
age.month.middle <- ddply(age.middle, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

age.ts.middleNEW <- ts(age.month.middle$mean, frequency = 12, start = c(2017, 1))
age.ts.middleNEW
age.Wts.Age.middleNEW <- ts(age.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
age.Wts.Age.middleNEW
age.Wts.middleNEW <- ts(age.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.middleNEW
plot(age.Wts.middleNEW)
lines(age.Wts.Age.middleNEW, col = "cadetblue")
lines(age.ts.middleNEW, col = "coral")

# Old
age.month.old <- ddply(age.old, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.age.mean = weighted.mean(dscore, weightvec.age))

age.ts.oldNEW <- ts(age.month.old$mean, frequency = 12, start = c(2017, 1))
age.ts.oldNEW
age.Wts.Age.oldNEW <- ts(age.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
age.Wts.Age.oldNEW
age.Wts.oldNEW <- ts(age.month.old$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.oldNEW
plot(age.Wts.oldNEW)
lines(age.Wts.Age.oldNEW, col = "cadetblue")
lines(age.ts.oldNEW, col = "coral")

## Religion:
age.allreligion$relisimple <- car::recode(age.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
age.christ <- age.allreligion[age.allreligion$relisimple == "christian",]
age.other <- age.allreligion[age.allreligion$relisimple == "other",]
age.nonrel <- age.allreligion[age.allreligion$relisimple == "nonrel",]
age.jew <- age.allreligion[age.allreligion$relisimple == "jewish",]

age.month.christian <- ddply(age.christ, .(month.cent), summarize, 
                             mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                             weight.rel.mean = weighted.mean(dscore, weightvec.religion))

age.ts.christianNEW <- ts(age.month.christian$mean, frequency = 12, start = c(2017, 1))
age.ts.christianNEW
age.Wts.Rel.christianNEW <- ts(age.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.Wts.Rel.christianNEW
age.Wts.christianNEW <- ts(age.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.christianNEW
plot(age.Wts.christianNEW)
lines(age.Wts.Rel.christianNEW, col = "cadetblue")
lines(age.ts.christianNEW, col = "coral")

# other
age.month.other <- ddply(age.other, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.rel.mean = weighted.mean(dscore, weightvec.religion))

age.ts.otherNEW <- ts(age.month.other$mean, frequency = 12, start = c(2017, 1))
age.ts.otherNEW
age.Wts.Rel.otherNEW <- ts(age.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.Wts.Rel.otherNEW
age.Wts.otherNEW <- ts(age.month.other$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.otherNEW
plot(age.Wts.otherNEW)
lines(age.Wts.Rel.otherNEW, col = "cadetblue")
lines(age.ts.otherNEW, col = "coral")

# non
age.month.non <- ddply(age.nonrel, .(month.cent), summarize, 
                       mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                       weight.rel.mean = weighted.mean(dscore, weightvec.religion))

age.ts.nonNEW <- ts(age.month.non$mean, frequency = 12, start = c(2017, 1))
age.ts.nonNEW
age.Wts.Rel.nonNEW <- ts(age.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.Wts.Rel.nonNEW
age.Wts.nonNEW <- ts(age.month.non$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.nonNEW
plot(age.Wts.nonNEW)
lines(age.Wts.Rel.nonNEW, col = "cadetblue")
lines(age.ts.nonNEW, col = "coral")

# jewish
age.month.jewish <- ddply(age.jew, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))
age.ts.jewishNEW <- ts(age.month.jewish$mean, frequency = 12, start = c(2017, 1))
age.ts.jewishNEW
age.Wts.Rel.jewishNEW <- ts(age.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.Wts.Rel.jewishNEW
age.Wts.jewishNEW <- ts(age.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
age.Wts.jewishNEW
plot(age.Wts.jewishNEW)
lines(age.Wts.Rel.jewishNEW, col = "cadetblue")
lines(age.ts.jewishNEW, col = "coral")


## EXPLICIT Age ----
## Politics:
# Liberals
age.month.E.lib <- ddply(age.liberals, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

age.Ets.libNEW <- ts(age.month.E.lib$mean, frequency = 12, start = c(2017, 1))
age.Ets.libNEW
age.WEts.Pol.libNEW <- ts(age.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.libNEW
age.WEts.libNEW <- ts(age.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.libNEW
plot(age.WEts.libNEW)
lines(age.WEts.Pol.libNEW, col = "cadetblue")
lines(age.Ets.libNEW, col = "coral")
# Conservatives
age.month.E.cons <- ddply(age.conserv, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
age.Ets.consNEW <- ts(age.month.E.cons$mean, frequency = 12, start = c(2017, 1))
age.Ets.consNEW
age.WEts.Pol.consNEW <- ts(age.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
age.WEts.Pol.consNEW
age.WEts.consNEW <- ts(age.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.consNEW
plot(age.WEts.consNEW)
lines(age.WEts.Pol.consNEW, col = "cadetblue")
lines(age.Ets.consNEW, col = "coral")

## Gender:
# Female
age.month.E.fem <- ddply(age.fem, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
age.Ets.femNEW <- ts(age.month.E.fem$mean, frequency = 12, start = c(2017, 1))
age.Ets.femNEW
age.WEts.Gen.femNEW <- ts(age.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
age.WEts.Gen.femNEW
age.WEts.femNEW <- ts(age.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.femNEW
plot(age.WEts.femNEW)
lines(age.WEts.Gen.femNEW, col = "cadetblue")
lines(age.Ets.femNEW, col = "coral")

# Males
age.month.E.mal <- ddply(age.mal, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
age.Ets.malNEW <- ts(age.month.E.mal$mean, frequency = 12, start = c(2017, 1))
age.Ets.malNEW
age.WEts.Gen.malNEW <- ts(age.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
age.WEts.Gen.malNEW
age.WEts.malNEW <- ts(age.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.malNEW
plot(age.WEts.malNEW)
lines(age.WEts.Gen.malNEW, col = "cadetblue")
lines(age.Ets.malNEW, col = "coral")

## Education:
# < College
age.month.E.ncol <- ddply(age.ncol, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

age.Ets.ncolNEW <- ts(age.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
age.Ets.ncolNEW
age.WEts.Edu.ncolNEW <- ts(age.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
age.WEts.Edu.ncolNEW
age.WEts.ncolNEW <- ts(age.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.ncolNEW
plot(age.WEts.ncolNEW)
lines(age.WEts.Edu.ncolNEW, col = "cadetblue")
lines(age.Ets.ncolNEW, col = "coral")
# > College
age.month.E.col <- ddply(age.col, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

age.Ets.colNEW <- ts(age.month.E.col$mean, frequency = 12, start = c(2017, 1))
age.Ets.colNEW
age.WEts.Edu.colNEW <- ts(age.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
age.WEts.Edu.colNEW
age.WEts.colNEW <- ts(age.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.colNEW
plot(age.WEts.colNEW)
lines(age.WEts.Edu.colNEW, col = "cadetblue")
lines(age.Ets.colNEW, col = "coral")

## Race:
# White
age.month.E.white <- ddply(age.white, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

age.Ets.whiteNEW <- ts(age.month.E.white$mean, frequency = 12, start = c(2017, 1))
age.Ets.whiteNEW
age.WEts.Race.whiteNEW <- ts(age.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
age.WEts.Race.whiteNEW
age.WEts.whiteNEW <- ts(age.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.whiteNEW
plot(age.WEts.whiteNEW)
lines(age.WEts.Race.whiteNEW, col = "cadetblue")
lines(age.Ets.whiteNEW, col = "coral")

# Black
age.month.E.black <- ddply(age.black, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

age.Ets.blackNEW <- ts(age.month.E.black$mean, frequency = 12, start = c(2017, 1))
age.Ets.blackNEW
age.WEts.Race.blackNEW <- ts(age.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
age.WEts.Race.blackNEW
age.WEts.blackNEW <- ts(age.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.blackNEW
plot(age.WEts.blackNEW)
lines(age.WEts.Race.blackNEW, col = "cadetblue")
lines(age.Ets.blackNEW, col = "coral")

# Asian
age.month.E.asian <- ddply(age.asian, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.race.mean = weighted.mean(attnumeric, weightvec.race))

age.Ets.asianNEW <- ts(age.month.E.asian$mean, frequency = 12, start = c(2017, 1))
age.Ets.asianNEW
age.WEts.Race.asianNEW <- ts(age.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
age.WEts.Race.asianNEW
age.WEts.asianNEW <- ts(age.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.asianNEW
plot(age.WEts.asianNEW)
lines(age.WEts.Race.asianNEW, col = "cadetblue")
lines(age.Ets.asianNEW, col = "coral")


## Age:
# Young
age.month.E.young <- ddply(age.young, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.age.mean = weighted.mean(attnumeric, weightvec.age))

age.Ets.youngNEW <- ts(age.month.E.young$mean, frequency = 12, start = c(2017, 1))
age.Ets.youngNEW
age.WEts.Age.youngNEW <- ts(age.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Age.youngNEW
age.WEts.youngNEW <- ts(age.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.youngNEW
plot(age.WEts.youngNEW)
lines(age.WEts.Age.youngNEW, col = "cadetblue")
lines(age.Ets.youngNEW, col = "coral")

# Middle
age.month.E.middle <- ddply(age.middle, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.age))

age.Ets.middleNEW <- ts(age.month.E.middle$mean, frequency = 12, start = c(2017, 1))
age.Ets.middleNEW
age.WEts.Age.middleNEW <- ts(age.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Age.middleNEW
age.WEts.middleNEW <- ts(age.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.middleNEW
plot(age.WEts.middleNEW)
lines(age.WEts.Age.middleNEW, col = "cadetblue")
lines(age.Ets.middleNEW, col = "coral")

# Old
age.month.E.old <- ddply(age.old, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.age.mean = weighted.mean(attnumeric, weightvec.age))

age.Ets.oldNEW <- ts(age.month.E.old$mean, frequency = 12, start = c(2017, 1))
age.Ets.oldNEW
age.WEts.Age.oldNEW <- ts(age.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
age.WEts.Age.oldNEW
age.WEts.oldNEW <- ts(age.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.oldNEW
plot(age.WEts.oldNEW)
lines(age.WEts.Age.oldNEW, col = "cadetblue")
lines(age.Ets.oldNEW, col = "coral")

## Religion:
# christian
age.month.E.christian <- ddply(age.christ, .(month.cent), summarize, 
                               mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                               weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

age.Ets.christianNEW <- ts(age.month.E.christian$mean, frequency = 12, start = c(2017, 1))
age.Ets.christianNEW
age.WEts.Rel.christianNEW <- ts(age.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.WEts.Rel.christianNEW
age.WEts.christianNEW <- ts(age.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.christianNEW
plot(age.WEts.christianNEW)
lines(age.WEts.Rel.christianNEW, col = "cadetblue")
lines(age.Ets.christianNEW, col = "coral")

# other
age.month.E.other <- ddply(age.other, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

age.Ets.otherNEW <- ts(age.month.E.other$mean, frequency = 12, start = c(2017, 1))
age.Ets.otherNEW
age.WEts.Rel.otherNEW <- ts(age.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.WEts.Rel.otherNEW
age.WEts.otherNEW <- ts(age.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.otherNEW
plot(age.WEts.otherNEW)
lines(age.WEts.Rel.otherNEW, col = "cadetblue")
lines(age.Ets.otherNEW, col = "coral")

# non
age.month.E.non <- ddply(age.nonrel, .(month.cent), summarize, 
                         mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                         weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

age.Ets.nonNEW <- ts(age.month.E.non$mean, frequency = 12, start = c(2017, 1))
age.Ets.nonNEW
age.WEts.Rel.nonNEW <- ts(age.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.WEts.Rel.nonNEW
age.WEts.nonNEW <- ts(age.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.nonNEW
plot(age.WEts.nonNEW)
lines(age.WEts.Rel.nonNEW, col = "cadetblue")
lines(age.Ets.nonNEW, col = "coral")

# jewish
age.month.E.jewish <- ddply(age.jew, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
age.Ets.jewishNEW <- ts(age.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
age.Ets.jewishNEW
age.WEts.Rel.jewishNEW <- ts(age.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
age.WEts.Rel.jewishNEW
age.WEts.jewishNEW <- ts(age.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
age.WEts.jewishNEW
plot(age.WEts.jewishNEW)
lines(age.WEts.Rel.jewishNEW, col = "cadetblue")
lines(age.Ets.jewishNEW, col = "coral")


### IMPLICIT Disability ----
## Politics:
# Liberals
dis.liberals <- dis.libcons[as.numeric(dis.libcons$politicalid.new) > 4,]
dis.conserv <- dis.libcons[as.numeric(dis.libcons$politicalid.new) < 4,]

dis.month.lib <- ddply(dis.liberals, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))

dis.ts.libNEW <- ts(dis.month.lib$mean, frequency = 12, start = c(2017, 1))
dis.ts.libNEW
dis.Wts.Pol.libNEW <- ts(dis.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Pol.libNEW
dis.Wts.libNEW <- ts(dis.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.libNEW
plot(dis.Wts.libNEW)
lines(dis.Wts.Pol.libNEW, col = "cadetblue")
lines(dis.ts.libNEW, col = "coral")
# Conservatives
dis.month.cons <- ddply(dis.conserv, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.pol.mean = weighted.mean(dscore, weightvec.pol))
dis.ts.consNEW <- ts(dis.month.cons$mean, frequency = 12, start = c(2017, 1))
dis.ts.consNEW
dis.Wts.Pol.consNEW <- ts(dis.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Pol.consNEW
dis.Wts.consNEW <- ts(dis.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.consNEW
plot(dis.Wts.consNEW)
lines(dis.Wts.Pol.consNEW, col = "cadetblue")
lines(dis.ts.consNEW, col = "coral")

## Gender:
# Female
dis.fem <- dis.femmal[dis.femmal$genderidentity_simp == "female",]
dis.mal <- dis.femmal[dis.femmal$genderidentity_simp == "male",]
dis.month.fem <- ddply(dis.fem, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
dis.ts.femNEW <- ts(dis.month.fem$mean, frequency = 12, start = c(2017, 1))
dis.ts.femNEW
dis.Wts.Gen.femNEW <- ts(dis.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Gen.femNEW
dis.Wts.femNEW <- ts(dis.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.femNEW
plot(dis.Wts.femNEW)
lines(dis.Wts.Gen.femNEW, col = "cadetblue")
lines(dis.ts.femNEW, col = "coral")

# Males
dis.month.mal <- ddply(dis.mal, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
dis.ts.malNEW <- ts(dis.month.mal$mean, frequency = 12, start = c(2017, 1))
dis.ts.malNEW
dis.Wts.Gen.malNEW <- ts(dis.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Gen.malNEW
dis.Wts.malNEW <- ts(dis.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.malNEW
plot(dis.Wts.malNEW)
lines(dis.Wts.Gen.malNEW, col = "cadetblue")
lines(dis.ts.malNEW, col = "coral")

## Education:
# < College
dis.ncol <- dis.ncolcol[dis.ncolcol$edu.grp == "nocollege",]
dis.col <- dis.ncolcol[dis.ncolcol$edu.grp == "college",]
dis.month.ncol <- ddply(dis.ncol, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.edu.mean = weighted.mean(dscore, weightvec.edu))

dis.ts.ncolNEW <- ts(dis.month.ncol$mean, frequency = 12, start = c(2017, 1))
dis.ts.ncolNEW
dis.Wts.Edu.ncolNEW <- ts(dis.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Edu.ncolNEW
dis.Wts.ncolNEW <- ts(dis.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.ncolNEW
plot(dis.Wts.ncolNEW)
lines(dis.Wts.Edu.ncolNEW, col = "cadetblue")
lines(dis.ts.ncolNEW, col = "coral")
# > College
dis.month.col <- ddply(dis.col, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

dis.ts.colNEW <- ts(dis.month.col$mean, frequency = 12, start = c(2017, 1))
dis.ts.colNEW
dis.Wts.Edu.colNEW <- ts(dis.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Edu.colNEW
dis.Wts.colNEW <- ts(dis.month.col$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.colNEW
plot(dis.Wts.colNEW)
lines(dis.Wts.Edu.colNEW, col = "cadetblue")
lines(dis.ts.colNEW, col = "coral")

## Race:
# White
dis.white <- dis.whblkasi[dis.whblkasi$racesimple == "White",]
dis.black <- dis.whblkasi[dis.whblkasi$racesimple == "Black",]
dis.asian <- dis.whblkasi[dis.whblkasi$racesimple == "Asian",]

dis.month.white <- ddply(dis.white, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

dis.ts.whiteNEW <- ts(dis.month.white$mean, frequency = 12, start = c(2017, 1))
dis.ts.whiteNEW
dis.Wts.Race.whiteNEW <- ts(dis.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Race.whiteNEW
dis.Wts.whiteNEW <- ts(dis.month.white$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.whiteNEW
plot(dis.Wts.whiteNEW)
lines(dis.Wts.Race.whiteNEW, col = "cadetblue")
lines(dis.ts.whiteNEW, col = "coral")

# Black
dis.month.black <- ddply(dis.black, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

dis.ts.blackNEW <- ts(dis.month.black$mean, frequency = 12, start = c(2017, 1))
dis.ts.blackNEW
dis.Wts.Race.blackNEW <- ts(dis.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Race.blackNEW
dis.Wts.blackNEW <- ts(dis.month.black$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.blackNEW
plot(dis.Wts.blackNEW)
lines(dis.Wts.Race.blackNEW, col = "cadetblue")
lines(dis.ts.blackNEW, col = "coral")

# Asian
dis.month.asian <- ddply(dis.asian, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

dis.ts.asianNEW <- ts(dis.month.asian$mean, frequency = 12, start = c(2017, 1))
dis.ts.asianNEW
dis.Wts.Race.asianNEW <- ts(dis.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Race.asianNEW
dis.Wts.asianNEW <- ts(dis.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.asianNEW
plot(dis.Wts.asianNEW)
lines(dis.Wts.Race.asianNEW, col = "cadetblue")
lines(dis.ts.asianNEW, col = "coral")


## Age:
dis.ygmidold$weightvec.age <- as.numeric(dis.ygmidold$weightvec.age)
# Young
dis.young <- dis.ygmidold[dis.ygmidold$age.grp == "young",]
dis.middle <- dis.ygmidold[dis.ygmidold$age.grp == "middle",]
dis.old <- dis.ygmidold[dis.ygmidold$age.grp == "old",]

dis.month.young <- ddply(dis.young, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

dis.ts.youngNEW <- ts(dis.month.young$mean, frequency = 12, start = c(2017, 1))
dis.ts.youngNEW
dis.Wts.Age.youngNEW <- ts(dis.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Age.youngNEW
dis.Wts.youngNEW <- ts(dis.month.young$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.youngNEW
plot(dis.Wts.youngNEW)
lines(dis.Wts.Age.youngNEW, col = "cadetblue")
lines(dis.ts.youngNEW, col = "coral")

# Middle
dis.month.middle <- ddply(dis.middle, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.age.mean = weighted.mean(dscore, weightvec.age))

dis.ts.middleNEW <- ts(dis.month.middle$mean, frequency = 12, start = c(2017, 1))
dis.ts.middleNEW
dis.Wts.Age.middleNEW <- ts(dis.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Age.middleNEW
dis.Wts.middleNEW <- ts(dis.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.middleNEW
plot(dis.Wts.middleNEW)
lines(dis.Wts.Age.middleNEW, col = "cadetblue")
lines(dis.ts.middleNEW, col = "coral")

# Old
dis.month.old <- ddply(dis.old, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.age.mean = weighted.mean(dscore, weightvec.age))

dis.ts.oldNEW <- ts(dis.month.old$mean, frequency = 12, start = c(2017, 1))
dis.ts.oldNEW
dis.Wts.Age.oldNEW <- ts(dis.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Age.oldNEW
dis.Wts.oldNEW <- ts(dis.month.old$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.oldNEW
plot(dis.Wts.oldNEW)
lines(dis.Wts.Age.oldNEW, col = "cadetblue")
lines(dis.ts.oldNEW, col = "coral")

## Religion:
dis.allreligion$relisimple <- car::recode(dis.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
dis.christ <- dis.allreligion[dis.allreligion$relisimple == "christian",]
dis.other <- dis.allreligion[dis.allreligion$relisimple == "other",]
dis.nonrel <- dis.allreligion[dis.allreligion$relisimple == "nonrel",]
dis.jew <- dis.allreligion[dis.allreligion$relisimple == "jewish",]

dis.month.christian <- ddply(dis.christ, .(month.cent), summarize, 
                              mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                              weight.rel.mean = weighted.mean(dscore, weightvec.religion))

dis.ts.christianNEW <- ts(dis.month.christian$mean, frequency = 12, start = c(2017, 1))
dis.ts.christianNEW
dis.Wts.Rel.christianNEW <- ts(dis.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Rel.christianNEW
dis.Wts.christianNEW <- ts(dis.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.christianNEW
plot(dis.Wts.christianNEW)
lines(dis.Wts.Rel.christianNEW, col = "cadetblue")
lines(dis.ts.christianNEW, col = "coral")

# other
dis.month.other <- ddply(dis.other, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))

dis.ts.otherNEW <- ts(dis.month.other$mean, frequency = 12, start = c(2017, 1))
dis.ts.otherNEW
dis.Wts.Rel.otherNEW <- ts(dis.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Rel.otherNEW
dis.Wts.otherNEW <- ts(dis.month.other$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.otherNEW
plot(dis.Wts.otherNEW)
lines(dis.Wts.Rel.otherNEW, col = "cadetblue")
lines(dis.ts.otherNEW, col = "coral")

# non
dis.month.non <- ddply(dis.nonrel, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.rel.mean = weighted.mean(dscore, weightvec.religion))

dis.ts.nonNEW <- ts(dis.month.non$mean, frequency = 12, start = c(2017, 1))
dis.ts.nonNEW
dis.Wts.Rel.nonNEW <- ts(dis.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Rel.nonNEW
dis.Wts.nonNEW <- ts(dis.month.non$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.nonNEW
plot(dis.Wts.nonNEW)
lines(dis.Wts.Rel.nonNEW, col = "cadetblue")
lines(dis.ts.nonNEW, col = "coral")

# jewish
dis.month.jewish <- ddply(dis.jew, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.rel.mean = weighted.mean(dscore, weightvec.religion))
dis.ts.jewishNEW <- ts(dis.month.jewish$mean, frequency = 12, start = c(2017, 1))
dis.ts.jewishNEW
dis.Wts.Rel.jewishNEW <- ts(dis.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.Wts.Rel.jewishNEW
dis.Wts.jewishNEW <- ts(dis.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
dis.Wts.jewishNEW
plot(dis.Wts.jewishNEW)
lines(dis.Wts.Rel.jewishNEW, col = "cadetblue")
lines(dis.ts.jewishNEW, col = "coral")


## EXPLICIT Disability ----
## Politics:
# Liberals
dis.month.E.lib <- ddply(dis.liberals, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

dis.Ets.libNEW <- ts(dis.month.E.lib$mean, frequency = 12, start = c(2017, 1))
dis.Ets.libNEW
dis.WEts.Pol.libNEW <- ts(dis.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Pol.libNEW
dis.WEts.libNEW <- ts(dis.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.libNEW
plot(dis.WEts.libNEW)
lines(dis.WEts.Pol.libNEW, col = "cadetblue")
lines(dis.Ets.libNEW, col = "coral")
# Conservatives
dis.month.E.cons <- ddply(dis.conserv, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
dis.Ets.consNEW <- ts(dis.month.E.cons$mean, frequency = 12, start = c(2017, 1))
dis.Ets.consNEW
dis.WEts.Pol.consNEW <- ts(dis.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Pol.consNEW
dis.WEts.consNEW <- ts(dis.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.consNEW
plot(dis.WEts.consNEW)
lines(dis.WEts.Pol.consNEW, col = "cadetblue")
lines(dis.Ets.consNEW, col = "coral")

## Gender:
# Female
dis.month.E.fem <- ddply(dis.fem, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
dis.Ets.femNEW <- ts(dis.month.E.fem$mean, frequency = 12, start = c(2017, 1))
dis.Ets.femNEW
dis.WEts.Gen.femNEW <- ts(dis.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Gen.femNEW
dis.WEts.femNEW <- ts(dis.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.femNEW
plot(dis.WEts.femNEW)
lines(dis.WEts.Gen.femNEW, col = "cadetblue")
lines(dis.Ets.femNEW, col = "coral")

# Males
dis.month.E.mal <- ddply(dis.mal, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
dis.Ets.malNEW <- ts(dis.month.E.mal$mean, frequency = 12, start = c(2017, 1))
dis.Ets.malNEW
dis.WEts.Gen.malNEW <- ts(dis.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Gen.malNEW
dis.WEts.malNEW <- ts(dis.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.malNEW
plot(dis.WEts.malNEW)
lines(dis.WEts.Gen.malNEW, col = "cadetblue")
lines(dis.Ets.malNEW, col = "coral")

## Education:
# < College
dis.month.E.ncol <- ddply(dis.ncol, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

dis.Ets.ncolNEW <- ts(dis.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
dis.Ets.ncolNEW
dis.WEts.Edu.ncolNEW <- ts(dis.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Edu.ncolNEW
dis.WEts.ncolNEW <- ts(dis.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.ncolNEW
plot(dis.WEts.ncolNEW)
lines(dis.WEts.Edu.ncolNEW, col = "cadetblue")
lines(dis.Ets.ncolNEW, col = "coral")
# > College
dis.month.E.col <- ddply(dis.col, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

dis.Ets.colNEW <- ts(dis.month.E.col$mean, frequency = 12, start = c(2017, 1))
dis.Ets.colNEW
dis.WEts.Edu.colNEW <- ts(dis.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Edu.colNEW
dis.WEts.colNEW <- ts(dis.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.colNEW
plot(dis.WEts.colNEW)
lines(dis.WEts.Edu.colNEW, col = "cadetblue")
lines(dis.Ets.colNEW, col = "coral")

## Race:
# White
dis.month.E.white <- ddply(dis.white, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

dis.Ets.whiteNEW <- ts(dis.month.E.white$mean, frequency = 12, start = c(2017, 1))
dis.Ets.whiteNEW
dis.WEts.Race.whiteNEW <- ts(dis.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Race.whiteNEW
dis.WEts.whiteNEW <- ts(dis.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.whiteNEW
plot(dis.WEts.whiteNEW)
lines(dis.WEts.Race.whiteNEW, col = "cadetblue")
lines(dis.Ets.whiteNEW, col = "coral")

# Black
dis.month.E.black <- ddply(dis.black, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

dis.Ets.blackNEW <- ts(dis.month.E.black$mean, frequency = 12, start = c(2017, 1))
dis.Ets.blackNEW
dis.WEts.Race.blackNEW <- ts(dis.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Race.blackNEW
dis.WEts.blackNEW <- ts(dis.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.blackNEW
plot(dis.WEts.blackNEW)
lines(dis.WEts.Race.blackNEW, col = "cadetblue")
lines(dis.Ets.blackNEW, col = "coral")

# Asian
dis.month.E.asian <- ddply(dis.asian, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

dis.Ets.asianNEW <- ts(dis.month.E.asian$mean, frequency = 12, start = c(2017, 1))
dis.Ets.asianNEW
dis.WEts.Race.asianNEW <- ts(dis.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Race.asianNEW
dis.WEts.asianNEW <- ts(dis.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.asianNEW
plot(dis.WEts.asianNEW)
lines(dis.WEts.Race.asianNEW, col = "cadetblue")
lines(dis.Ets.asianNEW, col = "coral")


## Age:
# Young
dis.month.E.young <- ddply(dis.young, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.age))

dis.Ets.youngNEW <- ts(dis.month.E.young$mean, frequency = 12, start = c(2017, 1))
dis.Ets.youngNEW
dis.WEts.Age.youngNEW <- ts(dis.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Age.youngNEW
dis.WEts.youngNEW <- ts(dis.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.youngNEW
plot(dis.WEts.youngNEW)
lines(dis.WEts.Age.youngNEW, col = "cadetblue")
lines(dis.Ets.youngNEW, col = "coral")

# Middle
dis.month.E.middle <- ddply(dis.middle, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.age.mean = weighted.mean(attnumeric, weightvec.age))

dis.Ets.middleNEW <- ts(dis.month.E.middle$mean, frequency = 12, start = c(2017, 1))
dis.Ets.middleNEW
dis.WEts.Age.middleNEW <- ts(dis.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Age.middleNEW
dis.WEts.middleNEW <- ts(dis.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.middleNEW
plot(dis.WEts.middleNEW)
lines(dis.WEts.Age.middleNEW, col = "cadetblue")
lines(dis.Ets.middleNEW, col = "coral")

# Old
dis.month.E.old <- ddply(dis.old, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))

dis.Ets.oldNEW <- ts(dis.month.E.old$mean, frequency = 12, start = c(2017, 1))
dis.Ets.oldNEW
dis.WEts.Age.oldNEW <- ts(dis.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Age.oldNEW
dis.WEts.oldNEW <- ts(dis.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.oldNEW
plot(dis.WEts.oldNEW)
lines(dis.WEts.Age.oldNEW, col = "cadetblue")
lines(dis.Ets.oldNEW, col = "coral")

## Religion:
# christian
dis.month.E.christian <- ddply(dis.christ, .(month.cent), summarize, 
                                mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                                weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

dis.Ets.christianNEW <- ts(dis.month.E.christian$mean, frequency = 12, start = c(2017, 1))
dis.Ets.christianNEW
dis.WEts.Rel.christianNEW <- ts(dis.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Rel.christianNEW
dis.WEts.christianNEW <- ts(dis.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.christianNEW
plot(dis.WEts.christianNEW)
lines(dis.WEts.Rel.christianNEW, col = "cadetblue")
lines(dis.Ets.christianNEW, col = "coral")

# other
dis.month.E.other <- ddply(dis.other, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

dis.Ets.otherNEW <- ts(dis.month.E.other$mean, frequency = 12, start = c(2017, 1))
dis.Ets.otherNEW
dis.WEts.Rel.otherNEW <- ts(dis.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Rel.otherNEW
dis.WEts.otherNEW <- ts(dis.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.otherNEW
plot(dis.WEts.otherNEW)
lines(dis.WEts.Rel.otherNEW, col = "cadetblue")
lines(dis.Ets.otherNEW, col = "coral")

# non
dis.month.E.non <- ddply(dis.nonrel, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

dis.Ets.nonNEW <- ts(dis.month.E.non$mean, frequency = 12, start = c(2017, 1))
dis.Ets.nonNEW
dis.WEts.Rel.nonNEW <- ts(dis.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Rel.nonNEW
dis.WEts.nonNEW <- ts(dis.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.nonNEW
plot(dis.WEts.nonNEW)
lines(dis.WEts.Rel.nonNEW, col = "cadetblue")
lines(dis.Ets.nonNEW, col = "coral")

# jewish
dis.month.E.jewish <- ddply(dis.jew, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
dis.Ets.jewishNEW <- ts(dis.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
dis.Ets.jewishNEW
dis.WEts.Rel.jewishNEW <- ts(dis.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
dis.WEts.Rel.jewishNEW
dis.WEts.jewishNEW <- ts(dis.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
dis.WEts.jewishNEW
plot(dis.WEts.jewishNEW)
lines(dis.WEts.Rel.jewishNEW, col = "cadetblue")
lines(dis.Ets.jewishNEW, col = "coral")


### IMPLICIT Weight ----
## Politics:
# Liberals
weight.liberals <- weight.libcons[as.numeric(weight.libcons$politicalid.new) > 4,]
weight.conserv <- weight.libcons[as.numeric(weight.libcons$politicalid.new) < 4,]

weight.month.lib <- ddply(weight.liberals, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.pol.mean = weighted.mean(dscore, weightvec.pol))

weight.ts.libNEW <- ts(weight.month.lib$mean, frequency = 12, start = c(2017, 1))
weight.ts.libNEW
weight.Wts.Pol.libNEW <- ts(weight.month.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Pol.libNEW
weight.Wts.libNEW <- ts(weight.month.lib$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.libNEW
plot(weight.Wts.libNEW)
lines(weight.Wts.Pol.libNEW, col = "cadetblue")
lines(weight.ts.libNEW, col = "coral")
# Conservatives
weight.month.cons <- ddply(weight.conserv, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.pol.mean = weighted.mean(dscore, weightvec.pol))
weight.ts.consNEW <- ts(weight.month.cons$mean, frequency = 12, start = c(2017, 1))
weight.ts.consNEW
weight.Wts.Pol.consNEW <- ts(weight.month.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Pol.consNEW
weight.Wts.consNEW <- ts(weight.month.cons$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.consNEW
plot(weight.Wts.consNEW)
lines(weight.Wts.Pol.consNEW, col = "cadetblue")
lines(weight.ts.consNEW, col = "coral")

## Gender:
# Female
weight.fem <- weight.femmal[weight.femmal$genderidentity_simp == "female",]
weight.mal <- weight.femmal[weight.femmal$genderidentity_simp == "male",]
weight.month.fem <- ddply(weight.fem, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
weight.ts.femNEW <- ts(weight.month.fem$mean, frequency = 12, start = c(2017, 1))
weight.ts.femNEW
weight.Wts.Gen.femNEW <- ts(weight.month.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Gen.femNEW
weight.Wts.femNEW <- ts(weight.month.fem$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.femNEW
plot(weight.Wts.femNEW)
lines(weight.Wts.Gen.femNEW, col = "cadetblue")
lines(weight.ts.femNEW, col = "coral")

# Males
weight.month.mal <- ddply(weight.mal, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.gen.mean = weighted.mean(dscore, weightvec.gender))
weight.ts.malNEW <- ts(weight.month.mal$mean, frequency = 12, start = c(2017, 1))
weight.ts.malNEW
weight.Wts.Gen.malNEW <- ts(weight.month.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Gen.malNEW
weight.Wts.malNEW <- ts(weight.month.mal$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.malNEW
plot(weight.Wts.malNEW)
lines(weight.Wts.Gen.malNEW, col = "cadetblue")
lines(weight.ts.malNEW, col = "coral")

## Education:
# < College
weight.ncol <- weight.ncolcol[weight.ncolcol$edu.grp == "nocollege",]
weight.col <- weight.ncolcol[weight.ncolcol$edu.grp == "college",]
weight.month.ncol <- ddply(weight.ncol, .(month.cent), summarize, 
                         mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                         weight.edu.mean = weighted.mean(dscore, weightvec.edu))

weight.ts.ncolNEW <- ts(weight.month.ncol$mean, frequency = 12, start = c(2017, 1))
weight.ts.ncolNEW
weight.Wts.Edu.ncolNEW <- ts(weight.month.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Edu.ncolNEW
weight.Wts.ncolNEW <- ts(weight.month.ncol$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.ncolNEW
plot(weight.Wts.ncolNEW)
lines(weight.Wts.Edu.ncolNEW, col = "cadetblue")
lines(weight.ts.ncolNEW, col = "coral")
# > College
weight.month.col <- ddply(weight.col, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.edu.mean = weighted.mean(dscore, weightvec.edu))

weight.ts.colNEW <- ts(weight.month.col$mean, frequency = 12, start = c(2017, 1))
weight.ts.colNEW
weight.Wts.Edu.colNEW <- ts(weight.month.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Edu.colNEW
weight.Wts.colNEW <- ts(weight.month.col$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.colNEW
plot(weight.Wts.colNEW)
lines(weight.Wts.Edu.colNEW, col = "cadetblue")
lines(weight.ts.colNEW, col = "coral")

## Race:
# White
weight.white <- weight.whblkasi[weight.whblkasi$racesimple == "White",]
weight.black <- weight.whblkasi[weight.whblkasi$racesimple == "Black",]
weight.asian <- weight.whblkasi[weight.whblkasi$racesimple == "Asian",]

weight.month.white <- ddply(weight.white, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

weight.ts.whiteNEW <- ts(weight.month.white$mean, frequency = 12, start = c(2017, 1))
weight.ts.whiteNEW
weight.Wts.Race.whiteNEW <- ts(weight.month.white$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Race.whiteNEW
weight.Wts.whiteNEW <- ts(weight.month.white$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.whiteNEW
plot(weight.Wts.whiteNEW)
lines(weight.Wts.Race.whiteNEW, col = "cadetblue")
lines(weight.ts.whiteNEW, col = "coral")

# Black
weight.month.black <- ddply(weight.black, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

weight.ts.blackNEW <- ts(weight.month.black$mean, frequency = 12, start = c(2017, 1))
weight.ts.blackNEW
weight.Wts.Race.blackNEW <- ts(weight.month.black$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Race.blackNEW
weight.Wts.blackNEW <- ts(weight.month.black$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.blackNEW
plot(weight.Wts.blackNEW)
lines(weight.Wts.Race.blackNEW, col = "cadetblue")
lines(weight.ts.blackNEW, col = "coral")

# Asian
weight.month.asian <- ddply(weight.asian, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.race.mean = weighted.mean(dscore, weightvec.race))

weight.ts.asianNEW <- ts(weight.month.asian$mean, frequency = 12, start = c(2017, 1))
weight.ts.asianNEW
weight.Wts.Race.asianNEW <- ts(weight.month.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Race.asianNEW
weight.Wts.asianNEW <- ts(weight.month.asian$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.asianNEW
plot(weight.Wts.asianNEW)
lines(weight.Wts.Race.asianNEW, col = "cadetblue")
lines(weight.ts.asianNEW, col = "coral")


## Age:
weight.ygmidold$weightvec.age <- as.numeric(weight.ygmidold$weightvec.age)
# Young
weight.young <- weight.ygmidold[weight.ygmidold$age.grp == "young",]
weight.middle <- weight.ygmidold[weight.ygmidold$age.grp == "middle",]
weight.old <- weight.ygmidold[weight.ygmidold$age.grp == "old",]

weight.month.young <- ddply(weight.young, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.age.mean = weighted.mean(dscore, weightvec.age))

weight.ts.youngNEW <- ts(weight.month.young$mean, frequency = 12, start = c(2017, 1))
weight.ts.youngNEW
weight.Wts.Age.youngNEW <- ts(weight.month.young$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Age.youngNEW
weight.Wts.youngNEW <- ts(weight.month.young$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.youngNEW
plot(weight.Wts.youngNEW)
lines(weight.Wts.Age.youngNEW, col = "cadetblue")
lines(weight.ts.youngNEW, col = "coral")

# Middle
weight.month.middle <- ddply(weight.middle, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.age.mean = weighted.mean(dscore, weightvec.age))

weight.ts.middleNEW <- ts(weight.month.middle$mean, frequency = 12, start = c(2017, 1))
weight.ts.middleNEW
weight.Wts.Age.middleNEW <- ts(weight.month.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Age.middleNEW
weight.Wts.middleNEW <- ts(weight.month.middle$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.middleNEW
plot(weight.Wts.middleNEW)
lines(weight.Wts.Age.middleNEW, col = "cadetblue")
lines(weight.ts.middleNEW, col = "coral")

# Old
weight.month.old <- ddply(weight.old, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.age.mean = weighted.mean(dscore, weightvec.age))

weight.ts.oldNEW <- ts(weight.month.old$mean, frequency = 12, start = c(2017, 1))
weight.ts.oldNEW
weight.Wts.Age.oldNEW <- ts(weight.month.old$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Age.oldNEW
weight.Wts.oldNEW <- ts(weight.month.old$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.oldNEW
plot(weight.Wts.oldNEW)
lines(weight.Wts.Age.oldNEW, col = "cadetblue")
lines(weight.ts.oldNEW, col = "coral")

## Religion:
weight.allreligion$relisimple <- car::recode(weight.allreligion$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")

# christian
weight.christ <- weight.allreligion[weight.allreligion$relisimple == "christian",]
weight.other <- weight.allreligion[weight.allreligion$relisimple == "other",]
weight.nonrel <- weight.allreligion[weight.allreligion$relisimple == "nonrel",]
weight.jew <- weight.allreligion[weight.allreligion$relisimple == "jewish",]

weight.month.christian <- ddply(weight.christ, .(month.cent), summarize, 
                              mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                              weight.rel.mean = weighted.mean(dscore, weightvec.religion))

weight.ts.christianNEW <- ts(weight.month.christian$mean, frequency = 12, start = c(2017, 1))
weight.ts.christianNEW
weight.Wts.Rel.christianNEW <- ts(weight.month.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Rel.christianNEW
weight.Wts.christianNEW <- ts(weight.month.christian$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.christianNEW
plot(weight.Wts.christianNEW)
lines(weight.Wts.Rel.christianNEW, col = "cadetblue")
lines(weight.ts.christianNEW, col = "coral")

# other
weight.month.other <- ddply(weight.other, .(month.cent), summarize, 
                          mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                          weight.rel.mean = weighted.mean(dscore, weightvec.religion))

weight.ts.otherNEW <- ts(weight.month.other$mean, frequency = 12, start = c(2017, 1))
weight.ts.otherNEW
weight.Wts.Rel.otherNEW <- ts(weight.month.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Rel.otherNEW
weight.Wts.otherNEW <- ts(weight.month.other$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.otherNEW
plot(weight.Wts.otherNEW)
lines(weight.Wts.Rel.otherNEW, col = "cadetblue")
lines(weight.ts.otherNEW, col = "coral")

# non
weight.month.non <- ddply(weight.nonrel, .(month.cent), summarize, 
                        mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                        weight.rel.mean = weighted.mean(dscore, weightvec.religion))

weight.ts.nonNEW <- ts(weight.month.non$mean, frequency = 12, start = c(2017, 1))
weight.ts.nonNEW
weight.Wts.Rel.nonNEW <- ts(weight.month.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Rel.nonNEW
weight.Wts.nonNEW <- ts(weight.month.non$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.nonNEW
plot(weight.Wts.nonNEW)
lines(weight.Wts.Rel.nonNEW, col = "cadetblue")
lines(weight.ts.nonNEW, col = "coral")

# jewish
weight.month.jewish <- ddply(weight.jew, .(month.cent), summarize, 
                           mean = mean(dscore), weight.mean = weighted.mean(dscore, weightvec), 
                           weight.rel.mean = weighted.mean(dscore, weightvec.religion))
weight.ts.jewishNEW <- ts(weight.month.jewish$mean, frequency = 12, start = c(2017, 1))
weight.ts.jewishNEW
weight.Wts.Rel.jewishNEW <- ts(weight.month.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.Wts.Rel.jewishNEW
weight.Wts.jewishNEW <- ts(weight.month.jewish$weight.mean, frequency = 12, start = c(2017, 1))
weight.Wts.jewishNEW
plot(weight.Wts.jewishNEW)
lines(weight.Wts.Rel.jewishNEW, col = "cadetblue")
lines(weight.ts.jewishNEW, col = "coral")


## EXPLICIT Weight ----
## Politics:
# Liberals
weight.month.E.lib <- ddply(weight.liberals, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))

weight.Ets.libNEW <- ts(weight.month.E.lib$mean, frequency = 12, start = c(2017, 1))
weight.Ets.libNEW
weight.WEts.Pol.libNEW <- ts(weight.month.E.lib$weight.pol.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Pol.libNEW
weight.WEts.libNEW <- ts(weight.month.E.lib$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.libNEW
plot(weight.WEts.libNEW)
lines(weight.WEts.Pol.libNEW, col = "cadetblue")
lines(weight.Ets.libNEW, col = "coral")
# Conservatives
weight.month.E.cons <- ddply(weight.conserv, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.pol.mean = weighted.mean(attnumeric, weightvec.pol))
weight.Ets.consNEW <- ts(weight.month.E.cons$mean, frequency = 12, start = c(2017, 1))
weight.Ets.consNEW
weight.WEts.Pol.consNEW <- ts(weight.month.E.cons$weight.pol.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Pol.consNEW
weight.WEts.consNEW <- ts(weight.month.E.cons$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.consNEW
plot(weight.WEts.consNEW)
lines(weight.WEts.Pol.consNEW, col = "cadetblue")
lines(weight.Ets.consNEW, col = "coral")

## Gender:
# Female
weight.month.E.fem <- ddply(weight.fem, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
weight.Ets.femNEW <- ts(weight.month.E.fem$mean, frequency = 12, start = c(2017, 1))
weight.Ets.femNEW
weight.WEts.Gen.femNEW <- ts(weight.month.E.fem$weight.gen.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Gen.femNEW
weight.WEts.femNEW <- ts(weight.month.E.fem$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.femNEW
plot(weight.WEts.femNEW)
lines(weight.WEts.Gen.femNEW, col = "cadetblue")
lines(weight.Ets.femNEW, col = "coral")

# Males
weight.month.E.mal <- ddply(weight.mal, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.gen.mean = weighted.mean(attnumeric, weightvec.gender))
weight.Ets.malNEW <- ts(weight.month.E.mal$mean, frequency = 12, start = c(2017, 1))
weight.Ets.malNEW
weight.WEts.Gen.malNEW <- ts(weight.month.E.mal$weight.gen.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Gen.malNEW
weight.WEts.malNEW <- ts(weight.month.E.mal$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.malNEW
plot(weight.WEts.malNEW)
lines(weight.WEts.Gen.malNEW, col = "cadetblue")
lines(weight.Ets.malNEW, col = "coral")

## Education:
# < College
weight.month.E.ncol <- ddply(weight.ncol, .(month.cent), summarize, 
                           mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                           weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

weight.Ets.ncolNEW <- ts(weight.month.E.ncol$mean, frequency = 12, start = c(2017, 1))
weight.Ets.ncolNEW
weight.WEts.Edu.ncolNEW <- ts(weight.month.E.ncol$weight.edu.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Edu.ncolNEW
weight.WEts.ncolNEW <- ts(weight.month.E.ncol$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.ncolNEW
plot(weight.WEts.ncolNEW)
lines(weight.WEts.Edu.ncolNEW, col = "cadetblue")
lines(weight.Ets.ncolNEW, col = "coral")
# > College
weight.month.E.col <- ddply(weight.col, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.edu.mean = weighted.mean(attnumeric, weightvec.edu))

weight.Ets.colNEW <- ts(weight.month.E.col$mean, frequency = 12, start = c(2017, 1))
weight.Ets.colNEW
weight.WEts.Edu.colNEW <- ts(weight.month.E.col$weight.edu.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Edu.colNEW
weight.WEts.colNEW <- ts(weight.month.E.col$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.colNEW
plot(weight.WEts.colNEW)
lines(weight.WEts.Edu.colNEW, col = "cadetblue")
lines(weight.Ets.colNEW, col = "coral")

## Race:
# White
weight.month.E.white <- ddply(weight.white, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

weight.Ets.whiteNEW <- ts(weight.month.E.white$mean, frequency = 12, start = c(2017, 1))
weight.Ets.whiteNEW
weight.WEts.Race.whiteNEW <- ts(weight.month.E.white$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Race.whiteNEW
weight.WEts.whiteNEW <- ts(weight.month.E.white$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.whiteNEW
plot(weight.WEts.whiteNEW)
lines(weight.WEts.Race.whiteNEW, col = "cadetblue")
lines(weight.Ets.whiteNEW, col = "coral")

# Black
weight.month.E.black <- ddply(weight.black, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

weight.Ets.blackNEW <- ts(weight.month.E.black$mean, frequency = 12, start = c(2017, 1))
weight.Ets.blackNEW
weight.WEts.Race.blackNEW <- ts(weight.month.E.black$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Race.blackNEW
weight.WEts.blackNEW <- ts(weight.month.E.black$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.blackNEW
plot(weight.WEts.blackNEW)
lines(weight.WEts.Race.blackNEW, col = "cadetblue")
lines(weight.Ets.blackNEW, col = "coral")

# Asian
weight.month.E.asian <- ddply(weight.asian, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.race.mean = weighted.mean(attnumeric, weightvec.race))

weight.Ets.asianNEW <- ts(weight.month.E.asian$mean, frequency = 12, start = c(2017, 1))
weight.Ets.asianNEW
weight.WEts.Race.asianNEW <- ts(weight.month.E.asian$weight.race.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Race.asianNEW
weight.WEts.asianNEW <- ts(weight.month.E.asian$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.asianNEW
plot(weight.WEts.asianNEW)
lines(weight.WEts.Race.asianNEW, col = "cadetblue")
lines(weight.Ets.asianNEW, col = "coral")


## Age:
# Young
weight.month.E.young <- ddply(weight.young, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.age.mean = weighted.mean(attnumeric, weightvec.age))

weight.Ets.youngNEW <- ts(weight.month.E.young$mean, frequency = 12, start = c(2017, 1))
weight.Ets.youngNEW
weight.WEts.Age.youngNEW <- ts(weight.month.E.young$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Age.youngNEW
weight.WEts.youngNEW <- ts(weight.month.E.young$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.youngNEW
plot(weight.WEts.youngNEW)
lines(weight.WEts.Age.youngNEW, col = "cadetblue")
lines(weight.Ets.youngNEW, col = "coral")

# Middle
weight.month.E.middle <- ddply(weight.middle, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.age.mean = weighted.mean(attnumeric, weightvec.age))

weight.Ets.middleNEW <- ts(weight.month.E.middle$mean, frequency = 12, start = c(2017, 1))
weight.Ets.middleNEW
weight.WEts.Age.middleNEW <- ts(weight.month.E.middle$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Age.middleNEW
weight.WEts.middleNEW <- ts(weight.month.E.middle$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.middleNEW
plot(weight.WEts.middleNEW)
lines(weight.WEts.Age.middleNEW, col = "cadetblue")
lines(weight.Ets.middleNEW, col = "coral")

# Old
weight.month.E.old <- ddply(weight.old, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.age.mean = weighted.mean(attnumeric, weightvec.age))

weight.Ets.oldNEW <- ts(weight.month.E.old$mean, frequency = 12, start = c(2017, 1))
weight.Ets.oldNEW
weight.WEts.Age.oldNEW <- ts(weight.month.E.old$weight.age.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Age.oldNEW
weight.WEts.oldNEW <- ts(weight.month.E.old$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.oldNEW
plot(weight.WEts.oldNEW)
lines(weight.WEts.Age.oldNEW, col = "cadetblue")
lines(weight.Ets.oldNEW, col = "coral")

## Religion:
# christian
weight.month.E.christian <- ddply(weight.christ, .(month.cent), summarize, 
                                mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                                weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

weight.Ets.christianNEW <- ts(weight.month.E.christian$mean, frequency = 12, start = c(2017, 1))
weight.Ets.christianNEW
weight.WEts.Rel.christianNEW <- ts(weight.month.E.christian$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Rel.christianNEW
weight.WEts.christianNEW <- ts(weight.month.E.christian$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.christianNEW
plot(weight.WEts.christianNEW)
lines(weight.WEts.Rel.christianNEW, col = "cadetblue")
lines(weight.Ets.christianNEW, col = "coral")

# other
weight.month.E.other <- ddply(weight.other, .(month.cent), summarize, 
                            mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                            weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

weight.Ets.otherNEW <- ts(weight.month.E.other$mean, frequency = 12, start = c(2017, 1))
weight.Ets.otherNEW
weight.WEts.Rel.otherNEW <- ts(weight.month.E.other$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Rel.otherNEW
weight.WEts.otherNEW <- ts(weight.month.E.other$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.otherNEW
plot(weight.WEts.otherNEW)
lines(weight.WEts.Rel.otherNEW, col = "cadetblue")
lines(weight.Ets.otherNEW, col = "coral")

# non
weight.month.E.non <- ddply(weight.nonrel, .(month.cent), summarize, 
                          mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                          weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))

weight.Ets.nonNEW <- ts(weight.month.E.non$mean, frequency = 12, start = c(2017, 1))
weight.Ets.nonNEW
weight.WEts.Rel.nonNEW <- ts(weight.month.E.non$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Rel.nonNEW
weight.WEts.nonNEW <- ts(weight.month.E.non$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.nonNEW
plot(weight.WEts.nonNEW)
lines(weight.WEts.Rel.nonNEW, col = "cadetblue")
lines(weight.Ets.nonNEW, col = "coral")

# jewish
weight.month.E.jewish <- ddply(weight.jew, .(month.cent), summarize, 
                             mean = mean(attnumeric), weight.mean = weighted.mean(attnumeric, weightvec), 
                             weight.rel.mean = weighted.mean(attnumeric, weightvec.religion))
weight.Ets.jewishNEW <- ts(weight.month.E.jewish$mean, frequency = 12, start = c(2017, 1))
weight.Ets.jewishNEW
weight.WEts.Rel.jewishNEW <- ts(weight.month.E.jewish$weight.rel.mean, frequency = 12, start = c(2017, 1))
weight.WEts.Rel.jewishNEW
weight.WEts.jewishNEW <- ts(weight.month.E.jewish$weight.mean, frequency = 12, start = c(2017, 1))
weight.WEts.jewishNEW
plot(weight.WEts.jewishNEW)
lines(weight.WEts.Rel.jewishNEW, col = "cadetblue")
lines(weight.Ets.jewishNEW, col = "coral")




## ** Save out all dataframes and trajectories ----
# Race:
save(list = c("race.ts.femNEW", "race.ts.malNEW", "race.Wts.femNEW", "race.Wts.malNEW", "race.Wts.Gen.femNEW", "race.Wts.Gen.malNEW",
              "race.ts.whiteNEW", "race.ts.blackNEW", "race.ts.asianNEW", 
              "race.Wts.whiteNEW", "race.Wts.blackNEW", "race.Wts.asianNEW",
              "race.Wts.Race.whiteNEW", "race.Wts.Race.blackNEW", "race.Wts.Race.asianNEW",
              "race.ts.youngNEW", "race.ts.middleNEW", "race.ts.oldNEW",
              "race.Wts.youngNEW", "race.Wts.middleNEW", "race.Wts.oldNEW",
              "race.Wts.Age.youngNEW", "race.Wts.Age.middleNEW", "race.Wts.Age.oldNEW",
              "race.ts.christianNEW", "race.ts.jewishNEW", "race.ts.otherNEW", "race.ts.nonNEW",
              "race.Wts.christianNEW", "race.Wts.jewishNEW", "race.Wts.otherNEW", "race.Wts.nonNEW",
              "race.Wts.Rel.christianNEW", "race.Wts.Rel.jewishNEW", "race.Wts.Rel.otherNEW", "race.Wts.Rel.nonNEW",
              "race.ts.colNEW", "race.ts.ncolNEW", "race.Wts.colNEW", "race.Wts.ncolNEW", "race.Wts.Edu.colNEW", "race.Wts.Edu.ncolNEW",
              "race.ts.libNEW", "race.ts.consNEW", "race.Wts.libNEW", "race.Wts.consNEW", "race.Wts.Pol.libNEW", "race.Wts.Pol.consNEW",
              "race.Ets.femNEW", "race.Ets.malNEW", "race.WEts.femNEW", "race.WEts.malNEW", "race.WEts.Gen.femNEW", "race.WEts.Gen.malNEW",
              "race.Ets.whiteNEW", "race.Ets.blackNEW", "race.Ets.asianNEW", 
              "race.WEts.whiteNEW", "race.WEts.blackNEW", "race.WEts.asianNEW",
              "race.WEts.Race.whiteNEW", "race.WEts.Race.blackNEW", "race.WEts.Race.asianNEW",
              "race.Ets.youngNEW", "race.Ets.middleNEW", "race.Ets.oldNEW",
              "race.WEts.youngNEW", "race.WEts.middleNEW", "race.WEts.oldNEW",
              "race.WEts.Age.youngNEW", "race.WEts.Age.middleNEW", "race.WEts.Age.oldNEW",
              "race.Ets.christianNEW", "race.Ets.jewishNEW", "race.Ets.otherNEW", "race.Ets.nonNEW",
              "race.WEts.christianNEW", "race.WEts.jewishNEW", "race.WEts.otherNEW", "race.WEts.nonNEW",
              "race.WEts.Rel.christianNEW", "race.WEts.Rel.jewishNEW", "race.WEts.Rel.otherNEW", "race.WEts.Rel.nonNEW",
              "race.Ets.colNEW", "race.Ets.ncolNEW", "race.WEts.colNEW", "race.WEts.ncolNEW", "race.WEts.Edu.colNEW", "race.WEts.Edu.ncolNEW",
              "race.Ets.libNEW", "race.Ets.consNEW", "race.WEts.libNEW", "race.WEts.consNEW", "race.WEts.Pol.libNEW", "race.WEts.Pol.consNEW"),
     file = "race_TS_demogs_NEWDATA.RData")


save(list = c("skin.ts.femNEW", "skin.ts.malNEW", "skin.Wts.femNEW", "skin.Wts.malNEW", "skin.Wts.Gen.femNEW", "skin.Wts.Gen.malNEW",
              "skin.ts.whiteNEW", "skin.ts.blackNEW", "skin.ts.asianNEW", 
              "skin.Wts.whiteNEW", "skin.Wts.blackNEW", "skin.Wts.asianNEW",
              "skin.Wts.Race.whiteNEW", "skin.Wts.Race.blackNEW", "skin.Wts.Race.asianNEW",
              "skin.ts.youngNEW", "skin.ts.middleNEW", "skin.ts.oldNEW",
              "skin.Wts.youngNEW", "skin.Wts.middleNEW", "skin.Wts.oldNEW",
              "skin.Wts.Age.youngNEW", "skin.Wts.Age.middleNEW", "skin.Wts.Age.oldNEW",
              "skin.ts.christianNEW", "skin.ts.jewishNEW", "skin.ts.otherNEW", "skin.ts.nonNEW",
              "skin.Wts.christianNEW", "skin.Wts.jewishNEW", "skin.Wts.otherNEW", "skin.Wts.nonNEW",
              "skin.Wts.Rel.christianNEW", "skin.Wts.Rel.jewishNEW", "skin.Wts.Rel.otherNEW", "skin.Wts.Rel.nonNEW",
              "skin.ts.colNEW", "skin.ts.ncolNEW", "skin.Wts.colNEW", "skin.Wts.ncolNEW", "skin.Wts.Edu.colNEW", "skin.Wts.Edu.ncolNEW",
              "skin.ts.libNEW", "skin.ts.consNEW", "skin.Wts.libNEW", "skin.Wts.consNEW", "skin.Wts.Pol.libNEW", "skin.Wts.Pol.consNEW",
              "skin.Ets.femNEW", "skin.Ets.malNEW", "skin.WEts.femNEW", "skin.WEts.malNEW", "skin.WEts.Gen.femNEW", "skin.WEts.Gen.malNEW",
              "skin.Ets.whiteNEW", "skin.Ets.blackNEW", "skin.Ets.asianNEW", 
              "skin.WEts.whiteNEW", "skin.WEts.blackNEW", "skin.WEts.asianNEW",
              "skin.WEts.Race.whiteNEW", "skin.WEts.Race.blackNEW", "skin.WEts.Race.asianNEW",
              "skin.Ets.youngNEW", "skin.Ets.middleNEW", "skin.Ets.oldNEW",
              "skin.WEts.youngNEW", "skin.WEts.middleNEW", "skin.WEts.oldNEW",
              "skin.WEts.Age.youngNEW", "skin.WEts.Age.middleNEW", "skin.WEts.Age.oldNEW",
              "skin.Ets.christianNEW", "skin.Ets.jewishNEW", "skin.Ets.otherNEW", "skin.Ets.nonNEW",
              "skin.WEts.christianNEW", "skin.WEts.jewishNEW", "skin.WEts.otherNEW", "skin.WEts.nonNEW",
              "skin.WEts.Rel.christianNEW", "skin.WEts.Rel.jewishNEW", "skin.WEts.Rel.otherNEW", "skin.WEts.Rel.nonNEW",
              "skin.Ets.colNEW", "skin.Ets.ncolNEW", "skin.WEts.colNEW", "skin.WEts.ncolNEW", "skin.WEts.Edu.colNEW", "skin.WEts.Edu.ncolNEW",
              "skin.Ets.libNEW", "skin.Ets.consNEW", "skin.WEts.libNEW", "skin.WEts.consNEW", "skin.WEts.Pol.libNEW", "skin.WEts.Pol.consNEW"),
     file = "skin_TS_demogs_NEWDATA.RData")


save(list = c("sex.ts.femNEW", "sex.ts.malNEW", "sex.Wts.femNEW", "sex.Wts.malNEW", "sex.Wts.Gen.femNEW", "sex.Wts.Gen.malNEW",
              "sex.ts.whiteNEW", "sex.ts.blackNEW", "sex.ts.asianNEW", 
              "sex.Wts.whiteNEW", "sex.Wts.blackNEW", "sex.Wts.asianNEW",
              "sex.Wts.Race.whiteNEW", "sex.Wts.Race.blackNEW", "sex.Wts.Race.asianNEW",
              "sex.ts.youngNEW", "sex.ts.middleNEW", "sex.ts.oldNEW",
              "sex.Wts.youngNEW", "sex.Wts.middleNEW", "sex.Wts.oldNEW",
              "sex.Wts.Age.youngNEW", "sex.Wts.Age.middleNEW", "sex.Wts.Age.oldNEW",
              "sex.ts.christianNEW", "sex.ts.jewishNEW", "sex.ts.otherNEW", "sex.ts.nonNEW",
              "sex.Wts.christianNEW", "sex.Wts.jewishNEW", "sex.Wts.otherNEW", "sex.Wts.nonNEW",
              "sex.Wts.Rel.christianNEW", "sex.Wts.Rel.jewishNEW", "sex.Wts.Rel.otherNEW", "sex.Wts.Rel.nonNEW",
              "sex.ts.colNEW", "sex.ts.ncolNEW", "sex.Wts.colNEW", "sex.Wts.ncolNEW", "sex.Wts.Edu.colNEW", "sex.Wts.Edu.ncolNEW",
              "sex.ts.libNEW", "sex.ts.consNEW", "sex.Wts.libNEW", "sex.Wts.consNEW", "sex.Wts.Pol.libNEW", "sex.Wts.Pol.consNEW",
              "sex.Ets.femNEW", "sex.Ets.malNEW", "sex.WEts.femNEW", "sex.WEts.malNEW", "sex.WEts.Gen.femNEW", "sex.WEts.Gen.malNEW",
              "sex.Ets.whiteNEW", "sex.Ets.blackNEW", "sex.Ets.asianNEW", 
              "sex.WEts.whiteNEW", "sex.WEts.blackNEW", "sex.WEts.asianNEW",
              "sex.WEts.Race.whiteNEW", "sex.WEts.Race.blackNEW", "sex.WEts.Race.asianNEW",
              "sex.Ets.youngNEW", "sex.Ets.middleNEW", "sex.Ets.oldNEW",
              "sex.WEts.youngNEW", "sex.WEts.middleNEW", "sex.WEts.oldNEW",
              "sex.WEts.Age.youngNEW", "sex.WEts.Age.middleNEW", "sex.WEts.Age.oldNEW",
              "sex.Ets.christianNEW", "sex.Ets.jewishNEW", "sex.Ets.otherNEW", "sex.Ets.nonNEW",
              "sex.WEts.christianNEW", "sex.WEts.jewishNEW", "sex.WEts.otherNEW", "sex.WEts.nonNEW",
              "sex.WEts.Rel.christianNEW", "sex.WEts.Rel.jewishNEW", "sex.WEts.Rel.otherNEW", "sex.WEts.Rel.nonNEW",
              "sex.Ets.colNEW", "sex.Ets.ncolNEW", "sex.WEts.colNEW", "sex.WEts.ncolNEW", "sex.WEts.Edu.colNEW", "sex.WEts.Edu.ncolNEW",
              "sex.Ets.libNEW", "sex.Ets.consNEW", "sex.WEts.libNEW", "sex.WEts.consNEW", "sex.WEts.Pol.libNEW", "sex.WEts.Pol.consNEW",
              "sex.ts.straightNEW", "sex.ts.lesgayNEW", "sex.ts.otherNEW",
              "sex.Wts.straightNEW", "sex.Wts.lesgayNEW", "sex.Wts.otherNEW",
              "sex.Wts.Sex.straightNEW", "sex.Wts.Sex.lesgayNEW", "sex.Wts.Sex.otherNEW",
              "sex.Ets.straightNEW", "sex.Ets.lesgayNEW", "sex.Ets.otherNEW",
              "sex.WEts.straightNEW", "sex.WEts.lesgayNEW", "sex.WEts.otherNEW",
              "sex.WEts.Sex.straightNEW", "sex.WEts.Sex.lesgayNEW", "sex.WEts.Sex.otherNEW"),
     file = "sex_TS_demogs_NEWDATA.RData")

save(list = c("age.ts.femNEW", "age.ts.malNEW", "age.Wts.femNEW", "age.Wts.malNEW", "age.Wts.Gen.femNEW", "age.Wts.Gen.malNEW",
              "age.ts.whiteNEW", "age.ts.blackNEW", "age.ts.asianNEW", 
              "age.Wts.whiteNEW", "age.Wts.blackNEW", "age.Wts.asianNEW",
              "age.Wts.Race.whiteNEW", "age.Wts.Race.blackNEW", "age.Wts.Race.asianNEW",
              "age.ts.youngNEW", "age.ts.middleNEW", "age.ts.oldNEW",
              "age.Wts.youngNEW", "age.Wts.middleNEW", "age.Wts.oldNEW",
              "age.Wts.Age.youngNEW", "age.Wts.Age.middleNEW", "age.Wts.Age.oldNEW",
              "age.ts.christianNEW", "age.ts.jewishNEW", "age.ts.otherNEW", "age.ts.nonNEW",
              "age.Wts.christianNEW", "age.Wts.jewishNEW", "age.Wts.otherNEW", "age.Wts.nonNEW",
              "age.Wts.Rel.christianNEW", "age.Wts.Rel.jewishNEW", "age.Wts.Rel.otherNEW", "age.Wts.Rel.nonNEW",
              "age.ts.colNEW", "age.ts.ncolNEW", "age.Wts.colNEW", "age.Wts.ncolNEW", "age.Wts.Edu.colNEW", "age.Wts.Edu.ncolNEW",
              "age.ts.libNEW", "age.ts.consNEW", "age.Wts.libNEW", "age.Wts.consNEW", "age.Wts.Pol.libNEW", "age.Wts.Pol.consNEW",
              "age.Ets.femNEW", "age.Ets.malNEW", "age.WEts.femNEW", "age.WEts.malNEW", "age.WEts.Gen.femNEW", "age.WEts.Gen.malNEW",
              "age.Ets.whiteNEW", "age.Ets.blackNEW", "age.Ets.asianNEW", 
              "age.WEts.whiteNEW", "age.WEts.blackNEW", "age.WEts.asianNEW",
              "age.WEts.Race.whiteNEW", "age.WEts.Race.blackNEW", "age.WEts.Race.asianNEW",
              "age.Ets.youngNEW", "age.Ets.middleNEW", "age.Ets.oldNEW",
              "age.WEts.youngNEW", "age.WEts.middleNEW", "age.WEts.oldNEW",
              "age.WEts.Age.youngNEW", "age.WEts.Age.middleNEW", "age.WEts.Age.oldNEW",
              "age.Ets.christianNEW", "age.Ets.jewishNEW", "age.Ets.otherNEW", "age.Ets.nonNEW",
              "age.WEts.christianNEW", "age.WEts.jewishNEW", "age.WEts.otherNEW", "age.WEts.nonNEW",
              "age.WEts.Rel.christianNEW", "age.WEts.Rel.jewishNEW", "age.WEts.Rel.otherNEW", "age.WEts.Rel.nonNEW",
              "age.Ets.colNEW", "age.Ets.ncolNEW", "age.WEts.colNEW", "age.WEts.ncolNEW", "age.WEts.Edu.colNEW", "age.WEts.Edu.ncolNEW",
              "age.Ets.libNEW", "age.Ets.consNEW", "age.WEts.libNEW", "age.WEts.consNEW", "age.WEts.Pol.libNEW", "age.WEts.Pol.consNEW"),
     file = "age_TS_demogs_NEWDATA.RData")


save(list = c("dis.ts.femNEW", "dis.ts.malNEW", "dis.Wts.femNEW", "dis.Wts.malNEW", "dis.Wts.Gen.femNEW", "dis.Wts.Gen.malNEW",
              "dis.ts.whiteNEW", "dis.ts.blackNEW", "dis.ts.asianNEW", 
              "dis.Wts.whiteNEW", "dis.Wts.blackNEW", "dis.Wts.asianNEW",
              "dis.Wts.Race.whiteNEW", "dis.Wts.Race.blackNEW", "dis.Wts.Race.asianNEW",
              "dis.ts.youngNEW", "dis.ts.middleNEW", "dis.ts.oldNEW",
              "dis.Wts.youngNEW", "dis.Wts.middleNEW", "dis.Wts.oldNEW",
              "dis.Wts.Age.youngNEW", "dis.Wts.Age.middleNEW", "dis.Wts.Age.oldNEW",
              "dis.ts.christianNEW", "dis.ts.jewishNEW", "dis.ts.otherNEW", "dis.ts.nonNEW",
              "dis.Wts.christianNEW", "dis.Wts.jewishNEW", "dis.Wts.otherNEW", "dis.Wts.nonNEW",
              "dis.Wts.Rel.christianNEW", "dis.Wts.Rel.jewishNEW", "dis.Wts.Rel.otherNEW", "dis.Wts.Rel.nonNEW",
              "dis.ts.colNEW", "dis.ts.ncolNEW", "dis.Wts.colNEW", "dis.Wts.ncolNEW", "dis.Wts.Edu.colNEW", "dis.Wts.Edu.ncolNEW",
              "dis.ts.libNEW", "dis.ts.consNEW", "dis.Wts.libNEW", "dis.Wts.consNEW", "dis.Wts.Pol.libNEW", "dis.Wts.Pol.consNEW",
              "dis.Ets.femNEW", "dis.Ets.malNEW", "dis.WEts.femNEW", "dis.WEts.malNEW", "dis.WEts.Gen.femNEW", "dis.WEts.Gen.malNEW",
              "dis.Ets.whiteNEW", "dis.Ets.blackNEW", "dis.Ets.asianNEW", 
              "dis.WEts.whiteNEW", "dis.WEts.blackNEW", "dis.WEts.asianNEW",
              "dis.WEts.Race.whiteNEW", "dis.WEts.Race.blackNEW", "dis.WEts.Race.asianNEW",
              "dis.Ets.youngNEW", "dis.Ets.middleNEW", "dis.Ets.oldNEW",
              "dis.WEts.youngNEW", "dis.WEts.middleNEW", "dis.WEts.oldNEW",
              "dis.WEts.Age.youngNEW", "dis.WEts.Age.middleNEW", "dis.WEts.Age.oldNEW",
              "dis.Ets.christianNEW", "dis.Ets.jewishNEW", "dis.Ets.otherNEW", "dis.Ets.nonNEW",
              "dis.WEts.christianNEW", "dis.WEts.jewishNEW", "dis.WEts.otherNEW", "dis.WEts.nonNEW",
              "dis.WEts.Rel.christianNEW", "dis.WEts.Rel.jewishNEW", "dis.WEts.Rel.otherNEW", "dis.WEts.Rel.nonNEW",
              "dis.Ets.colNEW", "dis.Ets.ncolNEW", "dis.WEts.colNEW", "dis.WEts.ncolNEW", "dis.WEts.Edu.colNEW", "dis.WEts.Edu.ncolNEW",
              "dis.Ets.libNEW", "dis.Ets.consNEW", "dis.WEts.libNEW", "dis.WEts.consNEW", "dis.WEts.Pol.libNEW", "dis.WEts.Pol.consNEW"),
     file = "dis_TS_demogs_NEWDATA.RData")


save(list = c("weight.ts.femNEW", "weight.ts.malNEW", "weight.Wts.femNEW", "weight.Wts.malNEW", "weight.Wts.Gen.femNEW", "weight.Wts.Gen.malNEW",
              "weight.ts.whiteNEW", "weight.ts.blackNEW", "weight.ts.asianNEW", 
              "weight.Wts.whiteNEW", "weight.Wts.blackNEW", "weight.Wts.asianNEW",
              "weight.Wts.Race.whiteNEW", "weight.Wts.Race.blackNEW", "weight.Wts.Race.asianNEW",
              "weight.ts.youngNEW", "weight.ts.middleNEW", "weight.ts.oldNEW",
              "weight.Wts.youngNEW", "weight.Wts.middleNEW", "weight.Wts.oldNEW",
              "weight.Wts.Age.youngNEW", "weight.Wts.Age.middleNEW", "weight.Wts.Age.oldNEW",
              "weight.ts.christianNEW", "weight.ts.jewishNEW", "weight.ts.otherNEW", "weight.ts.nonNEW",
              "weight.Wts.christianNEW", "weight.Wts.jewishNEW", "weight.Wts.otherNEW", "weight.Wts.nonNEW",
              "weight.Wts.Rel.christianNEW", "weight.Wts.Rel.jewishNEW", "weight.Wts.Rel.otherNEW", "weight.Wts.Rel.nonNEW",
              "weight.ts.colNEW", "weight.ts.ncolNEW", "weight.Wts.colNEW", "weight.Wts.ncolNEW", "weight.Wts.Edu.colNEW", "weight.Wts.Edu.ncolNEW",
              "weight.ts.libNEW", "weight.ts.consNEW", "weight.Wts.libNEW", "weight.Wts.consNEW", "weight.Wts.Pol.libNEW", "weight.Wts.Pol.consNEW",
              "weight.Ets.femNEW", "weight.Ets.malNEW", "weight.WEts.femNEW", "weight.WEts.malNEW", "weight.WEts.Gen.femNEW", "weight.WEts.Gen.malNEW",
              "weight.Ets.whiteNEW", "weight.Ets.blackNEW", "weight.Ets.asianNEW", 
              "weight.WEts.whiteNEW", "weight.WEts.blackNEW", "weight.WEts.asianNEW",
              "weight.WEts.Race.whiteNEW", "weight.WEts.Race.blackNEW", "weight.WEts.Race.asianNEW",
              "weight.Ets.youngNEW", "weight.Ets.middleNEW", "weight.Ets.oldNEW",
              "weight.WEts.youngNEW", "weight.WEts.middleNEW", "weight.WEts.oldNEW",
              "weight.WEts.Age.youngNEW", "weight.WEts.Age.middleNEW", "weight.WEts.Age.oldNEW",
              "weight.Ets.christianNEW", "weight.Ets.jewishNEW", "weight.Ets.otherNEW", "weight.Ets.nonNEW",
              "weight.WEts.christianNEW", "weight.WEts.jewishNEW", "weight.WEts.otherNEW", "weight.WEts.nonNEW",
              "weight.WEts.Rel.christianNEW", "weight.WEts.Rel.jewishNEW", "weight.WEts.Rel.otherNEW", "weight.WEts.Rel.nonNEW",
              "weight.Ets.colNEW", "weight.Ets.ncolNEW", "weight.WEts.colNEW", "weight.WEts.ncolNEW", "weight.WEts.Edu.colNEW", "weight.WEts.Edu.ncolNEW",
              "weight.Ets.libNEW", "weight.Ets.consNEW", "weight.WEts.libNEW", "weight.WEts.consNEW", "weight.WEts.Pol.libNEW", "weight.WEts.Pol.consNEW"),
     file = "weight_TS_demogs_NEWDATA.RData")

