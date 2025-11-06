## Charlesworth, & Banaji. Patterns of Change I.
## Script version date: January 21st, 2018

####################################################################################################################################

## This file creates a vector of participants' weighting values for the all IATs from 2007 - 2016
## controlling for census demographics of: sex, race, education, age, and political orientation (derived from GSS data)

## starting from .full.demogs files and setting targets in accordance with ACS values
## the outputs are a weighting vector attached to the timeseries data objects, 
# to be used in all descriptives, and in computing weighted monthly averages for the TS vectors

## Note: demographics are sufficiently off the population targets that the results achieve only partial convergence. 
## Results are provided for illustration and should be interpreted with caution.

####################################################################################################################################

## Load the data
load("full.demogs.NOZIP_012518.RData")


## Install packages
install.packages("anesrake", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("lsr", dependencies = TRUE)

## Load packages
require(anesrake)
require(car)
require(plyr)
require(lsr)

## set seed
set.seed(000)

####################################################################################################################################

# Set up the weighting targets (will be the same across all attitudes)
# See demographictargets.csv for full data (2007 - 2016)

# Sex: (1) Male = 49.23, (2) Female = 50.77
sex.weights <- c(.4923, .5077)
# Race: (1) White: .7401, (2) American Indian: 0.0081, (3) Asian: .0484, (4) Biracials: .0272
# (5) Black/AA: .1254, (6) Other: .0509
race.weights <- c(.7401, .0081, .0484, .0272, .1254, .0509) # will have to force sum to 1.
# Education: (1) less than high school: .1406; (2) high school graduate: .2834; (3) Some college: .2878
# (4) BA: .1805; (5) MA: .0759; (6) Professional degree: .0194; (7) Doctorate: .0124
edu.weights <- c(.1406, .2834, .2878, .1805, .0759, .0194, .0124)
# Age: Under 25: .3365, 25-34: .1341, 35-44: .1329, 45-54: .1416, 54-65: .1195, 65+: .1354
age.weights <- c(.3365, .1341, .1329, .1416, .1195, .1354)
# Political: Strong Liberal: .1675,	Mod Liberal: .165	Slight Lib: .125	Independ: .2225	
# Slight Cons: .09	Mod Cons: .1375	Strong Cons: .10
pol.weights <- c(.10, .1375, .09, .2225, .125, .165, .1675)

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights, pol.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage", "Wpol")



## AGE attitudes -------------------------------------------------------------------------------------------------------------------
table(age.full.demogs$month.cent)
age2007 <- age.full.demogs[age.full.demogs$month.cent>=37,]
age2007 <- age2007[age2007$year!=2006,]
table(age2007$month)

## First we need to clean the factor levels
### age
table(age2007$sex.new, age2007$year)
### race
table(age2007$ethnic.race.new, age2007$year)
# collapse biracials
age2007$race.recat <- recode(age2007$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
age2007$race.recat <- relevel(age2007$race.recat, "White")
### education
table(age2007$edu.new, age2007$year)
# collapse
age2007$edu.recat <- factor(age2007$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
age2007$edu.recat <- recode(age2007$edu.recat, "'elementary school' = 'less than HS';
                            'junior high' = 'less than HS';
                            'some high school' = 'less than HS';
                            'some graduate school' = 'bachelors degree';
                            'associates degree' = 'some college';
                            'M.B.A.' = 'professional degree';
                            'J.D.' = 'professional degree';
                            'M.D.' = 'professional degree';
                            'other advanced degree' = 'professional degree'")
table(age2007$edu.recat, age2007$year)
age2007$edu.recat <- factor(age2007$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))
### age
table(age2007$age.new, age2007$year)
age2007$agecats <- cut(age2007$age.new, c(0, 25,35,45,55,63,99))
levels(age2007$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(age2007$agecats, age2007$year)
# now all age categories for each year
### political
table(age2007$politicalid, age2007$year)


# new matched variables in the data set
age2007$Wsex <- as.numeric(age2007$sex.new)
age2007$Wrace <- as.numeric(age2007$race.recat)
age2007$Wedu <- as.numeric(age2007$edu.recat)
age2007$Wage <- as.numeric(age2007$agecats)
age2007$Wpol <- as.numeric(age2007$politicalid)

# now get sub-data for each year, to equate to full population
age.2007 <- age2007[age2007$year==2007,]
age.2008 <- age2007[age2007$year==2008,]
age.2009 <- age2007[age2007$year==2009,]
age.2010 <- age2007[age2007$year==2010,]
age.2011 <- age2007[age2007$year==2011,]
age.2012 <- age2007[age2007$year==2012,]
age.2013 <- age2007[age2007$year==2013,]
age.2014 <- age2007[age2007$year==2014,]
age.2015 <- age2007[age2007$year==2015,]
age.2016 <- age2007[age2007$year==2016,]

## create the weights per year...
weights2007age <- anesrake(targets, age.2007, caseid=age.2007$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2008age <- anesrake(targets, age.2008, caseid=age.2008$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2009age <- anesrake(targets, age.2009, caseid=age.2009$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2010age <- anesrake(targets, age.2010, caseid=age.2010$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2011age <- anesrake(targets, age.2011, caseid=age.2011$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2012age <- anesrake(targets, age.2012, caseid=age.2012$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2013age <- anesrake(targets, age.2013, caseid=age.2013$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2014age <- anesrake(targets, age.2014, caseid=age.2014$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2015age <- anesrake(targets, age.2015, caseid=age.2015$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2016age <- anesrake(targets, age.2016, caseid=age.2016$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007age <- as.data.frame(weights2007age[c("caseid", "weightvec")])
weights2008age <- as.data.frame(weights2008age[c("caseid", "weightvec")])
weights2009age <- as.data.frame(weights2009age[c("caseid", "weightvec")])
weights2010age <- as.data.frame(weights2010age[c("caseid", "weightvec")])
weights2011age <- as.data.frame(weights2011age[c("caseid", "weightvec")])
weights2012age <- as.data.frame(weights2012age[c("caseid", "weightvec")])
weights2013age <- as.data.frame(weights2013age[c("caseid", "weightvec")])
weights2014age <- as.data.frame(weights2014age[c("caseid", "weightvec")])
weights2015age <- as.data.frame(weights2015age[c("caseid", "weightvec")])
weights2016age <- as.data.frame(weights2016age[c("caseid", "weightvec")])

weights.allage <- rbind(weights2007age, weights2008age, weights2009age, weights2010age,
                        weights2011age, weights2012age, weights2013age, weights2014age, weights2015age, weights2016age)

## now create the vector of dscores and weights and month variables
colnames(weights.allage) <- c("session_id", "weightvec")
weights.allage$session_id <- as.character(weights.allage$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesAgeCensus <- age2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new")]
timeseriesAgeCensus <- merge(timeseriesAgeCensus, weights.allage, by = "session_id")


## DISABILITY attitudes -------------------------------------------------------------------------------------------------------------------

table(dis.full.demogs$month.cent)
dis2007 <- dis.full.demogs[dis.full.demogs$month.cent>=37,]
dis2007 <- dis2007[dis2007$year!=2006,]
table(dis2007$month)

## First we need to clean the factor levels
### sex
table(dis2007$sex.new, dis2007$year)
### race
table(dis2007$ethnic.race.new, dis2007$year)
# collapse biracials
dis2007$race.recat <- recode(dis2007$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
dis2007$race.recat <- relevel(dis2007$race.recat, "White")
### education
table(dis2007$edu.new, dis2007$year)
# collapse
dis2007$edu.recat <- factor(dis2007$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
dis2007$edu.recat <- recode(dis2007$edu.recat, "'elementary school' = 'less than HS';
                            'junior high' = 'less than HS';
                            'some high school' = 'less than HS';
                            'some graduate school' = 'bachelors degree';
                            'associates degree' = 'some college';
                            'M.B.A.' = 'professional degree';
                            'J.D.' = 'professional degree';
                            'M.D.' = 'professional degree';
                            'other advanced degree' = 'professional degree'")
table(dis2007$edu.recat, dis2007$year)
dis2007$edu.recat <- factor(dis2007$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(dis2007$age.new, dis2007$year)
dis2007$agecats <- cut(dis2007$age.new, c(0, 25,35,45,55,63,99))
levels(dis2007$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(dis2007$agecats, dis2007$year)
# now all age categories for each year

# new matched variables in the data set
dis2007$Wsex <- as.numeric(dis2007$sex.new)
dis2007$Wrace <- as.numeric(dis2007$race.recat)
dis2007$Wedu <- as.numeric(dis2007$edu.recat)
dis2007$Wage <- as.numeric(dis2007$agecats)
dis2007$Wpol <- as.numeric(dis2007$politicalid)

# now get sub-data for each year, to equate to full population
dis.2007 <- dis2007[dis2007$year==2007,]
dis.2008 <- dis2007[dis2007$year==2008,]
dis.2009 <- dis2007[dis2007$year==2009,]
dis.2010 <- dis2007[dis2007$year==2010,]
dis.2011 <- dis2007[dis2007$year==2011,]
dis.2012 <- dis2007[dis2007$year==2012,]
dis.2013 <- dis2007[dis2007$year==2013,]
dis.2014 <- dis2007[dis2007$year==2014,]
dis.2015 <- dis2007[dis2007$year==2015,]
dis.2016 <- dis2007[dis2007$year==2016,]

## create the weights per year...
weights2007dis <- anesrake(targets, dis.2007, caseid=dis.2007$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2008dis <- anesrake(targets, dis.2008, caseid=dis.2008$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2009dis <- anesrake(targets, dis.2009, caseid=dis.2009$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2010dis <- anesrake(targets, dis.2010, caseid=dis.2010$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2011dis <- anesrake(targets, dis.2011, caseid=dis.2011$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2012dis <- anesrake(targets, dis.2012, caseid=dis.2012$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2013dis <- anesrake(targets, dis.2013, caseid=dis.2013$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2014dis <- anesrake(targets, dis.2014, caseid=dis.2014$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2015dis <- anesrake(targets, dis.2015, caseid=dis.2015$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2016dis <- anesrake(targets, dis.2016, caseid=dis.2016$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007dis <- as.data.frame(weights2007dis[c("caseid", "weightvec")])
weights2008dis <- as.data.frame(weights2008dis[c("caseid", "weightvec")])
weights2009dis <- as.data.frame(weights2009dis[c("caseid", "weightvec")])
weights2010dis <- as.data.frame(weights2010dis[c("caseid", "weightvec")])
weights2011dis <- as.data.frame(weights2011dis[c("caseid", "weightvec")])
weights2012dis <- as.data.frame(weights2012dis[c("caseid", "weightvec")])
weights2013dis <- as.data.frame(weights2013dis[c("caseid", "weightvec")])
weights2014dis <- as.data.frame(weights2014dis[c("caseid", "weightvec")])
weights2015dis <- as.data.frame(weights2015dis[c("caseid", "weightvec")])
weights2016dis <- as.data.frame(weights2016dis[c("caseid", "weightvec")])

weights.alldis <- rbind(weights2007dis, weights2008dis, weights2009dis, weights2010dis,
                        weights2011dis, weights2012dis, weights2013dis, weights2014dis, weights2015dis, weights2016dis)

## now create the vector of dscores and weights and month variables
colnames(weights.alldis) <- c("session_id", "weightvec")
weights.alldis$session_id <- as.character(weights.alldis$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesDisCensus <- dis2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new", "dis.new", "distypes.new")]
timeseriesDisCensus <- merge(timeseriesDisCensus, weights.alldis, by = "session_id")


## RACE attitudes -------------------------------------------------------------------------------------------------------------------
table(race.full.demogs$month.cent)
race2007 <- race.full.demogs[race.full.demogs$month.cent>=37,]
race2007 <- race2007[race2007$year!=2006,]
table(race2007$month)

## First we need to clean the factor levels
### sex
table(race2007$sex.new, race2007$year)
### race
table(race2007$ethnic.race.new, race2007$year)
# collapse biracials
race2007$race.recat <- recode(race2007$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
race2007$race.recat <- relevel(race2007$race.recat, "White")
### education
table(race2007$edu.new, race2007$year)
# collapse
race2007$edu.recat <- factor(race2007$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
race2007$edu.recat <- recode(race2007$edu.recat, "'elementary school' = 'less than HS';
                            'junior high' = 'less than HS';
                            'some high school' = 'less than HS';
                            'some graduate school' = 'bachelors degree';
                            'associates degree' = 'some college';
                            'M.B.A.' = 'professional degree';
                            'J.D.' = 'professional degree';
                            'M.D.' = 'professional degree';
                            'other advanced degree' = 'professional degree'")
table(race2007$edu.recat, race2007$year)
race2007$edu.recat <- factor(race2007$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(race2007$age.new, race2007$year)
race2007$agecats <- cut(race2007$age.new, c(0, 25,35,45,55,63,99))
levels(race2007$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(race2007$agecats, race2007$year)
# now all age categories for each year

# new matched variables in the data set
race2007$Wsex <- as.numeric(race2007$sex.new)
race2007$Wrace <- as.numeric(race2007$race.recat)
race2007$Wedu <- as.numeric(race2007$edu.recat)
race2007$Wage <- as.numeric(race2007$agecats)
race2007$Wpol <- as.numeric(race2007$politicalid)

# now get sub-data for each year, to equate to full population
race.2007 <- race2007[race2007$year==2007,]
race.2008 <- race2007[race2007$year==2008,]
race.2009 <- race2007[race2007$year==2009,]
race.2010 <- race2007[race2007$year==2010,]
race.2011 <- race2007[race2007$year==2011,]
race.2012 <- race2007[race2007$year==2012,]
race.2013 <- race2007[race2007$year==2013,]
race.2014 <- race2007[race2007$year==2014,]
race.2015 <- race2007[race2007$year==2015,]
race.2016 <- race2007[race2007$year==2016,]

## create the weights per year...
weights2007race <- anesrake(targets, race.2007, caseid=race.2007$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2008race <- anesrake(targets, race.2008, caseid=race.2008$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2009race <- anesrake(targets, race.2009, caseid=race.2009$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2010race <- anesrake(targets, race.2010, caseid=race.2010$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2011race <- anesrake(targets, race.2011, caseid=race.2011$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2012race <- anesrake(targets, race.2012, caseid=race.2012$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2013race <- anesrake(targets, race.2013, caseid=race.2013$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2014race <- anesrake(targets, race.2014, caseid=race.2014$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2015race <- anesrake(targets, race.2015, caseid=race.2015$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2016race <- anesrake(targets, race.2016, caseid=race.2016$session_id,
                            verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007race <- as.data.frame(weights2007race[c("caseid", "weightvec")])
weights2008race <- as.data.frame(weights2008race[c("caseid", "weightvec")])
weights2009race <- as.data.frame(weights2009race[c("caseid", "weightvec")])
weights2010race <- as.data.frame(weights2010race[c("caseid", "weightvec")])
weights2011race <- as.data.frame(weights2011race[c("caseid", "weightvec")])
weights2012race <- as.data.frame(weights2012race[c("caseid", "weightvec")])
weights2013race <- as.data.frame(weights2013race[c("caseid", "weightvec")])
weights2014race <- as.data.frame(weights2014race[c("caseid", "weightvec")])
weights2015race <- as.data.frame(weights2015race[c("caseid", "weightvec")])
weights2016race <- as.data.frame(weights2016race[c("caseid", "weightvec")])

weights.allrace <- rbind(weights2007race, weights2008race, weights2009race, weights2010race,
                        weights2011race, weights2012race, weights2013race, weights2014race, weights2015race, weights2016race)

## now create the vector of dscores and weights and month variables
colnames(weights.allrace) <- c("session_id", "weightvec")
weights.allrace$session_id <- as.character(weights.allrace$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesRaceCensus <- race2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new")]
timeseriesRaceCensus <- merge(timeseriesRaceCensus, weights.allrace, by = "session_id")


## SKIN TONE attitudes -------------------------------------------------------------------------------------------------------------------

table(skin.full.demogs$month.cent)
skin2007 <- skin.full.demogs[skin.full.demogs$month.cent>=37,]
skin2007 <- skin2007[skin2007$year!=2006,]
table(skin2007$month)

## First we need to clean the factor levels
### sex
table(skin2007$sex.new, skin2007$year)
### race
table(skin2007$ethnic.race.new, skin2007$year)
# collapse biracials
skin2007$race.recat <- recode(skin2007$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
skin2007$race.recat <- relevel(skin2007$race.recat, "White")
### education
table(skin2007$edu.new, skin2007$year)
# collapse
skin2007$edu.recat <- factor(skin2007$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
skin2007$edu.recat <- recode(skin2007$edu.recat, "'elementary school' = 'less than HS';
                            'junior high' = 'less than HS';
                            'some high school' = 'less than HS';
                            'some graduate school' = 'bachelors degree';
                            'associates degree' = 'some college';
                            'M.B.A.' = 'professional degree';
                            'J.D.' = 'professional degree';
                            'M.D.' = 'professional degree';
                            'other advanced degree' = 'professional degree'")
table(skin2007$edu.recat, skin2007$year)
skin2007$edu.recat <- factor(skin2007$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(skin2007$age.new, skin2007$year)
skin2007$agecats <- cut(skin2007$age.new, c(0, 25,35,45,55,63,99))
levels(skin2007$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(skin2007$agecats, skin2007$year)
# now all age categories for each year

# new matched variables in the data set
skin2007$Wsex <- as.numeric(skin2007$sex.new)
skin2007$Wrace <- as.numeric(skin2007$race.recat)
skin2007$Wedu <- as.numeric(skin2007$edu.recat)
skin2007$Wage <- as.numeric(skin2007$agecats)
skin2007$Wpol <- as.numeric(skin2007$politicalid)

# now get sub-data for each year, to equate to full population
skin.2007 <- skin2007[skin2007$year==2007,]
skin.2008 <- skin2007[skin2007$year==2008,]
skin.2009 <- skin2007[skin2007$year==2009,]
skin.2010 <- skin2007[skin2007$year==2010,]
skin.2011 <- skin2007[skin2007$year==2011,]
skin.2012 <- skin2007[skin2007$year==2012,]
skin.2013 <- skin2007[skin2007$year==2013,]
skin.2014 <- skin2007[skin2007$year==2014,]
skin.2015 <- skin2007[skin2007$year==2015,]
skin.2016 <- skin2007[skin2007$year==2016,]

## create the weights per year...
weights2007skin <- anesrake(targets, skin.2007, caseid=skin.2007$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2008skin <- anesrake(targets, skin.2008, caseid=skin.2008$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2009skin <- anesrake(targets, skin.2009, caseid=skin.2009$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2010skin <- anesrake(targets, skin.2010, caseid=skin.2010$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2011skin <- anesrake(targets, skin.2011, caseid=skin.2011$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2012skin <- anesrake(targets, skin.2012, caseid=skin.2012$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2013skin <- anesrake(targets, skin.2013, caseid=skin.2013$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2014skin <- anesrake(targets, skin.2014, caseid=skin.2014$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2015skin <- anesrake(targets, skin.2015, caseid=skin.2015$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2016skin <- anesrake(targets, skin.2016, caseid=skin.2016$session_id,
                            verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007skin <- as.data.frame(weights2007skin[c("caseid", "weightvec")])
weights2008skin <- as.data.frame(weights2008skin[c("caseid", "weightvec")])
weights2009skin <- as.data.frame(weights2009skin[c("caseid", "weightvec")])
weights2010skin <- as.data.frame(weights2010skin[c("caseid", "weightvec")])
weights2011skin <- as.data.frame(weights2011skin[c("caseid", "weightvec")])
weights2012skin <- as.data.frame(weights2012skin[c("caseid", "weightvec")])
weights2013skin <- as.data.frame(weights2013skin[c("caseid", "weightvec")])
weights2014skin <- as.data.frame(weights2014skin[c("caseid", "weightvec")])
weights2015skin <- as.data.frame(weights2015skin[c("caseid", "weightvec")])
weights2016skin <- as.data.frame(weights2016skin[c("caseid", "weightvec")])

weights.allskin <- rbind(weights2007skin, weights2008skin, weights2009skin, weights2010skin,
                        weights2011skin, weights2012skin, weights2013skin, weights2014skin, weights2015skin, weights2016skin)

## now create the vector of dscores and weights and month variables
colnames(weights.allskin) <- c("session_id", "weightvec")
weights.allskin$session_id <- as.character(weights.allskin$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSkinCensus <- skin2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new")]
timeseriesSkinCensus <- merge(timeseriesSkinCensus, weights.allskin, by = "session_id")


## SEXUALITY attitudes -------------------------------------------------------------------------------------------------------------------
table(sex.full.demogs$month.cent)
sex2007 <- sex.full.demogs[sex.full.demogs$month.cent>=37,]
sex2007 <- sex2007[sex2007$year!=2006,]
table(sex2007$month)

## First we need to clean the factor levels
### sex
table(sex2007$sex.new, sex2007$year)
### race
table(sex2007$ethnic.race.new, sex2007$year)
# collapse biracials
sex2007$race.recat <- recode(sex2007$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
sex2007$race.recat <- relevel(sex2007$race.recat, "White")
### education
table(sex2007$edu.new, sex2007$year)
# collapse
sex2007$edu.recat <- factor(sex2007$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
sex2007$edu.recat <- recode(sex2007$edu.recat, "'elementary school' = 'less than HS';
                                  'junior high' = 'less than HS';
                          'some high school' = 'less than HS';
                          'some graduate school' = 'bachelors degree';
                          'associates degree' = 'some college';
                          'M.B.A.' = 'professional degree';
                          'J.D.' = 'professional degree';
                          'M.D.' = 'professional degree';
                          'other advanced degree' = 'professional degree'")
table(sex2007$edu.recat, sex2007$year)
sex2007$edu.recat <- factor(sex2007$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(sex2007$age.new, sex2007$year)
sex2007$agecats <- cut(sex2007$age.new, c(0, 25,35,45,55,63,99))
levels(sex2007$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(sex2007$agecats, sex2007$year)
# now all age categories for each year

# new matched variables in the data set
sex2007$Wsex <- as.numeric(sex2007$sex.new)
sex2007$Wrace <- as.numeric(sex2007$race.recat)
sex2007$Wedu <- as.numeric(sex2007$edu.recat)
sex2007$Wage <- as.numeric(sex2007$agecats)
sex2007$Wpol <- as.numeric(sex2007$politicalid)

# now get sub-data for each year, to equate to full population
sex.2007 <- sex2007[sex2007$year==2007,]
sex.2008 <- sex2007[sex2007$year==2008,]
sex.2009 <- sex2007[sex2007$year==2009,]
sex.2010 <- sex2007[sex2007$year==2010,]
sex.2011 <- sex2007[sex2007$year==2011,]
sex.2012 <- sex2007[sex2007$year==2012,]
sex.2013 <- sex2007[sex2007$year==2013,]
sex.2014 <- sex2007[sex2007$year==2014,]
sex.2015 <- sex2007[sex2007$year==2015,]
sex.2016 <- sex2007[sex2007$year==2016,]

## create the weights per year...
weights2007sex <- anesrake(targets, sex.2007, caseid=sex.2007$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2008sex <- anesrake(targets, sex.2008, caseid=sex.2008$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2009sex <- anesrake(targets, sex.2009, caseid=sex.2009$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2010sex <- anesrake(targets, sex.2010, caseid=sex.2010$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2011sex <- anesrake(targets, sex.2011, caseid=sex.2011$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2012sex <- anesrake(targets, sex.2012, caseid=sex.2012$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2013sex <- anesrake(targets, sex.2013, caseid=sex.2013$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2014sex <- anesrake(targets, sex.2014, caseid=sex.2014$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2015sex <- anesrake(targets, sex.2015, caseid=sex.2015$session_id,
                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2016sex <- anesrake(targets, sex.2016, caseid=sex.2016$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2007sex <- as.data.frame(weights2007sex[c("caseid", "weightvec")])
weights2008sex <- as.data.frame(weights2008sex[c("caseid", "weightvec")])
weights2009sex <- as.data.frame(weights2009sex[c("caseid", "weightvec")])
weights2010sex <- as.data.frame(weights2010sex[c("caseid", "weightvec")])
weights2011sex <- as.data.frame(weights2011sex[c("caseid", "weightvec")])
weights2012sex <- as.data.frame(weights2012sex[c("caseid", "weightvec")])
weights2013sex <- as.data.frame(weights2013sex[c("caseid", "weightvec")])
weights2014sex <- as.data.frame(weights2014sex[c("caseid", "weightvec")])
weights2015sex <- as.data.frame(weights2015sex[c("caseid", "weightvec")])
weights2016sex <- as.data.frame(weights2016sex[c("caseid", "weightvec")])

weights.allsex <- rbind(weights2007sex, weights2008sex, weights2009sex, weights2010sex,
                     weights2011sex, weights2012sex, weights2013sex, weights2014sex, weights2015sex, weights2016sex)

## now create the vector of dscores and weights and month variables
colnames(weights.allsex) <- c("session_id", "weightvec")
weights.allsex$session_id <- as.character(weights.allsex$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSexCensus <- sex2007[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "State", "County", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new", "sexuality.new", "Stimuli")]
timeseriesSexCensus <- merge(timeseriesSexCensus, weights.allsex, by = "session_id")


## BODY WEIGHT attitudes -------------------------------------------------------------------------------------------------------------------
## (1) Body figure ------------
# Only those who have taken the body figure test
table(weight.full.demogs$Stimuli.new)
weight.full.demogs$stim.collapse <- recode(weight.full.demogs$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weightbody <- weight.full.demogs[weight.full.demogs$stim.collapse=="body",]
table(weightbody$month)

## First we need to clean the factor levels
### sex
table(weightbody$sex.new, weightbody$year)
### race
table(weightbody$ethnic.race.new, weightbody$year)
# collapse biracials
weightbody$race.recat <- recode(weightbody$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                  'More than one race - Other' = 'Biracial'")
weightbody$race.recat <- relevel(weightbody$race.recat, "White")
### education
table(weightbody$edu.new, weightbody$year)
# collapse
weightbody$edu.recat <- factor(weightbody$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                        "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                        "J.D.", "M.D.", "Ph.D.","other advanced degree"))
weightbody$edu.recat <- recode(weightbody$edu.recat, "'elementary school' = 'less than HS';
                            'junior high' = 'less than HS';
                            'some high school' = 'less than HS';
                            'some graduate school' = 'bachelors degree';
                            'associates degree' = 'some college';
                            'M.B.A.' = 'professional degree';
                            'J.D.' = 'professional degree';
                            'M.D.' = 'professional degree';
                            'other advanced degree' = 'professional degree'")
table(weightbody$edu.recat, weightbody$year)
weightbody$edu.recat <- factor(weightbody$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                          "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(weightbody$age.new, weightbody$year)
weightbody$agecats <- cut(weightbody$age.new, c(0, 25,35,45,55,63,99))
levels(weightbody$agecats) <- c("age0024", "age2534", "age3544",
                             "age4554", "age5562", "age6399")
table(weightbody$agecats, weightbody$year)
# now all age categories for each year

# new matched variables in the data set
weightbody$Wsex <- as.numeric(weightbody$sex.new)
weightbody$Wrace <- as.numeric(weightbody$race.recat)
weightbody$Wedu <- as.numeric(weightbody$edu.recat)
weightbody$Wage <- as.numeric(weightbody$agecats)
weightbody$Wpol <- as.numeric(weightbody$politicalid)

# now get sub-data for each year, to equate to full population
weight.2010 <- weightbody[weightbody$year==2010,]
weight.2011 <- weightbody[weightbody$year==2011,]
weight.2012 <- weightbody[weightbody$year==2012,]
weight.2013 <- weightbody[weightbody$year==2013,]
weight.2014 <- weightbody[weightbody$year==2014,]
weight.2015 <- weightbody[weightbody$year==2015,]
weight.2016 <- weightbody[weightbody$year==2016,]

## create the weights per year...
weights2010weight <- anesrake(targets, weight.2010, caseid=weight.2010$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2011weight <- anesrake(targets, weight.2011, caseid=weight.2011$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2012weight <- anesrake(targets, weight.2012, caseid=weight.2012$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2013weight <- anesrake(targets, weight.2013, caseid=weight.2013$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2014weight <- anesrake(targets, weight.2014, caseid=weight.2014$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2015weight <- anesrake(targets, weight.2015, caseid=weight.2015$session_id,
                           verbose=TRUE,type="nolim",convcrit=0.005)
weights2016weight <- anesrake(targets, weight.2016, caseid=weight.2016$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2010weight <- as.data.frame(weights2010weight[c("caseid", "weightvec")])
weights2011weight <- as.data.frame(weights2011weight[c("caseid", "weightvec")])
weights2012weight <- as.data.frame(weights2012weight[c("caseid", "weightvec")])
weights2013weight <- as.data.frame(weights2013weight[c("caseid", "weightvec")])
weights2014weight <- as.data.frame(weights2014weight[c("caseid", "weightvec")])
weights2015weight <- as.data.frame(weights2015weight[c("caseid", "weightvec")])
weights2016weight <- as.data.frame(weights2016weight[c("caseid", "weightvec")])

weights.allweight <- rbind(weights2010weight,
                        weights2011weight, weights2012weight, weights2013weight, weights2014weight, weights2015weight, weights2016weight)

## now create the vector of dscores and weights and month variables
colnames(weights.allweight) <- c("session_id", "weightvec")
weights.allweight$session_id <- as.character(weights.allweight$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesWeightBodyCensus <- weightbody[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                           "date", "politicalid", "sex.new", "ethnic.race.new",
                           "edu.new", "num.new", "age.new", "order.new", "iam.new")]
timeseriesWeightBodyCensus <- merge(timeseriesWeightBodyCensus, weights.allweight, by = "session_id")

## (2) Face ------------
## Needs new targets for 2004 - 2011
## Using ACS 2005 - 2009 estimates

# Sex: (1) Male = 49.3, (2) Female = 50.7
sex.weights <- c(.493, .507)
# Race: (1) White: .745, (2) American Indian: 0.0081, (3) Asian: .046, (4) Biracials: .022
# (5) Black/AA: .124, (6) Other: .056
race.weights <- c(.745, .0081, .046, .022, .124, .056) # will have to force sum to 1.
# Education: (1) less than high school: .154; (2) high school graduate: .293; (3) Some college: .277
# (4) BA: .174; (5) MA: .070; (6) Professional degree: .020; (7) Doctorate: .011
edu.weights <- c(.154, .293, .277, .174, .07, .02, .011)
# Age: Under 25: .345, 25-34: .134, 35-44: .142, 45-54: .145, 55-64: .108, 65+: .126
age.weights <- c(.345, .134, .142, .145, .108, .126)

# Cannot weight by poltical because they changed in 2007.

## targets
targets <- list(sex.weights, race.weights, edu.weights, age.weights)
names(targets) <- c("Wsex", "Wrace", "Wedu", "Wage")

# Only those who have taken the face test
weightface <- weight.full.demogs[weight.full.demogs$stim.collapse=="face",]
table(weightface$month)

## First we need to clean the factor levels
### sex
table(weightface$sex.new, weightface$year)
### race
table(weightface$ethnic.race.new, weightface$year)
# collapse biracials
weightface$race.recat <- recode(weightface$ethnic.race.new, "'More than one race - Black/White' = 'Biracial';
                                'More than one race - Other' = 'Biracial'")
weightface$race.recat <- relevel(weightface$race.recat, "White")
table(weightface$race.recat)
### education
table(weightface$edu.new, weightface$year)
# collapse
weightface$edu.recat <- factor(weightface$edu.new, labels = c("elementary school","junior high","some high school","high school graduate", "some college",
                                                              "associates degree","bachelors degree", "some graduate school","masters degree","M.B.A.",             
                                                              "J.D.", "M.D.", "Ph.D.","other advanced degree"))
weightface$edu.recat <- recode(weightface$edu.recat, "'elementary school' = 'less than HS';
                               'junior high' = 'less than HS';
                               'some high school' = 'less than HS';
                               'some graduate school' = 'bachelors degree';
                               'associates degree' = 'some college';
                               'M.B.A.' = 'professional degree';
                               'J.D.' = 'professional degree';
                               'M.D.' = 'professional degree';
                               'other advanced degree' = 'professional degree'")
table(weightface$edu.recat, weightface$year)
weightface$edu.recat <- factor(weightface$edu.recat, levels = c("less than HS", "high school graduate", "some college",
                                                                "bachelors degree", "masters degree","professional degree", "Ph.D."))

### age
table(weightface$age.new, weightface$year)
weightface$agecats <- cut(weightface$age.new, c(0, 25,35,45,55,63,99))
levels(weightface$agecats) <- c("age0024", "age2534", "age3544",
                                "age4554", "age5562", "age6399")
table(weightface$agecats, weightface$year)
# now all age categories for each year

# new matched variables in the data set
weightface$Wsex <- as.numeric(weightface$sex.new)
weightface$Wrace <- as.numeric(weightface$race.recat)
weightface$Wedu <- as.numeric(weightface$edu.recat)
weightface$Wage <- as.numeric(weightface$agecats)

# now get sub-data for each year, to equate to full population
weight.2004 <- weightface[weightface$year==2004,]
weight.2005 <- weightface[weightface$year==2005,]
weight.2006 <- weightface[weightface$year==2006,]
weight.2007 <- weightface[weightface$year==2007,]
weight.2008 <- weightface[weightface$year==2008,]
weight.2009 <- weightface[weightface$year==2009,]
weight.2010 <- weightface[weightface$year==2010,]
weight.2011 <- weightface[weightface$year==2011,]


## create the weights per year...
weights2004weight <- anesrake(targets, weight.2004, caseid=weight.2004$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2005weight <- anesrake(targets, weight.2005, caseid=weight.2005$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2006weight <- anesrake(targets, weight.2006, caseid=weight.2006$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2007weight <- anesrake(targets, weight.2007, caseid=weight.2007$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2008weight <- anesrake(targets, weight.2008, caseid=weight.2008$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2009weight <- anesrake(targets, weight.2009, caseid=weight.2009$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2010weight <- anesrake(targets, weight.2010, caseid=weight.2010$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)
weights2011weight <- anesrake(targets, weight.2011, caseid=weight.2011$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2004weight <- as.data.frame(weights2004weight[c("caseid", "weightvec")])
weights2005weight <- as.data.frame(weights2005weight[c("caseid", "weightvec")])
weights2006weight <- as.data.frame(weights2006weight[c("caseid", "weightvec")])
weights2007weight <- as.data.frame(weights2007weight[c("caseid", "weightvec")])
weights2008weight <- as.data.frame(weights2008weight[c("caseid", "weightvec")])
weights2009weight <- as.data.frame(weights2009weight[c("caseid", "weightvec")])
weights2010weight <- as.data.frame(weights2010weight[c("caseid", "weightvec")])
weights2011weight <- as.data.frame(weights2011weight[c("caseid", "weightvec")])


weights.allweight <- rbind(weights2004weight, weights2005weight,
                           weights2006weight, weights2007weight, weights2008weight, 
                           weights2008weight,
                           weights2009weight,
                           weights2010weight,
                           weights2011weight)

## now create the vector of dscores and weights and month variables
colnames(weights.allweight) <- c("session_id", "weightvec")
weights.allweight$session_id <- as.character(weights.allweight$session_id)


# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesWeightFaceCensus <- weightface[c("session_id", "dscore", "att.new", "att.center1", "month.cent", "month",
                                           "date", "politicalid", "sex.new", "ethnic.race.new",
                                           "edu.new", "num.new", "age.new", "order.new", "weight.COMB")]
timeseriesWeightFaceCensus <- merge(timeseriesWeightFaceCensus, weights.allweight, by = "session_id")


