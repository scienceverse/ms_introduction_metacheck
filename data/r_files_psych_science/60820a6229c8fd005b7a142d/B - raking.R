
## Patterns of Implicit and Explicit Attitudes IV. 
## Script version date: September 1st, 2021

####################################################################################################################################

## This file creates a vector of participants' weighting values for the all IATs
## controlling for: gender, race, education, age, and political orientation
## starting from .full.demogs files
## the outputs are a weighting vector attached to the timeseries data objects, to be used in all descriptives, and in computing weighted monthly averages

## NOTE: 
# This weighting includes the past data from 2007-2016 to set up the target weights appropriately across the whole time span investigated

####################################################################################################################################
## Installing required packages
if (!require(car)) {install.packages("car", dependencies = TRUE); require(car)}
if (!require(anesrake)) {install.packages("anesrake", dependencies = TRUE); require(anesrake)}

####################################################################################################################################

# setwd("**") # set working directory if necessary

## Load the data object from 2007-2016 data
load("fulldemogs_OLDDATA.RData")

## Load the NEWDATA object from 2017-2020
load("fulldemogs_NEWDATA.RData")

####################################################################################################################################

## MERGING data frames to create target weights -------

## RACE ------------------------------------------------
colnames(race.full.demogs)
race.full.demogsOLDDATA <- race.full.demogs[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                              "politicalid", "edu.new")]
colnames(race.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
race.full.demogsOLDDATA$race <- recode(race.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(race.full.demogsOLDDATA$race)

race.full.demogsNEWDATA <- race.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                                     "politicalid.new", "edu.new")]
colnames(race.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
race.full.demogsNEWDATA$race <- recode(race.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(race.full.demogsNEWDATA$race)
race.full.demogsNEWDATA$pol <- factor(race.full.demogsNEWDATA$pol, labels = levels(race.full.demogsOLDDATA$pol))

race.ALLDATA <- rbind(race.full.demogsOLDDATA, race.full.demogsNEWDATA)


## SEXUALITY -----------------------------------------------
sexuality.full.demogs <- sex.full.demogs
colnames(sexuality.full.demogs)
sexuality.full.demogsOLDDATA <- sexuality.full.demogs[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                              "politicalid", "edu.new")]
colnames(sexuality.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
sexuality.full.demogsOLDDATA$race <- recode(sexuality.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(sexuality.full.demogsOLDDATA$race)

sexuality.full.demogsNEWDATA <- sexuality.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                                     "politicalid.new", "edu.new")]
colnames(sexuality.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
sexuality.full.demogsNEWDATA$race <- recode(sexuality.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(sexuality.full.demogsNEWDATA$race)
sexuality.full.demogsNEWDATA$pol <- factor(sexuality.full.demogsNEWDATA$pol, labels = levels(sexuality.full.demogsOLDDATA$pol))

sexuality.ALLDATA <- rbind(sexuality.full.demogsOLDDATA, sexuality.full.demogsNEWDATA)

## AGE -----------------------------------------------
colnames(age.full.demogs)
age.full.demogsOLDDATA <- age.full.demogs[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                            "politicalid", "edu.new")]
colnames(age.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
age.full.demogsOLDDATA$race <- recode(age.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(age.full.demogsOLDDATA$race)

age.full.demogsNEWDATA <- age.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                               "politicalid.new", "edu.new")]
colnames(age.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
age.full.demogsNEWDATA$race <- recode(age.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(age.full.demogsNEWDATA$race)
age.full.demogsNEWDATA$pol <- factor(age.full.demogsNEWDATA$pol, labels = levels(age.full.demogsOLDDATA$pol))

age.ALLDATA <- rbind(age.full.demogsOLDDATA, age.full.demogsNEWDATA)


## DISABILITY -----------------------------------------------
disability.full.demogs <- dis.full.demogs
colnames(disability.full.demogs)
disability.full.demogsOLDDATA <- disability.full.demogs[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                                        "politicalid", "edu.new")]
colnames(disability.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
disability.full.demogsOLDDATA$race <- recode(disability.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(disability.full.demogsOLDDATA$race)

disability.full.demogsNEWDATA <- disability.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                                       "politicalid.new", "edu.new")]
colnames(disability.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
disability.full.demogsNEWDATA$race <- recode(disability.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(disability.full.demogsNEWDATA$race)
disability.full.demogsNEWDATA$pol <- factor(disability.full.demogsNEWDATA$pol, labels = levels(disability.full.demogsOLDDATA$pol))

disability.ALLDATA <- rbind(disability.full.demogsOLDDATA, disability.full.demogsNEWDATA)


## WEIGHT -----------------------------------------------
## Need to merge only with the latter test (Body Figure)
table(weight.full.demogs$Stimuli.new)
weight.full.demogs$stim.collapse <- recode(weight.full.demogs$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weight.full.demogsOLDDATA <- weight.full.demogs[weight.full.demogs$stim.collapse=="body",]
table(weight.full.demogsOLDDATA$month)

weight.full.demogsOLDDATA <- weight.full.demogsOLDDATA[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                              "politicalid", "edu.new")]
colnames(weight.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
weight.full.demogsOLDDATA$race <- recode(weight.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(weight.full.demogsOLDDATA$race)

weight.full.demogsNEWDATA <- weight.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                             "politicalid.new", "edu.new")]
colnames(weight.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
weight.full.demogsNEWDATA$race <- recode(weight.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(weight.full.demogsNEWDATA$race)
weight.full.demogsNEWDATA$pol <- factor(weight.full.demogsNEWDATA$pol, labels = levels(weight.full.demogsOLDDATA$pol))

weight.ALLDATA <- rbind(weight.full.demogsOLDDATA, weight.full.demogsNEWDATA)

## SKIN-TONE -----------------------------------------------
colnames(skin.full.demogs)
skin.full.demogsOLDDATA <- skin.full.demogs[c("session_id", "year", "age.new", "sex.new", "ethnic.race.new",
                                              "politicalid", "edu.new")]
colnames(skin.full.demogsOLDDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
skin.full.demogsOLDDATA$race <- recode(skin.full.demogsOLDDATA$race, "'More than one race - Black/White' = 'Multiracial';
                                       'More than one race - Other' = 'Multiracial'")
table(skin.full.demogsOLDDATA$race)

skin.full.demogsNEWDATA <- skin.full.demogsNEW[c("session_id", "year", "age.new", "genderidentity_simp", "race.new",
                                                 "politicalid.new", "edu.new")]
colnames(skin.full.demogsNEWDATA) <- c("session_id", "year", "age", "gender", "race", "pol", "edu")
skin.full.demogsNEWDATA$race <- recode(skin.full.demogsNEWDATA$race, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
table(skin.full.demogsNEWDATA$race)
skin.full.demogsNEWDATA$pol <- factor(skin.full.demogsNEWDATA$pol, labels = levels(skin.full.demogsOLDDATA$pol))

skin.ALLDATA <- rbind(skin.full.demogsOLDDATA, skin.full.demogsNEWDATA)



####################################################################################################################################
####################################################################################################################################

### **** CREATING WEIGHTS FOR OVERALL SAMPLE -----------------------------------------------------------------
## RACE ------------------------------------------------
race.ALLDATA <- race.ALLDATA[race.ALLDATA$year>=2007,]
table(race.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(race.ALLDATA$gender, race.ALLDATA$year)
race.ALLDATA$gender <- factor(race.ALLDATA$gender, levels = c("female", "male"))
### race
table(race.ALLDATA$race, race.ALLDATA$year)
### education
table(race.ALLDATA$edu, race.ALLDATA$year)
race.ALLDATA$educats <- cut(as.numeric(race.ALLDATA$edu), c(0,4,7,9,14))
table(race.ALLDATA$educats, race.ALLDATA$year)
### age
table(race.ALLDATA$age, race.ALLDATA$year)
race.ALLDATA$agecats <- cut(race.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(race.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                  "age4554", "age5562", "age6399")
table(race.ALLDATA$agecats, race.ALLDATA$year)
# now all age categories for each year
### political
table(race.ALLDATA$pol, race.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_race <- as.data.frame(table(race.ALLDATA$gender)/sum(table(race.ALLDATA$gender)))
race.weights_race <- as.data.frame(table(race.ALLDATA$race)/sum(table(race.ALLDATA$race)))
edu.weights_race <- as.data.frame(table(race.ALLDATA$educats)/sum(table(race.ALLDATA$educats)))
age.weights_race <- as.data.frame(table(race.ALLDATA$agecats)/sum(table(race.ALLDATA$agecats)))
pol.weights_race <- as.data.frame(table(race.ALLDATA$pol)/sum(table(race.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_race <- gender.weights_race$Freq
race.weights_race <- race.weights_race$Freq
edu.weights_race <- edu.weights_race$Freq
age.weights_race <- age.weights_race$Freq
pol.weights_race <- pol.weights_race$Freq

## targets
targets_race <- list(gender.weights_race, race.weights_race, edu.weights_race, age.weights_race, pol.weights_race)
names(targets_race) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_race

# new matched variables in the data set
race.ALLDATA$Wgender <- as.numeric(race.ALLDATA$gender)
race.ALLDATA$Wrace <- as.numeric(race.ALLDATA$race)
race.ALLDATA$Wedu <- as.numeric(race.ALLDATA$educats)
race.ALLDATA$Wage <- as.numeric(race.ALLDATA$agecats)
race.ALLDATA$Wpol <- as.numeric(race.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
race.2017 <- race.ALLDATA[race.ALLDATA$year==2017,]
race.2018 <- race.ALLDATA[race.ALLDATA$year==2018,]
race.2019 <- race.ALLDATA[race.ALLDATA$year==2019,]
race.2020 <- race.ALLDATA[race.ALLDATA$year==2020,]

## create the weights per year...
weights2017_race <- anesrake(targets_race, race.2017, caseid=race.2017$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_race <- anesrake(targets_race, race.2018, caseid=race.2018$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_race <- anesrake(targets_race, race.2019, caseid=race.2019$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_race <- anesrake(targets_race, race.2020, caseid=race.2020$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_race <- as.data.frame(weights2017_race[c("caseid", "weightvec")])
weights2018_race  <- as.data.frame(weights2018_race[c("caseid", "weightvec")])
weights2019_race  <- as.data.frame(weights2019_race[c("caseid", "weightvec")])
weights2020_race  <- as.data.frame(weights2020_race[c("caseid", "weightvec")])

weights.all_race <- rbind(weights2017_race, weights2018_race,
                          weights2019_race, weights2020_race)

## now create the vector of dscores and weights and month variables
colnames(weights.all_race) <- c("session_id", "weightvec")
weights.all_race$session_id <- as.character(weights.all_race$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesRace_NEW <- race.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                            "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                            "politicalid.new", "edu.new", "Order", "num.new",
                                            "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesRace_NEW <- merge(timeseriesRace_NEW, weights.all_race, by = "session_id")

write.csv(timeseriesRace_NEW, "timeseriesRace_NEW.csv")




## SEXUALITY ------------------------------------------------
sexuality.ALLDATA <- sexuality.ALLDATA[sexuality.ALLDATA$year>=2007,]
table(sexuality.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(sexuality.ALLDATA$gender, sexuality.ALLDATA$year)
sexuality.ALLDATA$gender <- recode(sexuality.ALLDATA$gender, "'Female' = 'female'; 'Male' = 'male'")
sexuality.ALLDATA$gender <- factor(sexuality.ALLDATA$gender, levels = c("female", "male"))
### race
table(sexuality.ALLDATA$race, sexuality.ALLDATA$year)
### education
table(sexuality.ALLDATA$edu, sexuality.ALLDATA$year)
levels(sexuality.ALLDATA$edu)[c(6,7,9)] <- c("associates degree", "bachelors degree", "masters degree")
sexuality.ALLDATA$educats <- cut(as.numeric(sexuality.ALLDATA$edu), c(0,4,7,9,14))
table(sexuality.ALLDATA$educats, sexuality.ALLDATA$year)
### age
table(sexuality.ALLDATA$age, sexuality.ALLDATA$year)
sexuality.ALLDATA$agecats <- cut(sexuality.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(sexuality.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                  "age4554", "age5562", "age6399")
table(sexuality.ALLDATA$agecats, sexuality.ALLDATA$year)
# now all age categories for each year
### political
table(sexuality.ALLDATA$pol, sexuality.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_sexuality <- as.data.frame(table(sexuality.ALLDATA$gender)/sum(table(sexuality.ALLDATA$gender)))
race.weights_sexuality <- as.data.frame(table(sexuality.ALLDATA$race)/sum(table(sexuality.ALLDATA$race)))
edu.weights_sexuality <- as.data.frame(table(sexuality.ALLDATA$educats)/sum(table(sexuality.ALLDATA$educats)))
age.weights_sexuality <- as.data.frame(table(sexuality.ALLDATA$agecats)/sum(table(sexuality.ALLDATA$agecats)))
pol.weights_sexuality <- as.data.frame(table(sexuality.ALLDATA$pol)/sum(table(sexuality.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_sexuality <- gender.weights_sexuality$Freq
race.weights_sexuality <- race.weights_sexuality$Freq
edu.weights_sexuality <- edu.weights_sexuality$Freq
age.weights_sexuality <- age.weights_sexuality$Freq
pol.weights_sexuality <- pol.weights_sexuality$Freq

## targets
targets_sexuality <- list(gender.weights_sexuality, race.weights_sexuality, edu.weights_sexuality, age.weights_sexuality, pol.weights_sexuality)
names(targets_sexuality) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_sexuality

# new matched variables in the data set
sexuality.ALLDATA$Wgender <- as.numeric(sexuality.ALLDATA$gender)
sexuality.ALLDATA$Wrace <- as.numeric(sexuality.ALLDATA$race)
sexuality.ALLDATA$Wedu <- as.numeric(sexuality.ALLDATA$educats)
sexuality.ALLDATA$Wage <- as.numeric(sexuality.ALLDATA$agecats)
sexuality.ALLDATA$Wpol <- as.numeric(sexuality.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
sexuality.2017 <- sexuality.ALLDATA[sexuality.ALLDATA$year==2017,]
sexuality.2018 <- sexuality.ALLDATA[sexuality.ALLDATA$year==2018,]
sexuality.2019 <- sexuality.ALLDATA[sexuality.ALLDATA$year==2019,]
sexuality.2020 <- sexuality.ALLDATA[sexuality.ALLDATA$year==2020,]

## create the weights per year...
weights2017_sexuality <- anesrake(targets_sexuality, sexuality.2017, caseid=sexuality.2017$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_sexuality <- anesrake(targets_sexuality, sexuality.2018, caseid=sexuality.2018$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_sexuality <- anesrake(targets_sexuality, sexuality.2019, caseid=sexuality.2019$session_id,
                             verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_sexuality <- anesrake(targets_sexuality, sexuality.2020, caseid=sexuality.2020$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_sexuality <- as.data.frame(weights2017_sexuality[c("caseid", "weightvec")])
weights2018_sexuality  <- as.data.frame(weights2018_sexuality[c("caseid", "weightvec")])
weights2019_sexuality  <- as.data.frame(weights2019_sexuality[c("caseid", "weightvec")])
weights2020_sexuality  <- as.data.frame(weights2020_sexuality[c("caseid", "weightvec")])

weights.all_sexuality <- rbind(weights2017_sexuality, weights2018_sexuality,
                               weights2019_sexuality, weights2020_sexuality)

## now create the vector of dscores and weights and month variables
colnames(weights.all_sexuality) <- c("session_id", "weightvec")
weights.all_sexuality$session_id <- as.character(weights.all_sexuality$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSex_NEW <- sexuality.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                            "age.new", "genderidentity_simp", "race.new", "ethnicity.new", "sexuality", "sexuality_simp",
                                            "politicalid.new", "edu.new", "Order", "num.new",
                                            "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesSex_NEW <- merge(timeseriesSex_NEW, weights.all_sexuality, by = "session_id")

write.csv(timeseriesSex_NEW, "timeseriesSex_NEW.csv")


## AGE ------------------------------------------------
age.ALLDATA <- age.ALLDATA[age.ALLDATA$year>=2007,]
table(age.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(age.ALLDATA$gender, age.ALLDATA$year)
age.ALLDATA$gender <- factor(age.ALLDATA$gender, levels = c("female", "male"))
### race
table(age.ALLDATA$race, age.ALLDATA$year)
### education
table(age.ALLDATA$edu, age.ALLDATA$year)
levels(age.ALLDATA$edu)[c(6,7,9)] <- c("associates degree", "bachelors degree", "masters degree")
age.ALLDATA$educats <- cut(as.numeric(age.ALLDATA$edu), c(0,4,7,9,14))
table(age.ALLDATA$educats, age.ALLDATA$year)
### age
table(age.ALLDATA$age, age.ALLDATA$year)
age.ALLDATA$agecats <- cut(age.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(age.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                       "age4554", "age5562", "age6399")
table(age.ALLDATA$agecats, age.ALLDATA$year)
# now all age categories for each year
### political
table(age.ALLDATA$pol, age.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_age <- as.data.frame(table(age.ALLDATA$gender)/sum(table(age.ALLDATA$gender)))
race.weights_age <- as.data.frame(table(age.ALLDATA$race)/sum(table(age.ALLDATA$race)))
edu.weights_age <- as.data.frame(table(age.ALLDATA$educats)/sum(table(age.ALLDATA$educats)))
age.weights_age <- as.data.frame(table(age.ALLDATA$agecats)/sum(table(age.ALLDATA$agecats)))
pol.weights_age <- as.data.frame(table(age.ALLDATA$pol)/sum(table(age.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_age <- gender.weights_age$Freq
race.weights_age <- race.weights_age$Freq
edu.weights_age <- edu.weights_age$Freq
age.weights_age <- age.weights_age$Freq
pol.weights_age <- pol.weights_age$Freq

## targets
targets_age <- list(gender.weights_age, race.weights_age, edu.weights_age, age.weights_age, pol.weights_age)
names(targets_age) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_age

# new matched variables in the data set
age.ALLDATA$Wgender <- as.numeric(age.ALLDATA$gender)
age.ALLDATA$Wrace <- as.numeric(age.ALLDATA$race)
age.ALLDATA$Wedu <- as.numeric(age.ALLDATA$educats)
age.ALLDATA$Wage <- as.numeric(age.ALLDATA$agecats)
age.ALLDATA$Wpol <- as.numeric(age.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
age.2017 <- age.ALLDATA[age.ALLDATA$year==2017,]
age.2018 <- age.ALLDATA[age.ALLDATA$year==2018,]
age.2019 <- age.ALLDATA[age.ALLDATA$year==2019,]
age.2020 <- age.ALLDATA[age.ALLDATA$year==2020,]

## create the weights per year...
weights2017_age <- anesrake(targets_age, age.2017, caseid=age.2017$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_age <- anesrake(targets_age, age.2018, caseid=age.2018$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_age <- anesrake(targets_age, age.2019, caseid=age.2019$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_age <- anesrake(targets_age, age.2020, caseid=age.2020$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_age <- as.data.frame(weights2017_age[c("caseid", "weightvec")])
weights2018_age  <- as.data.frame(weights2018_age[c("caseid", "weightvec")])
weights2019_age  <- as.data.frame(weights2019_age[c("caseid", "weightvec")])
weights2020_age  <- as.data.frame(weights2020_age[c("caseid", "weightvec")])

weights.all_age <- rbind(weights2017_age, weights2018_age,
                               weights2019_age, weights2020_age)

## now create the vector of dscores and weights and month variables
colnames(weights.all_age) <- c("session_id", "weightvec")
weights.all_age$session_id <- as.character(weights.all_age$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesAge_NEW <- age.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                                "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                                "politicalid.new", "edu.new", "Order", "num.new",
                                                "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesAge_NEW <- merge(timeseriesAge_NEW, weights.all_age, by = "session_id")

write.csv(timeseriesAge_NEW, "timeseriesAge_NEW.csv")

## DISABILITY ------------------------------------------------
disability.ALLDATA <- disability.ALLDATA[disability.ALLDATA$year>=2007,]
table(disability.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(disability.ALLDATA$gender, disability.ALLDATA$year)
disability.ALLDATA$gender <- factor(disability.ALLDATA$gender, levels = c("female", "male"))
### race
table(disability.ALLDATA$race, disability.ALLDATA$year)
### education
table(disability.ALLDATA$edu, disability.ALLDATA$year)
levels(disability.ALLDATA$edu)[c(6,7,9)] <- c("associates degree", "bachelors degree", "masters degree")
disability.ALLDATA$educats <- cut(as.numeric(disability.ALLDATA$edu), c(0,4,7,9,14))
table(disability.ALLDATA$educats, disability.ALLDATA$year)
### age
table(disability.ALLDATA$age, disability.ALLDATA$year)
disability.ALLDATA$agecats <- cut(disability.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(disability.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                       "age4554", "age5562", "age6399")
table(disability.ALLDATA$agecats, disability.ALLDATA$year)
# now all age categories for each year
### political
table(disability.ALLDATA$pol, disability.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_disability <- as.data.frame(table(disability.ALLDATA$gender)/sum(table(disability.ALLDATA$gender)))
race.weights_disability <- as.data.frame(table(disability.ALLDATA$race)/sum(table(disability.ALLDATA$race)))
edu.weights_disability <- as.data.frame(table(disability.ALLDATA$educats)/sum(table(disability.ALLDATA$educats)))
age.weights_disability <- as.data.frame(table(disability.ALLDATA$agecats)/sum(table(disability.ALLDATA$agecats)))
pol.weights_disability <- as.data.frame(table(disability.ALLDATA$pol)/sum(table(disability.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_disability <- gender.weights_disability$Freq
race.weights_disability <- race.weights_disability$Freq
edu.weights_disability <- edu.weights_disability$Freq
age.weights_disability <- age.weights_disability$Freq
pol.weights_disability <- pol.weights_disability$Freq

## targets
targets_disability <- list(gender.weights_disability, race.weights_disability, edu.weights_disability, age.weights_disability, pol.weights_disability)
names(targets_disability) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_disability

# new matched variables in the data set
disability.ALLDATA$Wgender <- as.numeric(disability.ALLDATA$gender)
disability.ALLDATA$Wrace <- as.numeric(disability.ALLDATA$race)
disability.ALLDATA$Wedu <- as.numeric(disability.ALLDATA$educats)
disability.ALLDATA$Wage <- as.numeric(disability.ALLDATA$agecats)
disability.ALLDATA$Wpol <- as.numeric(disability.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
disability.2017 <- disability.ALLDATA[disability.ALLDATA$year==2017,]
disability.2018 <- disability.ALLDATA[disability.ALLDATA$year==2018,]
disability.2019 <- disability.ALLDATA[disability.ALLDATA$year==2019,]
disability.2020 <- disability.ALLDATA[disability.ALLDATA$year==2020,]

## create the weights per year...
weights2017_disability <- anesrake(targets_disability, disability.2017, caseid=disability.2017$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_disability <- anesrake(targets_disability, disability.2018, caseid=disability.2018$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_disability <- anesrake(targets_disability, disability.2019, caseid=disability.2019$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_disability <- anesrake(targets_disability, disability.2020, caseid=disability.2020$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_disability <- as.data.frame(weights2017_disability[c("caseid", "weightvec")])
weights2018_disability  <- as.data.frame(weights2018_disability[c("caseid", "weightvec")])
weights2019_disability  <- as.data.frame(weights2019_disability[c("caseid", "weightvec")])
weights2020_disability  <- as.data.frame(weights2020_disability[c("caseid", "weightvec")])

weights.all_disability <- rbind(weights2017_disability, weights2018_disability,
                               weights2019_disability, weights2020_disability)

## now create the vector of dscores and weights and month variables
colnames(weights.all_disability) <- c("session_id", "weightvec")
weights.all_disability$session_id <- as.character(weights.all_disability$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesDis_NEW <- disability.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                                "age.new", "genderidentity_simp", "race.new", "ethnicity.new", "reporteddis",
                                                "politicalid.new", "edu.new", "Order", "num.new",
                                                "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesDis_NEW <- merge(timeseriesDis_NEW, weights.all_disability, by = "session_id")

write.csv(timeseriesDis_NEW, "timeseriesDis_NEW.csv")


## WEIGHT ------------------------------------------------
weight.ALLDATA <- weight.ALLDATA[weight.ALLDATA$year>=2007,]
table(weight.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(weight.ALLDATA$gender, weight.ALLDATA$year)
weight.ALLDATA$gender <- factor(weight.ALLDATA$gender, levels = c("female", "male"))
### race
table(weight.ALLDATA$race, weight.ALLDATA$year)
### education
table(weight.ALLDATA$edu, weight.ALLDATA$year)
levels(weight.ALLDATA$edu)[c(6,7,9)] <- c("associates degree", "bachelors degree", "masters degree")
weight.ALLDATA$educats <- cut(as.numeric(weight.ALLDATA$edu), c(0,4,7,9,14))
table(weight.ALLDATA$educats, weight.ALLDATA$year)
### age
table(weight.ALLDATA$age, weight.ALLDATA$year)
weight.ALLDATA$agecats <- cut(weight.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(weight.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                       "age4554", "age5562", "age6399")
table(weight.ALLDATA$agecats, weight.ALLDATA$year)
# now all age categories for each year
### political
table(weight.ALLDATA$pol, weight.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_weight <- as.data.frame(table(weight.ALLDATA$gender)/sum(table(weight.ALLDATA$gender)))
race.weights_weight <- as.data.frame(table(weight.ALLDATA$race)/sum(table(weight.ALLDATA$race)))
edu.weights_weight <- as.data.frame(table(weight.ALLDATA$educats)/sum(table(weight.ALLDATA$educats)))
age.weights_weight <- as.data.frame(table(weight.ALLDATA$agecats)/sum(table(weight.ALLDATA$agecats)))
pol.weights_weight <- as.data.frame(table(weight.ALLDATA$pol)/sum(table(weight.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_weight <- gender.weights_weight$Freq
race.weights_weight <- race.weights_weight$Freq
edu.weights_weight <- edu.weights_weight$Freq
age.weights_weight <- age.weights_weight$Freq
pol.weights_weight <- pol.weights_weight$Freq

## targets
targets_weight <- list(gender.weights_weight, race.weights_weight, edu.weights_weight, age.weights_weight, pol.weights_weight)
names(targets_weight) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_weight

# new matched variables in the data set
weight.ALLDATA$Wgender <- as.numeric(weight.ALLDATA$gender)
weight.ALLDATA$Wrace <- as.numeric(weight.ALLDATA$race)
weight.ALLDATA$Wedu <- as.numeric(weight.ALLDATA$educats)
weight.ALLDATA$Wage <- as.numeric(weight.ALLDATA$agecats)
weight.ALLDATA$Wpol <- as.numeric(weight.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
weight.2017 <- weight.ALLDATA[weight.ALLDATA$year==2017,]
weight.2018 <- weight.ALLDATA[weight.ALLDATA$year==2018,]
weight.2019 <- weight.ALLDATA[weight.ALLDATA$year==2019,]
weight.2020 <- weight.ALLDATA[weight.ALLDATA$year==2020,]

## create the weights per year...
weights2017_weight <- anesrake(targets_weight, weight.2017, caseid=weight.2017$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_weight <- anesrake(targets_weight, weight.2018, caseid=weight.2018$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_weight <- anesrake(targets_weight, weight.2019, caseid=weight.2019$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_weight <- anesrake(targets_weight, weight.2020, caseid=weight.2020$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_weight <- as.data.frame(weights2017_weight[c("caseid", "weightvec")])
weights2018_weight  <- as.data.frame(weights2018_weight[c("caseid", "weightvec")])
weights2019_weight  <- as.data.frame(weights2019_weight[c("caseid", "weightvec")])
weights2020_weight  <- as.data.frame(weights2020_weight[c("caseid", "weightvec")])

weights.all_weight <- rbind(weights2017_weight, weights2018_weight,
                               weights2019_weight, weights2020_weight)

## now create the vector of dscores and weights and month variables
colnames(weights.all_weight) <- c("session_id", "weightvec")
weights.all_weight$session_id <- as.character(weights.all_weight$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesWeight_NEW <- weight.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                                "age.new", "genderidentity_simp", "race.new", "ethnicity.new", "reportedweight",
                                                "politicalid.new", "edu.new", "Order", "num.new",
                                                "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesWeight_NEW <- merge(timeseriesWeight_NEW, weights.all_weight, by = "session_id")

write.csv(timeseriesWeight_NEW, "timeseriesWeight_NEW.csv")


## SKIN-TONE ------------------------------------------------
skin.ALLDATA <- skin.ALLDATA[skin.ALLDATA$year>=2007,]
table(skin.ALLDATA$year)

## First we need to clean the factor levels
### sex
table(skin.ALLDATA$gender, skin.ALLDATA$year)
skin.ALLDATA$gender <- recode(skin.ALLDATA$gender, "'Female' = 'female'; 'Male' = 'male'")
skin.ALLDATA$gender <- factor(skin.ALLDATA$gender, levels = c("female", "male"))
### race
table(skin.ALLDATA$race, skin.ALLDATA$year)
### education
table(skin.ALLDATA$edu, skin.ALLDATA$year)
levels(skin.ALLDATA$edu)[c(6,7,9)] <- c("associates degree", "bachelors degree", "masters degree")
skin.ALLDATA$educats <- cut(as.numeric(skin.ALLDATA$edu), c(0,4,7,9,14))
table(skin.ALLDATA$educats, skin.ALLDATA$year)
### age
table(skin.ALLDATA$age, skin.ALLDATA$year)
skin.ALLDATA$agecats <- cut(skin.ALLDATA$age, c(0, 25,35,45,55,63,99))
levels(skin.ALLDATA$agecats) <- c("age0024", "age2534", "age3544",
                                       "age4554", "age5562", "age6399")
table(skin.ALLDATA$agecats, skin.ALLDATA$year)
# now all age categories for each year
### political
table(skin.ALLDATA$pol, skin.ALLDATA$year)

#### get the weights of the full population (2007 - 2020)
gender.weights_skin <- as.data.frame(table(skin.ALLDATA$gender)/sum(table(skin.ALLDATA$gender)))
race.weights_skin <- as.data.frame(table(skin.ALLDATA$race)/sum(table(skin.ALLDATA$race)))
edu.weights_skin <- as.data.frame(table(skin.ALLDATA$educats)/sum(table(skin.ALLDATA$educats)))
age.weights_skin <- as.data.frame(table(skin.ALLDATA$agecats)/sum(table(skin.ALLDATA$agecats)))
pol.weights_skin <- as.data.frame(table(skin.ALLDATA$pol)/sum(table(skin.ALLDATA$pol)))

#### create the weights (frequency tables)
gender.weights_skin <- gender.weights_skin$Freq
race.weights_skin <- race.weights_skin$Freq
edu.weights_skin <- edu.weights_skin$Freq
age.weights_skin <- age.weights_skin$Freq
pol.weights_skin <- pol.weights_skin$Freq

## targets
targets_skin <- list(gender.weights_skin, race.weights_skin, edu.weights_skin, age.weights_skin, pol.weights_skin)
names(targets_skin) <- c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")
targets_skin

# new matched variables in the data set
skin.ALLDATA$Wgender <- as.numeric(skin.ALLDATA$gender)
skin.ALLDATA$Wrace <- as.numeric(skin.ALLDATA$race)
skin.ALLDATA$Wedu <- as.numeric(skin.ALLDATA$educats)
skin.ALLDATA$Wage <- as.numeric(skin.ALLDATA$agecats)
skin.ALLDATA$Wpol <- as.numeric(skin.ALLDATA$pol)

# now get sub-data for each NEWDATA year, to equate to full population
skin.2017 <- skin.ALLDATA[skin.ALLDATA$year==2017,]
skin.2018 <- skin.ALLDATA[skin.ALLDATA$year==2018,]
skin.2019 <- skin.ALLDATA[skin.ALLDATA$year==2019,]
skin.2020 <- skin.ALLDATA[skin.ALLDATA$year==2020,]

## create the weights per year...
weights2017_skin <- anesrake(targets_skin, skin.2017, caseid=skin.2017$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2018_skin <- anesrake(targets_skin, skin.2018, caseid=skin.2018$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2019_skin <- anesrake(targets_skin, skin.2019, caseid=skin.2019$session_id,
                                  verbose=TRUE,type="nolim",convcrit=0.005)
weights2020_skin <- anesrake(targets_skin, skin.2020, caseid=skin.2020$session_id,
                              verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017_skin <- as.data.frame(weights2017_skin[c("caseid", "weightvec")])
weights2018_skin  <- as.data.frame(weights2018_skin[c("caseid", "weightvec")])
weights2019_skin  <- as.data.frame(weights2019_skin[c("caseid", "weightvec")])
weights2020_skin  <- as.data.frame(weights2020_skin[c("caseid", "weightvec")])

weights.all_skin <- rbind(weights2017_skin, weights2018_skin,
                               weights2019_skin, weights2020_skin)

## now create the vector of dscores and weights and month variables
colnames(weights.all_skin) <- c("session_id", "weightvec")
weights.all_skin$session_id <- as.character(weights.all_skin$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
timeseriesSkin_NEW <- skin.full.demogsNEW[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                                "age.new", "genderidentity_simp", "race.new", "ethnicity.new", "reportedskin", 
                                                "politicalid.new", "edu.new", "Order", "num.new",
                                                "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
timeseriesSkin_NEW <- merge(timeseriesSkin_NEW, weights.all_skin, by = "session_id")

write.csv(timeseriesSkin_NEW, "timeseriesSkin_NEW.csv")


# Quick cleaning of factor levels ------
## Race
timeseriesAge_NEW$racesimple <- recode(timeseriesAge_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesAge_NEW$racesimple <- factor(timeseriesAge_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))

timeseriesDis_NEW$racesimple <- recode(timeseriesDis_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesDis_NEW$racesimple <- factor(timeseriesDis_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))

timeseriesSkin_NEW$racesimple <- recode(timeseriesSkin_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesSkin_NEW$racesimple <- factor(timeseriesSkin_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))

timeseriesRace_NEW$racesimple <- recode(timeseriesRace_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesRace_NEW$racesimple <- factor(timeseriesRace_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))

timeseriesSex_NEW$racesimple <- recode(timeseriesSex_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesSex_NEW$racesimple <- factor(timeseriesSex_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))

timeseriesWeight_NEW$racesimple <- recode(timeseriesWeight_NEW$race.new, "'South Asian' = 'Asian';
                                       'East Asian' = 'Asian'; 'White' = 'White';
                                       'Black or African American' = 'Black';
                                       'Multiracial' = 'Multiracial';
                                       else = 'Other'")
timeseriesWeight_NEW$racesimple <- factor(timeseriesWeight_NEW$racesimple, levels = c("White", "Black", "Asian", "Multiracial"))


## Education
timeseriesAge_NEW$edu.new <- factor(timeseriesAge_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                          "some high school", "high school graduate", "some college",
                                                                          "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                          "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesAge_NEW$edu.releveled <- as.numeric(timeseriesAge_NEW$edu.new) - 5
timeseriesAge_NEW$edu.grp <- ifelse(timeseriesAge_NEW$edu.releveled < 0, "ncol", "col")

timeseriesDis_NEW$edu.new <- factor(timeseriesDis_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                          "some high school", "high school graduate", "some college",
                                                                          "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                          "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesDis_NEW$edu.releveled <- as.numeric(timeseriesDis_NEW$edu.new) - 5
timeseriesDis_NEW$edu.grp <- ifelse(timeseriesDis_NEW$edu.releveled < 0, "ncol", "col")

timeseriesSkin_NEW$edu.new <- factor(timeseriesSkin_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                          "some high school", "high school graduate", "some college",
                                                                          "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                          "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesSkin_NEW$edu.releveled <- as.numeric(timeseriesSkin_NEW$edu.new) - 5
timeseriesSkin_NEW$edu.grp <- ifelse(timeseriesSkin_NEW$edu.releveled < 0, "ncol", "col")

timeseriesSex_NEW$edu.new <- factor(timeseriesSex_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                          "some high school", "high school graduate", "some college",
                                                                          "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                          "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesSex_NEW$edu.releveled <- as.numeric(timeseriesSex_NEW$edu.new) - 5
timeseriesSex_NEW$edu.grp <- ifelse(timeseriesSex_NEW$edu.releveled < 0, "ncol", "col")

timeseriesRace_NEW$edu.new <- factor(timeseriesRace_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                          "some high school", "high school graduate", "some college",
                                                                          "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                          "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesRace_NEW$edu.releveled <- as.numeric(timeseriesRace_NEW$edu.new) - 5
timeseriesRace_NEW$edu.grp <- ifelse(timeseriesRace_NEW$edu.releveled < 0, "ncol", "col")

timeseriesWeight_NEW$edu.new <- factor(timeseriesWeight_NEW$edu.new, levels = c("elementary school", "junior high", 
                                                                            "some high school", "high school graduate", "some college",
                                                                            "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                            "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
timeseriesWeight_NEW$edu.releveled <- as.numeric(timeseriesWeight_NEW$edu.new) - 5
timeseriesWeight_NEW$edu.grp <- ifelse(timeseriesWeight_NEW$edu.releveled < 0, "ncol", "col")


## Politics
timeseriesAge_NEW$politicalid.new <- factor(timeseriesAge_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                  "moderately conservative",
                                                                                  "slightly conservative",
                                                                                  "neutral",
                                                                                  "slightly liberal",
                                                                                  "moderately liberal",
                                                                                  "strongly liberal"))
timeseriesAge_NEW$politicalid.releveled <- as.numeric(timeseriesAge_NEW$politicalid.new) - 4
timeseriesAge_NEW$pol.grp <- factor(ifelse(timeseriesAge_NEW$politicalid.releveled < 0, "cons", 
                                    ifelse(timeseriesAge_NEW$politicalid.releveled == 0, "neutral", "lib")))

timeseriesDis_NEW$politicalid.new <- factor(timeseriesDis_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                          "moderately conservative",
                                                                                          "slightly conservative",
                                                                                          "neutral",
                                                                                          "slightly liberal",
                                                                                          "moderately liberal",
                                                                                          "strongly liberal"))
timeseriesDis_NEW$politicalid.releveled <- as.numeric(timeseriesDis_NEW$politicalid.new) - 4
timeseriesDis_NEW$pol.grp <- factor(ifelse(timeseriesDis_NEW$politicalid.releveled < 0, "cons", 
                                    ifelse(timeseriesDis_NEW$politicalid.releveled == 0, "neutral", "lib")))

timeseriesSkin_NEW$politicalid.new <- factor(timeseriesSkin_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                          "moderately conservative",
                                                                                          "slightly conservative",
                                                                                          "neutral",
                                                                                          "slightly liberal",
                                                                                          "moderately liberal",
                                                                                          "strongly liberal"))
timeseriesSkin_NEW$politicalid.releveled <- as.numeric(timeseriesSkin_NEW$politicalid.new) - 4
timeseriesSkin_NEW$pol.grp <- factor(ifelse(timeseriesSkin_NEW$politicalid.releveled < 0, "cons", 
                                     ifelse(timeseriesSkin_NEW$politicalid.releveled == 0, "neutral", "lib")))

timeseriesSex_NEW$politicalid.new <- factor(timeseriesSex_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                          "moderately conservative",
                                                                                          "slightly conservative",
                                                                                          "neutral",
                                                                                          "slightly liberal",
                                                                                          "moderately liberal",
                                                                                          "strongly liberal"))
timeseriesSex_NEW$politicalid.releveled <- as.numeric(timeseriesSex_NEW$politicalid.new) - 4
timeseriesSex_NEW$pol.grp <- factor(ifelse(timeseriesSex_NEW$politicalid.releveled < 0, "cons", 
                                    ifelse(timeseriesSex_NEW$politicalid.releveled == 0, "neutral", "lib")))

timeseriesRace_NEW$politicalid.new <- factor(timeseriesRace_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                          "moderately conservative",
                                                                                          "slightly conservative",
                                                                                          "neutral",
                                                                                          "slightly liberal",
                                                                                          "moderately liberal",
                                                                                          "strongly liberal"))
timeseriesRace_NEW$politicalid.releveled <- as.numeric(timeseriesRace_NEW$politicalid.new) - 4
timeseriesRace_NEW$pol.grp <- factor(ifelse(timeseriesRace_NEW$politicalid.releveled < 0, "cons", 
                                     ifelse(timeseriesRace_NEW$politicalid.releveled == 0, "neutral", "lib")))

timeseriesWeight_NEW$politicalid.new <- factor(timeseriesWeight_NEW$politicalid.new, levels = c("strongly conservative",
                                                                                          "moderately conservative",
                                                                                          "slightly conservative",
                                                                                          "neutral",
                                                                                          "slightly liberal",
                                                                                          "moderately liberal",
                                                                                          "strongly liberal"))
timeseriesWeight_NEW$politicalid.releveled <- as.numeric(timeseriesWeight_NEW$politicalid.new) - 4
timeseriesWeight_NEW$pol.grp <- factor(ifelse(timeseriesWeight_NEW$politicalid.releveled < 0, "cons", 
                                       ifelse(timeseriesWeight_NEW$politicalid.releveled == 0, "neutral", "lib")))



## Explicit attitudes
timeseriesAge_NEW$att.new <- factor(timeseriesAge_NEW$att.new , levels = c("I strongly prefer old people to young people.",
                                                  "I moderately prefer old people to young people.",
                                                  "I slightly prefer old people to young people.",
                                                  "I like young people and old people equally.",
                                                  "I slightly prefer young people to old people.",
                                                  "I moderately prefer young people to old people.",
                                                  "I strongly prefer young people to old people."))
timeseriesDis_NEW$att.new <- factor(timeseriesDis_NEW$att.new, levels = c("I strongly prefer disabled people to abled people.",
                                                                          "I moderately prefer disabled people to abled people.",
                                                                          "I slightly prefer disabled people to abled people.",
                                                                          "I like abled people and disabled people equally.",
                                                                          "I slightly prefer abled people to disabled people.",
                                                                          "I moderately prefer abled people to disabled people.",
                                                                          "I strongly prefer abled people to disabled people."))

timeseriesSkin_NEW$att.new <- factor(timeseriesSkin_NEW$att.new, levels = c("I strongly prefer Dark Skinned People to Light Skinned People.",
                                                                            "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                                            "I slightly prefer Dark Skinned People to Light Skinned People.",
                                                                            "I like Light Skinned People and Dark Skinned People equally.",
                                                                            "I slightly prefer Light Skinned People to Dark Skinned People.",
                                                                            "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                                            "I strongly prefer Light Skinned People to Dark Skinned People."))

timeseriesRace_NEW$att.new <- factor(timeseriesRace_NEW$att.new, levels = c("I strongly prefer African Americans to European Americans.",
                                                                            "I moderately prefer African Americans to European Americans.",
                                                                            "I slightly prefer African Americans to European Americans.",
                                                                            "I like European Americans and African Americans equally.",
                                                                            "I slightly prefer European Americans to African Americans.",
                                                                            "I moderately prefer European Americans to African Americans.",
                                                                            "I strongly prefer European Americans to African Americans."))

timeseriesSex_NEW$att.new <- factor(timeseriesSex_NEW$att.new, levels = c("I strongly prefer Gay People to Straight People.",
                                                                          "I moderately prefer Gay People to Straight People.",
                                                                          "I slightly prefer Gay People to Straight People.",
                                                                          "I like Straight People and Gay People equally.",
                                                                          "I slightly prefer Straight People to Gay People.",
                                                                          "I moderately prefer Straight People to Gay People.",
                                                                          "I strongly prefer Straight People to Gay People."))

timeseriesWeight_NEW$att.new <- factor(timeseriesWeight_NEW$att.new, levels = c("I strongly prefer Fat People to Thin People.",
                                                                                "I moderately prefer Fat People to Thin People.",
                                                                                "I slightly prefer Fat People to Thin People.",
                                                                                "I like Thin People and Fat People equally.",
                                                                                "I slightly prefer Thin People to Fat People.",
                                                                                "I moderately prefer Thin People to Fat People.",
                                                                                "I strongly prefer Thin People to Fat People."))

timeseriesAge_NEW$attnumeric <- as.numeric(timeseriesAge_NEW$att.new) - 4
timeseriesDis_NEW$attnumeric <- as.numeric(timeseriesDis_NEW$att.new) - 4
timeseriesSkin_NEW$attnumeric <- as.numeric(timeseriesSkin_NEW$att.new) - 4
timeseriesRace_NEW$attnumeric <- as.numeric(timeseriesRace_NEW$att.new) - 4
timeseriesSex_NEW$attnumeric <- as.numeric(timeseriesSex_NEW$att.new) - 4
timeseriesWeight_NEW$attnumeric <- as.numeric(timeseriesWeight_NEW$att.new) - 4



### SAVE OUT overall weighting data frames -----
save(list = c("timeseriesAge_NEW", "timeseriesDis_NEW", "timeseriesSex_NEW",
              "timeseriesRace_NEW", "timeseriesSkin_NEW", "timeseriesWeight_NEW"),
     file = "timeseries_NEWDATA.RData")



####################################################################################################################################

## **** DEMOGRAPHICS WEIGHTING ---------------
# We will work from the timeseriesX_NEW objects which no longer have the Wgender, etc. variables. Therefore add them all back in here.
## For Race Attitudes
timeseriesRace_NEW$Wgender <- as.numeric(factor(timeseriesRace_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesRace_NEW$Wrace <- recode(timeseriesRace_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesRace_NEW$Wrace <- as.numeric(timeseriesRace_NEW$Wrace)
timeseriesRace_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesRace_NEW$edu.new), c(0,4,7,9,14)))
timeseriesRace_NEW$Wage <- cut(timeseriesRace_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesRace_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesRace_NEW$Wage <- as.numeric(timeseriesRace_NEW$Wage)
timeseriesRace_NEW$Wpol <- as.numeric(timeseriesRace_NEW$politicalid.new)

## For Skin Attitudes:
timeseriesSkin_NEW$Wgender <- as.numeric(factor(timeseriesSkin_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesSkin_NEW$Wrace <- recode(timeseriesSkin_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesSkin_NEW$Wrace <- as.numeric(timeseriesSkin_NEW$Wrace)
timeseriesSkin_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesSkin_NEW$edu.new), c(0,4,7,9,14)))
timeseriesSkin_NEW$Wage <- cut(timeseriesSkin_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesSkin_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesSkin_NEW$Wage <- as.numeric(timeseriesSkin_NEW$Wage)
timeseriesSkin_NEW$Wpol <- as.numeric(timeseriesSkin_NEW$politicalid.new)

## For Age Attitudes:
timeseriesAge_NEW$Wgender <- as.numeric(factor(timeseriesAge_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesAge_NEW$Wrace <- recode(timeseriesAge_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesAge_NEW$Wrace <- as.numeric(timeseriesAge_NEW$Wrace)
timeseriesAge_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesAge_NEW$edu.new), c(0,4,7,9,14)))
timeseriesAge_NEW$Wage <- cut(timeseriesAge_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesAge_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesAge_NEW$Wage <- as.numeric(timeseriesAge_NEW$Wage)
timeseriesAge_NEW$Wpol <- as.numeric(timeseriesAge_NEW$politicalid.new)

## For Dis Attitudes:
timeseriesDis_NEW$Wgender <- as.numeric(factor(timeseriesDis_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesDis_NEW$Wrace <- recode(timeseriesDis_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesDis_NEW$Wrace <- as.numeric(timeseriesDis_NEW$Wrace)
timeseriesDis_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesDis_NEW$edu.new), c(0,4,7,9,14)))
timeseriesDis_NEW$Wage <- cut(timeseriesDis_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesDis_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesDis_NEW$Wage <- as.numeric(timeseriesDis_NEW$Wage)
timeseriesDis_NEW$Wpol <- as.numeric(timeseriesDis_NEW$politicalid.new)

## For Sex Attitudes:
timeseriesSex_NEW$Wgender <- as.numeric(factor(timeseriesSex_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesSex_NEW$Wrace <- recode(timeseriesSex_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesSex_NEW$Wrace <- as.numeric(timeseriesSex_NEW$Wrace)
timeseriesSex_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesSex_NEW$edu.new), c(0,4,7,9,14)))
timeseriesSex_NEW$Wage <- cut(timeseriesSex_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesSex_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesSex_NEW$Wage <- as.numeric(timeseriesSex_NEW$Wage)
timeseriesSex_NEW$Wpol <- as.numeric(timeseriesSex_NEW$politicalid.new)

## For Weight Attitudes:
timeseriesWeight_NEW$Wgender <- as.numeric(factor(timeseriesWeight_NEW$genderidentity_simp, 
                                                levels = c("female", "male")))
timeseriesWeight_NEW$Wrace <- recode(timeseriesWeight_NEW$race.new, "'Other or unknown' = 'Other or Unknown';
                                       'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                       'Native Hawaiian or other Pacific Islander' = 'Asian'")
timeseriesWeight_NEW$Wrace <- as.numeric(timeseriesWeight_NEW$Wrace)
timeseriesWeight_NEW$Wedu <- as.numeric(cut(as.numeric(timeseriesWeight_NEW$edu.new), c(0,4,7,9,14)))
timeseriesWeight_NEW$Wage <- cut(timeseriesWeight_NEW$age.new, c(0, 25,35,45,55,63,99))
levels(timeseriesWeight_NEW$Wage) <- c("age0024", "age2534", "age3544",
                                     "age4554", "age5562", "age6399")
timeseriesWeight_NEW$Wage <- as.numeric(timeseriesWeight_NEW$Wage)
timeseriesWeight_NEW$Wpol <- as.numeric(timeseriesWeight_NEW$politicalid.new)


## RACE ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.race <- targets_race[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.race

## Get sub-data for each year and each political identity, to then equate to full population
race.lib.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$Wpol>4,]
race.lib.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$Wpol>4,]
race.lib.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$Wpol>4,]
race.lib.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$Wpol>4,]
# repeat for conservatives
race.cons.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$Wpol<4,] 
race.cons.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$Wpol<4,]
race.cons.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$Wpol<4,]
race.cons.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.race.lib <- anesrake(targets.pol.race, race.lib.2017, caseid=race.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.race.lib <- anesrake(targets.pol.race, race.lib.2018, caseid=race.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.race.lib <- anesrake(targets.pol.race, race.lib.2019, caseid=race.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.race.lib <- anesrake(targets.pol.race, race.lib.2020, caseid=race.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.race.lib <- as.data.frame(weights2017.pol.race.lib[c("caseid", "weightvec")])
weights2018.pol.race.lib <- as.data.frame(weights2018.pol.race.lib[c("caseid", "weightvec")])
weights2019.pol.race.lib <- as.data.frame(weights2019.pol.race.lib[c("caseid", "weightvec")])
weights2020.pol.race.lib <- as.data.frame(weights2020.pol.race.lib[c("caseid", "weightvec")])

weights.all.pol.race.lib <- rbind(weights2017.pol.race.lib, weights2018.pol.race.lib, weights2019.pol.race.lib, weights2020.pol.race.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.race.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.race.lib$session_id <- as.character(weights.all.pol.race.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.liberals <- timeseriesRace_NEW[timeseriesRace_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
race.liberals <- merge(timeseriesRace_NEW, weights.all.pol.race.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.race.cons <- anesrake(targets.pol.race, race.cons.2017, caseid=race.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.race.cons <- anesrake(targets.pol.race, race.cons.2018, caseid=race.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.race.cons <- anesrake(targets.pol.race, race.cons.2019, caseid=race.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.race.cons <- anesrake(targets.pol.race, race.cons.2020, caseid=race.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.race.cons <- as.data.frame(weights2017.pol.race.cons[c("caseid", "weightvec")])
weights2018.pol.race.cons <- as.data.frame(weights2018.pol.race.cons[c("caseid", "weightvec")])
weights2019.pol.race.cons <- as.data.frame(weights2019.pol.race.cons[c("caseid", "weightvec")])
weights2020.pol.race.cons <- as.data.frame(weights2020.pol.race.cons[c("caseid", "weightvec")])

weights.all.pol.race.cons <- rbind(weights2017.pol.race.cons, weights2018.pol.race.cons, weights2019.pol.race.cons, weights2020.pol.race.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.race.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.race.cons$session_id <- as.character(weights.all.pol.race.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.conserv <- timeseriesRace_NEW[timeseriesRace_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
race.conserv <- merge(timeseriesRace_NEW, weights.all.pol.race.cons, by = "session_id")

race.libcons <- rbind(race.liberals, race.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.race <- targets_race[c("Wpol", "Wrace", "Wedu", "Wage")]

## Get sub-data for each year and each group, to then equate to full population
race.fem.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$genderidentity_simp == "female",]
race.fem.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$genderidentity_simp == "female",]
race.fem.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$genderidentity_simp == "female",]
race.fem.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$genderidentity_simp == "female",]
# repeat for other group
race.mal.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$genderidentity_simp == "male",] 
race.mal.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$genderidentity_simp == "male",]
race.mal.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$genderidentity_simp == "male",]
race.mal.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.race.fem <- anesrake(targets.gender.race, race.fem.2017, caseid=race.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.race.fem <- anesrake(targets.gender.race, race.fem.2018, caseid=race.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.race.fem <- anesrake(targets.gender.race, race.fem.2019, caseid=race.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.race.fem <- anesrake(targets.gender.race, race.fem.2020, caseid=race.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.race.fem <- as.data.frame(weights2017.gender.race.fem[c("caseid", "weightvec")])
weights2018.gender.race.fem <- as.data.frame(weights2018.gender.race.fem[c("caseid", "weightvec")])
weights2019.gender.race.fem <- as.data.frame(weights2019.gender.race.fem[c("caseid", "weightvec")])
weights2020.gender.race.fem <- as.data.frame(weights2020.gender.race.fem[c("caseid", "weightvec")])

weights.all.gender.race.fem <- rbind(weights2017.gender.race.fem, weights2018.gender.race.fem, weights2019.gender.race.fem, weights2020.gender.race.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.race.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.race.fem$session_id <- as.character(weights.all.gender.race.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.fem <- timeseriesRace_NEW[timeseriesRace_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
race.fem <- merge(timeseriesRace_NEW, weights.all.gender.race.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.race.mal <- anesrake(targets.gender.race, race.mal.2017, caseid=race.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.race.mal <- anesrake(targets.gender.race, race.mal.2018, caseid=race.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.race.mal <- anesrake(targets.gender.race, race.mal.2019, caseid=race.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.race.mal <- anesrake(targets.gender.race, race.mal.2020, caseid=race.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.race.mal <- as.data.frame(weights2017.gender.race.mal[c("caseid", "weightvec")])
weights2018.gender.race.mal <- as.data.frame(weights2018.gender.race.mal[c("caseid", "weightvec")])
weights2019.gender.race.mal <- as.data.frame(weights2019.gender.race.mal[c("caseid", "weightvec")])
weights2020.gender.race.mal <- as.data.frame(weights2020.gender.race.mal[c("caseid", "weightvec")])

weights.all.gender.race.mal <- rbind(weights2017.gender.race.mal, weights2018.gender.race.mal, weights2019.gender.race.mal, weights2020.gender.race.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.race.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.race.mal$session_id <- as.character(weights.all.gender.race.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.mal <- timeseriesRace_NEW[timeseriesRace_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
race.mal <- merge(timeseriesRace_NEW, weights.all.gender.race.mal, by = "session_id")

race.femmal <- rbind(race.fem, race.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.race <- targets_race[c("Wpol", "Wrace", "Wgender", "Wage")]

timeseriesRace_NEW$edu.grp <- as.numeric(timeseriesRace_NEW$edu.new) - 5
timeseriesRace_NEW$edu.grp <- ifelse(timeseriesRace_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
race.ncol.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$edu.grp == "nocollege",]
race.ncol.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$edu.grp == "nocollege",]
race.ncol.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$edu.grp == "nocollege",]
race.ncol.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$edu.grp == "nocollege",]
# repeat for other group
race.col.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$edu.grp == "college",] 
race.col.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$edu.grp == "college",]
race.col.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$edu.grp == "college",]
race.col.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.race.ncol <- anesrake(targets.edu.race, race.ncol.2017, caseid=race.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.race.ncol <- anesrake(targets.edu.race, race.ncol.2018, caseid=race.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.race.ncol <- anesrake(targets.edu.race, race.ncol.2019, caseid=race.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.race.ncol <- anesrake(targets.edu.race, race.ncol.2020, caseid=race.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.race.ncol <- as.data.frame(weights2017.edu.race.ncol[c("caseid", "weightvec")])
weights2018.edu.race.ncol <- as.data.frame(weights2018.edu.race.ncol[c("caseid", "weightvec")])
weights2019.edu.race.ncol <- as.data.frame(weights2019.edu.race.ncol[c("caseid", "weightvec")])
weights2020.edu.race.ncol <- as.data.frame(weights2020.edu.race.ncol[c("caseid", "weightvec")])

weights.all.edu.race.ncol <- rbind(weights2017.edu.race.ncol, weights2018.edu.race.ncol, weights2019.edu.race.ncol, weights2020.edu.race.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.race.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.race.ncol$session_id <- as.character(weights.all.edu.race.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.ncol <- timeseriesRace_NEW[timeseriesRace_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
race.ncol <- merge(timeseriesRace_NEW, weights.all.edu.race.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.race.col <- anesrake(targets.edu.race, race.col.2017, caseid=race.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.race.col <- anesrake(targets.edu.race, race.col.2018, caseid=race.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.race.col <- anesrake(targets.edu.race, race.col.2019, caseid=race.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.race.col <- anesrake(targets.edu.race, race.col.2020, caseid=race.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.race.col <- as.data.frame(weights2017.edu.race.col[c("caseid", "weightvec")])
weights2018.edu.race.col <- as.data.frame(weights2018.edu.race.col[c("caseid", "weightvec")])
weights2019.edu.race.col <- as.data.frame(weights2019.edu.race.col[c("caseid", "weightvec")])
weights2020.edu.race.col <- as.data.frame(weights2020.edu.race.col[c("caseid", "weightvec")])

weights.all.edu.race.col <- rbind(weights2017.edu.race.col, weights2018.edu.race.col, weights2019.edu.race.col, weights2020.edu.race.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.race.col) <- c("session_id", "weightvec.edu")
weights.all.edu.race.col$session_id <- as.character(weights.all.edu.race.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.col <- timeseriesRace_NEW[timeseriesRace_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
race.col <- merge(timeseriesRace_NEW, weights.all.edu.race.col, by = "session_id")

race.ncolcol <- rbind(race.ncol, race.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.race <- targets_race[c("Wpol", "Wgender", "Wedu", "Wage")]

timeseriesRace_NEW$racesimple <- recode(timeseriesRace_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
race.white.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$racesimple == "White",]
race.white.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$racesimple == "White",]
race.white.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$racesimple == "White",]
race.white.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$racesimple == "White",]
# repeat for other group
race.black.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$racesimple == "Black",] 
race.black.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$racesimple == "Black",]
race.black.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$racesimple == "Black",]
race.black.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$racesimple == "Black",]
# repeat for other group
race.asian.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$racesimple == "Asian",] 
race.asian.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$racesimple == "Asian",]
race.asian.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$racesimple == "Asian",]
race.asian.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.race.white <- anesrake(targets.race.race, race.white.2017, caseid=race.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.race.white <- anesrake(targets.race.race, race.white.2018, caseid=race.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.race.white <- anesrake(targets.race.race, race.white.2019, caseid=race.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.race.white <- anesrake(targets.race.race, race.white.2020, caseid=race.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.race.white <- as.data.frame(weights2017.race.race.white[c("caseid", "weightvec")])
weights2018.race.race.white <- as.data.frame(weights2018.race.race.white[c("caseid", "weightvec")])
weights2019.race.race.white <- as.data.frame(weights2019.race.race.white[c("caseid", "weightvec")])
weights2020.race.race.white <- as.data.frame(weights2020.race.race.white[c("caseid", "weightvec")])

weights.all.race.race.white <- rbind(weights2017.race.race.white, weights2018.race.race.white, weights2019.race.race.white, weights2020.race.race.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.race.white) <- c("session_id", "weightvec.race")
weights.all.race.race.white$session_id <- as.character(weights.all.race.race.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.white <- timeseriesRace_NEW[timeseriesRace_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
race.white <- merge(timeseriesRace_NEW, weights.all.race.race.white, by = "session_id")

## create the weights per year for black
weights2017.race.race.black <- anesrake(targets.race.race, race.black.2017, caseid=race.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.race.black <- anesrake(targets.race.race, race.black.2018, caseid=race.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.race.black <- anesrake(targets.race.race, race.black.2019, caseid=race.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.race.black <- anesrake(targets.race.race, race.black.2020, caseid=race.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.race.black <- as.data.frame(weights2017.race.race.black[c("caseid", "weightvec")])
weights2018.race.race.black <- as.data.frame(weights2018.race.race.black[c("caseid", "weightvec")])
weights2019.race.race.black <- as.data.frame(weights2019.race.race.black[c("caseid", "weightvec")])
weights2020.race.race.black <- as.data.frame(weights2020.race.race.black[c("caseid", "weightvec")])

weights.all.race.race.black <- rbind(weights2017.race.race.black, weights2018.race.race.black, weights2019.race.race.black, weights2020.race.race.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.race.black) <- c("session_id", "weightvec.race")
weights.all.race.race.black$session_id <- as.character(weights.all.race.race.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.black <- timeseriesRace_NEW[timeseriesRace_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
race.black <- merge(timeseriesRace_NEW, weights.all.race.race.black, by = "session_id")

## create the weights per year for asian
weights2017.race.race.asian <- anesrake(targets.race.race, race.asian.2017, caseid=race.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.race.asian <- anesrake(targets.race.race, race.asian.2018, caseid=race.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.race.asian <- anesrake(targets.race.race, race.asian.2019, caseid=race.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.race.asian <- anesrake(targets.race.race, race.asian.2020, caseid=race.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.race.asian <- as.data.frame(weights2017.race.race.asian[c("caseid", "weightvec")])
weights2018.race.race.asian <- as.data.frame(weights2018.race.race.asian[c("caseid", "weightvec")])
weights2019.race.race.asian <- as.data.frame(weights2019.race.race.asian[c("caseid", "weightvec")])
weights2020.race.race.asian <- as.data.frame(weights2020.race.race.asian[c("caseid", "weightvec")])

weights.all.race.race.asian <- rbind(weights2017.race.race.asian, weights2018.race.race.asian, weights2019.race.race.asian, weights2020.race.race.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.race.asian) <- c("session_id", "weightvec.race")
weights.all.race.race.asian$session_id <- as.character(weights.all.race.race.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.asian <- timeseriesRace_NEW[timeseriesRace_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
race.asian <- merge(timeseriesRace_NEW, weights.all.race.race.asian, by = "session_id")

race.whblkasi <- rbind(race.white, race.black, race.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.race <- targets_race[c("Wpol", "Wrace", "Wedu", "Wgender")]

timeseriesRace_NEW$age.grp <- cut(timeseriesRace_NEW$age.new, c(0,20,40,99))
levels(timeseriesRace_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
race.young.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$age.grp == "young",]
race.young.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$age.grp == "young",]
race.young.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$age.grp == "young",]
race.young.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$age.grp == "young",]
# repeat for other group
race.middle.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$age.grp == "middle",] 
race.middle.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$age.grp == "middle",]
race.middle.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$age.grp == "middle",]
race.middle.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$age.grp == "middle",]
# repeat for other group
race.old.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$age.grp == "old",] 
race.old.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$age.grp == "old",]
race.old.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$age.grp == "old",]
race.old.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.race.young <- anesrake(targets.age.race, race.young.2017, caseid=race.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.race.young <- anesrake(targets.age.race, race.young.2018, caseid=race.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.race.young <- anesrake(targets.age.race, race.young.2019, caseid=race.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.race.young <- anesrake(targets.age.race, race.young.2020, caseid=race.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for race attitudes, add in workaround due to NULL rownames)
weights2017.age.race.young2 <- weights2017.age.race.young[c("caseid", "weightvec")]
weights2017.age.race.young2 <- as.data.frame(cbind(weights2017.age.race.young2$caseid,
                                               weights2017.age.race.young2$weightvec))
weights2018.age.race.young2 <- weights2018.age.race.young[c("caseid", "weightvec")]
weights2018.age.race.young2 <- as.data.frame(cbind(weights2018.age.race.young2$caseid,
                                                   weights2018.age.race.young2$weightvec))
weights2019.age.race.young2 <- weights2019.age.race.young[c("caseid", "weightvec")]
weights2019.age.race.young2 <- as.data.frame(cbind(weights2019.age.race.young2$caseid,
                                                   weights2019.age.race.young2$weightvec))
weights2020.age.race.young2 <- weights2020.age.race.young[c("caseid", "weightvec")]
weights2020.age.race.young2 <- as.data.frame(cbind(weights2020.age.race.young2$caseid,
                                                   weights2020.age.race.young2$weightvec))

weights.all.age.race.young <- rbind(weights2017.age.race.young2, weights2018.age.race.young2,
                                    weights2019.age.race.young2, weights2020.age.race.young2)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.race.young) <- c("session_id", "weightvec.age")
weights.all.age.race.young$session_id <- as.character(weights.all.age.race.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.young <- timeseriesRace_NEW[timeseriesRace_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
race.young <- merge(timeseriesRace_NEW, weights.all.age.race.young, by = "session_id")

## create the weights per year for middle
weights2017.age.race.middle <- anesrake(targets.age.race, race.middle.2017, caseid=race.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.race.middle <- anesrake(targets.age.race, race.middle.2018, caseid=race.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.race.middle <- anesrake(targets.age.race, race.middle.2019, caseid=race.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.race.middle <- anesrake(targets.age.race, race.middle.2020, caseid=race.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for race attitudes, add in workaround due to NULL rownames)
weights2017.age.race.middle2 <- weights2017.age.race.middle[c("caseid", "weightvec")]
weights2017.age.race.middle2 <- as.data.frame(cbind(weights2017.age.race.middle2$caseid,
                                                   weights2017.age.race.middle2$weightvec))
weights2018.age.race.middle2 <- weights2018.age.race.middle[c("caseid", "weightvec")]
weights2018.age.race.middle2 <- as.data.frame(cbind(weights2018.age.race.middle2$caseid,
                                                   weights2018.age.race.middle2$weightvec))
weights2019.age.race.middle2 <- weights2019.age.race.middle[c("caseid", "weightvec")]
weights2019.age.race.middle2 <- as.data.frame(cbind(weights2019.age.race.middle2$caseid,
                                                   weights2019.age.race.middle2$weightvec))
weights2020.age.race.middle2 <- weights2020.age.race.middle[c("caseid", "weightvec")]
weights2020.age.race.middle2 <- as.data.frame(cbind(weights2020.age.race.middle2$caseid,
                                                   weights2020.age.race.middle2$weightvec))

weights.all.age.race.middle <- rbind(weights2017.age.race.middle2, weights2018.age.race.middle2,
                                    weights2019.age.race.middle2, weights2020.age.race.middle2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.race.middle) <- c("session_id", "weightvec.age")
weights.all.age.race.middle$session_id <- as.character(weights.all.age.race.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.middle <- timeseriesRace_NEW[timeseriesRace_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
race.middle <- merge(timeseriesRace_NEW, weights.all.age.race.middle, by = "session_id")

## create the weights per year for old
weights2017.age.race.old <- anesrake(targets.age.race, race.old.2017, caseid=race.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.race.old <- anesrake(targets.age.race, race.old.2018, caseid=race.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.race.old <- anesrake(targets.age.race, race.old.2019, caseid=race.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.race.old <- anesrake(targets.age.race, race.old.2020, caseid=race.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for race attitudes, add in workaround due to NULL rownames)
weights2017.age.race.old2 <- weights2017.age.race.old[c("caseid", "weightvec")]
weights2017.age.race.old2 <- as.data.frame(cbind(weights2017.age.race.old2$caseid,
                                                   weights2017.age.race.old2$weightvec))
weights2018.age.race.old2 <- weights2018.age.race.old[c("caseid", "weightvec")]
weights2018.age.race.old2 <- as.data.frame(cbind(weights2018.age.race.old2$caseid,
                                                   weights2018.age.race.old2$weightvec))
weights2019.age.race.old2 <- weights2019.age.race.old[c("caseid", "weightvec")]
weights2019.age.race.old2 <- as.data.frame(cbind(weights2019.age.race.old2$caseid,
                                                   weights2019.age.race.old2$weightvec))
weights2020.age.race.old2 <- weights2020.age.race.old[c("caseid", "weightvec")]
weights2020.age.race.old2 <- as.data.frame(cbind(weights2020.age.race.old2$caseid,
                                                   weights2020.age.race.old2$weightvec))

weights.all.age.race.old <- rbind(weights2017.age.race.old2, weights2018.age.race.old2,
                                    weights2019.age.race.old2, weights2020.age.race.old2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.race.old) <- c("session_id", "weightvec.age")
weights.all.age.race.old$session_id <- as.character(weights.all.age.race.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.old <- timeseriesRace_NEW[timeseriesRace_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
race.old <- merge(timeseriesRace_NEW, weights.all.age.race.old, by = "session_id")


race.ygmidold <- rbind(race.young, race.middle, race.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.race <- targets_race[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]

timeseriesRace_NEW$relisimple <- recode(timeseriesRace_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
race.christ.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$relisimple == "christian",]
race.christ.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$relisimple == "christian",]
race.christ.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$relisimple == "christian",]
race.christ.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$relisimple == "christian",]
# repeat for other group
race.jew.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$relisimple == "jewish",] 
race.jew.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$relisimple == "jewish",]
race.jew.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$relisimple == "jewish",]
race.jew.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$relisimple == "jewish",]
# repeat for other group
race.nonrel.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$relisimple == "nonrel",] 
race.nonrel.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$relisimple == "nonrel",]
race.nonrel.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$relisimple == "nonrel",]
race.nonrel.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$relisimple == "nonrel",]
# repeat for other group
race.other.2017 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2017 & timeseriesRace_NEW$relisimple == "other",] 
race.other.2018 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2018 & timeseriesRace_NEW$relisimple == "other",]
race.other.2019 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2019 & timeseriesRace_NEW$relisimple == "other",]
race.other.2020 <- timeseriesRace_NEW[timeseriesRace_NEW$year==2020 & timeseriesRace_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.race.christ <- anesrake(targets.reli.race, race.christ.2017, caseid=race.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.race.christ <- anesrake(targets.reli.race, race.christ.2018, caseid=race.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.race.christ <- anesrake(targets.reli.race, race.christ.2019, caseid=race.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.race.christ <- anesrake(targets.reli.race, race.christ.2020, caseid=race.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.race.christ <- as.data.frame(weights2017.reli.race.christ[c("caseid", "weightvec")])
weights2018.reli.race.christ <- as.data.frame(weights2018.reli.race.christ[c("caseid", "weightvec")])
weights2019.reli.race.christ <- as.data.frame(weights2019.reli.race.christ[c("caseid", "weightvec")])
weights2020.reli.race.christ <- as.data.frame(weights2020.reli.race.christ[c("caseid", "weightvec")])

weights.all.reli.race.christ <- rbind(weights2017.reli.race.christ, weights2018.reli.race.christ, weights2019.reli.race.christ, weights2020.reli.race.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.race.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.race.christ$session_id <- as.character(weights.all.reli.race.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.christ <- timeseriesRace_NEW[timeseriesRace_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
race.christ <- merge(timeseriesRace_NEW, weights.all.reli.race.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.race.jew <- anesrake(targets.reli.race, race.jew.2017, caseid=race.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.race.jew <- anesrake(targets.reli.race, race.jew.2018, caseid=race.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.race.jew <- anesrake(targets.reli.race, race.jew.2019, caseid=race.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.race.jew <- anesrake(targets.reli.race, race.jew.2020, caseid=race.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.race.jew <- as.data.frame(weights2017.reli.race.jew[c("caseid", "weightvec")])
weights2018.reli.race.jew <- as.data.frame(weights2018.reli.race.jew[c("caseid", "weightvec")])
weights2019.reli.race.jew <- as.data.frame(weights2019.reli.race.jew[c("caseid", "weightvec")])
weights2020.reli.race.jew <- as.data.frame(weights2020.reli.race.jew[c("caseid", "weightvec")])

weights.all.reli.race.jew <- rbind(weights2017.reli.race.jew, weights2018.reli.race.jew, weights2019.reli.race.jew, weights2020.reli.race.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.race.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.race.jew$session_id <- as.character(weights.all.reli.race.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.jew <- timeseriesRace_NEW[timeseriesRace_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
race.jew <- merge(timeseriesRace_NEW, weights.all.reli.race.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.race.nonrel <- anesrake(targets.reli.race, race.nonrel.2017, caseid=race.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.race.nonrel <- anesrake(targets.reli.race, race.nonrel.2018, caseid=race.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.race.nonrel <- anesrake(targets.reli.race, race.nonrel.2019, caseid=race.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.race.nonrel <- anesrake(targets.reli.race, race.nonrel.2020, caseid=race.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.race.nonrel <- as.data.frame(weights2017.reli.race.nonrel[c("caseid", "weightvec")])
weights2018.reli.race.nonrel <- as.data.frame(weights2018.reli.race.nonrel[c("caseid", "weightvec")])
weights2019.reli.race.nonrel <- as.data.frame(weights2019.reli.race.nonrel[c("caseid", "weightvec")])
weights2020.reli.race.nonrel <- as.data.frame(weights2020.reli.race.nonrel[c("caseid", "weightvec")])

weights.all.reli.race.nonrel <- rbind(weights2017.reli.race.nonrel, weights2018.reli.race.nonrel, weights2019.reli.race.nonrel, weights2020.reli.race.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.race.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.race.nonrel$session_id <- as.character(weights.all.reli.race.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.nonrel <- timeseriesRace_NEW[timeseriesRace_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
race.nonrel <- merge(timeseriesRace_NEW, weights.all.reli.race.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.race.other <- anesrake(targets.reli.race, race.other.2017, caseid=race.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.race.other <- anesrake(targets.reli.race, race.other.2018, caseid=race.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.race.other <- anesrake(targets.reli.race, race.other.2019, caseid=race.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.race.other <- anesrake(targets.reli.race, race.other.2020, caseid=race.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.race.other <- as.data.frame(weights2017.reli.race.other[c("caseid", "weightvec")])
weights2018.reli.race.other <- as.data.frame(weights2018.reli.race.other[c("caseid", "weightvec")])
weights2019.reli.race.other <- as.data.frame(weights2019.reli.race.other[c("caseid", "weightvec")])
weights2020.reli.race.other <- as.data.frame(weights2020.reli.race.other[c("caseid", "weightvec")])

weights.all.reli.race.other <- rbind(weights2017.reli.race.other, weights2018.reli.race.other, weights2019.reli.race.other, weights2020.reli.race.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.race.other) <- c("session_id", "weightvec.religion")
weights.all.reli.race.other$session_id <- as.character(weights.all.reli.race.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
race.other <- timeseriesRace_NEW[timeseriesRace_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
race.other <- merge(timeseriesRace_NEW, weights.all.reli.race.other, by = "session_id")

race.allreligion <- rbind(race.christ, race.jew, race.nonrel, race.other)
length(which(colnames(race.christ) %in% colnames(race.other)))


## SKIN-TONE ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.skin <- targets_skin[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.skin

## Get sub-data for each year and each political identity, to then equate to full population
skin.lib.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$Wpol>4,]
skin.lib.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$Wpol>4,]
skin.lib.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$Wpol>4,]
skin.lib.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$Wpol>4,]
# repeat for conservatives
skin.cons.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$Wpol<4,] 
skin.cons.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$Wpol<4,]
skin.cons.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$Wpol<4,]
skin.cons.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.skin.lib <- anesrake(targets.pol.skin, skin.lib.2017, caseid=skin.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.skin.lib <- anesrake(targets.pol.skin, skin.lib.2018, caseid=skin.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.skin.lib <- anesrake(targets.pol.skin, skin.lib.2019, caseid=skin.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.skin.lib <- anesrake(targets.pol.skin, skin.lib.2020, caseid=skin.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.skin.lib <- as.data.frame(weights2017.pol.skin.lib[c("caseid", "weightvec")])
weights2018.pol.skin.lib <- as.data.frame(weights2018.pol.skin.lib[c("caseid", "weightvec")])
weights2019.pol.skin.lib <- as.data.frame(weights2019.pol.skin.lib[c("caseid", "weightvec")])
weights2020.pol.skin.lib <- as.data.frame(weights2020.pol.skin.lib[c("caseid", "weightvec")])

weights.all.pol.skin.lib <- rbind(weights2017.pol.skin.lib, weights2018.pol.skin.lib, weights2019.pol.skin.lib, weights2020.pol.skin.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.skin.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.skin.lib$session_id <- as.character(weights.all.pol.skin.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.liberals <- timeseriesSkin_NEW[timeseriesSkin_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
skin.liberals <- merge(timeseriesSkin_NEW, weights.all.pol.skin.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.skin.cons <- anesrake(targets.pol.skin, skin.cons.2017, caseid=skin.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.skin.cons <- anesrake(targets.pol.skin, skin.cons.2018, caseid=skin.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.skin.cons <- anesrake(targets.pol.skin, skin.cons.2019, caseid=skin.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.skin.cons <- anesrake(targets.pol.skin, skin.cons.2020, caseid=skin.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.skin.cons <- as.data.frame(weights2017.pol.skin.cons[c("caseid", "weightvec")])
weights2018.pol.skin.cons <- as.data.frame(weights2018.pol.skin.cons[c("caseid", "weightvec")])
weights2019.pol.skin.cons <- as.data.frame(weights2019.pol.skin.cons[c("caseid", "weightvec")])
weights2020.pol.skin.cons <- as.data.frame(weights2020.pol.skin.cons[c("caseid", "weightvec")])

weights.all.pol.skin.cons <- rbind(weights2017.pol.skin.cons, weights2018.pol.skin.cons, weights2019.pol.skin.cons, weights2020.pol.skin.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.skin.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.skin.cons$session_id <- as.character(weights.all.pol.skin.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.conserv <- timeseriesSkin_NEW[timeseriesSkin_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
skin.conserv <- merge(timeseriesSkin_NEW, weights.all.pol.skin.cons, by = "session_id")

skin.libcons <- rbind(skin.liberals, skin.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.skin <- targets_skin[c("Wpol", "Wrace", "Wedu", "Wage")]

## Get sub-data for each year and each group, to then equate to full population
skin.fem.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$genderidentity_simp == "female",]
skin.fem.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$genderidentity_simp == "female",]
skin.fem.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$genderidentity_simp == "female",]
skin.fem.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$genderidentity_simp == "female",]
# repeat for other group
skin.mal.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$genderidentity_simp == "male",] 
skin.mal.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$genderidentity_simp == "male",]
skin.mal.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$genderidentity_simp == "male",]
skin.mal.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.skin.fem <- anesrake(targets.gender.skin, skin.fem.2017, caseid=skin.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.skin.fem <- anesrake(targets.gender.skin, skin.fem.2018, caseid=skin.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.skin.fem <- anesrake(targets.gender.skin, skin.fem.2019, caseid=skin.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.skin.fem <- anesrake(targets.gender.skin, skin.fem.2020, caseid=skin.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.skin.fem <- as.data.frame(weights2017.gender.skin.fem[c("caseid", "weightvec")])
weights2018.gender.skin.fem <- as.data.frame(weights2018.gender.skin.fem[c("caseid", "weightvec")])
weights2019.gender.skin.fem <- as.data.frame(weights2019.gender.skin.fem[c("caseid", "weightvec")])
weights2020.gender.skin.fem <- as.data.frame(weights2020.gender.skin.fem[c("caseid", "weightvec")])

weights.all.gender.skin.fem <- rbind(weights2017.gender.skin.fem, weights2018.gender.skin.fem, weights2019.gender.skin.fem, weights2020.gender.skin.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.skin.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.skin.fem$session_id <- as.character(weights.all.gender.skin.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.fem <- timeseriesSkin_NEW[timeseriesSkin_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
skin.fem <- merge(timeseriesSkin_NEW, weights.all.gender.skin.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.skin.mal <- anesrake(targets.gender.skin, skin.mal.2017, caseid=skin.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.skin.mal <- anesrake(targets.gender.skin, skin.mal.2018, caseid=skin.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.skin.mal <- anesrake(targets.gender.skin, skin.mal.2019, caseid=skin.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.skin.mal <- anesrake(targets.gender.skin, skin.mal.2020, caseid=skin.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.skin.mal <- as.data.frame(weights2017.gender.skin.mal[c("caseid", "weightvec")])
weights2018.gender.skin.mal <- as.data.frame(weights2018.gender.skin.mal[c("caseid", "weightvec")])
weights2019.gender.skin.mal <- as.data.frame(weights2019.gender.skin.mal[c("caseid", "weightvec")])
weights2020.gender.skin.mal <- as.data.frame(weights2020.gender.skin.mal[c("caseid", "weightvec")])

weights.all.gender.skin.mal <- rbind(weights2017.gender.skin.mal, weights2018.gender.skin.mal, weights2019.gender.skin.mal, weights2020.gender.skin.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.skin.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.skin.mal$session_id <- as.character(weights.all.gender.skin.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.mal <- timeseriesSkin_NEW[timeseriesSkin_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
skin.mal <- merge(timeseriesSkin_NEW, weights.all.gender.skin.mal, by = "session_id")

skin.femmal <- rbind(skin.fem, skin.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.skin <- targets_skin[c("Wpol", "Wrace", "Wgender", "Wage")]

timeseriesSkin_NEW$edu.grp <- as.numeric(timeseriesSkin_NEW$edu.new) - 5
timeseriesSkin_NEW$edu.grp <- ifelse(timeseriesSkin_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
skin.ncol.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$edu.grp == "nocollege",]
skin.ncol.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$edu.grp == "nocollege",]
skin.ncol.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$edu.grp == "nocollege",]
skin.ncol.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$edu.grp == "nocollege",]
# repeat for other group
skin.col.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$edu.grp == "college",] 
skin.col.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$edu.grp == "college",]
skin.col.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$edu.grp == "college",]
skin.col.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.skin.ncol <- anesrake(targets.edu.skin, skin.ncol.2017, caseid=skin.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.skin.ncol <- anesrake(targets.edu.skin, skin.ncol.2018, caseid=skin.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.skin.ncol <- anesrake(targets.edu.skin, skin.ncol.2019, caseid=skin.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.skin.ncol <- anesrake(targets.edu.skin, skin.ncol.2020, caseid=skin.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.skin.ncol <- as.data.frame(weights2017.edu.skin.ncol[c("caseid", "weightvec")])
weights2018.edu.skin.ncol <- as.data.frame(weights2018.edu.skin.ncol[c("caseid", "weightvec")])
weights2019.edu.skin.ncol <- as.data.frame(weights2019.edu.skin.ncol[c("caseid", "weightvec")])
weights2020.edu.skin.ncol <- as.data.frame(weights2020.edu.skin.ncol[c("caseid", "weightvec")])

weights.all.edu.skin.ncol <- rbind(weights2017.edu.skin.ncol, weights2018.edu.skin.ncol, weights2019.edu.skin.ncol, weights2020.edu.skin.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.skin.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.skin.ncol$session_id <- as.character(weights.all.edu.skin.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.ncol <- timeseriesSkin_NEW[timeseriesSkin_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
skin.ncol <- merge(timeseriesSkin_NEW, weights.all.edu.skin.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.skin.col <- anesrake(targets.edu.skin, skin.col.2017, caseid=skin.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.skin.col <- anesrake(targets.edu.skin, skin.col.2018, caseid=skin.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.skin.col <- anesrake(targets.edu.skin, skin.col.2019, caseid=skin.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.skin.col <- anesrake(targets.edu.skin, skin.col.2020, caseid=skin.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.skin.col <- as.data.frame(weights2017.edu.skin.col[c("caseid", "weightvec")])
weights2018.edu.skin.col <- as.data.frame(weights2018.edu.skin.col[c("caseid", "weightvec")])
weights2019.edu.skin.col <- as.data.frame(weights2019.edu.skin.col[c("caseid", "weightvec")])
weights2020.edu.skin.col <- as.data.frame(weights2020.edu.skin.col[c("caseid", "weightvec")])

weights.all.edu.skin.col <- rbind(weights2017.edu.skin.col, weights2018.edu.skin.col, weights2019.edu.skin.col, weights2020.edu.skin.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.skin.col) <- c("session_id", "weightvec.edu")
weights.all.edu.skin.col$session_id <- as.character(weights.all.edu.skin.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.col <- timeseriesSkin_NEW[timeseriesSkin_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
skin.col <- merge(timeseriesSkin_NEW, weights.all.edu.skin.col, by = "session_id")

skin.ncolcol <- rbind(skin.ncol, skin.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.skin <- targets_skin[c("Wpol", "Wgender", "Wedu", "Wage")]

timeseriesSkin_NEW$racesimple <- recode(timeseriesSkin_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
skin.white.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$racesimple == "White",]
skin.white.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$racesimple == "White",]
skin.white.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$racesimple == "White",]
skin.white.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$racesimple == "White",]
# repeat for other group
skin.black.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$racesimple == "Black",] 
skin.black.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$racesimple == "Black",]
skin.black.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$racesimple == "Black",]
skin.black.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$racesimple == "Black",]
# repeat for other group
skin.asian.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$racesimple == "Asian",] 
skin.asian.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$racesimple == "Asian",]
skin.asian.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$racesimple == "Asian",]
skin.asian.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.skin.white <- anesrake(targets.race.skin, skin.white.2017, caseid=skin.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.skin.white <- anesrake(targets.race.skin, skin.white.2018, caseid=skin.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.skin.white <- anesrake(targets.race.skin, skin.white.2019, caseid=skin.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.skin.white <- anesrake(targets.race.skin, skin.white.2020, caseid=skin.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.skin.white <- as.data.frame(weights2017.race.skin.white[c("caseid", "weightvec")])
weights2018.race.skin.white <- as.data.frame(weights2018.race.skin.white[c("caseid", "weightvec")])
weights2019.race.skin.white <- as.data.frame(weights2019.race.skin.white[c("caseid", "weightvec")])
weights2020.race.skin.white <- as.data.frame(weights2020.race.skin.white[c("caseid", "weightvec")])

weights.all.race.skin.white <- rbind(weights2017.race.skin.white, weights2018.race.skin.white, weights2019.race.skin.white, weights2020.race.skin.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.skin.white) <- c("session_id", "weightvec.race")
weights.all.race.skin.white$session_id <- as.character(weights.all.race.skin.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.white <- timeseriesSkin_NEW[timeseriesSkin_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
skin.white <- merge(timeseriesSkin_NEW, weights.all.race.skin.white, by = "session_id")

## create the weights per year for black
weights2017.race.skin.black <- anesrake(targets.race.skin, skin.black.2017, caseid=skin.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.skin.black <- anesrake(targets.race.skin, skin.black.2018, caseid=skin.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.skin.black <- anesrake(targets.race.skin, skin.black.2019, caseid=skin.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.skin.black <- anesrake(targets.race.skin, skin.black.2020, caseid=skin.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.skin.black <- as.data.frame(weights2017.race.skin.black[c("caseid", "weightvec")])
weights2018.race.skin.black <- as.data.frame(weights2018.race.skin.black[c("caseid", "weightvec")])
weights2019.race.skin.black <- as.data.frame(weights2019.race.skin.black[c("caseid", "weightvec")])
weights2020.race.skin.black <- as.data.frame(weights2020.race.skin.black[c("caseid", "weightvec")])

weights.all.race.skin.black <- rbind(weights2017.race.skin.black, weights2018.race.skin.black, weights2019.race.skin.black, weights2020.race.skin.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.skin.black) <- c("session_id", "weightvec.race")
weights.all.race.skin.black$session_id <- as.character(weights.all.race.skin.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.black <- timeseriesSkin_NEW[timeseriesSkin_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
skin.black <- merge(timeseriesSkin_NEW, weights.all.race.skin.black, by = "session_id")

## create the weights per year for asian
weights2017.race.skin.asian <- anesrake(targets.race.skin, skin.asian.2017, caseid=skin.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.skin.asian <- anesrake(targets.race.skin, skin.asian.2018, caseid=skin.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.skin.asian <- anesrake(targets.race.skin, skin.asian.2019, caseid=skin.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.skin.asian <- anesrake(targets.race.skin, skin.asian.2020, caseid=skin.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.skin.asian <- as.data.frame(weights2017.race.skin.asian[c("caseid", "weightvec")])
weights2018.race.skin.asian <- as.data.frame(weights2018.race.skin.asian[c("caseid", "weightvec")])
weights2019.race.skin.asian <- as.data.frame(weights2019.race.skin.asian[c("caseid", "weightvec")])
weights2020.race.skin.asian <- as.data.frame(weights2020.race.skin.asian[c("caseid", "weightvec")])

weights.all.race.skin.asian <- rbind(weights2017.race.skin.asian, weights2018.race.skin.asian, weights2019.race.skin.asian, weights2020.race.skin.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.skin.asian) <- c("session_id", "weightvec.race")
weights.all.race.skin.asian$session_id <- as.character(weights.all.race.skin.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.asian <- timeseriesSkin_NEW[timeseriesSkin_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
skin.asian <- merge(timeseriesSkin_NEW, weights.all.race.skin.asian, by = "session_id")

skin.whblkasi <- rbind(skin.white, skin.black, skin.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.skin <- targets_skin[c("Wpol", "Wrace", "Wedu", "Wgender")]

timeseriesSkin_NEW$age.grp <- cut(timeseriesSkin_NEW$age.new, c(0,20,40,99))
levels(timeseriesSkin_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
skin.young.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$age.grp == "young",]
skin.young.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$age.grp == "young",]
skin.young.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$age.grp == "young",]
skin.young.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$age.grp == "young",]
# repeat for other group
skin.middle.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$age.grp == "middle",] 
skin.middle.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$age.grp == "middle",]
skin.middle.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$age.grp == "middle",]
skin.middle.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$age.grp == "middle",]
# repeat for other group
skin.old.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$age.grp == "old",] 
skin.old.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$age.grp == "old",]
skin.old.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$age.grp == "old",]
skin.old.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.skin.young <- anesrake(targets.age.skin, skin.young.2017, caseid=skin.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.skin.young <- anesrake(targets.age.skin, skin.young.2018, caseid=skin.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.skin.young <- anesrake(targets.age.skin, skin.young.2019, caseid=skin.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.skin.young <- anesrake(targets.age.skin, skin.young.2020, caseid=skin.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for skin attitudes, add in workaround due to NULL rownames)
weights2017.age.skin.young2 <- weights2017.age.skin.young[c("caseid", "weightvec")]
weights2017.age.skin.young2 <- as.data.frame(cbind(weights2017.age.skin.young2$caseid,
                                                   weights2017.age.skin.young2$weightvec))
weights2018.age.skin.young2 <- weights2018.age.skin.young[c("caseid", "weightvec")]
weights2018.age.skin.young2 <- as.data.frame(cbind(weights2018.age.skin.young2$caseid,
                                                   weights2018.age.skin.young2$weightvec))
weights2019.age.skin.young2 <- weights2019.age.skin.young[c("caseid", "weightvec")]
weights2019.age.skin.young2 <- as.data.frame(cbind(weights2019.age.skin.young2$caseid,
                                                   weights2019.age.skin.young2$weightvec))
weights2020.age.skin.young2 <- weights2020.age.skin.young[c("caseid", "weightvec")]
weights2020.age.skin.young2 <- as.data.frame(cbind(weights2020.age.skin.young2$caseid,
                                                   weights2020.age.skin.young2$weightvec))

weights.all.age.skin.young <- rbind(weights2017.age.skin.young2, weights2018.age.skin.young2,
                                    weights2019.age.skin.young2, weights2020.age.skin.young2)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.skin.young) <- c("session_id", "weightvec.age")
weights.all.age.skin.young$session_id <- as.character(weights.all.age.skin.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.young <- timeseriesSkin_NEW[timeseriesSkin_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
skin.young <- merge(timeseriesSkin_NEW, weights.all.age.skin.young, by = "session_id")

## create the weights per year for middle
weights2017.age.skin.middle <- anesrake(targets.age.skin, skin.middle.2017, caseid=skin.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.skin.middle <- anesrake(targets.age.skin, skin.middle.2018, caseid=skin.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.skin.middle <- anesrake(targets.age.skin, skin.middle.2019, caseid=skin.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.skin.middle <- anesrake(targets.age.skin, skin.middle.2020, caseid=skin.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for skin attitudes, add in workaround due to NULL rownames)
weights2017.age.skin.middle2 <- weights2017.age.skin.middle[c("caseid", "weightvec")]
weights2017.age.skin.middle2 <- as.data.frame(cbind(weights2017.age.skin.middle2$caseid,
                                                   weights2017.age.skin.middle2$weightvec))
weights2018.age.skin.middle2 <- weights2018.age.skin.middle[c("caseid", "weightvec")]
weights2018.age.skin.middle2 <- as.data.frame(cbind(weights2018.age.skin.middle2$caseid,
                                                   weights2018.age.skin.middle2$weightvec))
weights2019.age.skin.middle2 <- weights2019.age.skin.middle[c("caseid", "weightvec")]
weights2019.age.skin.middle2 <- as.data.frame(cbind(weights2019.age.skin.middle2$caseid,
                                                   weights2019.age.skin.middle2$weightvec))
weights2020.age.skin.middle2 <- weights2020.age.skin.middle[c("caseid", "weightvec")]
weights2020.age.skin.middle2 <- as.data.frame(cbind(weights2020.age.skin.middle2$caseid,
                                                   weights2020.age.skin.middle2$weightvec))

weights.all.age.skin.middle <- rbind(weights2017.age.skin.middle2, weights2018.age.skin.middle2,
                                    weights2019.age.skin.middle2, weights2020.age.skin.middle2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.skin.middle) <- c("session_id", "weightvec.age")
weights.all.age.skin.middle$session_id <- as.character(weights.all.age.skin.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.middle <- timeseriesSkin_NEW[timeseriesSkin_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
skin.middle <- merge(timeseriesSkin_NEW, weights.all.age.skin.middle, by = "session_id")

## create the weights per year for old
weights2017.age.skin.old <- anesrake(targets.age.skin, skin.old.2017, caseid=skin.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.skin.old <- anesrake(targets.age.skin, skin.old.2018, caseid=skin.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.skin.old <- anesrake(targets.age.skin, skin.old.2019, caseid=skin.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.skin.old <- anesrake(targets.age.skin, skin.old.2020, caseid=skin.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for race attitudes, add in workaround due to NULL rownames)
weights2017.age.skin.old2 <- weights2017.age.skin.old[c("caseid", "weightvec")]
weights2017.age.skin.old2 <- as.data.frame(cbind(weights2017.age.skin.old2$caseid,
                                                   weights2017.age.skin.old2$weightvec))
weights2018.age.skin.old2 <- weights2018.age.skin.old[c("caseid", "weightvec")]
weights2018.age.skin.old2 <- as.data.frame(cbind(weights2018.age.skin.old2$caseid,
                                                   weights2018.age.skin.old2$weightvec))
weights2019.age.skin.old2 <- weights2019.age.skin.old[c("caseid", "weightvec")]
weights2019.age.skin.old2 <- as.data.frame(cbind(weights2019.age.skin.old2$caseid,
                                                   weights2019.age.skin.old2$weightvec))
weights2020.age.skin.old2 <- weights2020.age.skin.old[c("caseid", "weightvec")]
weights2020.age.skin.old2 <- as.data.frame(cbind(weights2020.age.skin.old2$caseid,
                                                   weights2020.age.skin.old2$weightvec))

weights.all.age.skin.old <- rbind(weights2017.age.skin.old2, weights2018.age.skin.old2,
                                    weights2019.age.skin.old2, weights2020.age.skin.old2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.skin.old) <- c("session_id", "weightvec.age")
weights.all.age.skin.old$session_id <- as.character(weights.all.age.skin.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.old <- timeseriesSkin_NEW[timeseriesSkin_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
skin.old <- merge(timeseriesSkin_NEW, weights.all.age.skin.old, by = "session_id")


skin.ygmidold <- rbind(skin.young, skin.middle, skin.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.skin <- targets_skin[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]

timeseriesSkin_NEW$relisimple <- recode(timeseriesSkin_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
skin.christ.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$relisimple == "christian",]
skin.christ.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$relisimple == "christian",]
skin.christ.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$relisimple == "christian",]
skin.christ.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$relisimple == "christian",]
# repeat for other group
skin.jew.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$relisimple == "jewish",] 
skin.jew.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$relisimple == "jewish",]
skin.jew.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$relisimple == "jewish",]
skin.jew.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$relisimple == "jewish",]
# repeat for other group
skin.nonrel.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$relisimple == "nonrel",] 
skin.nonrel.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$relisimple == "nonrel",]
skin.nonrel.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$relisimple == "nonrel",]
skin.nonrel.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$relisimple == "nonrel",]
# repeat for other group
skin.other.2017 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2017 & timeseriesSkin_NEW$relisimple == "other",] 
skin.other.2018 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2018 & timeseriesSkin_NEW$relisimple == "other",]
skin.other.2019 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2019 & timeseriesSkin_NEW$relisimple == "other",]
skin.other.2020 <- timeseriesSkin_NEW[timeseriesSkin_NEW$year==2020 & timeseriesSkin_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.skin.christ <- anesrake(targets.reli.skin, skin.christ.2017, caseid=skin.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.skin.christ <- anesrake(targets.reli.skin, skin.christ.2018, caseid=skin.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.skin.christ <- anesrake(targets.reli.skin, skin.christ.2019, caseid=skin.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.skin.christ <- anesrake(targets.reli.skin, skin.christ.2020, caseid=skin.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.skin.christ <- as.data.frame(weights2017.reli.skin.christ[c("caseid", "weightvec")])
weights2018.reli.skin.christ <- as.data.frame(weights2018.reli.skin.christ[c("caseid", "weightvec")])
weights2019.reli.skin.christ <- as.data.frame(weights2019.reli.skin.christ[c("caseid", "weightvec")])
weights2020.reli.skin.christ <- as.data.frame(weights2020.reli.skin.christ[c("caseid", "weightvec")])

weights.all.reli.skin.christ <- rbind(weights2017.reli.skin.christ, weights2018.reli.skin.christ, weights2019.reli.skin.christ, weights2020.reli.skin.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.skin.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.skin.christ$session_id <- as.character(weights.all.reli.skin.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.christ <- timeseriesSkin_NEW[timeseriesSkin_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
skin.christ <- merge(timeseriesSkin_NEW, weights.all.reli.skin.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.skin.jew <- anesrake(targets.reli.skin, skin.jew.2017, caseid=skin.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.skin.jew <- anesrake(targets.reli.skin, skin.jew.2018, caseid=skin.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.skin.jew <- anesrake(targets.reli.skin, skin.jew.2019, caseid=skin.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.skin.jew <- anesrake(targets.reli.skin, skin.jew.2020, caseid=skin.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.skin.jew <- as.data.frame(weights2017.reli.skin.jew[c("caseid", "weightvec")])
weights2018.reli.skin.jew <- as.data.frame(weights2018.reli.skin.jew[c("caseid", "weightvec")])
weights2019.reli.skin.jew <- as.data.frame(weights2019.reli.skin.jew[c("caseid", "weightvec")])
weights2020.reli.skin.jew <- as.data.frame(weights2020.reli.skin.jew[c("caseid", "weightvec")])

weights.all.reli.skin.jew <- rbind(weights2017.reli.skin.jew, weights2018.reli.skin.jew, weights2019.reli.skin.jew, weights2020.reli.skin.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.skin.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.skin.jew$session_id <- as.character(weights.all.reli.skin.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.jew <- timeseriesSkin_NEW[timeseriesSkin_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
skin.jew <- merge(timeseriesSkin_NEW, weights.all.reli.skin.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.skin.nonrel <- anesrake(targets.reli.skin, skin.nonrel.2017, caseid=skin.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.skin.nonrel <- anesrake(targets.reli.skin, skin.nonrel.2018, caseid=skin.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.skin.nonrel <- anesrake(targets.reli.skin, skin.nonrel.2019, caseid=skin.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.skin.nonrel <- anesrake(targets.reli.skin, skin.nonrel.2020, caseid=skin.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.skin.nonrel <- as.data.frame(weights2017.reli.skin.nonrel[c("caseid", "weightvec")])
weights2018.reli.skin.nonrel <- as.data.frame(weights2018.reli.skin.nonrel[c("caseid", "weightvec")])
weights2019.reli.skin.nonrel <- as.data.frame(weights2019.reli.skin.nonrel[c("caseid", "weightvec")])
weights2020.reli.skin.nonrel <- as.data.frame(weights2020.reli.skin.nonrel[c("caseid", "weightvec")])

weights.all.reli.skin.nonrel <- rbind(weights2017.reli.skin.nonrel, weights2018.reli.skin.nonrel, weights2019.reli.skin.nonrel, weights2020.reli.skin.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.skin.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.skin.nonrel$session_id <- as.character(weights.all.reli.skin.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.nonrel <- timeseriesSkin_NEW[timeseriesSkin_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
skin.nonrel <- merge(timeseriesSkin_NEW, weights.all.reli.skin.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.skin.other <- anesrake(targets.reli.skin, skin.other.2017, caseid=skin.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.skin.other <- anesrake(targets.reli.skin, skin.other.2018, caseid=skin.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.skin.other <- anesrake(targets.reli.skin, skin.other.2019, caseid=skin.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.skin.other <- anesrake(targets.reli.skin, skin.other.2020, caseid=skin.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.skin.other <- as.data.frame(weights2017.reli.skin.other[c("caseid", "weightvec")])
weights2018.reli.skin.other <- as.data.frame(weights2018.reli.skin.other[c("caseid", "weightvec")])
weights2019.reli.skin.other <- as.data.frame(weights2019.reli.skin.other[c("caseid", "weightvec")])
weights2020.reli.skin.other <- as.data.frame(weights2020.reli.skin.other[c("caseid", "weightvec")])

weights.all.reli.skin.other <- rbind(weights2017.reli.skin.other, weights2018.reli.skin.other, weights2019.reli.skin.other, weights2020.reli.skin.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.skin.other) <- c("session_id", "weightvec.religion")
weights.all.reli.skin.other$session_id <- as.character(weights.all.reli.skin.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
skin.other <- timeseriesSkin_NEW[timeseriesSkin_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
skin.other <- merge(timeseriesSkin_NEW, weights.all.reli.skin.other, by = "session_id")

skin.allreligion <- rbind(skin.christ, skin.jew, skin.nonrel, skin.other)


## SEXUALITY ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.sex <- targets_sexuality[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.sex

## Get sub-data for each year and each political identity, to then equate to full population
sex.lib.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$Wpol>4,]
sex.lib.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$Wpol>4,]
sex.lib.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$Wpol>4,]
sex.lib.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$Wpol>4,]
# repeat for conservatives
sex.cons.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$Wpol<4,] 
sex.cons.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$Wpol<4,]
sex.cons.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$Wpol<4,]
sex.cons.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.sex.lib <- anesrake(targets.pol.sex, sex.lib.2017, caseid=sex.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.sex.lib <- anesrake(targets.pol.sex, sex.lib.2018, caseid=sex.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.sex.lib <- anesrake(targets.pol.sex, sex.lib.2019, caseid=sex.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.sex.lib <- anesrake(targets.pol.sex, sex.lib.2020, caseid=sex.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.sex.lib <- as.data.frame(weights2017.pol.sex.lib[c("caseid", "weightvec")])
weights2018.pol.sex.lib <- as.data.frame(weights2018.pol.sex.lib[c("caseid", "weightvec")])
weights2019.pol.sex.lib <- as.data.frame(weights2019.pol.sex.lib[c("caseid", "weightvec")])
weights2020.pol.sex.lib <- as.data.frame(weights2020.pol.sex.lib[c("caseid", "weightvec")])

weights.all.pol.sex.lib <- rbind(weights2017.pol.sex.lib, weights2018.pol.sex.lib, weights2019.pol.sex.lib, weights2020.pol.sex.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.sex.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.sex.lib$session_id <- as.character(weights.all.pol.sex.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.liberals <- timeseriesSex_NEW[timeseriesSex_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
sex.liberals <- merge(timeseriesSex_NEW, weights.all.pol.sex.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.sex.cons <- anesrake(targets.pol.sex, sex.cons.2017, caseid=sex.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.sex.cons <- anesrake(targets.pol.sex, sex.cons.2018, caseid=sex.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.sex.cons <- anesrake(targets.pol.sex, sex.cons.2019, caseid=sex.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.sex.cons <- anesrake(targets.pol.sex, sex.cons.2020, caseid=sex.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.sex.cons <- as.data.frame(weights2017.pol.sex.cons[c("caseid", "weightvec")])
weights2018.pol.sex.cons <- as.data.frame(weights2018.pol.sex.cons[c("caseid", "weightvec")])
weights2019.pol.sex.cons <- as.data.frame(weights2019.pol.sex.cons[c("caseid", "weightvec")])
weights2020.pol.sex.cons <- as.data.frame(weights2020.pol.sex.cons[c("caseid", "weightvec")])

weights.all.pol.sex.cons <- rbind(weights2017.pol.sex.cons, weights2018.pol.sex.cons, weights2019.pol.sex.cons, weights2020.pol.sex.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.sex.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.sex.cons$session_id <- as.character(weights.all.pol.sex.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.conserv <- timeseriesSex_NEW[timeseriesSex_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
sex.conserv <- merge(timeseriesSex_NEW, weights.all.pol.sex.cons, by = "session_id")

sex.libcons <- rbind(sex.liberals, sex.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.sex <- targets_sexuality[c("Wpol", "Wrace", "Wedu", "Wage")]
targets.gender.sex

## Get sub-data for each year and each group, to then equate to full population
sex.fem.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$genderidentity_simp == "female",]
sex.fem.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$genderidentity_simp == "female",]
sex.fem.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$genderidentity_simp == "female",]
sex.fem.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$genderidentity_simp == "female",]
# repeat for other group
sex.mal.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$genderidentity_simp == "male",] 
sex.mal.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$genderidentity_simp == "male",]
sex.mal.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$genderidentity_simp == "male",]
sex.mal.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.sex.fem <- anesrake(targets.gender.sex, sex.fem.2017, caseid=sex.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.sex.fem <- anesrake(targets.gender.sex, sex.fem.2018, caseid=sex.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.sex.fem <- anesrake(targets.gender.sex, sex.fem.2019, caseid=sex.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.sex.fem <- anesrake(targets.gender.sex, sex.fem.2020, caseid=sex.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.sex.fem <- as.data.frame(weights2017.gender.sex.fem[c("caseid", "weightvec")])
weights2018.gender.sex.fem <- as.data.frame(weights2018.gender.sex.fem[c("caseid", "weightvec")])
weights2019.gender.sex.fem <- as.data.frame(weights2019.gender.sex.fem[c("caseid", "weightvec")])
weights2020.gender.sex.fem <- as.data.frame(weights2020.gender.sex.fem[c("caseid", "weightvec")])

weights.all.gender.sex.fem <- rbind(weights2017.gender.sex.fem, weights2018.gender.sex.fem, weights2019.gender.sex.fem, weights2020.gender.sex.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.sex.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.sex.fem$session_id <- as.character(weights.all.gender.sex.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.fem <- timeseriesSex_NEW[timeseriesSex_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.fem <- merge(timeseriesSex_NEW, weights.all.gender.sex.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.sex.mal <- anesrake(targets.gender.sex, sex.mal.2017, caseid=sex.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.sex.mal <- anesrake(targets.gender.sex, sex.mal.2018, caseid=sex.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.sex.mal <- anesrake(targets.gender.sex, sex.mal.2019, caseid=sex.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.sex.mal <- anesrake(targets.gender.sex, sex.mal.2020, caseid=sex.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.sex.mal <- as.data.frame(weights2017.gender.sex.mal[c("caseid", "weightvec")])
weights2018.gender.sex.mal <- as.data.frame(weights2018.gender.sex.mal[c("caseid", "weightvec")])
weights2019.gender.sex.mal <- as.data.frame(weights2019.gender.sex.mal[c("caseid", "weightvec")])
weights2020.gender.sex.mal <- as.data.frame(weights2020.gender.sex.mal[c("caseid", "weightvec")])

weights.all.gender.sex.mal <- rbind(weights2017.gender.sex.mal, weights2018.gender.sex.mal, weights2019.gender.sex.mal, weights2020.gender.sex.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.sex.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.sex.mal$session_id <- as.character(weights.all.gender.sex.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.mal <- timeseriesSex_NEW[timeseriesSex_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.mal <- merge(timeseriesSex_NEW, weights.all.gender.sex.mal, by = "session_id")

sex.femmal <- rbind(sex.fem, sex.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.sex <- targets_sexuality[c("Wpol", "Wrace", "Wgender", "Wage")]
targets.edu.sex

timeseriesSex_NEW$edu.grp <- as.numeric(timeseriesSex_NEW$edu.new) - 5
timeseriesSex_NEW$edu.grp <- ifelse(timeseriesSex_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
sex.ncol.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$edu.grp == "nocollege",]
sex.ncol.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$edu.grp == "nocollege",]
sex.ncol.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$edu.grp == "nocollege",]
sex.ncol.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$edu.grp == "nocollege",]
# repeat for other group
sex.col.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$edu.grp == "college",] 
sex.col.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$edu.grp == "college",]
sex.col.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$edu.grp == "college",]
sex.col.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.sex.ncol <- anesrake(targets.edu.sex, sex.ncol.2017, caseid=sex.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.sex.ncol <- anesrake(targets.edu.sex, sex.ncol.2018, caseid=sex.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.sex.ncol <- anesrake(targets.edu.sex, sex.ncol.2019, caseid=sex.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.sex.ncol <- anesrake(targets.edu.sex, sex.ncol.2020, caseid=sex.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.sex.ncol <- as.data.frame(weights2017.edu.sex.ncol[c("caseid", "weightvec")])
weights2018.edu.sex.ncol <- as.data.frame(weights2018.edu.sex.ncol[c("caseid", "weightvec")])
weights2019.edu.sex.ncol <- as.data.frame(weights2019.edu.sex.ncol[c("caseid", "weightvec")])
weights2020.edu.sex.ncol <- as.data.frame(weights2020.edu.sex.ncol[c("caseid", "weightvec")])

weights.all.edu.sex.ncol <- rbind(weights2017.edu.sex.ncol, weights2018.edu.sex.ncol, weights2019.edu.sex.ncol, weights2020.edu.sex.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.sex.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.sex.ncol$session_id <- as.character(weights.all.edu.sex.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.ncol <- timeseriesSex_NEW[timeseriesSex_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
sex.ncol <- merge(timeseriesSex_NEW, weights.all.edu.sex.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.sex.col <- anesrake(targets.edu.sex, sex.col.2017, caseid=sex.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.sex.col <- anesrake(targets.edu.sex, sex.col.2018, caseid=sex.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.sex.col <- anesrake(targets.edu.sex, sex.col.2019, caseid=sex.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.sex.col <- anesrake(targets.edu.sex, sex.col.2020, caseid=sex.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.sex.col <- as.data.frame(weights2017.edu.sex.col[c("caseid", "weightvec")])
weights2018.edu.sex.col <- as.data.frame(weights2018.edu.sex.col[c("caseid", "weightvec")])
weights2019.edu.sex.col <- as.data.frame(weights2019.edu.sex.col[c("caseid", "weightvec")])
weights2020.edu.sex.col <- as.data.frame(weights2020.edu.sex.col[c("caseid", "weightvec")])

weights.all.edu.sex.col <- rbind(weights2017.edu.sex.col, weights2018.edu.sex.col, weights2019.edu.sex.col, weights2020.edu.sex.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.sex.col) <- c("session_id", "weightvec.edu")
weights.all.edu.sex.col$session_id <- as.character(weights.all.edu.sex.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.col <- timeseriesSex_NEW[timeseriesSex_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.col <- merge(timeseriesSex_NEW, weights.all.edu.sex.col, by = "session_id")

sex.ncolcol <- rbind(sex.ncol, sex.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.sex <- targets_sexuality[c("Wpol", "Wgender", "Wedu", "Wage")]
targets.race.sex

timeseriesSex_NEW$racesimple <- recode(timeseriesSex_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
sex.white.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$racesimple == "White",]
sex.white.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$racesimple == "White",]
sex.white.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$racesimple == "White",]
sex.white.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$racesimple == "White",]
# repeat for other group
sex.black.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$racesimple == "Black",] 
sex.black.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$racesimple == "Black",]
sex.black.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$racesimple == "Black",]
sex.black.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$racesimple == "Black",]
# repeat for other group
sex.asian.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$racesimple == "Asian",] 
sex.asian.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$racesimple == "Asian",]
sex.asian.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$racesimple == "Asian",]
sex.asian.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.sex.white <- anesrake(targets.race.sex, sex.white.2017, caseid=sex.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.sex.white <- anesrake(targets.race.sex, sex.white.2018, caseid=sex.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.sex.white <- anesrake(targets.race.sex, sex.white.2019, caseid=sex.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.sex.white <- anesrake(targets.race.sex, sex.white.2020, caseid=sex.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.sex.white <- as.data.frame(weights2017.race.sex.white[c("caseid", "weightvec")])
weights2018.race.sex.white <- as.data.frame(weights2018.race.sex.white[c("caseid", "weightvec")])
weights2019.race.sex.white <- as.data.frame(weights2019.race.sex.white[c("caseid", "weightvec")])
weights2020.race.sex.white <- as.data.frame(weights2020.race.sex.white[c("caseid", "weightvec")])

weights.all.race.sex.white <- rbind(weights2017.race.sex.white, weights2018.race.sex.white, weights2019.race.sex.white, weights2020.race.sex.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.sex.white) <- c("session_id", "weightvec.race")
weights.all.race.sex.white$session_id <- as.character(weights.all.race.sex.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.white <- timeseriesSex_NEW[timeseriesSex_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
sex.white <- merge(timeseriesSex_NEW, weights.all.race.sex.white, by = "session_id")

## create the weights per year for black
weights2017.race.sex.black <- anesrake(targets.race.sex, sex.black.2017, caseid=sex.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.sex.black <- anesrake(targets.race.sex, sex.black.2018, caseid=sex.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.sex.black <- anesrake(targets.race.sex, sex.black.2019, caseid=sex.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.sex.black <- anesrake(targets.race.sex, sex.black.2020, caseid=sex.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.sex.black <- as.data.frame(weights2017.race.sex.black[c("caseid", "weightvec")])
weights2018.race.sex.black <- as.data.frame(weights2018.race.sex.black[c("caseid", "weightvec")])
weights2019.race.sex.black <- as.data.frame(weights2019.race.sex.black[c("caseid", "weightvec")])
weights2020.race.sex.black <- as.data.frame(weights2020.race.sex.black[c("caseid", "weightvec")])

weights.all.race.sex.black <- rbind(weights2017.race.sex.black, weights2018.race.sex.black, weights2019.race.sex.black, weights2020.race.sex.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.sex.black) <- c("session_id", "weightvec.race")
weights.all.race.sex.black$session_id <- as.character(weights.all.race.sex.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.black <- timeseriesSex_NEW[timeseriesSex_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
sex.black <- merge(timeseriesSex_NEW, weights.all.race.sex.black, by = "session_id")

## create the weights per year for asian
weights2017.race.sex.asian <- anesrake(targets.race.sex, sex.asian.2017, caseid=sex.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.sex.asian <- anesrake(targets.race.sex, sex.asian.2018, caseid=sex.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.sex.asian <- anesrake(targets.race.sex, sex.asian.2019, caseid=sex.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.sex.asian <- anesrake(targets.race.sex, sex.asian.2020, caseid=sex.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.sex.asian <- as.data.frame(weights2017.race.sex.asian[c("caseid", "weightvec")])
weights2018.race.sex.asian <- as.data.frame(weights2018.race.sex.asian[c("caseid", "weightvec")])
weights2019.race.sex.asian <- as.data.frame(weights2019.race.sex.asian[c("caseid", "weightvec")])
weights2020.race.sex.asian <- as.data.frame(weights2020.race.sex.asian[c("caseid", "weightvec")])

weights.all.race.sex.asian <- rbind(weights2017.race.sex.asian, weights2018.race.sex.asian, weights2019.race.sex.asian, weights2020.race.sex.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.sex.asian) <- c("session_id", "weightvec.race")
weights.all.race.sex.asian$session_id <- as.character(weights.all.race.sex.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.asian <- timeseriesSex_NEW[timeseriesSex_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
sex.asian <- merge(timeseriesSex_NEW, weights.all.race.sex.asian, by = "session_id")

sex.whblkasi <- rbind(sex.white, sex.black, sex.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.sex <- targets_sexuality[c("Wpol", "Wrace", "Wedu", "Wgender")]
targets.age.sex

timeseriesSex_NEW$age.grp <- cut(timeseriesSex_NEW$age.new, c(0,20,40,99))
levels(timeseriesSex_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
sex.young.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$age.grp == "young",]
sex.young.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$age.grp == "young",]
sex.young.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$age.grp == "young",]
sex.young.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$age.grp == "young",]
# repeat for other group
sex.middle.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$age.grp == "middle",] 
sex.middle.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$age.grp == "middle",]
sex.middle.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$age.grp == "middle",]
sex.middle.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$age.grp == "middle",]
# repeat for other group
sex.old.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$age.grp == "old",] 
sex.old.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$age.grp == "old",]
sex.old.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$age.grp == "old",]
sex.old.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.sex.young <- anesrake(targets.age.sex, sex.young.2017, caseid=sex.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.sex.young <- anesrake(targets.age.sex, sex.young.2018, caseid=sex.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.sex.young <- anesrake(targets.age.sex, sex.young.2019, caseid=sex.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.sex.young <- anesrake(targets.age.sex, sex.young.2020, caseid=sex.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.sex.young2 <- weights2017.age.sex.young[c("caseid", "weightvec")]
weights2017.age.sex.young2 <- as.data.frame(cbind(weights2017.age.sex.young2$caseid,
                                                  weights2017.age.sex.young2$weightvec))
weights2018.age.sex.young2 <- weights2018.age.sex.young[c("caseid", "weightvec")]
weights2018.age.sex.young2 <- as.data.frame(cbind(weights2018.age.sex.young2$caseid,
                                                  weights2018.age.sex.young2$weightvec))
weights2019.age.sex.young2 <- weights2019.age.sex.young[c("caseid", "weightvec")]
weights2019.age.sex.young2 <- as.data.frame(cbind(weights2019.age.sex.young2$caseid,
                                                  weights2019.age.sex.young2$weightvec))
weights2020.age.sex.young2 <- weights2020.age.sex.young[c("caseid", "weightvec")]
weights2020.age.sex.young2 <- as.data.frame(cbind(weights2020.age.sex.young2$caseid,
                                                  weights2020.age.sex.young2$weightvec))

weights.all.age.sex.young <- rbind(weights2017.age.sex.young2, weights2018.age.sex.young2,
                                   weights2019.age.sex.young2, weights2020.age.sex.young2)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.sex.young) <- c("session_id", "weightvec.age")
weights.all.age.sex.young$session_id <- as.character(weights.all.age.sex.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.young <- timeseriesSex_NEW[timeseriesSex_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
sex.young <- merge(timeseriesSex_NEW, weights.all.age.sex.young, by = "session_id")

## create the weights per year for middle
weights2017.age.sex.middle <- anesrake(targets.age.sex, sex.middle.2017, caseid=sex.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.sex.middle <- anesrake(targets.age.sex, sex.middle.2018, caseid=sex.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.sex.middle <- anesrake(targets.age.sex, sex.middle.2019, caseid=sex.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.sex.middle <- anesrake(targets.age.sex, sex.middle.2020, caseid=sex.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.sex.middle2 <- weights2017.age.sex.middle[c("caseid", "weightvec")]
weights2017.age.sex.middle2 <- as.data.frame(cbind(weights2017.age.sex.middle2$caseid,
                                                  weights2017.age.sex.middle2$weightvec))
weights2018.age.sex.middle2 <- weights2018.age.sex.middle[c("caseid", "weightvec")]
weights2018.age.sex.middle2 <- as.data.frame(cbind(weights2018.age.sex.middle2$caseid,
                                                  weights2018.age.sex.middle2$weightvec))
weights2019.age.sex.middle2 <- weights2019.age.sex.middle[c("caseid", "weightvec")]
weights2019.age.sex.middle2 <- as.data.frame(cbind(weights2019.age.sex.middle2$caseid,
                                                  weights2019.age.sex.middle2$weightvec))
weights2020.age.sex.middle2 <- weights2020.age.sex.middle[c("caseid", "weightvec")]
weights2020.age.sex.middle2 <- as.data.frame(cbind(weights2020.age.sex.middle2$caseid,
                                                  weights2020.age.sex.middle2$weightvec))

weights.all.age.sex.middle <- rbind(weights2017.age.sex.middle2, weights2018.age.sex.middle2,
                                   weights2019.age.sex.middle2, weights2020.age.sex.middle2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.sex.middle) <- c("session_id", "weightvec.age")
weights.all.age.sex.middle$session_id <- as.character(weights.all.age.sex.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.middle <- timeseriesSex_NEW[timeseriesSex_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
sex.middle <- merge(timeseriesSex_NEW, weights.all.age.sex.middle, by = "session_id")

## create the weights per year for old
weights2017.age.sex.old <- anesrake(targets.age.sex, sex.old.2017, caseid=sex.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.sex.old <- anesrake(targets.age.sex, sex.old.2018, caseid=sex.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.sex.old <- anesrake(targets.age.sex, sex.old.2019, caseid=sex.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.sex.old <- anesrake(targets.age.sex, sex.old.2020, caseid=sex.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.sex.old2 <- weights2017.age.sex.old[c("caseid", "weightvec")]
weights2017.age.sex.old2 <- as.data.frame(cbind(weights2017.age.sex.old2$caseid,
                                                  weights2017.age.sex.old2$weightvec))
weights2018.age.sex.old2 <- weights2018.age.sex.old[c("caseid", "weightvec")]
weights2018.age.sex.old2 <- as.data.frame(cbind(weights2018.age.sex.old2$caseid,
                                                  weights2018.age.sex.old2$weightvec))
weights2019.age.sex.old2 <- weights2019.age.sex.old[c("caseid", "weightvec")]
weights2019.age.sex.old2 <- as.data.frame(cbind(weights2019.age.sex.old2$caseid,
                                                  weights2019.age.sex.old2$weightvec))
weights2020.age.sex.old2 <- weights2020.age.sex.old[c("caseid", "weightvec")]
weights2020.age.sex.old2 <- as.data.frame(cbind(weights2020.age.sex.old2$caseid,
                                                  weights2020.age.sex.old2$weightvec))

weights.all.age.sex.old <- rbind(weights2017.age.sex.old2, weights2018.age.sex.old2,
                                   weights2019.age.sex.old2, weights2020.age.sex.old2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.sex.old) <- c("session_id", "weightvec.age")
weights.all.age.sex.old$session_id <- as.character(weights.all.age.sex.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.old <- timeseriesSex_NEW[timeseriesSex_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.old <- merge(timeseriesSex_NEW, weights.all.age.sex.old, by = "session_id")


sex.ygmidold <- rbind(sex.young, sex.middle, sex.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.sex <- targets_sexuality[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]
targets.reli.sex

timeseriesSex_NEW$relisimple <- recode(timeseriesSex_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
sex.christ.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$relisimple == "christian",]
sex.christ.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$relisimple == "christian",]
sex.christ.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$relisimple == "christian",]
sex.christ.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$relisimple == "christian",]
# repeat for other group
sex.jew.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$relisimple == "jewish",] 
sex.jew.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$relisimple == "jewish",]
sex.jew.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$relisimple == "jewish",]
sex.jew.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$relisimple == "jewish",]
# repeat for other group
sex.nonrel.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$relisimple == "nonrel",] 
sex.nonrel.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$relisimple == "nonrel",]
sex.nonrel.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$relisimple == "nonrel",]
sex.nonrel.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$relisimple == "nonrel",]
# repeat for other group
sex.other.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$relisimple == "other",] 
sex.other.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$relisimple == "other",]
sex.other.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$relisimple == "other",]
sex.other.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.sex.christ <- anesrake(targets.reli.sex, sex.christ.2017, caseid=sex.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.sex.christ <- anesrake(targets.reli.sex, sex.christ.2018, caseid=sex.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.sex.christ <- anesrake(targets.reli.sex, sex.christ.2019, caseid=sex.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.sex.christ <- anesrake(targets.reli.sex, sex.christ.2020, caseid=sex.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.sex.christ <- as.data.frame(weights2017.reli.sex.christ[c("caseid", "weightvec")])
weights2018.reli.sex.christ <- as.data.frame(weights2018.reli.sex.christ[c("caseid", "weightvec")])
weights2019.reli.sex.christ <- as.data.frame(weights2019.reli.sex.christ[c("caseid", "weightvec")])
weights2020.reli.sex.christ <- as.data.frame(weights2020.reli.sex.christ[c("caseid", "weightvec")])

weights.all.reli.sex.christ <- rbind(weights2017.reli.sex.christ, weights2018.reli.sex.christ, weights2019.reli.sex.christ, weights2020.reli.sex.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.sex.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.sex.christ$session_id <- as.character(weights.all.reli.sex.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.christ <- timeseriesSex_NEW[timeseriesSex_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
sex.christ <- merge(timeseriesSex_NEW, weights.all.reli.sex.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.sex.jew <- anesrake(targets.reli.sex, sex.jew.2017, caseid=sex.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.sex.jew <- anesrake(targets.reli.sex, sex.jew.2018, caseid=sex.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.sex.jew <- anesrake(targets.reli.sex, sex.jew.2019, caseid=sex.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.sex.jew <- anesrake(targets.reli.sex, sex.jew.2020, caseid=sex.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.sex.jew <- as.data.frame(weights2017.reli.sex.jew[c("caseid", "weightvec")])
weights2018.reli.sex.jew <- as.data.frame(weights2018.reli.sex.jew[c("caseid", "weightvec")])
weights2019.reli.sex.jew <- as.data.frame(weights2019.reli.sex.jew[c("caseid", "weightvec")])
weights2020.reli.sex.jew <- as.data.frame(weights2020.reli.sex.jew[c("caseid", "weightvec")])

weights.all.reli.sex.jew <- rbind(weights2017.reli.sex.jew, weights2018.reli.sex.jew, weights2019.reli.sex.jew, weights2020.reli.sex.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.sex.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.sex.jew$session_id <- as.character(weights.all.reli.sex.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.jew <- timeseriesSex_NEW[timeseriesSex_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.jew <- merge(timeseriesSex_NEW, weights.all.reli.sex.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.sex.nonrel <- anesrake(targets.reli.sex, sex.nonrel.2017, caseid=sex.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.sex.nonrel <- anesrake(targets.reli.sex, sex.nonrel.2018, caseid=sex.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.sex.nonrel <- anesrake(targets.reli.sex, sex.nonrel.2019, caseid=sex.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.sex.nonrel <- anesrake(targets.reli.sex, sex.nonrel.2020, caseid=sex.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.sex.nonrel <- as.data.frame(weights2017.reli.sex.nonrel[c("caseid", "weightvec")])
weights2018.reli.sex.nonrel <- as.data.frame(weights2018.reli.sex.nonrel[c("caseid", "weightvec")])
weights2019.reli.sex.nonrel <- as.data.frame(weights2019.reli.sex.nonrel[c("caseid", "weightvec")])
weights2020.reli.sex.nonrel <- as.data.frame(weights2020.reli.sex.nonrel[c("caseid", "weightvec")])

weights.all.reli.sex.nonrel <- rbind(weights2017.reli.sex.nonrel, weights2018.reli.sex.nonrel, weights2019.reli.sex.nonrel, weights2020.reli.sex.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.sex.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.sex.nonrel$session_id <- as.character(weights.all.reli.sex.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.nonrel <- timeseriesSex_NEW[timeseriesSex_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
sex.nonrel <- merge(timeseriesSex_NEW, weights.all.reli.sex.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.sex.other <- anesrake(targets.reli.sex, sex.other.2017, caseid=sex.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.sex.other <- anesrake(targets.reli.sex, sex.other.2018, caseid=sex.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.sex.other <- anesrake(targets.reli.sex, sex.other.2019, caseid=sex.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.sex.other <- anesrake(targets.reli.sex, sex.other.2020, caseid=sex.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.sex.other <- as.data.frame(weights2017.reli.sex.other[c("caseid", "weightvec")])
weights2018.reli.sex.other <- as.data.frame(weights2018.reli.sex.other[c("caseid", "weightvec")])
weights2019.reli.sex.other <- as.data.frame(weights2019.reli.sex.other[c("caseid", "weightvec")])
weights2020.reli.sex.other <- as.data.frame(weights2020.reli.sex.other[c("caseid", "weightvec")])

weights.all.reli.sex.other <- rbind(weights2017.reli.sex.other, weights2018.reli.sex.other, weights2019.reli.sex.other, weights2020.reli.sex.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.sex.other) <- c("session_id", "weightvec.religion")
weights.all.reli.sex.other$session_id <- as.character(weights.all.reli.sex.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.other <- timeseriesSex_NEW[timeseriesSex_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
sex.other <- merge(timeseriesSex_NEW, weights.all.reli.sex.other, by = "session_id")

sex.allreligion <- rbind(sex.christ, sex.jew, sex.nonrel, sex.other)

#### Weighting for SEXUALITY (Straight, Les/Gay, Bisexual) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.sexori.sex <- targets_sexuality[c("Wpol", "Wrace", "Wedu", "Wgender", "Wage")]
targets.sexori.sex

table(timeseriesSex_NEW$sexuality_simp)

## Get sub-data for each year and each group, to then equate to full population
sex.straight.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$sexuality_simp == "Straight",]
sex.straight.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$sexuality_simp == "Straight",]
sex.straight.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$sexuality_simp == "Straight",]
sex.straight.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$sexuality_simp == "Straight",]
# repeat for other group
sex.lesgay.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$sexuality_simp == "Lesbian or Gay",] 
sex.lesgay.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$sexuality_simp == "Lesbian or Gay",]
sex.lesgay.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$sexuality_simp == "Lesbian or Gay",]
sex.lesgay.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$sexuality_simp == "Lesbian or Gay",]
# repeat for other group
sex.other.2017 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2017 & timeseriesSex_NEW$sexuality_simp == "Other",] 
sex.other.2018 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2018 & timeseriesSex_NEW$sexuality_simp == "Other",]
sex.other.2019 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2019 & timeseriesSex_NEW$sexuality_simp == "Other",]
sex.other.2020 <- timeseriesSex_NEW[timeseriesSex_NEW$year==2020 & timeseriesSex_NEW$sexuality_simp == "Other",]

## create the weights per year for straight
weights2017.sexori.sex.straight <- anesrake(targets.sexori.sex, sex.straight.2017, caseid=sex.straight.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.sexori.sex.straight <- anesrake(targets.sexori.sex, sex.straight.2018, caseid=sex.straight.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.sexori.sex.straight <- anesrake(targets.sexori.sex, sex.straight.2019, caseid=sex.straight.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.sexori.sex.straight <- anesrake(targets.sexori.sex, sex.straight.2020, caseid=sex.straight.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.sexori.sex.straight <- as.data.frame(weights2017.sexori.sex.straight[c("caseid", "weightvec")])
weights2018.sexori.sex.straight <- as.data.frame(weights2018.sexori.sex.straight[c("caseid", "weightvec")])
weights2019.sexori.sex.straight <- as.data.frame(weights2019.sexori.sex.straight[c("caseid", "weightvec")])
weights2020.sexori.sex.straight <- as.data.frame(weights2020.sexori.sex.straight[c("caseid", "weightvec")])

weights.all.sexori.sex.straight <- rbind(weights2017.sexori.sex.straight, weights2018.sexori.sex.straight, weights2019.sexori.sex.straight, weights2020.sexori.sex.straight)

# now create the vector of dscores and weights and month variables
colnames(weights.all.sexori.sex.straight) <- c("session_id", "weightvec.sexori")
weights.all.sexori.sex.straight$session_id <- as.character(weights.all.sexori.sex.straight$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.straight <- timeseriesSex_NEW[timeseriesSex_NEW$sexuality_simp == "Straight", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
sex.straight <- merge(timeseriesSex_NEW, weights.all.sexori.sex.straight, by = "session_id")

## create the weights per year for lesgay
weights2017.sexori.sex.lesgay <- anesrake(targets.sexori.sex, sex.lesgay.2017, caseid=sex.lesgay.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.sexori.sex.lesgay <- anesrake(targets.sexori.sex, sex.lesgay.2018, caseid=sex.lesgay.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.sexori.sex.lesgay <- anesrake(targets.sexori.sex, sex.lesgay.2019, caseid=sex.lesgay.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.sexori.sex.lesgay <- anesrake(targets.sexori.sex, sex.lesgay.2020, caseid=sex.lesgay.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.sexori.sex.lesgay <- as.data.frame(weights2017.sexori.sex.lesgay[c("caseid", "weightvec")])
weights2018.sexori.sex.lesgay <- as.data.frame(weights2018.sexori.sex.lesgay[c("caseid", "weightvec")])
weights2019.sexori.sex.lesgay <- as.data.frame(weights2019.sexori.sex.lesgay[c("caseid", "weightvec")])
weights2020.sexori.sex.lesgay <- as.data.frame(weights2020.sexori.sex.lesgay[c("caseid", "weightvec")])

weights.all.sexori.sex.lesgay <- rbind(weights2017.sexori.sex.lesgay, weights2018.sexori.sex.lesgay, weights2019.sexori.sex.lesgay, weights2020.sexori.sex.lesgay)

## now create the vector of dscores and weights and month variables
colnames(weights.all.sexori.sex.lesgay) <- c("session_id", "weightvec.sexori")
weights.all.sexori.sex.lesgay$session_id <- as.character(weights.all.sexori.sex.lesgay$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.lesgay <- timeseriesSex_NEW[timeseriesSex_NEW$sexuality_simp == "Lesbian or Gay", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
sex.lesgay <- merge(timeseriesSex_NEW, weights.all.sexori.sex.lesgay, by = "session_id")

## create the weights per year for other
weights2017.sexori.sex.other <- anesrake(targets.sexori.sex, sex.other.2017, caseid=sex.other.2017$session_id,
                                    verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.sexori.sex.other <- anesrake(targets.sexori.sex, sex.other.2018, caseid=sex.other.2018$session_id,
                                    verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.sexori.sex.other <- anesrake(targets.sexori.sex, sex.other.2019, caseid=sex.other.2019$session_id,
                                    verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.sexori.sex.other <- anesrake(targets.sexori.sex, sex.other.2020, caseid=sex.other.2020$session_id,
                                    verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.sexori.sex.other <- as.data.frame(weights2017.sexori.sex.other[c("caseid", "weightvec")])
weights2018.sexori.sex.other <- as.data.frame(weights2018.sexori.sex.other[c("caseid", "weightvec")])
weights2019.sexori.sex.other <- as.data.frame(weights2019.sexori.sex.other[c("caseid", "weightvec")])
weights2020.sexori.sex.other <- as.data.frame(weights2020.sexori.sex.other[c("caseid", "weightvec")])

weights.all.sexori.sex.other <- rbind(weights2017.sexori.sex.other, weights2018.sexori.sex.other, weights2019.sexori.sex.other, weights2020.sexori.sex.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.sexori.sex.other) <- c("session_id", "weightvec.sexori")
weights.all.sexori.sex.other$session_id <- as.character(weights.all.sexori.sex.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
sex.other <- timeseriesSex_NEW[timeseriesSex_NEW$sexuality_simp == "Other", 
                             c("session_id", "date", "year", "month", "month.cent", 
                               "dscore", "att.new", "attnumeric",
                               "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                               "politicalid.new", "edu.new", "Order", "num.new",
                               "STATE", "MSAName", "CountyNo", "rel.new",
                               "weightvec")]
sex.other <- merge(timeseriesSex_NEW, weights.all.sexori.sex.other, by = "session_id")


sex.sexuality <- rbind(sex.straight, sex.lesgay, sex.other)



## AGE ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.age <- targets_age[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.age

## Get sub-data for each year and each political identity, to then equate to full population
age.lib.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$Wpol>4,]
age.lib.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$Wpol>4,]
age.lib.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$Wpol>4,]
age.lib.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$Wpol>4,]
# repeat for conservatives
age.cons.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$Wpol<4,] 
age.cons.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$Wpol<4,]
age.cons.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$Wpol<4,]
age.cons.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.age.lib <- anesrake(targets.pol.age, age.lib.2017, caseid=age.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.age.lib <- anesrake(targets.pol.age, age.lib.2018, caseid=age.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.age.lib <- anesrake(targets.pol.age, age.lib.2019, caseid=age.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.age.lib <- anesrake(targets.pol.age, age.lib.2020, caseid=age.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.age.lib <- as.data.frame(weights2017.pol.age.lib[c("caseid", "weightvec")])
weights2018.pol.age.lib <- as.data.frame(weights2018.pol.age.lib[c("caseid", "weightvec")])
weights2019.pol.age.lib <- as.data.frame(weights2019.pol.age.lib[c("caseid", "weightvec")])
weights2020.pol.age.lib <- as.data.frame(weights2020.pol.age.lib[c("caseid", "weightvec")])

weights.all.pol.age.lib <- rbind(weights2017.pol.age.lib, weights2018.pol.age.lib, weights2019.pol.age.lib, weights2020.pol.age.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.age.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.age.lib$session_id <- as.character(weights.all.pol.age.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.liberals <- timeseriesAge_NEW[timeseriesAge_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
age.liberals <- merge(timeseriesAge_NEW, weights.all.pol.age.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.age.cons <- anesrake(targets.pol.age, age.cons.2017, caseid=age.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.age.cons <- anesrake(targets.pol.age, age.cons.2018, caseid=age.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.age.cons <- anesrake(targets.pol.age, age.cons.2019, caseid=age.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.age.cons <- anesrake(targets.pol.age, age.cons.2020, caseid=age.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.age.cons <- as.data.frame(weights2017.pol.age.cons[c("caseid", "weightvec")])
weights2018.pol.age.cons <- as.data.frame(weights2018.pol.age.cons[c("caseid", "weightvec")])
weights2019.pol.age.cons <- as.data.frame(weights2019.pol.age.cons[c("caseid", "weightvec")])
weights2020.pol.age.cons <- as.data.frame(weights2020.pol.age.cons[c("caseid", "weightvec")])

weights.all.pol.age.cons <- rbind(weights2017.pol.age.cons, weights2018.pol.age.cons, weights2019.pol.age.cons, weights2020.pol.age.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.age.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.age.cons$session_id <- as.character(weights.all.pol.age.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.conserv <- timeseriesAge_NEW[timeseriesAge_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
age.conserv <- merge(timeseriesAge_NEW, weights.all.pol.age.cons, by = "session_id")

age.libcons <- rbind(age.liberals, age.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.age <- targets_age[c("Wpol", "Wrace", "Wedu", "Wage")]

## Get sub-data for each year and each group, to then equate to full population
age.fem.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$genderidentity_simp == "female",]
age.fem.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$genderidentity_simp == "female",]
age.fem.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$genderidentity_simp == "female",]
age.fem.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$genderidentity_simp == "female",]
# repeat for other group
age.mal.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$genderidentity_simp == "male",] 
age.mal.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$genderidentity_simp == "male",]
age.mal.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$genderidentity_simp == "male",]
age.mal.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.age.fem <- anesrake(targets.gender.age, age.fem.2017, caseid=age.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.age.fem <- anesrake(targets.gender.age, age.fem.2018, caseid=age.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.age.fem <- anesrake(targets.gender.age, age.fem.2019, caseid=age.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.age.fem <- anesrake(targets.gender.age, age.fem.2020, caseid=age.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.age.fem <- as.data.frame(weights2017.gender.age.fem[c("caseid", "weightvec")])
weights2018.gender.age.fem <- as.data.frame(weights2018.gender.age.fem[c("caseid", "weightvec")])
weights2019.gender.age.fem <- as.data.frame(weights2019.gender.age.fem[c("caseid", "weightvec")])
weights2020.gender.age.fem <- as.data.frame(weights2020.gender.age.fem[c("caseid", "weightvec")])

weights.all.gender.age.fem <- rbind(weights2017.gender.age.fem, weights2018.gender.age.fem, weights2019.gender.age.fem, weights2020.gender.age.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.age.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.age.fem$session_id <- as.character(weights.all.gender.age.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.fem <- timeseriesAge_NEW[timeseriesAge_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
age.fem <- merge(timeseriesAge_NEW, weights.all.gender.age.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.age.mal <- anesrake(targets.gender.age, age.mal.2017, caseid=age.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.age.mal <- anesrake(targets.gender.age, age.mal.2018, caseid=age.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.age.mal <- anesrake(targets.gender.age, age.mal.2019, caseid=age.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.age.mal <- anesrake(targets.gender.age, age.mal.2020, caseid=age.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.age.mal <- as.data.frame(weights2017.gender.age.mal[c("caseid", "weightvec")])
weights2018.gender.age.mal <- as.data.frame(weights2018.gender.age.mal[c("caseid", "weightvec")])
weights2019.gender.age.mal <- as.data.frame(weights2019.gender.age.mal[c("caseid", "weightvec")])
weights2020.gender.age.mal <- as.data.frame(weights2020.gender.age.mal[c("caseid", "weightvec")])

weights.all.gender.age.mal <- rbind(weights2017.gender.age.mal, weights2018.gender.age.mal, weights2019.gender.age.mal, weights2020.gender.age.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.age.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.age.mal$session_id <- as.character(weights.all.gender.age.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.mal <- timeseriesAge_NEW[timeseriesAge_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
age.mal <- merge(timeseriesAge_NEW, weights.all.gender.age.mal, by = "session_id")

age.femmal <- rbind(age.fem, age.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.age <- targets_age[c("Wpol", "Wrace", "Wgender", "Wage")]

timeseriesAge_NEW$edu.grp <- as.numeric(timeseriesAge_NEW$edu.new) - 5
timeseriesAge_NEW$edu.grp <- ifelse(timeseriesAge_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
age.ncol.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$edu.grp == "nocollege",]
age.ncol.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$edu.grp == "nocollege",]
age.ncol.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$edu.grp == "nocollege",]
age.ncol.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$edu.grp == "nocollege",]
# repeat for other group
age.col.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$edu.grp == "college",] 
age.col.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$edu.grp == "college",]
age.col.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$edu.grp == "college",]
age.col.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.age.ncol <- anesrake(targets.edu.age, age.ncol.2017, caseid=age.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.age.ncol <- anesrake(targets.edu.age, age.ncol.2018, caseid=age.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.age.ncol <- anesrake(targets.edu.age, age.ncol.2019, caseid=age.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.age.ncol <- anesrake(targets.edu.age, age.ncol.2020, caseid=age.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.age.ncol <- as.data.frame(weights2017.edu.age.ncol[c("caseid", "weightvec")])
weights2018.edu.age.ncol <- as.data.frame(weights2018.edu.age.ncol[c("caseid", "weightvec")])
weights2019.edu.age.ncol <- as.data.frame(weights2019.edu.age.ncol[c("caseid", "weightvec")])
weights2020.edu.age.ncol <- as.data.frame(weights2020.edu.age.ncol[c("caseid", "weightvec")])

weights.all.edu.age.ncol <- rbind(weights2017.edu.age.ncol, weights2018.edu.age.ncol, weights2019.edu.age.ncol, weights2020.edu.age.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.age.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.age.ncol$session_id <- as.character(weights.all.edu.age.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.ncol <- timeseriesAge_NEW[timeseriesAge_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
age.ncol <- merge(timeseriesAge_NEW, weights.all.edu.age.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.age.col <- anesrake(targets.edu.age, age.col.2017, caseid=age.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.age.col <- anesrake(targets.edu.age, age.col.2018, caseid=age.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.age.col <- anesrake(targets.edu.age, age.col.2019, caseid=age.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.age.col <- anesrake(targets.edu.age, age.col.2020, caseid=age.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.age.col <- as.data.frame(weights2017.edu.age.col[c("caseid", "weightvec")])
weights2018.edu.age.col <- as.data.frame(weights2018.edu.age.col[c("caseid", "weightvec")])
weights2019.edu.age.col <- as.data.frame(weights2019.edu.age.col[c("caseid", "weightvec")])
weights2020.edu.age.col <- as.data.frame(weights2020.edu.age.col[c("caseid", "weightvec")])

weights.all.edu.age.col <- rbind(weights2017.edu.age.col, weights2018.edu.age.col, weights2019.edu.age.col, weights2020.edu.age.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.age.col) <- c("session_id", "weightvec.edu")
weights.all.edu.age.col$session_id <- as.character(weights.all.edu.age.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.col <- timeseriesAge_NEW[timeseriesAge_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
age.col <- merge(timeseriesAge_NEW, weights.all.edu.age.col, by = "session_id")

age.ncolcol <- rbind(age.ncol, age.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.age <- targets_age[c("Wpol", "Wgender", "Wedu", "Wage")]

timeseriesAge_NEW$racesimple <- recode(timeseriesAge_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
age.white.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$racesimple == "White",]
age.white.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$racesimple == "White",]
age.white.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$racesimple == "White",]
age.white.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$racesimple == "White",]
# repeat for other group
age.black.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$racesimple == "Black",] 
age.black.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$racesimple == "Black",]
age.black.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$racesimple == "Black",]
age.black.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$racesimple == "Black",]
# repeat for other group
age.asian.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$racesimple == "Asian",] 
age.asian.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$racesimple == "Asian",]
age.asian.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$racesimple == "Asian",]
age.asian.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.age.white <- anesrake(targets.race.age, age.white.2017, caseid=age.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.age.white <- anesrake(targets.race.age, age.white.2018, caseid=age.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.age.white <- anesrake(targets.race.age, age.white.2019, caseid=age.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.age.white <- anesrake(targets.race.age, age.white.2020, caseid=age.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.age.white <- as.data.frame(weights2017.race.age.white[c("caseid", "weightvec")])
weights2018.race.age.white <- as.data.frame(weights2018.race.age.white[c("caseid", "weightvec")])
weights2019.race.age.white <- as.data.frame(weights2019.race.age.white[c("caseid", "weightvec")])
weights2020.race.age.white <- as.data.frame(weights2020.race.age.white[c("caseid", "weightvec")])

weights.all.race.age.white <- rbind(weights2017.race.age.white, weights2018.race.age.white, weights2019.race.age.white, weights2020.race.age.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.age.white) <- c("session_id", "weightvec.race")
weights.all.race.age.white$session_id <- as.character(weights.all.race.age.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.white <- timeseriesAge_NEW[timeseriesAge_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
age.white <- merge(timeseriesAge_NEW, weights.all.race.age.white, by = "session_id")

## create the weights per year for black
weights2017.race.age.black <- anesrake(targets.race.age, age.black.2017, caseid=age.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.age.black <- anesrake(targets.race.age, age.black.2018, caseid=age.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.age.black <- anesrake(targets.race.age, age.black.2019, caseid=age.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.age.black <- anesrake(targets.race.age, age.black.2020, caseid=age.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.age.black <- as.data.frame(weights2017.race.age.black[c("caseid", "weightvec")])
weights2018.race.age.black <- as.data.frame(weights2018.race.age.black[c("caseid", "weightvec")])
weights2019.race.age.black <- as.data.frame(weights2019.race.age.black[c("caseid", "weightvec")])
weights2020.race.age.black <- as.data.frame(weights2020.race.age.black[c("caseid", "weightvec")])

weights.all.race.age.black <- rbind(weights2017.race.age.black, weights2018.race.age.black, weights2019.race.age.black, weights2020.race.age.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.age.black) <- c("session_id", "weightvec.race")
weights.all.race.age.black$session_id <- as.character(weights.all.race.age.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.black <- timeseriesAge_NEW[timeseriesAge_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
age.black <- merge(timeseriesAge_NEW, weights.all.race.age.black, by = "session_id")

## create the weights per year for asian
weights2017.race.age.asian <- anesrake(targets.race.age, age.asian.2017, caseid=age.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.age.asian <- anesrake(targets.race.age, age.asian.2018, caseid=age.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.age.asian <- anesrake(targets.race.age, age.asian.2019, caseid=age.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.age.asian <- anesrake(targets.race.age, age.asian.2020, caseid=age.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.age.asian <- as.data.frame(weights2017.race.age.asian[c("caseid", "weightvec")])
weights2018.race.age.asian <- as.data.frame(weights2018.race.age.asian[c("caseid", "weightvec")])
weights2019.race.age.asian <- as.data.frame(weights2019.race.age.asian[c("caseid", "weightvec")])
weights2020.race.age.asian <- as.data.frame(weights2020.race.age.asian[c("caseid", "weightvec")])

weights.all.race.age.asian <- rbind(weights2017.race.age.asian, weights2018.race.age.asian, weights2019.race.age.asian, weights2020.race.age.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.age.asian) <- c("session_id", "weightvec.race")
weights.all.race.age.asian$session_id <- as.character(weights.all.race.age.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.asian <- timeseriesAge_NEW[timeseriesAge_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
age.asian <- merge(timeseriesAge_NEW, weights.all.race.age.asian, by = "session_id")

age.whblkasi <- rbind(age.white, age.black, age.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.age <- targets_age[c("Wpol", "Wrace", "Wedu", "Wgender")]

timeseriesAge_NEW$age.grp <- cut(timeseriesAge_NEW$age.new, c(0,20,40,99))
levels(timeseriesAge_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
age.young.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$age.grp == "young",]
age.young.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$age.grp == "young",]
age.young.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$age.grp == "young",]
age.young.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$age.grp == "young",]
# repeat for other group
age.middle.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$age.grp == "middle",] 
age.middle.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$age.grp == "middle",]
age.middle.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$age.grp == "middle",]
age.middle.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$age.grp == "middle",]
# repeat for other group
age.old.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$age.grp == "old",] 
age.old.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$age.grp == "old",]
age.old.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$age.grp == "old",]
age.old.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.age.young <- anesrake(targets.age.age, age.young.2017, caseid=age.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.age.young <- anesrake(targets.age.age, age.young.2018, caseid=age.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.age.young <- anesrake(targets.age.age, age.young.2019, caseid=age.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.age.young <- anesrake(targets.age.age, age.young.2020, caseid=age.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.age.young2 <- weights2017.age.age.young[c("caseid", "weightvec")]
weights2017.age.age.young2 <- as.data.frame(cbind(weights2017.age.age.young2$caseid,
                                                   weights2017.age.age.young2$weightvec))
weights2018.age.age.young2 <- weights2018.age.age.young[c("caseid", "weightvec")]
weights2018.age.age.young2 <- as.data.frame(cbind(weights2018.age.age.young2$caseid,
                                                   weights2018.age.age.young2$weightvec))
weights2019.age.age.young2 <- weights2019.age.age.young[c("caseid", "weightvec")]
weights2019.age.age.young2 <- as.data.frame(cbind(weights2019.age.age.young2$caseid,
                                                   weights2019.age.age.young2$weightvec))
weights2020.age.age.young2 <- weights2020.age.age.young[c("caseid", "weightvec")]
weights2020.age.age.young2 <- as.data.frame(cbind(weights2020.age.age.young2$caseid,
                                                   weights2020.age.age.young2$weightvec))

weights.all.age.age.young <- rbind(weights2017.age.age.young2, weights2018.age.age.young2,
                                    weights2019.age.age.young2, weights2020.age.age.young2)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.age.young) <- c("session_id", "weightvec.age")
weights.all.age.age.young$session_id <- as.character(weights.all.age.age.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.young <- timeseriesAge_NEW[timeseriesAge_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
age.young <- merge(timeseriesAge_NEW, weights.all.age.age.young, by = "session_id")

## create the weights per year for middle
weights2017.age.age.middle <- anesrake(targets.age.age, age.middle.2017, caseid=age.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.age.middle <- anesrake(targets.age.age, age.middle.2018, caseid=age.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.age.middle <- anesrake(targets.age.age, age.middle.2019, caseid=age.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.age.middle <- anesrake(targets.age.age, age.middle.2020, caseid=age.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.age.middle2 <- weights2017.age.age.middle[c("caseid", "weightvec")]
weights2017.age.age.middle2 <- as.data.frame(cbind(weights2017.age.age.middle2$caseid,
                                                  weights2017.age.age.middle2$weightvec))
weights2018.age.age.middle2 <- weights2018.age.age.middle[c("caseid", "weightvec")]
weights2018.age.age.middle2 <- as.data.frame(cbind(weights2018.age.age.middle2$caseid,
                                                  weights2018.age.age.middle2$weightvec))
weights2019.age.age.middle2 <- weights2019.age.age.middle[c("caseid", "weightvec")]
weights2019.age.age.middle2 <- as.data.frame(cbind(weights2019.age.age.middle2$caseid,
                                                  weights2019.age.age.middle2$weightvec))
weights2020.age.age.middle2 <- weights2020.age.age.middle[c("caseid", "weightvec")]
weights2020.age.age.middle2 <- as.data.frame(cbind(weights2020.age.age.middle2$caseid,
                                                  weights2020.age.age.middle2$weightvec))

weights.all.age.age.middle <- rbind(weights2017.age.age.middle2, weights2018.age.age.middle2,
                                   weights2019.age.age.middle2, weights2020.age.age.middle2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.age.middle) <- c("session_id", "weightvec.age")
weights.all.age.age.middle$session_id <- as.character(weights.all.age.age.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.middle <- timeseriesAge_NEW[timeseriesAge_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
age.middle <- merge(timeseriesAge_NEW, weights.all.age.age.middle, by = "session_id")

## create the weights per year for old
weights2017.age.age.old <- anesrake(targets.age.age, age.old.2017, caseid=age.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.age.old <- anesrake(targets.age.age, age.old.2018, caseid=age.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.age.old <- anesrake(targets.age.age, age.old.2019, caseid=age.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.age.old <- anesrake(targets.age.age, age.old.2020, caseid=age.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for age attitudes, add in workaround due to NULL rownames)
weights2017.age.age.old2 <- weights2017.age.age.old[c("caseid", "weightvec")]
weights2017.age.age.old2 <- as.data.frame(cbind(weights2017.age.age.old2$caseid,
                                                  weights2017.age.age.old2$weightvec))
weights2018.age.age.old2 <- weights2018.age.age.old[c("caseid", "weightvec")]
weights2018.age.age.old2 <- as.data.frame(cbind(weights2018.age.age.old2$caseid,
                                                  weights2018.age.age.old2$weightvec))
weights2019.age.age.old2 <- weights2019.age.age.old[c("caseid", "weightvec")]
weights2019.age.age.old2 <- as.data.frame(cbind(weights2019.age.age.old2$caseid,
                                                  weights2019.age.age.old2$weightvec))
weights2020.age.age.old2 <- weights2020.age.age.old[c("caseid", "weightvec")]
weights2020.age.age.old2 <- as.data.frame(cbind(weights2020.age.age.old2$caseid,
                                                  weights2020.age.age.old2$weightvec))

weights.all.age.age.old <- rbind(weights2017.age.age.old2, weights2018.age.age.old2,
                                   weights2019.age.age.old2, weights2020.age.age.old2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.age.old) <- c("session_id", "weightvec.age")
weights.all.age.age.old$session_id <- as.character(weights.all.age.age.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.old <- timeseriesAge_NEW[timeseriesAge_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
age.old <- merge(timeseriesAge_NEW, weights.all.age.age.old, by = "session_id")


age.ygmidold <- rbind(age.young, age.middle, age.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.age <- targets_age[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]

timeseriesAge_NEW$relisimple <- recode(timeseriesAge_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
age.christ.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$relisimple == "christian",]
age.christ.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$relisimple == "christian",]
age.christ.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$relisimple == "christian",]
age.christ.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$relisimple == "christian",]
# repeat for other group
age.jew.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$relisimple == "jewish",] 
age.jew.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$relisimple == "jewish",]
age.jew.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$relisimple == "jewish",]
age.jew.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$relisimple == "jewish",]
# repeat for other group
age.nonrel.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$relisimple == "nonrel",] 
age.nonrel.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$relisimple == "nonrel",]
age.nonrel.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$relisimple == "nonrel",]
age.nonrel.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$relisimple == "nonrel",]
# repeat for other group
age.other.2017 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2017 & timeseriesAge_NEW$relisimple == "other",] 
age.other.2018 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2018 & timeseriesAge_NEW$relisimple == "other",]
age.other.2019 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2019 & timeseriesAge_NEW$relisimple == "other",]
age.other.2020 <- timeseriesAge_NEW[timeseriesAge_NEW$year==2020 & timeseriesAge_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.age.christ <- anesrake(targets.reli.age, age.christ.2017, caseid=age.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.age.christ <- anesrake(targets.reli.age, age.christ.2018, caseid=age.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.age.christ <- anesrake(targets.reli.age, age.christ.2019, caseid=age.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.age.christ <- anesrake(targets.reli.age, age.christ.2020, caseid=age.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.age.christ <- as.data.frame(weights2017.reli.age.christ[c("caseid", "weightvec")])
weights2018.reli.age.christ <- as.data.frame(weights2018.reli.age.christ[c("caseid", "weightvec")])
weights2019.reli.age.christ <- as.data.frame(weights2019.reli.age.christ[c("caseid", "weightvec")])
weights2020.reli.age.christ <- as.data.frame(weights2020.reli.age.christ[c("caseid", "weightvec")])

weights.all.reli.age.christ <- rbind(weights2017.reli.age.christ, weights2018.reli.age.christ, weights2019.reli.age.christ, weights2020.reli.age.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.age.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.age.christ$session_id <- as.character(weights.all.reli.age.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.christ <- timeseriesAge_NEW[timeseriesAge_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
age.christ <- merge(timeseriesAge_NEW, weights.all.reli.age.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.age.jew <- anesrake(targets.reli.age, age.jew.2017, caseid=age.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.age.jew <- anesrake(targets.reli.age, age.jew.2018, caseid=age.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.age.jew <- anesrake(targets.reli.age, age.jew.2019, caseid=age.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.age.jew <- anesrake(targets.reli.age, age.jew.2020, caseid=age.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.age.jew <- as.data.frame(weights2017.reli.age.jew[c("caseid", "weightvec")])
weights2018.reli.age.jew <- as.data.frame(weights2018.reli.age.jew[c("caseid", "weightvec")])
weights2019.reli.age.jew <- as.data.frame(weights2019.reli.age.jew[c("caseid", "weightvec")])
weights2020.reli.age.jew <- as.data.frame(weights2020.reli.age.jew[c("caseid", "weightvec")])

weights.all.reli.age.jew <- rbind(weights2017.reli.age.jew, weights2018.reli.age.jew, weights2019.reli.age.jew, weights2020.reli.age.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.age.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.age.jew$session_id <- as.character(weights.all.reli.age.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.jew <- timeseriesAge_NEW[timeseriesAge_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
age.jew <- merge(timeseriesAge_NEW, weights.all.reli.age.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.age.nonrel <- anesrake(targets.reli.age, age.nonrel.2017, caseid=age.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.age.nonrel <- anesrake(targets.reli.age, age.nonrel.2018, caseid=age.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.age.nonrel <- anesrake(targets.reli.age, age.nonrel.2019, caseid=age.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.age.nonrel <- anesrake(targets.reli.age, age.nonrel.2020, caseid=age.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.age.nonrel <- as.data.frame(weights2017.reli.age.nonrel[c("caseid", "weightvec")])
weights2018.reli.age.nonrel <- as.data.frame(weights2018.reli.age.nonrel[c("caseid", "weightvec")])
weights2019.reli.age.nonrel <- as.data.frame(weights2019.reli.age.nonrel[c("caseid", "weightvec")])
weights2020.reli.age.nonrel <- as.data.frame(weights2020.reli.age.nonrel[c("caseid", "weightvec")])

weights.all.reli.age.nonrel <- rbind(weights2017.reli.age.nonrel, weights2018.reli.age.nonrel, weights2019.reli.age.nonrel, weights2020.reli.age.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.age.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.age.nonrel$session_id <- as.character(weights.all.reli.age.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.nonrel <- timeseriesAge_NEW[timeseriesAge_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
age.nonrel <- merge(timeseriesAge_NEW, weights.all.reli.age.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.age.other <- anesrake(targets.reli.age, age.other.2017, caseid=age.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.age.other <- anesrake(targets.reli.age, age.other.2018, caseid=age.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.age.other <- anesrake(targets.reli.age, age.other.2019, caseid=age.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.age.other <- anesrake(targets.reli.age, age.other.2020, caseid=age.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.age.other <- as.data.frame(weights2017.reli.age.other[c("caseid", "weightvec")])
weights2018.reli.age.other <- as.data.frame(weights2018.reli.age.other[c("caseid", "weightvec")])
weights2019.reli.age.other <- as.data.frame(weights2019.reli.age.other[c("caseid", "weightvec")])
weights2020.reli.age.other <- as.data.frame(weights2020.reli.age.other[c("caseid", "weightvec")])

weights.all.reli.age.other <- rbind(weights2017.reli.age.other, weights2018.reli.age.other, weights2019.reli.age.other, weights2020.reli.age.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.age.other) <- c("session_id", "weightvec.religion")
weights.all.reli.age.other$session_id <- as.character(weights.all.reli.age.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
age.other <- timeseriesAge_NEW[timeseriesAge_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
age.other <- merge(timeseriesAge_NEW, weights.all.reli.age.other, by = "session_id")

age.allreligion <- rbind(age.christ, age.jew, age.nonrel, age.other)




## DISABILITY ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.dis <- targets_disability[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.dis

## Get sub-data for each year and each political identity, to then equate to full population
dis.lib.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$Wpol>4,]
dis.lib.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$Wpol>4,]
dis.lib.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$Wpol>4,]
dis.lib.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$Wpol>4,]
# repeat for conservatives
dis.cons.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$Wpol<4,] 
dis.cons.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$Wpol<4,]
dis.cons.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$Wpol<4,]
dis.cons.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.dis.lib <- anesrake(targets.pol.dis, dis.lib.2017, caseid=dis.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.dis.lib <- anesrake(targets.pol.dis, dis.lib.2018, caseid=dis.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.dis.lib <- anesrake(targets.pol.dis, dis.lib.2019, caseid=dis.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.dis.lib <- anesrake(targets.pol.dis, dis.lib.2020, caseid=dis.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.dis.lib <- as.data.frame(weights2017.pol.dis.lib[c("caseid", "weightvec")])
weights2018.pol.dis.lib <- as.data.frame(weights2018.pol.dis.lib[c("caseid", "weightvec")])
weights2019.pol.dis.lib <- as.data.frame(weights2019.pol.dis.lib[c("caseid", "weightvec")])
weights2020.pol.dis.lib <- as.data.frame(weights2020.pol.dis.lib[c("caseid", "weightvec")])

weights.all.pol.dis.lib <- rbind(weights2017.pol.dis.lib, weights2018.pol.dis.lib, weights2019.pol.dis.lib, weights2020.pol.dis.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.dis.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.dis.lib$session_id <- as.character(weights.all.pol.dis.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.liberals <- timeseriesDis_NEW[timeseriesDis_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
dis.liberals <- merge(timeseriesDis_NEW, weights.all.pol.dis.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.dis.cons <- anesrake(targets.pol.dis, dis.cons.2017, caseid=dis.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.dis.cons <- anesrake(targets.pol.dis, dis.cons.2018, caseid=dis.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.dis.cons <- anesrake(targets.pol.dis, dis.cons.2019, caseid=dis.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.dis.cons <- anesrake(targets.pol.dis, dis.cons.2020, caseid=dis.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.dis.cons <- as.data.frame(weights2017.pol.dis.cons[c("caseid", "weightvec")])
weights2018.pol.dis.cons <- as.data.frame(weights2018.pol.dis.cons[c("caseid", "weightvec")])
weights2019.pol.dis.cons <- as.data.frame(weights2019.pol.dis.cons[c("caseid", "weightvec")])
weights2020.pol.dis.cons <- as.data.frame(weights2020.pol.dis.cons[c("caseid", "weightvec")])

weights.all.pol.dis.cons <- rbind(weights2017.pol.dis.cons, weights2018.pol.dis.cons, weights2019.pol.dis.cons, weights2020.pol.dis.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.dis.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.dis.cons$session_id <- as.character(weights.all.pol.dis.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.conserv <- timeseriesDis_NEW[timeseriesDis_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
dis.conserv <- merge(timeseriesDis_NEW, weights.all.pol.dis.cons, by = "session_id")

dis.libcons <- rbind(dis.liberals, dis.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.dis <- targets_disability[c("Wpol", "Wrace", "Wedu", "Wage")]

## Get sub-data for each year and each group, to then equate to full population
dis.fem.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$genderidentity_simp == "female",]
dis.fem.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$genderidentity_simp == "female",]
dis.fem.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$genderidentity_simp == "female",]
dis.fem.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$genderidentity_simp == "female",]
# repeat for other group
dis.mal.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$genderidentity_simp == "male",] 
dis.mal.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$genderidentity_simp == "male",]
dis.mal.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$genderidentity_simp == "male",]
dis.mal.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.dis.fem <- anesrake(targets.gender.dis, dis.fem.2017, caseid=dis.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.dis.fem <- anesrake(targets.gender.dis, dis.fem.2018, caseid=dis.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.dis.fem <- anesrake(targets.gender.dis, dis.fem.2019, caseid=dis.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.dis.fem <- anesrake(targets.gender.dis, dis.fem.2020, caseid=dis.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.dis.fem <- as.data.frame(weights2017.gender.dis.fem[c("caseid", "weightvec")])
weights2018.gender.dis.fem <- as.data.frame(weights2018.gender.dis.fem[c("caseid", "weightvec")])
weights2019.gender.dis.fem <- as.data.frame(weights2019.gender.dis.fem[c("caseid", "weightvec")])
weights2020.gender.dis.fem <- as.data.frame(weights2020.gender.dis.fem[c("caseid", "weightvec")])

weights.all.gender.dis.fem <- rbind(weights2017.gender.dis.fem, weights2018.gender.dis.fem, weights2019.gender.dis.fem, weights2020.gender.dis.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.dis.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.dis.fem$session_id <- as.character(weights.all.gender.dis.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.fem <- timeseriesDis_NEW[timeseriesDis_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
dis.fem <- merge(timeseriesDis_NEW, weights.all.gender.dis.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.dis.mal <- anesrake(targets.gender.dis, dis.mal.2017, caseid=dis.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.dis.mal <- anesrake(targets.gender.dis, dis.mal.2018, caseid=dis.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.dis.mal <- anesrake(targets.gender.dis, dis.mal.2019, caseid=dis.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.dis.mal <- anesrake(targets.gender.dis, dis.mal.2020, caseid=dis.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.dis.mal <- as.data.frame(weights2017.gender.dis.mal[c("caseid", "weightvec")])
weights2018.gender.dis.mal <- as.data.frame(weights2018.gender.dis.mal[c("caseid", "weightvec")])
weights2019.gender.dis.mal <- as.data.frame(weights2019.gender.dis.mal[c("caseid", "weightvec")])
weights2020.gender.dis.mal <- as.data.frame(weights2020.gender.dis.mal[c("caseid", "weightvec")])

weights.all.gender.dis.mal <- rbind(weights2017.gender.dis.mal, weights2018.gender.dis.mal, weights2019.gender.dis.mal, weights2020.gender.dis.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.dis.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.dis.mal$session_id <- as.character(weights.all.gender.dis.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.mal <- timeseriesDis_NEW[timeseriesDis_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
dis.mal <- merge(timeseriesDis_NEW, weights.all.gender.dis.mal, by = "session_id")

dis.femmal <- rbind(dis.fem, dis.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.dis <- targets_disability[c("Wpol", "Wrace", "Wgender", "Wage")]

timeseriesDis_NEW$edu.grp <- as.numeric(timeseriesDis_NEW$edu.new) - 5
timeseriesDis_NEW$edu.grp <- ifelse(timeseriesDis_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
dis.ncol.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$edu.grp == "nocollege",]
dis.ncol.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$edu.grp == "nocollege",]
dis.ncol.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$edu.grp == "nocollege",]
dis.ncol.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$edu.grp == "nocollege",]
# repeat for other group
dis.col.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$edu.grp == "college",] 
dis.col.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$edu.grp == "college",]
dis.col.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$edu.grp == "college",]
dis.col.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.dis.ncol <- anesrake(targets.edu.dis, dis.ncol.2017, caseid=dis.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.dis.ncol <- anesrake(targets.edu.dis, dis.ncol.2018, caseid=dis.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.dis.ncol <- anesrake(targets.edu.dis, dis.ncol.2019, caseid=dis.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.dis.ncol <- anesrake(targets.edu.dis, dis.ncol.2020, caseid=dis.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.dis.ncol <- as.data.frame(weights2017.edu.dis.ncol[c("caseid", "weightvec")])
weights2018.edu.dis.ncol <- as.data.frame(weights2018.edu.dis.ncol[c("caseid", "weightvec")])
weights2019.edu.dis.ncol <- as.data.frame(weights2019.edu.dis.ncol[c("caseid", "weightvec")])
weights2020.edu.dis.ncol <- as.data.frame(weights2020.edu.dis.ncol[c("caseid", "weightvec")])

weights.all.edu.dis.ncol <- rbind(weights2017.edu.dis.ncol, weights2018.edu.dis.ncol, weights2019.edu.dis.ncol, weights2020.edu.dis.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.dis.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.dis.ncol$session_id <- as.character(weights.all.edu.dis.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.ncol <- timeseriesDis_NEW[timeseriesDis_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
dis.ncol <- merge(timeseriesDis_NEW, weights.all.edu.dis.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.dis.col <- anesrake(targets.edu.dis, dis.col.2017, caseid=dis.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.dis.col <- anesrake(targets.edu.dis, dis.col.2018, caseid=dis.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.dis.col <- anesrake(targets.edu.dis, dis.col.2019, caseid=dis.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.dis.col <- anesrake(targets.edu.dis, dis.col.2020, caseid=dis.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.dis.col <- as.data.frame(weights2017.edu.dis.col[c("caseid", "weightvec")])
weights2018.edu.dis.col <- as.data.frame(weights2018.edu.dis.col[c("caseid", "weightvec")])
weights2019.edu.dis.col <- as.data.frame(weights2019.edu.dis.col[c("caseid", "weightvec")])
weights2020.edu.dis.col <- as.data.frame(weights2020.edu.dis.col[c("caseid", "weightvec")])

weights.all.edu.dis.col <- rbind(weights2017.edu.dis.col, weights2018.edu.dis.col, weights2019.edu.dis.col, weights2020.edu.dis.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.dis.col) <- c("session_id", "weightvec.edu")
weights.all.edu.dis.col$session_id <- as.character(weights.all.edu.dis.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.col <- timeseriesDis_NEW[timeseriesDis_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
dis.col <- merge(timeseriesDis_NEW, weights.all.edu.dis.col, by = "session_id")

dis.ncolcol <- rbind(dis.ncol, dis.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.dis <- targets_disability[c("Wpol", "Wgender", "Wedu", "Wage")]

timeseriesDis_NEW$racesimple <- recode(timeseriesDis_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
dis.white.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$racesimple == "White",]
dis.white.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$racesimple == "White",]
dis.white.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$racesimple == "White",]
dis.white.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$racesimple == "White",]
# repeat for other group
dis.black.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$racesimple == "Black",] 
dis.black.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$racesimple == "Black",]
dis.black.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$racesimple == "Black",]
dis.black.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$racesimple == "Black",]
# repeat for other group
dis.asian.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$racesimple == "Asian",] 
dis.asian.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$racesimple == "Asian",]
dis.asian.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$racesimple == "Asian",]
dis.asian.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.dis.white <- anesrake(targets.race.dis, dis.white.2017, caseid=dis.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.dis.white <- anesrake(targets.race.dis, dis.white.2018, caseid=dis.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.dis.white <- anesrake(targets.race.dis, dis.white.2019, caseid=dis.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.dis.white <- anesrake(targets.race.dis, dis.white.2020, caseid=dis.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.dis.white <- as.data.frame(weights2017.race.dis.white[c("caseid", "weightvec")])
weights2018.race.dis.white <- as.data.frame(weights2018.race.dis.white[c("caseid", "weightvec")])
weights2019.race.dis.white <- as.data.frame(weights2019.race.dis.white[c("caseid", "weightvec")])
weights2020.race.dis.white <- as.data.frame(weights2020.race.dis.white[c("caseid", "weightvec")])

weights.all.race.dis.white <- rbind(weights2017.race.dis.white, weights2018.race.dis.white, weights2019.race.dis.white, weights2020.race.dis.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.dis.white) <- c("session_id", "weightvec.race")
weights.all.race.dis.white$session_id <- as.character(weights.all.race.dis.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.white <- timeseriesDis_NEW[timeseriesDis_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.white <- merge(timeseriesDis_NEW, weights.all.race.dis.white, by = "session_id")

## create the weights per year for black
weights2017.race.dis.black <- anesrake(targets.race.dis, dis.black.2017, caseid=dis.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.dis.black <- anesrake(targets.race.dis, dis.black.2018, caseid=dis.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.dis.black <- anesrake(targets.race.dis, dis.black.2019, caseid=dis.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.dis.black <- anesrake(targets.race.dis, dis.black.2020, caseid=dis.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.dis.black <- as.data.frame(weights2017.race.dis.black[c("caseid", "weightvec")])
weights2018.race.dis.black <- as.data.frame(weights2018.race.dis.black[c("caseid", "weightvec")])
weights2019.race.dis.black <- as.data.frame(weights2019.race.dis.black[c("caseid", "weightvec")])
weights2020.race.dis.black <- as.data.frame(weights2020.race.dis.black[c("caseid", "weightvec")])

weights.all.race.dis.black <- rbind(weights2017.race.dis.black, weights2018.race.dis.black, weights2019.race.dis.black, weights2020.race.dis.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.dis.black) <- c("session_id", "weightvec.race")
weights.all.race.dis.black$session_id <- as.character(weights.all.race.dis.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.black <- timeseriesDis_NEW[timeseriesDis_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.black <- merge(timeseriesDis_NEW, weights.all.race.dis.black, by = "session_id")

## create the weights per year for asian
weights2017.race.dis.asian <- anesrake(targets.race.dis, dis.asian.2017, caseid=dis.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.dis.asian <- anesrake(targets.race.dis, dis.asian.2018, caseid=dis.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.dis.asian <- anesrake(targets.race.dis, dis.asian.2019, caseid=dis.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.dis.asian <- anesrake(targets.race.dis, dis.asian.2020, caseid=dis.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.dis.asian <- as.data.frame(weights2017.race.dis.asian[c("caseid", "weightvec")])
weights2018.race.dis.asian <- as.data.frame(weights2018.race.dis.asian[c("caseid", "weightvec")])
weights2019.race.dis.asian <- as.data.frame(weights2019.race.dis.asian[c("caseid", "weightvec")])
weights2020.race.dis.asian <- as.data.frame(weights2020.race.dis.asian[c("caseid", "weightvec")])

weights.all.race.dis.asian <- rbind(weights2017.race.dis.asian, weights2018.race.dis.asian, weights2019.race.dis.asian, weights2020.race.dis.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.dis.asian) <- c("session_id", "weightvec.race")
weights.all.race.dis.asian$session_id <- as.character(weights.all.race.dis.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.asian <- timeseriesDis_NEW[timeseriesDis_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.asian <- merge(timeseriesDis_NEW, weights.all.race.dis.asian, by = "session_id")

dis.whblkasi <- rbind(dis.white, dis.black, dis.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.dis <- targets_disability[c("Wpol", "Wrace", "Wedu", "Wgender")]

timeseriesDis_NEW$age.grp <- cut(timeseriesDis_NEW$age.new, c(0,20,40,99))
levels(timeseriesDis_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
dis.young.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$age.grp == "young",]
dis.young.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$age.grp == "young",]
dis.young.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$age.grp == "young",]
dis.young.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$age.grp == "young",]
# repeat for other group
dis.middle.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$age.grp == "middle",] 
dis.middle.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$age.grp == "middle",]
dis.middle.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$age.grp == "middle",]
dis.middle.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$age.grp == "middle",]
# repeat for other group
dis.old.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$age.grp == "old",] 
dis.old.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$age.grp == "old",]
dis.old.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$age.grp == "old",]
dis.old.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.dis.young <- anesrake(targets.age.dis, dis.young.2017, caseid=dis.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.dis.young <- anesrake(targets.age.dis, dis.young.2018, caseid=dis.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.dis.young <- anesrake(targets.age.dis, dis.young.2019, caseid=dis.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.dis.young <- anesrake(targets.age.dis, dis.young.2020, caseid=dis.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.age.dis.young <- as.data.frame(weights2017.age.dis.young[c("caseid", "weightvec")])
weights2018.age.dis.young <- as.data.frame(weights2018.age.dis.young[c("caseid", "weightvec")])
weights2019.age.dis.young <- as.data.frame(weights2019.age.dis.young[c("caseid", "weightvec")])
weights2020.age.dis.young <- as.data.frame(weights2020.age.dis.young[c("caseid", "weightvec")])

weights.all.age.dis.young <- rbind(weights2017.age.dis.young, weights2018.age.dis.young, weights2019.age.dis.young, weights2020.age.dis.young)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.dis.young) <- c("session_id", "weightvec.age")
weights.all.age.dis.young$session_id <- as.character(weights.all.age.dis.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.young <- timeseriesDis_NEW[timeseriesDis_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.young <- merge(timeseriesDis_NEW, weights.all.age.dis.young, by = "session_id")

## create the weights per year for middle
weights2017.age.dis.middle <- anesrake(targets.age.dis, dis.middle.2017, caseid=dis.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.dis.middle <- anesrake(targets.age.dis, dis.middle.2018, caseid=dis.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.dis.middle <- anesrake(targets.age.dis, dis.middle.2019, caseid=dis.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.dis.middle <- anesrake(targets.age.dis, dis.middle.2020, caseid=dis.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.age.dis.middle <- as.data.frame(weights2017.age.dis.middle[c("caseid", "weightvec")])
weights2018.age.dis.middle <- as.data.frame(weights2018.age.dis.middle[c("caseid", "weightvec")])
weights2019.age.dis.middle <- as.data.frame(weights2019.age.dis.middle[c("caseid", "weightvec")])
weights2020.age.dis.middle <- as.data.frame(weights2020.age.dis.middle[c("caseid", "weightvec")])

weights.all.age.dis.middle <- rbind(weights2017.age.dis.middle, weights2018.age.dis.middle, weights2019.age.dis.middle, weights2020.age.dis.middle)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.dis.middle) <- c("session_id", "weightvec.age")
weights.all.age.dis.middle$session_id <- as.character(weights.all.age.dis.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.middle <- timeseriesDis_NEW[timeseriesDis_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
dis.middle <- merge(timeseriesDis_NEW, weights.all.age.dis.middle, by = "session_id")

## create the weights per year for old
weights2017.age.dis.old <- anesrake(targets.age.dis, dis.old.2017, caseid=dis.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.dis.old <- anesrake(targets.age.dis, dis.old.2018, caseid=dis.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.dis.old <- anesrake(targets.age.dis, dis.old.2019, caseid=dis.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.dis.old <- anesrake(targets.age.dis, dis.old.2020, caseid=dis.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.age.dis.old <- as.data.frame(weights2017.age.dis.old[c("caseid", "weightvec")])
weights2018.age.dis.old <- as.data.frame(weights2018.age.dis.old[c("caseid", "weightvec")])
weights2019.age.dis.old <- as.data.frame(weights2019.age.dis.old[c("caseid", "weightvec")])
weights2020.age.dis.old <- as.data.frame(weights2020.age.dis.old[c("caseid", "weightvec")])

weights.all.age.dis.old <- rbind(weights2017.age.dis.old, weights2018.age.dis.old, weights2019.age.dis.old, weights2020.age.dis.old)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.dis.old) <- c("session_id", "weightvec.age")
weights.all.age.dis.old$session_id <- as.character(weights.all.age.dis.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.old <- timeseriesDis_NEW[timeseriesDis_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
dis.old <- merge(timeseriesDis_NEW, weights.all.age.dis.old, by = "session_id")


dis.ygmidold <- rbind(dis.young, dis.middle, dis.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.dis <- targets_disability[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]

timeseriesDis_NEW$relisimple <- recode(timeseriesDis_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
dis.christ.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$relisimple == "christian",]
dis.christ.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$relisimple == "christian",]
dis.christ.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$relisimple == "christian",]
dis.christ.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$relisimple == "christian",]
# repeat for other group
dis.jew.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$relisimple == "jewish",] 
dis.jew.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$relisimple == "jewish",]
dis.jew.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$relisimple == "jewish",]
dis.jew.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$relisimple == "jewish",]
#** note: disability, jewish 2018 has no American Indian/Alaska Native observations, so add one to allow raking to proceed
adddisjew18 <- dis.jew.2018[sample(1:dim(dis.jew.2018)[1], 1),]
adddisjew18$Wrace <- 1
adddisjew18$racesimple <- "American Indian/Alaska Native"
dis.jew.2018 <- rbind(dis.jew.2018, adddisjew18)

# repeat for other group
dis.nonrel.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$relisimple == "nonrel",] 
dis.nonrel.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$relisimple == "nonrel",]
dis.nonrel.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$relisimple == "nonrel",]
dis.nonrel.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$relisimple == "nonrel",]
# repeat for other group
dis.other.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$relisimple == "other",] 
dis.other.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$relisimple == "other",]
dis.other.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$relisimple == "other",]
dis.other.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.dis.christ <- anesrake(targets.reli.dis, dis.christ.2017, caseid=dis.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.dis.christ <- anesrake(targets.reli.dis, dis.christ.2018, caseid=dis.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.dis.christ <- anesrake(targets.reli.dis, dis.christ.2019, caseid=dis.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.dis.christ <- anesrake(targets.reli.dis, dis.christ.2020, caseid=dis.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.dis.christ <- as.data.frame(weights2017.reli.dis.christ[c("caseid", "weightvec")])
weights2018.reli.dis.christ <- as.data.frame(weights2018.reli.dis.christ[c("caseid", "weightvec")])
weights2019.reli.dis.christ <- as.data.frame(weights2019.reli.dis.christ[c("caseid", "weightvec")])
weights2020.reli.dis.christ <- as.data.frame(weights2020.reli.dis.christ[c("caseid", "weightvec")])

weights.all.reli.dis.christ <- rbind(weights2017.reli.dis.christ, weights2018.reli.dis.christ, weights2019.reli.dis.christ, weights2020.reli.dis.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.dis.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.dis.christ$session_id <- as.character(weights.all.reli.dis.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.christ <- timeseriesDis_NEW[timeseriesDis_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
dis.christ <- merge(timeseriesDis_NEW, weights.all.reli.dis.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.dis.jew <- anesrake(targets.reli.dis, dis.jew.2017, caseid=dis.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.dis.jew <- anesrake(targets.reli.dis, dis.jew.2018, caseid=dis.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.dis.jew <- anesrake(targets.reli.dis, dis.jew.2019, caseid=dis.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.dis.jew <- anesrake(targets.reli.dis, dis.jew.2020, caseid=dis.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.dis.jew <- as.data.frame(weights2017.reli.dis.jew[c("caseid", "weightvec")])
weights2018.reli.dis.jew <- as.data.frame(weights2018.reli.dis.jew[c("caseid", "weightvec")])
weights2019.reli.dis.jew <- as.data.frame(weights2019.reli.dis.jew[c("caseid", "weightvec")])
weights2020.reli.dis.jew <- as.data.frame(weights2020.reli.dis.jew[c("caseid", "weightvec")])

weights.all.reli.dis.jew <- rbind(weights2017.reli.dis.jew, weights2018.reli.dis.jew, weights2019.reli.dis.jew, weights2020.reli.dis.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.dis.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.dis.jew$session_id <- as.character(weights.all.reli.dis.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.jew <- timeseriesDis_NEW[timeseriesDis_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
dis.jew <- merge(timeseriesDis_NEW, weights.all.reli.dis.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.dis.nonrel <- anesrake(targets.reli.dis, dis.nonrel.2017, caseid=dis.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.dis.nonrel <- anesrake(targets.reli.dis, dis.nonrel.2018, caseid=dis.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.dis.nonrel <- anesrake(targets.reli.dis, dis.nonrel.2019, caseid=dis.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.dis.nonrel <- anesrake(targets.reli.dis, dis.nonrel.2020, caseid=dis.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.dis.nonrel <- as.data.frame(weights2017.reli.dis.nonrel[c("caseid", "weightvec")])
weights2018.reli.dis.nonrel <- as.data.frame(weights2018.reli.dis.nonrel[c("caseid", "weightvec")])
weights2019.reli.dis.nonrel <- as.data.frame(weights2019.reli.dis.nonrel[c("caseid", "weightvec")])
weights2020.reli.dis.nonrel <- as.data.frame(weights2020.reli.dis.nonrel[c("caseid", "weightvec")])

weights.all.reli.dis.nonrel <- rbind(weights2017.reli.dis.nonrel, weights2018.reli.dis.nonrel, weights2019.reli.dis.nonrel, weights2020.reli.dis.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.dis.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.dis.nonrel$session_id <- as.character(weights.all.reli.dis.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.nonrel <- timeseriesDis_NEW[timeseriesDis_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
dis.nonrel <- merge(timeseriesDis_NEW, weights.all.reli.dis.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.dis.other <- anesrake(targets.reli.dis, dis.other.2017, caseid=dis.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.dis.other <- anesrake(targets.reli.dis, dis.other.2018, caseid=dis.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.dis.other <- anesrake(targets.reli.dis, dis.other.2019, caseid=dis.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.dis.other <- anesrake(targets.reli.dis, dis.other.2020, caseid=dis.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.dis.other <- as.data.frame(weights2017.reli.dis.other[c("caseid", "weightvec")])
weights2018.reli.dis.other <- as.data.frame(weights2018.reli.dis.other[c("caseid", "weightvec")])
weights2019.reli.dis.other <- as.data.frame(weights2019.reli.dis.other[c("caseid", "weightvec")])
weights2020.reli.dis.other <- as.data.frame(weights2020.reli.dis.other[c("caseid", "weightvec")])

weights.all.reli.dis.other <- rbind(weights2017.reli.dis.other, weights2018.reli.dis.other, weights2019.reli.dis.other, weights2020.reli.dis.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.dis.other) <- c("session_id", "weightvec.religion")
weights.all.reli.dis.other$session_id <- as.character(weights.all.reli.dis.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.other <- timeseriesDis_NEW[timeseriesDis_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.other <- merge(timeseriesDis_NEW, weights.all.reli.dis.other, by = "session_id")

dis.allreligion <- rbind(dis.christ, dis.jew, dis.nonrel, dis.other)


#### Weighting for ABILITY (Disabled, Abled) ----
## Set the new target weights by all other demographics besides that one of interest
targets.disability.dis <- targets_disability[c("Wgender", "Wrace", "Wedu", "Wage", "Wpol")]
targets.disability.dis

table(timeseriesDis_NEW$reporteddis)

## Get sub-data for each year and each political identity, to then equate to full population
dis.nondis.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$reporteddis == "No",]
dis.nondis.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$reporteddis == "No",]
dis.nondis.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$reporteddis == "No",]
dis.nondis.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$reporteddis == "No",]
# repeat for other group
dis.wdis.2017 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2017 & timeseriesDis_NEW$reporteddis == "Yes",] 
dis.wdis.2018 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2018 & timeseriesDis_NEW$reporteddis == "Yes",]
dis.wdis.2019 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2019 & timeseriesDis_NEW$reporteddis == "Yes",]
dis.wdis.2020 <- timeseriesDis_NEW[timeseriesDis_NEW$year==2020 & timeseriesDis_NEW$reporteddis == "Yes",]

## create the weights per year for nondisability
weights2017.disability.dis.nondis <- anesrake(targets.disability.dis, dis.nondis.2017, caseid=dis.nondis.2017$session_id,
                                    verbose=TRUE,type="Nolim",convcrit=0.005)
weights2018.disability.dis.nondis <- anesrake(targets.disability.dis, dis.nondis.2018, caseid=dis.nondis.2018$session_id,
                                    verbose=TRUE,type="Nolim",convcrit=0.005)
weights2019.disability.dis.nondis <- anesrake(targets.disability.dis, dis.nondis.2019, caseid=dis.nondis.2019$session_id,
                                    verbose=TRUE,type="Nolim",convcrit=0.005)
weights2020.disability.dis.nondis <- anesrake(targets.disability.dis, dis.nondis.2020, caseid=dis.nondis.2020$session_id,
                                    verbose=TRUE,type="Nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.disability.dis.nondis <- as.data.frame(weights2017.disability.dis.nondis[c("caseid", "weightvec")])
weights2018.disability.dis.nondis <- as.data.frame(weights2018.disability.dis.nondis[c("caseid", "weightvec")])
weights2019.disability.dis.nondis <- as.data.frame(weights2019.disability.dis.nondis[c("caseid", "weightvec")])
weights2020.disability.dis.nondis <- as.data.frame(weights2020.disability.dis.nondis[c("caseid", "weightvec")])

weights.all.disability.dis.nondis <- rbind(weights2017.disability.dis.nondis, weights2018.disability.dis.nondis, weights2019.disability.dis.nondis, weights2020.disability.dis.nondis)

# now create the vector of dscores and weights and month variables
colnames(weights.all.disability.dis.nondis) <- c("session_id", "weightvec.dis")
weights.all.disability.dis.nondis$session_id <- as.character(weights.all.disability.dis.nondis$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.nondis <- timeseriesDis_NEW[timeseriesDis_NEW$reporteddis == "No", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
dis.nondis <- merge(timeseriesDis_NEW, weights.all.disability.dis.nondis, by = "session_id")

## create the weights per year for wdisability
weights2017.disability.dis.wdis <- anesrake(targets.disability.dis, dis.wdis.2017, caseid=dis.wdis.2017$session_id,
                                     verbose=TRUE,type="Nolim",convcrit=0.005)
weights2018.disability.dis.wdis <- anesrake(targets.disability.dis, dis.wdis.2018, caseid=dis.wdis.2018$session_id,
                                     verbose=TRUE,type="Nolim",convcrit=0.005)
weights2019.disability.dis.wdis <- anesrake(targets.disability.dis, dis.wdis.2019, caseid=dis.wdis.2019$session_id,
                                     verbose=TRUE,type="Nolim",convcrit=0.005)
weights2020.disability.dis.wdis <- anesrake(targets.disability.dis, dis.wdis.2020, caseid=dis.wdis.2020$session_id,
                                     verbose=TRUE,type="Nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.disability.dis.wdis <- as.data.frame(weights2017.disability.dis.wdis[c("caseid", "weightvec")])
weights2018.disability.dis.wdis <- as.data.frame(weights2018.disability.dis.wdis[c("caseid", "weightvec")])
weights2019.disability.dis.wdis <- as.data.frame(weights2019.disability.dis.wdis[c("caseid", "weightvec")])
weights2020.disability.dis.wdis <- as.data.frame(weights2020.disability.dis.wdis[c("caseid", "weightvec")])

weights.all.disability.dis.wdis <- rbind(weights2017.disability.dis.wdis, weights2018.disability.dis.wdis, weights2019.disability.dis.wdis, weights2020.disability.dis.wdis)

## now create the vector of dscores and weights and month variables
colnames(weights.all.disability.dis.wdis) <- c("session_id", "weightvec.pol")
weights.all.disability.dis.wdis$session_id <- as.character(weights.all.disability.dis.wdis$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
dis.wdis <- timeseriesDis_NEW[timeseriesDis_NEW$reporteddis == "Yes", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
dis.wdis <- merge(timeseriesDis_NEW, weights.all.disability.dis.wdis, by = "session_id")

dis.disability <- rbind(dis.nondis, dis.wdis)


## WEIGHT ATTITUDES ----
# Weighting for POLITICS (Liberal, Conservative) ----
## Set the new target weights by all other demographics besides that one of interest
targets.pol.weight <- targets_weight[c("Wgender", "Wrace", "Wedu", "Wage")]
targets.pol.weight

## Get sub-data for each year and each political identity, to then equate to full population
weight.lib.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$Wpol>4,]
weight.lib.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$Wpol>4,]
weight.lib.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$Wpol>4,]
weight.lib.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$Wpol>4,]
# repeat for conservatives
weight.cons.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$Wpol<4,] 
weight.cons.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$Wpol<4,]
weight.cons.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$Wpol<4,]
weight.cons.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$Wpol<4,]

## create the weights per year for liberals
weights2017.pol.weight.lib <- anesrake(targets.pol.weight, weight.lib.2017, caseid=weight.lib.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.weight.lib <- anesrake(targets.pol.weight, weight.lib.2018, caseid=weight.lib.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.weight.lib <- anesrake(targets.pol.weight, weight.lib.2019, caseid=weight.lib.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.weight.lib <- anesrake(targets.pol.weight, weight.lib.2020, caseid=weight.lib.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.weight.lib <- as.data.frame(weights2017.pol.weight.lib[c("caseid", "weightvec")])
weights2018.pol.weight.lib <- as.data.frame(weights2018.pol.weight.lib[c("caseid", "weightvec")])
weights2019.pol.weight.lib <- as.data.frame(weights2019.pol.weight.lib[c("caseid", "weightvec")])
weights2020.pol.weight.lib <- as.data.frame(weights2020.pol.weight.lib[c("caseid", "weightvec")])

weights.all.pol.weight.lib <- rbind(weights2017.pol.weight.lib, weights2018.pol.weight.lib, weights2019.pol.weight.lib, weights2020.pol.weight.lib)

# now create the vector of dscores and weights and month variables
colnames(weights.all.pol.weight.lib) <- c("session_id", "weightvec.pol")
weights.all.pol.weight.lib$session_id <- as.character(weights.all.pol.weight.lib$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.liberals <- timeseriesWeight_NEW[timeseriesWeight_NEW$Wpol>4, 
                                    c("session_id", "date", "year", "month", "month.cent", 
                                      "dscore", "att.new", "attnumeric",
                                      "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                      "politicalid.new", "edu.new", "Order", "num.new",
                                      "STATE", "MSAName", "CountyNo", "rel.new",
                                      "weightvec")]
weight.liberals <- merge(timeseriesWeight_NEW, weights.all.pol.weight.lib, by = "session_id")

## create the weights per year for conservatives
weights2017.pol.weight.cons <- anesrake(targets.pol.weight, weight.cons.2017, caseid=weight.cons.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.pol.weight.cons <- anesrake(targets.pol.weight, weight.cons.2018, caseid=weight.cons.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.pol.weight.cons <- anesrake(targets.pol.weight, weight.cons.2019, caseid=weight.cons.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.pol.weight.cons <- anesrake(targets.pol.weight, weight.cons.2020, caseid=weight.cons.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.pol.weight.cons <- as.data.frame(weights2017.pol.weight.cons[c("caseid", "weightvec")])
weights2018.pol.weight.cons <- as.data.frame(weights2018.pol.weight.cons[c("caseid", "weightvec")])
weights2019.pol.weight.cons <- as.data.frame(weights2019.pol.weight.cons[c("caseid", "weightvec")])
weights2020.pol.weight.cons <- as.data.frame(weights2020.pol.weight.cons[c("caseid", "weightvec")])

weights.all.pol.weight.cons <- rbind(weights2017.pol.weight.cons, weights2018.pol.weight.cons, weights2019.pol.weight.cons, weights2020.pol.weight.cons)

## now create the vector of dscores and weights and month variables
colnames(weights.all.pol.weight.cons) <- c("session_id", "weightvec.pol")
weights.all.pol.weight.cons$session_id <- as.character(weights.all.pol.weight.cons$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.conserv <- timeseriesWeight_NEW[timeseriesWeight_NEW$Wpol<4, 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
weight.conserv <- merge(timeseriesWeight_NEW, weights.all.pol.weight.cons, by = "session_id")

weight.libcons <- rbind(weight.liberals, weight.conserv)

## Weighting for GENDER (Male, Female) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.gender.weight <- targets_weight[c("Wpol", "Wrace", "Wedu", "Wage")]

## Get sub-data for each year and each group, to then equate to full population
weight.fem.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$genderidentity_simp == "female",]
weight.fem.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$genderidentity_simp == "female",]
weight.fem.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$genderidentity_simp == "female",]
weight.fem.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$genderidentity_simp == "female",]
# repeat for other group
weight.mal.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$genderidentity_simp == "male",] 
weight.mal.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$genderidentity_simp == "male",]
weight.mal.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$genderidentity_simp == "male",]
weight.mal.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$genderidentity_simp == "male",]

## create the weights per year for females
weights2017.gender.weight.fem <- anesrake(targets.gender.weight, weight.fem.2017, caseid=weight.fem.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.weight.fem <- anesrake(targets.gender.weight, weight.fem.2018, caseid=weight.fem.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.weight.fem <- anesrake(targets.gender.weight, weight.fem.2019, caseid=weight.fem.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.weight.fem <- anesrake(targets.gender.weight, weight.fem.2020, caseid=weight.fem.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.weight.fem <- as.data.frame(weights2017.gender.weight.fem[c("caseid", "weightvec")])
weights2018.gender.weight.fem <- as.data.frame(weights2018.gender.weight.fem[c("caseid", "weightvec")])
weights2019.gender.weight.fem <- as.data.frame(weights2019.gender.weight.fem[c("caseid", "weightvec")])
weights2020.gender.weight.fem <- as.data.frame(weights2020.gender.weight.fem[c("caseid", "weightvec")])

weights.all.gender.weight.fem <- rbind(weights2017.gender.weight.fem, weights2018.gender.weight.fem, weights2019.gender.weight.fem, weights2020.gender.weight.fem)

# now create the vector of dscores and weights and month variables
colnames(weights.all.gender.weight.fem) <- c("session_id", "weightvec.gender")
weights.all.gender.weight.fem$session_id <- as.character(weights.all.gender.weight.fem$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.fem <- timeseriesWeight_NEW[timeseriesWeight_NEW$genderidentity_simp == "female", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
weight.fem <- merge(timeseriesWeight_NEW, weights.all.gender.weight.fem, by = "session_id")

## create the weights per year for males
weights2017.gender.weight.mal <- anesrake(targets.gender.weight, weight.mal.2017, caseid=weight.mal.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.gender.weight.mal <- anesrake(targets.gender.weight, weight.mal.2018, caseid=weight.mal.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.gender.weight.mal <- anesrake(targets.gender.weight, weight.mal.2019, caseid=weight.mal.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.gender.weight.mal <- anesrake(targets.gender.weight, weight.mal.2020, caseid=weight.mal.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.gender.weight.mal <- as.data.frame(weights2017.gender.weight.mal[c("caseid", "weightvec")])
weights2018.gender.weight.mal <- as.data.frame(weights2018.gender.weight.mal[c("caseid", "weightvec")])
weights2019.gender.weight.mal <- as.data.frame(weights2019.gender.weight.mal[c("caseid", "weightvec")])
weights2020.gender.weight.mal <- as.data.frame(weights2020.gender.weight.mal[c("caseid", "weightvec")])

weights.all.gender.weight.mal <- rbind(weights2017.gender.weight.mal, weights2018.gender.weight.mal, weights2019.gender.weight.mal, weights2020.gender.weight.mal)

## now create the vector of dscores and weights and month variables
colnames(weights.all.gender.weight.mal) <- c("session_id", "weightvec.gender")
weights.all.gender.weight.mal$session_id <- as.character(weights.all.gender.weight.mal$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.mal <- timeseriesWeight_NEW[timeseriesWeight_NEW$genderidentity_simp == "male", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
weight.mal <- merge(timeseriesWeight_NEW, weights.all.gender.weight.mal, by = "session_id")

weight.femmal <- rbind(weight.fem, weight.mal)


#### Weighting for EDUCATION (No college, College) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.edu.weight <- targets_weight[c("Wpol", "Wrace", "Wgender", "Wage")]

timeseriesWeight_NEW$edu.grp <- as.numeric(timeseriesWeight_NEW$edu.new) - 5
timeseriesWeight_NEW$edu.grp <- ifelse(timeseriesWeight_NEW$edu.grp < 0, "nocollege", "college")


## Get sub-data for each year and each group, to then equate to full population
weight.ncol.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$edu.grp == "nocollege",]
weight.ncol.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$edu.grp == "nocollege",]
weight.ncol.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$edu.grp == "nocollege",]
weight.ncol.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$edu.grp == "nocollege",]
# repeat for other group
weight.col.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$edu.grp == "college",] 
weight.col.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$edu.grp == "college",]
weight.col.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$edu.grp == "college",]
weight.col.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$edu.grp == "college",]

## create the weights per year for females
weights2017.edu.weight.ncol <- anesrake(targets.edu.weight, weight.ncol.2017, caseid=weight.ncol.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.weight.ncol <- anesrake(targets.edu.weight, weight.ncol.2018, caseid=weight.ncol.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.weight.ncol <- anesrake(targets.edu.weight, weight.ncol.2019, caseid=weight.ncol.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.weight.ncol <- anesrake(targets.edu.weight, weight.ncol.2020, caseid=weight.ncol.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.weight.ncol <- as.data.frame(weights2017.edu.weight.ncol[c("caseid", "weightvec")])
weights2018.edu.weight.ncol <- as.data.frame(weights2018.edu.weight.ncol[c("caseid", "weightvec")])
weights2019.edu.weight.ncol <- as.data.frame(weights2019.edu.weight.ncol[c("caseid", "weightvec")])
weights2020.edu.weight.ncol <- as.data.frame(weights2020.edu.weight.ncol[c("caseid", "weightvec")])

weights.all.edu.weight.ncol <- rbind(weights2017.edu.weight.ncol, weights2018.edu.weight.ncol, weights2019.edu.weight.ncol, weights2020.edu.weight.ncol)

# now create the vector of dscores and weights and month variables
colnames(weights.all.edu.weight.ncol) <- c("session_id", "weightvec.edu")
weights.all.edu.weight.ncol$session_id <- as.character(weights.all.edu.weight.ncol$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.ncol <- timeseriesWeight_NEW[timeseriesWeight_NEW$edu.grp == "nocollege", 
                                c("session_id", "date", "year", "month", "month.cent", 
                                  "dscore", "att.new", "attnumeric",
                                  "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                  "politicalid.new", "edu.new", "Order", "num.new",
                                  "STATE", "MSAName", "CountyNo", "rel.new",
                                  "weightvec")]
weight.ncol <- merge(timeseriesWeight_NEW, weights.all.edu.weight.ncol, by = "session_id")

## create the weights per year for males
weights2017.edu.weight.col <- anesrake(targets.edu.weight, weight.col.2017, caseid=weight.col.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.edu.weight.col <- anesrake(targets.edu.weight, weight.col.2018, caseid=weight.col.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.edu.weight.col <- anesrake(targets.edu.weight, weight.col.2019, caseid=weight.col.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.edu.weight.col <- anesrake(targets.edu.weight, weight.col.2020, caseid=weight.col.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.edu.weight.col <- as.data.frame(weights2017.edu.weight.col[c("caseid", "weightvec")])
weights2018.edu.weight.col <- as.data.frame(weights2018.edu.weight.col[c("caseid", "weightvec")])
weights2019.edu.weight.col <- as.data.frame(weights2019.edu.weight.col[c("caseid", "weightvec")])
weights2020.edu.weight.col <- as.data.frame(weights2020.edu.weight.col[c("caseid", "weightvec")])

weights.all.edu.weight.col <- rbind(weights2017.edu.weight.col, weights2018.edu.weight.col, weights2019.edu.weight.col, weights2020.edu.weight.col)

## now create the vector of dscores and weights and month variables
colnames(weights.all.edu.weight.col) <- c("session_id", "weightvec.edu")
weights.all.edu.weight.col$session_id <- as.character(weights.all.edu.weight.col$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.col <- timeseriesWeight_NEW[timeseriesWeight_NEW$edu.grp == "college", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
weight.col <- merge(timeseriesWeight_NEW, weights.all.edu.weight.col, by = "session_id")

weight.ncolcol <- rbind(weight.ncol, weight.col)


#### Weighting for RACE (White, Black, Asian) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.race.weight <- targets_weight[c("Wpol", "Wgender", "Wedu", "Wage")]

timeseriesWeight_NEW$racesimple <- recode(timeseriesWeight_NEW$race.new, "'White' = 'White';
                                        'Black or African American' = 'Black';
                                        'East Asian' = 'Asian'; 'South Asian' = 'Asian';
                                        else = 'Other'")

## Get sub-data for each year and each group, to then equate to full population
weight.white.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$racesimple == "White",]
weight.white.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$racesimple == "White",]
weight.white.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$racesimple == "White",]
weight.white.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$racesimple == "White",]
# repeat for other group
weight.black.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$racesimple == "Black",] 
weight.black.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$racesimple == "Black",]
weight.black.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$racesimple == "Black",]
weight.black.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$racesimple == "Black",]
# repeat for other group
weight.asian.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$racesimple == "Asian",] 
weight.asian.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$racesimple == "Asian",]
weight.asian.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$racesimple == "Asian",]
weight.asian.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$racesimple == "Asian",]


## create the weights per year for white
weights2017.race.weight.white <- anesrake(targets.race.weight, weight.white.2017, caseid=weight.white.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.weight.white <- anesrake(targets.race.weight, weight.white.2018, caseid=weight.white.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.weight.white <- anesrake(targets.race.weight, weight.white.2019, caseid=weight.white.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.weight.white <- anesrake(targets.race.weight, weight.white.2020, caseid=weight.white.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.weight.white <- as.data.frame(weights2017.race.weight.white[c("caseid", "weightvec")])
weights2018.race.weight.white <- as.data.frame(weights2018.race.weight.white[c("caseid", "weightvec")])
weights2019.race.weight.white <- as.data.frame(weights2019.race.weight.white[c("caseid", "weightvec")])
weights2020.race.weight.white <- as.data.frame(weights2020.race.weight.white[c("caseid", "weightvec")])

weights.all.race.weight.white <- rbind(weights2017.race.weight.white, weights2018.race.weight.white, weights2019.race.weight.white, weights2020.race.weight.white)

# now create the vector of dscores and weights and month variables
colnames(weights.all.race.weight.white) <- c("session_id", "weightvec.race")
weights.all.race.weight.white$session_id <- as.character(weights.all.race.weight.white$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.white <- timeseriesWeight_NEW[timeseriesWeight_NEW$racesimple == "White", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
weight.white <- merge(timeseriesWeight_NEW, weights.all.race.weight.white, by = "session_id")

## create the weights per year for black
weights2017.race.weight.black <- anesrake(targets.race.weight, weight.black.2017, caseid=weight.black.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.weight.black <- anesrake(targets.race.weight, weight.black.2018, caseid=weight.black.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.weight.black <- anesrake(targets.race.weight, weight.black.2019, caseid=weight.black.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.weight.black <- anesrake(targets.race.weight, weight.black.2020, caseid=weight.black.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.weight.black <- as.data.frame(weights2017.race.weight.black[c("caseid", "weightvec")])
weights2018.race.weight.black <- as.data.frame(weights2018.race.weight.black[c("caseid", "weightvec")])
weights2019.race.weight.black <- as.data.frame(weights2019.race.weight.black[c("caseid", "weightvec")])
weights2020.race.weight.black <- as.data.frame(weights2020.race.weight.black[c("caseid", "weightvec")])

weights.all.race.weight.black <- rbind(weights2017.race.weight.black, weights2018.race.weight.black, weights2019.race.weight.black, weights2020.race.weight.black)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.weight.black) <- c("session_id", "weightvec.race")
weights.all.race.weight.black$session_id <- as.character(weights.all.race.weight.black$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.black <- timeseriesWeight_NEW[timeseriesWeight_NEW$racesimple == "Black", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
weight.black <- merge(timeseriesWeight_NEW, weights.all.race.weight.black, by = "session_id")

## create the weights per year for asian
weights2017.race.weight.asian <- anesrake(targets.race.weight, weight.asian.2017, caseid=weight.asian.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.race.weight.asian <- anesrake(targets.race.weight, weight.asian.2018, caseid=weight.asian.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.race.weight.asian <- anesrake(targets.race.weight, weight.asian.2019, caseid=weight.asian.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.race.weight.asian <- anesrake(targets.race.weight, weight.asian.2020, caseid=weight.asian.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.race.weight.asian <- as.data.frame(weights2017.race.weight.asian[c("caseid", "weightvec")])
weights2018.race.weight.asian <- as.data.frame(weights2018.race.weight.asian[c("caseid", "weightvec")])
weights2019.race.weight.asian <- as.data.frame(weights2019.race.weight.asian[c("caseid", "weightvec")])
weights2020.race.weight.asian <- as.data.frame(weights2020.race.weight.asian[c("caseid", "weightvec")])

weights.all.race.weight.asian <- rbind(weights2017.race.weight.asian, weights2018.race.weight.asian, weights2019.race.weight.asian, weights2020.race.weight.asian)

## now create the vector of dscores and weights and month variables
colnames(weights.all.race.weight.asian) <- c("session_id", "weightvec.race")
weights.all.race.weight.asian$session_id <- as.character(weights.all.race.weight.asian$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.asian <- timeseriesWeight_NEW[timeseriesWeight_NEW$racesimple == "Asian", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
weight.asian <- merge(timeseriesWeight_NEW, weights.all.race.weight.asian, by = "session_id")

weight.whblkasi <- rbind(weight.white, weight.black, weight.asian)



#### Weighting for AGE (Young < 20; Middle 20-40; Old > 40) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.age.weight <- targets_weight[c("Wpol", "Wrace", "Wedu", "Wgender")]

timeseriesWeight_NEW$age.grp <- cut(timeseriesWeight_NEW$age.new, c(0,20,40,99))
levels(timeseriesWeight_NEW$age.grp) <- c("young", "middle", "old")

## Get sub-data for each year and each group, to then equate to full population
weight.young.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$age.grp == "young",]
weight.young.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$age.grp == "young",]
weight.young.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$age.grp == "young",]
weight.young.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$age.grp == "young",]
# repeat for other group
weight.middle.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$age.grp == "middle",] 
weight.middle.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$age.grp == "middle",]
weight.middle.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$age.grp == "middle",]
weight.middle.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$age.grp == "middle",]
# repeat for other group
weight.old.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$age.grp == "old",] 
weight.old.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$age.grp == "old",]
weight.old.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$age.grp == "old",]
weight.old.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$age.grp == "old",]

## create the weights per year for young
weights2017.age.weight.young <- anesrake(targets.age.weight, weight.young.2017, caseid=weight.young.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.weight.young <- anesrake(targets.age.weight, weight.young.2018, caseid=weight.young.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.weight.young <- anesrake(targets.age.weight, weight.young.2019, caseid=weight.young.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.weight.young <- anesrake(targets.age.weight, weight.young.2020, caseid=weight.young.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for weight attitudes, add in workaround due to NULL rownames)
weights2017.age.weight.young2 <- weights2017.age.weight.young[c("caseid", "weightvec")]
weights2017.age.weight.young2 <- as.data.frame(cbind(weights2017.age.weight.young2$caseid,
                                                  weights2017.age.weight.young2$weightvec))
weights2018.age.weight.young2 <- weights2018.age.weight.young[c("caseid", "weightvec")]
weights2018.age.weight.young2 <- as.data.frame(cbind(weights2018.age.weight.young2$caseid,
                                                  weights2018.age.weight.young2$weightvec))
weights2019.age.weight.young2 <- weights2019.age.weight.young[c("caseid", "weightvec")]
weights2019.age.weight.young2 <- as.data.frame(cbind(weights2019.age.weight.young2$caseid,
                                                  weights2019.age.weight.young2$weightvec))
weights2020.age.weight.young2 <- weights2020.age.weight.young[c("caseid", "weightvec")]
weights2020.age.weight.young2 <- as.data.frame(cbind(weights2020.age.weight.young2$caseid,
                                                  weights2020.age.weight.young2$weightvec))

weights.all.age.weight.young <- rbind(weights2017.age.weight.young2, weights2018.age.weight.young2,
                                   weights2019.age.weight.young2, weights2020.age.weight.young2)

# now create the vector of dscores and weights and month variables
colnames(weights.all.age.weight.young) <- c("session_id", "weightvec.age")
weights.all.age.weight.young$session_id <- as.character(weights.all.age.weight.young$session_id)


# now create the vector of dscores and weights and month variables
colnames(weights.all.age.weight.young) <- c("session_id", "weightvec.age")
weights.all.age.weight.young$session_id <- as.character(weights.all.age.weight.young$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.young <- timeseriesWeight_NEW[timeseriesWeight_NEW$age.grp == "young", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
weight.young <- merge(timeseriesWeight_NEW, weights.all.age.weight.young, by = "session_id")

## create the weights per year for middle
weights2017.age.weight.middle <- anesrake(targets.age.weight, weight.middle.2017, caseid=weight.middle.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.weight.middle <- anesrake(targets.age.weight, weight.middle.2018, caseid=weight.middle.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.weight.middle <- anesrake(targets.age.weight, weight.middle.2019, caseid=weight.middle.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.weight.middle <- anesrake(targets.age.weight, weight.middle.2020, caseid=weight.middle.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for weight attitudes, add in workaround due to NULL rownames)
weights2017.age.weight.middle2 <- weights2017.age.weight.middle[c("caseid", "weightvec")]
weights2017.age.weight.middle2 <- as.data.frame(cbind(weights2017.age.weight.middle2$caseid,
                                                     weights2017.age.weight.middle2$weightvec))
weights2018.age.weight.middle2 <- weights2018.age.weight.middle[c("caseid", "weightvec")]
weights2018.age.weight.middle2 <- as.data.frame(cbind(weights2018.age.weight.middle2$caseid,
                                                     weights2018.age.weight.middle2$weightvec))
weights2019.age.weight.middle2 <- weights2019.age.weight.middle[c("caseid", "weightvec")]
weights2019.age.weight.middle2 <- as.data.frame(cbind(weights2019.age.weight.middle2$caseid,
                                                     weights2019.age.weight.middle2$weightvec))
weights2020.age.weight.middle2 <- weights2020.age.weight.middle[c("caseid", "weightvec")]
weights2020.age.weight.middle2 <- as.data.frame(cbind(weights2020.age.weight.middle2$caseid,
                                                     weights2020.age.weight.middle2$weightvec))

weights.all.age.weight.middle <- rbind(weights2017.age.weight.middle2, weights2018.age.weight.middle2,
                                      weights2019.age.weight.middle2, weights2020.age.weight.middle2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.weight.middle) <- c("session_id", "weightvec.age")
weights.all.age.weight.middle$session_id <- as.character(weights.all.age.weight.middle$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.middle <- timeseriesWeight_NEW[timeseriesWeight_NEW$age.grp == "middle", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
weight.middle <- merge(timeseriesWeight_NEW, weights.all.age.weight.middle, by = "session_id")

## create the weights per year for old
weights2017.age.weight.old <- anesrake(targets.age.weight, weight.old.2017, caseid=weight.old.2017$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.age.weight.old <- anesrake(targets.age.weight, weight.old.2018, caseid=weight.old.2018$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.age.weight.old <- anesrake(targets.age.weight, weight.old.2019, caseid=weight.old.2019$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.age.weight.old <- anesrake(targets.age.weight, weight.old.2020, caseid=weight.old.2020$session_id,
                                     verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together (* for weight attitudes, add in workaround due to NULL rownames)
weights2017.age.weight.old2 <- weights2017.age.weight.old[c("caseid", "weightvec")]
weights2017.age.weight.old2 <- as.data.frame(cbind(weights2017.age.weight.old2$caseid,
                                                     weights2017.age.weight.old2$weightvec))
weights2018.age.weight.old2 <- weights2018.age.weight.old[c("caseid", "weightvec")]
weights2018.age.weight.old2 <- as.data.frame(cbind(weights2018.age.weight.old2$caseid,
                                                     weights2018.age.weight.old2$weightvec))
weights2019.age.weight.old2 <- weights2019.age.weight.old[c("caseid", "weightvec")]
weights2019.age.weight.old2 <- as.data.frame(cbind(weights2019.age.weight.old2$caseid,
                                                     weights2019.age.weight.old2$weightvec))
weights2020.age.weight.old2 <- weights2020.age.weight.old[c("caseid", "weightvec")]
weights2020.age.weight.old2 <- as.data.frame(cbind(weights2020.age.weight.old2$caseid,
                                                     weights2020.age.weight.old2$weightvec))

weights.all.age.weight.old <- rbind(weights2017.age.weight.old2, weights2018.age.weight.old2,
                                      weights2019.age.weight.old2, weights2020.age.weight.old2)

## now create the vector of dscores and weights and month variables
colnames(weights.all.age.weight.old) <- c("session_id", "weightvec.age")
weights.all.age.weight.old$session_id <- as.character(weights.all.age.weight.old$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.old <- timeseriesWeight_NEW[timeseriesWeight_NEW$age.grp == "old", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
weight.old <- merge(timeseriesWeight_NEW, weights.all.age.weight.old, by = "session_id")


weight.ygmidold <- rbind(weight.young, weight.middle, weight.old)


#### Weighting for RELIGION (Christian, Other, Non, Jewish) ----
## Set the target weights by all other demographics besides that one of interest
## Set the new target weights by all other demographics besides that one of interest
targets.reli.weight <- targets_weight[c("Wpol", "Wrace", "Wedu", "Wage", "Wgender")]

timeseriesWeight_NEW$relisimple <- recode(timeseriesWeight_NEW$rel.new, "'catorth' = 'christian';
                                        'prot' = 'christian'; 'jewish' = 'jewish';
                                        'non' = 'nonrel'; else = 'other'")


## Get sub-data for each year and each group, to then equate to full population
weight.christ.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$relisimple == "christian",]
weight.christ.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$relisimple == "christian",]
weight.christ.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$relisimple == "christian",]
weight.christ.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$relisimple == "christian",]
# repeat for other group
weight.jew.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$relisimple == "jewish",] 
weight.jew.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$relisimple == "jewish",]
weight.jew.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$relisimple == "jewish",]
weight.jew.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$relisimple == "jewish",]
# repeat for other group
weight.nonrel.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$relisimple == "nonrel",] 
weight.nonrel.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$relisimple == "nonrel",]
weight.nonrel.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$relisimple == "nonrel",]
weight.nonrel.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$relisimple == "nonrel",]
# repeat for other group
weight.other.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$relisimple == "other",] 
weight.other.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$relisimple == "other",]
weight.other.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$relisimple == "other",]
weight.other.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$relisimple == "other",]

## create the weights per year for christian
weights2017.reli.weight.christ <- anesrake(targets.reli.weight, weight.christ.2017, caseid=weight.christ.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.weight.christ <- anesrake(targets.reli.weight, weight.christ.2018, caseid=weight.christ.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.weight.christ <- anesrake(targets.reli.weight, weight.christ.2019, caseid=weight.christ.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.weight.christ <- anesrake(targets.reli.weight, weight.christ.2020, caseid=weight.christ.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.weight.christ <- as.data.frame(weights2017.reli.weight.christ[c("caseid", "weightvec")])
weights2018.reli.weight.christ <- as.data.frame(weights2018.reli.weight.christ[c("caseid", "weightvec")])
weights2019.reli.weight.christ <- as.data.frame(weights2019.reli.weight.christ[c("caseid", "weightvec")])
weights2020.reli.weight.christ <- as.data.frame(weights2020.reli.weight.christ[c("caseid", "weightvec")])

weights.all.reli.weight.christ <- rbind(weights2017.reli.weight.christ, weights2018.reli.weight.christ, weights2019.reli.weight.christ, weights2020.reli.weight.christ)

# now create the vector of dscores and weights and month variables
colnames(weights.all.reli.weight.christ) <- c("session_id", "weightvec.religion")
weights.all.reli.weight.christ$session_id <- as.character(weights.all.reli.weight.christ$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.christ <- timeseriesWeight_NEW[timeseriesWeight_NEW$relisimple == "christian", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
weight.christ <- merge(timeseriesWeight_NEW, weights.all.reli.weight.christ, by = "session_id")

## create the weights per year for jewish
weights2017.reli.weight.jew <- anesrake(targets.reli.weight, weight.jew.2017, caseid=weight.jew.2017$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.weight.jew <- anesrake(targets.reli.weight, weight.jew.2018, caseid=weight.jew.2018$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.weight.jew <- anesrake(targets.reli.weight, weight.jew.2019, caseid=weight.jew.2019$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.weight.jew <- anesrake(targets.reli.weight, weight.jew.2020, caseid=weight.jew.2020$session_id,
                                      verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.weight.jew <- as.data.frame(weights2017.reli.weight.jew[c("caseid", "weightvec")])
weights2018.reli.weight.jew <- as.data.frame(weights2018.reli.weight.jew[c("caseid", "weightvec")])
weights2019.reli.weight.jew <- as.data.frame(weights2019.reli.weight.jew[c("caseid", "weightvec")])
weights2020.reli.weight.jew <- as.data.frame(weights2020.reli.weight.jew[c("caseid", "weightvec")])

weights.all.reli.weight.jew <- rbind(weights2017.reli.weight.jew, weights2018.reli.weight.jew, weights2019.reli.weight.jew, weights2020.reli.weight.jew)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.weight.jew) <- c("session_id", "weightvec.religion")
weights.all.reli.weight.jew$session_id <- as.character(weights.all.reli.weight.jew$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.jew <- timeseriesWeight_NEW[timeseriesWeight_NEW$relisimple == "jewish", 
                               c("session_id", "date", "year", "month", "month.cent", 
                                 "dscore", "att.new", "attnumeric",
                                 "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                 "politicalid.new", "edu.new", "Order", "num.new",
                                 "STATE", "MSAName", "CountyNo", "rel.new",
                                 "weightvec")]
weight.jew <- merge(timeseriesWeight_NEW, weights.all.reli.weight.jew, by = "session_id")

## create the weights per year for nonrel
weights2017.reli.weight.nonrel <- anesrake(targets.reli.weight, weight.nonrel.2017, caseid=weight.nonrel.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.weight.nonrel <- anesrake(targets.reli.weight, weight.nonrel.2018, caseid=weight.nonrel.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.weight.nonrel <- anesrake(targets.reli.weight, weight.nonrel.2019, caseid=weight.nonrel.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.weight.nonrel <- anesrake(targets.reli.weight, weight.nonrel.2020, caseid=weight.nonrel.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.weight.nonrel <- as.data.frame(weights2017.reli.weight.nonrel[c("caseid", "weightvec")])
weights2018.reli.weight.nonrel <- as.data.frame(weights2018.reli.weight.nonrel[c("caseid", "weightvec")])
weights2019.reli.weight.nonrel <- as.data.frame(weights2019.reli.weight.nonrel[c("caseid", "weightvec")])
weights2020.reli.weight.nonrel <- as.data.frame(weights2020.reli.weight.nonrel[c("caseid", "weightvec")])

weights.all.reli.weight.nonrel <- rbind(weights2017.reli.weight.nonrel, weights2018.reli.weight.nonrel, weights2019.reli.weight.nonrel, weights2020.reli.weight.nonrel)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.weight.nonrel) <- c("session_id", "weightvec.religion")
weights.all.reli.weight.nonrel$session_id <- as.character(weights.all.reli.weight.nonrel$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.nonrel <- timeseriesWeight_NEW[timeseriesWeight_NEW$relisimple == "nonrel", 
                                  c("session_id", "date", "year", "month", "month.cent", 
                                    "dscore", "att.new", "attnumeric",
                                    "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                    "politicalid.new", "edu.new", "Order", "num.new",
                                    "STATE", "MSAName", "CountyNo", "rel.new",
                                    "weightvec")]
weight.nonrel <- merge(timeseriesWeight_NEW, weights.all.reli.weight.nonrel, by = "session_id")

## create the weights per year for other
weights2017.reli.weight.other <- anesrake(targets.reli.weight, weight.other.2017, caseid=weight.other.2017$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.reli.weight.other <- anesrake(targets.reli.weight, weight.other.2018, caseid=weight.other.2018$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.reli.weight.other <- anesrake(targets.reli.weight, weight.other.2019, caseid=weight.other.2019$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.reli.weight.other <- anesrake(targets.reli.weight, weight.other.2020, caseid=weight.other.2020$session_id,
                                        verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.reli.weight.other <- as.data.frame(weights2017.reli.weight.other[c("caseid", "weightvec")])
weights2018.reli.weight.other <- as.data.frame(weights2018.reli.weight.other[c("caseid", "weightvec")])
weights2019.reli.weight.other <- as.data.frame(weights2019.reli.weight.other[c("caseid", "weightvec")])
weights2020.reli.weight.other <- as.data.frame(weights2020.reli.weight.other[c("caseid", "weightvec")])

weights.all.reli.weight.other <- rbind(weights2017.reli.weight.other, weights2018.reli.weight.other, weights2019.reli.weight.other, weights2020.reli.weight.other)

## now create the vector of dscores and weights and month variables
colnames(weights.all.reli.weight.other) <- c("session_id", "weightvec.religion")
weights.all.reli.weight.other$session_id <- as.character(weights.all.reli.weight.other$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.other <- timeseriesWeight_NEW[timeseriesWeight_NEW$relisimple == "other", 
                                 c("session_id", "date", "year", "month", "month.cent", 
                                   "dscore", "att.new", "attnumeric",
                                   "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                   "politicalid.new", "edu.new", "Order", "num.new",
                                   "STATE", "MSAName", "CountyNo", "rel.new",
                                   "weightvec")]
weight.other <- merge(timeseriesWeight_NEW, weights.all.reli.weight.other, by = "session_id")

weight.allreligion <- rbind(weight.christ, weight.jew, weight.nonrel, weight.other)





#### Weighting for WEIGHT (Under, Average, Over) ----
## Set the new target weights by all other demographics besides that one of interest
targets.ownweight.weight <- targets_weight[c("Wpol", "Wrace", "Wedu", "Wgender", "Wage")]
targets.ownweight.weight

table(timeseriesWeight_NEW$reportedweight)
timeseriesWeight_NEW$weightsimp <- recode(timeseriesWeight_NEW$reportedweight, "'Very underweight' = 'under';
                                          'Moderately underweight' = 'under';
                                          'Slightly underweight' = 'under';
                                          'Neither underweight nor overweight' = 'average';
                                          'Slightly overweight' = 'over';
                                          'Moderately overweight' = 'over';
                                          'Very overweight' = 'over'")
table(timeseriesWeight_NEW$weightsimp)

## Get sub-data for each year and each group, to then equate to full population
weight.under.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$weightsimp == "under",]
weight.under.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$weightsimp == "under",]
weight.under.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$weightsimp == "under",]
weight.under.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$weightsimp == "under",]
# repeat for other group
weight.average.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$weightsimp == "average",] 
weight.average.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$weightsimp == "average",]
weight.average.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$weightsimp == "average",]
weight.average.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$weightsimp == "average",]
# repeat for other group
weight.over.2017 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2017 & timeseriesWeight_NEW$weightsimp == "over",] 
weight.over.2018 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2018 & timeseriesWeight_NEW$weightsimp == "over",]
weight.over.2019 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2019 & timeseriesWeight_NEW$weightsimp == "over",]
weight.over.2020 <- timeseriesWeight_NEW[timeseriesWeight_NEW$year==2020 & timeseriesWeight_NEW$weightsimp == "over",]

## create the weights per year for under
weights2017.ownweight.weight.under <- anesrake(targets.ownweight.weight, weight.under.2017, caseid=weight.under.2017$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.ownweight.weight.under <- anesrake(targets.ownweight.weight, weight.under.2018, caseid=weight.under.2018$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.ownweight.weight.under <- anesrake(targets.ownweight.weight, weight.under.2019, caseid=weight.under.2019$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.ownweight.weight.under <- anesrake(targets.ownweight.weight, weight.under.2020, caseid=weight.under.2020$session_id,
                                         verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.ownweight.weight.under <- as.data.frame(weights2017.ownweight.weight.under[c("caseid", "weightvec")])
weights2018.ownweight.weight.under <- as.data.frame(weights2018.ownweight.weight.under[c("caseid", "weightvec")])
weights2019.ownweight.weight.under <- as.data.frame(weights2019.ownweight.weight.under[c("caseid", "weightvec")])
weights2020.ownweight.weight.under <- as.data.frame(weights2020.ownweight.weight.under[c("caseid", "weightvec")])

weights.all.ownweight.weight.under <- rbind(weights2017.ownweight.weight.under, weights2018.ownweight.weight.under, weights2019.ownweight.weight.under, weights2020.ownweight.weight.under)

# now create the vector of dscores and weights and month variables
colnames(weights.all.ownweight.weight.under) <- c("session_id", "weightvec.ownweight")
weights.all.ownweight.weight.under$session_id <- as.character(weights.all.ownweight.weight.under$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.under <- timeseriesWeight_NEW[timeseriesWeight_NEW$weightsimp == "under", 
                                     c("session_id", "date", "year", "month", "month.cent", 
                                       "dscore", "att.new", "attnumeric",
                                       "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                       "politicalid.new", "edu.new", "Order", "num.new",
                                       "STATE", "MSAName", "CountyNo", "rel.new",
                                       "weightvec")]
weight.under <- merge(timeseriesWeight_NEW, weights.all.ownweight.weight.under, by = "session_id")

## create the weights per year for average
weights2017.ownweight.weight.average <- anesrake(targets.ownweight.weight, weight.average.2017, caseid=weight.average.2017$session_id,
                                          verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.ownweight.weight.average <- anesrake(targets.ownweight.weight, weight.average.2018, caseid=weight.average.2018$session_id,
                                          verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.ownweight.weight.average <- anesrake(targets.ownweight.weight, weight.average.2019, caseid=weight.average.2019$session_id,
                                          verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.ownweight.weight.average <- anesrake(targets.ownweight.weight, weight.average.2020, caseid=weight.average.2020$session_id,
                                          verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.ownweight.weight.average <- as.data.frame(weights2017.ownweight.weight.average[c("caseid", "weightvec")])
weights2018.ownweight.weight.average <- as.data.frame(weights2018.ownweight.weight.average[c("caseid", "weightvec")])
weights2019.ownweight.weight.average <- as.data.frame(weights2019.ownweight.weight.average[c("caseid", "weightvec")])
weights2020.ownweight.weight.average <- as.data.frame(weights2020.ownweight.weight.average[c("caseid", "weightvec")])

weights.all.ownweight.weight.average <- rbind(weights2017.ownweight.weight.average, weights2018.ownweight.weight.average, weights2019.ownweight.weight.average, weights2020.ownweight.weight.average)

## now create the vector of dscores and weights and month variables
colnames(weights.all.ownweight.weight.average) <- c("session_id", "weightvec.ownweight")
weights.all.ownweight.weight.average$session_id <- as.character(weights.all.ownweight.weight.average$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.average <- timeseriesWeight_NEW[timeseriesWeight_NEW$weightsimp == "average", 
                                      c("session_id", "date", "year", "month", "month.cent", 
                                        "dscore", "att.new", "attnumeric",
                                        "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                        "politicalid.new", "edu.new", "Order", "num.new",
                                        "STATE", "MSAName", "CountyNo", "rel.new",
                                        "weightvec")]
weight.average <- merge(timeseriesWeight_NEW, weights.all.ownweight.weight.average, by = "session_id")

## create the weights per year for over
weights2017.ownweight.weight.over <- anesrake(targets.ownweight.weight, weight.over.2017, caseid=weight.over.2017$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2018.ownweight.weight.over <- anesrake(targets.ownweight.weight, weight.over.2018, caseid=weight.over.2018$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2019.ownweight.weight.over <- anesrake(targets.ownweight.weight, weight.over.2019, caseid=weight.over.2019$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)
weights2020.ownweight.weight.over <- anesrake(targets.ownweight.weight, weight.over.2020, caseid=weight.over.2020$session_id,
                                       verbose=TRUE,type="nolim",convcrit=0.005)

## bind all the weight vectors together
weights2017.ownweight.weight.over <- as.data.frame(weights2017.ownweight.weight.over[c("caseid", "weightvec")])
weights2018.ownweight.weight.over <- as.data.frame(weights2018.ownweight.weight.over[c("caseid", "weightvec")])
weights2019.ownweight.weight.over <- as.data.frame(weights2019.ownweight.weight.over[c("caseid", "weightvec")])
weights2020.ownweight.weight.over <- as.data.frame(weights2020.ownweight.weight.over[c("caseid", "weightvec")])

weights.all.ownweight.weight.over <- rbind(weights2017.ownweight.weight.over, weights2018.ownweight.weight.over, weights2019.ownweight.weight.over, weights2020.ownweight.weight.over)

## now create the vector of dscores and weights and month variables
colnames(weights.all.ownweight.weight.over) <- c("session_id", "weightvec.ownweight")
weights.all.ownweight.weight.over$session_id <- as.character(weights.all.ownweight.weight.over$session_id)

# add on the additional variables that we will want to use again later for modeling/model comparisons
weight.over <- timeseriesWeight_NEW[timeseriesWeight_NEW$age.grp == "over", 
                                   c("session_id", "date", "year", "month", "month.cent", 
                                     "dscore", "att.new", "attnumeric",
                                     "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                     "politicalid.new", "edu.new", "Order", "num.new",
                                     "STATE", "MSAName", "CountyNo", "rel.new",
                                     "weightvec")]
weight.over <- merge(timeseriesWeight_NEW, weights.all.ownweight.weight.over, by = "session_id")


weight.allweights <- rbind(weight.under, weight.average, weight.over)




## **** SAVE OUT demographic-specific dataframes ----
save(list = c("race.femmal", "race.whblkasi", "race.ygmidold", 
              "race.allreligion", "race.ncolcol",  "race.libcons"),
     file = "race_demogsdataframes.Rdata")

save(list = c("skin.femmal", "skin.whblkasi", "skin.ygmidold", 
              "skin.allreligion", "skin.ncolcol",  "skin.libcons"),
     file = "skin_demogsdataframes.Rdata")

save(list = c("sex.femmal", "sex.whblkasi", "sex.ygmidold", 
              "sex.allreligion", "sex.ncolcol",  "sex.libcons",
              "sex.sexuality"),
     file = "sex_demogsdataframes.Rdata")

save(list = c("age.femmal", "age.whblkasi", "age.ygmidold", 
              "age.allreligion", "age.ncolcol",  "age.libcons"),
     file = "age_demogsdataframes.Rdata")

save(list = c("dis.femmal", "dis.whblkasi", "dis.ygmidold", 
              "dis.allreligion", "dis.ncolcol",  "dis.libcons",
              "dis.disability"),
     file = "dis_demogsdataframes.Rdata")

save(list = c("weight.femmal", "weight.whblkasi", "weight.ygmidold", 
              "weight.allreligion", "weight.ncolcol",  "weight.libcons",
              "weight.allweights"),
     file = "weight_demogsdataframes.Rdata")

save(list = c("race.femmal", "race.whblkasi", "race.ygmidold", 
              "race.allreligion", "race.ncolcol",  "race.libcons",
              "skin.femmal", "skin.whblkasi", "skin.ygmidold", 
              "skin.allreligion", "skin.ncolcol",  "skin.libcons",
              "sex.femmal", "sex.whblkasi", "sex.ygmidold", 
              "sex.allreligion", "sex.ncolcol",  "sex.libcons",
              "sex.sexuality",
              "age.femmal", "age.whblkasi", "age.ygmidold", 
              "age.allreligion", "age.ncolcol",  "age.libcons",
              "dis.femmal", "dis.whblkasi", "dis.ygmidold", 
              "dis.allreligion", "dis.ncolcol",  "dis.libcons",
              "weight.femmal", "weight.whblkasi", "weight.ygmidold", 
              "weight.allreligion", "weight.ncolcol",  "weight.libcons",
              "weight.allweights"),
     file = "ALL_demogdataframes.Rdata")
