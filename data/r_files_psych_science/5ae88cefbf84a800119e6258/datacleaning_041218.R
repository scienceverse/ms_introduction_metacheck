## Charlesworth & Banaji. Patterns of Implicit and Explicit Attitudes I. Long-term trends from 2007 to 2016.
## Script version date: April 11th, 2018

####################################################################################################################################

## This file demonstrates how we go from raw data (from secure zip code data provided via email by Project Implicit coordinator)
# to fully cleaned data

## It generates three clean files for each attitude:
# (1) One ending in .iat, which includes a variety of relevant variables, and is limited to those with complete IAT scores.
# (2) One ending in .full, which includes a variety of relevant variables, but is limited to those with complete IAT and explicit scores, 
# and only US participants
# (3) One ending in .full.demogs, which includes only a selection of variables (covariates), 
# and is limited to only those with complete data on implicit and explicit, as well as complete covariate data

## At the end of this file, we also center and make numeric all the demographic variables in the .full.demogs files.

# The full data object, with all .full.demogs files can be found in the full.demogs.RData data object

####################################################################################################################################
## Installing required packages
install.packages("foreign", dependencies = TRUE) # for importing SPSS data
install.packages("car", dependencies = TRUE) # for recoding functions
install.packages("chron", dependencies = TRUE) # for changing time information
install.packages("pastecs", dependencies = TRUE) # for basic descriptives

## Loading required packages
require(foreign)
require(car)
require(chron)
require(pastecs)

####################################################################################################################################
## CLEAN THE AGE DATA ---------------------------
# import the data from 2002 - 2016
age.iat <- read.spss("Age IAT.2002-2016.sav",
                     to.data.frame = TRUE) # 1,913,129

# get the date in the right format
age.iat$date <- as.chron(ISOdate(1582, 10, 14) + age.iat$date) 
age.iat$date <- as.Date(age.iat$date)

# get a month variable
age.iat$month <- format(age.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (subtract 13 months to make month.cent 1 equivalent to January, 2004)
table(age.iat$month)
age.iat$month.cent <- as.numeric(factor(age.iat$month)) - 13

table(age.iat$month)
table(age.iat$month.cent)

# keep only those with month.cent > 0 and < 157 (2004 - 2016)
age.iat <- age.iat[age.iat$month.cent > 0,]
age.iat <- age.iat[age.iat$month.cent < 157,]
table(age.iat$month)
## for the data from 2004 - 2015, the original total is 1,848,941 participants.

## how many US subjects? -----------
## recode country cit and country res
sort(table(age.iat$countrycit))
sort(table(age.iat$countryres))
sort(table(age.iat$countrycit_num))
sort(table(age.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(age.iat$countrycit)
age.iat$countrycit <- gsub("  ", "", age.iat$countrycit)
age.iat$countrycit <- gsub(" ", "", age.iat$countrycit)
age.iat$countrycit <- factor(age.iat$countrycit)
age.iat$countrycit1 <- recode(age.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(age.iat$countrycit1))
age.iat$countrycit1 <- recode(age.iat$countrycit1, "'1' = 'US'")
age.iat$countrycit2 <- recode(age.iat$countrycit_num, "'U.S.A. ' = 'US'")

age.iat$countrycit.new <- paste(age.iat$countrycit1, age.iat$countrycit2)
age.iat$countrycit.new <- gsub("NA  ", "", age.iat$countrycit.new)
age.iat$countrycit.new <- gsub(" NA", "", age.iat$countrycit.new)
age.iat$countrycit.new <- factor(age.iat$countrycit.new)
age.iat$countrycit.new <- recode(age.iat$countrycit.new, "'US US' = 'US'")
sort(table(age.iat$countrycit.new))

table(age.iat$countrycit.new, age.iat$month)["US",] # complete data after 0404

levels(age.iat$countryres)
age.iat$countryres <- gsub("  ", "", age.iat$countryres)
age.iat$countryres <- gsub(" ", "", age.iat$countryres)
age.iat$countryres <- factor(age.iat$countryres)
age.iat$countryres1 <- recode(age.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(age.iat$countryres1))
age.iat$countryres1 <- recode(age.iat$countryres1, "'1' = 'US'")
age.iat$countryres2 <- recode(age.iat$countryres_num, "'U.S.A. ' = 'US'")

age.iat$countryres.new <- paste(age.iat$countryres1, age.iat$countryres2)
age.iat$countryres.new <- gsub("NA  ", "", age.iat$countryres.new)
age.iat$countryres.new <- gsub(" NA", "", age.iat$countryres.new)
age.iat$countryres.new <- factor(age.iat$countryres.new)
age.iat$countryres.new <- recode(age.iat$countryres.new, "'US US' = 'US'")
sort(table(age.iat$countryres.new))

table(age.iat$countryres.new, age.iat$month)["US",] # complete data after 0404

# how many US?
table(age.iat$countryres.new)["US"] # 888,157

# how many complete US?
table(age.iat$countryres.new[age.iat$session_status=="C   "])["US"] # 672,639

# keep a backup here
age.iat2 <- age.iat

## continue with data from 2004 through 2016
# keep only complete subjects ------
age.iat <- age.iat[age.iat$session_status=="C   ",] # complete subjects = 896,701
# keep only subjects with IAT scores -------
age.iat <- age.iat[is.na(age.iat$D_biep.Young_Good_all)==FALSE,] # complete subjects for IAT data = 874,900

# keep only the relevant variables of interest for 2004 - 2015 data
age.iat <- age.iat[c("session_id", "date", "month", "month.cent", "year", "age", "birthyear",
                     "num", "num_002", "sex", "birthsex", "genderidentity",
                     "D_biep.Young_Good_all", "Order",
                     "told", "told_0to10", "tyoung", "tyoung_0to10",
                     "att", "att_5", "att_7", "att7",
                     "choosetobe", "feel", "hopetolive", "othersthink", "atoma", "ctoya", "matoo", "yatoa",
                     "countrycit", "countryres", "countrycit_num", "countryres_num", 
                     "edu", "edu_9", "edu_13", "edu_14",
                     "ethnic", "ethnicityomb",
                     "politicalid", "politicalid_7", "politicalid_6", 
                     "raceomb", "raceomb_002", "raceombmulti",
                     "religion", "religion2009", "religionid", "religion2014",
                     "postcodelong", "postcodenow")]


## dscore ----------
names(age.iat)[which(names(age.iat)=="D_biep.Young_Good_all")] <- "dscore"

## session id ---------
age.iat$session_id <- as.character(age.iat$session_id)

## age ----------
table(age.iat$age, age.iat$month) # not collected after mid-2016

# working with birthyear
table(age.iat$birthyear, age.iat$year) # this one picks up in 2016
table(age.iat$birthyear) # combines codes that are real values and coded values

age.iat$birthyear.new <- factor(age.iat$birthyear)
levels(age.iat$birthyear.new) # missing 1:2, 4:5, 73:74, 76:78, 80:82, 84:99

birthyears <- seq(2009, 1910, -1)
birthyears <- birthyears[-c(1:2, 4:5, 73:74, 76:78, 80:82, 84:99)]
birthyears

levels(age.iat$birthyear.new)[1:72] <- birthyears
table(age.iat$birthyear.new)

# birthyear was introduced in 2016, so subtract birthyear from 2016 to get age
# get in the right order
age.iat$birthyear.new2 <- factor(age.iat$birthyear.new, levels = c(1910:2009))
# save as character
age.iat$birthyear.new2 <- as.character(age.iat$birthyear.new2)
# then as numeric
age.iat$birthyear.new2 <- as.numeric(age.iat$birthyear.new2)
# subtract
age.iat$birthyear.new2 <- 2016 - age.iat$birthyear.new2

table(age.iat$birthyear.new2)

## now combine
age.iat$age.new <- paste(age.iat$age, age.iat$birthyear.new2)

table(age.iat$age.new)
age.iat$age.new <- gsub("NA ", "", age.iat$age.new)
age.iat$age.new <- gsub(" NA", "", age.iat$age.new)

age.iat$age.new <- factor(age.iat$age.new)

table(age.iat$age.new)

age.iat$age.new <- factor(age.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
age.iat$age.new <- droplevels(age.iat$age.new)

table(age.iat$age.new)

age.iat$age.new <- as.character(age.iat$age.new)
age.iat$age.new <- as.numeric(age.iat$age.new)

table(age.iat$age.new, age.iat$year)
table(age.iat$age.new[age.iat$age.new>25], age.iat$month[age.iat$age.new>25]) # complete data

## num --------
table(age.iat$num)
age.iat$num <- gsub("  ", "", age.iat$num)
age.iat$num <- factor(age.iat$num)
levels(age.iat$num)[levels(age.iat$num)=="." | levels(age.iat$num)== " " | levels(age.iat$num)== "nul" | levels(age.iat$num)== "-99"] <- NA
age.iat$num <- recode(age.iat$num, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")
table(age.iat$num, age.iat$year)


table(age.iat$num)
age.iat$num <- gsub("  ", "", age.iat$num)
age.iat$num <- factor(age.iat$num)
levels(age.iat$num)[levels(age.iat$num)=="." | levels(age.iat$num)== " " | levels(age.iat$num)== "nul" | levels(age.iat$num)== "-99"] <- NA
age.iat$num <- recode(age.iat$num, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")

table(age.iat$num_002, age.iat$month)
age.iat$num_002 <- factor(age.iat$num_002)
levels(age.iat$num_002)[levels(age.iat$num_002)=="-99"] <- NA
age.iat$num_002 <- recode(age.iat$num_002, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")

age.iat$num.new <- paste(age.iat$num, age.iat$num_002)

table(age.iat$num.new, age.iat$month)
age.iat$num.new <- gsub("NA ", "", age.iat$num.new)
age.iat$num.new <- gsub(" NA", "", age.iat$num.new)

age.iat$num.new <- factor(age.iat$num.new)
levels(age.iat$num.new)
age.iat$num.new <- recode(age.iat$num.new, "'1 0' = '0'; '2 1' = '1';
'3-5 3-5' = '3-5'; '3-5 2' = '2'; '3-5 6+' = '6+ ';'NA' = NA") ## recode and take second value (because this is the correct one from 2016 onwards)

table(age.iat$num.new)
table(age.iat$num.new, age.iat$month) # complete data


## sex -------------
## refactor sex (only female and male, no other)
table(age.iat$sex)
levels(age.iat$sex)[levels(age.iat$sex)=="." | levels(age.iat$sex)== " " | levels(age.iat$sex)== "n"] <- NA
age.iat$sex <- recode(age.iat$sex, "'f' = 'female'; 'm' = 'male'; 'F' = 'female'; 'M' = 'male';
                      'Female' = 'female'; 'Male' = 'male'")

table(age.iat$birthsex)
age.iat$birthsex <- factor(age.iat$birthsex, labels = c("male", "female"))

age.iat$sex.new <- paste(age.iat$sex, age.iat$birthsex)

table(age.iat$sex.new)
age.iat$sex.new <- gsub("NA ", "", age.iat$sex.new)
age.iat$sex.new <- gsub(" NA", "", age.iat$sex.new)

table(age.iat$sex.new)
age.iat$sex.new <- factor(age.iat$sex.new)
age.iat$sex.new <- recode(age.iat$sex.new, "'NA' = NA")

table(age.iat$sex.new, age.iat$month)

## race --------------
table(age.iat$raceomb, age.iat$month)[3:8,]
table(age.iat$raceomb_002, age.iat$month)[3:8,] # 1607 onwards
table(age.iat$raceombmulti, age.iat$month) # 1607 onwards

## raceomb (with factor levels determined from documentation at OSF, matched to individual year data sets)
race <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
          "Black or African American", "White", "More than one race - Black/White", "More than one race - Other", "Other or Unknown")
table(age.iat$raceomb)

age.iat$raceomb <- factor(age.iat$raceomb)
levels(age.iat$raceomb)[levels(age.iat$raceomb)== "-999"] <- NA
age.iat$raceomb1 <- factor(age.iat$raceomb, labels = race)

table(age.iat$raceomb1)

table(age.iat$raceombmulti) # if participant includes 5 and 6 = More than one race - B/W; all others = More than one race - Other
age.iat$raceomb2 <- factor(age.iat$raceombmulti)
levels(age.iat$raceomb2) <- trimws(levels(age.iat$raceomb2), which = "right")
age.iat$raceomb2 <- recode(age.iat$raceomb2, "'[5,6]' = 'More than one race - Black/White'; '[5]' = 'Black or African American'; 
                           '[6]' = 'White'; '[1]' = 'American Indian/Alaska Native'; '[2]' = 'East Asian';
                           '[3]' = 'South Asian'; '[4]' = 'Native Hawaiian or other Pacific Islander'; 
                           '[7]' = 'Other or Unknown'; '' = NA; '-999' = NA; '.' = NA; '[]' = NA; else = 'More than one race - Other'")
table(age.iat$raceomb2)

age.iat$raceomb.new <- paste(age.iat$raceomb1, age.iat$raceomb2)
table(age.iat$raceomb.new)
age.iat$raceomb.new <- gsub(" NA", "", age.iat$raceomb.new)
age.iat$raceomb.new <- factor(age.iat$raceomb.new)

levels(age.iat$raceomb.new)
age.iat$raceomb.new <- recode(age.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White';
                              'NA' = NA")
levels(age.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(age.iat$raceomb.new)), "Other or Unknown",
                                      levels(age.iat$raceomb.new))
levels(age.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(age.iat$raceomb.new)), "More than one race - Other",
                                      levels(age.iat$raceomb.new))
levels(age.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(age.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(age.iat$raceomb.new))

table(age.iat$raceomb.new, age.iat$year)
table(age.iat$raceomb.new, age.iat$month)[3:9,] # complete data after 2006.



table(age.iat$raceomb_002)
# drop the Multiracial level (included in the raceombmulti)
age.iat$raceomb3 <- factor(age.iat$raceomb_002, levels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                                           "Black or African American", "White", "Other or unknown"),
                           labels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                      "Black or African American", "White", "Other or Unknown"))
table(age.iat$raceomb3)

age.iat$raceomb.new <- paste(age.iat$raceomb.new, age.iat$raceomb3)
table(age.iat$raceomb.new)
age.iat$raceomb.new <- gsub(" NA", "", age.iat$raceomb.new)
age.iat$raceomb.new <- factor(age.iat$raceomb.new)
table(age.iat$raceomb.new)

age.iat$raceomb.new <- recode(age.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'Other or Unknown Other or Unknown'  = 'Other or Unknown';
                              'NA' = NA")
table(age.iat$raceomb.new, age.iat$month)[4:9,]
levels(age.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(age.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(age.iat$raceomb.new))
levels(age.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(age.iat$raceomb.new)), "More than one race - Other",
                                      levels(age.iat$raceomb.new))
levels(age.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(age.iat$raceomb.new)), "Other or Unknown",
                                      levels(age.iat$raceomb.new))


table(age.iat$raceomb.new, age.iat$month)


#  ethnic (to match raceomb codes)
table(age.iat$ethnic, age.iat$month) # only to 0609
age.iat$ethnic.new <- recode(age.iat$ethnic, "'White-Not of Hispanic Origin' = 'White'; 'Black-Not of Hispanic Origin' = 'Black or African American';
                             'Asian or Pacific Islander' = 'Asian'; 'Multi-racial (Black and White)' = 'More than one race - Black/White';
                             'Multi-racial (other)' = 'More than one race - Other'; 'North American Indian or First Nation' = 'American Indian/Alaska Native';
                             'Hispanic' = 'Other or Unknown'")

age.iat$race.new <- recode(age.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(age.iat$ethnic.new)
table(age.iat$race.new)

age.iat$ethnic.race.new <- paste(age.iat$ethnic.new, age.iat$race.new)
table(age.iat$ethnic.race.new)

age.iat$ethnic.race.new <- gsub("NA ", "", age.iat$ethnic.race.new)
age.iat$ethnic.race.new <- gsub(" NA", "", age.iat$ethnic.race.new)
age.iat$ethnic.race.new <- recode(age.iat$ethnic.race.new, "'NA' = NA")

age.iat$ethnic.race.new <- factor(age.iat$ethnic.race.new)
levels(age.iat$ethnic.race.new)
table(age.iat$ethnic.race.new, age.iat$year)
table(age.iat$ethnic.race.new, age.iat$month) # complete data from 2004 - 2016
table(age.iat$ethnic.race.new, age.iat$month)[3:7,]

## ethnicity ----------
## ethnicityomb (again, with values determined from documentation and single year codes)
table(age.iat$ethnicityomb)

age.iat$ethnicityomb <- factor(age.iat$ethnicityomb)
levels(age.iat$ethnicityomb)[levels(age.iat$ethnicityomb)== "-999"] <- NA

age.iat$ethnicityomb <- factor(age.iat$ethnicityomb, labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown"))
table(age.iat$ethnicityomb, age.iat$year)
table(age.iat$ethnicityomb, age.iat$month) # complete data after 0609


## explicit attitude -----------
table(age.iat$att) # falsely combines 5 and 7.
table(age.iat$att_5)
table(age.iat$att_7)
table(age.iat$att7)

# add two levels to att 5
levels(age.iat$att_5) <- c(levels(age.iat$att_5), "I slightly prefer old people to young people.",
                           "I slightly prefer young people to old people.")

# get the right order
age.iat$att_51 <- factor(age.iat$att_5, levels = c("I strongly prefer old people to young people.",
                                                   "I moderately prefer old people to young people.",
                                                   "I slightly prefer old people to young people.",
                                                   "I like young people and old people equally",
                                                   "I slightly prefer young people to old people.",
                                                   "I moderately prefer young people to old people.",
                                                   "I strongly prefer young people to old people."))
# get the right order
age.iat$att_71 <- factor(age.iat$att_7, levels = c("I strongly prefer old people to young people.",
                                                   "I moderately prefer old people to young people.",
                                                   "I slightly prefer old people to young people.",
                                                   "I like young people and old people equally",
                                                   "I slightly prefer young people to old people.",
                                                   "I moderately prefer young people to old people.",
                                                   "I strongly prefer young people to old people."))


age.iat$att.new <- paste(age.iat$att_51, age.iat$att_71)
table(age.iat$att.new)

age.iat$att.new <- gsub("NA ", "", age.iat$att.new)
age.iat$att.new <- gsub(" NA", "", age.iat$att.new)
age.iat$att.new <- recode(age.iat$att.new, "'NA' = NA")
table(age.iat$att.new)

age.iat$att.new <- factor(age.iat$att.new, levels = c("I strongly prefer old people to young people.",
                                                      "I moderately prefer old people to young people.",
                                                      "I slightly prefer old people to young people.",
                                                      "I like young people and old people equally",
                                                      "I slightly prefer young people to old people.",
                                                      "I moderately prefer young people to old people.",
                                                      "I strongly prefer young people to old people."))
table(age.iat$att.new)

# one more attitude variable to add on...
age.iat$att.new <- paste(age.iat$att.new, age.iat$att7)
table(age.iat$att.new)

age.iat$att.new <- gsub("NA ", "", age.iat$att.new)
age.iat$att.new <- gsub(" NA", "", age.iat$att.new)
age.iat$att.new <- recode(age.iat$att.new, "'NA' = NA")

age.iat$att.new <- factor(age.iat$att.new, levels = c("I strongly prefer old people to young people.",
                                                        "I moderately prefer old people to young people.",
                                                        "I slightly prefer old people to young people.",
                                                        "I like young people and old people equally",
                                                        "I slightly prefer young people to old people.",
                                                        "I moderately prefer young people to old people.",
                                                        "I strongly prefer young people to old people."))
table(age.iat$att.new, age.iat$month) # complete data now.



## education --------
table(age.iat$edu)
table(age.iat$edu_13)
table(age.iat$edu_14)

# make edu 13 a factor and add labels
age.iat$edu_13 <- factor(age.iat$edu_13)
levels(age.iat$edu_13)[levels(age.iat$edu_13)== "0"] <- NA
age.iat$edu_131 <- factor(age.iat$edu_13, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                     "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                                     "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
# add the missing level (MBA)
levels(age.iat$edu_131) <- c(levels(age.iat$edu_131), "M.B.A.")
table(age.iat$edu_131)

# get the right order for edu 14
table(age.iat$edu_14)
levels(age.iat$edu_14)[levels(age.iat$edu_14)== "-999"] <- NA
age.iat$edu_141 <- factor(age.iat$edu_14, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                     "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                                     "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
age.iat$edu.new <- paste(age.iat$edu_131, age.iat$edu_141)
age.iat$edu.new <- gsub("NA ", "", age.iat$edu.new)
age.iat$edu.new <- gsub(" NA", "", age.iat$edu.new)
age.iat$edu.new <- recode(age.iat$edu.new, "'NA' = NA")

# get the right order
age.iat$edu.new <- factor(age.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                      "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                      "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
table(age.iat$edu.new, age.iat$year)
table(age.iat$edu.new, age.iat$month) # complete data after 2004

## political ideology --------
table(age.iat$politicalid) # falsely combines pol 6 and pol 7
table(age.iat$politicalid_6)
table(age.iat$politicalid_7)

## remove the extraneous levels from the variables
age.iat$politicalid_6 <- factor(age.iat$politicalid_6)
levels(age.iat$politicalid_6)[levels(age.iat$politicalid_6)== "0"] <- NA

# get the right labels for pol 6
age.iat$politicalid_61 <- factor(age.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                   "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(age.iat$politicalid_61) <- c(levels(age.iat$politicalid_61), "Neutral")

# get the right order
age.iat$politicalid_61 <- factor(age.iat$politicalid_61, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                    "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(age.iat$politicalid_61, age.iat$year)

# add labels for pol 7
age.iat$politicalid_71 <- factor(age.iat$politicalid_7)
levels(age.iat$politicalid_71)[levels(age.iat$politicalid_71)== "-999"] <- NA
table(age.iat$politicalid_71)
age.iat$politicalid_71 <- factor(age.iat$politicalid_71, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
  "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# combine the raw scales.
age.iat$pol.new <- paste(age.iat$politicalid_61, age.iat$politicalid_71)
table(age.iat$pol.new)
age.iat$pol.new <- gsub("NA ", "", age.iat$pol.new)
age.iat$pol.new <- gsub(" NA", "", age.iat$pol.new)
age.iat$pol.new <- recode(age.iat$pol.new, "'NA' = NA")

table(age.iat$pol.new)
# get the right order
age.iat$pol.new <- factor(age.iat$pol.new, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                      "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(age.iat$pol.new, age.iat$year) # complete data now after 2004
age.iat$politicalid <- age.iat$pol.new

table(age.iat$politicalid, age.iat$month) # complete data here too..

## country of citizenship and residence -----------
## recode country cit and country res
sort(table(age.iat$countrycit))
sort(table(age.iat$countryres))
sort(table(age.iat$countrycit_num))
sort(table(age.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(age.iat$countrycit)
age.iat$countrycit <- gsub("  ", "", age.iat$countrycit)
age.iat$countrycit <- gsub(" ", "", age.iat$countrycit)
age.iat$countrycit <- factor(age.iat$countrycit)
age.iat$countrycit1 <- recode(age.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(age.iat$countrycit1))
age.iat$countrycit1 <- recode(age.iat$countrycit1, "'1' = 'US'")
age.iat$countrycit2 <- recode(age.iat$countrycit_num, "'U.S.A. ' = 'US'")

age.iat$countrycit.new <- paste(age.iat$countrycit1, age.iat$countrycit2)
age.iat$countrycit.new <- gsub("NA  ", "", age.iat$countrycit.new)
age.iat$countrycit.new <- gsub(" NA", "", age.iat$countrycit.new)
age.iat$countrycit.new <- factor(age.iat$countrycit.new)
age.iat$countrycit.new <- recode(age.iat$countrycit.new, "'US US' = 'US'")
sort(table(age.iat$countrycit.new))

table(age.iat$countrycit.new, age.iat$month)["US",] # complete data after 0404

levels(age.iat$countryres)
age.iat$countryres <- gsub("  ", "", age.iat$countryres)
age.iat$countryres <- gsub(" ", "", age.iat$countryres)
age.iat$countryres <- factor(age.iat$countryres)
age.iat$countryres1 <- recode(age.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(age.iat$countryres1))
age.iat$countryres1 <- recode(age.iat$countryres1, "'1' = 'US'")
age.iat$countryres2 <- recode(age.iat$countryres_num, "'U.S.A. ' = 'US'")

age.iat$countryres.new <- paste(age.iat$countryres1, age.iat$countryres2)
age.iat$countryres.new <- gsub("NA  ", "", age.iat$countryres.new)
age.iat$countryres.new <- gsub(" NA", "", age.iat$countryres.new)
age.iat$countryres.new <- factor(age.iat$countryres.new)
age.iat$countryres.new <- recode(age.iat$countryres.new, "'US US' = 'US'")
sort(table(age.iat$countryres.new))

table(age.iat$countryres.new, age.iat$month)["US",] # complete data after 0404



## zipcodes ----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
table(age.iat$postcodelong)
table(age.iat$postcodenow)

# make it a character to use regular expressions
age.iat$zip <- as.character(age.iat$postcodelong)

age.iat$zip <- gsub("[[:punct:]]", "", age.iat$zip) # remove punctuation
age.iat$zip <- gsub("[[:space:]]", "", age.iat$zip) # remove all white space
age.iat$zip <- gsub("[^0-9]", "", age.iat$zip) # remove all non-numeric characters

age.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", age.iat$zip) # remove all strings shorter than 5
age.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", age.iat$zip1) # remove all strings longer than 5, shorter than 9
age.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", age.iat$zip1) # remove all strings longer 9

age.iat$zip1 <- as.character(age.iat$zip1)

table(age.iat$zip1[age.iat$zip1>02112], age.iat$month[age.iat$zip1>02112]) # data only available after 0404

# add on postcode and geographic data (will take a while)
# returns NA for ppts. without zipcodes
age.iat.places <- merge(age.iat, zips, by = "zip1", all = TRUE)



## data subsetting -------------------------------------------------------------------------------------------------------
# subset to US participants (by countryres.new), allows for retaining the most participants over time
age.iatUSres <- age.iat.places[age.iat.places$countryres.new=="US",]
sort(table(age.iatUSres$countryres.new))
## total with US country residence = 657,550


## now keep only those with complete data for the following:
# dscore, att.new, sex.new
# race, age, Order, ethnic.race.new, politicalid, edu
age.full <- age.iatUSres[is.na(age.iatUSres$dscore)==FALSE,] # 657,550
age.full <- age.full[is.na(age.full$att.new)==FALSE,] #  629,045

age.full.demogs <- age.full[is.na(age.full$sex.new)==FALSE,] # 625,022
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$age.new)==FALSE,] # 620,785
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$ethnic.race.new)==FALSE,] # 612,328
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$politicalid)==FALSE,] # 592,585
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$edu.new)==FALSE,] # 588,230

# also include the following variables: num, and geographic information
age.full.demogs <- age.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                       "age.new", "sex.new", "ethnic.race.new", "raceomb.new", "ethnicityomb",
                                     "politicalid", "edu.new", "countryres.new", "countrycit.new",
                                       "Order", "num.new", 
                                       "zip1", "State", "Place.Name", "State.Abbreviation", "County")]

table(age.full.demogs$month.cent)
table(age.full.demogs$month)



####################################################################################################################################
## CLEAN THE DISABILITY DATA ---------------------------
# import the data from 2002 - 2016
dis.iat <- read.spss("Disability IAT.2004-2016.sav", 
                     to.data.frame = TRUE) # 611,335

# get the date in the right format
dis.iat$date <- as.chron(ISOdate(1582, 10, 14) + dis.iat$date) 
dis.iat$date <- as.Date(dis.iat$date)

# get a month variable
dis.iat$month <- format(dis.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (add 3 months to make month.cent 1 equivalent to January, 2004)
table(dis.iat$month)
dis.iat$month.cent <- as.numeric(factor(dis.iat$month)) + 3

table(dis.iat$month)
table(dis.iat$month.cent)

# keep only those with month.cent != 4, which only has one observation (and < 157)
dis.iat <- dis.iat[dis.iat$month.cent != 4,]
dis.iat <- dis.iat[dis.iat$month.cent < 157,]

## for the data from 2004 - 2016, the original total is 611,303 participants.

## how many US? -----------
## recode country cit and country res
sort(table(dis.iat$countrycit))
sort(table(dis.iat$countryres))
sort(table(dis.iat$countrycit_num))
sort(table(dis.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(dis.iat$countrycit)
dis.iat$countrycit <- gsub("  ", "", dis.iat$countrycit)
dis.iat$countrycit <- gsub(" ", "", dis.iat$countrycit)
dis.iat$countrycit <- factor(dis.iat$countrycit)
dis.iat$countrycit1 <- recode(dis.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(dis.iat$countrycit1))
dis.iat$countrycit1 <- recode(dis.iat$countrycit1, "'1' = 'US'")
dis.iat$countrycit2 <- recode(dis.iat$countrycit_num, "'U.S.A. ' = 'US'")

dis.iat$countrycit.new <- paste(dis.iat$countrycit1, dis.iat$countrycit2)
dis.iat$countrycit.new <- gsub("NA  ", "", dis.iat$countrycit.new)
dis.iat$countrycit.new <- gsub(" NA", "", dis.iat$countrycit.new)
dis.iat$countrycit.new <- factor(dis.iat$countrycit.new)
dis.iat$countrycit.new <- recode(dis.iat$countrycit.new, "'US US' = 'US'")
sort(table(dis.iat$countrycit.new))

table(dis.iat$countrycit.new, dis.iat$month)["US",] # complete data after 0404

levels(dis.iat$countryres)
dis.iat$countryres <- gsub("  ", "", dis.iat$countryres)
dis.iat$countryres <- gsub(" ", "", dis.iat$countryres)
dis.iat$countryres <- factor(dis.iat$countryres)
dis.iat$countryres1 <- recode(dis.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(dis.iat$countryres1))
dis.iat$countryres1 <- recode(dis.iat$countryres1, "'1' = 'US'")
dis.iat$countryres2 <- recode(dis.iat$countryres_num, "'U.S.A. ' = 'US'")

dis.iat$countryres.new <- paste(dis.iat$countryres1, dis.iat$countryres2)
dis.iat$countryres.new <- gsub("NA  ", "", dis.iat$countryres.new)
dis.iat$countryres.new <- gsub(" NA", "", dis.iat$countryres.new)
dis.iat$countryres.new <- factor(dis.iat$countryres.new)
dis.iat$countryres.new <- recode(dis.iat$countryres.new, "'US US' = 'US'")
sort(table(dis.iat$countryres.new))

table(dis.iat$countryres.new, dis.iat$month)["US",]

# how many US?
table(dis.iat$countryres.new)["US"] # 295,826

# how many complete US?
table(dis.iat$countryres.new[dis.iat$session_status=="C   "])["US"] # 214,647

# keep a backup here
dis.iat2 <- dis.iat

# keep only complete subjects ------
dis.iat <- dis.iat[dis.iat$session_status=="C   ",] # complete subjects = 288,802
# keep only subjects with IAT scores -------
dis.iat <- dis.iat[is.na(dis.iat$D_biep.Abled_Good_all)==FALSE,] # complete subjects for IAT data = 282,563

# keep only the relevant variables of interest for 2004 - 2015 data
dis.iat <- dis.iat[c("session_id", "date", "month", "month.cent", "year", "birthyear", "age",
                     "num", "num_002", "sex", "birthsex", "genderidentity", 
                     "disabled", "disabled_001", "distype", "distypes",
                     "D_biep.Abled_Good_all", "Order",
                     "att", "att_5", "att_7",
                     "countrycit", "countryres","countrycit_num", "countryres_num", 
                     "edu", "edu_13", "edu_14",
                     "ethnic", "ethnicityomb","raceomb", "raceomb_002", "raceombmulti",
                     "religion", "religion2009", "religionid", "religion2014",
                     "politicalid", "politicalid_7", "politicalid_6", 
                     "postcodelong", "postcodenow")]



## dscore ----------
names(dis.iat)[which(names(dis.iat)=="D_biep.Abled_Good_all")] <- "dscore"

## session id ---------
dis.iat$session_id <- as.character(dis.iat$session_id)

## age ----------
table(dis.iat$age, dis.iat$month) # not collected after mid-2016

# working with birthyear
table(dis.iat$birthyear, dis.iat$year) # this one picks up in 2016
table(dis.iat$birthyear) # combines codes that are real values and coded values

dis.iat$birthyear.new <- factor(dis.iat$birthyear)
levels(dis.iat$birthyear.new) # missing 1, 3:6, 63, 67:99

birthyears <- seq(2009, 1910, -1)
birthyears <- birthyears[-c(1, 3:6, 63, 67:99)]
birthyears

levels(dis.iat$birthyear.new)[1:61] <- birthyears
table(dis.iat$birthyear.new)

# birthyear was introduced in 2016, so subtract birthyear from 2016 to get age
# get in the right order
dis.iat$birthyear.new2 <- factor(dis.iat$birthyear.new, levels = c(1910:2009))
# save as character
dis.iat$birthyear.new2 <- as.character(dis.iat$birthyear.new2)
# then as numeric
dis.iat$birthyear.new2 <- as.numeric(dis.iat$birthyear.new2)
# subtract
dis.iat$birthyear.new2 <- 2016 - dis.iat$birthyear.new2

table(dis.iat$birthyear.new2)

## now combine
dis.iat$age.new <- paste(dis.iat$age, dis.iat$birthyear.new2)

table(dis.iat$age.new)
dis.iat$age.new <- gsub("NA ", "", dis.iat$age.new)
dis.iat$age.new <- gsub(" NA", "", dis.iat$age.new)

dis.iat$age.new <- factor(dis.iat$age.new)

table(dis.iat$age.new)

dis.iat$age.new <- factor(dis.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
dis.iat$age.new <- droplevels(dis.iat$age.new)

table(dis.iat$age.new)

dis.iat$age.new <- as.character(dis.iat$age.new)
dis.iat$age.new <- as.numeric(dis.iat$age.new)

table(dis.iat$age.new, dis.iat$year)
table(dis.iat$age.new[dis.iat$age.new>25], dis.iat$month[dis.iat$age.new>25]) # complete data

## num --------
table(dis.iat$num)
dis.iat$num <- gsub("  ", "", dis.iat$num)
dis.iat$num <- factor(dis.iat$num)
levels(dis.iat$num)[levels(dis.iat$num)=="." | levels(dis.iat$num)== " " | levels(dis.iat$num)== "nul" | levels(dis.iat$num)== "-99"] <- NA
dis.iat$num <- recode(dis.iat$num, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")
table(dis.iat$num, dis.iat$year)

table(dis.iat$num_002, dis.iat$month)
dis.iat$num_002 <- factor(dis.iat$num_002)
levels(dis.iat$num_002)[levels(dis.iat$num_002)=="-99"] <- NA
dis.iat$num_002 <- recode(dis.iat$num_002, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")

dis.iat$num.new <- paste(dis.iat$num, dis.iat$num_002)

table(dis.iat$num.new, dis.iat$month)
dis.iat$num.new <- gsub("NA ", "", dis.iat$num.new)
dis.iat$num.new <- gsub(" NA", "", dis.iat$num.new)

dis.iat$num.new <- factor(dis.iat$num.new)
levels(dis.iat$num.new)
dis.iat$num.new <- recode(dis.iat$num.new, "'1 0' = '0'; '2 1' = '1';
                          '3-5 3-5' = '3-5'; '3-5 2' = '2'; '3-5 6+' = '6+ ';'NA' = NA") ## recode and take second value (because this is the correct one from 2016 onwards)

table(dis.iat$num.new)
table(dis.iat$num.new, dis.iat$month) # complete data


## sex -------------
## refactor sex (only female and male, no other)
table(dis.iat$sex)
levels(dis.iat$sex)[levels(dis.iat$sex)=="." | levels(dis.iat$sex)== "1" | levels(dis.iat$sex)== "n"] <- NA
dis.iat$sex <- recode(dis.iat$sex, "'f' = 'female'; 'm' = 'male'; 'F' = 'female'; 'M' = 'male';
                      'Female' = 'female'; 'Male' = 'male'")

table(dis.iat$birthsex)
dis.iat$birthsex <- factor(dis.iat$birthsex, labels = c("male", "female"))

dis.iat$sex.new <- paste(dis.iat$sex, dis.iat$birthsex)

table(dis.iat$sex.new)
dis.iat$sex.new <- gsub("NA ", "", dis.iat$sex.new)
dis.iat$sex.new <- gsub(" NA", "", dis.iat$sex.new)

table(dis.iat$sex.new)
dis.iat$sex.new <- factor(dis.iat$sex.new)
dis.iat$sex.new <- recode(dis.iat$sex.new, "'NA' = NA")

table(dis.iat$sex.new, dis.iat$month)

## race --------------
table(dis.iat$raceomb, dis.iat$month)[3:8,]
table(dis.iat$raceomb_002, dis.iat$month)[3:8,] # 1607 onwards
table(dis.iat$raceombmulti, dis.iat$month) # 1607 onwards

## raceomb (with factor levels determined from documentation at OSF, matched to individual year data sets)
race <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
          "Black or African American", "White", "More than one race - Black/White", "More than one race - Other", "Other or Unknown")
table(dis.iat$raceomb)

dis.iat$raceomb <- factor(dis.iat$raceomb)
levels(dis.iat$raceomb)[levels(dis.iat$raceomb)== "-999"] <- NA
dis.iat$raceomb1 <- factor(dis.iat$raceomb, labels = race)

table(dis.iat$raceomb1)

table(dis.iat$raceombmulti) # if participant includes 5 and 6 = More than one race - B/W; all others = More than one race - Other
dis.iat$raceomb2 <- factor(dis.iat$raceombmulti)
levels(dis.iat$raceomb2) <- trimws(levels(dis.iat$raceomb2), which = "right")
dis.iat$raceomb2 <- recode(dis.iat$raceomb2, "'[5,6]' = 'More than one race - Black/White'; '[5]' = 'Black or African American'; 
                           '[6]' = 'White'; '[1]' = 'American Indian/Alaska Native'; '[2]' = 'East Asian';
                           '[3]' = 'South Asian'; '[4]' = 'Native Hawaiian or other Pacific Islander'; 
                           '[7]' = 'Other or Unknown'; '' = NA; '-999' = NA; '.' = NA; '[]' = NA; else = 'More than one race - Other'")
table(dis.iat$raceomb2)

dis.iat$raceomb.new <- paste(dis.iat$raceomb1, dis.iat$raceomb2)
table(dis.iat$raceomb.new)
dis.iat$raceomb.new <- gsub(" NA", "", dis.iat$raceomb.new)
dis.iat$raceomb.new <- factor(dis.iat$raceomb.new)

levels(dis.iat$raceomb.new)
dis.iat$raceomb.new <- recode(dis.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'White South Asian' = 'White';
                              'NA' = NA")
levels(dis.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(dis.iat$raceomb.new)), "Other or Unknown",
                                      levels(dis.iat$raceomb.new))
levels(dis.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(dis.iat$raceomb.new)), "More than one race - Other",
                                      levels(dis.iat$raceomb.new))
levels(dis.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(dis.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(dis.iat$raceomb.new))

table(dis.iat$raceomb.new, dis.iat$year)
table(dis.iat$raceomb.new, dis.iat$month)[3:9,] # complete data after 2006.



table(dis.iat$raceomb_002)
# drop the Multiracial level (included in the raceombmulti)
dis.iat$raceomb3 <- factor(dis.iat$raceomb_002, levels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                                           "Black or African American", "White", "Other or unknown"),
                           labels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                      "Black or African American", "White", "Other or Unknown"))
table(dis.iat$raceomb3)

dis.iat$raceomb.new <- paste(dis.iat$raceomb.new, dis.iat$raceomb3)
table(dis.iat$raceomb.new)
dis.iat$raceomb.new <- gsub(" NA", "", dis.iat$raceomb.new)
dis.iat$raceomb.new <- factor(dis.iat$raceomb.new)
table(dis.iat$raceomb.new)

dis.iat$raceomb.new <- recode(dis.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'Other or Unknown Other or Unknown'  = 'Other or Unknown';
                              'NA' = NA")
table(dis.iat$raceomb.new, dis.iat$month)[4:9,]
levels(dis.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(dis.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(dis.iat$raceomb.new))
levels(dis.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(dis.iat$raceomb.new)), "More than one race - Other",
                                      levels(dis.iat$raceomb.new))
levels(dis.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(dis.iat$raceomb.new)), "Other or Unknown",
                                      levels(dis.iat$raceomb.new))


table(dis.iat$raceomb.new, dis.iat$month)


#  ethnic (to match raceomb codes)
table(dis.iat$ethnic, dis.iat$month) # only to 0609
dis.iat$ethnic.new <- recode(dis.iat$ethnic, "'White-Not of Hispanic Origin' = 'White'; 'Black-Not of Hispanic Origin' = 'Black or African American';
                             'Asian or Pacific Islander' = 'Asian'; 'Multi-racial (Black and White)' = 'More than one race - Black/White';
                             'Multi-racial (other)' = 'More than one race - Other'; 'North American Indian or First Nation' = 'American Indian/Alaska Native';
                             'Hispanic' = 'Other or Unknown'")

dis.iat$race.new <- recode(dis.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(dis.iat$ethnic.new)
table(dis.iat$race.new)

dis.iat$ethnic.race.new <- paste(dis.iat$ethnic.new, dis.iat$race.new)
table(dis.iat$ethnic.race.new)

dis.iat$ethnic.race.new <- gsub("NA ", "", dis.iat$ethnic.race.new)
dis.iat$ethnic.race.new <- gsub(" NA", "", dis.iat$ethnic.race.new)
dis.iat$ethnic.race.new <- recode(dis.iat$ethnic.race.new, "'NA' = NA")

dis.iat$ethnic.race.new <- factor(dis.iat$ethnic.race.new)
levels(dis.iat$ethnic.race.new)
table(dis.iat$ethnic.race.new, dis.iat$year)
table(dis.iat$ethnic.race.new, dis.iat$month) # complete data from 2004 - 2016
table(dis.iat$ethnic.race.new, dis.iat$month)[3:7,]



## ethnicity ----------
## ethnicityomb (again, with values determined from documentation and single year codes)
table(dis.iat$ethnicityomb)

dis.iat$ethnicityomb <- factor(dis.iat$ethnicityomb)
levels(dis.iat$ethnicityomb)[levels(dis.iat$ethnicityomb)== "-999"] <- NA

dis.iat$ethnicityomb <- factor(dis.iat$ethnicityomb, labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown"))
table(dis.iat$ethnicityomb, dis.iat$year)
table(dis.iat$ethnicityomb, dis.iat$month) # complete data after 0609

## explicit attitude -----------
table(dis.iat$att) # falsely combines 5 and 7.
table(dis.iat$att_5)
table(dis.iat$att_7)

# get the right labels for att 7
dis.iat$att_7 <- recode(dis.iat$att_7, "'I slightly prefer abled people to disabled people..' = 'I slightly prefer abled people to disabled people.';
                      'I moderately prefer abled people to disabled people..' = 'I moderately prefer abled people to disabled people.';
                      'I like abled people and disabled people. equally.' = 'I like abled people and disabled people equally'")
# get the right order for att 7
dis.iat$att_7 <- factor(dis.iat$att_7, levels = c("I strongly prefer disabled people to abled people.", "I moderately prefer disabled people to abled people.",
                                              "I slightly prefer disabled people to abled people.", "I like abled people and disabled people equally",
                                              "I slightly prefer abled people to disabled people.", "I moderately prefer abled people to disabled people.",
                                              "I strongly prefer abled people to disabled people."))


# add two levels to att 5
levels(dis.iat$att_5) <- c(levels(dis.iat$att_5), "I slightly prefer disabled people to able people.", "I slightly prefer abled people to disabled people.")

# get the right order
dis.iat$att_5 <- factor(dis.iat$att_5, levels = c("I strongly prefer disabled people to abled people.", "I moderately prefer disabled people to abled people.",
                                              "I slightly prefer disabled people to abled people.", "I like abled people and disabled people equally",
                                              "I slightly prefer abled people to disabled people.", "I moderately prefer abled people to disabled people.",
                                              "I strongly prefer abled people to disabled people."))


dis.iat$att.new <- paste(dis.iat$att_5, dis.iat$att_7)
table(dis.iat$att.new)

dis.iat$att.new <- gsub("NA ", "", dis.iat$att.new)
dis.iat$att.new <- gsub(" NA", "", dis.iat$att.new)
dis.iat$att.new <- recode(dis.iat$att.new, "'NA' = NA")

# get the right order
dis.iat$att.new <- factor(dis.iat$att.new, levels = c("I strongly prefer disabled people to abled people.", "I moderately prefer disabled people to abled people.",
                                                  "I slightly prefer disabled people to abled people.", "I like abled people and disabled people equally",
                                                  "I slightly prefer abled people to disabled people.", "I moderately prefer abled people to disabled people.",
                                                  "I strongly prefer abled people to disabled people."))
levels(dis.iat$att.new)
table(dis.iat$att.new, dis.iat$year) # complete data
table(dis.iat$att.new, dis.iat$month) # complete data

## education --------
table(dis.iat$edu)
table(dis.iat$edu_13)
table(dis.iat$edu_14)

# make edu 13 a factor and add labels
dis.iat$edu_13 <- factor(dis.iat$edu_13)
levels(dis.iat$edu_13)[levels(dis.iat$edu_13)== "0"] <- NA
dis.iat$edu_131 <- factor(dis.iat$edu_13, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                     "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                                     "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
# add the missing level (MBA)
levels(dis.iat$edu_131) <- c(levels(dis.iat$edu_131), "M.B.A.")
table(dis.iat$edu_131)

# get the right order for edu 14
table(dis.iat$edu_14)
levels(dis.iat$edu_14)[levels(dis.iat$edu_14)== "-999"] <- NA
dis.iat$edu_141 <- factor(dis.iat$edu_14, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                     "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                                     "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
dis.iat$edu.new <- paste(dis.iat$edu_131, dis.iat$edu_141)
dis.iat$edu.new <- gsub("NA ", "", dis.iat$edu.new)
dis.iat$edu.new <- gsub(" NA", "", dis.iat$edu.new)
dis.iat$edu.new <- recode(dis.iat$edu.new, "'NA' = NA")

# get the right order
dis.iat$edu.new <- factor(dis.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                      "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                      "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
table(dis.iat$edu.new, dis.iat$year)
table(dis.iat$edu.new, dis.iat$month) # complete data after 2004

## political ideology --------
table(dis.iat$politicalid) # falsely combines pol 6 and pol 7
table(dis.iat$politicalid_6)
table(dis.iat$politicalid_7)

## remove the extraneous levels from the variables
dis.iat$politicalid_6 <- factor(dis.iat$politicalid_6)
levels(dis.iat$politicalid_6)[levels(dis.iat$politicalid_6)== "0"] <- NA

# get the right labels for pol 6
dis.iat$politicalid_61 <- factor(dis.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                   "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(dis.iat$politicalid_61) <- c(levels(dis.iat$politicalid_61), "Neutral")

# get the right order
dis.iat$politicalid_61 <- factor(dis.iat$politicalid_61, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                    "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(dis.iat$politicalid_61, dis.iat$year)

# add labels for pol 7
dis.iat$politicalid_71 <- factor(dis.iat$politicalid_7)
levels(dis.iat$politicalid_71)[levels(dis.iat$politicalid_71)== "-999"] <- NA
table(dis.iat$politicalid_71)
dis.iat$politicalid_71 <- factor(dis.iat$politicalid_71, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                    "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# combine the raw scales.
dis.iat$pol.new <- paste(dis.iat$politicalid_61, dis.iat$politicalid_71)
table(dis.iat$pol.new)
dis.iat$pol.new <- gsub("NA ", "", dis.iat$pol.new)
dis.iat$pol.new <- gsub(" NA", "", dis.iat$pol.new)
dis.iat$pol.new <- recode(dis.iat$pol.new, "'NA' = NA")

table(dis.iat$pol.new)
# get the right order
dis.iat$pol.new <- factor(dis.iat$pol.new, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                      "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(dis.iat$pol.new, dis.iat$year) # complete data now after 2004
dis.iat$politicalid <- dis.iat$pol.new

table(dis.iat$politicalid, dis.iat$month) # complete data here too..

## country of citizenship and residence -----------
## recode country cit and country res
sort(table(dis.iat$countrycit))
sort(table(dis.iat$countryres))
sort(table(dis.iat$countrycit_num))
sort(table(dis.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(dis.iat$countrycit)
dis.iat$countrycit <- gsub("  ", "", dis.iat$countrycit)
dis.iat$countrycit <- gsub(" ", "", dis.iat$countrycit)
dis.iat$countrycit <- factor(dis.iat$countrycit)
dis.iat$countrycit1 <- recode(dis.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(dis.iat$countrycit1))
dis.iat$countrycit1 <- recode(dis.iat$countrycit1, "'1' = 'US'")
dis.iat$countrycit2 <- recode(dis.iat$countrycit_num, "'U.S.A. ' = 'US'")

dis.iat$countrycit.new <- paste(dis.iat$countrycit1, dis.iat$countrycit2)
dis.iat$countrycit.new <- gsub("NA  ", "", dis.iat$countrycit.new)
dis.iat$countrycit.new <- gsub(" NA", "", dis.iat$countrycit.new)
dis.iat$countrycit.new <- factor(dis.iat$countrycit.new)
dis.iat$countrycit.new <- recode(dis.iat$countrycit.new, "'US US' = 'US'")
sort(table(dis.iat$countrycit.new))

table(dis.iat$countrycit.new, dis.iat$month)["US",] # complete data after 0404

levels(dis.iat$countryres)
dis.iat$countryres <- gsub("  ", "", dis.iat$countryres)
dis.iat$countryres <- gsub(" ", "", dis.iat$countryres)
dis.iat$countryres <- factor(dis.iat$countryres)
dis.iat$countryres1 <- recode(dis.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(dis.iat$countryres1))
dis.iat$countryres1 <- recode(dis.iat$countryres1, "'1' = 'US'")
dis.iat$countryres2 <- recode(dis.iat$countryres_num, "'U.S.A. ' = 'US'")

dis.iat$countryres.new <- paste(dis.iat$countryres1, dis.iat$countryres2)
dis.iat$countryres.new <- gsub("NA  ", "", dis.iat$countryres.new)
dis.iat$countryres.new <- gsub(" NA", "", dis.iat$countryres.new)
dis.iat$countryres.new <- factor(dis.iat$countryres.new)
dis.iat$countryres.new <- recode(dis.iat$countryres.new, "'US US' = 'US'")
sort(table(dis.iat$countryres.new))

table(dis.iat$countryres.new, dis.iat$month)["US",] # complete data after 0404

## disabled --------------
table(dis.iat$disabled, dis.iat$year)
table(dis.iat$disabled_001, dis.iat$year)

levels(dis.iat$disabled)[levels(dis.iat$disabled)==".   " | levels(dis.iat$disabled)== "    " | levels(dis.iat$disabled)=="null"] <- NA
dis.iat$disabled <- factor(dis.iat$disabled, labels = c("No", "Yes"))
table(dis.iat$disabled, dis.iat$year)

dis.iat$dis.new <- paste(dis.iat$disabled, dis.iat$disabled_001)
dis.iat$dis.new <- gsub("NA ", "", dis.iat$dis.new)
dis.iat$dis.new <- gsub(" NA", "", dis.iat$dis.new)
dis.iat$dis.new <- factor(dis.iat$dis.new, levels = c("No", "Yes"))
table(dis.iat$dis.new, dis.iat$year)

## disability type ---------
table(dis.iat$distype, dis.iat$month)
sort(table(dis.iat$distypes))

levels(dis.iat$distype)[levels(dis.iat$distype)==".         " | levels(dis.iat$distype)== "          " | levels(dis.iat$distype)=="null      "] <- NA
dis.iat$distype <- factor(dis.iat$distype, labels = c("both", "mental", "physical"))
table(dis.iat$distype, dis.iat$month)


dis.iat$distypes2 <- gsub("[[:space:]]", "", dis.iat$distypes)
dis.iat$distypes2 <- factor(dis.iat$distypes2)
levels(dis.iat$distypes2)[levels(dis.iat$distypes2)=="" | levels(dis.iat$distypes2)== "." | levels(dis.iat$distypes2)=="-999" | levels(dis.iat$distypes2)=="[]"] <- NA
levels(dis.iat$distypes2)
# 1 = visual; 2 = hearing; 6 = wheelchair; 7 = mobility; 11 = other physical;[physical]
# 3 = complex; 12 = else [both]
# 4 = dyscalculia; 5 = dyslexia; 8 = mental; 9 = emotional/behavioral; 10 = other learning [mental]

dis.iat$distypes2 <- recode(dis.iat$distypes2, "'[1]' = 'physical'; '[2]' = 'physical'; '[6]' = 'physical';
                               '[7]' = 'physical'; '[11]' = 'physical'; '[3]' = 'both'; '[12]' = 'both';
                               '[4]' = 'mental'; '[5]' = 'mental'; '[8]' = 'mental'; '[9]' = 'mental'; '[10]' = 'mental';
                               NA = NA; else = 'both'")
table(dis.iat$distypes2)

dis.iat$distypes.new <- paste(dis.iat$distype, dis.iat$distypes2)
dis.iat$distypes.new <- gsub("NA ", "", dis.iat$distypes.new)
dis.iat$distypes.new <- gsub(" NA", "", dis.iat$distypes.new)
dis.iat$distypes.new <- factor(dis.iat$distypes.new, levels = c("both", "mental", "physical"))
table(dis.iat$distypes.new, dis.iat$year)
table(dis.iat$distypes.new, dis.iat$month) # complete data after 0609


## zipcodes ----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
table(dis.iat$postcodelong)
table(dis.iat$postcodenow)

# make it a character to use regular expressions
dis.iat$zip <- as.character(dis.iat$postcodelong)

dis.iat$zip <- gsub("[[:punct:]]", "", dis.iat$zip) # remove punctuation
dis.iat$zip <- gsub("[[:space:]]", "", dis.iat$zip) # remove all white space
dis.iat$zip <- gsub("[^0-9]", "", dis.iat$zip) # remove all non-numeric characters

dis.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", dis.iat$zip) # remove all strings shorter than 5
dis.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", dis.iat$zip1) # remove all strings longer than 5, shorter than 9
dis.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", dis.iat$zip1) # remove all strings longer 9

dis.iat$zip1 <- as.character(dis.iat$zip1)

table(dis.iat$zip1[dis.iat$zip1>02112], dis.iat$month[dis.iat$zip1>02112]) # data only available after 0404

## add on geographic information
dis.iat.places <- merge(dis.iat, zips, by = "zip1", all = TRUE)
## total with zipcode information (US participants) = 150,226

## data subsetting -------------------------------------------------------------------------------------------------------
# subset to US participants (by countryres.new), allows for retaining the most participants over time
dis.iatUSres <- dis.iat.places[dis.iat.places$countryres.new=="US",]
sort(table(dis.iatUSres$countryres.new))
## total with US country residence = 210,418


## now keep only those with complete data for the following:
# dscore, att.new, sex.new
# race, dis, Order, ethnic.race.new, politicalid, edu, disabled
dis.full <- dis.iatUSres[is.na(dis.iatUSres$dscore)==FALSE,] # 210,418
dis.full <- dis.full[is.na(dis.full$att.new)==FALSE,] #  203,891

dis.full.demogs <- dis.full[is.na(dis.full$sex.new)==FALSE,] # 202,475
dis.full.demogs <- dis.full.demogs[is.na(dis.full.demogs$age.new)==FALSE,] # 200,816
dis.full.demogs <- dis.full.demogs[is.na(dis.full.demogs$ethnic.race.new)==FALSE,] # 198,061
dis.full.demogs <- dis.full.demogs[is.na(dis.full.demogs$politicalid)==FALSE,] # 193,484
dis.full.demogs <- dis.full.demogs[is.na(dis.full.demogs$edu.new)==FALSE,] # 192,358
dis.full.demogs <- dis.full.demogs[is.na(dis.full.demogs$dis.new)==FALSE,] # 191,499

# also include the following variables: num, and geographic information
dis.full.demogs <- dis.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new", "age.new",
                                     "dis.new", "distypes.new", "sex.new", "ethnic.race.new", "raceomb.new", "ethnicityomb",
                                     "politicalid", "edu.new", "countryres.new", "countrycit.new",
                                     "Order", "num.new", 
                                     "zip1", "State", "Place.Name", "State.Abbreviation", "County")]

table(dis.full.demogs$month.cent)
table(dis.full.demogs$month)

##############################################################################################################################################
## CLEAN THE RACE DATA ---------------------------
race.iat <- read.spss("Race IAT.2002-2016.sav", to.data.frame = TRUE) # 6,624,119

## to optimize, first reduce the number of variables...
race.iat <- race.iat[c("session_id", "session_status", "date", "year", "age", "birthyear",
                       "num", "num_002", "sex", "sex_5", "birthsex", "genderidentity",
                       "D_biep.White_Good_all", "Order", "labels",
                       "att", "att_5", "att_7", "att7",
                       "countrycit", "countryres",
                       "edu", "edu_9", "edu_14", "edustudent", "edunotstudent", "studentornot",
                       "ethnic", "ethnicityomb",  "raceomb", "raceomb_002", "raceombmulti",
                       "politicalid", "politicalid_7", "politicalid_6",
                       "religion", "religion2009", "religionid", "religion2014",
                       "postcodelong", "postcodenow",
                       "Mn_RT_correct_3", "Mn_RT_correct_4", "Mn_RT_correct_6", "Mn_RT_correct_7",
                       "N_ERROR_3", "N_ERROR_4", "N_ERROR_6", "N_ERROR_7")]

# get the date in the right format
race.iat$date <- as.chron(ISOdate(1582, 10, 14) + race.iat$date) 
race.iat$date <- as.Date(race.iat$date)

# get a month variable
race.iat$month <- format(race.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (subtract 13 months to make month.cent 1 equivalent to January, 2004)
table(race.iat$month)
race.iat$month.cent <- as.numeric(factor(race.iat$month)) - 13

table(race.iat$month)
table(race.iat$month.cent)

# keep only those with month.cent > 0 and < 157
race.iat <- race.iat[race.iat$month.cent > 0,]
race.iat <- race.iat[race.iat$month.cent < 157,]
# new total for 2004 - 2016 = 6,544,350

# how many US subjects? -------------
## recode country cit and country res
sort(table(race.iat$countrycit))
sort(table(race.iat$countryres))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(race.iat$countrycit)
race.iat$countrycit <- gsub("  ", "", race.iat$countrycit)
race.iat$countrycit <- gsub(" ", "", race.iat$countrycit)
race.iat$countrycit <- factor(race.iat$countrycit)
race.iat$countrycit1 <- recode(race.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(race.iat$countrycit1))
race.iat$countrycit.new <- recode(race.iat$countrycit1, "'1' = 'US'")

table(race.iat$countrycit.new, race.iat$month)["US",] # complete data after 0403

levels(race.iat$countryres)
race.iat$countryres <- gsub("  ", "", race.iat$countryres)
race.iat$countryres <- gsub(" ", "", race.iat$countryres)
race.iat$countryres <- factor(race.iat$countryres)
race.iat$countryres1 <- recode(race.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(race.iat$countryres1))
race.iat$countryres.new <- recode(race.iat$countryres1, "'1' = 'US'")

# how many US overall?
table(race.iat$countryres.new)["US"] # 3,114,638
# how many US complete?
table(race.iat$countryres.new[race.iat$session_status=="C   "])["US"] # 2,207,592

# save a back-up before any further cleaning
race.iat2 <- race.iat

# keep only complete subjects --------
race.iat <- race.iat[race.iat$session_status=="C   ",] # complete subjects = 2,930,395
race.iat <- race.iat[is.na(race.iat$D_biep.White_Good_all)==FALSE,] # complete subjects for IAT data = 2,835,543



## dscore -----------------
names(race.iat)[which(names(race.iat)=="D_biep.White_Good_all")] <- "dscore"

## session id  -----------------
race.iat$session_id <- as.character(race.iat$session_id)

## age ------------
table(race.iat$age, race.iat$year) # not collected after mid-2015

# working with birthyear
table(race.iat$birthyear, race.iat$year) # this one picks up...
table(race.iat$birthyear) # combines codes that are real values and coded values

race.iat$birthyear.new <- factor(race.iat$birthyear)
levels(race.iat$birthyear.new) # missing 1:6, 25:28, 30, 32:33, 37:38, 40:46,
# max is 47, so subtract 46
2009-46 # 1963

birthyears <- seq(2009, 1963, -1)
birthyears <- birthyears[-c(1:6, 25:28, 30, 32:33, 37:38, 40:46)]
birthyears

levels(race.iat$birthyear.new)[1:25] <- birthyears
table(race.iat$birthyear.new)

# birthyear was introduced in 2016, so subtract birthyear from 2016 to get age
# get in the right order
race.iat$birthyear.new2 <- factor(race.iat$birthyear.new, levels = c(1910:2009))
# save as character
race.iat$birthyear.new2 <- as.character(race.iat$birthyear.new2)
# then as numeric
race.iat$birthyear.new2 <- as.numeric(race.iat$birthyear.new2)
# subtract
race.iat$birthyear.new2 <- 2016 - race.iat$birthyear.new2

table(race.iat$birthyear.new2)

## now combine
race.iat$age.new <- paste(race.iat$age, race.iat$birthyear.new2)

table(race.iat$age.new)
race.iat$age.new <- gsub("NA ", "", race.iat$age.new)
race.iat$age.new <- gsub(" NA", "", race.iat$age.new)

race.iat$age.new <- factor(race.iat$age.new)

race.iat$age.new <- factor(race.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
race.iat$age.new <- droplevels(race.iat$age.new)

table(race.iat$age.new)

race.iat$age.new <- as.character(race.iat$age.new)
race.iat$age.new <- as.numeric(race.iat$age.new)

table(race.iat$age.new, race.iat$year)
table(race.iat$age.new[race.iat$age.new>25], race.iat$month[race.iat$age.new>25]) # complete data

## num ----------------
table(race.iat$num)
# remove extraneous levels
levels(race.iat$num)[levels(race.iat$num)==".  " | levels(race.iat$num)== "   " | levels(race.iat$num)== "nul" | levels(race.iat$num)== "-99"] <- NA

table(race.iat$num, race.iat$month.cent)
table(race.iat$num, race.iat$month.cent)[3:8,]

race.iat$numpre152 <- ifelse(race.iat$month.cent <= 152, race.iat$num, NA)
table(race.iat$numpre152)
table(race.iat$numpre152, race.iat$month.cent)

race.iat$numpre152 <- recode(race.iat$numpre152, "'1' = '0'; '2' = '1'; '3' = '2'; '4' = '3-5'; '5' = '3-5'; '6' = '3-5'; '7' = '3-5'; '8' = '6+'")
table(race.iat$numpre152, race.iat$month.cent)

table(race.iat$num_002, race.iat$month.cent)
race.iat$numpost152 <- recode(race.iat$num_002, "'1' = '0'; '2' = '1'; '3' = '2'; '4' = '3-5'; '5' = '6+'")
table(race.iat$numpost152)
table(race.iat$numpost152, race.iat$month.cent)

race.iat$num.new <- paste(race.iat$numpre152, race.iat$numpost152)

table(race.iat$num.new)
race.iat$num.new <- gsub("NA ", "", race.iat$num.new)
race.iat$num.new <- gsub(" NA", "", race.iat$num.new)

race.iat$num.new <- factor(race.iat$num.new)
levels(race.iat$num.new)
race.iat$num.new <- recode(race.iat$num.new, "'NA' = NA")

table(race.iat$num.new)
table(race.iat$num.new, race.iat$month) # complete data


## sex -------------
table(race.iat$sex)
levels(race.iat$sex)[levels(race.iat$sex)=="." | levels(race.iat$sex)== " " | levels(race.iat$sex)== "1" | levels(race.iat$sex)== "A" | levels(race.iat$sex)== "n"] <- NA
race.iat$sex <- recode(race.iat$sex, "'f' = 'female'; 'm' = 'male'; 'F' = 'female'; 'M' = 'male'")

table(race.iat$birthsex)
race.iat$birthsex <- factor(race.iat$birthsex, labels = c("male", "female"))

race.iat$sex.new <- paste(race.iat$sex, race.iat$birthsex)

table(race.iat$sex.new)
race.iat$sex.new <- gsub("NA ", "", race.iat$sex.new)
race.iat$sex.new <- gsub(" NA", "", race.iat$sex.new)

table(race.iat$sex.new)
race.iat$sex.new <- factor(race.iat$sex.new)
race.iat$sex.new <- recode(race.iat$sex.new, "'NA' = NA")

## one more sex variable to paste on...
table(race.iat$sex_5)
race.iat$sex_5new <- recode(race.iat$sex_5, "'Female' = 'female'; 'Male' = 'male'; 'Transgender, MTF' = 'female'; 'Transgender, FTM' = 'male'; 'Other' = NA")
table(race.iat$sex_5new)

race.iat$sex.new2 <- paste(race.iat$sex.new, race.iat$sex_5new)

table(race.iat$sex.new2)
race.iat$sex.new2 <- gsub("NA ", "", race.iat$sex.new2)
race.iat$sex.new2 <- gsub(" NA", "", race.iat$sex.new2)

table(race.iat$sex.new2)
race.iat$sex.new2 <- factor(race.iat$sex.new2)
race.iat$sex.new2 <- recode(race.iat$sex.new2, "'NA' = NA; 'female female' = 'female'; 'male female' = NA; 'male male' = 'male'")

table(race.iat$sex.new2, race.iat$month) # complete data

## reassign to sex.new
race.iat$sex.new <- race.iat$sex.new2


## race -----------------
table(race.iat$raceomb, race.iat$month)[3:8,]
table(race.iat$raceomb_002, race.iat$month)[3:8,] # 1607 onwards
table(race.iat$raceombmulti, race.iat$month) # 1607 onwards

## raceomb (with factor levels determined from documentation at OSF, matched to individual year data sets)
race <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
          "Black or African American", "White", "More than one race - Black/White", "More than one race - Other", "Other or Unknown")
table(race.iat$raceomb)

race.iat$raceomb <- factor(race.iat$raceomb)
levels(race.iat$raceomb)[levels(race.iat$raceomb)== "-2147483648" | levels(race.iat$raceomb)== "376"] <- NA
race.iat$raceomb1 <- factor(race.iat$raceomb, labels = race)

table(race.iat$raceomb1)

table(race.iat$raceombmulti) # if participant includes 5 and 6 = More than one race - B/W; all others = More than one race - Other
race.iat$raceomb2 <- factor(race.iat$raceombmulti)
levels(race.iat$raceomb2) <- trimws(levels(race.iat$raceomb2), which = "right")
race.iat$raceomb2 <- recode(race.iat$raceomb2, "'[5,6]' = 'More than one race - Black/White'; '[5]' = 'Black or African American'; 
                           '[6]' = 'White'; '[1]' = 'American Indian/Alaska Native'; '[2]' = 'East Asian';
                           '[3]' = 'South Asian'; '[4]' = 'Native Hawaiian or other Pacific Islander'; 
                           '[7]' = 'Other or Unknown'; '' = NA; '-999' = NA; '.' = NA; '[]' = NA; else = 'More than one race - Other'")
table(race.iat$raceomb2)

race.iat$raceomb.new <- paste(race.iat$raceomb1, race.iat$raceomb2)
table(race.iat$raceomb.new)
race.iat$raceomb.new <- gsub(" NA", "", race.iat$raceomb.new)
race.iat$raceomb.new <- gsub("NA ", "", race.iat$raceomb.new)

race.iat$raceomb.new <- factor(race.iat$raceomb.new)

levels(race.iat$raceomb.new)
race.iat$raceomb.new <- recode(race.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'White South Asian' = 'White'; 'White Native Hawaiian or other Pacific Islander' = 'White';
                              'NA' = NA")
levels(race.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(race.iat$raceomb.new)), "Other or Unknown",
                                      levels(race.iat$raceomb.new))
levels(race.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(race.iat$raceomb.new)), "More than one race - Other",
                                      levels(race.iat$raceomb.new))
levels(race.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(race.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(race.iat$raceomb.new))

table(race.iat$raceomb.new, race.iat$year)
table(race.iat$raceomb.new, race.iat$month)[3:8,] # complete data after 2006.



table(race.iat$raceomb_002)
# drop the Multiracial level (included in the raceombmulti)
race.iat$raceomb3 <- factor(race.iat$raceomb_002, levels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                                           "Black or African American", "White", "Other or unknown"),
                           labels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                      "Black or African American", "White", "Other or Unknown"))
table(race.iat$raceomb3)

race.iat$raceomb.new <- paste(race.iat$raceomb.new, race.iat$raceomb3)
table(race.iat$raceomb.new)
race.iat$raceomb.new <- gsub(" NA", "", race.iat$raceomb.new)
race.iat$raceomb.new <- factor(race.iat$raceomb.new)
table(race.iat$raceomb.new)

race.iat$raceomb.new <- recode(race.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'Other or Unknown Other or Unknown'  = 'Other or Unknown';
                              'NA' = NA")
table(race.iat$raceomb.new, race.iat$month)[4:9,]
levels(race.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(race.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(race.iat$raceomb.new))
levels(race.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(race.iat$raceomb.new)), "More than one race - Other",
                                      levels(race.iat$raceomb.new))
levels(race.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(race.iat$raceomb.new)), "Other or Unknown",
                                      levels(race.iat$raceomb.new))

table(race.iat$raceomb.new, race.iat$year)
table(race.iat$raceomb.new, race.iat$month)


#  ethnic (to match raceomb codes)
table(race.iat$ethnic, race.iat$month) # only to 0609
race.iat$ethnic.new <- recode(race.iat$ethnic, "'White-Not of Hispanic Origin' = 'White'; 'Black-Not of Hispanic Origin' = 'Black or African American';
                             'Asian or Pacific Islander' = 'Asian'; 'Multi-racial (Black and White)' = 'More than one race - Black/White';
                             'Multi-racial (other)' = 'More than one race - Other'; 'North American Indian or First Nation' = 'American Indian/Alaska Native';
                             'Hispanic' = 'Other or Unknown'")

race.iat$race.new <- recode(race.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(race.iat$ethnic.new)
table(race.iat$race.new)

race.iat$ethnic.race.new <- paste(race.iat$ethnic.new, race.iat$race.new)
table(race.iat$ethnic.race.new)

race.iat$ethnic.race.new <- gsub("NA ", "", race.iat$ethnic.race.new)
race.iat$ethnic.race.new <- gsub(" NA", "", race.iat$ethnic.race.new)
race.iat$ethnic.race.new <- recode(race.iat$ethnic.race.new, "'NA' = NA")

race.iat$ethnic.race.new <- factor(race.iat$ethnic.race.new)
levels(race.iat$ethnic.race.new)
table(race.iat$ethnic.race.new, race.iat$year)
table(race.iat$ethnic.race.new, race.iat$month) # complete data from 2004 - 2016
table(race.iat$ethnic.race.new, race.iat$month)[3:7,]





## ethnicityomb -------------
## (again, with values determined from documentation and single year codes)
table(race.iat$ethnicityomb)

levels(race.iat$ethnicityomb)[levels(race.iat$ethnicityomb)== "-2147483648"] <- NA

race.iat$ethnicityomb <- factor(race.iat$ethnicityomb, labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown"))
table(race.iat$ethnicityomb, race.iat$year)
table(race.iat$ethnicityomb, race.iat$month) # complete data after 0609.


## explicit attitude -------------------
table(race.iat$att, race.iat$year) # falsely combines 5 and 7.
table(race.iat$att_5, race.iat$year) # 2004 - 2006
table(race.iat$att_7, race.iat$year) # 2006 - 2016
table(race.iat$att7, race.iat$year) # end of 2016

# add two levels to att 5
levels(race.iat$att_5) <- c(levels(race.iat$att_5), "I slightly prefer African Americans to European Americans.",
                            "I slightly prefer European Americans to African Americans.")

# get the right order
race.iat$att_51 <- factor(race.iat$att_5, levels = c("I strongly prefer African Americans to European Americans.",
                                                     "I moderately prefer African Americans to European Americans.",
                                                     "I slightly prefer African Americans to European Americans.",
                                                     "I like European Americans and African Americans equally.",
                                                     "I slightly prefer European Americans to African Americans.",
                                                     "I moderately prefer European Americans to African Americans.",
                                                     "I strongly prefer European Americans to African Americans."))
# get the right order
race.iat$att_71 <- factor(race.iat$att_7, levels = c("I strongly prefer African Americans to European Americans.",
                                                     "I moderately prefer African Americans to European Americans.",
                                                     "I slightly prefer African Americans to European Americans.",
                                                     "I like European Americans and African Americans equally.",
                                                     "I slightly prefer European Americans to African Americans.",
                                                     "I moderately prefer European Americans to African Americans.",
                                                     "I strongly prefer European Americans to African Americans."))


race.iat$att.new <- paste(race.iat$att_51, race.iat$att_71)
table(race.iat$att.new)

race.iat$att.new <- gsub("NA ", "", race.iat$att.new)
race.iat$att.new <- gsub(" NA", "", race.iat$att.new)
race.iat$att.new <- recode(race.iat$att.new, "'NA' = NA")
table(race.iat$att.new)

race.iat$att.new <- factor(race.iat$att.new, levels = c("I strongly prefer African Americans to European Americans.",
                                                        "I moderately prefer African Americans to European Americans.",
                                                        "I slightly prefer African Americans to European Americans.",
                                                        "I like European Americans and African Americans equally.",
                                                        "I slightly prefer European Americans to African Americans.",
                                                        "I moderately prefer European Americans to African Americans.",
                                                        "I strongly prefer European Americans to African Americans."))
table(race.iat$att.new)

# one more attitude variable to add on...
race.iat$att.new2 <- paste(race.iat$att.new, race.iat$att7)
table(race.iat$att.new2)

race.iat$att.new2 <- gsub("NA ", "", race.iat$att.new2)
race.iat$att.new2 <- gsub(" NA", "", race.iat$att.new2)
race.iat$att.new2 <- recode(race.iat$att.new2, "'NA' = NA")

table(race.iat$att.new2)

## get the right order and the right name for the final variable.
race.iat$att.new <- factor(race.iat$att.new2, levels = c("I strongly prefer African Americans to European Americans.",
                                                          "I moderately prefer African Americans to European Americans.",
                                                          "I slightly prefer African Americans to European Americans.",
                                                          "I like European Americans and African Americans equally.",
                                                          "I slightly prefer European Americans to African Americans.",
                                                          "I moderately prefer European Americans to African Americans.",
                                                          "I strongly prefer European Americans to African Americans."))
table(race.iat$att.new, race.iat$month) # complete data now.


## education -------------------
table(race.iat$edu, race.iat$year)
table(race.iat$edunotstudent, race.iat$year) # 2015 and 2016
table(race.iat$edustudent, race.iat$year) # 2015 and 2016

# relabel and remove single quotation marks
race.iat$edu <- factor(race.iat$edu, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order:
race.iat$edu.new <- factor(race.iat$edu, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                    "associates degree", "bachelors degree", "some graduate school", "masters degree", "M.B.A.",
                                                    "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(race.iat$edu.new, race.iat$month) # no data available in 1504 - 1608


# ordering is different for other variables:
race.iat$edunotstudent2 <- factor(race.iat$edunotstudent, 
                                  labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                             "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                             "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
race.iat$edustudent2 <- recode(race.iat$edustudent, "'Student in the first or second year of college' = 'some college';
                               'Student in the third or higher year of college' = 'some college'")
table(race.iat$edustudent2)
race.iat$edustudent2 <- factor(race.iat$edustudent2, 
                               labels = c("some college", "elementary school", "some high school",
                                          "J.D.", "junior high", "M.B.A.",
                                          "M.D.", "other advanced degree", "masters degree", "Ph.D."))


table(race.iat$edu)
table(race.iat$edunotstudent2)
table(race.iat$edustudent2)

# get the correct ordering
race.iat$edunotstudent2 <- factor(race.iat$edunotstudent2, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                                      "associates degree", "bachelors degree", "some graduate school", "masters degree", "M.B.A.",
                                                                      "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
race.iat$edustudent2 <- factor(race.iat$edustudent2, levels = c("elementary school", "junior high", "some high school", "some college",
                                                                "masters degree", "M.B.A.",
                                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
table(race.iat$edu)
table(race.iat$edunotstudent2)
table(race.iat$edustudent2)

# paste all together
race.iat$edu.new <- paste(race.iat$edu, race.iat$edunotstudent2)
table(race.iat$edu.new)

race.iat$edu.new <- gsub("NA ", "", race.iat$edu.new)
race.iat$edu.new <- gsub(" NA", "", race.iat$edu.new)
race.iat$edu.new <- recode(race.iat$edu.new, "'masters degree masters degree' = 'masters degree'; 'NA' = NA")
table(race.iat$edu.new)

race.iat$edu.new <- paste(race.iat$edu.new, race.iat$edustudent2)
table(race.iat$edu.new)
race.iat$edu.new <- gsub("NA ", "", race.iat$edu.new)
race.iat$edu.new <- gsub(" NA", "", race.iat$edu.new)
race.iat$edu.new <- recode(race.iat$edu.new, "'associates degree some college' = NA; 'bachelors degree some college' = NA;
                           'high school graduate some college' = NA; 'junior high junior high' = 'junior high';
                           'masters degree masters degree' = 'masters degree'; 'masters degree some high school' = NA;
                           'Ph.D. Ph.D.' = 'Ph.D.'; 'some college some college' = 'some college';
                           'some high school some college' = NA; 'some high school some high school' = 'some high school';
                           'NA' = NA")
table(race.iat$edu.new)

race.iat$edu.new <- factor(race.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                        "associates degree", "bachelors degree", "some graduate school", "masters degree", "M.B.A.",
                                                        "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(race.iat$edu.new, race.iat$month) # available all months ar expected frequencies.


## political ideology ------------------
table(race.iat$politicalid, race.iat$year) # falsely combines pol 6 and pol 7
table(race.iat$politicalid_6, race.iat$year) # 2004 - 2006
table(race.iat$politicalid_7, race.iat$year) # 2006 - 2016

# get the right labels for pol 6
race.iat$politicalid_61 <- factor(race.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                     "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(race.iat$politicalid_61) <- c(levels(race.iat$politicalid_61), "Neutral")

# get the right order
race.iat$politicalid_61 <- factor(race.iat$politicalid_61, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                      "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(race.iat$politicalid_61)

# add labels for pol 7
pol <- levels(race.iat$politicalid_61)

race.iat$politicalid_71 <- factor(race.iat$politicalid_7, labels = pol)
table(race.iat$politicalid_71)

# combine the raw scales.
race.iat$pol.new <- paste(race.iat$politicalid_61, race.iat$politicalid_71)
table(race.iat$pol.new)
race.iat$pol.new <- gsub("NA ", "", race.iat$pol.new)
race.iat$pol.new <- gsub(" NA", "", race.iat$pol.new)
race.iat$pol.new <- recode(race.iat$pol.new, "'NA' = NA")

table(race.iat$pol.new)

# get the right order
race.iat$pol.new <- factor(race.iat$pol.new, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                        "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(race.iat$pol.new, race.iat$month) # complete data with expected frequencies
race.iat$politicalid <- race.iat$pol.new

table(race.iat$politicalid, race.iat$month) # complete data here too..


## country of citizenship and country of residence -------------
## recode country cit and country res
sort(table(race.iat$countrycit))
sort(table(race.iat$countryres))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(race.iat$countrycit)
race.iat$countrycit <- gsub("  ", "", race.iat$countrycit)
race.iat$countrycit <- gsub(" ", "", race.iat$countrycit)
race.iat$countrycit <- factor(race.iat$countrycit)
race.iat$countrycit1 <- recode(race.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(race.iat$countrycit1))
race.iat$countrycit.new <- recode(race.iat$countrycit1, "'1' = 'US'")

table(race.iat$countrycit.new, race.iat$month)["US",] # complete data after 0403

levels(race.iat$countryres)
race.iat$countryres <- gsub("  ", "", race.iat$countryres)
race.iat$countryres <- gsub(" ", "", race.iat$countryres)
race.iat$countryres <- factor(race.iat$countryres)
race.iat$countryres1 <- recode(race.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(race.iat$countryres1))
race.iat$countryres.new <- recode(race.iat$countryres1, "'1' = 'US'")

table(race.iat$countryres.new, race.iat$month)["US",] # complete data after 0403
## note, however, a problem with the 2015 data? Low frequencies...


## zipcodes ----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
table(race.iat$postcodelong)
table(race.iat$postcodenow)

# make it a character to use regular expressions
race.iat$zip <- as.character(race.iat$postcodelong)

race.iat$zip <- gsub("[[:punct:]]", "", race.iat$zip) # remove punctuation
race.iat$zip <- gsub("[[:space:]]", "", race.iat$zip) # remove all white space
race.iat$zip <- gsub("[^0-9]", "", race.iat$zip) # remove all non-numeric characters

race.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", race.iat$zip) # remove all strings shorter than 5
race.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", race.iat$zip1) # remove all strings longer than 5, shorter than 9
race.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", race.iat$zip1) # remove all strings longer 9

race.iat$zip1 <- as.character(race.iat$zip1)

table(race.iat$zip1[race.iat$zip1>02112], race.iat$month[race.iat$zip1>02112]) # complete data

## add on the geographic information (from zipcodes)
race.iat.places <- merge(race.iat, zips, by = "zip1", all = TRUE) 

## data subsetting -----------------------------------------------------------------------------------------------------------------
race.iatUSres <- race.iat.places[race.iat.places$countryres.new=="US",]
sort(table(race.iatUSres$countryres.new))
## total with US country residence = 2,150,952



## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
race.full <- race.iatUSres[is.na(race.iatUSres$dscore)==FALSE,] # 2,150,952
race.full <- race.full[is.na(race.full$att.new)==FALSE,] # 2,004,363

table(race.full$month) ## all available months.

race.full.demogs <- race.full[is.na(race.full$sex.new)==FALSE,] # 1,970,387
table(race.full.demogs$month) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$ethnic.race.new)==FALSE,] # 1,926,562
table(race.full.demogs$month) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$politicalid)==FALSE,] # 1,881,079
table(race.full.demogs$month) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$edu.new)==FALSE,] # 1,866,357
table(race.full.demogs$month) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$age.new)==FALSE,] # 1,851,445
table(race.full.demogs$month) ## no loss of months

# also include the following variables: num and further error/RT variables
race.full.demogs <- race.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                       "age.new", "sex.new", "ethnic.race.new", "raceomb.new", "ethnicityomb",
                                       "politicalid", "edu.new",
                                       "Order", "num.new",
                                       "countryres.new", "countrycit.new",
                                       "zip1", "State", "Place.Name", "State.Abbreviation", "County",
                                       "Mn_RT_correct_3", "Mn_RT_correct_4", "Mn_RT_correct_6", "Mn_RT_correct_7",
                                       "N_ERROR_3", "N_ERROR_4", "N_ERROR_6", "N_ERROR_7")]
table(race.full.demogs$month.cent)
table(race.full.demogs$month)
length(table(race.full.demogs$month.cent))

##############################################################################################################################################
## CLEAN THE SEXUALITY DATA -----------------------------------------------------------------------------------------------------

s.iat <- read.spss("Sexuality IAT.2004-2016.sav", 
                   to.data.frame = TRUE) # 2,694,240

# get the date in the right format
s.iat$date <- as.chron(ISOdate(1582, 10, 14) + s.iat$date) 
s.iat$date <- as.Date(s.iat$date)

# get a month variable
s.iat$month <- format(s.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (add 3 months to make month.cent 1 equivalent to January, 2004)
table(s.iat$month)
s.iat$month.cent <- as.numeric(factor(s.iat$month)) + 3

table(s.iat$month)
table(s.iat$month.cent)

# keep only those with month.cent = 4 (only 9 observations) and < 157
s.iat <- s.iat[s.iat$month.cent != 4,]
s.iat <- s.iat[s.iat$month.cent < 157,]

## for the data from 2004 - 2016, the new total is 2,694,129 participants.

# how many US? ---------------
## recode country cit and country res
sort(table(s.iat$countrycit))
sort(table(s.iat$countryres))
sort(table(s.iat$countrycit_num))
sort(table(s.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(s.iat$countrycit)
s.iat$countrycit <- gsub("  ", "", s.iat$countrycit)
s.iat$countrycit <- gsub(" ", "", s.iat$countrycit)
s.iat$countrycit <- factor(s.iat$countrycit)
s.iat$countrycit1 <- recode(s.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(s.iat$countrycit1))
s.iat$countrycit1 <- recode(s.iat$countrycit1, "'1' = 'US'")
s.iat$countrycit2 <- recode(s.iat$countrycit_num, "'United States' = 'US'; 'UNITED STATES' = 'US'")

s.iat$countrycit.new <- paste(s.iat$countrycit1, s.iat$countrycit2)
sort(table(s.iat$countrycit.new))
s.iat$countrycit.new <- gsub("NA ", "", s.iat$countrycit.new)
s.iat$countrycit.new <- gsub(" NA", "", s.iat$countrycit.new)
s.iat$countrycit.new <- factor(s.iat$countrycit.new)
s.iat$countrycit.new <- recode(s.iat$countrycit.new, "'US US' = 'US'")
sort(table(s.iat$countrycit.new))

table(s.iat$countrycit.new, s.iat$month)["US",] # complete data after 0602

levels(s.iat$countryres)
s.iat$countryres <- gsub("  ", "", s.iat$countryres)
s.iat$countryres <- gsub(" ", "", s.iat$countryres)
s.iat$countryres <- factor(s.iat$countryres)
s.iat$countryres1 <- recode(s.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(s.iat$countryres1))
s.iat$countryres1 <- recode(s.iat$countryres1, "'1' = 'US'")
s.iat$countryres2 <- recode(s.iat$countryres_num, "'United States' = 'US'; 'UNITED STATES' = 'US'")

s.iat$countryres.new <- paste(s.iat$countryres1, s.iat$countryres2)
s.iat$countryres.new <- gsub("NA ", "", s.iat$countryres.new)
s.iat$countryres.new <- gsub(" NA", "", s.iat$countryres.new)
s.iat$countryres.new <- factor(s.iat$countryres.new)
s.iat$countryres.new <- recode(s.iat$countryres.new, "'US US' = 'US'")
sort(table(s.iat$countryres.new))

table(s.iat$countryres.new, s.iat$month)["US",] # complete data after 0602

# how many US?
table(s.iat$countryres.new)["US"] # 1,233,300

# how many complete US?
table(s.iat$countryres.new[s.iat$session_status=="C   "])["US"] # 808,879

# keep a backup here
s.iat2 <- s.iat

# keep only complete participants ------------
s.iat <- s.iat[s.iat$session_status=="C   ",] # complete subjects = 1,239,671

# and those with complete IATs
s.iat <- s.iat[is.na(s.iat$D_biep.Straight_Good_all)==FALSE,] # complete IATs = 1,210,291

## keep only the variables of interest.
s.iat <- s.iat[c("session_id", "date", "month", "month.cent", "year", "age", "birthmonth", "birthyear",
                 "num", "num_002",
                 "sex", "birthsex", "genderidentity",
                 "sexuality", "sexuality_3", "sexuality_3new", "sexuality_4", "sexuality_5", "sexuality_5new",
                 "sexualityorigin",
                 "D_biep.Straight_Good_all", "Order", "Stimuli",
                 "att", "att_5", "att_7", 
                "countrycit", "countryres", "countrycit_num", "countryres_num", 
                "edu", "ethnic", "ethnicityomb",
                 "politicalid", "politicalid_7", "politicalid_6", "raceomb", "raceomb_002",
                 "religion", "religion2009", "religionid",
                 "postcodelong", "postcodenow")]


## dscore --------------------
names(s.iat)[which(names(s.iat)=="D_biep.Straight_Good_all")] <- "dscore"

## session id -----------------
s.iat$session_id <- as.character(s.iat$session_id)

## age ------------
table(s.iat$age) # not collected after mid-2016

# working with birthyear
table(s.iat$birthyear, s.iat$year) # this one picks up...
table(s.iat$birthyear) # combines codes that are real values and coded values

s.iat$birthyear.new <- factor(s.iat$birthyear)
levels(s.iat$birthyear.new) # missing 78:98

birthyears <- seq(2009, 1910, -1)
birthyears <- birthyears[-c(78:98)]
birthyears

levels(s.iat$birthyear.new)[1:79] <- birthyears
table(s.iat$birthyear.new)

# birthyear was introduced in 2016, so subtract birthyear from 2016 to get age
# get in the right order
s.iat$birthyear.new2 <- factor(s.iat$birthyear.new, levels = c(1910:2009))
# save as character
s.iat$birthyear.new2 <- as.character(s.iat$birthyear.new2)
# then as numeric
s.iat$birthyear.new2 <- as.numeric(s.iat$birthyear.new2)
# subtract
s.iat$birthyear.new2 <- 2016 - s.iat$birthyear.new2

table(s.iat$birthyear.new2)

## now combine
s.iat$age.new <- paste(s.iat$age, s.iat$birthyear.new2)

table(s.iat$age.new)
s.iat$age.new <- gsub("NA ", "", s.iat$age.new)
s.iat$age.new <- gsub(" NA", "", s.iat$age.new)

s.iat$age.new <- factor(s.iat$age.new)

table(s.iat$age.new)

s.iat$age.new <- factor(s.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
s.iat$age.new <- droplevels(s.iat$age.new)

table(s.iat$age.new)

s.iat$age.new <- as.character(s.iat$age.new)
s.iat$age.new <- as.numeric(s.iat$age.new)

table(s.iat$age.new, s.iat$year)
table(s.iat$age.new[s.iat$age.new>25], s.iat$month[s.iat$age.new>25]) # complete data

## num ------------
table(s.iat$num)
levels(s.iat$num)
levels(s.iat$num)[levels(s.iat$num)==".  " | levels(s.iat$num)== "   " | levels(s.iat$num)== "nul" | levels(s.iat$num)== "-99"] <- NA

table(s.iat$num, s.iat$month.cent) # shifting codes from 93 - 100 and 147- - 150, and then (for different reasons) from 151 onwards
table(s.iat$num, s.iat$month.cent)[6:12,]

## fix all the subparts...
s.iat$num93100 <- ifelse(s.iat$month.cent>=93 & s.iat$month.cent<=100, s.iat$num, NA)
table(s.iat$num93100, s.iat$month.cent)
table(s.iat$num93100, s.iat$month.cent)[3:8,]
levels(s.iat$num93100)

s.iat$num93100 <- factor(s.iat$num93100)
s.iat$num93100 <- recode(s.iat$num93100, "'6' = '0'; '7' = '0'; '8' = '1'; '9' = '2'; '10' = '3-5'; '11' = '3-5'; '12' = '6+'; '13' = '6+'")
table(s.iat$num93100, s.iat$month.cent)

s.iat$num147150 <- ifelse(s.iat$month.cent>=147 & s.iat$month.cent<=150, s.iat$num, NA)
table(s.iat$num147150, s.iat$month.cent)[2:7,]
s.iat$num147150 <- recode(s.iat$num147150, "'6' = '0'; '7' = '1'; '8' = '2'; '9' = '2'; '10' = '3-5'; '11' = '3-5'; '12' = '6+'; '13' = '6+'")
table(s.iat$num147150, s.iat$month.cent)

s.iat$num151 <- ifelse(s.iat$month.cent==151, s.iat$num, NA)
s.iat$num151 <- factor(s.iat$num151)
levels(s.iat$num151)
s.iat$num151 <- recode(s.iat$num151, "'1' = '0'; '2' = '1'; '3' = '2'; '4' = '3-5'; '5' = '6+';
                       '6' = '0'; '7' = '1'; '8' = '2'; '10' = '3-5'; '13' = '6+'")

s.iat$num152 <- ifelse(s.iat$month.cent>=152, s.iat$num, NA)
s.iat$num152 <- recode(s.iat$num152, "'1' = '0'; '2' = '1'; '3' = '2'; '4' = '3-5'; '5' = '6+'")
table(s.iat$num152, s.iat$month.cent)

s.iat$numallother <- ifelse(s.iat$month.cent>=101 & s.iat$month.cent<=146 | s.iat$month.cent <=92, s.iat$num, NA)
table(s.iat$numallother, s.iat$month.cent)
s.iat$numallother <- recode(s.iat$numallother, "'6' = '0'; '7' = '1'; '8' = '2'; '10' = '3-5'; '13' = '6+'")
table(s.iat$numallother, s.iat$month.cent)

## now we are going to paste them together in order.
## add in the 93 - 100
s.iat$num.new <- paste(s.iat$numallother, s.iat$num93100)

table(s.iat$num.new, s.iat$month)
s.iat$num.new <- gsub("NA ", "", s.iat$num.new)
s.iat$num.new <- gsub(" NA", "", s.iat$num.new)
s.iat$num.new <- factor(s.iat$num.new)
levels(s.iat$num.new)
s.iat$num.new <- recode(s.iat$num.new, "'NA' = NA")
table(s.iat$num.new)
table(s.iat$num.new, s.iat$month)

## add in the 147
s.iat$num.new <- paste(s.iat$num.new, s.iat$num147150)

table(s.iat$num.new, s.iat$month)
s.iat$num.new <- gsub("NA ", "", s.iat$num.new)
s.iat$num.new <- gsub(" NA", "", s.iat$num.new)
s.iat$num.new <- factor(s.iat$num.new)
levels(s.iat$num.new)
s.iat$num.new <- recode(s.iat$num.new, "'NA' = NA")
table(s.iat$num.new)
table(s.iat$num.new, s.iat$month)

## add in the 151
s.iat$num.new <- paste(s.iat$num.new, s.iat$num151)

table(s.iat$num.new, s.iat$month)
s.iat$num.new <- gsub("NA ", "", s.iat$num.new)
s.iat$num.new <- gsub(" NA", "", s.iat$num.new)
s.iat$num.new <- factor(s.iat$num.new)
levels(s.iat$num.new)
s.iat$num.new <- recode(s.iat$num.new, "'NA' = NA")
table(s.iat$num.new)
table(s.iat$num.new, s.iat$month)

## add in the 152+
s.iat$num.new <- paste(s.iat$num.new, s.iat$num152)

table(s.iat$num.new, s.iat$month)
s.iat$num.new <- gsub("NA ", "", s.iat$num.new)
s.iat$num.new <- gsub(" NA", "", s.iat$num.new)
s.iat$num.new <- factor(s.iat$num.new)
levels(s.iat$num.new)
s.iat$num.new <- recode(s.iat$num.new, "'NA' = NA")
table(s.iat$num.new)
table(s.iat$num.new, s.iat$month) # complete data


## sex ----------
table(s.iat$sex)
levels(s.iat$sex)[levels(s.iat$sex)=="." | levels(s.iat$sex)== " " | levels(s.iat$sex)== "3" | levels(s.iat$sex)== "4" | levels(s.iat$sex)== "n"] <- NA
s.iat$sex <- recode(s.iat$sex, "'f' = 'female'; 'm' = 'male'")

table(s.iat$birthsex)
s.iat$birthsex <- factor(s.iat$birthsex, labels = c("Male", "Female"))

s.iat$sex.new <- paste(s.iat$sex, s.iat$birthsex)

table(s.iat$sex.new)
s.iat$sex.new <- gsub("NA ", "", s.iat$sex.new)
s.iat$sex.new <- gsub(" NA", "", s.iat$sex.new)

table(s.iat$sex.new)
s.iat$sex.new <- factor(s.iat$sex.new)
s.iat$sex.new <- recode(s.iat$sex.new, "'NA' = NA")

table(s.iat$sex.new, s.iat$month.cent)

## sexuality ---------
table(s.iat$sexuality, s.iat$year) # falsely combines those scales below
table(s.iat$sexuality_3, s.iat$year) # Straight, Gay/Lesbian, Bisexual
table(s.iat$sexuality_3new, s.iat$year) # Hetero, Homo, Bi
table(s.iat$sexuality_4, s.iat$year) # Hetero, Homo, Bi, As
table(s.iat$sexuality_5, s.iat$year) # Hetero, Gay or lesbian, Bi, As, Qu
table(s.iat$sexuality_5new, s.iat$year) # Hetero, Homo, Bi, Queer, Other

# get the same order and labels.
s.iat$sexuality_3 <- factor(s.iat$sexuality_3, labels = c("Straight", "Gay/Lesbian", "Bisexual"))
s.iat$sexuality_3new <- factor(s.iat$sexuality_3new, labels = c("Straight", "Gay/Lesbian", "Bisexual"))
s.iat$sexuality_4 <- factor(s.iat$sexuality_4, labels = c("Straight", "Gay/Lesbian", "Bisexual", "Asexual"))
s.iat$sexuality_5 <- factor(s.iat$sexuality_5, labels = c("Straight", "Gay/Lesbian", "Bisexual", "Asexual", "Questioning"))
s.iat$sexuality_5new <- factor(s.iat$sexuality_5new, labels = c("Straight", "Gay/Lesbian", "Bisexual", "Queer", "Other"))

# combine all sexuality sets, we are only going to use the first three factor levels ("Straight", "Gay/Lesbian", "Bisexual") in the final analyses

s.iat$sexuality.new <- paste(s.iat$sexuality_3new, s.iat$sexuality_3)
table(s.iat$sexuality.new)
s.iat$sexuality.new <- gsub("NA ", "", s.iat$sexuality.new)
s.iat$sexuality.new <- gsub(" NA", "", s.iat$sexuality.new)
s.iat$sexuality.new <- recode(s.iat$sexuality.new, "'NA' = NA")
table(s.iat$sexuality.new)

s.iat$sexuality.new <- paste(s.iat$sexuality.new, s.iat$sexuality_4)
table(s.iat$sexuality.new)
s.iat$sexuality.new <- gsub("NA ", "", s.iat$sexuality.new)
s.iat$sexuality.new <- gsub(" NA", "", s.iat$sexuality.new)
s.iat$sexuality.new <- recode(s.iat$sexuality.new, "'NA' = NA")
table(s.iat$sexuality.new)

s.iat$sexuality.new <- paste(s.iat$sexuality.new, s.iat$sexuality_5)
table(s.iat$sexuality.new)
s.iat$sexuality.new <- gsub("NA ", "", s.iat$sexuality.new)
s.iat$sexuality.new <- gsub(" NA", "", s.iat$sexuality.new)
s.iat$sexuality.new <- recode(s.iat$sexuality.new, "'NA' = NA")
table(s.iat$sexuality.new)

s.iat$sexuality.new <- paste(s.iat$sexuality.new, s.iat$sexuality_5new)
table(s.iat$sexuality.new)
s.iat$sexuality.new <- gsub(" NA", "", s.iat$sexuality.new)
s.iat$sexuality.new <- gsub("NA ", "", s.iat$sexuality.new)

s.iat$sexuality.new <- recode(s.iat$sexuality.new, "'NA' = NA")
table(s.iat$sexuality.new)

s.iat$sexuality.new <- factor(s.iat$sexuality.new)
s.iat$sexuality.new <- relevel(s.iat$sexuality.new, "Straight")
table(s.iat$sexuality.new, s.iat$year)

# should reassign to a sexuality new 2, then make a sexuality new with only 3 levels (available all years)
s.iat$sexuality.new2 <- s.iat$sexuality.new

levels(s.iat$sexuality.new)[levels(s.iat$sexuality.new)== "Asexual" | levels(s.iat$sexuality.new)== "Questioning" | levels(s.iat$sexuality.new)== "Queer" | levels(s.iat$sexuality.new)== "Other"] <- NA
table(s.iat$sexuality.new, s.iat$year)

## race ----------
## no raceombmulti for sexuality.
## raceomb (with factor levels determined from documentation at OSF, matched to individual year data sets)
race <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
          "Black or African American", "White", "More than one race - Black/White", "More than one race - Other", "Other or Unknown")
table(s.iat$raceomb)
s.iat$raceomb <- factor(s.iat$raceomb)
levels(s.iat$raceomb)[levels(s.iat$raceomb)== "-999"] <- NA
s.iat$raceomb <- factor(s.iat$raceomb, labels = race)
table(s.iat$raceomb)

race2 <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
           "Black or African American", "White", "Other or Unknown", "More than one race - Other")
table(s.iat$raceomb_002)
s.iat$raceomb2 <- factor(s.iat$raceomb_002, labels = race2)
table(s.iat$raceomb2)

s.iat$raceomb.new <- paste(s.iat$raceomb, s.iat$raceomb2)
table(s.iat$raceomb.new)
s.iat$raceomb.new <- gsub(" NA", "", s.iat$raceomb.new)
s.iat$raceomb.new <- factor(s.iat$raceomb.new)

levels(s.iat$raceomb.new)
s.iat$raceomb.new <- recode(s.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                            'Black or African American Black or African American' = 'Black or African American';
                            'East Asian East Asian' = 'East Asian';
                            'More than one race - Black/White Other or Unknown' = 'Other or Unknown';
                            'More than one race - Other More than one race - Other' = 'More than one race - Other';
                            'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                            'South Asian South Asian' = 'South Asian';
                            'White White' = 'White';
                            'NA' = NA")
table(s.iat$raceomb.new, s.iat$year)

#  ethnic (to match raceomb codes)
table(s.iat$ethnic)
s.iat$ethnic.new <- recode(s.iat$ethnic, "'WhiteNot of Hispanic Origin' = 'White'; 'BlackNot of Hispanic Origin' = 'Black or African American';
                           'Asian or Pacific Islander' = 'Asian'; 'Multiracial (Black and White)' = 'More than one race - Black/White';
                           'Multiracial (other)' = 'More than one race - Other'; 'North American Indian or Alaskan Native' = 'American Indian/Alaska Native';
                           'Hispanic' = 'Other or Unknown'")

s.iat$race.new <- recode(s.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(s.iat$ethnic.new)
table(s.iat$race.new)

s.iat$ethnic.race.new <- paste(s.iat$ethnic.new, s.iat$race.new)
table(s.iat$ethnic.race.new)

s.iat$ethnic.race.new <- gsub("NA ", "", s.iat$ethnic.race.new)
s.iat$ethnic.race.new <- gsub(" NA", "", s.iat$ethnic.race.new)
s.iat$ethnic.race.new <- recode(s.iat$ethnic.race.new, "'NA' = NA")

s.iat$ethnic.race.new <- factor(s.iat$ethnic.race.new)
levels(s.iat$ethnic.race.new)
table(s.iat$ethnic.race.new, s.iat$month) # provides 2004 data as well...


## ethnicityomb ----------
## (again, with values determined from documentation and single year codes)
table(s.iat$ethnicityomb)
levels(s.iat$ethnicityomb)[levels(s.iat$ethnicityomb)== "-999"] <- NA
s.iat$ethnicityomb <- factor(s.iat$ethnicityomb)
table(s.iat$ethnicityomb, s.iat$year)


## explicit attitudes ----------------
## recode attitude
table(s.iat$att, s.iat$month.cent) # falsely combines 5 and 7.
table(s.iat$att_5, s.iat$month.cent)
table(s.iat$att_7, s.iat$month.cent)


# add two levels to att 5
levels(s.iat$att_5) <- c(levels(s.iat$att_5), "I slightly prefer Gay People to Straight People.", "I slightly prefer Straight People to Gay People.")

# get the right order
s.iat$att_5 <- factor(s.iat$att_5, levels = c("I strongly prefer Gay People to Straight People.", "I moderately prefer Gay People to Straight People.",
                                              "I slightly prefer Gay People to Straight People.", "I like Straight People and Gay People equally.",
                                              "I slightly prefer Straight People to Gay People.", "I moderately prefer Straight People to Gay People.",
                                              "I strongly prefer Straight People to Gay People."))
# get the right order for att 7
s.iat$att_7 <- factor(s.iat$att_7, levels = c("I strongly prefer Gay People to Straight People.", "I moderately prefer Gay People to Straight People.",
                                              "I slightly prefer Gay People to Straight People.", "I like Straight People and Gay People equally.",
                                              "I slightly prefer Straight People to Gay People.", "I moderately prefer Straight People to Gay People.",
                                              "I strongly prefer Straight People to Gay People."))

s.iat$att.new <- paste(s.iat$att_5, s.iat$att_7)
table(s.iat$att.new)

s.iat$att.new <- gsub("NA ", "", s.iat$att.new)
s.iat$att.new <- gsub(" NA", "", s.iat$att.new)
s.iat$att.new <- recode(s.iat$att.new, "'NA' = NA")
table(s.iat$att.new)

s.iat$att.new <- factor(s.iat$att.new, levels = c("I strongly prefer Gay People to Straight People.", "I moderately prefer Gay People to Straight People.",
                                                  "I slightly prefer Gay People to Straight People.", "I like Straight People and Gay People equally.",
                                                  "I slightly prefer Straight People to Gay People.", "I moderately prefer Straight People to Gay People.",
                                                  "I strongly prefer Straight People to Gay People."))
levels(s.iat$att.new)
table(s.iat$att.new, s.iat$year) # complete data
table(s.iat$att.new, s.iat$month.cent) # complete data

## education -----------------
table(s.iat$edu, s.iat$year)

s.iat$edu <- factor(s.iat$edu)
levels(s.iat$edu)[levels(s.iat$edu)== "-999"] <- NA

s.iat$edu <- factor(s.iat$edu, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                          "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                          "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))

# get the correct order
s.iat$edu.new <- factor(s.iat$edu, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                    "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                    "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(s.iat$edu.new, s.iat$month)


## political ideology --------------
table(s.iat$politicalid) # falsely combines pol 6 and pol 7
table(s.iat$politicalid_6, s.iat$year)
table(s.iat$politicalid_7, s.iat$month) # changed in 0612 (appears to have previously been a 6-category scale)

## remove the extra level from political id 7
levels(s.iat$politicalid_7)[levels(s.iat$politicalid_7)== "-999"] <- NA

# first, politicalid_6
# get the right labels for pol 6
s.iat$politicalid_6 <- factor(s.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                              "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(s.iat$politicalid_6) <- c(levels(s.iat$politicalid_6), "Neutral")

# get the right order
s.iat$politicalid_6 <- factor(s.iat$politicalid_6, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                              "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(s.iat$politicalid_6)

# now subset political id into a pre and post 0612 component
s.iat$politicalid6NEW <- ifelse(s.iat$month.cent<=34, s.iat$politicalid_7, NA)
table(s.iat$politicalid6NEW, s.iat$month)

s.iat$politicalid7NEW <- ifelse(s.iat$month.cent>34, s.iat$politicalid_7, NA)
table(s.iat$politicalid7NEW, s.iat$month)

# for politicalid6NEW, recode the variables like politicalid 6 above.
table(s.iat$politicalid6NEW)

# get the right labels for pol 6
s.iat$politicalid6NEW <- factor(s.iat$politicalid6NEW, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                              "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(s.iat$politicalid6NEW) <- c(levels(s.iat$politicalid6NEW), "Neutral")

# get the right order
s.iat$politicalid6NEW <- factor(s.iat$politicalid6NEW, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                              "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(s.iat$politicalid6NEW)


# now continue with the actual politicalid 7 variable
pol <- levels(s.iat$politicalid_6)

s.iat$politicalid7NEW <- factor(s.iat$politicalid7NEW, labels = pol)
table(s.iat$politicalid7NEW)

# combine the raw scales.
s.iat$pol.new <- paste(s.iat$politicalid6NEW, s.iat$politicalid7NEW)
table(s.iat$pol.new)
s.iat$pol.new <- gsub("NA ", "", s.iat$pol.new)
s.iat$pol.new <- gsub(" NA", "", s.iat$pol.new)
s.iat$pol.new <- recode(s.iat$pol.new, "'NA' = NA")

table(s.iat$pol.new, s.iat$month)

# add on political id 6
s.iat$pol.new <- paste(s.iat$politicalid_6, s.iat$pol.new)
table(s.iat$pol.new)
s.iat$pol.new <- gsub("NA ", "", s.iat$pol.new)
s.iat$pol.new <- gsub(" NA", "", s.iat$pol.new)
s.iat$pol.new <- recode(s.iat$pol.new, "'NA' = NA")

table(s.iat$pol.new, s.iat$month)

# get the right order
s.iat$pol.new <- factor(s.iat$pol.new, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                  "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(s.iat$pol.new, s.iat$year) # complete data now
s.iat$politicalid <- s.iat$pol.new
table(s.iat$politicalid, s.iat$month) # complete data now (except 2005)

## country of citizenship and residence ---------------
## recode country cit and country res
sort(table(s.iat$countrycit))
sort(table(s.iat$countryres))
sort(table(s.iat$countrycit_num))
sort(table(s.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(s.iat$countrycit)
s.iat$countrycit <- gsub("  ", "", s.iat$countrycit)
s.iat$countrycit <- gsub(" ", "", s.iat$countrycit)
s.iat$countrycit <- factor(s.iat$countrycit)
s.iat$countrycit1 <- recode(s.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(s.iat$countrycit1))
s.iat$countrycit1 <- recode(s.iat$countrycit1, "'1' = 'US'")
s.iat$countrycit2 <- recode(s.iat$countrycit_num, "'United States' = 'US'; 'UNITED STATES' = 'US'")

s.iat$countrycit.new <- paste(s.iat$countrycit1, s.iat$countrycit2)
sort(table(s.iat$countrycit.new))
s.iat$countrycit.new <- gsub("NA ", "", s.iat$countrycit.new)
s.iat$countrycit.new <- gsub(" NA", "", s.iat$countrycit.new)
s.iat$countrycit.new <- factor(s.iat$countrycit.new)
s.iat$countrycit.new <- recode(s.iat$countrycit.new, "'US US' = 'US'")
sort(table(s.iat$countrycit.new))

table(s.iat$countrycit.new, s.iat$month)["US",] # complete data after 0602

levels(s.iat$countryres)
s.iat$countryres <- gsub("  ", "", s.iat$countryres)
s.iat$countryres <- gsub(" ", "", s.iat$countryres)
s.iat$countryres <- factor(s.iat$countryres)
s.iat$countryres1 <- recode(s.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(s.iat$countryres1))
s.iat$countryres1 <- recode(s.iat$countryres1, "'1' = 'US'")
s.iat$countryres2 <- recode(s.iat$countryres_num, "'United States' = 'US'; 'UNITED STATES' = 'US'")

s.iat$countryres.new <- paste(s.iat$countryres1, s.iat$countryres2)
s.iat$countryres.new <- gsub("NA ", "", s.iat$countryres.new)
s.iat$countryres.new <- gsub(" NA", "", s.iat$countryres.new)
s.iat$countryres.new <- factor(s.iat$countryres.new)
s.iat$countryres.new <- recode(s.iat$countryres.new, "'US US' = 'US'")
sort(table(s.iat$countryres.new))

table(s.iat$countryres.new, s.iat$month)["US",] # complete data after 0602

## zipcodes ----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
table(s.iat$postcodelong)
table(s.iat$postcodenow)

# make it a character to use regular expressions
s.iat$zip <- as.character(s.iat$postcodelong)

s.iat$zip <- gsub("[[:punct:]]", "", s.iat$zip) # remove punctuation
s.iat$zip <- gsub("[[:space:]]", "", s.iat$zip) # remove all white space
s.iat$zip <- gsub("[^0-9]", "", s.iat$zip) # remove all non-numeric characters

s.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", s.iat$zip) # remove all strings shorter than 5
s.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", s.iat$zip1) # remove all strings longer than 5, shorter than 9
s.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", s.iat$zip1) # remove all strings longer 9

s.iat$zip1 <- as.character(s.iat$zip1)

## include geographic data (from zip codes)
s.iat.places <- merge(s.iat, zips, by = "zip1", all = TRUE)


## data subsetting -----------------------------------------------------------------------------------------------------------------
# subset to US participants (by countryres.new), allows for retaining the most participants over time
s.iatUSres <- s.iat.places[s.iat.places$countryres.new=="US",]
sort(table(s.iatUSres$countryres.new))
## total with US country residence (and NAs omitted) = 808,879

## now keep only those with complete data for the following:
# dscore, att.new,
# sex, age, Order, ethnic.sex.new, politicalid, edu, sexuality
sex.full <- s.iatUSres[is.na(s.iatUSres$dscore)==FALSE,] # 793,707
sex.full <- sex.full[is.na(sex.full$att.new)==FALSE,] #  759,197

sex.full.demogs <- sex.full[is.na(sex.full$sex.new)==FALSE,] # 753,655
sex.full.demogs <- sex.full.demogs[is.na(sex.full.demogs$age.new)==FALSE,] # 749,007
sex.full.demogs <- sex.full.demogs[is.na(sex.full.demogs$ethnic.race.new)==FALSE,] # 739,492
sex.full.demogs <- sex.full.demogs[is.na(sex.full.demogs$politicalid)==FALSE,] # 725,752
sex.full.demogs <- sex.full.demogs[is.na(sex.full.demogs$edu.new)==FALSE,] # 719,925
sex.full.demogs <- sex.full.demogs[is.na(sex.full.demogs$sexuality.new)==FALSE,] # 692,425

table(sex.full.demogs$month)

# also include the following variables: num.new
sex.full.demogs <- sex.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                     "age.new", "sex.new", "ethnic.race.new", "raceomb.new", "ethnicityomb",
                                     "politicalid", "edu.new", "sexuality.new",
                                     "Order", "num.new", "Stimuli", 
                                     "countrycit.new", "countryres.new",
                                     "zip1", "State", "Place.Name", "State.Abbreviation", "County")]

table(sex.full.demogs$month)
table(sex.full.demogs$month.cent)


##############################################################################################################################################
## CLEAN THE SKIN-TONE DATA ---------------------------
skin.iat <- read.spss("Skin IAT.2004-2016.sav",
                      to.data.frame = TRUE) # 1,769,274

# get the date in the right format
skin.iat$date <- as.chron(ISOdate(1582, 10, 14) + skin.iat$date) 
skin.iat$date <- as.Date(skin.iat$date)

# get a month variable
skin.iat$month <- format(skin.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (add 3 months to make month.cent 1 equivalent to January, 2004)
table(skin.iat$month) 
skin.iat$month.cent <- as.numeric(factor(skin.iat$month)) + 3

table(skin.iat$month)
table(skin.iat$month.cent)

# there is only one participant in month.cent = 4, so remove them.
skin.iat <- skin.iat[skin.iat$month.cent != 4,]

# keep only those with month.cent > 0 and < 157
skin.iat <- skin.iat[skin.iat$month.cent > 0,]
skin.iat <- skin.iat[skin.iat$month.cent < 157,]

## for the data from 2004 - 2016, the new total is 1,769,180 participants.

## how many US? ---------
## countrycit and countryres
sort(table(skin.iat$countrycit))
sort(table(skin.iat$countryres))
sort(table(skin.iat$countrycit_num))
sort(table(skin.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(skin.iat$countrycit)
skin.iat$countrycit <- gsub("  ", "", skin.iat$countrycit)
skin.iat$countrycit <- gsub(" ", "", skin.iat$countrycit)
skin.iat$countrycit <- factor(skin.iat$countrycit)
skin.iat$countrycit1 <- recode(skin.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(skin.iat$countrycit1))
skin.iat$countrycit1 <- recode(skin.iat$countrycit1, "'1' = 'US'")
skin.iat$countrycit2 <- recode(skin.iat$countrycit_num, "'1' = 'US'")

skin.iat$countrycit.new <- paste(skin.iat$countrycit1, skin.iat$countrycit2)
sort(table(skin.iat$countrycit.new))
skin.iat$countrycit.new <- gsub("NA ", "", skin.iat$countrycit.new)
skin.iat$countrycit.new <- gsub(" NA", "", skin.iat$countrycit.new)
skin.iat$countrycit.new <- factor(skin.iat$countrycit.new)
skin.iat$countrycit.new <- recode(skin.iat$countrycit.new, "'US US' = 'US'")
sort(table(skin.iat$countrycit.new))

table(skin.iat$countrycit.new, skin.iat$month)["US",] # complete data

levels(skin.iat$countryres)
skin.iat$countryres <- gsub("  ", "", skin.iat$countryres)
skin.iat$countryres <- gsub(" ", "", skin.iat$countryres)
skin.iat$countryres <- factor(skin.iat$countryres)
skin.iat$countryres1 <- recode(skin.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(skin.iat$countryres1))
skin.iat$countryres1 <- recode(skin.iat$countryres1, "'1' = 'US'")
skin.iat$countryres2 <- recode(skin.iat$countryres_num, "'1' = 'US'")

skin.iat$countryres.new <- paste(skin.iat$countryres1, skin.iat$countryres2)
skin.iat$countryres.new <- gsub("NA ", "", skin.iat$countryres.new)
skin.iat$countryres.new <- gsub(" NA", "", skin.iat$countryres.new)
skin.iat$countryres.new <- factor(skin.iat$countryres.new)
skin.iat$countryres.new <- recode(skin.iat$countryres.new, "'US US' = 'US'")
sort(table(skin.iat$countryres.new))

table(skin.iat$countryres.new, skin.iat$month)["US",] # complete data after 0602

# how many US?
table(skin.iat$countryres.new)["US"] # 893,483

# how many complete US?
table(skin.iat$countryres.new[skin.iat$session_status=="C   "])["US"] # 620,797

# keep a backup here
skin.iat2 <- skin.iat

# keep only complete subjects -------
skin.iat <- skin.iat[skin.iat$session_status=="C   ",] # complete subjects = 786,821

## keep complete dscore
skin.iat <- skin.iat[is.na(skin.iat$D_biep.LightSkin_Good_all)==FALSE,] # complete IAT = 753,237

# keep only the variables of interest
skin.iat <- skin.iat[c("session_id", "date", "year", "month", "month.cent", "age", "birthyear",
                       "num", "num_002",  "sex", "birthsex", "genderidentity",
                       "ethnic", "raceomb", "raceomb_002", "raceombmulti", "ethnicityomb",
                       "D_biep.LightSkin_Good_all", "Order", "Stimuli",
                       "att", "att_5", "att_7", "jatt", "xatt", "att7",
                       "countrycit", "countryres", "countrycit_num", "countryres_num", 
                       "edu", "edu_13", "edu_14",  
                       "politicalid", "politicalid_7", "politicalid_6",
                       "jdem_econpoliticalid", "jdem_socialpoliticalid", "jdem_partyid",
                       "postcodelong", "postcodenow")]

## dscore --------
names(skin.iat)[which(names(skin.iat)=="D_biep.LightSkin_Good_all")] <- "dscore"

## session id  --------
skin.iat$session_id <- as.character(skin.iat$session_id)

## age ----------
table(skin.iat$age, skin.iat$year) # not collected after mid-2016

# working with birthyear
table(skin.iat$birthyear, skin.iat$year) # this one picks up...
table(skin.iat$birthyear) # only uses real year values...

skin.iat$birthyear.new <- 2016 - skin.iat$birthyear

table(skin.iat$birthyear.new)

## now combine
skin.iat$age.new <- paste(skin.iat$age, skin.iat$birthyear.new)

table(skin.iat$age.new)
skin.iat$age.new <- gsub("NA ", "", skin.iat$age.new)
skin.iat$age.new <- gsub(" NA", "", skin.iat$age.new)

skin.iat$age.new <- factor(skin.iat$age.new)

skin.iat$age.new <- factor(skin.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
skin.iat$age.new <- droplevels(skin.iat$age.new)

table(skin.iat$age.new)

skin.iat$age.new <- as.character(skin.iat$age.new)
skin.iat$age.new <- as.numeric(skin.iat$age.new)

table(skin.iat$age.new[skin.iat$age.new>25], skin.iat$month[skin.iat$age.new>25]) # complete data

## num ----------
table(skin.iat$num)
skin.iat$num <- gsub("  ", "", skin.iat$num)
skin.iat$num <- factor(skin.iat$num)
levels(skin.iat$num)[levels(skin.iat$num)=="." | levels(skin.iat$num)== " " | levels(skin.iat$num)== "nul" | levels(skin.iat$num)== "-99"] <- NA
skin.iat$num <- recode(skin.iat$num, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")

table(skin.iat$num_002)
skin.iat$num_002 <- factor(skin.iat$num_002)
levels(skin.iat$num_002)[levels(skin.iat$num_002)== "-99"] <- NA

skin.iat$num.new <- paste(skin.iat$num, skin.iat$num_002)

table(skin.iat$num.new)
skin.iat$num.new <- gsub("NA ", "", skin.iat$num.new)
skin.iat$num.new <- gsub(" NA", "", skin.iat$num.new)

skin.iat$num.new <- factor(skin.iat$num.new)
levels(skin.iat$num.new)
skin.iat$num.new <- recode(skin.iat$num.new, "'1 0' = '0'; '2 1' = '1'; '3-5 3-5' = '3-5'; '3-5 2' = '2';
                           '3-5 6+' = '6+ ' ; 'NA' = NA")

table(skin.iat$num.new)
table(skin.iat$num.new, skin.iat$month) # complete data

## sex -----------
## refactor sex (only female and male, no other)
table(skin.iat$sex)
levels(skin.iat$sex)[levels(skin.iat$sex)=="." | levels(skin.iat$sex)== "1" | levels(skin.iat$sex)== "n" | levels(skin.iat$sex)== "1"] <- NA
skin.iat$sex <- recode(skin.iat$sex, "'f' = 'Female'; 'm' = 'Male'; 'F' = 'Female'")

table(skin.iat$birthsex)
skin.iat$birthsex <- factor(skin.iat$birthsex, labels = c("Male", "Female"))

skin.iat$sex.new <- paste(skin.iat$sex, skin.iat$birthsex)

table(skin.iat$sex.new)
skin.iat$sex.new <- gsub("NA ", "", skin.iat$sex.new)
skin.iat$sex.new <- gsub(" NA", "", skin.iat$sex.new)

table(skin.iat$sex.new)
skin.iat$sex.new <- factor(skin.iat$sex.new)
skin.iat$sex.new <- recode(skin.iat$sex.new, "'NA' = NA")

table(skin.iat$sex.new, skin.iat$year)
table(skin.iat$sex.new, skin.iat$month) # complete data

## race ------------
table(skin.iat$raceomb, skin.iat$month)[4:9,]
table(skin.iat$raceomb_002, skin.iat$month)[3:8,] # 1607 onwards
table(skin.iat$raceombmulti, skin.iat$month) # 1607 onwards

## raceomb (with factor levels determined from documentation at OSF, matched to individual year data sets)
race <- c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
          "Black or African American", "White", "More than one race - Black/White", "More than one race - Other", "Other or Unknown")
table(skin.iat$raceomb)

skin.iat$raceomb <- factor(skin.iat$raceomb)
levels(skin.iat$raceomb)[levels(skin.iat$raceomb)== "-999"] <- NA
skin.iat$raceomb1 <- factor(skin.iat$raceomb, labels = race)

table(skin.iat$raceomb1)

table(skin.iat$raceombmulti) # if participant includes 5 and 6 = More than one race - B/W; all others = More than one race - Other
skin.iat$raceomb2 <- factor(skin.iat$raceombmulti)
levels(skin.iat$raceomb2) <- trimws(levels(skin.iat$raceomb2), which = "right")
skin.iat$raceomb2 <- recode(skin.iat$raceomb2, "'[5,6]' = 'More than one race - Black/White'; '[5]' = 'Black or African American'; 
                           '[6]' = 'White'; '[1]' = 'American Indian/Alaska Native'; '[2]' = 'East Asian';
                           '[3]' = 'South Asian'; '[4]' = 'Native Hawaiian or other Pacific Islander'; 
                           '[7]' = 'Other or Unknown'; '' = NA; '-999' = NA; '.' = NA; '[]' = NA; else = 'More than one race - Other'")
table(skin.iat$raceomb2)

skin.iat$raceomb.new <- paste(skin.iat$raceomb1, skin.iat$raceomb2)
table(skin.iat$raceomb.new)
skin.iat$raceomb.new <- gsub(" NA", "", skin.iat$raceomb.new)
skin.iat$raceomb.new <- factor(skin.iat$raceomb.new)

levels(skin.iat$raceomb.new)
skin.iat$raceomb.new <- recode(skin.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'NA White' = 'White'; 'American Indian/Alaska Native White' = 'American Indian/Alaska Native';
                              'NA' = NA")
levels(skin.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(skin.iat$raceomb.new)), "Other or Unknown",
                                      levels(skin.iat$raceomb.new))
levels(skin.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(skin.iat$raceomb.new)), "More than one race - Other",
                                      levels(skin.iat$raceomb.new))
levels(skin.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(skin.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(skin.iat$raceomb.new))

table(skin.iat$raceomb.new, skin.iat$year)
table(skin.iat$raceomb.new, skin.iat$month)[3:9,] # complete data after 2006.



table(skin.iat$raceomb_002)
# drop the Multiracial level (included in the raceombmulti)
skin.iat$raceomb3 <- factor(skin.iat$raceomb_002, levels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                                           "Black or African American", "White", "Other or unknown"),
                           labels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                      "Black or African American", "White", "Other or Unknown"))
table(skin.iat$raceomb3)

skin.iat$raceomb.new <- paste(skin.iat$raceomb.new, skin.iat$raceomb3)
table(skin.iat$raceomb.new)
skin.iat$raceomb.new <- gsub(" NA", "", skin.iat$raceomb.new)
skin.iat$raceomb.new <- factor(skin.iat$raceomb.new)
table(skin.iat$raceomb.new)

skin.iat$raceomb.new <- recode(skin.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                              'Black or African American Black or African American' = 'Black or African American';
                              'East Asian East Asian' = 'East Asian';
                              'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                              'More than one race - Other More than one race - Other' = 'More than one race - Other';
                              'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                              'South Asian South Asian' = 'South Asian';
                              'White White' = 'White'; 'Other or Unknown Other or Unknown'  = 'Other or Unknown';
                              'NA' = NA")
table(skin.iat$raceomb.new, skin.iat$month)[4:9,]
levels(skin.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(skin.iat$raceomb.new)), "More than one race - Black/White",
                                      levels(skin.iat$raceomb.new))
levels(skin.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(skin.iat$raceomb.new)), "More than one race - Other",
                                      levels(skin.iat$raceomb.new))
levels(skin.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(skin.iat$raceomb.new)), "Other or Unknown",
                                      levels(skin.iat$raceomb.new))


table(skin.iat$raceomb.new, skin.iat$month)


#  ethnic (to match raceomb codes)
table(skin.iat$ethnic, skin.iat$month) # only to 0609
skin.iat$ethnic.new <- recode(skin.iat$ethnic, "'White-Not of Hispanic Origin' = 'White'; 'Black-Not of Hispanic Origin' = 'Black or African American';
                             'Asian or Pacific Islander' = 'Asian'; 'Multi-racial (Black and White)' = 'More than one race - Black/White';
                             'Multi-racial (other)' = 'More than one race - Other'; 'North American Indian or First Nation' = 'American Indian/Alaska Native';
                             'Hispanic' = 'Other or Unknown'")

skin.iat$race.new <- recode(skin.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(skin.iat$ethnic.new)
table(skin.iat$race.new)

skin.iat$ethnic.race.new <- paste(skin.iat$ethnic.new, skin.iat$race.new)
table(skin.iat$ethnic.race.new)

skin.iat$ethnic.race.new <- gsub("NA ", "", skin.iat$ethnic.race.new)
skin.iat$ethnic.race.new <- gsub(" NA", "", skin.iat$ethnic.race.new)
skin.iat$ethnic.race.new <- recode(skin.iat$ethnic.race.new, "'NA' = NA")

skin.iat$ethnic.race.new <- factor(skin.iat$ethnic.race.new)
levels(skin.iat$ethnic.race.new)
table(skin.iat$ethnic.race.new, skin.iat$year)
table(skin.iat$ethnic.race.new, skin.iat$month) # complete data from 2004 - 2016
table(skin.iat$ethnic.race.new, skin.iat$month)[3:7,]

## ethnicity ----------
## ethnicityomb (again, with values determined from documentation and single year codes)
table(skin.iat$ethnicityomb, skin.iat$month)

levels(skin.iat$ethnicityomb)[levels(skin.iat$ethnicityomb)=="-999"] <- NA

skin.iat$ethnicityomb <- factor(skin.iat$ethnicityomb, labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown"))
table(skin.iat$ethnicityomb, skin.iat$year)
table(skin.iat$ethnicityomb, skin.iat$month) # complete data after 0609


## explicit attitudes -----------
table(skin.iat$att) # falsely combines 5 and 7.
table(skin.iat$att_5, skin.iat$month) # until 0612
table(skin.iat$att_7, skin.iat$month) # 0701 until 1002
table(skin.iat$jatt, skin.iat$month) # to be combined with the new att5 and att7 combination (1002 - 1609)
table(skin.iat$xatt, skin.iat$month) # ** reverse coded, will recode and then combine with att5 and att7 (1002 - 1004)
table(skin.iat$att7, skin.iat$month) # 1610 - 1612

# get the right labels for att 5
skin.iat$att_5 <- recode(skin.iat$att_5, "'I strongly prefer Dark Skinned People to Light Skinned People' = 'I strongly prefer Dark Skinned People to Light Skinned People.';
                         'I like Dark Skinned People and Light Skinned People equally' = 'I like Dark Skinned People and Light Skinned People equally.'")

# add two levels to att 5
levels(skin.iat$att_5) <- c(levels(skin.iat$att_5), "I slightly prefer Dark Skinned People to Light Skinned People.", "I slightly prefer Light Skinned People to Dark Skinned People.")

# get the right order
skin.iat$att_5 <- factor(skin.iat$att_5, levels = c("I strongly prefer Dark Skinned People to Light Skinned People.", "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                    "I slightly prefer Dark Skinned People to Light Skinned People.", "I like Dark Skinned People and Light Skinned People equally.",
                                                    "I slightly prefer Light Skinned People to Dark Skinned People.", "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                    "I strongly prefer Light Skinned People to Dark Skinned People."))

# get the right order for the xatt variable
skin.iat$xattNew <- factor(skin.iat$xatt, labels = c("I strongly prefer Light Skinned People to Dark Skinned People.", "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                    "I slightly prefer Light Skinned People to Dark Skinned People.", "I like Dark Skinned People and Light Skinned People equally.",
                                                    "I slightly prefer Dark Skinned People to Light Skinned People.", "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                    "I strongly prefer Dark Skinned People to Light Skinned People."))


skin.iat$att.new <- paste(skin.iat$att_5, skin.iat$att_7)
table(skin.iat$att.new)
skin.iat$att.new <- gsub("NA ", "", skin.iat$att.new)
skin.iat$att.new <- gsub(" NA", "", skin.iat$att.new)
skin.iat$att.new <- recode(skin.iat$att.new, "'NA' = NA")
table(skin.iat$att.new)

## add on jatt
skin.iat$att.new <- paste(skin.iat$att.new, skin.iat$jatt)
skin.iat$att.new <- gsub("NA ", "", skin.iat$att.new)
skin.iat$att.new <- gsub(" NA", "", skin.iat$att.new)
skin.iat$att.new <- recode(skin.iat$att.new, "'NA' = NA")
table(skin.iat$att.new)

## add on xatt
skin.iat$att.new <- paste(skin.iat$att.new, skin.iat$xattNew)
skin.iat$att.new <- gsub("NA ", "", skin.iat$att.new)
skin.iat$att.new <- gsub(" NA", "", skin.iat$att.new)
skin.iat$att.new <- recode(skin.iat$att.new, "'NA' = NA")
table(skin.iat$att.new)

# add on att7 (2016 data)
table(skin.iat$att7)
skin.iat$att71 <- factor(skin.iat$att7, labels = c("I strongly prefer Dark Skinned People to Light Skinned People.", "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                     "I slightly prefer Dark Skinned People to Light Skinned People.", "I like Dark Skinned People and Light Skinned People equally.",
                                                     "I slightly prefer Light Skinned People to Dark Skinned People.", "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                     "I strongly prefer Light Skinned People to Dark Skinned People."))

skin.iat$att.new <- paste(skin.iat$att.new, skin.iat$att71)
skin.iat$att.new <- gsub("NA ", "", skin.iat$att.new)
skin.iat$att.new <- gsub(" NA", "", skin.iat$att.new)
skin.iat$att.new <- recode(skin.iat$att.new, "'NA' = NA")
table(skin.iat$att.new)


## get in the right order
skin.iat$att.new <- factor(skin.iat$att.new, levels = c("I strongly prefer Dark Skinned People to Light Skinned People.", "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                        "I slightly prefer Dark Skinned People to Light Skinned People.", "I like Dark Skinned People and Light Skinned People equally.",
                                                        "I slightly prefer Light Skinned People to Dark Skinned People.", "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                        "I strongly prefer Light Skinned People to Dark Skinned People."))
levels(skin.iat$att.new)
table(skin.iat$att.new, skin.iat$year) # complete data
table(skin.iat$att.new, skin.iat$month) # complete data


## education -----------
table(skin.iat$edu)
table(skin.iat$edu_13, skin.iat$month)
table(skin.iat$edu_14, skin.iat$month)

# add one level to edu 13: MBA
skin.iat$edu_131 <- skin.iat$edu_13
levels(skin.iat$edu_131) <- c(levels(skin.iat$edu_13), "M.B.A.")
table(skin.iat$edu_131)

# factor and label edu 14
table(skin.iat$edu_14)
levels(skin.iat$edu_14)[levels(skin.iat$edu_14)=="-999"] <- NA
skin.iat$edu_141 <- factor(skin.iat$edu_14, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                       "associate's degree", "bachelor's degree", "some graduate school", "master's degree",
                                                       "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
skin.iat$edu.new <- paste(skin.iat$edu_131, skin.iat$edu_141)
skin.iat$edu.new <- gsub("NA ", "", skin.iat$edu.new)
skin.iat$edu.new <- gsub(" NA", "", skin.iat$edu.new)
skin.iat$edu.new <- recode(skin.iat$edu.new, "'NA' = NA")

# get the right order
skin.iat$edu.new <- factor(skin.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                        "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                        "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
table(skin.iat$edu.new, skin.iat$month) # complete data
table(skin.iat$edu.new, skin.iat$year) # complete data


## political ideology ---------
## recode and combine politicalid
table(skin.iat$politicalid) # falsely combines pol 6 and pol 7
table(skin.iat$politicalid_6, skin.iat$month)
table(skin.iat$politicalid_7, skin.iat$month)

# get the right labels for pol 6
skin.iat$politicalid_6 <- factor(skin.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                    "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add one level to pol 6
levels(skin.iat$politicalid_6) <- c(levels(skin.iat$politicalid_6), "Neutral")

# add labels for pol 7
levels(skin.iat$politicalid_7)[levels(skin.iat$politicalid_7)=="-999"] <- NA
skin.iat$politicalid_7 <- factor(skin.iat$politicalid_7, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", "Neutral",
                                                                    "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add in the other variables...
table(skin.iat$jdem_econpoliticalid, skin.iat$month.cent) # frequencies look like there was a change in 2011.
table(skin.iat$jdem_socialpoliticalid, skin.iat$month.cent) # frequencies look like there was a change in 2011.


## subset to a 2011 component and not
skin.iat$econpol.2011 <- ifelse(skin.iat$month.cent<=87, skin.iat$jdem_econpoliticalid, NA)
table(skin.iat$econpol.2011, skin.iat$month)

skin.iat$socipol.2011 <- ifelse(skin.iat$month.cent<=87, skin.iat$jdem_socialpoliticalid, NA)
table(skin.iat$socipol.2011, skin.iat$month)

skin.iat$econpol.2011 <- factor(skin.iat$econpol.2011, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                             "Neutral", "Slightly Liberal", "Moderately Liberal",
                                                                          "Strongly Liberal"))
skin.iat$socipol.2011 <- factor(skin.iat$socipol.2011, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                  "Neutral", "Slightly Liberal", "Moderately Liberal",
                                                                  "Strongly Liberal"))

skin.iat$econpol.post11 <- ifelse(skin.iat$month.cent>87, skin.iat$jdem_econpoliticalid, NA)
table(skin.iat$econpol.post11, skin.iat$month)
skin.iat$socipol.post11 <- ifelse(skin.iat$month.cent>87, skin.iat$jdem_socialpoliticalid, NA)
table(skin.iat$socipol.post11, skin.iat$month)

skin.iat$econpol.post11 <- factor(skin.iat$econpol.post11, labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal",
                                                                      "Neutral", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"))
skin.iat$socipol.post11 <- factor(skin.iat$socipol.post11, labels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal",
                                                                      "Neutral", "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"))

# now combine the jdems for econ
skin.iat$econpol.NEW <- paste(skin.iat$econpol.2011, skin.iat$econpol.post11)
table(skin.iat$econpol.NEW)
skin.iat$econpol.NEW <- gsub("NA ", "", skin.iat$econpol.NEW)
skin.iat$econpol.NEW <- gsub(" NA", "", skin.iat$econpol.NEW)
skin.iat$econpol.NEW <- recode(skin.iat$econpol.NEW, "'NA' = NA")
table(skin.iat$econpol.NEW, skin.iat$year)

# now combine the jdems for social
skin.iat$socipol.NEW <- paste(skin.iat$socipol.2011, skin.iat$socipol.post11)
table(skin.iat$socipol.NEW)
skin.iat$socipol.NEW <- gsub("NA ", "", skin.iat$socipol.NEW)
skin.iat$socipol.NEW <- gsub(" NA", "", skin.iat$socipol.NEW)
skin.iat$socipol.NEW <- recode(skin.iat$socipol.NEW, "'NA' = NA")
table(skin.iat$socipol.NEW, skin.iat$year)

# all same factor levels
table(skin.iat$politicalid_6)
table(skin.iat$politicalid_7)
table(skin.iat$econpol.NEW)
table(skin.iat$socipol.NEW)

# combine all the raw scales
skin.iat$pol.new <- paste(skin.iat$politicalid_6, skin.iat$politicalid_7)
table(skin.iat$pol.new)
skin.iat$pol.new <- gsub("NA ", "", skin.iat$pol.new)
skin.iat$pol.new <- gsub(" NA", "", skin.iat$pol.new)
skin.iat$pol.new <- recode(skin.iat$pol.new, "'NA' = NA")
table(skin.iat$pol.new, skin.iat$year)

# add in the other available variables
skin.iat$pol.new <- paste(skin.iat$pol.new, skin.iat$econpol.NEW)
table(skin.iat$pol.new) 
skin.iat$pol.new <- gsub("NA ", "", skin.iat$pol.new)
skin.iat$pol.new <- gsub(" NA", "", skin.iat$pol.new)
skin.iat$pol.new <- recode(skin.iat$pol.new, "'NA' = NA")
skin.iat$pol.new <- recode(skin.iat$pol.new, "'Strongly Conservative' = 'Strongly Conservative';
                           'Moderately Conservative' = 'Moderately Conservative';
                           'Slightly Conservative' = 'Slightly Conservative';
                           'Neutral' = 'Neutral';
                           'Strongly Liberal' = 'Strongly Liberal';
                           'Moderately Liberal' = 'Moderately Liberal';
                           'Slightly Liberal' = 'Slightly Liberal';
                           'Strongly Conservative Strongly Conservative' = 'Strongly Conservative';
                           'Moderately Conservative Moderately Conservative' = 'Moderately Conservative';
                           'Slightly Conservative Slightly Conservative' = 'Slightly Conservative';
                           'Neutral Neutral' = 'Neutral';
                           'Strongly Liberal Strongly Liberal' = 'Strongly Liberal';
                           'Moderately Liberal Moderately Liberal' = 'Moderately Liberal';
                           'Slightly Liberal Slightly Liberal' = 'Slightly Liberal';
                           else = NA")

table(skin.iat$pol.new, skin.iat$year)

skin.iat$pol.new <- paste(skin.iat$pol.new, skin.iat$socipol.NEW)
table(skin.iat$pol.new)
skin.iat$pol.new <- gsub("NA ", "", skin.iat$pol.new)
skin.iat$pol.new <- gsub(" NA", "", skin.iat$pol.new)
skin.iat$pol.new <- recode(skin.iat$pol.new, "'NA' = NA")
skin.iat$pol.new <- recode(skin.iat$pol.new, "'Strongly Conservative' = 'Strongly Conservative';
                           'Moderately Conservative' = 'Moderately Conservative';
                           'Slightly Conservative' = 'Slightly Conservative';
                           'Neutral' = 'Neutral';
                           'Strongly Liberal' = 'Strongly Liberal';
                           'Moderately Liberal' = 'Moderately Liberal';
                           'Slightly Liberal' = 'Slightly Liberal';
                           'Strongly Conservative Strongly Conservative' = 'Strongly Conservative';
                           'Moderately Conservative Moderately Conservative' = 'Moderately Conservative';
                           'Slightly Conservative Slightly Conservative' = 'Slightly Conservative';
                           'Neutral Neutral' = 'Neutral';
                           'Strongly Liberal Strongly Liberal' = 'Strongly Liberal';
                           'Moderately Liberal Moderately Liberal' = 'Moderately Liberal';
                           'Slightly Liberal Slightly Liberal' = 'Slightly Liberal';
                           else = NA")

table(skin.iat$pol.new, skin.iat$month) # complete and consistent data

# reassign
skin.iat$politicalid <- skin.iat$pol.new
# and get the right order
skin.iat$politicalid<- factor(skin.iat$politicalid, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                               "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

table(skin.iat$politicalid, skin.iat$month) # complete data

## Stimuli --------
table(skin.iat$Stimuli, skin.iat$month.cent)

# country of citizenship and residency ---------
## countrycit and countryres
sort(table(skin.iat$countrycit))
sort(table(skin.iat$countryres))
sort(table(skin.iat$countrycit_num))
sort(table(skin.iat$countryres_num))

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(skin.iat$countrycit)
skin.iat$countrycit <- gsub("  ", "", skin.iat$countrycit)
skin.iat$countrycit <- gsub(" ", "", skin.iat$countrycit)
skin.iat$countrycit <- factor(skin.iat$countrycit)
skin.iat$countrycit1 <- recode(skin.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(skin.iat$countrycit1))
skin.iat$countrycit1 <- recode(skin.iat$countrycit1, "'1' = 'US'")
skin.iat$countrycit2 <- recode(skin.iat$countrycit_num, "'1' = 'US'")

skin.iat$countrycit.new <- paste(skin.iat$countrycit1, skin.iat$countrycit2)
sort(table(skin.iat$countrycit.new))
skin.iat$countrycit.new <- gsub("NA ", "", skin.iat$countrycit.new)
skin.iat$countrycit.new <- gsub(" NA", "", skin.iat$countrycit.new)
skin.iat$countrycit.new <- factor(skin.iat$countrycit.new)
skin.iat$countrycit.new <- recode(skin.iat$countrycit.new, "'US US' = 'US'")
sort(table(skin.iat$countrycit.new))

table(skin.iat$countrycit.new, skin.iat$month)["US",] # complete data

levels(skin.iat$countryres)
skin.iat$countryres <- gsub("  ", "", skin.iat$countryres)
skin.iat$countryres <- gsub(" ", "", skin.iat$countryres)
skin.iat$countryres <- factor(skin.iat$countryres)
skin.iat$countryres1 <- recode(skin.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(skin.iat$countryres1))
skin.iat$countryres1 <- recode(skin.iat$countryres1, "'1' = 'US'")
skin.iat$countryres2 <- recode(skin.iat$countryres_num, "'1' = 'US'")

skin.iat$countryres.new <- paste(skin.iat$countryres1, skin.iat$countryres2)
skin.iat$countryres.new <- gsub("NA ", "", skin.iat$countryres.new)
skin.iat$countryres.new <- gsub(" NA", "", skin.iat$countryres.new)
skin.iat$countryres.new <- factor(skin.iat$countryres.new)
skin.iat$countryres.new <- recode(skin.iat$countryres.new, "'US US' = 'US'")
sort(table(skin.iat$countryres.new))

table(skin.iat$countryres.new, skin.iat$month)["US",] # complete data after 0602


## start cleaning the zipcodes ----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
table(skin.iat$postcodelong)
table(skin.iat$postcodenow)

# make it a character to use regular expressions
skin.iat$zip <- as.character(skin.iat$postcodelong)

skin.iat$zip <- gsub("[[:punct:]]", "", skin.iat$zip) # remove punctuation
skin.iat$zip <- gsub("[[:space:]]", "", skin.iat$zip) # remove all white space
skin.iat$zip <- gsub("[^0-9]", "", skin.iat$zip) # remove all non-numeric characters

skin.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", skin.iat$zip) # remove all strings shorter than 5
skin.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", skin.iat$zip1) # remove all strings longer than 5, shorter than 9
skin.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", skin.iat$zip1) # remove all strings longer 9

skin.iat$zip1 <- as.character(skin.iat$zip1)

table(skin.iat$zip1[skin.iat$zip1>02112], skin.iat$month[skin.iat$zip1>02112])

## add in the geographic data
skin.iat.places <- merge(skin.iat, zips, by = "zip1", all = TRUE)

## data subsetting -----------------------------------------------------------------------------------------------------------------------
# subset to US participants (by countryres.new), allows for retaining the most participants over time
skin.iatUSres <- skin.iat.places[skin.iat.places$countryres.new=="US",]
sort(table(skin.iatUSres$countryres.new))
## total with US country residence (and NAs omitted) = 595,329

# now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu, raceuality
skin.full <- skin.iatUSres[is.na(skin.iatUSres$dscore)==FALSE,] # 595,329
skin.full <- skin.full[is.na(skin.full$att.new)==FALSE,] # 563,236

skin.full.demogs <- skin.full[is.na(skin.full$sex.new)==FALSE,] # 559,803
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$age.new)==FALSE,] # 555,261
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$ethnic.race.new)==FALSE,] # 547,090
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$politicalid)==FALSE,] #491,985
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$edu.new)==FALSE,] # 488,330

# also include the following variables: num and geographic information
skin.full.demogs <- skin.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                       "age.new", "sex.new", "ethnic.race.new", "politicalid", "edu.new",
                                       "Order", "num.new", "Stimuli",
                                       "countrycit.new", "countryres.new",
                                       "zip1", "State", "Place.Name", "State.Abbreviation", "County")]

table(skin.full.demogs$month.cent)
table(skin.full.demogs$month)
length(table(skin.full.demogs$month))


##############################################################################################################################################
## CLEAN THE WEIGHT DATA  -----------------------------------------------------------------------------------------------------------------
weight.iat <- read.spss("Weight IAT.2004-2016.sav",
                        to.data.frame = TRUE) # 1,934,230

# get the date in the right format
weight.iat$date <- as.chron(ISOdate(1582, 10, 14) + weight.iat$date) 
weight.iat$date <- as.Date(weight.iat$date)

# get a month variable
weight.iat$month <- format(weight.iat$date, "%y%m")
# get a month factor variable, beginning in 2004 (add 4 months to make month.cent 1 equivalent to January, 2004)
table(weight.iat$month) # (also note that there is missing data from 1405 - 1412), and missing from 1209 - 1211 (104 and up)
weight.iat$month.cent <- as.numeric(factor(weight.iat$month)) + 4

weight.iat$month.cent[weight.iat$month.cent>104] <- weight.iat$month.cent[weight.iat$month.cent>104] + 3
table(weight.iat$month.cent)

# keep only those with month.cent > 0 and < 157 (2004 - 2016)
weight.iat <- weight.iat[weight.iat$month.cent > 0,]
weight.iat <- weight.iat[weight.iat$month.cent < 157,]

## for the data from 2004 - 2016, the new total is 1,934,138 participants.

## how many US? --------------
## combine country cit and dem_citizenship
sort(table(weight.iat$countrycit))
sort(table(weight.iat$dem_citizenship))
sort(table(weight.iat$countrycit_num))
levels(weight.iat$countrycit)
levels(weight.iat$dem_citizenship)
levels(weight.iat$countrycit_num)

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(weight.iat$countrycit)
weight.iat$countrycit <- gsub("  ", "", weight.iat$countrycit)
weight.iat$countrycit <- gsub(" ", "", weight.iat$countrycit)
weight.iat$countrycit <- factor(weight.iat$countrycit)
weight.iat$countrycit1 <- recode(weight.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(weight.iat$countrycit1))
weight.iat$countrycit1 <- recode(weight.iat$countrycit1, "'1' = 'US'")
levels(weight.iat$dem_citizenship)
weight.iat$countrycit2 <- recode(weight.iat$dem_citizenship, "'U.S.A.' = 'US'; 'U.S.A._duplicated_226' = 'US'")
levels(weight.iat$countrycit_num)
weight.iat$countrycit3 <- recode(weight.iat$countrycit_num, "'U.S.A. ' = 'US'")

weight.iat$countrycit.new <- paste(weight.iat$countrycit1, weight.iat$countrycit2)
sort(table(weight.iat$countrycit.new))
weight.iat$countrycit.new <- gsub("NA ", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- gsub(" NA", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- factor(weight.iat$countrycit.new)
weight.iat$countrycit.new <- recode(weight.iat$countrycit.new, "'US US' = 'US'")
sort(table(weight.iat$countrycit.new))

weight.iat$countrycit.new <- paste(weight.iat$countrycit.new, weight.iat$countrycit3)
sort(table(weight.iat$countrycit.new))
weight.iat$countrycit.new <- gsub("NA ", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- gsub(" NA", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- factor(weight.iat$countrycit.new)
weight.iat$countrycit.new <- recode(weight.iat$countrycit.new, "'US US' = 'US'")
sort(table(weight.iat$countrycit.new))

table(weight.iat$countrycit.new, weight.iat$month)["US",] # complete data after 0602

## repeat for country of residence
levels(weight.iat$countryres)
weight.iat$countryres <- gsub("  ", "", weight.iat$countryres)
weight.iat$countryres <- gsub(" ", "", weight.iat$countryres)
weight.iat$countryres <- factor(weight.iat$countryres)
weight.iat$countryres1 <- recode(weight.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(weight.iat$countryres1))
weight.iat$countryres1 <- recode(weight.iat$countryres1, "'1' = 'US'")
levels(weight.iat$dem_residence)
weight.iat$countryres2 <- recode(weight.iat$dem_residence, "'U.S.A.' = 'US'; 'U.S.A._duplicated_226' = 'US'")
levels(weight.iat$countryres_num)
weight.iat$countryres3 <- recode(weight.iat$countryres_num, "'U.S.A. ' = 'US'")

weight.iat$countryres.new <- paste(weight.iat$countryres1, weight.iat$countryres2)
sort(table(weight.iat$countryres.new))
weight.iat$countryres.new <- gsub("NA ", "", weight.iat$countryres.new)
weight.iat$countryres.new <- gsub(" NA", "", weight.iat$countryres.new)
weight.iat$countryres.new <- factor(weight.iat$countryres.new)
weight.iat$countryres.new <- recode(weight.iat$countryres.new, "'US US' = 'US'")
sort(table(weight.iat$countryres.new))

weight.iat$countryres.new <- paste(weight.iat$countryres.new, weight.iat$countryres3)
sort(table(weight.iat$countryres.new))
weight.iat$countryres.new <- gsub("NA ", "", weight.iat$countryres.new)
weight.iat$countryres.new <- gsub(" NA", "", weight.iat$countryres.new)
weight.iat$countryres.new <- factor(weight.iat$countryres.new)
weight.iat$countryres.new <- recode(weight.iat$countryres.new, "'US US' = 'US'")
sort(table(weight.iat$countryres.new))

table(weight.iat$countryres.new, weight.iat$month)["US",] # complete data after 0602

# how many US?
table(weight.iat$countryres.new)["US"] # 951,033

# how many complete US?
table(weight.iat$countryres.new[weight.iat$session_status=="C   "])["US"] # 653,343

# save a backup here
weight.iat2 <- weight.iat

## keep only complete participants ---------
weight.iat <- weight.iat[weight.iat$session_status=="C   ",] # complete subjects = 920,212

## keep only complete dscores -------
weight.iat <- weight.iat[is.na(weight.iat$D_biep.Thin_Good_all)==FALSE,] # complete IAT = 893,842

# keep only the variables of interest
weight.iat <- weight.iat[c("session_id", "date", "year", "month", "month.cent",
                           "D_biep.Thin_Good_all", "Order", "Stimuli",
                           "att", "att_5", "att_7", "att_reversed", "att7_reversed", "att7", "weightpref_001",
                           "age", "dem_byear", "birthyear",
                           "num", "num_002",
                           "sex", "dem_sex", "birthsex", "genderidentity",
                           "ethnic", "ethnicityomb", "dem_ethnicityomb", 
                           "raceomb", "dem_raceomb", "raceomb_002", "raceombmulti",
                           "countrycit", "countryres", "dem_citizenship", "dem_residence", "countrycit_num", "countryres_num",
                           "edu", "edu_13", "edu_14", "dem_education",
                           "politicalid", "politicalid_6", "politicalid_7", 
                           "dem_econpoliticalid", "dem_socialpoliticalid", "dem_politicalid", "dem_politicalid_001",
                           "dem_politicalparty_001", "dem4_001", "dem8_001",
                           "religion", "religionid", "religion2009", "dem_religion", "dem_religiosity",
                           "weight", "weightunit", "weightlbs", "weightkg", "myweight", "myweight_001", "myweight_002",
                           "height", "heightunit", "heightft", "heightin", "heightm", "heightcm", "myheight", "myheight_001",
                           "iam", "iam_001", "iam_reversed",
                           "tfat", "tthin", "tbefat", "tbethin", "identfat", "identthin",
                           "tfat_1to11_reversed", "tthin_1to11_reversed", "tempfat_001", "tempthin_001",
                           "postcodelong", "postcodenow")]


## dscore ------------
names(weight.iat)[which(names(weight.iat)=="D_biep.Thin_Good_all")] <- "dscore"


## session id ---------------
weight.iat$session_id <- as.character(weight.iat$session_id)

## age --------------
table(weight.iat$dem_byear) 
weight.iat$age2 <- weight.iat$year - weight.iat$dem_byear
table(weight.iat$age2)

weight.iat$age.new <- paste(weight.iat$age, weight.iat$age2)
table(weight.iat$age.new)

weight.iat$age.new <- gsub("NA ", "", weight.iat$age.new)
weight.iat$age.new <- gsub(" NA", "", weight.iat$age.new)
weight.iat$age.new <- recode(weight.iat$age.new, "'NA' = NA")

table(weight.iat$age.new, weight.iat$month)[25:35,] ## missing from 1604 onwards

# now combine with birthyear
table(weight.iat$birthyear) # combines codes that are real values and coded values

weight.iat$birthyear.new <- factor(weight.iat$birthyear)
levels(weight.iat$birthyear.new) # missing 81, 84, 86:94, 96:97

birthyears <- seq(2009, 1910, -1)
birthyears <- birthyears[-c(81, 84, 86:94, 96:97)]
birthyears

levels(weight.iat$birthyear.new)[1:87] <- birthyears
table(weight.iat$birthyear.new)

# birthyear was introduced in 2016, so subtract birthyear from 2016 to get age
# get in the right order
weight.iat$birthyear.new2 <- factor(weight.iat$birthyear.new, levels = c(1910:2009))
# save as character
weight.iat$birthyear.new2 <- as.character(weight.iat$birthyear.new2)
# then as numeric
weight.iat$birthyear.new2 <- as.numeric(weight.iat$birthyear.new2)
# subtract
weight.iat$birthyear.new2 <- 2016 - weight.iat$birthyear.new2

table(weight.iat$birthyear.new2)
table(weight.iat$age.new)

## now combine
weight.iat$age.new <- paste(weight.iat$age.new, weight.iat$birthyear.new2)

table(weight.iat$age.new)
weight.iat$age.new <- gsub("NA ", "", weight.iat$age.new)
weight.iat$age.new <- gsub(" NA", "", weight.iat$age.new)

weight.iat$age.new <- factor(weight.iat$age.new)

weight.iat$age.new <- factor(weight.iat$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.

table(weight.iat$age.new)

weight.iat$age.new <- as.character(weight.iat$age.new)
weight.iat$age.new <- as.numeric(weight.iat$age.new)

table(weight.iat$age.new, weight.iat$month)[25:35,]


## num  ----------
# delete unnecessary levels (should only be 5 levels)
table(weight.iat$num)
levels(weight.iat$num)
levels(weight.iat$num)[levels(weight.iat$num)==".  " | levels(weight.iat$num)== "   " | levels(weight.iat$num)== "nul" | levels(weight.iat$num)== "***" | levels(weight.iat$num)== "-99"] <- NA

table(weight.iat$num, weight.iat$month) # missing from 1005 to 1411
table(weight.iat$num, weight.iat$month.cent)[6:11,]


weight.iat$num <- gsub("  ", "", weight.iat$num)

weight.iat$num <- factor(weight.iat$num)
weight.iat$num <- recode(weight.iat$num, "'3' = '3-5'; '4' = '3-5'; '5' = '3-5'")

# combine with num_002
table(weight.iat$num_002, weight.iat$month)

weight.iat$num.new <- paste(weight.iat$num, weight.iat$num_002)

table(weight.iat$num.new)
weight.iat$num.new <- gsub("NA ", "", weight.iat$num.new)
weight.iat$num.new <- gsub(" NA", "", weight.iat$num.new)
weight.iat$num.new <- gsub("3-5 ", "", weight.iat$num.new)
weight.iat$num.new <- factor(weight.iat$num.new)
levels(weight.iat$num.new)
weight.iat$num.new <- recode(weight.iat$num.new, "'1 0' = '0'; '2 1' = '1'; '3-5 2' = '2'; '6+ ' = '6+'; 'NA' = NA")

table(weight.iat$num.new, weight.iat$month.cent) ## incomplete data, no num variable was collected for these months. 
# Documented in emails with Nicole Lofaro


## sex --------
# only keep f, m, n and combine with dem_sex and birthsex
table(weight.iat$sex)
levels(weight.iat$sex)
levels(weight.iat$sex)[levels(weight.iat$sex)=="." | levels(weight.iat$sex)== " " | levels(weight.iat$sex)== "n"] <- NA
weight.iat$sex <- recode(weight.iat$sex, "'f' = 'Female'; 'm' = 'Male'; 'M' = 'Male'")

table(weight.iat$dem_sex)

weight.iat$sex.new <- paste(weight.iat$dem_sex, weight.iat$sex)
table(weight.iat$sex.new)

weight.iat$sex.new <- gsub("NA ", "", weight.iat$sex.new)
weight.iat$sex.new <- gsub(" NA", "", weight.iat$sex.new)
weight.iat$sex.new <- recode(weight.iat$sex.new, "'NA' = NA")

table(weight.iat$sex.new, weight.iat$year)
weight.iat$sex.new <- factor(weight.iat$sex.new, labels = c("female", "male"))


## combine with birthsex
table(weight.iat$birthsex)
weight.iat$birthsex <- factor(weight.iat$birthsex, labels = c("male", "female"))

weight.iat$sex.new <- paste(weight.iat$sex.new, weight.iat$birthsex)

table(weight.iat$sex.new)
weight.iat$sex.new <- gsub("NA ", "", weight.iat$sex.new)
weight.iat$sex.new <- gsub(" NA", "", weight.iat$sex.new)

weight.iat$sex.new <- factor(weight.iat$sex.new)
weight.iat$sex.new <- recode(weight.iat$sex.new, "'NA' = NA")

table(weight.iat$sex.new, weight.iat$month.cent) ## complete data for available months

## race -------
# combine raceomb and dem_raceomb
table(weight.iat$raceomb, weight.iat$year)
table(weight.iat$dem_raceomb, weight.iat$year)

weight.iat$raceomb.new <- paste(weight.iat$dem_raceomb, weight.iat$raceomb)
table(weight.iat$raceomb.new)

weight.iat$raceomb.new <- gsub("NA ", "", weight.iat$raceomb.new)
weight.iat$raceomb.new <- gsub(" NA", "", weight.iat$raceomb.new)
weight.iat$raceomb.new <- recode(weight.iat$raceomb.new, "'NA' = NA")

table(weight.iat$raceomb.new, weight.iat$year)

table(weight.iat$raceombmulti) # if participant includes 5 and 6 = More than one race - B/W; all others = More than one race - Other
weight.iat$raceomb2 <- factor(weight.iat$raceombmulti)
levels(weight.iat$raceomb2) <- trimws(levels(weight.iat$raceomb2), which = "right")
weight.iat$raceomb2 <- recode(weight.iat$raceomb2, "'[5,6]' = 'More than one race - Black/White'; '[5]' = 'Black or African American'; 
                            '[6]' = 'White'; '[1]' = 'American Indian/Alaska Native'; '[2]' = 'East Asian';
                            '[3]' = 'South Asian'; '[4]' = 'Native Hawaiian or other Pacific Islander'; 
                            '[7]' = 'Other or Unknown'; '' = NA; '-999' = NA; '.' = NA; '[]' = NA; else = 'More than one race - Other'")
table(weight.iat$raceomb2)

weight.iat$raceomb.new <- paste(weight.iat$raceomb.new, weight.iat$raceomb2)
table(weight.iat$raceomb.new)
weight.iat$raceomb.new <- gsub(" NA", "", weight.iat$raceomb.new)
weight.iat$raceomb.new <- factor(weight.iat$raceomb.new)

levels(weight.iat$raceomb.new)
weight.iat$raceomb.new <- recode(weight.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                               'Black or African American Black or African American' = 'Black or African American';
                               'East Asian East Asian' = 'East Asian';
                               'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                               'More than one race - Other More than one race - Other' = 'More than one race - Other';
                               'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                               'South Asian South Asian' = 'South Asian';
                               'White White' = 'White'; 'NA White' = 'White';
'White American Indian/Alaska Native' = 'White'; 'White Native Hawaiian or other Pacific Islander' = 'White'; 'White South Asian' = 'White';
                               'NA' = NA")
levels(weight.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(weight.iat$raceomb.new)), "Other or Unknown",
                                       levels(weight.iat$raceomb.new))
levels(weight.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(weight.iat$raceomb.new)), "More than one race - Other",
                                       levels(weight.iat$raceomb.new))
levels(weight.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(weight.iat$raceomb.new)), "More than one race - Black/White",
                                       levels(weight.iat$raceomb.new))


table(weight.iat$raceomb.new, weight.iat$year)
table(weight.iat$raceomb.new, weight.iat$month)[3:9,] # complete data after 2006, beyond the missing months

# drop the Multiracial level (included in the raceombmulti)
weight.iat$raceomb3 <- factor(weight.iat$raceomb_002, levels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                                             "Black or African American", "White", "Other or unknown"),
                            labels = c("American Indian/Alaska Native", "East Asian", "South Asian", "Native Hawaiian or other Pacific Islander",
                                       "Black or African American", "White", "Other or Unknown"))
table(weight.iat$raceomb3)

weight.iat$raceomb.new <- paste(weight.iat$raceomb.new, weight.iat$raceomb3)
table(weight.iat$raceomb.new)
weight.iat$raceomb.new <- gsub(" NA", "", weight.iat$raceomb.new)
weight.iat$raceomb.new <- factor(weight.iat$raceomb.new)
table(weight.iat$raceomb.new)

weight.iat$raceomb.new <- recode(weight.iat$raceomb.new, "'American Indian/Alaska Native American Indian/Alaska Native' = 'American Indian/Alaska Native';
                               'Black or African American Black or African American' = 'Black or African American';
                               'East Asian East Asian' = 'East Asian';
                               'More than one race - Black/White More than one race - Black/White' = 'More than one race - Black/White';
                               'More than one race - Other More than one race - Other' = 'More than one race - Other';
                               'Native Hawaiian or other Pacific Islander Native Hawaiian or other Pacific Islander' = 'Native Hawaiian or other Pacific Islander';
                               'South Asian South Asian' = 'South Asian';
                               'White White' = 'White'; 'Other or Unknown Other or Unknown'  = 'Other or Unknown';
                               'NA' = NA")
table(weight.iat$raceomb.new, weight.iat$month)[4:9,]
levels(weight.iat$raceomb.new) <- ifelse(grepl("More than one race - Black/White", levels(weight.iat$raceomb.new)), "More than one race - Black/White",
                                       levels(weight.iat$raceomb.new))
levels(weight.iat$raceomb.new) <- ifelse(grepl("More than one race - Other", levels(weight.iat$raceomb.new)), "More than one race - Other",
                                       levels(weight.iat$raceomb.new))
levels(weight.iat$raceomb.new) <- ifelse(grepl("Other or Unknown", levels(weight.iat$raceomb.new)), "Other or Unknown",
                                       levels(weight.iat$raceomb.new))

table(weight.iat$raceomb.new)
table(weight.iat$raceomb.new, weight.iat$month)


#  ethnic (to match raceomb codes)
table(weight.iat$ethnic, weight.iat$month) # only to 0609
weight.iat$ethnic.new <- recode(weight.iat$ethnic, "'White-Not of Hispanic Origin' = 'White'; 'Black-Not of Hispanic Origin' = 'Black or African American';
                              'Asian or Pacific Islander' = 'Asian'; 'Multi-racial (Black and White)' = 'More than one race - Black/White';
                              'Multi-racial (other)' = 'More than one race - Other'; 'North American Indian or First Nation' = 'American Indian/Alaska Native';
                              'Hispanic' = 'Other or Unknown'")

weight.iat$race.new <- recode(weight.iat$raceomb.new, "'East Asian' = 'Asian'; 'South Asian' = 'Asian'; 'Native Hawaiian or other Pacific Islander' = 'Asian'")

table(weight.iat$ethnic.new)
table(weight.iat$race.new)

weight.iat$ethnic.race.new <- paste(weight.iat$ethnic.new, weight.iat$race.new)
table(weight.iat$ethnic.race.new)

weight.iat$ethnic.race.new <- gsub("NA ", "", weight.iat$ethnic.race.new)
weight.iat$ethnic.race.new <- gsub(" NA", "", weight.iat$ethnic.race.new)
weight.iat$ethnic.race.new <- recode(weight.iat$ethnic.race.new, "'NA' = NA")

weight.iat$ethnic.race.new <- factor(weight.iat$ethnic.race.new)
levels(weight.iat$ethnic.race.new)
table(weight.iat$ethnic.race.new, weight.iat$year)
table(weight.iat$ethnic.race.new, weight.iat$month) # complete data from 2004 - 2016
table(weight.iat$ethnic.race.new, weight.iat$month)[3:7,]

## ethnicityomb --------
# combine ethnicityomb and dem_ethnicityomb
table(weight.iat$ethnicityomb, weight.iat$year)
table(weight.iat$dem_ethnicityomb, weight.iat$year)

weight.iat$ethnicityomb.new <- paste(weight.iat$dem_ethnicityomb, weight.iat$ethnicityomb)
table(weight.iat$ethnicityomb.new)

weight.iat$ethnicityomb.new <- gsub("NA ", "", weight.iat$ethnicityomb.new)
weight.iat$ethnicityomb.new <- gsub(" NA", "", weight.iat$ethnicityomb.new)
weight.iat$ethnicityomb.new <- recode(weight.iat$ethnicityomb.new, "'NA' = NA")

table(weight.iat$ethnicityomb.new, weight.iat$year)

weight.iat$ethnicityomb <- weight.iat$ethnicityomb.new

table(weight.iat$ethnicityomb.new, weight.iat$month)

## education -----------
table(weight.iat$edu)
table(weight.iat$edu_13) # falsely combined in edu
table(weight.iat$edu_14) # falsely combined in edu
table(weight.iat$dem_education)

# add one level to edu 13: MBA
weight.iat$edu_131 <- weight.iat$edu_13
levels(weight.iat$edu_131) <- c(levels(weight.iat$edu_13), "M.B.A.")
table(weight.iat$edu_131)
# and the right order
weight.iat$edu_131 <- factor(weight.iat$edu_131, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                            "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                            "J.D.", "M.D.", "Ph.D.", "other advanced degree"))


# get the right order for edu 14
table(weight.iat$edu_14)
weight.iat$edu_141 <- factor(weight.iat$edu_14, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                           "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                           "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
weight.iat$edu.new <- paste(weight.iat$edu_131, weight.iat$edu_141)
weight.iat$edu.new <- gsub("NA ", "", weight.iat$edu.new)
weight.iat$edu.new <- gsub(" NA", "", weight.iat$edu.new)
weight.iat$edu.new <- recode(weight.iat$edu.new, "'NA' = NA")

# get the right order
weight.iat$edu.new <- factor(weight.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                            "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                            "J.D.", "M.D.", "Ph.D.", "other advanced degree"))
table(weight.iat$edu.new, weight.iat$month)

## add in dem_education
weight.iat$dem_education1 <- factor(weight.iat$dem_education, labels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                                         "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                                         "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(weight.iat$dem_education1)
table(weight.iat$edu.new)

weight.iat$edu.new <- paste(weight.iat$dem_education1, weight.iat$edu.new)
table(weight.iat$edu.new)

weight.iat$edu.new <- gsub("NA ", "", weight.iat$edu.new)
weight.iat$edu.new <- gsub(" NA", "", weight.iat$edu.new)
weight.iat$edu.new <- recode(weight.iat$edu.new, "'NA' = NA")

table(weight.iat$edu.new, weight.iat$year)

# and, finally, get the right order:
weight.iat$edu.new <- factor(weight.iat$edu.new, levels = c("elementary school", "junior high", "some high school", "high school graduate", "some college",
                                                            "associate's degree", "bachelor's degree", "some graduate school", "master's degree", "M.B.A.",
                                                            "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(weight.iat$edu.new, weight.iat$month) ## now complete data (Except 2014)


## political ideology --------
# including: "dem_politicalid_001", "dem_politicalparty_001", "dem4_001", "dem8_001",
table(weight.iat$politicalid, weight.iat$year) # falsely combines the two scales
table(weight.iat$politicalid_6, weight.iat$year) # 2004-2006
table(weight.iat$politicalid_7, weight.iat$year) # 2006 - 2010, and 2015 * has an extraneous level
table(weight.iat$dem_politicalid, weight.iat$year) # 2012-2014 * has an extraneous level
table(weight.iat$dem_econpoliticalid, weight.iat$year) # 2010 - 2012
table(weight.iat$dem_socialpoliticalid, weight.iat$year) # 2010 - 2012
table(weight.iat$dem_politicalid_001, weight.iat$year) # 2015-16 ((redundant with politicalid_7, gives mixed information))
table(weight.iat$dem4_001, weight.iat$year) # 2015-16 (redundant with politicalid_7)

# politicalid 6
# get the right labels
weight.iat$politicalid_6 <- factor(weight.iat$politicalid_6, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                        "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# add the Neutral level to pol 6
levels(weight.iat$politicalid_6) <- c(levels(weight.iat$politicalid_6), "Neutral")

# get the right order
weight.iat$politicalid_6 <- factor(weight.iat$politicalid_6, levels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                        "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))
table(weight.iat$politicalid_6)

# politicalid 7
levels(weight.iat$politicalid_7)[levels(weight.iat$politicalid_7)=="-999"] <- NA
table(weight.iat$politicalid_7)
weight.iat$politicalid_7 <- factor(weight.iat$politicalid_7, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative",
                                                                        "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# get the right levels and order for the other variable.
table(weight.iat$dem_politicalid)
weight.iat$dem_politicalid <- factor(weight.iat$dem_politicalid)
levels(weight.iat$dem_politicalid)[levels(weight.iat$dem_politicalid)=="-900"] <- NA
weight.iat$dem_politicalid <- factor(weight.iat$dem_politicalid, labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                                                            "Neutral", "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

# keep the social political id only, the other is redundant
table(weight.iat$dem_socialpoliticalid)
weight.iat$politicalid.new <- recode(weight.iat$dem_socialpoliticalid, "'I am strongly liberal on social issues' = 'Strongly Liberal';
                                     'I am moderately liberal on social issues' = 'Moderately Liberal';
                                     'I am slightly liberal on social issues' = 'Slightly Liberal';
                                     'I am strongly conservative on social issues' = 'Strongly Conservative';
                                     'I am moderately conservative on social issues' = 'Moderately Conservative';
                                     'I am slightly conservative on social issues' = 'Slightly Conservative';
                                     'I am in the middle on social issues' = 'Neutral'")
table(weight.iat$politicalid.new)


## paste together
# political id 6 and 7
weight.iat$pol.new.1 <- paste(weight.iat$politicalid_6, weight.iat$politicalid_7)
table(weight.iat$pol.new.1)
weight.iat$pol.new.1 <- gsub("NA ", "", weight.iat$pol.new.1)
weight.iat$pol.new.1 <- gsub(" NA", "", weight.iat$pol.new.1)
weight.iat$pol.new.1 <- recode(weight.iat$pol.new.1, "'NA' = NA")


# pol new 1 and dem_politicalid
weight.iat$pol.new.2 <- paste(weight.iat$pol.new.1, weight.iat$dem_politicalid) 
table(weight.iat$pol.new.2)
weight.iat$pol.new.2 <- gsub("NA ", "", weight.iat$pol.new.2)
weight.iat$pol.new.2 <- gsub(" NA", "", weight.iat$pol.new.2)
weight.iat$pol.new.2 <- recode(weight.iat$pol.new.2, "'NA' = NA")
table(weight.iat$pol.new.2)


# and lastly, paste with dem social
weight.iat$pol.new.final <- paste(weight.iat$pol.new.2, weight.iat$politicalid.new) 
table(weight.iat$pol.new.final)
weight.iat$pol.new.final <- gsub("NA ", "", weight.iat$pol.new.final)
weight.iat$pol.new.final <- gsub(" NA", "", weight.iat$pol.new.final)
weight.iat$pol.new.final <- recode(weight.iat$pol.new.final, "'NA' = NA")

# make sure we have complete data across time
table(weight.iat$pol.new.final, weight.iat$year) # complete data now.
table(weight.iat$pol.new.final, weight.iat$month.cent) # complete data now.

weight.iat$politicalid <- weight.iat$pol.new.final

# get the right order
weight.iat$politicalid <- factor(weight.iat$politicalid, levels = c("Strongly Conservative",
                                                                    "Moderately Conservative", 
                                                                    "Slightly Conservative",
                                                                    "Neutral", 
                                                                    "Slightly Liberal", 
                                                                    "Moderately Liberal", 
                                                                    "Strongly Liberal"))

table(weight.iat$politicalid, weight.iat$year)



## explicit attitudes  -----------
table(weight.iat$att, weight.iat$year) # falsely combines 5 and 7, only up to 2010
table(weight.iat$att_5, weight.iat$year) # 2004-06
table(weight.iat$att_7, weight.iat$year) # 2006-2010, 2012 
# ** note that in 2012, this variable reversed, even though it wasn't listed as doing so. The reversal can be seen from the reversal of frequencies
table(weight.iat$att7, weight.iat$year) # 2016
table(weight.iat$att7_reversed, weight.iat$year) # 2010-2011, 2013-2014
table(weight.iat$weightpref_001, weight.iat$year) # 2015-2016
## when combined, we will have complete data.

# get the right labels
weight.iat$att_5 <- recode(weight.iat$att_5, "'I strongly prefer fat people to thin people.' = 'I strongly prefer Fat People to Thin People.';
                           'I moderately prefer fat people to thin people.' = 'I moderately prefer Fat People to Thin People.';
                           'I like thin people and fat people equally' = 'I like Thin People and Fat People equally.';
                           'I moderately prefer thin people to fat people.' = 'I moderately prefer Thin People to Fat People.';
                           'I strongly prefer thin people to fat people.' = 'I strongly prefer Thin People to Fat People'")

# add two levels
levels(weight.iat$att_5) <- c(levels(weight.iat$att_5), "I slightly prefer Fat People to Thin People.", "I slightly prefer Thin People to Fat People.")

# get the right order
weight.iat$att_5 <- factor(weight.iat$att_5, levels = c("I strongly prefer Fat People to Thin People.", "I moderately prefer Fat People to Thin People.",
                                                        "I slightly prefer Fat People to Thin People.", "I like Thin People and Fat People equally.", 
                                                        "I slightly prefer Thin People to Fat People.", "I moderately prefer Thin People to Fat People.",
                                                        "I strongly prefer Thin People to Fat People"))

table(weight.iat$att_5)

# for att7, break into a 2012 component and a pre-2012 component
table(weight.iat$att_7, weight.iat$year)

weight.iat$att_7NEW <- ifelse(weight.iat$year!=2012, weight.iat$att_7, NA)
table(weight.iat$att_7NEW, weight.iat$year)
# get the right labels
weight.iat$att_7NEW <- factor(weight.iat$att_7NEW, labels = c("I strongly prefer Fat People to Thin People.", "I moderately prefer Fat People to Thin People.",
                                                              "I slightly prefer Fat People to Thin People.", "I like Thin People and Fat People equally.", 
                                                              "I slightly prefer Thin People to Fat People.", "I moderately prefer Thin People to Fat People.",
                                                              "I strongly prefer Thin People to Fat People"))
table(weight.iat$att_7NEW, weight.iat$year)

# now start by assigning a new variable to the old data, and then we will switch around the factor labels
weight.iat$att_7.2012 <- ifelse(weight.iat$year==2012, weight.iat$att_7, NA)
table(weight.iat$att_7.2012, weight.iat$year)

weight.iat$att_7.2012 <- factor(weight.iat$att_7.2012, labels = c("I strongly prefer Thin People to Fat People", "I moderately prefer Thin People to Fat People.",
                                                                  "I slightly prefer Thin People to Fat People.", "I like Thin People and Fat People equally.", 
                                                                  "I slightly prefer Fat People to Thin People.", "I moderately prefer Fat People to Thin People.",
                                                                  "I strongly prefer Fat People to Thin People."))
# get the right order
weight.iat$att_7.2012 <- factor(weight.iat$att_7.2012, levels = c("I strongly prefer Fat People to Thin People.", "I moderately prefer Fat People to Thin People.",
                                                                  "I slightly prefer Fat People to Thin People.", "I like Thin People and Fat People equally.", 
                                                                  "I slightly prefer Thin People to Fat People.", "I moderately prefer Thin People to Fat People.",
                                                                  "I strongly prefer Thin People to Fat People"))
table(weight.iat$att_7.2012, weight.iat$year)


## now combine the att_7 variables back together
weight.iat$att_7actual <- paste(weight.iat$att_7NEW, weight.iat$att_7.2012)
table(weight.iat$att_7actual)
weight.iat$att_7actual <- gsub("NA ", "", weight.iat$att_7actual)
weight.iat$att_7actual <- gsub(" NA", "", weight.iat$att_7actual)
weight.iat$att_7actual <- recode(weight.iat$att_7actual, "'NA' = NA")

table(weight.iat$att_7actual, weight.iat$year)
# now it's consistent.

# before worrying about order, we can go ahead and combine with the order variable: att_5
weight.iat$att.new <- paste(weight.iat$att_5, weight.iat$att_7actual) 
table(weight.iat$att.new)
weight.iat$att.new <- gsub("NA ", "", weight.iat$att.new)
weight.iat$att.new <- gsub(" NA", "", weight.iat$att.new)
weight.iat$att.new <- recode(weight.iat$att.new, "'NA' = NA")

table(weight.iat$att.new, weight.iat$year)

# add in the punctuation...
weight.iat$att.new <- recode(weight.iat$att.new, "'I strongly prefer Thin People to Fat People' = 'I strongly prefer Thin People to Fat People.'")


## additional attitude variables as well (att7_reversed)
table(weight.iat$att7_reversed, weight.iat$year)
weight.iat$att7_reversed <- recode(weight.iat$att7_reversed, "'I strongly prefer Fat People to Thin People' = 'I strongly prefer Fat People to Thin People.'")

# combine the two scales
weight.iat$att.new2 <- paste(weight.iat$att.new, weight.iat$att7_reversed) 
table(weight.iat$att.new2)
weight.iat$att.new2 <- gsub("NA ", "", weight.iat$att.new2)
weight.iat$att.new2 <- gsub(" NA", "", weight.iat$att.new2)
weight.iat$att.new2 <- recode(weight.iat$att.new2, "'NA' = NA")

table(weight.iat$att.new2, weight.iat$year) 
# still missing 2015 and 2016

## additional attitude variables as well (weightpref_001) 
table(weight.iat$weightpref_001, weight.iat$year)
weight.iat$weightpref_001 <- recode(weight.iat$weightpref_001, "'I strongly prefer Thin People to Fat People' = 'I strongly prefer Thin People to Fat People.'")

# combine the two scales
weight.iat$att.new3 <- paste(weight.iat$att.new2, weight.iat$weightpref_001) 
table(weight.iat$att.new3)
weight.iat$att.new3 <- gsub("NA ", "", weight.iat$att.new3)
weight.iat$att.new3 <- gsub(" NA", "", weight.iat$att.new3)
weight.iat$att.new3 <- recode(weight.iat$att.new3, "'NA' = NA")

table(weight.iat$att.new3, weight.iat$year) 


## additional attitude variables as well (att7)
table(weight.iat$att7, weight.iat$year)
weight.iat$att7 <- recode(weight.iat$att7, "'I strongly prefer Thin People to Fat People' = 'I strongly prefer Thin People to Fat People.'")

# combine the two scales
weight.iat$att.new4 <- paste(weight.iat$att.new3, weight.iat$att7) 
table(weight.iat$att.new4)
weight.iat$att.new4 <- gsub("NA ", "", weight.iat$att.new4)
weight.iat$att.new4 <- gsub(" NA", "", weight.iat$att.new4)
weight.iat$att.new4 <- recode(weight.iat$att.new4, "'NA' = NA")

table(weight.iat$att.new4, weight.iat$year) # complete!


# get the right order and reassign to att.new
weight.iat$att.new <- factor(weight.iat$att.new4, levels = c("I strongly prefer Fat People to Thin People.", "I moderately prefer Fat People to Thin People.",
                                                             "I slightly prefer Fat People to Thin People.", "I like Thin People and Fat People equally.", 
                                                             "I slightly prefer Thin People to Fat People.", "I moderately prefer Thin People to Fat People.",
                                                             "I strongly prefer Thin People to Fat People."))
table(weight.iat$att.new, weight.iat$year) # complete and consistent distributions.


## iam (weight variable) --------
table(weight.iat$iam, weight.iat$year) # 2006:2010, and reversed 2012
table(weight.iat$iam_001, weight.iat$year) # 2015 and 2016
table(weight.iat$iam_reversed, weight.iat$year) # 2010:2011 and 2013:2014

iam.labels <- levels(weight.iat$iam)
iam.labelsREVERSE <- levels(weight.iat$iam_reversed)

## resolve the 2012 data issue by creating a sub-variable (as we did for att_7)
weight.iat$iamNEW <- ifelse(weight.iat$year!=2012, weight.iat$iam, NA)
table(weight.iat$iamNEW, weight.iat$year) # consistent coding

weight.iat$iamNEW <- factor(weight.iat$iamNEW, labels = iam.labels)
table(weight.iat$iamNEW, weight.iat$year) # consistent coding

# now start by assigning a new variable to the old data, and then we will switch around the factor labels
weight.iat$iam.2012 <- ifelse(weight.iat$year==2012, weight.iat$iam, NA)
table(weight.iat$iam.2012, weight.iat$year) # see the problem with very underweight being much higher frequency than very overweight, in contrast to all other years.

weight.iat$iam.2012 <- factor(weight.iat$iam.2012, labels = iam.labelsREVERSE)
table(weight.iat$iam.2012, weight.iat$year) # better.


## now combine the iam variables back together
weight.iat$iam.actual <- paste(weight.iat$iamNEW, weight.iat$iam.2012)
table(weight.iat$iam.actual)
weight.iat$iam.actual <- gsub("NA ", "", weight.iat$iam.actual)
weight.iat$iam.actual <- gsub(" NA", "", weight.iat$iam.actual)
weight.iat$iam.actual <- recode(weight.iat$iam.actual, "'NA' = NA")

table(weight.iat$iam.actual, weight.iat$year)
# now it's consistent.

## now add in the other variables as well
table(weight.iat$iam_001, weight.iat$year)
weight.iat$iam.new <- paste(weight.iat$iam.actual, weight.iat$iam_001) 
table(weight.iat$iam.new)
weight.iat$iam.new <- gsub("NA ", "", weight.iat$iam.new)
weight.iat$iam.new <- gsub(" NA", "", weight.iat$iam.new)
weight.iat$iam.new <- recode(weight.iat$iam.new, "'NA' = NA")

weight.iat$iam.new2 <- paste(weight.iat$iam.new, weight.iat$iam_reversed)
table(weight.iat$iam.new2)
weight.iat$iam.new2 <- gsub("NA ", "", weight.iat$iam.new2)
weight.iat$iam.new2 <- gsub(" NA", "", weight.iat$iam.new2)
weight.iat$iam.new2 <- recode(weight.iat$iam.new2, "'NA' = NA")

table(weight.iat$iam.new2, weight.iat$year)

weight.iat$iam.new <- weight.iat$iam.new2

# get the right order
weight.iat$iam.new <- factor(weight.iat$iam.new, levels = iam.labelsREVERSE) # 1 = Very underweight
table(weight.iat$iam.new, weight.iat$year) # all data except 2004 - 2005.


## myweight ------------
table(weight.iat$myweight, weight.iat$year) # 2010 - 2014
table(weight.iat$myweight_001, weight.iat$year) # 2015 - 2016
table(weight.iat$myweight_002, weight.iat$year) # 2016

weight.iat$myweight.all <- paste(weight.iat$myweight, weight.iat$myweight_001)
table(weight.iat$myweight.all)
weight.iat$myweight.all <- gsub("NA ", "", weight.iat$myweight.all)
weight.iat$myweight.all <- gsub(" NA", "", weight.iat$myweight.all)
weight.iat$myweight.all <- recode(weight.iat$myweight.all, "'NA' = NA")

weight.iat$myweight.all <- paste(weight.iat$myweight.all, weight.iat$myweight_002)
table(weight.iat$myweight.all)
weight.iat$myweight.all <- gsub("NA ", "", weight.iat$myweight.all)
weight.iat$myweight.all <- gsub(" NA", "", weight.iat$myweight.all)
weight.iat$myweight.all <- recode(weight.iat$myweight.all, "'NA' = NA")

table(weight.iat$myweight.all, weight.iat$month.cent) # complete from month 76 onwards.


## weight units -------
levels(weight.iat$weightunit)
weight.iat$weightunit <- gsub("                 ", "", weight.iat$weightunit)
weight.iat$weightunit <- factor(weight.iat$weightunit)
levels(weight.iat$weightunit)
weight.iat$weightunit <- recode(weight.iat$weightunit, "'   ' = NA; '.  ' = NA")
table(weight.iat$weightunit)


## weight ----------
#remove weight outliers
table(weight.iat$weight) # many different units... 
table(weight.iat$weight, weight.iat$year) # and also not available before 2007
hist(weight.iat$weight) # drops off around 400, so include up to 500, also only include above 50, and only include those listing lbs.
weight.iat$weight.new <- ifelse(weight.iat$weightunit=="lbs" & weight.iat$weight<501 & weight.iat$weight>50 , 
                                weight.iat$weight, NA)
table(weight.iat$weight.new, weight.iat$year)

## no data from iam was available before 2006, so generate similar cut-offs for categories from the continuous weight variable, which was available 2004 and 2005
# aiming for these proportions:
table(weight.iat$iam.new)/sum(table(weight.iat$iam.new)) 
table(weight.iat$weight.new)
stat.desc(weight.iat$weight.new) # min = 51, max = 500, mean = 156
sd(weight.iat$weight.new, na.rm = TRUE) # 43.7

# very underweight = 70 or below
# moderately underweight = 70 - 85
# slightly underweight =  85 - 110
# neither under nor over = 110 - 150
# slightly over = 150 - 190
# moderately over = 190 - 250
# very over = 250 +

weight.iat$weight.factor <- cut(weight.iat$weight.new, breaks = c(0, 70, 95, 110, 150, 190, 250, 500))
table(weight.iat$weight.factor) # aiming for similar proportions to the iam weight factor variable
weight.iat$weight.factor <- factor(weight.iat$weight.factor, labels = c("Very underweight", "Moderately underweight",
                                                                        "Slightly underweight", "Neither underweight nor overweight",
                                                                        "Slightly overweight", "Moderately overweight",
                                                                        "Very overweight"))
table(weight.iat$weight.factor)/sum(table(weight.iat$weight.factor))
table(weight.iat$iam.new)/sum(table(weight.iat$iam.new))

## merge with iam
weight.iat$weight.COMB <- paste(weight.iat$iam.new, weight.iat$weight.factor)
table(weight.iat$weight.COMB)
weight.iat$weight.COMB <- gsub("NA ", "", weight.iat$weight.COMB)
weight.iat$weight.COMB <- gsub(" NA", "", weight.iat$weight.COMB)
weight.iat$weight.COMB <- recode(weight.iat$weight.COMB, "'NA' = NA")

weight.iat$weight.COMB <- factor(weight.iat$weight.COMB, levels = c("Very underweight", "Moderately underweight",
                                                                    "Slightly underweight", "Neither underweight nor overweight",
                                                                    "Slightly overweight", "Moderately overweight",
                                                                    "Very overweight"))
table(weight.iat$weight.COMB, weight.iat$year) # pseudo-complete data across all years
# (although the preferred variable to use would be the iam variable.)

## recode Stimuli --------
# it is recommended to check with Nicole Lofaro on the changes to the test
table(weight.iat$Stimuli, weight.iat$year)
weight.iat$Stimuli <- factor(weight.iat$Stimuli, labels = c("face", "bodyfig1", "bodyfig2"))
table(weight.iat$Stimuli, weight.iat$month)
weight.iat$Stimuli.new <- ifelse(weight.iat$year<2010, "1", weight.iat$Stimuli)
table(weight.iat$Stimuli.new, weight.iat$year)
weight.iat$Stimuli.new <- recode(weight.iat$Stimuli.new, "'1' = 'face'; '2' = 'body figure v1'; '3' = 'body figure v2'")

table(weight.iat$Stimuli.new, weight.iat$month)

## country of citizenship and residence --------------
## combine country cit and dem_citizenship
sort(table(weight.iat$countrycit))
sort(table(weight.iat$dem_citizenship))
sort(table(weight.iat$countrycit_num))
levels(weight.iat$countrycit)
levels(weight.iat$dem_citizenship)
levels(weight.iat$countrycit_num)

countrycodes <- read.csv("countrycodevector.csv") 
# to see which countries map onto which numbers/full names
# because we are restricting to US, we can just recode the US participants.

levels(weight.iat$countrycit)
weight.iat$countrycit <- gsub("  ", "", weight.iat$countrycit)
weight.iat$countrycit <- gsub(" ", "", weight.iat$countrycit)
weight.iat$countrycit <- factor(weight.iat$countrycit)
weight.iat$countrycit1 <- recode(weight.iat$countrycit, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(weight.iat$countrycit1))
weight.iat$countrycit1 <- recode(weight.iat$countrycit1, "'1' = 'US'")
levels(weight.iat$dem_citizenship)
weight.iat$countrycit2 <- recode(weight.iat$dem_citizenship, "'U.S.A.' = 'US'; 'U.S.A._duplicated_226' = 'US'")
levels(weight.iat$countrycit_num)
weight.iat$countrycit3 <- recode(weight.iat$countrycit_num, "'U.S.A. ' = 'US'")

weight.iat$countrycit.new <- paste(weight.iat$countrycit1, weight.iat$countrycit2)
sort(table(weight.iat$countrycit.new))
weight.iat$countrycit.new <- gsub("NA ", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- gsub(" NA", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- factor(weight.iat$countrycit.new)
weight.iat$countrycit.new <- recode(weight.iat$countrycit.new, "'US US' = 'US'")
sort(table(weight.iat$countrycit.new))

weight.iat$countrycit.new <- paste(weight.iat$countrycit.new, weight.iat$countrycit3)
sort(table(weight.iat$countrycit.new))
weight.iat$countrycit.new <- gsub("NA ", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- gsub(" NA", "", weight.iat$countrycit.new)
weight.iat$countrycit.new <- factor(weight.iat$countrycit.new)
weight.iat$countrycit.new <- recode(weight.iat$countrycit.new, "'US US' = 'US'")
sort(table(weight.iat$countrycit.new))

table(weight.iat$countrycit.new, weight.iat$month)["US",] # complete data after 0602

## repeat for country of residence
levels(weight.iat$countryres)
weight.iat$countryres <- gsub("  ", "", weight.iat$countryres)
weight.iat$countryres <- gsub(" ", "", weight.iat$countryres)
weight.iat$countryres <- factor(weight.iat$countryres)
weight.iat$countryres1 <- recode(weight.iat$countryres, "'.' = NA; '-999' = NA; '-9' = NA; '' = NA; 'nu' = NA")
sort(table(weight.iat$countryres1))
weight.iat$countryres1 <- recode(weight.iat$countryres1, "'1' = 'US'")
levels(weight.iat$dem_residence)
weight.iat$countryres2 <- recode(weight.iat$dem_residence, "'U.S.A.' = 'US'; 'U.S.A._duplicated_226' = 'US'")
levels(weight.iat$countryres_num)
weight.iat$countryres3 <- recode(weight.iat$countryres_num, "'U.S.A. ' = 'US'")

weight.iat$countryres.new <- paste(weight.iat$countryres1, weight.iat$countryres2)
sort(table(weight.iat$countryres.new))
weight.iat$countryres.new <- gsub("NA ", "", weight.iat$countryres.new)
weight.iat$countryres.new <- gsub(" NA", "", weight.iat$countryres.new)
weight.iat$countryres.new <- factor(weight.iat$countryres.new)
weight.iat$countryres.new <- recode(weight.iat$countryres.new, "'US US' = 'US'")
sort(table(weight.iat$countryres.new))

weight.iat$countryres.new <- paste(weight.iat$countryres.new, weight.iat$countryres3)
sort(table(weight.iat$countryres.new))
weight.iat$countryres.new <- gsub("NA ", "", weight.iat$countryres.new)
weight.iat$countryres.new <- gsub(" NA", "", weight.iat$countryres.new)
weight.iat$countryres.new <- factor(weight.iat$countryres.new)
weight.iat$countryres.new <- recode(weight.iat$countryres.new, "'US US' = 'US'")
sort(table(weight.iat$countryres.new))

table(weight.iat$countryres.new, weight.iat$month)["US",] # complete data after 0602


## zipcodes -----------
# import a list of zip codes with place names, etc.
zips <- read.csv("us_postal_codes.csv")
zips$X <- NULL
# add zeros to Postal.Code to make 5 digits.
zips$zip1 <- sprintf("%05d", zips$Postal.Code)

## now clean the zip code data
length(table(weight.iat$postcodelong))
length(table(weight.iat$postcodenow))

# make it a character to use regular expressions
weight.iat$zip <- as.character(weight.iat$postcodenow)

weight.iat$zip <- gsub("[[:punct:]]", "", weight.iat$zip) # remove punctuation
weight.iat$zip <- gsub("[[:space:]]", "", weight.iat$zip) # remove all white space
weight.iat$zip <- gsub("[^0-9]", "", weight.iat$zip) # remove all non-numeric characters

weight.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{1,4}\\b", "", weight.iat$zip) # remove all strings shorter than 5
weight.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{6,8}\\b", "", weight.iat$zip1) # remove all strings longer than 5, shorter than 9
weight.iat$zip1 <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", weight.iat$zip1) # remove all strings longer 9

weight.iat$zip1 <- as.character(weight.iat$zip1)

# add on the geographic information from the zipcodes
weight.iat.places <- merge(weight.iat, zips, by = "zip1", all = TRUE) 


## data subsetting -------------------------
# subset to US participants (by countryres.new), allows for retaining the most participants over time
weight.iatUSres <- weight.iat.places[weight.iat.places$countryres.new=="US",]
sort(table(weight.iatUSres$countryres.new))
## total with US country residence (and NAs omitted) = 635,936

## now keep only those with complete data for the following:
# dscore, att.new,
# sex, age, Order, ethnic.sex.new, politicalid, edu

## narrowing for implicit and explicit
weight.full <- weight.iatUSres[is.na(weight.iatUSres$dscore)==FALSE,] # 635,963
weight.full <- weight.full[is.na(weight.full$att.new)==FALSE,] # 620,714

weight.full.demogs <- weight.full[is.na(weight.full$sex.new)==FALSE,] # 617,681
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$age.new)==FALSE,] # 611,303
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$ethnic.race.new)==FALSE,] # 602,507
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$politicalid)==FALSE,] # 585,251
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$edu.new)==FALSE,] # 581,433


# also include the following variables
weight.full.demogs <- weight.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                           "age.new", "sex.new", "ethnic.race.new", "ethnicityomb", "raceomb.new",
                                           "politicalid", "edu.new", "weight.COMB", "iam.new",
                                           "Order", "num.new", "Stimuli.new",
                                           "countrycit.new", "countryres.new",
                                           "zip1", "State", "Place.Name", "State.Abbreviation", "County")]


table(weight.full.demogs$month.cent)
table(weight.full.demogs$month) # low observations in 2014, missing 1209, 1210, 1211, 1412.
length(table(weight.full.demogs$month.cent)) # 148 months



############################################################################################################
############################################################################################################
## CONFIRM DATE VARIABLES -------------------------
table(age.full.demogs$month.cent) # complete from 4 - 156
table(dis.full.demogs$month.cent) # complete from 5 - 156
table(race.full.demogs$month.cent) # complete from 4 - 156
table(sex.full.demogs$month.cent) # complete from 5 - 7 and 26 - 156
table(skin.full.demogs$month.cent) # complete from 5 - 156, missing 106, 107
table(weight.full.demogs$month.cent) # complete from 5 - 156, missing 105-107, 132 and low data from 125 - 131

# total number of months
length(table(age.full.demogs$month.cent)) # 153
length(table(dis.full.demogs$month.cent)) # 152
length(table(race.full.demogs$month.cent)) # 153
length(table(sex.full.demogs$month.cent)) # 134
length(table(skin.full.demogs$month.cent)) # 150
length(table(weight.full.demogs$month.cent)) # 148


## CENTER all the explicit variables: ------------------
# center1 is centered with  0 = neutral, center is mean-centered
table(age.full.demogs$att.new) # 1 = prefer old to young
age.full.demogs$att.center1 <- as.numeric(age.full.demogs$att.new)
age.full.demogs$att.center1 <- age.full.demogs$att.center1 - 4
age.full.demogs$att.center <- scale(age.full.demogs$att.center1, scale = FALSE)
table(age.full.demogs$att.center1)

table(dis.full.demogs$att.new) # 1 = prefer dis to abled
dis.full.demogs$att.center1 <- as.numeric(dis.full.demogs$att.new)
dis.full.demogs$att.center1 <- dis.full.demogs$att.center1 - 4
dis.full.demogs$att.center <- scale(dis.full.demogs$att.center1, scale = FALSE)
table(dis.full.demogs$att.center1)

table(race.full.demogs$att.new) # 1 = prefer Black to White
race.full.demogs$att.center1 <- as.numeric(race.full.demogs$att.new)
race.full.demogs$att.center1 <- race.full.demogs$att.center1 - 4
race.full.demogs$att.center <- scale(race.full.demogs$att.center1, scale = FALSE)
table(race.full.demogs$att.center1)

table(skin.full.demogs$att.new) # 1 = prefer Dark to Light
skin.full.demogs$att.center1 <- as.numeric(skin.full.demogs$att.new)
skin.full.demogs$att.center1 <- skin.full.demogs$att.center1 - 4
skin.full.demogs$att.center <- scale(skin.full.demogs$att.center1, scale = FALSE)
table(skin.full.demogs$att.center1)

table(sex.full.demogs$att.new) # 1 = prefer Gay to Straight
sex.full.demogs$att.center1 <- as.numeric(sex.full.demogs$att.new)
sex.full.demogs$att.center1 <- sex.full.demogs$att.center1 - 4
sex.full.demogs$att.center <- scale(sex.full.demogs$att.center1, scale = FALSE)
table(sex.full.demogs$att.center1)

table(weight.full.demogs$att.new)  # 1 = prefer fat to thin
weight.full.demogs$att.center1 <- as.numeric(weight.full.demogs$att.new)
weight.full.demogs$att.center1 <- weight.full.demogs$att.center1 - 4
weight.full.demogs$att.center <- scale(weight.full.demogs$att.center1, scale = FALSE)
table(weight.full.demogs$att.center1)


## CENTER all the demographic variables: ------------------------------------------------------------------------
# Age
age.full.demogs$age.center <- scale(age.full.demogs$age.new, center = TRUE, scale = FALSE)
dis.full.demogs$age.center <- scale(dis.full.demogs$age.new, center = TRUE, scale = FALSE)
race.full.demogs$age.center <- scale(race.full.demogs$age.new, center = TRUE, scale = FALSE)
sex.full.demogs$age.center <- scale(sex.full.demogs$age.new, center = TRUE, scale = FALSE)
skin.full.demogs$age.center <- scale(skin.full.demogs$age.new, center = TRUE, scale = FALSE)
weight.full.demogs$age.center <- scale(weight.full.demogs$age.new, center = TRUE, scale = FALSE)

# order
age.full.demogs$order.new <- relevel(age.full.demogs$Order, "Young+Good first")
dis.full.demogs$order.new <- relevel(dis.full.demogs$Order, "Abled+Good first")
race.full.demogs$order.new <- relevel(race.full.demogs$Order, "White+Good first")
sex.full.demogs$order.new <- relevel(sex.full.demogs$Order, "Straight+Good first")
skin.full.demogs$order.new <- relevel(skin.full.demogs$Order, "Light Skin+Good first")
weight.full.demogs$order.new <- relevel(weight.full.demogs$Order, "Thin+Good first")

# ethnic.race.new
race.full.demogs$ethnic.race.new <- relevel(race.full.demogs$ethnic.race.new, "White")
dis.full.demogs$ethnic.race.new <- relevel(dis.full.demogs$ethnic.race.new, "White")
sex.full.demogs$ethnic.race.new <- relevel(sex.full.demogs$ethnic.race.new, "White")
skin.full.demogs$ethnic.race.new <- relevel(skin.full.demogs$ethnic.race.new, "White")
weight.full.demogs$ethnic.race.new <- relevel(weight.full.demogs$ethnic.race.new, "White")
age.full.demogs$ethnic.race.new <- relevel(age.full.demogs$ethnic.race.new, "White")

race.full.demogs$ethnicrace.center <- as.numeric(race.full.demogs$ethnic.race.new)
race.full.demogs$ethnicrace.center <- scale(race.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)
dis.full.demogs$ethnicrace.center <- as.numeric(dis.full.demogs$ethnic.race.new)
dis.full.demogs$ethnicrace.center <- scale(dis.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)
sex.full.demogs$ethnicrace.center <- as.numeric(sex.full.demogs$ethnic.race.new)
sex.full.demogs$ethnicrace.center <- scale(sex.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)
skin.full.demogs$ethnicrace.center <- as.numeric(skin.full.demogs$ethnic.race.new)
skin.full.demogs$ethnicrace.center <- scale(skin.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)
weight.full.demogs$ethnicrace.center <- as.numeric(weight.full.demogs$ethnic.race.new)
weight.full.demogs$ethnicrace.center <- scale(weight.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)
age.full.demogs$ethnicrace.center <- as.numeric(age.full.demogs$ethnic.race.new)
age.full.demogs$ethnicrace.center <- scale(age.full.demogs$ethnicrace.center, center = TRUE, scale = FALSE)

# sex
table(age.full.demogs$sex.new)
table(dis.full.demogs$sex.new)
table(race.full.demogs$sex.new)
table(sex.full.demogs$sex.new)
table(skin.full.demogs$sex.new)
table(weight.full.demogs$sex.new)
race.full.demogs$sex.center <- as.numeric(race.full.demogs$sex.new)
race.full.demogs$sex.center <- scale(race.full.demogs$sex.center, center = TRUE, scale = FALSE)
dis.full.demogs$sex.center <- as.numeric(dis.full.demogs$sex.new)
dis.full.demogs$sex.center <- scale(dis.full.demogs$sex.center, center = TRUE, scale = FALSE)
sex.full.demogs$sex.center <- as.numeric(sex.full.demogs$sex.new)
sex.full.demogs$sex.center <- scale(sex.full.demogs$sex.center, center = TRUE, scale = FALSE)
skin.full.demogs$sex.center <- as.numeric(skin.full.demogs$sex.new)
skin.full.demogs$sex.center <- scale(skin.full.demogs$sex.center, center = TRUE, scale = FALSE)
weight.full.demogs$sex.center <- as.numeric(weight.full.demogs$sex.new)
weight.full.demogs$sex.center <- scale(weight.full.demogs$sex.center, center = TRUE, scale = FALSE)
age.full.demogs$sex.center <- as.numeric(age.full.demogs$sex.new)
age.full.demogs$sex.center <- scale(age.full.demogs$sex.center, center = TRUE, scale = FALSE)

# political id
table(age.full.demogs$politicalid)
table(race.full.demogs$politicalid)
table(dis.full.demogs$politicalid)
table(sex.full.demogs$politicalid)
table(skin.full.demogs$politicalid)
table(weight.full.demogs$politicalid)

age.full.demogs$politicalid.releveled <- as.numeric(age.full.demogs$politicalid)
age.full.demogs$politicalid.releveled <- age.full.demogs$politicalid.releveled - 4
age.full.demogs$politicalid.center <- scale(age.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)
dis.full.demogs$politicalid.releveled <- as.numeric(dis.full.demogs$politicalid)
dis.full.demogs$politicalid.releveled <- dis.full.demogs$politicalid.releveled - 4
dis.full.demogs$politicalid.center <- scale(dis.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)
race.full.demogs$politicalid.releveled <- as.numeric(race.full.demogs$politicalid)
race.full.demogs$politicalid.releveled <- race.full.demogs$politicalid.releveled - 4
race.full.demogs$politicalid.center <- scale(race.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)
sex.full.demogs$politicalid.releveled <- as.numeric(sex.full.demogs$politicalid)
sex.full.demogs$politicalid.releveled <- sex.full.demogs$politicalid.releveled - 4
sex.full.demogs$politicalid.center <- scale(sex.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)
skin.full.demogs$politicalid.releveled <- as.numeric(skin.full.demogs$politicalid)
skin.full.demogs$politicalid.releveled <- skin.full.demogs$politicalid.releveled - 4
skin.full.demogs$politicalid.center <- scale(skin.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)
weight.full.demogs$politicalid.releveled <- as.numeric(weight.full.demogs$politicalid)
weight.full.demogs$politicalid.releveled <- weight.full.demogs$politicalid.releveled - 4
weight.full.demogs$politicalid.center <- scale(weight.full.demogs$politicalid.releveled, center = TRUE, scale = FALSE)


# education, with "some college" (factor level 5) as 0.
table(age.full.demogs$edu.new)
age.full.demogs$edu.releveled <- as.numeric(age.full.demogs$edu.new)
age.full.demogs$edu.releveled <- age.full.demogs$edu.releveled - 5
age.full.demogs$edu.center <- scale(age.full.demogs$edu.releveled, center = TRUE, scale = FALSE)

table(dis.full.demogs$edu.new)
dis.full.demogs$edu.releveled <- as.numeric(dis.full.demogs$edu.new)
dis.full.demogs$edu.releveled <- dis.full.demogs$edu.releveled - 5
dis.full.demogs$edu.center <- scale(dis.full.demogs$edu.releveled, center = TRUE, scale = FALSE)

table(race.full.demogs$edu.new)
race.full.demogs$edu.releveled <- as.numeric(race.full.demogs$edu.new)
race.full.demogs$edu.releveled <- race.full.demogs$edu.releveled - 5
race.full.demogs$edu.center <- scale(race.full.demogs$edu.releveled, center = TRUE, scale = FALSE)

table(sex.full.demogs$edu.new)
sex.full.demogs$edu.releveled <- as.numeric(sex.full.demogs$edu.new)
sex.full.demogs$edu.releveled <- sex.full.demogs$edu.releveled - 5
sex.full.demogs$edu.center <- scale(sex.full.demogs$edu.releveled, center = TRUE, scale = FALSE)

table(skin.full.demogs$edu.new)
skin.full.demogs$edu.releveled <- as.numeric(skin.full.demogs$edu.new)
skin.full.demogs$edu.releveled <- skin.full.demogs$edu.releveled - 5
skin.full.demogs$edu.center <- scale(skin.full.demogs$edu.releveled, center = TRUE, scale = FALSE)

table(weight.full.demogs$edu.new)
weight.full.demogs$edu.releveled <- as.numeric(weight.full.demogs$edu.new)
weight.full.demogs$edu.releveled <- weight.full.demogs$edu.releveled - 5
weight.full.demogs$edu.center <- scale(weight.full.demogs$edu.releveled, center = TRUE, scale = FALSE)


# and attitude-specific variables
# disability
table(dis.full.demogs$dis.new) # 1 = no
dis.full.demogs$disabled.center <- as.numeric(dis.full.demogs$dis.new)
dis.full.demogs$disabled.center <- scale(dis.full.demogs$disabled.center, center = TRUE, scale = FALSE)

# sexuality
table(sex.full.demogs$sexuality.new) # 1 = straight
sex.full.demogs$sexuality.center <- relevel(sex.full.demogs$sexuality.new, "Straight")
table(sex.full.demogs$sexuality.center)
sex.full.demogs$sexuality.center <- as.numeric(sex.full.demogs$sexuality.new)
sex.full.demogs$sexuality.center <- scale(sex.full.demogs$sexuality.center, center = TRUE, scale = FALSE)

# weight
table(weight.full.demogs$weight.COMB)
weight.full.demogs$weight.center <- as.numeric(weight.full.demogs$weight.COMB)
weight.full.demogs$weight.center <- scale(weight.full.demogs$weight.center, center = TRUE, scale = FALSE)

table(weight.full.demogs$iam.new)
weight.full.demogs$iam.center <- as.numeric(weight.full.demogs$iam.new)
weight.full.demogs$iam.center <- scale(weight.full.demogs$iam.center, center = TRUE, scale = FALSE)


