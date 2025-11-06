
## Patterns of Implicit and Explicit Attitudes IV. 
## Script version date: September 1st, 2021

####################################################################################################################################

## This file demonstrates how we go from raw, public data to fully cleaned data

## It generates three clean files for each attitude:
# (1) One ending in .iat, which includes a variety of relevant variables, and is limited to those with complete IAT scores.
# (2) One ending in .full, which includes a variety of relevant variables, but is limited to those with complete IAT and explicit scores
# and only US participants
# (3) One ending in .full.demogs, which includes only a selection of variables (covariates), 
# and is limited to only those with complete data on implicit and explicit, as well as complete covariate data

## At the end of this file, we also center and make numeric all the demographic variables in the .full.demogs files.

# The full data object, with all .full.demogs files can be found in the full.demogs.RData data object

####################################################################################################################################
## Installing required packages
if (!require(car)) {install.packages("car", dependencies = TRUE); require(car)}
if (!require(chron)) {install.packages("chron", dependencies = TRUE); require(chron)}
if (!require(foreign)) {install.packages("foreign", dependencies = TRUE); require(foreign)}
if (!require(pastecs)) {install.packages("pastecs", dependencies = TRUE); require(pastecs)}


##############################################################################################################################################
## CLEAN THE RACE DATA ---------------------------
# setwd("**") # set working directory if necessary

# Load the individual files
race.iat17 <- read.spss("Race_IAT.public.2017.sav", to.data.frame = TRUE) # 945,101
race.iat18 <- read.spss("Race_IAT.public.2018.sav", to.data.frame = TRUE) # 859,472
race.iat19 <- read.spss("Race_IAT.public.2019.sav", to.data.frame = TRUE) # 875,209
race.iat20 <- read.spss("Race.IAT.public.2020.sav", to.data.frame = TRUE) # 

## To optimize, first reduce the number of variables (24 variables, different names across years)...
# list for subset of 2017 and 2018 data
subsetcolnames_race <- c("session_id", "session_status", "date", "year", "birthyear",
                         "num_002", "birthsex", "genderidentity",
                         "D_biep.White_Good_all", "Order",
                         "att_7", 
                         "countrycit_num", "countryres_num",
                         "edu", "edu_14",
                         "ethnicityomb",  "raceomb_002", "raceombmulti",
                         "politicalid_7",
                         "religionid", "religion2014",
                         "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2019 data
subsetcolnames_race2 <- c("session_id", "session_status", "date", "year", "birthyear",
                          "num_002", "birthSex", "genderIdentity",
                          "D_biep.White_Good_all", "Order",
                          "att7", 
                          "countrycit_num", "countryres_num",
                          "edu", "edu_14",
                          "ethnicityomb",  "raceomb_002", "raceombmulti",
                          "politicalid_7",
                          "religionid", "religion2014",
                          "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2020 data
subsetcolnames_race3 <- c("session_id", "session_status", "date", "year", "birthyear",
                          "num_002", "birthSex", "genderIdentity",
                          "D_biep.White_Good_all", "Order",
                          "att7", 
                          "countrycit_num", "countryres_num",
                          "edu", "edu_14",
                          "ethnicityomb",  "raceomb002", "raceombmulti",
                          "politicalid_7",
                          "religionid", "religion2014",
                          "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_race %in% colnames(race.iat17))) == length(subsetcolnames_race) #should be same length (TRUE)
length(which(subsetcolnames_race %in% colnames(race.iat18))) == length(subsetcolnames_race) #should be same length (TRUE) 
length(which(subsetcolnames_race2 %in% colnames(race.iat19))) == length(subsetcolnames_race2) #should be same length (TRUE)
length(which(subsetcolnames_race3 %in% colnames(race.iat20))) == length(subsetcolnames_race) #should be same length (TRUE)

## Then select just those columns from each data frame
race.iat17 <- race.iat17[subsetcolnames_race]
race.iat18 <- race.iat18[subsetcolnames_race]
race.iat19 <- race.iat19[subsetcolnames_race2]
colnames(race.iat19) <- subsetcolnames_race
race.iat20 <- race.iat20[subsetcolnames_race3]
colnames(race.iat20) <- subsetcolnames_race

## Issue to be cleaned individually: dates
race.iat17$date <- as.chron(ISOdate(1582, 10, 14) + race.iat17$date) # get the date in the right format
race.iat17$date <- as.Date(race.iat17$date)
race.iat18$date <- as.chron(ISOdate(1582, 10, 14) + race.iat18$date) # repeat for 2018
race.iat18$date <- as.Date(race.iat18$date)
race.iat19$date <- as.chron(ISOdate(1582, 10, 14) + race.iat19$date) # repeat for 2019
race.iat19$date <- as.Date(race.iat19$date)
race.iat20$date <- as.chron(ISOdate(1582, 10, 14) + race.iat20$date) # repeat for 2020
race.iat20$date <- as.Date(race.iat20$date)


## Further cleaning for 2020 data ----
# Country codes for 2020 data
race.iat20$countrycit_num <- recode(factor(race.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
race.iat20$countryres_num <- recode(factor(race.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")

# Num codes for 2020 data
table(race.iat20$num_002)
race.iat20$num_002 <- factor(race.iat20$num_002, levels = levels(race.iat19$num_002))
# Att codes for 2017 data
table(race.iat17$att_7)
race.iat17$att_7 <- factor(race.iat17$att_7, labels = levels(race.iat19$att_7))


## Lastly, rbind all dataframes together with the identical columns 
race.iat_all <- rbind(race.iat17, race.iat18, race.iat19, race.iat20)
write.csv(race.iat_all, file = "raceiat_all.csv") # write it out to speed up later processing
## Read in if there's an error in later processing
# race.iat_all <- read.csv("raceiat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
race.iat_all$session_status <- trimws(race.iat_all$session_status)
table(race.iat_all$session_status)
race.iat_all$session_status <- factor(race.iat_all$session_status, levels = c("", "C"),
                                      labels = c("notcomp", "comp"))

# get a month variable
race.iat_all$month <- format(race.iat_all$date, "%y%m")
table(race.iat_all$month)
race.iat_all$month.cent <- as.numeric(factor(race.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
# new total for 2017-2020 data = 4,437,070
race.iat_all <- race.iat_all[race.iat_all$month != "2101",]


# how many US subjects? -------------
sort(table(race.iat_all$countrycit_num))
sort(table(race.iat_all$countryres_num))

table(race.iat_all$countrycit_num, race.iat_all$month)["U.S.A. ",] # complete 
table(race.iat_all$countryres_num, race.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(race.iat_all$countryres_num)["U.S.A. "] # 2,621,634
# how many US complete?
table(race.iat_all$countryres_num[race.iat_all$session_status=="comp"])["U.S.A. "] # 1,799,915

# keep only complete, USA subjects --------
race.iat_all <- race.iat_all[race.iat_all$session_status=="comp",] # complete subjects = 2,218,938
race.iat_all <- race.iat_all[is.na(race.iat_all$D_biep.White_Good_all)==FALSE,] # complete subjects for IAT data = 2,137,300
race.iat_all <- race.iat_all[race.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 1,904,781


## dscore -----------------
names(race.iat_all)[which(names(race.iat_all)=="D_biep.White_Good_all")] <- "dscore"

## session id  -----------------
race.iat_all$session_id <- as.character(race.iat_all$session_id)

## age ------------
# working with birthyear
table(race.iat_all$birthyear, race.iat_all$year) 
table(race.iat_all$birthyear) 

# subtract birthyear from year of test
race.iat_all$age.new <- race.iat_all$year - race.iat_all$birthyear

table(race.iat_all$age.new)
race.iat_all$age.new <- factor(race.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
race.iat_all$age.new <- droplevels(race.iat_all$age.new)

table(race.iat_all$age.new)

race.iat_all$age.new <- as.numeric(as.character(race.iat_all$age.new))

table(race.iat_all$age.new, race.iat_all$year)
table(race.iat_all$age.new[race.iat_all$age.new>25], race.iat_all$month[race.iat_all$age.new>25]) # complete data

## num ----------------
table(race.iat_all$num_002)

# simply rename
names(race.iat_all)[which(names(race.iat_all)=="num_002")] <- "num.new"

table(race.iat_all$num.new)
table(race.iat_all$num.new, race.iat_all$month) # complete data


## gender -------------
table(race.iat_all$genderidentity)
race.iat_all$genderidentity <- trimws(race.iat_all$genderidentity)
race.iat_all$genderidentity_simp <- recode(race.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(race.iat_all$genderidentity_simp, race.iat_all$month) # complete data


## sex -------------
table(race.iat_all$birthsex)
race.iat_all$birthsex <- factor(race.iat_all$birthsex, levels = c(1,2), labels = c("male", "female"))
table(race.iat_all$birthsex, race.iat_all$month)



## race -----------------
table(race.iat_all$raceomb_002, race.iat_all$month)

# simply rename
names(race.iat_all)[which(names(race.iat_all)=="raceomb_002")] <- "race.new"

table(race.iat_all$race.new)
table(race.iat_all$race.new, race.iat_all$month) # complete data


## ethnicityomb -------------
## (again, with values determined from documentation and single year codes)
table(race.iat_all$ethnicityomb, race.iat_all$month)

# simply rename
names(race.iat_all)[which(names(race.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(race.iat_all$ethnicity.new)
table(race.iat_all$ethnicity.new, race.iat_all$month) # complete data



## explicit attitude -------------------
table(race.iat_all$att_7, race.iat_all$year) 

## get the right order and the right name for the final variable.
race.iat_all$att.new <- factor(race.iat_all$att_7, levels = c("I strongly prefer African Americans to European Americans.",
                                                              "I moderately prefer African Americans to European Americans.",
                                                              "I slightly prefer African Americans to European Americans.",
                                                              "I like European Americans and African Americans equally.",
                                                              "I slightly prefer European Americans to African Americans.",
                                                              "I moderately prefer European Americans to African Americans.",
                                                              "I strongly prefer European Americans to African Americans."))
table(race.iat_all$att.new, race.iat_all$month) # complete data 


## education -------------------
table(race.iat_all$edu, race.iat_all$month) # edu is the variable to use across years (complete)
race.iat_all$edu.new <- factor(race.iat_all$edu, labels = c("elementary school", "junior high", 
                                                            "some high school", "high school graduate", "some college",
                                                            "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                            "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
race.iat_all$edu.new <- factor(race.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(race.iat_all$edu.new, race.iat_all$month) # complete



## political ideology ------------------
table(race.iat_all$politicalid_7, race.iat_all$month) 

# simply rename
names(race.iat_all)[which(names(race.iat_all)=="politicalid_7")] <- "politicalid.new"

table(race.iat_all$politicalid.new)
table(race.iat_all$politicalid.new, race.iat_all$month) 


## religion -------
table(race.iat_all$religion2014, race.iat_all$year) 
table(race.iat_all$religionid, race.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

race.iat_all$rel.new <- factor(race.iat_all$religion2014, 
                                 labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(race.iat_all$rel.new, race.iat_all$month)


## brought website ----
table(race.iat_all$broughtwebsite)
race.iat_all$broughtwebsite <- trimws(race.iat_all$broughtwebsite)
race.iat_all$broughtwebsite <- factor(race.iat_all$broughtwebsite)
levels(race.iat_all$broughtwebsite)
race.iat_all$brought_simp <- car::recode(race.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(race.iat_all$brought_simp)
table(race.iat_all$brought_simp, race.iat_all$month)


## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
race.full <- race.iat_all[is.na(race.iat_all$dscore)==FALSE,] # 1,766,573
race.full <- race.full[is.na(race.full$att.new)==FALSE,] # 1,721,455

length(table(race.full$month)) ## all available months.

race.full.demogs <- race.full[is.na(race.full$genderidentity_simp)==FALSE,] # 1,721,455
length(table(race.full.demogs$month)) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$race.new)==FALSE,] # 1,601,580
length(table(race.full.demogs$month)) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$ethnicity.new)==FALSE,] # 1,543,329
length(table(race.full.demogs$month)) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$edu.new)==FALSE,] # 1,522,898
length(table(race.full.demogs$month)) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$age.new)==FALSE,] # 1,493,882
length(table(race.full.demogs$month)) ## no loss of months
race.full.demogs <- race.full.demogs[is.na(race.full.demogs$politicalid.new)==FALSE,] #1,474,545
length(table(race.full.demogs$month)) ## no loss of months

# also include the following variables: num and further error/RT variables
race.full.demogs <- race.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                       "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                       "politicalid.new", "edu.new", "Order", "num.new",
                                       "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(race.full.demogs$month.cent)
write.csv(race.full.demogs, "race.fulldemogs.csv")


##############################################################################################################################################
## CLEAN THE WEIGHT DATA  -----------------------------------------------------------------------------------------------------------------
# setwd("**")

# Load the individual files
weight.iat17 <- read.spss("Weight IAT.public.2017.sav", to.data.frame = TRUE) # 271,687
weight.iat18 <- read.spss("Weight_IAT.public.2018.sav", to.data.frame = TRUE) # 272,833
weight.iat19 <- read.spss("Weight IAT.public.2019.sav", to.data.frame = TRUE) # 307,718
weight.iat20 <- read.spss("Weight_IAT.public.2020.sav", to.data.frame = TRUE) # 437,825

## To optimize, first reduce the number of variables (24 variables, different names across years)...
# list for subset of 2017 and 2018 data
colnames(weight.iat17); colnames(weight.iat18); colnames(weight.iat19)
subsetcolnames_weight <- c("session_id", "session_status", "date", "year", "birthyear",
                         "num_002", "birthsex", "genderidentity",
                         "D_biep.Thin_Good_all", "Order", "iam_001",
                         "att7", 
                         "countrycit_num", "countryres_num",
                         "edu", "edu_14",
                         "ethnicityomb",  "raceomb_002", "raceombmulti",
                         "politicalid_7",
                         "religionid", "religion2014",
                         "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2018/2019 data
subsetcolnames_weight2 <- c("session_id", "session_status", "date", "year", "birthyear",
                          "num_002", "birthSex", "genderIdentity",
                          "D_biep.Thin_Good_all", "Order", "iam_001",
                          "att7", 
                          "countrycit_num", "countryres_num",
                          "edu", "edu_14",
                          "ethnicityomb",  "raceomb_002", "raceombmulti",
                          "politicalid_7",
                          "religionid", "religion2014",
                          "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_weight %in% colnames(weight.iat17))) == length(subsetcolnames_weight) #should be same length (TRUE)
length(which(subsetcolnames_weight2 %in% colnames(weight.iat18))) == length(subsetcolnames_weight2) #should be same length (TRUE) 
length(which(subsetcolnames_weight2 %in% colnames(weight.iat19))) == length(subsetcolnames_weight2) #should be same length (TRUE)
length(which(subsetcolnames_weight2 %in% colnames(weight.iat20))) == length(subsetcolnames_weight2) #should be same length (TRUE)

## Then select just those columns from each data frame
weight.iat17 <- weight.iat17[subsetcolnames_weight]
weight.iat18 <- weight.iat18[subsetcolnames_weight2]
weight.iat19 <- weight.iat19[subsetcolnames_weight2]
weight.iat20 <- weight.iat20[subsetcolnames_weight2]
colnames(weight.iat18) <- subsetcolnames_weight
colnames(weight.iat19) <- subsetcolnames_weight
colnames(weight.iat20) <- subsetcolnames_weight

## Issue to be cleaned individually: dates
weight.iat17$date <- as.chron(ISOdate(1582, 10, 14) + weight.iat17$date) # get the date in the right format
weight.iat17$date <- as.Date(weight.iat17$date)
weight.iat18$date <- as.chron(ISOdate(1582, 10, 14) + weight.iat18$date) # repeat for 2018
weight.iat18$date <- as.Date(weight.iat18$date)
weight.iat19$date <- as.chron(ISOdate(1582, 10, 14) + weight.iat19$date) # repeat for 2019
weight.iat19$date <- as.Date(weight.iat19$date)
weight.iat20$date <- as.chron(ISOdate(1582, 10, 14) + weight.iat20$date) # repeat for 2020
weight.iat20$date <- as.Date(weight.iat20$date)


## Further cleaning for yearly data ----
# Country codes for 2020 data
weight.iat20$countrycit_num <- recode(factor(weight.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
weight.iat20$countryres_num <- recode(factor(weight.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")
# Att codes for 2018 data
table(weight.iat18$att7)
weight.iat18$att7 <- factor(weight.iat18$att7, 
                            levels = levels(weight.iat19$att7),
                            labels = levels(weight.iat19$att7))
weight.iat18$att7_new <- NULL

## Lastly, rbind all dataframes together with the identical columns 
weight.iat_all <- rbind(weight.iat17, weight.iat18, weight.iat19, weight.iat20)
write.csv(weight.iat_all, file = "weightiat_all.csv") # write it out to speed up later processing
# Read back in if there is a later error in processing
# weight.iat_all <- read.csv("weightiat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
weight.iat_all$session_status <- trimws(weight.iat_all$session_status)
table(weight.iat_all$session_status)
weight.iat_all$session_status <- factor(weight.iat_all$session_status, levels = c("", "C"),
                                      labels = c("notcomp", "comp"))

# get a month variable
weight.iat_all$month <- format(as.Date(weight.iat_all$date), "%y%m")
table(weight.iat_all$month)
weight.iat_all$month.cent <- as.numeric(factor(weight.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
weight.iat_all <- weight.iat_all[weight.iat_all$month != "2101",]
# new total for 2017-2020 data = 1,289,874
dim(weight.iat_all)[1]

# how many US subjects? -------------
sort(table(weight.iat_all$countrycit_num))
sort(table(weight.iat_all$countryres_num))

table(weight.iat_all$countrycit_num, weight.iat_all$month)["U.S.A. ",] # complete 
table(weight.iat_all$countryres_num, weight.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(weight.iat_all$countryres_num)["U.S.A. "] # 717,043
# how many US complete?
table(weight.iat_all$countryres_num[weight.iat_all$session_status=="comp"])["U.S.A. "] # 499,204

# keep only complete, USA subjects --------
weight.iat_all <- weight.iat_all[weight.iat_all$session_status=="comp",] # complete subjects = 649,606
weight.iat_all <- weight.iat_all[is.na(weight.iat_all$D_biep.Thin_Good_all)==FALSE,] # complete subjects for IAT data = 644,929
weight.iat_all <- weight.iat_all[weight.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 552,136


## dscore -----------------
names(weight.iat_all)[which(names(weight.iat_all)=="D_biep.Thin_Good_all")] <- "dscore"

## session id  -----------------
weight.iat_all$session_id <- as.character(weight.iat_all$session_id)

## age ------------
# working with birthyear
table(weight.iat_all$birthyear, weight.iat_all$year) 
table(weight.iat_all$birthyear) 

# subtract birthyear from year of test
weight.iat_all$age.new <- weight.iat_all$year - weight.iat_all$birthyear

table(weight.iat_all$age.new)
weight.iat_all$age.new <- factor(weight.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
weight.iat_all$age.new <- droplevels(weight.iat_all$age.new)

table(weight.iat_all$age.new)

weight.iat_all$age.new <- as.numeric(as.character(weight.iat_all$age.new))

table(weight.iat_all$age.new, weight.iat_all$year)
table(weight.iat_all$age.new[weight.iat_all$age.new>25], weight.iat_all$month[weight.iat_all$age.new>25]) # complete data

## num ----------------
table(weight.iat_all$num_002)

# simply rename
names(weight.iat_all)[which(names(weight.iat_all)=="num_002")] <- "num.new"

table(weight.iat_all$num.new)
table(weight.iat_all$num.new, weight.iat_all$month) # complete data


## gender -------------
table(weight.iat_all$genderidentity)
weight.iat_all$genderidentity <- trimws(weight.iat_all$genderidentity)
weight.iat_all$genderidentity_simp <- recode(weight.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(weight.iat_all$genderidentity_simp, weight.iat_all$month) # complete data


## sex -------------
table(weight.iat_all$birthsex)
weight.iat_all$birthsex <- factor(weight.iat_all$birthsex, levels = c("male", "female"), labels = c("male", "female"))
table(weight.iat_all$birthsex, weight.iat_all$month)



## race -----------------
table(weight.iat_all$raceomb_002, weight.iat_all$month)

# simply rename
names(weight.iat_all)[which(names(weight.iat_all)=="raceomb_002")] <- "race.new"

table(weight.iat_all$race.new)
table(weight.iat_all$race.new, weight.iat_all$month) # complete data


## ethnicityomb -------------
## (again, with values determined from documentation and single year codes)
table(weight.iat_all$ethnicityomb, weight.iat_all$month)

# simply rename
names(weight.iat_all)[which(names(weight.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(weight.iat_all$ethnicity.new)
table(weight.iat_all$ethnicity.new, weight.iat_all$month) # complete data



## explicit attitude -------------------
table(weight.iat_all$att7, weight.iat_all$year) 

## get the right order and the right name for the final variable.
weight.iat_all$att.new <- factor(weight.iat_all$att7, levels = c("I strongly prefer Fat People to Thin People.",
                                                              "I moderately prefer Fat People to Thin People.",
                                                              "I slightly prefer Fat People to Thin People.",
                                                              "I like Thin People and Fat People equally.",
                                                              "I slightly prefer Thin People to Fat People.",
                                                              "I moderately prefer Thin People to Fat People.",
                                                              "I strongly prefer Thin People to Fat People"),
                                 labels = c("I strongly prefer Fat People to Thin People.",
                                            "I moderately prefer Fat People to Thin People.",
                                            "I slightly prefer Fat People to Thin People.",
                                            "I like Thin People and Fat People equally.",
                                            "I slightly prefer Thin People to Fat People.",
                                            "I moderately prefer Thin People to Fat People.",
                                            "I strongly prefer Thin People to Fat People."))
table(weight.iat_all$att.new, weight.iat_all$month) # complete data now.


## education -------------------
table(weight.iat_all$edu, weight.iat_all$month) # edu is the variable to use across years (complete)
weight.iat_all$edu.new <- factor(weight.iat_all$edu, labels = c("elementary school", "junior high", 
                                                            "some high school", "high school graduate", "some college",
                                                            "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                            "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
weight.iat_all$edu.new <- factor(weight.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(weight.iat_all$edu.new, weight.iat_all$month) # complete



## political ideology ------------------
table(weight.iat_all$politicalid_7, weight.iat_all$month) 

# simply rename
names(weight.iat_all)[which(names(weight.iat_all)=="politicalid_7")] <- "politicalid.new"

table(weight.iat_all$politicalid.new)
table(weight.iat_all$politicalid.new, weight.iat_all$month) 


## religion -------
table(weight.iat_all$religion2014, weight.iat_all$year) 
table(weight.iat_all$religionid, weight.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

weight.iat_all$rel.new <- factor(weight.iat_all$religion2014, 
                           labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(weight.iat_all$rel.new, weight.iat_all$month)
## available consistently



## additional variable (individual weight) -----------------
table(weight.iat_all$iam_001, weight.iat_all$month) # available across time.

# simply rename
names(weight.iat_all)[which(names(weight.iat_all)=="iam_001")] <- "reportedweight"

# and reorder
weight.iat_all$reportedweight <- factor(weight.iat_all$reportedweight, levels = c("Very underweight",
                                                                                  "Moderately underweight",
                                                                                  "Slightly underweight",
                                                                                  "Neither underweight nor overweight",
                                                                                  "Slightly overweight",
                                                                                  "Moderately overweight",
                                                                                  "Very overweight"))

table(weight.iat_all$reportedweight)
table(weight.iat_all$reportedweight, weight.iat_all$month) 

## brought website ----
table(weight.iat_all$broughtwebsite)
weight.iat_all$broughtwebsite <- trimws(weight.iat_all$broughtwebsite)
weight.iat_all$broughtwebsite <- factor(weight.iat_all$broughtwebsite)
levels(weight.iat_all$broughtwebsite)
weight.iat_all$brought_simp <- car::recode(weight.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(weight.iat_all$brought_simp)
table(weight.iat_all$brought_simp, weight.iat_all$month)


## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
weight.full <- weight.iat_all[is.na(weight.iat_all$dscore)==FALSE,] # 496,167
weight.full <- weight.full[is.na(weight.full$att.new)==FALSE,] # 488,475

length(table(weight.full$month)) ## all available months (48)

weight.full.demogs <- weight.full[is.na(weight.full$genderidentity_simp)==FALSE,] # 488,475
length(table(weight.full.demogs$month)) ## no loss of months
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$race.new)==FALSE,] # 453,888
length(table(weight.full.demogs$month)) ## no loss of months
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$ethnicity.new)==FALSE,] # 438,296
length(table(weight.full.demogs$month)) ## no loss of months
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$edu.new)==FALSE,] # 431,675
length(table(weight.full.demogs$month)) ## no loss of months
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$age.new)==FALSE,] # 424.998
length(table(weight.full.demogs$month)) ## no loss of months
weight.full.demogs <- weight.full.demogs[is.na(weight.full.demogs$politicalid.new)==FALSE,] # 418,277
length(table(weight.full.demogs$month)) # no loss of months

# also include the following variables: num and further error/RT variables
weight.full.demogs <- weight.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                       "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                       "politicalid.new", "edu.new", "Order", "num.new", "reportedweight",
                                       "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(weight.full.demogs$month.cent)
write.csv(weight.full.demogs, "weight.fulldemogs.csv")




####################################################################################################################################
## CLEAN THE AGE DATA  -----------------------------------------------------------------------------------------------------------------
# setwd("**")

# Load the individual files
age.iat17 <- read.spss("Age IAT.public.2017.sav", to.data.frame = TRUE) # 239,054
age.iat18 <- read.spss("Age IAT.public.2018.sav", to.data.frame = TRUE) # 255,650
age.iat19 <- read.spss("Age IAT.public.2019.sav", to.data.frame = TRUE) # 301,859
age.iat20 <- read.spss("Age_IAT.public.2020.sav", to.data.frame = TRUE) # 411,330

## To optimize, first reduce the number of variables (24 variables, different names across years)...
# list for subset of 2017 and 2018 data
colnames(age.iat17); colnames(age.iat18)
subsetcolnames_age <- c("session_id", "session_status", "date", "year", "birthyear",
                           "num_002", "birthsex", "genderidentity",
                           "D_biep.Young_Good_all", "Order", 
                           "att_7", 
                           "countrycit_num", "countryres_num",
                           "edu", "edu_14",
                           "ethnicityomb",  "raceomb_002", "raceombmulti",
                           "politicalid_7",
                           "religionid", "religion2014",
                           "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2018/2019 data
subsetcolnames_age2 <- c("session_id", "session_status", "date", "year", "birthyear",
                            "num_002", "birthSex", "genderIdentity",
                            "D_biep.Young_Good_all", "Order", 
                            "att7", 
                            "countrycit_num", "countryres_num",
                            "edu", "edu_14",
                            "ethnicityomb",  "raceomb_002", "raceombmulti",
                            "politicalid_7",
                            "religionid", "religion2014",
                            "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_age %in% colnames(age.iat17))) == length(subsetcolnames_age) #should be same length (TRUE)
length(which(subsetcolnames_age2 %in% colnames(age.iat18))) == length(subsetcolnames_age2) #should be same length (TRUE) 
length(which(subsetcolnames_age2 %in% colnames(age.iat19))) == length(subsetcolnames_age2) #should be same length (TRUE)
length(which(subsetcolnames_age2 %in% colnames(age.iat20))) == length(subsetcolnames_age2) #should be same length (TRUE)

## Then select just those columns from each data frame
age.iat17 <- age.iat17[subsetcolnames_age]
age.iat18 <- age.iat18[subsetcolnames_age2]
age.iat19 <- age.iat19[subsetcolnames_age2]
age.iat20 <- age.iat20[subsetcolnames_age2]
colnames(age.iat18) <- subsetcolnames_age
colnames(age.iat19) <- subsetcolnames_age
colnames(age.iat20) <- subsetcolnames_age

## Issue to be cleaned individually: dates
age.iat17$date <- as.chron(ISOdate(1582, 10, 14) + age.iat17$date) # get the date in the right format
age.iat17$date <- as.Date(age.iat17$date)
age.iat18$date <- as.chron(ISOdate(1582, 10, 14) + age.iat18$date) # repeat for 2018
age.iat18$date <- as.Date(age.iat18$date)
age.iat19$date <- as.chron(ISOdate(1582, 10, 14) + age.iat19$date) # repeat for 2019
age.iat19$date <- as.Date(age.iat19$date)
age.iat20$date <- as.chron(ISOdate(1582, 10, 14) + age.iat20$date) # repeat for 2020
age.iat20$date <- as.Date(age.iat20$date)


## Further cleaning for 2020 data ----
# Country codes for 2020 data
age.iat20$countrycit_num <- recode(factor(age.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
age.iat20$countryres_num <- recode(factor(age.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")
# Raceomb codes
table(age.iat20$raceomb_002)
age.iat20$raceomb_002 <- factor(age.iat20$raceomb_002, 
                                levels = levels(age.iat19$raceomb_002),
                                labels = levels(age.iat19$raceomb_002))

## Lastly, rbind all dataframes together with the identical columns 
age.iat_all <- rbind(age.iat17, age.iat18, age.iat19, age.iat20)
write.csv(age.iat_all, file = "ageiat_all.csv") # write it out to speed up later processing
# Read back in if there is a later error in processing
# age.iat_all <- read.csv("ageiat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
age.iat_all$session_status <- trimws(age.iat_all$session_status)
table(age.iat_all$session_status)
age.iat_all$session_status <- factor(age.iat_all$session_status, levels = c("", "C"),
                                        labels = c("notcomp", "comp"))

# get a month variable
age.iat_all$month <- format(as.Date(age.iat_all$date), "%y%m")
table(age.iat_all$month)
age.iat_all$month.cent <- as.numeric(factor(age.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
age.iat_all <- age.iat_all[age.iat_all$month != "2101",]
# new total for 2017-2020 data = 1,207,730
dim(age.iat_all)[1]

# how many US subjects? -------------
sort(table(age.iat_all$countrycit_num))
sort(table(age.iat_all$countryres_num))

table(age.iat_all$countrycit_num, age.iat_all$month)["U.S.A. ",] # complete 
table(age.iat_all$countryres_num, age.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(age.iat_all$countryres_num)["U.S.A. "] # 675,293
# how many US complete?
table(age.iat_all$countryres_num[age.iat_all$session_status=="comp"])["U.S.A. "] # 488,513

# keep only complete, USA subjects --------
age.iat_all <- age.iat_all[age.iat_all$session_status=="comp",] # complete subjects = 635,000
age.iat_all <- age.iat_all[is.na(age.iat_all$D_biep.Young_Good_all)==FALSE,] # complete subjects for IAT data = 630,175
age.iat_all <- age.iat_all[age.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 526,440


## dscore -----------------
names(age.iat_all)[which(names(age.iat_all)=="D_biep.Young_Good_all")] <- "dscore"

## session id  -----------------
age.iat_all$session_id <- as.character(age.iat_all$session_id)

## age ------------
# working with birthyear
table(age.iat_all$birthyear, age.iat_all$year) 
table(age.iat_all$birthyear) 

# subtract birthyear from year of test
age.iat_all$age.new <- age.iat_all$year - age.iat_all$birthyear

table(age.iat_all$age.new)
age.iat_all$age.new <- factor(age.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
age.iat_all$age.new <- droplevels(age.iat_all$age.new)

table(age.iat_all$age.new)

age.iat_all$age.new <- as.numeric(as.character(age.iat_all$age.new))

table(age.iat_all$age.new, age.iat_all$year)
table(age.iat_all$age.new[age.iat_all$age.new>25], age.iat_all$month[age.iat_all$age.new>25]) # complete data

## num ----------------
table(age.iat_all$num_002)

# simply rename
names(age.iat_all)[which(names(age.iat_all)=="num_002")] <- "num.new"

table(age.iat_all$num.new)
table(age.iat_all$num.new, age.iat_all$month) # complete data


## gender -------------
table(age.iat_all$genderidentity)
age.iat_all$genderidentity <- trimws(age.iat_all$genderidentity)
age.iat_all$genderidentity_simp <- recode(age.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(age.iat_all$genderidentity_simp, age.iat_all$month) # complete data


## sex -------------
table(age.iat_all$birthsex)
age.iat_all$birthsex <- factor(age.iat_all$birthsex, levels = c(1, 2), labels = c("male", "female"))
table(age.iat_all$birthsex, age.iat_all$month)



## race -----------------
table(age.iat_all$raceomb_002, age.iat_all$month)

# simply rename
names(age.iat_all)[which(names(age.iat_all)=="raceomb_002")] <- "race.new"

table(age.iat_all$race.new)
table(age.iat_all$race.new, age.iat_all$month) # complete data


## ethnicityomb -------------
table(age.iat_all$ethnicityomb, age.iat_all$month)
levels(age.iat_all$ethnicityomb)
age.iat_all$ethnicityomb <- factor(age.iat_all$ethnicityomb, levels = c("Hispanic or Latino",
                                                                        "Not Hispanic or Latino",
                                                                        "Unknown"))

# simply rename
names(age.iat_all)[which(names(age.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(age.iat_all$ethnicity.new)
table(age.iat_all$ethnicity.new, age.iat_all$month) # complete data



## explicit attitude -------------------
table(age.iat_all$att_7, age.iat_all$year) 

## get the right order and the right name for the final variable.
age.iat_all$att.new <- factor(age.iat_all$att_7, levels = c("I strongly prefer old people to young people.",
                                                                 "I moderately prefer old people to young people.",
                                                                 "I slightly prefer old people to young people.",
                                                                 "I like young people and old people equally",
                                                                 "I slightly prefer young people to old people.",
                                                                 "I moderately prefer young people to old people.",
                                                                 "I strongly prefer young people to old people."),
                                 labels = c("I strongly prefer old people to young people.",
                                            "I moderately prefer old people to young people.",
                                            "I slightly prefer old people to young people.",
                                            "I like young people and old people equally.",
                                            "I slightly prefer young people to old people.",
                                            "I moderately prefer young people to old people.",
                                            "I strongly prefer young people to old people."))
table(age.iat_all$att.new, age.iat_all$month) # complete data now.


## education -------------------
table(age.iat_all$edu, age.iat_all$month) # edu is the variable to use across years (complete)
age.iat_all$edu.new <- factor(age.iat_all$edu, labels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
age.iat_all$edu.new <- factor(age.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                    "some high school", "high school graduate", "some college",
                                                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                    "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(age.iat_all$edu.new, age.iat_all$month) # complete



## political ideology ------------------
table(age.iat_all$politicalid_7, age.iat_all$month) 

# simply rename
names(age.iat_all)[which(names(age.iat_all)=="politicalid_7")] <- "politicalid.new"

table(age.iat_all$politicalid.new)
table(age.iat_all$politicalid.new, age.iat_all$month) 

## religion -------
table(age.iat_all$religion2014, age.iat_all$year) 
table(age.iat_all$religionid, age.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

age.iat_all$rel.new <- factor(age.iat_all$religion2014, 
                                 labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(age.iat_all$rel.new, age.iat_all$month)
## available consistently


## brought website ----
table(age.iat_all$broughtwebsite)
age.iat_all$broughtwebsite <- trimws(age.iat_all$broughtwebsite)
age.iat_all$broughtwebsite <- factor(age.iat_all$broughtwebsite)
levels(age.iat_all$broughtwebsite)
age.iat_all$brought_simp <- car::recode(age.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(age.iat_all$brought_simp)
table(age.iat_all$brought_simp, age.iat_all$month)


## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
age.full <- age.iat_all[is.na(age.iat_all$dscore)==FALSE,] # 485,402
age.full <- age.full[is.na(age.full$att.new)==FALSE,] # 478,807

length(table(age.full$month)) ## all available months.

age.full.demogs <- age.full[is.na(age.full$genderidentity_simp)==FALSE,] # 478,807
length(table(age.full.demogs$month)) ## no loss of months
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$race.new)==FALSE,] # 446,463
length(table(age.full.demogs$month)) ## no loss of months
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$ethnicity.new)==FALSE,] # 429.953
length(table(age.full.demogs$month)) ## no loss of months
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$edu.new)==FALSE,] # 418,137
length(table(age.full.demogs$month)) ## no loss of months
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$age.new)==FALSE,] # 418,137
length(table(age.full.demogs$month)) ## no loss of months
age.full.demogs <- age.full.demogs[is.na(age.full.demogs$politicalid.new)==FALSE,] # 409,214
length(table(age.full.demogs$month)) # no loss of months

# also include the following variables: num and further error/RT variables
age.full.demogs <- age.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                           "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                           "politicalid.new", "edu.new", "Order", "num.new",
                                           "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(age.full.demogs$month.cent)
write.csv(age.full.demogs, "age.fulldemogs.csv")


####################################################################################################################################
## CLEAN THE DISABILITY DATA  -----------------------------------------------------------------------------------------------------------------
# setwd("**")

# Load the individual files
dis.iat17 <- read.spss("Disability IAT.public.2017.sav", to.data.frame = TRUE) # 116,799
dis.iat18 <- read.spss("Disability_IAT.public.2018.sav", to.data.frame = TRUE) # 124,265
dis.iat19 <- read.spss("Disability IAT.public.2019.sav", to.data.frame = TRUE) # 148,802
dis.iat20 <- read.spss("Disability_IAT.public.2020.sav", to.data.frame = TRUE) # 209,662

## To optimize, first reduce the number of variables (26 variables, different names across years)...
# list for subset of 2017 and 2019 data
colnames(dis.iat17); colnames(dis.iat19)
subsetcolnames_disability <- c("session_id", "session_status", "date", "year", "birthyear",
                           "num_002", "birthsex", "genderidentity",
                           "D_biep.Abled_Good_all", "Order", "disabled_001", "distypes",
                           "att_7", 
                           "countrycit_num", "countryres_num",
                           "edu", "edu_14",
                           "ethnicityomb",  "raceomb_002", "raceombmulti",
                           "politicalid_7",
                           "religionid", "religion2014",
                           "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2019 data
subsetcolnames_disability2 <- c("session_id", "session_status", "date", "year", "birthyear",
                            "num_002", "birthSex", "genderIdentity",
                            "D_biep.Abled_Good_all", "Order", "disabled_001", "distypes",
                            "att_7", 
                            "countrycit_num", "countryres_num",
                            "edu", "edu_14",
                            "ethnicityomb",  "raceomb_002", "raceombmulti",
                            "politicalid_7",
                            "religionid", "religion2014",
                            "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_disability %in% colnames(dis.iat17))) == length(subsetcolnames_disability) #should be same length (TRUE)
length(which(subsetcolnames_disability %in% colnames(dis.iat18))) == length(subsetcolnames_disability) #should be same length (TRUE) 
length(which(subsetcolnames_disability2 %in% colnames(dis.iat19))) == length(subsetcolnames_disability2) #should be same length (TRUE)
length(which(subsetcolnames_disability2 %in% colnames(dis.iat20))) == length(subsetcolnames_disability2) #should be same length (TRUE)

## Then select just those columns from each data frame
dis.iat17 <- dis.iat17[subsetcolnames_disability]
dis.iat18 <- dis.iat18[subsetcolnames_disability]
dis.iat19 <- dis.iat19[subsetcolnames_disability2]
dis.iat20 <- dis.iat20[subsetcolnames_disability2]
colnames(dis.iat19) <- subsetcolnames_disability
colnames(dis.iat20) <- subsetcolnames_disability

## Issue to be cleaned individually: dates
dis.iat17$date <- as.chron(ISOdate(1582, 10, 14) + dis.iat17$date) # get the date in the right format
dis.iat17$date <- as.Date(dis.iat17$date)
dis.iat18$date <- as.chron(ISOdate(1582, 10, 14) + dis.iat18$date) # repeat for 2018
dis.iat18$date <- as.Date(dis.iat18$date)
dis.iat19$date <- as.chron(ISOdate(1582, 10, 14) + dis.iat19$date) # repeat for 2019
dis.iat19$date <- as.Date(dis.iat19$date)
dis.iat20$date <- as.chron(ISOdate(1582, 10, 14) + dis.iat20$date) # repeat for 2020
dis.iat20$date <- as.Date(dis.iat20$date)


## Further cleaning for 2020 data ----
# Country codes for 2020 data
dis.iat20$countrycit_num <- recode(factor(dis.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
dis.iat20$countryres_num <- recode(factor(dis.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")

## Lastly, rbind all dataframes together with the identical columns 
dis.iat_all <- rbind(dis.iat17, dis.iat18, dis.iat19, dis.iat20)
write.csv(dis.iat_all, file = "disabilityiat_all.csv") # write it out to speed up later processing
# Read back in if there is a later error in processing
# dis.iat_all <- read.csv("disabilityiat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
dis.iat_all$session_status <- trimws(dis.iat_all$session_status)
table(dis.iat_all$session_status)
dis.iat_all$session_status <- factor(dis.iat_all$session_status, levels = c("", "C"),
                                        labels = c("notcomp", "comp"))

# get a month variable
dis.iat_all$month <- format(as.Date(dis.iat_all$date), "%y%m")
table(dis.iat_all$month)
dis.iat_all$month.cent <- as.numeric(factor(dis.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
dis.iat_all <- dis.iat_all[dis.iat_all$month != "2101",]
# new total for 2017-2020 data = 599,462
dim(dis.iat_all)[1]

# how many US subjects? -------------
sort(table(dis.iat_all$countrycit_num))
sort(table(dis.iat_all$countryres_num))

table(dis.iat_all$countrycit_num, dis.iat_all$month)["U.S.A. ",] # complete 
table(dis.iat_all$countryres_num, dis.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(dis.iat_all$countryres_num)["U.S.A. "] # 328,751
# how many US complete?
table(dis.iat_all$countryres_num[dis.iat_all$session_status=="comp"])["U.S.A. "] # 238,844

# keep only complete, USA subjects --------
dis.iat_all <- dis.iat_all[dis.iat_all$session_status=="comp",] # complete subjects = 308,826
dis.iat_all <- dis.iat_all[is.na(dis.iat_all$D_biep.Abled_Good_all)==FALSE,] # complete subjects for IAT data = 306,517
dis.iat_all <- dis.iat_all[dis.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 266,348


## dscore -----------------
names(dis.iat_all)[which(names(dis.iat_all)=="D_biep.Abled_Good_all")] <- "dscore"

## session id  -----------------
dis.iat_all$session_id <- as.character(dis.iat_all$session_id)

## age ------------
# working with birthyear
table(dis.iat_all$birthyear, dis.iat_all$year) 
table(dis.iat_all$birthyear) 

# subtract birthyear from year of test
dis.iat_all$age.new <- dis.iat_all$year - dis.iat_all$birthyear

table(dis.iat_all$age.new)
dis.iat_all$age.new <- factor(dis.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
dis.iat_all$age.new <- droplevels(dis.iat_all$age.new)

table(dis.iat_all$age.new)

dis.iat_all$age.new <- as.numeric(as.character(dis.iat_all$age.new))

table(dis.iat_all$age.new, dis.iat_all$year)
table(dis.iat_all$age.new[dis.iat_all$age.new>25], dis.iat_all$month[dis.iat_all$age.new>25]) # complete data

## num ----------------
table(dis.iat_all$num_002)

# simply rename
names(dis.iat_all)[which(names(dis.iat_all)=="num_002")] <- "num.new"

table(dis.iat_all$num.new)
table(dis.iat_all$num.new, dis.iat_all$month) # complete data


## gender -------------
table(dis.iat_all$genderidentity)
dis.iat_all$genderidentity <- trimws(dis.iat_all$genderidentity)
dis.iat_all$genderidentity_simp <- recode(dis.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(dis.iat_all$genderidentity_simp, dis.iat_all$month) # complete data


## sex -------------
table(dis.iat_all$birthsex)
dis.iat_all$birthsex <- factor(dis.iat_all$birthsex, levels = c("Male", "Female"), labels = c("male", "female"))
table(dis.iat_all$birthsex, dis.iat_all$month)



## race -----------------
table(dis.iat_all$raceomb_002, dis.iat_all$month)

# simply rename
names(dis.iat_all)[which(names(dis.iat_all)=="raceomb_002")] <- "race.new"

table(dis.iat_all$race.new)
table(dis.iat_all$race.new, dis.iat_all$month) # complete data


## ethnicityomb -------------
table(dis.iat_all$ethnicityomb, dis.iat_all$month)

# simply rename
names(dis.iat_all)[which(names(dis.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(dis.iat_all$ethnicity.new)
table(dis.iat_all$ethnicity.new, dis.iat_all$month) # complete data



## explicit attitude -------------------
table(dis.iat_all$att_7, dis.iat_all$year) 
levels(dis.iat_all$att_7)

## get the right order and the right name for the final variable.
dis.iat_all$att.new <- factor(dis.iat_all$att_7, levels = c("I strongly prefer disabled people to abled people.",
                                                                 "I moderately prefer disabled people to abled people.",
                                                                 "I slightly prefer disabled people to abled people.",
                                                                 "I like abled people and disabled people. equally.",
                                                                 "I slightly prefer abled people to disabled people..",
                                                                 "I moderately prefer abled people to disabled people..",
                                                                 "I strongly prefer abled people to disabled people."),
                                 labels = c("I strongly prefer disabled people to abled people.",
                                            "I moderately prefer disabled people to abled people.",
                                            "I slightly prefer disabled people to abled people.",
                                            "I like abled people and disabled people equally.",
                                            "I slightly prefer abled people to disabled people.",
                                            "I moderately prefer abled people to disabled people.",
                                            "I strongly prefer abled people to disabled people."))
table(dis.iat_all$att.new, dis.iat_all$month) # complete data now.


## education -------------------
table(dis.iat_all$edu, dis.iat_all$month) # edu is the variable to use across years (complete)
dis.iat_all$edu.new <- factor(dis.iat_all$edu, levels = c(1:14),
                                labels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
dis.iat_all$edu.new <- factor(dis.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                    "some high school", "high school graduate", "some college",
                                                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                    "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(dis.iat_all$edu.new, dis.iat_all$month) # complete



## political ideology ------------------
table(dis.iat_all$politicalid_7, dis.iat_all$month) 

# simply rename
names(dis.iat_all)[which(names(dis.iat_all)=="politicalid_7")] <- "politicalid.new"

table(dis.iat_all$politicalid.new)
table(dis.iat_all$politicalid.new, dis.iat_all$month) 

## religion -------
table(dis.iat_all$religion2014, dis.iat_all$year) 
table(dis.iat_all$religionid, dis.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

dis.iat_all$rel.new <- factor(dis.iat_all$religion2014, 
                                 labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(dis.iat_all$rel.new, dis.iat_all$month)
## available consistently



## additional variable (reported disability) -----------------
# disability (yes/no)
table(dis.iat_all$disabled_001, dis.iat_all$month) # available across time.
# simply rename
names(dis.iat_all)[which(names(dis.iat_all)=="disabled_001")] <- "reporteddis"

table(dis.iat_all$reporteddis, dis.iat_all$month) 


## brought website ----
table(dis.iat_all$broughtwebsite)
dis.iat_all$broughtwebsite <- trimws(dis.iat_all$broughtwebsite)
dis.iat_all$broughtwebsite <- factor(dis.iat_all$broughtwebsite)
levels(dis.iat_all$broughtwebsite)
dis.iat_all$brought_simp <- car::recode(dis.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(dis.iat_all$brought_simp)
table(dis.iat_all$brought_simp, dis.iat_all$month)



## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
disability.full <- dis.iat_all[is.na(dis.iat_all$dscore)==FALSE,] # 237,399
disability.full <- disability.full[is.na(disability.full$att.new)==FALSE,] # 232,744

length(table(disability.full$month)) ## all available months.

disability.full.demogs <- disability.full[is.na(disability.full$genderidentity_simp)==FALSE,] # 232,744
length(table(disability.full.demogs$month)) ## no loss of months
disability.full.demogs <- disability.full.demogs[is.na(disability.full.demogs$race.new)==FALSE,] # 137,566
length(table(disability.full.demogs$month)) ## no loss of months
disability.full.demogs <- disability.full.demogs[is.na(disability.full.demogs$ethnicity.new)==FALSE,] # 214,508
length(table(disability.full.demogs$month)) ## no loss of months
disability.full.demogs <- disability.full.demogs[is.na(disability.full.demogs$edu.new)==FALSE,] # 211,559
length(table(disability.full.demogs$month)) ## no loss of months
disability.full.demogs <- disability.full.demogs[is.na(disability.full.demogs$age.new)==FALSE,] # 207,818
length(table(disability.full.demogs$month)) ## no loss of months
disability.full.demogs <- disability.full.demogs[is.na(disability.full.demogs$politicalid.new)==FALSE,] # 204,025
length(table(disability.full.demogs$month)) # no loss of months

# also include the following variables: num and further error/RT variables
disability.full.demogs <- disability.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                           "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                           "politicalid.new", "edu.new", "Order", "num.new", "reporteddis",
                                           "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(disability.full.demogs$month.cent)
write.csv(disability.full.demogs, "disability.fulldemogs.csv")


##############################################################################################################################################
## CLEAN THE SEXUALITY DATA  -----------------------------------------------------------------------------------------------------------------
# setwd("**")

# Load the individual files
sexuality.iat17 <- read.spss("Sexuality IAT.public.2017.sav", to.data.frame = TRUE) # 346,024
sexuality.iat18 <- read.spss("Sexuality IAT.public.2018.sav", to.data.frame = TRUE) # 337,793
sexuality.iat19 <- read.spss("Sexuality IAT.public.2019.sav", to.data.frame = TRUE) # 373,969
sexuality.iat20 <- read.spss("Sexuality_IAT.public.2020.sav", to.data.frame = TRUE) # 511,161

## To optimize, first reduce the number of variables (24 variables, different names across years)...
# list for subset of 2017 and 2018 data
colnames(sexuality.iat17); colnames(sexuality.iat18); colnames(sexuality.iat19)
subsetcolnames_sexuality <- c("session_id", "session_status", "date", "year", "birthyear",
                           "num_002", "birthsex", "genderidentity",
                           "D_biep.Straight_Good_all", "Order", "sexuality_5new",
                           "att_7", 
                           "countrycit_num", "countryres_num",
                           "edu", "edu_14",
                           "ethnicityomb",  "raceomb_002", "raceombmulti",
                           "politicalid_7",
                           "religionid", "religion2014",
                           "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2018/2019 data
subsetcolnames_sexuality2 <- c("session_id", "session_status", "date", "year", "birthyear",
                            "num_002", "birthSex", "genderIdentity",
                            "D_biep.Straight_Good_all", "Order", "sexuality_5",
                            "att_7", 
                            "countrycit_num", "countryres_num",
                            "edu", "edu_14",
                            "ethnicityomb",  "raceomb_002", "raceombmulti",
                            "politicalid_7",
                            "religionid", "religion2014",
                            "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_sexuality %in% colnames(sexuality.iat17))) == length(subsetcolnames_sexuality) #should be same length (TRUE)
length(which(subsetcolnames_sexuality2 %in% colnames(sexuality.iat18))) == length(subsetcolnames_sexuality2) #should be same length (TRUE) 
length(which(subsetcolnames_sexuality2 %in% colnames(sexuality.iat19))) == length(subsetcolnames_sexuality2) #should be same length (TRUE)
length(which(subsetcolnames_sexuality2 %in% colnames(sexuality.iat20))) == length(subsetcolnames_sexuality2) #should be same length (TRUE)

## Then select just those columns from each data frame
sexuality.iat17 <- sexuality.iat17[subsetcolnames_sexuality]
sexuality.iat18 <- sexuality.iat18[subsetcolnames_sexuality2]
sexuality.iat19 <- sexuality.iat19[subsetcolnames_sexuality2]
sexuality.iat20 <- sexuality.iat20[subsetcolnames_sexuality2]
colnames(sexuality.iat18) <- subsetcolnames_sexuality
colnames(sexuality.iat19) <- subsetcolnames_sexuality
colnames(sexuality.iat20) <- subsetcolnames_sexuality

## Issue to be cleaned individually: dates
sexuality.iat17$date <- as.chron(ISOdate(1582, 10, 14) + sexuality.iat17$date) # get the date in the right format
sexuality.iat17$date <- as.Date(sexuality.iat17$date)
sexuality.iat18$date <- as.chron(ISOdate(1582, 10, 14) + sexuality.iat18$date) # repeat for 2018
sexuality.iat18$date <- as.Date(sexuality.iat18$date)
sexuality.iat19$date <- as.chron(ISOdate(1582, 10, 14) + sexuality.iat19$date) # repeat for 2019
sexuality.iat19$date <- as.Date(sexuality.iat19$date)
sexuality.iat20$date <- as.chron(ISOdate(1582, 10, 14) + sexuality.iat20$date) # repeat for 2020
sexuality.iat20$date <- as.Date(sexuality.iat20$date)


## Further cleaning for 2020 data ----
# Country codes for 2020 data
sexuality.iat20$countrycit_num <- recode(factor(sexuality.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
sexuality.iat20$countryres_num <- recode(factor(sexuality.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")

## Lastly, rbind all dataframes together with the identical columns 
sexuality.iat_all <- rbind(sexuality.iat17, sexuality.iat18, sexuality.iat19, sexuality.iat20)
write.csv(sexuality.iat_all, file = "sexualityiat_all.csv") # write it out to speed up later processing
# Read back in if there is a later error in processing
# sexuality.iat_all <- read.csv("sexualityiat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
sexuality.iat_all$session_status <- trimws(sexuality.iat_all$session_status)
table(sexuality.iat_all$session_status)
sexuality.iat_all$session_status <- factor(sexuality.iat_all$session_status, levels = c("", "C"),
                                        labels = c("notcomp", "comp"))

# get a month variable
sexuality.iat_all$month <- format(as.Date(sexuality.iat_all$date), "%y%m")
table(sexuality.iat_all$month)
sexuality.iat_all$month.cent <- as.numeric(factor(sexuality.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
sexuality.iat_all <- sexuality.iat_all[sexuality.iat_all$month != "2101",]
# new total for 2017-2020 data = 1,568,750
dim(sexuality.iat_all)[1]

# how many US subjects? -------------
sort(table(sexuality.iat_all$countrycit_num))
sort(table(sexuality.iat_all$countryres_num))

table(sexuality.iat_all$countrycit_num, sexuality.iat_all$month)["U.S.A. ",] # complete 
table(sexuality.iat_all$countryres_num, sexuality.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(sexuality.iat_all$countryres_num)["U.S.A. "] # 867,949
# how many US complete?
table(sexuality.iat_all$countryres_num[sexuality.iat_all$session_status=="comp"])["U.S.A. "] # 598,374

# keep only complete, USA subjects --------
sexuality.iat_all <- sexuality.iat_all[sexuality.iat_all$session_status=="comp",] # complete subjects = 792,223
sexuality.iat_all <- sexuality.iat_all[is.na(sexuality.iat_all$D_biep.Straight_Good_all)==FALSE,] # complete subjects for IAT data = 785,898
sexuality.iat_all <- sexuality.iat_all[sexuality.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 645,290


## dscore -----------------
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="D_biep.Straight_Good_all")] <- "dscore"

## session id  -----------------
sexuality.iat_all$session_id <- as.character(sexuality.iat_all$session_id)

## age ------------
# working with birthyear
table(sexuality.iat_all$birthyear, sexuality.iat_all$year) 
table(sexuality.iat_all$birthyear) 

# subtract birthyear from year of test
sexuality.iat_all$age.new <- sexuality.iat_all$year - sexuality.iat_all$birthyear

table(sexuality.iat_all$age.new)
sexuality.iat_all$age.new <- factor(sexuality.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
sexuality.iat_all$age.new <- droplevels(sexuality.iat_all$age.new)

table(sexuality.iat_all$age.new)

sexuality.iat_all$age.new <- as.numeric(as.character(sexuality.iat_all$age.new))

table(sexuality.iat_all$age.new, sexuality.iat_all$year)
table(sexuality.iat_all$age.new[sexuality.iat_all$age.new>25], sexuality.iat_all$month[sexuality.iat_all$age.new>25]) # complete data

## num ----------------
table(sexuality.iat_all$num_002)

# simply rename
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="num_002")] <- "num.new"

table(sexuality.iat_all$num.new)
table(sexuality.iat_all$num.new, sexuality.iat_all$month) # complete data


## gender -------------
table(sexuality.iat_all$genderidentity)
sexuality.iat_all$genderidentity <- trimws(sexuality.iat_all$genderidentity)
sexuality.iat_all$genderidentity_simp <- recode(sexuality.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(sexuality.iat_all$genderidentity_simp, sexuality.iat_all$month) # complete data


## sex -------------
table(sexuality.iat_all$birthsex)
sexuality.iat_all$birthsex <- factor(sexuality.iat_all$birthsex, levels = c(1,2), labels = c("male", "female"))
table(sexuality.iat_all$birthsex, sexuality.iat_all$month)



## race -----------------
table(sexuality.iat_all$raceomb_002, sexuality.iat_all$month)

# simply rename
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="raceomb_002")] <- "race.new"

table(sexuality.iat_all$race.new)
table(sexuality.iat_all$race.new, sexuality.iat_all$month) # complete data


## ethnicityomb -------------
## (again, with values determined from documentation and single year codes)
table(sexuality.iat_all$ethnicityomb, sexuality.iat_all$month)

# simply rename
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(sexuality.iat_all$ethnicity.new)
table(sexuality.iat_all$ethnicity.new, sexuality.iat_all$month) # complete data



## explicit attitude -------------------
table(sexuality.iat_all$att_7, sexuality.iat_all$year) 

## get the right order and the right name for the final variable.
sexuality.iat_all$att.new <- factor(sexuality.iat_all$att_7, levels = c("I strongly prefer Gay People to Straight People.",
                                                                 "I moderately prefer Gay People to Straight People.",
                                                                 "I slightly prefer Gay People to Straight People.",
                                                                 "I like Straight People and Gay People equally.",
                                                                 "I slightly prefer Straight People to Gay People.",
                                                                 "I moderately prefer Straight People to Gay People.",
                                                                 "I strongly prefer Straight People to Gay People."))
table(sexuality.iat_all$att.new, sexuality.iat_all$month) # complete data now.


## education -------------------
table(sexuality.iat_all$edu, sexuality.iat_all$month) # edu is the variable to use across years (complete)
sexuality.iat_all$edu.new <- factor(sexuality.iat_all$edu, labels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
sexuality.iat_all$edu.new <- factor(sexuality.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                    "some high school", "high school graduate", "some college",
                                                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                    "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(sexuality.iat_all$edu.new, sexuality.iat_all$month) # complete



## political ideology ------------------
table(sexuality.iat_all$politicalid_7, sexuality.iat_all$month) 

# simply rename
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="politicalid_7")] <- "politicalid.new"

table(sexuality.iat_all$politicalid.new)
table(sexuality.iat_all$politicalid.new, sexuality.iat_all$month) 

## religion -------
table(sexuality.iat_all$religion2014, sexuality.iat_all$year) 
table(sexuality.iat_all$religionid, sexuality.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

sexuality.iat_all$rel.new <- factor(sexuality.iat_all$religion2014, 
                                 labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(sexuality.iat_all$rel.new, sexuality.iat_all$month)
## available consistently



## additional variable (individual sexuality) -----------------
table(sexuality.iat_all$sexuality_5new, sexuality.iat_all$month) # available across time.

# simply rename
names(sexuality.iat_all)[which(names(sexuality.iat_all)=="sexuality_5new")] <- "sexuality"

# and make one simplified
sexuality.iat_all$sexuality_simp <- recode(sexuality.iat_all$sexuality, "'Heterosexual or Straight' = 'Straight';
                                           'Lesbian or Gay' = 'Lesbian or Gay'; else = 'Other'")
sexuality.iat_all$sexuality_simp <- relevel(sexuality.iat_all$sexuality_simp, "Straight")

table(sexuality.iat_all$sexuality)
table(sexuality.iat_all$sexuality_simp)
table(sexuality.iat_all$sexuality_simp, sexuality.iat_all$month) 


## brought website ----
table(sexuality.iat_all$broughtwebsite)
sexuality.iat_all$broughtwebsite <- trimws(sexuality.iat_all$broughtwebsite)
sexuality.iat_all$broughtwebsite <- factor(sexuality.iat_all$broughtwebsite)
levels(sexuality.iat_all$broughtwebsite)
sexuality.iat_all$brought_simp <- car::recode(sexuality.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(sexuality.iat_all$brought_simp)
table(sexuality.iat_all$brought_simp, sexuality.iat_all$month)


## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
sexuality.full <- sexuality.iat_all[is.na(sexuality.iat_all$dscore)==FALSE,] # 594,348
sexuality.full <- sexuality.full[is.na(sexuality.full$att.new)==FALSE,] # 586,683

length(table(sexuality.full$month)) ## all available months.

sexuality.full.demogs <- sexuality.full[is.na(sexuality.full$genderidentity_simp)==FALSE,] # 586,683
length(table(sexuality.full.demogs$month)) ## no loss of months
sexuality.full.demogs <- sexuality.full.demogs[is.na(sexuality.full.demogs$race.new)==FALSE,] # 547,613
length(table(sexuality.full.demogs$month)) ## no loss of months
sexuality.full.demogs <- sexuality.full.demogs[is.na(sexuality.full.demogs$ethnicity.new)==FALSE,] # 528,904
length(table(sexuality.full.demogs$month)) ## no loss of months
sexuality.full.demogs <- sexuality.full.demogs[is.na(sexuality.full.demogs$edu.new)==FALSE,] # 519,504
length(table(sexuality.full.demogs$month)) ## no loss of months
sexuality.full.demogs <- sexuality.full.demogs[is.na(sexuality.full.demogs$age.new)==FALSE,] # 511,904
length(table(sexuality.full.demogs$month)) ## no loss of months
sexuality.full.demogs <- sexuality.full.demogs[is.na(sexuality.full.demogs$politicalid.new)==FALSE,] # 505,262
length(table(sexuality.full.demogs$month)) # no loss of months

# also include the following variables: num and further error/RT variables
sexuality.full.demogs <- sexuality.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                           "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                           "politicalid.new", "edu.new", "Order", "num.new", "sexuality", "sexuality_simp",
                                           "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(sexuality.full.demogs$month.cent)
write.csv(sexuality.full.demogs, "sexuality.fulldemogs.csv")


##############################################################################################################################################
## CLEAN THE SKIN-TONE DATA  -----------------------------------------------------------------------------------------------------------------
# setwd("**")

# Load the individual files
skin.iat17 <- read.spss("Skin IAT.public.2017.sav", to.data.frame = TRUE) # 257,180
skin.iat18 <- read.spss("Skin_Tone_IAT.public.2018.sav", to.data.frame = TRUE) # 250,812
skin.iat19 <- read.spss("Skin IAT.public.2019.sav", to.data.frame = TRUE) # 278,600
skin.iat20 <- read.spss("Skin IAT.public.2020.sav", to.data.frame = TRUE) # 293,602

## To optimize, first reduce the number of variables (24 variables, different names across years)...
# list for subset of 2017 and 2018 data
colnames(skin.iat17); colnames(skin.iat18); colnames(skin.iat19); colnames(skin.iat20)
subsetcolnames_skin <- c("session_id", "session_status", "date", "year", "birthyear",
                           "num_002", "birthsex", "genderidentity",
                           "D_biep.LightSkin_Good_all", "Order", "myskin",
                           "att7", 
                           "countrycit_num", "countryres_num",
                           "edu", "edu_14",
                           "ethnicityomb",  "raceomb_002", "raceombmulti",
                           "politicalid_7",
                           "religionid", "religion2014",
                           "STATE", "MSAName", "CountyNo", "broughtwebsite")
# list for subset of 2019/2020 data
subsetcolnames_skin2 <- c("session_id", "session_status", "date", "year", "birthyear",
                            "num_002", "birthSex", "genderIdentity",
                            "D_biep.LightSkin_Good_all", "Order", "myskin",
                            "att7", 
                            "countrycit_num", "countryres_num",
                            "edu", "edu_14",
                            "ethnicityomb",  "raceomb_002", "raceombmulti",
                            "politicalid_7",
                            "religionid", "religion2014",
                            "STATE", "MSAName", "CountyNo", "broughtwebsite")
length(which(subsetcolnames_skin %in% colnames(skin.iat17))) == length(subsetcolnames_skin) #should be same length (TRUE)
length(which(subsetcolnames_skin %in% colnames(skin.iat18))) == length(subsetcolnames_skin) #should be same length (TRUE) 
length(which(subsetcolnames_skin2 %in% colnames(skin.iat19))) == length(subsetcolnames_skin2) #should be same length (TRUE)
length(which(subsetcolnames_skin2 %in% colnames(skin.iat20))) == length(subsetcolnames_skin2) #should be same length (TRUE)

## Then select just those columns from each data frame
skin.iat17 <- skin.iat17[subsetcolnames_skin]
skin.iat18 <- skin.iat18[subsetcolnames_skin]
skin.iat19 <- skin.iat19[subsetcolnames_skin2]
skin.iat20 <- skin.iat20[subsetcolnames_skin2]
colnames(skin.iat19) <- subsetcolnames_skin
colnames(skin.iat20) <- subsetcolnames_skin

## Issue to be cleaned individually: dates
skin.iat17$date <- as.chron(ISOdate(1582, 10, 14) + skin.iat17$date) # get the date in the right format
skin.iat17$date <- as.Date(skin.iat17$date)
skin.iat18$date <- as.chron(ISOdate(1582, 10, 14) + skin.iat18$date) # repeat for 2018
skin.iat18$date <- as.Date(skin.iat18$date)
skin.iat19$date <- as.chron(ISOdate(1582, 10, 14) + skin.iat19$date) # repeat for 2019
skin.iat19$date <- as.Date(skin.iat19$date)
skin.iat20$date <- as.chron(ISOdate(1582, 10, 14) + skin.iat20$date) # repeat for 2020
skin.iat20$date <- as.Date(skin.iat20$date)


## Further cleaning for 2020 data ----
# Country codes for 2020 data
sort(levels(skin.iat20$countrycit_num))
skin.iat20$countrycit_num <- recode(factor(skin.iat20$countrycit_num), "'U.S.A.' = 'U.S.A. '")
skin.iat20$countryres_num <- recode(factor(skin.iat20$countryres_num), "'U.S.A.' = 'U.S.A. '")

## Lastly, rbind all dataframes together with the identical columns 
skin.iat_all <- rbind(skin.iat17, skin.iat18, skin.iat19, skin.iat20)
write.csv(skin.iat_all, file = "skiniat_all.csv") # write it out to speed up later processing
# Read back in if there is a later error in processing
# skin.iat_all <- read.csv("skiniat_all.csv")


## Now process the remainder of the data as before -----
# clean the "session status"
skin.iat_all$session_status <- trimws(skin.iat_all$session_status)
table(skin.iat_all$session_status)
skin.iat_all$session_status <- factor(skin.iat_all$session_status, levels = c("", "C"),
                                        labels = c("notcomp", "comp"))

# get a month variable
skin.iat_all$month <- format(as.Date(skin.iat_all$date), "%y%m")
table(skin.iat_all$month)
skin.iat_all$month.cent <- as.numeric(factor(skin.iat_all$month))

# keep only those with month.cent > 2017, 1 and < 2020, 12
skin.iat_all <- skin.iat_all[skin.iat_all$month != "2101",]
# new total for 2017-2020 data = 1,286,545
dim(skin.iat_all)[1]

# how many US subjects? -------------
sort(table(skin.iat_all$countrycit_num))
sort(table(skin.iat_all$countryres_num))

table(skin.iat_all$countrycit_num, skin.iat_all$month)["U.S.A. ",] # complete 
table(skin.iat_all$countryres_num, skin.iat_all$month)["U.S.A. ",] # complete 

# For table reporting
# how many US overall?
table(skin.iat_all$countryres_num)["U.S.A. "] # 693,763
# how many US complete?
table(skin.iat_all$countryres_num[skin.iat_all$session_status=="comp"])["U.S.A. "] # 482,660

# keep only complete, USA subjects --------
skin.iat_all <- skin.iat_all[skin.iat_all$session_status=="comp",] # complete subjects = 630,778
skin.iat_all <- skin.iat_all[is.na(skin.iat_all$D_biep.LightSkin_Good_all)==FALSE,] # complete subjects for IAT data = 626,015
skin.iat_all <- skin.iat_all[skin.iat_all$countryres_num=="U.S.A. ",] # complete subjects for USA, IAT data = 529,574


## dscore -----------------
names(skin.iat_all)[which(names(skin.iat_all)=="D_biep.LightSkin_Good_all")] <- "dscore"

## session id  -----------------
skin.iat_all$session_id <- as.character(skin.iat_all$session_id)

## age ------------
# working with birthyear
table(skin.iat_all$birthyear, skin.iat_all$year) 
table(skin.iat_all$birthyear) 

# subtract birthyear from year of test
skin.iat_all$age.new <- skin.iat_all$year - skin.iat_all$birthyear

table(skin.iat_all$age.new)
skin.iat_all$age.new <- factor(skin.iat_all$age.new, levels = c(6:100)) # factor and remove kids under 6 years, over 100.
skin.iat_all$age.new <- droplevels(skin.iat_all$age.new)

table(skin.iat_all$age.new)

skin.iat_all$age.new <- as.numeric(as.character(skin.iat_all$age.new))

table(skin.iat_all$age.new, skin.iat_all$year)
table(skin.iat_all$age.new[skin.iat_all$age.new>25], skin.iat_all$month[skin.iat_all$age.new>25]) # complete data

## num ----------------
table(skin.iat_all$num_002)

# simply rename
names(skin.iat_all)[which(names(skin.iat_all)=="num_002")] <- "num.new"

table(skin.iat_all$num.new)
table(skin.iat_all$num.new, skin.iat_all$month) # complete data


## gender -------------
table(skin.iat_all$genderidentity)
skin.iat_all$genderidentity <- trimws(skin.iat_all$genderidentity)
skin.iat_all$genderidentity_simp <- recode(skin.iat_all$genderidentity, "'[1]' = 'male'; '[2]' = 'female'; else = 'other'")
table(skin.iat_all$genderidentity_simp, skin.iat_all$month) # complete data


## sex -------------
table(skin.iat_all$birthsex)
skin.iat_all$birthsex <- factor(skin.iat_all$birthsex, levels = c(1, 2), labels = c("male", "female"))
table(skin.iat_all$birthsex, skin.iat_all$month)



## race -----------------
table(skin.iat_all$raceomb_002, skin.iat_all$month)

# simply rename
names(skin.iat_all)[which(names(skin.iat_all)=="raceomb_002")] <- "race.new"

table(skin.iat_all$race.new)
table(skin.iat_all$race.new, skin.iat_all$month) # complete data


## ethnicityomb -------------
## (again, with values determined from documentation and single year codes)
table(skin.iat_all$ethnicityomb, skin.iat_all$month)

# simply rename
names(skin.iat_all)[which(names(skin.iat_all)=="ethnicityomb")] <- "ethnicity.new"

table(skin.iat_all$ethnicity.new)
table(skin.iat_all$ethnicity.new, skin.iat_all$month) # complete data



## explicit attitude -------------------
table(skin.iat_all$att7, skin.iat_all$year) 

## get the right order and the right name for the final variable.
skin.iat_all$att.new <- factor(skin.iat_all$att7, levels = c("I strongly prefer Dark Skinned People to Light Skinned People.",
                                                                 "I moderately prefer Dark Skinned People to Light Skinned People.",
                                                                 "I slightly prefer Dark Skinned People to Light Skinned People.",
                                                                 "I like Light Skinned People and Dark Skinned People equally.",
                                                                 "I slightly prefer Light Skinned People to Dark Skinned People.",
                                                                 "I moderately prefer Light Skinned People to Dark Skinned People.",
                                                                 "I strongly prefer Light Skinned People to Dark Skinned People."))
table(skin.iat_all$att.new, skin.iat_all$month) # complete data now.


## education -------------------
table(skin.iat_all$edu, skin.iat_all$month) # edu is the variable to use across years (complete)
skin.iat_all$edu.new <- factor(skin.iat_all$edu, labels = c("elementary school", "junior high", 
                                                                "some high school", "high school graduate", "some college",
                                                                "associates degree", "bachelors degree", "some graduate school", "masters degree", 
                                                                "J.D.", "M.D.", "Ph.D.", "other advanced degree", "M.B.A."))
# get the correct order (moving MBA to other professional degrees)
skin.iat_all$edu.new <- factor(skin.iat_all$edu.new, levels = c("elementary school", "junior high", 
                                                                    "some high school", "high school graduate", "some college",
                                                                    "associates degree", "bachelors degree", "some graduate school", "masters degree",
                                                                    "M.B.A.", "J.D.", "M.D.", "Ph.D.", "other advanced degree"))

table(skin.iat_all$edu.new, skin.iat_all$month) # complete



## political ideology ------------------
table(skin.iat_all$politicalid_7, skin.iat_all$month) 

# simply rename
names(skin.iat_all)[which(names(skin.iat_all)=="politicalid_7")] <- "politicalid.new"

# and reorder
skin.iat_all$politicalid.new <- factor(skin.iat_all$politicalid.new, levels = c("strongly conservative",
                                                                                "moderately conservative",
                                                                                "slightly conservative",
                                                                                "neutral",
                                                                                "slightly liberal",
                                                                                "moderately liberal",
                                                                                "strongly liberal"))

table(skin.iat_all$politicalid.new)
table(skin.iat_all$politicalid.new, skin.iat_all$month) 

## religion -------
table(skin.iat_all$religion2014, skin.iat_all$year) 
table(skin.iat_all$religionid, skin.iat_all$year) ## religiosity

# Get down to 8 factor levels
# Jewish, Christian (Catholicism or Orthodox), Christian (Protestantism), 
# Islam, Buddhism/Eastern, Hindu, Other, Non-religion

skin.iat_all$rel.new <- factor(skin.iat_all$religion2014, 
                                 labels = c("buddhist", "catorth", "prot", "hindu", "jewish", "muslim", "non", "other"))

table(skin.iat_all$rel.new, skin.iat_all$month)
## available consistently




## additional variable (individual skintone) -----------------
table(skin.iat_all$myskin, skin.iat_all$month) # available across time.

# simply rename
names(skin.iat_all)[which(names(skin.iat_all)=="myskin")] <- "reportedskin"

# and reorder
skin.iat_all$reportedskin <- factor(skin.iat_all$reportedskin, levels = c("Very light",
                                                              "Light",
                                                              "Somewhat light",
                                                              "Medium",
                                                              "Somewhat Dark",
                                                              "Dark",
                                                              "Very dark"))

table(skin.iat_all$reportedskin)
table(skin.iat_all$reportedskin, skin.iat_all$month) 

## brought website ----
table(skin.iat_all$broughtwebsite)
skin.iat_all$broughtwebsite <- trimws(skin.iat_all$broughtwebsite)
skin.iat_all$broughtwebsite <- factor(skin.iat_all$broughtwebsite)
levels(skin.iat_all$broughtwebsite)
skin.iat_all$brought_simp <- car::recode(skin.iat_all$broughtwebsite, "'Assignment for school' = 'assign';
                                         'Assignment for work' = 'assign'; 
                                         'Mention in a news story (any medium)' = 'search';
                                         'Mention or link at a non-news Internet site' = 'search';
                                         'My Internet search for this topic or a rela' = 'search';
                                         'My Internet search for this topic or a related topic' = 'search';
                                         'Recommendation of a friend or co-worker' = 'search';
                                         'Other' = 'other'; else = NA")
table(skin.iat_all$brought_simp)
table(skin.iat_all$brought_simp, skin.iat_all$month)


## data subsetting -----------------------------------------------------------------------------------------------------------------
## now keep only those with complete data for the following:
# dscore, att.new,
# race, age, Order, ethnic.race.new, politicalid, edu
skin.full <- skin.iat_all[is.na(skin.iat_all$dscore)==FALSE,] # 479,619
skin.full <- skin.full[is.na(skin.full$att.new)==FALSE,] # 464,618

length(table(skin.full$month)) ## all available months.

skin.full.demogs <- skin.full[is.na(skin.full$genderidentity_simp)==FALSE,] # 464,618
length(table(skin.full.demogs$month)) ## no loss of months
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$race.new)==FALSE,] # 428,951
length(table(skin.full.demogs$month)) ## no loss of months
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$ethnicity.new)==FALSE,] # 415,009
length(table(skin.full.demogs$month)) ## no loss of months
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$edu.new)==FALSE,] # 408,319
length(table(skin.full.demogs$month)) ## no loss of months
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$age.new)==FALSE,] # 400,291
length(table(skin.full.demogs$month)) ## no loss of months
skin.full.demogs <- skin.full.demogs[is.na(skin.full.demogs$politicalid.new)==FALSE,] # 394,010
length(table(skin.full.demogs$month)) # no loss of months

# also include the following variables: num and further error/RT variables
skin.full.demogs <- skin.full.demogs[c("session_id", "date", "year", "month", "month.cent", "dscore", "att.new",
                                           "age.new", "genderidentity_simp", "race.new", "ethnicity.new",
                                           "politicalid.new", "edu.new", "Order", "num.new", "reportedskin",
                                           "STATE", "MSAName", "CountyNo", "rel.new", "broughtwebsite", "brought_simp")]
table(skin.full.demogs$month.cent)
write.csv(skin.full.demogs, "skin.fulldemogs.csv")



############################################################################################################
# total number of months (should all be 48)
length(table(age.full.demogs$month.cent)) 
length(table(disability.full.demogs$month.cent)) 
length(table(race.full.demogs$month.cent))
length(table(sexuality.full.demogs$month.cent)) 
length(table(skin.full.demogs$month.cent)) 
length(table(weight.full.demogs$month.cent)) 

############################################################################################################


## Read out as combined object ----
# setwd("**")

# Rename to clearly label them as the new data frame
age.full.demogsNEW <- age.full.demogs
disability.full.demogsNEW <- disability.full.demogs
race.full.demogsNEW <- race.full.demogs
sexuality.full.demogsNEW <- sexuality.full.demogs
weight.full.demogsNEW <- weight.full.demogs
skin.full.demogsNEW <- skin.full.demogs

save(list = c("age.full.demogsNEW", "disability.full.demogsNEW", "race.full.demogsNEW", 
       "sexuality.full.demogsNEW", "skin.full.demogsNEW", "weight.full.demogsNEW"), 
     file = "fulldemogs_NEWDATA.RData")




