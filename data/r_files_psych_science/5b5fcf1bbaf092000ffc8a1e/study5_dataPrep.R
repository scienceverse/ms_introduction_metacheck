## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 5: Data preparation

## Packages needed
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("nlme")) {install.packages("nlme"); require("nlme")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("multcomp")) {install.packages("multcomp"); require("multcomp")}
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("grid")) {install.packages("grid"); require("grid")}

## Load data
load("study5_rawDat.Rda")

## Rearrange columns
colnames(dat)

dat <- dat[,c(3,4,1,2,72,73,
              65:71,
              5:52,
              53:64
              )]

## Rename "ResponseId" to "ResponseID"
colnames(dat)[1] <- "ResponseID"

## Get rid of unnecessary rows
dat <- dat[c(1, 14:366),]
  ## First row will help with renaming variables/coding

## Drop unused levels
dat <- droplevels(x = dat)

## Rename judgment data columns: 14-61
colnames(dat)

## cond1
colnames(dat)[14] <- "cond1.lk_percent.male.pilots.comm.ATC"
colnames(dat)[15] <- "cond1.lk_percent.male.flightAttendants.comm.ATC"
colnames(dat)[16] <- "cond1.prior_manPilot.womanFlightAttendant"
colnames(dat)[17] <- "cond1.prior_womanPilot.manFlightAttendant"
colnames(dat)[18] <- "cond1.post_manPilot.womanFlightAttendant"
colnames(dat)[19] <- "cond1.post_womanPilot.manFlightAttendant"

## cond2
colnames(dat)[20] <- "cond2.prior_manPilot.womanFlightAttendant"
colnames(dat)[21] <- "cond2.prior_womanPilot.manFlightAttendant"
colnames(dat)[22] <- "cond2.post_manPilot.womanFlightAttendant"
colnames(dat)[23] <- "cond2.post_womanPilot.manFlightAttendant"
colnames(dat)[24] <- "cond2.lk_percent.male.pilots.comm.ATC"
colnames(dat)[25] <- "cond2.lk_percent.male.flightAttendants.comm.ATC"

## cond7
colnames(dat)[26] <- "cond7.lk_percent.female.pilots.comm.ATC"
colnames(dat)[27] <- "cond7.lk_percent.female.flightAttendants.comm.ATC"
colnames(dat)[28] <- "cond7.prior_manPilot.womanFlightAttendant"
colnames(dat)[29] <- "cond7.prior_womanPilot.manFlightAttendant"
colnames(dat)[30] <- "cond7.post_manPilot.womanFlightAttendant"
colnames(dat)[31] <- "cond7.post_womanPilot.manFlightAttendant"

## cond8
colnames(dat)[32] <- "cond8.prior_manPilot.womanFlightAttendant"
colnames(dat)[33] <- "cond8.prior_womanPilot.manFlightAttendant"
colnames(dat)[34] <- "cond8.post_manPilot.womanFlightAttendant"
colnames(dat)[35] <- "cond8.post_womanPilot.manFlightAttendant"
colnames(dat)[36] <- "cond8.lk_percent.female.pilots.comm.ATC"
colnames(dat)[37] <- "cond8.lk_percent.female.flightAttendants.comm.ATC"

## cond13
colnames(dat)[38] <- "cond13.lk_percent.male.pilots.comm.ATC"
colnames(dat)[39] <- "cond13.lk_percent.male.flightAttendants.comm.ATC"
colnames(dat)[40] <- "cond13.prior_womanPilot.manFlightAttendant"
colnames(dat)[41] <- "cond13.prior_manPilot.womanFlightAttendant"
colnames(dat)[42] <- "cond13.post_womanPilot.manFlightAttendant"
colnames(dat)[43] <- "cond13.post_manPilot.womanFlightAttendant"

## cond14
colnames(dat)[44] <- "cond14.prior_womanPilot.manFlightAttendant"
colnames(dat)[45] <- "cond14.prior_manPilot.womanFlightAttendant"
colnames(dat)[46] <- "cond14.post_womanPilot.manFlightAttendant"
colnames(dat)[47] <- "cond14.post_manPilot.womanFlightAttendant"
colnames(dat)[48] <- "cond14.lk_percent.male.pilots.comm.ATC"
colnames(dat)[49] <- "cond14.lk_percent.male.flightAttendants.comm.ATC"

## cond19
colnames(dat)[50] <- "cond19.lk_percent.female.pilots.comm.ATC"
colnames(dat)[51] <- "cond19.lk_percent.female.flightAttendants.comm.ATC"
colnames(dat)[52] <- "cond19.prior_womanPilot.manFlightAttendant"
colnames(dat)[53] <- "cond19.prior_manPilot.womanFlightAttendant"
colnames(dat)[54] <- "cond19.post_womanPilot.manFlightAttendant"
colnames(dat)[55] <- "cond19.post_manPilot.womanFlightAttendant"

## cond20
colnames(dat)[56] <- "cond20.prior_womanPilot.manFlightAttendant"
colnames(dat)[57] <- "cond20.prior_manPilot.womanFlightAttendant"
colnames(dat)[58] <- "cond20.post_womanPilot.manFlightAttendant"
colnames(dat)[59] <- "cond20.post_manPilot.womanFlightAttendant"
colnames(dat)[60] <- "cond20.lk_percent.female.pilots.comm.ATC"
colnames(dat)[61] <- "cond20.lk_percent.female.flightAttendants.comm.ATC"

## Confirm column names
confirm.column.names <- data.frame(col.names = c(colnames(dat)[14:61]),
                                   col.description = t(dat[1, c(14:61)])
                                  )

rownames(confirm.column.names) <- NULL

## Remove first row
dat <- dat[c(2:354),]
dat <- droplevels(dat)

## Do all conditions show up?
count(dat$condition)
  ## yes

## ========================================== ##
## Convert judgments from factor to numeric
str(dat)
colnames(dat)

dat[14:61] <- sapply(X = dat[14:61], 
                     FUN = as.character)

dat[14:61] <- sapply(X = dat[14:61],
                     FUN = as.numeric)

## Convert to decimal out of 1
divide.by.100 <- function(x) {
  x/100
}

dat[14:61] <- sapply(X = dat[14:61],
                     FUN = divide.by.100)

## ========================================== ##
## Convert attributions from factor to numeric
str(dat)
colnames(dat)
  
  ## c1
dat[62:66] <- sapply(X = dat[62:66], 
                     FUN = as.character)

dat[62:66] <- sapply(X = dat[62:66], 
                     FUN = as.numeric)

  ## c2
dat[68:72] <- sapply(X = dat[68:72], 
                     FUN = as.character)

dat[68:72] <- sapply(X = dat[68:72], 
                     FUN = as.numeric)

## ========================================== ##
## Clean up demographics

## gender
count(dat$gender)
  ## 2 unspecified
  ## 181 males
  ## 170 females

dat$gender <- factor(x = dat$gender, 
                     levels = c(1:2),
                     labels = c("Male", "Female"))

## education
count(dat$educ)

dat$educ <- factor(dat$educ, 
                   levels = c(3,4,5,6,7,8,9,10,11,12,13,14), 
                   labels = c("Some HS", "HS Grad", 
                              "Some College", "Associate's", "Bachelor's", "Some Grad School", 
                              "Master's", "MBA", "JD", "MD", "PhD", "Other Advanced Degree"))

## b.month
count(dat$bmonth)
dat$bmonth <- as.numeric(levels(dat$bmonth))[dat$bmonth]

## b.year
dat$byear <- as.numeric(levels(dat$byear))[dat$byear]

## Create birthday for each participant; assume everyone was born on 1st day of month
dat$birthday <- paste(dat$bmonth, 
                      "1",          
                      dat$byear, 
                      sep = "/")

dat$birthday <- as.POSIXct(dat$birthday, 
                           format = "%m/%d/%Y")


## politics
count(dat$politicalid)
dat$politicalid <- factor(x = dat$politicalid, 
                          levels = c(1:7),
                          labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                     "Neutral/Moderate", 
                                     "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

## lookup
count(dat$lookup)
dat$lookup <- factor(x = dat$lookup, 
                     levels = c(1:2),
                     labels = c("No", "Yes"))


colnames(dat)

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Create new variable, how much time each respondent spent on survey, in minutes
dat$Total.Time.Spent <- with(dat, (EndDate - StartDate))
dat$Total.Time.Spent <- as.numeric(dat$Total.Time.Spent)

summary(dat$Total.Time.Spent)

## Calculate age for each participant
## Get each participant's StartDate

dat$Date.of.Completion <- sapply(X = strsplit(x = as.character(dat$StartDate), split = " "), 
                                 "[", 
                                 1)

dat$Date.of.Completion <- as.POSIXct(dat$Date.of.Completion, 
                                     format = "%Y-%m-%d")

dat$age <- with(dat, (Date.of.Completion - birthday)/365)

dat$age <- as.numeric(dat$age)

summary(dat$age)
sd(dat$age, na.rm = TRUE)
colnames(dat)

## =================================================================== ##
## Specify what was different about each condition

dat$order <- NA
dat[grepl("^1$", dat$condition), "order"] <- "LPP"
dat[grepl("^2$", dat$condition), "order"] <- "PPL"

dat[grepl("^7$", dat$condition), "order"] <- "LPP"
dat[grepl("^8$", dat$condition), "order"] <- "PPL"

dat[grepl("^13$", dat$condition), "order"] <- "LPP"
dat[grepl("^14$", dat$condition), "order"] <- "PPL"

dat[grepl("^19$", dat$condition), "order"] <- "LPP"
dat[grepl("^20$", dat$condition), "order"] <- "PPL"

dat$order <- factor(x = dat$order, 
                    levels = c("LPP", "PPL"))


dat$target <- NA
dat[grepl("^1$", dat$condition), "target"] <- "man"
dat[grepl("^2$", dat$condition), "target"] <- "man"

dat[grepl("^7$", dat$condition), "target"] <- "woman"
dat[grepl("^8$", dat$condition), "target"] <- "woman"

dat[grepl("^13$", dat$condition), "target"] <- "man"
dat[grepl("^14$", dat$condition), "target"] <- "man"

dat[grepl("^19$", dat$condition), "target"] <- "woman"
dat[grepl("^20$", dat$condition), "target"] <- "woman"

dat$target <- factor(x = dat$target, 
                     levels = c("man", "woman"))

dat$question_order <- NA
dat[grepl("^1$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^2$", dat$condition), "question_order"] <- "man_pilot_1st"

dat[grepl("^7$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^8$", dat$condition), "question_order"] <- "man_pilot_1st"

dat[grepl("^13$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^14$", dat$condition), "question_order"] <- "woman_pilot_1st"

dat[grepl("^19$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^20$", dat$condition), "question_order"] <- "woman_pilot_1st"

dat$question_order <- factor(x = dat$question_order, 
                             levels = c("man_pilot_1st", "woman_pilot_1st"))

## =================================================================== ##

## Rearrange columns
colnames(dat)
dat <- dat[,c(1, 2,                       ## ResponseID, MTurkID
              3, 4, 75, 76,               ## StartDate, EndDate, Total.Time.Spent, Date.of.Completion, 
              5, 6,                       ## condition, attr.cond,
              78, 79, 80,                 ## order, target, question_order
              7, 8, 11, 12,               ## gender, education, politics, lookup
              9, 10, 74, 77,              ## bmonth, byear, birthday, age
              14:61,                      ## judgments data
              62:73,                      ## attributions data
              13                          ## comments
            )]

## =================================================================== ##

## Relevel condition
dat$condition <- factor(x = dat$condition, 
                        levels = c("1", "2",
                                   "7", "8", 
                                   "13", "14",  
                                   "19", "20"))

count(dat$condition)

## =================================================================== ##


## Prior.Man.Pilot: prior probability that man is the Pilot
dat$Prior.Man.Pilot <- NA

dat$Prior.Man.Pilot[dat$condition == "1"] <- dat$cond1.prior_manPilot.womanFlightAttendant[dat$condition == "1"]
dat$Prior.Man.Pilot[dat$condition == "2"] <- dat$cond2.prior_manPilot.womanFlightAttendant[dat$condition == "2"]

dat$Prior.Man.Pilot[dat$condition == "7"] <- dat$cond7.prior_manPilot.womanFlightAttendant[dat$condition == "7"]
dat$Prior.Man.Pilot[dat$condition == "8"] <- dat$cond8.prior_manPilot.womanFlightAttendant[dat$condition == "8"]

dat$Prior.Man.Pilot[dat$condition == "13"] <- dat$cond13.prior_manPilot.womanFlightAttendant[dat$condition == "13"]
dat$Prior.Man.Pilot[dat$condition == "14"] <- dat$cond14.prior_manPilot.womanFlightAttendant[dat$condition == "14"]

dat$Prior.Man.Pilot[dat$condition == "19"] <- dat$cond19.prior_manPilot.womanFlightAttendant[dat$condition == "19"]
dat$Prior.Man.Pilot[dat$condition == "20"] <- dat$cond20.prior_manPilot.womanFlightAttendant[dat$condition == "20"]

## Prior.Woman.Pilot: prior probability that woman is the Pilot
dat$Prior.Woman.Pilot <- NA

dat$Prior.Woman.Pilot[dat$condition == "1"] <- dat$cond1.prior_womanPilot.manFlightAttendant[dat$condition == "1"]
dat$Prior.Woman.Pilot[dat$condition == "2"] <- dat$cond2.prior_womanPilot.manFlightAttendant[dat$condition == "2"]

dat$Prior.Woman.Pilot[dat$condition == "7"] <- dat$cond7.prior_womanPilot.manFlightAttendant[dat$condition == "7"]
dat$Prior.Woman.Pilot[dat$condition == "8"] <- dat$cond8.prior_womanPilot.manFlightAttendant[dat$condition == "8"]

dat$Prior.Woman.Pilot[dat$condition == "13"] <- dat$cond13.prior_womanPilot.manFlightAttendant[dat$condition == "13"]
dat$Prior.Woman.Pilot[dat$condition == "14"] <- dat$cond14.prior_womanPilot.manFlightAttendant[dat$condition == "14"]

dat$Prior.Woman.Pilot[dat$condition == "19"] <- dat$cond19.prior_womanPilot.manFlightAttendant[dat$condition == "19"]
dat$Prior.Woman.Pilot[dat$condition == "20"] <- dat$cond20.prior_womanPilot.manFlightAttendant[dat$condition == "20"]

## Confirm that priors sum to 100
count(with(dat, Prior.Man.Pilot + Prior.Woman.Pilot))

## Peak at distribution of priors overall
hist(dat$Prior.Man.Pilot)

## ================================================================================================= ##

## Prior.Target.Pilot
dat$Prior.Target.Pilot <- ifelse(test = dat$target == "man", 
                                 yes = dat$Prior.Man.Pilot, 
                                 no = dat$Prior.Woman.Pilot)

## ================================================================== ##
## Able to update
count(dat$Prior.Target.Pilot)

dat$able.to.update <- ifelse(test = dat$Prior.Target.Pilot == 1 | dat$Prior.Target.Pilot == 0, 
                             yes = "no", 
                             no = "yes")

dat$able.to.update <- factor(x = dat$able.to.update, 
                             labels = c("no", "yes"))

count(dat$able.to.update)
  ## 5 participants can't update their priors

## =================================================================== ##

## Posterior.Man.Pilot: posterior probability that man is the Pilot
dat$Posterior.Man.Pilot <- NA

dat$Posterior.Man.Pilot[dat$condition == "1"] <- dat$cond1.post_manPilot.womanFlightAttendant[dat$condition == "1"]
dat$Posterior.Man.Pilot[dat$condition == "2"] <- dat$cond2.post_manPilot.womanFlightAttendant[dat$condition == "2"]

dat$Posterior.Man.Pilot[dat$condition == "7"] <- dat$cond7.post_manPilot.womanFlightAttendant[dat$condition == "7"]
dat$Posterior.Man.Pilot[dat$condition == "8"] <- dat$cond8.post_manPilot.womanFlightAttendant[dat$condition == "8"]

dat$Posterior.Man.Pilot[dat$condition == "13"] <- dat$cond13.post_manPilot.womanFlightAttendant[dat$condition == "13"]
dat$Posterior.Man.Pilot[dat$condition == "14"] <- dat$cond14.post_manPilot.womanFlightAttendant[dat$condition == "14"]

dat$Posterior.Man.Pilot[dat$condition == "19"] <- dat$cond19.post_manPilot.womanFlightAttendant[dat$condition == "19"]
dat$Posterior.Man.Pilot[dat$condition == "20"] <- dat$cond20.post_manPilot.womanFlightAttendant[dat$condition == "20"]

## Posterior.Woman.Pilot: prior probability that woman is the Pilot
dat$Posterior.Woman.Pilot <- NA

dat$Posterior.Woman.Pilot[dat$condition == "1"] <- dat$cond1.post_womanPilot.manFlightAttendant[dat$condition == "1"]
dat$Posterior.Woman.Pilot[dat$condition == "2"] <- dat$cond2.post_womanPilot.manFlightAttendant[dat$condition == "2"]

dat$Posterior.Woman.Pilot[dat$condition == "7"] <- dat$cond7.post_womanPilot.manFlightAttendant[dat$condition == "7"]
dat$Posterior.Woman.Pilot[dat$condition == "8"] <- dat$cond8.post_womanPilot.manFlightAttendant[dat$condition == "8"]

dat$Posterior.Woman.Pilot[dat$condition == "13"] <- dat$cond13.post_womanPilot.manFlightAttendant[dat$condition == "13"]
dat$Posterior.Woman.Pilot[dat$condition == "14"] <- dat$cond14.post_womanPilot.manFlightAttendant[dat$condition == "14"]

dat$Posterior.Woman.Pilot[dat$condition == "19"] <- dat$cond19.post_womanPilot.manFlightAttendant[dat$condition == "19"]
dat$Posterior.Woman.Pilot[dat$condition == "20"] <- dat$cond20.post_womanPilot.manFlightAttendant[dat$condition == "20"]

## Confirm that posteriors sum to 100
count(with(dat, Posterior.Man.Pilot + Posterior.Woman.Pilot))

## ================================================================================================= ##

## Posterior.Target.Pilot
dat$Posterior.Target.Pilot <- ifelse(test = dat$target == "man", 
                                     yes = dat$Posterior.Man.Pilot, 
                                     no = dat$Posterior.Woman.Pilot)

## ================================================================================================= ##

## Save dat as its own dataframe
save(dat, file = "study5_dat.Rda")

