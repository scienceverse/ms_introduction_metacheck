## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study: statistical judgments about doctors, egalitarian preferences about pilots

# Packages needed ---------------------------------------------------------
if (!require("tidyverse")) {install.packages("tidyverse")}; require("tidyverse")
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("nlme")) {install.packages("nlme"); require("nlme")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("multcomp")) {install.packages("multcomp"); require("multcomp")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("cowplot")) {install.packages("cowplot"); require("cowplot")}
if (!require("boot")) {install.packages("boot"); require("boot")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("MBESS")) {install.packages("MBESS"); require("MBESS")}

# Set working directory ---------------------------------------------------
setwd(dirname(getActiveDocumentContext()$path))

# Data preparation --------------------------------------------------------
## Load data
load("SupplementalStudy_SwitchWithinSs.Rda")

## Get rid of unncessary rows
dat <- dat[c(1, 13:nrow(dat)),]

## Drop unused levels
dat <- droplevels(dat)

## Rename columns
colnames(dat)

## cond1: columns 7-12
colnames(dat)[7:12]
dat[1,7:12]
colnames(dat)[7] <- "cond1.lk_percent.male.doctors.perform.surgery"
colnames(dat)[8] <- "cond1.lk_percent.male.nurses.perform.surgery"
colnames(dat)[9] <- "cond1.prior_manDoctor.womanNurse"
colnames(dat)[10] <- "cond1.prior_womanDoctor.manNurse"
colnames(dat)[11] <- "cond1.post_manDoctor.womanNurse"
colnames(dat)[12] <- "cond1.post_womanDoctor.manNurse"

## cond2: columns 13-18
colnames(dat)[13:18]
dat[1,13:18]
colnames(dat)[13] <- "cond2.prior_manDoctor.womanNurse"
colnames(dat)[14] <- "cond2.prior_womanDoctor.manNurse"
colnames(dat)[15] <- "cond2.post_manDoctor.womanNurse"
colnames(dat)[16] <- "cond2.post_womanDoctor.manNurse"
colnames(dat)[17] <- "cond2.lk_percent.male.doctors.perform.surgery"
colnames(dat)[18] <- "cond2.lk_percent.male.nurses.perform.surgery"

## cond7: columns 19-24
colnames(dat)[19:24]
dat[1,19:24]
colnames(dat)[19] <- "cond7.lk_percent.female.doctors.perform.surgery"
colnames(dat)[20] <- "cond7.lk_percent.female.nurses.perform.surgery"
colnames(dat)[21] <- "cond7.prior_manDoctor.womanNurse"
colnames(dat)[22] <- "cond7.prior_womanDoctor.manNurse"
colnames(dat)[23] <- "cond7.post_manDoctor.womanNurse"
colnames(dat)[24] <- "cond7.post_womanDoctor.manNurse"

## cond8: columns 25-30
colnames(dat)[25:30]
dat[1,25:30]
colnames(dat)[25] <- "cond8.prior_manDoctor.womanNurse"
colnames(dat)[26] <- "cond8.prior_womanDoctor.manNurse"
colnames(dat)[27] <- "cond8.post_manDoctor.womanNurse"
colnames(dat)[28] <- "cond8.post_womanDoctor.manNurse"
colnames(dat)[29] <- "cond8.lk_percent.female.doctors.perform.surgery"
colnames(dat)[30] <- "cond8.lk_percent.female.nurses.perform.surgery"

## cond13: columns 31-36
colnames(dat)[31:36]
dat[1,31:36]
colnames(dat)[31] <- "cond13.lk_percent.male.doctors.perform.surgery"
colnames(dat)[32] <- "cond13.lk_percent.male.nurses.perform.surgery"
colnames(dat)[33] <- "cond13.prior_womanDoctor.manNurse"
colnames(dat)[34] <- "cond13.prior_manDoctor.womanNurse"
colnames(dat)[35] <- "cond13.post_womanDoctor.manNurse"
colnames(dat)[36] <- "cond13.post_manDoctor.womanNurse"

## cond14
colnames(dat)[37:42]
dat[1,37:42]
colnames(dat)[37] <- "cond14.prior_womanDoctor.manNurse"
colnames(dat)[38] <- "cond14.prior_manDoctor.womanNurse"
colnames(dat)[39] <- "cond14.post_womanDoctor.manNurse"
colnames(dat)[40] <- "cond14.post_manDoctor.womanNurse"
colnames(dat)[41] <- "cond14.lk_percent.male.doctors.perform.surgery"
colnames(dat)[42] <- "cond14.lk_percent.male.nurses.perform.surgery"

## cond19
colnames(dat)[43:48]
dat[1,43:48]
colnames(dat)[43] <- "cond19.lk_percent.female.doctors.perform.surgery"
colnames(dat)[44] <- "cond19.lk_percent.female.nurses.perform.surgery"
colnames(dat)[45] <- "cond19.prior_womanDoctor.manNurse"
colnames(dat)[46] <- "cond19.prior_manDoctor.womanNurse"
colnames(dat)[47] <- "cond19.post_womanDoctor.manNurse"
colnames(dat)[48] <- "cond19.post_manDoctor.womanNurse"

## cond20
colnames(dat)[49:54]
dat[1,49:54]
colnames(dat)[49] <- "cond20.prior_womanDoctor.manNurse"
colnames(dat)[50] <- "cond20.prior_manDoctor.womanNurse"
colnames(dat)[51] <- "cond20.post_womanDoctor.manNurse"
colnames(dat)[52] <- "cond20.post_manDoctor.womanNurse"
colnames(dat)[53] <- "cond20.lk_percent.female.doctors.perform.surgery"
colnames(dat)[54] <- "cond20.lk_percent.female.nurses.perform.surgery"

## Confirm column names
confirm.column.names <- data.frame(col.names = c(colnames(dat)[7:54]),
                                   col.description = t(dat[1, c(7:54)])
                                  )

rownames(confirm.column.names) <- NULL

## Remove first row
dat <- dat[c(2:nrow(dat)),]
dat <- droplevels(dat)

## Do all conditions show up?
table(dat$condition)
  ## Yes

## Convert judgments from factor to numeric
str(dat)
colnames(dat)

dat[7:54] <- sapply(X = dat[7:54], 
                     FUN = as.character)

dat[7:54] <- sapply(X = dat[7:54],
                     FUN = as.numeric)

## Convert to decimal out of 1
divide.by.100 <- function(x) {
  x/100
}

dat[7:54] <- sapply(X = dat[7:54],
                    FUN = divide.by.100)


## Convert attributions from factor to numeric
str(dat)
colnames(dat)

## c1
dat[55:59] <- sapply(X = dat[55:59], 
                     FUN = as.character)

dat[55:59] <- sapply(X = dat[55:59], 
                     FUN = as.numeric)

## c2
dat[61:65] <- sapply(X = dat[61:65], 
                     FUN = as.character)

dat[61:65] <- sapply(X = dat[61:65], 
                     FUN = as.numeric)

## Clean up demographics
  
  ## gender
table(dat$gender)

dat$gender <- factor(x = dat$gender, 
                     levels = c(1:2),
                     labels = c("Male", "Female"))

  ## education
table(dat$educ)

dat$educ <- factor(dat$educ, 
                   levels = c(3,4,5,6,7,8,9,10,11,13,14), 
                   labels = c("Some HS", "HS Grad", 
                              "Some College", "Associate's", "Bachelor's", "Some Grad School", 
                              "Master's", "MBA", "JD", "PhD", "Other Advanced Degree"))

  ## bmonth
table(dat$bmonth)
dat$bmonth <- as.numeric(levels(dat$bmonth))[dat$bmonth]

  ## byear
table(dat$byear)
dat$byear <- as.numeric(levels(dat$byear))[dat$byear]

  ## Create birthday for each participant; assume everyone was born on 1st day of month
dat$birthday <- paste(dat$bmonth, 
                      "1",          
                      dat$byear, 
                      sep = "/")

dat$birthday <- as.POSIXct(dat$birthday, 
                           format = "%m/%d/%Y")

  ## politicalid
table(dat$politicalid)
dat$politicalid <- factor(x = dat$politicalid, 
                          levels = c(1:7),
                          labels = c("Strongly Conservative", "Moderately Conservative", "Slightly Conservative", 
                                     "Neutral/Moderate", 
                                     "Slightly Liberal", "Moderately Liberal", "Strongly Liberal"))

  ## lookup
table(dat$lookup)
dat$lookup <- factor(x = dat$lookup, 
                     levels = c(1:2),
                     labels = c("No", "Yes"))


## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Create new variable, how much time each respondent spent on survey, in minutes
dat$Total.Time.Spent <- with(dat, (EndDate - StartDate))
dat$Total.Time.Spent <- as.numeric(dat$Total.Time.Spent)

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

## Specify what was different about each condition
table(dat$condition)
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
dat[grepl("^1$", dat$condition), "question_order"] <- "man_doctor_1st"
dat[grepl("^2$", dat$condition), "question_order"] <- "man_doctor_1st"

dat[grepl("^7$", dat$condition), "question_order"] <- "man_doctor_1st"
dat[grepl("^8$", dat$condition), "question_order"] <- "man_doctor_1st"

dat[grepl("^13$", dat$condition), "question_order"] <- "woman_doctor_1st"
dat[grepl("^14$", dat$condition), "question_order"] <- "woman_doctor_1st"

dat[grepl("^19$", dat$condition), "question_order"] <- "woman_doctor_1st"
dat[grepl("^20$", dat$condition), "question_order"] <- "woman_doctor_1st"

dat$question_order <- factor(x = dat$question_order, 
                             levels = c("man_doctor_1st", "woman_doctor_1st"))

## Rearrage columns
colnames(dat)
dat <- dat[,c(3:4,              ## MTurkID, ResponseId
              1:2, 75, 76,      ## StartDate, EndDate, Total.Time.Spent, Date.of.Completion
              5, 6,             ## condition, attr.cond
              78, 79, 80,       ## order, target, question_order
              67, 68, 71, 72,   ## gender, educ, politicalid, lookup
              69, 70, 74, 77,   ## bmonth, byear, birthday, age
              7:54,             ## judgment data
              55:66,            ## attributions data
              73                ## comments
             )
          ]

## Relevel condition
table(dat$condition)
dat$condition <- factor(x = dat$condition, 
                        levels = c("1", "2",
                                   "7", "8", 
                                   "13", "14",  
                                   "19", "20"))

table(dat$condition)

## Prior.Man.Doctor: prior probability that man is the doctor
dat$Prior.Man.Doctor <- NA

dat$Prior.Man.Doctor[dat$condition == "1"] <- dat$cond1.prior_manDoctor.womanNurse[dat$condition == "1"]
dat$Prior.Man.Doctor[dat$condition == "2"] <- dat$cond2.prior_manDoctor.womanNurse[dat$condition == "2"]

dat$Prior.Man.Doctor[dat$condition == "7"] <- dat$cond7.prior_manDoctor.womanNurse[dat$condition == "7"]
dat$Prior.Man.Doctor[dat$condition == "8"] <- dat$cond8.prior_manDoctor.womanNurse[dat$condition == "8"]

dat$Prior.Man.Doctor[dat$condition == "13"] <- dat$cond13.prior_manDoctor.womanNurse[dat$condition == "13"]
dat$Prior.Man.Doctor[dat$condition == "14"] <- dat$cond14.prior_manDoctor.womanNurse[dat$condition == "14"]

dat$Prior.Man.Doctor[dat$condition == "19"] <- dat$cond19.prior_manDoctor.womanNurse[dat$condition == "19"]
dat$Prior.Man.Doctor[dat$condition == "20"] <- dat$cond20.prior_manDoctor.womanNurse[dat$condition == "20"]

## Prior.Woman.Pilot: prior probability that woman is the Pilot
dat$Prior.Woman.Doctor <- NA

dat$Prior.Woman.Doctor[dat$condition == "1"] <- dat$cond1.prior_womanDoctor.manNurse[dat$condition == "1"]
dat$Prior.Woman.Doctor[dat$condition == "2"] <- dat$cond2.prior_womanDoctor.manNurse[dat$condition == "2"]

dat$Prior.Woman.Doctor[dat$condition == "7"] <- dat$cond7.prior_womanDoctor.manNurse[dat$condition == "7"]
dat$Prior.Woman.Doctor[dat$condition == "8"] <- dat$cond8.prior_womanDoctor.manNurse[dat$condition == "8"]

dat$Prior.Woman.Doctor[dat$condition == "13"] <- dat$cond13.prior_womanDoctor.manNurse[dat$condition == "13"]
dat$Prior.Woman.Doctor[dat$condition == "14"] <- dat$cond14.prior_womanDoctor.manNurse[dat$condition == "14"]

dat$Prior.Woman.Doctor[dat$condition == "19"] <- dat$cond19.prior_womanDoctor.manNurse[dat$condition == "19"]
dat$Prior.Woman.Doctor[dat$condition == "20"] <- dat$cond20.prior_womanDoctor.manNurse[dat$condition == "20"]

## Confirm that priors sum to 100
table(with(dat, Prior.Man.Doctor + Prior.Woman.Doctor))

## Peak at distribution of priors overall
hist(dat$Prior.Man.Doctor)

## Prior.Target.Doctor
dat$Prior.Target.Doctor <- ifelse(test = dat$target == "man", 
                                  yes = dat$Prior.Man.Doctor, 
                                  no = dat$Prior.Woman.Doctor)

## Able to update
table(dat$Prior.Target.Doctor)

dat$able.to.update <- ifelse(test = dat$Prior.Target.Doctor == 1 | dat$Prior.Target.Doctor == 0, 
                             yes = "no", 
                             no = "yes")

dat$able.to.update <- factor(x = dat$able.to.update, 
                             labels = c("no", "yes"))

table(dat$able.to.update)
  ## 4 participants can't update their priors

## Posterior.Man.Doctor: posterior probability that man is the doctor
dat$Posterior.Man.Doctor <- NA

dat$Posterior.Man.Doctor[dat$condition == "1"] <- dat$cond1.post_manDoctor.womanNurse[dat$condition == "1"]
dat$Posterior.Man.Doctor[dat$condition == "2"] <- dat$cond2.post_manDoctor.womanNurse[dat$condition == "2"]

dat$Posterior.Man.Doctor[dat$condition == "7"] <- dat$cond7.post_manDoctor.womanNurse[dat$condition == "7"]
dat$Posterior.Man.Doctor[dat$condition == "8"] <- dat$cond8.post_manDoctor.womanNurse[dat$condition == "8"]

dat$Posterior.Man.Doctor[dat$condition == "13"] <- dat$cond13.post_manDoctor.womanNurse[dat$condition == "13"]
dat$Posterior.Man.Doctor[dat$condition == "14"] <- dat$cond14.post_manDoctor.womanNurse[dat$condition == "14"]

dat$Posterior.Man.Doctor[dat$condition == "19"] <- dat$cond19.post_manDoctor.womanNurse[dat$condition == "19"]
dat$Posterior.Man.Doctor[dat$condition == "20"] <- dat$cond20.post_manDoctor.womanNurse[dat$condition == "20"]

## Posterior.Woman.Doctor: prior probability that woman is the doctor
dat$Posterior.Woman.Doctor <- NA

dat$Posterior.Woman.Doctor[dat$condition == "1"] <- dat$cond1.post_womanDoctor.manNurse[dat$condition == "1"]
dat$Posterior.Woman.Doctor[dat$condition == "2"] <- dat$cond2.post_womanDoctor.manNurse[dat$condition == "2"]

dat$Posterior.Woman.Doctor[dat$condition == "7"] <- dat$cond7.post_womanDoctor.manNurse[dat$condition == "7"]
dat$Posterior.Woman.Doctor[dat$condition == "8"] <- dat$cond8.post_womanDoctor.manNurse[dat$condition == "8"]

dat$Posterior.Woman.Doctor[dat$condition == "13"] <- dat$cond13.post_womanDoctor.manNurse[dat$condition == "13"]
dat$Posterior.Woman.Doctor[dat$condition == "14"] <- dat$cond14.post_womanDoctor.manNurse[dat$condition == "14"]

dat$Posterior.Woman.Doctor[dat$condition == "19"] <- dat$cond19.post_womanDoctor.manNurse[dat$condition == "19"]
dat$Posterior.Woman.Doctor[dat$condition == "20"] <- dat$cond20.post_womanDoctor.manNurse[dat$condition == "20"]

## Confirm that posteriors sum to 100
table(with(dat, Posterior.Man.Doctor + Posterior.Woman.Doctor))
plyr::count(with(dat, Posterior.Man.Doctor + Posterior.Woman.Doctor))
  ## 1 NA

## Where is the NA?
which(is.na(dat$Posterior.Man.Doctor) == TRUE)
which(is.na(dat$Posterior.Woman.Doctor) == TRUE)
  ## Row 201

## Which condition?
dat[201,7]
  ## Condition 8: PPL, man doctor 1st, woman performed surgery

## View judgment data
dat[201,38:43]

## Replace NA with 0
dat[201,40] <- 0

## Calculate posterior again
dat$Posterior.Man.Doctor[dat$condition == "1"] <- dat$cond1.post_manDoctor.womanNurse[dat$condition == "1"]
dat$Posterior.Man.Doctor[dat$condition == "2"] <- dat$cond2.post_manDoctor.womanNurse[dat$condition == "2"]
dat$Posterior.Man.Doctor[dat$condition == "7"] <- dat$cond7.post_manDoctor.womanNurse[dat$condition == "7"]
dat$Posterior.Man.Doctor[dat$condition == "8"] <- dat$cond8.post_manDoctor.womanNurse[dat$condition == "8"]
dat$Posterior.Man.Doctor[dat$condition == "13"] <- dat$cond13.post_manDoctor.womanNurse[dat$condition == "13"]
dat$Posterior.Man.Doctor[dat$condition == "14"] <- dat$cond14.post_manDoctor.womanNurse[dat$condition == "14"]
dat$Posterior.Man.Doctor[dat$condition == "19"] <- dat$cond19.post_manDoctor.womanNurse[dat$condition == "19"]
dat$Posterior.Man.Doctor[dat$condition == "20"] <- dat$cond20.post_manDoctor.womanNurse[dat$condition == "20"]

dat$Posterior.Woman.Doctor[dat$condition == "1"] <- dat$cond1.post_womanDoctor.manNurse[dat$condition == "1"]
dat$Posterior.Woman.Doctor[dat$condition == "2"] <- dat$cond2.post_womanDoctor.manNurse[dat$condition == "2"]
dat$Posterior.Woman.Doctor[dat$condition == "7"] <- dat$cond7.post_womanDoctor.manNurse[dat$condition == "7"]
dat$Posterior.Woman.Doctor[dat$condition == "8"] <- dat$cond8.post_womanDoctor.manNurse[dat$condition == "8"]
dat$Posterior.Woman.Doctor[dat$condition == "13"] <- dat$cond13.post_womanDoctor.manNurse[dat$condition == "13"]
dat$Posterior.Woman.Doctor[dat$condition == "14"] <- dat$cond14.post_womanDoctor.manNurse[dat$condition == "14"]
dat$Posterior.Woman.Doctor[dat$condition == "19"] <- dat$cond19.post_womanDoctor.manNurse[dat$condition == "19"]
dat$Posterior.Woman.Doctor[dat$condition == "20"] <- dat$cond20.post_womanDoctor.manNurse[dat$condition == "20"]

## Confirm that posteriors sum to 100
table(with(dat, Posterior.Man.Doctor + Posterior.Woman.Doctor))
  ## Now all good

## ================================================================================================= ##

## Posterior.Target.Pilot
dat$Posterior.Target.Doctor <- ifelse(test = dat$target == "man", 
                                      yes = dat$Posterior.Man.Doctor, 
                                      no = dat$Posterior.Woman.Doctor)

tapply(X = dat$Posterior.Target.Doctor, 
       INDEX = dat$target, 
       FUN = mean)

## ================================================================================================= ##
## ================================================================================================= ##
## ================================================================================================= ##
## ================================================================================================= ##
## Analyses begin below

## Exclude those who can't update
dat <- dat %>% filter(able.to.update == "yes")
  ## 4 participants excluded for not being able to update

table(dat$lookup)

plyr::count(dat$gender)

round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)


## Distribution of prior beliefs
dat %>% 
  ggplot() + 
  geom_histogram(mapping = aes(x = Prior.Man.Doctor, 
                               y = (..count..)/sum(..count..)))

## Calculate log likeilhood ratios
  ## Create wide version of dataset containing only:
    ## ResponseId, target, data, and likelihood responses
colnames(dat)

dat.lr <- dat %>%
  dplyr::select(ResponseId, target, condition, contains("lk"))

## ------------------------------------------------------------------------- ##
## percent.male.doctors.perform.surgery
dat.lr$percent.male.doctors.perform.surgery <- NA

dat.lr$percent.male.doctors.perform.surgery[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.doctors.perform.surgery[dat.lr$condition == "1"]

dat.lr$percent.male.doctors.perform.surgery[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.doctors.perform.surgery[dat.lr$condition == "2"]

dat.lr$percent.male.doctors.perform.surgery[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.doctors.perform.surgery[dat.lr$condition == "13"]

dat.lr$percent.male.doctors.perform.surgery[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.doctors.perform.surgery[dat.lr$condition == "14"]

library(plyr)
plyr::count(dat.lr$percent.male.doctors.perform.surgery)
  ## 172 particiapnts don't have values for percent.male.doctors.perform.surgery
  ## These 172 participants were in the "woman performed surgery" condition
nrow(dat.lr)-172
  ## 183 particiapnts in the "man performed surgery" condition
table(dat.lr$target)
  ## Agrees with above

hist(dat.lr$percent.male.doctors.perform.surgery)
  ## Looks reasonable

## ------------------------------------------------------------------------- ##
## percent.male.nurses.perform.surgery
dat.lr$percent.male.nurses.perform.surgery <- NA

dat.lr$percent.male.nurses.perform.surgery[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.nurses.perform.surgery[dat.lr$condition == "1"]

dat.lr$percent.male.nurses.perform.surgery[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.nurses.perform.surgery[dat.lr$condition == "2"]

dat.lr$percent.male.nurses.perform.surgery[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.nurses.perform.surgery[dat.lr$condition == "13"]

dat.lr$percent.male.nurses.perform.surgery[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.nurses.perform.surgery[dat.lr$condition == "14"]

plyr::count(dat.lr$percent.male.nurses.perform.surgery)
  ## 172 particiapnts don't have values for percent.male.nurses.perform.surgery
  ## These 172 participants were in the "woman performed surgery" condition
nrow(dat.lr)-172
  ## 183 particiapnts in the "man performed surgery" condition
table(dat.lr$target)
  ## Agrees with above

hist(dat.lr$percent.male.nurses.perform.surgery)
  ## Looks reasonable

## ------------------------------------------------------------------------- ##
## percent.female.doctors.perform.surgery
dat.lr$percent.female.doctors.perform.surgery <- NA

dat.lr$percent.female.doctors.perform.surgery[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.doctors.perform.surgery[dat.lr$condition == "7"]

dat.lr$percent.female.doctors.perform.surgery[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.doctors.perform.surgery[dat.lr$condition == "8"]

dat.lr$percent.female.doctors.perform.surgery[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.doctors.perform.surgery[dat.lr$condition == "19"]

dat.lr$percent.female.doctors.perform.surgery[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.doctors.perform.surgery[dat.lr$condition == "20"]

plyr::count(dat.lr$percent.female.doctors.perform.surgery)
  ## 183 particiapnts don't have values for percent.male.nurses.perform.surgery
  ## These 183 participants were in the "man performed surgery" condition
nrow(dat.lr)-183
  ## 172 particiapnts in the "woman performed surgery" condition
table(dat.lr$target)
  ## Agrees with above

hist(dat.lr$percent.female.doctors.perform.surgery)
  ## Looks reasonable

## ------------------------------------------------------------------------- ##
## percent.female.nurses.perform.surgery
dat.lr$percent.female.nurses.perform.surgery <- NA

dat.lr$percent.female.nurses.perform.surgery[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.nurses.perform.surgery[dat.lr$condition == "7"]

dat.lr$percent.female.nurses.perform.surgery[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.nurses.perform.surgery[dat.lr$condition == "8"]

dat.lr$percent.female.nurses.perform.surgery[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.nurses.perform.surgery[dat.lr$condition == "19"]

dat.lr$percent.female.nurses.perform.surgery[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.nurses.perform.surgery[dat.lr$condition == "20"]

plyr::count(dat.lr$percent.female.nurses.perform.surgery)
  ## 183 particiapnts don't have values for percent.male.nurses.perform.surgery
  ## These 183 participants were in the "man performed surgery" condition
nrow(dat.lr)-183
  ## 172 particiapnts in the "woman performed surgery" condition
table(dat.lr$target)
  ## Agrees with above

hist(dat.lr$percent.female.nurses.perform.surgery)
  ## Looks reasonable

## ======== ##
## ======== ##
## ======== ##
## Reorganize dataset
colnames(dat.lr)
LR.dat <- dat.lr[,c(1:3, 20:23, 4:19)]
colnames(LR.dat)

LR.dat$likelihood.doctor <- NA

LR.dat$likelihood.doctor[LR.dat$target == "man"] <- 
  LR.dat$percent.male.doctors.perform.surgery[LR.dat$target == "man"]

LR.dat$likelihood.doctor[LR.dat$target == "woman"] <- 
  LR.dat$percent.female.doctors.perform.surgery[LR.dat$target == "woman"]


## --------------- ##
## --------------- ##
## --------------- ##
LR.dat$likelihood.nurse <- NA

LR.dat$likelihood.nurse[LR.dat$target == "man"] <- 
  LR.dat$percent.male.nurses.perform.surgery[LR.dat$target == "man"]

LR.dat$likelihood.nurse[LR.dat$target == "woman"] <- 
  LR.dat$percent.female.nurses.perform.surgery[LR.dat$target == "woman"]

## ======== ##
## ======== ##
## ======== ##
LR.dat$LR <- with(LR.dat, likelihood.doctor / likelihood.nurse)
plyr::count(LR.dat$LR)
  ## 0 NaNs
  ## 113 Infs

## Do data overwhelm prior completely?
LR.dat$overwhelm.prior <- ifelse(test = LR.dat$LR == 0, 
                                 yes = "Data entirely diagnostic of nurse", 
                                 no = ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                             yes = "Data entirely diagnostic of doctor", 
                                             no = "Data not entirely diagnostic"))

LR.dat$overwhelm.prior <- factor(LR.dat$overwhelm.prior, 
                                 levels = c("Data entirely diagnostic of nurse", 
                                            "Data not entirely diagnostic", 
                                            "Data entirely diagnostic of doctor"))


levels(LR.dat$overwhelm.prior)
plyr::count(LR.dat$overwhelm.prior)
  ## Agrees with above

## Create LR for plotting purposes
  ## Infinity = 10,000
  ## Zero = 1/10,000
LR.dat$value.plotting <- ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                yes = 10000, 
                                no = ifelse(test = LR.dat$LR == 0, 
                                            yes = 1/10000, 
                                            no = LR.dat$LR))

## condition for plotting purposes
plyr::count(LR.dat$target)
LR.dat$condition.plotting <- NA

LR.dat$condition.plotting <- ifelse(test = LR.dat$target == "man", 
                                    yes = "Man Performed Surgery\nn = 183", 
                                    no =  "Woman Performed Surgery\nn = 172")

LR.dat$condition.plotting <- factor(LR.dat$condition.plotting, 
                                    levels = c("Man Performed Surgery\nn = 183", 
                                               "Woman Performed Surgery\nn = 172"))

## Make sure n's sum up to 355
183+172

## Visualize
ggplot(data = LR.dat, 
       mapping = aes(x = log(value.plotting), fill = overwhelm.prior)) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.80)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                "-5", "-4", "-3", "-2", "-1",
                                "0", 
                                "1", "2", "3", "4", "5", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(label = percent_format()) + 
  scale_fill_manual(values = c("white", "darkgrey", "black")) + 
  labs(x = "\nLog Likelihood Ratio", y = "Proportion of participants (in each condition)\n") + 
  theme_bw() + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(face = "bold")) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## ==================================================================================== ##
## Inferential statistics on likelihood ratios
  ## Comparisons between genders

## Man vs. Woman - comm w/ATC
wilcox.test(log(LR) ~ target, data = LR.dat)
  ## Wilcoxon P = 0.19
  ## Effect size, Pearson's r = 0.07
qnorm(wilcox.test(log(LR) ~ condition.plotting, data = LR.dat)$p.value/2) / sqrt(183+172)

tapply(log(LR.dat$LR), 
       LR.dat$condition.plotting, 
       FUN = median)

## ================================================================================================== ##
## Calculate model posterior; compare to reported posterior

## Create new dataframe
prior.model.reported <- merge(x = dat[,c("ResponseId", "target",
                                         "Prior.Man.Doctor", "Prior.Woman.Doctor", "Prior.Target.Doctor", 
                                         "Posterior.Man.Doctor", "Posterior.Woman.Doctor", "Posterior.Target.Doctor")], 
                              y = LR.dat[,c("ResponseId", "LR")], 
                              by = "ResponseId",
                              all.x = FALSE, 
                              all.y = TRUE)

## Create prior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$prior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                yes = prior.model.reported$Prior.Man.Doctor / prior.model.reported$Prior.Woman.Doctor, 
                                                no = prior.model.reported$Prior.Woman.Doctor / prior.model.reported$Prior.Man.Doctor)

## Create posterior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$posterior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                    yes = prior.model.reported$Posterior.Man.Doctor / prior.model.reported$Posterior.Woman.Doctor, 
                                                    no = prior.model.reported$Posterior.Woman.Doctor / prior.model.reported$Posterior.Man.Doctor)


## Calculate model.posterior.odds.ratio
prior.model.reported$model.posterior.odds.ratio <- with(prior.model.reported, 
                                                        prior.odds.ratio * LR)

## Convert the following to percent format:
  ## prior.odds.ratio
  ## posterior.odds.ratio
  ## model.posterior.odds.ratio

## prior.odds.ratio --> percent format
prior.model.reported$prior_percent <- with(prior.model.reported, prior.odds.ratio / (1 + prior.odds.ratio) )

tapply(X = prior.model.reported$prior_percent, 
       INDEX = prior.model.reported$target, 
       FUN = mean)

## posterior.odds.ratio --> percent format
prior.model.reported$posterior_percent <- ifelse(test = is.infinite(prior.model.reported$posterior.odds.ratio) == TRUE, 
                                                 yes = 1, 
                                                 no = ( prior.model.reported$posterior.odds.ratio / (1 + prior.model.reported$posterior.odds.ratio) )
                                                )

tapply(X = prior.model.reported$posterior_percent, 
       INDEX = list(prior.model.reported$target), 
       FUN = mean)

## model.posterior.odds.ratio --> percent format
prior.model.reported$model.posterior_percent <- ifelse(test = is.infinite(prior.model.reported$model.posterior.odds.ratio) == TRUE, 
                                                       yes = 1, 
                                                       no = ( prior.model.reported$model.posterior.odds.ratio / (1 + prior.model.reported$model.posterior.odds.ratio) )
                                                      )

tapply(X = prior.model.reported$model.posterior_percent, 
       INDEX = list(prior.model.reported$target), 
       FUN = mean)

## Calculate difference
prior.model.reported$diff <- with(prior.model.reported, posterior_percent - model.posterior_percent)

## ================================================================================================== ##
## Create long form version for plotting purposes
prior.model.reported_long <- melt(prior.model.reported, measure.vars = c("prior_percent", "posterior_percent", "model.posterior_percent"))
355*3
## Each subject gives three responses: prior, model posterior, and reported posterior

prior.model.reported_long$variable <- factor(prior.model.reported_long$variable, 
                                             levels = c("prior_percent", "model.posterior_percent", "posterior_percent"), 
                                             labels = c("Prior", "Model Posterior", "Reported Posterior"))

prior.model.reported_long$target <- factor(prior.model.reported_long$target, 
                                           levels = c("man", "woman"), 
                                           labels = c("Man", "Woman"))

## Plot means
ggplot(data = prior.model.reported_long, 
       aes(x = target, y = value, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  theme_bw() + 
  labs(y = "Probability that target is doctor\n") + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format()) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(2, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## ================================================================================================== ##
## Inferential statistics

## All subjects
fit.p_vs_mp <- lme(fixed = value ~ variable * target, 
                   random = ~1|ResponseId, 
                   data = prior.model.reported_long, 
                   method = "ML")

## Dataframe of fitted values
names(allEffects(fit.p_vs_mp))
p_vs_mp.plot <- as.data.frame(allEffects(fit.p_vs_mp)[["variable:target"]])
p_vs_mp.plot <- droplevels(p_vs_mp.plot)

## Relevel for plotting purposes
levels(p_vs_mp.plot$variable)

p_vs_mp.plot$variable <- factor(p_vs_mp.plot$variable, 
                                levels = c("Prior", "Model Posterior", "Reported Posterior"))

## Plot fitted values
ggplot(data = p_vs_mp.plot, 
       aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Doctor)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man performed surgery\nn = 183", "Woman performed surgery\nn = 172")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(),
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

# Obtain contrasts
contrasts <- contrast(lsmeans(object = fit.p_vs_mp, specs = ~variable * target), 
                      method = "pairwise", 
                      adjust = "none")

## Save as dataframe
contrasts <- as.data.frame(subset(summary(contrasts)))

## Subset out only relevant contrasts
contrasts <- subset(contrasts,
                    contrast == "Prior,Man - Prior,Woman" |
                      contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                      contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                      contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                      contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
contrasts$effect.size <- with(contrasts, 
                              sqrt((t.ratio^2)/(t.ratio^2 + df)))

contrasts

## Round P values
round(contrasts$p.value, digits = 4)

## Means
p_vs_mp.plot[,c("variable", "target", "fit")]

## Percent saying >50% for priors
table(prior.model.reported$Prior.Man.Doctor > 0.50)
254/(254+101)

## ========================================================================================================================== ##
## Performed surgery: likelihoods
likelihoods <- ggplot(data = LR.dat, 
                      mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 0.40)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.4) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14.4)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

prior.posterior.plot <- ggplot(data = p_vs_mp.plot, 
                               mapping = aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14.4) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Doctor)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man performed surgery\nn = 183", "Woman performed surgery\nn = 172")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(),
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1)

plot_grid(likelihoods, prior.posterior.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ========================================================================================================================== ##
## ========================================================================================================================== ##
## ========================================================================================================================== ##

## Evaluations of Person X data
str(dat)

## Collapse responses
levels(dat$attr.cond)

dat$query <- ifelse(test = dat$attr.cond == "1", 
                    yes = dat$c1.self, 
                    no = dat$c2.self)

dat$intelligent <- ifelse(test = dat$attr.cond == "1", 
                          yes = dat$c1.intel, 
                          no = dat$c2.intel)

dat$accurate <- ifelse(test = dat$attr.cond == "1", 
                       yes = dat$c1.acc, 
                       no = dat$c2.acc)

dat$fair <- ifelse(test = dat$attr.cond == "1", 
                   yes = dat$c1.fair, 
                   no = dat$c2.fair)

dat$just <- ifelse(test = dat$attr.cond == "1", 
                   yes = dat$c1.just, 
                   no = dat$c2.just)

## Collapse comments
dat$c1.free <- as.character(dat$c1.free)
dat$c2.free <- as.character(dat$c2.free)

dat$comment <- ifelse(test = dat$attr.cond == "1", 
                      yes = dat$c1.free, 
                      no = dat$c2.free)

## Rename rows
rownames(dat) <- c(1:nrow(dat))

## =============================================================================================================== ##
## query: which statement do participants agree with?
count(dat$query)
round(x = 13/355*100, digits = 1)   ## M < W 
round(x = 157/355*100, digits = 1)  ## M = W  
round(x = 185/355*100, digits = 1)  ## M > W

dat$query <- factor(x = dat$query, 
                    levels = c(1:3), 
                    labels = c("Woman more likely to be pilot", 
                               "Equally likley to be pilot", 
                               "Man more likely to be pilot"))

## =============================================================================================================== ##
## Four main items of interest: intelligent, accurate, fair, and just

## Create dataset with just composite items
dat.composite <- dat[,c("intelligent", "accurate", "fair", "just")]

dat.composite <- melt(dat.composite, measure.vars = c("intelligent", "accurate", "fair", "just"))

levels(dat.composite$variable)
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("fair", "intelligent", "just", "accurate"), 
                                 labels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unintelligent ... 7 = Intelligent", 
                                            "1 = Unjust ... 7 = Just",
                                            "1 = Inaccurate ... 7 = Accurate"))

## Visualize
ggplot(data = dat.composite, 
       aes(x = value)) + 
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  facet_wrap(~variable, nrow = 2) + 
  coord_cartesian(xlim = c(0:8)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), labels = percent_format()) + 
  labs(y = "Proprotion of participants") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_blank()) + 
  theme(panel.spacing = unit(2, "lines")) + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  theme(axis.title.y = element_text(vjust = 2, size = 16)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title.x = element_blank())

## Correlations among items
cor(dat[,c("intelligent", "accurate", "fair", "just")])

## Cronbach's alpha
psych::alpha(x = var(dat[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.89

## Compute composite average
dat$composite.avg <- with(dat, (intelligent + accurate + fair + just) / 4)

## Descriptives of composite average
library(plotrix)
round(x = mean(dat$composite.avg), digits = 2)
round(x = std.error(dat$composite.avg), digits = 2)

## Inferential statistics on composite average
t.test(x = dat$composite.avg, 
       mu = 4)

## Cohen's d
round(x = cohensD(x = dat$composite.avg, 
                  mu = 4), 
      digits = 2)

## 95% CI
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

one.sample.t.cohensD.results <- boot(data = dat, 
                                     statistic = one.sample.t.cohensD, 
                                     R = 10000)

hist(one.sample.t.cohensD.results$t)

round(x = quantile(x = one.sample.t.cohensD.results$t, 
                   probs = c(0.025, 0.975)), 
      digits = 2)

## Item means and standard errors
round(apply(X = dat[,c("fair", "just", "accurate", "intelligent")], 
            MARGIN = 2, 
            FUN = mean), 
      digits = 2)

round(apply(X = dat[,c("fair", "just", "accurate", "intelligent")], 
            MARGIN = 2, 
            FUN = std.error), 
      digits = 2)

std.error(dat$composite.avg)

## ====== ##
## ====== ##
## ====== ##
## ====== ##

## Merge prior, model posterior, and reported posterior with dat
colnames(dat)
colnames(prior.model.reported)

dat <- merge(x = dat, 
             y = prior.model.reported[,c("ResponseId", "posterior_percent", "model.posterior_percent", "diff")], 
             by = "ResponseId")


## Reported:
ggplot(data = dat, 
       mapping = aes(x = composite.avg, y = posterior_percent, color = target)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "<-Negative             Positive->\n\nAverage Evaluation",
       y = expression(paste(italic(P), "(Target = Doctor)"))) + 
  coord_cartesian(xlim = c(0.5, 7.5), 
                  ylim = c(0.65, 0.85)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0.65, 0.85, 0.05),
                     label = percent_format()) + 
  scale_color_discrete(labels = c("Man Performed Surgery",
                                  "Woman Performed Surgery")) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  theme(legend.direction = "vertical") + 
  theme(aspect.ratio = 5/7)

dat %>%
  ggplot(mapping = aes(x = composite.avg, y = posterior_percent, color = target)) + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "<-Negative     Positive->\n\nEvaluation of Person X",
       y = expression(paste(italic(P), "(Target = Doctor)"))) + 
  coord_cartesian(xlim = c(0.5, 7.5),
                  ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.10),
                     label = percent_format()) + 
  scale_color_discrete(labels = c("Man Performed Surgery",
                                  "Woman Performed Surgery")) + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.50, 0.25)) + 
  theme(aspect.ratio = 10/7)

## 350 x 500; PNG

fit <- lm(formula = posterior_percent ~ composite.avg * target, 
          data = dat)

summary(fit)
anova(fit)
etaSquared(fit)

## 95% CI around effect sizes
  ## Main effect of target
ci.pvaf(F.value = 7.5204, 
        df.1 = 1,
        df.2 = 351,
        N = 355, 
        conf.level = 0.95)

## Shifted
  ## At what point do we lose statistical significance?
shift.amount <- rev(seq(1, 4, 0.05))
p.val <- rep(NA, length(shift.amount))
shift.dat <- dat

for(i in 1:length(shift.amount)) {
  
  shift.dat$composite.avg.shifted <- shift.dat$composite.avg - shift.amount[i]
  
  summary.fit <- summary(lm(formula = posterior_percent ~ composite.avg.shifted * target,
                         data = shift.dat))
  
  p.val[i] <- summary.fit$coefficients[[3, 4]]
  
}

ggplot(data = data.frame(shift.amount = shift.amount, p.val = p.val), 
       mapping = aes(x = shift.amount, y = p.val)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0.05, color = "red")

which(p.val <= 0.05)
shift.amount[11]
hist(dat$composite.avg)
quantile(dat$composite.avg, prob = seq(0, 1, length = 11))
summary(dat$composite.avg)

## ================================================================================================ ##

## Correlation between diff and composite.avg
cor.test(x = dat$composite.avg, 
         y = dat$diff, 
         method = "pearson")

## Slope
summary(lm(diff ~ composite.avg, dat))

## Histograms on sides of scatterplot
library("ggExtra")

p <- ggplot(data = dat,
            mapping = aes(x = composite.avg, y = diff)) +
  geom_point()
ggExtra::ggMarginal(p, type = "histogram")

p <- ggplot(data = dat, 
            mapping = aes(x = composite.avg, y = diff)) + 
  geom_jitter(color = "darkgrey", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1.25) + 
  labs(x = "<-Negative             Positive->\n\nAverage Evaluation", 
       y = "Judgment\n\n<-Underestimate      Overestimate->") + 
  coord_cartesian(xlim = c(0:8), ylim = c(-1.1, 1.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) + 
  theme_bw(base_size = 14) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(aspect.ratio = 1)

p


ggExtra::ggMarginal(p, type = "histogram")
## 525 x 525

