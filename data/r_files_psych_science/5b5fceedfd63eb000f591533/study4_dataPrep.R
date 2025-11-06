## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 4: Data preparation

## Packages needed
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("plyr")) {install.packages("plyr"); require("plyr")}

## Load data
load("doctor.nurse.Rda")

## Eliminate unnecessary rows
doctor.nurse <- doctor.nurse[-c(1:15),]

## Rename some of the columns; everything except heart of the data
colnames(doctor.nurse)

colnames(doctor.nurse)[1] <- "QualtricsID" 
colnames(doctor.nurse)[2] <- "StartTime" 
colnames(doctor.nurse)[3] <- "EndTime" 
colnames(doctor.nurse)[8] <- "Instructions.Time" 

## Convert main data to from factor to numeric
str(doctor.nurse)

doctor.nurse[8:116] <- sapply(X = doctor.nurse[8:116], 
                              FUN = as.character)

doctor.nurse[8:116] <- sapply(X = doctor.nurse[8:116], 
                              FUN = as.numeric)

str(doctor.nurse)

## Clean it up by each of the 12 conditions ============================================================================================= ##

## Condition 1: Target = Man, Data = Surgery, Order = LPP ================================================== ##
cond1 <- subset(doctor.nurse, Target == "Man" & Data == "Surgery" & Order == "LPP")
cond1 <- cond1[,c(1:17, 117:123)]
colnames(cond1)[9:17] <- c("Percentage.Male.Doctors.Surgery",
                           "Percentage.Male.Nurses.Surgery", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")          

cond1 <- melt(data = cond1, measure.vars = c("Percentage.Male.Doctors.Surgery",
                                             "Percentage.Male.Nurses.Surgery", 
                                             "Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor"))


## Condition 2: Target = Man, Data = Surgery, Order = PPL ================================================== ##
cond2 <- subset(doctor.nurse, Target == "Man" & Data == "Surgery" & Order == "PPL")
cond2 <- cond2[,c(1:8, 18:26, 117:123)]
colnames(cond2)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Male.Doctors.Surgery", 
                           "Percentage.Male.Nurses.Surgery", 
                           "Likelihood.Time")
                    
cond2 <- melt(data = cond2, measure.vars = c("Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor", 
                                             "Percentage.Male.Doctors.Surgery", 
                                             "Percentage.Male.Nurses.Surgery"))


## Condition 3: Target = Man, Data= CPR, Order = LPP ================================================== ##
cond3 <- subset(doctor.nurse, Target == "Man" & Data == "CPR" & Order == "LPP")
cond3 <- cond3[,c(1:8, 27:35, 117:123)]
colnames(cond3)[9:17] <- c("Percentage.Male.Doctors.CPR",
                           "Percentage.Male.Nurses.CPR", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")

cond3 <- melt(data = cond3, measure.vars = c("Percentage.Male.Doctors.CPR",
                                             "Percentage.Male.Nurses.CPR", 
                                             "Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor"))

## Condition 4: Target = Man, Data= CPR, Order = PPL ================================================== ##
cond4 <- subset(doctor.nurse, Target == "Man" & Data == "CPR" & Order == "PPL")
cond4 <- cond4[,c(1:8, 36:44, 117:123)]
colnames(cond4)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Male.Doctors.CPR", 
                           "Percentage.Male.Nurses.CPR", 
                           "Likelihood.Time")

cond4 <- melt(data = cond4, measure.vars = c("Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor", 
                                             "Percentage.Male.Doctors.CPR", 
                                             "Percentage.Male.Nurses.CPR"))


## Condition 5: Target = Man, Data= SpongeBath, Order = LPP ================================================== ##
cond5 <- subset(doctor.nurse, Target == "Man" & Data == "SpongeBath" & Order == "LPP")
cond5 <- cond5[,c(1:8, 45:53, 117:123)]
colnames(cond5)[9:17] <- c("Percentage.Male.Doctors.SpongeBath",
                           "Percentage.Male.Nurses.SpongeBath", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")

cond5 <- melt(data = cond5, measure.vars = c("Percentage.Male.Doctors.SpongeBath",
                                             "Percentage.Male.Nurses.SpongeBath", 
                                             "Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor"))


## Condition 6: Target = Man, Data = SpongeBath, Order = PPL ================================================== ##
cond6 <- subset(doctor.nurse, Target == "Man" & Data == "SpongeBath" & Order == "PPL")
cond6 <- cond6[,c(1:8, 54:62, 117:123)]
colnames(cond6)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Male.Doctors.SpongeBath", 
                           "Percentage.Male.Nurses.SpongeBath", 
                           "Likelihood.Time")

cond6 <- melt(data = cond6, measure.vars = c("Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor", 
                                             "Percentage.Male.Doctors.SpongeBath", 
                                             "Percentage.Male.Nurses.SpongeBath"))


## =================================================================================================== ##
## =================================================================================================== ##
## =================================================================================================== ##
## =================================================================================================== ##
## =================================================================================================== ##
## =================================================================================================== ##
## =================================================================================================== ##
## Conditions 7-12 are when Target = Woman

## Condition 7: Target = Woman, Data = Surgery, Order = LPP ================================================== ##
cond7 <- subset(doctor.nurse, Target == "Woman" & Data == "Surgery" & Order == "LPP")
cond7 <- cond7[,c(1:8, 63:71, 117:123)]
colnames(cond7)[9:17] <- c("Percentage.Female.Doctors.Surgery",
                           "Percentage.Female.Nurses.Surgery", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")

cond7 <- melt(data = cond7, measure.vars = c("Percentage.Female.Doctors.Surgery",
                                             "Percentage.Female.Nurses.Surgery", 
                                             "Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor"))


## Condition 8: Target = Woman, Data= Surgery, Order = PPL ================================================== ##
cond8 <- subset(doctor.nurse, Target == "Woman" & Data == "Surgery" & Order == "PPL")
cond8 <- cond8[,c(1:8, 72:80, 117:123)]
colnames(cond8)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Female.Doctors.Surgery", 
                           "Percentage.Female.Nurses.Surgery", 
                           "Likelihood.Time")

cond8 <- melt(data = cond8, measure.vars = c("Prior.Man.Doctor", 
                                              "Prior.Woman.Doctor", 
                                              "Posterior.Man.Doctor", 
                                              "Posterior.Woman.Doctor", 
                                              "Percentage.Female.Doctors.Surgery", 
                                              "Percentage.Female.Nurses.Surgery"))


## Condition 9: Target = Woman, Data= CPR, Order = LPP ================================================== ##
cond9 <- subset(doctor.nurse, Target == "Woman" & Data == "CPR" & Order == "LPP")
cond9 <- cond9[,c(1:8, 81:89, 117:123)]
colnames(cond9)[9:17] <- c("Percentage.Female.Doctors.CPR",
                           "Percentage.Female.Nurses.CPR", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")

cond9 <- melt(data = cond9, measure.vars = c("Percentage.Female.Doctors.CPR",
                                             "Percentage.Female.Nurses.CPR", 
                                             "Prior.Man.Doctor", 
                                             "Prior.Woman.Doctor", 
                                             "Posterior.Man.Doctor", 
                                             "Posterior.Woman.Doctor"))


## Condition 10: Target = Woman, Data= CPR, Order = PPL ================================================== ##
cond10 <- subset(doctor.nurse, Target == "Woman" & Data == "CPR" & Order == "PPL")
cond10 <- cond10[,c(1:8, 90:98, 117:123)]
colnames(cond10)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Female.Doctors.CPR", 
                           "Percentage.Female.Nurses.CPR", 
                           "Likelihood.Time")

cond10 <- melt(data = cond10, measure.vars = c("Prior.Man.Doctor", 
                                               "Prior.Woman.Doctor", 
                                               "Posterior.Man.Doctor", 
                                               "Posterior.Woman.Doctor", 
                                               "Percentage.Female.Doctors.CPR", 
                                               "Percentage.Female.Nurses.CPR"))

## Condition 11: Target = Woman, Data= SpongeBath, Order = LPP ================================================== ##
cond11 <- subset(doctor.nurse, Target == "Woman" & Data == "SpongeBath" & Order == "LPP")
cond11 <- cond11[,c(1:8, 99:107, 117:123)]
colnames(cond11)[9:17] <- c("Percentage.Female.Doctors.SpongeBath",
                           "Percentage.Female.Nurses.SpongeBath", 
                           "Likelihood.Time", 
                           "Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time")

cond11 <- melt(data = cond11, measure.vars = c("Percentage.Female.Doctors.SpongeBath",
                                               "Percentage.Female.Nurses.SpongeBath", 
                                               "Prior.Man.Doctor", 
                                               "Prior.Woman.Doctor", 
                                               "Posterior.Man.Doctor", 
                                               "Posterior.Woman.Doctor"))


## Condition 12: Target = Woman, Data= SpongeBath, Order = PPL ================================================== ##
cond12 <- subset(doctor.nurse, Target == "Woman" & Data == "SpongeBath" & Order == "PPL")
cond12 <- cond12[,c(1:8, 108:116, 117:123)]
colnames(cond12)[9:17] <- c("Prior.Man.Doctor", 
                           "Prior.Woman.Doctor", 
                           "Prior.Time", 
                           "Posterior.Man.Doctor", 
                           "Posterior.Woman.Doctor", 
                           "Posterior.Time", 
                           "Percentage.Female.Doctors.SpongeBath", 
                           "Percentage.Female.Nurses.SpongeBath", 
                           "Likelihood.Time")

cond12 <- melt(data = cond12, measure.vars = c("Prior.Man.Doctor", 
                                               "Prior.Woman.Doctor", 
                                               "Posterior.Man.Doctor", 
                                               "Posterior.Woman.Doctor", 
                                               "Percentage.Female.Doctors.SpongeBath", 
                                               "Percentage.Female.Nurses.SpongeBath"))


## Confirm no subjects were lost
dim(cond1)[1] + dim(cond2)[1] + 
  dim(cond3)[1] + dim(cond4)[1] + 
  dim(cond5)[1] + dim(cond6)[1] + 
  dim(cond7)[1] + dim(cond8)[1] + 
  dim(cond9)[1] + dim(cond10)[1] + 
  dim(cond11)[1] + dim(cond12)[1]
  
## Equals 5394
5394/899
  ## Each subject gave 6 data points
    ## prior (2x), posterior (2x), and likelihood (2x)

## ==================================================================================================== ##
## Merge datasets

dat <- rbind(cond1, cond2, cond3, cond4, cond5, cond6, 
             cond7, cond8, cond9, cond10, cond11, cond12)

## Get rid of individual condition datasets
rm(cond1)
rm(cond2)
rm(cond3)
rm(cond4)
rm(cond5)
rm(cond6)
rm(cond7)
rm(cond8)
rm(cond9)
rm(cond10)
rm(cond11)
rm(cond12)

## Reformat variables
str(dat)

## Drop unused levels
dat <- droplevels(dat)

## Convert QualtricsID to character
dat$QualtricsID <- as.character(dat$QualtricsID)
is.character(dat$QualtricsID)

## StartTime and EndTime need to be in date format
dat$StartTime <- as.POSIXct(dat$StartTime, format = "%Y-%m-%d %H:%M:%S")
dat$EndTime <- as.POSIXct(dat$EndTime, format = "%Y-%m-%d %H:%M:%S")

## Create new variable, how much time each respondent spent on survey, in minutes
dat$Total.Time.Spent <- with(dat, (EndTime - StartTime)/60)
dat$Total.Time.Spent <- as.numeric(dat$Total.Time.Spent)

## Condition
levels(dat$Target)
levels(dat$Data)
levels(dat$Order)

## Convert MTurkID to character
dat$MTurkID <- as.character(dat$MTurkID)

## Relabel demographics variables

  ## Gender
levels(dat$gender)

dat$gender <- factor(dat$gender, 
                     levels = c("1", "2"), 
                     labels = c("Male", "Female"))

levels(dat$gender)

  ## Education
levels(dat$education)

dat$education <- factor(dat$education, 
                        levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), 
                        labels = c("Some High School", 
                                   "High School Graduate", 
                                   "Some College", 
                                   "Associate's Degree", 
                                   "Bachelor's Degree", 
                                   "Some Graduate School", 
                                   "Master's Degree", 
                                   "MBA", 
                                   "JD", 
                                   "MD",
                                   "PhD", 
                                   "Other Advanced Degree"))

levels(dat$education)

  ## politics
levels(dat$politics)

dat$politics <- factor(dat$politics, 
                       levels = c("1", "2", "3", "4", "5", "6", "7"), 
                       labels = c("Strongly Conservative", 
                                  "Moderately Conservative", 
                                  "Slightly Conservative", 
                                  "Neutral", 
                                  "Slightly Liberal", 
                                  "Moderately Liberal", 
                                  "Strongly Liberal"))

levels(dat$politics)

  ## lookup
levels(dat$lookup)

dat$lookup <- factor(dat$lookup, 
                     levels = c("1", "2"), 
                     labels = c("Yes", "No"))

levels(dat$lookup)

## Convert comments to character
dat$comments <- as.character(dat$comments)

## Calcuate age of each participant at time of survey completion
dat$Birthday <- paste(dat$b.month, 
                      "16",           ## Because data collection happened of 16th date of April 2015
                      dat$b.year, 
                      sep = "/")

dat$Birthday <- as.POSIXct(dat$Birthday, 
                           format = "%m/%d/%Y")

dat$Date.of.Completion <- as.POSIXct("4/16/2015", format = "%m/%d/%Y")

dat$Age <- with(dat, (Date.of.Completion - Birthday)/365)

dat$Age <- as.numeric(dat$Age)

## Convert b.month to factor and b.year to numeric
dat$b.month <- factor(dat$b.month, 
                      levels = c(1:12), 
                      labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", 
                                 "July", "Aug", "Sept", "Oct", "Nov", "Dec"))

dat$b.year <- as.numeric(levels(dat$b.year))[dat$b.year]


## Change order of coluns
colnames(dat)

dat <- dat[,c("QualtricsID", "MTurkID", "Date.of.Completion",
              "StartTime", "EndTime", "Total.Time.Spent", 
              "Target", "Data", "Order", 
              "Instructions.Time", "Likelihood.Time", "Prior.Time", "Posterior.Time", 
              "gender", "education", "politics", "b.month", "b.year", "Birthday", "Age", 
              "lookup",
              "variable", "value", "comments")]

## Make value into a percentage
dat$value <- dat$value/100

## Convert dat to wide format
colnames(dat)

dat.wide <- dcast(data =  dat, 
                  formula = QualtricsID + MTurkID + Date.of.Completion + StartTime + EndTime + 
                    Total.Time.Spent + Target + Data + Order + Instructions.Time + 
                    Likelihood.Time + Prior.Time + Posterior.Time + gender + education + 
                    politics + b.month + b.year + Birthday + Age + lookup + comments ~ variable, 
                  value.var = 'value')

## Reorder columns in dat.wide
dat.wide <- dat.wide[,c(1:21, 
                        25:26, 
                        27:28,
                        23:24,
                        29:38,
                        22)]

## Save dat.wide as R dataframe
save(dat.wide, file = "dat.wide.Rda")

