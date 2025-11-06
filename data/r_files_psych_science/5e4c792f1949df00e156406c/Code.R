############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-02-2020                                  #
#                                                          #
############################################################

# 1.   Install and Load packages

install.packages("plyr")
install.packages("dplyr")
install.packages("effsize")

library("plyr")
library("dplyr")
library("effsize")


# 2.1. Load Data

setwd("location to the folder that contains the data")
dat <- read.csv("Data.csv")

# 2.2. Create Single Column for First Subscale of Dependent Variable (OPTIONAL)

dat <- dat %>%
  mutate(confidence1 = case_when(choice ==  .5 & choice_first ==  .5  ~ c_cf_confidence1,
                                 choice == -.5 & choice_first ==  .5  ~ nc_cf_confidence1,
                                 choice ==  .5 & choice_first == -.5  ~ c_cl_confidence1,
                                 choice == -.5 & choice_first == -.5  ~ nc_cl_confidence1))

# 2.3. Create Single Column for Second Subscale of Dependent Variable (OPTIONAL)

dat <- dat %>%
  mutate(confidence2 = case_when(choice ==  .5 & choice_first ==  .5  ~ c_cf_confidence2,
                                 choice == -.5 & choice_first ==  .5  ~ nc_cf_confidence2,
                                 choice ==  .5 & choice_first == -.5  ~ c_cl_confidence2,
                                 choice == -.5 & choice_first == -.5  ~ nc_cl_confidence2))

# 2.4. Create Column for Composite Measure of Dependent Variable (OPTIONAL)

dat$confidence <- (dat$confidence1 + dat$confidence2)/2

# 2.5. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effects of Choice and Timing-of-choice (All Conditions)

summary(lm(confidence ~ choice * choice_first, data=dat))

# 3.2. The Effects of Choice (Choice-first Condition)

summary(lm(confidence ~ choice, data=dat[dat$choice_first==.5,]))

# 3.3. The Effects of Choice (Choice-last Condition)

summary(lm(confidence ~ choice, data=dat[dat$choice_first==-.5,]))

# 3.4. The Effect of Choice (All Conditions)

summary(lm(confidence ~ choice, data=dat))


# 4.   Calculate Summary Statistics

# 4.1. Confidence by Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(confidence),
      mean = mean(confidence),
      sd   = sd(confidence),
      se   = sd / sqrt(N))

# 4.2. Confidence by Choice and Timing-of-choice Conditions

ddply(dat, c("choice_first", "choice"), summarise,
      N    = length(confidence),
      mean = mean(confidence),
      sd   = sd(confidence),
      se   = sd / sqrt(N))

# 4.3. Correlation Between Two Dependent Variable Questions

cor.test(dat$confidence1, dat$confidence2)

# 4.3. Effect Size by Choice Condition

cohen.d(dat$confidence[dat$choice==.5], dat$confidence[dat$choice==-.5])

# 4.4. Effect Size by Choice and Timing-of-choice Conditions

cohen.d(dat$confidence[dat$choice==.5 & dat$choice_first==.5], dat$confidence[dat$choice==-.5 & dat$choice_first==.5])
cohen.d(dat$confidence[dat$choice==.5 & dat$choice_first==-.5], dat$confidence[dat$choice==-.5 & dat$choice_first==-.5])