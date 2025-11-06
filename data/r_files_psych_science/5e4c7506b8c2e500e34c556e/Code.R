############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-01-2020                                  #
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

# 2.2. Create Single Column for Dependent Variable Likelihood (OPTIONAL)

dat <- dat %>%
  mutate(likelihood = case_when(choice ==  .5 & choice_first ==  .5  ~ c_cf_likelihood,
                                choice == -.5 & choice_first ==  .5  ~ nc_cf_likelihood,
                                choice ==  .5 & choice_first == -.5  ~ c_cl_likelihood,
                                choice == -.5 & choice_first == -.5  ~ nc_cl_likelihood))

# 2.3. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.  Run Analyses

# 3.1 The Effects of Choice and Timing-of-choice (All Conditions)

summary(lm(likelihood ~ choice * choice_first, data=dat))

# 3.2 The Effect of Choice (Choice-first Condition)

summary(lm(likelihood ~ choice, data=dat[dat$choice_first==.5,]))

# 3.3 The Effect of Choice (Choice-last Condition)

summary(lm(likelihood ~ choice, data=dat[dat$choice_first==-.5,]))

# 3.4 The Effect of Choice (All Conditions)

summary(lm(likelihood ~ choice, data=dat))


# 4.  Calculate Summary Statistics

# 4.1 Likelihood by Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(likelihood),
      mean = mean(likelihood),
      sd   = sd(likelihood),
      se   = sd / sqrt(N))

# 4.2 Likelihood by Choice and Timing-of-choice Conditions

ddply(dat, c("choice_first","choice"), summarise,
      N    = length(likelihood),
      mean = mean(likelihood),
      sd   = sd(likelihood),
      se   = sd / sqrt(N))

# 4.3. Effect Size by Choice Condition

cohen.d(dat$likelihood[dat$choice==.5], dat$likelihood[dat$choice==-.5])

# 4.4. Effect Size by Choice and Timing-of-choice Conditions

cohen.d(dat$likelihood[dat$choice==.5 & dat$choice_first==.5], dat$likelihood[dat$choice==-.5 & dat$choice_first==.5])
cohen.d(dat$likelihood[dat$choice==.5 & dat$choice_first==-.5], dat$likelihood[dat$choice==-.5 & dat$choice_first==-.5])
