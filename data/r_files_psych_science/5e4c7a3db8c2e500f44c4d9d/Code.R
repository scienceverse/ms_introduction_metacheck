############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-10-2020                                  #
#                                                          #
############################################################

# 1.   Install and Load packages

install.packages("plyr")
install.packages("dplyr")
install.packages("effsize")

library("plyr")
library("dplyr")
library("effsize")


# 2.   Prepare Data

# 2.1. Load Data

setwd("location to the folder that contains the data")
dat <- read.csv("Data.csv")

# 2.2. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Wager (All Conditions)

summary(lm(wager ~ choice*actual_control, dat=dat))

# 3.2. The Effect of Choice on Wager (Illusory-control Condition)

summary(lm(wager ~ choice, dat=dat[dat$actual_control == -.5,]))

# 3.3. The Effect of Choice on Wager (Actual-control Condition)

summary(lm(wager ~ choice, dat=dat[dat$actual_control ==  .5,]))


# 4.   Calculate Summary Statistics

# 4.1. Wager by Choice and Control Conditions

ddply(dat, c("actual_control", "choice"), summarise,
      N    = length(wager),
      mean = mean(wager),
      sd   = sd(wager),
      se   = sd / sqrt(N))

# 4.2. Effect Size for Illusory-control Condition

cohen.d(dat$wager[dat$choice==.5 & dat$actual_control==-.5],
        dat$wager[dat$choice==-.5 & dat$actual_control==-.5])

# 4.3. Effect Size for Actual-control Condition

cohen.d(dat$wager[dat$choice==.5 & dat$actual_control==.5],
        dat$wager[dat$choice==-.5 & dat$actual_control==.5])