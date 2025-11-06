############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-11-2020                                  #
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

# 2.1. Create Single Column for Composite Measure of Predicted Satisfaction (OPTIONAL)

dat <- dat %>%
  mutate(predicted = case_when(choice == .5 & actual_control == .5 ~ (c_a_happy+c_a_tasty)/2,
                               choice == -.5 & actual_control == .5 ~ (nc_a_happy+nc_a_tasty)/2,
                               choice == .5 & actual_control == -.5 ~ (c_i_happy+c_i_tasty)/2,
                               choice == -.5 & actual_control == -.5 ~ (nc_i_happy+nc_i_tasty)/2))

# 2.3. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Predicted Satisfaction (All Conditions)

summary(lm(predicted ~ choice*actual_control, dat=dat))

# 3.2. The Effect of Choice on Predicted Satisfaction (Illusory-control Condition)

summary(lm(predicted ~ choice, dat=dat[dat$actual_control == -.5,]))

# 3.3. The Effect of Choice on Predicted Satisfaction (Actual-control Condition)

summary(lm(predicted ~ choice, dat=dat[dat$actual_control ==  .5,]))


# 4.   Calculate Summary Statistics

# 4.1. Predicted Satisfaction by Choice and Control Conditions

ddply(dat, c("choice", "actual_control"), summarise,
      N    = length(predicted),
      mean = mean(predicted),2,
      sd   = sd(predicted),2,
      se   = sd / sqrt(N),2)