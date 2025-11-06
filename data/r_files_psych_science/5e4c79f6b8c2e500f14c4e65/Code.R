############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-07-2020                                  #
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

# 2.2. Create Single Column for Composite Measure of Confidence (OPTIONAL)

dat$confidence <- (dat$confidence1 + dat$confidence2)/2
  
# 2.3. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Wager

t.test(wager~choice, data=dat)

# 3.2. The Effect of Choice on Confidence

t.test(confidence~choice, data=dat)


# 4.   Calculate Summary Statistics

# 4.1. Wager By Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(wager),
      mean = mean(wager),
      sd   = sd(wager),
      se   = sd / sqrt(N))

# 4.2. Confidence by Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(confidence),
      mean = mean(confidence),
      sd   = sd(confidence),
      se   = sd / sqrt(N))

# 4.3. Effect Size for Wager

cohen.d(dat$wager[dat$choice==1], dat$wager[dat$choice==0])

# 4.4. Effect Size for Confidence

cohen.d(dat$confidence[dat$choice==1], dat$confidence[dat$choice==0])