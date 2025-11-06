############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-08-2020                                  #
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

# 2.2. Create Single Columns for Variables Happy and Tasty (OPTIONAL)

dat <- dat %>%
  mutate(happy = case_when(choice == 1 ~ c_happy,
                           choice == 0 ~ nc_happy),
         tasty = case_when(choice == 1 ~ c_tasty,
                           choice == 0 ~ nc_tasty))

# 2.3. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Predicted Happiness

t.test(happy~choice, data=dat)

# 3.2. The Effect of Choice on Predicted Taste

t.test(tasty~choice, data=dat)


# 4.   Calculate Summary Statistics

# 4.1. Predicted Happiness By Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(happy),
      mean = mean(happy),
      sd   = sd(happy),
      se   = sd / sqrt(N))

# 4.2. Predicted Taste by Choice Condition

ddply(dat, c("choice"), summarise,
      N    = length(tasty),
      mean = mean(tasty),
      sd   = sd(tasty),
      se   = sd / sqrt(N))

# 4.3. Effect Size for Predicted Happiness

cohen.d(dat$happy[dat$choice==1], dat$happy[dat$choice==0])

# 4.4. Effect Size for Predicted Taste

cohen.d(dat$tasty[dat$choice==1], dat$tasty[dat$choice==0])