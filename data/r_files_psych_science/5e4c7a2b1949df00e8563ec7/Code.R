############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-09-2020                                  #
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

# 2.2. Re-code Choice and Baseline liking as Pre-registered (OPTIONAL)

dat$choice <- dat$raw_choice - .5

dat$baseline_liking <- dat$raw_baseline_liking - mean(dat$raw_baseline_liking, na.rm = TRUE)

# 2.3. Create Column to Indicate Choosing-to-choose vs. Choice/No-choice (OPTIONAL)

dat <- dat %>%
   mutate(choosing_to_choose = case_when(prefer_c=="" ~ 0,
                                         prefer_c!="" ~ 1))

# 2.4. Create Single Columns for Variables Predicted and Actual Satisfaction (OPTIONAL)

dat <- dat %>%
   mutate(predicted = case_when(choice ==  .5 ~ (c_predicted1 + c_predicted2) / 2,
                                choice == -.5 ~ (nc_predicted1 + nc_predicted2) /2),
          actual = case_when(choice ==  .5 ~ (c_actual1 + c_actual2) / 2,
                             choice == -.5 ~ (nc_actual1 + nc_actual2) /2))

# 2.5. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Predicted Satisfaction, Controlling for Baseline Liking

summary(lm(predicted ~ choice*baseline_liking, dat=dat[dat$choosing_to_choose==0,]))

# 3.2. The Effect of Choice on Predicted Satisfaction, Uncontrolling for Baseline Liking

summary(lm(predicted ~ choice, dat=dat[dat$choosing_to_choose==0,]))

# 3.3. The Effect of Choice on Actual Satisfaction, Controlling for Baseline Liking

summary(lm(actual ~ choice*baseline_liking, dat=dat[dat$choosing_to_choose==0,]))

# 3.4. The Effect of Choice on Actual Satisfaction, Uncontrolling for Baseline Liking

summary(lm(actual ~ choice, dat=dat[dat$choosing_to_choose==0,]))

# 3.5. Proportion of Participants Preferring Choice vs. No-choice

binom.test(sum(dat$choosing_to_choose == 1 & dat$prefer_c == 1), sum(dat$choosing_to_choose ==1), p = .5)


# 4.   Calculate Summary Statistics

# 4.1. Predicted Satisfaction By Choice Condition

ddply(dat[dat$choosing_to_choose == 0,], c("choice"), summarise,
      N    = length(predicted),
      mean = mean(predicted),
      sd   = sd(predicted),
      se   = sd / sqrt(N))

# 4.2. Actual Satisfaction By Choice Condition

ddply(dat[dat$choosing_to_choose == 0,], c("choice"), summarise,
      N    = length(actual),
      mean = mean(actual),
      sd   = sd(actual),
      se   = sd / sqrt(N))

# 4.3. Effect Size for Predicted Satisfaction

cohen.d(dat$predicted[dat$choice==.5 & dat$choosing_to_choose==0],
        dat$predicted[dat$choice==-.5 & dat$choosing_to_choose==0])

# 4.4. Effect Size for Actual Satisfaction

cohen.d(dat$actual[dat$choice==.5 & dat$choosing_to_choose==0],
        dat$actual[dat$choice==-.5 & dat$choosing_to_choose==0])