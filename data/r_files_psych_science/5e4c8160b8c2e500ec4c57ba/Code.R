############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-15-2020                                  #
#                                                          #
############################################################

# 1.   Install and Load packages

install.packages("plyr")
install.packages("dplyr")

library("plyr")
library("dplyr")


# 2.   Prepare Data

# 2.1. Load Data

setwd("location to the folder that contains the data")
dat <- read.csv("Data.csv")

# 2.2. Create Single Column for Probability Estimate for Highest-rated Box (P_highest) (OPTIONAL)

dat <- dat %>%
  group_by(workerId_coded, excluded) %>%
  mutate(p_highest = case_when(pre == 1 ~ max(pre1, pre2, pre3),
                               pre == 0 ~ max(post1, post2, post3)))%>%
  ungroup()

# 2.3. Create Column Indicating Uniform Probability Distribution (Uniform) (OPTIONAL)

dat <- dat %>%
  group_by(workerId_coded, excluded) %>%
  mutate(p_lowest = case_when(pre == 1 ~ min(pre1, pre2, pre3),
                              pre == 0 ~ min(post1, post2, post3)),
         p_second_highest = 100 - p_highest - p_lowest,
         uniform  = case_when((p_highest <= 34 & p_lowest >= 33) ~ 1,
                             !(p_highest  <= 34 & p_lowest >= 33) ~ 0)) %>%
  ungroup()

# 2.4. Create Single Column Indicating Pre (0), Post-choice (1), and Post-no-choice (2) Condition

dat <- dat %>%
  mutate(choice_tertiary = case_when(pre == 1 ~ 0,
                                     pre == 0 & choice == 1 ~ 1,
                                     pre == 0 & choice == 0 ~ 2))

# 2.5. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses and Calculate Effect Sizes

# 3.1. Post-choice vs. Pre on Probability Estimate for Highest-rated Box

t.test(dat$p_highest[dat$choice_tertiary==1], dat$p_highest[dat$choice_tertiary==0])
cohen.d(dat$p_highest[dat$choice_tertiary==1], dat$p_highest[dat$choice_tertiary==0])

# 3.2. Post-choice vs. Post-no-choice on Probability Estimate for Highest-rated Box

t.test(dat$p_highest[dat$choice_tertiary==1], dat$p_highest[dat$choice_tertiary==2])
cohen.d(dat$p_highest[dat$choice_tertiary==1], dat$p_highest[dat$choice_tertiary==2])

# 3.3. Post-choice vs. Pre on Probability Estimate for Highest-rated Box

t.test(dat$p_highest[dat$choice_tertiary==2], dat$p_highest[dat$choice_tertiary==0])
cohen.d(dat$p_highest[dat$choice_tertiary==2], dat$p_highest[dat$choice_tertiary==0])

# 3.4. Compare Post-choice vs. Post-no-choice  on Probability Estimate for Highest-rated Box

t.test(dat$p_selected[dat$choice_tertiary==1], dat$p_selected[dat$choice_tertiary==2])
cohen.d(dat$p_selected[dat$choice_tertiary==1], dat$p_selected[dat$choice_tertiary==2])


# 4.   Calculate Summary Statistics

# 4.1. Probability Estimate for Highest-rated Box by Condition

ddply(dat, c("choice_tertiary"), summarise,
      N    = length(p_highest),
      mean = mean(p_highest),
      sd   = sd(p_highest),
      se   = sd / sqrt(N))

# 4.2. Probability Estimate for Second-highest-rated Box by Condition

ddply(dat, c("choice_tertiary"), summarise,
      N    = length(p_second_highest),
      mean = mean(p_second_highest),
      sd   = sd(p_second_highest),
      se   = sd / sqrt(N))

# 4.3. Probability Estimate for Lowest-rated Box by Condition

ddply(dat, c("choice_tertiary"), summarise,
      N    = length(p_lowest),
      mean = mean(p_lowest),
      sd   = sd(p_lowest),
      se   = sd / sqrt(N))

# 4.4. Probability Estimate for Selected Box by Condition

ddply(dat, c("choice_tertiary"), summarise,
      N    = length(p_selected),
      mean = mean(p_selected),
      sd   = sd(p_selected),
      se   = sd / sqrt(N))