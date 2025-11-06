############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-05-2020                                  #
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

# 2.2. Create Single Column for Variable Wager (OPTIONAL)

dat <- dat %>%
  mutate(wager = case_when(choice == 1 & uncertainty == "risky" ~ c_cr_wager,
                           choice == 0 & uncertainty == "risky" ~ nc_cr_wager,
                           choice == 1 & uncertainty == "ambiguous" ~ c_a_wager,
                           choice == 0 & uncertainty == "ambiguous" ~ nc_a_wager))

# 2.3. Create Single Column for Variable Confidence (OPTIONAL)

dat <- dat %>%
  mutate(confidence = case_when(choice == 1 & uncertainty == "risky" ~ (c_cr_confidence1+c_cr_confidence2)/2,
                                choice == 0 & uncertainty == "risky" ~ (nc_cr_confidence1+nc_cr_confidence2)/2,
                                choice == 1 & uncertainty == "ambiguous" ~ (c_a_confidence1+c_a_confidence2)/2,
                                choice == 0 & uncertainty == "ambiguous" ~ (nc_a_confidence1+nc_a_confidence2)/2))

# 2.4. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Wager (Study 5; Compound-risk)

t.test(wager~choice, data=dat[dat$uncertainty == "risky",])

# 3.2. The Effect of Choice on Confidence (Study 5; Compound-risk)

t.test(confidence~choice, data=dat[dat$uncertainty == "risky",])

# 3.3. The Effect of Choice on Wager (Study 6; Ambiguity)

t.test(wager~choice, data=dat[dat$uncertainty == "ambiguous",])

# 3.4. The Effect of Choice on Confidence (Study 6; Ambiguity)

t.test(confidence~choice, dat[dat$uncertainty == "ambiguous",])


# 4.   Calculate Summary Statistics

# 4.1. Wager By Choice Condition (Study 5; Compound-risk)

ddply(dat[dat$uncertainty == "risky",], c("choice"), summarise,
      N    = length(wager),
      mean = mean(wager),
      sd   = sd(wager),
      se   = sd / sqrt(N))

# 4.2. Confidence by Choice Condition (Study 5; Compound-risk)

ddply(dat[dat$uncertainty == "risky",], c("choice"), summarise,
      N    = length(confidence),
      mean = mean(confidence),
      sd   = sd(confidence),
      se   = sd / sqrt(N))

# 4.3. Wager By Choice Condition (Study 6; Ambiguity)

ddply(dat[dat$uncertainty == "ambiguous",], c("choice"), summarise,
      N    = length(wager),
      mean = mean(wager),
      sd   = sd(wager),
      se   = sd / sqrt(N))

# 4.4. Confidence by Choice Condition (Study 6; Ambiguity)

ddply(dat[dat$uncertainty == "ambiguous",], c("choice"), summarise,
      N    = length(confidence),
      mean = mean(confidence),
      sd   = sd(confidence),
      se   = sd / sqrt(N))

# 4.5. Effect Size for Wager (Study 5; Compound-risk)

cohen.d(dat$wager[dat$choice==1 & dat$uncertainty=="risky"],
        dat$wager[dat$choice==0 & dat$uncertainty=="risky"])

# 4.6. Effect Size for Confidence (Study 5; Compound-risk)

cohen.d(dat$confidence[dat$choice==1 & dat$uncertainty=="risky"],
        dat$confidence[dat$choice==0 & dat$uncertainty=="risky"])

# 4.7. Effect Size for Wager (Study 6; Ambiguity)

cohen.d(dat$wager[dat$choice==1 & dat$uncertainty=="ambiguous"],
        dat$wager[dat$choice==0 & dat$uncertainty=="ambiguous"])

# 4.8. Effect Size for Confidence (Study 6; Ambiguity)

cohen.d(dat$confidence[dat$choice==1 & dat$uncertainty=="ambiguous"],
        dat$confidence[dat$choice==0 & dat$uncertainty=="ambiguous"])