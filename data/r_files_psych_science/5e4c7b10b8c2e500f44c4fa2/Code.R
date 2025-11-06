############################################################
#                                                          #
# Does Choice Cause an Illusion of Control?                #
# R Code to Reproduce the Analyses                         #
# Author: Joowon Klusowski                                 #
# Last Update: 05-13-2020                                  #
#                                                          #
############################################################

# 1.   Install and Load packages

install.packages("plyr")
install.packages("dplyr")
install.packages("effsize")
install.packages("ggplot2")
install.packages("scales")

library("plyr")
library("dplyr")
library("effsize")
library("ggplot2")
library("scales")


# 2.   Prepare Data

# 2.1. Load Data

setwd("location to the folder that contains the data")
dat <- read.csv("Data.csv")

# 2.2. Create Single Column for Probability Estimate for Selected Box (P_selected) (OPTIONAL)

dat <- dat %>%
  mutate(p_selected = case_when(one_option ==  .5            ~ o_prob,
                                one_option == -.5 & box == 1 ~ a_prob1,
                                one_option == -.5 & box == 2 ~ a_prob2,
                                one_option == -.5 & box == 3 ~ a_prob3))

# 2.3. Create Single Columns for Probability Estimates for Highest-, Second-highest-, and Lowest-rated Boxes (P_highest, P_second_highest, and P_lowest) (OPTIONAL)

dat <- dat %>%
  group_by(ResponseId) %>%
  mutate(p_highest = sort(c(a_prob1, a_prob2, a_prob3),decreasing=TRUE)[1],
         p_second_highest = sort(c(a_prob1, a_prob2, a_prob3),decreasing=TRUE)[2],
         p_lowest = sort(c(a_prob1, a_prob2, a_prob3),decreasing=TRUE)[3]) %>%
  ungroup()
  
# 2.4. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice and Number of Options on Probability Estimate for Selected Box (All Conditions)

summary(lm(p_selected ~ choice*one_option, data=dat))

# 3.2. The Effect of Choice and Number of Options on Probability Estimate for Selected Box (One-option Condition)

summary(lm(p_selected ~ choice, data=dat[dat$one_option== .5,]))

# 3.3. The Effect of Choice and Number of Options on Probability Estimate for Selected Box (All-options Condition)

summary(lm(p_selected ~ choice, data=dat[dat$one_option==-.5,]))

# 3.4. The Effect of Choice on Probability Estimate for Selected Box (All Conditions)

summary(lm(p_selected ~ choice, data=dat))

# 3.5. The Effect of Choice on Probability Estimate for Highest-rated Box (All-options Condition)

summary(lm(p_highest ~ choice, data=dat[dat$one_option==-.5,]))


# 4.   Calculate Summary Statistics

# 4.1. Probability Estimate for Selected Box by Choice and Number of Options Conditions

ddply(dat, c("one_option", "choice"), summarise,
      N    = length(p_selected),
      mean = mean(p_selected),
      sd   = sd(p_selected),
      se   = sd / sqrt(N))

# 4.2. Probability Estimate for Selected Box By Choice Condition (All Conditions)

ddply(dat, c("choice"), summarise,
      N    = length(p_selected),
      mean = mean(p_selected),
      sd   = sd(p_selected),
      se   = sd / sqrt(N))

# 4.3. Probability Estimate for Highest-rated Box By Choice Condition (All-options Condition)

ddply(dat[dat$one_option==-.5,], c("choice"), summarise,
      N    = length(p_highest),
      mean = mean(p_highest),
      sd   = sd(p_highest),
      se   = sd / sqrt(N))

# 4.4. Effect Size for P-selected in All Conditions

cohen.d(dat$p_selected[dat$choice==.5], dat$p_selected[dat$choice==-.5])

# 4.5. Effect Size for P-selected in One-option Condition

cohen.d(dat$p_selected[dat$choice==.5 & dat$one_option==.5], dat$p_selected[dat$choice==-.5 & dat$one_option==.5])

# 4.6. Effect Size for P-selected in All-options Condition

cohen.d(dat$p_selected[dat$choice==.5 & dat$one_option==-.5], dat$p_selected[dat$choice==-.5 & dat$one_option==-.5])

# 4.7. Proportion Estimating 33-34% for Their Selected Box

mean(dat$p_selected <= 34 & dat$p_selected >= 33)

# 4.8. Proportion Estimating Uniform Probabilities

mean(dat$p_highest<=34 & dat$p_lowest>=33, na.rm=TRUE)

# 4.9. Effect Size for P-highest in All-options Condition

cohen.d(dat$p_highest[dat$choice== .5 & dat$one_option==-.5],
        dat$p_highest[dat$choice==-.5 & dat$one_option==-.5])


# 5.   Plot Data

# 5.1. P-selected for Choice Condition

choice13 <-
  ggplot(dat, aes(x=p_selected))+
  geom_histogram(data=subset(dat,choice==.5),aes(y=(..count..)/sum(..count..)),
                 color="black",fill="white",binwidth=1) +
  scale_x_continuous(name="Probability Estimate\nfor Selected Box", labels = function(x) paste0(x, "%"), breaks=c(0,33,100))+
  scale_y_continuous(name="Proportion\nof\nParticipants", labels = scales::percent_format(accuracy=1), limits=c(0,1), breaks=seq(0,1,.1))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_text(size=10))

ggsave("choice13.png", units="in", width=2.5, height=2.5, dpi=600)

# 5.2. P-selected for No-choice Condition

nochoice13 <-
  ggplot(dat, aes(x=p_selected))+
  geom_histogram(data=subset(dat,choice==-.5),aes(y=(..count..)/sum(..count..)),
                 color="black",fill="white",binwidth=1) +
  scale_x_continuous(name="Probability Estimate\nfor Selected Box", labels = function(x) paste0(x, "%"), breaks=c(0,33,100))+
  scale_y_continuous(name="Proportion\nof\nParticipants", labels = scales::percent_format(accuracy=1), limits=c(0,1), breaks=seq(0,1,.1))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text=element_text(size=10))

ggsave("nochoice13.png", units="in", width=2.5, height=2.5, dpi=600)