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

# 2.2. Create Single Column for Probability Estimate for Selected Box (P_selected) (Probability-estimates Condition) (OPTIONAL)

dat <- dat %>%
  mutate(p_selected = case_when(probability_estimates == .5 & box == 1 ~ pe1,
                                probability_estimates == .5 & box == 2 ~ pe2,
                                probability_estimates == .5 & box == 3 ~ pe3))

# 2.3. Create Single Column for Variable Indicating Illusion of Control (Illusion) (All conditions) (OPTIONAL)

dat <- dat %>%
  mutate(illusion = case_when(probability_estimates ==  .5 & p_selected  > 34 ~ 1,
                              probability_estimates ==  .5 & p_selected <= 34 ~ 0,
                              probability_estimates == -.5 & mc         >=  1 ~ 1,
                              probability_estimates == -.5 & mc          <  1 ~ 0))

# 2.4. Create Single Columns for Probability Estimates for Highest-, Second-highest-, and Lowest-rated Boxes (P_highest, P_second_highest, and P_lowest) (OPTIONAL)

dat <- dat %>%
  group_by(ResponseId) %>%
  mutate(p_highest = sort(c(pe1, pe2, pe3),decreasing=TRUE)[1],
         p_second_highest = sort(c(pe1, pe2, pe3),decreasing=TRUE)[2],
         p_lowest = sort(c(pe1, pe2, pe3),decreasing=TRUE)[3]) %>%
  ungroup()
  
# 2.5. Exclude Participants As Pre-registered

dat <- dat[dat$excluded==0,]


# 3.   Run Analyses

# 3.1. The Effect of Choice on Probability Estimate for Selected Box (Probability-estimates Conditions)

summary(lm(p_selected ~ choice, data=dat[dat$probability_estimates == .5,]))

# 3.2. The Effect of Choice on Multiple Choice for Selected Box (Multiple-choice Condition)

chisq.test(table(dat$illusion[dat$probability_estimates == -.5],dat$choice[dat$probability_estimates == -.5]))

# 3.3. The Effect of Choice and Outcome Measure on Illusion of Control (All Conditions)

summary(lm(illusion ~ choice*probability_estimates, data=dat))

# 3.4. The Effect of Choice on Illusion of Control (All Conditions)

summary(lm(illusion ~ choice, data=dat))

# 3.5. The Effect of Choice on Illusion of Control (Probability-estimates Condition)

summary(lm(illusion ~ choice, data=dat[dat$probability_estimates== .5,]))

# 3.6. The Effect of Choice on Illusion of Control (Multiple-choice Condition)

summary(lm(illusion ~ choice, data=dat[dat$probability_estimates==-.5,]))

# 3.7. The Effect of Choice on Probability Estimate for Highest-rated Box (Probability-estimates Condition)

summary(lm(p_highest ~ choice, data=dat[dat$probability_estimates== .5,]))


# 4.   Calculate Summary Statistics

# 4.1. Probability Estimate for Selected Box By Choice Condition (Probability-estimates Condition)

ddply(dat[dat$probability_estimates==.5,], c("choice"), summarise,
      N    = length(p_selected),
      mean = mean(p_selected),
      sd   = sd(p_selected),
      se   = sd / sqrt(N))

# 4.2. Illusion by Choice and Outcome Measure Conditions (All Conditions)

ddply(dat, c("probability_estimates", "choice"), summarise,
      N    = length(illusion),
      mean = mean(illusion),
      sd   = sd(illusion),
      se   = sd / sqrt(N))

# 4.3. Illusion by Choice Condition (All Conditions)

ddply(dat, c("choice"), summarise,
      N    = length(illusion),
      mean = mean(illusion),
      sd   = sd(illusion),
      se   = sd / sqrt(N))

# 4.4. Probability Estimate for Highest-rated Box By Choice Condition (Probability-estimates Condition)

ddply(dat[dat$probability_estimates==.5,], c("choice"), summarise,
      N    = length(p_highest),
      mean = mean(p_highest),
      sd   = sd(p_highest),
      se   = sd / sqrt(N))

# 4.5. Effect Size for P_selected (All Conditions)

cohen.d(dat$p_selected[dat$choice==.5 & dat$probability_estimates==.5],
        dat$p_selected[dat$choice==-.5 & dat$probability_estimates==.5])

# 4.6. Effect Size for Illusion (All Conditions)

cohen.d(dat$illusion[dat$choice==.5],
        dat$illusion[dat$choice==-.5])

# 4.7. Effect Size for Illusion (Probability-estimates Conditions)

cohen.d(dat$illusion[dat$choice==.5 & dat$probability_estimates==.5],
        dat$illusion[dat$choice==-.5 & dat$probability_estimates==.5])

# 4.7. Effect Size for Illusion (Multiple-choice Conditions)

cohen.d(dat$illusion[dat$choice==.5 & dat$probability_estimates==-.5],
        dat$illusion[dat$choice==-.5 & dat$probability_estimates==-.5])

# 4.8. Proportion Estimating 33-34% for Their Selected Box

(sum(dat$mc==0, na.rm=TRUE) + sum(dat$p_selected <= 34 & dat$p_selected >= 33, na.rm=TRUE))/sum(dat$excluded==0)

# 4.9. Proportion Estimating Uniform Probabilities

mean(dat$p_highest <= 34 & dat$p_lowest >=33, na.rm=TRUE)

# 4.10. Effect Size for P-highest (Probability-estimates condition)

cohen.d(dat$p_highest[dat$choice== .5 & dat$probability_estimates==.5],
        dat$p_highest[dat$choice==-.5 & dat$probability_estimates==.5])


# 5.   Plot Data

# 5.1. P-selected for Choice Condition

choice15pe <-
  ggplot(dat, aes(x=p_selected))+
  geom_histogram(data=subset(dat,choice==.5&probability_estimates==.5),aes(y=(..count..)/sum(..count..)),
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

ggsave("choice15pe.png", units="in", width=2.5, height=2.5, dpi=600)

# 5.2. P-selected for No-choice Condition

nochoice15pe <-
  ggplot(dat, aes(x=p_selected))+
  geom_histogram(data=subset(dat,choice==-.5&probability_estimates==.5),aes(y=(..count..)/sum(..count..)),
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

ggsave("nochoice15pe.png", units="in", width=2.5, height=2.5, dpi=600)

# 5.3. Multiple-choice Response for Choice Condition

choice15mc <-
  ggplot(dat[dat$choice==.5&dat$probability_estimates==-.5,], aes(x=factor(mc)))+
  geom_bar(aes(y=(..count..)/sum(..count..)), color="black", width=.2, fill="white")+
  geom_text(aes(y=((..count..)/sum(..count..)),
                label=scales::percent((..count..)/sum(..count..),accuracy=.01)),
            stat="count", vjust=-0.5, size=9*0.3528)+ 
  scale_x_discrete(labels=c("Less\nLikely", "Equally\nLikely","More\nLikely"))+
  scale_y_continuous(name="Proportion\nof\nParticipants", labels=scales::percent_format(accuracy=1), limits=c(0,1), breaks=seq(0,1,.1))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=10))+
  geom_hline(yintercept=0)

ggsave("choice15mc.png", units="in", width=2.5, height=2.5, dpi=600)

# 5.4. Multiple-choice Response for No-choice Condition

nochoice15mc <-
  ggplot(dat[dat$choice==-.5&dat$probability_estimates==-.5,], aes(x=factor(mc)))+
  geom_bar(aes(y=(..count..)/sum(..count..)), color="black", width=.2, fill="white")+
  geom_text(aes(y=((..count..)/sum(..count..)),
                label=scales::percent((..count..)/sum(..count..),accuracy=.01)),
            stat="count", vjust=-0.5, size=9*0.3528)+ 
  scale_x_discrete(labels=c("Less\nLikely", "Equally\nLikely","More\nLikely"))+
  scale_y_continuous(name="Proportion\nof\nParticipants", labels=scales::percent_format(accuracy=1), limits=c(0,1), breaks=seq(0,1,.1))+
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=10))+
  geom_hline(yintercept=0)

ggsave("nochoice15mc.png", units="in", width=2.5, height=2.5, dpi=600)