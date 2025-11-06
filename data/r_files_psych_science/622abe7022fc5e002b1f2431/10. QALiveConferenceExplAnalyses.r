###############################################################################################
####                                                                                       ####
####                              PART 1: Data Cleaning                                    ####
####                                                                                       ####
###############################################################################################

# Set Up 

library(compute.es)
library(Hmisc)

# Load Data

setwd("C:/Users/[user]/Downloads") 
lconEX <- read.csv("06. LiveConferenceExplQ&A.csv") 
lconEX <- lconEX[which(lconEX$Gender!="N"),]

###############################################################################################
####                                                                                       ####
####                        PART 2: Descriptives and Correlations                          ####
####                                                                                       ####
###############################################################################################

# Self-Report Outcomes

## Descriptives

describeBy(lconEX$QACquestion.spec, group = lconEX$Gender)
describeBy(lconEX$QACquestion.gen, group = lconEX$Gender)
describeBy(lconEX$QACcomment.spec, group = lconEX$Gender)
describeBy(lconEX$QACcomment.gen, group = lconEX$Gender)
describeBy(lconEX$QACspeaker.spec, group = lconEX$Gender)
describeBy(lconEX$QACspeaker.gen, group = lconEX$Gender)
describeBy(lconEX$QACholdback, group = lconEX$Gender)

## Correlations

rcorr(as.matrix(cbind(lconEX$QACquestion.spec, lconEX$QACquestion.gen,
                      lconEX$QACcomment.spec, lconEX$QACcomment.gen,
                      lconEX$QACspeaker.spec, lconEX$QACspeaker.gen,
                      lconEX$QACholdback)))

# Hold Back Qualitative Outcomes

## Descriptives

describeBy(lconEX$QUALanxiety, group = lconEX$Gender)
describeBy(lconEX$QUALspace, group = lconEX$Gender)
describeBy(lconEX$QUALlogistics, group = lconEX$Gender)
describeBy(lconEX$QUALoneonone, group = lconEX$Gender)

## Correlations

rcorr(as.matrix(cbind(lconEX$QUALanxiety, lconEX$QUALspace, 
                      lconEX$QUALlogistics, lconEX$QUALoneonone)))

###############################################################################################
####                                                                                       ####
####                            PART 3: Self-Report Analyses                               ####
####                                                                                       ####
###############################################################################################

# How comfortable were you asking questions during the question and answer sessions at [conference]?

## Analysis

t.1 <- t.test(QACquestion.spec ~ Gender, data = lconEX)
t.1

## Effect Size

gencount <- count(lconEX[which(is.na(lconEX$QACquestion.gen)==F),]$Gender)
tes(t.1$statistic, gencount[1,2], gencount[2,2])

# How comfortable are you asking questions during the question and answer sessions at conferences generally?

## Analysis

t.2 <- t.test(QACquestion.gen ~ Gender, data = lconEX)
t.2

## Effect Size

tes(t.2$statistic, gencount[1,2], gencount[2,2])

# How comfortable were you sharing comments or opinions during the question and answer sessions at [conference]?

## Analysis

t.3 <- t.test(QACcomment.spec ~ Gender, data = lconEX)
t.3

## Effect Size

tes(t.3$statistic, gencount[1,2], gencount[2,2])

# How comfortable are you sharing comments or opinions during the question and answer sessions at conferences generally?

## Analysis

t.4 <- t.test(QACcomment.gen ~ Gender, data = lconEX)
t.4

## Effect Size

tes(t.4$statistic, gencount[1,2], gencount[2,2])

# How comfortable were you approaching speakers outside question and answer sessions at [conference] (e.g., during coffee breaks or lunch)?

## Analysis

t.5 <- t.test(QACspeaker.spec ~ Gender, data = lconEX)
t.5

## Effect Size

tes(t.5$statistic, gencount[1,2], gencount[2,2])

# How comfortable are you approaching speakers outside question and answer sessions at conferences generally (e.g., during coffee breaks or lunch)?

## Analysis

t.6 <- t.test(QACspeaker.gen ~ Gender, data = lconEX)
t.6

## Effect Size

tes(t.6$statistic, gencount[1,2], gencount[2,2])

# How often do you have questions but hold back asking them during question and answer sessions?

## Analysis

t.7 <- t.test(QACholdback ~ Gender, data = lconEX)
t.7

## Effect Size

tes(t.7$statistic, gencount[1,2], gencount[2,2])

###############################################################################################
####                                                                                       ####
####                       PART 3: Hold Back Qualitative Analyses                          ####
####                                                                                       ####
###############################################################################################

# Anxiety 

m.1 <- glm(QUALanxiety ~ Gender_ec, family = "binomial", data = lconEX)
summary(m.1)
confint(m.1)

# Space 

m.2 <- glm(QUALspace ~ Gender_ec, family = "binomial", data = lconEX)
summary(m.2)
confint(m.2)

# Logistics 

m.3 <- glm(QUALlogistics ~ Gender_ec, family = "binomial", data = lconEX)
summary(m.3)
confint(m.3)

# One on One 

m.4 <- glm(QUALoneonone ~ Gender_ec, family = "binomial", data = lconEX)
summary(m.4)
confint(m.4)
