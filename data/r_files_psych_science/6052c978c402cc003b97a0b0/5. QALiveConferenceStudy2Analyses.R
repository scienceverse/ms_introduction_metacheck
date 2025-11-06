
###############################################################################################
####                                                                                       ####
####                              PART 1: Data Cleaning                                    ####
####                                                                                       ####
###############################################################################################

# Set Up

library(plyr)
library(psych)
library(compute.es)
library(Hmisc)
library(stringr)

# Load Data

setwd("C:/Users/[user]/Downloads") 
lcon2 <- read.csv("3. LiveConference2Q&A.csv") 
sem <- read.csv("Participation_in_seminars_Qualtrics.csv") #replication survey data
semholdback <- read.csv("holdback_seminar.csv") #RA coding for why participants hold back questions

# Clean Gender Variable

lcon2$Gender <- ifelse(lcon2$ResponseId=="R_23gkTLJejvmfjce", 2, lcon2$Gender) # Participant identifies as a woman but selected other to give us feedback on our gender question

# Specific Conference Comfort Composite

lcon2$QACcomp3.spec <- rowMeans(cbind(lcon2$QACquestion.spec, lcon2$QACcomment.spec, lcon2$QACspeaker.spec), na.rm = T)

# General Conference Comfort Composite

lcon2$QACcomp3.gen <- rowMeans(cbind(lcon2$QACquestion.gen, lcon2$QACcomment.gen, lcon2$QACspeaker.gen), na.rm = T)

# Merge Replication Survey and Qual Coded Data

sem$ResponseId <- str_trim(sem$ResponseId)
sem <- merge(sem, semholdback, by = "ResponseId", all = T)

# Effect Code Gender

sem$Gender_ec <- ifelse(sem$Gender_MF_01 == 0, -0.5,
                        ifelse(sem$Gender_MF_01 == 1, 0.5, NA))

# Match Binary Replication Gender Variable to Survey Coding

sem$Gender <- ifelse(sem$Gender_MF_01 == 0, 1,
                     ifelse(sem$Gender_MF_01 == 1, 2, NA))

# Discomfort Composite 

sem$anxietycomp5 <- rowMeans(cbind(sem$Q5_2, sem$Q5_3, sem$Q5_4, sem$Q5_6, sem$Q5_7), na.rm = T)

###############################################################################################
####                                                                                       ####
####                             PART 2: Demographics & Alphas                             ####
####                                                                                       ####
###############################################################################################

# Survey Demographics

count(lcon2$Gender)
count(lcon2$Race)
count(lcon2$Rank)
count(lcon2$Rank_10_TEXT) 
    # 1 Associate Dean (left as other)
    # 1 Associate Director of an Academic Research Center (left as other)
    # 1 ABD grad student full time at non-profit (left as other)
    # 2 Research assistant/staff (left as other)

# Compare Recruited Demographics to Conference Demographics

## Race
survey_race <- c(0, 30, 5, 20, 7, 0, 136, 19, 4, 11)
conference_race <- c(25, 632, 197, 261, 79, 3, 2555, 204, 82, 170)
ch.2 <- chisq.test(survey_race, p=conference_race, rescale.p=T)
ch.2
chies(ch.2$statistic, sum(survey_race))

## Gender
survey_gender <- c(65, 161, 0, 3, 1, 3) #moved 1 from other (5) to woman (2)
conference_gender <- c(1462, 2580, 10, 50, 8, 97) #1 less because someone selected cis-male & cis-female simultaneously
ch.1 <- chisq.test(survey_gender, p=conference_gender, rescale.p=T)
ch.1
chies(ch.1$statistic, sum(survey_gender))

## Rank/Position
survey_rank <- c(6+120, 22+39, 17+18, 4+1+2+5)
conference_rank <- c(2214, 378, 977, 245)
ch.3 <- chisq.test(survey_rank, p=conference_rank, rescale.p=T)
ch.3
chies(ch.3$statistic, sum(survey_rank))

## Attendee Gender by Rank
confmen <- c(45, 38, 169, 510, 526, 114)
confwomen <- c(75, 53, 257, 596, 1061, 415)
ch.4 <- chisq.test(confmen, p=confwomen, rescale.p=T)
chies(ch.4$statistic, sum(conference_rank))

# Exclude Participants Outside of the Gender Binary 

lcon2 <- lcon2[which(lcon2$Gender < 3),]
sem <- sem[which(sem$Gender < 3),]

# Correlations for Comfort Participating Composites

rcorr(as.matrix(cbind(lcon2$QACquestion.spec, lcon2$QACcomment.spec, 
                      lcon2$QACspeaker.spec)))

rcorr(as.matrix(cbind(lcon2$QACquestion.gen, lcon2$QACcomment.gen,
                      lcon2$QACspeaker.gen)))

# Alphas for Comfort Participating Composites

psych::alpha(cbind.data.frame(lcon2$QACquestion.spec, lcon2$QACcomment.spec, lcon2$QACspeaker.spec))$total$raw_alpha
psych::alpha(cbind.data.frame(lcon2$QACquestion.gen, lcon2$QACcomment.gen, lcon2$QACspeaker.gen))$total$raw_alpha

# Gender Breakdown for Hold Back Open Responses

count(lcon2[which(is.na(lcon2$QUALspace)==F),]$Gender)

# Average Length of Hold Back Responses

lcon2$holdbackcount <- str_count(lcon2[which(is.na(lcon2$QACholdback_open)==F),]$QACholdback_open, "\\w+")
mean(lcon2[which(lcon2$QACholdback_open>0),]$holdbackcount)

# Number of Reasons for 4 Confirmatory Codes per Response

lcon2$reasontotal <- rowSums(data.frame(lcon2$QUALspace, lcon2$QUALlogistics, lcon2$QUALanxiety,
                                        lcon2$QUALoneonone))
mean(lcon2[which(lcon2$QACholdback_open!=""),]$reasontotal, na.rm = T)

# Alpha for Replication Discomfort Composite

psych::alpha(cbind.data.frame(sem$Q5_2, sem$Q5_3, sem$Q5_4, sem$Q5_6, sem$Q5_7))$total$raw_alpha

###############################################################################################
####                                                                                       ####
####                        PART 3: Descriptives and Correlations                          ####
####                                                                                       ####
###############################################################################################

# Self-Report Outcomes

## Descriptives

describeBy(lcon2$QACcomp3.spec, group = lcon2$Gender)
describeBy(lcon2$QACcomp3.gen, group = lcon2$Gender)
describeBy(lcon2$QACquestion.spec, group = lcon2$Gender)
describeBy(lcon2$QACquestion.gen, group = lcon2$Gender)
describeBy(lcon2$QACcomment.spec, group = lcon2$Gender)
describeBy(lcon2$QACcomment.gen, group = lcon2$Gender)
describeBy(lcon2$QACspeaker.spec, group = lcon2$Gender)
describeBy(lcon2$QACspeaker.gen, group = lcon2$Gender)
describeBy(lcon2$QACbacklash.spec, group = lcon2$Gender)
describeBy(lcon2$QACbacklash.gen, group = lcon2$Gender)
describeBy(lcon2$QACholdback, group = lcon2$Gender)

## Correlations

rcorr(as.matrix(cbind(lcon2$QACquestion.spec, lcon2$QACquestion.gen,
                      lcon2$QACcomment.spec, lcon2$QACcomment.gen,
                      lcon2$QACspeaker.spec, lcon2$QACspeaker.gen,
                      lcon2$QACbacklash.spec, lcon2$QACbacklash.gen, 
                      lcon2$QACholdback)))

# Hold Back Qualitative Outcomes

## Descriptives

describeBy(lcon2$QUALanxiety, group = lcon2$Gender)
describeBy(lcon2$QUALspace, group = lcon2$Gender)
describeBy(lcon2$QUALlogistics, group = lcon2$Gender)
describeBy(lcon2$QUALoneonone, group = lcon2$Gender)
describeBy(lcon2$QUALbacklash, group = lcon2$Gender)
describeBy(lcon2$QUALaudience, group = lcon2$Gender)
describeBy(lcon2$QUALrace, group = lcon2$Gender)
describeBy(lcon2$QUALinternational, group = lcon2$Gender)
describeBy(lcon2$QUALgender, group = lcon2$Gender)
describeBy(lcon2$QUALrank, group = lcon2$Gender)
describeBy(lcon2$QUALirrelevance, group = lcon2$Gender)
describeBy(lcon2$QUALnochallenge, group = lcon2$Gender)
describeBy(lcon2$QUALidentity, group = lcon2$Gender)

## Correlations

rcorr(as.matrix(cbind(lcon2$QUALanxiety, lcon2$QUALspace, lcon2$QUALlogistics,
                      lcon2$QUALoneonone, lcon2$QUALbacklash, lcon2$QUALaudience,
                      lcon2$QUALrace, lcon2$QUALinternational, lcon2$QUALgender,
                      lcon2$QUALrank, lcon2$QUALirrelevance, lcon2$QUALnochallenge,
                      lcon2$QUALidentity)))

# Hold Back Conceptual Replication Analyses

## Descriptives

describeBy(sem$anxietycomp5, group = sem$Gender)
describeBy(sem$space, group = sem$Gender)
describeBy(sem$anxiety, group = sem$Gender)
describeBy(sem$logistics, group = sem$Gender)
describeBy(sem$oneonone, group = sem$Gender)

## Correlations

rcorr(as.matrix(cbind(sem$anxietycomp5, sem$anxiety, sem$space, 
                      sem$logistics, sem$oneonone)))

###############################################################################################
####                                                                                       ####
####                            PART 4: Self-Report Analyses                               ####
####                                                                                       ####
###############################################################################################

# How comfortable participating at specific?

## Analysis

t.1 <- t.test(QACcomp3.spec ~ Gender, data = lcon2)
t.1

## Effect Size

g.1 <- count(lcon2[which(is.na(lcon2$QACcomp3.spec)==F),]$Gender)
tes(t.1$statistic, g.1[1,2], g.1[2,2])

# How comfortable general?

## Analysis

t.2 <- t.test(QACcomp3.gen ~ Gender, data = lcon2)
t.2

## Effect Size

g.2 <- count(lcon2[which(is.na(lcon2$QACcomp3.gen)==F),]$Gender)
tes(t.2$statistic, g.2[1,2], g.2[2,2])

# How comfortable were you asking questions during the question and answer sessions at [conference]?

## Analysis

t.3 <- t.test(QACquestion.spec ~ Gender, data = lcon2)
t.3

## Effect Size

g.3 <- count(lcon2[which(is.na(lcon2$QACquestion.spec)==F),]$Gender)
tes(t.3$statistic, g.3[1,2], g.3[2,2])

# How comfortable are you asking questions during the question and answer sessions at conferences generally?

## Analysis

t.4 <- t.test(QACquestion.gen ~ Gender, data = lcon2)
t.4

## Effect Size

g.4 <- count(lcon2[which(is.na(lcon2$QACquestion.gen)==F),]$Gender)
tes(t.4$statistic, g.4[1,2], g.4[2,2])

# How comfortable were you sharing comments or opinions during the question and answer sessions at [conference]?

## Analysis

t.5 <- t.test(QACcomment.spec ~ Gender, data = lcon2)
t.5

## Effect Size

g.5 <- count(lcon2[which(is.na(lcon2$QACcomment.spec)==F),]$Gender)
tes(t.5$statistic, g.5[1,2], g.5[2,2])

# How comfortable are you sharing comments or opinions during the question and answer sessions at conferences generally?

## Analysis

t.6 <- t.test(QACcomment.gen ~ Gender, data = lcon2)
t.6

## Effect Size

g.6 <- count(lcon2[which(is.na(lcon2$QACcomment.gen)==F),]$Gender)
tes(t.6$statistic, g.6[1,2], g.6[2,2])

# How comfortable were you approaching speakers outside question and answer sessions at [conference] (e.g., during coffee breaks or lunch)?

## Analysis

t.7 <- t.test(QACspeaker.spec ~ Gender, data = lcon2)
t.7

## Effect Size

g.7 <- count(lcon2[which(is.na(lcon2$QACspeaker.spec)==F),]$Gender)
tes(t.7$statistic, g.7[1,2], g.7[2,2])

# How comfortable are you approaching speakers outside question and answer sessions at conferences generally (e.g., during coffee breaks or lunch)?

## Analysis

t.8 <- t.test(QACspeaker.gen ~ Gender, data = lcon2)
t.8

## Effect Size

g.8 <- count(lcon2[which(is.na(lcon2$QACspeaker.gen)==F),]$Gender)
tes(t.8$statistic, g.8[1,2], g.8[2,2])

# How often were you afraid of receiving backlash (from either the speaker or other audience members) after asking a question during question and answer sessions at [conference]?

## Analysis

t.9 <- t.test(QACbacklash.spec ~ Gender, data = lcon2)
t.9

## Effect Size

g.9 <- count(lcon2[which(is.na(lcon2$QACbacklash.spec)==F),]$Gender)
tes(t.9$statistic, g.9[1,2], g.9[2,2])

# How often are you afraid of receiving backlash after asking a question during question and answer sessions at conferences generally?

## Analysis

t.10 <- t.test(QACbacklash.gen ~ Gender, data = lcon2)
t.10

## Effect Size

g.10 <- count(lcon2[which(is.na(lcon2$QACbacklash.gen)==F),]$Gender)
tes(t.10$statistic, g.10[1,2], g.10[2,2])

# How often do you have questions but hold back asking them during question and answer sessions?

## Analysis

t.11 <- t.test(QACholdback ~ Gender, data = lcon2)
t.11

## Effect Size

g.11 <- count(lcon2[which(is.na(lcon2$QACholdback)==F),]$Gender)
tes(t.11$statistic, g.11[1,2], g.11[2,2])

###############################################################################################
####                                                                                       ####
####                       PART 5: Hold Back Qualitative Analyses                          ####
####                                                                                       ####
###############################################################################################

# Anxiety

m.1 <- glm(QUALanxiety ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.1)
confint(m.1)

# Space 

m.2 <- glm(QUALspace ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.2)
confint(m.2)

# Logistics 

m.3 <- glm(QUALlogistics ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.3)
confint(m.3)

# One on One 

m.4 <- glm(QUALoneonone ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.4)
confint(m.4)

###############################################################################################
####                                                                                       ####
####                  PART 6: Hold Back Conceptual Replication Analyses                    ####
####                                                                                       ####
###############################################################################################

# Self-Report Discomfort

t.1 <- t.test (anxietycomp5 ~ Gender, data = sem)
t.1
g.1 <- count(sem$Gender)
tes(t.1$statistic, g.1[1,2], g.1[2,2])

# Space 

m.2 <- glm(space ~ Gender_ec, family = "binomial", data = sem)
summary(m.2)
confint(m.2)

# Anxiety

m.3 <- glm(anxiety ~ Gender_ec, family = "binomial", data = sem)
summary(m.3)
confint(m.3)

# Logistics

m.4 <- glm(logistics ~ Gender_ec, family = "binomial", data = sem)
summary(m.4)
confint(m.4)

# One on One

m.5 <- glm(oneonone ~ Gender_ec, family = "binomial", data = sem)
summary(m.5)
confint(m.5)

###############################################################################################
####                                                                                       ####
####                         PART 7: Supplemental Data Analysis                            ####
####                                                                                       ####
###############################################################################################

# Backlash 

m.1 <- glm(QUALbacklash ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.1)
confint(m.1)

# Audience 

m.2 <- glm(QUALaudience ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.2)
confint(m.2)

# Identity (low N)

m.3 <- glm(QUALidentity ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.3)
confint(m.3)

# Rank

m.4 <- glm(QUALrank ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.4)
confint(m.4)

# Irrelevance 

m.5 <- glm(QUALirrelevance ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.5)
confint(m.5)

# No Challenge (low N)

m.6 <- glm(QUALnochallenge ~ Gender_ec, family = "binomial", data = lcon2)
summary(m.6)
confint(m.6)


