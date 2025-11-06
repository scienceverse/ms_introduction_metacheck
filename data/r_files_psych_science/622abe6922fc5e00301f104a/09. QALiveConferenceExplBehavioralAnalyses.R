###############################################################################################
####                                                                                       ####
####                              PART 1: Data Cleaning                                    ####
####                                                                                       ####
###############################################################################################

# Set Up 

library(compute.es)
library(MASS)
library(pscl)
library(plyr)
library(doBy)
library(Hmisc)

# Load Data

setwd("C:/Users/[user]/Downloads") 
lconEX <- read.csv("05. LiveConferenceExplQ&ABehavioral.csv") 

# Data Cleaning

lconEX$gender.asker_ec <- ifelse(lconEX$gender.asker==1, -.5,
                                 ifelse(lconEX$gender.asker==2, .5, NA))

lconEX$gender.speaker_ec <- ifelse(lconEX$gender.speaker==1, -.5,
                                   ifelse(lconEX$gender.speaker==2, .5, NA))

lconEX$time.asker.total_clean <- ifelse(lconEX$time.asker.total == 6.5, 7, #time was calculated as the average of two coders. 
                                        ifelse(lconEX$time.asker.total == 23.5, 24, #For averages that ended in decimals, 
                                        ifelse(lconEX$time.asker.total == 25.5, 26, #they were rounded to the nearest whole number to be used in 
                                        ifelse(lconEX$time.asker.total == 40.5, 41, #models with negative binomial distributions.
                                        ifelse(lconEX$time.asker.total == 47.5, 48,
                                        ifelse(lconEX$time.asker.total == 50.5, 51,
                                        ifelse(lconEX$time.asker.total == 54.5, 55,
                                        ifelse(lconEX$time.asker.total == 67.5, 68,
                                        ifelse(lconEX$time.asker.total == 69.5, 70,
                                        ifelse(lconEX$time.asker.total == 77.5, 78,
                                        ifelse(lconEX$time.asker.total == 78.5, 79,
                                        ifelse(lconEX$time.asker.total == 94.5, 95,
                                        ifelse(lconEX$time.asker.total == 134.5, 135, lconEX$time.asker.total)))))))))))))

lconEX$time.intro_clean <- ifelse(lconEX$time.intro == 2.5, 3,
                                   ifelse(lconEX$time.intro == 4.5, 4, lconEX$time.intro))

lconEX$time.question_clean <- ifelse(lconEX$time.question == 6.5, 7,
                                     ifelse(lconEX$time.question == 16.5, 17,
                                     ifelse(lconEX$time.question == 22.5, 23,
                                     ifelse(lconEX$time.question == 23.5, 24,
                                     ifelse(lconEX$time.question == 42.5, 43,
                                     ifelse(lconEX$time.question == 50.5, 51,
                                     ifelse(lconEX$time.question == 54.5, 55,
                                     ifelse(lconEX$time.question == 67.5, 68,
                                     ifelse(lconEX$time.question == 78.5, 79,
                                     ifelse(lconEX$time.question == 94.5, 95,
                                     ifelse(lconEX$time.question == 134.5, 135, lconEX$time.question)))))))))))

lconEX$SumInitialRemarks <- rowSums(data.frame(lconEX$IQCquestion, lconEX$IQCcomment, lconEX$IQCcomquest, lconEX$IQCquestcom), na.rm = T)
lconEX$SumInitialRemarks[is.na(lconEX$questionID)] <- NA

lconEX$SumFollowUp <- rowSums(data.frame(lconEX$FQCquestion, lconEX$FQCcomment, lconEX$FQCcomquest), na.rm = T)
lconEX$SumFollowUp[is.na(lconEX$questionID)] <- NA

lconEX$SumRemarks <- rowSums(data.frame(lconEX$IQCquestion, lconEX$IQCcomment, lconEX$IQCcomquest, 
                                        lconEX$IQCquestcom, lconEX$FQCquestion, lconEX$FQCcomment,
                                        lconEX$FQCcomquest), na.rm = T)
lconEX$SumRemarks[is.na(lconEX$questionID)] <- NA

lconEX$SumFollowUp <- rowSums(data.frame(lconEX$FQCquestion, lconEX$FQCcomment, lconEX$FQCcomquest), na.rm = T)
lconEX$SumFollowUp[is.na(lconEX$questionID)] <- NA

lconEX$NoQuestions <- ifelse(lconEX$IQCquestion == 0, 1, 0)
lconEX$NoQuestions[is.na(lconEX$questionID)] <- NA

lconEX$CommentOnly <- ifelse(lconEX$IQCcomment>0 & lconEX$IQCquestion==0 & lconEX$IQCcomquest==0 & lconEX$IQCquestcom==0, 1, 0)
lconEX$CommentOnly[is.na(lconEX$questionID)] <- NA

lconEX$Speak.Listen.Ratio <- lconEX$time.asker.total - lconEX$time.response

lconEX$ThankYouEver <- ifelse(lconEX$thankyou.talk==1 | lconEX$thankyou.response ==1, 1, 0)
lconEX$ThankYouEver[is.na(lconEX$questionID)] <- NA

lconEX$Nice <- ifelse(lconEX$thankyou.talk==1 | lconEX$thankyou.response ==1 | lconEX$pos.comment==1, 1, 0)
lconEX$Nice[is.na(lconEX$questionID)] <- NA

lconEX$SumNice <- rowSums(data.frame(lconEX$thankyou.talk, lconEX$thankyou.response, lconEX$pos.comment), na.rm = T)
lconEX$SumNice[is.na(lconEX$questionID)] <- NA

lconEX$pos.research <- 0
lconEX$pos.research[lconEX$research.valence==1] <- 1 
lconEX$pos.research[is.na(lconEX$questionID)] <- NA

lconEX$retain <- ifelse(lconEX$pass.retain==1, 1,0)
lconEX$retain[is.na(lconEX$questionID)] <- NA

lconEX$pass <- ifelse(lconEX$pass.retain==-1, 1,0)
lconEX$pass[is.na(lconEX$questionID)] <- NA

lconEX_q <-lconEX[-which(duplicated(lconEX$questionID)),] #subsetting data so that questions answered by multiple speakers aren't doubly weighted

lconEX_q$ZNoQuestions <- scale(lconEX_q$NoQuestions)
lconEX_q$Ztalk.over.speaker <- scale(lconEX_q$talk.over.speaker)
lconEX_q$Zpos.research <- scale(lconEX_q$pos.research)
lconEX_q$Zchallenge <- scale(lconEX_q$challenge)
lconEX_q$Ztime.intro <- scale(lconEX_q$time.intro)
lconEX_q$Ztime.question <- scale(lconEX_q$time.question)

lconEX_q$Peacocking <- rowMeans(cbind(lconEX_q$ZNoQuestions, lconEX_q$Ztalk.over.speaker, lconEX_q$Zpos.research,  
                                              lconEX_q$Zchallenge, lconEX_q$Ztime.intro, lconEX_q$Ztime.question), na.rm = T)

keeps <- c("questionID", "Peacocking")

lconEX_qpeacock <- lconEX_q[, (names(lconEX_q) %in% keeps)]

lconEX <- merge(lconEX, lconEX_qpeacock, by = "questionID", all.x = T)

###############################################################################################
####                                                                                       ####
####                        PART 2: Descriptives and Correlations                          ####
####                                                                                       ####
###############################################################################################

# Descriptives

describeBy(lconEX_q$time.intro, group = lconEX_q$gender.asker)
describeBy(lconEX_q$time.question, group = lconEX_q$gender.asker)
describeBy(lconEX_q$time.asker.total, group = lconEX_q$gender.asker)
describeBy(lconEX_q$SumInitialRemarks, group = lconEX_q$gender.asker)
describeBy(lconEX_q$SumFollowUp, group = lconEX_q$gender.asker)
describeBy(lconEX_q$SumRemarks, group = lconEX_q$gender.asker)
describeBy(lconEX_q$CommentOnly, group = lconEX_q$gender.asker)
describeBy(lconEX_q$NoQuestions, group = lconEX_q$gender.asker)
describeBy(lconEX_q$thankyou.talk, group = lconEX_q$gender.asker)
describeBy(lconEX_q$thankyou.response, group = lconEX_q$gender.asker)
describeBy(lconEX_q$pos.comment, group = lconEX_q$gender.asker)
describeBy(lconEX_q$talk.over.speaker, group = lconEX_q$gender.asker)
describeBy(lconEX_q$reference.research, group = lconEX_q$gender.asker)
describeBy(lconEX_q$research.valence, group = lconEX_q$gender.asker)
describeBy(lconEX_q$challenge, group = lconEX_q$gender.asker)
describeBy(lconEX_q$Speak.Listen.Ratio, group = lconEX_q$gender.asker)
describeBy(lconEX_q$ThankYouEver, group = lconEX_q$gender.asker)
describeBy(lconEX_q$Nice, group = lconEX_q$gender.asker)
describeBy(lconEX_q$SumNice, group = lconEX_q$gender.asker)
describeBy(lconEX_q$pos.research, group = lconEX_q$gender.asker)
describeBy(lconEX_q$Peacocking, group = lconEX_q$gender.asker)

# Correlations

rcorr(as.matrix(cbind(lconEX_q$time.intro_clean, lconEX_q$time.question_clean,
                      lconEX_q$time.asker.total_clean, lconEX_q$SumInitialRemarks,
                      lconEX_q$SumFollowUp, lconEX_q$SumRemarks,
                      lconEX_q$CommentOnly, lconEX_q$NoQuestions, 
                      lconEX_q$thankyou.talk, lconEX_q$thankyou.response, lconEX_q$pos.comment, 
                      lconEX_q$talk.over.speaker, lconEX_q$reference.research, lconEX_q$research.valence,
                      lconEX_q$challenge, lconEX_q$Speak.Listen.Ratio, lconEX_q$ThankYouEver,
                      lconEX_q$Nice, lconEX_q$SumNice, lconEX_q$pos.research, lconEX_q$Peacocking)))

###############################################################################################
####                                                                                       ####
####                                    PART 3: Analyses                                   ####
####                                                                                       ####
###############################################################################################

## Proportions of male and female attendees and speakers

## Gender for attendees was determined based on names and internet searches.
## Gender for speakers was determined based on gender presentation in the videos.

# Men = 60
# Women = 32
# Could not identify = 1

ProportionlconEXAttendeesMale<- .65
ProportionlconEXAttendeesFemale<- .34

# 6 women speakers
# 8 men speakers

ProportionlconEXSpeakersMale <- .57
ProportionlconEXSpeakersFemale <- .43

# Participation Rates ####

## Proportion of Questions Asked by Gender

MaleTotalQuestions<-length((subset(lconEX_q, gender.asker == 1))$question.symposium)
FemaleTotalQuestions<-length((subset(lconEX_q, gender.asker == 2))$question.symposium)
MaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*ProportionlconEXAttendeesMale
FemaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*ProportionlconEXAttendeesFemale

table1<-rbind(c(MaleTotalQuestions,FemaleTotalQuestions),c(MaleExpectedQuestions,FemaleExpectedQuestions))
chisq.test(table1)
chies(chisq.test(table1)$statistic, table1[1,1]+table1[1,2])

## Time Speaking per Question

m.2 <- glm.nb(time.asker.total_clean ~ gender.asker_ec, data = lconEX_q) 
summary(m.2)
confint(m.2)

## Disproportionate Total Time in Question and Answer Period

MaleTotalTime<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$time.asker.total, na.rm =T)
FemaleTotalTime<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$time.asker.total, na.rm =T)
MaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionlconEXAttendeesMale
FemaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionlconEXAttendeesFemale

table3<-rbind(c(MaleTotalTime,FemaleTotalTime),c(MaleExpectedTime,FemaleExpectedTime))
chisq.test(table3)
chies(chisq.test(table3)$statistic, table3[1,1]+table3[1,2])

## Speaker to Asker Time Ratio

t.4 <- t.test(Speak.Listen.Ratio ~ gender.asker, data = lconEX)
t.4

gencount <- count(lconEX$gender.asker)
tes(t.4$statistic, gencount[1,2], gencount[2,2])

# Analysis of Remark Content ####

## Initial Questions

MaleTotalQuestion<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$IQCquestion, na.rm =T)
FemaleTotalQuestion<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$IQCquestion, na.rm =T)
MaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*ProportionlconEXAttendeesMale
FemaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*ProportionlconEXAttendeesFemale

table5<-rbind(c(MaleTotalQuestion,FemaleTotalQuestion),c(MaleExpectedQuestion,FemaleExpectedQuestion))
chisq.test(table5)
chies(chisq.test(table5)$statistic, table5[1,1]+table5[1,2])

## Initial Comments

MaleTotalComment<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$IQCcomment, na.rm =T)
FemaleTotalComment<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$IQCcomment, na.rm =T)
MaleExpectedComment<-(MaleTotalComment+FemaleTotalComment)*ProportionlconEXAttendeesMale
FemaleExpectedComment<-(MaleTotalComment+FemaleTotalComment)*ProportionlconEXAttendeesFemale

table6<-rbind(c(MaleTotalComment,FemaleTotalComment),c(MaleExpectedComment,FemaleExpectedComment))
chisq.test(table6)
chies(chisq.test(table6)$statistic, table6[1,1]+table6[1,2])

## Initial Comment-Questions

MaleTotalCommQ<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$IQCcomquest, na.rm =T)
FemaleTotalCommQ<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$IQCcomquest, na.rm =T)
MaleExpectedCommQ<-(MaleTotalCommQ+FemaleTotalCommQ)*ProportionlconEXAttendeesMale
FemaleExpectedCommQ<-(MaleTotalCommQ+FemaleTotalCommQ)*ProportionlconEXAttendeesFemale

table7<-rbind(c(MaleTotalCommQ,FemaleTotalCommQ),c(MaleExpectedCommQ,FemaleExpectedCommQ))
chisq.test(table7)
chies(chisq.test(table7)$statistic, table7[1,1]+table7[1,2])

## Initial Questions-Comments

MaleTotalQComm<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$IQCquestcom, na.rm =T)
FemaleTotalQComm<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$IQCquestcom, na.rm =T)
MaleExpectedQComm<-(MaleTotalQComm+FemaleTotalQComm)*ProportionlconEXAttendeesMale
FemaleExpectedQComm<-(MaleTotalQComm+FemaleTotalQComm)*ProportionlconEXAttendeesFemale

table8<-rbind(c(MaleTotalQComm,FemaleTotalQComm),c(MaleExpectedQComm,FemaleExpectedQComm))
chisq.test(table8)
chies(chisq.test(table8)$statistic, table8[1,1]+table8[1,2])

## Initial Questions

MaleTotalQuestion<-sum(lconEX_q[which(lconEX_q$gender.asker==1),]$SumInitialRemarks, na.rm =T)
FemaleTotalQuestion<-sum(lconEX_q[which(lconEX_q$gender.asker==2),]$SumInitialRemarks, na.rm =T)
MaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*ProportionlconEXAttendeesMale
FemaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*ProportionlconEXAttendeesFemale

table9<-rbind(c(MaleTotalQuestion,FemaleTotalQuestion),c(MaleExpectedQuestion,FemaleExpectedQuestion))
chisq.test(table9)
chies(chisq.test(table9)$statistic, table9[1,1]+table9[1,2])


## Initial Remakrs

t.10 <- t.test(SumInitialRemarks ~ gender.asker, data = lconEX_q) #negative binomial didn't converge
t.10 
tes(t.10$statistic, gencount[1,2], gencount[2,2])

## Follow up Remarks

t.11 <- t.test(SumFollowUp ~ gender.asker, data = lconEX_q) #negative binomial didn't converge
t.11
tes(t.11$statistic, gencount[1,2], gencount[2,2])

## Total Remarks

t.12 <- t.test(SumRemarks ~ gender.asker, data = lconEX_q) # negative binomial didn't converge
t.12 
tes(t.12$statistic, gencount[1,2], gencount[2,2])

## Only Comments

m.13 <- glm(CommentOnly ~ gender.asker_ec, data = lconEX_q)
summary(m.13)
confint(m.13)

## No Questions

m.14 <- glm(NoQuestions ~ gender.asker, data = lconEX_q)
summary(m.14) 
confint(m.14)
table(lconEX_q$NoQuestions, lconEX_q$gender.asker)

# Dominance ####

## First Four Question Askers

FirstFour<-subset(lconEX_q, question.symposium < 5)
TotalFirstFour<-length(FirstFour$question.symposium)

MaleFirstFour<-length((subset(FirstFour, gender.asker == 1))$question.symposium)
FemaleFirstFour<-length((subset(FirstFour, gender.asker == 2))$question.symposium)

table15<-rbind(c(MaleFirstFour,FemaleFirstFour),c(TotalFirstFour*ProportionlconEXAttendeesMale,TotalFirstFour*ProportionlconEXAttendeesFemale))
chisq.test(table15)
chies(chisq.test(table15)$statistic, table15[1,1]+table15[1,2])

## Challenge 

m.16 <- glm(challenge ~ gender.asker_ec, data = lconEX_q)
summary(m.16)
confint(m.16)

## Peacocking

t.17 <- t.test(Peacocking ~ gender.asker, data = lconEX_q)
t.17
tes(t.17$statistic, gencount[1,2], gencount[2,2])

## Nice to Female Speakers

m.17 <- glm(Nice ~ gender.speaker, data = lconEX)
summary(m.18) 
confint(m.18)
table(lconEX$Nice, lconEX$gender.speaker)

# Breaking Down Peacocking ####

r.18 <- rcorr(as.matrix(data.frame(lconEX_q$time.intro, lconEX_q$time.question, lconEX_q$NoQuestions, 
                                   lconEX_q$challenge, lconEX_q$pos.research, lconEX_q$talk.over.speaker)))

## Full composite

t.18a <- t.test(Peacocking ~ gender.asker, data = lconEX_q)
t.18a
tes(t.18a$statistic, gencount[1,2], gencount[2,2])

## Time introducing themselves

m.18b.a <- glm.nb(time.intro_clean ~ gender.asker, data = lconEX_q) #negative binomial 
m.18b.b <- zeroinfl(time.intro_clean ~ gender.asker, data = lconEX_q) #zero inflated negative binomial
vuong(m.18b.a, m.18b.b) #model 2 is better than model 1
summary(m.18b.b) #no significant differences
confint(m.18b.b)

## Time asking initial question

m.18c <- glm.nb(time.question_clean ~ gender.asker, data = lconEX_q)
summary(m.18c) 
confint(m.18c)

## No questions

m.18d <- glm(NoQuestions ~ gender.asker, data = lconEX_q)
summary(m.18d) 
confint(m.18d)
table(lconEX_q$NoQuestions, lconEX_q$gender.asker)

## Challenging the speaker

m.18e <- glm(challenge ~ gender.asker, data = lconEX_q)
summary(m.18e) 
confint(m.18e)
table(lconEX_q$challenge, lconEX_q$gender.asker)

## Positive Self-Reference

m.18f <- glm(pos.research ~ gender.asker, data = lconEX_q)
summary(m.18f) 
confint(m.18f)
table(lconEX_q$pos.research, lconEX_q$gender.asker)

## Talking over Speaker

lconEX_q$talk.over.speaker <- ifelse(lconEX_q$talk.over.speaker==2,1,lconEX_q$talk.over.speaker) # There is only one person who talked over twice. Changing this to a binary of if ever in order to simplify analyses
m.18g <- glm(talk.over.speaker ~ gender.asker, data = lconEX_q)
summary(m.18g)
confint(m.18g)
table(lconEX_q$talk.over.speaker, lconEX_q$gender.asker)

