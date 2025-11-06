###############################################################################################
####                                                                                       ####
####                              PART 1: Data Cleaning                                    ####
####                                                                                       ####
###############################################################################################

library(compute.es)
library(MASS)
library(pscl)
library(plyr)
library(doBy)
library(lme4)
library(lmerTest)
library(psych)
library(Hmisc)

setwd("C:/Users/[user]/Downloads") 
lcon1 <- read.csv("3. LiveConference1Q&ABehavioral.csv")

# Data Cleaning ####

lcon1$mod[is.na(lcon1$mod)] <- 0
lcon1 <- lcon1[which(lcon1$mod!=1),]

lcon1$gender.asker_ec <- ifelse(lcon1$gender.asker==1, -.5,
                         ifelse(lcon1$gender.asker==2, .5, NA))

lcon1$gender.speaker_ec <- ifelse(lcon1$gender.speaker==1, -.5,
                           ifelse(lcon1$gender.speaker==2, .5, NA))

lcon1$time.asker.total_clean <- ifelse(lcon1$time.asker.total == 4.5, 5, #time was calculated as the average of two coders. 
                                ifelse(lcon1$time.asker.total == 11.5, 12, #For averages that ended in decimals, 
                                ifelse(lcon1$time.asker.total == 16.5, 17, #they were rounded to the nearest whole number to be used in 
                                ifelse(lcon1$time.asker.total == 17.5, 18, #models with negative binomial distributions.
                                ifelse(lcon1$time.asker.total == 18.5, 19,
                                ifelse(lcon1$time.asker.total == 19.5, 20,
                                ifelse(lcon1$time.asker.total == 20.5, 21,
                                ifelse(lcon1$time.asker.total == 21.5, 22,
                                ifelse(lcon1$time.asker.total == 22.5, 23,
                                ifelse(lcon1$time.asker.total == 23.5, 24,
                                ifelse(lcon1$time.asker.total == 24.5, 25,
                                ifelse(lcon1$time.asker.total == 25.5, 26,
                                ifelse(lcon1$time.asker.total == 27.5, 28,
                                ifelse(lcon1$time.asker.total == 28.5, 29,
                                ifelse(lcon1$time.asker.total == 29.5, 30,
                                ifelse(lcon1$time.asker.total == 30.5, 31,
                                ifelse(lcon1$time.asker.total == 31.5, 32,
                                ifelse(lcon1$time.asker.total == 33.5, 34,
                                ifelse(lcon1$time.asker.total == 35.5, 36,
                                ifelse(lcon1$time.asker.total == 37.5, 38,
                                ifelse(lcon1$time.asker.total == 38.5, 39,
                                ifelse(lcon1$time.asker.total == 39.5, 40,  
                                ifelse(lcon1$time.asker.total == 40.5, 41,
                                ifelse(lcon1$time.asker.total == 41.5, 42,
                                ifelse(lcon1$time.asker.total == 44.5, 45,
                                ifelse(lcon1$time.asker.total == 45.5, 46,
                                ifelse(lcon1$time.asker.total == 46.5, 47,
                                ifelse(lcon1$time.asker.total == 47.5, 48,
                                ifelse(lcon1$time.asker.total == 48.5, 49,
                                ifelse(lcon1$time.asker.total == 49.5, 50,
                                ifelse(lcon1$time.asker.total == 51.5, 52,
                                ifelse(lcon1$time.asker.total == 52.5, 53,
                                ifelse(lcon1$time.asker.total == 53.5, 54,
                                ifelse(lcon1$time.asker.total == 54.5, 55,
                                ifelse(lcon1$time.asker.total == 55.5, 56,
                                ifelse(lcon1$time.asker.total == 56.5, 57,
                                ifelse(lcon1$time.asker.total == 57.5, 58,
                                ifelse(lcon1$time.asker.total == 58.5, 59,
                                ifelse(lcon1$time.asker.total == 59.5, 60,
                                ifelse(lcon1$time.asker.total == 60.5, 61,
                                ifelse(lcon1$time.asker.total == 62.5, 63,
                                ifelse(lcon1$time.asker.total == 63.5, 64,
                                ifelse(lcon1$time.asker.total == 64.5, 65,
                                ifelse(lcon1$time.asker.total == 65.5, 66,
                                ifelse(lcon1$time.asker.total == 66.5, 67,
                                ifelse(lcon1$time.asker.total == 67.5, 68,
                                ifelse(lcon1$time.asker.total == 70.5, 71,
                                ifelse(lcon1$time.asker.total == 72.5, 73,
                                ifelse(lcon1$time.asker.total == 74.5, 75,
                                ifelse(lcon1$time.asker.total == 75.5, 76, lcon1$time.asker.total))))))))))))))))))))))))))))))))))))))))))))))))))

lcon1$time.asker.total_clean <- ifelse(lcon1$time.asker.total_clean == 76.5, 77, 
                                ifelse(lcon1$time.asker.total_clean == 78.5, 79,
                                ifelse(lcon1$time.asker.total_clean == 85.5, 86,
                                ifelse(lcon1$time.asker.total_clean == 87.5, 88,
                                ifelse(lcon1$time.asker.total_clean == 92.5, 93,
                                ifelse(lcon1$time.asker.total_clean == 99.5, 100,
                                ifelse(lcon1$time.asker.total_clean == 102.5, 103,
                                ifelse(lcon1$time.asker.total_clean == 105.5, 106,
                                ifelse(lcon1$time.asker.total_clean == 112.5, 113,
                                ifelse(lcon1$time.asker.total_clean == 116.5, 117,
                                ifelse(lcon1$time.asker.total_clean == 117.5, 118,
                                ifelse(lcon1$time.asker.total_clean == 122.5, 123,
                                ifelse(lcon1$time.asker.total_clean == 126.5, 127,
                                ifelse(lcon1$time.asker.total_clean == 137.5, 138,
                                ifelse(lcon1$time.asker.total_clean == 144.5, 145,
                                ifelse(lcon1$time.asker.total_clean == 190.5, 191,
                                ifelse(lcon1$time.asker.total_clean == 301.5, 302, lcon1$time.asker.total_clean)))))))))))))))))


lcon1$time.question_clean <- ifelse(lcon1$time.question == 4.5, 5, 
                             ifelse(lcon1$time.question == 8.5, 9, 
                             ifelse(lcon1$time.question == 15.5, 16, 
                             ifelse(lcon1$time.question == 17.5, 18, 
                             ifelse(lcon1$time.question == 18.5, 19,
                             ifelse(lcon1$time.question == 19.5, 20,
                             ifelse(lcon1$time.question == 20.5, 21,
                             ifelse(lcon1$time.question == 21.5, 22,
                             ifelse(lcon1$time.question == 24.5, 25,
                             ifelse(lcon1$time.question == 25.5, 26,
                             ifelse(lcon1$time.question == 27.5, 28,
                             ifelse(lcon1$time.question == 28.5, 29,
                             ifelse(lcon1$time.question == 29.5, 30,
                             ifelse(lcon1$time.question == 30.5, 31,
                             ifelse(lcon1$time.question == 31.5, 32,
                             ifelse(lcon1$time.question == 32.5, 33,
                             ifelse(lcon1$time.question == 33.5, 34,
                             ifelse(lcon1$time.question == 34.5, 35,
                             ifelse(lcon1$time.question == 37.5, 38,
                             ifelse(lcon1$time.question == 38.5, 39,
                             ifelse(lcon1$time.question == 39.5, 40,
                             ifelse(lcon1$time.question == 40.5, 41,  
                             ifelse(lcon1$time.question == 41.5, 42,
                             ifelse(lcon1$time.question == 44.5, 45,
                             ifelse(lcon1$time.question == 45.5, 46,
                             ifelse(lcon1$time.question == 46.5, 47,
                             ifelse(lcon1$time.question == 47.5, 48,
                             ifelse(lcon1$time.question == 49.5, 50,
                             ifelse(lcon1$time.question == 50.5, 51,
                             ifelse(lcon1$time.question == 51.5, 52,
                             ifelse(lcon1$time.question == 52.5, 53,
                             ifelse(lcon1$time.question == 53.5, 54,
                             ifelse(lcon1$time.question == 54.5, 55,
                             ifelse(lcon1$time.question == 55.5, 56,
                             ifelse(lcon1$time.question == 56.5, 57,
                             ifelse(lcon1$time.question == 57.5, 58,
                             ifelse(lcon1$time.question == 58.5, 59,
                             ifelse(lcon1$time.question == 60.5, 61,
                             ifelse(lcon1$time.question == 62.5, 63,
                             ifelse(lcon1$time.question == 63.5, 64,
                             ifelse(lcon1$time.question == 64.5, 65,
                             ifelse(lcon1$time.question == 63.5, 64,
                             ifelse(lcon1$time.question == 64.5, 65,
                             ifelse(lcon1$time.question == 66.5, 67,
                             ifelse(lcon1$time.question == 67.5, 68,
                             ifelse(lcon1$time.question == 69.5, 70,
                             ifelse(lcon1$time.question == 70.5, 71,
                             ifelse(lcon1$time.question == 71.5, 72,
                             ifelse(lcon1$time.question == 73.5, 74,
                             ifelse(lcon1$time.question == 76.5, 77, lcon1$time.question))))))))))))))))))))))))))))))))))))))))))))))))))

lcon1$time.question_clean <- ifelse(lcon1$time.question_clean == 77.5, 78, 
                             ifelse(lcon1$time.question_clean == 80.5, 81,
                             ifelse(lcon1$time.question_clean == 85.5, 86,
                             ifelse(lcon1$time.question_clean == 86.5, 87,
                             ifelse(lcon1$time.question_clean == 92.5, 93,
                             ifelse(lcon1$time.question_clean == 95.5, 96,
                             ifelse(lcon1$time.question_clean == 96.5, 97,
                             ifelse(lcon1$time.question_clean == 99.5, 100,
                             ifelse(lcon1$time.question_clean == 102.5, 103,
                             ifelse(lcon1$time.question_clean == 105.5, 106,
                             ifelse(lcon1$time.question_clean == 106.5, 107,
                             ifelse(lcon1$time.question_clean == 122.5, 123,
                             ifelse(lcon1$time.question_clean == 123.5, 124,
                             ifelse(lcon1$time.question_clean == 137.5, 138,
                             ifelse(lcon1$time.question_clean == 201.5, 202,
                             ifelse(lcon1$time.question_clean == 290.5, 291, lcon1$time.question_clean))))))))))))))))

lcon1$time.intro_clean <- ifelse(lcon1$time.intro == 0.5, 1, 
                          ifelse(lcon1$time.intro == 1.5, 2,
                          ifelse(lcon1$time.intro == 2.5, 3,
                          ifelse(lcon1$time.intro == 3.5, 4,
                          ifelse(lcon1$time.intro == 4.5, 5,
                          ifelse(lcon1$time.intro == 5.5, 6,
                          ifelse(lcon1$time.intro == 6.5, 7,
                          ifelse(lcon1$time.intro == 7.5, 8,
                          ifelse(lcon1$time.intro == 8.5, 9, lcon1$time.intro)))))))))


lcon1$SumInitialRemarks <- rowSums(data.frame(lcon1$IQCquestion, lcon1$IQCcomment, lcon1$IQCcomquest, lcon1$IQCquestcom), na.rm = T)
lcon1$SumInitialRemarks[is.na(lcon1$questionID)] <- NA

lcon1$SumRemarks <- rowSums(data.frame(lcon1$IQCquestion, lcon1$IQCcomment, lcon1$IQCcomquest, 
                                      lcon1$IQCquestcom, lcon1$FQCquestion, lcon1$FQCcomment,
                                      lcon1$FQCcomquest), na.rm = T)
lcon1$SumRemarks[is.na(lcon1$questionID)] <- NA

lcon1$SumFollowUp <- rowSums(data.frame(lcon1$FQCquestion, lcon1$FQCcomment, lcon1$FQCcomquest), na.rm = T)
lcon1$SumFollowUp[is.na(lcon1$questionID)] <- NA

lcon1$NoQuestions <- ifelse(lcon1$IQCquestion == 0, 1, 0)
lcon1$NoQuestions[is.na(lcon1$questionID)] <- NA

lcon1$CommentOnly <- ifelse(lcon1$IQCcomment>0 & lcon1$IQCquestion==0 & lcon1$IQCcomquest==0 & lcon1$IQCquestcom==0, 1, 0)
lcon1$CommentOnly[is.na(lcon1$questionID)] <- NA

lcon1$Speak.Listen.Ratio <- lcon1$time.asker.total - lcon1$time.response

lcon1$ThankYouEver <- ifelse(lcon1$thankyou.talk==1 | lcon1$thankyou.response ==1, 1, 0)
lcon1$ThankYouEver[is.na(lcon1$questionID)] <- NA

lcon1$Nice <- ifelse(lcon1$thankyou.talk==1 | lcon1$thankyou.response ==1 | lcon1$pos.comment==1, 1, 0)
lcon1$Nice[is.na(lcon1$questionID)] <- NA

lcon1$SumNice <- rowSums(data.frame(lcon1$thankyou.talk, lcon1$thankyou.response, lcon1$pos.comment), na.rm = T)
lcon1$SumNice[is.na(lcon1$questionID)] <- NA

lcon1$pos.research <- 0
lcon1$pos.research[lcon1$research.valence==1] <- 1 
lcon1$pos.research[is.na(lcon1$questionID)] <- NA

lcon1_q <-lcon1[-which(duplicated(lcon1$questionID)),] #subsetting data so that questions answered by multiple speakers aren't doubly weighted

lcon1_q$ZNoQuestions <- scale(lcon1_q$NoQuestions)
lcon1_q$Ztalk.over.speaker <- scale(lcon1_q$talk.over.speaker)
lcon1_q$Zpos.research <- scale(lcon1_q$pos.research)
lcon1_q$Zchallenge <- scale(lcon1_q$challenge)
lcon1_q$Ztime.intro <- scale(lcon1_q$time.intro)
lcon1_q$Ztime.question <- scale(lcon1_q$time.question)

lcon1_q$Peacocking <- rowMeans(cbind(lcon1_q$ZNoQuestions, lcon1_q$Ztalk.over.speaker, lcon1_q$Zpos.research,  
                                    lcon1_q$Zchallenge, lcon1_q$Ztime.intro, lcon1_q$Ztime.question), na.rm = T)

keeps <- c("questionID", "Peacocking")

lcon1_qpeacock <- lcon1_q[, (names(lcon1_q) %in% keeps)]

lcon1 <- merge(lcon1, lcon1_qpeacock, by = "questionID", all.x = T)

###############################################################################################
####                                                                                       ####
####                                 PART 2: Demographics                                  ####
####                                                                                       ####
###############################################################################################

sum(lcon1_q$SumInitialRemarks, na.rm = T)
sum(lcon1_q$SumFollowUp, na.rm = T)

###############################################################################################
####                                                                                       ####
####                        PART 2: Descriptives and Correlations                          ####
####                                                                                       ####
###############################################################################################

# Descriptives 

describeBy(lcon1_q$time.intro, group = lcon1_q$gender.asker)
describeBy(lcon1_q$time.question, group = lcon1_q$gender.asker)
describeBy(lcon1_q$time.asker.total, group = lcon1_q$gender.asker)
describeBy(lcon1_q$SumInitialRemarks, group = lcon1_q$gender.asker)
describeBy(lcon1_q$SumFollowUp, group = lcon1_q$gender.asker)
describeBy(lcon1_q$SumRemarks, group = lcon1_q$gender.asker)
describeBy(lcon1_q$CommentOnly, group = lcon1_q$gender.asker)
describeBy(lcon1_q$NoQuestions, group = lcon1_q$gender.asker)
describeBy(lcon1_q$thankyou.talk, group = lcon1_q$gender.asker)
describeBy(lcon1_q$thankyou.response, group = lcon1_q$gender.asker)
describeBy(lcon1_q$pos.comment, group = lcon1_q$gender.asker)
describeBy(lcon1_q$talk.over.speaker, group = lcon1_q$gender.asker)
describeBy(lcon1_q$reference.research, group = lcon1_q$gender.asker)
describeBy(lcon1_q$research.valence, group = lcon1_q$gender.asker)
describeBy(lcon1_q$challenge, group = lcon1_q$gender.asker)
describeBy(lcon1_q$Speak.Listen.Ratio, group = lcon1_q$gender.asker)
describeBy(lcon1_q$ThankYouEver, group = lcon1_q$gender.asker)
describeBy(lcon1_q$Nice, group = lcon1_q$gender.asker)
describeBy(lcon1_q$SumNice, group = lcon1_q$gender.asker)
describeBy(lcon1_q$pos.research, group = lcon1_q$gender.asker)
describeBy(lcon1_q$Peacocking, group = lcon1_q$gender.asker)

# Correlations

rcorr(as.matrix(cbind(lcon1_q$time.intro, lcon1_q$time.question, lcon1_q$time.asker.total, 
                      lcon1_q$SumInitialRemarks, lcon1_q$SumFollowUp, lcon1_q$SumRemarks,
                      lcon1_q$CommentOnly, lcon1_q$NoQuestions, 
                      lcon1_q$thankyou.talk, lcon1_q$thankyou.response, lcon1_q$pos.comment, 
                      lcon1_q$talk.over.speaker, lcon1_q$reference.research, lcon1_q$research.valence,
                      lcon1_q$challenge, lcon1_q$Speak.Listen.Ratio, lcon1_q$ThankYouEver,
                      lcon1_q$Nice, lcon1_q$SumNice, lcon1_q$pos.research, lcon1_q$Peacocking)))

###############################################################################################
####                                                                                       ####
####                                    PART 3: Analyses                                   ####
####                                                                                       ####
###############################################################################################

#Proportions of male and female attendees 

Proportionlcon1AttendeesMale<- .632
Proportionlcon1AttendeesFemale<- .352

Proportionlcon1SpeakersMale <- .55
Proportionlcon1SpeakersFemale <- .45

## Disproportionate Participation ####

### Proportion of Questions Asked by Gender

MaleTotalQuestions<-length((subset(lcon1_q, gender.asker == 1))$question.speaker)
FemaleTotalQuestions<-length((subset(lcon1_q, gender.asker == 2))$question.speaker)
MaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*Proportionlcon1AttendeesMale
FemaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*Proportionlcon1AttendeesFemale

table1<-rbind(c(MaleTotalQuestions,FemaleTotalQuestions),c(MaleExpectedQuestions,FemaleExpectedQuestions))
chisq.test(table1)
chies(chisq.test(table1)$statistic, table1[1,1]+table1[1,2])

### Time Speaking per Question

m.2 <- glm.nb(time.asker.total_clean ~ gender.asker_ec, data = lcon1_q) #glmer.nb did not converge 
summary(m.2)
confint(m.2)

### Disproportionate Total Time in Question and Answer Period

MaleTotalTime<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$time.asker.total, na.rm =T)
FemaleTotalTime<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$time.asker.total, na.rm =T)
MaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*Proportionlcon1AttendeesMale
FemaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*Proportionlcon1AttendeesFemale

table3<-rbind(c(MaleTotalTime,FemaleTotalTime),c(MaleExpectedTime,FemaleExpectedTime))
chisq.test(table3)
chies(chisq.test(table3)$statistic, table3[1,1]+table3[1,2])

## Type of Question ####

### Initial Questions

MaleTotalQuestion<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$IQCquestion, na.rm =T)
FemaleTotalQuestion<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$IQCquestion, na.rm =T)
MaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*Proportionlcon1AttendeesMale
FemaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*Proportionlcon1AttendeesFemale

table4<-rbind(c(MaleTotalQuestion,FemaleTotalQuestion),c(MaleExpectedQuestion,FemaleExpectedQuestion))
chisq.test(table4)
chies(chisq.test(table4)$statistic, table4[1,1]+table4[1,2])

### Iniital Comments

MaleTotalComment<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$IQCcomment, na.rm =T)
FemaleTotalComment<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$IQCcomment, na.rm =T)
MaleExpectedComment<-(MaleTotalComment+FemaleTotalComment)*Proportionlcon1AttendeesMale
FemaleExpectedComment<-(MaleTotalComment+FemaleTotalComment)*Proportionlcon1AttendeesFemale

table5<-rbind(c(MaleTotalComment,FemaleTotalComment),c(MaleExpectedComment,FemaleExpectedComment))
chisq.test(table5)
chies(chisq.test(table5)$statistic, table5[1,1]+table5[1,2])

### Initial Comment-Questions

MaleTotalCommQ<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$IQCcomquest, na.rm =T)
FemaleTotalCommQ<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$IQCcomquest, na.rm =T)
MaleExpectedCommQ<-(MaleTotalCommQ+FemaleTotalCommQ)*Proportionlcon1AttendeesMale
FemaleExpectedCommQ<-(MaleTotalCommQ+FemaleTotalCommQ)*Proportionlcon1AttendeesFemale

table6<-rbind(c(MaleTotalCommQ,FemaleTotalCommQ),c(MaleExpectedCommQ,FemaleExpectedCommQ))
chisq.test(table6)
chies(chisq.test(table6)$statistic, table6[1,1]+table6[1,2])

### Initial Questions-Comments

MaleTotalQComm<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$IQCquestcom, na.rm =T)
FemaleTotalQComm<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$IQCquestcom, na.rm =T)
MaleExpectedQComm<-(MaleTotalQComm+FemaleTotalQComm)*Proportionlcon1AttendeesMale
FemaleExpectedQComm<-(MaleTotalQComm+FemaleTotalQComm)*Proportionlcon1AttendeesFemale

table7<-rbind(c(MaleTotalQComm,FemaleTotalQComm),c(MaleExpectedQComm,FemaleExpectedQComm))
chisq.test(table7)
chies(chisq.test(table7)$statistic, table7[1,1]+table7[1,2])

### All Remarks

MaleTotalQuestion<-sum(lcon1_q[which(lcon1_q$gender.asker==1),]$SumInitialRemarks, na.rm=T)  
FemaleTotalQuestion<-sum(lcon1_q[which(lcon1_q$gender.asker==2),]$SumInitialRemarks, na.rm =T) 
MaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*Proportionlcon1AttendeesMale
FemaleExpectedQuestion<-(MaleTotalQuestion+FemaleTotalQuestion)*Proportionlcon1AttendeesFemale

table8<-rbind(c(MaleTotalQuestion,FemaleTotalQuestion),c(MaleExpectedQuestion,FemaleExpectedQuestion))
chisq.test(table8)
chies(chisq.test(table8)$statistic, table8[1,1]+table8[1,2])

### Initial Remakrs

t.9 <- t.test(SumInitialRemarks ~ gender.asker, data = lcon1_q)  #glmer.nb didn't converge
t.9 

gencount <- count(lcon1_q$gender.asker)
tes(t.9$statistic, gencount[1,2], gencount[2,2])


### Follow up Remarks

t.10 <- t.test(SumFollowUp ~ gender.asker, data = lcon1_q) #negative binomial didn't converge
t.10 
tes(t.10$statistic, gencount[1,2], gencount[2,2])

### Total Remarks

t.11 <- t.test(SumRemarks ~ gender.asker, data = lcon1_q) #negative binomial didn't converge, so testing with a t-test
t.11 
tes(t.11$statistic, gencount[1,2], gencount[2,2])

### Only Comments

m.12 <- glm(CommentOnly ~ gender.asker_ec, data = lcon1_q) #glmer didn't converge
summary(m.12)
confint(m.12)

### No Questions

m.13 <- glm(NoQuestions ~ gender.asker_ec, data = lcon1_q) #glmer didn't converge
summary(m.13)
confint(m.13)

## How People Asked Questions ####

### First Four Question Askers

FirstFour<-subset(lcon1_q, question.speaker < 5)
TotalFirstFour<-length(FirstFour$question.speaker)

MaleFirstFour<-length((subset(FirstFour, gender.asker == 1))$question.speaker)
FemaleFirstFour<-length((subset(FirstFour, gender.asker == 2))$question.speaker)

table14<-rbind(c(MaleFirstFour,FemaleFirstFour),c(TotalFirstFour*Proportionlcon1AttendeesMale,TotalFirstFour*Proportionlcon1AttendeesFemale))
chisq.test(table14)
chies(chisq.test(table14)$statistic, table14[1,1]+table14[1,2])

### Challenge

m.14 <- glm(challenge ~ gender.asker_ec, data = lcon1_q, family=gaussian)
summary(m.14)
confint(m.14)

### Nice 

m.15 <- glm(Nice ~ gender.asker_ec, data = lcon1_q, family=gaussian)
summary(m.15)
confint(m.15)

## Nice to Female Speakers ####

m.16 <- glm(Nice ~ gender.speaker, data = lcon1)
summary(m.16) 
table(lcon1$Nice, lcon1$gender.speaker)
confint(m.16)
describeBy(lcon1_q$Nice, group = lcon1_q$gender.speaker)


###############################################################################################
####                                                                                       ####
####                             PART 3: Supplemental Analyses                             ####
####                                                                                       ####
###############################################################################################

## Speaker to Asker Time Ratio

m.17 <- lmer(Speak.Listen.Ratio ~ gender.asker_ec + (1|speakerID), data = lcon1)
summary(m.17)
confint(m.17)

## Peacocking

m.18 <- lmer(Peacocking ~ gender.asker_ec + (1|speakerID), data = lcon1_q)
summary(m.18)
confint(m.18)

psych::alpha(cbind.data.frame(lcon1_q$ZNoQuestions, lcon1_q$Ztalk.over.speaker, lcon1_q$Zpos.research,  
                              lcon1_q$Zchallenge, lcon1_q$Ztime.intro, lcon1_q$Ztime.question))$total$raw_alpha

r.18 <- rcorr(as.matrix(data.frame(lcon1_q$time.intro, lcon1_q$time.question, lcon1_q$NoQuestions, 
                                   lcon1_q$challenge, lcon1_q$pos.research, lcon1_q$talk.over.speaker)))

### Pulling apart Peacocking

#### Full composite

m.18a <- lmer(Peacocking ~ gender.asker_ec + (1|speakerID), data = lcon1_q)
summary(m.18a)
confint(m.18a)

#### Time introducing themselves

m.18b.a <- glm.nb(time.intro_clean ~ gender.asker, data = lcon1_q)
m.18b.b <- zeroinfl(time.intro_clean ~ gender.asker, data = lcon1_q)
vuong(m.18b.a, m.18b.b) #model 2 is better than model 1
summary(m.18b.b) 
confint(m.18b.b)

#### Time asking initial question

m.18c <- glm.nb(time.question_clean ~ gender.asker, data = lcon1_q)
summary(m.18c) 
confint(m.18c)

#### No questions

m.18d <- glm(NoQuestions ~ gender.asker, data = lcon1_q)
summary(m.18d) 
confint(m.18d)
table(lcon1_q$NoQuestions, lcon1_q$gender.asker)

#### Challenging the speaker

m.18e <- glm(challenge ~ gender.asker, data = lcon1_q)
summary(m.18e) 
confint(m.18e)
table(lcon1_q$challenge, lcon1_q$gender.asker)

#### Positive Self-Reference

m.18f <- glm(pos.research ~ gender.asker, data = lcon1_q)
summary(m.18f) 
confint(m.18f)
table(lcon1_q$pos.research, lcon1_q$gender.asker)

#### Talking over Speaker

lcon1_q$talk.over.speaker <- ifelse(lcon1_q$talk.over.speaker==2,1,lcon1_q$talk.over.speaker) # There is only one person who talked over twice. Changing this to a binary of if ever in order to simplify analyses
m.18g <- glm(talk.over.speaker ~ gender.asker, data = lcon1_q)
summary(m.18g)
confint(m.18g)
table(lcon1_q$talk.over.speaker, lcon1_q$gender.asker)
