#############Examining Question and Answer Behaviors############
###Begin 6-11-2020###

####Set up####
###packages
library(MASS)
library(ggplot2)
library(pscl)
library(plyr)
library(doBy)
library(lme4)
library(lmerTest)

###reading data
#Note: We used data from another conference to practice our coding scheme and conduct exploratory analyses
#The following row loads those data for the purposes of demonstrating the cleaning functions
setwd("/Users/[User]/Downloads/")
con <- read.csv("TestConferenceQAdata.csv") 

####Cleaning Data####

con$gender.asker_ec <- ifelse(con$gender.asker==1, -.5,
                              ifelse(con$gender.asker==2, .5, NA))

con$gender.speaker_ec <- ifelse(con$gender.speaker==1, -.5,
                              ifelse(con$gender.speaker==2, .5, NA))

con$time.asker.total_clean <- ifelse(con$time.asker.total == 6.5, 7, #time was calculated as the average of two coders. 
                              ifelse(con$time.asker.total == 23.5, 24, #For averages that ended in decimals, 
                              ifelse(con$time.asker.total == 25.5, 26, #they were rounded to the nearest whole number to be used in 
                              ifelse(con$time.asker.total == 40.5, 41, #models with negative binomial distributions.
                              ifelse(con$time.asker.total == 47.5, 48,
                              ifelse(con$time.asker.total == 50.5, 51,
                              ifelse(con$time.asker.total == 54.5, 55,
                              ifelse(con$time.asker.total == 67.5, 68,
                              ifelse(con$time.asker.total == 69.5, 70,
                              ifelse(con$time.asker.total == 77.5, 78,
                              ifelse(con$time.asker.total == 78.5, 79,
                              ifelse(con$time.asker.total == 94.5, 95,
                              ifelse(con$time.asker.total == 134.5, 135, con$time.asker.total)))))))))))))

con$SumInitialRemarks <- rowSums(data.frame(con$IQCquestion, con$IQCcomment, con$IQCcomquest, con$IQCquestcom), na.rm = T)
con$SumInitialRemarks[is.na(con$questionID)] <- NA

con$SumRemarks <- rowSums(data.frame(con$IQCquestion, con$IQCcomment, con$IQCcomquest, 
                                     con$IQCquestcom, con$FQCquestion, con$FQCcomment,
                                     con$FQCcomquest), na.rm = T)
con$SumRemarks[is.na(con$questionID)] <- NA

con$SumFollowUp <- rowSums(data.frame(con$FQCquestion, con$FQCcomment, con$FQCcomquest), na.rm = T)
con$SumFollowUp[is.na(con$questionID)] <- NA

con$NoQuestions <- ifelse(con$IQCquestion == 0, 1, 0)
con$NoQuestions[is.na(con$questionID)] <- NA

con$CommentOnly <- ifelse(con$IQCcomment>0 & con$IQCquestion==0 & con$IQCcomquest==0 & con$IQCquestcom==0, 1, 0)
con$CommentOnly[is.na(con$questionID)] <- NA

con$Speak.Listen.Ratio <- con$time.asker.total - con$time.response

con$ThankYouEver <- ifelse(con$thankyou.talk==1 | con$thankyou.response ==1, 1, 0)
con$ThankYouEver[is.na(con$questionID)] <- NA

con$Nice <- ifelse(con$thankyou.talk==1 | con$thankyou.response ==1 | con$pos.comment==1, 1, 0)
con$Nice[is.na(con$questionID)] <- NA

con$SumNice <- rowSums(data.frame(con$thankyou.talk, con$thankyou.response, con$pos.comment), na.rm = T)
con$SumNice[is.na(con$questionID)] <- NA

con$pos.research <- 0
con$pos.research[con$research.valence==1] <- 1 
con$pos.research[is.na(con$questionID)] <- NA

con_q <-con[-which(duplicated(con$questionID)),] #subsetting data so that questions answered by multiple speakers aren't doubly weighted

con_q$ZNoQuestions <- scale(con_q$NoQuestions)
con_q$Ztalk.over.speaker <- scale(con_q$talk.over.speaker)
con_q$Zpos.research <- scale(con_q$pos.research)
con_q$Zchallenge <- scale(con_q$challenge)
con_q$Ztime.intro <- scale(con_q$time.intro)
con_q$Ztime.question <- scale(con_q$time.question)

con_q$Peacocking <- rowMeans(cbind(con_q$ZNoQuestions, con_q$Ztalk.over.speaker, con_q$Zpos.research,  
                                   con_q$Zchallenge, con_q$Ztime.intro, con_q$Ztime.question), na.rm = T)

keeps <- c("questionID", "Peacocking")

con_qpeacock <- con_q[, (names(con_q) %in% keeps)]

con <- merge(con, con_qpeacock, by = "questionID", all.x = T)

#Proportions of male and female attendees - to be added
ProportionAttendeesMale<- 
ProportionAttendeesFemale<- 

####Primary Analyses####

#1 - Do men spend more time speaking per question?
m.1 <- glmer.nb(time.asker.total ~ gender.asker_ec + (1|speakerID), data = con_q) #analyses are nexted within speaker
summary(m.1)

#2 - Do men take up a disproporitionate amount of time during Q&A sessions?
MaleTotalTime<-sum(con_q[which(con_q$gender.asker==1),]$time.asker.total, na.rm =T)
FemaleTotalTime<-sum(con_q[which(con_q$gender.asker==2),]$time.asker.total, na.rm =T)
MaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesMale
FemaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesFemale

table2<-rbind(c(MaleTotalTime,FemaleTotalTime),c(MaleExpectedTime,FemaleExpectedTime))
chisq.test(table2)

#3 - Are there gender differences in the ratio of length of question to length of response?
m.3 <- lmer(Speak.Listen.Ratio ~ gender.asker_ec + (1|speakerID), data = con)
summary(m.3)

#4 - Are men overrepresented among the first four question askers in Q&A sessions?
FirstFour<-subset(con_q, question.symposium < 5)
TotalFirstFour<-length(FirstFour$question.symposium)

MaleFirstFour<-length((subset(FirstFour, gender.asker == 1))$question.symposium)
FemaleFirstFour<-length((subset(FirstFour, gender.asker == 2))$question.symposium)

table4<-rbind(c(MaleFirstFour,FemaleFirstFour),c(TotalFirstFour*ProportionAttendeesMale,TotalFirstFour*ProportionAttendeesFemale))
chisq.test(table4)

#5 - Did men or women ask a disproportionate number of questions (relative to their proportion in attendance)? 
MaleTotalQuestions<-length((subset(con_q, gender.asker == 1))$question.symposium)
FemaleTotalQuestions<-length((subset(con_q, gender.asker == 2))$question.symposium)
MaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*ProportionAttendeesMale
FemaleExpectedQuestions<-(MaleTotalQuestions+FemaleTotalQuestions)*ProportionAttendeesFemale

table5<-rbind(c(MaleTotalQuestions,FemaleTotalQuestions),c(MaleExpectedQuestions,FemaleExpectedQuestions))
chisq.test(table5)

#6 - Are men more likely to challenge the speaker with their question/comment?
m.6 <- glmer(challenge ~ gender.asker_ec + (1|speakerID), data = con_q, family=gaussian)
summary(m.6)

#7 - Are men more likely to exhibit behaviors of peacocking? (see lines 76-84 for construction of this variable)
m.7 <- lmer(Peacocking ~ gender.asker_ec + (1|speakerID), data = con_q)
summary(m.7)

#8 - Do men make more contirbutions that are only comments compared to women?
m.8 <- glmer(CommentOnly ~ gender.asker_ec + (1|speakerID), data = con_q) 
summary(m.8)

####Planned Exploratory Analyses####
#On a few occassions during the conference, the moderators asked the attendees
#to be more conscientious in their question asking (e.g., ask shorter questions, give others a chance to ask questions)
#We will examine whether or not these interventions improved question asking behavior (e.g., shortened questions, reduced repeat askers)


####Additional Exploratory Analyses####
#to be added

