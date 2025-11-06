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

con$time.intro_clean <- ifelse(con$time.intro == 2.5, 3,
                        ifelse(con$time.intro == 4.5, 4, con$time.intro))

con$time.question_clean <- ifelse(con$time.question == 6.5, 7,
                           ifelse(con$time.question == 16.5, 17,
                           ifelse(con$time.question == 22.5, 23,
                           ifelse(con$time.question == 23.5, 24,
                           ifelse(con$time.question == 42.5, 43,
                           ifelse(con$time.question == 50.5, 51,
                           ifelse(con$time.question == 54.5, 55,
                           ifelse(con$time.question == 67.5, 68,
                           ifelse(con$time.question == 78.5, 79,
                           ifelse(con$time.question == 94.5, 95,
                           ifelse(con$time.question == 134.5, 135, con$time.question)))))))))))

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

con$retain <- ifelse(con$pass.retain==1, 1,0)
con$retain[is.na(con$questionID)] <- NA

con$pass <- ifelse(con$pass.retain==-1, 1,0)
con$pass[is.na(con$questionID)] <- NA

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

#Proportions of male and female attendees and speakers ####

## Gender for attendees was determined based on names and internet searches.
## Gender for speakers was deterimend based on gender presentation in the videos.
## This will be refined after collecting data from participants.

# Men = 60
# Women = 32
# Could not identify = 1

ProportionAttendeesMale<- .65
ProportionAttendeesFemale<- .34

# 6 women speakers
# 8 men speakers

ProportionSpeakersMale <- .57
ProportionSpeakersFemale <- .43

####Primary Analyses####

#1 - Do men spend more time speaking per question?
m.1 <- glm.nb(time.asker.total ~ gender.asker_ec, data = con_q) 
summary(m.1)

#2 - Do men take up a disproporitionate amount of time during Q&A sessions?
MaleTotalTime<-sum(con_q[which(con_q$gender.asker==1),]$time.asker.total, na.rm =T)
FemaleTotalTime<-sum(con_q[which(con_q$gender.asker==2),]$time.asker.total, na.rm =T)
MaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesMale
FemaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesFemale

table2<-rbind(c(MaleTotalTime,FemaleTotalTime),c(MaleExpectedTime,FemaleExpectedTime))
chisq.test(table2)

#3 - Are there gender differences in the ratio of length of question to length of response?
t.3 <- t.test(Speak.Listen.Ratio ~ gender.asker_ec, data = con)
t.3

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
m.6 <- glm(challenge ~ gender.asker_ec, data = con_q)
summary(m.6)

#7 - Are men more likely to exhibit behaviors of peacocking? (see lines 92-100 for construction of this variable)
t.7 <- t.test(Peacocking ~ gender.asker, data = con_q)
t.7

#8 - Are men more likely to make contirbutions that are only comments compared to women?

m.8 <- glm(CommentOnly ~ gender.asker_ec, data = con_q)
summary(m.8)

####Additional Analyses####

## Frequency of codes to determine if there's enough variance to analyze.
## Codes with 5 or fewer occurences were determined to be too infrequent to analyze.
        
count(con$IQCquestion) #happens 26 times
count(con$IQCcomment) #happens 14 times         
count(con$IQCcomquest) #happens 4 times
count(con$IQCquestcom) #happens 2 times
count(con$FQCquestion) #happens 3 times
count(con$FQCcomment) #happens 3 times       
count(con$FQCcomquest) #does not happen
count(con$CommentOnly) #happens 10 times
count(con$NoQuestions) #happens 16 times
count(con$thankyou.talk)  #happens 10 times  
count(con$thankyou.response) #happens 1 time  
count(con$pos.comment) #happens 11 times
count(con$pass.retain) # retain 23, pass 10         
count(con$talk.over.speaker) #happens twice
count(con$reference.research) #happens 11 times       
count(con$research.valence) #6 neutral, 5 positive
count(con$challenge) #happens 7 times           
count(con$ThankYouEver) #happens 10 times
count(con$Nice) #happens 16 times    
count(con$SumFollowUp) #happens 5 times   

## The following variables should not be looked at on their own because there is no variance:
## IQCcomquest, IQCquestcom, FQCquestion, FQCcomquest, FQCcomquest, thankyou.response,
## talk.over.speaker, SumFollowUp

# Do men take more space than women in Q&As? ####

### When at the mic, do men take more time than women?

m.1 <- glm.nb(time.asker.total_clean ~ gender.asker, data = con_q) #used clean variable where all values are whole numbers so negative binomial could be used 
summary(m.1) #men asked significantly longer questions than women

### Do men provide more contributions during their initial question asking period than women?

m.2 <- glm.nb(SumInitialRemarks ~ gender.asker, data = con_q) 
summary(m.2) #negative binomial didn't converge, so testing with a t-test

t.2 <- t.test(SumInitialRemarks ~ gender.asker, data = con_q) 
t.2 #no significant differences

### Do men provide more contributions than women per mic time?

m.3 <- glm.nb(SumRemarks ~ gender.asker, data = con_q) 
summary(m.3) #negative binomial didn't converge, so testing with a t-test

t.3 <- t.test(SumRemarks ~ gender.asker, data = con_q) # do men provide more contributions per mic time) 
t.3 #no significant differences

### Do men take more total time than women as compared to base rates?

MaleTotalTime<-sum(con_q[which(con_q$gender.asker==1),]$time.asker.total, na.rm =T)
FemaleTotalTime<-sum(con_q[which(con_q$gender.asker==2),]$time.asker.total, na.rm =T)
MaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesMale
FemaleExpectedTime<-(MaleTotalTime+FemaleTotalTime)*ProportionAttendeesFemale

table4<-rbind(c(MaleTotalTime,FemaleTotalTime),c(MaleExpectedTime,FemaleExpectedTime))
chisq.test(table4) #men used more question time than women as compared to base rates

### Are men more likely to be earlier to ask questions during Q&A time?

t.5 <- t.test(question.symposium ~ gender.asker, data = con_q) 
t.5 #no differences, but potentially because men asked more questions at all points

#### graph of when men and women asked questions

d.tmsbg <- ggplot(con_q, aes(x=gender.asker, y=question.symposium)) +
  geom_jitter(size=2, position = position_jitter(width = .2)) + 
  theme_bw() +
  theme(strip.text.x = element_text(size = 14)) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  stat_summary(fun.y=mean, geom='point', size=4, color="black", fill="black") + 
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', fun.args=list(conf.int=.95), size=1.5, color="black") + 
  labs(x="", y= "Question Position")+
  theme(axis.title.y = element_text(size=18)) +
  scale_color_manual(breaks = c("White", "Black"), values = c("grey80", "grey40")) +
  theme(axis.text.y=element_text(color = "black", size = 14))+
  theme(axis.text.x=element_blank(),  axis.ticks = element_blank())+
  theme(legend.position = "right") +
  theme(legend.title=element_text(size=14)) +
  theme(legend.text=element_text(size=12)) +
  theme(axis.line= element_line(color="black")) +
  theme(axis.ticks.y = element_line(color="black"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank()) +
  theme(plot.background = element_rect(fill = "white"))
d.tmsbg

### likely to be more informative of an analysis to limit to a meaningful chunk.

FirstFour<-subset(con_q, question.symposium < 5)
TotalFirstFour<-length(FirstFour$question.symposium)

MaleFirstFour<-length((subset(FirstFour, gender.asker == 1))$question.symposium)
FemaleFirstFour<-length((subset(FirstFour, gender.asker == 2))$question.symposium)

table5<-rbind(c(MaleFirstFour,FemaleFirstFour),c(TotalFirstFour*ProportionAttendeesMale,TotalFirstFour*ProportionAttendeesFemale))
chisq.test(table5) #no significant differences, but pattern is in the predicted direction

### Do men spend more time asking questions as compared to the answers they receive compared to women?

t.6 <- t.test(Speak.Listen.Ratio ~ gender.asker, data = con)
t.6 #no significant differences, but pattern is in the predicted direction

### Are men more likely to hold onto the mic after asking their question compared to women?

m.7 <- glm(retain ~ gender.asker, data = con_q)
summary(m.7) #no significant differences
table(con_q$retain, con_q$gender.asker)

# Are women more polite/respectful than men? ####

### Are women more likely than men to say thank you?

m.1 <- glm(ThankYouEver ~ gender.asker, data = con_q) 
summary(m.1) #no significant differences
table(con_q$ThankYouEver, con_q$gender.asker)

### Are women more likely than men to start their question with a positive comment about the talk?

m.2 <- glm(pos.comment ~ gender.asker, data = con_q)
summary(m.2) #no significant differences
table(con_q$pos.comment, con_q$gender.asker)

### Are women more likely than men to say polite pleasantries during their mic time?

m.3 <- glm(Nice ~ gender.asker, data = con_q)
summary(m.3) #no significant differences
table(con_q$Nice, con_q$gender.asker)

### Do women say more polite pleasantries than men during their mic time ?

m.4a <- glm.nb(SumNice ~ gender.asker, data = con_q)
m.4b <- zeroinfl(SumNice ~ gender.asker, data = con_q)
vuong(m.4a, m.4b) #model 2 is a better fit than model 1
summary(m.4b) #no significant differences
table(con_q$SumNice, con_q$gender.asker)

### Are women mre likely than men to pass the mic after they finished asking their question?

m.5 <- glm(pass ~ gender.asker, data = con_q)
summary(m.5) #men were more likely to pass the mic than women
table(con_q$pass, con_q$gender.asker)

### Are men more likely to make contirbutions that are only comments compared to women?

m.6 <- glm(CommentOnly ~ gender.asker, data = con_q)
summary(m.6) #no significant differences, but pattern is in the predicted direction
table(con_q$CommentOnly, con_q$gender.asker)

# Do men peacock more than women? ####

### Full composite

t.1 <- t.test(Peacocking ~ gender.asker, data = con_q) 
t.1 #no significant differences, but pattern is in the predicted direction

### Time introducing themselves

m.2a <- glm.nb(time.intro_clean ~ gender.asker, data = con_q)
m.2b <- zeroinfl(time.intro_clean ~ gender.asker, data = con_q)
vuong(m.2a, m.2b) #model 2 is better than model 1
summary(m.2b) #no significant differences

### No questions

m.3 <- glm(NoQuestions ~ gender.asker, data = con_q)
summary(m.3) #men were more likely to not ask any questions
table(con_q$NoQuestions, con_q$gender.asker)

### Time asking initial question

m.4 <- glm.nb(time.question_clean ~ gender.asker, data = con_q)
summary(m.4) #men spent more time on their initial question than women

### Challenging the speaker

m.5 <- glm(challenge ~ gender.asker, data = con_q)
summary(m.5) #no significant differences, but 7/8 of the challenges were men
table(con_q$challenge, con_q$gender.asker)

## Can't test these individually because the frequency is too low

table(con_q$talk.over.speaker, con_q$gender.asker)
table(con_q$pos.research, con_q$gender.asker)

# Are question askers more respectful to male speakers than female speakers ####

### Are question askers more likely to say thank you ever to male speakers than female speakers?

m.1 <- glm(ThankYouEver ~ gender.speaker, data = con) 
summary(m.1) #no significant differences
table(con$ThankYouEver, con$gender.speaker)

### Are question askers more likely to start their response with a positive comment about the talk for male as opposed to female speakers?

m.2 <- glm(pos.comment ~ gender.speaker, data = con)
summary(m.2) #no siginificant differences
table(con$pos.comment, con$gender.speaker)

### Are question askers more likely to say polite pleasantries during their mic time when the speaker is male as opposed to female?

m.3 <- glm(Nice ~ gender.speaker, data = con)
summary(m.3) #nosignificant differences
table(con$Nice, con$gender.speaker)

### Are question askers more likely to say more polite pleasantries during their mic time when the speaker is male as opposed to female?

m.4a <- glm.nb(SumNice ~ gender.speaker, data = con)
m.4b <- zeroinfl(SumNice ~ gender.speaker, data = con)
vuong(m.4a, m.4b) #model 1 is better than model 2
summary(m.4a) #no significant differences
table(con$SumNice, con$gender.speaker)

### Are question askers more likely to challenge the speaker if the speaker is female as opposed to male?

m.5 <- glm(challenge ~ gender.speaker, data = con)
summary(m.5) #no significant differences
table(con$challenge, con$gender.speaker)

### Are question askers more likely to peacock if the speaker is female as opposed to male?

t.6 <- t.test(Peacocking ~ gender.speaker, data = con)
t.6 #no significant differences, but effect is in the predicted direction

### Are question askers more likely to have longer questions than the answers they receive if the speaker is female as opposed to male?

t.7 <- t.test(Speak.Listen.Ratio ~ gender.speaker, data = con)
t.7 #no significant differences

### Are question askers more likely to not ask a question if the speaker is female as opposed to male?

m.8 <- glm(NoQuestions ~ gender.speaker, data = con)
summary(m.8) #no significant differences
table(con$NoQuestions, con$gender.speaker)

### Are question askers more likely to only provide comments if the speaker is female as opposed to male?

m.9 <- glm(CommentOnly ~ gender.speaker, data = con)
summary(m.9) #no significant differences, but effect is in the predicted direction
table(con$CommentOnly, con$gender.speaker)

### Do men get asked more questions than women?

MaleQuestions<-nrow(con[which(con$gender.speaker==1),])
FemaleQuestions<-nrow(con[which(con$gender.speaker==2),])
MaleExpectedQuestions<-(MaleQuestions+FemaleQuestions)*ProportionSpeakersMale
FemaleExpectedQuestions<-(MaleQuestions+FemaleQuestions)*ProportionSpeakersFemale

table8<-rbind(c(MaleQuestions,FemaleQuestions),c(MaleExpectedQuestions,FemaleExpectedQuestions))
chisq.test(table8) #no significant differences

### Can't test talk over the speaker because it was too infrequent of an outcome.

## Moderation by Participant Gender -- Are male question askers less respectful to female speakers? ####

### Thank yous

m.1 <- glm(ThankYouEver ~ gender.speaker*gender.asker, data = con) 
summary(m.1) #no significant differences

### Initial positive comments

m.2 <- glm(pos.comment ~ gender.speaker*gender.asker, data = con)
summary(m.2) #main effect of gender of speaker - positive comments more likely to go to female speakers
table(con$pos.comment, con$gender.speaker, con$gender.asker)

### Any positive pleasantries 

m.3 <- glm(Nice ~ gender.speaker*gender.asker, data = con)
summary(m.3) #no significant differences

### More positive pleasantries

m.4a <- glm.nb(SumNice ~ gender.speaker*gender.asker, data = con) #negative binomial did not converge
m.4b <- zeroinfl(SumNice ~ gender.speaker*gender.asker, data = con)
summary(m.4b) #no significant differences

### Challenge the speaker

m.5 <- glm(challenge ~ gender.speaker*gender.asker, data = con)
summary(m.5) #main effect of gender of speaker - challenges more likely to go to female speakers, trending interaction that men are more likely to challenge female speakers
table(con$challenge, con$gender.speaker, con$gender.asker)

### Peacocking

m.6 <- lm(Peacocking ~ gender.speaker*gender.asker, data = con)
summary(m.6) #no significant differences

### Speak to listen ratio

m.7 <- lm(Speak.Listen.Ratio ~ gender.speaker*gender.asker, data = con)
summary(m.7) #no significant differences

### No questions

m.8 <- glm(NoQuestions ~ gender.speaker*gender.asker, data = con)
summary(m.8) #no significant differences
table(con$NoQuestions, con$gender.speaker)

### Comments only

m.9 <- glm(CommentOnly ~ gender.speaker*gender.asker, data = con)
summary(m.9) #no significant differences
table(con$CommentOnly, con$gender.speaker)

### Can't test talk over the speaker because it was too infrequent of an outcome.


