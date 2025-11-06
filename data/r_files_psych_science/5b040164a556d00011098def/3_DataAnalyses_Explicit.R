############### DATA ANALYSES EXP 3 PART 1: explicit ##############
## Last update: 19/10/2017 ##
## Author: Pieter Van Dessel ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)
library(car)
library(effects)
library(BayesFactor)
library(lsr)
library(ez)
library(reshape2)
library(psy)
library(heplots)
library(MBESS)

##### 1. Read in final data-file ####

Data<-read.table("Data1.txt")
describe(Data) #72 pps
head(Data)

##### 2. Prepare ANOVA explicit #####

## Compute Variables for the two targets
Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
a<-cronbach(Data[c(2:4)])$alpha

Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3
b<-cronbach(Data[c(8:10)])$alpha

Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3
c<-cronbach(Data[c(5:7)])$alpha

Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3
d<-cronbach(Data[c(11:13)])$alpha

(a+b+c+d)/4 # cronbach's alpha .94

## compute EC explicit scores
Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)
Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)

##t-test
Data1<-Data[Data$hypnosis==1,]#hyp
Data2<-Data[Data$hypnosis==2,]#ct

t.test(Data1$exp_post,Data2$exp_post, alternative="less") #p.043
ttestBF(Data1$exp_post,Data2$exp_post, rscale=0.56, nullInterval = c(0, Inf)) #1.86
cohensD(Data1$exp_post-Data2$exp_post) #d=0.34

mean(Data1$exp_post)
sd(Data1$exp_post)
mean(Data2$exp_post)
sd(Data2$exp_post)

hist(Data1$exp_post)
hist(Data2$exp_post)

hist(Data1$exp_pre)
hist(Data2$exp_pre)

##### 3. ANOVA explicit #####

## repeated-measures ANOVA SPSS
Time = factor(c("V1", "V2"))

options(contrasts = c("contr.sum","contr.poly"))

Data$hypnosis<-factor(Data$hypnosis)
Data$EC<-factor(Data$EC)

Anv1 = lm(cbind(Data$exp_pre, Data$exp_post)~Data$hypnosis*Data$EC)
Anv2 = Anova(Anv1, idata=data.frame(Time), idesign=~Time, type=3)
summary(Anv2, multivariate=F) # hypno:p.27, Time p<.001, interact: p.11

etasq(Anv2, anova = TRUE)
ci.pvaf(F.value=2.6741, df.1=1, df.2=68, N=72, alpha.lower=.025,alpha.upper=.025)

## add reactance and suggestibility

Anv1 = lm(cbind(Data$exp_pre, Data$exp_post)~Data$hypnosis+Data$suggest+Data$react)
Anv2 = Anova(Anv1, idata=data.frame(Time), idesign=~Time, type=3)
summary(Anv2, multivariate=F) # hypno:p.25, Time p.029, interact: p.17, react:p.016

etasq(Anv2, anova = TRUE)
ci.pvaf(F.value=1.8884, df.1=1, df.2=68, N=72, alpha.lower=.025,alpha.upper=.025)

#follow-up Time effect: p<.001

t.test(Data$exp_pre,Data$exp_post)

## follow-up Time x hypnosis effect: 

Data1<-Data[Data$hypnosis==1,]#hypno
t.test(Data1$exp_pre,Data1$exp_post, paired=T) #t=6.35, p<.001
hist(Data1$exp_pre)
hist(Data1$exp_post)

Data2<-Data[Data$hypnosis==2,]#ctr
t.test(Data2$exp_pre,Data2$exp_post, paired=T) #t=5.46, p<.001
hist(Data2$exp_pre)
hist(Data2$exp_post)

boxplot(Data1$exp_pre,Data2$exp_pre,Data1$exp_post,Data2$exp_post)

#compare two groups
t.test(Data1$exp_post,Data2$exp_post, alternative="less") #p.043
ttestBF(Data2$exp_post-Data1$exp_post,nullInterval = c(-Inf, 0)) # BF1=2.20
t.test(Data1$exp_pre,Data2$exp_pre, alternative="less") #p.65
1/ttestBF(Data2$exp_pre-Data1$exp_pre,nullInterval = c(-Inf, 0)) # BF0=8.06

