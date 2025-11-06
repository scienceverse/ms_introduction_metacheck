############### DATA ANALYSES EXP 2 PART 2: AMP ##############
## Last update: 12/12/2017 ##
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
library(lme4)

##### 1. Read in final data-file ####

Data<-read.table("Data1.txt")
describe(Data) #72 pps

##### 2. Prepare ANOVA AMP #####

## Compute Variables
Data$AMP_pre<-ifelse(Data$EC==1,Data$AMPPre_Bob-Data$AMPPre_Jan,Data$AMPPre_Jan-Data$AMPPre_Bob)
Data$AMP_post<-ifelse(Data$EC==1,Data$AMPPost_Bob-Data$AMPPost_Jan,Data$AMPPost_Jan-Data$AMPPost_Bob)

## t-test
Data1<-Data[Data$hypnosis==1,]#hyp
Data2<-Data[Data$hypnosis==2,]#ct

t.test(Data1$AMP_post,Data2$AMP_post, alternative="less") #p.009
ttestBF(Data1$AMP_post,Data2$AMP_post, rscale=0.56, nullInterval = c(0, Inf)) #5.99
cohensD(Data1$AMP_post-Data2$AMP_post) #d=0.41

mean(Data1$AMP_post)
sd(Data1$AMP_pre)
mean(Data2$AMP_post)
sd(Data2$AMP_pre)

hist(Data1$AMP_post)
hist(Data2$AMP_post)

hist(Data1$AMP_pre)
hist(Data2$AMP_pre)

##### 3. ANOVA AMP #####

## repeated-measures ANOVA SPSS
Time = factor(c("V1", "V2"))

options(contrasts = c("contr.sum","contr.poly"))

Data$hypnosis<-factor(Data$hypnosis)
Data$EC<-factor(Data$EC)
summary(Data$hypnosis)
summary(Data$suggest)
summary(Data$react)

Anv1 = lm(cbind(Data$AMP_pre, Data$AMP_post)~Data$hypnosis*Data$EC)
Anv2 = Anova(Anv1, idata=data.frame(Time), idesign=~Time, type=3)
summary(Anv2, multivariate=F) # hypno:p.066, Time p<.001, interact: p.065

etasq(Anv2, anova = TRUE)
ci.pvaf(F.value=3.5206, df.1=1, df.2=68, N=72, alpha.lower=.025,alpha.upper=.025)

## follow-up Time effect: p<.001

t.test(Data$AMP_pre,Data$AMP_post)

## follow-up Time x hypnosis effect: 

Data1<-Data[Data$hypnosis==1,]#hypno
t.test(Data1$AMP_pre,Data1$AMP_post, paired=T) #t=5.31, p<.001
hist(Data1$AMP_pre)
hist(Data1$AMP_post)

Data2<-Data[Data$hypnosis==2,]#ctr
t.test(Data2$AMP_pre,Data2$AMP_post, paired=T) #t=3.01, p.005
ttestBF(Data2$AMP_pre-Data2$AMP_post, rscale=0.56, nullInterval = c(-Inf, 0)) # BF1=10.02

hist(Data2$AMP_pre)
hist(Data2$AMP_post)

boxplot(Data1$AMP_pre,Data2$AMP_pre,Data1$AMP_post,Data2$AMP_post)
plot(AMP_pre~hypnosis, Data)
plot(AMP_post~hypnosis, Data)

## compare two hypnosis groups
t.test(Data1$AMP_post,Data2$AMP_post, alternative="less") #p.009
cohensD(Data1$AMP_post-Data2$AMP_post) 
1/ttestBF(Data1$AMP_pre-Data2$AMP_pre, rscale=0.56, nullInterval = c(-Inf, 0)) # BF1=10.02
sd(Data1$AMP_post)
sd(Data2$AMP_post)

t.test(Data1$AMP_pre,Data2$AMP_pre, alternative="less", var.equal=T) #p.50
sd(Data1$AMP_pre)
sd(Data2$AMP_pre)
cohensD(Data1$AMP_pre-Data2$AMP_pre) 
1/ttestBF(Data1$AMP_pre-Data2$AMP_pre, rscale=0.56, nullInterval = c(-Inf, 0)) # BF1=10.02

## follow-up reactance and suggestibility

fit <- lm(AMP_post~hypnosis*EC+suggest+react,data=Data, contrasts = list(hypnosis="contr.sum", EC="contr.sum"))
Anova(fit, type='III') 
effect('hypnosis', fit)         # p.020
