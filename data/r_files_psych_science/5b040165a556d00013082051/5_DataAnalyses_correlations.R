############### DATA ANALYSES EXP 2 PART 3: correlations ##############
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

##### 1. Read in final data-file ####

Data<-read.table("Data1.txt")
describe(Data) #72 pps

##### 2. correlations #####

## Compute Variables
Data$exp_Bobpre<-(Data$qBF_pre+Data$qBL_pre+Data$qBT_pre)/3
Data$exp_Bobpost<-(Data$qBF_post+Data$qBL_post+Data$qBT_post)/3

Data$exp_Janpre<-(Data$qJF_pre+Data$qJL_pre+Data$qJT_pre)/3
Data$exp_Janpost<-(Data$qJF_post+Data$qJL_post+Data$qJT_post)/3

Data$exp_pre<-ifelse(Data$EC==1,Data$exp_Bobpre-Data$exp_Janpre,Data$exp_Janpre-Data$exp_Bobpre)
Data$exp_post<-ifelse(Data$EC==1,Data$exp_Bobpost-Data$exp_Janpost,Data$exp_Janpost-Data$exp_Bobpost)

Data$AMP_pre<-ifelse(Data$EC==1,Data$AMPPre_Bob-Data$AMPPre_Jan,Data$AMPPre_Jan-Data$AMPPre_Bob)
Data$AMP_post<-ifelse(Data$EC==1,Data$AMPPost_Bob-Data$AMPPost_Jan,Data$AMPPost_Jan-Data$AMPPost_Bob)

## correlate implicit explicit

cor.test(Data$exp_pre,Data$AMP_pre)   # .28*
cor.test(Data$exp_post,Data$AMP_post) # .59*

##### 3. correlations only hypnosis cdt#####

Data1<-Data[Data$hypnosis==1,]#hypno
Data2<-Data[Data$hypnosis==2,]#ctr

cor.test(Data1$exp_post,Data1$hypn) #-0.30
cor.test(Data1$AMP_post,Data1$hypn) #-0.14

cor.test(Data1$exp_post,Data1$react) #0.32
cor.test(Data1$AMP_post,Data1$react) #0.06

cor.test(Data2$exp_post,Data2$react) #-0.06
cor.test(Data2$AMP_post,Data2$react) #-0.25

cor.test(Data1$exp_post,Data1$suggest) #-0.15
cor.test(Data1$AMP_post,Data1$suggest) #-0.14

cor.test(Data2$exp_post,Data2$suggest) #0.08
cor.test(Data2$AMP_post,Data2$suggest) #0.04

Data1$hy<-c(7,5,4,5,6,6,5,6,5,4,4,6,5,4,6,5,9,6,6,6,3,6,5,5,5,4,5,5,7,7,6,6,6,6,7,6)

cor.test(Data1$hypn,Data1$hy) #-0.04

cor.test(Data1$exp_post,Data1$hy) #-0.10
cor.test(Data1$AMP_post,Data1$hy) #-0.29

##contingency aware
Data3<-Data[Data$contECcorrect==2,] #72
