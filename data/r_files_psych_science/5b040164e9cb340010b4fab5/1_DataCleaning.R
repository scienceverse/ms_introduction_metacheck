############### DATA PREPARATION EXP 2 PART 1: Data Cleaning  ##############
## Last update: 12/12/2017 ##
## Author: Pieter Van Dessel ##
## Made in: RStudio Version 0.98.1103 ##
## R version 3.2.5 (2016-04-14) ##

##### Load libraries ####
library(Hmisc)
library(BayesFactor)
library (reshape2)

##### 1. Read in Raw Data  ####

#read in  data
Data<-read.csv("../../02RawData/Experiment 1/Experiment2.dat", header=TRUE, sep='\t')
Data2<-read.csv("../../02RawData/Experiment 1/C2Experiment2.dat", header=TRUE, sep='\t')
Data<-rbind(Data,Data2)
describe(Data) # so 72 pps 43 vars 15300 observs
table(Data$subject) #213 obs/pp

## check out nr of trials per ppt/block
table(Data$subject,Data$blocknum)
table(Data$blockcode,Data$blocknum)

Data$blocknum<-ifelse(Data$subject==8,Data$blocknum-2,Data$blocknum)
Dataraw<-Data

##### 2. Prepare PreAMP data file #### 

#only keep important vars: subject, block, trial, blockcode, trialcode, response, lat, target
head(Data)
Data<-Data[c(3,5,6,7,8,15)] 
colnames(Data)=c("subject", "blocknumber", "trialnumber","blockcode", "trialcode","response")

#exclude pps with uncompleted AMP (<60 trials)
Data<-Data[Data$blockcode=='AMPPre',] 
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b1<-b[b$n<60|b$n>60,]
Data<-Data[!(Data$subject %in% b1$number),]
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b # 72 pps (=0 pps excluded)

##### 3. Make aggregated data-file with explicit ratings and pp nr #######

## read in rawdata: 
Data2<-Dataraw

## only keep important vars: subject, rated face, response
Data2<-Data2[Data2$blockcode=='explicitPre',]
Data2<-Data2[c(3,8,15)] 
head(Data2)

## Create aggregatedDatafile with explicit ratings
DataAgg<-dcast(Data2, subject ~ trialcode, value.var="response")
head(DataAgg)
colnames(DataAgg)=c("subject", "qBF_pre", "qBL_pre","qBT_pre", "qJF_pre", "qJL_pre","qJT_pre")

## read in rawdata: 
Data2<-Dataraw

## only keep important vars: subject, rated face, response
Data2<-Data2[Data2$blockcode=='explicitPost',]
Data2<-Data2[c(3,8,15)] 
head(Data2)

## Create aggregatedDatafile with explicit ratings
Data2<-dcast(Data2, subject ~ trialcode, value.var="response")
head(Data2)
colnames(Data2)=c("subject", "qBF_post", "qBL_post","qBT_post", "qJF_post", "qJL_post","qJT_post")
DataAgg<-merge(DataAgg,Data2,by="subject")
describe(DataAgg)

##### 4. Add condition variables to Aggregated data file: EC and hypno #######

## read in rawdata for EC:
Data2<-Dataraw
Data2<-Data2[Data2$blocknum==4&Data2$trialnum==1,] 

## only keep important vars: subject, blockcode
Data2<-Data2[c(3,7)] 
head(Data2)

## create EC variable: 1= Bobpos,2=Janpos
Data2$EC<-ifelse(Data2$blockcode=='ECBobpos',1,0)
Data2$EC<-ifelse(Data2$blockcode=='ECJanpos',2,Data2$EC)
table(Data2$EC)
DataAgg<-merge(DataAgg,Data2,by="subject")

## create hypnosis variable: 1= yes,2=no
DataAgg$hypnosis<-ifelse(DataAgg$subject%%4==1|DataAgg$subject%%4==2, 1,2)

table(DataAgg$hypnosis,DataAgg$EC)

##### 5. add answers to additional questions #######

## read in rawdata for hy:
Data2<-Dataraw
Data2<-Data2[Data2$blockcode=='hy',] 

## only keep important vars: subject, response
Data2<-Data2[c(3,15)] 
head(Data2)
colnames(Data2)=c("subject", "hypn")
DataAgg2<-merge(DataAgg,Data2,by="subject")
DataAgg<-DataAgg[!(DataAgg$subject %in% DataAgg2$subject),]
DataAgg$hypn<-NA
DataAgg<-rbind(DataAgg,DataAgg2)
head(DataAgg)
describe(DataAgg)

## read in rawdata for hyp_scored:
Data2<-read.csv("../../02RawData/Experiment 1/questions1.dat", header=TRUE, sep='\t')
Data3<-read.csv("../../02RawData/Experiment 1/C2questions1.dat", header=TRUE, sep='\t')
Data2<-rbind(Data2,Data3) 

## only keep important vars: subject, hyp_scored
Data2<-Data2[c(3,4)] 
head(Data2)
colnames(Data2)=c("subject", "hyp_scored")
DataAgg2<-merge(DataAgg,Data2,by="subject")
DataAgg<-DataAgg[!(DataAgg$subject %in% DataAgg2$subject),]
DataAgg$hyp_scored<-NA
DataAgg<-rbind(DataAgg,DataAgg2)
head(DataAgg)
describe(DataAgg)

## read in rawdata for react en suggest:
Data2<-read.csv("../../02RawData/Experiment 1/questions1_.dat", header=TRUE, sep='\t')
Data3<-read.csv("../../02RawData/Experiment 1/C2questions1_.dat", header=TRUE, sep='\t')
Data2<-rbind(Data2,Data3)
Data2$react<-Data2$react1_response+Data2$react2_response+Data2$react3_response+Data2$react4_response+Data2$react5_response+Data2$react6_response+Data2$react7_response+Data2$react8_response+Data2$react9_response+Data2$react10_response+Data2$react11_response
Data2$suggest<-Data2$suggest1_response+Data2$suggest2_response+Data2$suggest3_response+Data2$suggest4_response+Data2$suggest5_response+Data2$suggest6_response+Data2$suggest7_response+Data2$suggest8_response+Data2$suggest9_response+Data2$suggest10_response+Data2$suggest11_response+Data2$suggest12_response
Data2<-Data2[c(3,52,53)] 
head(Data2)

Data3<-read.csv("../../02RawData/Experiment 1/questions2.dat", header=TRUE, sep='\t')
Data4<-read.csv("../../02RawData/Experiment 1/C2questions2.dat", header=TRUE, sep='\t')
Data3<-rbind(Data3,Data4)
Data3$react<-Data3$react1_response+Data3$react2_response+Data3$react3_response+Data3$react4_response+Data3$react5_response+Data3$react6_response+Data3$react7_response+Data3$react8_response+Data3$react9_response+Data3$react10_response+Data3$react11_response
Data3$suggest<-Data3$suggest1_response+Data3$suggest2_response+Data3$suggest3_response+Data3$suggest4_response+Data3$suggest5_response+Data3$suggest6_response+Data3$suggest7_response+Data3$suggest8_response+Data3$suggest9_response+Data3$suggest10_response+Data3$suggest11_response+Data3$suggest12_response
Data3<-Data3[c(3,60,61)] 
head(Data3)

Data3<-rbind(Data2,Data3)
describe(Data3)
DataAgg<-merge(DataAgg,Data3,by="subject")
describe(DataAgg)

##### 6.  compute contingency awareness #######

## read in rawdata for contingency
Data2<-read.csv("../../02RawData/Experiment 1/questions1.dat", header=TRUE, sep='\t')
Data3<-read.csv("../../02RawData/Experiment 1/C2questions1.dat", header=TRUE, sep='\t')
Data2<-rbind(Data2,Data3)
describe(Data2) # 37 pps
head(Data2)

#recode vars:
Data2<-Data2[c(3,6,8,10,12)] 
colnames(Data2)=c("subject", "qB_EC", "qJ_EC","qB_CA", "qJ_CA")

## read in rawdata for contingency
Data3<-read.csv("../../02RawData/Experiment 1/questions2.dat", header=TRUE, sep='\t')
Data4<-read.csv("../../02RawData/Experiment 1/C2questions2.dat", header=TRUE, sep='\t')
Data3<-rbind(Data3,Data4)
describe(Data3) # 35 pps
head(Data3)

#recode vars:
Data3<-Data3[c(3,5,7,9,11)] 
colnames(Data3)=c("subject", "qB_EC", "qJ_EC","qB_CA", "qJ_CA")

Data3<-rbind(Data2,Data3)
describe(Data3)

DataAgg<-merge(DataAgg,Data3,by="subject")
describe(DataAgg)
table(DataAgg$subject) 

DataAgg$contECcorrect<-ifelse((DataAgg$qB_EC=='Deze foto werd steeds gepresenteerd met POSITIEVE prenten en woorden')&(DataAgg$EC==1),1,0)
DataAgg$contECcorrect<-ifelse((DataAgg$qB_EC=='Deze foto werd steeds gepresenteerd met NEGATIEVE prenten en woorden')&(DataAgg$EC==2),DataAgg$contECcorrect+1,DataAgg$contECcorrect)
DataAgg$contECcorrect<-ifelse((DataAgg$qJ_EC=='Deze foto werd steeds gepresenteerd met POSITIEVE prenten en woorden')&(DataAgg$EC==2),DataAgg$contECcorrect+1,DataAgg$contECcorrect)
DataAgg$contECcorrect<-ifelse((DataAgg$qJ_EC=='Deze foto werd steeds gepresenteerd met NEGATIEVE prenten en woorden')&(DataAgg$EC==1),DataAgg$contECcorrect+1,DataAgg$contECcorrect)

table(DataAgg$contECcorrect) #72 cont aware
table(DataAgg$qB_CA,DataAgg$subject)

DataAgg$contCAcorrect<-ifelse((DataAgg$qB_CA=='Deze persoon is gemeen en onaangenaam')&(DataAgg$EC==1),1,0)
DataAgg$contCAcorrect<-ifelse((DataAgg$qB_CA=='Deze persoon is lief en aangenaam')&(DataAgg$EC==2),DataAgg$contCAcorrect+1,DataAgg$contCAcorrect)
DataAgg$contCAcorrect<-ifelse((DataAgg$qJ_CA=='Deze persoon is gemeen en onaangenaam')&(DataAgg$EC==2),DataAgg$contCAcorrect+1,DataAgg$contCAcorrect)
DataAgg$contCAcorrect<-ifelse((DataAgg$qJ_CA=='Deze persoon is lief en aangenaam')&(DataAgg$EC==1),DataAgg$contCAcorrect+1,DataAgg$contCAcorrect)

table(DataAgg$contCAcorrect) #63 cont aware

##### 7. In Pre AMP data file: do AMP data cutting #######

## exclude pps in amp data file with incomplete data

Data<-Data[(Data$subject %in% DataAgg$subject),]
describe(Data)# 72 pps completed our exp ! incomplete: 0 / 72: 0%
describe(DataAgg)

## discard data if all pleasant or unpleasant:

D1<-Data
ar<-table(D1$subject,D1$response)
c1<-as.data.frame(ar)
c2<-c1[c1$Var2=='18'|c1$Var2=='23',]
c2<-c2[c2$Freq==0,]

Data<-Data[!(Data$subject %in% c2$Var1),]

describe(Data) # 72 pps ! AMP excluded pps: 0

#### 8. compute Pre AMP scores #######

#compute AMP scores Bob:
Data1<-Data[Data$trialcode=='AMPBob',]
Data1$resp<-ifelse(Data1$response=='18',0,1)
t<-aggregate(Data1$resp, list(Data1$subject), mean)
colnames(t)=c("subject", "AMPPre_Bob")
DataAgg<-merge(DataAgg,t, by="subject")

#compute AMP scores Jan:
Data1<-Data[Data$trialcode=='AMPJan',]
Data1$resp<-ifelse(Data1$response=='18',0,1)
t<-aggregate(Data1$resp, list(Data1$subject), mean)
colnames(t)=c("subject", "AMPPre_Jan")
DataAgg<-merge(DataAgg,t, by="subject")
describe(DataAgg)

##### 9. Read in Raw Post AMP Data  ####

#read in  data
Data<-Dataraw

##### 10. Prepare PostAMP data file #### 

#only keep important vars: subject, block, trial, blockcode, trialcode, response, lat, target
head(Data)
Data<-Data[c(3,5,6,7,8,15)] 
colnames(Data)=c("subject", "blocknumber", "trialnumber","blockcode", "trialcode","response")

#exclude pps with uncompleted AMP (<60 trials)
Data<-Data[Data$blockcode=='AMPPost',] 
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b1<-b[b$n<60|b$n>60,]
Data<-Data[!(Data$subject %in% b1$number),]
a <- rle(sort(Data$subject))
b <- data.frame(number=a$values, n=a$lengths)
b # 72 pps (=0 pps excluded)

##### 11. In Post AMP data file: do AMP data cutting #######

## exclude pps in amp data file with incomplete data
Data<-Data[(Data$subject %in% DataAgg$subject),]
describe(Data)# 72 pps completed our exp ! incomplete: 0 / 72: 0%

## discard data if all pleasant or unpleasant:

D1<-Data
ar<-table(D1$subject,D1$response)
c1<-as.data.frame(ar)
c2<-c1[c1$Var2=='18'|c1$Var2=='23',]
c2<-c2[c2$Freq==0,]

Data<-Data[!(Data$subject %in% c2$Var1),]

describe(Data) # 72 pps ! AMP excluded pps: 0

#### 12. compute Pre AMP scores #######

#compute AMP scores Bob:
Data1<-Data[Data$trialcode=='AMPBob',]
Data1$resp<-ifelse(Data1$response=='18',0,1)
t<-aggregate(Data1$resp, list(Data1$subject), mean)
colnames(t)=c("subject", "AMPPost_Bob")
DataAgg<-merge(DataAgg,t, by="subject")

#compute AMP scores Jan:
Data1<-Data[Data$trialcode=='AMPJan',]
Data1$resp<-ifelse(Data1$response=='18',0,1)
t<-aggregate(Data1$resp, list(Data1$subject), mean)
colnames(t)=c("subject", "AMPPost_Jan")
DataAgg<-merge(DataAgg,t, by="subject")

##### 13. write away final data file #######

write.table(DataAgg, "Data1.txt", sep="\t") 
