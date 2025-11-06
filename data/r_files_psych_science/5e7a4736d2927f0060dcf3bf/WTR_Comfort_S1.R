#WTR_Comfort Project Study 1

#Data Importing
raw<-read.csv('WTR_Comfort_S1.csv')

#Exclusion of Free Response
library(dplyr)
raw<-filter(raw,English_exclude==0)

#Recoding for Contact Comfort
rawCC<-raw[,30:39]
rawCC<-as.data.frame(as.matrix(rawCC)-4)

#Variable Computing
rawDS<-as.data.frame(as.matrix(raw[,9:15])-1)
rawHH<-raw[,20:29]

rawWTR<-raw[,c(49:40,59:50,69:60,79:70,89:80,99:90)]
rawWTR$X37_.13[which(rawWTR$X37_.13==-.45)]<-0
rawWTR$X23_.8[which(rawWTR$X23_.8==-.45)]<-0
rawWTR$X75_.26[which(rawWTR$X75_.26==-.45)]<-0
rawWTR$X19_.7[which(rawWTR$X19_.7==-.45)]<-0
rawWTR$X46_.16[which(rawWTR$X46_.16==-.45)]<-0
rawWTR$X68_.24[which(rawWTR$X68_.24==-.45)]<-0
rawWTRdummy<-as.matrix(rawWTR)
rawWTRdummy[rawWTRdummy!=0]<-1
rawWTR<-as.data.frame(rawWTRdummy)
rm(rawWTRdummy)

participantnumber <- 1
Caulsum <- matrix(,,ncol=9)
Caulsum <- Caulsum[-1,]
colnames(Caulsum) <- c("WTR37","WTR23","WTR75","WTR19","WTR46","WTR68","WTRTOTAL",'DS','HH')

wtrcal <- function(rawWTR){
  rawWTR <- as.numeric(rawWTR)
  m <- seq(-35, 145, by = 20)
  if(sum(rawWTR) == 10){
    anchorWTR = 1.55
  }else if(sum(rawWTR) == 0){
    anchorWTR = -0.45
  }else{
    shiftcount = 0
    shiftpoint = 0
    for(i in 1:9){
      if((rawWTR[i] - rawWTR[i + 1]) == 1){
        shiftcount = shiftcount + 1
        shiftpoint <- (m[i] + m[i + 1])/2 + shiftpoint
      }
    }
    if(shiftcount > 2){anchorWTR = NA}else{ 
      anchorWTR <- 0.01*shiftpoint/shiftcount
    }
  }
}

while(participantnumber <= nrow(raw)){
  dumx <- rawWTR[participantnumber,]
  dummysheetWTR <- matrix(data = dumx, ncol = 10, byrow = TRUE)
  colnames(dummysheetWTR) <- seq(-35, 145, by = 20)
  WTRAnchor <- apply(dummysheetWTR, 1, wtrcal)
  
  DSRaw <- rawDS[participantnumber,]
  HHRaw <- rawHH[participantnumber,]
  
  if(NA%in%WTRAnchor){
    WTRTotal<-NA
  }else{
    WTRTotal<-mean(WTRAnchor)
  }
  
  Caulperson <- matrix(c(WTRAnchor,WTRTotal,mean(data.matrix(DSRaw)), mean(data.matrix(HHRaw))), ncol = 9)
  
  Caulsum <- rbind(Caulsum,Caulperson)
  
  participantnumber<-participantnumber+1}

Caulsum<-as.data.frame(Caulsum)
Caulsum<-round(Caulsum, digits = 3)

Caulsum<-mutate(Caulsum,dummy=c(1:501))
rawCC<-mutate(rawCC,dummy=c(1:501))
rawHH<-mutate(rawHH,dummy=c(1:501))
rawDS<-mutate(rawDS,dummy=c(1:501))
Caulsum<-merge.data.frame(Caulsum,rawCC,by='dummy')
Caulsum<-merge.data.frame(Caulsum,rawHH,by='dummy')
Caulsum<-merge.data.frame(Caulsum,rawDS,by='dummy')

#Exclusion of Sex and Dataset Merging
Finaldata<-as.data.frame(filter(mutate(Caulsum,sex=raw$sex,age=raw$age,part_sex=raw$part_sex,relation=raw$relationship_category,
                                       income=raw$income,part_age=raw$part_age,part_leng=raw$part_leng,spa=raw$poli_soc,epa=raw$poli_econ,
                                       trust=raw$trust_gen), sex!=3))
Finaldata<-Finaldata[complete.cases(Finaldata$WTRTOTAL),]

#EFA and Crobach alpha's
library(psych,GPArotation)
fa(Finaldata[,11:20],nfactors = 1,rotate = 'oblimin',fm='ml')#CC
alpha(Finaldata[,11:20])
Finaldata<-mutate(Finaldata,CC=unname(rowMeans(select(Finaldata,comf1:comf10))))

alpha(Finaldata[,21:30])#HH
alpha(Finaldata[,31:37])#DS
alpha(Finaldata[,2:7])#WTR

#Descriptive Analysis
describe(Finaldata)

#Formatting Dataset and Cleaning Enviornment 
Finaldata$sex<-as.factor(Finaldata$sex)
Finaldata$part_sex<-as.factor(Finaldata$part_sex)
Finaldata$relation<-as.factor(Finaldata$relation)
rm(Caulperson,Caulsum,DSRaw,dummysheetWTR,dumx,HHRaw,rawCC,rawDS,rawHH,rawWTR,participantnumber,WTRAnchor,WTRTotal)
Analysisdata<-select(Finaldata,CC,WTRTOTAL,DS,HH,income,sex:part_sex,part_age,relation)

#Anova and Correlation Matrix
library(Hmisc)
library(lsr)
options(contrasts = c("contr.sum","contr.poly"))

anovaCC<-aov(Analysisdata$CC~Analysisdata$relation)
summary(anovaCC)
etaSquared(anovaCC)
rcorr(as.matrix(select(Analysisdata,CC,WTRTOTAL,DS,HH,age,income,part_age)))

rm(anovaCC)

#T-test's for Correlation Matrix
Analysisdata_ttest<-select(Analysisdata,CC,WTRTOTAL,DS,HH,age,income,part_age)
ncol_ttest<-1
ttest<-0
dummyttest<-0
while (ncol_ttest<=ncol(Analysisdata_ttest)) {
  ttest<-t.test(Analysisdata_ttest[,ncol_ttest]~Analysisdata$sex)
  dummyttest<-unname(ttest$statistic)
  print(names(Analysisdata_ttest)[ncol_ttest])
  print(dummyttest)
  print((dummyttest^2/(dummyttest^2+unname(ttest$parameter)))^0.5)
  print(ttest$p.value)
  print('---------------------------------')
  ncol_ttest<-ncol_ttest+1
  rm(ttest)
  }

ncol_ttest<-1
while (ncol_ttest<=ncol(Analysisdata_ttest)) {
  ttest<-t.test(Analysisdata_ttest[,ncol_ttest]~Analysisdata$part_sex)
  dummyttest<-unname(ttest$statistic)
  print(names(Analysisdata_ttest)[ncol_ttest])
  print(dummyttest)
  print((dummyttest^2/(dummyttest^2+unname(ttest$parameter)))^0.5)
  print(ttest$p.value)
  print('---------------------------------')
  ncol_ttest<-ncol_ttest+1
  rm(ttest)
}
rm(dummyttest,Analysisdata_ttest,ncol_ttest)

#Chisqr-test's for Correlation Matrix
#Final report was converted with the result of the current section by using
#http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-R5.php
sexchi<-select(Analysisdata,sex,part_sex)
table(sexchi)
chisq.test(table(sexchi))

rm(sexchi)
#Regression for Patial Correlations in Correlation Matrix
library(rsq)
ncol_regression<-1
ncol_regression_predictor<-0
regression_pc<-0
regression_rsq<-0
Analysisdata_pc_ctn<-select(Analysisdata,CC:HH,age,income,part_age,sex,part_sex,relation)

while(ncol_regression<=(ncol(Analysisdata_pc_ctn)-3)){
  ncol_regression_predictor<-ncol_regression+1
  while (ncol_regression_predictor<=(ncol(Analysisdata_pc_ctn)-1)) {
    regression_pc<-lm(Analysisdata_pc_ctn[,ncol_regression]~Analysisdata_pc_ctn[,ncol_regression_predictor]+Analysisdata_pc_ctn$relation,data = Analysisdata_pc_ctn)
    regression_rsq<-rsq.partial(regression_pc,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
    print(names(Analysisdata_pc_ctn)[ncol_regression])
    print(names(Analysisdata_pc_ctn)[ncol_regression_predictor])
    print(((abs(regression_rsq$partial.rsq))^.5)[1])
    print(summary(regression_pc)$coefficients[2,c(1,4)])
    ncol_regression_predictor<-ncol_regression_predictor+1
    print('---------------------------------')
  }
  ncol_regression<-ncol_regression+1
  print('---------------------------------------------------------------------------')
}

regression_pc<-glm(sex~part_sex+relation,family=binomial,data = Analysisdata_pc_ctn)
regression_rsq<-rsq.partial(regression_pc,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
print(((abs(regression_rsq$partial.rsq))^.5)[1])
print(summary(regression_pc)$coefficients[2,c(1,4)])

#Pre-registered Regression Analysis
Analysisdata<-na.omit(Analysisdata)

CR0<-lm(CC~1,Analysisdata)
CR1<-lm(CC~sex+income+part_sex+DS,Analysisdata)
CR2<-lm(CC~sex+income+part_sex+DS+WTRTOTAL+HH,Analysisdata)
CR3<-lm(CC~sex+income+part_sex+DS+WTRTOTAL+HH+relation,Analysisdata)
CR4<-lm(CC~sex+income+part_sex+DS+WTRTOTAL+HH+relation+sex*part_sex,Analysisdata)

rsq.partial(CR1,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
rsq.partial(CR2,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
rsq.partial(CR3,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
rsq.partial(CR4,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))

#Simple Slopes for interaction between sexes
library(emmeans)
library(reghelper)
emmeans(CR4, pairwise ~ sex | part_sex)
emmeans(CR4, pairwise ~ part_sex | sex)
simple_slopes(CR4)

#Regression Report
library(sjPlot)
library(sjmisc)
tab_model(CR1,CR2,CR3,CR4)
