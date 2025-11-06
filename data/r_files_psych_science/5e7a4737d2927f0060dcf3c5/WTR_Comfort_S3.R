#WTR_Comfort Project Study 3

#Data Importing
raw<-read.csv('WTR_Comfort_S3.csv')

#Exclusion of Free Response
library(dplyr)
library(lmerTest)
library(pbkrtest)
raw<-filter(raw,English_Exclude==0)

#Recoding for Contact Comfort
rawCC<-raw[,27:36]
rawCC<-as.data.frame(as.matrix(rawCC)-4)

#Decoding the target faces
part_sex<-ifelse(raw$Target<20.5,yes =1,no=2)
part_att<-ifelse(raw$Target<20.5,yes = raw$Target,no=raw$Target-20)

#Variable Computing
rawDS<-as.data.frame(as.matrix(raw[,7:13])-1)

rawWTR<-raw[,c(47:38,57:48,67:58)]
rawWTR$X75_.26[which(rawWTR$X75_.26==-.45)]<-0
rawWTR$X19_.7[which(rawWTR$X19_.7==-.45)]<-0
rawWTR$X46_.16[which(rawWTR$X46_.16==-.45)]<-0
rawWTRdummy<-as.matrix(rawWTR)
rawWTRdummy[rawWTRdummy!=0]<-1
rawWTR<-as.data.frame(rawWTRdummy)
rm(rawWTRdummy)

rawDS<-as.data.frame(as.matrix(raw[,7:13])-1)


participantnumber <- 1
Caulsum <- matrix(,,ncol=5)
Caulsum <- Caulsum[-1,]
colnames(Caulsum) <- c("WTR75","WTR19","WTR46","WTRTOTAL",'DS')

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
  #print(anchorWTR)
}

while(participantnumber <= nrow(raw)){
  dumx <- rawWTR[participantnumber,]
  dummysheetWTR <- matrix(data = dumx, ncol = 10, byrow = TRUE)
  colnames(dummysheetWTR) <- seq(-35, 145, by = 20)
  WTRAnchor <- apply(dummysheetWTR, 1, wtrcal)
  
  DSRaw <- rawDS[participantnumber,]
  
  if(NA%in%WTRAnchor){
    WTRTotal<-NA
  }else{
    WTRTotal<-mean(WTRAnchor)
  }
  
  Caulperson <- matrix(c(WTRAnchor,WTRTotal,mean(data.matrix(DSRaw))), ncol = 5)
  
  Caulsum <- rbind(Caulsum,Caulperson)
  
  participantnumber<-participantnumber+1}

Caulsum<-as.data.frame(Caulsum)
Caulsum<-round(Caulsum, digits = 3)

Caulsum<-mutate(Caulsum,dummy=c(1:nrow(Caulsum)))
rawCC<-mutate(rawCC,dummy=c(1:nrow(Caulsum)))
rawDS<-mutate(rawDS,dummy=c(1:nrow(Caulsum)))
Caulsum<-merge.data.frame(Caulsum,rawCC,by='dummy')
Caulsum<-merge.data.frame(Caulsum,rawDS,by='dummy')

#Exclusion of Sex and Dataset Merging
Finaldata<-mutate(Caulsum,sex=raw$sex,age=raw$age,value=raw$Value,part_sex,part_att,faces=raw$Target)
Finaldata<-as.data.frame(filter(Finaldata, sex!=3))
Finaldata<-Finaldata[complete.cases(Finaldata$WTRTOTAL),]

#EFA and Crobach alpha's
library(psych,GPArotation)
fa(Finaldata[,7:16],nfactors = 1,rotate = 'oblimin',fm='ml')#CC
alpha(Finaldata[,7:16])
Finaldata<-mutate(Finaldata,CC=unname(rowMeans(select(Finaldata,comf1:comf10))))

alpha(Finaldata[,17:23])#DS
alpha(Finaldata[,2:4])#WTR

#Descriptive Analysis
describe(Finaldata)

#Formatting Dataset and Cleaning Enviornment 
Finaldata$sex<-as.factor(Finaldata$sex)
Finaldata$part_sex<-as.factor(Finaldata$part_sex)
Finaldata$value<-as.factor(Finaldata$value)

rm(Caulperson,Caulsum,DSRaw,dummysheetWTR,dumx,rawCC,rawDS,rawWTR,participantnumber,WTRAnchor,WTRTotal,part_att,part_sex)

Analysisdata<-select(Finaldata,CC,WTRTOTAL,DS,sex,age,part_sex,part_att,value)


#Manipulation checks
Height<-lmer(Alex_height~Value+(1+Value|Target),data=raw)
Weight<-lmer(Alex_weight~Value+(1+Value|Target),data=raw)
Age<-lmer(Alex_age~Value+(1+Value|Target),data=raw)
Wealth<-lmer(Alex_wealth~Value+(1+Value|Target),data=raw)
Intelligent<-lmer(Alex_intelligent~Value+(1+Value|Target),data=raw)
Attractiveness<-lmer(Alex_attractive~Value+(1+Value|Target),data=raw)
Kindness<-lmer(Alex_kind~Value+(1+Value|Target),data=raw)
Honesty<-lmer(Alex_honest~Value+(1+Value|Target),data=raw)
Care<-lmer(Alex_care~Value+(1+Value|Target),data=raw)
emmeans(Height, pairwise ~ Value)
emmeans(Weight, pairwise ~ Value)
emmeans(Age, pairwise ~ Value)
emmeans(Wealth, pairwise ~ Value)
emmeans(Intelligent, pairwise ~ Value)
emmeans(Attractiveness, pairwise ~ Value)
emmeans(Kindness, pairwise ~ Value)
emmeans(Honesty, pairwise ~ Value)
emmeans(Care, pairwise ~ Value)

summary(Height)

#Correlation Matrix
library(Hmisc)
library(lsr)


rcorr(as.matrix(select(Analysisdata,CC,WTRTOTAL,DS,age,part_att)))



#T-test's for Correlation Matrix
Analysisdata_ttest<-select(Analysisdata,CC,WTRTOTAL,DS,age,part_att)
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
Analysisdata_pc_ctn<-select(Analysisdata,CC:DS,age,part_att,sex,part_sex,value)

while(ncol_regression<=(ncol(Analysisdata_pc_ctn)-3)){
  ncol_regression_predictor<-ncol_regression+1
  while (ncol_regression_predictor<=(ncol(Analysisdata_pc_ctn)-1)) {
    regression_pc<-lm(Analysisdata_pc_ctn[,ncol_regression]~Analysisdata_pc_ctn[,ncol_regression_predictor]+Analysisdata_pc_ctn$value,data = Analysisdata_pc_ctn)
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

regression_pc<-glm(sex~part_sex+value,family=binomial,data = Analysisdata_pc_ctn)
regression_rsq<-rsq.partial(regression_pc,objR=NULL,adj=TRUE,type=c('v','kl','sse','lr','n'))
print(((abs(regression_rsq$partial.rsq))^.5)[1])
print(summary(regression_pc)$coefficients[2,c(1,4)])


#Pre-registered multilevel regression
library(lme4)

Analysisdata_lme<-mutate(Analysisdata,faces=Finaldata$faces)

CR1<-lmer(CC~value+(1+WTRTOTAL|faces),data=Analysisdata_lme)

CR2a<-lmer(CC~value+WTRTOTAL+(1+WTRTOTAL|faces),data=Analysisdata_lme)#Singular
CR2b<-lmer(CC~value+WTRTOTAL+(1|faces),data=Analysisdata_lme)
CR2<-CR2b

CR3a<-lmer(CC~value+WTRTOTAL+DS+sex+part_sex+(1+WTRTOTAL|faces),data=Analysisdata_lme)#Singular
CR3b<-lmer(CC~value+WTRTOTAL+DS+sex+part_sex+(1|faces),data=Analysisdata_lme)
CR3<-CR3b

CR4a<-lmer(CC~value+WTRTOTAL+DS+sex+part_sex+sex*part_sex+(1+WTRTOTAL|faces),data=Analysisdata_lme)#Singular
CR4b<-lmer(CC~value+WTRTOTAL+DS+sex+part_sex+sex*part_sex+(1|faces),data=Analysisdata_lme)
CR4<-CR4b


#Simple Slopes for interaction between sexes
library(emmeans)
library(interactions)
emmeans(CR4, pairwise ~ sex | part_sex)
emmeans(CR4, pairwise ~ part_sex | sex)

Analysisdata_lme_interaction<-Analysisdata_lme
Analysisdata_lme_interaction$sex<-as.numeric(Analysisdata_lme_interaction$sex)
Analysisdata_lme_interaction$sex<-Analysisdata_lme_interaction$sex-1
CR4itr<-lmer(CC~value+WTRTOTAL+DS+sex+part_sex+sex*part_sex+(1|faces),data=Analysisdata_lme_interaction)
interactions::sim_slopes(model=CR4itr, pred=sex, modx=part_sex)


#Regression Report
library(sjPlot)
library(sjmisc)
tab_model(CR1,CR2,CR3,CR4)


#Additional Info in the text
mean((as.data.frame(filter(Analysisdata,value==1)))[,1],na.rm = TRUE)
mean((as.data.frame(filter(Analysisdata,value==0)))[,1],na.rm = TRUE)
anovaCC<-aov(Analysisdata$CC~Analysisdata$value)
summary(anovaCC)
etaSquared(anovaCC)

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}
(stdCoef.merMod(CR3))^2
