library(psych)
library(Hmisc)
library(forecast)
library(fpp)
library(lmtest)
library(Kendall)
library(car)
library(urca)
library(Hmisc)
library(lmerTest)
library(ggplot2)

setwd("~/Desktop")
d_punitive<-read.csv("PunitiveChapters.csv")
d_loving<-read.csv("LovingChapters.csv")
d_neutral<-read.csv("NeutralChapters.csv")

##Defining indices
d_punitive$pun<-(d_punitive$Deuteronomy_9+d_punitive$Numbers_11+d_punitive$Job_4+d_punitive$Isaiah_13+d_punitive$Lamentations_2+d_punitive$Ezekiel_7+d_punitive$Habakkuk_3+d_punitive$Jeremiah_7+d_punitive$Psalm.78+d_punitive$Exodus.15)/10
d_loving$lov<-(d_loving$Psalm_136+d_loving$Psalm_103+d_loving$Psalm_86+d_loving$Psalm_146+d_loving$Psalm_145+d_loving$Psalm_36+d_loving$Psalm_57+d_loving$Psalm_63+d_loving$Psalm_107+d_loving$Psalm.34)/10
d_neutral$neu<-(d_neutral$Ecclesiastes.1+d_neutral$Ecclesiastes.3+d_neutral$Genesis.11+d_neutral$Genesis.1+d_neutral$Genesis.2+d_neutral$Joshua.1+d_neutral$Proverbs.27+d_neutral$Proverbs.4+d_neutral$Leviticus.19+d_neutral$Psalms.127)/10

d<-data.frame("pun"=d_punitive$pun,
              "lov"=d_loving$lov,
              "neut"=d_neutral$neu,
              "year"=d_punitive$Year)

ggplot(d,aes(x=year))+
  geom_line(aes(y=lov),color="green")+
  geom_line(aes(y=neut),color="salmon")+
  geom_line(aes(y=pun),color="slategray")+
  theme_classic()

d$pun_resid<-lm(pun~neut,data=d)$residuals
d$lov_resid<-lm(lov~neut,data=d)$residuals

ggplot(d,aes(x=year))+
  geom_line(aes(y=lov_resid),color="green")+
  geom_line(aes(y=pun_resid),color="slategray")+
  theme_classic()

#################################
########Merging in Conflict######
#################################

d<-d[d$year>1800,]

conflict<-read.csv("ConflictCatalog.csv")
conflict <- conflict[!is.na(conflict$TotalFatalities),]
conflict$meanyear<-(conflict$StartYear+conflict$EndYear)/2
conflict$meanyear<-ceiling(conflict$meanyear)

conflict_english<-conflict[conflict$English==1,]
conflict_nonenglish<-conflict[conflict$English==0,]

conflict_english_agg<-aggregate(conflict_english,by=list(conflict_english$meanyear),FUN=mean,na.rm=T)
conflict1800_english<-conflict_english_agg[conflict_english_agg$meanyear>1799,]
conflict1800_english$year<-conflict1800_english$meanyear

conflict_nonenglish_agg<-aggregate(conflict_nonenglish,by=list(conflict_nonenglish$meanyear),FUN=mean,na.rm=T)
conflict1800_nonenglish<-conflict_nonenglish_agg[conflict_nonenglish_agg$meanyear>1799,]
conflict1800_nonenglish$year<-conflict1800_nonenglish$meanyear

d<-d[d$year<2001,]

d2_english<- merge(conflict1800_english,d,by="year")
d2_nonenglish<- merge(conflict1800_nonenglish,d,by="year")

##########################
####Analyses begin########
##########################

                      ###English Speaking Conflicts###

####De-trending#####
d2_english$TotalFatalities_u<-lm(TotalFatalities~neut+year,data=d2_english)$residuals
d2_english$pun_u<-lm(pun~neut+year,data=d2_english)$residuals
d2_english$lov_u<-lm(lov~neut+year,data=d2_english)$residuals

#Dickey-Fuller
d1<-d2_english[,c("pun_u","TotalFatalities_u")]
summary(d1)
pun.adf<-ur.df(d1[,"pun_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(pun.adf)
fatal.adf<-ur.df(d1[,"TotalFatalities_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(fatal.adf)
lov.adf<-ur.df(d2_english[,"lov_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(lov.adf)

####Standardize
d2_english$Zpun_u<-scale(d2_english$pun_u)
d2_english$Zlov_u<-scale(d2_english$lov_u)
d2_english$ZTotalFatalities_u<-scale(d2_english$TotalFatalities_u)

                      ###Non-English Speaking Conflicts###
####De-trending#####
d2_nonenglish$TotalFatalities_u<-lm(TotalFatalities~neut+year,data=d2_nonenglish)$residuals
d2_nonenglish$pun_u<-lm(pun~neut+year,data=d2_nonenglish)$residuals
d2_nonenglish$lov_u<-lm(lov~neut+year,data=d2_nonenglish)$residuals

#Dickey-Fuller
d1<-d2_nonenglish[,c("pun_u","TotalFatalities_u")]
summary(d1)
pun.adf<-ur.df(d1[,"pun_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(pun.adf)
fatal.adf<-ur.df(d1[,"TotalFatalities_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(fatal.adf)
lov.adf<-ur.df(d2_nonenglish[,"lov_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(lov.adf)

####Standardize
d2_nonenglish$Zpun_u<-scale(d2_nonenglish$pun_u)
d2_nonenglish$Zlov_u<-scale(d2_nonenglish$lov_u)
d2_nonenglish$ZTotalFatalities_u<-scale(d2_nonenglish$TotalFatalities_u)

####Regrsssions
summary(lm(Zpun_u~ZTotalFatalities_u,data=d2_english))
summary(lm(Zpun_u~ZTotalFatalities_u,data=d2_nonenglish))

summary(lm(Zlov_u~ZTotalFatalities_u,data=d2_english))
summary(lm(Zlov_u~ZTotalFatalities_u,data=d2_nonenglish))

##Cross-Correlation
require(forecast)
ccfnames.warpun_e<-ccf(d2_english$TotalFatalities_u,d2_english$pun_u,lag.max = 20, ylab="conflict and punitive god",na.action=na.exclude)
ccfnames.warlov_e<-ccf(d2_english$TotalFatalities_u,d2_english$lov_u,lag.max = 20, ylab="conflict and loving god",na.action=na.exclude)
ccfnames.warpun_ne<-ccf(d2_nonenglish$TotalFatalities_u,d2_nonenglish$pun_u,lag.max = 20, ylab="conflict and punitive god",na.action=na.exclude)
ccfnames.warlov_ne<-ccf(d2_nonenglish$TotalFatalities_u,d2_nonenglish$lov_u,lag.max = 20, ylab="conflict and loving god",na.action=na.exclude)

