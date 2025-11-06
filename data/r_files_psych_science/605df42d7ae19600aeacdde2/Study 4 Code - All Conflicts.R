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

d_punitive<-read.csv("~/Downloads/PunitiveChapters.csv")
d_loving<-read.csv("~/Downloads/LovingChapters.csv")
d_neutral<-read.csv("~/Downloads/NeutralChapters.csv")

##Defining indices
d_punitive$pun<-(d_punitive$Deuteronomy_9+d_punitive$Numbers_11+d_punitive$Job_4+d_punitive$Isaiah_13+d_punitive$Lamentations_2+d_punitive$Ezekiel_7+d_punitive$Habakkuk_3+d_punitive$Jeremiah_7+d_punitive$Psalm.78+d_punitive$Exodus.15)/10
d_loving$lov<-(d_loving$Psalm_136+d_loving$Psalm_103+d_loving$Psalm_86+d_loving$Psalm_146+d_loving$Psalm_145+d_loving$Psalm_36+d_loving$Psalm_57+d_loving$Psalm_63+d_loving$Psalm_107+d_loving$Psalm.34)/10
d_neutral$neu<-(d_neutral$Ecclesiastes.1+d_neutral$Ecclesiastes.3+d_neutral$Genesis.11+d_neutral$Genesis.1+d_neutral$Genesis.2+d_neutral$Joshua.1+d_neutral$Proverbs.27+d_neutral$Proverbs.4+d_neutral$Leviticus.19+d_neutral$Psalms.127)/10

d<-data.frame("pun"=d_punitive$pun,
              "lov"=d_loving$lov,
              "neut"=d_neutral$neu,
              "year"=d_punitive$Year)

#################################
####### Merging in Conflict #####
#################################

d<-d[d$year>1800,]


conflict<-read.csv("~/Downloads/ConflictCatalog.csv")
conflict <- conflict[!is.na(conflict$TotalFatalities),]
conflict$meanyear<-(conflict$StartYear+conflict$EndYear)/2
conflict$meanyear<-ceiling(conflict$meanyear)
conflict_agg<-aggregate(TotalFatalities~meanyear,conflict,sum)
conflict1800<-conflict_agg[conflict_agg$meanyear>1799,]
conflict1800$year<-conflict1800$meanyear

d<-d[d$year<2001,]

d2<- merge(conflict1800,d,by="year")

##########################
####Analyses begin########
##########################

####De-trending#####
d2$TotalFatalities_u<-lm(TotalFatalities~neut+year,data=d2)$residuals
d2$pun_u<-lm(pun~neut+year,data=d2)$residuals
d2$lov_u<-lm(lov~neut+year,data=d2)$residuals

#Dickey-Fuller
d1<-d2[,c("pun_u","TotalFatalities_u")]
summary(d1)
pun.adf<-ur.df(d1[,"pun_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(pun.adf)
fatal.adf<-ur.df(d1[,"TotalFatalities_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(fatal.adf)
lov.adf<-ur.df(d2[,"lov_u"],type="trend",selectlags="AIC") ##Dickey-fuller root test
summary(lov.adf)

####Standardize
d2$Zpun_u<-scale(d2$pun_u)
d2$Zlov_u<-scale(d2$lov_u)
d2$ZTotalFatalities_u<-scale(d2$TotalFatalities_u)

###Regressions
summary(fit1<-lm(Zpun_u~ZTotalFatalities_u,data=d2))
summary(fit2<-lm(Zlov_u~ZTotalFatalities_u,data=d2))

##Cross-Correlation
require(forecast)
ccfnames.warpun<-ccf(d2$TotalFatalities_u,d2$pun_u,lag.max = 20, ylab="conflict and punitive god",na.action=na.exclude)
ccfnames.warlov<-ccf(d2$TotalFatalities_u,d2$lov_u,lag.max = 20, ylab="conflict and loving god",na.action=na.exclude)
ccfnames.warpun
ccfnames.warlov

##Granger test of causality
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 1, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 2, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 3, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 4, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 5, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 6, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 7, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 8, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 9, na.action=na.exclude) 
grangertest(d2$pun_u,d2$TotalFatalities_u,order = 10, na.action=na.exclude) 

grangertest(d2$lov_u,d2$TotalFatalities_u,order = 1, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 2, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 3, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 4, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 5, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 6, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 7, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 8, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 9, na.action=na.exclude) 
grangertest(d2$lov_u,d2$TotalFatalities_u,order = 9, na.action=na.exclude) 

##Granger test of causality
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 1, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 2, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 3, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 4, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 5, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 6, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 7, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 8, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 9, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$pun_u,order = 10, na.action=na.exclude) 

grangertest(d2$TotalFatalities_u,d2$lov_u,order = 1, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 2, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 3, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 4, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 5, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 6, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 7, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 8, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 9, na.action=na.exclude) 
grangertest(d2$TotalFatalities_u,d2$lov_u,order = 10, na.action=na.exclude) 

##Measure Correlations
library(Hmisc)
correlations<-data.frame(d2$TotalFatalities_u, d2$lov_u, d2$pun_u)
rcorr(as.matrix(correlations))
