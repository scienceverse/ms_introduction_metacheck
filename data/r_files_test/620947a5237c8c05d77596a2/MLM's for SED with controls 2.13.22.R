#'---
#'title: "Global Increases in Individualism" Updated MLM's
#'authors: "Santos, H. C., Grossmann, I., & Varnum, M. E. W. "
#'date: "13 Februrary 2022"
#'---

#' Loading packages 
library(lme4)
library(lmerTest)
library(effects)
library(plyr)
library(lmtest)
library(psych)
library(MuMIn)
library(ggplot2)
library(rworldmap)
library(classInt)
library(RColorBrewer)
library(gridExtra)
#' Removing scientific notation 
options(scipen=999)
#' Retrieve Data (or Global Increases in Individualism Data in OSF)
CCHANGE<-read.csv(file="~/Desktop/Data.csv", header=TRUE, na.strings="") #Should have 2346 obs. of 39 variables
# Note: Country ID 30 (Pakistan) and 74 (Slovakia) were mistakenly given ID numbers, then dropped for not having enough data points as specified in methods
#'
#### Preparing Variables ####
#'
#' Log Transforming GDP 
describe(CCHANGE$GDP)
CCHANGE$GDP.log <- log10(CCHANGE$GDP)
describe(CCHANGE$GDP.log)
#' Log Transforming Disater 
hist(CCHANGE$disaster)
describe(CCHANGE$disaster) # High kurtosis (>10) and skew (>3) suggests log transformation
CCHANGE$disaster.log <- log10(CCHANGE$disaster)
CCHANGE$disaster.log[CCHANGE$disaster==0]<-NA
hist(CCHANGE$disaster.log)
describe(CCHANGE$disaster.log) # Kurtosis and skew values are better
#' Calculating Service v. Agriculture (IPUMS)
CCHANGE$servagri<-CCHANGE$service-CCHANGE$agriculture
CCHANGE$servagri[is.na(CCHANGE$service)] <- NA
CCHANGE$servagri[is.na(CCHANGE$agriculture)] <- NA
# Predictor Interpolations to Account for Missing Data
describe(CCHANGE$educ) #Educational Attainment
describe(CCHANGE$siops) #Occupational Prestige
describe(CCHANGE$agriculture) #Agriculture Jobs
describe(CCHANGE$service) #White-collar Jobs
describe(CCHANGE$urban) #Urban vs. Rural
CCHANGE <- ddply(CCHANGE,"ID",mutate,educ.int = tryCatch(na.approx(educ, na.rm = FALSE, maxgap = 10),error = function(e) NA))
CCHANGE <- ddply(CCHANGE,"ID",mutate,siops.int = tryCatch(na.approx(siops, na.rm = FALSE, maxgap = 10),error = function(e) NA))
CCHANGE <- ddply(CCHANGE,"ID",mutate,urban.int = tryCatch(na.approx(urban, na.rm = FALSE, maxgap = 10),error = function(e) NA))
CCHANGE <- ddply(CCHANGE,"ID",mutate,agriculture.int = tryCatch(na.approx(agriculture, na.rm = FALSE, maxgap = 10),error = function(e) NA))
CCHANGE <- ddply(CCHANGE,"ID",mutate,service.int = tryCatch(na.approx(service, na.rm = FALSE, maxgap = 10),error = function(e) NA))
CCHANGE$servagri.int<-CCHANGE$service.int-CCHANGE$agriculture.int #Computing white-collar vs. agriculture with imputed data
CCHANGE$servagri.int[is.na(CCHANGE$service.int)] <- NA
CCHANGE$servagri.int[is.na(CCHANGE$agriculture.int)] <- NA
describe(CCHANGE$educ.int)
describe(CCHANGE$siops.int)
describe(CCHANGE$urban.int)
describe(CCHANGE$servagri.int)
#' Computing Composite Socioeconomic Development (SED)
cor(CCHANGE[,c('servagri.int','siops.int','educ.int','GDP.log','urban.int')], use='pairwise.complete.obs', method='kendall') #Checking correlations
psych::alpha(CCHANGE[,c('servagri.int','siops.int','educ.int','GDP.log','urban.int')], check.keys=FALSE) #Checking reliability
CCHANGE$zGDP.log<- scale(CCHANGE$GDP.log, center=TRUE, scale=TRUE) #Standardizing variables
CCHANGE$zeduc.int<- scale(CCHANGE$educ.int, center=TRUE, scale=TRUE)
CCHANGE$zsiops.int<- scale(CCHANGE$siops.int, center=TRUE, scale=TRUE)
CCHANGE$zurban.int<- scale(CCHANGE$urban.int, center=TRUE, scale=TRUE)
CCHANGE$zservagri.int<- scale(CCHANGE$servagri.int, center=TRUE, scale=TRUE)
CCHANGE$SED<-apply(CCHANGE[,c('zGDP.log','zeduc.int','zsiops.int','zservagri.int','zurban.int')], 1, mean, na.rm=TRUE) #Computing mean
#' Reverse Scoring and Computing Composite Individualist Practices 
cor(CCHANGE[,c('famsize','alone','oldalone','divorce')], use='pairwise.complete.obs', method='kendall') #Checking correlations (famsize and chbord are reversed)
psych::alpha(as.matrix(CCHANGE[,c('famsize','alone','oldalone','divorce')]), check.keys=TRUE) #Checking reliability, famsize and chborn are automatically reversed in this analysis (see - sign) reversed
CCHANGE$zfamsize.raw<- scale(CCHANGE$famsize, center=TRUE, scale=TRUE) #Standardizing variables - Household size
CCHANGE$zalone.raw<- scale(CCHANGE$alone, center=TRUE, scale=TRUE) # Living alone
CCHANGE$zoldalone.raw<- scale(CCHANGE$oldalone, center=TRUE, scale=TRUE) # Older adults living alone
CCHANGE$zdivorce.raw<- scale(CCHANGE$divorce, center=TRUE, scale=TRUE) # Divorce
CCHANGE$zfamsize.raw<-CCHANGE$zfamsize.raw[,] #Removing attributes R attached to the bottom of the standardized variables
CCHANGE$zalone.raw<-CCHANGE$zalone.raw[,]
CCHANGE$zoldalone.raw<-CCHANGE$zoldalone.raw[,]
CCHANGE$zdivorce.raw<-CCHANGE$zdivorce.raw[,]
CCHANGE$zfamsize.raw.r<-CCHANGE$zfamsize.raw*-1 #Reversing household size
CCHANGE$Practices.raw<-apply(CCHANGE[,c('zfamsize.raw.r','zalone.raw','zoldalone.raw','zdivorce.raw')], 1, mean, na.rm=TRUE) #Computing mean
#' Computing Importance of Friends vs. Family (Difference Score) 
CCHANGE$FriendsFam.raw <- CCHANGE$friends.imp - CCHANGE$fam.imp
CCHANGE$FriendsFam.raw[is.na(CCHANGE$friends.imp)] <- NA
CCHANGE$FriendsFam.raw[is.na(CCHANGE$fam.imp)] <- NA
#' Computing Composite Individualist Values 
cor(CCHANGE[,c('FriendsFam.raw','ind.child','PostMat')], use='pairwise.complete.obs', method='kendall') 
psych::alpha(as.matrix(CCHANGE[,c('FriendsFam.raw','ind.child','PostMat')]), check.keys=TRUE) #Checking reliability
CCHANGE$zFriendsFam.raw<- scale(CCHANGE$FriendsFam.raw, center=TRUE, scale=TRUE) #Standardizing variables - Friends-Family
CCHANGE$zIndChild.raw<- scale(CCHANGE$ind.child, center=TRUE, scale=TRUE) #Independent Children
CCHANGE$zPostMat.raw<- scale(CCHANGE$PostMat, center=TRUE, scale=TRUE) #Preference for self-expression
CCHANGE$zFriendsFam.raw<-CCHANGE$zFriendsFam.raw[,] #Removing attributes R attached to the bottom of the standardized variables
CCHANGE$zIndChild.raw<-CCHANGE$zIndChild.raw[,]
CCHANGE$zPostMat.raw<-CCHANGE$zPostMat.raw[,]
CCHANGE$Values.raw <- apply(CCHANGE[,c('zFriendsFam.raw','zIndChild.raw','zPostMat.raw')], 1, mean, na.rm=TRUE) #Computing mean
#' Calculating Pathogens/Population
CCHANGE$Cholera<-0 # Creating empty variable
CCHANGE$Diptheria<-0
CCHANGE$Measles<-0
CCHANGE$NTetanus<-0
CCHANGE$Pertussis<-0
CCHANGE$TTetanus<-0
CCHANGE$Tuberculosis<-0
CCHANGE$Cholera<-ifelse(is.na(CCHANGE$Path1),NA,CCHANGE$Path1/CCHANGE$TotalPop) #Pathogen incidence divided by population
CCHANGE$Diptheria<-ifelse(is.na(CCHANGE$Path3),NA,CCHANGE$Path3/CCHANGE$TotalPop)
CCHANGE$Measles<-ifelse(is.na(CCHANGE$Path6),NA,CCHANGE$Path6/CCHANGE$TotalPop)
CCHANGE$NTetanus<-ifelse(is.na(CCHANGE$Path9),NA,CCHANGE$Path9/CCHANGE$TotalPop)
CCHANGE$Pertussis<-ifelse(is.na(CCHANGE$Path10),NA,CCHANGE$Path10/CCHANGE$TotalPop)
CCHANGE$TTetanus<-ifelse(is.na(CCHANGE$Path13),NA,CCHANGE$Path13/CCHANGE$TotalPop)
CCHANGE$Tuberculosis<-ifelse(is.na(CCHANGE$Path14),NA,CCHANGE$Path14/CCHANGE$TotalPop)
CCHANGE$PathogenPop<-apply(CCHANGE[,c('Cholera','Diptheria','Measles','NTetanus','Pertussis','TTetanus','Tuberculosis')],1,sum,na.rm=TRUE) #Computing mean
hist(CCHANGE$PathogenPop)
describe(CCHANGE$PathogenPop) # High kurtosis and skew suggests log transformation
CCHANGE$PathogenPop.log <- log10(CCHANGE$PathogenPop)
CCHANGE$PathogenPop.log[CCHANGE$PathogenPop==0]<-NA
hist(CCHANGE$PathogenPop.log)
describe(CCHANGE$PathogenPop.log)
#' Leading Practices and Values 10 Years Forward (10) 
library(dplyr)
CCHANGE<-CCHANGE %>%
  group_by(ID)%>%
  mutate(Practices10.raw = lead(Practices.raw,n = 10L))%>%
  mutate(Values10.raw = lead(Values.raw,n = 10L))
detach("package:dplyr")
#' Subsetting Samples with Values Data from World Values Survey (WVS) 
WVS<-subset(CCHANGE,ID%in%c(1,2,8,9,15,18,19,21,27,28,32,34,35,36,37,39,40,44:81))
WVS<-subset(WVS, Year > 1979) #Limiting year range to the range of WVS (1980-2011); End with 576 obs. of 75 variables
WVS$Year0<-WVS$Year-1979 #Resetting Year0 to start at 0
#'

#### Effects of Socioeconomic Development on Changes in Individualism controlling for year####

#' Effect of SED on Practices nested by country controlling for Year## Original code had errors that caused it not to run, this is what was modified on 2/13/22##
resultS <- lmer(Practices.raw ~ SED+Year0+(1|Country), CCHANGE, REML = FALSE)
summary(resultS)#Significant effect of SED

#' Effect of SED on Values nested by country controlling for Year
resultSw <- lmer(Values.raw ~ SED+Year0+(1|Country), WVS, REML = FALSE)
summary(resultSw)#Significant effect of SED

#' Effect of SED on Practices nested by country controlling for Year, Disaster Frequency, Pathogen Prevalence, and Climate
resultS <- lmer(Practices.raw ~ SED+Year0+SED+disaster.log+PathogenPop.log+TempDev+(1|Country), CCHANGE, REML = FALSE)
summary(resultS)#Significant effect of SED

#' Effect of SED on Values nested by country controlling for Year, Disaster Frequency, Pathogen Prevalence, and Climate
resultSw <- lmer(Values.raw ~ SED+Year0+SED+disaster.log+PathogenPop.log+TempDev+(1|Country), CCHANGE, REML = FALSE)
summary(resultSw)#Signficant effect of SED

