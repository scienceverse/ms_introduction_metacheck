#'---
#'title: "Global Increases in Individualism"
#'authors: "Santos, H. C., Grossmann, I., & Varnum, M. E. W. "
#'date: "30 May 2017"
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
#' Retrieve CulturalChangeDataR2.csv (or Global Increases in Individualism Data in OSF)
CCHANGE<-read.csv(file.choose(), header=TRUE, na.strings=" ") #Should have 2346 obs. of 39 variables
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
#### Plots - Overall Change (Figure 1-2) ####
#'
P1label<- expression(paste("Individualist Practices ( ", italic("z"), " score)")) #Preparing italics in labels
P2label<- expression(paste("Individualist Values ( ", italic("z"), " score)")) #Preparing italics in labels
#' Individualist Practices Over Time (warnings removing non-finite and missing values are okay here) 
p1<-ggplot(data=subset(CCHANGE,ID%in%c(1:34,36:43)),mapping=aes(x=Year,y=Practices.raw,na.rm=TRUE))+
  geom_jitter(alpha=1,size=1.5,color="gray40")+
  geom_smooth(alpha=1,size=1,se=FALSE,method="lm",fullrange=TRUE,color="black")+
  geom_smooth(alpha=.5,size=1,se=TRUE,method="lm",fullrange=TRUE,color="black")+
  scale_y_continuous(name=P1label,limits=c(-2.5,2.5))+
  theme_classic()+theme(legend.position="bottom",legend.direction="vertical",axis.text=element_text(colour="black",size=12),text=element_text(colour="black",size=14))
#' Individualist Values Over Time (warnings removing non-finite and missing values are okay here) 
p2<-ggplot(data=WVS,mapping=aes(x=Year,y=Values.raw,na.rm=TRUE))+
  geom_jitter(alpha=1, size=1.5,color="gray40")+
  scale_color_discrete(name="Country")+scale_x_continuous(limits = c(1980, 2010))+guides(color = guide_legend(ncol = 5))+
  geom_smooth(alpha=1,size=1,se=FALSE,method="lm",fullrange=TRUE,color="black")+
  geom_smooth(alpha=.5,size=1,se=TRUE,method="lm",fullrange=TRUE,color="black")+
  scale_y_continuous(name=P2label,limits=c(-2.5,2.5))+
  theme_classic()+theme(legend.position="bottom",legend.direction="vertical",axis.text=element_text(colour="black",size=12),text=element_text(colour="black",size=14))
#' Putting plots together (Figure 1)
grid.arrange(p1,p2,ncol=2) #Export with 600x600 as dimensions
#' Preparing data for worldmap figure - creating dataframe with country and slope
IPUMSorder<-ddply(CCHANGE[!is.na(CCHANGE$Practices.raw),],c("Country"),function(x) coef(summary(lm(Practices.raw ~ Year0,data=x)))[2])
WVSorder<-ddply(WVS,c("Country"),function(x) coef(summary(lm(Values.raw ~ Year0,data=x)))[2])
IPUMSorder[order(IPUMSorder$V1),]
WVSorder[order(WVSorder$V1),]
#' Change in Individualist Practices across Countries (Figure 2 top half; Two world map figures need to be copied, pasted, and later combined from the dialogue boxes)
IPUMSmap<-joinCountryData2Map(IPUMSorder,joinCode="NAME",nameJoinColumn="Country",verbose="True")
catMethod<-seq(from=-.07,to=.07,by=.014) #Creating scale cut-off points
colourPalette=c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#d1e5f0","#92c5de","#4393c3","#2166ac","#053061") #Creating color scheme
map1<-mapCountryData(IPUMSmap,nameColumnToPlot="V1",catMethod=catMethod,colourPalette=colourPalette,addLegend=FALSE,mapTitle="Individualist Practices")
do.call(addMapLegend,c(map1,legendLabels="all",legendWidth=0.5,legendIntervals=10,legendMar=8,labelFontSize=1.2)) #Copy figure after this point as 525x525 pixels
#' Change in Individualist Values across Countries (Figure 2, bottom half; Two world map figures need to be copied, pasted, and later combined from the dialogue boxes)
WVSmap<-joinCountryData2Map(WVSorder,joinCode="NAME",nameJoinColumn="Country",verbose="True")
map2<-mapCountryData(WVSmap,nameColumnToPlot="V1",catMethod=catMethod,colourPalette=colourPalette,addLegend=FALSE,mapTitle="Individualist Values")
do.call(addMapLegend,c(map2,legendLabels="all",legendWidth=0.5,legendIntervals=10,legendMar=8,labelFontSize=1.2)) #Copy figure after this point as 525x525 pixels
#'
#### Part 0: Null Model Establishing Necessity of Multilevel Model (MLM) ####
#'
#' Model of Practices nested by country 
resultY0 <- lmer(Practices.raw ~1+(1|Country), CCHANGE, REML = FALSE)
summary(resultY0)
vcov0<-as.data.frame(VarCorr(resultY0),comp=c("Variance","Std.Dev."))
vcov0[1,4]/(vcov0[1,4]+vcov0[2,4]) #Intraclass correlation (ICC)
(vcov0[1,4]/(vcov0[1,4]+vcov0[2,4]))^2 #Percentage of variance explained
# Model of Values nested by country 
resultY0wvs <- lmer(Values.raw ~1+(1|Country), WVS, REML = FALSE)
summary(resultY0wvs)
vcov0wvs<-as.data.frame(VarCorr(resultY0wvs),comp=c("Variance","Std.Dev."))
vcov0wvs[1,4]/(vcov0wvs[1,4]+vcov0wvs[2,4]) #Intraclass correlation (ICC)
(vcov0wvs[1,4]/(vcov0wvs[1,4]+vcov0wvs[2,4]))^2 #Percentage of variance explained
#'
#### Part 1: Change in Individualism Over Time ####
#'
#' Effect of Year on Practices nested by country 
resultY <- lmer(Practices.raw ~ Year0+(1|Country), CCHANGE, REML = FALSE)
summary(resultY) #Significant effect
r.squaredGLMM(resultY) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYw <- lmer(Values.raw ~ Year0+(1|Country), WVS, REML = FALSE)
summary(resultYw) #Significant effect
r.squaredGLMM(resultYw) #Marginal R2 (R2m)
#'
#### Part 2: Predictors of Change in Individualism (Tables 2 and 3) ####
#'
#' Correlations between other predictors
cor(CCHANGE[,c('Practices.raw','Values.raw','SED','disaster.log','TempDev','PathogenPop.log')], use='pairwise.complete.obs',method='kendall')
#'
## Socioeconomic Development: Overall ##
#'
#' Effect of SED on Practices nested by country
resultS <- lmer(Practices.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultS) #Significant effect
r.squaredGLMM(resultS) #Marginal R2 (R2m) 
#' Effect of SED on Values nested by country 
resultSw <- lmer(Values.raw ~ SED+(1|Country), WVS, REML = FALSE)
summary(resultSw)#Significant effect
r.squaredGLMM(resultSw) #Marginal R2 (R2m)
#'
## Socioeconomic Development: White-Collar vs. Agriculture ##
#'
#' Effect of White-Collar vs. Agriculture on Practices nested by country
resultSWc <- lmer(Practices.raw ~ servagri.int+(1|Country), CCHANGE, REML = FALSE)
summary(resultSWc) #Significant effect
r.squaredGLMM(resultSWc) #Marginal R2 (R2m) 
#' Effect of White-Collar vs. Agriculture on Values nested by country 
resultSwWc <- lmer(Values.raw ~ servagri.int+(1|Country), WVS, REML = FALSE)
summary(resultSwWc)#Significant effect
r.squaredGLMM(resultSwWc) #Marginal R2 (R2m)
#'
#'
## Socioeconomic Development: Occupational Prestige ##
#'
#' Effect of Occupational Prestige on Practices nested by country
resultSOp <- lmer(Practices.raw ~ siops.int+(1|Country), CCHANGE, REML = FALSE)
summary(resultSOp) #Significant effect
r.squaredGLMM(resultSOp) #Marginal R2 (R2m) 
#' Effect of Occupational Prestige on Values nested by country 
resultSwOp <- lmer(Values.raw ~ siops.int+(1|Country), WVS, REML = FALSE)
summary(resultSwOp)#Significant effect
r.squaredGLMM(resultSwOp) #Marginal R2 (R2m)
#'
#'
## Socioeconomic Development: Education ##
#'
#' Effect of Education on Practices nested by country
resultSE <- lmer(Practices.raw ~ educ.int+(1|Country), CCHANGE, REML = FALSE)
summary(resultSE) #Significant effect
r.squaredGLMM(resultSE) #Marginal R2 (R2m) 
#' Effect of Education on Values nested by country 
resultSwE <- lmer(Values.raw ~ educ.int+(1|Country), WVS, REML = FALSE)
summary(resultSwE)#Significant effect
r.squaredGLMM(resultSwE) #Marginal R2 (R2m)
#'
#'
## Socioeconomic Development: Income ##
#'
#' Effect of Income on Practices nested by country
resultSI <- lmer(Practices.raw ~ GDP.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultSI) #Significant effect
r.squaredGLMM(resultSI) #Marginal R2 (R2m) 
#' Effect of Income on Values nested by country 
resultSwI <- lmer(Values.raw ~ GDP.log+(1|Country), WVS, REML = FALSE)
summary(resultSwI)#Significant effect
r.squaredGLMM(resultSwI) #Marginal R2 (R2m)
#'
#'
## Socio-Economic Development: Urbanization ##
#'
#' Effect of Urbanization on Practices nested by country 
resultU <- lmer(Practices.raw ~ urban+(1|Country), CCHANGE, REML = FALSE)
summary(resultU) #Significant effect
r.squaredGLMM(resultU) #Marginal R2 (R2m)
#'
## Disaster Frequency ##
#'
#' Effect of Disaster Frequency on Practices nested by country 
resultD <- lmer(Practices.raw ~ disaster.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultD) #Significant effect
r.squaredGLMM(resultD) #Marginal R2 (R2m)
#' Effect of Disaster Frequency on Values nested by country 
resultDw <- lmer(Values.raw ~ disaster.log+(1|Country), WVS, REML = FALSE)
summary(resultDw) #No effect
r.squaredGLMM(resultDw) #Marginal R2 (R2m)
#'
## Pathogen Prevalence ##
#'
#' Effect of Pathogen Prevalence on Practices nested by country 
resultP <- lmer(Practices.raw ~ PathogenPop.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultP) #Significant effect
r.squaredGLMM(resultP) #Marginal R2 (R2m)
#' Effect of Pathogen Prevalence on Values nested by country 
resultPw <- lmer(Values.raw ~ PathogenPop.log+(1|Country), WVS, REML = FALSE)
summary(resultPw) #Significant effect
r.squaredGLMM(resultPw) #Marginal R2 (R2m)
#'
## Climate ##
#'
#' Effect of Climate and SED on Practices nested by country 
CCHANGE$TempDev.c<-scale(CCHANGE$TempDev, center=TRUE, scale=FALSE)[,] #Centering climate for interaction (practices)
CCHANGE$SED.c<-scale(CCHANGE$SED, center=TRUE, scale=FALSE)[,] #Centering SED for interaction (practices)
resultCS <- lmer(Practices.raw ~ TempDev.c*SED.c+(1|Country), CCHANGE, REML = FALSE)
summary(resultCS) #Significant interaction
r.squaredGLMM(resultCS) #Marginal R2 (R2m)
#' Effect of Climate and SED on Values nested by country 
WVS$TempDev.cw<-scale(WVS$TempDev, center=TRUE, scale=FALSE)[,] #Centering Climate for Interaction (values)
WVS$SED.cw<-scale(WVS$SED, center=TRUE, scale=FALSE)[,] #Centering SED for Interaction (values)
resultCSw <- lmer(Values.raw ~ TempDev.cw*SED.cw+(1|Country), WVS, REML = FALSE)
summary(resultCSw) # Non-significant interaction
r.squaredGLMM(resultCSw) #Marginal R2 (R2m)
#' Calculating Delta AIC of the interaction by comparing change in AIC with and without interaction term 
resultCSb <- lmer(Practices.raw ~ TempDev.c+SED.c+(1|Country), CCHANGE, REML = FALSE) # Practices analysis without interaction term
summary(resultCSb)
resultCSwb <- lmer(Values.raw ~ TempDev.cw+SED.cw+(1|Country), WVS, REML = FALSE) # Values analysis without interaction term
summary(resultCSwb)
#' Plotting Climate x SED Interaction on Practices
quantile(CCHANGE$SED.c, na.rm=TRUE) #SED quantiles (values)
plot((effect(term="TempDev.c:SED.c",mod=resultCS,partial.residuals=TRUE,quantiles=c(.25,.75))),
     multiline=TRUE,main="Climate X SED",xlab="Temperature Deviation",ylab="Individualist Practices") #Recreate plot in MS Excel using quantile data and MLM estimates
#'
#### Part 3A: SED as a Mediator - Lagged Effects ####
#'
## Socioeconomic Development 10 Years Later ##
#'
#' Effect of SED on Practices +10 nested by country 
resultS10 <- lmer(Practices10.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultS10) #Significant leading effect of SED
r.squaredGLMM(resultS10)
#' Effect of SED on Values +10 nested by country 
resultS10w <- lmer(Values10.raw ~ SED+(1|Country), WVS, REML = FALSE)
summary(resultS10w) #Significant leading effect of SED
r.squaredGLMM(resultS10w)
#'
#### Part 3B: SED as a Mediator - Mediation ####
#'
#' Detaching lmerTest for mediation package to work 
detach("package:lmerTest")
library(mediation)
#' Year --> SED --> Practices 
subset.medS10 <- subset(CCHANGE,select=c('Year0', 'Country', 'SED', 'Practices10.raw')) #Subsetting relevant variables
aS10 <- lmer(SED ~ Year0+(1|Country), subset.medS10, REML = FALSE) #A path
cpS10 <- lmer(Practices10.raw ~ Year0+SED+(1|Country), subset.medS10, REML = FALSE) #C Prime path (effect with mediator)
mediationS10 <- mediate(aS10, cpS10, boot = FALSE, treat='Year0', mediator='SED',dropobs = TRUE,sims=5000) 
summary(mediationS10) #Significant mediation (ACME)
#' Year --> SED --> Values 
subset.medS10w <- subset(WVS,select=c('Year0', 'Country', 'SED', 'Values10.raw')) #Subsetting relevant variables
aS10w <- lmer(SED ~ Year0+(1|Country), subset.medS10w, REML = FALSE) #A path
cpS10w <- lmer(Values10.raw ~ Year0+SED+(1|Country), subset.medS10w, REML = FALSE) #C Prime path (effect with mediator)
mediationS10w <- mediate(aS10w, cpS10w, boot = FALSE, treat='Year0', mediator='SED',dropobs = TRUE,sims=5000)
summary(mediationS10w) #Significant mediation (ACME)
library(lmerTest) #Reloading lmerTest
#' Rerunning Analyses for p Values for the Mediation Figure 
#' Year --> SED --> Practices 
aS10 <- lmer(SED ~ Year0+(1|Country), subset.medS10, REML = FALSE)
cS10 <- lmer(Practices10.raw ~ Year0+(1|Country), subset.medS10, REML = FALSE)
cpS10 <- lmer(Practices10.raw ~ Year0+SED+(1|Country), subset.medS10, REML = FALSE)
summary(aS10) #A path
summary(resultS10) #B path
summary(cS10) #C path
summary(cpS10) #C Prime path (effect with mediator)
#' Year --> SED --> Values 
aS10w <- lmer(SED ~ Year0+(1|Country), subset.medS10w, REML = FALSE)
cS10w <- lmer(Values10.raw ~ Year0+(1|Country), subset.medS10w, REML = FALSE)
cpS10w <- lmer(Values10.raw ~ Year0+SED+(1|Country), subset.medS10w, REML = FALSE)
summary(aS10w) #A path
summary(resultS10w) #B path
summary(cS10w) #C path
summary(cpS10w) #C Prime path (effect with mediator)
#'
##### SOM #####
#'
#### Examining Cultural Zones ####
#'
#' Creating zones
Africa2<-subset(CCHANGE,ID %in% c(6,7,23,24,26,28,35,43,69))
Asia<-subset(CCHANGE,ID %in% c(51,61,62,3,19,20,25,30,38,42,45,47,77))
EEurope<-subset(CCHANGE,ID %in% c(34,44,49,50,55,57,63,66,73,78,79,52,53,18,64,72,74,75))
WEurope<-subset(CCHANGE,ID %in% c(2,15,32,36,48,60,65,16,37,54,56,58,59,67,70,76))
EnglishSpeaking2<-subset(CCHANGE,ID %in% c(8,21,39,46,68,80,81))
LatinAmerica2<-subset(CCHANGE,ID %in% c(1,4,5,9,10,11,12,13,17,27,29,31,33,40,41,71))
Africa2.w<-subset(WVS,ID %in% c(6,7,23,24,26,28,35,43,69))
Asia.w<-subset(WVS,ID %in% c(51,61,62,3,19,20,25,30,38,42,45,47,77))
EEurope.w<-subset(WVS,ID %in% c(34,44,49,50,55,57,63,66,73,78,79,52,53,18,64,72,74,75))
WEurope.w<-subset(WVS,ID %in% c(2,15,32,36,48,60,65,16,37,54,56,58,59,67,70,76))
EnglishSpeaking2.w<-subset(WVS,ID %in% c(8,21,39,46,68,80,81))
LatinAmerica2.w<-subset(WVS,ID %in% c(1,4,5,9,10,11,12,13,17,27,29,31,33,40,41,71))
#' 
## Africa ## 
#' 
#' Effect of Year on Practices nested by country 
resultYAf <- lmer(Practices.raw ~ Year0+(1|Country), Africa2, REML = FALSE)
summary(resultYAf) #Significant effect
r.squaredGLMM(resultYAf) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYAfw <- lmer(Values.raw ~ Year0+(1|Country), Africa2.w, REML = FALSE)
summary(resultYAfw) #Significant effect
r.squaredGLMM(resultYAfw) #Marginal R2 (R2m)
#' 
## Asia ## 
#' 
#' Effect of Year on Practices nested by country 
resultYAs <- lmer(Practices.raw ~ Year0+(1|Country), Asia, REML = FALSE)
summary(resultYAs) #Significant effect
r.squaredGLMM(resultYAs) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYAsw <- lmer(Values.raw ~ Year0+(1|Country), Asia.w, REML = FALSE)
summary(resultYAsw) #Significant effect
r.squaredGLMM(resultYAsw) #Marginal R2 (R2m)
#' 
## EEurope ## 
#' 
#' Effect of Year on Practices nested by country 
resultYEE <- lmer(Practices.raw ~ Year0+(1|Country), EEurope, REML = FALSE)
summary(resultYEE) #Significant effect
r.squaredGLMM(resultYEE) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYEEw <- lmer(Values.raw ~ Year0+(1|Country), EEurope.w, REML = FALSE)
summary(resultYEEw) #Significant effect
r.squaredGLMM(resultYEEw) #Marginal R2 (R2m)
#' 
## WEurope ## 
#' 
#' Effect of Year on Practices nested by country 
resultYWE <- lmer(Practices.raw ~ Year0+(1|Country), WEurope, REML = FALSE)
summary(resultYWE) #Significant effect
r.squaredGLMM(resultYWE) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYWEw <- lmer(Values.raw ~ Year0+(1|Country), WEurope.w, REML = FALSE)
summary(resultYWEw) #Significant effect
r.squaredGLMM(resultYWEw) #Marginal R2 (R2m)
#' 
## EnglishSpeaking ## 
#' 
#' Effect of Year on Practices nested by country 
resultYES2 <- lmer(Practices.raw ~ Year0+(1|Country), EnglishSpeaking2, REML = FALSE)
summary(resultYES2) #Significant effect
r.squaredGLMM(resultYES2) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYES2w <- lmer(Values.raw ~ Year0+(1|Country), EnglishSpeaking2.w, REML = FALSE)
summary(resultYES2w) #Significant effect
r.squaredGLMM(resultYES2w) #Marginal R2 (R2m)
#' 
## LatinAmerica ## 
#' 
#' Effect of Year on Practices nested by country 
resultYLA2 <- lmer(Practices.raw ~ Year0+(1|Country), LatinAmerica2, REML = FALSE)
summary(resultYLA2) #Significant effect
r.squaredGLMM(resultYLA2) #Marginal R2 (R2m)
#' Effect of Year on Values nested by country 
resultYLA2w <- lmer(Values.raw ~ Year0+(1|Country), LatinAmerica2.w, REML = FALSE)
summary(resultYLA2w) #Significant effect
r.squaredGLMM(resultYLA2w) #Marginal R2 (R2m)
#'
#### Testing the Unique Effects of Urbanization ####
#'
CCHANGE$SED.noU<-apply(CCHANGE[,c('zGDP.log','zeduc.int','zsiops.int','zservagri.int')], 1, mean, na.rm=TRUE) #Computing composite without urbanization
WVS$SED.noU<-apply(WVS[,c('zGDP.log','zeduc.int','zsiops.int','zservagri.int')], 1, mean, na.rm=TRUE) #Computing composite without urbanization
#'
## Socioeconomic Development without Urbanization ##
#'
#' Effect of SED (No U) on Practices nested by country 
resultSNU <- lmer(Practices.raw ~ SED.noU+(1|Country), CCHANGE, REML = FALSE)
summary(resultSNU) #Significant effect
r.squaredGLMM(resultSNU) #Marginal R2 (R2m)
#' Effect of SED (No U) on Values nested by country 
resultSNUw <- lmer(Values.raw ~ SED.noU+(1|Country), WVS, REML = FALSE)
summary(resultSNUw)#Significant effect
r.squaredGLMM(resultSNUw) #Marginal R2 (R2m)
#'
#### Running SED Analysis with the IPUMS Sample Only ####
#'
#' Effect of SED on Practices nested by country
resultS <- lmer(Practices.raw ~ SED+(1|Country), CCHANGE[CCHANGE$ID<44,], REML = FALSE)
summary(resultS) #Significant effect
r.squaredGLMM(resultS) #Marginal R2 (R2m) 
#' Effect of SED on Values nested by country 
resultSw <- lmer(Values.raw ~ SED+(1|Country), WVS[WVS$ID<44,], REML = FALSE)
summary(resultSw)#Significant effect
r.squaredGLMM(resultSw) #Marginal R2 (R2m)

#'
#### Running Analyses without the Preference for Independent Children ####
#'
WVS$Values.noI.raw <- apply(WVS[,c('zFriendsFam.raw','zPostMat.raw')], 1, mean, na.rm=TRUE) #Computing mean
#' Effect of Year on Values nested by country 
resultYwI <- lmer(Values.noI.raw ~ Year0+(1|Country), WVS, REML = FALSE)
summary(resultYwI) #Significant effect
r.squaredGLMM(resultYwI) #Marginal R2 (R2m)
#' Effect of SED on Values nested by country 
resultSwI <- lmer(Values.noI.raw ~ SED+(1|Country), WVS, REML = FALSE)
summary(resultSwI)#Significant effect
r.squaredGLMM(resultSwI) #Marginal R2 (R2m)
#' Effect of Disaster Frequency on Values nested by country 
resultDwI <- lmer(Values.noI.raw ~ disaster.log+(1|Country), WVS, REML = FALSE)
summary(resultDwI) #No effect
r.squaredGLMM(resultDwI) #Marginal R2 (R2m)
#' Effect of Pathogen Prevalence on Values nested by country 
resultPwI <- lmer(Values.noI.raw ~ PathogenPop.log+(1|Country), WVS, REML = FALSE)
summary(resultPwI) #Significant effect
r.squaredGLMM(resultPwI) #Marginal R2 (R2m)
#' Effect of Climate and SED on Values nested by country 
resultCSwI <- lmer(Values.noI.raw ~ TempDev.cw*SED.cw+(1|Country), WVS, REML = FALSE)
summary(resultCSwI) #Marginal interaction
r.squaredGLMM(resultCSwI) #Marginal R2 (R2m)
#### Running Analyses without Divorce ####
#'
CCHANGE$PracticesND.raw<-apply(CCHANGE[,c('zfamsize.raw.r','zalone.raw','zoldalone.raw')], 1, mean, na.rm=TRUE) #Computing mean with no life history (ND) variables
#'
## Change Over Time ##
#'
#' Effect of Year on Practices nested by country 
resultYND <- lmer(PracticesND.raw ~ Year0+(1|Country), CCHANGE, REML = FALSE)
summary(resultYND) #Significant effect
r.squaredGLMM(resultYND) #Marginal R2 (R2m)
#'
## Predictors of Change ##
#'
#' Effect of SED on Practices nested by country 
resultSND <- lmer(PracticesND.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultSND) #Significant effect
r.squaredGLMM(resultSND) #Marginal R2 (R2m) 
#' Effect of Disaster Frequency on Practices nested by country 
resultDND <- lmer(PracticesND.raw ~ disaster.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultDND) #Significant effect
r.squaredGLMM(resultDND) #Marginal R2 (R2m)
#' Effect of Pathogen Prevalence on Practices nested by country 
resultPND <- lmer(PracticesND.raw ~ PathogenPop.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultPND) #Significant effect
r.squaredGLMM(resultPND) #Marginal R2 (R2m)
#' Effect of Climate and SED on Practices nested by country 
resultCSND <- lmer(PracticesND.raw ~ TempDev.c*SED.c+(1|Country), CCHANGE, REML = FALSE)
summary(resultCSND) #Significant interaction
r.squaredGLMM(resultCSND) #Marginal R2 (R2m)
#' Calculating Delta AIC of the interaction by comparing change in AIC with and without interaction term 
resultCSbNDb <- lmer(PracticesND.raw ~ TempDev+SED+(1|Country), CCHANGE, REML = FALSE) # Practices analysis without interaction term
summary(resultCSbNDb)
#'Plotting interaction
quantile(CCHANGE$SED.c, na.rm=TRUE) #SED quantiles (practices)
plot((effect(term="TempDev.c:SED.c",mod=resultCSND,partial.residuals=TRUE,quantiles=c(.25,.75))),
     multiline=TRUE,main="Climate X SED",xlab="Temperature Deviation",ylab="Individualist Values") #Recreate plot in MS Excel using quantile data and MLM estimates
#'
#### Considering the Number of Children as Individualism ####
#'
cor(CCHANGE[,c('famsize','chborn', 'onechild','childless','alone','oldalone','divorce')], use='pairwise.complete.obs', method='kendall') #Checking correlations (famsize and chbord are reversed)
psych::alpha(as.matrix(CCHANGE[,c('famsize','chborn', 'onechild','childless','alone','oldalone','divorce')]), check.keys=TRUE) #Checking reliability, famsize and chborn are automatically reversed in this analysis (see - sign) reversed
CCHANGE$zchborn.raw<- scale(CCHANGE$chborn, center=TRUE, scale=TRUE) # Number of children
CCHANGE$zonechild.raw<- scale(CCHANGE$onechild, center=TRUE, scale=TRUE) # One child families
CCHANGE$zchildless.raw<- scale(CCHANGE$childless, center=TRUE, scale=TRUE) # Childless couples
CCHANGE$zchborn.raw<-CCHANGE$zchborn.raw[,] #Removing attributes R attached to the bottom of the standardized variables
CCHANGE$zonechild.raw<-CCHANGE$zonechild.raw[,]
CCHANGE$zchildless.raw<-CCHANGE$zchildless.raw[,]
CCHANGE$zfamsize.raw.r<-CCHANGE$zfamsize.raw*-1
CCHANGE$zchborn.raw.r<-CCHANGE$zchborn.raw*-1 #Reversing number of children
CCHANGE$PracticesWC.raw<-apply(CCHANGE[,c('zfamsize.raw.r','zchborn.raw.r','zonechild.raw','zchildless.raw','zalone.raw','zoldalone.raw')], 1, mean, na.rm=TRUE) #Computing mean
#'
## Change Over Time ##
#'
#' Effect of Year on Practices nested by country 
resultYWC <- lmer(PracticesWC.raw ~ Year0+(1|Country), CCHANGE, REML = FALSE)
summary(resultYWC) #Significant effect
r.squaredGLMM(resultYWC) #Marginal R2 (R2m)
#'
## Predictors of Change ##
#'
#' Effect of SED on Practices nested by country 
resultSWC <- lmer(PracticesWC.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultSWC) #Significant effect
r.squaredGLMM(resultSWC) #Marginal R2 (R2m) 
#' Effect of Disaster Frequency on Practices nested by country 
resultDWC <- lmer(PracticesWC.raw ~ disaster.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultDWC) #Significant effect
r.squaredGLMM(resultDWC) #Marginal R2 (R2m)
#' Effect of Pathogen Prevalence on Practices nested by country 
resultPWC <- lmer(PracticesWC.raw ~ PathogenPop.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultPWC) #Significant effect
r.squaredGLMM(resultPWC) #Marginal R2 (R2m)
#' Effect of Climate and SED on Practices nested by country 
resultCSWC <- lmer(PracticesWC.raw ~ TempDev.c*SED.c+(1|Country), CCHANGE, REML = FALSE)
summary(resultCSWC) #No significant interaction
r.squaredGLMM(resultCSWC) #Marginal R2 (R2m)
#' Calculating marginal R2 of the interaction by comparing change in R2m with and without interaction term 
resultCSbWCb <- lmer(PracticesWC.raw ~ TempDev+SED+(1|Country), CCHANGE, REML = FALSE) # Practices analysis without interaction term
summary(resultCSbWCb)
r.squaredGLMM(resultCSbWCb) #Marginal R2 (R2m)
#'Plotting interaction
quantile(CCHANGE$SED.c, na.rm=TRUE) #SED quantiles (practices)
plot((effect(term="TempDev.c:SED.c",mod=resultCSWC,partial.residuals=TRUE,quantiles=c(.25,.75))),
     multiline=TRUE,main="Climate X SED",xlab="Temperature Deviation",ylab="Individualist Values") #Recreate plot in MS Excel using quantile data and MLM estimates
#'
#### Utility of Socioeconomic Indicators from the WVS ####
#'
# Predictor Interpolations to Account for Missing Data (WVS)
describe(WVS$edu.wvs) #Educational Attainment (WVS)
describe(WVS$agri.wvs) #Agriculture Jobs (WVS)
describe(WVS$serv.wvs) #Service Jobs (WVS)
describe(WVS$inc.wvs) #Income (WVS)
WVS <- ddply(WVS,"ID",mutate,edu.wvs.int = tryCatch(na.approx(edu.wvs, na.rm = FALSE, maxgap = 10),error = function(e) NA))
WVS <- ddply(WVS,"ID",mutate,agri.wvs.int = tryCatch(na.approx(agri.wvs, na.rm = FALSE, maxgap = 10),error = function(e) NA))
WVS <- ddply(WVS,"ID",mutate,serv.wvs.int = tryCatch(na.approx(serv.wvs, na.rm = FALSE, maxgap = 10),error = function(e) NA))
WVS <- ddply(WVS,"ID",mutate,inc.wvs.int = tryCatch(na.approx(inc.wvs, na.rm = FALSE, maxgap = 10),error = function(e) NA))
WVS$servagri.wvs.int<-WVS$serv.wvs.int-WVS$agri.wvs.int #Computing white-collar vs. agriculture with imputed data
WVS$servagri.wvs.int[is.na(WVS$serv.wvs.int)] <- NA
WVS$servagri.wvs.int[is.na(WVS$agri.wvs.int)] <- NA
describe(WVS$edu.wvs.int)
describe(WVS$inc.wvs.int)
describe(WVS$servagri.wvs.int)
#' Computing Composite Socioeconomic Development (SED - WVS)
cor(WVS[,c('edu.wvs.int','servagri.wvs.int','GDP.log')], use='pairwise.complete.obs', method='kendall') #Checking correlations
WVS$zedu.wvs.int<- scale(WVS$edu.wvs.int, center=TRUE, scale=TRUE) #Standardizing variables
WVS$zinc.wvs.int<- scale(WVS$inc.wvs.int, center=TRUE, scale=TRUE)
WVS$zservagri.wvs.int<- scale(WVS$servagri.wvs.int, center=TRUE, scale=TRUE)
#' Effect of SED (WVS): White-collar vs. Agriculture on Practices nested by country 
resultSW_w <- lmer(Practices.raw ~ zservagri.wvs.int+(1|Country), WVS, REML = FALSE)
summary(resultSW_w)#No effect
r.squaredGLMM(resultSW_w) #Marginal R2 (R2m)
#' Effect of SED (WVS): White-collar vs. Agriculture on Values nested by country 
resultSWww <- lmer(Values.raw ~ zservagri.wvs.int+(1|Country), WVS, REML = FALSE)
summary(resultSWww)#Significant effect
r.squaredGLMM(resultSWww) #Marginal R2 (R2m)
#' Effect of SED (WVS): Education on Practices nested by country 
resultSE_w <- lmer(Practices.raw ~ zedu.wvs.int+(1|Country), WVS, REML = FALSE)
summary(resultSE_w)#No effect
r.squaredGLMM(resultSE_w) #Marginal R2 (R2m)
#' Effect of SED (WVS): Education on Values nested by country 
resultSEww <- lmer(Values.raw ~ zedu.wvs.int+(1|Country), WVS, REML = FALSE)
summary(resultSEww)#No effect
r.squaredGLMM(resultSEww) #Marginal R2 (R2m)
#'
#### Disaster Frequency, Pathogen Prevalence, and Climate as Mediators (Lagged Analyses) ####
#'
## Disaster Frequency 10 Years Later ##
#'
#' Effect of Disaster Frequency on Practices +10 nested by country 
resultD10 <- lmer(Practices10.raw ~ disaster.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultD10) #Significant effect
r.squaredGLMM(resultD10) #Marginal R2 (R2m)
#' Effect of Disaster Frequency on Values +10 nested by country 
resultD10w <- lmer(Values10.raw ~ disaster.log+(1|Country), WVS, REML = FALSE)
summary(resultD10w) #Marginal significant effect
r.squaredGLMM(resultD10w) #Marginal R2 (R2m)
#'
## Pathogen Prevalence 10 Years Later ##
#'
#' Effect of Pathogen Prevalence on Practices +10 nested by country 
resultP10 <- lmer(Practices10.raw ~ PathogenPop.log+(1|Country), CCHANGE, REML = FALSE)
summary(resultP10) #Not significant effect
r.squaredGLMM(resultP10) #Marginal R2 (R2m)
#' Effect of Pathogen Prevalence on Values +10 nested by country 
resultP10w <- lmer(Values10.raw ~ PathogenPop.log+(1|Country), WVS, REML = FALSE)
summary(resultP10w) #Not significant effect
r.squaredGLMM(resultP10w) #Marginal R2 (R2m)
#'
## Climate Interaction 10 Years Later
#'
#' Effect of Climate and SED on Practices nested by country 
resultCS10 <- lmer(Practices10.raw ~ TempDev.c*SED.c+(1|Country), CCHANGE, REML = FALSE)
summary(resultCS10) #Marginal effect
r.squaredGLMM(resultCS10) #Marginal R2 (R2m)
plot((effect(term="TempDev.c:SED.c",mod=resultCS10,partial.residuals=TRUE,quantiles=c(.25,.75))),
     multiline=TRUE,main="Climate x Socioeconomic Development",xlab="Temperature Deviation",ylab="Individualist Values") #Recreate plot in MS Excel using quantile data and MLM estimates
#' Effect of Climate and SED on Values nested by country 
resultCS10w <- lmer(Values10.raw ~ TempDev.cw*SED.cw+(1|Country), WVS, REML = FALSE)
summary(resultCS10w) #Significant effect
r.squaredGLMM(resultCS10w) #Marginal R2 (R2m)
plot((effect(term="TempDev.cw:SED.cw",mod=resultCS10w,partial.residuals=TRUE,quantiles=c(.25,.75))),
     multiline=TRUE,main="Climate x Socioeconomic Development",xlab="Temperature Deviation",ylab="Individualist Values") #Recreate plot in MS Excel using quantile data and MLM estimates
#' Calculating Delta AIC of the interaction by comparing change in AIC with and without interaction term 
resultCS10b <- lmer(Practices10.raw ~ TempDev+SED+(1|Country), CCHANGE, REML = FALSE) # Practices analysis without interaction term
summary(resultCS10b)
resultCS10wb <- lmer(Values10.raw ~ TempDev.cw+SED.cw+(1|Country), WVS, REML = FALSE) # Values analysis without interaction term
summary(resultCS10wb)
#'
#### Disaster Frequency, Pathogen Prevalence, and Climate as Mediators (Mediation Analyses with Individualism 10 Years Later) #### 
#'
#' Detaching lmerTest for mediation package to work 
detach("package:lmerTest")
library(mediation)
#'
## Disaster Frequency ##
#'
#' Year --> Disaster Frequency --> Practices 
subset.medD10 <- subset(CCHANGE,select=c('Year0', 'Country', 'disaster', 'Practices10.raw')) #Subsetting relevant variables
aD10 <- lmer(disaster ~ Year0+(1|Country), subset.medD10, REML = FALSE) #A path
cpD10 <- lmer(Practices10.raw ~ Year0+disaster+(1|Country), subset.medD10, REML = FALSE) #C Prime path (effect with mediator)
mediationD10 <- mediate(aD10, cpD10, boot = FALSE, treat='Year0', mediator='disaster',dropobs = TRUE,sims=5000) 
summary(mediationD10) #Mediation not significant (ACME)
#' Year --> Disaster Frequency --> Values 
subset.medD10w <- subset(WVS,select=c('Year0', 'Country', 'disaster', 'Values10.raw')) #Subsetting relevant variables
aD10w <- lmer(disaster ~ Year0+(1|Country), subset.medD10w, REML = FALSE) #A path
cpD10w <- lmer(Values10.raw ~ Year0+disaster+(1|Country), subset.medD10w, REML = FALSE) #C Prime path (effect with mediator)
mediationD10w <- mediate(aD10w, cpD10w, boot = FALSE, treat='Year0', mediator='disaster',dropobs = TRUE,sims=5000)
summary(mediationD10w) #Mediation not significant (ACME)
#'
## Pathogen Prevalence ##
#'
#' Year --> Pathogen Prevalence --> Practices 
subset.medP10 <- subset(CCHANGE,select=c('Year0', 'Country', 'PathogenPop', 'Practices10.raw')) #Subsetting relevant variables
aP10 <- lmer(PathogenPop ~ Year0+(1|Country), subset.medP10, REML = FALSE) #A path
cpP10 <- lmer(Practices10.raw ~ Year0+PathogenPop+(1|Country), subset.medP10, REML = FALSE) #C Prime path (effect with mediator)
mediationP10 <- mediate(aP10, cpP10, boot = FALSE, treat='Year0', mediator='PathogenPop',dropobs = TRUE,sims=5000) 
summary(mediationP10) #Mediation not significant (ACME)
#' Year --> Pathogen Prevalence --> Values 
subset.medP10w <- subset(WVS,select=c('Year0', 'Country', 'PathogenPop', 'Values10.raw')) #Subsetting relevant variables
aP10w <- lmer(PathogenPop ~ Year0+(1|Country), subset.medP10w, REML = FALSE) #A path
cpP10w <- lmer(Values10.raw ~ Year0+PathogenPop+(1|Country), subset.medP10w, REML = FALSE) #C Prime path (effect with mediator)
mediationP10w <- mediate(aP10w, cpP10w, boot = FALSE, treat='Year0', mediator='PathogenPop',dropobs = TRUE,sims=5000)
summary(mediationP10w) #Mediation not significant (ACME)
#'
## Climate Interaction ##
#'
CCHANGE$TempSED<-CCHANGE$TempDev.c*CCHANGE$SED.c #Interaction term between centered climate and SED
WVS$TempSED<-WVS$TempDev.cw*WVS$SED.cw #Interaction term between centered climate and SED
#' Year --> Climate Interaction --> Practices 
subset.medC10 <- subset(CCHANGE,select=c('Year0', 'Country', 'TempSED', 'Practices10.raw')) #Subsetting relevant variables
aC10 <- lmer(TempSED ~ Year0+(1|Country), subset.medC10, REML = FALSE) #A path
cpC10 <- lmer(Practices10.raw ~ Year0+TempSED+(1|Country), subset.medC10, REML = FALSE) #C Prime path (effect with mediator)
mediationC10 <- mediate(aC10, cpC10, boot = FALSE, treat='Year0', mediator='TempSED',dropobs = TRUE,sims=5000) 
summary(mediationC10) #Mediation not significant (ACME)
#' Year --> Climate Interaction --> Values 
subset.medC10w <- subset(WVS,select=c('Year0', 'Country', 'TempSED', 'Values10.raw')) #Subsetting relevant variables
aC10w <- lmer(TempSED ~ Year0+(1|Country), subset.medC10w, REML = FALSE) #A path
cpC10w <- lmer(Values10.raw ~ Year0+TempSED+(1|Country), subset.medC10w, REML = FALSE) #C Prime path (effect with mediator)
mediationC10w <- mediate(aC10w, cpC10w, boot = FALSE, treat='Year0', mediator='TempSED',dropobs = TRUE,sims=5000)
summary(mediationC10w) #Mediation not significant (ACME)
library(lmerTest) #Reloading lmerTest
#'
#### Testing the SED Lagged Effect with 5- and 8-Year Leads ####
#'
library(dplyr)
CCHANGE<-CCHANGE %>%
  group_by(ID)%>%
  mutate(Practices5.raw = lead(Practices.raw,n = 5L))
CCHANGE<-CCHANGE %>%
  group_by(ID)%>%
  mutate(Practices8.raw = lead(Practices.raw,n = 8L))
WVS<-WVS %>%
  group_by(ID)%>%
  mutate(Values5.raw = lead(Values.raw,n = 5L))
WVS<-WVS %>%
  group_by(ID)%>%
  mutate(Values8.raw = lead(Values.raw,n = 8L))
detach("package:dplyr")
#'
## Socioeconomic Development 5 Years Later ##
#'
#' Effect of SED on Practices +5 nested by country 
resultS5 <- lmer(Practices5.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultS5) #Significant leading effect of SED
r.squaredGLMM(resultS5)
#' Effect of SED on Values +5 nested by country 
resultS5w <- lmer(Values5.raw ~ SED+(1|Country), WVS, REML = FALSE)
summary(resultS5w) #Significant leading effect of SED
r.squaredGLMM(resultS5w)
#'
#'
## Socioeconomic Development 8 Years Later ##
#'
#' Effect of SED on Practices +8 nested by country 
resultS8 <- lmer(Practices8.raw ~ SED+(1|Country), CCHANGE, REML = FALSE)
summary(resultS8) #Significant leading effect of SED
r.squaredGLMM(resultS8)
#' Effect of SED on Values +8 nested by country 
resultS8w <- lmer(Values8.raw ~ SED+(1|Country), WVS, REML = FALSE)
summary(resultS8w) #Significant leading effect of SED
r.squaredGLMM(resultS8w)
#'
#'
#### Testing the SED Mediation Model Without a 10-Year Lead ####
#'
#' Detaching lmerTest for mediation package to work 
detach("package:lmerTest")
library(mediation)
#' Year --> SED --> Practices 
subset.medS <- subset(CCHANGE,select=c('Year0', 'Country', 'SED', 'Practices.raw')) #Subsetting relevant variables
aS <- lmer(SED ~ Year0+(1|Country), subset.medS, REML = FALSE) #A path
cpS <- lmer(Practices.raw ~ Year0+SED+(1|Country), subset.medS, REML = FALSE) #C Prime path (effect with mediator)
mediationS <- mediate(aS, cpS, boot = FALSE, treat='Year0', mediator='SED',dropobs = TRUE,sims=5000) 
summary(mediationS) #Significant mediation (ACME)
#' Year --> SED --> Values 
subset.medSw <- subset(WVS,select=c('Year0', 'Country', 'SED', 'Values.raw')) #Subsetting relevant variables
aSw <- lmer(SED ~ Year0+(1|Country), subset.medSw, REML = FALSE) #A path
cpSw <- lmer(Values.raw ~ Year0+SED+(1|Country), subset.medSw, REML = FALSE) #C Prime path (effect with mediator)
mediationSw <- mediate(aSw, cpSw, boot = FALSE, treat='Year0', mediator='SED',dropobs = TRUE,sims=5000)
summary(mediationSw) #Significant mediation (ACME)
library(lmerTest) #Reloading lmerTest
#'
#### Correlations between SED variables (Supplementary Table 2) ####
#'
cor(CCHANGE[,c('servagri.int','siops.int','educ.int','GDP.log','urban.int')], use='pairwise.complete.obs', method='kendall') #Checking correlations
#'
#### Correlations between individualist practices variables (Supplementary Table 3) ####
#'
cor(CCHANGE[,c('famsize','alone','oldalone','divorce','chborn','onechild','childless')], use='pairwise.complete.obs', method='kendall') #Checking correlations (famsize and chbord are reversed)
#'
#### Correlations between WVS variables (Supplementary Table 4) ####
#'
cor(CCHANGE[,c('FriendsFam.raw','ind.child','PostMat')], use='pairwise.complete.obs', method='kendall') #Checking correlations
#'
#### Slopes of Change in Individualism in Each Country (Supplementary Tables 6 and 7)####
#'
ddply(CCHANGE,c("Country"),function(x) length(which(!is.na(x$Practices.raw)))) #Getting sample sizes for practices
ddply(WVS,c("Country"),function(x) length(which(!is.na(x$Values.raw)))) #Getting sample sizes for values     
PracticesOrder<-ddply(CCHANGE[CCHANGE$ID<44&CCHANGE$ID!=35,],c("Country"),function(x) coef(summary(lm(Practices.raw ~ Year0,data=x)))[c(2,4)]) #Computing slope, SE
ValuesOrder<-ddply(WVS,c("Country"),function(x) coef(summary(lm(Values.raw ~ Year0,data=x)))[c(2,4)]) #Computing slope, SE
colnames(PracticesOrder)<-c('Country','Slope','SE')
colnames(ValuesOrder)<-c('Country','Slope','SE')
#### Pearson's Correlations between SED and GDP (Supplementary Table 8) ####
ddply(WVS,"Year",function(x) cor(x[,c('edu.wvs.int','GDP.log')], use='pairwise.complete.obs', method='pearson')[1,2])
ddply(WVS,"Year",function(x) cor(x[,c('servagri.wvs.int','GDP.log')], use='pairwise.complete.obs', method='pearson')[1,2])
ddply(WVS,"Year",function(x) cor(x[,c('educ.int','GDP.log')], use='pairwise.complete.obs', method='pearson')[1,2])