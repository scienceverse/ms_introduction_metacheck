#Study 2 analyses

#Change the working directory statement to match the one on your machine.

#read in data.
Study2dat <- read.table("Study2dat.csv", 
                  head = TRUE, sep = ",", fill = TRUE)
###############

#Read in libraries.
#lme4 and nlme both fit mixed-effects models.
if(!"lme4" %in% installed.packages()) install.packages("lme4")
library(lme4)
#geepack and gee both fit generalized estimating equations.
if(!"geepack" %in% installed.packages()) install.packages("geepack")
library(geepack)
if(!"gee" %in% installed.packages()) install.packages("gee")
library(gee)
#mediation has functions for 1-1-1 mediation.
if(!"mediation" %in% installed.packages()) install.packages("mediation")
library(mediation)
#lmerTest adds p-values to lme4 summaries, using Satterthwaite approximation.
if(!"lmerTest" %in% installed.packages()) install.packages("lmerTest")
library(lmerTest)
if(!"gvlma" %in% installed.packages()) install.packages("gvlma")
library(gvlma)
if(!"MASS" %in% installed.packages()) install.packages("MASS")
library(MASS)
if(!"car" %in% installed.packages()) install.packages("car")
library(car)
install.packages("psychometric")
library(psychometric)
if(!"BSagri" %in% installed.packages()) install.packages("BSagri")
library(BSagri)



#Function to plot residuals.
plot.resid <- function(mod){
  fitted <- mod$fitted.values
  actual <- mod$fitted.values + mod$residuals
  plot(fitted, actual)
}



#function for computing pseduo R2

R2.full.geepack <- function(mod){
  beta <- mod$coefficients
  k <- length(beta)
  Vmat <- vcov(mod)
  Cmat <- cbind(rep(0,k-1), diag(k-1))
  Q <- t(Cmat %*% beta) %*% solve(Cmat %*% Vmat %*% t(Cmat)) %*% (Cmat %*% beta)
  N <- length(mod$residuals)
  return((Q/N)/ (1 + Q/N))
}

#####################COMPUTING COVARIATES


##Compute total population
Study2dat$Poptot <- Study2dat$BlackPop + Study2dat$WhitePop 

#Compute proportion Black popuation
Study2dat$Pop_propBlack <- Study2dat$BlackPop/(Study2dat$BlackPop + Study2dat$WhitePop)

#make average income var interpretable
Study2dat$Av_Inc_2005_2013_div10000 <- Study2dat$Av_Inc_2005_2013/10000

### Center predictors and covariates in analyses
Study2dat$WhiteExplicitWeightedByAgeGroupC <- Study2dat$WhiteExplicitWeightedByAgeGroup - mean(Study2dat$WhiteExplicitWeightedByAgeGroup, na.rm = TRUE)
Study2dat$WhiteImplicitWeightedByAgeGroupC <- Study2dat$WhiteImplicitWeightedByAgeGroup - mean(Study2dat$WhiteImplicitWeightedByAgeGroup, na.rm = TRUE)
Study2dat$PoptotC <- (log10(Study2dat$Poptot) - mean(log10(Study2dat$Poptot), na.rm = TRUE))
Study2dat$Pop_propBlackC <- Study2dat$Pop_propBlack - mean(Study2dat$Pop_propBlack, na.rm = TRUE)
Study2dat$Av_Inc_2005_2013_div10000C <- Study2dat$Av_Inc_2005_2013_div10000 - mean(Study2dat$Av_Inc_2005_2013_div10000, na.rm = TRUE) 
Study2dat$Av_Edu_2009_2013C <- Study2dat$Av_Edu_2009_2013 - mean(Study2dat$Av_Edu_2009_2013, na.rm = TRUE) 
Study2dat$Av_Inc_2005_2013C <- Study2dat$Av_Inc_2005_2013 - mean(Study2dat$Av_Inc_2005_2013, na.rm = TRUE) 
Study2dat$Dissim_NHW_vs_NHB_2000_2010_avC <- Study2dat$Dissim_NHW_vs_NHB_2000_2010_av - mean(Study2dat$Dissim_NHW_vs_NHB_2000_2010_av, na.rm = TRUE)
Study2dat$TotalBlackMoveC <- Study2dat$TotalBlackMove - mean(Study2dat$TotalBlackMove, na.rm = TRUE) #Black geo mobility
Study2dat$Av_Unemp_2005_2013C <- Study2dat$Av_Unemp_2005_2013 - mean(Study2dat$Av_Unemp_2005_2013, na.rm = TRUE) 
Study2dat$Av_Pov_2005_2013C <- Study2dat$Av_Pov_2005_2013 - mean(Study2dat$Av_Pov_2005_2013, na.rm = TRUE) 
Study2dat$HousingDensity2000.2010C <- Study2dat$HousingDensity2000.2010 - mean(Study2dat$HousingDensity2000.2010, na.rm = TRUE)
Study2dat$AgeBiasExplicit_AgeGrpWC <- Study2dat$AgeBiasExplicit_AgeGrpW - mean(Study2dat$AgeBiasExplicit_AgeGrpW, na.rm = TRUE)
Study2dat$AgeBiasImplicit_AgeGrpWC <- Study2dat$AgeBiasImplicit_AgeGrpW - mean(Study2dat$AgeBiasImplicit_AgeGrpW, na.rm = TRUE)


#centering bias computed from 2003 - 2012 only
Study2dat$Explicit2003.2012AgeGrpWeightC <- Study2dat$Explicit2003.2012AgeGrpWeight - mean(Study2dat$Explicit2003.2012AgeGrpWeight, na.rm = TRUE)
Study2dat$Implicit2003.2012AgeGrpWeightC <- Study2dat$Implicit2003.2012AgeGrpWeight - mean(Study2dat$Implicit2003.2012AgeGrpWeight, na.rm = TRUE)


#Centering unweighted bias
Study2dat$ExpUnweightedC <- Study2dat$ExpUnweighted - mean(Study2dat$ExpUnweighted, na.rm = TRUE)
Study2dat$ImpUnweightedC <- Study2dat$ImpUnweighted - mean(Study2dat$ImpUnweighted, na.rm = TRUE)


#neoplasm centered
Study2dat$AgeAdjustedRate.NeoplasmsC <- Study2dat$AgeAdjustedRate.Neoplasms - mean(Study2dat$AgeAdjustedRate.Neoplasms, na.rm = TRUE)


############# MAIN GEE ANALYSES


#Predicting circulatory death rate
vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
"WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
"RaceEffects",
"PoptotC", "Pop_propBlackC", "Av_Inc_2005_2013_div10000C",
 "Av_Edu_2009_2013C", 
"N_fipsWhite", "FIPS",  "TotalBlackMoveC", 
"Dissim_NHW_vs_NHB_2000_2010_avC",
"Av_Unemp_2005_2013C", "HousingDensity2000.2010C",
"Av_Pov_2005_2013C", "AgeBiasExplicit_AgeGrpWC", "AgeBiasImplicit_AgeGrpWC" )
for(i in vars)
print(summary(Study2dat[,names(Study2dat) == i]))


#this tells us how many cases were valid for for Blacks (White == 0) and Whites (White == 1)
dat.check <- na.omit(Study2dat[, names(Study2dat) %in% vars])
sum(dat.check$White == 1)
sum(dat.check$White == 0)

#main effects are slopes for Blacks (White=0). This is Table 3 in paper. 
#Death rate multiplied by 100 to generate estimates for deaths per 100,000.
gee.circ <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
( WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC + PoptotC + 
Pop_propBlackC 
+ Av_Edu_2009_2013C 
+ Av_Inc_2005_2013_div10000C
+ Dissim_NHW_vs_NHB_2000_2010_avC 
+ TotalBlackMoveC 
+ Av_Unemp_2005_2013C
+ Av_Pov_2005_2013C
+ HousingDensity2000.2010C
+ AgeBiasExplicit_AgeGrpWC
+ AgeBiasImplicit_AgeGrpWC), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
weights = N_fipsWhite,
id = FIPS, corstr = "exchangeable")
summary(gee.circ)
plot.resid(gee.circ)
R2.full.geepack(gee.circ)

#main effects are slopes for Whites (Black=0)
gee.circ <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ Black * 
                     ( WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC + PoptotC + 
                         Pop_propBlackC 
                       + Av_Edu_2009_2013C 
                       + Av_Inc_2005_2013_div10000C
                       + Dissim_NHW_vs_NHB_2000_2010_avC 
                       + TotalBlackMoveC 
                       + Av_Unemp_2005_2013C
                       + Av_Pov_2005_2013C
                       + HousingDensity2000.2010C
                       + AgeBiasExplicit_AgeGrpWC
                       + AgeBiasImplicit_AgeGrpWC), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
                   weights = N_fipsWhite,
                   id = FIPS, corstr = "exchangeable")
summary(gee.circ)
plot.resid(gee.circ)
R2.full.geepack(gee.circ)



#Adding cancer to the model

vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
"WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
"RaceEffects",
"PoptotC", "Pop_propBlackC", "Av_Inc_2005_2013_div10000C",
"Av_Edu_2009_2013C", 
"N_fipsWhite", "FIPS",  "TotalBlackMoveC", 
"Dissim_NHW_vs_NHB_2000_2010_avC",
"Av_Unemp_2005_2013C", "HousingDensity2000.2010C",
"Av_Pov_2005_2013C", "AgeBiasExplicit_AgeGrpWC", "AgeBiasImplicit_AgeGrpWC", "AgeAdjustedRate.NeoplasmsC" )
for(i in vars)
print(summary(Study2dat[,names(Study2dat) == i]))


gee.circ2 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
( WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC + PoptotC + 
Pop_propBlackC 
+ Av_Edu_2009_2013C 
+ Av_Inc_2005_2013_div10000C
+ Dissim_NHW_vs_NHB_2000_2010_avC 
+ TotalBlackMoveC 
+ Av_Unemp_2005_2013C
+ Av_Pov_2005_2013C
+ HousingDensity2000.2010C
+ AgeBiasExplicit_AgeGrpWC
+ AgeBiasImplicit_AgeGrpWC
+ AgeAdjustedRate.NeoplasmsC
), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
weights = N_fipsWhite,
id = FIPS, corstr = "exchangeable")
summary(gee.circ2)
plot.resid(gee.circ2)
R2.full.geepack(gee.circ2)



# predicting neoplasm

vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
"WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
"RaceEffects",
"PoptotC", "Pop_propBlackC", "Av_Inc_2005_2013_div10000C",
"Av_Edu_2009_2013C", 
"N_fipsWhite", "FIPS",  "TotalBlackMoveC", 
"Dissim_NHW_vs_NHB_2000_2010_avC",
"Av_Unemp_2005_2013C", "HousingDensity2000.2010C",
"Av_Pov_2005_2013C",  "AgeAdjustedRate.NeoplasmsC",
 "AgeBiasExplicit_AgeGrpWC", "AgeBiasImplicit_AgeGrpWC")
for(i in vars)
print(summary(Study2dat[,names(Study2dat) == i]))



gee.neo <- geeglm(AgeAdjustedRate.NeoplasmsC * 100 ~ White * 
( WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC + PoptotC + 
Pop_propBlackC 
+ Av_Edu_2009_2013C 
+ Av_Inc_2005_2013_div10000C
+ Dissim_NHW_vs_NHB_2000_2010_avC 
+ TotalBlackMoveC 
+ Av_Unemp_2005_2013C
+ Av_Pov_2005_2013C
+ HousingDensity2000.2010C
+ AgeBiasExplicit_AgeGrpWC
+ AgeBiasImplicit_AgeGrpWC
), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
weights = N_fipsWhite,
id = FIPS, corstr = "exchangeable")
summary(gee.neo)
plot.resid(gee.neo)
R2.full.geepack(gee.neo)




###### IN SUPPLEMENTARY MATERIALS

#Models without covariates

  vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
  "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
  "RaceEffects",
  "N_fipsWhite", "FIPS" )
  for(i in vars)
  print(summary(Study2dat[,names(Study2dat) == i]))
  
#Table S5
  gee.circ3 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
                       ( WhiteExplicitWeightedByAgeGroupC 
                       ), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
                     weights = N_fipsWhite,
                     id = FIPS, corstr = "exchangeable")
  summary(gee.circ3)
  plot.resid(gee.circ3)
  R2.full.geepack(gee.circ3)

#Table S6
  gee.circ4 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
  ( WhiteImplicitWeightedByAgeGroupC 
  ), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
  weights = N_fipsWhite,
  id = FIPS, corstr = "exchangeable")
  summary(gee.circ4)
  plot.resid(gee.circ4)
  R2.full.geepack(gee.circ4)

  

#Modeling unweighted bias

vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
"RaceEffects", "ExpUnweightedC", "ImpUnweightedC",
"PoptotC", "Pop_propBlackC", "Av_Inc_2005_2013_div10000C",
"Av_Edu_2009_2013C", 
"N_fipsWhite", "FIPS",  "TotalBlackMoveC", 
"Dissim_NHW_vs_NHB_2000_2010_avC",
"Av_Unemp_2005_2013C", "HousingDensity2000.2010C",
"Av_Pov_2005_2013C", "AgeBiasExplicit_AgeGrpWC", "AgeBiasImplicit_AgeGrpWC" )
for(i in vars)
print(summary(Study2dat[,names(Study2dat) == i]))


#Main effects are slopes for Blacks (White=0)
gee.circ5 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
( ExpUnweightedC + ImpUnweightedC + PoptotC + 
Pop_propBlackC 
+ Av_Edu_2009_2013C 
+ Av_Inc_2005_2013_div10000C
+ Dissim_NHW_vs_NHB_2000_2010_avC 
+ TotalBlackMoveC 
+ Av_Unemp_2005_2013C
+ Av_Pov_2005_2013C
+ HousingDensity2000.2010C
+ AgeBiasExplicit_AgeGrpWC
+ AgeBiasImplicit_AgeGrpWC
), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
weights = N_fipsWhite,
id = FIPS, corstr = "exchangeable")
summary(gee.circ5)
plot.resid(gee.circ5)
R2.full.geepack(gee.circ5)

#Main effects are slopes for Whites (Black=0)
gee.circ5 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ Black * 
                      ( ExpUnweightedC + ImpUnweightedC + PoptotC + 
                          Pop_propBlackC 
                        + Av_Edu_2009_2013C 
                        + Av_Inc_2005_2013_div10000C
                        + Dissim_NHW_vs_NHB_2000_2010_avC 
                        + TotalBlackMoveC 
                        + Av_Unemp_2005_2013C
                        + Av_Pov_2005_2013C
                        + HousingDensity2000.2010C
                        + AgeBiasExplicit_AgeGrpWC
                        + AgeBiasImplicit_AgeGrpWC
                      ), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
                    weights = N_fipsWhite,
                    id = FIPS, corstr = "exchangeable")
summary(gee.circ5)
plot.resid(gee.circ5)
R2.full.geepack(gee.circ5)


#modeling with lmm instead of GEE

lmm.circ<- lmer(AgeAdjustedRate.Circulatory * 100 ~ White * 
( WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC + PoptotC + 
    Pop_propBlackC 
  + Av_Edu_2009_2013C 
  + Av_Inc_2005_2013_div10000C
  + Dissim_NHW_vs_NHB_2000_2010_avC 
  + TotalBlackMoveC 
  + Av_Unemp_2005_2013C
  + Av_Pov_2005_2013C
  + HousingDensity2000.2010C
  + AgeBiasExplicit_AgeGrpWC
  + AgeBiasImplicit_AgeGrpWC) + (1|FIPS), weights = N_fipsWhite,  data = Study2dat)
summary(lmm.circ)
plot(lmm.circ)
qqnorm(resid(lmm.circ))



#Same analyses as above, but with only 2003-2012 age-group weighted bias data

vars <- c( "AgeAdjustedRate.Circulatory", "White", "Black", 
           "Explicit2003.2012AgeGrpWeightC", "Implicit2003.2012AgeGrpWeightC", 
           "RaceEffects",
           "PoptotC", "Pop_propBlackC", "Av_Inc_2005_2013_div10000C",
           "Av_Edu_2009_2013C", 
           "N_fipsWhite", "FIPS",  "TotalBlackMoveC", 
           "Dissim_NHW_vs_NHB_2000_2010_avC",
           "Av_Unemp_2005_2013C", "HousingDensity2000.2010C",
           "Av_Pov_2005_2013C",  "AgeBiasExplicit_AgeGrpWC", "AgeBiasImplicit_AgeGrpWC", "AgeAdjustedRate.NeoplasmsC" )
for(i in vars)
  print(summary(Study2dat[,names(Study2dat) == i]))


gee.circ6 <- geeglm(AgeAdjustedRate.Circulatory * 100 ~ White * 
                     ( Explicit2003.2012AgeGrpWeightC + Implicit2003.2012AgeGrpWeightC + PoptotC + 
                         Pop_propBlackC 
                       + Av_Edu_2009_2013C 
                       + Av_Inc_2005_2013_div10000C
                       + Dissim_NHW_vs_NHB_2000_2010_avC 
                       + TotalBlackMoveC 
                       + Av_Unemp_2005_2013C
                       + Av_Pov_2005_2013C
                       + HousingDensity2000.2010C
                       + AgeBiasExplicit_AgeGrpWC
                       + AgeBiasImplicit_AgeGrpWC
                       + AgeAdjustedRate.NeoplasmsC
                     ), data = na.omit(Study2dat[,names(Study2dat) %in% vars]), 
                   weights = N_fipsWhite,
                   id = FIPS, corstr = "exchangeable")
summary(gee.circ6)
plot.resid(gee.circ6)
R2.full.geepack(gee.circ6)



