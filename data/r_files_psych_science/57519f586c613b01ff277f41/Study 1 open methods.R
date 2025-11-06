#Study 1 Analyses 

#Change the working directory statement to match the one on your machine.

#read in data
Study1dat <- read.table("Study1dat.csv", head = TRUE, sep = ",", fill = TRUE)

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


#Function to plot residuals
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




############# COMPUTING COVARIATES FOR ANALYSES

##Compute total population
Study1dat$Poptot <- Study1dat$BlackPop + Study1dat$WhitePop 

#Compute proportion Black population
Study1dat$Pop_propBlack <- Study1dat$BlackPop/(Study1dat$BlackPop + Study1dat$WhitePop)

#Make average income var interpretable
Study1dat$Av_Inc_2005_2013_div10000 <- Study1dat$Av_Inc_2005_2013/10000

### Center predictors and covariates in analyses
Study1dat$WhiteExplicitWeightedByAgeGroupC <- Study1dat$WhiteExplicitWeightedByAgeGroup - mean(Study1dat$WhiteExplicitWeightedByAgeGroup, na.rm = TRUE)
Study1dat$WhiteImplicitWeightedByAgeGroupC <- Study1dat$WhiteImplicitWeightedByAgeGroup - mean(Study1dat$WhiteImplicitWeightedByAgeGroup, na.rm = TRUE)
Study1dat$AGE_meanC <- Study1dat$AGE_mean - mean(Study1dat$AGE_mean, na.rm = TRUE)
Study1dat$PoptotC <- (log10(Study1dat$Poptot) - mean(log10(Study1dat$Poptot), na.rm = TRUE))
Study1dat$Pop_propBlackC <- Study1dat$Pop_propBlack - mean(Study1dat$Pop_propBlack, na.rm = TRUE)
Study1dat$Av_Inc_2005_2013_div10000C <- Study1dat$Av_Inc_2005_2013_div10000 - mean(Study1dat$Av_Inc_2005_2013_div10000, na.rm = TRUE) 
Study1dat$Av_Edu_2009_2013C <- Study1dat$Av_Edu_2009_2013 - mean(Study1dat$Av_Edu_2009_2013, na.rm = TRUE) 
Study1dat$Av_Inc_2005_2013C <- Study1dat$Av_Inc_2005_2013 - mean(Study1dat$Av_Inc_2005_2013, na.rm = TRUE) 
Study1dat$Dissim_NHW_vs_NHB_2000_2010_avC <- Study1dat$Dissim_NHW_vs_NHB_2000_2010_av - mean(Study1dat$Dissim_NHW_vs_NHB_2000_2010_av, na.rm = TRUE)
Study1dat$TotalBlackMoveC <- Study1dat$TotalBlackMove - mean(Study1dat$TotalBlackMove, na.rm = TRUE) 
Study1dat$Av_Unemp_2005_2013C <- Study1dat$Av_Unemp_2005_2013 - mean(Study1dat$Av_Unemp_2005_2013, na.rm = TRUE) 
Study1dat$Av_Pov_2005_2013C <- Study1dat$Av_Pov_2005_2013 - mean(Study1dat$Av_Pov_2005_2013, na.rm = TRUE) 
Study1dat$HousingDensity2000.2010C <- Study1dat$HousingDensity2000.2010 - mean(Study1dat$HousingDensity2000.2010, na.rm = TRUE)
Study1dat$AgeBiasExplicit_AgeGrpWC <- Study1dat$AgeBiasExplicit_AgeGrpW - mean(Study1dat$AgeBiasExplicit_AgeGrpW, na.rm = TRUE)
Study1dat$AgeBiasImplicit_AgeGrpWC <- Study1dat$AgeBiasImplicit_AgeGrpW - mean(Study1dat$AgeBiasImplicit_AgeGrpW, na.rm = TRUE)
Study1dat$ExpUnweightedC <- Study1dat$ExpUnweighted - mean(Study1dat$ExpUnweighted, na.rm = TRUE)
Study1dat$ImpUnweightedC <- Study1dat$ImpUnweighted - mean(Study1dat$ImpUnweighted, na.rm = TRUE)


#Compute circ diagnosis var
Study1dat$MR_CircDiag <- (Study1dat$MR_CVDCRHD4_mean + Study1dat$MR_CVDINFR4_mean)/2   

#centering bias computed from 2003 - 2012 only
Study1dat$Explicit2003.2012AgeGrpWeightC <- Study1dat$Explicit2003.2012AgeGrpWeight - mean(Study1dat$Explicit2003.2012AgeGrpWeight, na.rm = TRUE)
Study1dat$Implicit2003.2012AgeGrpWeightC <- Study1dat$Implicit2003.2012AgeGrpWeight - mean(Study1dat$Implicit2003.2012AgeGrpWeight, na.rm = TRUE)


################## MAIN GEE ANALYSES 

#Table 1 in paper - PREDICTING ACCESS TO HEALTH CARE
vars <- c( "MR_MDCOST_mean", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "SexEffects", "AGE_meanC", "PoptotC",
          "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
             "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}

#Main effects show Black slope (White = 0). Subtract 1 from MR_MDCOST since the CDC health survey coded responses as 1=yes, 2=no.
gee.MDCOST.Black <- geeglm(MR_MDCOST_mean - 1 ~ White * 
                     (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                      + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST.Black)
plot.resid(gee.MDCOST.Black)
R2.full.geepack(gee.MDCOST.Black)


#Main effects show White slope (Black = 0)
gee.MDCOST.White <- geeglm(MR_MDCOST_mean - 1 ~ Black * 
                     (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                      + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST.White)
plot.resid(gee.MDCOST.White)
R2.full.geepack(gee.MDCOST.White)




#Explicit and implicit biases interact?
vars <- c( "MR_MDCOST_mean", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "SexEffects", "AGE_meanC", "PoptotC",
           "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
           "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


gee.MDCOST2 <- geeglm(MR_MDCOST_mean - 1 ~ White * 
                     (WhiteExplicitWeightedByAgeGroupC * WhiteImplicitWeightedByAgeGroupC
                      + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST2)
plot.resid(gee.MDCOST2)
R2.full.geepack(gee.MDCOST2)


#Race x explicit race bias interaction further moderated by sex?
gee.MDCOST3 <- geeglm(MR_MDCOST_mean - 1 ~ White * SexEffects *
                     (WhiteExplicitWeightedByAgeGroupC * WhiteImplicitWeightedByAgeGroupC
                      + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST3)
plot.resid(gee.MDCOST3)
R2.full.geepack(gee.MDCOST3)









####Predicting Circulatory Diagnosis - Table 2 in paper
vars <- c( "MR_CircDiag", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "SexEffects", "AGE_meanC", "PoptotC",
           "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
           "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


#Main effects show slopes for Blacks (White=0). Subtract MR_CircDiag from 2 since CDC health survey coded responses as 1=yes, 2=no. 
gee.circDiag <- geeglm(2 - MR_CircDiag  ~ White * 
                     (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                      + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.circDiag)
plot.resid(gee.circDiag)
R2.full.geepack(gee.circDiag)



#predicting BMI (in supplements)
vars <- c( "BMI_div100", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "SexEffects", "AGE_meanC", "PoptotC",
           "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
           "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


gee.bmi <- geeglm(BMI_div100  ~ White * 
                     (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                      + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                      + Av_Edu_2009_2013C
                      + Av_Inc_2005_2013_div10000C  
                      + Dissim_NHW_vs_NHB_2000_2010_avC
                      + TotalBlackMoveC 
                      + Av_Unemp_2005_2013C 
                      + Av_Pov_2005_2013C
                      + HousingDensity2000.2010C
                      + AgeBiasImplicit_AgeGrpWC
                      + AgeBiasExplicit_AgeGrpWC
                     ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.bmi)
plot.resid(gee.bmi)
R2.full.geepack(gee.bmi)




#Predicting access to health care with no covariates in model (in supplements)
vars <- c( "MR_MDCOST_mean", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "N_CDC_calls", "FIPS")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


#Table S1
gee.MDCOST4 <- geeglm(MR_MDCOST_mean - 1~ White * 
                        (WhiteExplicitWeightedByAgeGroupC ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                      weights = N_CDC_calls,
                      id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST4)
plot.resid(gee.MDCOST4)
R2.full.geepack(gee.MDCOST4)

#Table S2
gee.MDCOST5 <- geeglm(MR_MDCOST_mean - 1 ~ White * 
                     (WhiteImplicitWeightedByAgeGroupC ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                   weights = N_CDC_calls,
                   id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST5)
plot.resid(gee.MDCOST5)
R2.full.geepack(gee.MDCOST5)




#Predicting CircDiag with no covariates in model (in supplements)
vars <- c( "MR_CircDiag", "White", "RaceEffects",
           "Black", "WhiteExplicitWeightedByAgeGroupC", "WhiteImplicitWeightedByAgeGroupC", 
           "N_CDC_calls", "FIPS")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}



#Table S3
gee.CircDiag2 <- geeglm(2 - MR_CircDiag ~ White * 
                          (WhiteExplicitWeightedByAgeGroupC ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                        weights = N_CDC_calls,
                        id = FIPS, corstr = "exchangeable")
summary(gee.CircDiag2)
plot.resid(gee.CircDiag2)
R2.full.geepack(gee.CircDiag2)


#Table S4
gee.CircDiag3 <- geeglm(2 - MR_CircDiag ~ White * 
                          (WhiteImplicitWeightedByAgeGroupC ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                        weights = N_CDC_calls,
                        id = FIPS, corstr = "exchangeable")
summary(gee.CircDiag3)
plot.resid(gee.CircDiag3)
R2.full.geepack(gee.CircDiag3)






#MODELING ACCESS TO HEALTH CARE WITH BIAS ESTIMATES THAT DID NOT APPLY POST-STRATIFICATION WEIGHTS (in supplements)

#Unweighted Analysis

vars <- c( "MR_MDCOST_mean", "White", "RaceEffects",
           "Black", "ExpUnweightedC", "ImpUnweightedC", 
           "SexEffects", "AGE_meanC", "PoptotC",
           "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
           "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


#Main effects show Black slopes
gee.MCOST.un <- geeglm(MR_MDCOST_mean - 1 ~ White * 
                         (ExpUnweightedC + ImpUnweightedC
                          + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                          + Av_Edu_2009_2013C
                          + Av_Inc_2005_2013_div10000C  
                          + Dissim_NHW_vs_NHB_2000_2010_avC
                          + TotalBlackMoveC 
                          + Av_Unemp_2005_2013C 
                          + Av_Pov_2005_2013C
                          + HousingDensity2000.2010C
                          + AgeBiasImplicit_AgeGrpWC
                          + AgeBiasExplicit_AgeGrpWC
                         ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                       weights = N_CDC_calls,
                       id = FIPS, corstr = "exchangeable")
summary(gee.MCOST.un)
plot.resid(gee.MCOST.un)
R2.full.geepack(gee.MCOST.un)


#Main effects show White slopes
gee.MCOST.un <- geeglm(MR_MDCOST_mean - 1 ~ Black * 
                         (ExpUnweightedC + ImpUnweightedC
                          + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                          + Av_Edu_2009_2013C
                          + Av_Inc_2005_2013_div10000C  
                          + Dissim_NHW_vs_NHB_2000_2010_avC
                          + TotalBlackMoveC 
                          + Av_Unemp_2005_2013C 
                          + Av_Pov_2005_2013C
                          + HousingDensity2000.2010C
                          + AgeBiasImplicit_AgeGrpWC
                          + AgeBiasExplicit_AgeGrpWC
                         ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                       weights = N_CDC_calls,
                       id = FIPS, corstr = "exchangeable")
summary(gee.MCOST.un)
plot.resid(gee.MCOST.un)
R2.full.geepack(gee.MCOST.un)





#modeling weighted bias 2003-2012 (in supplements)

vars <- c( "MR_MDCOST_mean", "White", "RaceEffects",
           "Black", "Explicit2003.2012AgeGrpWeightC", "Implicit2003.2012AgeGrpWeightC", 
           "SexEffects", "AGE_meanC", "PoptotC",
           "Av_Unemp_2005_2013C","Av_Edu_2009_2013C",
           "Pop_propBlackC", "AgeBiasImplicit_AgeGrpWC", "AgeBiasExplicit_AgeGrpWC",
           "N_CDC_calls", "FIPS", "TotalBlackMoveC", "Dissim_NHW_vs_NHB_2000_2010_avC", "Av_Pov_2005_2013C",
           "HousingDensity2000.2010C", 
           "Av_Inc_2005_2013_div10000C")
for(i in vars){
  print(summary(Study1dat[,names(Study1dat) == i]))
}


#Main effects show Black slope
gee.MDCOST.2003.2012 <- geeglm(MR_MDCOST_mean - 1 ~ White * 
                                 (Explicit2003.2012AgeGrpWeightC + Implicit2003.2012AgeGrpWeightC
                                  + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                                  + Av_Edu_2009_2013C
                                  + Av_Inc_2005_2013_div10000C  
                                  + Dissim_NHW_vs_NHB_2000_2010_avC
                                  + TotalBlackMoveC 
                                  + Av_Unemp_2005_2013C 
                                  + Av_Pov_2005_2013C
                                  + HousingDensity2000.2010C
                                  + AgeBiasImplicit_AgeGrpWC
                                  + AgeBiasExplicit_AgeGrpWC
                                 ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                               weights = N_CDC_calls,
                               id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST.2003.2012)
plot.resid(gee.MDCOST.2003.2012)
R2.full.geepack(gee.MDCOST.2003.2012)

#Main effects show White slope
gee.MDCOST.2003.2012 <- geeglm(MR_MDCOST_mean - 1 ~ Black * 
                                 (Explicit2003.2012AgeGrpWeightC + Implicit2003.2012AgeGrpWeightC
                                  + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                                  + Av_Edu_2009_2013C
                                  + Av_Inc_2005_2013_div10000C  
                                  + Dissim_NHW_vs_NHB_2000_2010_avC
                                  + TotalBlackMoveC 
                                  + Av_Unemp_2005_2013C 
                                  + Av_Pov_2005_2013C
                                  + HousingDensity2000.2010C
                                  + AgeBiasImplicit_AgeGrpWC
                                  + AgeBiasExplicit_AgeGrpWC
                                 ), data = na.omit(Study1dat[,names(Study1dat) %in% vars]), 
                               weights = N_CDC_calls,
                               id = FIPS, corstr = "exchangeable")
summary(gee.MDCOST.2003.2012)
plot.resid(gee.MDCOST.2003.2012)
R2.full.geepack(gee.MDCOST.2003.2012)


#Modleing race x explicit effect with LMM instead of GEE (in supplements)

lmm.MDCOST<- lmer(MR_MDCOST_mean-1 ~ White *
                 (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                  + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                  + Av_Edu_2009_2013C
                  + Av_Inc_2005_2013_div10000C  
                  + Dissim_NHW_vs_NHB_2000_2010_avC
                  + TotalBlackMoveC 
                  + Av_Unemp_2005_2013C 
                  + Av_Pov_2005_2013C
                  + HousingDensity2000.2010C
                  + AgeBiasImplicit_AgeGrpWC
                  + AgeBiasExplicit_AgeGrpWC) + (1|FIPS), weights = N_CDC_calls,  data = Study1dat)
summary(lmm.MDCOST)
plot(lmm.MDCOST)
qqnorm(resid(lmm.MDCOST))


lmm.MDCOST<- lmer(MR_MDCOST_mean-1 ~ Black *
                    (WhiteExplicitWeightedByAgeGroupC + WhiteImplicitWeightedByAgeGroupC
                     + SexEffects + AGE_meanC + PoptotC + Pop_propBlackC 
                     + Av_Edu_2009_2013C
                     + Av_Inc_2005_2013_div10000C  
                     + Dissim_NHW_vs_NHB_2000_2010_avC
                     + TotalBlackMoveC 
                     + Av_Unemp_2005_2013C 
                     + Av_Pov_2005_2013C
                     + HousingDensity2000.2010C
                     + AgeBiasImplicit_AgeGrpWC
                     + AgeBiasExplicit_AgeGrpWC) + (1|FIPS), weights = N_CDC_calls,  data = Study1dat)
summary(lmm.MDCOST)
plot(lmm.MDCOST)
qqnorm(resid(lmm.MDCOST))



