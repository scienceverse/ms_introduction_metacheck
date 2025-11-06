# Title: Physical effort exertion for peer feedback reveals evolving social motivations from adolescence to young adulthood
# Authors: [blinded]
# Code written by [blinded]
# Last updated: 07/01/22 | ANALYSES

##############################################################################
##                                                                          ##
## ----------------------------- SET UP ----------------------------------- ##
##                                                                          ##
##############################################################################
#### LIBRARIES ####
 clc <- function() {
   rm(list = ls(envir = globalenv()),envir = globalenv()) ## clear Vars from global environment
   gc()  ## garbage collector
   cat("\014") ## clc
   .rs.restartR() ## clear session
 }    ## this function cleans the environment and restarts R
 clc()

ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("pscl", "lme4", "reghelper","MuMIn", "Hmisc", "ISLR", "sjPlot",
              "mgcv","itsadug", "ggplot2", "psych", "mgcViz","ggeffects")

ipak(packages) # May need up to date R to load all packages (R v. 4.2.0 used for these analyses)

##### LOAD DATA #####
load(file = "SocaMotiveLong_OSF.RData")
load(file = "SocaMoneyMotive_OSF.RData")

##### DICTIONARY #####
# SOCAMONEYMOTIVE dataset contains trials from both the money and social tasks and is used for the first set of analyses comparing effort exertion for money and social targets.
  # Subject = Subject ID
  # Age = raw age
  # AgeC = age mean centered
  # Gender = Male (.5) or Female (-.5)
  # Max = maximum grip force calibration
  # Difficulty = trial difficulty either Easy (40% of max) or Hard (80% of max)
  # TrialTotal = cumulative trial number (money trials 1-32; social trials 33-92)
  # Task = either money or social task
  # Peak_Wins = Peak grip force, winsorized for outliers (DV USED)
  # Speed_log_Wins = Speed, log transformed, and winsorized for outliers (DV USED)
  # OptOut = 1/0 of whether participants opted out of the trial and did not squeeze (1=optout, 0=not an optout) (DV USED)
  # Peak = Peak grip force (% of max)
  # Speed = raw speed in ms
  # Speed_log = Speed log transformed 
  # Max3Max1Diff = maximum grip force calibration before the tasks subtracted from maximum grip force calibration before the tasks difference score, used as covariate in sensitivity analyses to account for potential fatigue.

# SOCAMOTIVELONG dataset contains trials from the social task only and is used for the second and third set of analyses examining the effects of acceptance expectancy (Predictionz) and peer desirablity (Ratingz)
  # Subject = Subject ID
  # Age = raw age
  # AgeC = age mean centered
  # Gender = Male (.5) or Female (-.5)
  # Max = maximum grip force calibration
  # Difficulty = trial difficulty either Easy (40% of max) or Hard (80% of max)
  # TrialNumber = trial number (1-60)
  # Prediction = raw rating for acceptance expectancy (0-100)
  # Predictionz = z-scored (within-subject) rating for acceptance expectancy
  # Rating = raw rating for peer desirability (0-100)
  # Ratingz = z-scored (within-subject) rating for peer desirability
  # Peak_Wins = Peak grip force, winsorized for outliers (DV USED)
  # Speed_log_Wins = Speed, log transformed, and winsorized for outliers (DV USED)
  # OptOut = 1/0 of whether participants opted out of the trial and did not squeeze (1=optout, 0=not an optout) (DV USED)
  # Peak = Peak grip force (% of max)
  # Speed = raw speed in ms
  # Speed_log = Speed log transformed 
  # StimGender = Gender of peer presented Male (M) or Female (F)
  # Predictionz_Wins = z-scored (within-subject) rating for acceptance expectancy winsorized for outliers (+/- 3SD) USED FOR PLOTTING
  # Ratingz_Wins = z-scored (within-subject) rating for peer desirability winsorized for outliers (+/- 3SD) USED FOR PLOTTING

##############################################################################
##                                                                          ##
## ---------------------- PRIMARY GAMM ANALYSES --------------------------- ##
##                                                                          ##
##############################################################################
# Here, we use a thin plate smoothing spline/tensor regression to examine interactions with the function ti(), 
# As explained in the manuscript, ti() for categorical predictors computes a smoothed fit on each level, but
# it does not provide a direct statistical test of the interaction term. Instead, we use itsadug to inspect the 
# relative difference between fits by task and compute 95% CI around it. See Plotting Script.
# For continuous predictors, it breaks down main effects from the interaction term, allowing for direct 
# statistical tests and inferences of the interaction term.
set.seed(123)
####----------------Money vs. Social Task Performance by Age-----------------####
#### PEAK GRIP ####
peak.ms.int.gam<-gamm(Peak_Wins ~Task+ ti(AgeC,by=Task) + Difficulty + Max + TrialTotal, 
                      random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(peak.ms.int.gam$gam) 
anova(peak.ms.int.gam$gam) 
#See Plotting Script for test of interaction

### To isolate the age effect ###
peak.ms.age.gam<-gamm(Peak_Wins ~ Task + s(AgeC) + Difficulty + Max + TrialTotal, 
                      random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(peak.ms.age.gam$gam) 
anova(peak.ms.age.gam$gam) 

#### SPEED ####
speed.ms.int.gam<-gamm(Speed_log_Wins ~ Task + ti(AgeC,by=Task)+ Difficulty + Max + TrialTotal, 
                       random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(speed.ms.int.gam$gam)
anova(speed.ms.int.gam$gam) 
#See Plotting Script for test of interaction

### To isolate the age effect ###
speed.ms.age.gam<-gamm(Speed_log_Wins ~ Task + s(AgeC) + Difficulty + Max + TrialTotal, 
                      random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(speed.ms.age.gam$gam) 
anova(speed.ms.age.gam$gam) 

#### OPT OUT ####
optout.ms.int.gam<-gamm(OptOut ~  Task + ti(AgeC, by=Task)+ Difficulty + Max + TrialTotal, 
                        random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
summary(optout.ms.int.gam$gam)
anova(optout.ms.int.gam$gam)
#See Plotting Script for test of interaction

### To isolate the age effect ###
optout.ms.age.gam<-gamm(OptOut ~  Task + s(AgeC) + Difficulty + Max + TrialTotal, 
                        random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
summary(optout.ms.age.gam$gam)
anova(optout.ms.age.gam$gam)

####---------------- Acceptance Expectancy (Predictionz) by Age -----------------####
#### PEAK GRIP ####
peak.p.int.gam<-gamm(Peak_Wins ~ s(Predictionz) + s(AgeC) + ti(AgeC, Predictionz) + Difficulty + Max + TrialNumber, 
                    random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
summary(peak.p.int.gam$gam)
anova(peak.p.int.gam$gam)

#### SPEED ####
speed.p.int.gam<-gamm(Speed_log_Wins ~ s(Predictionz) + s(AgeC) + ti(AgeC, Predictionz) + Difficulty + Max + TrialNumber, 
                      random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
summary(speed.p.int.gam$gam)
anova(speed.p.int.gam$gam)

#### OPT OUT #### 
optout.p.int.gam<-gamm(OptOut ~ s(Predictionz) + s(AgeC) + ti(AgeC, Predictionz) + Difficulty + Max + TrialNumber, 
                       random=list(Subject=~1), family=binomial(link=logit),data = SocaMotiveLong_OSF,na.action=na.omit)
summary(optout.p.int.gam$gam)
anova(optout.p.int.gam$gam)

####---------------- Peer Desirability (Ratingz) by Age -----------------####
#### PEAK GRIP ####
peak.r.int.gam<-gamm(Peak_Wins ~ s(Ratingz) + s(AgeC) + ti(AgeC, Ratingz) + Difficulty + Max + TrialNumber, 
                     random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
summary(peak.r.int.gam$gam)
anova(peak.r.int.gam$gam)

#### SPEED ####
speed.r.int.gam<-gamm(Speed_log_Wins ~ s(Ratingz) + s(AgeC) + ti(AgeC, Ratingz) + Difficulty + Max + TrialNumber, 
                      random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
summary(speed.r.int.gam$gam)
anova(speed.r.int.gam$gam)

#### OPT OUT ####
optout.r.int.gam<-gamm(OptOut ~ s(Ratingz) + s(AgeC) + ti(AgeC, Ratingz) + Difficulty + Max + TrialNumber, 
                       random=list(Subject=~1), family=binomial,data = SocaMotiveLong_OSF,na.action=na.omit)
summary(optout.r.int.gam$gam)
anova(optout.r.int.gam$gam)

##############################################################################
##                                                                          ##
## ---------------------- SUPPLEMENTAL ANALYSES --------------------------- ##
##                                                                          ##
##############################################################################

#### -------- SECONDARY RATING ANALYSIS --------- ####
SocaMotiveLong_OSF$Rating<-SocaMotiveLong_OSF$Rating*100
SocaMotiveLong_OSF$Prediction<-SocaMotiveLong_OSF$Prediction*100
SocaMotiveLong_OSF$Gender<-as.factor(SocaMotiveLong_OSF$Gender)

#### ------------- Main effect of age, gender, and peer gender on ratings ___________ ####
#Because predictor Age solved as linear functions when using GAMM framework, models were estimated using traditional linear regression for ease of interpretability, especially for subsequent 3-way-interaction.
#### Expectancy ####
pred.as.lme<-lme(Prediction ~ AgeC + Gender + StimGender, random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
summary(pred.as.lme) 

#### Desirability ####
rat.as.lme<-lme(Rating ~ AgeC + Gender + StimGender, random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
summary(rat.as.lme)

#### ------------- Effect of age by gender by peer gender ___________ ####
#### Expectancy ###
pred.int.lme<-lme(Prediction ~ AgeC*Gender*StimGender, random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
summary(pred.int.lme) 

#### Desirability #### 
rat.int.lme<-lme(Rating ~ AgeC*Gender*StimGender, random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
summary(rat.int.lme)

###### ------------------------ FATIGUE CONTROL ANALYSES ---------------------- ##########
### Including Pre-Post Task Max Calibration (Fatigue) as a control variable for Money vs. Social Targets ###

#### PEAK GRIP ####
peak.ms.int.f.gam<-gamm(Peak_Wins ~ Task + ti(AgeC,by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff,
                           random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(peak.ms.int.f.gam$gam)
anova(peak.ms.int.f.gam$gam)

#See Plotting Script for test of interaction (findings hold)

#### SPEED ####
speed.ms.int.f.gam<-gamm(Speed_log_Wins ~ Task + ti(AgeC,by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff, 
                            random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(speed.ms.int.f.gam$gam)
anova(speed.ms.int.f.gam$gam)

#See Plotting Script for test of interaction (findings hold)

#### OPT OUT ####
optout.ms.int.f.gam<-gamm(OptOut ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff, 
                             random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
summary(optout.ms.int.f.gam$gam)
anova(optout.ms.int.f.gam$gam)

#See plotting script for test of interaction (findings hold)


####----------------Money vs. Social Task Performance including random slope of Task-----------------####
#### PEAK GRIP ####
peak.ms.int.gam.rs<-gamm(Peak_Wins ~ Task + ti(AgeC,by=Task) + Difficulty + Max + TrialTotal, 
                      random=list(Subject=~1+Task), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(peak.ms.int.gam.rs$gam) 
anova(peak.ms.int.gam.rs$gam) 

#See Plotting Script for test of interaction (findings hold)

#### SPEED ####
speed.ms.int.gam.rs<-gamm(Speed_log_Wins ~ Task + ti(AgeC,by=Task)+ Difficulty + Max + TrialTotal, 
                       random=list(Subject=~1+Task), data=SocaMoneyMotive_OSF, na.action = na.omit)
summary(speed.ms.int.gam.rs$gam)
anova(speed.ms.int.gam.rs$gam) 

#See Plotting Script for test of interaction (findings hold)

#### OPT OUT ####
optout.ms.int.gam.rs<-gamm(OptOut ~  Task + ti(AgeC, by=Task)+ Difficulty + Max + TrialTotal, 
                        random=list(Subject=~1+Task), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
summary(optout.ms.int.gam.rs$gam)
anova(optout.ms.int.gam.rs$gam)

#See Plotting Script for test of interaction (findings hold)
