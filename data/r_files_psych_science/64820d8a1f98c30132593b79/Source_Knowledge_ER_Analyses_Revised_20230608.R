#####ANALYSES: KNOWLEDGE ABOUT THE SOURCE OF EMOTION AND EMOTION REGULATION#############

###clean the workspace
rm(list = ls())
cat ("\014")

####LOAD IN PACKAGES####
library(tidyverse) #organizing data
library(sjPlot) #for exporting tables of output
library(lme4) #for running multilevel models
library(lmerTest) #for getting p-values (Satterthwaite adjusted) from multilevel models
library(interactions) #for conducting simple slopes analysis in interactions
library(glmmTMB) #required by sjPlot for some of diagnostic plots
library(ggplot2)#for plots
library(psych) #to calculate within-person stats 
library(data.table)#for converting wide to long format
library(plyr)#for converting wide to long format
library(modelbased)# for calculating means of factor in multilevel models
library(gmodels)#for crosstabs
library(dplyr)#for centering
library(jtools)# for simple slope analysis
library(effects)#for plots
library(nlme)#for plots
library(readr)#for reading CSV
library(MuMIn)#for effect size
library(ggrepel)#for inserting text to graphs
library(ggeffects)#for graphs

#*******************************************************************************
#*UPLOADING DATA
#********************************************************************************

setwd("~/Dropbox/Postdoc/Source attribution of emotion project/Raw data")#set working directory

Know_EMA <- read_csv("Source_Knowledge_EMA_Scales_Created.csv") #uploading EMA data
Know_Baseline <- read_csv("Source_Knowledge_Baseline_Scales_Created.csv") #uploading baseline data
Know_Baseline_EMA <- read_csv("Source_Knowledge_EMA_Baseline_Scales_Created.csv") #uploading EMA and baseline combined data


#*******************************************************************************
#*DEMOGRAPHICS
#*******************************************************************************

#NUMBER OF PARTICIPANTS (ensuring that it is the same in each file):
EMA_PARTICIPANT_ID <- unique(Know_EMA$Participant_ID) #making list of the EMA file participant ID
length(EMA_PARTICIPANT_ID) #how many participants in EMA file?
Baseline_PARTICIPANT_ID <- unique(Know_Baseline$Participant_ID) #same for baseline file
length(Baseline_PARTICIPANT_ID) #how many participants in baseline file?
EMA_Baseline_PARTICIPANT_ID <- unique(Know_Baseline_EMA$Participant_ID) #same for EMA and Baseline combined file 
length(EMA_Baseline_PARTICIPANT_ID)

#Age
psych::describe(Know_Baseline$Age)

#Gender
table(Know_Baseline$Gender)#frequency

#Sex at birth
table(Know_Baseline$Sex)#frequency

#Sex at birth variable for analyses: female vs. male
Know_Baseline_EMA$Sex_F <- ifelse(Know_Baseline_EMA$Sex=="Female",1,
                                  ifelse(Know_Baseline_EMA$Sex=="Male",0,NA))

#Ethnicity
table(Know_Baseline$Ethnicity)#frequency

#completed surveys:
psych::describe(Know_EMA$number_complete)#mean number of completed surveys

#Ethnicity variable for analyses: white vs.none-white
Know_Baseline_EMA$Ethnicity_W <- ifelse(Know_Baseline_EMA$Ethnicity=="White/Caucasian",1,0)


#**************************************************************************************
#*TESTING WHETHER SOURCE ATTRIBUTION OF EMOTION PREDICTS EMOTION REGULATION ATTEMPTS
#**************************************************************************************

#Taking-out time points with no negative emotion:

Know_EMA_NEG <- Know_EMA[Know_EMA$PastHour_neg>0,]#EMA only file
Know_Baseline_EMA_NEG <- Know_Baseline_EMA[Know_Baseline_EMA$PastHour_neg>0,]#EMA and baseline file

###Predicting emotion regulation attempts controlling for negative emotion in the past hour

modelTryReactBW <- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG)
summary(modelTryReactBW)
confint(modelTryReactBW)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBW)#effect size


###Predicting emotion regulation attempts controlling for negative emotion in the past hour and emotional intelligence

modelTryReactBWEI <- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryReactBWEI)
confint(modelTryReactBWEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBWEI)#effect size


###Predicting emotion regulation attempts controlling for negative emotion in the past hour and self concept clarity

modelTryReactBWC <- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_PastHour_neg+PC_know|Participant_ID), 
                          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryReactBWC)
confint(modelTryReactBWC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBWC)#effect size


###Emotion regulation attempts controlling for negative emotion in the past hour and DSM-5 symptoms

modelTryReactBWDSM <- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryReactBWDSM)
confint(modelTryReactBWDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBWDSM)#effect size


###Emotion regulation attempts controlling for negative emotion in the past hour and emotional inertia

modelTryReactBWInertia <- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG)
summary(modelTryReactBWInertia)
confint(modelTryReactBWInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBWInertia)#effect size


###Emotion regulation attempts controlling for negative emotion in the past hour and demographics (age,ethnicity and sex)

modelTryReactBWDemo<- lmer(neg_try ~ PC_know+PC_PastHour_neg+GC_PM_know+Age+Sex_F+Ethnicity_W+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryReactBWDemo)
confint(modelTryReactBWDemo)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryReactBWDemo)#effect size

###Emotion regulation attempts- moderated by age
modelTryModeratedAge<- lmer(neg_try ~ PC_know*Age+PC_PastHour_neg+GC_PM_know*Age+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryModeratedAge)
confint(modelTryModeratedAge)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryModeratedAge)#effect size

###Emotion regulation attempts- moderated by sex
modelTryModeratedSex<- lmer(neg_try ~ PC_know*Sex_F+PC_PastHour_neg+GC_PM_know*Sex_F+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryModeratedSex)
confint(modelTryModeratedSex)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryModeratedSex)#effect size

###Emotion regulation attempts- moderated by ethnicity
modelTryModeratedEthnicity<- lmer(neg_try ~ PC_know*Ethnicity_W+PC_PastHour_neg+GC_PM_know*Ethnicity_W+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelTryModeratedEthnicity)
confint(modelTryModeratedEthnicity)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTryModeratedEthnicity)#effect size


#*******************************************************************************
#*TESTING WHETHER SOURCE ATTRIBUTION OF EMOTION PREDICTS EMOTION REGULATION STRATEGY USE
#*******************************************************************************

###################Taking-out instances with no attempts to regulate:##########

Know_EMA_NEG_TRY <- Know_EMA_NEG[Know_EMA_NEG$neg_try>0,]#EMA file
Know_Baseline_EMA_NEG_TRY <- Know_Baseline_EMA_NEG[Know_Baseline_EMA_NEG$neg_try>0,]#EMA and baseline file

######SPECIFIC STRATEGIES#####################################################:

#Situation modification:
modelmodi <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelmodi)
confint(modelmodi)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodi)#effect size

#Situation modification controlling for emotional intelligence:
modelmodiEI <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelmodiEI)
confint(modelmodiEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodiEI)#effect size

#Situation modification controlling for self-concept clarity:
modelmodiC <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelmodiC)
confint(modelmodiC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodiC)#effect size


#Situation modification controlling for DSM-5 symptoms:
modelmodiDSM <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelmodiDSM)
confint(modelmodiDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodiDSM)#effect size


#Situation modification controlling for emotional inertia:
modelmodiInertia <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelmodiInertia)
confint(modelmodiInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodiInertia)#effect size

#Situation modification controlling for all other strategies:
modelmodiStrategies <- lmer(try_modification ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_reappraisal+PC_try_distraction+
                           PC_try_social+PC_try_rumination+PC_try_body+PC_try_eat+PC_try_substance+PC_try_suppression+
                           PC_try_self_harm+(1+PC_know|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelmodiStrategies)
confint(modelmodiStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelmodiStrategies)#effect size


###############################################################################################################
#Reappraisal:
modelreappraisal <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_EMA_NEG_TRY)
summary(modelreappraisal)
confint(modelreappraisal)#CONFIDENCE INTERAVLS
r.squaredGLMM(modelreappraisal)#effect size

#Reappraisal controlling for emotional intelligence:
modelreappraisalEI <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelreappraisalEI)
confint(modelreappraisalEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelreappraisalEI)#effect size


#Reappraisal controlling for self-concept clarity:
modelreappraisalC <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelreappraisalC)
confint(modelreappraisalC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelreappraisalC)#effect size


#Reappraisal controlling for DSM-5 symptoms:
modelreappraisalDSM <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelreappraisalDSM)
confint(modelreappraisalDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelreappraisalDSM)#effect size


#Reappraisal controlling for emotional inertia:
modelreappraisalInertia <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_EMA_NEG_TRY)
summary(modelreappraisalInertia)
confint(modelreappraisalInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelreappraisalInertia)#effect size

#Reappraisal controlling for all other strategies:
modelreappraisalStrategies <- lmer(try_reappraisal ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_distraction+
                                     PC_try_social+PC_try_rumination+PC_try_body+PC_try_eat+PC_try_substance+PC_try_suppression+
                                     PC_try_self_harm+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                                control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)), data = Know_EMA_NEG_TRY)
summary(modelreappraisalStrategies)
confint(modelreappraisalStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelreappraisalStrategies)#effect size

############################################################################################
#Social support
modelsocial <- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsocial)
confint(modelsocial)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocial)#effect size

#Social support controlling for emotional intelligence
modelsocialEI <- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsocialEI)
confint(modelsocialEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocialEI)#effect size

#Social support controlling for self-concept clarity
modelsocialC <- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsocialC)
confint(modelsocialC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocialC)#effect size

#Social support controlling for DSM-5 symptoms
modelsocialDSM <- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsocialDSM)
confint(modelsocialDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocialDSM)#effect size

#Social support controlling for emotional inertia
modelsocialInertia<- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsocialInertia)
confint(modelsocialInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocialInertia)#effect size

#Social support controlling for all other strategies
modelsocialStrategies<- lmer(try_social ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_distraction+
                               PC_try_reappraisal+PC_try_rumination+PC_try_body+PC_try_eat+PC_try_substance+PC_try_suppression+
                               PC_try_self_harm+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsocialStrategies)
confint(modelsocialStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsocialStrategies)#effect size

################################################################################################
#Body relaxation:
modelbody <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelbody)
confint(modelbody)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbody)#effect size

#Body relaxation controlling for emotional intelligence
modelbodyEI <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelbodyEI)
confint(modelbodyEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbodyEI)#effect size


#Body relaxation controlling for self-concept clarity
modelbodyC <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelbodyC)
confint(modelbodyC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbodyC)#effect size


#Body relaxation controlling for DSM-5 symptoms
modelbodyDSM <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelbodyDSM)
confint(modelbodyDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbodyDSM)#effect size

#Body relaxation controlling for emotional inertia
modelbodyInertia <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelbodyInertia)
confint(modelbodyInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbodyInertia)#effect size

#Body relaxation controlling for all other strategies
modelbodyStrategies <- lmer(try_body ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_distraction+
                           PC_try_reappraisal+PC_try_rumination+PC_try_social+PC_try_eat+PC_try_substance+PC_try_suppression+
                           PC_try_self_harm+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelbodyStrategies)
confint(modelbodyStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelbodyStrategies)#effect size

################################################################################################
#Rumination
modelrumination <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), data = Know_EMA_NEG_TRY)
summary(modelrumination)
confint(modelrumination)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelrumination)#effect size

#Rumination controlling for emotional intelligence
modelruminationEI <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), data = Know_Baseline_EMA_NEG_TRY)
summary(modelruminationEI)
confint(modelruminationEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelruminationEI)#effect size


#Rumination controlling for self-concept clarity
modelruminationC <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelruminationC)
confint(modelruminationC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelruminationC)#effect size


#Rumination controlling for DSM-5 symptoms
#model converged only after removing slope for negative emotion in the past hour 
modelruminationDSM <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know|Participant_ID), data = Know_Baseline_EMA_NEG_TRY)
summary(modelruminationDSM)
confint(modelruminationDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelruminationDSM)#effect size


#Rumination controlling for emotional inertia
modelruminationInertia <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), data = Know_EMA_NEG_TRY)
summary(modelruminationInertia)
confint(modelruminationInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelruminationInertia)#effect size


#Rumination controlling for all other strategies
modelruminationStrategies <- lmer(try_rumination ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_distraction+
                                 PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_eat+PC_try_substance+PC_try_suppression+
                                 PC_try_self_harm+(1+PC_know|Participant_ID), data = Know_EMA_NEG_TRY)
summary(modelruminationStrategies)
confint(modelruminationStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelruminationStrategies)#effect size

#######################################################################################################
#Suppression:
modelsupp <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsupp)
confint(modelsupp)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsupp)#effect size

#Suppression controlling for emotional intelligence
modelsuppEI <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsuppEI)
confint(modelsuppEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsuppEI)#effect size


#Suppression controlling for self-concept clarity
modelsuppC <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsuppC)
confint(modelsuppC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsuppC)#effect size


#Suppression controlling for DSM5 symptoms
modelsuppD <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modelsuppD)
confint(modelsuppD)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsuppD)#effect size


#Suppression controlling for emotional inertia
modelsuppInertia <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsuppInertia)
confint(modelsuppInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsuppInertia)#effect size

#Suppression controlling for all other strategies
modelsuppStrategies <- lmer(try_suppression ~ PC_know +PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_distraction+
                              PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_eat+PC_try_substance+PC_try_rumination+
                              PC_try_self_harm+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelsuppStrategies)
confint(modelsuppStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsuppStrategies)#effect size

##################################################################################################
#Distraction
modeldistraction <- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modeldistraction)
confint(modeldistraction)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistraction)#effect size


#Distraction controlling for emotional intelligence
modeldistractionEI <- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modeldistractionEI)
confint(modeldistractionEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistractionEI)#effect size

#Distraction controlling for self-concept clarity
modeldistractionC <- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modeldistractionC)
confint(modeldistractionC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistractionC)#effect size


#Distraction controlling for DSM-5 symptoms
modeldistractionDSM <- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_Baseline_EMA_NEG_TRY)
summary(modeldistractionDSM)
confint(modeldistractionDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistractionDSM)#effect size


#Distraction controlling for emotional inertia
modeldistractionInertia <- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modeldistractionInertia)
confint(modeldistractionInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistractionInertia)#effect size

#Distraction controlling for all other strategies
modeldistractionStrategies<- lmer(try_distraction ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_suppression+
                                  PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_eat+PC_try_substance+PC_try_rumination+
                                  PC_try_self_harm+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                                control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modeldistractionStrategies)
confint(modeldistractionStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeldistractionStrategies)#effect size

##################################################################################################
#Substance Use
#removed slope for negative emotion because model did not converge
modelsubstance <- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelsubstance)
confint(modelsubstance)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstance)#effect size

#Substance Use controlling for emotional intelligence
modelsubstanceEI <- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelsubstanceEI)
confint(modelsubstanceEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstanceEI)#effect size


#Substance Use controlling for self-concept clarity
modelsubstanceC <- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelsubstanceC)
confint(modelsubstanceC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstanceC)#effect size

#Substance Use controlling for DSM-5 symptoms
modelsubstanceDSM <- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelsubstanceDSM)
confint(modelsubstanceDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstanceDSM)#effect size


#Substance Use controlling for emotional inertia
modelsubstanceInertia <- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know|Participant_ID), 
                          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelsubstanceInertia)
confint(modelsubstanceInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstanceInertia)#effect size

#Substance Use controlling for all other strategies
modelsubstanceStrategies<- lmer(try_substance ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_suppression+
                                PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_eat+PC_try_distraction+PC_try_rumination+
                                PC_try_self_harm+(1+PC_know|Participant_ID), 
                              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelsubstanceStrategies)
confint(modelsubstanceStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelsubstanceStrategies)#effect size

################################################################################################
#Self injury:
modelharm <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelharm)
confint(modelharm)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharm)#effect size

#Self injury controlling for emotional intelligence
modelharmEI <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelharmEI)
confint(modelharmEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharmEI)#effect size


#Self injury controlling for self-concept clarity
modelharmC <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelharmC)
confint(modelharmC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharmC)#effect size


#Self injury controlling for DSM5 symptoms
modelharmD <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelharmD)
confint(modelharmD)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharmD)#effect size


#Self injury controlling for emotional inertia
modelharmInertia <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelharmInertia)
confint(modelharmInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharmInertia)#effect size

#Self injury controlling for all other strategies
modelharmStrategies <- lmer(try_self_harm ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_suppression+
                           PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_eat+PC_try_distraction+PC_try_rumination+
                           PC_try_substance+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelharmStrategies)
confint(modelharmStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelharmStrategies)#effect size

###################################################################################################
#Emotional eating:
modeleat <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modeleat)
confint(modeleat)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleat)#effect size

#Emotional eating controlling for emotional intelligence
modeleatEI <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                 control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modeleatEI)
confint(modeleatEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleatEI)#effect size


#Emotional eating controlling for self-concept clarity
modeleatC <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modeleatC)
confint(modeleatC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleatC)#effect size


#Emotional eating controlling for DSM5 symptoms
modeleatD <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                   control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modeleatD)
confint(modeleatD)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleatD)#effect size

#Emotional eating controlling for emotional inertia
modeleatInertia <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modeleatInertia)
confint(modeleatInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleatInertia)#effect size

#Emotional eating controlling for all other strategies
modeleatStrategies <- lmer(try_eat ~ PC_know+PC_PastHour_neg+GC_PM_know+PC_try_modification+PC_try_suppression+
                          PC_try_reappraisal+PC_try_body+PC_try_social+PC_try_self_harm+PC_try_distraction+PC_try_rumination+
                          PC_try_substance+(1+PC_PastHour_neg|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modeleatStrategies)
confint(modeleatStrategies)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modeleatStrategies)#effect size

####PLOT FOR STRATEGIES- WITHIN EFFECTS:

#Import data:
library(readxl)
strategy_graph_within <- read_excel("~/Dropbox/Postdoc/Source attribution of emotion project/Raw data/Stratergies_Graph.xlsx")

#FOREST PLOT:

Strategy_plot <- ggplot(data=strategy_graph_within, aes(y=Index, x=Coefficient, xmin=Lower, xmax=Upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(strategy_graph_within), labels=strategy_graph_within$Strategy)+
  geom_vline(xintercept=0, color='red', linetype='dashed', alpha=.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+theme(axis.line=element_line())+theme(text = element_text(size = 16))
Strategy_plot
ggsave("Strategy_Plot_within.png", width = 13,
       height =9)

####PLOT FOR ALL STRATEGIES- BETWEEN EFFECTS:

#Import data:
strategy_graph_between <- read_excel("~/Dropbox/Postdoc/Source attribution of emotion project/Raw data/Stratergies_Graph_Between_effects.xlsx")

#FOREST PLOT:

Strategy_plot_between <- ggplot(data=strategy_graph_between, aes(y=Index, x=Coefficient, xmin=Lower, xmax=Upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(name = "", breaks=1:nrow(strategy_graph_between), labels=strategy_graph_between$Strategy)+
  geom_vline(xintercept=0, color='red', linetype='dashed', alpha=.5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+theme(axis.line=element_line())+theme(text = element_text(size = 16))
Strategy_plot_between
ggsave("Strategy_plot_between.png")



###################COMPARING SOURCE-DEPENDENT VS. SOURCE-INDEPENDENT STRATEGIES########################

#Creating source dependent vs. source independent strategies variables:

#Source dependent strategies in regulation attempts dataset:
Know_Baseline_EMA_NEG_TRY$Source_dependent_S <- rowMeans(
Know_Baseline_EMA_NEG_TRY[,c("try_reappraisal","try_modification","try_social")],na.rm = TRUE)

#Source independent strategies in regulation attempts dataset:
Know_Baseline_EMA_NEG_TRY$Source_Independent_S <- rowMeans(
Know_Baseline_EMA_NEG_TRY[,c("try_body","try_suppression",
                "try_self_harm","try_substance","try_distraction",
                "try_eat")],na.rm = TRUE)

#Source dependent strategies in negative emotion dataset:
Know_Baseline_EMA_NEG$Source_dependent_S <- rowMeans(
  Know_Baseline_EMA_NEG[,c("try_reappraisal","try_modification","try_social")],na.rm = TRUE)

#Source independent strategies in negative emotion dataset:
Know_Baseline_EMA_NEG$Source_Independent_S <- rowMeans(
  Know_Baseline_EMA_NEG[,c("try_body","try_suppression",
                               "try_self_harm","try_substance","try_distraction",
                               "try_eat")],na.rm = TRUE)

#Source dependent strategies in EMA dataset:
Know_EMA_NEG_TRY$Source_dependent_S <- rowMeans(
  Know_EMA_NEG_TRY[,c("try_reappraisal","try_modification","try_social")],na.rm = TRUE)

#Source independent strategies in EMA dataset:
Know_EMA_NEG_TRY$Source_Independent_S <- rowMeans(
  Know_EMA_NEG_TRY[,c("try_body","try_suppression",
                               "try_self_harm","try_substance","try_distraction",
                               "try_eat")],na.rm = TRUE)


#CREATING DATA FRAME WITH RELEVANT VARIABLES #############

Know_strategy <- Know_Baseline_EMA_NEG_TRY[,c(
  "Participant_ID",
  "PC_PastHour_neg",
  "PC_PastHour_pos",
  "PC_know",
  "Source_dependent_S",
  "Source_Independent_S",
  "GC_PM_know",
  "GC_SREIS_M",
  "GC_Clarity_M",
  "GC_DSM5_M",
  "Inertia_Slope",
  "Age",
  "Sex_F",
  "Ethnicity_W")]

#converting to long format - creating "strategies" variable (adaptive/maladaptive)

Know_strategy_long <- melt.data.table(setDT(Know_strategy),
                                      measure.vars = list(c("Source_dependent_S" ,"Source_Independent_S")),
                                      value.name = c("Strategy"),
                                      variable.name = "Source_dependency")

#defining "Source_dependency" as factor

Know_strategy_long$Source_dependency <- as.factor(Know_strategy_long$Source_dependency)

#defining levels of "Source_dependency"

library(plyr)
Know_strategy_long$Source_dependency <- revalue(Know_strategy_long$Source_dependency,
                                       c("Source_dependent_S" = "Source-dependent", 
                                         "Source_Independent_S" = "Source-independent"))


#model for comparing use of dependent vs. independent strategies as a function of source knowledge:
#Removed the slope for the within-person interaction because the model did not converge

StrategyB <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+
                    (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                  data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyB) 
confint(StrategyB)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyB)#effect size

###Means for dependent and independent
estimate_means(
  StrategyB,
  at = "Source_dependency",
  fixed = NULL,
  modulate = NULL,
  transform = "response",
  length = 10,
)

#Testing whether the slope for dependent strategies is significant:

#dataframe with only dependent strategies
Know_strategy_long_Dep <- Know_strategy_long[Know_strategy_long$Source_dependency=="Source-dependent",]

#testing model with only dependent instances
StrategyDep <- lmer(Strategy ~ PC_know + GC_PM_know + PC_PastHour_neg+
                    (1+PC_know+PC_PastHour_neg|Participant_ID), 
                  data = Know_strategy_long_Dep,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyDep) 
confint(StrategyDep)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyDep)#effect size

#Testing whether the slope for independent strategies is significant:

#dataframe with only independent strategies  
Know_strategy_long_Ind <- Know_strategy_long[Know_strategy_long$Source_dependency=="Source-independent",]

#testing model with only independent instances
StrategyInd <- lmer(Strategy ~ PC_know + GC_PM_know + PC_PastHour_neg+
                      (1+PC_know+PC_PastHour_neg|Participant_ID), 
                    data = Know_strategy_long_Ind,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyInd) 
confint(StrategyInd)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyInd)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#controlling for emotional intelligence:

StrategyBEI <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+GC_SREIS_M+
                      (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                    data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBEI) 
confint(StrategyBEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBEI)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#controlling for self-concept clarity:

StrategyBC <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+GC_Clarity_M+
                      (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                    data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBC) 
confint(StrategyBC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBC)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#controlling for DSM5 symptoms:

StrategyBDSM <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+GC_DSM5_M+
                     (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                   data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBDSM) 
confint(StrategyBDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBDSM)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#controlling for emotional ineritia:

StrategyBInertia <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+Inertia_Slope+
                       (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                     data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBInertia)
confint(StrategyBInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBInertia)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#controlling for demographics:

StrategyBDemo <- lmer(Strategy ~ PC_know * Source_dependency + GC_PM_know*Source_dependency + PC_PastHour_neg+Age+Sex_F+Ethnicity_W+
                           (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                         data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBDemo)
confint(StrategyBDemo)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBDemo)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#Moderation by age:

StrategyBAge <- lmer(Strategy ~ PC_know * Source_dependency*Age + GC_PM_know*Source_dependency*Age+ PC_PastHour_neg+
                        (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                      data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBAge)
confint(StrategyBAge)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBAge)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#Moderation by sex:

StrategyBSex <- lmer(Strategy ~ PC_know * Source_dependency*Sex_F + GC_PM_know*Source_dependency*Sex_F+ PC_PastHour_neg+
                       (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                     data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBSex)
confint(StrategyBSex)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBSex)#effect size

#model for comparing use of dependent vs. independent strategies as a function of source knowledge
#Moderation by Ethnicity:

StrategyBEthnicity <- lmer(Strategy ~ PC_know * Source_dependency*Ethnicity_W + GC_PM_know*Source_dependency*Ethnicity_W+ PC_PastHour_neg+
                       (1+PC_know+Source_dependency+PC_PastHour_neg|Participant_ID), 
                     data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBEthnicity)
confint(StrategyBEthnicity)#CONFIDENCE INTERAVLS 
r.squaredGLMM(StrategyBEthnicity)#effect size

#######################PLOTS FOR SOURCE-DEPENDENT VS. SOURCE-INDEPENDENT STRATEGIES############

###Plot- within effect

ef1 <- effect(term="PC_know*Source_dependency", mod=StrategyB)#defining model and interaction term

efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1 #print effects data frame

efdata1$Source_dependency<-as.factor(efdata1$Source_dependency)#defining Source dependency as factor


#ploting the model
ggplot(efdata1, aes(x=PC_know, y=fit, color=Source_dependency,group=Source_dependency)) + 
  geom_point() + 
  geom_line(size=1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Source_dependency),alpha=0.3) +scale_color_manual(values=c('darkviolet','darkorange'))+
  scale_fill_manual(aesthetics = c("colour", "fill"),values=c('darkviolet','darkorange'))+ 
  labs(title = "",
       x= "Momentary knowledge about the source of emotion (person-mean centred)", y="Extent of using the strategies", color="Strategy type", fill="Strategy type") + theme_classic() + theme(text=element_text(size=18))
ggsave("Strategy_Dependent_within.png",width = 11,
       height = 7.22)

#Model for between participants effect:
StrategyBet <- lmer(Strategy ~ GC_PM_know*Source_dependency + PC_PastHour_neg+
                      (1+Source_dependency+PC_PastHour_neg|Participant_ID), 
                    data = Know_strategy_long,control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)))
summary(StrategyBet) 

###Plot- between effect

ef1 <- effect(term="GC_PM_know*Source_dependency", mod=StrategyBet)#defining model and interaction term

efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1 #print effects data frame

efdata1$Source_dependency<-as.factor(efdata1$Source_dependency)#defining Source dependency as factor

#ploting the model
ggplot(efdata1, aes(x=GC_PM_know, y=fit, color=Source_dependency,group=Source_dependency)) + 
  geom_point() + 
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Source_dependency),alpha=0.3) + scale_color_manual(values=c('darkviolet','darkorange'))+
  scale_fill_manual(aesthetics = c("colour", "fill"),values=c('darkviolet','darkorange'))+ 
  labs(title = "",
       x= "knowledge about the source of emotion (grand-mean centred)", y="Extent of using the strategies", color="Strategy type", fill="Strategy type") + theme_classic() + theme(text=element_text(size=18))
ggsave("Strategy_dependent_between.png",width = 11,
       height = 7.22)


#***********************************************************************************************
#*TESTING WHETHER SOURCE ATTRIBUTION OF EMOTION PREDICTS PERCEIVED EMOTION REGULATION SUCCESS
#***********************************************************************************************

###Testing whether knowing predicts more perceived regulation success controlling for negative emotion in past hour
modelableBW <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelableBW)
confint(modelableBW)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBW)#effect size


###Testing whether knowing predicts more perceived regulation success controlling for negative emotion in past hour and emotional intelligence

modelableBWEI <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_SREIS_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelableBWEI)
confint(modelableBWEI)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBWEI)#effect size


###Testing whether knowing predicts more perceived regulation success controlling for negative emotion in past hour and self-concept clarity

modelableBWC <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_Clarity_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelableBWC)
confint(modelableBWC)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBWC)#effect size


###Testing whether knowing predicts more perceived regulation success controlling for negative emotion in past hour and DSM-5 symptoms

modelableBWDSM <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+GC_DSM5_M+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelableBWDSM)
confint(modelableBWDSM)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBWDSM)#effect size


###Testing whether knowing predicts more perceived regulation success controlling for negative emotion in past hour and emotional inertia

modelableBWInertia <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+Inertia_Slope+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_EMA_NEG_TRY)
summary(modelableBWInertia)
confint(modelableBWInertia)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBWInertia)#effect size

###Testing whether knowing predicts perceived regulation success controlling for negative emotion in past hour and demographics

modelableBWDemo <- lmer(neg_able_regulate ~ PC_know+PC_PastHour_neg+GC_PM_know+Age+Sex_F+Ethnicity_W+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(modelableBWDemo)
confint(modelableBWDemo)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelableBWDemo)#effect size

###Testing whether knowing predicts perceived regulation success- moderation by Age:

ableModeratedAge <- lmer(neg_able_regulate ~ PC_know*Age+PC_PastHour_neg+GC_PM_know*Age+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(ableModeratedAge)
confint(ableModeratedAge)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ableModeratedAge)#effect size

###Testing whether knowing predicts perceived regulation success- moderation by Sex:

ableModeratedSex <- lmer(neg_able_regulate ~ PC_know*Sex_F+PC_PastHour_neg+GC_PM_know*Sex_F+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(ableModeratedSex)
confint(ableModeratedSex)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ableModeratedSex)#effect size

###Testing whether knowing predicts more perceived regulation success- moderation by Ethnicity:

ableModeratedEthnicity <- lmer(neg_able_regulate ~ PC_know*Ethnicity_W+PC_PastHour_neg+GC_PM_know*Ethnicity_W+(1+PC_know+PC_PastHour_neg|Participant_ID), 
                         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG_TRY)
summary(ableModeratedEthnicity)
confint(ableModeratedEthnicity)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ableModeratedEthnicity)#effect size


#********************************************************************************************
#*TESTING ASSOCIATIONS BETWEEN SOURCE ATTRIBUTION OF EMOTION, WELL-BEING AND PSYCHOPATHOLOGY
#********************************************************************************************

#well being:

modelSWL <- lmer(know ~ GC_SWL_M+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
                 control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelSWL)
confint(modelSWL)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelSWL)#effect size

#Psychopathology:

modelpsych <- lmer(know ~ GC_DSM5_M+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
                 control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(modelpsych)
confint(modelpsych)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelpsych)#effect size

#*************************************************************************************************************************************
#*ANALYSES FOR SUPPLEMENTAL MATERIALS:
#*************************************************************************************************************************************

#*************************************************************************************************************************************
#*ASSOCIATIONS BETWEEN TRAIT DESCRETE NEGATIVE AND POSITIVE EMOTIONS (AT BASELINE) AND EMA MEASURES OF NEGATIVE AND POSITIVE EMOTIONS
#*************************************************************************************************************************************

#Does trait discrete negative emotions predict negative emotions in daily life- validation of daily life measure
modelNegEmo <- lmer(PastHour_neg ~ GC_TraitEmotion_Neg_M+(1|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA)
summary(modelNegEmo)
confint(modelNegEmo)#CONFIDENCE INTERAVLS 
r2beta(modelNegEmo)#effect size

#Does trait discrete positive emotion predict positive emotions in daily life- validation of daily life measure
modelPosEmo <- lmer(PastHour_pos ~ GC_TraitEmotion_Pos_M+(1|Participant_ID), 
                    control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA)
summary(modelPosEmo)
confint(modelPosEmo)#CONFIDENCE INTERAVLS 
r2beta(modelPosEmo)#effect size


#*******************************************
#*ZERO ORDER CORRELATIONS BETWEEN VARIABLES
#*******************************************

#new dataframe that just selects in the relevant variables
#Plus the ID variable (Participant_ID)
EMA_describe <- Know_Baseline_EMA_NEG %>% dplyr::select(Participant_ID,know,PastHour_neg,neg_try,Source_dependent_S, Source_Independent_S, neg_able_regulate)

#sending the output to a new object
EMA_statsBy <- statsBy(EMA_describe, "Participant_ID")

#MEANS and SD OF VARIABLES (within and between)
EMA_statsBy_mean <- as.data.frame(EMA_statsBy[["mean"]]) #means
EMA_statsBy_withinsd <- as.data.frame(EMA_statsBy[["sd"]]) #within-person SDs
#between-person SDs 
EMA_statsBy_betweensd <- as.data.frame(sapply(EMA_statsBy_mean, sd, na.rm = TRUE))
#sending this to a new dataframe
EMA_statsBy_output <- data.frame(Variable=colnames(EMA_statsBy_mean), #this is grabbing the column names as a new column called "variable" to print
                                          M=colMeans(EMA_statsBy_mean, na.rm = TRUE), #this is going to put the mean of the person means as a column called M
                                          Within_person_SD=colMeans(EMA_statsBy_withinsd, na.rm=TRUE), #same for the within SDs
                                          Between_person_SD=EMA_statsBy_betweensd[,1], #this is grabbing the first column of the bewteen SD datafile (which has one column)
                                          ICC=EMA_statsBy[["ICC1"]]) #this is taking the ICCs from the statsBy output

#delete the row for ID, removing the first row
EMA_statsBy_output <- EMA_statsBy_output[-1,]

####CORRELATIONS####

#BETWEEN-GROUP correlations
EMA_statsBy[["rbg"]] # correlations between groups

#p values
EMA_statsBy[["pbg"]]#p-values for between groups correlations

#WITHIN-GROUP correlations
EMA_statsBy[["rwg"]] # correlations within groups) 

#p-values
EMA_statsBy[["pwg"]]#p-values for within groups correlations


#*****************************************************************
#*ZERO ORDER CORRELATIONS BETWEEN EMOTION REGULATION STRATEGIES
#*****************************************************************

#new dataframe that just selects in the relevant variables
#Plus the ID variable (Participant_ID)
EMA_describe_strategy <- Know_Baseline_EMA_NEG %>% dplyr::select(Participant_ID,try_modification,try_reappraisal,try_social,
                                                                 try_body,try_rumination,try_suppression,try_distraction,try_substance,
                                                                 try_self_harm,try_eat)

#sending the output to a new object
EMA_statsBy <- statsBy(EMA_describe_strategy, "Participant_ID")

#MEANS and SD OF VARIABLES (within and between)
EMA_statsBy_mean <- as.data.frame(EMA_statsBy[["mean"]]) #means
EMA_statsBy_withinsd <- as.data.frame(EMA_statsBy[["sd"]]) #within-person SDs
#between-person SDs 
EMA_statsBy_betweensd <- as.data.frame(sapply(EMA_statsBy_mean, sd, na.rm = TRUE))
#sending this to a new dataframe
EMA_statsBy_output <- data.frame(Variable=colnames(EMA_statsBy_mean), #this is grabbing the column names as a new column called "variable" to print
                                 M=colMeans(EMA_statsBy_mean, na.rm = TRUE), #this is going to put the mean of the person means as a column called M
                                 Within_person_SD=colMeans(EMA_statsBy_withinsd, na.rm=TRUE), #same for the within SDs
                                 Between_person_SD=EMA_statsBy_betweensd[,1], #this is grabbing the first column of the bewteen SD datafile (which has one column)
                                 ICC=EMA_statsBy[["ICC1"]]) #this is taking the ICCs from the statsBy output

#delete the row for ID, removing the first row
EMA_statsBy_output <- EMA_statsBy_output[-1,]

####CORRELATIONS####

#BETWEEN-GROUP correlations
EMA_statsBy[["rbg"]] # correlations between groups

#p values
EMA_statsBy[["pbg"]]#p-values for between groups correlations

#WITHIN-GROUP correlations
EMA_statsBy[["rwg"]] # correlations within groups) 

#p-values
EMA_statsBy[["pwg"]]#p-values for within groups correlations


#*******************************************************************
#*NUMBER OF EMOTION REGULATION STRATEGIES USED AT EACH TIME POINT
#*******************************************************************

#Transforming strategy use to yes or no categories (0=no, >0 = yes)

#Situation modification
Know_Baseline_EMA_NEG_TRY$try_modification_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_modification==0,0,
                                                        ifelse(Know_Baseline_EMA_NEG_TRY$try_modification=="NA",NA,1))

#Reappraisal
Know_Baseline_EMA_NEG_TRY$try_reappraisal_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_reappraisal==0,0,
                                                        ifelse(Know_Baseline_EMA_NEG_TRY$try_reappraisal=="NA",NA,1))

#Social support
Know_Baseline_EMA_NEG_TRY$try_social_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_social==0,0,
                                                       ifelse(Know_Baseline_EMA_NEG_TRY$try_social=="NA",NA,1))

#Distraction
Know_Baseline_EMA_NEG_TRY$try_distraction_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_distraction==0,0,
                                                  ifelse(Know_Baseline_EMA_NEG_TRY$try_distraction=="NA",NA,1))

#Rumination
Know_Baseline_EMA_NEG_TRY$try_rumination_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_rumination==0,0,
                                                       ifelse(Know_Baseline_EMA_NEG_TRY$try_rumination=="NA",NA,1))

#Suppression
Know_Baseline_EMA_NEG_TRY$try_suppression_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_suppression==0,0,
                                                      ifelse(Know_Baseline_EMA_NEG_TRY$try_suppression=="NA",NA,1))

#Substance use
Know_Baseline_EMA_NEG_TRY$try_substance_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_substance==0,0,
                                                       ifelse(Know_Baseline_EMA_NEG_TRY$try_substance=="NA",NA,1))

#Self-injury
Know_Baseline_EMA_NEG_TRY$try_self_harm_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_self_harm==0,0,
                                                     ifelse(Know_Baseline_EMA_NEG_TRY$try_self_harm=="NA",NA,1))

#Body relaxation
Know_Baseline_EMA_NEG_TRY$try_body_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_body==0,0,
                                                     ifelse(Know_Baseline_EMA_NEG_TRY$try_body=="NA",NA,1))

#Emotional eating
Know_Baseline_EMA_NEG_TRY$try_eat_YN <- ifelse(Know_Baseline_EMA_NEG_TRY$try_eat==0,0,
                                                ifelse(Know_Baseline_EMA_NEG_TRY$try_eat=="NA",NA,1))

#VARIABLE FOR NUMBER OF EMOTION REGULATION STRATEGIES USED AT EACH TIME POINT

Know_Baseline_EMA_NEG_TRY$Strategy_Num <- rowSums(
  Know_Baseline_EMA_NEG_TRY[,c("try_eat_YN","try_body_YN",
                   "try_self_harm_YN","try_substance_YN",
                   "try_suppression_YN","try_rumination_YN",
                   "try_distraction_YN","try_reappraisal_YN",
                   "try_modification_YN",
                   "try_social_YN")],na.rm = TRUE)

#descriptives of number of strategies:

describe(Know_Baseline_EMA_NEG_TRY$Strategy_Num)

#Frequency of polyregulation:

table(Know_Baseline_EMA_NEG_TRY$Strategy_Num)

#Histogram of polyregulation:

hist(Know_Baseline_EMA_NEG_TRY$Strategy_Num,xlab='Number of emotion regulation strategies used')
ggsave("polyregulation.png")


#*************************************************************************************
#*CLUSTER ANALYSIS OF STRATEGIES AND ASSOCIATION WITH SOURCE ATTRIBUTION OF EMOTION
#*************************************************************************************

#taking out "NA"
Know_EMA_NEG_TRY <- subset(Know_EMA_NEG_TRY, nchar(as.character(Participant_ID)) > 0)

#####PERSON-CETERED STRATEGY USE#####################

#selecting only relevant variables
Know_try_subset <- subset(Know_EMA_NEG_TRY[,c(1,11,61:63,65:74)])

#taking out "NA"s
Know_try_subset <- na.omit(Know_try_subset)#omit NA

#Cluster analysis of strategies:
d <- dist(Know_try_subset[,c(6:15)], method="euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)#plot dendrogram

#Selection of 3-cluster solution
rect.hclust(fit, k=3) 

#creating a "cluster" variable:
Know_try_subset$Cluster <- cutree(fit, k=3)

#defining variable as factor:
Know_try_subset$Cluster <- factor(Know_try_subset$Cluster)

#Calculating means of emotion regulation strategy use in each cluster
Cluster1 <- sapply(Know_try_subset[Know_try_subset$Cluster == 1,6:15], mean)
Cluster2 <- sapply(Know_try_subset[Know_try_subset$Cluster == 2,6:15], mean)
Cluster3 <- sapply(Know_try_subset[Know_try_subset$Cluster == 3,6:15], mean)
print(Cluster1)
print(Cluster2)
print(Cluster3)

#Calculating SDs of emotion regulation strategy use in each cluster
Cluster1SD <- sapply(Know_try_subset[Know_try_subset$Cluster == 1,6:15], sd)
Cluster2SD <- sapply(Know_try_subset[Know_try_subset$Cluster == 2,6:15], sd)
Cluster3SD <- sapply(Know_try_subset[Know_try_subset$Cluster == 3,6:15], sd)
print(Cluster1SD)
print(Cluster2SD)
print(Cluster3SD)

#Testing whether "Cluster" predicts knowledge about the source of emotion, controlling for negative emotion in past hour
modelCluster <- lmer(know ~ Cluster+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_try_subset)
summary(modelCluster)
confint(modelCluster)#CONFIDENCE INTERAVLS
r.squaredGLMM(modelCluster)#effect size

#Changing reference point to "cluster 2"
Know_try_subset$Cluster <- relevel(Know_try_subset$Cluster,"2")#Turning cluster 2 to be reference point

#Changing reference point to "cluster 1"
Know_try_subset$Cluster <- relevel(Know_try_subset$Cluster,"1")#Turning cluster 1 back to be reference point

###Mean source attribution as a function of cluster:
library(modelbased)
estimate_means(
  modelCluster,
  at = "Cluster",
  fixed = NULL,
  modulate = NULL,
  transform = "response",
  length = 10,
)

# Loading data for graph:
library(readxl)
Clusters <- read_excel("Clusters.xlsx")

#Plot for means of emotion regulation strategies by cluster
Clusters$Cluster <- as.factor(Clusters$Cluster)
ggplot(Clusters, aes(y=Mean, x=Strategy, group=Cluster, color=Cluster)) + geom_line()+
  geom_point()+geom_hline(yintercept=0, color='red', linetype='dashed', alpha=.5)+theme(panel.background = element_blank())+
  theme(panel.grid.major=element_blank())+theme(axis.text.x = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 18))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.text = element_text(size = 18))+
  theme(legend.title = element_text(size = 18))
ggsave("strategy_clusters.png",width = 14,
       height = 6)


#*************************************************************************************
#*TIME TRENDS
#*************************************************************************************

#Centering survey number (centering around the first survey):

Know_EMA$SurveyNumberCF <- Know_EMA$SurveyNumber -1
Know_EMA_NEG$SurveyNumberCF <- Know_EMA_NEG$SurveyNumber -1
Know_EMA_NEG_TRY$SurveyNumberCF <- Know_EMA_NEG_TRY$SurveyNumber -1

#checking variable:
table(Know_EMA_NEG$SurveyNumberCF)

#Change in knowledge over time:

modelTimeKnow <- lmer(know ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG)
summary(modelTimeKnow)
confint(modelTimeKnow)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeKnow)#effect size

#Change in negative emotion over time:
#no random slope because of singular fit
modelTimeNeg <- lmer(PastHour_neg ~ SurveyNumberCF+(1|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA)
summary(modelTimeNeg)
confint(modelTimeNeg)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeNeg)#effect size


#Change in regulation attempts over time:

modelTimeTry <- lmer(neg_try ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG)
summary(modelTimeTry)
confint(modelTimeTry)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeTry)#effect size


#Change in situation modification over time:

modelTimeModi <- lmer(try_modification ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeModi)
confint(modelTimeModi)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeModi)#effect size


#Change in reappraisal over time:

modelTimeReappraisal <- lmer(try_reappraisal ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeReappraisal)
confint(modelTimeReappraisal)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeReappraisal)#effect size


#Change in social support over time:

modelTimeSocial <- lmer(try_social ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeSocial)
confint(modelTimeSocial)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeSocial)#effect size


#Change in distraction over time:

modelTimeDistract <- lmer(try_distraction ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeDistract)
confint(modelTimeDistract)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeDistract)#effect size

#Change in rumination over time:

modelTimeRumination <- lmer(try_rumination ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeRumination)
confint(modelTimeRumination)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeRumination)#effect size

#Change in suppression over time:

modelTimeSuppression <- lmer(try_suppression ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeSuppression)
confint(modelTimeSuppression)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeSuppression)#effect size

#Change in substance use over time:

modelTimeSubstance <- lmer(try_substance ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeSubstance)
confint(modelTimeSubstance)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeSubstance)#effect size


#Change in self-injury use over time:
#no slope because of singular fit

modelTimeSelfInjury <- lmer(try_self_harm ~ SurveyNumberCF+(1|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeSelfInjury)
confint(modelTimeSelfInjury)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeSelfInjury)#effect size


#Change in body relaxation over time:

modelTimeBody <- lmer(try_body ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeBody)
confint(modelTimeBody)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeBody)#effect size


#Change in emotional eating over time:
#no slope because of singular fit

modelTimeEat <- lmer(try_eat ~ SurveyNumberCF+(1|Participant_ID), 
                      control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeEat)
confint(modelTimeEat)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeEat)#effect size


#Change in perceived emotion regulation ability over time:

modelTimeAbility <- lmer(neg_able_regulate ~ SurveyNumberCF+(1+SurveyNumberCF|Participant_ID), 
                             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)), data = Know_EMA_NEG_TRY)
summary(modelTimeAbility)
confint(modelTimeAbility)#CONFIDENCE INTERAVLS 
r.squaredGLMM(modelTimeAbility)#effect size


#*************************************************************************************
#*ASSOCIATIONS WITH AGE, RACE AND GENDER
#*************************************************************************************

###Does age predict knowledge about the source of emotion?
KnowAge<- lmer(know ~ Age+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
                            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(KnowAge)
confint(KnowAge)#CONFIDENCE INTERAVLS 
r.squaredGLMM(KnowAge)#effect size

###Does sex predict knowledge about the source of emotion?
KnowSex<- lmer(know ~ Sex_F+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(KnowSex)
confint(KnowSex)#CONFIDENCE INTERAVLS 
r.squaredGLMM(KnowSex)#effect size


###Does ethnicity predict knowledge about the source of emotion?
KnowEthnicity<- lmer(know ~ Ethnicity_W+PC_PastHour_neg+(1+PC_PastHour_neg|Participant_ID), 
               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = Know_Baseline_EMA_NEG)
summary(KnowEthnicity)
confint(KnowEthnicity)#CONFIDENCE INTERAVLS 
r.squaredGLMM(KnowEthnicity)#effect size

#*************************************************************************************
#*DOES CONCRETENESS OF WRITTEN DESCRIPTIONS PREDICT EMOTION REGULATION VARIABLES
#*************************************************************************************

#uploading the concreteness ratings dataframe:
d <- read.csv('~/Dropbox/Postdoc/concreteness_ratings_knowledge.csv')

#Creating the source dependency variables In the EMA DATA:
#Source dependent strategies:
Know_EMA$Source_dependent_S <- rowMeans(
  Know_EMA[,c("try_reappraisal","try_modification","try_social")],na.rm = TRUE)

#Source independent strategies:
Know_EMA$Source_Independent_S <- rowMeans(
  Know_EMA[,c("try_body","try_suppression",
              "try_self_harm","try_substance","try_distraction",
              "try_eat")],na.rm = TRUE)

#new dataframe that just selects in the relevant variables from the EMA:

EMA_regulation <- Know_EMA %>% dplyr::select(Participant_ID,Response_Id,neg_try,Source_dependent_S, Source_Independent_S, neg_able_regulate,PC_PastHour_neg)

#combining the two data sets based on response ID:
d_reguation <- left_join(d, EMA_regulation, "Response_Id")

#selecting the first row for each response ID (to have just one row per response)
library(dplyr)
d_regulation_wide <- d_reguation %>%
  group_by(Response_Id) %>%
  filter(row_number()==1)

# Person centering the concreteness score:
d_regulation_wide <- d_regulation_wide %>% group_by(Participant_ID) %>% dplyr::mutate(PC_concreteness = scale(concreteness, scale = FALSE))

d_regulation_wide <- ungroup(d_regulation_wide)#Ungrouping data

# Grand-mean centering of concreteness score:
d_regulation_wide <- d_regulation_wide %>% group_by(Participant_ID) %>% dplyr::mutate(PM_concreteness= mean(concreteness,na.rm=TRUE))
d_regulation_wide <- ungroup(d_regulation_wide)#Ungrouping data

d_regulation_wide <- d_regulation_wide %>% mutate (GC_PM_concreteness = scale(PM_concreteness, scale = FALSE))


#Taking out instances with no negative emotion:
d_regulation_wide_NEG <- d_regulation_wide[d_regulation_wide$PastHour_neg>0,]


#Predicting regulation attempts:

ConcreteAttempts<- lmer(neg_try ~ PC_concreteness+PC_PastHour_neg+GC_PM_concreteness+(1+PC_PastHour_neg+PC_concreteness|Participant_ID), 
                        control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = d_regulation_wide_NEG)
summary(ConcreteAttempts)
confint(ConcreteAttempts)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ConcreteAttempts)#effect size


#Taking out instances with no regulation attempts:
d_regulation_wide_NEG_TRY <- d_regulation_wide_NEG[d_regulation_wide_NEG$neg_try>0,]


#Predicting regulation strategies_source dependent:

ConcreteSourceDependent<- lmer(Source_dependent_S ~ PC_concreteness+PC_PastHour_neg+GC_PM_concreteness+(1+PC_PastHour_neg+PC_concreteness|Participant_ID), 
                               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = d_regulation_wide_NEG_TRY)
summary(ConcreteSourceDependent)
confint(ConcreteSourceDependent)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ConcreteSourceDependent)#effect size


#Predicting regulation strategies_source independent:
#no slope for concreteness because did not converge
ConcreteSourceIndependent<- lmer(Source_Independent_S ~ PC_concreteness+PC_PastHour_neg+GC_PM_concreteness+(1+PastHour_neg|Participant_ID), 
                                 control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = d_regulation_wide_NEG_TRY)
summary(ConcreteSourceIndependent)
confint(ConcreteSourceIndependent)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ConcreteSourceIndependent)#effect size


#Predicting regulation ability:
#no slope for concreteness because did not converge
ConcreteAbility<- lmer(neg_able_regulate ~ PC_concreteness+PC_PastHour_neg+GC_PM_concreteness+(1+PC_PastHour_neg|Participant_ID), 
                       control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 100000)),data = d_regulation_wide_NEG_TRY)
summary(ConcreteAbility)
confint(ConcreteAbility)#CONFIDENCE INTERAVLS 
r.squaredGLMM(ConcreteAbility)#effect size


