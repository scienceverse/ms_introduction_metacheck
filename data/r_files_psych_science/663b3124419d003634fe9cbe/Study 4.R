
#####################################
#########packages
###################################

library (foreign)
library (psych)
library (openxlsx)
library (ppcor)
library (reshape2)
library (data.table)
library (nlme)
library(lme4)
library (DataCombine)
library(plyr)
library(multilevel)
library(dplyr)
library (lavaan)
library (matrixStats)
library(magrittr)
library (sjPlot)
library (stargazer)
library (ggplot2)
library (interactions)
library(lme4)
library(lmerTest)
library (interactions)
library (multilevelTools)

#for adjusted p values
library(modelsummary)
library(lmerTest) 
library(parameters)
library(performance)
library (huxtable)
library (broom.mixed)
library (multcomp)


#for loading in momentary ED function - https://github.com/seanchrismurphy/emodiff
library (devtools)
library(emodiff) #to calculate the emotion differentiation indices


##################################################################################
##################### DESCRIPTIVES ##############################################
#################################################################################

###
## setwd 
### 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Preregistered")

######
# loading in preprocessed data 

load(file = "Pre-processed momentary ESM.Rdata")

ESM <- ESM[with(ESM, order(ID, SurveyNumber)),]

descriptives <- ESM %>% select(ID,negemo,neg_m_ED,ES,RUM)
#automatically adds beepday here, not needed
descriptives <- subset(descriptives, select = -1)

descriptives <- descriptives %>% 
  rename("Negative emotion" = negemo,
         "Emotion differentiation" = neg_m_ED, 
         "Social sharing" = ES, 
         "Rumination" = RUM)

relevantvars <- descriptives %>% 
  statsBy(group = 'ID', na.rm = TRUE) 

#Calculate mean, SD, variance, and ICC 
relevantvars_withinM <- as.data.frame(relevantvars[["mean"]]) 
relevantvars_withinSD <- as.data.frame(relevantvars [["sd"]]) 
relevantvars_betweenSD <- as.data.frame(sapply(relevantvars_withinM, sd, na.rm = TRUE))


#combine the above into one dataframe
relevantvars_output <- data.frame(Variable=colnames(relevantvars_withinM),
                                  M=colMeans(relevantvars_withinM, na.rm = TRUE), 
                                  SDw=colMeans(relevantvars_withinSD, na.rm=TRUE), 
                                  SDb=relevantvars_betweenSD[,1],
                                  ICC=relevantvars[["ICC1"]]) 

#delete person ID
relevantvars_output <- relevantvars_output[-1,]

# View(relevantvars_output)
tab_df(relevantvars_output, title = "Descriptive Statistics", file = "descriptives_study4.html")

#####################################################################################
############################### analyses ############################################
#####################################################################################

###
## setwd 
### 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Preregistered")

######
# loading in preprocessed data (created using the corresponding preprocessing code)
######

load(file = "Pre-processed momentary ESM.Rdata")


######################################################################################


#######
## 1. MAIN ANALYSES (note that the reversed analyses - 2 & 3RE- are moved to supplemental materials)
#######

analysis1_NE <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|ID),
                     data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE)


# better presentation
tab_model(analysis1_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE 1.doc")


############
#### 2. REVERSED
###########


### note: random slope of mean negative emotion is removed because of high correlations in slopes & less theoretically relevant
#random slope of previous Social sharing is removed too because of low variance, and less theoretically relevant 
analysis2_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale(PC_negemo) +
                       (scale(PC_neg_m_ED_1)|ID),
                     data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis2_NE)

#better representation
tab_model(analysis2_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation", "Social sharing at time T-1", "Negative emotion"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses2_NE 1.doc")


######
##  3. RUMINATION
#####


#note: too little variance in some random effects, runs without including random slope for rumin & interaction
analysis3_NE <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+ 
                       (scale(PC_neg_m_ED_1)+ scale(PC_ES) +scale(PC_negemo)|ID),
                     data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE)

#better representation
tab_model(analysis3_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE 1.doc")


### rumination for reversed relationship? 
#random slope for mean negative emotion & interaction rumination had to be omitted 

analysis3rev_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_rum) +scale(PC_negemo) + scale (PC_rum):scale(PC_neg_m_ED_1)+
                           (scale(PC_neg_m_ED_1)+scale (PC_rum)+scale(PC_ES_1)|ID),
                         data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3rev_NE)


#better representation
tab_model(analysis3rev_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "Rumination","Negative emotion","Interaction  emotion differentiation at time T-1 and rumination"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses reversed & rumination.doc")

#############################################################
############## MANUSCRIPT MODELS WITH ADJUSTED P-VALUES

###################################################
###################################################
###################################################


l_mod <- list("mod1" = analysis1_NE, "mod3"=analysis3_NE)
msummary(l_mod)

adjMC <- function( model ) {
  model_glht <- glht(model)
  model_mc_adj <- summary(model_glht, test = adjusted('BH')) # Benjamini-hochberg
  return(model_mc_adj)
}

mod1_adj <- adjMC(l_mod$mod1)
mod3_adj <- adjMC(l_mod$mod3)

##################################
########## SUPPLEMENTARY MATERIALS
##################################


######### TIME INCLUDED 
#########


#main model including time
analysis1_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +scale (SurveyNumber)+
                                 (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|ID),
                               data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_with_time)


# better presentation
tab_model(analysis1_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1", "Negative emotion", "Beepnumber"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_with_time.doc")

#rumination model including time
analysis3_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+ +scale (SurveyNumber)+
                                 (scale(PC_neg_m_ED_1)+ scale(PC_ES) +scale(PC_negemo)|ID),
                               data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_with_time)

#better representation
tab_model(analysis3_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Beepnumber","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_with_time.doc")

############ EXCLUDING LAGGED EFFECTS
############

#1: main model excluding lagged 
analysis1_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_negemo) +
                          (scale(PC_ES)+scale(PC_negemo)|ID),
                        data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_wl)


# better presentation
tab_model(analysis1_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl.doc")

#rumination model excluding lagged
analysis3_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+ 
                          (scale(PC_ES) +scale(PC_negemo)|ID),
                        data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_wl)

#better representation
tab_model(analysis3_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl.doc")



########## EXCLUDING LAGGED, INCLUDING AR STRUCTURE
#########


# main model with autoregressive structure & no lagged effect, lme instead of lme4
analysis1_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), random = ~ scale(PC_ES)+scale(PC_negemo)|ID, correlation=corAR1(form = ~ SurveyNumber |ID), data=ESM, na.action=na.omit)
summary(analysis1_NE_wl_AR)

# better presentation
tab_model(analysis1_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl AR.doc")

#interaction model with autoregressive structure & no lagged effect, lme instead of lme4, does not converge without simplifying random structure to random intercept only
analysis3_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum),random = ~1|ID, correlation=corAR1(form = ~ SurveyNumber |ID), data=ESM, na.action=na.omit)
summary(analysis3_NE_wl_AR)

#better representation
tab_model(analysis3_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl AR.doc")


######
### PE
######


#######
## 1. MAIN ANALYSES
#######

Sanalysis1_PE <- lmer(scale(pos_m_ED) ~ scale(PC_ES) +scale(PC_pos_m_ED_1)+scale(PC_posemo) +
                       (scale(PC_ES)+scale(PC_pos_m_ED_1)+scale(PC_posemo)|ID),
                     data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis1_PE)

tab_model(Sanalysis1_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1", "positive emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_PE.doc")


######
##  3. RUMINATION
#####

#note: too little variance in some random effects, runs without including random slope for rumin & interaction
Sanalysis3_PE <- lmer(scale(pos_m_ED) ~ scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)+
                       (scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_posemo)|ID),
                     data = ESM, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis3_PE)

#better presentation
tab_model(Sanalysis3_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination","Positive emotion","Interaction social sharing and rumination"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses3_PE.doc")




#############################################################
############## ALL MODELS IN SUPPLEMENTAL MATERIALS WITH ADJUSTED P-VALUES, in order of supplemental materials

###################################################
###################################################
###################################################


l_mod <- list("mod1s" = analysis1_NE_with_time, "mod2s" = analysis3_NE_with_time, "mod3s"=analysis1_NE_wl, "mod4s"=analysis3_NE_wl, "mod5s"=analysis1_NE_wl_AR, "mod6s"= analysis3_NE_wl_AR, 
              "mod7s"=Sanalysis1_PE, "mod8s"=Sanalysis3_PE,"mod9s"=analysis2_NE, "mod10s"=analysis3rev_NE)
msummary(l_mod)

adjMC <- function( model ) {
  model_glht <- glht(model)
  model_mc_adj <- summary(model_glht, test = adjusted('BH')) 
  return(model_mc_adj)
}

mod1s_adj <- adjMC(l_mod$mod1s)
mod2s_adj <- adjMC(l_mod$mod2s)
mod3s_adj <- adjMC(l_mod$mod3s)
mod4s_adj <-adjMC(l_mod$mod4s)
mod5s_adj <- adjMC(l_mod$mod5s)
mod6s_adj <- adjMC(l_mod$mod6s)
mod7s_adj <- adjMC(l_mod$mod7s)
mod8s_adj <-adjMC(l_mod$mod8s)
mod9s_adj <-adjMC(l_mod$mod9s)
mod10s_adj <-adjMC(l_mod$mod10s)



model_glht <- glht(analysis3_NE_wl_AR)
model_mc_adj <- summary(model_glht, test = adjusted('BH')) 


