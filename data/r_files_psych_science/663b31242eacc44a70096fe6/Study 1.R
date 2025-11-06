

##########################################################################
#######################    emotion sharing -->  emotion differentiation? 
#########################################################################

#####################################
#########packages
#####################################

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
library(dplyr)
library(multilevel)
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
library (base)
library (metafor)
library(dplyr) 

#for adjusted p values
library(modelsummary)
library(lmerTest) 
library(parameters)
library(performance)
library (huxtable)
library (broom.mixed)
library (multcomp)


#for loading in momentary ED function - https://github.com/seanchrismurphy/emodiff
library (usethis)
library (devtools)
# install_github ("seanchrismurphy/emodiff")
library(emodiff) #to calculate the emotion differentiation indices

#######################################
########## study 1: daily diary########
#######################################


##################################################################################
##################### DESCRIPTIVES ##############################################
#################################################################################
######
# 1) loading in preprocessed data 
######

#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Diary")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Diary")

load(file = "Pre-processed momentary ED diary.RData")

day1 <- day1[with(day1, order(day, pnum)),]

descriptives <- day1 %>% select(pnum,negemo,neg_m_ED,ES,rum)

descriptives <- descriptives %>% 
  rename("Negative emotion" = negemo,
         "Emotion differentiation" = neg_m_ED, 
         "Social sharing" = ES, 
         "Rumination" = rum)

relevantvars <- descriptives %>% 
  statsBy(group = 'pnum', na.rm = TRUE) 

#Calculate mean, SDs, variance, and ICC
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
tab_df(relevantvars_output, title = "Descriptive Statistics", file = "descriptives_study1.html")

#####################################################################################
############################### analyses ############################################
#####################################################################################

######
# 1) loading in preprocessed data 
######

#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Diary")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Diary")

load(file = "Pre-processed momentary ED diary.RData")


#######
## 1. MAIN ANALYSES (note that the reversed analyses - 2 & 3RE- are moved to supplemental materials)
#######

#lme works when random slope for mean neg emotion is left out 
analysis1_NE <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_neg_m_ED_1)+scale (PC_negemo), random = ~ 1+scale(PC_ES)+scale(PC_neg_m_ED_1) | pnum, data=day1, na.action=na.omit)

summary (analysis1_NE)

# better presentation
tab_model(analysis1_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE 1.doc")


#analysis with only lagged neg ED as a predictor
analysislagonly <- lme(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1), random = ~ 1+scale(PC_neg_m_ED_1) | pnum, data=day1, na.action=na.omit)
summary (analysislagonly)
# better presentation
tab_model(analysislagonly,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file =" lag only.doc")


#########
############
#### 2. REVERSED
###########

#lme: works when removing random slopes of lagged effect of ES & mean  
analysis2_NE <- lme(scale(ES) ~ scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_negemo), random = ~ 1+scale(PC_neg_m_ED_1)| pnum, data=day1, na.action=na.omit)
summary(analysis2_NE)


#better representation
tab_model(analysis2_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "Negative emotion"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses2_NE 1.doc")


######
##  3. RUMINATION
#####


#I had to remove more random slopes 
analysis3_NE <- lme(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+scale (PC_ES)+scale(PC_rum)+scale(PC_negemo)+scale (PC_ES):scale(PC_rum), random = ~ 1+scale(PC_ES) | pnum, data=day1, na.action=na.omit)
summary(analysis3_NE)


#better representation
tab_model(analysis3_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE 1.doc")

# does rumination also play a role in reversed relationship? 
#only random slope of main predictor is kept because of convergence issues 

analysis3rev_NE <- lme(scale(ES) ~ scale(PC_neg_m_ED_1)+scale(PC_ES_1)+ scale (PC_rum)+scale(PC_negemo)+scale(PC_neg_m_ED_1):scale (PC_rum), random = ~1+
                           scale(PC_neg_m_ED_1)|pnum, data = day1, na.action=na.omit)
summary(analysis3rev_NE)


#better representation
tab_model(analysis3rev_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1","Social sharing at time T-1",  "Rumination", "Negative emotion","interaction emotion differentiation at time T-1 and rumination"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses reversed & rumination 1.doc")

#############################################################
############## MANUSCRIPT MODELS WITH ADJUSTED P-VALUES

###################################################
###################################################
###################################################


l_mod <- list("mod1" = analysis1_NE, "mod3"=analysis3_NE)
msummary(l_mod)

adjMC <- function( model ) {
  model_glht <- glht(model)
  model_mc_adj <- summary(model_glht, test = adjusted('BH')) 
  return(model_mc_adj)
}

mod1_adj <- adjMC(l_mod$mod1)
mod3_adj <- adjMC(l_mod$mod3)

#############################################################
###################### SUPPLEMENTARY MATERIALS ####################
############################################################

#################### TIME INCLUDED 
#####################
# main model including time 
# works when random slope for mean neg emotion is left out 
analysis1_NE_with_time <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_neg_m_ED_1)+scale (PC_negemo)+scale(day), random = ~ 1+scale(PC_ES)+scale(PC_neg_m_ED_1) | pnum, data=day1, na.action=na.omit)
summary(analysis1_NE_with_time)

# better presentation
tab_model(analysis1_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1","Negative emotion","Day"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_with_time 1.doc")


#rumination model including time
analysis3_NE_with_time <- lme(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+scale (PC_ES)+scale(PC_rum)+scale(PC_negemo)+scale (PC_ES):scale(PC_rum)+scale (day), random = ~ 1+scale(PC_ES) | pnum, data=day1, na.action=na.omit)
summary(analysis3_NE_with_time)


#better representation: day comes before interaction in output, so change order
tab_model(analysis3_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","day","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_with_time 1.doc")

################## LAG EXCLUDED
###################

#main model excluding lagged, random intercept only because of convergence
analysis1_NE_wl <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), random = ~ 1| pnum, data=day1, na.action=na.omit)
summary(analysis1_NE_wl)

# better presentation
tab_model(analysis1_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl 1.doc")

#rumination model excluding lagged, random intercept only because of convergence
analysis3_NE_wl <- lme(scale(neg_m_ED) ~ scale (PC_ES)+scale(PC_rum)+scale(PC_negemo)+scale (PC_ES):scale(PC_rum), random = ~ 1 | pnum, data=day1, na.action=na.omit)
summary(analysis3_NE_wl)

tab_model(analysis3_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_WL 1.doc")


########### LAG EXCLUDED & AR STRUCTURE
###########

#main model with autoregressive structure & no lagged effect
#lme works only when random intercept only

analysis1_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), random = ~ 1| pnum, correlation=corAR1(form = ~ day | pnum), data=day1, na.action=na.omit)
summary(analysis1_NE_wl_AR)

tab_model(analysis1_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl_AR.doc")

#interaction model with autoregressive structure & no lagged effect, random intercept only
analysis3_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale (PC_ES)+scale(PC_rum)+scale(PC_negemo)+scale (PC_ES):scale(PC_rum), random = ~ 1 | pnum, correlation=corAR1(form = ~ day | pnum), data=day1, na.action=na.omit)
summary(analysis3_NE_wl_AR)

tab_model(analysis3_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_WL AR.doc")


#############################################################
############## ALL MODELS IN SUPPLEMENTAL MATERIALS WITH ADJUSTED P-VALUES, in order of supplemental materials

###################################################
###################################################
###################################################


l_mod <- list("mod1s" = analysis1_NE_with_time, "mod2s" = analysis3_NE_with_time, "mod3s"=analysis1_NE_wl, "mod4s"=analysis3_NE_wl, "mod5s"=analysis1_NE_wl_AR, "mod6s"= analysis3_NE_wl_AR, "mod7s"=analysis2_NE, "mod8s"=analysis3rev_NE)
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

