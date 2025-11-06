

##########################################################################"
#######################    emotion sharing -->  emotion differentiation? 
#############################################################################

#####################################
#########packages
###################################

library (foreign)
library (psych)
library (openxlsx)
library (ppcor)
library (reshape2)
library (data.table)
library(lme4)
library (DataCombine)
library(plyr)
library(multilevel)
library(dplyr)
library (lavaan)
library(nlme)
library (matrixStats)
library(magrittr)
library (sjPlot)
library (stargazer)
library (ggplot2)
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
library (usethis)
library (devtools)
library(emodiff) #to calculate the emotion differentiation indices


##################################################################################
##################### DESCRIPTIVES ##############################################
#################################################################################

######
# 1) loading in preprocessed data 
######
###
#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Exam")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Exam")

######
# loading in preprocessed data (created using the corresponding preprocessing code)
######

load(file = "Pre-processed momentary ED Exam.Rdata")
Exam <- Exam[with(Exam, order(Participant, beepnum)),]

descriptives <- Exam %>% select(Participant,negemo,neg_m_ED,ES,ER_rumination)
#automatically adds beepday here, not needed
descriptives <- subset(descriptives, select = -1)

descriptives <- descriptives %>% 
  rename("Negative emotion" = negemo,
         "Emotion differentiation" = neg_m_ED, 
         "Social sharing" = ES, 
         "Rumination" = ER_rumination)

relevantvars <- descriptives %>% 
  statsBy(group = 'Participant', na.rm = TRUE) 

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
tab_df(relevantvars_output, title = "Descriptive Statistics", file = "descriptives_study3.html")


#####################################################################################
############################### analyses ############################################
#####################################################################################

#####################################################################################

###
#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Exam")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Exam")

######
# loading in preprocessed data (created using the corresponding preprocessing code)
######

load(file = "Pre-processed momentary ED Exam.Rdata")


##########################
#######
## 1. MAIN ANALYSES (note that the reversed analyses - 2 & 3RE- are moved to supplemental materials)
#######

analysis1_NE <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|Participant),
                     data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
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

analysis2_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale(PC_negemo) +
                       (scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale(PC_negemo)|Participant),
                     data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis2_NE)


#better representation
tab_model(analysis2_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "Negative emotion"), 
          dv.labels = "SOcial sharing",
          use.viewer=TRUE, file ="overviewanalyses2_NE 1.doc")


######
##  3. RUMINATION
#####


analysis3_NE <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) +scale(PC_ES):scale(PC_rum)+
                       (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|Participant),
                     data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
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
#random effect of interaction is omitted (excluding random slope for neg emotion did not work,)

analysis3rev_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale (PC_rum)+scale(PC_negemo) +scale (PC_neg_m_ED_1):scale (PC_rum)+
                           (scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_rum)+scale(PC_negemo)|Participant),
                         data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3rev_NE)


#better representation
tab_model(analysis3rev_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "rumination", "Negative emotion","interaction  emotion differentiation at time T-1 and rumination"), 
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


##################################
########## SUPPLEMENTARY MATERIALS 
##################################


######### TIME INCLUDED 
#########

#2 main model including time 
analysis1_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) + scale (beepnum)+
                                 (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|Participant),
                               data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_with_time)


# better presentation
tab_model(analysis1_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Emotion differentiation at time T-1", "Negative emotion","Beepnumber"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_with_time 1.doc")

#rumination model including time 
analysis3_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1) + scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) +scale(PC_ES):scale(PC_rum)+scale (beepnum)+
                                 (scale(PC_neg_m_ED_1) +scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|Participant),
                               data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_with_time)


#better representation
tab_model(analysis3_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept",  "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Beep number","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_with_time 1.doc")


############ EXCLUDING LAGGED EFFECTS
############

#1: main model excluding lagged 
analysis1_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_negemo)|Participant),
                     data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_wl)


# better presentation
tab_model(analysis1_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl 1.doc")


# rumination model excluding lagged 
analysis3_NE_wl <- lmer(scale(neg_m_ED) ~ + scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) +scale(PC_ES):scale(PC_rum)+
                       (scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|Participant),
                     data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_wl)


#better representation
tab_model(analysis3_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl 1.doc")




########## EXCLUDING LAGGED, INCLUDING AR STRUCTURE
#########


#3 main model with autoregressive structure & no lagged effect, lme instead of lme4

analysis1_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), random = ~ scale(PC_ES)+scale(PC_negemo)|Participant, correlation=corAR1(form = ~ beepnum |Participant), data=Exam, na.action=na.omit)
summary(analysis1_NE_wl_AR)

# better presentation
tab_model(analysis1_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl AR.doc")

#interaction model with autoregressive structure & no lagged effect, lme instead of lme4
analysis3_NE_wl_AR <- lme(scale(neg_m_ED) ~ + scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) +scale(PC_ES):scale(PC_rum),
                          random = ~ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|Participant,correlation=corAR1(form = ~ beepnum |Participant), data=Exam, na.action=na.omit)
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
                        (scale(PC_ES)+scale(PC_pos_m_ED_1)+scale(PC_posemo)|Participant),
                      data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis1_PE)

# better presentation
tab_model(Sanalysis1_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Positive emotion differentiation at time T-1", "Positive emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_PE 1.doc") 


######
##  3. RUMINATION
#####


#note: too little variance in some random effects, runs without including random slope for rumin & interaction
Sanalysis3_PE <- lmer(scale(pos_m_ED) ~ scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)+
                        (scale(PC_pos_m_ED_1)+scale(PC_ES) +scale(PC_posemo)|Participant),
                      data = Exam, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis3_PE)


#better representation
tab_model(Sanalysis3_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Positive emotion differentiation at time T-1", "Social sharing", "Rumination", "Positive emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_PE 1.doc")



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





