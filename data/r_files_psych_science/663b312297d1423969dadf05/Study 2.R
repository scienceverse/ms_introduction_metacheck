##########################################################################"
#######################    emotion expression -->  emotion differentiation? 
#############################################################################

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
library (usethis)
library (devtools)
# install_github ("seanchrismurphy/emodiff")
library(emodiff) #to calculate the emotion differentiation indices



###########################################################################
#################### LONGITUDINAL##################################
###########################################################################
###########################################################################

##################################################################################
##################### DESCRIPTIVES ##############################################
#################################################################################
######
# 1) loading in preprocessed data 
######

#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Diary")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Longitudinal")

load(file = "Pre-processed momentary ED longitudinal.RData")

long <- long[with(long, order(Wave, PpID, DayNr,BeepNr)),]

descriptives <- long %>% select(PpID,negemo,neg_m_ED,ES,rum)
#automatically adds days & waves here, not needed
descriptives <- subset(descriptives, select = -1)
descriptives <- subset(descriptives, select = -1)


descriptives <- descriptives %>% 
  rename("Negative emotion" = negemo,
         "Emotion differentiation" = neg_m_ED, 
         "Social sharing" = ES, 
         "Rumination" = rum)

relevantvars <- descriptives %>% 
  statsBy(group = 'PpID', na.rm = TRUE) 

#Calculate means, SD, variance, and ICC
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
tab_df(relevantvars_output, title = "Descriptive Statistics", file = "descriptives_study2.html")

#####################################################################################
############################### analyses ############################################
#####################################################################################

### 
#setwd ("C:/Users/lasels/Desktop/ES _ ED/paper/Analyses/Longitudinal")
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Longitudinal")

######
# loading in preprocessed data 
######

load(file = "Pre-processed momentary ED longitudinal.Rdata")

##########


#######
## 1. MAIN ANALYSES (note that the reversed analyses - 2 & 3RE- are moved to supplemental materials)
#######
#######

#model 1

#crossed 
analysis1_NE_crossed <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|PpID)+(1|Wave),
                     data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_crossed)

#better presentation
tab_model(analysis1_NE_crossed,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "emotion differentiation at time T-1", "negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE 1.doc")

#when I do this model with a random intercept only (random intercept of wave has to be removed as well because of singular fit), effect is significant too. 
analysis1_NE_crossed_RI <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +
                               (1|PpID),
                             data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_crossed_RI)

tab_model(analysis1_NE_crossed_RI,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "emotion differentiation at time T-1", "negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE 1 with random intercept only.doc")

############
#### 2. REVERSED
###########


#crossed
analysis2_NE_crossed <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale(PC_negemo) +
                        (scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale(PC_negemo)|PpID) +
                        (1|Wave),
                      data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis2_NE_crossed)


#better representation
tab_model(analysis2_NE_crossed,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time t-1", "Social sharing at time T-1", "Negative emotion"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses2_NE 1.doc")


######
##  3. RUMINATION
#####


# crossed
analysis3_NE_crossed <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+
                            (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)|PpID) + 
                            (1|Wave),
                          data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_crossed)


#better representation
tab_model(analysis3_NE_crossed,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE 1.doc")




# does rumination also play a role in reversed relationship? 

# crossed model 
#removing  random intercept of NE & additionally removing random lagged effect ES did also not work 
#removing random wave intercept because of low variance, while adding these random effects back in also didn't work. 
# in the end, removed random intercept of NE & interaction with rumination: worked

analysis3rev_NE_crossed <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_rum)+scale(PC_negemo) +scale(PC_neg_m_ED_1):scale (PC_rum)+
                                   (scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_rum)|PpID) +
                                   (1|Wave),
                                 data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3rev_NE_crossed)

#better representation
tab_model(analysis3rev_NE_crossed,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "Rumination", "Negative emotion","Interaction emotion differentiation and rumination"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses reversed & rumination 1.doc")

#############################################################
############## MANUSCRIPT MODELS WITH ADJUSTED P-VALUES

###################################################
###################################################
###################################################


l_mod <- list("mod1" = analysis1_NE_crossed, "mod3"=analysis3_NE_crossed)
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

#main model including time 
analysis1_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +scale (BeepNr)+
                                 (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|PpID) + 
                                 (1|Wave),
                               data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_with_time)


# better presentation
tab_model(analysis1_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion differentiation at time T-1", "Negative emotion","beepnumber"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_with_time 1.doc")

# rumination model including time
analysis3_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+scale (BeepNr)+
                                 (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)|PpID) + 
                                 (1|Wave),
                               data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_with_time)


#better representation
tab_model(analysis3_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Beepnumber","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_with_time 1.doc")


############ EXCLUDING LAGGED EFFECTS
############

#main model excluding lagged 
analysis1_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_negemo)|PpID) + 
                       (1|Wave),
                     data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_wl)

# better presentation
tab_model(analysis1_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl 1.doc")

#check: also significant when random intercept only? 
analysis1_NE_wl_RI <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_negemo) +
                          (1|PpID) + 
                          (1|Wave),
                        data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_wl_RI)

# better presentation
tab_model(analysis1_NE_wl_RI,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl with random intercept only.doc")

# rumination model excluding lagged
analysis3_NE_wl <- lmer(scale(neg_m_ED) ~  scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+
                               ( scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)|PpID) + 
                               (1|Wave),
                             data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
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

#3 main model with autoregressive structure & no lagged effect, lme instead of lme4, simplified random structure because of convergence issues & errors
analysis1_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), 
                          random = list (PpID = ~ 1, Wave = ~ 1), correlation=corAR1(form = ~ BeepNr |PpID/Wave), data=long, na.action=na.omit)
summary(analysis1_NE_wl_AR)

# better presentation
tab_model(analysis1_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl AR.doc")

#interaction model with autoregressive structure & no lagged effect, lme instead of lme4, only  converges with random intercept
analysis3_NE_wl_AR <- lme(scale(neg_m_ED) ~  scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum),
                         random = list (PpID = ~ 1, Wave = ~ 1), correlation=corAR1(form = ~ BeepNr |PpID/Wave), data=long, na.action=na.omit)
summary(analysis3_NE_wl_AR)

tab_model(analysis3_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion", "Rumination","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl AR.doc")


########## MORE STRICT VERSION OF RUMINATION 
##################

#what for rumination when only past item? 
long$rum_OP <-rowMeans(subset(long, select="gepiekerdverleden"), na.rm=TRUE)
long$PC_rum_OP <- long$rum_OP - ave(long$rum_OP, long$Wave,long$PpID, FUN=function(x) mean(x, na.rm=TRUE))


analysis3_NE_crossed_OP <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum_OP) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum_OP)+
                                  (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum_OP) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum_OP)|PpID) + 
                                  (1|Wave),
                                data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_crossed_OP)


#better representation
tab_model(analysis3_NE_crossed_OP,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE 1 OP.doc")

###########
### PE
######


#######
## 1. MAIN ANALYSES
#######

Sanalysis1_PE <- lmer(scale(pos_m_ED) ~ scale(PC_ES) +scale(PC_pos_m_ED_1)+scale(PC_posemo) +
                        (scale(PC_ES)+scale(PC_pos_m_ED_1)+scale(PC_posemo)|PpID)+ (1|Wave),
                      data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis1_PE)

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

#removing random effects didn't work, random intercept wave was removed because of its low variance, just as lagged ED 
Sanalysis3_PE <- lmer(scale(pos_m_ED) ~ scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)+ 
                        (scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)|PpID),
                      data = long, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis3_PE)


#better representation
tab_model(Sanalysis3_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Positive emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_PE 1.doc")



#############################################################
############## ALL MODELS IN SUPPLEMENTAL MATERIALS WITH ADJUSTED P-VALUES, in order of supplemental materials

###################################################
###################################################
###################################################


l_mod <- list("mod1s" = analysis1_NE_with_time, "mod2s" = analysis3_NE_with_time, "mod3s"=analysis1_NE_wl, "mod4s"=analysis3_NE_wl, "mod5s"=analysis1_NE_wl_AR, "mod6s"= analysis3_NE_wl_AR, 
              "mod7s"=analysis3_NE_crossed_OP,"mod8s"=Sanalysis1_PE, "mod9s"=Sanalysis3_PE,"mod10s"=analysis2_NE_crossed, "mod11s"=analysis3rev_NE_crossed)
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
mod11s_adj <-adjMC(l_mod$mod11s)


