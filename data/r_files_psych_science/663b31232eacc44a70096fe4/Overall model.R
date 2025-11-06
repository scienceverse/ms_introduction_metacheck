####packages 
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

############ load in all data ######

##
#study 1
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Diary")
load(file = "Pre-processed momentary ED diary.RData")
data1<-day1
data1<-data1 %>%rename("ID"="pnum","time"="day")

##
#study 2
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Longitudinal")
load(file = "Pre-processed momentary ED longitudinal.Rdata")
data2<-long

#make one study for longitudinal, in which beepnr really becomes one accumulative beep, across waves 
#sort in right way
data2 <- data2[with(data2, order(data2$PpID, data2$Wave,data2$DayNr),data2$BeepNr),]

#make accumulative variable
data2$time <- 1
data2 <- data2 %>% group_by(PpID) %>% mutate(time = cumsum(time))


#rename
data2<-data2 %>%rename("ID"="PpID")

##
#study 3 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Exam")
load(file = "Pre-processed momentary ED Exam.RData")
data3<-Exam
data3<-data3 %>%rename("ID"="Participant","time"="beepnum", "rum"="ER_rumination")


##
#study 4
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Preregistered")
load(file = "Pre-processed momentary ESM.Rdata")
data4<-ESM

data4<-data4 %>%rename("time"="SurveyNumber","rum"="RUM")

############ subset ###########

subs<- c("ID","time", "neg_m_ED","PC_ES","PC_neg_m_ED_1", "PC_negemo","ES","PC_ES_1", "PC_rum","negemo","rum")     
data1_NA<-data1[,subs]
data2_NA<-data2[,subs]
data3_NA<-data3[,subs]
data4_NA<-data4[,subs]


###### preparing & merging data 
data1_NA$study<-1
data2_NA$study<-2
data3_NA$study<-3
data4_NA$study<-4

merged_data<-rbind (data1_NA,data2_NA,data3_NA,data4_NA)

#####EXPORTING THE PREPROCESSED DATA####
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/metamodel")
save(merged_data, file = "Pre-processed momentary ED overall.RData")

######################################################################################
######################################################################################

###
## setwd 
### 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/metamodel")

######
# loading in preprocessed data (created using the corresponding preprocessing code)
######

load(file = "Pre-processed momentary ED overall.RData")

#######
## 1. MAIN ANALYSES (note that the reversed analyses - 2 & 3RE- are moved to supplemental materials)
#######

#model 1
analysis1_NE <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +
                       (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|ID) + 
                       (1|study), 
                     data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE)

# better presentation
tab_model(analysis1_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "emotion differentiation at time T-1", "negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_overall.doc")

############
#### 2. REVERSED
###########

analysis2_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1) +scale(PC_ES_1)+scale(PC_negemo) +
                        (scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale(PC_negemo)|ID) + 
                        (1|study),
                      data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis2_NE)


#better representation
tab_model(analysis2_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time t-1", "Social sharing at time T-1", "Negative emotion"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses2_NE_overall.doc")


######
##  3. RUMINATION
#####

analysis3_NE <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+
                       (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|ID) + 
                       (1|study),
                     data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE)


#better representation
tab_model(analysis3_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_overall.doc")


####for visualisation 
#pull the scaled analysis data from the lme4 element itself
analysis3_NE_data <- analysis3_NE@frame

#the variables are still called "scale(variable)" which will create problems in later analysis...
#so I'm renaming them 
analysis3_NE_data <- analysis3_NE_data %>% rename("S_neg_m_ED" = "scale(neg_m_ED)", 
                                                  "S_PC_neg_m_ED_1" = "scale(PC_neg_m_ED_1)",
                                                  "S_PC_ES" = "scale(PC_ES)",
                                                  "S_PC_rum" = "scale(PC_rum)",
                                                  "S_PC_negemo" = "scale(PC_negemo)")


#now I'm running an analysis on that dataframe - which looks exactly the same as the initial analysis
analysis3_NE_S <- lmer(S_neg_m_ED ~ S_PC_neg_m_ED_1 + S_PC_ES+S_PC_rum +S_PC_negemo +S_PC_ES:S_PC_rum+
                         (S_PC_neg_m_ED_1 + S_PC_ES+S_PC_rum +S_PC_negemo+S_PC_ES:S_PC_rum|ID) + 
                         (1|study),
                       data = analysis3_NE_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_S)


#now I can do a simple slopes analysis on this output... yay! 
sim_slopes_model<-sim_slopes(model = analysis3_NE_S,
           pred = S_PC_ES,
           modx = S_PC_rum)

help (plot_model)
# VISUALIZATION 
p <- plot_model(analysis3_NE_S, type = "int", mdrt.values="meansd", title="    Model across studies", 
                axis.title=c("Social sharing"," Emotion differentiation"), 
                legend.title="Rumination",ci.lvl=0.90,legend.format)
p <- p + theme_bw()+theme(panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text(family='Times',size=12))

p<-p + scale_color_hue(labels = c("-1", "0", "1"))


help (plot_model)
# does rumination also play a role in reversed relationship? 
#random interaction effect is removed because of convergence issues, just as random slope for neg emotion 
analysis3rev_NE <- lmer(scale(ES) ~ scale(PC_neg_m_ED_1)+scale(PC_ES_1)+scale (PC_rum)+scale(PC_negemo) +scale(PC_neg_m_ED_1):scale (PC_rum)+
                           (scale(PC_neg_m_ED_1)+scale (PC_rum)|ID) + 
                           (1|study),
                         data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3rev_NE)


#better representation
tab_model(analysis3rev_NE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing at time T-1", "Rumination", "Negative emotion","Interaction emotion differentiation and rumination"), 
          dv.labels = "Social sharing",
          use.viewer=TRUE, file ="overviewanalyses reversed & rumination_overall.doc")


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


#for simple slopes model
sim_slopes_model
p.values <-c(.00,.54,.00)
p.adjust (p.values, method="BH")


##################################
########## SUPPLEMENTARY MATERIALS
##################################


######### TIME INCLUDED 
#########

#main model including time 
analysis1_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_neg_m_ED_1)+scale(PC_negemo) +scale (time)+
                                 (scale(PC_ES)+scale(PC_neg_m_ED_1)+scale(PC_negemo)|ID) + 
                                 (1|study),
                               data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_with_time)


# better presentation
tab_model(analysis1_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion differentiation at time T-1", "Negative emotion","time"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_with_time overall.doc")

#rumination model including time 
analysis3_NE_with_time <- lmer(scale(neg_m_ED) ~ scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+scale (time)+
                                 (scale(PC_neg_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|ID) + 
                                 (1|study),
                               data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_with_time)


#better representation,  time for some reason comes before the interaction, so also adapted in labels
tab_model(analysis3_NE_with_time,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Negative emotion","Time", "Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_with_time overall.doc")



############ EXCLUDING LAGGED EFFECTS
############

#1: main model excluding lagged, does not converge, therefore random slope of negemo & social sharing removed 
analysis1_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES) +scale(PC_negemo) +
                          (1|ID) + 
                          (1|study),
                        data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis1_NE_wl)

# better presentation
tab_model(analysis1_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_NE_wl overall.doc")

labels (merged_data)

#rumination model excluding lagged
analysis3_NE_wl <- lmer(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum)+
                       ( scale(PC_ES)+scale(PC_rum) +scale(PC_negemo)+ scale(PC_ES):scale(PC_rum)|ID) + 
                       (1|study),
                     data = merged_data, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(analysis3_NE_wl)


#better representation
tab_model(analysis3_NE_wl,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl overall.doc")


########## EXCLUDING LAGGED, INCLUDING AR STRUCTURE
#########

#3 main model with autoregressive structure & no lagged effect, lme instead of lme4
#random slope of neg emotion had to be removed to converge 
analysis1_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale (PC_negemo), list (ID = ~ scale(PC_ES), study = ~ 1), correlation=corAR1(form = ~ time |ID/study), data=merged_data, na.action=na.omit)

summary(analysis1_NE_wl_AR)

# better presentation -->can't get tab_Model to work here, therefore just summary results are used 
tab_model(analysis1_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE, show.effects = "fixed",
          string.ci = "95% CI", collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Negative emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer = TRUE, file = "overviewanalyses1_NE_wl AR overall.doc")

intervals (analysis1_NE_wl_AR)


#interaction model with autoregressive structure & no lagged effect, lme instead of lme4, simplified random structure
analysis3_NE_wl_AR <- lme(scale(neg_m_ED) ~ scale(PC_ES)+scale(PC_rum) +scale(PC_negemo) + scale(PC_ES):scale(PC_rum),
                          list (ID = ~ scale(PC_ES)+scale (PC_rum)+ scale(PC_ES):scale(PC_rum), study = ~ 1), correlation=corAR1(form = ~ time |ID/study), data=merged_data, na.action=na.omit)
summary(analysis3_NE_wl_AR)


# better presentation -->can't get tab_Model to work here, therefore just summary results are used 
tab_model(analysis3_NE_wl_AR,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Rumination", "Negative emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_NE_wl AR overall.doc")

intervals (analysis3_NE_wl_AR, which = "fixed")


######
### PE
######

## study 2 to 4 because study 4 doesn't have pos emotions

##
#study 1
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Diary")
load(file = "Pre-processed momentary ED diary.RData")
data1<-day1
data1<-data1 %>%rename("ID"="pnum","time"="day")

##
#study 2
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Longitudinal")
load(file = "Pre-processed momentary ED longitudinal.Rdata")
data2<-long

#make one study for longitudinal, in which beepnr really becomes one accumulative beep, across waves 
#sort in right way
data2 <- data2[with(data2, order(data2$PpID, data2$Wave,data2$DayNr),data2$BeepNr),]

#make accumulative variable
data2$time <- 1
data2 <- data2 %>% group_by(PpID) %>% mutate(time = cumsum(time))


#rename
data2<-data2 %>%rename("ID"="PpID")

##
#study 3 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Exam")
load(file = "Pre-processed momentary ED Exam.RData")
data3<-Exam
data3<-data3 %>%rename("ID"="Participant","time"="beepnum", "rum"="ER_rumination")


##
#study 4
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/Preregistered")
load(file = "Pre-processed momentary ESM.Rdata")
data4<-ESM

data4<-data4 %>%rename("time"="SurveyNumber","rum"="RUM")


#subset for the needed variables
subs_withpos<- c("ID","time", "pos_m_ED","PC_ES","PC_pos_m_ED_1", "PC_posemo","ES","PC_ES_1", "PC_rum")     

data2_PA<-data2[,subs_withpos]
data3_PA<-data3[,subs_withpos]
data4_PA<-data4[,subs_withpos]


###### preparing & merging data 
data2_PA$study<-2
data3_PA$study<-3
data4_PA$study<-4

merged_datapos<-rbind (data2_PA,data3_PA,data4_PA)

#####EXPORTING THE PREPROCESSED DATA####
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/metamodel")
save(long, file = "Pre-processed momentary ED overall PA.RData")

###
## setwd 
### 
setwd ("C:/Users/lasels/OneDrive - UGent/Desktop/alle files/ES _ ED/paper/Analyses/metamodel")

######
# loading in preprocessed data (created using the corresponding preprocessing code)
######

load(file = "Pre-processed momentary ED overall PA.RData")


#######
## 1. MAIN ANALYSES
#######

Sanalysis1_PE <- lmer(scale(pos_m_ED) ~ scale(PC_ES) +scale(PC_pos_m_ED_1)+scale(PC_posemo) +
                        (scale(PC_ES)+scale(PC_pos_m_ED_1)+scale(PC_posemo)|ID) + 
                        (1|study),
                      data = merged_datapos, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis1_PE)

tab_model(Sanalysis1_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Social sharing", "Positive emotion differentiation at time T-1", "Positive emotion"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses1_PE overall.doc")


######
##  3. RUMINATION
#####

Sanalysis3_PE <- lmer(scale(pos_m_ED) ~ scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)+ 
                        (scale(PC_pos_m_ED_1)+ scale(PC_ES)+scale(PC_rum) +scale(PC_posemo) +scale(PC_ES):scale(PC_rum)|ID) + 
                        (1|study),
                      data = merged_datapos, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(Sanalysis3_PE)

#better representation
tab_model(Sanalysis3_PE,
          show.re.var = FALSE, show.icc = FALSE, show.se = TRUE, show.r2 = FALSE, show.ci = 0.95,
          show.obs = FALSE, show.ngroups = FALSE,
          string.ci = "95% CI",collapse.se = TRUE, string.est = "Estimate (SE)", 
          pred.labels = c("Intercept", "Emotion differentiation at time T-1", "Social sharing", "Rumination", "Positive emotion","Interaction social sharing and rumination"), 
          dv.labels = "Emotion differentiation",
          use.viewer=TRUE, file ="overviewanalyses3_PE overall.doc")




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
