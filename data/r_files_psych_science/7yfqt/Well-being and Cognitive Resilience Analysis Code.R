library(modelr)
library(dplyr)
library(lme4)
library(ggplot2)
library(gridExtra)

#compute eudaimonic well-being variable with purpose subscale omitted
wellbeing_sanspurpose = with(data_long, cbind(wellbeing_accept,wellbeing_auton,wellbeing_envmas,wellbeing_growth,wellbeing_posrel), na.rm = T)
data_long$wellbeing_sanspurpose <- wellbeing_sanspurpose
data_long$wellbeing_sanspurpose <- rowMeans(wellbeing_sanspurpose, na.rm = T)
cor.test(data_long$wellbeing_sanspurpose,data_long$wellbeing)

#APOE genotype was dichotomized by presence of e4 allele (0 = No; 1 = Yes).
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==24]<-1
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==34]<-1
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==44]<-1
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==22]<-0
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==23]<-0
data$apoe[!is.na(data$apoe_genotype)&data$apoe_genotype==33]<-0

#Determine whether well-being data is present  
data_long$wellbeing_present[!is.na(data_long$wellbeing)|!is.na(data_long$satisfaction)]<-1
data_long$wellbeing_present[is.na(data_long$wellbeing)&is.na(data_long$satisfaction)]<-0
summary(data_long$wellbeing_present)

#merge last global cognition score in main dataset 
last_timepoint<-subset(data_long,!is.na(data_long$cogn_global)) #store only rows with cognition data
last_timepoint<-last_timepoint %>% group_by(projid) %>% slice_max(fu_year) #store only the last MO for each participant
last_timepoint_cog<-cbind.data.frame(last_timepoint$projid,last_timepoint$cogn_global,last_timepoint$age_at_visit,last_timepoint$fu_year) #store only relevant variables from last cognitive assessment before death
names(last_timepoint_cog)<-c("projid","cogn_global","cog_age_at_visit","cog_fu_year")
data<-merge(data,last_timepoint_cog,by="projid") #merge data

#merge first well-being assessment in main dataset 
first_timepoint<-subset(data_long,!is.na(data_long$wellbeing)|!is.na(data_long$satisfaction)) #store only rows with wb data
first_timepoint<-first_timepoint %>% group_by(projid) %>% slice_min(fu_year) #store only first MO for each participant

first_timepoint_vars<-cbind.data.frame(first_timepoint$dcfdx,first_timepoint$age_at_visit,first_timepoint$projid,first_timepoint$fu_year,first_timepoint$satisfaction,first_timepoint$wellbeing,
                                       first_timepoint$purpose_total,first_timepoint$wellbeing_sanspurpose,
                                       first_timepoint$late_life_cogact_freq,first_timepoint$cesdsum,
                                       first_timepoint$wellbeing_item1,first_timepoint$wellbeing_item2,first_timepoint$wellbeing_item3,
                                       first_timepoint$wellbeing_item4,first_timepoint$wellbeing_item5,first_timepoint$wellbeing_item6,
                                       first_timepoint$wellbeing_item7,first_timepoint$wellbeing_item8,first_timepoint$wellbeing_item9,
                                       first_timepoint$wellbeing_item10,first_timepoint$wellbeing_item11,first_timepoint$wellbeing_item12,
                                       first_timepoint$wellbeing_item13,first_timepoint$wellbeing_item14,first_timepoint$wellbeing_item15,
                                       first_timepoint$wellbeing_item16,first_timepoint$wellbeing_item17,first_timepoint$wellbeing_item18,
                                       first_timepoint$satisf_item1,first_timepoint$satisf_item2,first_timepoint$satisf_item3,first_timepoint$satisf_item4,first_timepoint$satisf_item5) #store only relevant variables from first WB MO 

names(first_timepoint_vars)<-c("dcfdx",
                               "wb_age_at_visit",
                               "projid","wb_fu_year",
                               "satisfaction","wellbeing",
                               "purpose_total","wellbeing_sanspurpose",
                               "late_life_cogact_freq","cesdsum",
                               "wellbeing_item1","wellbeing_item2","wellbeing_item3",
                               "wellbeing_item4","wellbeing_item5","wellbeing_item6",
                               "wellbeing_item7","wellbeing_item8","wellbeing_item9",
                               "wellbeing_item10","wellbeing_item11","wellbeing_item12",
                               "wellbeing_item13","wellbeing_item14","wellbeing_item15",
                               "wellbeing_item16","wellbeing_item17","wellbeing_item18",
                               "satisf_item1","satisf_item2","satisf_item3","satisf_item4","satisf_item5")

data<-merge(data,first_timepoint_vars,by="projid")

#compute time lag between last cognitive assessment and death 
data$time_lag_cog_death<-data$age_death-data$cog_age_at_visit
#compute time lag between WB assessment and death
data$time_lag_wb_death<-data$age_death-data$wb_age_at_visit
#compute time lag between WB assessment and cognitive assessment 
data$time_lag_wb_cog<-data$cog_age_at_visit-data$wb_age_at_visit

data<-subset(data,data$wb_fu_year<=data$cog_fu_year)
nrow(data) #remove 1 participant whose final cognitive assessment occurs before wb data collection 

#Figure 1 Flowchart of participant inclusion 
data<-subset(data,
             !is.na(data$wellbeing)|
               !is.na(data$satisfaction))

nrow(data) #At least one MO of well-being and one MO of cognition = 1328

summary(as.factor(data$dcfdx)) 
data<-subset(data,is.na(data$dcfdx)|data$dcfdx==1|data$dcfdx==2|data$dcfdx==3)
nrow(data) #No dementia at analytic basleine = 1280

data<-subset(data,
             !is.na(data$amyloid)|
               !is.na(data$tangles)|
               !is.na(data$ci_num2_gct)|
               !is.na(data$ci_num2_mct)|
               !is.na(data$lb_neo)|
               !is.na(data$tdp_st4)|
               !is.na(data$caa_4gp)|
               !is.na(data$cvda_4gp2)|
               !is.na(data$arteriol_scler)|
               !is.na(data$hip_scl_3reg_yn))

data<-subset(data,
             !is.na(data$amyloid)&
               !is.na(data$tangles)&
               !is.na(data$ci_num2_gct)&
               !is.na(data$ci_num2_mct)&
               !is.na(data$lb_neo)&
               !is.na(data$tdp_st4)&
               !is.na(data$caa_4gp)&
               !is.na(data$cvda_4gp2)&
               !is.na(data$arteriol_scler)&
               !is.na(data$hip_scl_3reg_yn))

nrow(data) #Complete neuropathology data = 348

#reduce data_long file to include only participants who meet inclusion criteria 
data_long<-subset(data_long,data_long$projid%in%data$projid)

#compute cognitive decline for alternative operationalization of resilience 
data_long<-merge(data_long,first_timepoint_vars,by="projid",all.x=T,all.y=T)
data_long<-subset(data_long,data_long$fu_year>data_long$wb_fu_year) #remove datapoints prior to and concurrent with first wb assignment
cog_decline_mod <- lmer(cogn_global~fu_year + (1+fu_year|projid), data=data_long, REML=F,
                        control = lmerControl(optimizer = nloptwrap, calc.derivs = FALSE))
reff = (coef(cog_decline_mod)$projid) 
projid = as.numeric(rownames(reff)) 
reff = as.data.frame(cbind(reff,projid)) 
names(reff)<-c("cog_int","cog_decline","projid") 
data<-merge(data,reff,by="projid",all.x=T)
summary(cog_decline_mod)

#The time lag between well-being assessment and death ranged from 0.15 to 11.84 years (M = 5.30, SD = 2.69).   
summary(data$time_lag_wb_death)
sd(data$time_lag_wb_death)

#The time lag between well-being assessment and final measurement of cognition before death ranged from 0.00 to 11.13 years (M = 4.34, SD = 2.64).  
summary(data$time_lag_wb_cog)
sd(data$time_lag_wb_cog)

#The time lag between final cognitive assessment and death was on average one year (M = 0.97, SD = 0.96, range = 0.03 to 6.66).
summary(data$time_lag_cog_death)
sd(data$time_lag_cog_death)

#Table 1
psych::describe(data$wellbeing)
psych::describe(data$satisfaction)
psych::describe(data$age_death)
table(data$msex)
table(data$msex)/nrow(data)
psych::describe(data$early_hh_ses)
psych::describe(data$educ)
psych::describe(data$lifetime_cogact_freq_bl)
psych::describe(data$neuroticism_12)
psych::describe(data$cesdsum)
table(data$apoe)
table(data$apoe)/nrow(data)
psych::describe(data$med_con_sum_bl)
psych::describe(data$time_lag_cog_death)
psych::describe(data$time_lag_wb_death) 
psych::describe(data$time_lag_wb_cog) 

#alpha reliabilities 
data$wellbeing_item1<-as.numeric(data$wellbeing_item1)
data$wellbeing_item2<-as.numeric(data$wellbeing_item2)
data$wellbeing_item3<-as.numeric(data$wellbeing_item3)
data$wellbeing_item4<-as.numeric(data$wellbeing_item4)
data$wellbeing_item5<-as.numeric(data$wellbeing_item5)
data$wellbeing_item6<-as.numeric(data$wellbeing_item6)
data$wellbeing_item7<-as.numeric(data$wellbeing_item7)
data$wellbeing_item8<-as.numeric(data$wellbeing_item8)
data$wellbeing_item9<-as.numeric(data$wellbeing_item9)
data$wellbeing_item10<-as.numeric(data$wellbeing_item10)
data$wellbeing_item11<-as.numeric(data$wellbeing_item11)
data$wellbeing_item12<-as.numeric(data$wellbeing_item12)
data$wellbeing_item13<-as.numeric(data$wellbeing_item13)
data$wellbeing_item14<-as.numeric(data$wellbeing_item14)
data$wellbeing_item15<-as.numeric(data$wellbeing_item15)
data$wellbeing_item16<-as.numeric(data$wellbeing_item16)
data$wellbeing_item17<-as.numeric(data$wellbeing_item17)
data$wellbeing_item18<-as.numeric(data$wellbeing_item18)

psych::alpha(cbind(data$wellbeing_item1,data$wellbeing_item2,data$wellbeing_item3,data$wellbeing_item4,data$wellbeing_item5,data$wellbeing_item6,
               data$wellbeing_item7,data$wellbeing_item8,data$wellbeing_item9,data$wellbeing_item10,data$wellbeing_item11,data$wellbeing_item12,
               data$wellbeing_item13,data$wellbeing_item14,data$wellbeing_item15,data$wellbeing_item16,data$wellbeing_item17,data$wellbeing_item18))

data$satisf_item1<-as.numeric(data$satisf_item1)
data$satisf_item2<-as.numeric(data$satisf_item2)
data$satisf_item3<-as.numeric(data$satisf_item3)
data$satisf_item4<-as.numeric(data$satisf_item4)
data$satisf_item5<-as.numeric(data$satisf_item5)

psych::alpha(cbind(data$satisf_item1,data$satisf_item2,data$satisf_item3,
                   data$satisf_item4,data$satisf_item5))

#Table 2
#Column 1
cor.test(data$wellbeing,data$satisfaction)
cor.test(data$wellbeing,data$amyloid)
cor.test(data$wellbeing,data$tangles)
cor.test(data$wellbeing,data$lb_neo)
cor.test(data$wellbeing,data$ci_num2_gct)
cor.test(data$wellbeing,data$ci_num2_mct)
cor.test(data$wellbeing,data$caa_4gp)
cor.test(data$wellbeing,data$cvda_4gp2)
cor.test(data$wellbeing,data$arteriol_scler)
cor.test(data$wellbeing,data$hip_scl_3reg_yn)
cor.test(data$wellbeing,data$tdp_st4)
#Column 2
cor.test(data$satisfaction,data$amyloid)
cor.test(data$satisfaction,data$tangles)
cor.test(data$satisfaction,data$lb_neo)
cor.test(data$satisfaction,data$ci_num2_gct)
cor.test(data$satisfaction,data$ci_num2_mct)
cor.test(data$satisfaction,data$caa_4gp)
cor.test(data$satisfaction,data$cvda_4gp2)
cor.test(data$satisfaction,data$arteriol_scler)
cor.test(data$satisfaction,data$hip_scl_3reg_yn)
cor.test(data$satisfaction,data$tdp_st4)

#All continuous variables will be z-standardized. 
data$cogn_global<-scale(data$cogn_global)
data$cog_decline<-scale(data$cog_decline)
data$wellbeing<-scale(data$wellbeing)
data$satisfaction<-scale(data$satisfaction)
data$purpose_total<-scale(data$purpose_total)
data$wellbeing_sanspurpose<-scale(data$wellbeing_sanspurpose)
data$age_death<-scale(data$age_death)
data$early_hh_ses<-scale(data$early_hh_ses)
data$med_con_sum_bl<-scale(data$med_con_sum_bl)
data$late_life_cogact_freq<-scale(data$late_life_cogact_freq)
data$neuroticism_12<-scale(data$neuroticism_12)
data$educ<-scale(data$educ)
data$cesdsum<-scale(data$cesdsum)

#Compute Coginitive Resilience Scores
step1<-lm(cogn_global~amyloid+tangles+ci_num2_gct+ci_num2_mct+
            lb_neo+tdp_st4+caa_4gp+cvda_4gp2+arteriol_scler+hip_scl_3reg_yn+
            time_lag_cog_death,data=data)
summary(step1) 
data<-add_residuals(data, step1, var = "resid")
data$resid<-scale(data$resid)
hist(data$resid)
summary(data$resid) 

#Compute Alternative Operationalization Coginitive Resilience Scores
step1b<-lm(cog_decline~amyloid+tangles+ci_num2_gct+ci_num2_mct+
             lb_neo+tdp_st4+caa_4gp+cvda_4gp2+arteriol_scler+hip_scl_3reg_yn+
             time_lag_cog_death,data=data)
summary(step1b) 
data<-add_residuals(data, step1b, var = "resid2")
data$resid2<-scale(data$resid2)
hist(data$resid2)
summary(data$resid2) 

#Associations between our primary and alternative operationalization of cognitive resilience were correlated .76 with one another. 
cor.test(data$resid,data$resid2) #.76

#Table 1 Correlations 
cor.test(data$wellbeing,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$satisfaction,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$age_death,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$msex,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$early_hh_ses,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$educ,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$late_life_cogact_freq,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$neuroticism_12,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$cesdsum,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$apoe,data$cogn_global,use="pairwise.complete.obs") 
cor.test(data$med_con_sum_bl,data$cogn_global,use="pairwise.complete.obs")

cor.test(data$wellbeing,data$resid,use="pairwise.complete.obs") 
cor.test(data$satisfaction,data$resid,use="pairwise.complete.obs") 
cor.test(data$age_death,data$resid,use="pairwise.complete.obs") 
cor.test(data$msex,data$resid,use="pairwise.complete.obs") 
cor.test(data$early_hh_ses,data$resid,use="pairwise.complete.obs") 
cor.test(data$educ,data$resid,use="pairwise.complete.obs") 
cor.test(data$late_life_cogact_freq,data$resid,use="pairwise.complete.obs") 
cor.test(data$neuroticism_12,data$resid,use="pairwise.complete.obs") 
cor.test(data$cesdsum,data$resid,use="pairwise.complete.obs") 
cor.test(data$apoe,data$resid,use="pairwise.complete.obs") 
cor.test(data$med_con_sum_bl,data$resid,use="pairwise.complete.obs") 

#Tables 3 and 4 Primary Analyses and Sensitivity Analyses 
mod1a<-lm(resid~wellbeing,data=data)
mod1b<-lm(resid~satisfaction,data=data)
mod2a<-lm(resid~wellbeing+age_death+msex+early_hh_ses+educ,data=data)
mod2b<-lm(resid~satisfaction+age_death+msex+early_hh_ses+educ,data=data)
mod3a<-lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data)
mod3b<-lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data)
mod4a<-lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data)
mod4b<-lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data)
mod5a<-lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data)
mod5b<-lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data)

#Table 3
summary(mod1a)
confint(mod1a)
summary(mod2a) 
confint(mod2a) 
summary(mod3a) 
confint(mod3a) 
summary(mod4a) 
confint(mod4a)
summary(mod5a) 
confint(mod5a)

#Table 4
summary(mod1b)
confint(mod1b)
summary(mod2b) 
confint(mod2b) 
summary(mod3b) 
confint(mod3b) 
summary(mod4b) 
confint(mod4b)
summary(mod5b) 
confint(mod5b)

#When including eudaimonic and hedonic well-being in the same model , a moderate association between eudaimonic well-being and cognitive resilience remained, B = 0.22, 95% CI = [0.11, 0.34], whereas the 95% CI around the association of hedonic well-being with cognitive resilience included zero, B = 0.04, 95% CI = [-0.07, 0.15].
exp<-lm(resid~satisfaction+wellbeing,data=data)
summary(exp)
confint(exp)

#Exploratory Analyses with Sense of Purpose 
purpose.mod1<-lm(resid~purpose_total,data=data)
purpose.mod2<-lm(resid~purpose_total+age_death+msex+early_hh_ses+educ,data=data)
purpose.mod3<-lm(resid~purpose_total+age_death+msex+early_hh_ses+educ+med_con_sum_bl+late_life_cogact_freq+apoe,data=data)
purpose.mod4<-lm(resid~purpose_total+age_death+msex+early_hh_ses+educ+med_con_sum_bl+late_life_cogact_freq+apoe+neuroticism_12,data=data)
purpose.mod5<-lm(resid~purpose_total+age_death+msex+early_hh_ses+educ+med_con_sum_bl+late_life_cogact_freq+apoe+neuroticism_12+cesdsum,data=data)
summary(purpose.mod1)
confint(purpose.mod1)
summary(purpose.mod2)
summary(purpose.mod3)
summary(purpose.mod4)
summary(purpose.mod5)
confint(purpose.mod5)
#Moreover, when including both eudaimonic well-being (minus the sense of purpose subscale) and sense of purpose in the same fully-adjusted model, a moderate association between eudaimonic well-being and cognitive resilience remained, B = 0.17, 95% CI = [0.04, 0.30], whereas the 95% CI around the association of sense of purpose with cognitive resilience included zero, B = 0.06, 95% CI = [-0.07, 0.19].
exp<-lm(resid~wellbeing_sanspurpose+purpose_total+age_death+msex+early_hh_ses+educ+med_con_sum_bl+late_life_cogact_freq+apoe+neuroticism_12+cesdsum,data=data)
summary(exp)
confint(exp)

#Tables S1 and S2 Exploratory Analyses using Alternative Operationalization of Cognitive Resilience 
mod1a_exp<-lm(resid2~wellbeing,data=data)
mod1b_exp<-lm(resid2~satisfaction,data=data)
mod2a_exp<-lm(resid2~wellbeing+age_death+msex+early_hh_ses+educ,data=data)
mod2b_exp<-lm(resid2~satisfaction+age_death+msex+early_hh_ses+educ,data=data)
mod3a_exp<-lm(resid2~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data)
mod3b_exp<-lm(resid2~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data)
mod4a_exp<-lm(resid2~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data)
mod4b_exp<-lm(resid2~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data)
mod5a_exp<-lm(resid2~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data)
mod5b_exp<-lm(resid2~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data)

#Table S1
summary(mod1a_exp)
confint(mod1a_exp)
summary(mod2a_exp) 
confint(mod2a_exp) 
summary(mod3a_exp) 
confint(mod3a_exp) 
summary(mod4a_exp) 
confint(mod4a_exp)
summary(mod5a_exp) 
confint(mod5a_exp)

#Table S2
summary(mod1b_exp)
confint(mod1b_exp)
summary(mod2b_exp) 
confint(mod2b_exp) 
summary(mod3b_exp) 
confint(mod3b_exp) 
summary(mod4b_exp) 
confint(mod4b_exp)
summary(mod5b_exp) 
confint(mod5b_exp)

#Footnote 1
#When excluding N=27 participants whose final cognitive assessment was concurrent with their only well-being assessment, results of primary and sensitivity analyses remained the same, with the following exception: the 95% CI around the unadjusted association between hedonic well-being and cognitive resilience included zero, B = 0.11, 95% CI = [-0.005, 0.23].
data_footnote1<-subset(data,data$wb_age_at_visit<data$cog_age_at_visit)
nrow(data_footnote1)
summary(lm(resid~wellbeing,data=data_footnote1))
summary(lm(resid~satisfaction,data=data_footnote1))
summary(lm(resid~wellbeing+age_death+msex+early_hh_ses+educ,data=data_footnote1))
summary(lm(resid~satisfaction+age_death+msex+early_hh_ses+educ,data=data_footnote1))
summary(lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data_footnote1))
summary(lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl,data=data_footnote1))
summary(lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data_footnote1))
summary(lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12,data=data_footnote1))
summary(lm(resid~wellbeing+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data_footnote1))
summary(lm(resid~satisfaction+age_death+msex+early_hh_ses+educ+late_life_cogact_freq+apoe+med_con_sum_bl+neuroticism_12+cesdsum,data=data_footnote1))

#Footnote 2: Results of primary analyses remained the same when adjusting for time between well-being assessment and death and the relationships between well-being and cognitive resilience were not moderated by time between well-being assessment and death.  
summary(lm(resid~wellbeing+time_lag_wb_death,data=data))
summary(lm(resid~wellbeing*time_lag_wb_death,data=data))
summary(lm(resid~satisfaction+time_lag_wb_death,data=data))
summary(lm(resid~satisfaction*time_lag_wb_death,data=data))

#Footnote 3: Results of primary analayses remained the same when adjusting for time between well-being assessment and cognition assessment and the relationships between well-being and cognitive resilience were not moderated by time between well-being assessment and cognitive assessment.  
summary(lm(resid~wellbeing+time_lag_wb_cog,data=data))
summary(lm(resid~wellbeing*time_lag_wb_cog,data=data))
summary(lm(resid~satisfaction+time_lag_wb_cog,data=data))
summary(lm(resid~satisfaction*time_lag_wb_cog,data=data))

#Footnote 4: Results of primary analyses remained the same when adjusting for time between final cognitive assessment and death and the relationships between well-being and cognitive resilience were not moderated by time between final cognitive assessment and death.
summary(lm(resid~wellbeing+time_lag_cog_death,data=data))
summary(lm(resid~wellbeing*time_lag_cog_death,data=data))
summary(lm(resid~satisfaction+time_lag_cog_death,data=data))
summary(lm(resid~satisfaction*time_lag_cog_death,data=data))

#Figure 3 
library(effects)

cleanup <- theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 text=element_text(size=24,  family="Tahoma"),
                 axis.line = element_line(color = "black"),
                 axis.title.x = element_text(colour="black",
                                             size=24,
                                             hjust=.5,vjust=.5,face="plain"),
                 axis.text.x = element_text(colour="black",
                                            size=15,
                                            hjust=.5,vjust=.5,face="plain"),
                 axis.title.y = element_text(colour="black",
                                             size=24,
                                             hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(colour="black",
                                            size=15,
                                            hjust=.5,vjust=.5,face="plain"),
                 aspect.ratio = 1,legend.key.size = unit(.5, 'cm'),
                 legend.text = element_text(size=13, colour="black"),legend.key.height = unit(1, 'cm'), #change legend key height
                 legend.key.width = unit(1, 'cm'), #change legend key width
                 legend.title = element_text(size=14))

(eudaimonic_predtable1 <- effect(term = "wellbeing", mod = mod1a) %>% as_tibble())
(eudaimonic_predtable2 <- effect(term = "wellbeing", mod = mod5a) %>% as_tibble())
(hedonic_predtable1 <- effect(term = "satisfaction", mod = mod1b) %>% as_tibble())
(hedonic_predtable2 <- effect(term = "satisfaction", mod = mod5b) %>% as_tibble())

panel1a<-data %>% 
  ggplot(mapping = aes(x = wellbeing, y = resid)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = eudaimonic_predtable1, mapping = aes(x = wellbeing, y = fit), color = "black") +
  geom_ribbon(data = eudaimonic_predtable1, mapping = aes(x = wellbeing, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Eudaimonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel2a<-data %>% 
  ggplot(mapping = aes(x = wellbeing, y = resid)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = eudaimonic_predtable2, mapping = aes(x = wellbeing, y = fit), color = "black") +
  geom_ribbon(data = eudaimonic_predtable2, mapping = aes(x = wellbeing, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Eudaimonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel1b<-data %>% 
  ggplot(mapping = aes(x = satisfaction, y = resid)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = hedonic_predtable1, mapping = aes(x = satisfaction, y = fit), color = "black") +
  geom_ribbon(data = hedonic_predtable1, mapping = aes(x = satisfaction, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Hedonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel2b<-data %>% 
  ggplot(mapping = aes(x = satisfaction, y = resid)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = hedonic_predtable2, mapping = aes(x = satisfaction, y = fit), color = "black") +
  geom_ribbon(data = hedonic_predtable2, mapping = aes(x = satisfaction, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Hedonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

grid.arrange(panel1a, panel1b, panel2a, panel2b, ncol=2)

#Figure S1
(eudaimonic_exp_predtable1 <- effect(term = "wellbeing", mod = mod1a_exp) %>% as_tibble())
(eudaimonic_exp_predtable2 <- effect(term = "wellbeing", mod = mod5a_exp) %>% as_tibble())
(hedonic_exp_predtable1 <- effect(term = "satisfaction", mod = mod1b_exp) %>% as_tibble())
(hedonic_exp_predtable2 <- effect(term = "satisfaction", mod = mod5b_exp) %>% as_tibble())

panel1a<-data %>% 
  ggplot(mapping = aes(x = wellbeing, y = resid2)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = eudaimonic_exp_predtable1, mapping = aes(x = wellbeing, y = fit), color = "black") +
  geom_ribbon(data = eudaimonic_exp_predtable1, mapping = aes(x = wellbeing, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Eudaimonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel2a<-data %>% 
  ggplot(mapping = aes(x = wellbeing, y = resid2)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = eudaimonic_exp_predtable2, mapping = aes(x = wellbeing, y = fit), color = "black") +
  geom_ribbon(data = eudaimonic_exp_predtable2, mapping = aes(x = wellbeing, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Eudaimonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel1b<-data %>% 
  ggplot(mapping = aes(x = satisfaction, y = resid2)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = hedonic_exp_predtable1, mapping = aes(x = satisfaction, y = fit), color = "black") +
  geom_ribbon(data = hedonic_exp_predtable1, mapping = aes(x = satisfaction, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Hedonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

panel2b<-data %>% 
  ggplot(mapping = aes(x = satisfaction, y = resid2)) +
  geom_point(alpha = 0.5, color = "grey") +
  geom_line(data = hedonic_exp_predtable2, mapping = aes(x = satisfaction, y = fit), color = "black") +
  geom_ribbon(data = hedonic_exp_predtable2, mapping = aes(x = satisfaction, y = fit, ymin = lower, ymax = upper), fill = "grey", alpha = 0.25) +
  labs(x = "Hedonic Wellbeing", y = "Cognitive Resilience")+
  cleanup

grid.arrange(panel1a, panel1b, panel2a, panel2b, ncol=2)

#Specifically, the final analytic sample size of 348 provided 80% statistical power to detect small associations between well-being and cognitive resilience of .15 or larger, given our alpha level of .05.
library(pwr)
pwr.r.test(n=348,power=.80,sig.level=.05)
