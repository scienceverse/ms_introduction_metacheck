
##########################################################################
### Yeager, D.S., Lee, H.Y., Jamieson, J.P. (2016) Psychological Science 
### Study 2 analysis syntax
##########################################################################

long <- read.csv("~/Desktop/psychscience_study2 data_final.csv", header=T, sep=",")


#############################################
### Treatment effects on academic grades
#############################################

# Descriptive statistics

round(tapply(long$gpa.avg.sem1, long$mindset_condition, mean, na.rm=T), 2)
round(tapply(long$gpa.avg.sem1, long$mindset_condition, sd, na.rm=T), 2)

round(tapply(long$gpa.avg.sem2, long$mindset_condition, mean, na.rm=T), 2)
round(tapply(long$gpa.avg.sem2, long$mindset_condition, sd, na.rm=T), 2)


# GPA semester 1
summary(lm(gpa.avg.sem1 ~ mindset_condition + baseline.grades.scaled + sex + as.factor(ap_eng) + as.factor(ap_science) + as.factor(ap_social), data=long, subset=day==0)) 

# GPA semester 2
summary(lm(gpa.avg.sem2 ~ mindset_condition + baseline.grades.scaled + sex + as.factor(ap_eng) + as.factor(ap_science) + as.factor(ap_social), data=long, subset=day==0))


################################################
### Main effects of treatment on daily threat appraisal
################################################

library(lme4)
library(Matrix)
library(lmerTest)
library(car)

# Descriptive stats

round(tapply(long$daily.threat[long$day==5], long$mindset_condition[long$day==5], mean, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==6], long$mindset_condition[long$day==6], mean, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==7], long$mindset_condition[long$day==7], mean, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==8], long$mindset_condition[long$day==8], mean, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==9], long$mindset_condition[long$day==9], mean, na.rm=T), 2)

round(tapply(long$daily.threat[long$day==5], long$mindset_condition[long$day==5], sd, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==6], long$mindset_condition[long$day==6], sd, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==7], long$mindset_condition[long$day==7], sd, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==8], long$mindset_condition[long$day==8], sd, na.rm=T), 2)
round(tapply(long$daily.threat[long$day==9], long$mindset_condition[long$day==9], sd, na.rm=T), 2)


# lm models 

summary(lm(daily.threat ~ mindset_condition+daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==5, na.action=na.exclude))

summary(lm(daily.threat ~ mindset_condition+daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==6, na.action=na.exclude))

summary(lm(daily.threat ~ mindset_condition+daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==7, na.action=na.exclude))

summary(lm(daily.threat ~ mindset_condition+daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==8, na.action=na.exclude))

summary(lm(daily.threat ~ mindset_condition+daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==9, na.action=na.exclude))


# lmer models 

summary(lmer(daily.threat ~ mindset_condition + daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily.threat ~ mindset_condition + daily.threat_base.dumm + daily.threat_base.miss+pss.sum_baseline.dumm+ pss.sum_baseline.miss+as.factor(sex)+as.factor(time)+gp1.math+(1|rid), data=long, subset=day%in%c(8,9)))


#############################################
### Main effects of treatment on cortisol
#############################################

# Descrpitive stats

round(tapply(long$daily_cortisol[long$day==5], long$mindset_condition[long$day==5], mean, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==6], long$mindset_condition[long$day==6], mean, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==7], long$mindset_condition[long$day==7], mean, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==8], long$mindset_condition[long$day==8], mean, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==9], long$mindset_condition[long$day==9], mean, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day%in%c(8,9)], long$mindset_condition[long$day%in%c(8,9)], mean, na.rm=T), 2)

round(tapply(long$daily_cortisol[long$day==5], long$mindset_condition[long$day==5], sd, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==6], long$mindset_condition[long$day==6], sd, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==7], long$mindset_condition[long$day==7], sd, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==8], long$mindset_condition[long$day==8], sd, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day==9], long$mindset_condition[long$day==9], sd, na.rm=T), 2)
round(tapply(long$daily_cortisol[long$day%in%c(8,9)], long$mindset_condition[long$day%in%c(8,9)], sd, na.rm=T), 2)


# lm models

summary(lm(daily_cortisol ~ mindset_condition + logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==5, na.action=na.exclude))

summary(lm(daily_cortisol ~ mindset_condition + logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==6, na.action=na.exclude))

summary(lm(daily_cortisol ~ mindset_condition + logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==7, na.action=na.exclude))

summary(lm(daily_cortisol ~ mindset_condition + logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==8, na.action=na.exclude))

summary(lm(daily_cortisol ~ mindset_condition + logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==9, na.action=na.exclude))


# lmer models

summary(lmer(daily_cortisol ~ mindset_condition + logcortbase_dumm + logcortbase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily_cortisol ~ mindset_condition + logcortbase_dumm + logcortbase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(8,9)))


#############################################
### Main effects of treatment on DHEA-S
#############################################

# Correlations between Cortisol & DHEA-S 

cor.test(long$daily_cortisol, long$daily_dheas, use="complete.obs")

# Descriptive stats

round(tapply(long$daily_dheas[long$day==5], long$mindset_condition[long$day==5], mean, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==6], long$mindset_condition[long$day==6], mean, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==7], long$mindset_condition[long$day==7], mean, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==8], long$mindset_condition[long$day==8], mean, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==9], long$mindset_condition[long$day==9], mean, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day%in%c(8,9)], long$mindset_condition[long$day%in%c(8,9)], mean, na.rm=T), 2)

round(tapply(long$daily_dheas[long$day==5], long$mindset_condition[long$day==5], sd, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==6], long$mindset_condition[long$day==6], sd, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==7], long$mindset_condition[long$day==7], sd, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==8], long$mindset_condition[long$day==8], sd, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day==9], long$mindset_condition[long$day==9], sd, na.rm=T), 2)
round(tapply(long$daily_dheas[long$day%in%c(8,9)], long$mindset_condition[long$day%in%c(8,9)], sd, na.rm=T), 2)

# lm models

summary(lm(daily_dheas ~ mindset_condition + logdheabase_dumm +logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==5, na.action=na.exclude))

summary(lm(daily_dheas ~ mindset_condition + logdheabase_dumm +logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==6, na.action=na.exclude))

summary(lm(daily_dheas ~ mindset_condition + logdheabase_dumm +logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==7, na.action=na.exclude))

summary(lm(daily_dheas ~ mindset_condition + logdheabase_dumm +logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==8, na.action=na.exclude))

summary(lm(daily_dheas ~ mindset_condition + logdheabase_dumm +logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==9, na.action=na.exclude))

# lmer models

summary(lmer(daily_dheas ~ mindset_condition + logdheabase_dumm + logdheabase_miss + as.factor(time)+gp1.math+as.factor(day)+sex+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily_dheas ~ mindset_condition + logdheabase_dumm + logdheabase_miss + as.factor(time)+gp1.math+as.factor(day)+sex+(1|rid), data=long, subset=day%in%c(8,9)))


########################################################
### Daily Social Stress x Treatment interaction models
########################################################

# Daily threat appraisal 

summary(lmer(daily.threat ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + daily.threat_base.dumm + daily.threat_base.miss + as.factor(time)+ pss.sum_baseline.dumm + pss.sum_baseline.miss+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily.threat ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + daily.threat_base.dumm + daily.threat_base.miss + as.factor(time)+ pss.sum_baseline.dumm + pss.sum_baseline.miss+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(8,9)))

# Daily cortisol 

summary(lmer(daily_cortisol ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + logcortbase_dumm + logcortbase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily_cortisol ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + logcortbase_dumm + logcortbase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(8,9)))


# Daily DHEA-S

summary(lmer(daily_dheas ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + logdheabase_dumm + logdheabase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))

summary(lmer(daily_dheas ~ mindset_condition*daily.neg.socialacademic + mindset_condition*daily.neg.baseline + logdheabase_dumm + logdheabase_miss + as.factor(time)+as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(8,9)))



#######################################################################
### PLOT 1: Daily stressors x Treatment >> Threat Secondary Appraisal 
#######################################################################

# scaling 

long$mindset_condition_reversed = ifelse(long$mindset_condition==1, 0, ifelse(long$mindset_condition==0, 1, NA))
                                                        
long$z.sex <- scale(long$sex, scale=T)
long$z.gp1.math <- scale(long$gp1.math, scale=T) 

long$z.daily.threat_base.dumm = scale(long$daily.threat_base.dumm, scale=T)
long$z.daily.threat_base.miss = scale(long$daily.threat_base.miss, scale=T)

long$z.pss.sum_baseline.dumm = scale(long$pss.sum_baseline.dumm, scale=F)
long$z.pss.sum_baseline.miss = scale(long$pss.sum_baseline.miss, scale=F)

long$period.dumm.1 <- ifelse(long$time==1, 1, 0)
long$period.dumm.2 <- ifelse(long$time==2, 1, 0)
long$period.dumm.3 <- ifelse(long$time==3, 1, 0)
long$period.dumm.4 <- ifelse(long$time==4, 1, 0)
long$period.dumm.5 <- ifelse(long$time==5, 1, 0)
long$period.dumm.6 <- ifelse(long$time==6, 1, 0)
long$period.dumm.7 <- ifelse(long$time==7, 1, 0)

long$z.period.dumm.1 <- scale(long$period.dumm.1, scale=T)
long$z.period.dumm.2 <- scale(long$period.dumm.2, scale=T)
long$z.period.dumm.3 <- scale(long$period.dumm.3, scale=T)
long$z.period.dumm.4 <- scale(long$period.dumm.4, scale=T)
long$z.period.dumm.5 <- scale(long$period.dumm.5, scale=T)
long$z.period.dumm.6 <- scale(long$period.dumm.6, scale=T)
long$z.period.dumm.7 <- scale(long$period.dumm.7, scale=T)

long$day.dumm.0 <- ifelse(long$day==0, 1, 0)
long$day.dumm.5 <- ifelse(long$day==5, 1, 0)
long$day.dumm.6 <- ifelse(long$day==6, 1, 0)
long$day.dumm.7 <- ifelse(long$day==7, 1, 0)
long$day.dumm.8 <- ifelse(long$day==5, 1, 0)
long$day.dumm.9 <- ifelse(long$day==6, 1, 0)

long$z.day.dumm.0 <- scale(long$day.dumm.0, scale=T)
long$z.day.dumm.5 <- scale(long$day.dumm.5, scale=T)
long$z.day.dumm.6 <- scale(long$day.dumm.6, scale=T)
long$z.day.dumm.7 <- scale(long$day.dumm.7, scale=T)
long$z.day.dumm.8 <- scale(long$day.dumm.8, scale=T)
long$z.day.dumm.9 <- scale(long$day.dumm.9, scale=T)

long$z.daily.neg.socialacademic = scale(long$daily.neg.socialacademic, scale=T)

long$z.daily.neg.socialacademic_plus1sd  = long$z.daily.neg.socialacademic - mean(long$z.daily.neg.socialacademic, na.rm=T) - sd(long$z.daily.neg.socialacademic, na.rm=T) #high stress

long$z.daily.neg.socialacademic_minus1sd = long$z.daily.neg.socialacademic - mean(long$z.daily.neg.socialacademic, na.rm=T) + sd(long$z.daily.neg.socialacademic, na.rm=T) #low stress

long$z.daily.neg.baseline <- scale(long$daily.neg.baseline, scale=T)

long$z.logcortbase_dumm  <- scale(long$logcortbase_dumm, scale=T)
long$z.logcortbase_miss  <- scale(long$logcortbase_miss, scale=T)

long$z.logdheabase_dumm  <- scale(long$logdheabase_dumm, scale=T)
long$z.logdheabase_miss  <- scale(long$logdheabase_miss, scale=T)


# lm 

control_minus1sd_day0 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd +z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_plus1sd_day0 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd +z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_minus1sd_day5 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(5))

control_plus1sd_day5 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(5))

control_minus1sd_day6 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(6))

control_plus1sd_day6 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(6))

control_minus1sd_day7 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(7))

control_plus1sd_day7 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(7))

control_minus1sd_day8 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(8))

control_plus1sd_day8 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(8))

control_minus1sd_day9 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(9))

control_plus1sd_day9 <- lm(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline++z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(9))


treatment_minus1sd_day0 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd +z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_plus1sd_day0 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd +z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_minus1sd_day5 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(5))

treatment_plus1sd_day5 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(5))

treatment_minus1sd_day6 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(6))

treatment_plus1sd_day6 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(6))

treatment_minus1sd_day7 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(7))

treatment_plus1sd_day7 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(7))

treatment_minus1sd_day8 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(8))

treatment_plus1sd_day8 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(8))

treatment_minus1sd_day9 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(9))

treatment_plus1sd_day9 <- lm(daily.threat ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(9))


### Plot for daily threat appraisal day0~9

control_mean = rbind(coef(summary(control_minus1sd_day0))[1,1], 
                     coef(summary(control_plus1sd_day0))[1,1], NA,
                     coef(summary(control_minus1sd_day5))[1,1], 
                     coef(summary(control_plus1sd_day5))[1,1], NA,
                     coef(summary(control_minus1sd_day6))[1,1], 
                     coef(summary(control_plus1sd_day6))[1,1], NA,
                     coef(summary(control_minus1sd_day7))[1,1], 
                     coef(summary(control_plus1sd_day7))[1,1], NA,
                     coef(summary(control_minus1sd_day8))[1,1], 
                     coef(summary(control_plus1sd_day8))[1,1], NA,
                     coef(summary(control_minus1sd_day9))[1,1], 
                     coef(summary(control_plus1sd_day9))[1,1])

treatment_mean = rbind(coef(summary(treatment_minus1sd_day0))[1,1], 
                       coef(summary(treatment_plus1sd_day0))[1,1], NA,
                       coef(summary(treatment_minus1sd_day5))[1,1], 
                       coef(summary(treatment_plus1sd_day5))[1,1], NA,
                       coef(summary(treatment_minus1sd_day6))[1,1], 
                       coef(summary(treatment_plus1sd_day6))[1,1], NA,
                       coef(summary(treatment_minus1sd_day7))[1,1], 
                       coef(summary(treatment_plus1sd_day7))[1,1], NA,
                       coef(summary(treatment_minus1sd_day8))[1,1], 
                       coef(summary(treatment_plus1sd_day8))[1,1], NA,
                       coef(summary(treatment_minus1sd_day9))[1,1], 
                       coef(summary(treatment_plus1sd_day9))[1,1])

control_se = rbind(coef(summary(control_minus1sd_day0))[1,2], 
                   coef(summary(control_plus1sd_day0))[1,2], NA,
                   coef(summary(control_minus1sd_day5))[1,2], 
                   coef(summary(control_plus1sd_day5))[1,2], NA, 
                   coef(summary(control_minus1sd_day6))[1,2], 
                   coef(summary(control_plus1sd_day6))[1,2], NA,
                   coef(summary(control_minus1sd_day7))[1,2], 
                   coef(summary(control_plus1sd_day7))[1,2], NA,
                   coef(summary(control_minus1sd_day8))[1,2], 
                   coef(summary(control_plus1sd_day8))[1,2], NA,
                   coef(summary(control_minus1sd_day9))[1,2], 
                   coef(summary(control_plus1sd_day9))[1,2])

treatment_se = rbind(coef(summary(treatment_minus1sd_day0))[1,2], 
                     coef(summary(treatment_plus1sd_day0))[1,2], NA,
                     coef(summary(treatment_minus1sd_day5))[1,2], 
                     coef(summary(treatment_plus1sd_day5))[1,2], NA,
                     coef(summary(treatment_minus1sd_day6))[1,2], 
                     coef(summary(treatment_plus1sd_day6))[1,2], NA,
                     coef(summary(treatment_minus1sd_day7))[1,2], 
                     coef(summary(treatment_plus1sd_day7))[1,2], NA,
                     coef(summary(treatment_minus1sd_day8))[1,2], 
                     coef(summary(treatment_plus1sd_day8))[1,2], NA,
                     coef(summary(treatment_minus1sd_day9))[1,2], 
                     coef(summary(treatment_plus1sd_day9))[1,2])

control_ci.up = control_mean + control_se
control_ci.down = control_mean - control_se

treatment_ci.up = treatment_mean + treatment_se
treatment_ci.down = treatment_mean - treatment_se


## paste p-values 

me.threat.day0 = lm(daily.threat ~ mindset_condition+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==0)
me.threat.day5 = lm(daily.threat ~ mindset_condition+daily.threat_base.dumm+daily.threat_base.miss+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==5)
me.threat.day6 = lm(daily.threat ~ mindset_condition+daily.threat_base.dumm+daily.threat_base.miss+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==6)
me.threat.day7 = lm(daily.threat ~ mindset_condition+daily.threat_base.dumm+daily.threat_base.miss+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==7)
me.threat.day8 = lm(daily.threat ~ mindset_condition+daily.threat_base.dumm+daily.threat_base.miss+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==8)
me.threat.day9 = lm(daily.threat ~ mindset_condition+daily.threat_base.dumm+daily.threat_base.miss+pss.sum_baseline.dumm+pss.sum_baseline.miss+as.factor(time)+sex+gp1.math, data=long, subset=long$day==9)

dsxt.threat.day0 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic +z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+ z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==0)
dsxt.threat.day5 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline +z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==5)
dsxt.threat.day6 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss+ z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==6)
dsxt.threat.day7 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==7)
dsxt.threat.day8 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==8)
dsxt.threat.day9 = lm(daily.threat~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline+z.daily.threat_base.dumm+z.daily.threat_base.miss+z.pss.sum_baseline.dumm+z.pss.sum_baseline.miss + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex +z.gp1.math, data=long, subset=long$day==9)


#install.packages("weights") #this package "rd" function removes leading zeros 
library("weights") 

me.threat.ps <- rbind(rd(coef(summary(me.threat.day0))[2,4], digits=3),  
                      rd(coef(summary(me.threat.day5))[2,4], digits=3), 
                      rd(coef(summary(me.threat.day6))[2,4], digits=3),
                      rd(coef(summary(me.threat.day7))[2,4], digits=3),
                      rd(coef(summary(me.threat.day8))[2,4], digits=3),
                      rd(coef(summary(me.threat.day9))[2,4], digits=3))

dsxt.threat.ps <- rbind(rd(coef(summary(dsxt.threat.day0))[14,4], digits=3),
                        rd(coef(summary(dsxt.threat.day5))[17,4], digits=3), 
                        rd(coef(summary(dsxt.threat.day6))[17,4], digits=3), 
                        rd(coef(summary(dsxt.threat.day7))[17,4], digits=3), 
                        rd(coef(summary(dsxt.threat.day8))[17,4], digits=3), 
                        rd(coef(summary(dsxt.threat.day9))[17,4], digits=3))

# paste p-values for simple effects

simple.threat.ps <- rbind(rd(coef(summary(control_plus1sd_day0))[2,4], digits=3),
                          rd(coef(summary(control_plus1sd_day5))[2,4], digits=3), 
                          rd(coef(summary(control_plus1sd_day6))[2,4], digits=3), 
                          rd(coef(summary(control_plus1sd_day7))[2,4], digits=3), 
                          rd(coef(summary(control_plus1sd_day8))[2,4], digits=3), 
                          rd(coef(summary(control_plus1sd_day9))[2,4], digits=3))

# Plot with MEANs & SEs estimates

pdf("PLOT_FINAL_threat appraisal_day5-9.pdf", height=5, width=13)
plot(control_mean, ylab="Daily Threat Secondary Appraisal", xlab=NA, type="b", col="black", ylim=c(1, 6), xaxt="n", lwd=3, pch=17, font.lab=2, main="Daily Stress x Condition Interaction Predicting Post-Intervention Daily Threat Appraisal")
lines(treatment_mean, type="b", col="black", lty=2, lwd=3, pch=15)
axis(1, at=(1:17), padj=0.4, font=0.8, labels=c("Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","","Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress"))
arrows(1, control_ci.up[1], 1, control_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, control_ci.up[2], 2, control_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, control_ci.up[4], 4, control_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, control_ci.up[5], 5, control_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, control_ci.up[7], 7, control_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, control_ci.up[8], 8, control_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, control_ci.up[10], 10, control_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, control_ci.up[11], 11, control_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, control_ci.up[13], 13, control_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, control_ci.up[14], 14, control_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, control_ci.up[16], 16, control_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, control_ci.up[17], 17, control_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(1, treatment_ci.up[1], 1, treatment_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, treatment_ci.up[2], 2, treatment_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, treatment_ci.up[4], 4, treatment_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, treatment_ci.up[5], 5, treatment_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, treatment_ci.up[7], 7, treatment_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, treatment_ci.up[8], 8, treatment_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, treatment_ci.up[10], 10, treatment_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, treatment_ci.up[11], 11, treatment_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, treatment_ci.up[13], 13, treatment_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, treatment_ci.up[14], 14, treatment_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, treatment_ci.up[16], 16, treatment_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, treatment_ci.up[17], 17, treatment_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
legend("bottomleft", c( "Control", "Incremental Theory of Personality"),  lty=c(1, 2), pch=c(17,15), col=c("black", "black"), lwd=2, inset=.02)
mtext("Baseline", side=1, line=3, at= 1.5, cex=1,  col="black", font=2)  
mtext("Day 5", side=1, line=3, at= 4.5, cex=1,  col="black", font=2) 
mtext("Day 6", side=1, line=3, at= 7.5, cex=1,  col="black", font=2)  
mtext("Day 7", side=1, line=3, at= 10.5, cex=1,  col="black", font=2) 
mtext("Day 8", side=1, line=3, at= 13.5, cex=1,  col="black", font=2)
mtext("Day 9", side=1, line=3, at= 16.5, cex=1,  col="black", font=2)  
text(1.5, 5.5,   labels=paste("Main  p= ", me.threat.ps[1], "\n Int  p= ", dsxt.threat.ps[1]), cex=.9)
text(3+1.5, 5.5, labels=paste("Main  p= ", me.threat.ps[2], "\n Int  p= ", dsxt.threat.ps[2]), cex=.9)
text(6+1.5, 5.5, labels=paste("Main  p= ", me.threat.ps[3], "\n Int  p= ", dsxt.threat.ps[3]), cex=.9)
text(9+1.5, 5.5, labels=paste("Main  p= ", me.threat.ps[4], "\n Int  p= ", dsxt.threat.ps[4]), cex=.9)
text(12+1.5, 5.5, labels=paste("Main  p= ", me.threat.ps[5], "\n Int  p= ", dsxt.threat.ps[5]), cex=.9)
text(15+1.5, 5.5, labels=paste("Main  p= ", me.threat.ps[6], "\n Int  p= ", dsxt.threat.ps[6]), cex=.9)
text(2,   treatment_ci.down[2]-0.5, labels=paste("p=", simple.threat.ps[1]), cex=.9)
text(3+2, treatment_ci.down[5]-0.5, labels=paste("p=", simple.threat.ps[2]), cex=.9)
text(6+2, treatment_ci.down[8]-0.5, labels=paste("p=", simple.threat.ps[3]), cex=.9)
text(9+2, treatment_ci.down[11]-0.5, labels=paste("p=", simple.threat.ps[4]), cex=.9)
text(12+2,treatment_ci.down[14]-0.5, labels=paste("p=", simple.threat.ps[5]), cex=.9)
text(15+2,treatment_ci.down[17]-0.5, labels=paste("p=", simple.threat.ps[6]), cex=.9)
dev.off()



##########################################################
### PLOT 2: Daily stressors x Treatment >> Daily Cortisol
##########################################################

## lm

control_minus1sd_day0 <- lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_plus1sd_day0  <- lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_minus1sd_day5 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5)) 

control_plus1sd_day5 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5))  

control_minus1sd_day6 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

control_plus1sd_day6 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

control_minus1sd_day7 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7)) 

control_plus1sd_day7 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7)) 

control_minus1sd_day8 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8)) 

control_plus1sd_day8 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

control_minus1sd_day9 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  

control_plus1sd_day9 = lm(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))
  


treatment_minus1sd_day0 <- lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_plus1sd_day0 <- lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_minus1sd_day5 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5)) 

treatment_plus1sd_day5 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5))  

treatment_minus1sd_day6 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

treatment_plus1sd_day6 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6))  

treatment_minus1sd_day7 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7))  

treatment_plus1sd_day7 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7))  

treatment_minus1sd_day8 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

treatment_plus1sd_day8 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

treatment_minus1sd_day9 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  

treatment_plus1sd_day9 = lm(daily_cortisol ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))    
 


# plot for cort day 0 ~ 9 

control_mean = rbind(coef(summary(control_minus1sd_day0))[1,1], 
                     coef(summary(control_plus1sd_day0))[1,1], NA,
                     coef(summary(control_minus1sd_day5))[1,1], 
                     coef(summary(control_plus1sd_day5))[1,1], NA,
                     coef(summary(control_minus1sd_day6))[1,1], 
                     coef(summary(control_plus1sd_day6))[1,1], NA,
                     coef(summary(control_minus1sd_day7))[1,1], 
                     coef(summary(control_plus1sd_day7))[1,1], NA,
                     coef(summary(control_minus1sd_day8))[1,1], 
                     coef(summary(control_plus1sd_day8))[1,1], NA,
                     coef(summary(control_minus1sd_day9))[1,1], 
                     coef(summary(control_plus1sd_day9))[1,1])

treatment_mean = rbind(coef(summary(treatment_minus1sd_day0))[1,1], 
                       coef(summary(treatment_plus1sd_day0))[1,1], NA,
                       coef(summary(treatment_minus1sd_day5))[1,1], 
                       coef(summary(treatment_plus1sd_day5))[1,1], NA,
                       coef(summary(treatment_minus1sd_day6))[1,1], 
                       coef(summary(treatment_plus1sd_day6))[1,1], NA,
                       coef(summary(treatment_minus1sd_day7))[1,1], 
                       coef(summary(treatment_plus1sd_day7))[1,1], NA,
                       coef(summary(treatment_minus1sd_day8))[1,1], 
                       coef(summary(treatment_plus1sd_day8))[1,1], NA,
                       coef(summary(treatment_minus1sd_day9))[1,1], 
                       coef(summary(treatment_plus1sd_day9))[1,1])

control_se = rbind(coef(summary(control_minus1sd_day0))[1,2], 
                   coef(summary(control_plus1sd_day0))[1,2], NA,
                   coef(summary(control_minus1sd_day5))[1,2], 
                   coef(summary(control_plus1sd_day5))[1,2], NA, 
                   coef(summary(control_minus1sd_day6))[1,2], 
                   coef(summary(control_plus1sd_day6))[1,2], NA,
                   coef(summary(control_minus1sd_day7))[1,2], 
                   coef(summary(control_plus1sd_day7))[1,2], NA,
                   coef(summary(control_minus1sd_day8))[1,2], 
                   coef(summary(control_plus1sd_day8))[1,2], NA,
                   coef(summary(control_minus1sd_day9))[1,2], 
                   coef(summary(control_plus1sd_day9))[1,2])

treatment_se = rbind(coef(summary(treatment_minus1sd_day0))[1,2], 
                     coef(summary(treatment_plus1sd_day0))[1,2], NA,
                     coef(summary(treatment_minus1sd_day5))[1,2], 
                     coef(summary(treatment_plus1sd_day5))[1,2], NA,
                     coef(summary(treatment_minus1sd_day6))[1,2], 
                     coef(summary(treatment_plus1sd_day6))[1,2], NA,
                     coef(summary(treatment_minus1sd_day7))[1,2], 
                     coef(summary(treatment_plus1sd_day7))[1,2], NA,
                     coef(summary(treatment_minus1sd_day8))[1,2], 
                     coef(summary(treatment_plus1sd_day8))[1,2], NA,
                     coef(summary(treatment_minus1sd_day9))[1,2], 
                     coef(summary(treatment_plus1sd_day9))[1,2])

control_ci.up = control_mean + control_se
control_ci.down = control_mean - control_se

treatment_ci.up = treatment_mean + treatment_se
treatment_ci.down = treatment_mean - treatment_se


## paste p-values 

me.cort.day0 = lm(daily_cortisol ~ mindset_condition+ as.factor(sex)+as.factor(time)+gp1.math, data=long, subset=long$day==0)
me.cort.day5 = lm(daily_cortisol ~ mindset_condition+ logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==5)
me.cort.day6 = lm(daily_cortisol ~ mindset_condition+ logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==6)
me.cort.day7 = lm(daily_cortisol ~ mindset_condition+ logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==7)
me.cort.day8 = lm(daily_cortisol ~ mindset_condition+ logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==8)
me.cort.day9 = lm(daily_cortisol ~ mindset_condition+ logcortbase_dumm+logcortbase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==9)

dsxt.cort.day0 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 
                    + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==0)
dsxt.cort.day5 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm 
                    + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==5)
dsxt.cort.day6 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm 
                    + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==6)
dsxt.cort.day7 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm 
                    + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==7)
dsxt.cort.day8 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm 
                    + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==8)
dsxt.cort.day9 = lm(daily_cortisol~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm 
                    + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==9)

me.cort.ps <- rbind(rd(coef(summary(me.cort.day0))[2,4], digits=3),  
                    rd(coef(summary(me.cort.day5))[2,4], digits=3), 
                    rd(coef(summary(me.cort.day6))[2,4], digits=3),
                    rd(coef(summary(me.cort.day7))[2,4], digits=3),
                    rd(coef(summary(me.cort.day8))[2,4], digits=3),
                    rd(coef(summary(me.cort.day9))[2,4], digits=3))

dsxt.cort.ps <- rbind(rd(coef(summary(dsxt.cort.day0))[12,4], digits=3),
                      rd(coef(summary(dsxt.cort.day5))[15,4], digits=3), 
                      rd(coef(summary(dsxt.cort.day6))[15,4], digits=3), 
                      rd(coef(summary(dsxt.cort.day7))[15,4], digits=3), 
                      rd(coef(summary(dsxt.cort.day8))[15,4], digits=3), 
                      rd(coef(summary(dsxt.cort.day9))[15,4], digits=3))

# paste p-values for simple effects

simple.cort.ps <- rbind(rd(coef(summary(control_plus1sd_day0))[2,4], digits=3),
                      rd(coef(summary(control_plus1sd_day5))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day6))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day7))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day8))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day9))[2,4], digits=3))

# Plot with MEANs & SEs

pdf("PLOT_FINAL2_cortisol_day59.pdf", height=5, width=13)
plot(control_mean, ylab="Salivary Cortisol (nmol/l)", xlab=NA, type="b", col="black", ylim=c(0, 25), xaxt="n", lwd=3, pch=17, font.lab=2, 
     main="Daily Stress x Condition Interaction Predicting Post-Intervention Salivary Cortisol")
lines(treatment_mean, type="b", col="black", lty=2, lwd=3, pch=15)
axis(1, at=(1:17), padj=0.4, labels=c("Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","","Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress"))
arrows(1, control_ci.up[1], 1, control_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, control_ci.up[2], 2, control_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, control_ci.up[4], 4, control_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, control_ci.up[5], 5, control_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, control_ci.up[7], 7, control_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, control_ci.up[8], 8, control_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, control_ci.up[10], 10, control_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, control_ci.up[11], 11, control_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, control_ci.up[13], 13, control_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, control_ci.up[14], 14, control_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, control_ci.up[16], 16, control_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, control_ci.up[17], 17, control_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(19, control_ci.up[19], 19, control_ci.down[19], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(20, control_ci.up[20], 20, control_ci.down[20], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(22, control_ci.up[22], 22, control_ci.down[22], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(23, control_ci.up[23], 23, control_ci.down[23], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(1, treatment_ci.up[1], 1, treatment_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, treatment_ci.up[2], 2, treatment_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, treatment_ci.up[4], 4, treatment_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, treatment_ci.up[5], 5, treatment_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, treatment_ci.up[7], 7, treatment_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, treatment_ci.up[8], 8, treatment_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, treatment_ci.up[10], 10, treatment_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, treatment_ci.up[11], 11, treatment_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, treatment_ci.up[13], 13, treatment_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, treatment_ci.up[14], 14, treatment_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, treatment_ci.up[16], 16, treatment_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, treatment_ci.up[17], 17, treatment_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
legend("bottomleft", c( "Control", "Incremental Theory of Personality"),  lty=c(1, 2), pch=c(17,15), col=c("black", "black"), lwd=2, inset=.02, cex=.8)
mtext("Baseline", side=1, line=3, at= 1.5, cex=1,  col="black", font=2)  
mtext("Day 5", side=1, line=3, at= 4.5, cex=1,  col="black", font=2) 
mtext("Day 6", side=1, line=3, at= 7.5, cex=1,  col="black", font=2)  
mtext("Day 7", side=1, line=3, at= 10.5, cex=1,  col="black", font=2) 
mtext("Day 8", side=1, line=3, at= 13.5, cex=1,  col="black", font=2)
mtext("Day 9", side=1, line=3, at= 16.5, cex=1,  col="black", font=2)  
text(1.5, 23,   labels=paste("Main  p=", me.cort.ps[1], "\n Int  p=", dsxt.cort.ps[1]), cex=.9)
text(3+1.5, 23, labels=paste("Main  p=", me.cort.ps[2], "\n Int  p=", dsxt.cort.ps[2]), cex=.9)
text(6+1.5, 23, labels=paste("Main  p=", me.cort.ps[3], "\n Int  p=", dsxt.cort.ps[3]), cex=.9)
text(9+1.5, 23, labels=paste("Main  p=", me.cort.ps[4], "\n Int  p=", dsxt.cort.ps[4]), cex=.9)
text(12+1.5, 23, labels=paste("Main  p=", me.cort.ps[5], "\n Int  p=", dsxt.cort.ps[5]), cex=.9)
text(15+1.5, 23, labels=paste("Main  p=", me.cort.ps[6], "\n Int  p=", dsxt.cort.ps[6]), cex=.9)
text(2,   control_ci.down[2]-2, labels=paste("p=", simple.cort.ps[1]), cex=.9)
text(3+2, control_ci.down[5]-2, labels=paste("p=", simple.cort.ps[2]), cex=.9)
text(6+2, control_ci.down[8]-2, labels=paste("p=", simple.cort.ps[3]), cex=.9)
text(9+2, control_ci.down[11]-2, labels=paste("p=", simple.cort.ps[4]), cex=.9)
text(12+2,treatment_ci.down[14]-2, labels=paste("p=", simple.cort.ps[5]), cex=.9)
text(15+2,treatment_ci.down[17]-2, labels=paste("p=", simple.cort.ps[6]), cex=.9)
dev.off()



########################################################
### PLOT 3: Daily stressors x Treatment >> Daily DHEA-S
########################################################

## lm

control_minus1sd_day0 <- lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_plus1sd_day0  <- lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

control_minus1sd_day5 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5)) 

control_plus1sd_day5 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5))  

control_minus1sd_day6 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

control_plus1sd_day6 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

control_minus1sd_day7 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7)) 

control_plus1sd_day7 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7)) 

control_minus1sd_day8 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8)) 

control_plus1sd_day8 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

control_minus1sd_day9 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_minus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  

control_plus1sd_day9 = lm(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  
 


treatment_minus1sd_day0 <- lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_plus1sd_day0 <- lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd+z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex+z.gp1.math, data=long, subset=long$day %in% c(0))

treatment_minus1sd_day5 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5)) 

treatment_plus1sd_day5 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(5))  

treatment_minus1sd_day6 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6)) 

treatment_plus1sd_day6 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(6))  

treatment_minus1sd_day7 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7))  

treatment_plus1sd_day7 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(7))  

treatment_minus1sd_day8 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

treatment_plus1sd_day8 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(8))  

treatment_minus1sd_day9 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_minus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  

treatment_plus1sd_day9 = lm(daily_dheas ~ mindset_condition_reversed*z.daily.neg.socialacademic_plus1sd + mindset_condition_reversed*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math+ z.period.dumm.1+z.period.dumm.2+z.period.dumm.3+z.period.dumm.5+z.period.dumm.6+z.period.dumm.7+z.sex, data=long, subset=long$day %in% c(9))  

						
# plot for DHEA day 0 ~ 9

control_mean = rbind(coef(summary(control_minus1sd_day0))[1,1], 
                     coef(summary(control_plus1sd_day0))[1,1], NA,
                     coef(summary(control_minus1sd_day5))[1,1], 
                     coef(summary(control_plus1sd_day5))[1,1], NA,
                     coef(summary(control_minus1sd_day6))[1,1], 
                     coef(summary(control_plus1sd_day6))[1,1], NA,
                     coef(summary(control_minus1sd_day7))[1,1], 
                     coef(summary(control_plus1sd_day7))[1,1], NA,
                     coef(summary(control_minus1sd_day8))[1,1], 
                     coef(summary(control_plus1sd_day8))[1,1], NA,
                     coef(summary(control_minus1sd_day9))[1,1], 
                     coef(summary(control_plus1sd_day9))[1,1])

treatment_mean = rbind(coef(summary(treatment_minus1sd_day0))[1,1], 
                       coef(summary(treatment_plus1sd_day0))[1,1], NA,
                       coef(summary(treatment_minus1sd_day5))[1,1], 
                       coef(summary(treatment_plus1sd_day5))[1,1], NA,
                       coef(summary(treatment_minus1sd_day6))[1,1], 
                       coef(summary(treatment_plus1sd_day6))[1,1], NA,
                       coef(summary(treatment_minus1sd_day7))[1,1], 
                       coef(summary(treatment_plus1sd_day7))[1,1], NA,
                       coef(summary(treatment_minus1sd_day8))[1,1], 
                       coef(summary(treatment_plus1sd_day8))[1,1], NA,
                       coef(summary(treatment_minus1sd_day9))[1,1], 
                       coef(summary(treatment_plus1sd_day9))[1,1])

control_se = rbind(coef(summary(control_minus1sd_day0))[1,2], 
                   coef(summary(control_plus1sd_day0))[1,2], NA,
                   coef(summary(control_minus1sd_day5))[1,2], 
                   coef(summary(control_plus1sd_day5))[1,2], NA, 
                   coef(summary(control_minus1sd_day6))[1,2], 
                   coef(summary(control_plus1sd_day6))[1,2], NA,
                   coef(summary(control_minus1sd_day7))[1,2], 
                   coef(summary(control_plus1sd_day7))[1,2], NA,
                   coef(summary(control_minus1sd_day8))[1,2], 
                   coef(summary(control_plus1sd_day8))[1,2], NA,
                   coef(summary(control_minus1sd_day9))[1,2], 
                   coef(summary(control_plus1sd_day9))[1,2])

treatment_se = rbind(coef(summary(treatment_minus1sd_day0))[1,2], 
                     coef(summary(treatment_plus1sd_day0))[1,2], NA,
                     coef(summary(treatment_minus1sd_day5))[1,2], 
                     coef(summary(treatment_plus1sd_day5))[1,2], NA,
                     coef(summary(treatment_minus1sd_day6))[1,2], 
                     coef(summary(treatment_plus1sd_day6))[1,2], NA,
                     coef(summary(treatment_minus1sd_day7))[1,2], 
                     coef(summary(treatment_plus1sd_day7))[1,2], NA,
                     coef(summary(treatment_minus1sd_day8))[1,2], 
                     coef(summary(treatment_plus1sd_day8))[1,2], NA,
                     coef(summary(treatment_minus1sd_day9))[1,2], 
                     coef(summary(treatment_plus1sd_day9))[1,2])

control_ci.up = control_mean + control_se
control_ci.down = control_mean - control_se

treatment_ci.up = treatment_mean + treatment_se
treatment_ci.down = treatment_mean - treatment_se



## paste p-values 

me.dhea.day0 = lm(daily_dheas ~ mindset_condition+ as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==0)
me.dhea.day5 = lm(daily_dheas ~ mindset_condition+ logdheabase_dumm+logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==5)
me.dhea.day6 = lm(daily_dheas ~ mindset_condition+ logdheabase_dumm+logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==6)
me.dhea.day7 = lm(daily_dheas ~ mindset_condition+ logdheabase_dumm+logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==7)
me.dhea.day8 = lm(daily_dheas ~ mindset_condition+ logdheabase_dumm+logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==8)
me.dhea.day9 = lm(daily_dheas ~ mindset_condition+ logdheabase_dumm+logdheabase_miss+as.factor(sex)+as.factor(time)+gp1.math, 
                  data=long, subset=long$day==9)

dsxt.dhea.day0 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 
                    + z.period.dumm.3 + z.period.dumm.5 + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==0)
dsxt.dhea.day5 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm 
                    + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==5)
dsxt.dhea.day6 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm 
                    + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==6)
dsxt.dhea.day7 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm 
                    + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==7)
dsxt.dhea.day8 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm 
                    + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==8)
dsxt.dhea.day9 = lm(daily_dheas~ mindset_condition*z.daily.neg.socialacademic + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm 
                    + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
                    + z.period.dumm.6 + z.period.dumm.7 + z.sex, data=long, subset=long$day==9)

me.dhea.ps <- rbind(rd(coef(summary(me.dhea.day0))[2,4], digits=3),  
                    rd(coef(summary(me.dhea.day5))[2,4], digits=3), 
                    rd(coef(summary(me.dhea.day6))[2,4], digits=3),
                    rd(coef(summary(me.dhea.day7))[2,4], digits=3),
                    rd(coef(summary(me.dhea.day8))[2,4], digits=3),
                    rd(coef(summary(me.dhea.day9))[2,4], digits=3))

dsxt.dhea.ps <- rbind(rd(coef(summary(dsxt.dhea.day0))[12,4], digits=3),
                      rd(coef(summary(dsxt.dhea.day5))[15,4], digits=3), 
                      rd(coef(summary(dsxt.dhea.day6))[15,4], digits=3), 
                      rd(coef(summary(dsxt.dhea.day7))[15,4], digits=3), 
                      rd(coef(summary(dsxt.dhea.day8))[15,4], digits=3), 
                      rd(coef(summary(dsxt.dhea.day9))[15,4], digits=3))

# paste p-values for simple effects

simple.dhea.ps <- rbind(rd(coef(summary(control_plus1sd_day0))[2,4], digits=3),
                       rd(coef(summary(control_plus1sd_day5))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day6))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day7))[2,4], digits=3), 
                      rd(coef(summary(control_plus1sd_day8))[2,4], digits=2), 
                      rd(coef(summary(control_plus1sd_day9))[2,4], digits=3))

# Plot with MEANs & SEs estimates

pdf("PLOT_FINAL3_DHEA-S_day59.pdf", height=5, width=13)
plot(control_mean, ylab="Salivary DHEA-S (pg/ml)", xlab=NA, type="b", ylim=c(0, 800), col="black", xaxt="n", lwd=3, pch=17, font.lab=2, main="Daily Stress x Condition Interaction Predicting Post-Intervention Salivary DHEA-S")
lines(treatment_mean, type="b", col="black", lty=2, lwd=3, pch=15)
axis(1, at=(1:17), padj=0.4, labels=c("Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","","Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress","", "Low\n Stress","High\n Stress"))
arrows(1, control_ci.up[1], 1, control_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, control_ci.up[2], 2, control_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, control_ci.up[4], 4, control_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, control_ci.up[5], 5, control_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, control_ci.up[7], 7, control_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, control_ci.up[8], 8, control_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, control_ci.up[10], 10, control_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, control_ci.up[11], 11, control_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, control_ci.up[13], 13, control_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, control_ci.up[14], 14, control_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, control_ci.up[16], 16, control_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, control_ci.up[17], 17, control_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(1, treatment_ci.up[1], 1, treatment_ci.down[1], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(2, treatment_ci.up[2], 2, treatment_ci.down[2], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(4, treatment_ci.up[4], 4, treatment_ci.down[4], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(5, treatment_ci.up[5], 5, treatment_ci.down[5], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(7, treatment_ci.up[7], 7, treatment_ci.down[7], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(8, treatment_ci.up[8], 8, treatment_ci.down[8], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(10, treatment_ci.up[10], 10, treatment_ci.down[10], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(11, treatment_ci.up[11], 11, treatment_ci.down[11], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(13, treatment_ci.up[13], 13, treatment_ci.down[13], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(14, treatment_ci.up[14], 14, treatment_ci.down[14], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(16, treatment_ci.up[16], 16, treatment_ci.down[16], code=3, length=0.05, lwd=0.7, angle=90, col='black')
arrows(17, treatment_ci.up[17], 17, treatment_ci.down[17], code=3, length=0.05, lwd=0.7, angle=90, col='black')
legend("bottomleft", c( "Control", "Incremental Theory of Personality"),  lty=c(1, 2), pch=c(17,15), col=c("black", "black"), lwd=2, inset=.02, cex=.8)
mtext("Baseline", side=1, line=3, at= 1.5, cex=1,  col="black", font=2)  
mtext("Day 5", side=1, line=3, at= 4.5, cex=1,  col="black", font=2) 
mtext("Day 6", side=1, line=3, at= 7.5, cex=1,  col="black", font=2)  
mtext("Day 7", side=1, line=3, at= 10.5, cex=1,  col="black", font=2) 
mtext("Day 8", side=1, line=3, at= 13.5, cex=1,  col="black", font=2)
mtext("Day 9", side=1, line=3, at= 16.5, cex=1,  col="black", font=2)    
text(1.5, 700,   labels=paste("Main  p=", me.dhea.ps[1], "\n Int  p=", dsxt.dhea.ps[1]), cex=.9)
text(3+1.5, 700, labels=paste("Main  p=", me.dhea.ps[2], "\n Int  p=", dsxt.dhea.ps[2]), cex=.9)
text(6+1.5, 700, labels=paste("Main  p=", me.dhea.ps[3], "\n Int  p=", dsxt.dhea.ps[3]), cex=.9)
text(9+1.5, 700, labels=paste("Main  p=", me.dhea.ps[4], "\n Int  p=", dsxt.dhea.ps[4]), cex=.9)
text(12+1.5, 700, labels=paste("Main  p=", me.dhea.ps[5], "\n Int  p=", ".000"), cex=.9)         # To replace "3e-04 (.0002)"
text(15+1.5, 700, labels=paste("Main  p=", me.dhea.ps[6], "\n Int  p=", dsxt.dhea.ps[6]), cex=.9)
text(2,   control_ci.down[2]-55, labels=paste("p=", simple.dhea.ps[1]), cex=.9)
text(3+2, treatment_ci.down[5]-55, labels=paste("p=", simple.dhea.ps[2]), cex=.9)
text(6+2, treatment_ci.down[8]-55, labels=paste("p=", simple.dhea.ps[3]), cex=.9)
text(9+2, treatment_ci.down[11]-55, labels=paste("p=", simple.dhea.ps[4]), cex=.9)  
text(12+2,treatment_ci.down[14]-55, labels=paste("p=", simple.dhea.ps[5]), cex=.9)
text(15+2,treatment_ci.down[17]-55, labels=paste("p=", simple.dhea.ps[6]), cex=.9)
dev.off()


################################################
### Aggregated simple effects on high stress days
################################################

# Daily threat appraisal 

summary(lmer(daily.threat ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.daily.threat_base.dumm + z.daily.threat_base.miss + z.pss.sum_baseline.dumm + z.pss.sum_baseline.miss + as.factor(time) + as.factor(day)+sex+gp1.math+(1|rid), data=long, subset=day%in%c(5,6,7,8,9)))       

# Daily cortisol 

summary(lmer(daily_cortisol ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logcortbase_dumm + z.logcortbase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
+ z.period.dumm.6 + z.period.dumm.7 + z.sex + (1|rid), data=long, subset=day%in%c(8,9)))

# Daily DHEA-S

summary(lmer(daily_dheas ~ mindset_condition*z.daily.neg.socialacademic_plus1sd + mindset_condition*z.daily.neg.baseline + z.logdheabase_dumm + z.logdheabase_miss + z.gp1.math + z.period.dumm.1 + z.period.dumm.2 + z.period.dumm.3 + z.period.dumm.5 
+ z.period.dumm.6 + z.period.dumm.7 + z.sex + (1|rid), data=long, subset=day%in%c(8,9)))

