####################cox regression multilevel###################
library(survival)
library(survminer)
library(frailtypack)
library(frailtyEM)

#######main analyses##########
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
cluster(HHID_copy), data = data)
summary(cox1)

cox2 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
scale(life_satconv)+cluster(HHID_copy), data = data)
summary(cox2)

cox3 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
race_white_r+race_white_s+
+edu_cat_r+edu_cat_s+
scale(income)+samesex+
cluster(HHID_copy), data = data)
summary(cox3)

cox4 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
race_white_r+race_white_s+
+edu_cat_r+edu_cat_s+
scale(income)+samesex+
scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
cluster(HHID_copy), data = data)
summary(cox4)

#########z life sat
cox1 <- coxph(Surv(month_death, dead) ~ Zlife_sat_fin_s+
                cluster(HHID_copy), data = data)
summary(cox1)

cox2 <- coxph(Surv(month_death, dead) ~ Zlife_sat_fin_s+
                Zlife_sat_fin+cluster(HHID_copy), data = data)
summary(cox2)

cox3 <- coxph(Surv(month_death, dead) ~ Zlife_sat_fin_s+
                Zlife_sat_fin+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                cluster(HHID_copy), data = data)
summary(cox3)

cox4 <- coxph(Surv(month_death, dead) ~ Zlife_sat_fin_s+
                Zlife_sat_fin+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                cluster(HHID_copy), data = data)
summary(cox4)


###interaction samesex by actor gender
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                male_r:samesex+
                cluster(HHID_copy), data = data)
summary(cox1)

###interactions partner life satisfaction and all other variables
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):male_r+
                cluster(HHID_copy), data = data)
summary(cox1)

cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(aget1)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):race_white_r+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(edu_cat_r)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(income)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(chronic)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(sibj_health)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(partnersup)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(act)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):dead_s+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+alive_s+
                scale(life_satconv_s):alive_s+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(age_s)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):race_white_s+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(edu_cat_s)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(chronic_s)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(sibj_health_s)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
                scale(life_satconv_s):scale(act_s)+
                cluster(HHID_copy), data = data)
summary(cox1)

###polynomial regression
attach(data)
Zlife_satconv<-scale(life_satconv)
Zlife_satconv_s<-scale(life_satconv_s)
life_satconv2<-Zlife_satconv*Zlife_satconv
life_satconv_s2<-Zlife_satconv_s*Zlife_satconv_s
i<-Zlife_satconv*Zlife_satconv_s

m1 <- coxph(Surv(month_death, dead) ~ Zlife_satconv+
Zlife_satconv_s+life_satconv2+life_satconv_s2+i+
cluster(HHID_copy), data = data)
summary(m1)

#################plot
# Fit survival curves
require("survival")
library(ggsci)
ggsurvplot(fit, data=data,
           conf.int = TRUE, 
           palette = "aaas",
           risk.table = "abs_pct", risk.table.col = "life_s_high",
           fun = "cumhaz",
           font.tickslab = c(10))

########################################################
#######################robustness checks################
########################################################

#########################only married###################
dataM <- subset(data, cohabiting==0) 

cox1 <- coxph(Surv(month_death, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
cluster(HHID_copy), data = dataM)
summary(cox1)


#########################age as time scale##############
cox1 <- coxph(Surv(death_days, dead) ~ scale(life_satconv_s)+
                scale(life_satconv)+wave+male_r+
                race_white_r+race_white_s+
                +edu_cat_r+edu_cat_s+
                scale(income)+samesex+
                scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s+
cluster(HHID_copy), data = data)
summary(cox1)


######################frailty model###################
model1 <- frailtyPenal(Surv(month_death, dead) ~ cluster(hhid) +
                         scale(life_satconv_s)+
                         scale(life_satconv)+wave+male_r+scale(aget1)+scale(age_s)+ 
                         race_white_r+race_white_s+
                         +edu_cat_r+edu_cat_s+
                         scale(income)+samesex+
                         scale(chronic)+scale(chronic_s)+scale(sibj_health)+scale(sibj_health_s)+dead_s, 
                       data = data1, n.knots = 8,
                       kappa=1000,
                       cross.validation = FALSE)
print(model1, digits = 4)

##########partner support##############
cox1 <- coxph(Surv(month_death, dead) ~ scale(partnersup)+
                cluster(HHID_copy), data = data)
summary(cox1)
cox1 <- coxph(Surv(month_death, dead) ~ scale(act_s)+
                cluster(HHID_copy), data = data)
summary(cox1)



