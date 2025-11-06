setwd("~/Documents/Scholarship/Manuscripts/Georgetown/Weekly Paper/ANALYSES/Weekly")

library(haven)
library(psych)
library(nlme)
library(lme4)
library(standardize)
library("sjPlot")
library(glmmTMB)
library(stats)
#COHEN-D
library(EMAtools)



data<-read.csv("weekly-datalong-wMain.csv")
describe(data)
d<-data



d$time
d$time1<-d$time+9
d$time1
# d$SickFt[d$SickF > 5] <- 5
dd<-d

#POWER
library(powerlmm)
citation("powerlmm")
vignette("two-level", package = "powerlmm")

p <- study_parameters(n1 = 3, # time points
                      n2 = 77, # subjects per cluster
                      sigma_subject_intercept = 1.5,
                      sigma_subject_slope = 0,
                      sigma_error = 0.5,
                      effect_size = cohend(0.2),
                      dropout = dropout_weibull(proportion = 0, rate = 1)
)
p <- study_parameters(n1 = 10, # time points
                      n2 = 50, # subjects per cluster
                      sigma_subject_intercept = 1,
                      sigma_subject_slope = 0.01,
                      sigma_error = 1,
                      effect_size = cohend(0.5),
                      dropout = dropout_weibull(proportion = 0, rate = 1)
)
get_power(p, alpha=0.0125)
get_power(p, alpha=0.016666666667)
get_power(p, alpha=0.025)
get_power(p)

#p-ADJUST
library(stats)
p=c(0.042, 0.000847, 0.001605, 0.0000193)
p=c(0.0788, 0.5824, 0.5987)
p
p.adjust(p, method="holm")


#DESCRIPRIVES
describe(d)
descr<-describeBy(d,group=d$Condition)
descr<-as.data.frame(descr)
write.csv(descr$`0`, "meansC.csv")
write.csv(descr$`1`, "meansT.csv")
                     


###MODELING####

#SWB
d$time
m <-lmer(Meaning ~ time * Condition + (time|| ID), d, na.action = na.omit)
tab_model(m,
          show.stat = T, show.df = T, show.std = "std2", show.se=T, show.ci = F, show.r2 = T)
# Back-calculating Cohen d (t-test)
lme.dscore(m,data=data,type="lme4")

#SICK: Gaussian (with outliers)
m <-lmer(SickF ~ time * Condition + (time|| ID), d, na.action = na.omit)
summary(m)
tab_model(m,
          show.stat = T, show.df = T, show.std = "std", show.se=T, show.ci = T, show.r2 = T)
lme.dscore(m,data=data,type="lme4")

#SICK Poisson (with outliers)
m <-glmer(SickF ~ time* Condition + (time|| ID), d, na.action = na.omit,
            family = 'poisson')
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)





#TABLE 3 (no outliers)
d$SickFt

#Sick Poisson time (null model)
m <-glmer(SickFt ~ 1+(1| ID), 
          d, na.action = na.omit, family = 'poisson') 
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)

#Sick Poisson time
m <-glmer(SickFt ~ time  + (time|| ID), d, na.action = na.omit, 
          family = 'poisson')
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)

#Sick Poisson Quadratic time
m <-glmer(SickFt ~ poly(time,2)  + (time|| ID), d, na.action = na.omit, 
          family = 'poisson')
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)

#SICK Poisson with no outliers
m <-glmer(SickFt ~ time* Condition + (time|| ID), data, na.action = na.omit,
          family = 'poisson')
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)

m <-glmer(SickFt ~ time * Condition + age.A1 + SWLS.A1 + PA.A1 + NA.A1 + MLQP.A1 + (time|| ID), 
                 d, na.action = na.omit, 
          glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
          family = 'poisson') 
tab_model(m, 
          show.aic = T, show.dev = T, show.obs=T, show.stat = T, show.df = T, show.r2 = T)
summary(m)



#CohenD
lme.dscore(m,data=d,type="lme4")



#PLOTTING
require(ggplot2)
library(hmisc)

#Value Lables
d$Condition <- factor(d$Condition,
                      levels = c(0,1),
                      labels = c("Control", "Treatment"))
is.numeric(d$SickF)
d$time2<-d$time+10
d$time2
#Linear Plots
ggplot(d, aes(time2, SickF, color=Condition)) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult = 1), geom="pointrange", position=position_dodge(width=0.5)) + 
  labs(y="Sick Days", x="Week") + 
  theme_bw() + scale_color_manual(values=c("blue", "red")) +
  scale_x_continuous (breaks=1:10) +
  scale_y_continuous (breaks=0:5) +
  geom_smooth(se = TRUE, method = "lm")

ggplot(d, aes(time2, PA, group = Condition, color=Condition)) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult = 1),  geom="pointrange", position=position_dodge(width=0.5)) +
  labs(y="Positive Affect", x="Week") + 
  theme_bw() + scale_color_manual(values=c("blue", "red")) +
  scale_x_continuous (breaks=1:10) +
  scale_y_continuous (breaks=1:5, limits=c(1,5)) +
  geom_smooth(se = TRUE, method = "lm")

ggplot(d, aes(time2, ff, group = Condition, color=Condition)) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult = 1), geom="pointrange", position=position_dodge(width=0.5)) + 
  labs(y="Negative Affect", x="Week") + 
  theme_bw() + scale_color_manual(values=c("blue", "red")) +
  scale_x_continuous (breaks=1:10) +
  scale_y_continuous (breaks=1:5, limits=c(1,5)) +
  geom_smooth(se = TRUE, method = "lm")

ggplot(d, aes(time2, LS, group = Condition, color=Condition)) + 
  stat_summary(fun.data="mean_sdl", fun.args = list(mult = 1), geom="pointrange", position=position_dodge(width=0.5)) + 
  labs(y="Life Satisfaction", x="Week") + 
  theme_bw() + scale_color_manual(values=c("blue", "red")) +
  scale_x_continuous (breaks=1:10) +
  scale_y_continuous (breaks=1:5, limits=c(1,5)) +
  geom_smooth(se = TRUE, method = "lm")


