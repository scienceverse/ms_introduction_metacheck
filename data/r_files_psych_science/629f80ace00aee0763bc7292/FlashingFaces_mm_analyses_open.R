library(readxl)
library(car)
library(lme4)
library(emmeans)
library(effects)

TR <- read_excel("R_trust_2faces_for_mm.xlsx")


TR$part<-factor(TR$part)
TR$cond<-factor(TR$cond)
TR$pic<-factor(TR$pic)
TR$choice<-factor(TR$choice)

TR$trial<-scale(TR$trial,center=TRUE,scale= TRUE)


m1=glmer(choice~ (1|pic), data=TR,family='binomial')
m2=glmer(choice~ (1|pic)+(1|part), data=TR,family='binomial')

anova(m1,m2)

m2=glmer(choice~cond+ (1|pic), data=TR,family='binomial')
m3=glmer(choice~cond+trust+(1|pic), data=TR,family='binomial')
m4=glmer(choice~cond+cond:trust+(1|pic), data=TR,family='binomial')

anova(m1,m2,m3,m4)
plot(allEffects(m2)) # just for quick visualization not the plots in the manuscript

emm_options(pbkrtest.limit = 6100)
emmeans(m2, list(pairwise ~ cond), adjust = "none")


##### STUDIES 2 and 3


T2 <- read_excel("Data_JOTandJOTD_mm.xlsx")

T2$part<-factor(T2$part)
T2$heart<-factor(T2$heart)
T2$pic<-factor(T2$pic)
T2$study=factor(T2$study)

########### Systole study data
TS=T2[which(T2$study==2),]  

TS$trial<-scale(TS$trial,center=TRUE,scale= TRUE)

s0=lmer(rating~ (1|pic), data=TS)
s1=lmer(rating~ (1|part)+(1|pic), data=TS)
s2=lmer(rating~ heart+ (1|part)+(1|pic), data=TS)
s3=lmer(rating~ heart+trust+ (1|part)+(1|pic), data=TS)
s4=lmer(rating~ heart*trust+ (1|part)+(1|pic), data=TS)

anova(s0,s1,s2,s3,s4)
plot(allEffects(s3)) # just for quick visualization not the plots in the manuscript

emm_options(pbkrtest.limit = 6100)
emmeans(s3, list(pairwise ~ heart), adjust = "none")


########### Diastole study data#

TD=T2[which(T2$study==3),]  

TD$trial<-scale(TD$trial,center=TRUE,scale= TRUE)

d0=lmer(rating~(1|pic), data=TD)
d1=lmer(rating~ (1|part)+(1|pic), data=TD)
d2=lmer(rating~ heart+ (1|part)+(1|pic), data=TD,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(d0,d1,d2)
d2=lmer(rating~ trust+ (1|part)+(1|pic), data=TD)
d3=lmer(rating~ heart:trust+trust+ (1|part)+(1|pic), data=TD)

anova(d0,d1,d2,d3)
plot(allEffects(d2)) # just for quick visualization not the plots in the manuscript


###### both studies

T2$trial<-scale(T2$trial,center=TRUE,scale= TRUE)

t0=lmer(rating~ (1|pic), data=T2)
t1=lmer(rating~ (1|part) +(1|pic), data=T2)
t2=lmer(rating~ trust+ (1|part)+(1|pic), data=T2)
t3=lmer(rating~ trust+heart+ (1|part)+(1|pic), data=T2)
anova(t0,t1,t2,t3)

t3=lmer(rating~ trust+study+ (1|part)+(1|pic), data=T2,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
anova(t2,t3)

t3=lmer(rating~ trust+study+ heart+ (1|part)+(1|pic), data=T2)
t4=lmer(rating~ trust+study*heart+(1|part)+(1|pic), data=T2)
anova(t2,t3,t4)
plot(allEffects(t4)) # just for quick visualization not the plots in the manuscript
