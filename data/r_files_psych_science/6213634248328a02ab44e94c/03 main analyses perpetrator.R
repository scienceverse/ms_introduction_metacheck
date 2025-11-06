library(lme4)
library(lmerTest)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sjPlot)
library(arm)
library(psych)


load("anchor.final.RData")

names(anchor.final)

###########################################main analyses##################
#anchor outcomes
#individual 
model.lsa.a <- lmer (z.an.lsa ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex + c.age + sq.age + married +
                     livetogether + an.rel + sep.after + cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                     livetogether + an.rel + sep.after +cheat.sum.max + (1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.sel.a)

#relationship outcomes
model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                         livetogether +  cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.a)

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rad.use.a)

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rdo.use.a)

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                         livetogether +  cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rcf.use.a)


##########partner outcomes
#individual outcomes
model.pr.lsa.a <- lmer (z.pr.lsa.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                       livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.lsa.a)

model.pr.sel.a <- lmer (z.pr.sel.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                       livetogether + cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.sel.a)

#relationship outcomes
model.pr.rsa.use.a <- lmer (z.pr.rsa.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                           livetogether +cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rsa.use.a)

model.pr.rin.use.a <- lmer (z.pr.rin.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rin.use.a)

model.pr.rad.use.a <- lmer (z.pr.rad.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rad.use.a)

model.pr.rdo.use.a <- lmer (z.pr.rdo.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rdo.use.a)

model.pr.rcf.use.a <- lmer (z.pr.rcf.use ~ row.n + an.inf1.preva + an.inf1.preLin + an.inf1.postD + an.inf1.postLin + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rcf.use.a)


#non linear effects
anchor.final$c.prelin=scale(anchor.final$an.inf1.preLin, scale=F)
anchor.final$sq.prelin=anchor.final$c.prelin^2

anchor.final$c.postlin=scale(anchor.final$an.inf1.postLin, scale=F)
anchor.final$sq.postlin=anchor.final$c.postlin^2

model.lsa.a.sq <- lmer (z.an.lsa ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2)  
                     + male + homosex +  c.age + sq.age + married +
                       livetogether + an.rel + sep.after +cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.lsa.a.sq)

model.sel.a.sq <- lmer (z.an.sel ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                     + male + homosex +  c.age + sq.age + married +
                       livetogether + an.rel + sep.after +cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.sel.a.sq)

#relationship outcomes
model.rsa.use.a.sq <- lmer (z.an.rsa.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                         + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.a.sq)

model.rin.use.a.sq <- lmer (z.an.rin.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                         + male + homosex +  c.age + sq.age + married +
                           livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rin.use.a.sq)

model.rad.use.a.sq <- lmer (z.an.rad.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                         + male + homosex +  c.age + sq.age + married +
                           livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rad.use.a.sq)

model.rdo.use.a.sq <- lmer (z.an.rdo.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                         + male + homosex +  c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rdo.use.a.sq)

model.rcf.use.a.sq <- lmer (z.an.rcf.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                         + male + homosex +  c.age + sq.age + married +
                           livetogether+  cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rcf.use.a.sq)


#partner outcomes
#individual outcomes
model.pr.lsa.a.sq <- lmer (z.pr.lsa.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2)
                           + male + homosex +  c.age + sq.age + married +
                             livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.lsa.a.sq)

model.pr.sel.a.sq <- lmer (z.pr.sel.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                        + male + homosex +  c.age + sq.age + married +
                          livetogether + cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.sel.a.sq)

#relationship outcomes
model.pr.rsa.use.a.sq <- lmer (z.pr.rsa.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                            + male + homosex +  c.age + sq.age + married +
                              livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rsa.use.a.sq)

model.pr.rin.use.a.sq <- lmer (z.pr.rin.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                            + male + homosex +  c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rin.use.a.sq)

model.pr.rad.use.a.sq <- lmer (z.pr.rad.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                            + male + homosex +  c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rad.use.a.sq)

model.pr.rdo.use.a.sq <- lmer (z.pr.rdo.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                            + male + homosex +  c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rdo.use.a.sq)

model.pr.rcf.use.a.sq <- lmer (z.pr.rcf.use ~ row.n + an.inf1.preva + poly(c.prelin, degree=2) + an.inf1.postD + poly(c.postlin, degree=2) 
                            + male + homosex +  c.age + sq.age + married +
                              livetogether+ cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rcf.use.a.sq)

##comparing models
anova(model.lsa.a, model.lsa.a.sq)
anova(model.sel.a, model.sel.a.sq)
anova(model.rsa.use.a, model.rsa.use.a.sq)
anova(model.rin.use.a, model.rin.use.a.sq)
anova(model.rad.use.a, model.rad.use.a.sq)
anova(model.rdo.use.a, model.rdo.use.a.sq)
anova(model.rcf.use.a, model.rcf.use.a.sq)

anova(model.pr.lsa.a, model.pr.lsa.a.sq)
anova(model.pr.sel.a, model.pr.sel.a.sq)
anova(model.pr.rsa.use.a, model.pr.rsa.use.a.sq)
anova(model.pr.rin.use.a, model.pr.rin.use.a.sq)
anova(model.pr.rad.use.a, model.pr.rad.use.a.sq)
anova(model.pr.rdo.use.a, model.pr.rdo.use.a.sq)
anova(model.pr.rcf.use.a, model.pr.rcf.use.a.sq)


###tables
library(sjPlot)
tab_model(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a.sq, model.rad.use.a.sq, model.rdo.use.a, model.rcf.use.a, file="anchor models all.doc",
          pred.labels = c("Intercept", "Testing effect", "Selection effect", "Anticipation effect",
                          "Post-event baseline change", "Socialization effect", 
                          "Gender", "Sexual orientation", "Age", "Age, squared", "Marial status", "Co-resident status", "Relationship status", "separation",
                          "Number of infidelity waves", "Linear anticipation", "Qudratic anticipation", "Linear socialization", "Quadratic socialization"),
          dv.labels = c("Actor life satisfaction", "Actor self-esteem", "Actor relationship satisfaction", "Actor intimacy", "Actor admiration", "Actor dominance", "Actor conflict"),
          string.pred = "b",
          string.ci = "95% CI",
          string.p = "p")

tab_model(model.pr.lsa.a, model.pr.sel.a, model.pr.rsa.use.a, model.pr.rin.use.a, model.pr.rad.use.a, model.pr.rdo.use.a, model.pr.rcf.use.a, file="anchor partner outcomes models all.doc",
          pred.labels = c("Intercept", "Testing effect", "Selection effect", "Anticipation effect",
                          "Post-event baseline change", "Socialization effect", 
                          "Gender", "Sexual orientation", "Age", "Age, squared", "Marial status", "Co-resident status", 
                          "Number of infidelity waves"),
          dv.labels = c("Partner life satisfaction", "Partner self-esteem", "Partner relationship satisfaction", "Partner intimacy", "Partner admiration", "Partner dominance", "Partner conflict"),
          string.pred = "b",
          string.ci = "95% CI",
          string.p = "p")



#save coefficients (for Figure 1)
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a$outcome='life satisfaction'
model.lsa.a$type='Actor perpetrator'
model.lsa.a=model.lsa.a[c(3:6),]
model.lsa.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a$outcome='self-esteem'
model.sel.a$type='Actor perpetrator'
model.sel.a=model.sel.a[c(3:6),]
model.sel.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a$outcome='relationship satisfaction'
model.rsa.use.a$type='Actor perpetrator'
model.rsa.use.a=model.rsa.use.a[c(3:6),]
model.rsa.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.rin.use.a.sq=as.data.frame(summary(model.rin.use.a.sq)$coefficients)
model.rin.use.a.sq$outcome='intimacy'
model.rin.use.a.sq$type='Actor perpetrator'
model.rin.use.a.sq=model.rin.use.a.sq[c(3:8),]
model.rin.use.a.sq$time=c('selection', 'linear anticipation', 'quadratic anticipation', 'post-event change', 'linear socialization', 'quadratic socialization')
model.rad.use.a.sq=as.data.frame(summary(model.rad.use.a.sq)$coefficients)
model.rad.use.a.sq$outcome='admiration'
model.rad.use.a.sq$type='Actor perpetrator'
model.rad.use.a.sq=model.rad.use.a.sq[c(3:8),]
model.rad.use.a.sq$time=c('selection', 'linear anticipation', 'quadratic anticipation', 'post-event change', 'linear socialization', 'quadratic socialization')
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a$outcome='dominance'
model.rdo.use.a$type='Actor perpetrator'
model.rdo.use.a=model.rdo.use.a[c(3:6),]
model.rdo.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a$outcome='conflict'
model.rcf.use.a$type='Actor perpetrator'
model.rcf.use.a=model.rcf.use.a[c(3:6),]
model.rcf.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')

models.a=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a.sq, model.rad.use.a.sq, model.rdo.use.a, model.rcf.use.a)

models.a$outcome.type='Actor outcomes'

#save coefficients
model.pr.lsa.a=as.data.frame(summary(model.pr.lsa.a)$coefficients)
model.pr.lsa.a$outcome='life satisfaction'
model.pr.lsa.a$type='Actor perpetrator'
model.pr.lsa.a=model.pr.lsa.a[c(3:6),]
model.pr.lsa.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.sel.a=as.data.frame(summary(model.pr.sel.a)$coefficients)
model.pr.sel.a$outcome='self-esteem'
model.pr.sel.a$type='Actor perpetrator'
model.pr.sel.a=model.pr.sel.a[c(3:6),]
model.pr.sel.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.rsa.use.a=as.data.frame(summary(model.pr.rsa.use.a)$coefficients)
model.pr.rsa.use.a$outcome='relationship satisfaction'
model.pr.rsa.use.a$type='Actor perpetrator'
model.pr.rsa.use.a=model.pr.rsa.use.a[c(3:6),]
model.pr.rsa.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.rin.use.a=as.data.frame(summary(model.pr.rin.use.a)$coefficients)
model.pr.rin.use.a$outcome='intimacy'
model.pr.rin.use.a$type='Actor perpetrator'
model.pr.rin.use.a=model.pr.rin.use.a[c(3:6),]
model.pr.rin.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.rad.use.a=as.data.frame(summary(model.pr.rad.use.a)$coefficients)
model.pr.rad.use.a$outcome='admiration'
model.pr.rad.use.a$type='Actor perpetrator'
model.pr.rad.use.a=model.pr.rad.use.a[c(3:6),]
model.pr.rad.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.rdo.use.a=as.data.frame(summary(model.pr.rdo.use.a)$coefficients)
model.pr.rdo.use.a$outcome='dominance'
model.pr.rdo.use.a$type='Actor perpetrator'
model.pr.rdo.use.a=model.pr.rdo.use.a[c(3:6),]
model.pr.rdo.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')
model.pr.rcf.use.a=as.data.frame(summary(model.pr.rcf.use.a)$coefficients)
model.pr.rcf.use.a$outcome='conflict'
model.pr.rcf.use.a$type='Actor perpetrator'
model.pr.rcf.use.a=model.pr.rcf.use.a[c(3:6),]
model.pr.rcf.use.a$time=c('selection', 'linear anticipation', 'post-event change', 'linear socialization')

models.pr=rbind(model.pr.lsa.a, model.pr.sel.a, model.pr.rsa.use.a, model.pr.rin.use.a, model.pr.rad.use.a, model.pr.rdo.use.a, model.pr.rcf.use.a)

models.pr$outcome.type='Partner outcomes'

a.models=rbind(models.a, models.pr)

save(a.models, file="a.models.RData")

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(a.models, file="a.models.RData")


