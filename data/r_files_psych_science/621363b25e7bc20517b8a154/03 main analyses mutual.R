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
model.lsa.a <- lmer (z.an.lsa ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                     livetogether + an.rel + sep.after + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                     livetogether + an.rel + sep.after+cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.sel.a)

#relationship outcomes
model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.a)

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rad.use.a)

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                         livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rdo.use.a)

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                         livetogether +  cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rcf.use.a)


######partner outcomes
#individual outcomes
model.pr.lsa.a <- lmer (z.pr.lsa.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.lsa.a)

model.pr.sel.a <- lmer (z.pr.sel.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.sel.a)

#relationship outcomes
model.pr.rsa.use.a <- lmer (z.pr.rsa.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rsa.use.a)

model.pr.rin.use.a <- lmer (z.pr.rin.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rin.use.a)

model.pr.rad.use.a <- lmer (z.pr.rad.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rad.use.a)

model.pr.rdo.use.a <- lmer (z.pr.rdo.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rdo.use.a)

model.pr.rcf.use.a <- lmer (z.pr.rcf.use ~ row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rcf.use.a)


#non linear effects
anchor.final$c.prelin=scale(anchor.final$anpr.inf1.preLin, scale=F)
anchor.final$sq.prelin=anchor.final$c.prelin^2

anchor.final$c.postlin=scale(anchor.final$anpr.inf1.postLin, scale=F)
anchor.final$sq.postlin=anchor.final$c.postlin^2

model.lsa.a.sq <- lmer (z.an.lsa ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)+ 
                      male + homosex + c.age + sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.lsa.a.sq)

model.sel.a.sq <- lmer (z.an.sel ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)+ 
                      male + homosex + c.age + sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.sel.a.sq)

#relationship outcomes
model.rsa.use.a.sq <- lmer (z.an.rsa.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)+
                          male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.a.sq)

model.rin.use.a.sq <- lmer (z.an.rin.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                         + male + homosex + c.age + sq.age + married +
                           livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rin.use.a.sq)

model.rad.use.a.sq <- lmer (z.an.rad.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                         + male + homosex + c.age + sq.age + married +
                           livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rad.use.a.sq)

model.rdo.use.a.sq <- lmer (z.an.rdo.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                         + male + homosex + c.age + sq.age + married +
                           livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rdo.use.a.sq)

model.rcf.use.a.sq <- lmer (z.an.rcf.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                         + male + homosex + c.age + sq.age + married +
                           livetogether+ cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rcf.use.a.sq)


#partner outcomes
#individual outcomes
model.pr.lsa.a.sq <- lmer (z.pr.lsa.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)+
                           + male + homosex + c.age + sq.age + married +
                             livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.lsa.a.sq)

model.pr.sel.a.sq <- lmer (z.pr.sel.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                        + male + homosex + c.age + sq.age + married +
                          livetogether + cheat.sum.max +(1 | id)+ (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.sel.a.sq)

#relationship outcomes
model.pr.rsa.use.a.sq <- lmer (z.pr.rsa.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                            + male + homosex + c.age + sq.age + married +
                              livetogether + cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rsa.use.a.sq)

model.pr.rin.use.a.sq <- lmer (z.pr.rin.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                            + male + homosex + c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rin.use.a.sq)

model.pr.rad.use.a.sq <- lmer (z.pr.rad.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                            + male + homosex + c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rad.use.a.sq)

model.pr.rdo.use.a.sq <- lmer (z.pr.rdo.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                            + male + homosex + c.age + sq.age + married +
                              livetogether+ cheat.sum.max +(1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rdo.use.a.sq)

model.pr.rcf.use.a.sq <- lmer (z.pr.rcf.use ~ row.n + anpr.inf1.preva + poly(c.prelin, degree=2) + anpr.inf1.postD + poly(c.postlin, degree=2)
                            + male + homosex + c.age + sq.age + married +
                              livetogether+ cheat.sum.max + (1 | id) + (1 | subclass), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.pr.rcf.use.a.sq)

anova(model.pr.lsa.a, model.pr.lsa.a.sq)
anova(model.pr.sel.a, model.pr.sel.a.sq)
anova(model.pr.rsa.use.a, model.pr.rsa.use.a.sq)
anova(model.pr.rin.use.a, model.pr.rin.use.a.sq)
anova(model.pr.rad.use.a, model.pr.rad.use.a.sq)
anova(model.pr.rdo.use.a, model.pr.rdo.use.a.sq)
anova(model.pr.rcf.use.a, model.pr.rcf.use.a.sq)
anova(model.lsa.a, model.lsa.a.sq)
anova(model.sel.a, model.sel.a.sq)
anova(model.rsa.use.a, model.rsa.use.a.sq)
anova(model.rin.use.a, model.rin.use.a.sq)
anova(model.rad.use.a, model.rad.use.a.sq)
anova(model.rdo.use.a, model.rdo.use.a.sq)
anova(model.rcf.use.a, model.rcf.use.a.sq)


###tables
library(sjPlot)
tab_model(model.lsa.a.sq, model.sel.a, model.rsa.use.a.sq, model.rin.use.a, model.rad.use.a, model.rdo.use.a, 
          model.rcf.use.a.sq, file="anchor models all.doc") 
      
tab_model(model.pr.lsa.a.sq, model.pr.sel.a.sq, model.pr.rsa.use.a.sq, model.pr.rin.use.a.sq,
          model.pr.rad.use.a.sq, model.pr.rdo.use.a, model.pr.rcf.use.a.sq, file="anchor partner outcomes model all.doc")


