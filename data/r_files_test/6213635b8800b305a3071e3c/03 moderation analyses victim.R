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


###############################moderation################
####commitment
anchor.final$c.commitment.first.use=scale(anchor.final$commitment.first.use, scale=F)
anchor.final$c.p.commitment.first.use=scale(anchor.final$p.commitment.first.use, scale=F)

#actor outcomes
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a)

model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                           pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.rsa.use.a) 

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                           pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a) 

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                           pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a) 

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                           pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.a)

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                           pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a) 

#partner outcomes
model.lsa.p <- lmer (z.pr.lsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                       pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.p)

model.sel.p <- lmer (z.pr.sel.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                       pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.p)

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                           pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.p) 

model.rin.use.p <- lmer (z.pr.rin.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                           pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.p)

model.rad.use.p <- lmer (z.pr.rad.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                           pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.p)

model.rdo.use.p <- lmer (z.pr.rdo.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                           pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.p)

model.rcf.use.p <- lmer (z.pr.rcf.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                           pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.p)

#save coefficients
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a=model.lsa.a[c(15:16),]
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a=model.sel.a[c(15:16),]
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a=model.rsa.use.a[c(15:16),]
model.rin.use.a=as.data.frame(summary(model.rin.use.a)$coefficients)
model.rin.use.a=model.rin.use.a[c(15:16),]
model.rad.use.a=as.data.frame(summary(model.rad.use.a)$coefficients)
model.rad.use.a=model.rad.use.a[c(15:16),]
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a=model.rdo.use.a[c(15:16),]
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a=model.rcf.use.a[c(15:16),]
model.lsa.p=as.data.frame(summary(model.lsa.p)$coefficients)
model.lsa.p=model.lsa.p[c(15:16),]
model.sel.p=as.data.frame(summary(model.sel.p)$coefficients)
model.sel.p=model.sel.p[c(15:16),]
model.rsa.use.p=as.data.frame(summary(model.rsa.use.p)$coefficients)
model.rsa.use.p=model.rsa.use.p[c(15:16),]
model.rin.use.p=as.data.frame(summary(model.rin.use.p)$coefficients)
model.rin.use.p=model.rin.use.p[c(15:16),]
model.rad.use.p=as.data.frame(summary(model.rad.use.p)$coefficients)
model.rad.use.p=model.rad.use.p[c(15:16),]
model.rdo.use.p=as.data.frame(summary(model.rdo.use.p)$coefficients)
model.rdo.use.p=model.rdo.use.p[c(15:16),]
model.rcf.use.p=as.data.frame(summary(model.rcf.use.p)$coefficients)
model.rcf.use.p=model.rcf.use.p[c(15:16),]

models.com=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a, model.rad.use.a, model.rdo.use.a, model.rcf.use.a,
                 model.lsa.p, model.sel.p, model.rsa.use.p, model.rin.use.p, model.rad.use.p, model.rdo.use.p, model.rcf.use.p)

save(models.com, file="models.com.RData")
write.csv(models.com, "models.com.csv")


names(anchor.final)


##################socio-demographics
####gender
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max + (1 + pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)
                     +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a) 

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a) 

model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.a)

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + 
                           pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)  +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a) 

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.a) #removed the random effect of prelin due to convergence problems

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a)

#partner
model.lsa.p <- lmer (z.pr.lsa.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.p)  #removed the random effect of prelin due to convergence problems

model.sel.p <- lmer (z.pr.sel.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max +(1 +pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.p) #removed the random effect of prelin due to convergence problems 

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1 +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.p) 

model.rin.use.p <- lmer (z.pr.rin.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.p)

model.rad.use.p <- lmer (z.pr.rad.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.p) 

model.rdo.use.p <- lmer (z.pr.rdo.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 + pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.p) 

model.rcf.use.p <- lmer (z.pr.rcf.use ~ row.n + pr.inf1.preva*male + pr.inf1.preLin*male + pr.inf1.postD*male + pr.inf1.postLin*male + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.p)


#save coefficients
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a=model.lsa.a[c(16:19),]
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a=model.sel.a[c(16:19),]
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a=model.rsa.use.a[c(14:17),]
model.rin.use.a=as.data.frame(summary(model.rin.use.a)$coefficients)
model.rin.use.a=model.rin.use.a[c(14:17),]
model.rad.use.a=as.data.frame(summary(model.rad.use.a)$coefficients)
model.rad.use.a=model.rad.use.a[c(14:17),]
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a=model.rdo.use.a[c(14:17),]
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a=model.rcf.use.a[c(14:17),]
model.lsa.p=as.data.frame(summary(model.lsa.p)$coefficients)
model.lsa.p=model.lsa.p[c(14:17),]
model.sel.p=as.data.frame(summary(model.sel.p)$coefficients)
model.sel.p=model.sel.p[c(14:17),]
model.rsa.use.p=as.data.frame(summary(model.rsa.use.p)$coefficients)
model.rsa.use.p=model.rsa.use.p[c(14:17),]
model.rin.use.p=as.data.frame(summary(model.rin.use.p)$coefficients)
model.rin.use.p=model.rin.use.p[c(14:17),]
model.rad.use.p=as.data.frame(summary(model.rad.use.p)$coefficients)
model.rad.use.p=model.rad.use.p[c(14:17),]
model.rdo.use.p=as.data.frame(summary(model.rdo.use.p)$coefficients)
model.rdo.use.p=model.rdo.use.p[c(14:17),]
model.rcf.use.p=as.data.frame(summary(model.rcf.use.p)$coefficients)
model.rcf.use.p=model.rcf.use.p[c(14:17),]

models.gender=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a, model.rad.use.a, model.rdo.use.a, model.rcf.use.a,
                    model.lsa.p, model.sel.p, model.rsa.use.p, model.rin.use.p, model.rad.use.p, model.rdo.use.p, model.rcf.use.p)

save(models.gender, file="models.gender.RData")
write.csv(models.gender, "models.gender.csv")


#####marital status interactions
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male 
                     + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)
                     +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a)

model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + 
                           male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1  + pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.a) 

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + 
                           pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)  +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a)

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+ pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.a) 

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1  + pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a) 

#partner outcomes
model.lsa.p <- lmer (z.pr.lsa.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1 + pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.p) 

model.sel.p <- lmer (z.pr.sel.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max +(1+pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.p) #removed the random effect of prelin due to convergence problems

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.p)

model.rin.use.p <- lmer (z.pr.rin.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.p)

model.rad.use.p <- lmer (z.pr.rad.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+ pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.p)

model.rdo.use.p <- lmer (z.pr.rdo.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 + pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.p) 

model.rcf.use.p <- lmer (z.pr.rcf.use ~ row.n + pr.inf1.preva*married + pr.inf1.preLin*married + pr.inf1.postD*married + pr.inf1.postLin*married + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1 +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.p)


#save coefficients
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a=model.lsa.a[c(16:19),]
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a=model.sel.a[c(16:19),]
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a=model.rsa.use.a[c(14:17),]
model.rin.use.a=as.data.frame(summary(model.rin.use.a)$coefficients)
model.rin.use.a=model.rin.use.a[c(14:17),]
model.rad.use.a=as.data.frame(summary(model.rad.use.a)$coefficients)
model.rad.use.a=model.rad.use.a[c(14:17),]
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a=model.rdo.use.a[c(14:17),]
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a=model.rcf.use.a[c(14:17),]
model.lsa.p=as.data.frame(summary(model.lsa.p)$coefficients)
model.lsa.p=model.lsa.p[c(14:17),]
model.sel.p=as.data.frame(summary(model.sel.p)$coefficients)
model.sel.p=model.sel.p[c(14:17),]
model.rsa.use.p=as.data.frame(summary(model.rsa.use.p)$coefficients)
model.rsa.use.p=model.rsa.use.p[c(14:17),]
model.rin.use.p=as.data.frame(summary(model.rin.use.p)$coefficients)
model.rin.use.p=model.rin.use.p[c(14:17),]
model.rad.use.p=as.data.frame(summary(model.rad.use.p)$coefficients)
model.rad.use.p=model.rad.use.p[c(14:17),]
model.rdo.use.p=as.data.frame(summary(model.rdo.use.p)$coefficients)
model.rdo.use.p=model.rdo.use.p[c(14:17),]
model.rcf.use.p=as.data.frame(summary(model.rcf.use.p)$coefficients)
model.rcf.use.p=model.rcf.use.p[c(14:17),]

models.married=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a, model.rad.use.a, model.rdo.use.a, model.rcf.use.a,
                     model.lsa.p, model.sel.p, model.rsa.use.p, model.rin.use.p, model.rad.use.p, model.rdo.use.p, model.rcf.use.p)

save(models.married, file="models.married.RData")
write.csv(models.married, "models.married.csv")


######co-resident status interactions
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male 
                     + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)
                     +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a)

model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + 
                           male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.a)

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + 
                           pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)  +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a)

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.a) #removed prelin

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married+
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a)

#partner
model.lsa.p <- lmer (z.pr.lsa.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.p)

model.sel.p <- lmer (z.pr.sel.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max +(1 +pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.p)  #removed the random effect of prelin due to convergence problems

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.p)

model.rin.use.p <- lmer (z.pr.rin.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.p)

model.rad.use.p <- lmer (z.pr.rad.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.p)

model.rdo.use.p <- lmer (z.pr.rdo.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+ pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.p) 

model.rcf.use.p <- lmer (z.pr.rcf.use ~ row.n + pr.inf1.preva*livetogether + pr.inf1.preLin*livetogether + pr.inf1.postD*livetogether + pr.inf1.postLin*livetogether + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.p)


#save coefficients
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a=model.lsa.a[c(16:19),]
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a=model.sel.a[c(16:19),]
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a=model.rsa.use.a[c(14:17),]
model.rin.use.a=as.data.frame(summary(model.rin.use.a)$coefficients)
model.rin.use.a=model.rin.use.a[c(14:17),]
model.rad.use.a=as.data.frame(summary(model.rad.use.a)$coefficients)
model.rad.use.a=model.rad.use.a[c(14:17),]
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a=model.rdo.use.a[c(14:17),]
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a=model.rcf.use.a[c(14:17),]
model.lsa.p=as.data.frame(summary(model.lsa.p)$coefficients)
model.lsa.p=model.lsa.p[c(14:17),]
model.sel.p=as.data.frame(summary(model.sel.p)$coefficients)
model.sel.p=model.sel.p[c(14:17),]
model.rsa.use.p=as.data.frame(summary(model.rsa.use.p)$coefficients)
model.rsa.use.p=model.rsa.use.p[c(14:17),]
model.rin.use.p=as.data.frame(summary(model.rin.use.p)$coefficients)
model.rin.use.p=model.rin.use.p[c(14:17),]
model.rad.use.p=as.data.frame(summary(model.rad.use.p)$coefficients)
model.rad.use.p=model.rad.use.p[c(14:17),]
model.rdo.use.p=as.data.frame(summary(model.rdo.use.p)$coefficients)
model.rdo.use.p=model.rdo.use.p[c(14:17),]
model.rcf.use.p=as.data.frame(summary(model.rcf.use.p)$coefficients)
model.rcf.use.p=model.rcf.use.p[c(14:17),]

models.livetogether=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a, model.rad.use.a, model.rdo.use.a, model.rcf.use.a,
                          model.lsa.p, model.sel.p, model.rsa.use.p, model.rin.use.p, model.rad.use.p, model.rdo.use.p, model.rcf.use.p)

save(models.livetogether, file="models.livetogether.RData")
write.csv(models.livetogether, "models.livetogether.csv")


###########################simple slopes
#####commitment
sd(anchor.final$c.commitment.first.use, na.rm=T)
anchor.final$com.low=anchor.final$c.commitment.first.use+.77
anchor.final$com.high=anchor.final$c.commitment.first.use-.77

sd(anchor.final$c.p.commitment.first.use, na.rm=T)
anchor.final$p.com.low=anchor.final$c.p.commitment.first.use+.69
anchor.final$p.com.high=anchor.final$c.p.commitment.first.use-.69

#low
model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.low+
                           pr.inf1.postLin*com.low + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.rsa.use.a) 

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.low+
                           pr.inf1.postLin*com.low + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a) 

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.low+
                           pr.inf1.postLin*com.low + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a) 

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.low+
                           pr.inf1.postLin*com.low + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a) 

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*p.com.low+
                           pr.inf1.postLin*p.com.low + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.p) 


#high
model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.high+
                           pr.inf1.postLin*com.high + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.rsa.use.a) 

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.high+
                           pr.inf1.postLin*com.high + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass),data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a) 

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.high+
                           pr.inf1.postLin*com.high+ male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a) 

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*com.high+
                           pr.inf1.postLin*com.high + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a) 

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva+ pr.inf1.preLin + pr.inf1.postD*p.com.high+
                           pr.inf1.postLin*p.com.high + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1 +pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.p) 


####separation
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin*sep.cat.max + male + homosex + c.age + sq.age + married +
                       livetogether  + cheat.sum.max + (1 + pr.inf1.postLin| id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a)
anova(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin*sep.cat.max + male + homosex + c.age + sq.age + married +
                       livetogether  + cheat.sum.max + (1 + pr.inf1.postLin| id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a)


nosep=subset(anchor.final, anchor.final$sep.cat.max==0)
single=subset(anchor.final, anchor.final$sep.cat.max==1)
repartnered=subset(anchor.final, anchor.final$sep.cat.max==2)

model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether  + cheat.sum.max + (1 | id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=nosep)
summary(model.lsa.a)
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether  + cheat.sum.max + (1 | id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=single)
summary(model.lsa.a)
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether  + cheat.sum.max + (1 | id)+ (1 | subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=repartnered)
summary(model.lsa.a)


######number of infidelity episodes
model.lsa.a <- lmer (z.an.lsa ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male 
                     + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)
                     +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.a)

model.sel.a <- lmer (z.an.sel ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                       livetogether + an.rel + sep.after+cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.a)

model.rsa.use.a <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + 
                           male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rsa.use.a)

model.rin.use.a <- lmer (z.an.rin.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + 
                           pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id)  +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.a)

model.rad.use.a <- lmer (z.an.rad.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.a)

model.rdo.use.a <- lmer (z.an.rdo.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.a)  #removed the random effect of prelin due to convergence problems

model.rcf.use.a <- lmer (z.an.rcf.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married+
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.a)

#partner
model.lsa.p <- lmer (z.pr.lsa.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.p)

model.sel.p <- lmer (z.pr.sel.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                       livetogether + cheat.sum.max +(1 +pr.inf1.postD + pr.inf1.postLin| id) +(1| subclass),
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=anchor.final)
summary(model.sel.p)  #removed the random effect of prelin due to convergence problems

model.rsa.use.p <- lmer (z.pr.rsa.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +(1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.use.p)

model.rin.use.p <- lmer (z.pr.rin.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rin.use.p)

model.rad.use.p <- lmer (z.pr.rad.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rad.use.p)

model.rdo.use.p <- lmer (z.pr.rdo.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male+ homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max + (1+ pr.inf1.preLin+pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rdo.use.p) 

model.rcf.use.p <- lmer (z.pr.rcf.use ~ row.n + pr.inf1.preva + pr.inf1.preLin*cheat.sum.max + pr.inf1.postD*cheat.sum.max + pr.inf1.postLin*cheat.sum.max + male + homosex + c.age+sq.age + married +
                           livetogether + cheat.sum.max +  (1+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin | id) +(1| subclass), data=anchor.final,
                         control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
summary(model.rcf.use.p)


#save coefficients
model.lsa.a=as.data.frame(summary(model.lsa.a)$coefficients)
model.lsa.a=model.lsa.a[c(16:18),]
model.sel.a=as.data.frame(summary(model.sel.a)$coefficients)
model.sel.a=model.sel.a[c(16:18),]
model.rsa.use.a=as.data.frame(summary(model.rsa.use.a)$coefficients)
model.rsa.use.a=model.rsa.use.a[c(14:16),]
model.rin.use.a=as.data.frame(summary(model.rin.use.a)$coefficients)
model.rin.use.a=model.rin.use.a[c(14:16),]
model.rad.use.a=as.data.frame(summary(model.rad.use.a)$coefficients)
model.rad.use.a=model.rad.use.a[c(14:16),]
model.rdo.use.a=as.data.frame(summary(model.rdo.use.a)$coefficients)
model.rdo.use.a=model.rdo.use.a[c(14:16),]
model.rcf.use.a=as.data.frame(summary(model.rcf.use.a)$coefficients)
model.rcf.use.a=model.rcf.use.a[c(14:16),]
model.lsa.p=as.data.frame(summary(model.lsa.p)$coefficients)
model.lsa.p=model.lsa.p[c(14:16),]
model.sel.p=as.data.frame(summary(model.sel.p)$coefficients)
model.sel.p=model.sel.p[c(14:16),]
model.rsa.use.p=as.data.frame(summary(model.rsa.use.p)$coefficients)
model.rsa.use.p=model.rsa.use.p[c(14:16),]
model.rin.use.p=as.data.frame(summary(model.rin.use.p)$coefficients)
model.rin.use.p=model.rin.use.p[c(14:16),]
model.rad.use.p=as.data.frame(summary(model.rad.use.p)$coefficients)
model.rad.use.p=model.rad.use.p[c(14:16),]
model.rdo.use.p=as.data.frame(summary(model.rdo.use.p)$coefficients)
model.rdo.use.p=model.rdo.use.p[c(14:16),]
model.rcf.use.p=as.data.frame(summary(model.rcf.use.p)$coefficients)
model.rcf.use.p=model.rcf.use.p[c(14:16),]

models.cheatmax=rbind(model.lsa.a, model.sel.a, model.rsa.use.a, model.rin.use.a, model.rad.use.a, model.rdo.use.a, model.rcf.use.a,
                          model.lsa.p, model.sel.p, model.rsa.use.p, model.rin.use.p, model.rad.use.p, model.rdo.use.p, model.rcf.use.p)

save(models.cheatmax, file="models.cheatmax.RData")
write.csv(models.cheatmax, "models.cheatmax.csv")

