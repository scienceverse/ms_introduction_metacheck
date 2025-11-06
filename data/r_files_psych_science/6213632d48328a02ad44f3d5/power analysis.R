library(simr)
library(dplyr)
library(lme4)
library(lmerTest)



###################test if the effect of post-event baseline change in perpetrators (actors) could be detected in victims (actors)
###lsa
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.lsa",  
                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                               "livetogether", "cheat.sum.max", "an.rel", "sep.after")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.an.lsa ~  row.n + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + an.rel + sep.after +(1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.m) 


fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.an.lsa ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max + an.rel + sep.after +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.08

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.lsa.RData")


#############sel
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.sel",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max", "an.rel", "sep.after")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.sel.m <- lmer (z.an.sel ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + an.rel + sep.after +(1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.sel.m) 


fixed <- fixef(model.sel.m)
random <- VarCorr(model.sel.m)
sig <- sigma(model.sel.m)

sim_model <- makeLmer(z.an.sel ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max + an.rel + sep.after +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.12

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.sel.RData")


###rsa
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")
anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.rsa.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)

model.rsa.m <- lmer (z.an.rsa.use ~ row.n + pr.inf1.preva+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max +(1 | id), data=anchor.final, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.m)


fixed <- fixef(model.rsa.m)
random <- VarCorr(model.rsa.m)
sig <- sigma(model.rsa.m)

sim_model <- makeLmer(z.an.rsa.use ~ row.n + pr.inf1.preva+pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max + (1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.34

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rsa.RData")

###rin
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")
anchor.final$na=rowSums(is.na(anchor.final[, c("id", "z.an.rin.use",  
                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rin.m <- lmer (z.an.rin.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rin.m)


fixed <- fixef(model.rin.m)
random <- VarCorr(model.rin.m)
sig <- sigma(model.rin.m)

sim_model <- makeLmer(z.an.rin.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.21

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rin.RData")

###rad
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")
anchor.final$na=rowSums(is.na(anchor.final[, c("id", "z.an.rad.use",  
                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rad.m <- lmer (z.an.rad.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rad.m)


fixed <- fixef(model.rad.m)
random <- VarCorr(model.rad.m)
sig <- sigma(model.rad.m)

sim_model <- makeLmer(z.an.rad.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether +  cheat.sum.max + (1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.14

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rad.RData")

###rdo
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")
anchor.final$na=rowSums(is.na(anchor.final[, c("id", "z.an.rdo.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rdo.m <- lmer (z.an.rdo.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rdo.m)


fixed <- fixef(model.rdo.m)
random <- VarCorr(model.rdo.m)
sig <- sigma(model.rdo.m)

sim_model <- makeLmer(z.an.rdo.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max+ (1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- .02

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rdo.RData")


###rcf
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")
anchor.final$na=rowSums(is.na(anchor.final[, c("id", "z.an.rcf.use",  
                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rcf.m <- lmer (z.an.rcf.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rcf.m)


fixed <- fixef(model.rcf.m)
random <- VarCorr(model.rcf.m)
sig <- sigma(model.rcf.m)

sim_model <- makeLmer(z.an.rcf.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max+ (1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- .19

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rcf.RData")

#####display power
load("pwr.postD.lsa.RData")
pwr
load("pwr.postD.sel.RData")
pwr
load("pwr.postD.rsa.RData")
pwr
load("pwr.postD.rin.RData")
pwr
load("pwr.postD.rad.RData")
pwr
load("pwr.postD.rdo.RData")
pwr
load("pwr.postD.rcf.RData")
pwr


############################################################################################################
###################test if the effect of post-event baseline change in actors in the perpetrator sample could be detected in partners in the victim sample
###lsa
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.lsa.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.pr.lsa.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.lsa.m) 
##removed subclass due to convergence issues

fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.pr.lsa.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.08

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.lsa.pr.RData")

###sel
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.sel.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.sel.m <- lmer (z.pr.sel.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.sel.m) 


fixed <- fixef(model.sel.m)
random <- VarCorr(model.sel.m)
sig <- sigma(model.sel.m)

sim_model <- makeLmer(z.pr.sel.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.12

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.sel.pr.RData")


###rsa
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.rsa.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rsa.m <- lmer (z.pr.rsa.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rsa.m) 


fixed <- fixef(model.rsa.m)
random <- VarCorr(model.rsa.m)
sig <- sigma(model.rsa.m)

sim_model <- makeLmer(z.pr.rsa.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.34

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rsa.pr.RData")

###rin
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.rin.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rin.m <- lmer (z.pr.rin.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rin.m) 


fixed <- fixef(model.rin.m)
random <- VarCorr(model.rin.m)
sig <- sigma(model.rin.m)

sim_model <- makeLmer(z.pr.rin.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.21

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rin.pr.RData")

###rad
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.rad.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rad.m <- lmer (z.pr.rad.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rad.m) 


fixed <- fixef(model.rad.m)
random <- VarCorr(model.rad.m)
sig <- sigma(model.rad.m)

sim_model <- makeLmer(z.pr.rad.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- -.14

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rad.pr.RData")


###rdo
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.rdo.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rdo.m <- lmer (z.pr.rdo.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rdo.m) 


fixed <- fixef(model.rdo.m)
random <- VarCorr(model.rdo.m)
sig <- sigma(model.rdo.m)

sim_model <- makeLmer(z.pr.rdo.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- .02

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rdo.pr.RData")

###rcf
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/victims")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.pr.rcf.use",  
                                               "row.n", "pr.inf1.preva", "pr.inf1.preLin", "pr.inf1.postD", 
                                               "pr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.rcf.m <- lmer (z.pr.rcf.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                       homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), control = lmerControl(calc.derivs = FALSE), data=anchor.final)
summary(model.rcf.m) 


fixed <- fixef(model.rcf.m)
random <- VarCorr(model.rcf.m)
sig <- sigma(model.rcf.m)

sim_model <- makeLmer(z.pr.rcf.use ~  row.n + pr.inf1.preva +pr.inf1.preLin + pr.inf1.postD + pr.inf1.postLin + male + 
                        homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id), 
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['pr.inf1.postD1'] <- .19

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('pr.inf1.postD'))
pwr 

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.postD.rcf.pr.RData")

#####display power
load("pwr.postD.lsa.pr.RData")
pwr
load("pwr.postD.sel.pr.RData")
pwr
load("pwr.postD.rsa.pr.RData")
pwr
load("pwr.postD.rin.pr.RData")
pwr
load("pwr.postD.rad.pr.RData")
pwr
load("pwr.postD.rdo.pr.RData")
pwr
load("pwr.postD.rcf.pr.RData")
pwr



#######################################################################################
###################test if the effects of time variables (similar to those in victims) could be detected in mutual infidelity sample 
###selection
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/mutual")
load("anchor.final.RData")

anchor.final$anpr.inf1.preva=as.factor(anchor.final$anpr.inf1.preva)

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.rcf.use",  
                                               "row.n", "anpr.inf1.preva", "anpr.inf1.preLin", "anpr.inf1.postD", 
                                               "anpr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.an.rcf.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), data=anchor.final)
summary(model.lsa.m)


fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.an.rcf.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['anpr.inf1.preva1'] <- .32 ##largest effect from victim model

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('anpr.inf1.preva'))
pwr

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.mutual.preva.RData")

###anticipation
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/mutual")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.rad.use",  
                                               "row.n", "anpr.inf1.preva", "anpr.inf1.preLin", "anpr.inf1.postD", 
                                               "anpr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.an.rad.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), data=anchor.final)
summary(model.lsa.m)


fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.an.rad.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['anpr.inf1.preLin'] <- -.09 ##largest effect from victim model

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('anpr.inf1.preLin'))
pwr

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.mutual.prelin.RData")


###postD
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/mutual")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.rcf.use",  
                                               "row.n", "anpr.inf1.preva", "anpr.inf1.preLin", "anpr.inf1.postD", 
                                               "anpr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.an.rcf.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether + cheat.sum.max + (1 | id), data=anchor.final)
summary(model.lsa.m)


fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.an.rcf.use ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether + cheat.sum.max +(1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['anpr.inf1.postD1'] <- -.32 ##largest effect from victim model

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('anpr.inf1.postD'))
pwr

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.mutual.postD.RData")


###postLin
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision/mutual")
load("anchor.final.RData")

anchor.final$na=rowSums(is.na(anchor.final[, c("id", "subclass", "z.an.lsa",  
                                               "row.n", "anpr.inf1.preva", "anpr.inf1.preLin", "anpr.inf1.postD", 
                                               "anpr.inf1.postLin", "male", "homosex", "c.age", "sq.age", "married",
                                               "livetogether", "cheat.sum.max", "an.rel", "sep.after")]))
table(anchor.final$na, exclude = T)
anchor.final=subset(anchor.final, anchor.final$na==0)


model.lsa.m <- lmer (z.an.lsa ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                       livetogether +an.rel + sep.after+ cheat.sum.max + (1 | id), data=anchor.final)
summary(model.lsa.m)


fixed <- fixef(model.lsa.m)
random <- VarCorr(model.lsa.m)
sig <- sigma(model.lsa.m)

sim_model <- makeLmer(z.an.lsa ~  row.n + anpr.inf1.preva + anpr.inf1.preLin + anpr.inf1.postD + anpr.inf1.postLin + male + homosex + c.age + sq.age + married +
                        livetogether +an.rel + sep.after+ cheat.sum.max +(1 | id),
                      fixef=fixed, 
                      VarCorr=random, 
                      sigma=sig,
                      data=anchor.final)
sim_model

model_large <- sim_model
fixef(model_large)['anpr.inf1.postLin'] <- .03 ##largest effect from victim model

pwr <- powerSim(model_large, nsim=1000, seed = 1234,
                test = fixed('anpr.inf1.postLin'))
pwr

setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
save(pwr, file="pwr.mutual.postLin.RData")


#####display power
setwd("C:/Users/stavrova/OneDrive - Tilburg University/large-scale data/pairfam/infidelity/osf/revision")
load("pwr.mutual.preva.RData")
pwr
load("pwr.mutual.prelin.RData")
pwr
load("pwr.mutual.postD.RData")
pwr
load("pwr.mutual.postlin.RData")
pwr

