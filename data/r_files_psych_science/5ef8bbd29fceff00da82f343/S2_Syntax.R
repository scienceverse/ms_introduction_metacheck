#install packages install.packages(c("psych","lme4","car","mediation","lavaan","BayesFactor","coda") ) 
#load packages
library(foreign, pos=4)
library(psych)
library(GPArotation)
library(lme4)
library(car)
library(mediation)
library(lavaan)
library(BayesFactor)
library(coda)
library(sjstats)
library(lavaan)
library(semTools)
library(readr)
library(dplyr)

#read data
#select OSF data file 2, or 'various cuts' for options permitting 
#exclusion of inexperienced meditators.
datR <- read.csv(choose.files())

#### BTA CFAs
# one-f
one.model <- ' pooled  =~ bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10 + bta11 + bta12 + bta13 + bta14 + bta15 + bta16'
fit <- cfa(one.model, data=datR,std.lv=TRUE)
summary(fit, fit.measures=TRUE,standardized=TRUE)
# two-f
two.model <- ' f1  =~ bta01 + bta02 + bta03 + bta04 + bta05 + bta06 + bta07 + bta08 + bta09 + bta10
f2 =~ bta11 + bta12 + bta13 + bta14 + bta15 + bta16'
fit2 <- cfa(two.model, data=datR, std.lv=TRUE)
summary(fit2, fit.measures=TRUE,standardized=TRUE)
anova(fit,fit2)

# one-f
one.model <- ' pooled  =~ cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16 + cni17 + cni18 + cni19 + cni20 + cni21 + cni22'
fit <- cfa(one.model, data=datR,std.lv=TRUE)
summary(fit, fit.measures=TRUE,standardized=TRUE)
# two-f
two.model <- ' f1  =~ cni01 + cni02 + cni03 + cni04 + cni05 + cni06 + cni07 + cni08 + cni09 + cni10 + cni11 + cni12 + cni13 + cni14 + cni15 + cni16
f2 =~ cni17 + cni18 + cni19 + cni20 + cni21 + cni22'
fit2 <- cfa(two.model, data=datR, std.lv=TRUE)
summary(fit2, fit.measures=TRUE,standardized=TRUE)
anova(fit,fit2)

############# OPTIONAL! REMOVAL OF INEXPERIENCED STUDY 2 PARTICIPANTS! 
# Choice A
datR <- datR %>% filter(NoZero == 0)
# Choice A
datR <- datR %>% filter(NoSD == 0)
# Choice A
datR <- datR %>% filter(NoSDorNA == 0)

#compute means
datR$imp <- rowMeans(datR[c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")], na.rm = T)
datR$bta <- rowMeans(datR[c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")], na.rm = T)
datR$cni <- rowMeans(datR[c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")], na.rm = T)
#expansion items
datR$impex <- rowMeans(datR[c("imp11","imp12","imp13","imp14","imp15","imp16")], na.rm = T)
datR$btaex <- rowMeans(datR[c("bta11","bta12","bta13","bta14","bta15","bta16")], na.rm = T)
datR$cniex <- rowMeans(datR[c("cni17","cni18","cni19","cni20","cni21","cni22")], na.rm = T)
#Rescale Rosenberg
datR$rse3r <- 100 - datR$rse3
datR$rse5r <- 100 - datR$rse5
datR$rse8r <- 100 - datR$rse8
datR$rse9r <- 100 - datR$rse9
datR$rse10r <- 100 - datR$rse10
datR$se <- rowMeans(datR[c("rse1","rse2","rse3r","rse4","rse5r","rse6","rse7","rse8","rse9r","rse10r")], na.rm = T)
#Rescale Hedonic wellbeing #5, 6, 7, 8, 9 --> 2, 4, 6, 8, 9 in ours
datR$aff02r <- 100 - datR$aff02
datR$aff04r <- 100 - datR$aff04
datR$aff06r <- 100 - datR$aff06
datR$aff08r <- 100 - datR$aff08
datR$aff09r <- 100 - datR$aff09
datR$aff <- rowMeans(datR[c("aff01","aff02r","aff03","aff04r","aff05","aff06r","aff07","aff08r" ,"aff09r")], na.rm = T)
datR$swl <- rowMeans(datR[c("swl01","swl02","swl03","swl04","swl05")], na.rm = T) 
datR$hed <- rowMeans(datR[c("aff","swl")], na.rm = T)
#Rescale Eudemonic wellbeing #2, 3, 6, 7, 10, 12 --> 2, 4, 6, 7, 9, 11 in ours
datR$eud02r <- 100 - datR$eud02
datR$eud04r <- 100 - datR$eud04
datR$eud06r <- 100 - datR$eud06
datR$eud07r <- 100 - datR$eud07
datR$eud09r <- 100 - datR$eud09
datR$eud11r <- 100 - datR$eud11
datR$eud <- rowMeans(datR[c("eud01","eud02r","eud03","eud04r","eud05","eud06r","eud07r","eud08","eud09r","eud10","eud11r","eud12")], na.rm = T)

#preparation for alpha computation per assessment 
mzp.1 <- subset(datR, subset = mzp == 1)
mzp.2 <- subset(datR, subset = mzp == 2)
mzp.3 <- subset(datR, subset = mzp == 3) 
mzp.4 <- subset(datR, subset = mzp == 4)

#alpha: self-centrality
omega(mzp.1[c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")])  
omega(mzp.2[c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")])  
omega(mzp.3[c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")])  
omega(mzp.4[c("imp01","imp02","imp03","imp04","imp05","imp06","imp07","imp08","imp09","imp10")]) 

#alpha: better-than-average
omega(mzp.1[c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")]) 
omega(mzp.2[c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")])
omega(mzp.3[c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")])
omega(mzp.4[c("bta01","bta02","bta03","bta04","bta05","bta06","bta07","bta08","bta09","bta10")])

#alpha: communal narcissism 
omega(mzp.1[c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")])
omega(mzp.2[c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")]) 
omega(mzp.3[c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")]) 
omega(mzp.4[c("cni01","cni02","cni03","cni04","cni05","cni06","cni07","cni08","cni09","cni10","cni11","cni12","cni13","cni14","cni15","cni16")]) 

#alpha: self-esteem
omega(mzp.1[c("rse1","rse2","rse3r","rse4","rse5r","rse6","rse7","rse8","rse9r","rse10r")], na.rm = T) 
omega(mzp.2[c("rse1","rse2","rse3r","rse4","rse5r","rse6","rse7","rse8","rse9r","rse10r")])
omega(mzp.3[c("rse1","rse2","rse3r","rse4","rse5r","rse6","rse7","rse8","rse9r","rse10r")]) 
omega(mzp.4[c("rse1","rse2","rse3r","rse4","rse5r","rse6","rse7","rse8","rse9r","rse10r")], na.rm = T) 

#alpha: hedonic well-being - affective component
omega(mzp.1[c("aff01","aff02r","aff03","aff04r","aff05","aff06r","aff07","aff08r" ,"aff09r")], na.rm = T)
omega(mzp.2[c("aff01","aff02r","aff03","aff04r","aff05","aff06r","aff07","aff08r" ,"aff09r")], na.rm = T)
omega(mzp.3[c("aff01","aff02r","aff03","aff04r","aff05","aff06r","aff07","aff08r" ,"aff09r")], na.rm = T) 
omega(mzp.4[c("aff01","aff02r","aff03","aff04r","aff05","aff06r","aff07","aff08r" ,"aff09r")], na.rm = T) 

#alpha: hedonic well-being - cognitive component
omega(mzp.1[c("swl01","swl02","swl03","swl04","swl05")])
omega(mzp.2[c("swl01","swl02","swl03","swl04","swl05")])
omega(mzp.3[c("swl01","swl02","swl03","swl04","swl05")])
omega(mzp.4[c("swl01","swl02","swl03","swl04","swl05")])

#correlation: affective and cognitive components of hedonic well-being
lm(scale(mzp.1$aff) ~ scale(mzp.1$swl))
lm(scale(mzp.2$aff) ~ scale(mzp.2$swl))
lm(scale(mzp.3$aff) ~ scale(mzp.3$swl))
lm(scale(mzp.4$aff) ~ scale(mzp.4$swl))

#alpha: eudemonic well-being
omega(mzp.1[c("eud01","eud02r","eud03","eud04r","eud05","eud06r","eud07r","eud08","eud09r","eud10","eud11r","eud12")]) 
omega(mzp.2[c("eud01","eud02r","eud03","eud04r","eud05","eud06r","eud07r","eud08","eud09r","eud10","eud11r","eud12")])
omega(mzp.3[c("eud01","eud02r","eud03","eud04r","eud05","eud06r","eud07r","eud08","eud09r","eud10","eud11r","eud12")]) 
omega(mzp.4[c("eud01","eud02r","eud03","eud04r","eud05","eud06r","eud07r","eud08","eud09r","eud10","eud11r","eud12")]) 

#alpha: expansion self-centrality
omega(mzp.1[c("imp11","imp12","imp13","imp14","imp15","imp16")]) 
omega(mzp.2[c("imp11","imp12","imp13","imp14","imp15","imp16")])
omega(mzp.3[c("imp11","imp12","imp13","imp14","imp15","imp16")]) 
omega(mzp.4[c("imp11","imp12","imp13","imp14","imp15","imp16")]) 

#alpha: expansion BTA
omega(mzp.1[c("bta11","bta12","bta13","bta14","bta15","bta16")]) 
omega(mzp.2[c("bta11","bta12","bta13","bta14","bta15","bta16")])
omega(mzp.3[c("bta11","bta12","bta13","bta14","bta15","bta16")]) 
omega(mzp.4[c("bta11","bta12","bta13","bta14","bta15","bta16")])

#alpha: expansion communal narcissism (communal goals)
omega(mzp.1[c("cni17","cni18","cni19","cni20","cni21","cni22")]) 
omega(mzp.2[c("cni17","cni18","cni19","cni20","cni21","cni22")])
omega(mzp.3[c("cni17","cni18","cni19","cni20","cni21","cni22")]) 
omega(mzp.4[c("cni17","cni18","cni19","cni20","cni21","cni22")])

#standardize variables
datR$z.imp <- scale(datR$imp)
datR$z.bta <- scale(datR$bta)
datR$z.cni <- scale(datR$cni)
datR$z.se <- scale(datR$se)
datR$z.hed <- scale(datR$hed)
datR$z.eud <- scale(datR$eud)
datR$z.impex <- scale(datR$impex)
datR$z.btaex <- scale(datR$btaex)
datR$z.cniex <- scale(datR$cniex)
datR$z.bta11 <- scale(datR$bta11)
datR$z.bta12 <- scale(datR$bta12)
datR$z.bta13 <- scale(datR$bta13)
datR$z.bta14 <- scale(datR$bta14)
datR$z.bta15 <- scale(datR$bta15)
datR$z.bta16 <- scale(datR$bta16)
  
#beginners vs. advanced
datR.b <- aggregate(expMed ~ vpn, datR, mean) 
mean(datR.b$exp, na.rm = T)
sd(datR.b$exp, na.rm = T)
datR.b$z.exp <- scale(datR.b$exp) 
datR.b$z.exp.max <- datR.b$z.exp - 1 
datR.b$z.exp.min <- datR.b$z.exp + 1
datR.b$exp <-NULL
datR <- merge(datR, datR.b, by = "vpn", all = T) 
remove(datR.b) 

#low vs high motivation
datR.b <- aggregate(intMed ~ vpn, datR, mean)
datR.b$z.int <- scale(datR.b$intMed) 
datR.b$z.int.max <- datR.b$intMed - 1 
datR.b$z.int.min <- datR.b$intMed + 1
datR.b$int <-NULL
datR <- merge(datR, datR.b, by = "vpn", all = T) 
remove(datR.b) 

#self-enhancement g-factor
mod <- 'z.g =~ z.bta + z.cni + z.se'
fit <- cfa(mod, data=datR, missing = "FIML", std.lv=T) 
summary(fit, fit.measures=T, standardized=T) 
z.g <- scale(predict(fit, newdata = datR)) 
datR <- data.frame(datR,z.g) 
remove(fit,z.g,mod) 

#self-enhancement g-factor for well-being analysis (without self-esteem) 
mod <- 'z.g2 =~ 1*z.bta + 1*z.cni'
fit <- cfa(mod, data=datR, missing = "FIML", std.lv=T)
summary(fit, fit.measures=T, standardized=T) 
z.g2 <- scale(predict(fit, newdata = datR))
datR <- data.frame(datR,z.g2)
remove(fit,z.g2,mod)

#group-mean center variables
datR$z.imp.grp <- datR$z.imp - (ave(datR$z.imp, datR$vpn)) 
datR$z.g.grp <- datR$z.g - (ave(datR$z.g, datR$vpn)) 
datR$z.g2.grp <- datR$z.g2 - (ave(datR$z.g2, datR$vpn)) 

#define vpn and cond as factors
datR$vpn <- as.factor(datR$vpn)
datR$cond <- as.factor(datR$cond)

#self-centrality
imp.mlm <- lmer(z.imp ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.imp")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(z.imp ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond-1"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond-1"] <= 0)
imp.posneg <- imp.pos / imp.neg
summary(imp.mlm)
print(imp.ci)
print("cond > 0:")
imp.pos
print("cond < 0:")
imp.neg
print("BF:")
imp.posneg
remove(imp.mlm,imp.ci,imp.bf,imp.post,imp.pos,imp.neg,imp.posneg) 

#self-centrality expanded
imp.mlm <- lmer(z.impex ~ cond + (1 | vpn), data=datR, verbose=2)
summary(imp.mlm)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
imp.ci
datR2 <- datR[,c("vpn","cond","z.impex")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(z.impex ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond-1"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond-1"] <= 0)
imp.posneg <- imp.pos / imp.neg
summary(imp.mlm)
print(imp.ci)
print("cond > 0:")
imp.pos
print("cond < 0:")
imp.neg
print("BF:")
imp.posneg
remove(imp.mlm,imp.ci,imp.bf,imp.post,imp.pos,imp.neg,imp.posneg) 


#g-factor
g.mlm <- lmer(z.g ~ cond + (1 | vpn), data=datR, verbose=2)
g.ci <- confint(g.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.g")]
datR2 <- na.omit(datR2)
g.bf <- lmBF(z.g ~ cond + vpn, data = datR2, whichRandom = "vpn") 
g.post <- posterior(g.bf, iterations = 300000)
g.pos <- sum(as.mcmc(g.post)[,"cond-1"] > 0)
g.neg <- sum(as.mcmc(g.post)[,"cond-1"] <= 0)
g.posneg <- g.pos / g.neg
summary(g.mlm)
print(g.ci)
print("cond > 0:")
g.pos
print("cond < 0:")
g.neg
print("BF:")
g.posneg
remove(g.mlm,g.ci,g.bf,g.post,g.pos,g.neg,g.posneg) 

#better-than-average
bta.mlm <- lmer(z.bta ~ cond + (1 | vpn), data=datR, verbose=2)
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average expansion
bta.mlm <- lmer(z.btaex ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.btaex")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.btaex ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average: appearance(13)
bta.mlm <- lmer(z.bta13 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta13")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta13 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average:intelligence (11)
bta.mlm <- lmer(z.bta11 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta11")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta11 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average:charm(12)
bta.mlm <- lmer(z.bta12 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta12")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta12 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average:enjoy_company(14)
bta.mlm <- lmer(z.bta14 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta14")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta14 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average:persuasive(15)
bta.mlm <- lmer(z.bta15 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta15")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta15 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#better-than-average:funny(16)
bta.mlm <- lmer(z.bta16 ~ cond + (1 | vpn), data=datR, verbose=2) 
bta.ci <- confint(bta.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta16")]
datR2 <- na.omit(datR2)
bta.bf <- lmBF(z.bta16 ~ cond + vpn, data = datR2, whichRandom = "vpn")
bta.post <- posterior(bta.bf, iterations = 300000)
bta.pos <- sum(as.mcmc(bta.post)[,"cond-1"] > 0)
bta.neg <- sum(as.mcmc(bta.post)[,"cond-1"] <= 0)
bta.posneg <- bta.pos / bta.neg
summary(bta.mlm)
print(bta.ci)
print("cond > 0:")
bta.pos
print("cond < 0:")
bta.neg
print("BF:")
bta.posneg

#communal narcissism
cni.mlm <- lmer(z.cni ~ cond + (1 | vpn), data=datR, verbose=2)
cni.ci <- confint(cni.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.cni")]
datR2 <- na.omit(datR2)
cni.bf <- lmBF(z.cni ~ cond + vpn, data = datR2, whichRandom = "vpn")
cni.post <- posterior(cni.bf, iterations = 300000)
cni.pos <- sum(as.mcmc(cni.post)[,"cond-1"] > 0)
cni.neg <- sum(as.mcmc(cni.post)[,"cond-1"] <= 0)
cni.posneg <- cni.pos / cni.neg
summary(cni.mlm)
print(cni.ci)
print("cond > 0:")
cni.pos
print("cond < 0:")
cni.neg
print("BF:")
cni.posneg

#communal narcissism expansion
cni.mlm <- lmer(z.cniex ~ cond + (1 | vpn), data=datR, verbose=2)
cni.ci <- confint(cni.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.cniex")]
datR2 <- na.omit(datR2)
cni.bf <- lmBF(z.cniex ~ cond + vpn, data = datR2, whichRandom = "vpn")
cni.post <- posterior(cni.bf, iterations = 300000)
cni.pos <- sum(as.mcmc(cni.post)[,"cond-1"] > 0)
cni.neg <- sum(as.mcmc(cni.post)[,"cond-1"] <= 0)
cni.posneg <- cni.pos / cni.neg
summary(cni.mlm)
print(cni.ci)
print("cond > 0:")
cni.pos
print("cond < 0:")
cni.neg
print("BF:")
cni.posneg


#self-esteem
se.mlm <- lmer(z.se ~ cond + (1 | vpn), data=datR, verbose=2)
se.ci <- confint(se.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.se")]
datR2 <- na.omit(datR2)
se.bfNull <- lmBF(z.se ~ vpn, data = datR2, whichRandom = "vpn") 
se.bf <- lmBF(z.se ~ cond + vpn, data = datR2, whichRandom = "vpn") 
se.post <- posterior(se.bf, iterations = 300000)
se.pos <- sum(as.mcmc(se.post)[,"cond-1"] > 0)
se.neg <- sum(as.mcmc(se.post)[,"cond-1"] <= 0)
se.posneg <- se.pos / se.neg
summary(se.mlm)
print(se.ci)
print("cond > 0:")
se.pos
print("cond < 0:")
se.neg
print("BF:")
se.posneg
se.bf
se.bfNull
options(scipen=100)
remove(se.mlm,se.ci,se.bf,se.post,se.pos,se.neg,se.posneg) 

#hedonic well-being
hed.mlm <- lmer(z.hed ~ cond + (1 | vpn), data=datR, verbose=2)
hed.ci <- confint(hed.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.hed")]
datR2 <- na.omit(datR2)
hed.bf <- lmBF(z.hed ~ cond + vpn, data = datR2, whichRandom = "vpn")
hed.post <- posterior(hed.bf, iterations = 300000) 
hed.pos <- sum(as.mcmc(hed.post)[,"cond-1"] > 0) 
hed.neg <- sum(as.mcmc(hed.post)[,"cond-1"] <= 0) 
hed.posneg <- hed.pos / hed.neg
summary(hed.mlm) 
print(hed.ci)
print("cond > 0:")
hed.pos
print("cond < 0:")
hed.neg
print("BF:")
hed.posneg 
remove(hed.mlm,hed.ci,hed.bf,hed.post,hed.pos,hed.neg,hed.posneg) 

#eudemonic well-being
eud.mlm <- lmer(z.eud ~ cond + (1 | vpn), data=datR, verbose=2)
eud.ci <- confint(eud.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.eud")]
datR2 <- na.omit(datR2)
eud.bf <- lmBF(z.eud ~ cond + vpn, data = datR2, whichRandom = "vpn")
eud.post <- posterior(eud.bf, iterations = 30000)
eud.pos <- sum(as.mcmc(eud.post)[,"cond-1"] > 0)
eud.neg <- sum(as.mcmc(eud.post)[,"cond-1"] <= 0)
eud.posneg <- eud.pos / eud.neg
summary(eud.mlm)
print(eud.ci)
print("cond > 0:")
eud.pos
print("cond < 0:")
eud.neg
print("BF:")
eud.posneg
remove(eud.mlm,eud.ci,eud.bf,eud.post,eud.pos,eud.neg,eud.posneg) 


#self-centrality x expertise
imp.exp.mlm <- lmer(z.imp ~ cond * z.exp + (1 | vpn), data=datR, verbose=2) 
imp.exp.ci <- confint(imp.exp.mlm, method = "boot", nsim = 1000) 
summary(imp.exp.mlm)
print(imp.exp.ci) 

#Higher expertise
imp.exp.max.mlm <- lmer(z.imp ~ cond * z.exp.max + (1 | vpn), data=datR, verbose=2) 
imp.exp.max.ci <- confint(imp.exp.max.mlm, method = "boot", nsim = 1000) 
summary(imp.exp.max.mlm)
print(imp.exp.max.ci) 

#Lower expertise
imp.exp.min.mlm <- lmer(z.imp ~ cond * z.exp.min + (1 | vpn), data=datR, verbose=2) 
imp.exp.min.ci <- confint(imp.exp.min.mlm, method = "boot", nsim = 1000) 
summary(imp.exp.min.mlm)
print(imp.exp.min.ci) 

# Higher - BF
datR2 <- datR[,c("vpn","cond","z.imp","z.exp.max")]
datR2 <- na.omit(datR2)
datR2$cond <- as.numeric(datR2$cond)
datR2$int <- datR2$cond * datR2$z.exp.max
zimp.bf <- lmBF(z.imp ~ cond*z.exp.max, data=datR2, whichRandom = "vpn")
zsimp.bf <- lmBF(z.imp ~ cond+z.exp.max, data=datR2, whichRandom = "vpn") 
zimp.bf/zsimp.bf
zimp.post <- posterior(zimp.bf, iterations = 300000)
zimp.pos <- sum(as.mcmc(zimp.post)[,"cond"] > 0)
zimp.neg <- sum(as.mcmc(zimp.post)[,"cond"] <= 0)
zimp.posneg <- zimp.pos / zimp.neg
summary(imp.exp.mlm)
print("BF:")
zimp.posneg
# Lower - BF
datR2 <- datR[,c("vpn","cond","z.imp","z.exp.min")]
datR2 <- na.omit(datR2)
datR2$cond <- as.numeric(datR2$cond)
datR2$int <- datR2$cond * datR2$z.exp.min
zimp.bf <- lmBF(z.imp ~ cond*z.exp.min, data=datR2, whichRandom = "vpn")
zsimp.bf <- lmBF(z.imp ~ cond+z.exp.min, data=datR2, whichRandom = "vpn") 
zimp.bf/zsimp.bf
zimp.post <- posterior(zimp.bf, iterations = 300000)
zimp.pos <- sum(as.mcmc(zimp.post)[,"cond"] > 0)
zimp.neg <- sum(as.mcmc(zimp.post)[,"cond"] <= 0)
zimp.posneg <- zimp.pos / zimp.neg
summary(imp.exp.mlm)
print("BF:")
zimp.posneg

#g-factor x expertise
g.exp.mlm <- lmer(z.g ~ cond * z.exp + (1 | vpn), data=datR, verbose=2) 
g.exp.ci <- confint(g.exp.mlm, method = "boot", nsim = 1000) 
summary(g.exp.mlm)
print(g.exp.ci) 

#self-centrality x motivation
imp.int.mlm <- lmer(z.imp ~ cond * z.int + (1 | vpn), data=datR, verbose=2) 
imp.int.ci <- confint(imp.int.mlm, method = "boot", nsim = 1000) 
summary(imp.int.mlm)
print(imp.int.ci) 

#g-factor x motivation
g.int.mlm <- lmer(z.g ~ cond * z.int + (1 | vpn), data=datR, verbose=2) 
g.int.ci <- confint(g.int.mlm, method = "boot", nsim = 1000) 
summary(g.int.mlm)
print(g.int.ci) 

#multi-level mediation: meditate -> self-centrality -> s-enh
datR2 <- datR[,c("vpn","cond","z.imp.grp","z.g","z.imp","z.imp.grp")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.g ~ cond + z.imp.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.imp ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.imp.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: meditate -> s-enh -> hedonic well-being
datR2 <- datR[,c("vpn","cond","z.g.grp","z.g","z.hed")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.hed ~ cond + z.g.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: meditate -> s-enh -> eudemonic well-being
datR2 <- datR[,c("vpn","cond","z.g.grp","z.g","z.eud")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.eud ~ cond + z.g.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g.grp") 
summary(med3)
remove(med1,med2,med3) 