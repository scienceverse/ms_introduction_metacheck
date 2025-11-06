library(haven)
# select file (Gebauer_Meta_analysis)
df <- read_sav(choose.files())

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Split out people who did not attend
library(dplyr)
datR <- df %>% filter(cond < 9)

############# OPTIONAL! REMOVAL OF INEXPERIENCED STUDY 2 PARTICIPANTS! 
datR <- datR %>% filter(Experience != 0)

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

#standardize variables
datR$z.cen <- scale(datR$CEN)
datR$z.bta <- scale(datR$BTA)
datR$z.cni <- scale(datR$CNI)
datR$z.se <- scale(datR$RSE)
datR$z.aff <- scale(datR$AFF)
datR$z.cog <- scale(datR$COG)
datR$z.hed <-  rowMeans(datR[c("z.aff","z.cog")], na.rm = T)
datR$z.eud <- scale(datR$EUD)

mod <- 'z.g =~ z.bta + z.cni + z.se'
fit <- cfa(mod, data=datR, missing = "FIML",std.lv=T) 
summary(fit, fit.measures=T, standardized=T) 
z.g <- scale(predict(fit, newdata = datR)) 
datR <- data.frame(datR,z.g) 

mod <- 'z.g2 =~ z.bta + z.cni'
fit <- cfa(mod, data=datR, missing = "FIML",std.lv=T) 
summary(fit, fit.measures=T, standardized=T) 
z.g2 <- scale(predict(fit, newdata = datR)) 
datR <- data.frame(datR,z.g2) 

#group-mean center variables
datR$z.cen.grp <- datR$z.cen - (ave(datR$z.cen, datR$vpn)) 
datR$z.g.grp <- datR$z.g - (ave(datR$z.g, datR$vpn)) 
datR$z.g2.grp <- datR$z.g2 - (ave(datR$z.g2, datR$vpn)) 
datR$z.cni.grp <- datR$z.cni - (ave(datR$z.cni, datR$vpn)) 
datR$z.se.grp <- datR$z.se - (ave(datR$z.se, datR$vpn))
datR$z.bta.grp <- datR$z.bta - (ave(datR$z.bta, datR$vpn)) 
datR$z.hed.grp <- datR$z.hed - (ave(datR$z.hed, datR$vpn))
datR$z.eud.grp <- datR$z.eud - (ave(datR$z.eud, datR$vpn))

datR$team <- ifelse(datR$team == 2, .5, -.5)
datR$studyNo <- ifelse(datR$studyNo == 2, .5, -.5)
datR$cond <- ifelse(datR$cond == 1, .5, -.5)

#define PID as factor
datR$vpn <- as.factor(datR$vpn)

### self-centrality
datR2 <- datR[,c("vpn","cond","z.cen","team","studyNo")]
datR2 <- na.omit(datR2)
library(nlme)
cen.mlm <- lme(z.cen ~ cond*team*studyNo, random = ~1|vpn, data=datR2)
summary(cen.mlm)
cen.ci <- intervals(cen.mlm)
print(cen.ci)
# Bayes
cen.bf <- lmBF(z.cen ~ cond*team*studyNo, data = datR2, whichRandom = "vpn")
cen.post <- posterior(cen.bf, iterations = 300000)

#A, B, C
cen.pos <- sum(as.mcmc(cen.post)[,"cond.&.team.&.studyNo"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"cond.&.team.&.studyNo"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#A, B
cen.pos <- sum(as.mcmc(cen.post)[,"cond.&.team"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"cond.&.team"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#A, C
cen.pos <- sum(as.mcmc(cen.post)[,"cond.&.studyNo"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"cond.&.studyNo"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#B, C
cen.pos <- sum(as.mcmc(cen.post)[,"team.&.studyNo"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"team.&.studyNo"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#A
cen.pos <- sum(as.mcmc(cen.post)[,"cond"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"cond"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#B
cen.pos <- sum(as.mcmc(cen.post)[,"team"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"team"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

#C
cen.pos <- sum(as.mcmc(cen.post)[,"studyNo"] > 0)
cen.neg <- sum(as.mcmc(cen.post)[,"studyNo"] <= 0)
cen.posneg <- cen.pos / cen.neg
cen.posneg

remove(cen.mlm,cen.ci,cen.bf,cen.post,cen.pos,cen.neg,cen.posneg) 

#g-factor
datR2 <- datR[,c("vpn","cond","z.g","team","studyNo")]
datR2 <- na.omit(datR2)
g.mlm <- lme(z.g ~ cond*team*studyNo, random = ~1|vpn, data=datR2)
summary(g.mlm)
g.ci <- intervals(g.mlm)
print(g.ci)
# Bayes; keep old model name so you can repeat code as per L133
cen.bf <- lmBF(z.g ~ cond*team*studyNo, data = datR2, whichRandom = "vpn")
cen.post <- posterior(cen.bf, iterations = 300000)

# Then repeat long code above (L77 to L117) to derive BF+/-

remove(cen.mlm,cen.ci,cen.bf,cen.post,cen.pos,cen.neg,cen.posneg) 

#se
datR2 <- datR[,c("vpn","cond","z.se","team","studyNo")]
datR2 <- na.omit(datR2)
se.mlm <- lme(z.se ~ cond*team*studyNo, random = ~1|vpn, data=datR2)
summary(se.mlm)
se.ci <- intervals(se.mlm)
print(se.ci)
# Bayes; keep old model name so you can repeat code as per L133
se.bf <- lmBF(z.se ~ cond*team*studyNo, data = datR2, whichRandom = "vpn")
cen.post <- posterior(se.bf, iterations = 300000)

# Then repeat long code above (L77 to L117) to derive BF+/-

remove(cen.mlm,cen.ci,cen.bf,cen.post,cen.pos,cen.neg,cen.posneg) 

#narcissism
datR2 <- datR[,c("vpn","cond","z.cni","team","studyNo")]
datR2 <- na.omit(datR2)
cni.mlm <- lme(z.cni ~ cond*team*studyNo, random = ~1|vpn, data=datR2)
summary(cni.mlm)
cni.ci <- intervals(cni.mlm)
print(cni.ci)
# Bayes; keep old model name so you can repeat code as per L133
cni.bf <- lmBF(z.cni ~ cond*team*studyNo, data = datR2, whichRandom = "vpn")
cen.post <- posterior(cni.bf, iterations = 300000)

#BTA
datR2 <- datR[,c("vpn","cond","z.bta","team","studyNo")]
datR2 <- na.omit(datR2)
bta.mlm <- lme(z.bta ~ cond*team*studyNo, random = ~1|vpn, data=datR2)
summary(bta.mlm)
bta.ci <- intervals(bta.mlm)
print(bta.ci)
# Bayes; keep old model name so you can repeat code as per L133
bta.bf <- lmBF(z.bta ~ cond*team*studyNo, data = datR2, whichRandom = "vpn")
cen.post <- posterior(bta.bf, iterations = 300000)

#HED
datR2 <- datR[,c("vpn","cond","z.hed","team","studyNo")]
datR2 <- na.omit(datR2)
hed.mlm <- lme(z.hed ~ cond*team, random = ~1|vpn, data=datR2)
summary(hed.mlm)
hed.ci <- intervals(hed.mlm)
print(hed.ci)
# Bayes; keep old model name so you can repeat code
hed.bf <- lmBF(z.hed ~ cond*team, data = datR2, whichRandom = "vpn")
cen.post <- posterior(hed.bf, iterations = 300000)

#EUD
datR2 <- datR[,c("vpn","cond","z.eud","team","studyNo")]
datR2 <- na.omit(datR2)
eud.mlm <- lme(z.eud ~ cond*team, random = ~1|vpn, data=datR2)
summary(eud.mlm)
eud.ci <- intervals(eud.mlm)
print(eud.ci)
# Bayes; keep old model name so you can repeat code
eud.bf <- lmBF(z.eud ~ cond*team, data = datR2, whichRandom = "vpn")
cen.post <- posterior(eud.bf, iterations = 300000)

##### 
# Mediation
#####

#multi-level mediation: yoga -> self-centrality -> s-enh
datR2 <- datR[,c("vpn","cond","z.cen.grp","z.g","z.cen")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.g ~ cond + z.cen.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.cen ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.cen.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: yoga -> self-enhance -> se
datR2 <- datR[,c("vpn","cond","z.g2","z.g2.grp","z.se")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.se ~ cond + z.g2.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g2 ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g2.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: yoga -> self-centrality -> s-enh
datR2 <- datR[,c("vpn","cond","z.g.grp","z.g","z.hed")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.hed ~ cond + z.g.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: yoga -> self-centrality -> s-enh
datR2 <- datR[,c("vpn","cond","z.g.grp","z.g","z.eud")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.eud ~ cond + z.g.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g.grp") 
summary(med3)
remove(med1,med2,med3) 
