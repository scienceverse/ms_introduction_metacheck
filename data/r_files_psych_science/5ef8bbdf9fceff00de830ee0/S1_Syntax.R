library(haven)
# select file (OSF_S1)
df <- read_sav(choose.files())

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Calculate Self-centrality
library(ltm)
library(sjstats)
df2 <- completeFun(df,5:8)
cronbach.alpha(df2[,c(5:8)])
df$SC <- mean_n(df[,c(5:8)],3)

# Calculate BTA
df2 <- completeFun(df,9:12)
cronbach.alpha(df2[,c(9:12)])
df$BTA_Basic <- mean_n(df[,c(9:12)],3)

df2 <- completeFun(df,13:14)
cronbach.alpha(df2[,c(13:14)])
df$BTA_Exp <- mean_n(df[,c(13:14)],1)

cor.test(df$BTA_Basic,df$BTA_Exp)
library(lavaan)

# one-f
one.model <- ' pooled  =~ BTA_1 + BTA_2 + BTA_3 + BTA_4 + BTA_5 + BTA_6'
fit <- cfa(one.model, data=df,std.lv=TRUE)
summary(fit, fit.measures=TRUE,standardized=TRUE)
# two-f
two.model <- ' f1  =~ BTA_1 + BTA_2 + BTA_3 + BTA_4
f2 =~ BTA_5 + BTA_6'
fit2 <- cfa(two.model, data=df, std.lv=TRUE)
summary(fit2, fit.measures=TRUE,standardized=TRUE)
anova(fit,fit2)

# Calculate Narcissism
df2 <- completeFun(df,16:19)
cronbach.alpha(df2[,c(16:19)])
df$Narcissism <- mean_n(df[,c(16:19)],3)

# Split out people who did not attend
library(dplyr)
df <- df %>% filter((attendance == 1) | (attendance == 2))

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

datR <- df

#standardize variables
datR$z.cen <- scale(datR$SC)
datR$z.bta <- scale(datR$BTA_Basic)
datR$z.btaex <- scale(datR$BTA_Exp)
datR$z.cni <- scale(datR$Narcissism)
datR$z.se <- scale(datR$Selfesteem_1)

mod <- 'z.g =~ z.bta + z.cni + z.se'
fit <- cfa(mod, data=datR, missing = "FIML",std.lv=T) 
summary(fit, fit.measures=T, standardized=T) 
z.g <- scale(predict(fit, newdata = datR)) 
datR <- data.frame(datR,z.g) 

mod <- 'z.g =~ z.bta + z.cni'
fit <- cfa(mod, data=datR, missing = "FIML",std.lv=T) 
summary(fit, fit.measures=T, standardized=T) 
z.g2 <- scale(predict(fit, newdata = datR)) 
datR <- data.frame(datR,z.g2) 

#group-mean center variables
datR$z.cen.grp <- datR$z.cen - (ave(datR$z.cen, datR$PID)) 
datR$z.g.grp <- datR$z.g - (ave(datR$z.g, datR$PID)) 
datR$z.g2.grp <- datR$z.g.2 - (ave(datR$z.g.2, datR$PID)) 
datR$z.cni.grp <- datR$z.cni - (ave(datR$z.cni, datR$PID)) 
datR$z.se.grp <- datR$z.se - (ave(datR$z.se, datR$PID))
datR$z.bta.grp <- datR$z.bta - (ave(datR$z.bta, datR$PID)) 

#define PID and cond as factors
datR$PID <- as.factor(datR$PID)
datR$attendance <- as.factor(datR$attendance)
datR$attendance <- ifelse(datR$attendance == 2, 1, 0)
datR$z.imp <- datR$z.cen
datR$cond <- datR$attendance
datR$vpn <- datR$PID

#self-centrality
imp.mlm <- lmer(z.imp ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.imp")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(z.imp ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
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
g.pos <- sum(as.mcmc(g.post)[,"cond"] > 0)
g.neg <- sum(as.mcmc(g.post)[,"cond"] <= 0)
g.posneg <- g.pos / g.neg
summary(g.mlm)
print(g.ci)
print("cond > 0:")
g.pos
print("cond < 0:")
g.neg
print("BF:")
g.posneg

#se
g.mlm <- lmer(z.se ~ cond + (1 | vpn), data=datR, verbose=2)
g.ci <- confint(g.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.se")]
datR2 <- na.omit(datR2)
g.bf <- lmBF(z.se ~ cond + vpn, data = datR2, whichRandom = "vpn") 
g.post <- posterior(g.bf, iterations = 300000)
g.pos <- sum(as.mcmc(g.post)[,"cond"] > 0)
g.neg <- sum(as.mcmc(g.post)[,"cond"] <= 0)
g.posneg <- g.pos / g.neg
summary(g.mlm)
print(g.ci)
print("cond > 0:")
g.pos
print("cond < 0:")
g.neg
print("BF:")
g.posneg

#narcissism
g.mlm <- lmer(z.cni ~ cond + (1 | vpn), data=datR, verbose=2)
g.ci <- confint(g.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.cni")]
datR2 <- na.omit(datR2)
g.bf <- lmBF(z.cni ~ cond + vpn, data = datR2, whichRandom = "vpn") 
g.post <- posterior(g.bf, iterations = 300000)
g.pos <- sum(as.mcmc(g.post)[,"cond"] > 0)
g.neg <- sum(as.mcmc(g.post)[,"cond"] <= 0)
g.posneg <- g.pos / g.neg
summary(g.mlm)
print(g.ci)
print("cond > 0:")
g.pos
print("cond < 0:")
g.neg
print("BF:")
g.posneg

#BTA
imp.mlm <- lmer(z.bta ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.bta")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(z.bta ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
imp.posneg <- imp.pos / imp.neg
summary(imp.mlm)
print(imp.ci)
print("cond > 0:")
imp.pos
print("cond < 0:")
imp.neg
print("BF:")
imp.posneg

#BTAEX
imp.mlm <- lmer(z.btaex ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","z.btaex")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(z.btaex ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
imp.posneg <- imp.pos / imp.neg
summary(imp.mlm)
print(imp.ci)
print("cond > 0:")
imp.pos
print("cond < 0:")
imp.neg
print("BF:")
imp.posneg

######
# Moderation
######

#beginners vs. advanced
datR.b <- aggregate(experience_1 ~ vpn, datR, mean) 
datR.b$z.exp <- scale(datR.b$experience_1) 
datR.b$z.exp.max <- datR.b$z.exp - 1 
datR.b$z.exp.min <- datR.b$z.exp + 1
datR.b$exp <-NULL
datR <- merge(datR, datR.b, by = "vpn", all = T) 
remove(datR.b) 

#low vs high motivation
datR.b <- aggregate(motivation_1 ~ vpn, datR, mean)
datR.b$z.int <- scale(datR.b$motivation_1) 
datR.b$z.int.max <- datR.b$z.int - 1 
datR.b$z.int.min <- datR.b$z.int + 1
datR.b$int <-NULL
datR <- merge(datR, datR.b, by = "vpn", all = T) 
remove(datR.b) 

#self-centrality x expertise
cen.exp.mlm <- lmer(z.imp ~ cond * z.exp + (1 | vpn), data=datR, verbose=2) 
confint(cen.exp.mlm, method = "boot", nsim = 1000) 
summary(cen.exp.mlm)
print(cen.exp.ci) 

#g-factor x experience
g.exp.mlm <- lmer(z.g ~ cond * z.exp + (1 | vpn), data=datR, verbose=2) 
g.exp.ci <- confint(g.exp.mlm, method = "boot", nsim = 1000) 
summary(g.exp.mlm)
print(g.exp.ci) 

#self-centrality x motivation
imp.int.mlm <- lmer(z.imp ~ cond * z.int + (1 | vpn), data=datR, verbose=2) 
confint(imp.int.mlm, method = "boot", nsim = 1000) 
summary(imp.int.mlm)
print(imp.int.ci) 

#Higher motivation
imp.int.max.mlm <- lmer(z.imp ~ cond * z.int.max + (1 | vpn), data=datR, verbose=2) 
imp.int.max.ci <- confint(imp.int.max.mlm, method = "boot", nsim = 1000) 
summary(imp.int.max.mlm)
print(imp.int.max.ci) 

datR2 <- datR[,c("vpn","cond","z.imp","z.int.max")]
datR2 <- na.omit(datR2)
datR2$cond <- as.numeric(datR2$cond)
datR2$int <- datR2$cond * datR2$z.int.max
zimp.bf <- lmBF(z.imp ~ cond*z.int.max, data=datR2, whichRandom = "vpn")
zimp.post <- posterior(zimp.bf, iterations = 300000)
zimp.pos <- sum(as.mcmc(zimp.post)[,"cond"] > 0)
zimp.neg <- sum(as.mcmc(zimp.post)[,"cond"] <= 0)
zimp.posneg <- zimp.pos / zimp.neg
summary(imp.int.max.mlm)
print("BF:")
zimp.posneg

#Lower motivation
imp.int.min.mlm <- lmer(z.imp ~ cond * z.int.min + (1 | vpn), data=datR, verbose=2) 
imp.int.min.ci <- confint(imp.int.min.mlm, method = "boot", nsim = 1000) 
summary(imp.int.min.mlm)
print(imp.int.min.ci) 

datR2 <- datR[,c("vpn","cond","z.imp","z.int.min")]
datR2 <- na.omit(datR2)
datR2$cond <- as.numeric(datR2$cond)
datR2$int <- datR2$cond * datR2$z.int.min
zimp.bf <- lmBF(z.imp ~ cond*z.int.min, data=datR2, whichRandom = "vpn")
zimp.post <- posterior(zimp.bf, iterations = 300000)
zimp.pos <- sum(as.mcmc(zimp.post)[,"cond"] > 0)
zimp.neg <- sum(as.mcmc(zimp.post)[,"cond"] <= 0)
zimp.posneg <- zimp.pos / zimp.neg
summary(imp.int.min.mlm)
print("BF:")
zimp.posneg

#g-factor x motivation
g.int.mlm <- lmer(z.g ~ cond * z.int + (1 | vpn), data=datR, verbose=2) 
confint(g.int.mlm, method = "boot", nsim = 1000) 
summary(g.int.mlm)
print(g.int.ci) 

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
datR2 <- datR[,c("vpn","cond","z.g.2","z.g2.grp","z.se")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.se ~ cond + z.g2.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.g.2 ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.g2.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: yoga -> cni -> se
datR2 <- datR[,c("vpn","cond","z.cni","z.cni.grp","z.se")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.se ~ cond + z.cni.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.cni ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.cni.grp") 
summary(med3)
remove(med1,med2,med3) 

#multi-level mediation: yoga -> bta -> se
datR2 <- datR[,c("vpn","cond","z.bta","z.bta.grp","z.se")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.se ~ cond + z.bta.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.bta ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- mediate(med2, med1, treat = "cond", mediator = "z.bta.grp") 
summary(med3)
remove(med1,med2,med3) 

datR2 <- datR[,c("vpn","cond","z.bta","z.bta.grp","z.cni","z.cni.grp","z.se")]
datR2 <- na.omit(datR2)
med1 <- lmer(z.se ~ cond + z.bta.grp + z.cni.grp + (1 | vpn), data=datR2, verbose=2) 
med2 <- lmer(z.bta ~ cond + (1 | vpn), data=datR2, verbose=2)
med3 <- lmer(z.cni ~ cond + (1 | vpn), data=datR2, verbose=2)
med4 <- mediate(med2, med1, treat = "cond", mediator = "z.bta.grp")
med5 <- mediate(med3, med1, treat = "cond", mediator = "z.cni.grp")
summary(med4)
summary(med5)

####
# Indi self-cen
####

#self-centrality #1
imp.mlm <- lmer(Selfcentrality_1 ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","Selfcentrality_1")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(Selfcentrality_1 ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
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

#self-centrality #2
imp.mlm <- lmer(Selfcentrality_2 ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","Selfcentrality_2")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(Selfcentrality_2 ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
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


#self-centrality #3
imp.mlm <- lmer(Selfcentrality_3 ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","Selfcentrality_3")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(Selfcentrality_3 ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
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


#self-centrality #4
imp.mlm <- lmer(Selfcentrality_4 ~ cond + (1 | vpn), data=datR, verbose=2)
imp.ci <- confint(imp.mlm, method = "boot", nsim = 1000)
datR2 <- datR[,c("vpn","cond","Selfcentrality_4")]
datR2 <- na.omit(datR2)
imp.bf <- lmBF(Selfcentrality_4 ~ cond + vpn, data = datR2, whichRandom = "vpn")
imp.post <- posterior(imp.bf, iterations = 300000)
imp.pos <- sum(as.mcmc(imp.post)[,"cond"] > 0)
imp.neg <- sum(as.mcmc(imp.post)[,"cond"] <= 0)
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
