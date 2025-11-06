##########################Spatial Cueing Study (Experiment 3)###################
cf<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/carrascofinal.csv")
#use lme4 for forced-choice measures, because it also has generalized models which are needed for binomial DVs.
install.packages("lme4")
library("lme4")
#use lmerTest for the ratings, because it also outputs DFs and p-values
install.packages("lmerTest")
library("lmerTest")

#intensity choice
m1 <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1)

#DV choice (distinctiveness + vividness)
m3 <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial, verbose=T)
summary(m3)

#liking
m2 <- lmer(liking ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (arousalcontrast*targetcode+valencecontrast*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2)

#intensity rating DV (intensitycombined)
m4 <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4)

#recognition
m5 <- glmer(correct ~ targetcode*arousalcontrast+targetcode*valencecontrast +
             (1|id) +
             (1|stim),
           data=cf, verbose=T,family=binomial)
summary(m5)

#manip check
m6 <- glmer(cbind(MC,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast +
              (arousalcontrast+targetcode+valencecontrast|id) +
              (targetcode|stim),
            data=cf, family=binomial,verbose=T)
summary(m6)

#simples (positive, neutral, negative) on intensity choice
m1a <- glmer(IMORE ~ targetcode*dummynegative+targetcode*dummyneutral +
              (dummynegative+dummyneutral|id) +
              (targetcode|stim),
            data=cf, family=binomial, verbose=T)
summary(m1a)

m1b <- glmer(IMORE ~ targetcode*dummynegative+targetcode*dummypositive +
              (dummynegative+dummypositive|id) +
              (targetcode|stim),
            data=cf, family=binomial, verbose=T)
summary(m1b)

m1c <- glmer(IMORE ~ targetcode*dummypositive+targetcode*dummyneutral +
              (dummypositive+dummyneutral|id) +
              (targetcode|stim),
            data=cf, family=binomial, verbose=T)
summary(m1c)

#control for liking and recognition
m1d <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+liking +
             (1|id) +
             (1|stim),
           data=cf, family=binomial,verbose=T)
summary(m1d)

m1e <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+correct +
             (1|id) +
             (1|stim),
           data=cf, family=binomial,verbose=T)
summary(m1e)

#just the no motor response trials
m1f <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*xtrial +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1f)

#just the no manip check trials
m1g <- glmer(IMORE ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*finalfour +
             (arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, family=binomial,verbose=T)
summary(m1g)


#simples on liking
m2a <- lmer(liking ~ targetcode*dummynegative+targetcode*dummyneutral +
             (dummynegative*targetcode+dummyneutral*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2a)

m2b <- lmer(liking ~ targetcode*dummynegative+targetcode*dummypositive +
              (dummynegative*targetcode+dummypositive*targetcode|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2b)

m2c <- lmer(liking ~ targetcode*dummypositive+targetcode*dummyneutral +
              (dummypositive*targetcode+dummyneutral*targetcode|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2c)

#liking model controlling for emotional intensity
m2d <- lmer(liking ~ targetcode*arousalcontrast+targetcode*valencecontrast+IMORE +
             (arousalcontrast*targetcode+valencecontrast*targetcode|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m2d)

#simples on distinctiveness choice
m3a <- glmer(cbind(DVMORE,2) ~ targetcode*dummynegative+targetcode*dummyneutral +
               (dummynegative+dummyneutral|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3a)

m3b <- glmer(cbind(DVMORE,2) ~ targetcode*dummynegative+targetcode*dummypositive +
               (dummynegative+dummypositive|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3b)

m3c <- glmer(cbind(DVMORE,2) ~ targetcode*dummypositive+targetcode*dummyneutral +
               (dummypositive+dummyneutral|id) +
               (targetcode|stim),
             data=cf, family=binomial, verbose=T)
summary(m3c)

#control for liking
m3d <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast+liking +
               (arousalcontrast+valencecontrast|id) +
               (1|stim),
             data=cf, family=binomial, verbose=T)
summary(m3d)

#control for recognition
m3e <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast+correct +
              (arousalcontrast+valencecontrast|id) +
              (1|stim),
            data=cf, family=binomial, verbose=T)
summary(m3e)

#just the no x trials
m3f <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*xtrial +
              (arousalcontrast+valencecontrast|id) +
              (1|stim),
            data=cf, family=binomial, verbose=T)
summary(m3f)

#just the no manip check trials
m3g <- glmer(cbind(DVMORE,2) ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*finalfour +
              (arousalcontrast+valencecontrast|id) +
              (targetcode|stim),
            data=cf, family=binomial, verbose=T)
summary(m3g)

m4a <- lmer(intensitycombined ~ targetcode*dummyneutral+targetcode*dummynegative +
             (targetcode*dummyneutral+targetcode*dummynegative|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4a)

m4b <- lmer(intensitycombined ~ targetcode*dummypositive+targetcode*dummynegative +
              (targetcode*dummypositive+targetcode*dummynegative|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4b)

m4c <- lmer(intensitycombined ~ targetcode*dummyneutral+targetcode*dummypositive +
              (targetcode*dummyneutral+targetcode*dummypositive|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4c)

#controlling for liking
newm4d <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+liking +
              (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(newm4d)

#controlling for recognition
m4e <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+correct +
             (targetcode+arousalcontrast+valencecontrast|id) +
             (targetcode|stim),
           data=cf, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4e)

#only the no motor response trials
m4f <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*xtrial +
              (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4f)

#only the no manip check trials
m4g <- lmer(intensitycombined ~ targetcode*arousalcontrast+targetcode*valencecontrast+targetcode*finalfour +
              (targetcode*arousalcontrast+targetcode*valencecontrast|id) +
              (targetcode|stim),
            data=cf, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4g)




#mediation
#Distinctiveness mediates Emotional Intensity forced-choice
model.a <- glmer(DVMORE ~ targetcode + (1|id) + (targetcode|stim), data=cf,family=poisson)
model.b <- glmer(IMORE ~ DVMORE + (1|id) + (1|stim), data=cf,family=binomial)
model.y <- glmer(IMORE ~ targetcode + (1|id) + (targetcode|stim), data=cf,family=binomial)
model.m <- glmer(IMORE ~ targetcode + DVMORE + (1|id) + (targetcode|stim), data=cf,family=binomial)
saveToLocation <- "/Users/Owner/Desktop/avgDIboot.dat"
nBoot <- 10000
bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
  bout[i,3] <- fixef(refit(model.y, simulate(model.y, nsim = 1 , seed = NULL)))[2]
  bout[i,4:5] <- fixef(refit(model.m, simulate(model.m, nsim = 1 , seed = NULL)))[2:3]
}
bout[,6] <- bout[,3]-bout[,4]
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')
save(bout, file="/Users/Kellen/Desktop/Boot.dat")
probs <- c(0.025, 0.975)
quantile(bout$c_cprime, probs = probs)


#Distinctiveness mediates Emotional Intensity ratings
model.a <- glmer(DVMORE ~ targetcode + (targetcode|id) + (targetcode|stim), data=cf,family=poisson)
model.b <- lmer(intensitycombined ~ DVMORE + (DVMORE|id) + (DVMORE|stim), data=cf)
model.y <- lmer(intensitycombined ~ targetcode + (targetcode|id) + (targetcode|stim), data=cf)
model.m <- lmer(intensitycombined ~ targetcode + DVMORE + (targetcode+DVMORE|id) + (targetcode+DVMORE|stim), data=cf)
saveToLocation <- "/Users/Owner/Desktop/avgDIboot.dat"
nBoot <- 100
bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
  bout[i,3] <- fixef(refit(model.y, simulate(model.y, nsim = 1 , seed = NULL)))[2]
  bout[i,4:5] <- fixef(refit(model.m, simulate(model.m, nsim = 1 , seed = NULL)))[2:3]
}
bout[,6] <- bout[,3]-bout[,4]
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')
save(bout, file="/Users/Kellen/Desktop/Boot.dat")
probs <- c(0.025, 0.975)
quantile(bout$c_cprime, probs = probs)


######################## Attention Repetition Study (Experiment 2)###############

dose<-read.csv("/Users/Owner/Desktop/iapsdose.csv")

#use lmerTest, because it also outputs DFs and p-values
install.packages("lmerTest")
library("lmerTest")

m1 <- lmer(intensitycombined ~ target*linear+target*quadratic+target*cubic +
             (target*linear+target*quadratic+target*cubic|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m1)

#distinctiveness
m4 <- lmer(distinctiveness ~ target*linear+target*quadratic+target*cubic +
             (target*linear+target*quadratic+target*cubic|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m4)

#liking
m5 <- lmer(liking ~ target*linear+target*quadratic+target*cubic +
             (target*linear+target*quadratic+target*cubic|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m5)

#time point 1
m6 <- lmer(intensitycombined ~ target*dummy30+target*dummy60+target*dummy100 +
             (target*dummy30+target*dummy60+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m6)

#time point 2
m7 <- lmer(intensitycombined ~ target*dummy0+target*dummy60+target*dummy100 +
             (target*dummy0+target*dummy60+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m7)

#time point 3
m8 <- lmer(intensitycombined ~ target*dummy30+target*dummy0+target*dummy100 +
             (target*dummy30+target*dummy0+target*dummy100|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m8)

#time point 4
m9 <- lmer(intensitycombined ~ target*dummy30+target*dummy60+target*dummy0 +
             (target*dummy30+target*dummy60+target*dummy0|id) +
             (target|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m9)

#Distinctiveness statistically mediates intensity increase in dose study
model.a <- lmer(distinctiveness ~ target + (target|id) + (target|stim), data=dose)
model.b <- lmer(intensitycombined ~ distinctiveness + (target|id) + (target|stim), data=dose)
model.y <- lmer(intensitycombined ~ target + (target|id) + (target|stim), data=dose)
model.m <- lmer(intensitycombined ~ target + distinctiveness + (target|id) + (target|stim), data=dose)
saveToLocation <- "/Users/Kellen/Desktop/mixedBoot.dat"
nBoot <- 10000
bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
  bout[i,3] <- fixef(refit(model.y, simulate(model.y, nsim = 1 , seed = NULL)))[2]
  bout[i,4:5] <- fixef(refit(model.m, simulate(model.m, nsim = 1 , seed = NULL)))[2:3]
}
bout[,6] <- bout[,3]-bout[,4]
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')
save(bout, file="/Users/Kellen/Desktop/Boot.dat")
probs <- c(0.025, 0.975)
quantile(bout$c_cprime, probs = probs)

#means or target effects larger in one anchoring condition compared to another?
m12 <- lmer(intensitycombined ~ target +
              (target|id) +
              (target|stim),
            data=dose, verbose=T,
            control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m12)

#Supplemental analysis: do targets increase relative to baseline? Yes (but p-value is .059). nontargets clearly don't decrease (p=.906)
m3 <- lmer(intensitycombined ~ targetdummy+nontargetdummy +
             (1|id) +
             (1|stim),
           data=dose, verbose=T,
           control=lmerControl(optCtrl=list(maxfun=100000)))
summary(m3)




############################meta analysis#############################
agg<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/aggregate.csv")

agg1 <- lmer(realintensity ~ target*ArousalContrast+target*ValenceContrast +
               (target*ArousalContrast+target*ValenceContrast|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg1)
confint(agg1,method='Wald')
#study 1
agg10 <- lmer(realintensity ~ target*ArousalContrast*dummystudy2+target*ValenceContrast*dummystudy2+target*ArousalContrast*dummystudy3+target*ValenceContrast*dummystudy3 +
                (target*ArousalContrast+target*ValenceContrast|ID) +
                (target|stim)+(1|study),
              data=agg, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg10)
confint(agg10,method='Wald')
#study 2
agg11 <- lmer(realintensity ~ target*ArousalContrast*dummystudy1+target*ValenceContrast*dummystudy1+target*ArousalContrast*dummystudy3+target*ValenceContrast*dummystudy3 +
                (target*ArousalContrast+target*ValenceContrast|ID) +
                (target|stim)+(1|study),
              data=agg, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg11)
confint(agg11,method='Wald')
#study 3
agg12 <- lmer(realintensity ~ target*ArousalContrast*dummystudy1+target*ValenceContrast*dummystudy1+target*ArousalContrast*dummystudy2+target*ValenceContrast*dummystudy2 +
                (target*ArousalContrast+target*ValenceContrast|ID) +
                (target|stim)+(1|study),
              data=agg, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg12)
confint(agg12,method='Wald')

#for positive
agg2 <- lmer(realintensity ~ target*dummyneu+target*dummyneg +
               (target*dummyneu+target*dummyneg|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg2)
confint(agg2,method='Wald')

#for negative
agg3 <- lmer(realintensity ~ target*dummyneu+target*dummypos +
               (target*dummyneu+target*dummypos|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg3)
confint(agg3,method='Wald')

#for neutral
agg4 <- lmer(realintensity ~ target*dummypos+target*dummyneg +
               (target*dummypos+target*dummyneg|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg4)
confint(agg4,method='Wald')

agg7 <- lmer(liking ~ target*ArousalContrast+target*ValenceContrast +
               (target*ArousalContrast+target*ValenceContrast|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7)
confint(agg7,method='Wald')

#simples for liking; we decided to not put these in the manuscript since the interactions weren't significant.
#neut
agg7a <- lmer(liking ~ target*dummypos+target*dummyneg +
               (target*dummypos+target*dummyneg|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7a)
confint(agg7a,method='Wald')
#pos
agg7b <- lmer(liking ~ target*dummyneu+target*dummyneg +
               (target*dummyneu+target*dummyneg|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7b)
#neg
agg7c <- lmer(liking ~ target*dummypos+target*dummyneu +
               (target*dummypos+target*dummyneu|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7c)


#emotional intensity when controlling for liking
agg12 <- lmer(realintensity ~ target*ArousalContrast+target*ValenceContrast+liking +
               (target*ArousalContrast+target*ValenceContrast+liking|ID) +
               (target|stim)+(target|study),
             data=agg, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg12)
confint(agg12,method='Wald')

agg$intlikediff<-agg$realintensity-agg$liking
agg17 <- lmer(intlikediff ~ target*ArousalContrast+target*ValenceContrast +
                (target*ArousalContrast+target*ValenceContrast|ID) +
                (target|stim)+(target|study),
              data=agg, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg17)
confint(agg17,method='Wald')



#liking (it's only studies 1 and 2)
metaL<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/meta_liking.csv")
agg7 <- lmer(liking ~ target*ArousalContrast+target*ValenceContrast +
               (target*ArousalContrast+target*ValenceContrast|ID) +
               (1|stim)+(1|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg7)
confint(agg7,method='Wald')


agg17 <- lmer(liking ~ target*baseArousal+target*baseValence +
                (target*baseArousal+target*baseValence|ID) +
                (target|stim)+(1|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg17)

agg18 <- lmer(realintensity ~ target*baseArousal+target*baseValence +
                (target*baseArousal+target*baseValence|ID) +
                (target|stim)+(1|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg18)

#liking, neutral
agg8 <- lmer(liking ~ target*posdummy+target*negdummy +
               (target*posdummy+target*negdummy|ID) +
               (target|stim)+(target|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg8)
confint(agg8,method='Wald')

#liking, positive
agg9 <- lmer(liking ~ target*negdummy+target*neutdummy +
               (target*negdummy+target*neutdummy|ID) +
               (target|stim)+(target|study),
             data=metaL, verbose=T,
             control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg9)
confint(agg9,method='Wald')

#liking, negative
agg10 <- lmer(liking ~ target*posdummy+target*neutdummy +
                (target*posdummy+target*neutdummy|ID) +
                (target|stim)+(target|study),
              data=metaL, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(agg10)
confint(agg10,method='Wald')


#Exp 1, comparisons to baseline

seq11 <- lmer(intensity ~ targetdummy*ArousalContrast+targetdummy*ValenceContrast+nontargetdummy*ArousalContrast+nontargetdummy*ValenceContrast +
                (ArousalContrast+ValenceContrast|sub) +
                (1|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11)
confint(seq11,method='Wald')
#pos
seq11a <- lmer(intensity ~ targetdummy*dummyneg+targetdummy*dummyneut+nontargetdummy*dummyneg+nontargetdummy*dummyneut +
                (dummyneg+dummyneut|sub) +
                (1|IAPSImage),
              data=sequential, verbose=T,
              control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11a)
confint(seq11a,method='Wald')

#neut
seq11b <- lmer(intensity ~ targetdummy*dummyneg+targetdummy*dummypos+nontargetdummy*dummyneg+nontargetdummy*dummypos +
                 (dummyneg+dummypos|sub) +
                 (1|IAPSImage),
               data=sequential, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11b)
confint(seq11b,method='Wald')

#neg
seq11c <- lmer(intensity ~ targetdummy*dummypos+targetdummy*dummyneut+nontargetdummy*dummypos+nontargetdummy*dummyneut +
                 (dummypos+dummyneut|sub) +
                 (1|IAPSImage),
               data=sequential, verbose=T,
               control=lmerControl(optCtrl=list(maxfun=100000)))
summary(seq11c)
confint(seq11c,method='Wald')

#analyses involving exp 1c data only
ivm<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/IVtoM.csv")


#distinctiveness Exp 1 (just 1c)
model2 <- lmer(distinctive ~ target*baselinearousal+target*baselinevalence +
                 (target*baselinearousal+target*baselinevalence|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model2)

#matching distinctiveness measure (saturation and contrast)
model1 <- lmer(saturation ~ target*baselinearousal+target*baselinevalence +
                 (target*baselinearousal+target*baselinevalence|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model1)

#I believe these are the final models for the simple effects, but I need to double-check this.
#neutral simple effect on self-report
model4 <- lmer(distinctive ~ target*dummypositive+target*dummynegative +
                 (target*dummypositive+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model4)
#positive simple effect on self-report
model5 <- lmer(distinctive ~ target*dummyneutral+target*dummynegative +
                 (target*dummyneutral+target*dummynegative|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model5)
#negative simple effect on self-report
model6 <- lmer(distinctive ~ target*dummypositive+target*dummyneutral +
                 (target*dummypositive+target*dummyneutral|ID) +
                 (target|stim),
               data=ivm, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(model6)

#delay study (Experiment 1b)
delay<-read.csv("/Users/Owner/Desktop/data ms/Attention, emotion/Experiment3.csv")
#simples at session1
delay2 <- lmer(EmoIntensity ~ ArousalContrast*Target*dummytime2 + ValenceContrast*Target*dummytime2 +
                 (ArousalContrast*Target*dummytime2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay2)

#simple at delay session (supplemental material)
delay3 <- lmer(EmoIntensity ~ ArousalContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1 +
                 (ArousalContrast*Target*dummytime1 + ValenceContrast*Target*dummytime1|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay3)
confint(delay3,method='Wald')
confint(delay2,method='Wald')

#across both sessions (supplemental material)
delay1 <- lmer(EmoIntensity ~ ArousalContrast*Target*Time1or2 + ValenceContrast*Target*Time1or2 +
                 (ArousalContrast*Target*Time1or2 + ValenceContrast*Target|ID) +
                 (Target|stim),
               data=delay, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=100000)))
summary(delay1)
confint(delay1,method='Wald')
