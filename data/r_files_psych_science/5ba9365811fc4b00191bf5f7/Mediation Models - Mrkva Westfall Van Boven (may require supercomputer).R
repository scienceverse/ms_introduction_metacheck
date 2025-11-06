#####Final Mediation Analyses for Mrkva,Westfall,VanBoven

library(lme4)
onea<-read.csv("Study1a.csv")
#add these 3 files to Globus with the exact titles, before running.

##Experiment 1

#Distinctiveness statistically mediates intensity effect
model.a <- lmer(dist ~ target + (target|id) + (target|stim), data=onea)
model.b <- lmer(intensitycombined ~ dist + target+ (dist|id) + (dist|stim), data=onea)

nBoot <- 5000
bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
}
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')

probs <- c(0.025, 0.5, 0.975)
quantile(bout$ab,probs=probs)



##Experiment 2
#Distinctiveness statistically mediates intensity effect
cf<-read.csv("carrascofinal.csv")

library(lme4)
#Distinctiveness mediates Emotional Intensity forced-choice; with any random slopes, this one produced convergence errors.
model.a <- glmer(DVMORE ~ targetcode + (1|id) + (1|stim), data=cf,family=poisson)
model.b <- glmer(IMORE ~ DVMORE+targetcode + (1|id) + (1|stim), data=cf,family=binomial)

bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
}
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')

probs <- c(0.025, 0.5, 0.975)
quantile(bout$ab,probs=probs)


##Experiment 3
#Distinctiveness statistically mediates intensity effect

att<-read.csv("ATTN-EMO-LONG.csv")

model.a <- glmer(dist ~ target + (1|id)+(1|stim), family=binomial,data=att)
model.b <- glmer(IMORE ~ dist+target + (1|id)+(1|stim),family=binomial, data=att)

bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
}
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout) <-c('a','b','c','cprime','m','c_cprime','ab')

probs <- c(0.025, 0.500, 0.975)
quantile(bout$ab, probs = probs)







##Other mediation models (not in main text)


#Distinctiveness mediates Emotional Intensity ratings; convergence errors happened if I tried to add any random slopes.
model.a <- glmer(DVMORE ~ targetcode + (1|id) + (1|stim), data=cf,family=poisson)
model.b <- lmer(intensitycombined ~ DVMORE+targetcode + (1|id) + (1|stim), data=cf)

bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
}
bout[,6] <- bout[,3]-bout[,4]
bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')
probs <- c(0.025, 0.5, 0.975)

quantile(bout$ab, probs = probs)




#Experiment 1d (repetition/dose)

dose<-read.csv("iapsdose.csv")

#Distinctiveness statistically mediates intensity increase in dose study
model.a <- lmer(distinctiveness ~ target + (target|id) + (target|stim), data=dose)
model.b <- lmer(intensitycombined ~ distinctiveness+target + (distinctiveness|id) + (distinctiveness|stim), data=dose)

bout <- matrix(nrow = nBoot,ncol = 7)
for(i in 1:nBoot){
  bout[i,1] <- fixef(refit(model.a, simulate(model.a, nsim = 1 , seed = NULL)))[2]
  bout[i,2] <- fixef(refit(model.b, simulate(model.b, nsim = 1 , seed = NULL)))[2]
}

bout[,7] <- bout[,1]*bout[,2]
bout <- data.frame(bout)
colnames(bout)<-c('a','b','c','cprime','m','c_cprime','ab')
probs <- c(0.025, 0.975)
quantile(bout$ab, probs = probs)


#11.49.20
