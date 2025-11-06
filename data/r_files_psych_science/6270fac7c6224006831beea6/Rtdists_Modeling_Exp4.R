library(rtdists)
library(optimx)
library(lattice)
library(tidyverse)
library(retimes)
library(glba)

#clean the enviorment:
rm(list = ls())


dataGENERAL <- read.csv(header=TRUE,file.choose())

dataGENERAL$acc_emo[dataGENERAL$acc_emo==1]<-2
dataGENERAL$acc_emo[dataGENERAL$acc_emo==0]<-1

dataGENERAL <- dataGENERAL %>%
  filter(title=='SAT_self')


dataGENERAL <- dataGENERAL %>%
  filter(Condition==c('Acc','Speed'))



accData <- dataGENERAL %>%
  filter(condition=='Acc')

speedData <- dataGENERAL %>%
  filter(condition=='Speed')


###comparing groups (pic_self):
##The Null Model
myLBA_cond_null<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 8)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5")
botval <- c(0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(5,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_null, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5] 
}  

write.csv(ModelsResults, "group_NULL.csv")

##SP
myLBA_cond_sp<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[6], 
             b=x[2]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,0)
topval <- c(2,1,.4,.98,.6,2)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "group_SP.csv")

##bound:
myLBA_cond_bound<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[6]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.2)
topval <- c(2,1,.4,.98,.6,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "group_BOUND.csv")

##nond
myLBA_cond_nond<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[6], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.4)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "group_nond.csv")

##drift
myLBA_cond_drift<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.55)
topval <- c(2,1,.4,.98,.6,.98)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "group_DRIFT.csv")

##sddr
myLBA_cond_sddr<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[6],x[6]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "group_SV.csv")

#all pars GROUP:
myLBA_cond_allPARS<-function(x) {
  lik1<-dLBA(rt=photoData$adjRT, response=photoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=selfData$adjRT, response=selfData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[7],x[7]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 10)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7")
botval <- c(0,.2,.05,.55,.05,.55,.05)
topval <- c(2,1,.4,.98,.6,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(7,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(dataGENERAL$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]

}  

write.csv(ModelsResults, "group_DRIFTandSV.csv")


###comparing intnsities - mid_extreme (only for self):
##The Null Model
extremeData <- selfData %>%
  filter(mid_extreme==1)

midData <- selfData %>%
  filter(mid_extreme==0)


myLBA_cond_null<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 8)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5")
botval <- c(0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(5,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_null, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5] 
}  

write.csv(ModelsResults, "self_null_model.csv")

##SP
myLBA_cond_sp<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[6], 
             b=x[2]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,0)
topval <- c(2,1,.4,.98,.6,2)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "intensity_SP.csv")

##bound:
myLBA_cond_bound<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[6]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.2)
topval <- c(2,1,.4,.98,.6,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "intensity_BOUND.csv")

##nond
myLBA_cond_nond<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[6], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.4)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "intensity_nond.csv")

##drift
myLBA_cond_drift<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.55)
topval <- c(2,1,.4,.98,.6,.98)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "intensity_DRIFT.csv")

##sddr
myLBA_cond_sddr<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[6],x[6]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "intensity_SV.csv")

##drift & SDDR betwenn intensities:

##drift
myLBA_cond_drift_sddr<-function(x) {
  lik1<-dLBA(rt=midData$adjRT, response=midData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=extremeData$adjRT, response=extremeData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[7],x[7]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 10)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7")
botval <- c(0,.2,.05,.55,.05,.55,.05)
topval <- c(2,1,.4,.98,.6,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(7,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
}  

write.csv(ModelsResults, "intensity_DRIFT_SV.csv")


Data <- read.csv(header = T, file.choose())

selfData <- Data %>%
  mutate(adjRT=rt_emo/1000)

###comparing valence - neg_pos (only for self):
##The Null Model
posData <- selfData %>%
  filter(emotion=='p')

negData <- selfData %>%
  filter(emotion=='n')


myLBA_cond_null<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 8)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5")
botval <- c(0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(5,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_null, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5] 
}  

write.csv(ModelsResults, "self_null_model.csv")

##SP
myLBA_cond_sp<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[6], 
             b=x[2]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,0)
topval <- c(2,1,.4,.98,.6,2)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "valence_SP.csv")

##bound:
myLBA_cond_bound<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[6]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.2)
topval <- c(2,1,.4,.98,.6,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "valence_BOUND.csv")

##nond
myLBA_cond_nond<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[6], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.4)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "valence_NOND.csv")

##drift
myLBA_cond_drift<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.55)
topval <- c(2,1,.4,.98,.6,.98)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "valence_DRIFT.csv")

##sddr
myLBA_cond_sddr<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[6],x[6]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "valence_SV.csv")

##drift & SV betwenn valences:


myLBA_cond_drift_sddr<-function(x) {
  lik1<-dLBA(rt=negData$adjRT, response=negData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=posData$adjRT, response=posData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[7],x[7]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 10)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7")
botval <- c(0,.2,.05,.55,.05,.55,.05)
topval <- c(2,1,.4,.98,.6,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(7,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
  
}  

write.csv(ModelsResults, "valence_DRIFT_SV.csv")

###comparing tasks - emo_faces (only for self):

Data <- read.csv(header=TRUE,file.choose())

Data$acc_emo[Data$acc_emo==1]<-2
Data$acc_emo[Data$acc_emo==0]<-1

Data <- Data %>%
  filter(Condition=='Baseline')

emoData <- Data %>%
  filter(emo_faces==0)

facesData <- Data %>%
  filter(emo_faces==1)

##The Null Model
myLBA_cond_null<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 8)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5")
botval <- c(0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(5,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_null, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5] 
}  

write.csv(ModelsResults, "TASKS_null_model.csv")

##SP
myLBA_cond_sp<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[6], 
             b=x[2]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,0)
topval <- c(2,1,.4,.98,.6,2)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "task_SP.csv")

##bound:
myLBA_cond_bound<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[1], 
             b=x[6]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.2)
topval <- c(2,1,.4,.98,.6,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "task_BOUND.csv")

##nond
myLBA_cond_nond<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[6], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.4)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "task_NOND.csv")

##drift
myLBA_cond_drift<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.55)
topval <- c(2,1,.4,.98,.6,.98)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "task_DRIFT.csv")

##sddr
myLBA_cond_sddr<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[6],x[6]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "task_SV.csv")

##all pars betwenn intensities:


myLBA_cond_allPARS<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[8], 
             mean_v=c(1-x[9],x[9]), 
             sd_v=c(x[10],x[10]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 13)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7","par_8","par_9","par_10")
botval <- c(0,.2,.05,.55,.05,0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6,2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(10,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_allPARS, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
  ModelsResults[i,11] <- outcomes$par[8]
  ModelsResults[i,12] <- outcomes$par[9]
  ModelsResults[i,13] <- outcomes$par[10]
  
}  

write.csv(ModelsResults, "task_AllPars.csv")

#sp & bound between tasks:
myLBA_cond_spBound<-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 10)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7")
botval <- c(0,.2,.05,.55,.05,0,.2)
topval <- c(2,1,.4,.98,.6,2,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(7,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_spBound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]

  
}  

write.csv(ModelsResults, "task_SP_BOUND.csv")

##sp, bound & SV between tasks:

myLBA_cond_spBoundSV <-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[8],x[8]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 11)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7","par_8")
botval <- c(0,.2,.05,.55,.05,0,.2,.05)
topval <- c(2,1,.4,.98,.6,2,1,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(8,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_spBoundSV, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
  ModelsResults[i,11] <- outcomes$par[8]
  
  
}  

write.csv(ModelsResults, "task_SP_BOUND_SV.csv")

##sp, bound, SV & DRIFT between tasks:

myLBA_cond_spBoundSVdrift <-function(x) {
  lik1<-dLBA(rt=emoData$adjRT, response=emoData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=facesData$adjRT, response=facesData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[8],x[8]), 
             sd_v=c(x[9],x[9]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 12)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7","par_8","par_9")
botval <- c(0,.2,.05,.55,.05,0,.2,.55,.05)
topval <- c(2,1,.4,.98,.6,2,1,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(9,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_spBoundSVdrift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(Data$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
  ModelsResults[i,11] <- outcomes$par[8]
  ModelsResults[i,12] <- outcomes$par[9]
  
}  

write.csv(ModelsResults, "task_SP_BOUND_SV_DRIFT.csv")



###Comparing acc and speed conditions for EMO task (only for self):

##The Null Model

selfDat <- selfData %>%
  filter(Condition=='Acc'| Condition=='Speed')


AccData <- selfData %>%
  filter(Condition=='Acc')

SpeedData <- selfData %>%
  filter(Condition=='Speed')


myLBA_cond_null<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 8)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5")
botval <- c(0,.2,.05,.55,.05)
topval <- c(2,1,.4,.98,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(5,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_null, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5] 
}  

write.csv(ModelsResults, "self_null_model_AccSpeed.csv")

##SP
myLBA_cond_sp<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[6], 
             b=x[2]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,0)
topval <- c(2,1,.4,.98,.6,2)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "condition_SP.csv")

##bound:
myLBA_cond_bound<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[1], 
             b=x[6]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.2)
topval <- c(2,1,.4,.98,.6,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "condition_BOUND.csv")

##nond
myLBA_cond_nond<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[6], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.4)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "condition_NOND.csv")

##drift
myLBA_cond_drift<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[6],x[6]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.55)
topval <- c(2,1,.4,.98,.6,.98)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_drift, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "condition_DRIFT.csv")

##sddr
myLBA_cond_sddr<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[6],x[6]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 9)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6")
botval <- c(0,.2,.05,.55,.05,.05)
topval <- c(2,1,.4,.98,.6,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(6,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sddr, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
}  

write.csv(ModelsResults, "condition_SV.csv")

##SP & bound
myLBA_cond_sp_bound<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 10)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7")
botval <- c(0,.2,.05,.55,.05,0,.2)
topval <- c(2,1,.4,.98,.6,2,1)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(7,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp_bound, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
}  

write.csv(ModelsResults, "condition_SP_BOUND.csv")

##SP & bound & sv & nond
myLBA_cond_sp_bound_sv_nond<-function(x) {
  lik1<-dLBA(rt=AccData$adjRT, response=AccData$acc_emo, 
             A=x[1], 
             b=x[2]+x[1], 
             t0=x[3], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[5],x[5]),
             silent=TRUE)
  
  lik2<-dLBA(rt=SpeedData$adjRT, response=SpeedData$acc_emo, 
             A=x[6], 
             b=x[7]+x[6], 
             t0=x[8], 
             mean_v=c(1-x[4],x[4]), 
             sd_v=c(x[9],x[9]),
             silent=TRUE)
  
  lik <- c(lik1,lik2)
  if (any(lik == 0)) return(1e6)
  
  logL<-sum(log(lik))
  return(-logL)
}

x <- matrix(nrow = 10, ncol = 12)
ModelsResults <- as.data.frame(x)
names(ModelsResults) <- c("logl","BIC","seed","par_1","par_2","par_3","par_4","par_5","par_6","par_7","par_8","par_9")
botval <- c(0,.2,.05,.55,.05,0,.2,.05,.05)
topval <- c(2,1,.4,.98,.6,2,1,.4,.6)

for (i in 1:nrow(x))  { 
  seedi <- sample(1:1000000,1,replace = F)
  
  set.seed(seedi)
  
  start<-runif(9,botval,topval)
  
  outcomes <- nlminb(start, myLBA_cond_sp_bound_sv_nond, lower = botval, upper = topval)  
  
  BIC <- 2*outcomes$objective+log(length(selfData$adjRT))*length(start)
  
  ModelsResults[i,1] <- outcomes$objective
  ModelsResults[i,2] <- BIC
  ModelsResults[i,3] <- seedi
  ModelsResults[i,4] <- outcomes$par[1]
  ModelsResults[i,5] <- outcomes$par[2]
  ModelsResults[i,6] <- outcomes$par[3]
  ModelsResults[i,7] <- outcomes$par[4]
  ModelsResults[i,8] <- outcomes$par[5]
  ModelsResults[i,9] <- outcomes$par[6]
  ModelsResults[i,10] <- outcomes$par[7]
  ModelsResults[i,11] <- outcomes$par[8]
  ModelsResults[i,12] <- outcomes$par[9]
} 

write.csv(ModelsResults, "condition_SP_BOUND_SV_NOND.csv")
