data1 <- read.csv("Study1_Data_VariousLevelsOfSmoothing.csv") 

##############All the data########

lm = lm(VoterIntentionIndex_FromStudy1 ~ Outbreak + Time + TimeSinceOutbreak, data=data1)
summary(lm)

lm = lm(VoterIntentionIndex_LessSmoothed_Pollster ~ Outbreak + Time + TimeSinceOutbreak, data=data1)
summary(lm)


###########Just the 1 week period before and after#####

data1_1week_beforeafter<- data1[c(7:14),]

lm = lm(VoterIntentionIndex_FromStudy1 ~ Outbreak + Time + TimeSinceOutbreak, data=data1_1week_beforeafter)
summary(lm)

lm = lm(VoterIntentionIndex_LessSmoothed_Pollster ~ Outbreak + Time + TimeSinceOutbreak, data=data1_1week_beforeafter)
summary(lm)


###########Simulating two causally-unrelated time series###

simnum<-10000
simul <- matrix(nrow=simnum, ncol=1, 0)
for (i in 1:simnum){
  ar.sim<-arima.sim(model=list(ar=c(.9)),n=23)
  ar.sim2<-arima.sim(model=list(ar=c(.9)),n=23)
  simul[i] <- cor(ar.sim,ar.sim2)
}
hist(simul)
quantile(simul,c(0.025,0.975))

ar.sim<-arima.sim(model=list(ar=c(.94)),n=23)
acf(ar.sim)


