# Dependencies analyses STUDY 1--------------------------------------------
# Installing WRS does not work via CRAN
library(devtools)
install_github("nicebread/WRS", subdir="pkg")

library(WRS)
library(pwr)
library(MASS)

dataNew=read.csv("data study 1.csv")
dataCompleet=dataNew[dataNew$Finished==1,]
dat=dataCompleet

# Methods section description ---------------------------------------------
dim(dataNew)[1] # The number of contacted researchers
round(dim(dataNew)[1] / 1135, 2) # Ibid, in %
dim(dat)[1] # The number of completed responses
round(dim(dat)[1] / 1135, 2) # Ibid, in %
table(dat$Condition) # Condition division


# Inter-coder reliability -------------------------------------------------
R2=read.csv("Q1dataC1eval_Second rater.csv")
R1=read.csv2("Q1dataC1eval_First rater.csv")
RF=read.csv("EvalFinal.csv")

# Data cleaning
R2$Power[which(is.na(R2$Power))]=0
R1$Power[which(is.na(R1$Power))]=0

R2$Other.experiments[which(is.na(R2$Other.experiments))]=0
R1$Other.experiments[which(is.na(R1$Other.experiments))]=0

R2$Rule.of.Thumb[which(is.na(R2$Rule.of.Thumb))]=0
R1$Rule.of.Thumb[which(is.na(R1$Rule.of.Thumb))]=0

R2$Practical.constraints[which(is.na(R2$Practical.constraints))]=0
R1$Practical.constraints[which(is.na(R1$Practical.constraints))]=0

R2$As.many.as.possible[which(is.na(R2$As.many.as.possible))]=0
R1$As.many.as.possible[which(is.na(R1$As.many.as.possible))]=0

#number of discrepancies
disP=length(which(R1$Power!=R2$Power))
disO=length(which(R1$Other.experiments!=R2$Other.experiments))
disR=length(which(R1$Rule.of.Thumb!=R2$Rule.of.Thumb))
disPC=length(which(R1$Practical.constraints!=R2$Practical.constraints))
disA=length(which(R1$As.many.as.possible!=R2$As.many.as.possible))

#show discrepancies
R2[which(R1$Power!=R2$Power),]
R2[which(R1$Other.experiments!=R2$Other.experiments),]
R2[which(R1$Rule.of.Thumb!=R2$Rule.of.Thumb),]
R2[which(R1$Practical.constraints!=R2$Practical.constraints),]
R2[which(R1$As.many.as.possible!=R2$As.many.as.possible),]

Pe = (sum(R1[,4:8])/(nrow(R1)*5))*(sum(R2[,4:8])/(nrow(R1)*5))+
  ((nrow(R1)*5-sum(R1[,4:8]))/(nrow(R1)*5))*((nrow(R1)*5-sum(R1[,4:8]))/(nrow(R1)*5))	#expected agreement
Po=(nrow(R1)*5-(disP+disO+disR+disPC+disA))/(nrow(R1)*5)		#observed agreement

k=(Po-Pe)/(1-Pe)

# Proportion agreement
round(Po, 2)
# Print cohen's kappa
round(k, 2)

# Power mentions
table(RF$Power)
round(table(RF$Power)[1] / 197, 2)

# Practical constraints mentioned of those mentioning power
table(RF$Practical.constraints[RF$Power == 1])
round(table(RF$Practical.constraints[RF$Power == 1]) / table(RF$Power)[1], 2)

# Practical constraints
table(RF$Practical.constraints)
round(table(RF$Practical.constraints) / 197, 2)

# Rule of thumb mention
table(RF$Rule.of.Thumb)
round(table(RF$Rule.of.Thumb) / 197, 2)

# Common practice mention
table(RF$Other.experiments)
round(table(RF$Other.experiments) / 197, 2)
  
# As many as possible mention
table(RF$As.many.as.possible)
round(table(RF$As.many.as.possible) / 197, 2)

# proportion alpha = .05 and power = .8 -----------------------------------
round(length(which(dat$Q3==.05))/length(na.omit(dat$Q3)), 2)
round(length(which(dat$Q4==.8))/length(na.omit(dat$Q4)), 2)

# Table 1 -----------------------------------------------------------------
tmean(dat$Q3,na.rm=T) 	#alpha estimate
round(tmean(dat$Q5,na.rm=T), 2)	#ES estimate
round(tmean(dat$Q6,na.rm=T), 2)	#N estimate 
round(tmean(dat$Q4,na.rm=T), 2)	#reported power estimate 
power.t.test(n = tmean(dat$Q6,na.rm=T), d = tmean(dat$Q5,na.rm=T)) #calculated power (overall) 
round(tmean(dat$Power,na.rm=T), 2)#calculated power (individual) 
round(tmean(dat$Bias,na.rm=T), 2)	#bias (individual)
##

# Response origin ---------------------------------------------------------
as.character(dat$Q1_C1[10])

# Calculated power --------------------------------------------------------
# Power for n = 20-25
power.t.test(n = 20, d = .5)
power.t.test(n = 25, d = .5)

# % Do not conduct power analyses
round(sum(dat$PowMen == 0) / length(dat$PowMen), 2)


# Reported and calculated power compared ----------------------------------
#difference reported and calculated power
yuendv2(dat$Q4,dat$Power) ##
yuendv2(dat$Q4_C1,dat$PowerC1)	#researchers
yuendv2(dat$Q4_C2,dat$PowerC2)	#reviewers

# Bias
length(which(dat$Bias<0))/length(na.omit(dat$Bias))*100		#prop neg bias ##
length(which(dat$Bias<(-0.5)))/length(na.omit(dat$Bias))*100	#prop large neg bias ##

# Supplementary Online Material Study 1 -----------------------------------

# Table 1 SOM -------------------------------------------------------------
tmean(dat$Q3_C1,na.rm=T) 	#alpha estimate researchers
median(dat$Q3_C1,na.rm=T)     #alpha estimate researchers
tmean(dat$Q3_C2,na.rm=T) 	#alpha estimate reviewers
median(dat$Q3_C2,na.rm=T)     #alpha estimate reviewers

tmean(dat$Q5_C1,na.rm=T)	#ES estimate researchers
median(dat$Q5_C1,na.rm=T)     #ES estimate researchers
tmean(dat$Q5_C2,na.rm=T)	#ES estimate reviewers
median(dat$Q5_C2,na.rm=T)     #ES estimate reviewers

tmean(dat$Q6_C1,na.rm=T)	#N estimate researchers
median(dat$Q6_C1,na.rm=T)     #N estimate researchers
tmean(dat$Q6_C2,na.rm=T)	#N estimate reviewers
median(dat$Q6_C2,na.rm=T)     #N estimate reviewers

tmean(dat$Q4_C1,na.rm=T)	#reported power estimate researchers
median(dat$Q4_C1,na.rm=T)     #power estimate researchers
tmean(dat$Q4_C2,na.rm=T)	#reported power estimate reviewers
median(dat$Q4_C2,na.rm=T)     #power estimate reviewers

#calculated power (overall) researchers
power.t.test(n = tmean(dat$Q6_C1,na.rm=T), d = tmean(dat$Q5_C1,na.rm=T)) #calculated power (overall) 
power.t.test(n = median(dat$Q6_C1,na.rm=T), d = median(dat$Q5_C1,na.rm=T))
#calculated power (overall) reviewers
power.t.test(n = tmean(dat$Q6_C2,na.rm=T), d = tmean(dat$Q5_C2,na.rm=T)) #calculated power (overall) 
power.t.test(n = median(dat$Q6_C2,na.rm=T), d = median(dat$Q5_C2,na.rm=T))

tmean(dat$PowerC1,na.rm=T)	#calculated power (individual) researchers
median(dat$PowerC1,na.rm=T)                #calculated power (individual) researchers
tmean(dat$PowerC2,na.rm=T)	#calculated power (individual) reviewers
median(dat$PowerC2,na.rm=T)                #calculated power (individual) reviewers

tmean(dat$BiasC1,na.rm=T)	#bias researchers
median(dat$BiasC1,na.rm=T)     #bias researchers
tmean(dat$BiasC2,na.rm=T)	#bias (individual) researchers
median(dat$BiasC2,na.rm=T)     #bias (individual) researchers


# Figure S1 ---------------------------------------------------------------

pdf("histogramms.pdf", width=9,height=18)
layout(matrix(1:18,6,3))

hist(dat$Q3,25, xlab="alpha",main="All")
abline(v=tmean(dat$Q3))
abline(v=mean(dat$Q3),lty=2)
abline(v=median(dat$Q3),lty=3)
legend(x = .4, y = 200, lty = c(1, 2, 3), legend = c("Trimmed mean",
                                                     "Mean",
                                                     "Median"),
       bty = 'n')

hist(dat$Q5[which(dat$Q5<=3)],25,main="All", xlab="ES (d)")
abline(v=tmean(dat$Q5))
abline(v=mean(dat$Q5),lty=2)
abline(v=median(dat$Q5),lty=3)

hist(dat$Q6[which(dat$Q6<501)],25,main="All", xlab="n")
abline(v=tmean(dat$Q6))
abline(v=mean(dat$Q6),lty=2)
abline(v=median(dat$Q6),lty=3)

hist(dat$Q4,25,main="All", xlab="Power (reported)")
abline(v=tmean(dat$Q4))
abline(v=mean(dat$Q4),lty=2)
abline(v=median(dat$Q4),lty=3)
legend(x = .05, y = 200, lty = c(1, 2, 3), legend = c("Trimmed mean",
                                                     "Mean",
                                                     "Median"),
       bty = 'n')

hist(dat$Power,25,main="All", xlab="Power (calculated)")
abline(v=tmean(dat$Power,na.rm=T))
abline(v=mean(dat$Power,na.rm=T),lty=2)
abline(v=median(dat$Power,na.rm=T),lty=3)

hist(dat$Bias,25,main="All", xlab="Bias")
abline(v=tmean(dat$Bias,na.rm=T))
abline(v=mean(dat$Bias,na.rm=T),lty=2)
abline(v=median(dat$Bias,na.rm=T),lty=3)

hist(dat$Q3_C1,25, xlab="alpha",main="Researcher")
abline(v=tmean(dat$Q3_C1,na.rm=T))
abline(v=mean(dat$Q3_C1,na.rm=T),lty=2)
abline(v=median(dat$Q3_C1,na.rm=T),lty=3)

hist(dat$Q5_C1[which(dat$Q5_C1<=3)],25,main="Researcher", xlab="ES (d)")
abline(v=tmean(dat$Q5_C1,na.rm=T))
abline(v=mean(dat$Q5_C1,na.rm=T),lty=2)
abline(v=median(dat$Q5_C1,na.rm=T),lty=3)

hist(dat$Q6_C1[which(dat$Q6_C1<501)],25,main="Researcher", xlab="n")
abline(v=tmean(dat$Q6_C1,na.rm=T))
abline(v=mean(dat$Q6_C1,na.rm=T),lty=2)
abline(v=median(dat$Q6_C1,na.rm=T),lty=3)

hist(dat$Q4_C1,25,main="Researcher", xlab="Power (reported)")
abline(v=tmean(dat$Q4_C1,na.rm=T))
abline(v=mean(dat$Q4_C1,na.rm=T),lty=2)
abline(v=median(dat$Q4_C1,na.rm=T),lty=3)

hist(dat$PowerC1,25,main="Researcher", xlab="Power (calculated)")
abline(v=tmean(dat$PowerC1,na.rm=T))
abline(v=mean(dat$PowerC1,na.rm=T),lty=2)
abline(v=median(dat$PowerC1,na.rm=T),lty=3)

hist(dat$BiasC1,25,main="Researcher", xlab="Bias")
abline(v=tmean(dat$BiasC1,na.rm=T))
abline(v=mean(dat$BiasC1,na.rm=T),lty=2)
abline(v=median(dat$BiasC1,na.rm=T),lty=3)

hist(dat$Q3_C2,25, xlab="alpha",main="Reviewer")
abline(v=tmean(dat$Q3_C2,na.rm=T))
abline(v=mean(dat$Q3_C2,na.rm=T),lty=2)
abline(v=median(dat$Q3_C2,na.rm=T),lty=3)

hist(dat$Q5_C2[which(dat$Q5_C2<=3)],25,main="Reviewer", xlab="ES (d)")
abline(v=tmean(dat$Q5_C2,na.rm=T))
abline(v=mean(dat$Q5_C2,na.rm=T),lty=2)
abline(v=median(dat$Q5_C2,na.rm=T),lty=3)

hist(dat$Q6_C2[which(dat$Q6_C2<501)],25,main="Reviewer", xlab="n")
abline(v=tmean(dat$Q6_C2,na.rm=T))
abline(v=mean(dat$Q6_C2,na.rm=T),lty=2)
abline(v=median(dat$Q6_C2,na.rm=T),lty=3)

hist(dat$Q4_C2,25,main="Reviewer", xlab="Power (reported)")
abline(v=tmean(dat$Q4_C2,na.rm=T))
abline(v=mean(dat$Q4_C2,na.rm=T),lty=2)
abline(v=median(dat$Q4_C2,na.rm=T),lty=3)

hist(dat$PowerC2,25,main="Reviewer", xlab="Power (calculated)")
abline(v=tmean(dat$PowerC2,na.rm=T))
abline(v=mean(dat$PowerC2,na.rm=T),lty=2)
abline(v=median(dat$PowerC2,na.rm=T),lty=3)

hist(dat$BiasC2,25,main="Reviewer", xlab="Bias")
abline(v=tmean(dat$BiasC2,na.rm=T))
abline(v=mean(dat$BiasC2,na.rm=T),lty=2)
abline(v=median(dat$BiasC2,na.rm=T),lty=3)

dev.off()

# Differences between researcher and reviewers ----------------------------
# alpha (no variance so ERROR)
yuenv2(dat$Q3_C1,dat$Q3_C2)
#ES
yuenv2(dat$Q5_C1,dat$Q5_C2)
#N
yuenv2(dat$Q6_C1,dat$Q6_C2)
#power
yuenv2(dat$Q4_C1,dat$Q4_C2)		

trimci(dat$PowerC1)			#CI Power researchers
trimci(dat$PowerC2)			#CI Power reviewers

trimci(dat$BiasC1)			#CI Bias researchers
trimci(dat$BiasC2)			#CI Bias reviewers

# Statistical knowledge ---------------------------------------------------
#correlations with statistical knowledge
cor.test(dat$Q9[which(dat$Condition==1)],dat$Power[which(dat$Condition==1)],use="pairwise.complete.obs",method="spearman")
cor.test(dat$Q9[which(dat$Condition==2)],dat$Power[which(dat$Condition==2)],use="pairwise.complete.obs",method="spearman")

cor.test(dat$Q9[which(dat$Condition==1)],dat$Bias[which(dat$Condition==1)],use="pairwise.complete.obs",method="spearman")
cor.test(dat$Q9[which(dat$Condition==2)],dat$Bias[which(dat$Condition==2)],use="pairwise.complete.obs",method="spearman")

# Table 2 SOM-----------------------------------------------------------------
table(dat$Q10) # N
round(table(dat$Q10)/sum(table(dat$Q10))*100) # N in %

setP=setP2=setP3=rep(NA,6)
for(i in 1:6)
{
  setP[i]=tmean(dat$Power[which(dat$Q10==i)],na.rm=T)	#trimmed mean estimated power for each # pub category
  setP2[i]=tmean(dat$Bias[which(dat$Q10==i)],na.rm=T)	#trimmed mean of bias for each # pub category
}
# Trimmed mean power
round(setP, 2)
# Trimmed mean bias
round(setP2, 2)

# Number of publications --------------------------------------------------
#robust regression
Q10b=dat$Q10-3.5		#centering the number of publications 

#power as dependent var
rr2b=rlm(dat$Power~Q10b*as.factor(dat$Condition))
summary(rr2b)
toss=summary(rr2b)$coefficients[,1]/summary(rr2b)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#only condition is significant

#bias as dependent var
rr2bias=rlm(dat$Bias~Q10b*as.factor(dat$Condition))
summary(rr2bias)
toss=summary(rr2bias)$coefficients[,1]/summary(rr2bias)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#no significant effects

#Alpha as dependent var
rr2al=rlm(dat$Q3~Q10b*as.factor(dat$Condition))
summary(rr2al)
toss=summary(rr2al)$coefficients[,1]/summary(rr2al)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#no significant effects

#reported power as dependent var
rr2po=rlm(dat$Q4~Q10b*as.factor(dat$Condition))
summary(rr2po)
toss=summary(rr2po)$coefficients[,1]/summary(rr2po)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#condition is significant, but probably problems due to non variability

#ES as dependent var
rr2es=rlm(dat$Q5~Q10b*as.factor(dat$Condition))
summary(rr2es)
toss=summary(rr2es)$coefficients[,1]/summary(rr2es)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#condition is signifcant

#N as dependent var
rr2N=rlm(dat$Q6~Q10b*as.factor(dat$Condition))
summary(rr2N)
toss=summary(rr2N)$coefficients[,1]/summary(rr2N)$coefficients[,2]
pval=2*(1-pnorm(abs(toss)))
pval
#number of publications is a significant predictor (b = 3.58, p = .002)

# Table 3 SOM -------------------------------------------------------------
#number of respondents per research field
table(dat$Q8)
round(table(dat$Q8)/sum(table(dat$Q8))*100)

#number of respondents in the research condition per research field
table(dat$Q8[which(dat$Condition==1)])
round(table(dat$Q8[which(dat$Condition==1)])/sum(table(dat$Q8[which(dat$Condition==1)]))*100)

#number of respondents in the reviewer condition per research field
table(dat$Q8[which(dat$Condition==2)])
round(table(dat$Q8[which(dat$Condition==2)])/sum(table(dat$Q8[which(dat$Condition==2)]))*100)

# Trimmed mean statistical knowledge
statField=rep(NA,10)
for(i in 1:10)
{
  statField[i]=tmean(dat$Q9[which(dat$Q8==i)])
}
round(statField,1)

#calculate trimmed mean of the calculated power, bias, N, and ES for each research field
## alpha and power are not calculated due to the low variability in the answers of the respondents on this question
set=set2=set3=set4=rep(NA,10)
for(i in 1:10)
{
  set[i]=tmean(dat$Power[which(dat$Q8==i)],na.rm=T)	#calculated power for each research field
  set2[i]=tmean(dat$Bias[which(dat$Q8==i)],na.rm=T)	#Bias for each research field
}
round(set, 2)
round(set2, 2)

# Research field ----------------------------------------------------------
#2 by 9 ANOVA with trimmed means

#difference in estimated power
mEP2w=na.omit(cbind(dat$Power[which(dat$Q8!=4)],dat$Condition[which(dat$Q8!=4)],dat$Q8[which(dat$Q8!=4)]))
zEP2w=fac2list(mEP2w[,1],mEP2w[,2:3])
t2way(2,9,zEP2w)
#no effects

#difference in bias
mB2w=na.omit(cbind(dat$Bias[which(dat$Q8!=4)],dat$Condition[which(dat$Q8!=4)],dat$Q8[which(dat$Q8!=4)]))
zB2w=fac2list(mB2w[,1],mB2w[,2:3])
t2way(2,9,zB2w)
#no effects

#differences in Alpha 
mA2w=na.omit(cbind(dat$Q3[which(dat$Q8!=4)],dat$Condition[which(dat$Q8!=4)],dat$Q8[which(dat$Q8!=4)]))
zA2w=fac2list(mA2w[,1],mA2w[,2:3])
t2way(2,9,zA2w)
## can't be tested due to low variability in the responses

#differences in ES
mES2w=na.omit(cbind(dat$Q5[which(dat$Q8!=4)],dat$Condition[which(dat$Q8!=4)],dat$Q8[which(dat$Q8!=4)]))
zES2w=fac2list(mES2w[,1],mES2w[,2:3])
t2way(2,9,zES2w)
#no main effects, but a small interaction effect between condition and research field (Q = 23.18, p = .032)
#$means
#          [,1] [,2] [,3]   [,4]      [,5]  [,6]      [,7]   [,8]      [,9]
#[1,] 0.3730769 0.40 0.40 0.2875 0.3285714 0.500 0.4035714 0.5000 0.4106061
#[2,] 0.4156250 0.45 0.32 0.4625 0.3650000 0.525 0.2727273 0.3875 0.3005556

#differences in N
mN2w=na.omit(cbind(dat$Q6[which(dat$Q8!=4)],dat$Condition[which(dat$Q8!=4)],dat$Q8[which(dat$Q8!=4)]))
zN2w=fac2list(mN2w[,1],mN2w[,2:3])
t2way(2,9,zN2w)
#main effect of condition Q = 21.44, p = .032
#$means
#         [,1]     [,2]     [,3]  [,4]     [,5]  [,6]     [,7]  [,8]     [,9]
#[1,] 57.30769 21.22222 39.16667 40.00 35.71429 23.00 55.21429 41.25 34.09091
#[2,] 37.50000 33.50000 27.20000 41.25 27.50000 31.25 35.90909 45.00 31.11111
#smallest N voor cognitive and neuroscience 


# Additional Questions ----------------------------------------------------
#difference reviewers and researchers in preference number of studies
mrr=matrix(c(table(dat$Q2_C1),table(dat$Q2_C2)),4,2)
chisq.test(mrr)
sqrt(chisq.test(mrr)$statistic/sum(mrr))


# table s4 ----------------------------------------------------------------

mrr
apply(X = mrr, 2, sum)
round(mrr[,1]/sum(mrr[,1])*100)
round(mrr[,2]/sum(mrr[,2])*100)


# table s5 ----------------------------------------------------------------

#question about removing outliers
table(dat$Q7_C1_v1)
round(table(dat$Q7_C1_v1)/sum(table(dat$Q7_C1_v1))*100)
sum(table(dat$Q7_C1_v1))

table(dat$Q7_C1_v2)
round(table(dat$Q7_C1_v2)/sum(table(dat$Q7_C1_v2))*100)
sum(table(dat$Q7_C1_v2))

table(dat$Q7_C2_v1)
round(table(dat$Q7_C2_v1)/sum(table(dat$Q7_C2_v1))*100)
sum(table(dat$Q7_C2_v1))

table(dat$Q7_C2_v2)
round(table(dat$Q7_C2_v2)/sum(table(dat$Q7_C2_v2))*100)
sum(table(dat$Q7_C2_v2))

