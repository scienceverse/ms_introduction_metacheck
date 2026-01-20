# Dependencies study 2 ----------------------------------------------------
# Installing WRS does not work via CRAN
library(devtools)
install_github("nicebread/WRS", subdir="pkg")
#load packages
library(pwr)
library(WRS)
library(gplots)
library(ggplot2)

#read data
dat=read.csv("data study 2.csv")


# Methods -----------------------------------------------------------------
# nr responses
dim(dat)[1]

#table conditions
table(dat$conditionN)

# Power question correct response
sum(dat$powInt == 2)
sum(dat$powInt == 2) / length(dat$powInt)

# Results --intuitions about power and sample size ------------------------
#true power values
TruePower=matrix(NA,3,3)	#n by d
N=c(40,80,160)
d=c(.2,.5,.8)

for(i in 1:3)
{
  for(j in 1:3)
  {
    TruePower[i,j]=pwr.t.test(n=N[i]/2,d=d[j])$power
  }
}

# % overestimate power
(length(which(dat$conditionN=="small"&dat$Sd>TruePower[1,1]))+
   length(which(dat$conditionN=="medium"&dat$Sd>TruePower[2,1]))+
   length(which(dat$conditionN=="large"&dat$Sd>TruePower[3,1])))/nrow(dat)
#proportion underestimation sample size when effect size is small
length(which(dat$powSd<TrueSampleSize[1]))/nrow(dat)

# Table 2 -----------------------------------------------------------------
round(TruePower, 2)

#trimmed mean and CI power estimates
trimci(dat$Sd[which(dat$conditionN=="small")],tr=.2,alpha=.05)
trimci(dat$Sd[which(dat$conditionN=="medium")],tr=.2,alpha=.05)
trimci(dat$Sd[which(dat$conditionN=="large")],tr=.2,alpha=.05)

trimci(dat$Md[which(dat$conditionN=="small")],tr=.2,alpha=.05)
trimci(dat$Md[which(dat$conditionN=="medium")],tr=.2,alpha=.05)
trimci(dat$Md[which(dat$conditionN=="large")],tr=.2,alpha=.05)

trimci(dat$Ld[which(dat$conditionN=="small")],tr=.2,alpha=.05)
trimci(dat$Ld[which(dat$conditionN=="medium")],tr=.2,alpha=.05)
trimci(dat$Ld[which(dat$conditionN=="large")],tr=.2,alpha=.05)


# Figure 1 ----------------------------------------------------------------
N=20:300
d=c(.2,.38,.5,.8)
pow=matrix(NA,length(N),length(d))

for(i in 1:length(d))
{
  for(j in 1:length(N))
  {
    pow[j,i]=power.t.test(n=N[j],d=d[i])$power
  }
}

plot((N*2)[1:61],pow[1:61,1],xlab=substitute(paste(italic('N'), ' (total)')),ylab="power", ylim=c(0,1),type="l",lwd=3)
lines((N*2)[1:61],pow[1:61,3],lty=3,lwd=3,col="blue")
lines((N*2)[1:61],pow[1:61,4],lty=4,lwd=3,col="green")

legend(128,.19,c("d = 0.20", "d = 0.50", "d = 0.80"),lty=c(1,3,4),title="True power", 
       col=c("black", "blue", "green"),lwd=3, bty="n",cex=1.1)

plotCI(40,tmean(dat$Sd[which(dat$conditionN=="small")],tr=.2),
       li=trimci(dat$Sd[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Sd[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[2],add=TRUE,cex=2,lwd=2,pch=16)
plotCI(80,tmean(dat$Sd[which(dat$conditionN=="medium")],tr=.2),
       li=trimci(dat$Sd[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Sd[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[2],add=TRUE,cex=2,lwd=2,pch=16)
plotCI(160,tmean(dat$Sd[which(dat$conditionN=="large")],tr=.2),
       li=trimci(dat$Sd[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Sd[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[2],add=TRUE,cex=2,lwd=2,pch=16)
plotCI(40,tmean(dat$Md[which(dat$conditionN=="small")],tr=.2),
       li=trimci(dat$Md[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Md[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="blue",cex=2,lwd=2,pch=16)
plotCI(80,tmean(dat$Md[which(dat$conditionN=="medium")],tr=.2),
       li=trimci(dat$Md[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Md[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="blue",cex=2,lwd=2,pch=16)
plotCI(160,tmean(dat$Md[which(dat$conditionN=="large")],tr=.2),
       li=trimci(dat$Md[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Md[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="blue",cex=2,lwd=2,pch=16)
plotCI(40,tmean(dat$Ld[which(dat$conditionN=="small")],tr=.2),
       li=trimci(dat$Ld[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Ld[which(dat$conditionN=="small")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="green",cex=2,lwd=2,pch=16)
plotCI(80,tmean(dat$Ld[which(dat$conditionN=="medium")],tr=.2),
       li=trimci(dat$Ld[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Ld[which(dat$conditionN=="medium")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="green",cex=2,lwd=2,pch=16)
plotCI(160,tmean(dat$Ld[which(dat$conditionN=="large")],tr=.2),
       li=trimci(dat$Ld[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[1],
       ui=trimci(dat$Ld[which(dat$conditionN=="large")],tr=.2,alpha=.05)$ci[2],add=TRUE,col="green",cex=2,lwd=2,pch=16)

# Table 3 -----------------------------------------------------------------
#sample size estimates
trimci(dat$powSd,tr=.2,alpha=.05)
trimci(dat$powMd,tr=.2,alpha=.05)
trimci(dat$powLd,tr=.2,alpha=.05)

#true sample sizes
TrueSampleSize=rep(NA,3)
TrueSampleSize[1]=round(pwr.t.test(d=.2,power=.8)$n)*2
TrueSampleSize[2]=round(pwr.t.test(d=.5,power=.8)$n)*2
TrueSampleSize[3]=round(pwr.t.test(d=.8,power=.8)$n)*2
TrueSampleSize

# Other factors study 2 ---------------------------------------------------
#Rescore power question (2 is correct) into correct/incorrect
powIntCor=rep(NA,nrow(dat))
powIntCor[which(dat$powInt!=2)]=0
powIntCor[which(dat$powInt==2)]=1
table(dat$powInt)
length(which(powIntCor==1))/sum(table(dat$powInt))

#pca of powerAnalysis, Statistical Knowledge and PowerQuestion
pca3=princomp(cbind(dat$powerAnalysis,dat$statKnowledge,powIntCor),cor=TRUE,scores=TRUE)
CS=pca3$scores[,1]*-1
summary(pca3)

#generate dummy variables for condition
D1=rep(0,nrow(dat))
D1[which(dat$conditionN=="medium")]=1
D2=rep(0,nrow(dat))
D2[which(dat$conditionN=="large")]=1
#generate interaction variables
D1TCS=D1*CS
D2TCS=D2*CS

#Component score as predictor of power estimates
TableCSP=matrix(NA,21,9)

#regression analyses small effect
mSd1=lm(dat$Sd~CS)
summary(mSd1)
mSd2=lm(dat$Sd~CS+D1+D2)
summary(mSd2)
mSd3=lm(dat$Sd~CS+D1+D2+D1TCS+D2TCS)
summary(mSd3)
#comparison of models small effect
aSd12=anova(mSd1,mSd2)
aSd23=anova(mSd2,mSd3)

#regression analyses medium effect
mMd1=lm(dat$Md~CS)
summary(mMd1)
mMd2=lm(dat$Md~CS+D1+D2)
summary(mMd2)
mMd3=lm(dat$Md~CS+D1+D2+D1TCS+D2TCS)
summary(mMd3)
#comparison of models medium effect
aMd12=anova(mMd1,mMd2)
aMd23=anova(mMd2,mMd3)

# regression analyses power large effects
mLd1=lm(dat$Ld~CS)
summary(mLd1)
mLd2=lm(dat$Ld~CS+D1+D2)
summary(mLd2)
mLd3=lm(dat$Ld~CS+D1+D2+D1TCS+D2TCS)
summary(mLd3)
#comparison of models medium effect
aLd12=anova(mLd1,mLd2)
aLd23=anova(mLd2,mLd3)

# regression analyses N small effects
mSN1=lm(dat$powSd~CS)
summary(mSN1)
mSN2=lm(dat$powSd~CS+D1+D2)
summary(mSN2)
mSN3=lm(dat$powSd~CS+D1+D2+D1TCS+D2TCS)
summary(mSN3)
#comparison of models small effect
aSN12=anova(mSN1,mSN2)
aSN23=anova(mSN2,mSN3)

# regression analyses N medium effects
mMN1=lm(dat$powMd~CS)
summary(mMN1)
mMN2=lm(dat$powMd~CS+D1+D2)
summary(mMN2)
mMN3=lm(dat$powMd~CS+D1+D2+D1TCS+D2TCS)
summary(mMN3)
#comparison of models medium effect
aMN12=anova(mMN1,mMN2)
aMN23=anova(mMN2,mMN3)

# regression analyses N large effects
mLN1=lm(dat$powLd~CS)
summary(mLN1)
mLN2=lm(dat$powLd~CS+D1+D2)
summary(mLN2)
mLN3=lm(dat$powLd~CS+D1+D2+D1TCS+D2TCS)
summary(mLN3)
#comparison of models large effect
aLN12=anova(mLN1,mLN2)
aLN23=anova(mLN2,mLN3)

### comparison of research fields
table(dat$researchField)
round(table(dat$researchField)/sum(table(dat$researchField))*100)

facRF=dat$researchField
facRF[which(facRF==7)]=2 	#combine cognitive and neurosciece due to only one participant in the small condition
facRF[which(facRF==4)]=11	#add the two participants from forensic to the other category

#Difference power estimates when ES is small
z=fac2list(dat$Sd,cbind(dat$conditionN,facRF))
t2way(3,9,z)
#Difference power estimates when ES is medium
z=fac2list(dat$Md,cbind(dat$conditionN,facRF))
t2way(3,9,z)
#Difference power estimates when ES is large
z=fac2list(dat$Ld,cbind(dat$conditionN,facRF))
t2way(3,9,z)
#Difference N estimates when ES is small
z=fac2list(dat$powSd,cbind(dat$conditionN,facRF))
t2way(3,9,z)
tapply(dat$powSd,facRF,tmean)
#Difference N estimates when ES is medium
z=fac2list(dat$powMd,cbind(dat$conditionN,facRF))
t2way(3,9,z)
#Difference N estimates when ES is large
z=fac2list(dat$powLd,cbind(dat$conditionN,facRF))
t2way(3,9,z)


# Supplementary online material study 2 -----------------------------------
datAll <- read.csv('data study 2 including incomplete.csv')[, -1]

# Table 6 SOM -------------------------------------------------------------
#trimmed mean and CI power estimates
trimci(datAll$Sd[which(datAll$conditionN=="Small")],tr=.2,alpha=.05)
trimci(datAll$Sd[which(datAll$conditionN=="Medium")],tr=.2,alpha=.05)
trimci(datAll$Sd[which(datAll$conditionN=="Large")],tr=.2,alpha=.05)

trimci(datAll$Md[which(datAll$conditionN=="Small")],tr=.2,alpha=.05)
trimci(datAll$Md[which(datAll$conditionN=="Medium")],tr=.2,alpha=.05)
trimci(datAll$Md[which(datAll$conditionN=="Large")],tr=.2,alpha=.05)

trimci(datAll$Ld[which(datAll$conditionN=="Small")],tr=.2,alpha=.05)
trimci(datAll$Ld[which(datAll$conditionN=="Medium")],tr=.2,alpha=.05)
trimci(datAll$Ld[which(datAll$conditionN=="Large")],tr=.2,alpha=.05)


# Table 7 SOM -------------------------------------------------------------
#sample size estimates
trimci(datAll$powSd,tr=.2,alpha=.05)
trimci(datAll$powMd,tr=.2,alpha=.05)
trimci(datAll$powLd,tr=.2,alpha=.05)

# Final paragraph study 2 SOM ---------------------------------------------

# 2(correct/incorrect) by 3 (conditie) anova met Sd (final paragraph)
#power estimates when ES is small
z=fac2list(dat$Sd,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)
#power estimates when ES is medium
z=fac2list(dat$Md,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)
#power estimates when ES is large
z=fac2list(dat$Ld,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)
#N estimates when ES is small
z=fac2list(dat$powSd,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)
#N estimates when ES is medium
z=fac2list(dat$powMd,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)
#N estimates when ES is large
z=fac2list(dat$powLd,cbind(dat$conditionN,powIntCor))
t2way(3,2,z)

#difference between conditions in sample size estimates
#small ES
z=fac2list(dat$powSd,dat$conditionN)
t1way(z)
tapply(dat$powSd,dat$conditionN,tmean)
#medium ES
z=fac2list(dat$powMd,dat$conditionN)
t1way(z)
tapply(dat$powMd,dat$conditionN,tmean)
#large
z=fac2list(dat$powLd,dat$conditionN)
t1way(z)
tapply(dat$powLd,dat$conditionN,tmean)

# Table 8 SOM -------------------------------------------------------------
TableCSP[2,1]=round(summary(mSd1)$r.squared,3)
TableCSP[2,3]=round(summary(mSd1)$coefficients[8],3)
TableCSP[3,1]=round(summary(mSd1)$coefficients[2],3)
TableCSP[3,2]=round(summary(mSd1)$coefficients[4],3)
TableCSP[3,3]=round(summary(mSd1)$coefficients[8],3)

TableCSP[2,4]=round(summary(mSd2)$r.squared-summary(mSd1)$r.squared,3)
TableCSP[2,6]=round(aSd12$"Pr(>F)"[2],3)
TableCSP[3,4]=round(summary(mSd2)$coefficients[2],3)
TableCSP[3,5]=round(summary(mSd2)$coefficients[6],3)
TableCSP[3,6]=round(summary(mSd2)$coefficients[14],3)
TableCSP[4,4]=round(summary(mSd2)$coefficients[3],3)
TableCSP[4,5]=round(summary(mSd2)$coefficients[7],3)
TableCSP[4,6]=round(summary(mSd2)$coefficients[15],3)
TableCSP[5,4]=round(summary(mSd2)$coefficients[4],3)
TableCSP[5,5]=round(summary(mSd2)$coefficients[8],3)
TableCSP[5,6]=round(summary(mSd2)$coefficients[16],3)

TableCSP[2,7]=round(summary(mSd3)$r.squared-summary(mSd2)$r.squared,3)
TableCSP[2,9]=round(aSd23$"Pr(>F)"[2],3)
TableCSP[3,7]=round(summary(mSd3)$coefficients[2],3)
TableCSP[3,8]=round(summary(mSd3)$coefficients[8],3)
TableCSP[3,9]=round(summary(mSd3)$coefficients[20],3)
TableCSP[4,7]=round(summary(mSd3)$coefficients[3],3)
TableCSP[4,8]=round(summary(mSd3)$coefficients[9],3)
TableCSP[4,9]=round(summary(mSd3)$coefficients[21],3)
TableCSP[5,7]=round(summary(mSd3)$coefficients[4],3)
TableCSP[5,8]=round(summary(mSd3)$coefficients[10],3)
TableCSP[5,9]=round(summary(mSd3)$coefficients[22],3)
TableCSP[6,7]=round(summary(mSd3)$coefficients[5],3)
TableCSP[6,8]=round(summary(mSd3)$coefficients[11],3)
TableCSP[6,9]=round(summary(mSd3)$coefficients[23],3)
TableCSP[7,7]=round(summary(mSd3)$coefficients[6],3)
TableCSP[7,8]=round(summary(mSd3)$coefficients[12],3)
TableCSP[7,9]=round(summary(mSd3)$coefficients[24],3)

#medium effects

TableCSP[9,1]=round(summary(mMd1)$r.squared,3)
TableCSP[9,3]=round(summary(mMd1)$coefficients[8],3)
TableCSP[10,1]=round(summary(mMd1)$coefficients[2],3)
TableCSP[10,2]=round(summary(mMd1)$coefficients[4],3)
TableCSP[10,3]=round(summary(mMd1)$coefficients[8],3)

TableCSP[9,4]=round(summary(mMd2)$r.squared-summary(mMd1)$r.squared,3)
TableCSP[9,6]=round(aMd12$"Pr(>F)"[2],3)
TableCSP[10,4]=round(summary(mMd2)$coefficients[2],3)
TableCSP[10,5]=round(summary(mMd2)$coefficients[6],3)
TableCSP[10,6]=round(summary(mMd2)$coefficients[14],3)
TableCSP[11,4]=round(summary(mMd2)$coefficients[3],3)
TableCSP[11,5]=round(summary(mMd2)$coefficients[7],3)
TableCSP[11,6]=round(summary(mMd2)$coefficients[15],3)
TableCSP[12,4]=round(summary(mMd2)$coefficients[4],3)
TableCSP[12,5]=round(summary(mMd2)$coefficients[8],3)
TableCSP[12,6]=round(summary(mMd2)$coefficients[16],3)

TableCSP[9,7]=round(summary(mMd3)$r.squared-summary(mMd2)$r.squared,3)
TableCSP[9,9]=round(aMd23$"Pr(>F)"[2],3)
TableCSP[10,7]=round(summary(mMd3)$coefficients[2],3)
TableCSP[10,8]=round(summary(mMd3)$coefficients[8],3)
TableCSP[10,9]=round(summary(mMd3)$coefficients[20],3)
TableCSP[11,7]=round(summary(mMd3)$coefficients[3],3)
TableCSP[11,8]=round(summary(mMd3)$coefficients[9],3)
TableCSP[11,9]=round(summary(mMd3)$coefficients[21],3)
TableCSP[12,7]=round(summary(mMd3)$coefficients[4],3)
TableCSP[12,8]=round(summary(mMd3)$coefficients[10],3)
TableCSP[12,9]=round(summary(mMd3)$coefficients[22],3)
TableCSP[13,7]=round(summary(mMd3)$coefficients[5],3)
TableCSP[13,8]=round(summary(mMd3)$coefficients[11],3)
TableCSP[13,9]=round(summary(mMd3)$coefficients[23],3)
TableCSP[14,7]=round(summary(mMd3)$coefficients[6],3)
TableCSP[14,8]=round(summary(mMd3)$coefficients[12],3)
TableCSP[14,9]=round(summary(mMd3)$coefficients[24],3)

# Large effects
TableCSP[16,1]=round(summary(mLd1)$r.squared,3)
TableCSP[16,3]=round(summary(mLd1)$coefficients[8],3)
TableCSP[17,1]=round(summary(mLd1)$coefficients[2],3)
TableCSP[17,2]=round(summary(mLd1)$coefficients[4],3)
TableCSP[17,3]=round(summary(mLd1)$coefficients[8],3)

TableCSP[16,4]=round(summary(mLd2)$r.squared-summary(mLd1)$r.squared,3)
TableCSP[16,6]=round(aLd12$"Pr(>F)"[2],3)
TableCSP[17,4]=round(summary(mLd2)$coefficients[2],3)
TableCSP[17,5]=round(summary(mLd2)$coefficients[6],3)
TableCSP[17,6]=round(summary(mLd2)$coefficients[14],3)
TableCSP[18,4]=round(summary(mLd2)$coefficients[3],3)
TableCSP[18,5]=round(summary(mLd2)$coefficients[7],3)
TableCSP[18,6]=round(summary(mLd2)$coefficients[15],3)
TableCSP[19,4]=round(summary(mLd2)$coefficients[4],3)
TableCSP[19,5]=round(summary(mLd2)$coefficients[8],3)
TableCSP[19,6]=round(summary(mLd2)$coefficients[16],3)

TableCSP[16,7]=round(summary(mLd3)$r.squared-summary(mLd2)$r.squared,3)
TableCSP[16,9]=round(aLd23$"Pr(>F)"[2],3)
TableCSP[17,7]=round(summary(mLd3)$coefficients[2],3)
TableCSP[17,8]=round(summary(mLd3)$coefficients[8],3)
TableCSP[17,9]=round(summary(mLd3)$coefficients[20],3)
TableCSP[18,7]=round(summary(mLd3)$coefficients[3],3)
TableCSP[18,8]=round(summary(mLd3)$coefficients[9],3)
TableCSP[18,9]=round(summary(mLd3)$coefficients[21],3)
TableCSP[19,7]=round(summary(mLd3)$coefficients[4],3)
TableCSP[19,8]=round(summary(mLd3)$coefficients[10],3)
TableCSP[19,9]=round(summary(mLd3)$coefficients[22],3)
TableCSP[20,7]=round(summary(mLd3)$coefficients[5],3)
TableCSP[20,8]=round(summary(mLd3)$coefficients[11],3)
TableCSP[20,9]=round(summary(mLd3)$coefficients[23],3)
TableCSP[21,7]=round(summary(mLd3)$coefficients[6],3)
TableCSP[21,8]=round(summary(mLd3)$coefficients[12],3)
TableCSP[21,9]=round(summary(mLd3)$coefficients[24],3)

write.csv(TableCSP,"TableCSP_8SOM.csv",na="")

# Table 9 SOM -------------------------------------------------------------
TableCSN=matrix(NA,21,9)

TableCSN[2,1]=round(summary(mSN1)$r.squared,3)
TableCSN[2,3]=round(summary(mSN1)$coefficients[8],3)
TableCSN[3,1]=round(summary(mSN1)$coefficients[2],3)
TableCSN[3,2]=round(summary(mSN1)$coefficients[4],3)
TableCSN[3,3]=round(summary(mSN1)$coefficients[8],3)

TableCSN[2,4]=round(summary(mSN2)$r.squared-summary(mSN1)$r.squared,3)
TableCSN[2,6]=round(aSN12$"Pr(>F)"[2],3)
TableCSN[3,4]=round(summary(mSN2)$coefficients[2],3)
TableCSN[3,5]=round(summary(mSN2)$coefficients[6],3)
TableCSN[3,6]=round(summary(mSN2)$coefficients[14],3)
TableCSN[4,4]=round(summary(mSN2)$coefficients[3],3)
TableCSN[4,5]=round(summary(mSN2)$coefficients[7],3)
TableCSN[4,6]=round(summary(mSN2)$coefficients[15],3)
TableCSN[5,4]=round(summary(mSN2)$coefficients[4],3)
TableCSN[5,5]=round(summary(mSN2)$coefficients[8],3)
TableCSN[5,6]=round(summary(mSN2)$coefficients[16],3)

TableCSN[2,7]=round(summary(mSN3)$r.squared-summary(mSN2)$r.squared,3)
TableCSN[2,9]=round(aSN23$"Pr(>F)"[2],3)
TableCSN[3,7]=round(summary(mSN3)$coefficients[2],3)
TableCSN[3,8]=round(summary(mSN3)$coefficients[8],3)
TableCSN[3,9]=round(summary(mSN3)$coefficients[20],3)
TableCSN[4,7]=round(summary(mSN3)$coefficients[3],3)
TableCSN[4,8]=round(summary(mSN3)$coefficients[9],3)
TableCSN[4,9]=round(summary(mSN3)$coefficients[21],3)
TableCSN[5,7]=round(summary(mSN3)$coefficients[4],3)
TableCSN[5,8]=round(summary(mSN3)$coefficients[10],3)
TableCSN[5,9]=round(summary(mSN3)$coefficients[22],3)
TableCSN[6,7]=round(summary(mSN3)$coefficients[5],3)
TableCSN[6,8]=round(summary(mSN3)$coefficients[11],3)
TableCSN[6,9]=round(summary(mSN3)$coefficients[23],3)
TableCSN[7,7]=round(summary(mSN3)$coefficients[6],3)
TableCSN[7,8]=round(summary(mSN3)$coefficients[12],3)
TableCSN[7,9]=round(summary(mSN3)$coefficients[24],3)

TableCSN[9,1]=round(summary(mMN1)$r.squared,3)
TableCSN[9,3]=round(summary(mMN1)$coefficients[8],3)
TableCSN[10,1]=round(summary(mMN1)$coefficients[2],3)
TableCSN[10,2]=round(summary(mMN1)$coefficients[4],3)
TableCSN[10,3]=round(summary(mMN1)$coefficients[8],3)

TableCSN[9,4]=round(summary(mMN2)$r.squared-summary(mMN1)$r.squared,3)
TableCSN[9,6]=round(aMN12$"Pr(>F)"[2],3)
TableCSN[10,4]=round(summary(mMN2)$coefficients[2],3)
TableCSN[10,5]=round(summary(mMN2)$coefficients[6],3)
TableCSN[10,6]=round(summary(mMN2)$coefficients[14],3)
TableCSN[11,4]=round(summary(mMN2)$coefficients[3],3)
TableCSN[11,5]=round(summary(mMN2)$coefficients[7],3)
TableCSN[11,6]=round(summary(mMN2)$coefficients[15],3)
TableCSN[12,4]=round(summary(mMN2)$coefficients[4],3)
TableCSN[12,5]=round(summary(mMN2)$coefficients[8],3)
TableCSN[12,6]=round(summary(mMN2)$coefficients[16],3)

TableCSN[9,7]=round(summary(mMN3)$r.squared-summary(mMN2)$r.squared,3)
TableCSN[9,9]=round(aMN23$"Pr(>F)"[2],3)
TableCSN[10,7]=round(summary(mMN3)$coefficients[2],3)
TableCSN[10,8]=round(summary(mMN3)$coefficients[8],3)
TableCSN[10,9]=round(summary(mMN3)$coefficients[20],3)
TableCSN[11,7]=round(summary(mMN3)$coefficients[3],3)
TableCSN[11,8]=round(summary(mMN3)$coefficients[9],3)
TableCSN[11,9]=round(summary(mMN3)$coefficients[21],3)
TableCSN[12,7]=round(summary(mMN3)$coefficients[4],3)
TableCSN[12,8]=round(summary(mMN3)$coefficients[10],3)
TableCSN[12,9]=round(summary(mMN3)$coefficients[22],3)
TableCSN[13,7]=round(summary(mMN3)$coefficients[5],3)
TableCSN[13,8]=round(summary(mMN3)$coefficients[11],3)
TableCSN[13,9]=round(summary(mMN3)$coefficients[23],3)
TableCSN[14,7]=round(summary(mMN3)$coefficients[6],3)
TableCSN[14,8]=round(summary(mMN3)$coefficients[12],3)
TableCSN[14,9]=round(summary(mMN3)$coefficients[24],3)


TableCSN[16,1]=round(summary(mLN1)$r.squared,3)
TableCSN[16,3]=round(summary(mLN1)$coefficients[8],3)
TableCSN[17,1]=round(summary(mLN1)$coefficients[2],3)
TableCSN[17,2]=round(summary(mLN1)$coefficients[4],3)
TableCSN[17,3]=round(summary(mLN1)$coefficients[8],3)

TableCSN[16,4]=round(summary(mLN2)$r.squared-summary(mLN1)$r.squared,3)
TableCSN[16,6]=round(aLN12$"Pr(>F)"[2],3)
TableCSN[17,4]=round(summary(mLN2)$coefficients[2],3)
TableCSN[17,5]=round(summary(mLN2)$coefficients[6],3)
TableCSN[17,6]=round(summary(mLN2)$coefficients[14],3)
TableCSN[18,4]=round(summary(mLN2)$coefficients[3],3)
TableCSN[18,5]=round(summary(mLN2)$coefficients[7],3)
TableCSN[18,6]=round(summary(mLN2)$coefficients[15],3)
TableCSN[19,4]=round(summary(mLN2)$coefficients[4],3)
TableCSN[19,5]=round(summary(mLN2)$coefficients[8],3)
TableCSN[19,6]=round(summary(mLN2)$coefficients[16],3)

TableCSN[16,7]=round(summary(mLN3)$r.squared-summary(mLN2)$r.squared,3)
TableCSN[16,9]=round(aLN23$"Pr(>F)"[2],3)
TableCSN[17,7]=round(summary(mLN3)$coefficients[2],3)
TableCSN[17,8]=round(summary(mLN3)$coefficients[8],3)
TableCSN[17,9]=round(summary(mLN3)$coefficients[20],3)
TableCSN[18,7]=round(summary(mLN3)$coefficients[3],3)
TableCSN[18,8]=round(summary(mLN3)$coefficients[9],3)
TableCSN[18,9]=round(summary(mLN3)$coefficients[21],3)
TableCSN[19,7]=round(summary(mLN3)$coefficients[4],3)
TableCSN[19,8]=round(summary(mLN3)$coefficients[10],3)
TableCSN[19,9]=round(summary(mLN3)$coefficients[22],3)
TableCSN[20,7]=round(summary(mLN3)$coefficients[5],3)
TableCSN[20,8]=round(summary(mLN3)$coefficients[11],3)
TableCSN[20,9]=round(summary(mLN3)$coefficients[23],3)
TableCSN[21,7]=round(summary(mLN3)$coefficients[6],3)
TableCSN[21,8]=round(summary(mLN3)$coefficients[12],3)
TableCSN[21,9]=round(summary(mLN3)$coefficients[24],3)

write.csv(TableCSN,"TableCSN_9SOM.csv",na="")

# Table 10 SOM -------------------------------------------------------------
PA=dat$powerAnalysis
D1TPA=D1*PA
D2TPA=D2*PA

#Power estimates (Table 9)
TablePAP=matrix(NA,21,9)

#small effects
mSd1=lm(dat$Sd~PA)
mSd2=lm(dat$Sd~PA+D1+D2)
mSd3=lm(dat$Sd~PA+D1+D2+D1TPA+D2TPA)

aSd12=anova(mSd1,mSd2)
aSd23=anova(mSd2,mSd3)

TablePAP[2,1]=round(summary(mSd1)$r.squared,3)
TablePAP[2,3]=round(summary(mSd1)$coefficients[8],3)
TablePAP[3,1]=round(summary(mSd1)$coefficients[2],3)
TablePAP[3,2]=round(summary(mSd1)$coefficients[4],3)
TablePAP[3,3]=round(summary(mSd1)$coefficients[8],3)

TablePAP[2,4]=round(summary(mSd2)$r.squared-summary(mSd1)$r.squared,3)
TablePAP[2,6]=round(aSd12$"Pr(>F)"[2],3)
TablePAP[3,4]=round(summary(mSd2)$coefficients[2],3)
TablePAP[3,5]=round(summary(mSd2)$coefficients[6],3)
TablePAP[3,6]=round(summary(mSd2)$coefficients[14],3)
TablePAP[4,4]=round(summary(mSd2)$coefficients[3],3)
TablePAP[4,5]=round(summary(mSd2)$coefficients[7],3)
TablePAP[4,6]=round(summary(mSd2)$coefficients[15],3)
TablePAP[5,4]=round(summary(mSd2)$coefficients[4],3)
TablePAP[5,5]=round(summary(mSd2)$coefficients[8],3)
TablePAP[5,6]=round(summary(mSd2)$coefficients[16],3)

TablePAP[2,7]=round(summary(mSd3)$r.squared-summary(mSd2)$r.squared,3)
TablePAP[2,9]=round(aSd23$"Pr(>F)"[2],3)
TablePAP[3,7]=round(summary(mSd3)$coefficients[2],3)
TablePAP[3,8]=round(summary(mSd3)$coefficients[8],3)
TablePAP[3,9]=round(summary(mSd3)$coefficients[20],3)
TablePAP[4,7]=round(summary(mSd3)$coefficients[3],3)
TablePAP[4,8]=round(summary(mSd3)$coefficients[9],3)
TablePAP[4,9]=round(summary(mSd3)$coefficients[21],3)
TablePAP[5,7]=round(summary(mSd3)$coefficients[4],3)
TablePAP[5,8]=round(summary(mSd3)$coefficients[10],3)
TablePAP[5,9]=round(summary(mSd3)$coefficients[22],3)
TablePAP[6,7]=round(summary(mSd3)$coefficients[5],3)
TablePAP[6,8]=round(summary(mSd3)$coefficients[11],3)
TablePAP[6,9]=round(summary(mSd3)$coefficients[23],3)
TablePAP[7,7]=round(summary(mSd3)$coefficients[6],3)
TablePAP[7,8]=round(summary(mSd3)$coefficients[12],3)
TablePAP[7,9]=round(summary(mSd3)$coefficients[24],3)

#medium effects
mMd1=lm(dat$Md~PA)
mMd2=lm(dat$Md~PA+D1+D2)
mMd3=lm(dat$Md~PA+D1+D2+D1TPA+D2TPA)

aMd12=anova(mMd1,mMd2)
aMd23=anova(mMd2,mMd3)

TablePAP[9,1]=round(summary(mMd1)$r.squared,3)
TablePAP[9,3]=round(summary(mMd1)$coefficients[8],3)
TablePAP[10,1]=round(summary(mMd1)$coefficients[2],3)
TablePAP[10,2]=round(summary(mMd1)$coefficients[4],3)
TablePAP[10,3]=round(summary(mMd1)$coefficients[8],3)

TablePAP[9,4]=round(summary(mMd2)$r.squared-summary(mMd1)$r.squared,3)
TablePAP[9,6]=round(aMd12$"Pr(>F)"[2],3)
TablePAP[10,4]=round(summary(mMd2)$coefficients[2],3)
TablePAP[10,5]=round(summary(mMd2)$coefficients[6],3)
TablePAP[10,6]=round(summary(mMd2)$coefficients[14],3)
TablePAP[11,4]=round(summary(mMd2)$coefficients[3],3)
TablePAP[11,5]=round(summary(mMd2)$coefficients[7],3)
TablePAP[11,6]=round(summary(mMd2)$coefficients[15],3)
TablePAP[12,4]=round(summary(mMd2)$coefficients[4],3)
TablePAP[12,5]=round(summary(mMd2)$coefficients[8],3)
TablePAP[12,6]=round(summary(mMd2)$coefficients[16],3)

TablePAP[9,7]=round(summary(mMd3)$r.squared-summary(mMd2)$r.squared,3)
TablePAP[9,9]=round(aMd23$"Pr(>F)"[2],3)
TablePAP[10,7]=round(summary(mMd3)$coefficients[2],3)
TablePAP[10,8]=round(summary(mMd3)$coefficients[8],3)
TablePAP[10,9]=round(summary(mMd3)$coefficients[20],3)
TablePAP[11,7]=round(summary(mMd3)$coefficients[3],3)
TablePAP[11,8]=round(summary(mMd3)$coefficients[9],3)
TablePAP[11,9]=round(summary(mMd3)$coefficients[21],3)
TablePAP[12,7]=round(summary(mMd3)$coefficients[4],3)
TablePAP[12,8]=round(summary(mMd3)$coefficients[10],3)
TablePAP[12,9]=round(summary(mMd3)$coefficients[22],3)
TablePAP[13,7]=round(summary(mMd3)$coefficients[5],3)
TablePAP[13,8]=round(summary(mMd3)$coefficients[11],3)
TablePAP[13,9]=round(summary(mMd3)$coefficients[23],3)
TablePAP[14,7]=round(summary(mMd3)$coefficients[6],3)
TablePAP[14,8]=round(summary(mMd3)$coefficients[12],3)
TablePAP[14,9]=round(summary(mMd3)$coefficients[24],3)

#large effects
mLd1=lm(dat$Ld~PA)
mLd2=lm(dat$Ld~PA+D1+D2)
mLd3=lm(dat$Ld~PA+D1+D2+D1TPA+D2TPA)

aLd12=anova(mLd1,mLd2)
aLd23=anova(mLd2,mLd3)

TablePAP[16,1]=round(summary(mLd1)$r.squared,3)
TablePAP[16,3]=round(summary(mLd1)$coefficients[8],3)
TablePAP[17,1]=round(summary(mLd1)$coefficients[2],3)
TablePAP[17,2]=round(summary(mLd1)$coefficients[4],3)
TablePAP[17,3]=round(summary(mLd1)$coefficients[8],3)

TablePAP[16,4]=round(summary(mLd2)$r.squared-summary(mLd1)$r.squared,3)
TablePAP[16,6]=round(aLd12$"Pr(>F)"[2],3)
TablePAP[17,4]=round(summary(mLd2)$coefficients[2],3)
TablePAP[17,5]=round(summary(mLd2)$coefficients[6],3)
TablePAP[17,6]=round(summary(mLd2)$coefficients[14],3)
TablePAP[18,4]=round(summary(mLd2)$coefficients[3],3)
TablePAP[18,5]=round(summary(mLd2)$coefficients[7],3)
TablePAP[18,6]=round(summary(mLd2)$coefficients[15],3)
TablePAP[19,4]=round(summary(mLd2)$coefficients[4],3)
TablePAP[19,5]=round(summary(mLd2)$coefficients[8],3)
TablePAP[19,6]=round(summary(mLd2)$coefficients[16],3)

TablePAP[16,7]=round(summary(mLd3)$r.squared-summary(mLd2)$r.squared,3)
TablePAP[16,9]=round(aLd23$"Pr(>F)"[2],3)
TablePAP[17,7]=round(summary(mLd3)$coefficients[2],3)
TablePAP[17,8]=round(summary(mLd3)$coefficients[8],3)
TablePAP[17,9]=round(summary(mLd3)$coefficients[20],3)
TablePAP[18,7]=round(summary(mLd3)$coefficients[3],3)
TablePAP[18,8]=round(summary(mLd3)$coefficients[9],3)
TablePAP[18,9]=round(summary(mLd3)$coefficients[21],3)
TablePAP[19,7]=round(summary(mLd3)$coefficients[4],3)
TablePAP[19,8]=round(summary(mLd3)$coefficients[10],3)
TablePAP[19,9]=round(summary(mLd3)$coefficients[22],3)
TablePAP[20,7]=round(summary(mLd3)$coefficients[5],3)
TablePAP[20,8]=round(summary(mLd3)$coefficients[11],3)
TablePAP[20,9]=round(summary(mLd3)$coefficients[23],3)
TablePAP[21,7]=round(summary(mLd3)$coefficients[6],3)
TablePAP[21,8]=round(summary(mLd3)$coefficients[12],3)
TablePAP[21,9]=round(summary(mLd3)$coefficients[24],3)

write.csv(TablePAP,"TablePAP_10SOM.csv",na="")

# Table 11 SOM ------------------------------------------------------------
TablePAN=matrix(NA,21,9)

#Small effects
mSN1=lm(dat$powSd~PA)
mSN2=lm(dat$powSd~PA+D1+D2)
mSN3=lm(dat$powSd~PA+D1+D2+D1TPA+D2TPA)

aSN12=anova(mSN1,mSN2)
aSN23=anova(mSN2,mSN3)

TablePAN[2,1]=round(summary(mSN1)$r.squared,3)
TablePAN[2,3]=round(summary(mSN1)$coefficients[8],3)
TablePAN[3,1]=round(summary(mSN1)$coefficients[2],3)
TablePAN[3,2]=round(summary(mSN1)$coefficients[4],3)
TablePAN[3,3]=round(summary(mSN1)$coefficients[8],3)

TablePAN[2,4]=round(summary(mSN2)$r.squared-summary(mSN1)$r.squared,3)
TablePAN[2,6]=round(aSN12$"Pr(>F)"[2],3)
TablePAN[3,4]=round(summary(mSN2)$coefficients[2],3)
TablePAN[3,5]=round(summary(mSN2)$coefficients[6],3)
TablePAN[3,6]=round(summary(mSN2)$coefficients[14],3)
TablePAN[4,4]=round(summary(mSN2)$coefficients[3],3)
TablePAN[4,5]=round(summary(mSN2)$coefficients[7],3)
TablePAN[4,6]=round(summary(mSN2)$coefficients[15],3)
TablePAN[5,4]=round(summary(mSN2)$coefficients[4],3)
TablePAN[5,5]=round(summary(mSN2)$coefficients[8],3)
TablePAN[5,6]=round(summary(mSN2)$coefficients[16],3)

TablePAN[2,7]=round(summary(mSN3)$r.squared-summary(mSN2)$r.squared,3)
TablePAN[2,9]=round(aSN23$"Pr(>F)"[2],3)
TablePAN[3,7]=round(summary(mSN3)$coefficients[2],3)
TablePAN[3,8]=round(summary(mSN3)$coefficients[8],3)
TablePAN[3,9]=round(summary(mSN3)$coefficients[20],3)
TablePAN[4,7]=round(summary(mSN3)$coefficients[3],3)
TablePAN[4,8]=round(summary(mSN3)$coefficients[9],3)
TablePAN[4,9]=round(summary(mSN3)$coefficients[21],3)
TablePAN[5,7]=round(summary(mSN3)$coefficients[4],3)
TablePAN[5,8]=round(summary(mSN3)$coefficients[10],3)
TablePAN[5,9]=round(summary(mSN3)$coefficients[22],3)
TablePAN[6,7]=round(summary(mSN3)$coefficients[5],3)
TablePAN[6,8]=round(summary(mSN3)$coefficients[11],3)
TablePAN[6,9]=round(summary(mSN3)$coefficients[23],3)
TablePAN[7,7]=round(summary(mSN3)$coefficients[6],3)
TablePAN[7,8]=round(summary(mSN3)$coefficients[12],3)
TablePAN[7,9]=round(summary(mSN3)$coefficients[24],3)

#medium effects
mMN1=lm(dat$powMd~PA)
mMN2=lm(dat$powMd~PA+D1+D2)
mMN3=lm(dat$powMd~PA+D1+D2+D1TPA+D2TPA)

aMN12=anova(mMN1,mMN2)
aMN23=anova(mMN2,mMN3)

TablePAN[9,1]=round(summary(mMN1)$r.squared,3)
TablePAN[9,3]=round(summary(mMN1)$coefficients[8],3)
TablePAN[10,1]=round(summary(mMN1)$coefficients[2],3)
TablePAN[10,2]=round(summary(mMN1)$coefficients[4],3)
TablePAN[10,3]=round(summary(mMN1)$coefficients[8],3)

TablePAN[9,4]=round(summary(mMN2)$r.squared-summary(mMN1)$r.squared,3)
TablePAN[9,6]=round(aMN12$"Pr(>F)"[2],3)
TablePAN[10,4]=round(summary(mMN2)$coefficients[2],3)
TablePAN[10,5]=round(summary(mMN2)$coefficients[6],3)
TablePAN[10,6]=round(summary(mMN2)$coefficients[14],3)
TablePAN[11,4]=round(summary(mMN2)$coefficients[3],3)
TablePAN[11,5]=round(summary(mMN2)$coefficients[7],3)
TablePAN[11,6]=round(summary(mMN2)$coefficients[15],3)
TablePAN[12,4]=round(summary(mMN2)$coefficients[4],3)
TablePAN[12,5]=round(summary(mMN2)$coefficients[8],3)
TablePAN[12,6]=round(summary(mMN2)$coefficients[16],3)

TablePAN[9,7]=round(summary(mMN3)$r.squared-summary(mMN2)$r.squared,3)
TablePAN[9,9]=round(aMN23$"Pr(>F)"[2],3)
TablePAN[10,7]=round(summary(mMN3)$coefficients[2],3)
TablePAN[10,8]=round(summary(mMN3)$coefficients[8],3)
TablePAN[10,9]=round(summary(mMN3)$coefficients[20],3)
TablePAN[11,7]=round(summary(mMN3)$coefficients[3],3)
TablePAN[11,8]=round(summary(mMN3)$coefficients[9],3)
TablePAN[11,9]=round(summary(mMN3)$coefficients[21],3)
TablePAN[12,7]=round(summary(mMN3)$coefficients[4],3)
TablePAN[12,8]=round(summary(mMN3)$coefficients[10],3)
TablePAN[12,9]=round(summary(mMN3)$coefficients[22],3)
TablePAN[13,7]=round(summary(mMN3)$coefficients[5],3)
TablePAN[13,8]=round(summary(mMN3)$coefficients[11],3)
TablePAN[13,9]=round(summary(mMN3)$coefficients[23],3)
TablePAN[14,7]=round(summary(mMN3)$coefficients[6],3)
TablePAN[14,8]=round(summary(mMN3)$coefficients[12],3)
TablePAN[14,9]=round(summary(mMN3)$coefficients[24],3)

#large effects
mLN1=lm(dat$powLd~PA)
mLN2=lm(dat$powLd~PA+D1+D2)
mLN3=lm(dat$powLd~PA+D1+D2+D1TPA+D2TPA)

aLN12=anova(mLN1,mLN2)
aLN23=anova(mLN2,mLN3)

TablePAN[16,1]=round(summary(mLN1)$r.squared,3)
TablePAN[16,3]=round(summary(mLN1)$coefficients[8],3)
TablePAN[17,1]=round(summary(mLN1)$coefficients[2],3)
TablePAN[17,2]=round(summary(mLN1)$coefficients[4],3)
TablePAN[17,3]=round(summary(mLN1)$coefficients[8],3)

TablePAN[16,4]=round(summary(mLN2)$r.squared-summary(mLN1)$r.squared,3)
TablePAN[16,6]=round(aLN12$"Pr(>F)"[2],3)
TablePAN[17,4]=round(summary(mLN2)$coefficients[2],3)
TablePAN[17,5]=round(summary(mLN2)$coefficients[6],3)
TablePAN[17,6]=round(summary(mLN2)$coefficients[14],3)
TablePAN[18,4]=round(summary(mLN2)$coefficients[3],3)
TablePAN[18,5]=round(summary(mLN2)$coefficients[7],3)
TablePAN[18,6]=round(summary(mLN2)$coefficients[15],3)
TablePAN[19,4]=round(summary(mLN2)$coefficients[4],3)
TablePAN[19,5]=round(summary(mLN2)$coefficients[8],3)
TablePAN[19,6]=round(summary(mLN2)$coefficients[16],3)

TablePAN[16,7]=round(summary(mLN3)$r.squared-summary(mLN2)$r.squared,3)
TablePAN[16,9]=round(aLN23$"Pr(>F)"[2],3)
TablePAN[17,7]=round(summary(mLN3)$coefficients[2],3)
TablePAN[17,8]=round(summary(mLN3)$coefficients[8],3)
TablePAN[17,9]=round(summary(mLN3)$coefficients[20],3)
TablePAN[18,7]=round(summary(mLN3)$coefficients[3],3)
TablePAN[18,8]=round(summary(mLN3)$coefficients[9],3)
TablePAN[18,9]=round(summary(mLN3)$coefficients[21],3)
TablePAN[19,7]=round(summary(mLN3)$coefficients[4],3)
TablePAN[19,8]=round(summary(mLN3)$coefficients[10],3)
TablePAN[19,9]=round(summary(mLN3)$coefficients[22],3)
TablePAN[20,7]=round(summary(mLN3)$coefficients[5],3)
TablePAN[20,8]=round(summary(mLN3)$coefficients[11],3)
TablePAN[20,9]=round(summary(mLN3)$coefficients[23],3)
TablePAN[21,7]=round(summary(mLN3)$coefficients[6],3)
TablePAN[21,8]=round(summary(mLN3)$coefficients[12],3)
TablePAN[21,9]=round(summary(mLN3)$coefficients[24],3)

write.csv(TablePAN,"TablePAN_11SOM.csv",na="")

# Table 12 SOM ------------------------------------------------------------
SK=dat$statKnowledge
D1TSK=D1*SK
D2TSK=D2*SK

#power estimates (Table 11)
TableSKP=matrix(NA,21,9)

#small effects
mSd1=lm(dat$Sd~SK)
mSd2=lm(dat$Sd~SK+D1+D2)
mSd3=lm(dat$Sd~SK+D1+D2+D1TSK+D2TSK)

aSd12=anova(mSd1,mSd2)
aSd23=anova(mSd2,mSd3)

TableSKP[2,1]=round(summary(mSd1)$r.squared,3)
TableSKP[2,3]=round(summary(mSd1)$coefficients[8],3)
TableSKP[3,1]=round(summary(mSd1)$coefficients[2],3)
TableSKP[3,2]=round(summary(mSd1)$coefficients[4],3)
TableSKP[3,3]=round(summary(mSd1)$coefficients[8],3)

TableSKP[2,4]=round(summary(mSd2)$r.squared-summary(mSd1)$r.squared,3)
TableSKP[2,6]=round(aSd12$"Pr(>F)"[2],3)
TableSKP[3,4]=round(summary(mSd2)$coefficients[2],3)
TableSKP[3,5]=round(summary(mSd2)$coefficients[6],3)
TableSKP[3,6]=round(summary(mSd2)$coefficients[14],3)
TableSKP[4,4]=round(summary(mSd2)$coefficients[3],3)
TableSKP[4,5]=round(summary(mSd2)$coefficients[7],3)
TableSKP[4,6]=round(summary(mSd2)$coefficients[15],3)
TableSKP[5,4]=round(summary(mSd2)$coefficients[4],3)
TableSKP[5,5]=round(summary(mSd2)$coefficients[8],3)
TableSKP[5,6]=round(summary(mSd2)$coefficients[16],3)

TableSKP[2,7]=round(summary(mSd3)$r.squared-summary(mSd2)$r.squared,3)
TableSKP[2,9]=round(aSd23$"Pr(>F)"[2],3)
TableSKP[3,7]=round(summary(mSd3)$coefficients[2],3)
TableSKP[3,8]=round(summary(mSd3)$coefficients[8],3)
TableSKP[3,9]=round(summary(mSd3)$coefficients[20],3)
TableSKP[4,7]=round(summary(mSd3)$coefficients[3],3)
TableSKP[4,8]=round(summary(mSd3)$coefficients[9],3)
TableSKP[4,9]=round(summary(mSd3)$coefficients[21],3)
TableSKP[5,7]=round(summary(mSd3)$coefficients[4],3)
TableSKP[5,8]=round(summary(mSd3)$coefficients[10],3)
TableSKP[5,9]=round(summary(mSd3)$coefficients[22],3)
TableSKP[6,7]=round(summary(mSd3)$coefficients[5],3)
TableSKP[6,8]=round(summary(mSd3)$coefficients[11],3)
TableSKP[6,9]=round(summary(mSd3)$coefficients[23],3)
TableSKP[7,7]=round(summary(mSd3)$coefficients[6],3)
TableSKP[7,8]=round(summary(mSd3)$coefficients[12],3)
TableSKP[7,9]=round(summary(mSd3)$coefficients[24],3)

#medium effects
mMd1=lm(dat$Md~SK)
mMd2=lm(dat$Md~SK+D1+D2)
mMd3=lm(dat$Md~SK+D1+D2+D1TSK+D2TSK)

aMd12=anova(mMd1,mMd2)
aMd23=anova(mMd2,mMd3)

TableSKP[9,1]=round(summary(mMd1)$r.squared,3)
TableSKP[9,3]=round(summary(mMd1)$coefficients[8],3)
TableSKP[10,1]=round(summary(mMd1)$coefficients[2],3)
TableSKP[10,2]=round(summary(mMd1)$coefficients[4],3)
TableSKP[10,3]=round(summary(mMd1)$coefficients[8],3)

TableSKP[9,4]=round(summary(mMd2)$r.squared-summary(mMd1)$r.squared,3)
TableSKP[9,6]=round(aMd12$"Pr(>F)"[2],3)
TableSKP[10,4]=round(summary(mMd2)$coefficients[2],3)
TableSKP[10,5]=round(summary(mMd2)$coefficients[6],3)
TableSKP[10,6]=round(summary(mMd2)$coefficients[14],3)
TableSKP[11,4]=round(summary(mMd2)$coefficients[3],3)
TableSKP[11,5]=round(summary(mMd2)$coefficients[7],3)
TableSKP[11,6]=round(summary(mMd2)$coefficients[15],3)
TableSKP[12,4]=round(summary(mMd2)$coefficients[4],3)
TableSKP[12,5]=round(summary(mMd2)$coefficients[8],3)
TableSKP[12,6]=round(summary(mMd2)$coefficients[16],3)

TableSKP[9,7]=round(summary(mMd3)$r.squared-summary(mMd2)$r.squared,3)
TableSKP[9,9]=round(aMd23$"Pr(>F)"[2],3)
TableSKP[10,7]=round(summary(mMd3)$coefficients[2],3)
TableSKP[10,8]=round(summary(mMd3)$coefficients[8],3)
TableSKP[10,9]=round(summary(mMd3)$coefficients[20],3)
TableSKP[11,7]=round(summary(mMd3)$coefficients[3],3)
TableSKP[11,8]=round(summary(mMd3)$coefficients[9],3)
TableSKP[11,9]=round(summary(mMd3)$coefficients[21],3)
TableSKP[12,7]=round(summary(mMd3)$coefficients[4],3)
TableSKP[12,8]=round(summary(mMd3)$coefficients[10],3)
TableSKP[12,9]=round(summary(mMd3)$coefficients[22],3)
TableSKP[13,7]=round(summary(mMd3)$coefficients[5],3)
TableSKP[13,8]=round(summary(mMd3)$coefficients[11],3)
TableSKP[13,9]=round(summary(mMd3)$coefficients[23],3)
TableSKP[14,7]=round(summary(mMd3)$coefficients[6],3)
TableSKP[14,8]=round(summary(mMd3)$coefficients[12],3)
TableSKP[14,9]=round(summary(mMd3)$coefficients[24],3)

#large effects
mLd1=lm(dat$Ld~SK)
mLd2=lm(dat$Ld~SK+D1+D2)
mLd3=lm(dat$Ld~SK+D1+D2+D1TSK+D2TSK)

aLd12=anova(mLd1,mLd2)
aLd23=anova(mLd2,mLd3)

TableSKP[16,1]=round(summary(mLd1)$r.squared,3)
TableSKP[16,3]=round(summary(mLd1)$coefficients[8],3)
TableSKP[17,1]=round(summary(mLd1)$coefficients[2],3)
TableSKP[17,2]=round(summary(mLd1)$coefficients[4],3)
TableSKP[17,3]=round(summary(mLd1)$coefficients[8],3)

TableSKP[16,4]=round(summary(mLd2)$r.squared-summary(mLd1)$r.squared,3)
TableSKP[16,6]=round(aLd12$"Pr(>F)"[2],3)
TableSKP[17,4]=round(summary(mLd2)$coefficients[2],3)
TableSKP[17,5]=round(summary(mLd2)$coefficients[6],3)
TableSKP[17,6]=round(summary(mLd2)$coefficients[14],3)
TableSKP[18,4]=round(summary(mLd2)$coefficients[3],3)
TableSKP[18,5]=round(summary(mLd2)$coefficients[7],3)
TableSKP[18,6]=round(summary(mLd2)$coefficients[15],3)
TableSKP[19,4]=round(summary(mLd2)$coefficients[4],3)
TableSKP[19,5]=round(summary(mLd2)$coefficients[8],3)
TableSKP[19,6]=round(summary(mLd2)$coefficients[16],3)

TableSKP[16,7]=round(summary(mLd3)$r.squared-summary(mLd2)$r.squared,3)
TableSKP[16,9]=round(aLd23$"Pr(>F)"[2],3)
TableSKP[17,7]=round(summary(mLd3)$coefficients[2],3)
TableSKP[17,8]=round(summary(mLd3)$coefficients[8],3)
TableSKP[17,9]=round(summary(mLd3)$coefficients[20],3)
TableSKP[18,7]=round(summary(mLd3)$coefficients[3],3)
TableSKP[18,8]=round(summary(mLd3)$coefficients[9],3)
TableSKP[18,9]=round(summary(mLd3)$coefficients[21],3)
TableSKP[19,7]=round(summary(mLd3)$coefficients[4],3)
TableSKP[19,8]=round(summary(mLd3)$coefficients[10],3)
TableSKP[19,9]=round(summary(mLd3)$coefficients[22],3)
TableSKP[20,7]=round(summary(mLd3)$coefficients[5],3)
TableSKP[20,8]=round(summary(mLd3)$coefficients[11],3)
TableSKP[20,9]=round(summary(mLd3)$coefficients[23],3)
TableSKP[21,7]=round(summary(mLd3)$coefficients[6],3)
TableSKP[21,8]=round(summary(mLd3)$coefficients[12],3)
TableSKP[21,9]=round(summary(mLd3)$coefficients[24],3)

write.csv(TableSKP,"TableSKP_12SOM.csv",na="")

# Table 13 SOM ------------------------------------------------------------
TableSKN=matrix(NA,21,9)

#small effects
mSN1=lm(dat$powSd~SK)
mSN2=lm(dat$powSd~SK+D1+D2)
mSN3=lm(dat$powSd~SK+D1+D2+D1TSK+D2TSK)

aSN12=anova(mSN1,mSN2)
aSN23=anova(mSN2,mSN3)

TableSKN[2,1]=round(summary(mSN1)$r.squared,3)
TableSKN[2,3]=round(summary(mSN1)$coefficients[8],3)
TableSKN[3,1]=round(summary(mSN1)$coefficients[2],3)
TableSKN[3,2]=round(summary(mSN1)$coefficients[4],3)
TableSKN[3,3]=round(summary(mSN1)$coefficients[8],3)

TableSKN[2,4]=round(summary(mSN2)$r.squared-summary(mSN1)$r.squared,3)
TableSKN[2,6]=round(aSN12$"Pr(>F)"[2],3)
TableSKN[3,4]=round(summary(mSN2)$coefficients[2],3)
TableSKN[3,5]=round(summary(mSN2)$coefficients[6],3)
TableSKN[3,6]=round(summary(mSN2)$coefficients[14],3)
TableSKN[4,4]=round(summary(mSN2)$coefficients[3],3)
TableSKN[4,5]=round(summary(mSN2)$coefficients[7],3)
TableSKN[4,6]=round(summary(mSN2)$coefficients[15],3)
TableSKN[5,4]=round(summary(mSN2)$coefficients[4],3)
TableSKN[5,5]=round(summary(mSN2)$coefficients[8],3)
TableSKN[5,6]=round(summary(mSN2)$coefficients[16],3)

TableSKN[2,7]=round(summary(mSN3)$r.squared-summary(mSN2)$r.squared,3)
TableSKN[2,9]=round(aSN23$"Pr(>F)"[2],3)
TableSKN[3,7]=round(summary(mSN3)$coefficients[2],3)
TableSKN[3,8]=round(summary(mSN3)$coefficients[8],3)
TableSKN[3,9]=round(summary(mSN3)$coefficients[20],3)
TableSKN[4,7]=round(summary(mSN3)$coefficients[3],3)
TableSKN[4,8]=round(summary(mSN3)$coefficients[9],3)
TableSKN[4,9]=round(summary(mSN3)$coefficients[21],3)
TableSKN[5,7]=round(summary(mSN3)$coefficients[4],3)
TableSKN[5,8]=round(summary(mSN3)$coefficients[10],3)
TableSKN[5,9]=round(summary(mSN3)$coefficients[22],3)
TableSKN[6,7]=round(summary(mSN3)$coefficients[5],3)
TableSKN[6,8]=round(summary(mSN3)$coefficients[11],3)
TableSKN[6,9]=round(summary(mSN3)$coefficients[23],3)
TableSKN[7,7]=round(summary(mSN3)$coefficients[6],3)
TableSKN[7,8]=round(summary(mSN3)$coefficients[12],3)
TableSKN[7,9]=round(summary(mSN3)$coefficients[24],3)

#medium effects
mMN1=lm(dat$powMd~SK)
mMN2=lm(dat$powMd~SK+D1+D2)
mMN3=lm(dat$powMd~SK+D1+D2+D1TSK+D2TSK)

aMN12=anova(mMN1,mMN2)
aMN23=anova(mMN2,mMN3)

TableSKN[9,1]=round(summary(mMN1)$r.squared,3)
TableSKN[9,3]=round(summary(mMN1)$coefficients[8],3)
TableSKN[10,1]=round(summary(mMN1)$coefficients[2],3)
TableSKN[10,2]=round(summary(mMN1)$coefficients[4],3)
TableSKN[10,3]=round(summary(mMN1)$coefficients[8],3)

TableSKN[9,4]=round(summary(mMN2)$r.squared-summary(mMN1)$r.squared,3)
TableSKN[9,6]=round(aMN12$"Pr(>F)"[2],3)
TableSKN[10,4]=round(summary(mMN2)$coefficients[2],3)
TableSKN[10,5]=round(summary(mMN2)$coefficients[6],3)
TableSKN[10,6]=round(summary(mMN2)$coefficients[14],3)
TableSKN[11,4]=round(summary(mMN2)$coefficients[3],3)
TableSKN[11,5]=round(summary(mMN2)$coefficients[7],3)
TableSKN[11,6]=round(summary(mMN2)$coefficients[15],3)
TableSKN[12,4]=round(summary(mMN2)$coefficients[4],3)
TableSKN[12,5]=round(summary(mMN2)$coefficients[8],3)
TableSKN[12,6]=round(summary(mMN2)$coefficients[16],3)

TableSKN[9,7]=round(summary(mMN3)$r.squared-summary(mMN2)$r.squared,3)
TableSKN[9,9]=round(aMN23$"Pr(>F)"[2],3)
TableSKN[10,7]=round(summary(mMN3)$coefficients[2],3)
TableSKN[10,8]=round(summary(mMN3)$coefficients[8],3)
TableSKN[10,9]=round(summary(mMN3)$coefficients[20],3)
TableSKN[11,7]=round(summary(mMN3)$coefficients[3],3)
TableSKN[11,8]=round(summary(mMN3)$coefficients[9],3)
TableSKN[11,9]=round(summary(mMN3)$coefficients[21],3)
TableSKN[12,7]=round(summary(mMN3)$coefficients[4],3)
TableSKN[12,8]=round(summary(mMN3)$coefficients[10],3)
TableSKN[12,9]=round(summary(mMN3)$coefficients[22],3)
TableSKN[13,7]=round(summary(mMN3)$coefficients[5],3)
TableSKN[13,8]=round(summary(mMN3)$coefficients[11],3)
TableSKN[13,9]=round(summary(mMN3)$coefficients[23],3)
TableSKN[14,7]=round(summary(mMN3)$coefficients[6],3)
TableSKN[14,8]=round(summary(mMN3)$coefficients[12],3)
TableSKN[14,9]=round(summary(mMN3)$coefficients[24],3)

#large effects
mLN1=lm(dat$powLd~SK)
mLN2=lm(dat$powLd~SK+D1+D2)
mLN3=lm(dat$powLd~SK+D1+D2+D1TSK+D2TSK)

aLN12=anova(mLN1,mLN2)
aLN23=anova(mLN2,mLN3)

TableSKN[16,1]=round(summary(mLN1)$r.squared,3)
TableSKN[16,3]=round(summary(mLN1)$coefficients[8],3)
TableSKN[17,1]=round(summary(mLN1)$coefficients[2],3)
TableSKN[17,2]=round(summary(mLN1)$coefficients[4],3)
TableSKN[17,3]=round(summary(mLN1)$coefficients[8],3)

TableSKN[16,4]=round(summary(mLN2)$r.squared-summary(mLN1)$r.squared,3)
TableSKN[16,6]=round(aLN12$"Pr(>F)"[2],3)
TableSKN[17,4]=round(summary(mLN2)$coefficients[2],3)
TableSKN[17,5]=round(summary(mLN2)$coefficients[6],3)
TableSKN[17,6]=round(summary(mLN2)$coefficients[14],3)
TableSKN[18,4]=round(summary(mLN2)$coefficients[3],3)
TableSKN[18,5]=round(summary(mLN2)$coefficients[7],3)
TableSKN[18,6]=round(summary(mLN2)$coefficients[15],3)
TableSKN[19,4]=round(summary(mLN2)$coefficients[4],3)
TableSKN[19,5]=round(summary(mLN2)$coefficients[8],3)
TableSKN[19,6]=round(summary(mLN2)$coefficients[16],3)

TableSKN[16,7]=round(summary(mLN3)$r.squared-summary(mLN2)$r.squared,3)
TableSKN[16,9]=round(aLN23$"Pr(>F)"[2],3)
TableSKN[17,7]=round(summary(mLN3)$coefficients[2],3)
TableSKN[17,8]=round(summary(mLN3)$coefficients[8],3)
TableSKN[17,9]=round(summary(mLN3)$coefficients[20],3)
TableSKN[18,7]=round(summary(mLN3)$coefficients[3],3)
TableSKN[18,8]=round(summary(mLN3)$coefficients[9],3)
TableSKN[18,9]=round(summary(mLN3)$coefficients[21],3)
TableSKN[19,7]=round(summary(mLN3)$coefficients[4],3)
TableSKN[19,8]=round(summary(mLN3)$coefficients[10],3)
TableSKN[19,9]=round(summary(mLN3)$coefficients[22],3)
TableSKN[20,7]=round(summary(mLN3)$coefficients[5],3)
TableSKN[20,8]=round(summary(mLN3)$coefficients[11],3)
TableSKN[20,9]=round(summary(mLN3)$coefficients[23],3)
TableSKN[21,7]=round(summary(mLN3)$coefficients[6],3)
TableSKN[21,8]=round(summary(mLN3)$coefficients[12],3)
TableSKN[21,9]=round(summary(mLN3)$coefficients[24],3)

write.csv(TableSKN,"TableSKN_13SOM.csv",na="")
