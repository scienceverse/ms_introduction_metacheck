#############################################################################
# Face-Based Judgments: Accuracy, Validity, and a Potential Underlying Mechanism 
# R code for analyzing Korean participants' data in study 3 
#############################################################################

#----------------------------------------------
# Read data
#----------------------------------------------

# set working directory
mywd="C:\\face study"  
setwd(mywd)

# read data from Korean participants
df = read.table(file="study3_data_K.txt",header=T)
# variables
# id: participant's id
# condition: 4 different scenarios
# noffer: total number of bribe offers received
# y.bin: binary decision of accepting a bribe offer (0:no; 1:yes)
# y.freq: total number of bribe offers accepted
# y.rfreq: relative frequency of accepting bribe offers (y.freq/n.offer)
# trust: average rating on trustworthy 
# corruptible: average rating on corruptible 
# ethical: average rating on ethical 
# honest: average rating on honest 
# dominant: average rating on dominant 
# competent: average rating on competent 

#----------------------------------------------
# Preprocess data
#----------------------------------------------

# define function 'gmc' for grand-mean centering
gmc = function(x){
  xc = x - mean(x,rm.na=T)
  return(xc)
}

# define function pmean for calculating person-specific means
pmean = function(df,xname,iname){
  mean.v = tapply(X=df[,xname],INDEX=df[,iname],mean)
  id.v = rownames(mean.v)
  df.mean = data.frame(id.v,mean.v)
  colnames(df.mean) = c(iname,paste("m.",xname,sep=""))
  dfr = merge(x=df,y=df.mean,by=iname)
  return(dfr)
}

# names of the variables to be pre-processed
varnames = c("trust","corruptible","ethical","honest","dominant","competent","noffer")

# grand-mean centering for the predictors
for (i in 1:length(varnames)){
  df[,paste("g.",varnames[i],sep="")] = gmc(df[,varnames[i]])
}

# obtain person-specific means for the predictors
for (i in 1:length(varnames)){
  df = pmean(df=df,xname=paste("g.",varnames[i],sep=""),iname="id")
}

#----------------------------------------------
# Analysis 1: predicting y.bin (Table S4)
#---------------------------------------------- 
#install.packages(pkgs="glmmTMB")
library(glmmTMB)

# names of the predictors to be analyzed
varnames.p = c("trust","corruptible","ethical","honest","dominant","competent")

# main analysis
result1 = list()
for (i in 1:length(varnames.p)){
  myformula = paste("y.bin~","m.g.",varnames.p[i],"+(1|id)",sep="")
  result1[[i]] = glmmTMB(formula(myformula),data=df,family="binomial")
}

# confidence intervals
ci1 = list()
for (i in 1:length(varnames.p)){
  ci1[[i]] = confint(result1[[i]])
}

# R squared (using rsqglmm.R function https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/inst/misc/rsqglmm.R)
rsq1 = list()
for (i in 1:length(varnames.p)){
  rsq1[[i]] = my_rsq(result1[[i]])
}

# print result
for (i in 1:length(varnames.p)){
  r = summary(result1[[i]])$coefficients$cond
  r = cbind(r,exp(r[,1]),exp(ci1[[i]][c(1:2),]))
  colnames(r)[5] = "OR"
  print(r)
}
print(rsq1)


#----------------------------------------------
# Analysis 2: predicting y.rfreq (Table S5)
#---------------------------------------------- 

# main analysis
result2 = list()
for (i in 1:length(varnames.p)){
  myformula = paste("y.rfreq~","m.g.",varnames.p[i],"+(1|id)",sep="")
  result2[[i]] = glmmTMB(formula(myformula),data=df,family="binomial",weights=noffer)
}

# confidence intervals
ci2 = list()
for (i in 1:length(varnames.p)){
  ci2[[i]] = confint(result2[[i]])
}

# R squared (using rsqglmm.R function https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/inst/misc/rsqglmm.R)
rsq2 = list()
for (i in 1:length(varnames.p)){
  rsq2[[i]] = my_rsq(result2[[i]])
}

# print result
for (i in 1:length(varnames.p)){
  r = summary(result2[[i]])$coefficients$cond
  r = cbind(r,exp(r[,1]),exp(ci2[[i]][c(1:2),]))
  colnames(r)[5] = "OR"
  print(r)
}
print(rsq2)

#----------------------------------------------
# Analysis 3: predicting y.freq (Table S6)
#---------------------------------------------- 

# main analysis
result3 = list()
for (i in 1:length(varnames.p)){
  myformula = paste("y.freq~","m.g.",varnames.p[i],"+(1|id)",sep="")
  result3[[i]] = glmmTMB(formula(myformula),data=df,family="nbinom2",ziformula=~1)
}

# confidence intervals
ci3 = list()
for (i in 1:length(varnames.p)){
  ci3[[i]] = confint(result3[[i]])
}

# R squared (using rsqglmm.R function https://github.com/glmmTMB/glmmTMB/blob/master/glmmTMB/inst/misc/rsqglmm.R)
rsq3 = list()
for (i in 1:length(varnames.p)){
  rsq3[[i]] = my_rsq(result3[[i]])
}

# print result 
for (i in 1:length(varnames.p)){
  r = summary(result3[[i]])$coefficients$cond
  r = cbind(r,exp(r[,1]),exp(ci3[[i]][c(1:2),]))
  colnames(r)[5] = "IRR"
  print(r)
}
print(rsq3)


#----------------------------------------------
# Analysis 4: mediation analysis (Table 1 & Table S7)
#----------------------------------------------

## X (trait) on M (noffer)
result4 = list()
for (i in 1:length(varnames.p)){
  myformula = paste("noffer~","m.g.",varnames.p[i],"+(1|id)",sep="")
  result4[[i]] = glmmTMB(formula(myformula),data=df,family="poisson")
}

# confidence intervals
ci4 = list()
for (i in 1:length(varnames.p)){
  ci4[[i]] = confint(result4[[i]])
}

# print result (Table 1 & S7)
for (i in 1:length(varnames.p)){
  r = summary(result4[[i]])$coefficients$cond
  r = cbind(r,exp(r[,1]),exp(ci4[[i]][c(1:2),]))
  colnames(r)[5] = "IRR"
  print(r)
}

## Y (y.freq) on X (trait) and M (noffer)
result5 = list()
for (i in 1:length(varnames.p)){
  myformula = paste("y.freq~noffer+m.g.",varnames.p[i],"+(1|id)",sep="")
  result5[[i]] = glmmTMB(formula(myformula),data=df,family="nbinom2",ziformula=~1)
}
# confidence intervals
ci5 = list()
for (i in 1:length(varnames.p)){
  ci5[[i]] = confint(result5[[i]])
}

# print result (Table 1 & S7)
for (i in 1:length(varnames.p)){
  r = summary(result5[[i]])$coefficients$cond
  r = cbind(r,exp(r[,1]),exp(ci5[[i]][c(1:3),]))
  colnames(r)[5] = "IRR"
  print(r)
}

save.image(file="study3_Results_K.RData")
