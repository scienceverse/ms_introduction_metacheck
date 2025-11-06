#############################################################################
# Face-Based Judgments: Accuracy, Validity, and a Potential Underlying Mechanism 
# R code for analyzing American participants' data in study 3 
#############################################################################

#----------------------------------------------
# Read data
#----------------------------------------------

# set working directory
mywd="C:\\face study"
setwd(mywd)

# read data from American participants
df = read.table(file="study3_data_A.txt",header=T)
# variables
# id: participant's id
# condition: 4 different scenarios
# target: Korean target's id 
# offer: binary decision of offering a bribe offer (0:no; 1:yes)
# trust: rating of the target's face on trustworthy 
# corruptible: rating of the target's face on corruptible 
# ethical: rating of the target's face on ethical 
# honest: rating of the target's face on honest 
# dominant: rating of the target's face on dominant 
# competent: rating of the target's face on competent 
# emotion: rating of the target's face on emotional expression
# attractive: rating of the target's face on attractiveness

#----------------------------------------------
# Preprocess data
#----------------------------------------------

# define function 'gmc' for grand-mean centering
gmc = function(x){
  xc = x - mean(x,rm.na=T)
  return(xc)
}

# names of the variables to be pre-processed
varnames = c("trust","corruptible","ethical","honest","dominant","competent","emotion","attractive")

# grand-mean centering for the predictors
for (i in 1:length(varnames)){
  df[,paste("g.",varnames[i],sep="")] = gmc(df[,varnames[i]])
}

#----------------------------------------------
# Main analysis: predicting bribe offer (Table S3)
#---------------------------------------------- 

# required package
#install.packages("lme4")
#install.packages("piecewiseSEM")
library(lme4)
library(piecewiseSEM)

# names of the predictors to be analyzed
varnames.p = c("trust","corruptible","ethical","honest","dominant","competent")

# main analysis
result = list()
for (i in 1:length(varnames.p)){
  myformula = paste("offer~g.",varnames.p[i],"+g.attractive+g.emotion+(1|id)+(1|target)",sep="")
  result[[i]] = glmer(formula(myformula),data=df,family=binomial(link="logit"))
}

# confidence intervals
ci = list()
for (i in 1:length(varnames.p)){
  ci[[i]] = confint.merMod(result[[i]])
}

# R squared
rsq = rsquared(modelList=list(result[[1]],result[[2]],result[[3]],result[[4]],result[[5]],result[[6]]))

# Print result (Table S3)
for (i in 1:length(varnames.p)){
  r = summary(result[[i]])$coefficients
  r = cbind(r,exp(r[,1]),exp(ci[[i]][c(3:6),]))
  colnames(r)[5] = "OR"
  print(round(r,3))
}
print(rsq)

save.image(file="study3_Results_A.RData")

