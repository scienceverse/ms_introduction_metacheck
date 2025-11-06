#############################################################################
# Face-Based Judgments: Accuracy, Validity, and a Potential Underlying Mechanism 
# R code for analyzing data in study 1
#############################################################################

#----------------------------------------------
# Read data
#----------------------------------------------

# set working directory
mywd = 
setwd(mywd)

# read data from American participants
df = read.table(file="study1_data.txt",header=T)
# variables
# id: participant's id
# pair: id of pairs (clean-record politician vs. corruption-record politician) 
# accuracy: binary decision of choosing a trustworthy politician (0 = wrong & 1 = correct)
# rt: response time (seconds)
# age: participant's age
# gender: participant's gender (string value)
# female: participant's gender (numeric value; 0 = male & 1 = female)
# race_a: participant's race (string value)
# race_b: participant's race (numeric value)
# income_a: participant's income (string value)
# income_b: participant's income (numeric value)
# ladder: participant's social class (higher score indicates higher social class)

#----------------------------------------------
# Preprocess data
#----------------------------------------------

# grand-mean centering age
df$g.age = df$age - mean(df$age, rm.na=T)


#---------------------------------------------------------
#  Analysis
#---------------------------------------------------------

## required packages
library(lme4)
library(piecewiseSEM)

# main analysis of model 1 and model 2 (age & gender controlled )
mod1 = glmer(accuracy~(1|id)+(1|pair),data=df,family=binomial(link="logit"))
mod2 = glmer(accuracy~female+g.age + (1|id)+(1|pair),data=df,family=binomial(link="logit")) 

# confidence intervals
ci.mod1 = confint.merMod(mod1)
ci.mod2 = confint.merMod(mod2)

# R squared
rsq = rsquared(mod2)

# Print results (Table S1)
r.mod1 = summary(mod1)$coefficients
r.mod1 = cbind(r.mod1, exp(r.mod1[,1]),exp(ci.mod1[3,1]),exp(ci.mod1[3,2]))
colnames(r.mod1)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod1,3)) # result for model 1

r.mod2 = summary(mod2)$coefficients
r.mod2 = cbind(r.mod2, exp(r.mod2[,1]),exp(ci.mod2[3:5,]))
colnames(r.mod2)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod2,3)) # result for model 2

print(rsq) # R-square for model 2


