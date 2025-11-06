#############################################################################
# Face-Based Judgments: Accuracy, Validity, and a Potential Underlying Mechanism 
# R code for analyzing data in study 2b
#############################################################################

#----------------------------------------------
# Read data
#----------------------------------------------

# set working directory
mywd=  
setwd(mywd)

# read data from Korean participants
df = read.table(file="study2b_data_K.txt",header=T)
# variables
# id: participant's id
# pair: id of pairs (clean-record politician vs. corruption-record politician) 
# condition: judgment type (0 = corruptibility judgment & 1 = trustworthiness judgment)
# accuracy: binary decision of choosing a corruptible or trustworthy politician (0 = wrong & 1 = correct)
# confidence: participants’ confidence about their face-based judgments
# age: participant's age
# female: participant's gender (0 = male & 1 = female)
# income: participant's income (numeric value)
# politic: participant's political orientation (1 = liberal, 2 = moderate, & 3 = conservative)
# ladder: participant's social class (higher score indicates higher social class)

#----------------------------------------------
# Preprocess data
#----------------------------------------------

# grand-mean centering age
df$g.age = df$age - mean(df$age, rm.na=T)
df$g.confidence = df$confidence - mean(df$confidence, rm.na=T)

#---------------------------------------------------------
#  Analysis
#---------------------------------------------------------

## required packages
library(lme4)
library(piecewiseSEM)

# Models
  ## 1) Judgment Accuracy (Replication of Study 1, Main Text)
    #### corruptibility judgment condition
    mod1 = glmer(accuracy~(1|id)+(1|pair),
                 data=df[df$condition==0,], family=binomial(link="logit")) # average accuracy in corruptibility condition
    mod2 = glmer(accuracy~g.age+female+(1|id)+(1|pair),
                 data=df[df$condition==0,], family=binomial(link="logit")) # mod1 + age,gender controlled
    #### trustworthiness judgment condition
    mod3 = glmer(accuracy~(1|id)+(1|pair),
                 data=df[df$condition==1,], family=binomial(link="logit")) # average accuracy in trustworthiness condition
    mod4 = glmer(accuracy~g.age+female+(1|id)+(1|pair),
                 data=df[df$condition==1,], family=binomial(link="logit")) # mod3 + age,gender controlled
  ## 2) Relationship between Judgment Confidence and Accuracy (Supplemental Material) 
    mod5 = glmer(accuracy~g.confidence*condition+(1|id)+(1|pair),
               data=df, family=binomial(link="logit")) # effect of confidence on judgment accuracy
    mod6 = glmer(accuracy~g.confidence*condition+g.age+female+(1|id)+(1|pair),
               data=df, family=binomial(link="logit")) # mod5 + age,gender controlled
  ## 3) Three-way interaction between Gender*Confidence*Condition  
    mod7 = glmer(accuracy~g.confidence*condition*female+g.age+(1|id)+(1|pair),
               data=df, family=binomial(link="logit"))

# confidence intervals
ci.mod1 = confint.merMod(mod1)
ci.mod2 = confint.merMod(mod2)
ci.mod3 = confint.merMod(mod3)
ci.mod4 = confint.merMod(mod4)
ci.mod5 = confint.merMod(mod5)
ci.mod6 = confint.merMod(mod6)
ci.mod7 = confint.merMod(mod7)

# R squared
rsq = rsquared(list(mod2,mod4,mod5,mod6,mod7)) 

#---------------------------------------------------------
#  Results
#---------------------------------------------------------

# 1) Judgment Accuracy (Replication of Study 1, Main Text)
r.mod1 = summary(mod1)$coefficients
r.mod1 = cbind(r.mod1, exp(r.mod1[,1]),exp(ci.mod1[3,1]),exp(ci.mod1[3,2]))
colnames(r.mod1)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod1,3)) # result for model 1

r.mod2 = summary(mod2)$coefficients
r.mod2 = cbind(r.mod2, exp(r.mod2[,1]),exp(ci.mod2[3:5,]))
colnames(r.mod2)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod2,3)) # result for model 2

r.mod3 = summary(mod3)$coefficients
r.mod3 = cbind(r.mod3, exp(r.mod3[,1]),exp(ci.mod3[3,1]),exp(ci.mod3[3,2]))
colnames(r.mod3)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod3,3)) # result for model 3

r.mod4 = summary(mod4)$coefficients
r.mod4 = cbind(r.mod4, exp(r.mod4[,1]),exp(ci.mod4[3:5,]))
colnames(r.mod4)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod4,3)) # result for model 4

# 2) Relationship between Judgment Confidence and Accuracy (Supplemental Material) 
r.mod5 = summary(mod5)$coefficients
r.mod5 = cbind(r.mod5, exp(r.mod5[,1]),exp(ci.mod5[3:6,]))
colnames(r.mod5)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod5,3)) # result for model 5

r.mod6 = summary(mod6)$coefficients
r.mod6 = cbind(r.mod6, exp(r.mod6[,1]),exp(ci.mod6[3:8,]))
colnames(r.mod6)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod6,3)) # result for model 6

# 3) Three-way interaction between Gender*Confidence*Condition  
r.mod7 = summary(mod7)$coefficients
r.mod7 = cbind(r.mod7, exp(r.mod7[,1]),exp(ci.mod7[-c(1:2),]))
colnames(r.mod7)[c(5,6,7)] = c("OR","2.5% CI", "97.5% CI")
print(round(r.mod7,3)) # result for model 6

## R-squares for model 2, model 4, model 5, model 6, model 7
print(rsq) 



