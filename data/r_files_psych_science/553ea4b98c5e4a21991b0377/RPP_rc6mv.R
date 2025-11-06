################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Why Do Implicit and Explicit Attitude Tests Diverge? The Role of Structural Fit
#@ Coder name: Felix Henninger
#@ Coder e-mail: felix.henninger@web.de
#@ Type of statistic: t-test (R^2 change)
#@ Type of effect-size: delta R^2
#@ OSF link project: https://osf.io/rc6mv/
#@ OSF link replication report: https://osf.io/79y8g/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
library('foreign')
library('lme4')

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

# The first dataset is targeted to the specific analysis reported here
file <- getOSFfile('https://osf.io/msrg2/')
data <- data.frame(read.spss(file))

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object

model1 <- lm(direct ~ indirect + manip, data=data)
model2 <- lm(direct ~ indirect + manip + indirect:manip, data=data)

test <- anova(model1, model2)
print(test)

##@@ STATISTIC @@##
test$F[2]

##@@ P-VALUE @@##
test[['Pr(>F)']][2]
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
summary(model2)$r.squared - summary(model1)$r.squared
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: None