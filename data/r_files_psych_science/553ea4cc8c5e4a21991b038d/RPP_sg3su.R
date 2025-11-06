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
#@ Study Title: Why People are Reluctant to Tempt Fate
#@ Coder name: Erica Baranski
#@ Coder e-mail: Ericanbaranski@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: d
#@ OSF link project: https://osf.io/sg3su/
#@ OSF link replication report: https://osf.io/wvcgb/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile("https://osf.io/9jvxd/")
data <- read.csv(file)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
#NONE
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
results <-aov(Likelihood ~ Load * Had.read, data = data)
summ<-summary(results)

##@@ STATISTIC @@##
summ[[1]][["F value"]][3]

##@@ P-VALUE @@##
summ[[1]][["Pr(>F)"]][3]
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
SumSq_effect <- summ[[1]][["Sum Sq"]][3]
SumSq_error <- summ[[1]][["Sum Sq"]][4]
SumSq_effect / (SumSq_effect + SumSq_error)
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---