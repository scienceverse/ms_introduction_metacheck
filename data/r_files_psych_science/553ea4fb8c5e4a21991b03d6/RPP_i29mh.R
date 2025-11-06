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
#@ Study Title: Value of believing in free will
#@ Coder name: Bobby den Bezemer
#@ Coder e-mail: bobbydenbezemer@gmail.com
#@ Type of statistic: t statistic
#@ Type of effect-size: cohen's d
#@ OSF link project: https://osf.io/i29mh/
#@ OSF link replication report: https://osf.io/5bn6g/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("foreign")
library("effsize")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
File <- getOSFfile("https://osf.io/ibp86/")
Data <- read.spss(File, to.data.frame = TRUE)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# releveling the factor condition, otherwise I get a negative effect size
Data$cond <- relevel(Data$cond, ref = "determinism essay")
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

# A test test with number of space bar presses as dependent variable
# and condition as independent variable
Result <- t.test(number_nonpresses ~ cond, data = Data)
print(Result) 

##@@ STATISTIC @@##
Result$statistic
  
##@@ P-VALUE @@##
Result$p.value
  
##@@ SAMPLE SIZE @@##
nrow(Data)
  
##@@ EFFECT SIZE @@##
cohen.d(number_nonpresses ~ cond, data = Data)$estimate
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: 