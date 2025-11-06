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
#@ Study Title: Distinguishing between silent and vocal minorities: Not all deviants feel marginal.
#@ Coder name: Dan Martin
#@ Coder e-mail: dpmartin42@gmail.com
#@ Type of statistic: X^2
#@ Type of effect-size: Cramer's V
#@ OSF link project: https://osf.io/nhwv5/
#@ OSF link replication report: https://osf.io/2jwi6/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library(gdata)
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
theFile <- getOSFfile("https://osf.io/ftzbj/")
myData <- read.xls(theFile)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Data format is free-hand xls, need to get counts and proportions for Staunton

demo_prop <- as.numeric(as.character(myData[17, 14]))/100
demo_N <- as.numeric(as.character(myData[17, 15]))
demo_registered <- as.numeric(as.character(myData[18, 14]))/100

rep_prop <- as.numeric(as.character(myData[17, 16]))/100
rep_N <- as.numeric(as.character(myData[17, 17]))
rep_registered <- as.numeric(as.character(myData[18, 16]))/100
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

nullProbs <- c(demo_registered, rep_registered)
theMod <- chisq.test(c(demo_N, rep_N), p = nullProbs)
theMod

##@@ STATISTIC @@##
theMod$statistic

##@@ P-VALUE @@##
theMod$p.value
  
##@@ SAMPLE SIZE @@##
demo_N + rep_N

##@@ EFFECT SIZE @@##
sqrt(theMod$statistic/(demo_N + rep_N))
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---