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
#@ Study Title: The rejection of moral rebels: resenting those who do the right thing
#@ Coder name: Stepan Bahnik
#@ Coder e-mail: bahniks@seznam.cz
#@ Type of statistic: F
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/pz0my/
#@ OSF link replication report: https://osf.io/a4fmg/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("heplots")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile("yg7ts")
data <- read.csv(file)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
attraction.composite <- rowMeans(data[, 18:20], na.rm = T)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
results <- aov(attraction.composite ~ data$Condition)

##@@ STATISTIC @@##
unlist(summary(results))["F value1"]

##@@ P-VALUE @@##
unlist(summary(results))["Pr(>F)1"]
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
etasq(results)[1, ]
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---