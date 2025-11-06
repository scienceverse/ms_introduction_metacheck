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
#@ Study Title: Increasing and decreasing motor and cognitive output: a model of general action and inaction goals. 
#@ Coder name: Onur Sahin
#@ Coder e-mail: 92onursahin@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: Eta squared
#@ OSF link project: https://osf.io/vy1bc/
#@ OSF link replication report: https://osf.io/2pbaf/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
File <- getOSFfile('https://osf.io/qxstp/')  # I used their final data
Data <- read.csv(File)   #Load the data into R
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# No data manipulation needed
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
Results <- aov(totalScore ~ condition, data = Data)
summary(Results)

##@@ STATISTIC @@##
summary(Results)[[1]][["F value"]][[1]] #extracted the F value

##@@ P-VALUE @@##
summary(Results)[[1]][["Pr(>F)"]][[1]]  #extracted the P value
  
##@@ SAMPLE SIZE @@##
nrow(Data) 

##@@ EFFECT SIZE @@##
sumsqEffect <- summary(Results)[[1]][["Sum Sq"]][[1]]
sumsqError <- summary(Results)[[1]][["Sum Sq"]][[2]]
sumsqEffect / (sumsqEffect + sumsqError)
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---