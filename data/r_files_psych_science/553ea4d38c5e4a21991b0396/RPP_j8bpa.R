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
#@ Study Title: Self-regulation and selective exposure
#@ Coder name: Bobby den Bezemer
#@ Coder e-mail: bobbydenbezemer@gmail.com
#@ Type of statistic: F statistic
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/j8bpa/
#@ OSF link replication report: https://osf.io/9pnct/

##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("foreign")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
File <- getOSFfile("https://osf.io/htu4p/")
Data <- read.spss(File, to.data.frame = TRUE)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# setting the independent variable to factor
Data$condition <- as.factor(Data$condition)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

# one way anova with Z scores of bias as dependent variable and condition
# as independent variable
Results <- aov(Zbias ~ condition, data = Data)
summary(Results)

##@@ STATISTIC @@##
summary(Results)[[1]][["F value"]][1]

##@@ P-VALUE @@##
summary(Results)[[1]][["Pr(>F)"]][1]
  
##@@ SAMPLE SIZE @@##
nrow(Data)

##@@ EFFECT SIZE @@##

# get the sum of squares for the effect
SS_effect <- summary(Results)[[1]][["Sum Sq"]][1]
# get the error sum of squares
SS_error <- summary(Results)[[1]][["Sum Sq"]][2]
# fill out the formula for the partial_eta_squared
eta_squared = SS_effect / (SS_effect + SS_error)
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: 