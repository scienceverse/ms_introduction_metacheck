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
#@ Study Title: Keeping one’s distance: The influence of spatial distance cues on affect and evaluation.
#@ Coder name: Dan Martin
#@ Coder e-mail: dpmartin42@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: eta^2
#@ OSF link project: https://osf.io/vnsqg/
#@ OSF link replication report: https://osf.io/7uh8g/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library(gdata)
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
theFile <- getOSFfile("sqcja")
myData <- read.xls(theFile)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Delete those who are an only child (N now matches with reported N)
myData <- na.omit(myData)

# Coerce group to be a factor
myData$COND <- as.factor(myData$COND)

# Average the three dependent measures (siblings, parents, homebound)
myData$outcome <- rowMeans(myData[, c("SIB_BOND", "PAR_BOND", "HOME_BOND")], na.rm = TRUE)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

the_mod <- lm(outcome ~ COND, data = myData)
anova(the_mod)

##@@ STATISTIC @@##
anova(the_mod)[[4]][1]

##@@ P-VALUE @@##
anova(the_mod)[[5]][1]
  
##@@ SAMPLE SIZE @@##
nrow(myData)

##@@ EFFECT SIZE @@##
anova(the_mod)[2][1, 1]/(anova(the_mod)[2][1, 1] + anova(the_mod)[2][2, 1])
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---