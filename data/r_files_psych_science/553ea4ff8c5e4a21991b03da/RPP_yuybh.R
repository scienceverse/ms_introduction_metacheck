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
#@ Study Title: Creating social connection through inferential reproduction: Loneliness and perceived agency in gadgets, Gods and greyhounds.
#@ Coder name: Dan Martin
#@ Coder e-mail: dpmartin42@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: eta^2
#@ OSF link project: https://osf.io/yuybh/
#@ OSF link replication report: https://osf.io/m5a2c/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library(Hmisc)
library(dplyr)
library(reshape2)
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
theFile <- getOSFfile("https://osf.io/wbtkm/")
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Outputs a warning but everything seems ok
myData <- spss.get(theFile)

# Get columns and remove the three participants who need to be deleted
myDataSub <- dplyr::select(myData, Include.YesNo, ParticipantNo, Condition, Drawing.Total.Z, Belief.Avg.Z, Pet.Traits.AnthroSocial.Z) %>%
  filter(Include.YesNo == "Yes") 

names(myDataSub) <- c("toInclude", "ParticipantNo", "condition", "drawing_z", "belief_z", "pets_z")

dataLong <- melt(myDataSub[, -1], id.vars = c("ParticipantNo", "condition")) %>%
  arrange(ParticipantNo)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

theMod <- aov(value ~ condition * variable + Error(ParticipantNo), data = dataLong)
summary(theMod)

##@@ STATISTIC @@##
summary(theMod)[[2]][[1]][["F value"]][3]

##@@ P-VALUE @@##
summary(theMod)[[2]][[1]][["Pr(>F)"]][3]
  
##@@ SAMPLE SIZE @@##
nrow(myDataSub)

##@@ EFFECT SIZE @@##
summary(theMod)[[2]][[1]][["Sum Sq"]][3]/(summary(theMod)[[2]][[1]][["Sum Sq"]][3] + summary(theMod)[[2]][[1]][["Sum Sq"]][4])
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---