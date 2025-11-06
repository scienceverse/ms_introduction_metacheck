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
#@ Study Title: Replication of Reynolds & Besner (2008, JEP:LMC, Study 5)
#@ Coder name: Daan van Renswoude
#@ Coder e-mail: d.r.vanrenswoude@uva.nl
#@ Type of statistic: F
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/hasfu/
#@ OSF link replication report: https://osf.io/fkcn5/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("foreign")
library("reshape2")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile('https://osf.io/kxf3h/')
data <- read.spss(file, to.data.frame = T)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# select the items, z-scores and wordclassifications
data <- data[,c(1:2, 5:6)]
# transform to long format
dataLong <- melt(data, 
                 id.vars = c('stimulusitem3', 'wordclassification'),
                 measure.vars = c('z_noswitch_latency', 'z_switch_latency'),
                 variable.name = 'condition',
                 value.name = 'zscore')
# set the between factor to factor
dataLong$wordclassification <- as.factor(dataLong$wordclassification)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
Results <- aov(zscore ~ wordclassification*condition + Error(stimulusitem3/condition), data = dataLong)
summary(Results)

##@@ STATISTIC @@##
summary(Results)[["Error: stimulusitem3:condition"]][[1]][["F value"]][2]

##@@ P-VALUE @@##
summary(Results)[["Error: stimulusitem3:condition"]][[1]][["Pr(>F)"]][2]

##@@ SAMPLE SIZE @@##
nrow(data) # note that these are items, not participants!

##@@ EFFECT SIZE @@##
SumSq_effect <- summary(Results)[["Error: stimulusitem3:condition"]][[1]][["Sum Sq"]][2]
SumSq_error <- summary(Results)[["Error: stimulusitem3:condition"]][[1]][["Sum Sq"]][3]
SumSq_effect / (SumSq_effect + SumSq_error)

##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---