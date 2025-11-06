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
#@ Study Title: Replication of Richeson & Trawalter (2008, PS, Study 1)
#@ Coder name: Daan van Renswoude
#@ Coder e-mail: d.r.vanrenswoude@uva.nl
#@ Type of statistic: F
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/c5fza/
#@ OSF link replication report: https://osf.io/phwi4/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library('reshape2')

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile('https://osf.io/97nuv/')
data <- read.table(file, header = T, sep = ',')
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# select subjects, motivation and 4 measures of the dependent variable
data <- data[c(1, 64, 67:70)]
# Transform to long format
DFlong <- melt(data,
               id.vars = names(data)[1:2],
               measure.vars = names(data)[3:6],
               variable.name = 'condition')
# add the two factors to the data frame
expression <- as.factor(rep(1:2, each = nrow(data) * 2))
duration <- as.factor(rep(rep(1:2, each = nrow(data)), 2))
DFlong <- data.frame(DFlong, expression, duration)

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
Results <- aov(value ~ ems_d*expression*duration + Error(as.factor(Subj)/(expression * duration)), data = DFlong)
summary(Results)

##@@ STATISTIC @@##
summary(Results)[["Error: as.factor(Subj):expression:duration"]][[1]][["F value"]][2]

##@@ P-VALUE @@##
summary(Results)[["Error: as.factor(Subj):expression:duration"]][[1]][["Pr(>F)"]][2]
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
SumSq_effect <- summary(Results)[["Error: as.factor(Subj):expression:duration"]][[1]][["Sum Sq"]][2]
SumSq_error <- summary(Results)[["Error: as.factor(Subj):expression:duration"]][[1]][["Sum Sq"]][3]
SumSq_effect / (SumSq_effect + SumSq_error)

##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---