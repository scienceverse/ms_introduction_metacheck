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
#@ Study Title: Replication of Makovski, Sussman, & Jiang (2008, JEP:LMC)
#@ Coder name: Daan van Renswoude
#@ Coder e-mail: d.r.vanrenswoude@uva.nl
#@ Type of statistic: F
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/0pxro/
#@ OSF link replication report: Not available
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("foreign")
library("reshape2")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile('https://osf.io/b5sp4/')
data <- read.spss(file, to.data.frame = T)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# select relevant variables
data <- data[,c(1:3, 8:9, 14:15, 20:21)]
# transform to long format
DFlong <- melt(data, id.vars = c('subject'))

# add the indepent variables as factors
task <- as.factor(rep(1:2, each = nrow(DFlong) / 2))
cue <- as.factor(rep(rep(1:2, each = nrow(DFlong) / 4), 2))
size <- as.factor(rep(rep(1:2, each = nrow(data)), 4))
DF <- data.frame(DFlong, task, cue, size)

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
Results <- aov(value ~ task*cue*size + Error(as.factor(subject)/(task*cue*size)), data = DF)
summary(Results)

##@@ STATISTIC @@##
summary(Results)[["Error: as.factor(subject):cue:size"]][[1]][["F value"]][1]

##@@ P-VALUE @@##
summary(Results)[["Error: as.factor(subject):cue:size"]][[1]][["Pr(>F)"]][1]

##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
SumSq_effect <- summary(Results)[["Error: as.factor(subject):cue:size"]][[1]][["Sum Sq"]][1]
SumSq_error <- summary(Results)[["Error: as.factor(subject):cue:size"]][[1]][["Sum Sq"]][2]
SumSq_effect / (SumSq_effect + SumSq_error)

##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---