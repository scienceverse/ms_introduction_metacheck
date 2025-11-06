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
#@ Study Title: Effects of fluency on psychological distance and mental construal (or why New York is a large city, but New York is a civilized jungle)
#@ Coder name: Stepan Bahnik
#@ Coder e-mail: bahniks@seznam.cz
#@ Type of statistic: chi squared
#@ Type of effect-size: phi
#@ OSF link project: https://osf.io/kegmc/
#@ OSF link replication report: https://osf.io/jym7h/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("memisc")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile("zpiyg")
data <- data.frame(as.data.set(spss.system.file(file)))
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# exclusion of participants who did not complete the entire study (q0010 is the last question)
data <- data[!is.na(data$q0010), ]
frequencies <- table(data$q0001, data$concrete_prefer)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
results <- chisq.test(frequencies, correct = T)
print(results)

##@@ STATISTIC @@##
results$statistic

##@@ P-VALUE @@##
results$p.value
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
sqrt(results$statistic / nrow(data))
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---