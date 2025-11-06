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
#@ Study Title: The value heuristic in judgments of relative frequency
#@ Coder name: Stepan Bahnik
#@ Coder e-mail: bahniks@seznam.cz
#@ Type of statistic: chi squared
#@ Type of effect-size: Cohen's w
#@ OSF link project: https://osf.io/q7f6w/
#@ OSF link replication report: https://osf.io/js7gd/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("foreign")
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile("w9vin")
data <- read.spss(file, use.value.labels = F, to.data.frame = T)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# exclusion of participants failing a manipulation check
data <- data[data$MCheck1c == 1 & data$MCheck2c == 1 & data$MCheck3c == 1, ]
# exclusion of participants reporting same frequencies of birds and flowers
data <- data[data$FreqFlower != data$FreqBird, ]
# table of frequencies (endowed category X category estimated as less frequent)
frequencies <- table(data$CoinFlip, data$FreqFlower > data$FreqBird)
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
results <- chisq.test(frequencies, correct = F)
print(results)

##@@ STATISTIC @@##
results$statistic

##@@ P-VALUE @@##
results$p.value
  
##@@ SAMPLE SIZE @@##
nrow(data)

##@@ EFFECT SIZE @@##
sqrt(sum((1/4 - frequencies/51)**2 / (1/4)))
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---