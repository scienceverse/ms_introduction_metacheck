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
#@ Study Title: Is it Right to Be Wrong About the Future?
#@ Coder name: Onur Sahin
#@ Coder e-mail: 92onursahin@gmail.com
#@ Type of statistic: t-test
#@ Type of effect-size: d
#@ OSF link project: https://osf.io/qlzap/
#@ OSF link replication report: https://osf.io/8u5v2/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R")
library("compute.es")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
File <- getOSFfile('https://osf.io/4f9g8/') # Store file in File
Data <- read.csv(File) # Read file into R
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# I only use the optimism responses, so I subset those 8 columns:
Optimism <- cbind(Data$s1q1_Best, Data$s2q1_Best, Data$s3q1_Best, 
                  Data$s4q1_Best, Data$s5q1_Best, Data$s6q1_Best, 
                  Data$s7q1_Best, Data$s8q1_Best)
## I calculate the mean optimism score of each participant:
meanOptimism <- cbind(rowMeans(Optimism, na.rm = T))  
meanOptimism <- meanOptimism[complete.cases(meanOptimism), ] #Clean out the NaN
meanOptimism <- cbind(meanOptimism) #Transform it into column

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
Result <- t.test(meanOptimism)
print(Result)
##@@ STATISTIC @@##
Result$statistic

##@@ P-VALUE @@##
Result$p.value
  
##@@ SAMPLE SIZE @@##
nrow(meanOptimism)   #I use the number of rows to indicate sample size

##@@ EFFECT SIZE @@##
t <- Result$statistic     #To calculate effect size I use t statistic
n <-  nrow(meanOptimism)   # And n
efd <- t/sqrt(n)          
eS <- des(efd, n.1 = n, n.2 = n, level = 95, dig = 2, id = NULL, data = NULL)
eS$d      #Using the package compute.es I calculate and substract d
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---
