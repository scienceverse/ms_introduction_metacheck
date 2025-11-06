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
#@ Study Title: Semantic Interference in a Delayed Naming Task: Evidence for the Response Exclusion Hypothesis
#@ Coder name: Johnny van Doorn
#@ Coder e-mail: Johnnydoorn@gmail.com  
#@ Type of statistic: F
#@ Type of effect-size: Partial Eta Squared
#@ OSF link project: https://osf.io/uhpyr/
#@ OSF link replication report: https://osf.io/ka5vp/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("XLConnect")
library("car")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
dir <- getOSFfile('https://osf.io/fp9aw/')


# Read files:
wb = loadWorkbook(dir)
df = readWorksheet(wb, sheet = "F1", header = TRUE)


##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# The following makes a matrix of the conditions
idata <- matrix(ncol=2,nrow=4)
colnames(idata) <- c("Freq","Rela")
idata[,1] <- c("HF","HF","LF","LF")
idata[,2] <- c("Related","Unrelated","Related","Unrelated")
idata <- as.data.frame(idata)

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
# interestBind contains the reaction times per condition
interestBind <- cbind(df$HF...Related, 
                      df$HF...Unrelated, 
                      df$LF...Related, 
                      df$LF...Unrelated)
interestModel <- lm(interestBind ~ 1)
analysis <- Anova(mod=interestModel, idata=idata, idesign= ~Freq * Rela)
smmry <- summary(analysis)
print(smmry)
##@@ STATISTIC @@##
# smmry$univariate.tests[2,5] # Main effect of Frequency
smmry$univariate.tests[3,5] # Main effect of Relatedness
# smmry$univariate.tests[4,5] # Interaction effect of Relatedness & Frequency

##@@ P-VALUE @@##
# smmry$univariate.tests[2,6] # P-val of Main effect of Frequency
smmry$univariate.tests[3,6] # P-val of Main effect of Relatedness
# smmry$univariate.tests[4,6] # P-val of Interaction effect of Relatedness & Frequency

##@@ SAMPLE SIZE @@##
nrow(df)
##@@ EFFECT SIZE @@##
# SumSq_effect <-smmry$univariate.tests[2,1]
# SumSq_error <- smmry$univariate.tests[2,3]
# SumSq_effect / (SumSq_effect + SumSq_error) # Partial Eta Squared for Frequency

SumSq_effect <-smmry$univariate.tests[3,1]
SumSq_error <- smmry$univariate.tests[3,3]
SumSq_effect / (SumSq_effect + SumSq_error) # Partial Eta Squared for Relatedness


##@@ AGREEMENT WITH AUTHORS @@##
TRUE  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---