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
#@ Study Title: Accounting for Occurrences: A New View of the Use of Contingency Information in Causal Judgment
#@ Coder name: Simon Columbus
#@ Coder e-mail: simon@simoncolumbus.com
#@ Type of statistic: F-statistic
#@ Type of effect-size: partial eta squared
#@ OSF link project: https://osf.io/rhbqj/
#@ OSF link replication report: https://osf.io/x7c9i/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("xlsx")
library("reshape2")
library("ez")
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

# startRow = 3, such that only one line of headers is imported

File <- getOSFfile('https://osf.io/kmwit/')
Data <- read.xlsx(File, sheetIndex=1, startRow=3, header=TRUE)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Reshape into long format

Data <- melt(Data,
              id.vars = c("Subject.ID", "Total.Time"),
              measure.vars = c("KC", "TY", "OJ", "NO", "DD", "VY", "JC", "GP",
                               "EA", "VC", "WU", "HW"),
              variable_name = "Chemical")

# Add first and second row of headers from the Excel file

Data <- cbind(Data[,1:2], 
              "A"=rep(c("A2", "A6"), each=228),
              "B"=rep(c("B2", "B4", "B6"), each=76),
              "Cause"=rep(c("Cause A", "Cause B"), each=38), Data[,3:4])

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

Result <- ezANOVA(data=Data[Data$Cause=="Cause A",], dv=value, wid = Subject.ID, within=.(A, B))

##@@ STATISTIC @@##

Result$ANOVA[1,4]

##@@ P-VALUE @@##

Result$ANOVA[1,5]
  
##@@ SAMPLE SIZE @@##

length(unique(Data$Subject.ID))

##@@ EFFECT SIZE @@##

F <- Result$ANOVA[1,4]
df1 <- 1
df2 <- 37

eta <- F*df1 / (F*df1 + df2)
eta
  
##@@ AGREEMENT WITH AUTHORS @@##

TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---