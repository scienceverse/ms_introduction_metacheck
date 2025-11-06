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
#@ Study Title: More than meets the eye: The role of language in binding visual properties
#@ Coder name: Marie Deserno
#@ Coder e-mail: marie.deserno@gmail.com
#@ Type of statistic: t statistic
#@ Type of effect-size: cohen's d
#@ OSF link project: https://osf.io/iajp5/
#@ OSF link replication report: https://osf.io/83n4z/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("xlsx")
library("dplyr")
library("lsr")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
OSFfile<-getOSFfile('https://osf.io/8249a/')
mydata<-read.xlsx(OSFfile,2) 

  ##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL difference between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.
mydata<-mydata[!mydata$Exclude..1...yes==1,]
mydata<-mydata[mydata$Trial>8,]

dat <- mydata %>% 
  group_by(SubjNum, Condition) %>%
  dplyr::summarize(select = mean(Accuracy.SelectedTarget.), n = n())

  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or sumarry call on the resulting object
ttest<-t.test(dat$select[dat$Condition == "dir"], dat$select[dat$Condition == "rel"], var.equal = TRUE)


  ##@@ STATISTIC @@##
ttest$statistic
  
  ##@@ P-VALUE @@##
ttest$p.value  

  ##@@ SAMPLE SIZE @@##
nrow(dat)
  
  ##@@ EFFECT SIZE @@##
cohensD(dat$select[dat$Condition == "dir"], dat$select[dat$Condition == "rel"])
  
  ##@@ AGREEMENT WITH AUTHORS @@##
 TRUE
  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---