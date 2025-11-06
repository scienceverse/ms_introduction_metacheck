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
#@ Study Title: Opening the mind to close it: Considering a message in light of important values increases message processing and later resistance to change.
#@ Coder name: Matthias Lippold
#@ Coder e-mail: matthias.lippold1@stud.uni-goettingen.de
#@ Type of statistic:F
#@ Type of effect-size:n^2
#@ OSF link project: https://osf.io/2gx4k/
#@ OSF link replication report: https://osf.io/v3e2z/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("foreign")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<- getOSFfile("https://osf.io/qifnv/")
data<-read.spss(file,to.data.frame =TRUE)

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
# data did not need to be changed

  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or sumarry call on the resulting object

Results<-aov(data = data,Attitude~arg_qual*value_imp)
summary(Results)
  ##@@ STATISTIC @@##
summary(Results)[[1]]$F[3]
  ##@@ P-VALUE @@##
summary(Results)[[1]]$P[3]
  ##@@ SAMPLE SIZE @@##
nrow(data)  
  ##@@ EFFECT SIZE @@##
Sumsq_effect<-summary(Results)[[1]]$Sum[3]
Sumsq_res<-summary(Results)[[1]]$Sum[4]
Sumsq_effect/(Sumsq_effect+Sumsq_res)
  ##@@ AGREEMENT WITH AUTHORS @@##
TRUE  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---