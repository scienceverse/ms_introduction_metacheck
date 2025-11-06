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
#@ Study Title: ---Independent Direct Replication #1 of Correll (2008, JPSP, Study 2)---
#@ Coder name: ---Sarah Thomas---
#@ Coder e-mail: ---slt4bg@virginia.edu---
#@ Type of statistic: ---F from ANOVA---
#@ Type of effect-size: ---Cohen's d---
#@ OSF link project: ---https://osf.io/fejxb---
#@ OSF link replication report: ---https://osf.io/hzka3/---

##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library(compute.es)
library(pwr)
library(car)
library(foreign)
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
Rawdat<-getOSFfile("https://osf.io/c5wjs/")
dat <- read.spss(Rawdat,use.value.labels = FALSE, to.data.frame=TRUE)

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
  
#Replicate condition recoding
dat$Cond_WIT<-recode(dat$Cond, "1=1; 2=1;3=2;4=2;5=3;6=3", as.factor.result=FALSE)
#Select only data with errorRate less than .80 (I think this is all the data)
newdat<-subset(dat, dat$errorRate<.80)
#Create  contrast codes for ANOVA -1, .5, and .5
newdat$Ccode<-recode(newdat$Cond_WIT, "1=-1;2=.5;3=.5", as.factor.result=TRUE)
str(newdat)
# The difference between this data and the replication data is that they excluded 
# non-compliant participant 213 (not sure why they did this), but I did not because
# there is no participant ID 213 in the data.

##@@ DATA ANALYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
# Conduct an ANOVA on PSD slopes according to contrast codes -1, .5, .5
aov(data=newdat,PSDslope~Ccode)
summary(aov(data=newdat,PSDslope~Ccode))
  
##@@ STATISTIC @@##
#F value
summary(aov(data=newdat,PSDslope~Ccode))[[1]]$F[1]

##@@ P-VALUE @@##
summary(aov(data=newdat,PSDslope~Ccode))[[1]]$Pr[1]

##@@ SAMPLE SIZE @@##
nrow(newdat)

##@@ EFFECT SIZE @@##
# Get control and treatment sample sizes
length(which(newdat$Ccode==-1))
length(which(newdat$Ccode==.5))  
#Compute effect size d
effectsize<-fes(.794, 48, 100)
effectsize$d

##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: The dfs for my final result is (1, 146) rather than the (1,145)
# reported in the replication report. This is because they excluded non-compliant participant 
# 213 (not sure why they did this), but I did not because there was no participant ID 213 in 
# the data.
# Also, my effect size d is .16 rather than .15. I think this is because they used a different 
# F (.891 to my .794). In their script, they obtained .16, but they report .15 in the 
# replication report. Here is their code: 
# replication #1  tes(.891,100,48) #d=.16 +/-.34 ---