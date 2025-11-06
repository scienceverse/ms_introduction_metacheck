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
#@ Study Title: Adaptive memory: The comparative value of survival processing.
#@ Coder name: Andreas Cordes (second pass audit); Frits Traets (original audit);
#@ Coder e-mail: acordes@uni-goettingen.de ; frits.traets@student.kuleuven.be
#@ Type of statistic: F
#@ Type of effect-size: partial eta-squared
#@ OSF link project: https://osf.io/jhkpe/
#@ OSF link replication report: https://osf.io/v4d2b/
#
##@@ REQUIRED PACKAGES @@##
library("httr")     #To read in data
library("RCurl")    #To read in data
library("xlsx")     #To read in data
library("dplyr")    #To organize data
library("ez")       #To analyse
library("tidyr")    #To organize data
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
require(schoRsch)
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<-getOSFfile("https://osf.io/4nd8g/")
data<-read.xlsx(file,1,header=TRUE)


##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

#Take necessary variables(5 data frames= 3 for data (participants), 2 for data2 (words):

#STUDY WITH PARTICIPANTS (data)-------------------------------------------------------
#dependent= remembered words (proportion correct)
part.prop<-dplyr::select(data, Subject,Mean_RememberedWords_Survival,Mean_RememberedWords_Vacation)

part.rating<-dplyr::select(data, Subject,MeanRating_SurvivalWords,MeanRating_VacationWords)


#Go to long format

names<-colnames(part.prop)
part.prop[,1]<-1:38
part_prop_long<-reshape(part.prop,varying=list(names[2:3]),idvar=names[1]
                        ,timevar="condition",v.names="score"
                        ,direction="long",times=names[2:3])

part_prop_long$condition<-as.factor(part_prop_long$condition)



names<-colnames(part.rating)
part.rating[,1]<-1:38
part_rating_long<-reshape(part.rating,varying=list(names[2:3]),idvar=names[1]
                          ,timevar="condition",v.names="score"
                          ,direction="long",times=names[2:3])

part_rating_long$condition<-as.factor(part_rating_long$condition)


##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

#DATA WITH PARTICIPANTS----------------------------------------------------
#results for proportion correct
result_prop<-ezANOVA(dv=score,within=condition,wid=Subject,data=part_prop_long,detailed=T)
mod<-anova_out(result_prop)  

result_rating<-ezANOVA(dv=score,within=condition,wid=Subject,data=part_rating_long,detailed=T)
mod2<-anova_out(result_rating)  


##@@ STATISTIC @@##

result_prop$ANOVA$F       #dependent= proportion correct


##@@ P-VALUE @@##

result_prop$ANOVA$p[2]


##@@ SAMPLE SIZE @@##

nrow(data)   #same for all three

##@@ EFFECT SIZE @@##

mod[[1]]$petasq[2]

##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement:
# Frits original comment 
#The effect of scenario in the dataset with participants, with rating as dependent measure, resulted in 
#F(1, 37) = 42.06119 in stead of the reported F(1, 37) = 44.34. 

# Andreas: The effect on which disagreement was found was irrelevent to the replication task.

