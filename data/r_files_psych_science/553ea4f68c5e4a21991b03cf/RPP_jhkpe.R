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
#@ Coder name: Frits Traets
#@ Coder e-mail: frits.traets@student.kuleuven.be
#@ Type of statistic: F
#@ Type of effect-size: eta-squared
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

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<-getOSFfile("https://osf.io/4nd8g/")
data<-read.xlsx(file,1,header=TRUE)
data2<-read.xlsx(file,2,header=TRUE)


##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL difference between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.
  
  #Take necessary variables(5 data frames= 3 for data (participants), 2 for data2 (words):
  
  #STUDY WITH PARTICIPANTS (data)-------------------------------------------------------
  #dependent= remembered words (proportion correct)
  part.prop<-select(data, Subject,Mean_RememberedWords_Survival,Mean_RememberedWords_Vacation)
  
  #dependent= ratings 
  part.rate<-select(data, MeanRating_SurvivalWords, MeanRating_VacationWords)

  #dependent= reaction time
  part.time<-select(data, ResponseTime_Survival, ResponseTime_Vacation)

  #STUDY WITH WORDS (data2)-------------------------------------------------------------
  #dependent= ratings
  words.rate<-select(data2, MeanRating_Survival, MeanRating_Vacation)

  #dependent= proportion correct
  words.prop<-select(data2,Mean_Remembered_Survival, Mean_Remembered_Vacation)
  
  
  #Go to long format
  part_prop_long<-gather(part.prop, scenario, score, Mean_RememberedWords_Survival:Mean_RememberedWords_Vacation)
  part_rate_long<-gather(part.rate, scenario, score, MeanRating_SurvivalWords:MeanRating_VacationWords)
  part_time_long<-gather(part.time, scenario, score, ResponseTime_Survival:ResponseTime_Vacation)
  words_rate_long<-gather(words.rate, scenario, score, MeanRating_Survival:MeanRating_Vacation)  
  words_prop_long<-gather(words.prop, scenario, score, Mean_Remembered_Survival:Mean_Remembered_Vacation)

  #Make factors (create new ID label because Subject label was not unique for each pp in data with participants)
  part_prop_long$ID<-as.factor(c(1:38))   
  part_rate_long$ID<-as.factor(c(1:38))
  part_time_long$ID<-as.factor(c(1:38))
  words_rate_long$ID<-as.factor(c(1:32))
  words_prop_long$ID<-as.factor(c(1:32))


  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or sumarry call on the resulting object
  
  #DATA WITH PARTICIPANTS----------------------------------------------------
  #results for proportion correct
  result_prop<-ezANOVA(dv=score,within=scenario,wid=ID,data=part_prop_long,return_aov=T)
  print(result_prop)  
  #results for rating
  result_rate<-ezANOVA(dv=score,within=scenario,wid=ID,data=part_rate_long,return_aov=T)
  print(result_rate)  
  #results for response time
  result_time<-ezANOVA(dv=score,within=scenario,wid=ID,data=part_time_long,return_aov=T)
  print(result_time)  
  
  #DATA WITH WORDS----------------------------------------------------------
  #results for rating
  result_rate.W<-ezANOVA(dv=score,within=scenario,wid=ID,data=words_rate_long,return_aov=T)
  print(result_rate.W)
  #results for prop correct
  result_prop.W<-ezANOVA(dv=score,within=scenario,wid=ID,data=words_prop_long,return_aov=T)
  print(result_prop.W)


  ##@@ STATISTIC @@##--------------------
  #PARTICIPANTS
  result_prop$ANOVA$F       #dependent= proportion correct
  result_rate$ANOVA$F       #dependent= rating
  result_time$ANOVA$F       #dependent= response time
  #WORDS
  result_rate.W$ANOVA$F     #dependent= rating
  result_prop.W$ANOVA$F     #dependent= proportion correct


  ##@@ P-VALUE @@##----------------------
  #PARTICIPANTS
  result_prop$ANOVA$p
  result_rate$ANOVA$p
  result_time$ANOVA$p

  #WORDS
  result_rate.W$ANOVA$p
  result_prop.W$ANOVA$p

  ##@@ SAMPLE SIZE @@##------------------
  #PARTICIPANTS
  nrow(data)   #same for all three
  #WORDS
  nrow(data2) #same for both
  
  ##@@ EFFECT SIZE @@## ----------------
  
  #(eta squared= SS effect / SS total)
  
  #PARTICIPANTS
  #proportion correct
   eta_sq_prop= 0.1670/(0.1670+0.7646)
   eta_sq_prop
  #rating 
   eta_sq_rate=3.746916/(3.746914+3.296053)
   eta_sq_rate
  #response time
   eta_sq_time=16387.58/(16387+243482.42)
   eta_sq_time

  #WORDS
  #rating
  eta_sq_rate.W= 0.1756553/(0.1756553+0.4489243)
  eta_sq_rate.W
  #proportion correct
  eta_sq_prop.W=1.853002/(1.853002+20.235648)
  eta_sq_prop.W


##@@ AGREEMENT WITH AUTHORS @@##
  # NO
  
  #@ Reason disagreement:
  #The effect of scenario in the dataset with participants, with rating as dependent measure, resulted in 
  #F(1, 37) = 42.06119 in stead of the reported F(1, 37) = 44.34. 


