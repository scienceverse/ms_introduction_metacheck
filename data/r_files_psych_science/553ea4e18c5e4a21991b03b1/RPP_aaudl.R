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
#@ Study Title: Anchor precision influences the amount of adjustment. Psychological Science
#@ Coder name: Frits Traets
#@ Coder e-mail: frits.traets@student.kuleuven.be
#@ Type of statistic: F
#@ Type of effect-size: eta squared
#@ OSF link project: https://osf.io/aaudl/                  
#@ OSF link replication report: https://osf.io/ehjdm/
#
  ##@@ REQUIRED PACKAGES @@##
  library("httr")     #for reading in data
  library("RCurl")    #for reading in data
  library("dplyr")    #To manipulate data
  library("ez")       #To analyse data
  library("lsr")      #To calculate effect size (eta squared)
  source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

  ##@@ DATA LOADING @@##
  #@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
  file<-getOSFfile("https://osf.io/9zer7/")
  data<-read.csv(file)
  
  ##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL difference between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.
  
  
  #6 participants were not selected for analysis (indicated by variable "DROP"), exclude those:
  data.selected<-subset(data,DROP=="0")
  
  #Take necessary variables (colums) and place them into new dataframe (d).
  #ID= participant, between variables (motivation= Anchortype, precision= magnitude)
  #dependent= mean2
  d<-dplyr::select(data.selected, Participant, Anchortype, magnitude,mean2)
  
  #Make factors of the factors
  d$Participant<-as.factor(d$Participant)
  d$Anchortype<-as.factor(d$Anchortype)
  d$magnitude<-as.factor(d$magnitude)

  
  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or sumarry call on the resulting object
  result<-ezANOVA(dv= mean2, wid=Participant, between=.(Anchortype, magnitude),data=d, return_aov=T)
  print(result)

  ##@@ STATISTIC @@##
  result$ANOVA$F

  ##@@ P-VALUE @@##
  result$ANOVA$p
  
  ##@@ SAMPLE SIZE @@##
  nrow(d)
  
  ##@@ EFFECT SIZE @@##
  etaSquared( result$aov )    
  
  ##@@ AGREEMENT WITH AUTHORS @@##
  TRUE
  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---