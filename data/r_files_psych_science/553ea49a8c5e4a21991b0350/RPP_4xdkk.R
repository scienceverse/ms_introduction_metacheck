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
#@ Study Title: The intermixed-blocked effect in human perceptual learning is not the consequence of trial spacing
#@ Coder name: Frits Traets
#@ Coder e-mail: frits.traets@student.kuleuven.be
#@ Type of statistic: F
#@ Type of effect-size: eta-squared
#@ OSF link project: https://osf.io/4xdkk/
#@ OSF link replication report: https://osf.io/beckg/


##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("foreign")
library("tidyr")
library("dplyr")
library("ez")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<-getOSFfile("https://osf.io/x46jy/")
data<-read.spss(file,to.data.frame=T)  
  

##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL differences between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.
  
  #take necessary variables
  d<-dplyr::select(data,PPNR,correct_intermixedAXBX_dif,correct_intermixedCXDY_dif,
            correct_intermixedAXBX_same,correct_intermixedCXDY_same)  


  #Go to long data format
  d.long<-gather(d, condition, score,correct_intermixedAXBX_dif:correct_intermixedCXDY_same )

  #create factors 
  d.long$AX.BX_CX.DY<-as.factor(rep(c("axbx","cxdy"),each=48))
  d.long$cond<-as.factor(rep(c("dif","same"),each=96))
  d.long$PPNR<-as.factor(d.long$PPNR)


  ##@@ DATA ANLAYSIS @@##
  #@ NOTE: Include a print or summary call on the resulting object
  result<-ezANOVA(dv=score, within=.(cond, AX.BX_CX.DY),wid=PPNR,data=d.long,return_aov=T )
  print(result)
  
  ##@@ STATISTIC @@##
  result$ANOVA$F[2]
  
  ##@@ P-VALUE @@##
  result$ANOVA$p[2]
  
  ##@@ SAMPLE SIZE @@##
  nrow(data)

  ##@@ EFFECT SIZE @@##
  #( eta squared= SS within / SS total)
   eta.axbx_cxdy <- 0.078206 / (0.078206 + 4.214762)
   eta.axbx_cxdy

#    eta.cond<- 15.257894/ (15.257894+3.941325)
#    eta.cond
# 
#    eta.interaction<-0.1797689/ (0.1797689+2.8006999)
#    eta.interaction


  ##@@ AGREEMENT WITH AUTHORS @@##
  TRUE
  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---