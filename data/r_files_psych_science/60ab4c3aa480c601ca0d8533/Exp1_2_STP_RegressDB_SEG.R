# Exp1_2_STP_RegressDB_SEG
# Segmentation allows to include "more than 1 time segment"
# in regression (for mediation like analysis)
# =============================================================
#Removing evrything from workspace
graphics.off()
rm(list = ls(all = TRUE))

#Setting up directory
fsep<-.Platform$file.sep;
Dir_R<-path.expand("~/w_Data_Code/General")
Dir_EDATA<-path.expand("~/w_Data_Code/Exp2/EEG") 
Dir_BDATA<-path.expand("~/w_Data_Code/Exp2/EEG") 

# Load libraries
library(data.table)
library(tidyverse)
library(broom)
library(rhdf5)
library(caret)
library(foreach)
library(doMC)
library(tidyr)
library(binhf)
library(RSQLite)
library(lazyeval)
library(stringr)
library(psych)
library(lme4)
library(effects)
library(rstanarm)
library(formattable)
library(sjstats)
library(optimx)
library(zoo)
# library(brms)

# Source Files
setwd(Dir_R)
source('basic_libS.R')

# Load database 
setwd(Dir_GRAND);
fileN<-"Exp1";
sessN<-"test";
timeL<-seq(-600,800,4)
timeS<-list(c(0,300)) # each element corresponds "_S#" segments
dbFiles<-c("HAL2017_STP_FIT_RSA_TSCONJ_CTRR") # hardcode (left most one has priority)
saveOn=TRUE;# put name for saving results
idxF <- function(i) which.min(abs(timeL-i))

# MODEL ===========================================================
# # Regression
depV<-"RT"
formula_pw<-paste0(depV,'~1+BLOCK+TRIAL+(1|SUBID)')
formula<-paste0(depV,'~1+RULE_RSA_S1+STIM_RSA_S1+RESP_RSA_S1+CONJ_RSA_S1+(1+RULE_RSA_S1+STIM_RSA_S1+RESP_RSA_S1+CONJ_RSA_S1|SUBID)');#post-stim

# Other variables & setting
mergeV<-c("SUBID","BLOCK","TRIAL")
timeV<-c("time")# this variable needs to be consistant across DBs
supV<-c("U1OKAY","L1OKAY","LACC","U1ACC","U1RT","RT","ACC","TASK","RESP","SWITCH","RESPREP","STOP","U1STOP")
facV<-cols<-c("TASK","RESP","SWITCH","RESPREP")

# LOAD & MERGE =====================================================
# Get merged dataset using "mergeDB_decode": check list of DB! 
mergeSet<-list(db=as.list(dbFiles),fmlL=list(formula_pw,formula),exvL=list(mergeV,timeV,supV,facV),timeS=timeS)
dsM <-mergeDB_decode(mergeSet);dsAGG <- dsM$dsM

# FITTING  ======================================================
# Filtering 
dsAGG[,POKAY := ifelse(STOP == 0 & ACC == 0, 0, 1)]
dsAGG<-dsAGG[STOP==0 & ACC==1,];#go-only

# # # # # # RT cutoff
# cutRT<-quantile(dsAGG$RT,0.999,na.rm=T);
# print(paste0("0.5% Response cutoff: ", round(cutRT,2)));
# dsAGG<-dsAGG[dsAGG$RT<cutRT,];
# dsAGG<-dsAGG[dsAGG$RT>10 & dsAGG$U1RT>10,];

#Adjust values & Factorize
dsAGG$RT<-log(dsAGG$RT)
dsAGG$U1RT<-log(dsAGG$U1RT)
dsAGG[,ODDEVEN := (dsAGG$TRIAL %% 2)]
dsAGG$BLOCK<-as.numeric(scale(dsAGG$BLOCK,center=T,scale=F));
dsAGG$TRIAL<-as.numeric(scale(dsAGG$TRIAL,center=T,scale=F));
dsAGG[,(facV):=lapply(.SD,factor),.SDcols=facV];

# Run regression ===========================================================
#Prewitten DV!
pw<-lmer(formula_pw,data=dsAGG,control=lmerControl(calc.derivs=FALSE));
dsAGG[[depV]]<-residuals(pw);

#Fit regression!!===========================================================
controlSL<-lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6))#bobyqa,Nelder_Mead,L-BFGS-B
m<-lme4::lmer(formula,data=dsAGG,control=controlSL);#Fitting
mr<-tidy(m,effects=c("fixed"),conf.int=T,conf.method="Wald");

# Summarize results
print(as.character(lapply(timeS,function(x) {paste(x,collapse = "_")})))
print(paste0("size of datasets: ",paste0(dsM$size,collapse=",")))
print(mr)
summary(m)
std_beta_MLM(m)@fixef
