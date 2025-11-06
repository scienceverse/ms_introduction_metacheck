# Exp1_L1Model_RSA
# load pred file (contains single-trial accuracy & confidence)
# fit RSA models to decoding resulf of the conjunction variable
# _pred could be from: CROSS,JMB,SEG, and DYN!
# =============================================================
#Removing evrything from workspace
graphics.off()
rm(list = ls(all = TRUE))

#Setting up directory
fsep<-.Platform$file.sep;
Dir_R<-path.expand("~/w_Data_Code/General")
Dir_EDATA<-path.expand("~/w_Data_Code/Exp1/EEG") # to load f5 file (STIMLOCK.h5,RESPLOCK.h5)
Dir_BDATA<-path.expand("~/w_Data_Code/Exp1/BEH") # to load Exp1_BehP.txt

# Load libraries
library(data.table)
library(tidyverse)
library(broom)
library(rhdf5)
library(caret)
library(foreach)
library(doMC)
library(binhf)
library(pROC)
library(RColorBrewer)
library(scales)
library(forcats)
library(stringr)
library(beepr)

# Source Files
setwd(Dir_R)
source('basic_lib.R')

# Setting for Database
setwd(Dir_EDATA);
subs<-list.files( pattern="^A[[:digit:]]{3}");
fileN<-"Exp1_STP"
modelV<-c("TSCONJ","")
f2Load<-paste0(modelV[1],'_nb_pred.rds');#[modelV]_nb_pred.rds/_nbRL_pred.rds
dbName<-paste0("FIT_RSA_",modelV[1],"_CTRR");#paste0("FIT_RSA_",modelV,"_CTR")/paste0("FIT_RSA_BASICVARS_CTR")
saveOn<-T;
dataL<-list();

# # Load beh data
setwd(Dir_BDATA)
ds_beh<-fread("Exp1_STP_BehP.txt");
ds_beh[is.nanM(ds_beh)]<-NA
ds_beh<-ds_beh[,!grepl("EV_|TASKTS|RSIJIT",names(ds_beh)),with=F]

# # Load Individuals' Conjunction RT
ds_cj<-fread("Exp1_STP_TSCONJ_CONTROL.txt")

#======================================================================================================
#Load RSA Models (adjusted for different decoding variables)
#======================================================================================================
# # Adjust RSA model sources!
setwd(paste0(Dir_RSAMODEL,"/",modelV[1]))
mlist <- list.files(pattern = ".txt")
models<-lapply(as.list(mlist),function(f){read.table(f,header = F)})
for (m in 1:length(mlist)){assign(gsub(".txt","",mlist)[m],models[[m]])}

#======================================================================================================
#Process3- Database
#======================================================================================================
# Open Database  (erase old database of the same datatype, if it exists!!)
setwd(Dir_GRAND);
registerDoMC(detectCores())
resultsA<-list();
if (file.exists(paste0(fileN,'_',dbName))){file.remove(list.files(pattern=dbName))}
dbCon=dbConnect(dbDriver("SQLite"),paste0(fileN,'_',dbName))# Open database connection

for (s in c(1:length(subs))){#
  # Load data & set properties
  Dir_Data_i<-paste0(Dir_EDATA,fsep,subs[s],fsep,"DATASETS");
  setwd(Dir_Data_i);f2LoadL<-paste0(subs[s],"_",f2Load);
  pred<-data.table(readRDS(f2LoadL));#pred<-rio::import(f2LoadL)
  subN <- gsub('[[:alpha:]]+', '', subs[s])
  mesVs<-colnames(pred)[colnames(pred) %in% LETTERS_ex(50)]
  idVs<-colnames(pred)[!colnames(pred) %in% c(mesVs,"acc")]
  nmbVs<-idVs[!idVs %in% c("FBAND","ELEC","CV")]# avoid character columns in idVs
  timeVs<-nmbVs[grepl('time',nmbVs)]# time coding columns
  castF1<-paste0("CLASS~",paste0(idVs[!idVs %in% c("SUBID")],collapse="+"))
  castF2<-paste0(paste0(idVs,collapse="+"),"~vars")
  
  # Step1: Melt data from wide (A:L) to long format (BLOCK,TRIAL,time)
  pred<-data.table::melt(pred,#this is way faster than gather/spread
                         id.vars = idVs,
                         measure.vars=mesVs,
                         variable.name="CLASS",value.name="PP")
  
  # Step1':Logit transform
  pred[PP==0,PP:=1e-10];pred[PP==1,PP:=1-1e-10]
  pred$PP<-psych::logit(pred$PP);
  
  # Check if all classes have complete cases 
  if (any(c(is.infinite(pred$PP) | is.nan(pred$PP)))){
    pred<-pred[!c(is.infinite(pred$PP) | is.nan(pred$PP)),]
    pred[,cmp:=uniqueN(CLASS),by=c("BLOCK","TRIAL",timeVs)]
    pred<-pred[cmp==length(mesVs),];pred[,cmp:=NULL]
  }
  
  # Step2: Add condition of Modeled variable from behavior (if necessary!)
  if (!any(grepl("obs",names(pred)))){
    ds_behi<-ds_beh[SUBID==subN,c(c("SUBID","BLOCK","TRIAL"),modelV[1]),with=F]
    pred<-merge(pred,ds_behi,by=c("SUBID","BLOCK","TRIAL"));setnames(pred,modelV[1],"obs")
    }
  
  # Step3-1:Long format (BLOCK,TRIAL,time) to another wide format (A:L to all combination of BLOCK,TRIAL,time)
  # Then, add model terms (e.g.,TASK,STIM,RESP,CONJ)
  if(is.factor(pred$obs)){pred[,obs:=as.numeric(obs)]}
  clist<-which(mesVs %in% LETTERS_ex(50))
  
  # Step (4): Prepare control vector of RT/ACC
  CJCTR<-scale(ds_cj[ds_cj$SUBID==subN,]$RT)[,1];
  CJCACC<-scale(ds_cj[ds_cj$SUBID==subN,]$ACC)[,1];
  
  results<-
    foreach(cc=clist) %dopar% {
      d<-pred[obs==cc,];
      predF<-data.table::dcast(d,castF1, value.var="PP")
      
      # This part could be more abstract with "eval" or list of models
      # TSCONJ (Rule x Resp/Stim) with CJ RT/ACC control
      predF$RULE_RSA<-TASK_M[,cc];#RULE MODEL
      predF$STIM_RSA<-STIMPOS_M[,cc];#STIM MODEL
      predF$RESP_RSA<-RESP_M[,cc];#RESP MODEL
      predF$CONJ_RSA<-CONJ_M[,cc];#CONJ MODEL
      predF$CTRRT_RSA<-CJCTR;#CONTROL MODEL(RT)
      predF$CTRACC_RSA<-CJCACC;#CONTROL MODEL(ACC)

      # TSCONJ_C1/C2 (Rule x Resp/Stim X Random/EvenOddB) with CJ RT/ACC control
      #predF$RULE_RSA<-TASK_M[,cc];#RULE MODEL
      #predF$STIM_RSA<-STIMPOS_M[,cc];#STIM MODEL
      #predF$RESP_RSA<-RESP_M[,cc];#RESP MODEL
      #predF$CONJ_RSA<-CONJ_M[,cc];#CONJ MODEL
      #predF$DIAG_RSA<-DIAG_M[,cc]#DIAGONAL (highest combination)
      #predF$RAND_RSA<-RANDOM_M[,cc]#RANDOM (random feature to expand the space)
      #predF$CTRRT_RSA<-CJCTR;#CONTROL MODEL(RT)
      #predF$CTRACC_RSA<-CJCACC;#CONTROL MODEL(ACC)

      # Step3-2:Regression all at once!
      dvIdx<-grepl("CLASS|*._RSA$",names(predF))
      Y<-as.matrix(predF[,which(!dvIdx),with=FALSE]);#DVs(this indexing takes time...)
      X<-as.matrix(predF[,which(dvIdx)[-1],with=FALSE]);#IVs(first var is CLASS!)
      # r<-coeff(.lm.fit(cbind(1,X),Y));#fastest, but only gives unstandardized coefficients
      r<-ls.print(lsfit(X,Y),print.it=F);#includes intercept!

      # Step3-3:Summarize results!
      names(r$coef.table)<-colnames(predF)[!dvIdx]
      estList<-rownames(r$coef.table[[1]]);#list of vars for estimates
      r<-rbindlist(lapply(r$coef.table,as.data.frame),idcol=TRUE)
      setnames(r,old=c("t-value","Pr(>|t|)","Std.Err"),new=c("tvalue","pvalue","SE"))
      
      #Rename variables and add basic identifier variables
      r[,c("SUBID",modelV[1]):=list(pred$SUBID[1],cc)]
      r[,vars:=rep(estList,dim(r)[1]/length(estList))]
      
      #Restore labels from list names, then convert to wide format
      bltrtime<-str_split_fixed(r$.id,"_",n=Inf);#no assumption for .id tokens
      r[,c(idVs[idVs!="SUBID"]):=narray::split(bltrtime,along=2)]
      r[,(nmbVs):=lapply(.SD,as.numeric),.SDcols=nmbVs]
      r<-r[vars!="Intercept",c(idVs,"vars","tvalue"),with=F]
      
      # Need wide format(dcast) // long-wide(dcast) & wide-long(melt)
      r_wide<-dcast(r,castF2,value.var="tvalue")
      return(r_wide)
    }
  
  # Summarize all results!!!!
  results <- rbindlist(results)
  setorderv(results,nmbVs)
  depV<-colnames(results)[!colnames(results) %in% c(idVs,"CTR_RSA","ODDEVEN_RSA")]
  
  # Detect outliers within each time point and replace with NA
  for (cc in depV){
    results[,std:=lapply(.SD,sd),by=c(timeVs),.SDcols=cc]
    results[,bad:=abs(results[[cc]]) > std*5] # outlier based on std
    results[,(cc):=ifelse(bad,NA,results[[cc]])]
    print(sprintf("For %s, %f2 percent of rows were removed",cc,(length(which(results$bad))/dim(results)[1])*100))
    results<-results[bad!=T,] # remove bad trials altogether??
    #results[,bad:=outlier(results[[cc]],logical=T)] # outlier based on package(only one value??)
  }

  # Merge to behavioral template and put into DB
  setnames(results,depV, paste0(depV,modelV[2])) #Rename F1(High TestP) and F2(Low TestP)
  predB<-merge(results,ds_beh,by=c("SUBID","BLOCK","TRIAL"));#use ds_beh(containing all vars)
  dbWriteTable(dbCon,name=dbName,value=predB,row.names=FALSE,append=TRUE);#Try to append!
}

# Checking
# beep()
# dscheck=as.data.table(dbGetQuery(dbCon,paste0('SELECT * FROM ',dbName,'  LIMIT 5')));