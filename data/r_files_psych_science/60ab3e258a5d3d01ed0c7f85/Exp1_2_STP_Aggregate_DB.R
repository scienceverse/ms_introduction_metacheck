# Exp2_STP_AggregateDB
# load db file (contains single-trial accuracy & confidence)
# then produce aggregated data for specified conditions (with few attributes)
# Additional Processing---------------------------------------
# -segmentation: averaging with time window 
# -realignment: re-epoching to specific event 
# =============================================================
#Removing evrything from workspace
# graphics.off()
rm(list = ls(all = TRUE))

#Setup directories
fsep<-.Platform$file.sep;
Dir_R<-path.expand("~/w_Data_Code/General")
Dir_EDATA<-path.expand("~/w_Data_Code/Exp2/EEG") # to load database file 
Dir_BDATA<-path.expand("~/w_Data_Code/Exp2/BEH") # to load Exp1_BehP.txt (not really necessary because information is contained in design file, just in case of analyses the use design informaton not contained in data base)

# Load libraries
library(data.table)
library(tidyverse)
library(broom)
library(rhdf5)
library(foreach)
library(zoo)
library(doMC)
library(binhf)
library(GGally)
library(RColorBrewer)
library(RSQLite)

# Source Files
setwd(Dir_R)
source('basic_S.R')
#Atsushis functions for decoding

#Define basic info  --- refRLV potential variable for realignement
saveOn<-T;
fileN<-"test"
timeV<-"time"
timeL<-seq(-600,800,4);#Stim_LK:seq(-600,1000,4),Resp_LK:seq(-800,0,4),NA=SEGMENTED DATA
timeS<-NA#seq(0,800,800)#c(0,600)#seq(-600,600,4)#c(begining,end,window)
refRLV <- NA;# Realignment: re-epoch data referencing refRLV timing. To use it specify variable containing reference time point here, such as SSD (stop signal time)
srate<-4
if (!is.na(refRLV)){adj<-which(timeL==0)}else{adj<-0}
cutF <- function(t){as.numeric(cut(t,breaks=timeS,include.lowest=T))}
idxF<-function(i) which.min(abs(timeL-i))-adj # normal

# Database= PP: REG / RSAFits: FIT_RSA_TSCONJ,FIT_RSA_L1TSCONJ,FIT_RSA_TSCONJ_RL,FIT_RSA_TCTSCONJ
setwd(Dir_GRAND)
expN<-"Exp1_STP"
tableN<-"FIT_RSA_TSCONJ_CTRR";#REG/FIT_RSA_TSCONJ_CTR/FIT_RSA_TSCONJ_AGE
depV<-c("CONJ_RSA","RULE_RSA","RESP_RSA","STIM_RSA");# TSCONJ, C2:"DIAG_RSA","RAND_RSA"
# grpV<-c("SUBID","time","STOP")#variables used for aggregation(2nd must be time!)
grpV<-c("SUBID","time","FBAND","ELEC","STOP")#variables used for aggregation, first element is assumed to be subject, second is time, rest is arbitrary.

# Open connections
dbCon=dbConnect(dbDriver("SQLite"),paste0(expN,'_',tableN))# Open database connection
dscheck=as.data.table(dbGetQuery(dbCon,paste0('SELECT * FROM ',tableN,'  LIMIT 10000')));
dbCL<-dbListFields(dbCon,dbListTables(dbCon))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Realignment: add timeLK_[event] variable, idx in same unit as time
# timeL will be arbitrary length, and 0 = onset of event
# it willadd timeLK_[ev] column in the dataset (when it does not exist)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!is.na(refRLV)){
  # Define RL eqn and update few variables for later process
  timeRLV<-paste0("timeLK_",refRLV)
  grpV<-gsub("time",timeRLV,grpV);

  # update original database (only when it does not exist before)
  if (!any(dbCL %in% timeRLV)){
    zeroIDX <- max(which(timeL<=0))
    tleqn<-sprintf("-1 *(floor(%s/%d) + %d - time)",refRLV,srate,zeroIDX);
    #eqn<-function(RT,time){-1 *(round(RT/srate) - time + adjIDX)}
    
    print(paste0('Realignment is requested:',timeRLV," = ",tleqn))
    dbExecute(dbCon, sprintf("ALTER TABLE %s ADD COLUMN %s NUMBER",tableN,timeRLV))
    dbExecute(dbCon,sprintf("UPDATE %s SET %s = %s",tableN,timeRLV,tleqn))
    }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AGGREGATION: grouping SUBID x gvar in database with flexible query sentences:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aggregation with SQ laguage
tIDX<-sapply(range(timeS),idxF);whereQ="";print(tIDX)
selectQ<-paste0('SELECT ',paste(unique(c(grpV)),collapse = ", "))
compQ<-paste(sprintf(", AVG(%s) AS %s",depV,depV),collapse = "")
if (!any(is.na(timeL))){whereQ <- sprintf(" WHERE %s >= %d AND %s <= %d",timeV,tIDX[1],timeV,tIDX[2])}
groupQ<-paste0(' GROUP BY ',paste(grpV,collapse = " , "))
querySTW<-paste0(selectQ,compQ,sprintf(" FROM %s",tableN),whereQ,groupQ);
print(querySTW);tic();dsA=as.data.table(dbGetQuery(dbCon,querySTW));toc();

# Keep consistant time labels (this should be fixed in decoding step...)!
timeC<-colnames(dsA)[grepl("time",colnames(dsA))]
if (diff(unique(dsA[,get(timeC)]))[1]!=srate){
  #dsA[,(timeC):=lapply(.SD,"*",adjV),.SDcols=timeC]
  dsA[,(timeC):=lapply(.SD,function(t)timeL[t]),.SDcols=timeC]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Segmentation: averaging data over time window (aggregation with time intervals)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (diff(timeS[c(1,2)])>srate & !any(is.na(timeL))){
  # Define averaging window then aggregate
  dsA[,(timeC):=(lapply(.SD,cutF)),.SDcols=timeC]
  dsA<-dsA[,lapply(.SD, mean,na.rm=T),by=grpV]# time labels will be mid-point
  dsA[,(timeC):=lapply(.SD,function(t)rollmean(timeS,2)[t]),.SDcols=timeC]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filtering out subjects??
# Filter out some conditions?
# Allows to exclude bad subejcts or trials to be ready for subequent analyis stage.  Currently nothing is excluded.

# Save session information as attributes
setwd(Dir_GRAND)
attributes(dsA)$timeL<-timeL
attributes(dsA)$timeS<-timeS
attributes(dsA)$timeSS<-timeSS
attributes(dsA)$grpV<-grpV
attributes(dsA)$depV<-depV
if(grep("time|time_f|time_j",grpV[2])){code<-"timeSERIES"}else{code<-timeRLV}
if (saveOn){saveRDS(dsA,paste0(expN,"_",fileN,"_",code,".rds"))}
# write.table(dsA,paste0('HAL2017_STOP_TEST.txt'), sep="\t",row.names=FALSE)

# checking
print(dsA);dsAGG<-dsA[, lapply(.SD, mean, na.rm=TRUE), by=c(grpV[-1]), .SDcols=depV]
# quartz(height=3,width=6);qplot(time,RESP_RSA,geom=c("auto"),facets = "~SUBID",color=interaction(STOP),data=dsA[SUBID==123,],size=0.15)#+theme_classic()
quartz(height=3,width=6);qplot(time,CONJ_RSA,geom=c("auto"),color=interaction(STOP),data=dsAGG,size=0.15)

