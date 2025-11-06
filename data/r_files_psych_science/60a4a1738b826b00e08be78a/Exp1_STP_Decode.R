# Exp1_STP_Decode
# peforms basic time-resolved decoding of label N
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

# Source Files
setwd(Dir_R)
source('basic_theme.R')
source('basic_libS.R')

#Load behavioral data
setwd(Dir_BDATA)
ds_b<-fread("Exp1_BehP.txt")
ds_b[is.nanM(ds_b)]<-NA;

#Analysis Setting
setwd(Dir_EDATA)
subs<-list.files( pattern="^A[[:digit:]]{3}");
modelV<-"TSCONJ";
sessN<-"stimLKf";
balanceV<-NA
cutV=NA;
saveACC<-F;
savePRED<-T;
saveCM<-F;
saveIMP<-F;
saveROC<-F

# Classification Setting
#"lda" = linear discriminant analysis
#"knn" = k nearest means
#"svmRadial" = support vector machine
#"nb" = naive bayes
#"rf" = random forest
#"svmLinear3" = L2 Regularized Support Vector Machine (dual) with Linear Kernel
#"pda" = penalized linear discriminant analysis
method<-'pda';
formula<-as.formula(paste(modelV,' ~ .'))
metric<-"Accuracy";
control<-trainControl(method="repeatedcv",number=4,repeats=8, 
                      selectionFunction = "oneSE",
                      classProbs=TRUE,allowParallel=TRUE,savePredictions=TRUE)

#Feature labels
freqL=c("Delta","Theta","Alpha","Beta","Gamma");
elecL<-fread(paste0(Dir_R,fsep,"chanlocs_20E_Oregon.txt"))
varL = expand.grid(elec = as.vector(elecL$Elec),freq = freqL)
varL = str_c(varL$freq,"_",varL$elec)


# IN THE LOOP FOR SUBJECT!!
for (s in subs){
  # STEP 1: Merging data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load EEG data via HDF5 file
  Dir_Data_i<-paste0(Dir_EDATA,fsep,s,fsep,"DATASETS")
  setwd(Dir_Data_i);H5close();
  f2load<-list.files(path=Dir_Data_i,pattern='*STIMLOCK.h5',full.names=TRUE)
  #f2load<-list.files(path=Dir_Data_i,pattern='*STOPLOCK.h5',full.names=TRUE)
  #f2load<-list.files(path=Dir_Data_i,pattern='*RESPLOCK.h5',full.names=TRUE)
  
  ds_eeg<-h5read(f2load,"eegpower");#ds_info<-h5ls(f2load);
  btIDX<-data.table(h5read(f2load ,"IDX"))%>%dplyr::rename(BLOCK=V1,TRIAL=V2);
  
  # Get Individual data to match to EEG data (this will reflect EEG artifact rejection)
  # ds_bl is typically smaller than original ds_b because btIDX reflects AR...
  subN<-as.numeric(gsub("A","",s));# Filter data and keep indexes
  ds_bl<-merge(btIDX,ds_b[SUBID==subN],by=c('BLOCK','TRIAL'))
  ds_bl[,POKAY := ifelse(STOP == 0 & ACC == 0, 0, 1)]

  # Applies 1) filtering, 2) cutting, 3) balancing, and 4) factorization
  prepSet <- list(
    filtIDX =! is.na(ds_bl[[modelV]]) & ds_bl$TRIAL>1 & ds_bl$POKAY==1, #StimLK, StopLK, RespLK
    #filtIDX =! is.na(ds_bl[[modelV]]) & ds_bl$TRIAL>1 & ds_bl$POKAY==1 & ds_bl$STOP==0,# for L1Stop effect?
    modelV = modelV, balanceV = balanceV, cutV = cutV)
  
  pp <- preprop_decode(ds_bl,ds_eeg,prepSet);
  ds_bl <- pp$ds_beh; ds_eeg <- pp$ds_eeg
  
  # STEP 2: Classification~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  registerDoMC(4);
  ptm<- proc.time();
  dims<-dim(ds_eeg);
  timeIDX<-1:dims[2];freqIDX<-1:dims[3];elecIDX<-1:dims[4];
  sprintf("Classification starts!! Retained data are %d trials- %d samples- %d fband- %d electrodes",dims[1],dims[2],dims[3],dims[4])
  
  results<-
    foreach(t=timeIDX) %dopar% {
      #Get data of one point!
      d<-ds_eeg[,t,freqIDX,elecIDX];#pick frequency band to include!
      dim(d)<-c(dim(d)[1],prod(dim(d)[2:3],na.rm=T))#adjust dimensions !
      
      #Assigne label
      C <- ds_bl[[modelV]];if(grepl("nppr",sessN)){C<-sample(C)} 
      d<-data.table(C=C,data.table(d));#make sure to data.table(d) before appending!
      setnames(d,"C",modelV)
      
      #Run classification
      m<-train(formula, data=d, method=method, metric=metric, trControl=control);
      
      # #Get all results
      r <- summary_decode(m,varL,list(PRED=savePRED,CM=saveCM,IMP=saveIMP,ROC=saveROC))
      r <- r[sapply(r, function(x) dim(x)[1]) > 0];# remove empty slots
      r_f <- lapply(r, cbind,SUBID=subN,time=timeIDX[t])# don't lapply r_f repeatedly
      r_f$r_prob <- r_f$r_prob %>% mutate(BLOCK=ds_bl$BLOCK,TRIAL=ds_bl$TRIAL) # should be safe!
      return(r_f)
    }
  proc.time() - ptm
  
  # Summarize all results
  rALL<-as.list(as.data.frame(do.call(rbind, results)))
  
  # Result 1:Overall accuracy
  r_accG<-rbindlist(rALL$r_acc);
  if (saveACC){saveRDS(r_accG,paste0(s,"_",modelV,"_",sessN,"_accG.rds"))}
  
  # Result 2:Single-trial accuracy and confidence
  # Both acc and prob would be 0~1 since its cross-validated!
  r_pred<-rbindlist(rALL$r_prob) %>% dplyr::select(-rowIndex);
  if (savePRED){saveRDS(r_pred,paste0(s,"_",modelV,"_",sessN,"_pred.rds"))}
  
  # Result 3:Confusion matrix
  r_cm<-rbindlist(rALL$r_cm)
  if (saveCM){saveRDS(r_cm,paste0(s,"_",modelV,"_",sessN,"_cm.rds"))}
  
  # Result 4:Importance map
  r_imp<-rbindlist(rALL$r_imp)
  if (saveIMP){saveRDS(r_imp,paste0(s,"_",modelV,"_",sessN,"_cm.rds"))}
  
  # STEP 3: Quickly plotting?~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot related setting (for quick check!)
  theme_set(theme_bw(base_size = 20))#32/28
  CSCALE_YlOrRd = rev(brewer.pal(9,"YlOrRd"));
  chance<-1/length(unique(ds_bl[[modelV]]))
  
  # Overall Accuracy
  quartz(width=7.5,height=4.5);theme_set(theme_bw(base_size = 20))#32/28
  print(ggplot(data=r_accG,aes(x=time,y=Accuracy,ymin=Accuracy-AccuracySD,ymax=Accuracy+AccuracySD)) +
          geom_vline(xintercept=75,linetype=1,size=1)+annotate("text",x=85, y=0.3,label="Cue")+
          geom_vline(xintercept=150,linetype=1,size=1)+annotate("text",x=175, y=0.3,label="Stimulus")+
          geom_hline(yintercept=chance,linetype=1,size=1)+
          geom_ribbon(alpha=0.2,linetype=0,fill="red")+#
          ggtitle(s)+
          geom_line(size=1.25,color="red"))
  
}
