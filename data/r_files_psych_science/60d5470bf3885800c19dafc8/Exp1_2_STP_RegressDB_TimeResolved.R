# Exp1_2_STP_RegressDB_TimeResolved
# runs time-series regression analysis (from database)
# -realignment: re-epoching to specific event (timeRF_[event] must exist in data)
# -segmentation: averaging with time window 
# =============================================================
#Removing evrything from workspace
graphics.off()
rm(list = ls(all = TRUE))

#Setting up directory
fsep<-.Platform$file.sep;
Dir_R<-path.expand("~/w_Data_Code/General")
Dir_EDATA<-path.expand("~/w_Data_Code/Exp1/EEG") 
Dir_BDATA<-path.expand("~/w_Data_Code/Exp1/EEG") 

# Load libraries
library(data.table)
library(tidyverse)
library(broom)
library(rhdf5)
library(caret)
library(doMC)
library(binhf)
library(RColorBrewer)
library(RSQLite)
library(lazyeval)
library(forcats)
library(stringr)
library(psych)
library(lme4)
library(sjstats)

# Source Files
setwd(Dir_R)
source('basic_libS.R')
logit2prob <- function(logit){odds <- exp(logit);prob <- odds / (1 + odds);return(prob)}

#Define basic info
setwd(Dir_EDATA)
subs<-list.files( pattern="^A[[:digit:]]{3}");
timeV<-"time_SSDLK"
srate<-4
timeL<-seq(-600,800,4)#Stim_LK:seq(-600,800,4),Resp_LK:seq(-800,0,4) -> different from realignment
timeS<-seq(0,800,20)#time segment:seq(-600,800,20) for stim LK/seq(-500,500,20) for StopRAL
aveW<-NA;# running averaging 
saveOn=TRUE;# put name for saving results

# Realignment
refRLV <- NA
adjIDX <- max(which(timeL<=0))
idxF <- function(i) which.min(abs(timeL-i))
if (!is.na(refRLV)){idxF<-function(i){which(timeL==0)-which(timeL%in%i)}}
# if (!is.na(refRLV)){idxF<-function(i) (i/abs(i))*which.min(abs(abs(timeL)*(i/abs(i))-i))}

# Database setting
setwd(Dir_GRAND)
expN<-"HAL2017_STPADP";
fileN<-"testMLM"
tableN<-"FIT_RSA_TSCONJ_CTR";
dbCon=dbConnect(dbDriver("SQLite"),paste0(expN,'_',tableN))# Open database connection
dbT <- tbl(dbCon, tableN);# Table for dbplyr
dscheck=as.data.table(dbGetQuery(dbCon,paste0('SELECT * FROM ',tableN,'  LIMIT 100000')));print(dscheck)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract all vars from equation and load all data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Stopping accuracy
# depV<-"ACC";
# formula<-paste0(depV,'~1+CONJ_RSA+RULE_RSA+STIM_RSA+RESP_RSA+SSD+(1|SUBID)');
# formula<-paste0(depV,'~1+RULE_RSA+STIM_RSA+RESP_RSA+CONJ_RSA+(1+RULE_RSA+STIM_RSA+RESP_RSA+CONJ_RSA|SUBID)');

# # Go RT
# depV<-"RT"
# formula_pw<-paste0(depV,'~1+BLOCK+TRIAL+(1|SUBID)')
# formula<-paste0(depV,'~1+RULE_RSA+STIM_RSA+RESP_RSA+CONJ_RSA+(1+RULE_RSA+STIM_RSA+RESP_RSA+CONJ_RSA|SUBID)');

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Realignment: add timeLK_[event] variable, idx in same unit as time
# timeL will be arbitrary length, and 0 = onset of event
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!is.na(refRLV)){
  # Define RL eqn and update few variables for later process
  timeRLV<-paste0("timeLK_",refRLV);timeV<-timeRLV
  timeL<-rev(seq(-3000,3000,unique(diff(timeL))))#up to 3000 ms
  tleqn<-sprintf("-1 *(round(%s/4) - time + %d)",refRLV,adjIDX);
  #tleqn<-sprintf("-1 *(floor(%s/%d) + %d - time)",refRLV,srate,adjIDX);#orig
  print(paste0('Realignment is requested:',timeRLV," = ",tleqn))
  
  # update original database (only when it does not exist before)
  if (!any(colnames(dbT) %in% timeRLV)){
    dbExecute(dbCon, sprintf("ALTER TABLE %s ADD COLUMN %s NUMBER",tableN,timeRLV))
    dbExecute(dbCon,sprintf("UPDATE %s SET %s = %s",tableN,timeRLV,tleqn))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract all vars from equation and load all data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
supV<-c("STOP","U1RT","RT","LACC","ACC","TASK","SSD")
facV<-cols<-c("STOP","TASK","RESP","SWITCH","RESPREP")
varList<-str_extract_all(formula,boundary("word"))[[1]]
varList<-gsub('^[[:digit:]]+', '', varList);
varList<-varList[!grepl(":",varList)];
varList<-c(varList,timeV,"BLOCK","TRIAL",supV)
varList<-paste(unique(varList[varList != ""]) ,collapse = " , ");print(varList)

#queryL <-dbT %>% filter() %>% group_by() %>% summarize %>% show_query()
tIDX<-sapply(range(timeS),idxF);print(tIDX)
selectQ <- paste0('SELECT ',varList)
whereQ <- sprintf(" WHERE %s >= %d AND %s <= %d",timeV,tIDX[1],timeV,tIDX[2])
querySTW<-paste0(selectQ,sprintf(" FROM %s",tableN),whereQ);print(querySTW)
dsA=as.data.table(dbGetQuery(dbCon,querySTW));# no aggregation for MLM

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Segmentation: averaging data over time window
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relabel time (or timeLK_[event]) 
if (!is.na(refRLV)){dsA[,(timeV):=dsA[[timeV]]*srate]}
if (is.na(refRLV)){dsA[,(timeV):=timeL[abs(dsA[[timeV]])]]}

if (diff(timeS[c(1,2)])>1){
  # Define averaging window then aggregate and update labels
  #eIDX<-!(1:length(timeS) %% 2);timeS[eIDX] <-timeS[eIDX]-1
  cutF <- function(t){as.numeric(cut(t,breaks=timeS,include.lowest=T))}
  dsA[,(timeV):=(lapply(.SD,cutF)),.SDcols=timeV]
  dsA<-dsA[,lapply(.SD, mean),by=c("SUBID","BLOCK","TRIAL",timeV)]
  timeSS<-cbind(timeS[1:length(timeS)-1],timeS[2:length(timeS)])
  dsA[,(timeV):=rowMeans(timeSS)[dsA[[timeV]]]]#mid-point
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust values 
vars<-colnames(dsA)
dsA[,PMRESP:=ifelse(STOP == 1 & ACC == 0, (RT < SSD)*1, NA)]#pre-mature response for failed stopping
dsA[,ODDEVEN := (dsA$TRIAL %% 2)]
dsA$BLOCK<-as.numeric(scale(dsA$BLOCK,center=T,scale=F));
dsA$TRIAL<-as.numeric(scale(dsA$TRIAL,center=T,scale=F));
dsA$RT<-log(dsA$RT)
dsA$U1RT<-log(dsA$U1RT)
dsA$SSD<-log(dsA$SSD);dsA<-dsA[!is.infinite(dsA$SSD),]

# Filtering
dsA<-dsA[SUBID!=216 & SUBID!=219 & SUBID!=230];# rejected
dsA<-dsA[SUBID!=207 & SUBID!=224 & SUBID!=235];# high stop accuracy
dsA[,POKAY:=ifelse(STOP==0 & ACC == 0 ,0,1)]

# Filtering again
# dsA<-dsA[ACC==1 & STOP==0 & RT>10 & RT<cutRT];#go trial
dsA<-dsA[STOP==1 & POKAY==1,];#stop trial
# dsA<-dsA[!which(dsA$PMRESP==1),];#exclude pre-mature responses in failed stop trials

# Run time-series regression
registerDoMC(6)
ptm<- proc.time()
r<-
  foreach(t=unique(dsA[[timeV]])) %dopar% {
    #Load all data ahead (its okay for data < 3GB)
    ds<-dsA[get(timeV)==t,]
    #ds<-dsA[time>=t-aveW & time<=t+aveW,lapply(.SD,mean),keyby=c("SUBID","BLOCK","TRIAL")]#moving ave
    
    #Prewitten DV!
    #pw<-lmer(formula_pw,data=ds);#
    #ds[[depV]]<-residuals(pw);
    
    #Fit a model
    #m<-lme4::lmer(formula,data=ds);#Fitting
    m<-lme4::glmer(formula,data=ds,family=binomial());#logistic-regression

    #Summarize results + calculate probability!
    r<-std_beta_MLM(m)@fixef %>% data.table()
    r<-r[term!="(Intercept)",]
    r$time<-t;# to convert logodds to prob (use predict or calculate by hand)
    feff<-fixef(m)#intercept + fixef of all predictors (https://sebastiansauer.github.io/convert_logit2prob/)
    prob<-logit2prob(c(feff[1],feff[1]+2*feff[-1]))
    r$prob<-prob[-1];r$probD<-prob[-1]-prob[1]
    return(r)
}
proc.time() - ptm

# Summarize data andsave!
setwd(Dir_GRAND);
rALL<-bind_rows(r);
attributes(rALL)$formula_pw<-formula_pw
attributes(rALL)$formula<-formula
if(timeV=="time"){code<-"stimLK"}else{code<-timeRLV}
if(saveOn){saveRDS(rALL,paste0(expN,"_",fileN,"_",code,".rds"))}

# #======================================================================================================
# # Plotting
# #======================================================================================================
theme_set(theme_bw(base_size = 22))#32/28
CSCALE_Greys = rev(brewer.pal(9,"Greys"));
CSCALE_Set1 = (brewer.pal(9,"Set1"));
CSCALE_Spectral = rev(brewer.pal(11,"Spectral"));
CSCALE_Paired = (brewer.pal(12,"Paired"));
CSCALE_Grey = (brewer.pal(9,"Greys"));
#csets<-c(CSCALE_Set1[4],CSCALE_Set1[2],CSCALE_Set1[3],CSCALE_Set1[1],CSCALE_Set1[5])
csets<-c(CSCALE_Set1[2],CSCALE_Set1[4],CSCALE_Set1[3],"gray30",CSCALE_Set1[1])

# Plot setting
actDF<-rALL;
timeL<-unique(actDF$time)
timeD<-abs(timeL[1]-timeL[2])
ylim<-c(-4,4)
xlim<-range(timeL)
gvar<-c("term")
rectangles<-data.frame(xmin= xlim[1]-timeD/2,xmax=xlim[2]+timeD/2,ymin=-2,ymax=2)

# General plot (use [Exp]_plot functions for pub)
quartz(width=8,height=5)
ggplot() +
  #Axis--------------------------------
  geom_rect(data=rectangles,inherit.aes = FALSE,aes(xmin=xmin,xmax=xmax, ymin=ymin, ymax=ymax),fill=CSCALE_Greys[5], alpha=0.2)+
  geom_hline(yintercept=0,linetype=2,size=1)+
  scale_y_continuous(breaks=seq(from=-20,to=20,by=2))+
  scale_x_continuous(breaks=seq(from=-1000,to=1000,by=200),expand=c(0,0))+
  scale_color_manual(values=csets)+scale_fill_manual(values=csets)+
  ylab("t value")+xlab("Time (ms)")+
  #Errorbar + sig bar-------------------
  geom_line(data=actDF,aes_string(x="time",y="statistic",group="term",colour="term"),size=1.5)+
  geom_point(data=actDF,aes_string(x="time",y="statistic",group="term",colour="term"),size=5)+
  #Aesthetics!-------------------------
  theme(panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.margin=unit(rep(1,1,4),"line"),
      axis.line=element_line(size=1),
      legend.key = element_blank(),
      #legend.key.size=unit(1,"lines"),
      legend.position = "right",
      legend.title=element_blank(),
      legend.text =element_text(family="Helvetica",size=12),
      axis.ticks.length = unit(0.25, "lines"),
      axis.title  = element_text(family="Helvetica", face="bold",vjust=1.8),
      axis.text   = element_text(size=18,face="bold"),
      #axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 20),
      strip.background=element_blank())

