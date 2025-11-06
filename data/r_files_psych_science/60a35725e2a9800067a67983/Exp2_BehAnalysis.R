# Exp2: 3 fixed stop-signal-delay
# =============================================================
#Removing evrything from workspace
graphics.off()
rm(list = ls(all = TRUE))

#Setting up directory
# Dir_R<-path.expand("~/w_Data_Code/General")
# Dir_Data<-path.expand("~/w_Data_Code/Exp1")

# Libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(lme4)
library(RColorBrewer)
library(forcats)
library(effects)
library(broom)
library(afex)
library(psych)

# Source Files
setwd(Dir_R)
source('basic_libS.R')
#======================================================================================================
#Load data
#======================================================================================================

# READING FILES
setwd(Dir_Data)#Change directory
ds<- fread('Exp2_BehP.txt', header=TRUE, fill=TRUE)
ds[is.nanM(ds)]<-NA;

#======================================================================================================
# Preprocessing
#======================================================================================================
# Add variables
ds[,OMIT:=RT>800]
ds[,RTlog:=log(RT)]
ds[,POKAY := ifelse(STOP == 0 & ACC == 0, 0, 1)]
ds[,L1POKAY := ifelse(L1STOP == 0 & LACC == 0, 0, 1)]
ds[,ACC_STOP_F := ifelse(STOP==1 & RESP!=99,(CORRESP==RESP)*1,NA)]

# SRmaps
tsconj<-apply(expand.grid(c("1","2","3","4"),c("V","H","D"))[,c(2,1)],1,paste,collapse=".")
ds$TSCONJ_L<-tsconj[ds$TSCONJ]#tsconj

# Recode and factorize
ds[,ACC_L:=factor(ACC,labels=c("Incorrect","Correct"))]
ds[,STOP_L:=factor(STOP,labels=c("Go","Stop"))]
ds[,L1STOP_L:=factor(L1STOP,labels=c("Go","Stop"))]
ds[,SWITCH_L:=factor(SWITCH,labels=c("Repeat","Change"))]
ds[,L1SWITCH_L:=factor(L1SWITCH,labels=c("Repeat","Change"))]
ds[,RESPREP_L:=factor(RESPREP,labels=c("Change","Repeat"))]
ds[,STIMREP_L:=factor(STIMREP,labels=c("Change","Repeat"))]

# RT cutoff
cutRTi<-ds[STOP_L=="Go" & ACC==1,list(cutRT=quantile(RT,0.995)),by=SUBID]
ds<-merge(ds,cutRTi,by="SUBID")
print(sprintf("Response cutoff %.3f",mean(cutRTi$cutRT)));

#Filtering (do this locally to be explicit!)
ds<-ds[TRIAL!=1]
ds<-ds[SUBID!=104 & SUBID!=111] # excluded subjects
ds<-ds[!c(ds$STOP_L=="Go" & ds$RT<10),];
ds<-ds[!c(ds$STOP_L=="Go" & ds$RT>ds$cutRT),];
ds_f<-ds[ACC==1 & L1POKAY==1,]

#======================================================================================================
# Data Aggregation
# write.table(paste0(Dir_Data,'/w_AGGREGATED/HAL2017_STP_L1STOP.txt'), sep="\t",row.names=FALSE)
#======================================================================================================
# # RT:L1STOP  
# l1stp_RT<-ds[STOP_L=="Go" & ACC==1 & LACC==1]%>%group_by(SUBID,L1STOP_L)%>%summarise(DV=mean(RT,na.rm=TRUE))%>%
#   normWS("SUBID","DV")%>%group_by(L1STOP_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_RT)
# 
# # ACC:L1STOP
# l1stp_ACC<-ds[STOP_L=="Go" & LACC==1]%>%group_by(SUBID,L1STOP_L)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE))*100)%>%
#   normWS("SUBID","DV")%>%group_by(L1STOP_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_ACC)
# 
# # STOPACC:L1STOP 
# l1stp_STPACC<-ds[STOP_L=="Stop" & LACC==1]%>%group_by(SUBID,L1STOP_L)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE))*100)%>%
#   normWS("SUBID","DV")%>%group_by(L1STOP_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_STPACC)

#======================================================================================================
# Conjunction RTs and Errors for RSA
#======================================================================================================
# # TSCONJ (Stopping accuracy)
# cj_DVi<-ds_f[STOP_L=="Go",.(RT=mean(RT),RTlog=log(mean(RT))),by=c("SUBID","TSCONJ")] 
# cj_DVi[,ACC:=ds[STOP_L=="Stop",.(ACC=mean(ACC)),by=c("SUBID","TSCONJ")]$ACC]
# write.table(cj_DVi,paste0(Dir_Data,'/HAL2017_STP_TSCONJ_CONTROL.txt'), sep="\t",row.names=FALSE)

# # # TSCONJ_C1 (Stopping accuracy)
# cj_DVi<-ds_f[STOP_L=="Go",.(RT=mean(RT),RTlog=log(mean(RT))),by=c("SUBID","TSCONJ_C1")]
# cj_DVi[,ACC:=ds[STOP_L=="Stop",.(ACC=mean(ACC)),by=c("SUBID","TSCONJ_C1")]$ACC][order(SUBID,TSCONJ_C1)]
# write.table(cj_DVi,paste0(Dir_Data,'/HAL2017_STP_TSCONJ_CONTROL_C12.txt'), sep="\t",row.names=FALSE)

#======================================================================================================
# Individual differences
#======================================================================================================
# -Go Omissions (GO)
# -Choice errors on go trials (GO)
# -RT on go trials (GO)
# -Probability of responding on stop trials (STOP)
# -RT of go responses on unsuccessful stop trials (STOP)
# -Choice errors on unsuccessful stop trials (STOP)
# (-Average SSD)
# (-SSRT)
#======================================================================================================


# Behavior from Go trials(Go Omissions(if there wa a cutoff),Error, RT)
go_BEH<-ds[STOP_L=="Go",list(GO_OMIT=mean(OMIT)*100,GO_ERROR=(1-mean(ACC))*100),by=SUBID]
go_BEH$GO_RT<-ds[STOP_L=="Go" & ACC==1,list(GO_RT=mean(RT)),by=SUBID]$GO_RT
go_BEH$N<-ds_f[,.N,by=SUBID]$N

# Behavior from Stop trials
stop_BEH<-ds[STOP_L=="Stop",list(STOP_ERROR=(1-mean(ACC))*100),by=SUBID]
stop_BEH$STOP_F_RT<-ds[STOP_L=="Stop" & ACC==0,list(DV=mean(RT)),by=SUBID]$DV
stop_BEH$STOP_F_ERR<-ds[STOP_L=="Stop" & ACC==0,list(DV=(1-mean(ACC_STOP_F))*100),by=SUBID]$DV

# Merge and save individuals' data and summary
both_BEH<-merge(go_BEH,stop_BEH,by="SUBID")
both_BEHs<-describe(both_BEH)%>%add_rownames();print(both_BEHs)
write.table(both_BEH,paste0(Dir_Data,'/Exp1_BEHBASIC_i.txt'), sep="\t",row.names=FALSE)
write.table(both_BEHs,paste0(Dir_Data,'/Exp1_BEHBASIC.txt'), sep="\t",row.names=FALSE)

# #======================================================================================================
# MLM
# #======================================================================================================
# # L1Stop effect
# m_rt=lme4::lmer(RTlog~1+L1STOP_L+(1+L1STOP_L|SUBID),data=ds[STOP_L=="Go" & ACC==1]);summary(m_rt)
# m_acc=glmer(ACC~1+L1STOP_L+(1+L1STOP_L|SUBID),family=binomial,data=ds[STOP_L=="Go"]);summary(m_acc)
# m_stop=glmer(ACC~1+L1STOP_L+(1+L1STOP_L|SUBID),family=binomial,data=ds[STOP_L=="Stop"]);summary(m_stop)

#======================================================================================================
# Plot
#======================================================================================================
theme_set(theme_bw(base_size = 20))#32/28
CSCALE_PURD = rev(brewer.pal(9,"PuRd"));
CSCALE_BLUE = rev(brewer.pal(9,"Blues"));
CSCALE_PiYG = rev(brewer.pal(11,"PiYG"));
CSCALE_RdBu = rev(brewer.pal(11,"RdBu"));
CSCALE_PAIRED = rev(brewer.pal(12,"Paired"));
CSCALE_YlGnBu = rev(brewer.pal(9,"YlGnBu"));
CSCALE_BrBG = rev(brewer.pal(9,"BrBG"));
CSCALE_Set1 = (brewer.pal(9,"Set1"));
CSCALE_Pair = (brewer.pal(9,"Paired"));
CSET_MODEL<-c(CSCALE_BLUE[1],CSCALE_BLUE[2],CSCALE_BLUE[3])
CSETS<-c("black","red")
# CSETS<-c("black","green")
# CSETS<-c("black","blue")

# Conjunction bar graph
ds[,(c("TSCONJ_L")):=lapply(.SD, as.factor),.SDcols=c("TSCONJ_L")]
# actDSi<-ds[STOP_L=="Go" & ACC==1,]%>%group_by(SUBID,TASK,TSCONJ_L,TSCONJ)%>%summarise(DV=mean(RT,na.rm=TRUE))
actDSi<-ds[STOP_L=="Stop",]%>%group_by(SUBID,TASK,TSCONJ_L,TSCONJ)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE)))

quartz(width=8.75,height=3.5)
ggplot(data=actDSi,aes(x=TSCONJ_L,y=DV,group=TSCONJ_L,fill=TSCONJ_L,color=TSCONJ_L)) +
  geom_line(aes(x=TSCONJ_L,y=DV,group=SUBID),size=0.2,alpha=.2,color="gray40")+
  geom_point(position=position_jitter(width=.1),size=2,alpha=.8)+
  geom_boxplot(aes(x=as.numeric(TSCONJ_L)-0.42),width=.4,outlier.shape=12, alpha=0.5)+
  #Axis--------------------------------
  # ylab("Response time (ms)")+xlab("")+#ggtitle("Response")+
  # coord_cartesian(ylim=c(300,1100),xlim=c(0.75,12))+
  # scale_y_continuous(breaks=seq(400,1500,200))+
  ylab("Stopping error (p)")+xlab("")+#ggtitle("Response")+
  coord_cartesian(ylim=c(0,0.8),xlim=c(0.75,12))+
  scale_y_continuous(breaks=seq(0,1,0.2))+
  #Aesthetics!-------------------------
  theme(panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.margin=unit(rep(1,1,4),"line"),
      axis.line=element_line(size=1),
      legend.key = element_blank(),
      #legend.key.size=unit(1,"lines"),
      legend.position = "none",
      legend.title=element_blank(),
      legend.text =element_text(family="Helvetica",size=8),
      axis.ticks.length = unit(0.5, "lines"),
      axis.title  = element_text(family="Helvetica",vjust=1.8),
      axis.text   = element_text(size=16),
      axis.text.x = element_blank(),
      #plot.title = element_blank(),
      strip.text = element_blank(),
      #strip.text  = element_text(family="Helvetica", face="bold",vjust=1.8),
      strip.background=element_blank())
