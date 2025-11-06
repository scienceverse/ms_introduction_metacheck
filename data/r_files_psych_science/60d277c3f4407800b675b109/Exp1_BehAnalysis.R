# Exp1: Adaptive stop-signal-delay
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

setwd(Dir_Data)#Change directory
ds<- fread('Exp1_BehP.txt', header=TRUE, fill=TRUE)
ds[is.nanM(ds)]<-NA;

#======================================================================================================
# Preprocessing
#======================================================================================================

# Add variables
ds[,OMIT:=RT>850]
# ds$RT[ds$STOP==1 & ds$ACC==1]<-NA;%calculate RT of go responses on unsuccessful stop trials (STOP)
ds[,RTlog:=log(RT)]
ds[,POKAY := ifelse(STOP == 0 & ACC == 0, 0, 1)]
ds[,L1POKAY := ifelse(L1STOP == 0 & LACC == 0, 0, 1)]
ds[,PMRESP:=ifelse(STOP == 1 & ACC == 0, (RT < SSD)*1, NA)]#pre-mature response for failed stopping
ds[,ACC_STOP_F := ifelse(STOP==1 & RESP!=99,(CORRESP==RESP)*1,NA)]

# SRmaps
tsconj<-apply(expand.grid(c("1","2","3","4"),c("V","H","D"))[,c(2,1)],1,paste,collapse=".")
ds$TSCONJ_L<-tsconj[ds$TSCONJ]#tsconj

# Create intervals for continuous variables
ds[,SSD_R:=as.numeric(cut(SSD,6,include.lowest=TRUE)),by="SUBID"]
ds[,RT_R:=as.numeric(cut(SSD,6,include.lowest=TRUE)),by=c("SUBID","STOP")]

# Recode and factorize
ds[,ACC_L:=factor(ACC,labels=c("Incorrect","Correct"))]
ds[,LACC_L:=factor(LACC,labels=c("Incorrect","Correct"))]
ds[,STOP_L:=factor(STOP,labels=c("Go","Stop"))]
ds[,L1STOP_L:=factor(L1STOP,labels=c("Go","Stop"))]
ds[,SWITCH_L:=factor(SWITCH,labels=c("Repeat","Change"))]
ds[,RESPREP_L:=factor(RESPREP,labels=c("Change","Repeat"))]
ds[,STIMREP_L:=factor(STIMREP,labels=c("Change","Repeat"))]
ds[,SDGROUP_L:=factor(SDGROUP,labels=c("Fast","Slow"))]

# STOP3 = (0=Stop_fail,1=Stop_success,2=Go)
ds[,STOP3:=factor(ifelse(STOP==0&ACC==0,NA,((1-STOP)+(ACC))),labels=c("STOP_F","STOP_S","GO"))]
ds[,L1STOP3:=factor(ifelse(L1STOP==0&LACC==0,NA,((1-L1STOP)+(LACC))),labels=c("STOP_F","STOP_S","GO"))]
contrC <- contr.helmert(3)[,c(2,1)]*-1#C1=Go vs Stop, C2=Stop_f vs Stop_s
contrasts(ds$STOP3)<-contrC;contrasts(ds$L1STOP3)<-contrC;

# RT cutoff
cutRTi<-ds[STOP_L=="Go" & ACC==1,list(cutRT=quantile(RT,0.995)),by=SUBID]
ds<-merge(ds,cutRTi,by="SUBID")
print(sprintf("Response cutoff %.3f",mean(cutRTi$cutRT)));

#Filtering (but alywas use POKAY or L1POKAT for further)
ds<-ds[TRIAL!=1]
ds<-ds[SUBID!=216 & SUBID!=219] # excluded subjects
# ds<-ds[SUBID!=207 & SUBID!=224 & SUBID!=235];# high stop accuracy
dsK<-ds;# for SSRT calculation, keeo unfliltered data
ds<-ds[!c(ds$STOP_L=="Go" & ds$RT<10),];
ds<-ds[!c(ds$STOP_L=="Go" & ds$RT>ds$cutRT),];

#======================================================================================================
#Process2-Data Aggregation
#======================================================================================================

# SSD: Learning effect
block_SSD<-ds[STOP_L=="Stop"] %>%group_by(SUBID,BLOCK) %>% summarise(DV=mean(SSD,na.rm=TRUE)) %>%
  ungroup() %>% mutate(SUBID=factor(SUBID))

# SSD: SSD & STOPACC for the range of SSD 
stp_SSD<-ds[STOP_L=="Stop"]%>%group_by(SUBID,SSD_R)%>%summarise(DV=mean(SSD,na.rm=TRUE))%>%
  normWS("SUBID","DV")%>%group_by(SSD_R)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(stp_SSD)

stp_ACC<-ds[STOP_L=="Stop"]%>%group_by(SUBID,SSD_R)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE)))%>%
  normWS("SUBID","DV")%>%group_by(SSD_R)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(stp_ACC)

# RT: Failed Stop RT
fstp_RT<-ds[STOP_L=="Stop" & ACC_L=="Incorrect"]%>%group_by(SUBID,SSD_R)%>%summarise(DV=mean(RT,na.rm=TRUE))%>%
  normWS("SUBID","DV")%>%group_by(SSD_R)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(fstp_RT)

# RT:L1STOP  
l1stp_RT<-ds[STOP_L=="Go" & ACC==1 & L1POKAY==1]%>%group_by(SUBID,L1STOP3,L1STOP_L,LACC_L)%>%summarise(DV=mean(RT,na.rm=TRUE))%>%
  normWS("SUBID","DV")%>%group_by(L1STOP3,L1STOP_L,LACC_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_RT)

# ACC:L1STOP
l1stp_ACC<-ds[STOP_L=="Go" & L1POKAY==1]%>%group_by(SUBID,L1STOP3,L1STOP_L,LACC_L)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE))*100)%>%
  normWS("SUBID","DV")%>%group_by(L1STOP3,L1STOP_L,LACC_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_ACC)

# STOPACC:L1STOP 
l1stp_STPACC<-ds[STOP_L=="Stop" & L1POKAY==1]%>%group_by(SUBID,L1STOP3,L1STOP_L,LACC_L)%>%summarise(DV=(1-mean(ACC,na.rm=TRUE))*100)%>%
  normWS("SUBID","DV")%>%group_by(L1STOP3,L1STOP_L,LACC_L)%>%summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n());print(l1stp_STPACC)

# Vincentising (keep it)
bin_RT<-ds %>% group_by(SUBID,STOP_L,ACC_L) %>%
  filter(STOP_L=="Go" & ACC_L=="Correct" | STOP_L=="Stop" & ACC_L=="Incorrect")%>%
  do(data.frame(t(quantile(.$RT,probs=seq(0.1,1,0.2),na.rm =T))))%>%
  gather(prob, DV, X10.:X90.) %>% normWS("SUBID","DV") %>%
  group_by(STOP_L,ACC_L,prob)%>% 
  summarize(DVm=mean(DV),se=sd(DV_n)/sqrt(n()),wseb=se*1.96,n=n()) %>% 
  ungroup() %>% mutate(prob=as.numeric(str_extract(prob,"[[:digit:]]+")))

#======================================================================================================
#Calculate SSRT (integration method (Logan & Cowan, 1984)) for individual differences
#======================================================================================================

# Step1: check all failed stop RTs are faster than Go -> Validate race model
rt_check<-dsK[,list(RT=mean(RT,na.rm=T)),by=c("SUBID","STOP_L","ACC_L")]
rt_check<-dcast(rt_check, SUBID ~ STOP_L+ACC_L,value.var=c("RT"))
rt_check[,SPEED:=Stop_Incorrect<Go_Correct]
all(which(rt_check$SPEED))# everybody meets assumptions for the integration method?

# Step2:  Calculate SSD and probability of failed Stop
stp_SSDi<-dsK[STOP_L=="Stop",list(SSDm=mean(SSD,na.rm=TRUE),FSR=1-mean(ACC,na.rm=T)),by=c("SUBID","SSD_R")]

# Step3: Get rank-ordered RT distribution
library(foreach)
rt_match<-foreach(s=unique(ds$SUBID)) %dopar% {
    quantile(dsK[STOP_L=="Go" & SUBID==s]$RT,stp_SSDi[SUBID==s,]$FSR) %>%
    data.table() %>% tibble::rowid_to_column("SSD_R" ) %>% mutate(SUBID=s)%>% rename("RTmatch" = ".")
  }

# Step 4: Merge data and calculate SSRT = match goRT - SSD ???
stp_SSDi <- merge(stp_SSDi,rbindlist(rt_match),by=c("SUBID","SSD_R")) %>%
  mutate(SSRT = RTmatch - SSDm) %>% group_by(SUBID) %>% summarise_all(.fun=mean) %>%
  mutate(SSDmo = ssd$DV);print(as.data.frame(stp_SSDi))#SSDmo = calculated without binning

# # Export
stp_SSD<-describe(stp_SSDi)%>%add_rownames();print(stp_SSD)
# write.table(stp_SSDi,paste0(Dir_Data,'/Exp1_SSRT_i.txt'), sep="\t",row.names=FALSE)
# write.table(stp_SSD,paste0(Dir_Data,'/Exp1_STPADP_SSRT.txt'), sep="\t",row.names=FALSE)
# # # # The matching no-stop reaction time (RT) was defined for each SSD by taking the nth no-stop trial RT
# # in the rank-ordered no-stop trial RTs, where n is defined by multiplying the number of RTs in the distribution by the probability of responding 
# # (i.e. unsuccessful stop trials) at a given SSD. SSRT for each SSD is calculated by subtracting the SSD value from corresponding matching no-stop RT.

#======================================================================================================
# -Go Omissions (GO) -> does not happen!
# -Choice errors on go trials (GO)
# -RT on go trials (GO)
# -Probability of responding on stop trials (STOP)
# -RT of go responses on unsuccessful stop trials (STOP)
# (-Average SSD)
# (-SSRT)
#======================================================================================================

# Behavior from Go trials(Go Omissions(if there wa a cutoff),Error, RT)
go_BEH<-ds[STOP_L=="Go",list(GO_OMIT=mean(OMIT)*100,GO_ERROR=(1-mean(ACC))*100),by=SUBID]
go_BEH$GO_RT<-ds[STOP_L=="Go" & ACC==1,list(GO_RT=mean(RT)),by=SUBID]$GO_RT

# Behavior from Stop trials
stop_BEH<-ds[STOP_L=="Stop",list(STOP_ERROR=(1-mean(ACC))*100),by=SUBID]
stop_BEH[,STOP_ERROR_GRP:=(STOP_ERROR>=median(STOP_ERROR))*1+1]# 1=low stop error, 2=high stop error 
stop_BEH$STOP_F_RT<-ds[STOP_L=="Stop" & ACC==0,list(DV=mean(RT,na.rm=T)),by=SUBID]$DV
stop_BEH$STOP_F_ERR<-ds[STOP_L=="Stop" & ACC==0,list(DV=(1-mean(ACC_STOP_F))*100),by=SUBID]$DV

# Merge and save individuals' data and summary
both_BEH<-merge(go_BEH,stop_BEH,by="SUBID")
both_BEHs<-describe(both_BEH)%>%add_rownames();print(both_BEHs)

write.table(both_BEH,paste0(Dir_Data,'/Exp1_BEHBASIC_i.txt'), sep="\t",row.names=FALSE)
write.table(both_BEHs,paste0(Dir_Data,'/Exp1_BEHBASIC.txt'), sep="\t",row.names=FALSE)
#======================================================================================================
# Conjunction RTs and Errors for RSA
#======================================================================================================
# # TSCONJ (Stopping accuracy)
# cj_DVi<-ds[STOP_L=="Go" & ACC==1,.(RT=mean(RT),RTlog=log(mean(RT))),by=c("SUBID","TSCONJ")]
# cj_DVi[,ACC:=ds[STOP_L=="Stop",.(ACC=mean(ACC)),by=c("SUBID","TSCONJ")]$ACC]
# write.table(cj_DVi,paste0(Dir_Data,'/Exp1_TSCONJ_CONTROL.txt'), sep="\t",row.names=FALSE)

# # # # # TSCONJ_C1 (Stopping accuracy) for expanded space
# cj_DVi<-ds[STOP_L=="Go" & ACC==1,.(RT=mean(RT),RTlog=log(mean(RT))),by=c("SUBID","TSCONJ_C2")]
# cj_DVi[,ACC:=ds[STOP_L=="Stop",.(ACC=mean(ACC)),by=c("SUBID","TSCONJ_C2")]$ACC][order(SUBID,TSCONJ_C1)]
# write.table(cj_DVi,paste0(Dir_Data,'/Exp1_TSCONJ_CONTROL_C12.txt'), sep="\t",row.names=FALSE)

# #======================================================================================================
# #Process-Statistical Analysis 
# #======================================================================================================
# # L1Stop effect (C1= GO < STOP, C2=STOP_S < STOP_F)
m_rt=lme4::lmer(RTlog~1+L1STOP3+(1+L1STOP3|SUBID),data=ds_f[STOP_L=="Go" & L1POKAY]);summary(m_rt)
m_acc=glmer(ACC~1+L1STOP3+(1+L1STOP3|SUBID),family=binomial,data=ds[STOP_L=="Go" & L1POKAY]);summary(m_acc)
m_stop=glmer(ACC~1+L1STOP3+(1+L1STOP3|SUBID),family=binomial,data=ds[STOP_L=="Stop" & L1POKAY]);summary(m_stop)

# # ANOVA ??
aggDV<-ds[STOP_L=="Go" & L1POKAY,.(DV=mean(ACC)),by=c("SUBID","L1STOP3")]
anova_r<-aov_ez("SUBID","DV",aggDV,within=c("L1STOP3"),anova_table=list(es="pes"));print(anova_r)
#======================================================================================================
#Process3-Plotting-ggplot2
#======================================================================================================
theme_set(theme_bw(base_size = 20))#32/28
CSCALE_BLUE = (brewer.pal(9,"Blues"));
CSCALE_RdBu = rev(brewer.pal(11,"RdBu"));
CSCALE_PAIRED = rev(brewer.pal(12,"Paired"));
CSCALE_YlGnBu = rev(brewer.pal(9,"YlGnBu"));
CSCALE_BrBG = rev(brewer.pal(9,"BrBG"));
CSCALE_Set1 = (brewer.pal(9,"Set1"));
CSCALE_Pair = (brewer.pal(9,"Paired"));
CSCALE_Blues = (brewer.pal(9,"Blues"));
CSET_MODEL<-c(CSCALE_BLUE[1],CSCALE_BLUE[2],CSCALE_BLUE[3])
CSCALE_Accent <- brewer.pal(8,'Accent')
CSETS<-c("black","red")

# SSD & error
stp_ACC<-cbind(stp_ACC,stp_SSD[,-1]%>%setNames(paste0(names(.),"_SSD")))

quartz(width=5,height=4)
ggplot(stp_ACC, aes(x=DVm_SSD, y=DVm)) + 
  geom_hline(yintercept = 0.5,linetype="dashed")+
  geom_errorbar(aes(ymin=DVm-wseb, ymax=DVm+wseb),width=0,size=0.75,color="gray40")+
  geom_errorbarh(aes(xmin=DVm_SSD-wseb_SSD, xmax=DVm_SSD+wseb_SSD),height=0,size=0.75,color="gray40")+
  #geom_rect(aes(xmin=DVm_SSD-SSDm_wseb,xmax=DVm_SSD+SSDm_wseb,ymin=-Inf,ymax=Inf),fill="red",alpha=0.1) +
  geom_line(size=0.75,color="black") + geom_point(size=3.75,color="black") +
  coord_cartesian(ylim=c(0.15,0.65),xlim=c(105,550))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  scale_x_continuous(expand=c(0,10),breaks=seq(from=0,to=10000,by=100))+#seq(from=1,to=300,by=50),
  scale_color_manual(values=CSETS) +
  ylab("Stopping error (p)")+xlab("Stop-signal delay (ms)")+#ggtitle('Across All Positions')+
  theme(plot.title = element_text(size =20,face='bold'))+
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
      #axis.text.y = element_blank(),
      plot.title = element_blank(),
      strip.text = element_blank(),
      strip.background=element_blank())

# vincentize
quartz(width=4,height=4)
ggplot(bin_RT,aes(x=DVm,y=prob,group=interaction(STOP_L,ACC_L),fill=STOP_L,colour=STOP_L,xmin=DVm-se,xmax=DVm+se)) + 
  geom_line(size=0.75,color="gray40") +
  geom_point(size=4) +
  #coord_flip()+
  #coord_cartesian(ylim=c(470,520))+#coord_cartesian(ylim=c(90,98))+
  scale_color_manual(values=c("black",CSCALE_Blues[7]))+
  scale_fill_manual(values=c("black",CSCALE_Blues[7]))+
  #geom_ribbon(alpha=0.2,linetype=0)+#alpha=0.5
  geom_errorbarh(aes(xmin=DVm-wseb, xmax=DVm+wseb),height=0,size=0.8)+#,color="black"
  xlab("Response time (ms)")+ylab("Percentile")+#ggtitle('Switch')+
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin=unit(rep(1.2,4),"line"),
        axis.line=element_line(size=1),
        legend.key = element_blank(),
        #legend.key.size=unit(1,"lines"),
        legend.position = "none",
        legend.title=element_blank(),
        legend.text =element_text(family="Helvetica",size=8),
        axis.ticks.length = unit(0.5, "lines"),
        axis.title  = element_text(family="Helvetica",vjust=1.8),
        axis.text   = element_text(size=16),
        #axis.text.y = element_blank(),
        plot.title = element_blank(),
        strip.text = element_blank(),
        strip.background=element_blank())


# Conjunction bar graph
ds[,(c("TSCONJ_L")):=lapply(.SD, as.factor),.SDcols=c("TSCONJ_L")]
actDSi<-ds[STOP_L=="Go" & ACC==1,]%>%group_by(SUBID,TASK,TSCONJ_L,TSCONJ)%>%summarise(DV=mean(RT,na.rm=TRUE))
# actDSi<-ds[STOP_L=="Stop",]%>%group_by(SUBID,TASK,TSCONJ_L,TSCONJ)%>%summarise(DV=1-mean(ACC,na.rm=TRUE))

quartz(width=8.75,height=3.5)
ggplot(data=actDSi,aes(x=TSCONJ_L,y=DV,group=TSCONJ_L,fill=TSCONJ_L,color=TSCONJ_L)) +
  geom_hline(yintercept = 0.5,linetype="dashed")+
  geom_line(aes(x=TSCONJ_L,y=DV,group=SUBID),size=0.2,alpha=.2,color="gray40")+
  geom_point(position=position_jitter(width=.1),size=2,alpha=.8)+
  geom_boxplot(aes(x=as.numeric(TSCONJ_L)-0.42),width=.4,outlier.shape=12, alpha=0.5)+
  #Axis--------------------------------
  ylab("Response time (ms)")+xlab("")+#ggtitle("Response")+
  coord_cartesian(ylim=c(300,1100),xlim=c(0.75,12))+
  scale_y_continuous(breaks=seq(400,1500,200))+
  #ylab("Stopping error (p)")+xlab("")+#ggtitle("Response")+
  #coord_cartesian(ylim=c(0,0.8),xlim=c(0.75,12))+
  #scale_y_continuous(breaks=seq(0,1,0.2))+
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

