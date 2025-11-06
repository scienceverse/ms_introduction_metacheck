####PISA Script 2015 data####

#Warning: Running this script will require substantial disk space and will take ~ 6 hours depending on computational resources

####libraries (ensure these packages are installed prior to running)####
rm(list=ls())
cat("\014")

library(foreign)
library(MplusAutomation)
library(stringr)
library(reshape2)
library(ggplot2)
library(ggExtra)
library(ggrepel)
library(png)
library(tidyr)
library(ggpubr)
library(gridExtra)
library(grid)
library(yhat)

####Download Data####
#Note: This will download large files

download.file("http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip",destfile = "PISA 2015.zip")
download.file("http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CM2_STU_QQQ_COG_QTM_SCH_TCH.zip", destfile = "PISA 2015 Supplement.zip")
download.file("https://files.osf.io/v1/resources/patgwjqs26/providers/osfstorage/?zip=",destfile="osf_data.zip",mode='wb')
#Note: This will unzip the folder, making it even larger ~1.59GB
unzip("PISA 2015.zip")
unzip("PISA 2015 Supplement.zip")
unzip(unzip("osf_data.zip"),junkpaths=T)
#Read data, ADMINMODE implies mode of assessment
df<-subset(read.spss("CY6_MS_CMB_STU_QQQ.sav",use.value.labels = F,to.data.frame = T),
           select=c("ADMINMODE","CNTRYID","CNT","CNTSCHID","CNTSTUID","W_FSTUWT","STRATUM","ESCS",paste0("PV",1:10,"SCIE"),paste0("ST094Q0",1:5,"NA")))
df.supp<-subset(read.spss("CY6_MS_CM2_STU_QQQ.sav",use.value.labels = F,to.data.frame = T),
                select=c("ADMINMODE","CNTRYID","CNT","CNTSCHID","CNTSTUID","W_FSTUWT","STRATUM","ESCS",paste0("PV",1:10,"SCIE"),paste0("ST094Q0",1:5,"NA")))
df.gdp<-subset(read.csv("data_all_withallcountry indicators0508.csv"), select=c("CNTID","lgGDP_2006","lgGDP_2015"))

#Merge supplemental data
df.1<-rbind(df,df.supp)

#Remove old data.frames
rm(df)
rm(df.supp)

#Code subgroups to nations
df.1$CNTRYID[df.1$CNTRYID == 972]<-840
df.1$CNTRYID[df.1$CNTRYID == 973]<-840
df.1$CNTRYID[df.1$CNTRYID == 971]<-724
df.1$CNTRYID[df.1$CNTRYID == 974]<-32

#Drop all Albania Observations due to non-matching
df.1<-subset(df.1, df.1$CNTRYID != 8)
hist(df.1$ESCS)
#Code NEW Countries
newIDs<-c(12,970,188,214,268,411,422,807,470,498,
          604,630,702,780,784,972,973,704,398,458)

df.1$NewCNT<-ifelse(df.1$CNTRYID %in% newIDs,1,0)
df.pba<-subset(df.1,df.1$ADMINMODE==1 & df.1$NewCNT==0)
#calculate Mean score for science interest
df.1$INTm<-rowMeans(df.1[,19:23],na.rm=T)
hist(df.1$INTm)
#calculate country-specific average for interest and SES
SESCNT<-aggregate(ESCS ~ CNTRYID, df.1, mean)
INTmCNT<-aggregate(INTm ~ CNTRYID, df.1, mean)
names(SESCNT)[2]<-"SESCNT"
names(INTmCNT)[2]<-"INTmCNT"

#calculate school-specific average for SES
SESSCH<-aggregate(ESCS ~ CNTSCHID, df.1, mean)
names(SESSCH)[2]<-"SESSCH"

df.1<-merge(df.1,SESCNT,by="CNTRYID",all.x=T,all.y=F)
df.1<-merge(df.1,INTmCNT,by="CNTRYID",all.x=T,all.y=F)
df.1<-merge(df.1,SESSCH,by="CNTSCHID",all.x=T,all.y=F)

#merge GDP with individual-level data
df.1<-merge(df.1, df.gdp, by.x="CNTRYID",by.y="CNTID",all.x=T,all.y=F)

#create individual dataframes with each plausible value of science achievement
#save data for each plausible value
for (i in 1:10){
  prepareMplusData(subset(df.1,select=c("CNTRYID","CNTSCHID","CNTSTUID","W_FSTUWT","NewCNT","ESCS","INTm",paste0("PV",i,"SCIE"),"SESCNT","INTmCNT","SESSCH","lgGDP_2006","lgGDP_2015")), paste0("PISA",i,".dat"))
}
for (i in 1:10){
  prepareMplusData(subset(df.1,select=c("ADMINMODE","CNTRYID","CNTSCHID","CNTSTUID","W_FSTUWT","NewCNT","ESCS","INTm",paste0("PV",i,"SCIE"),"SESCNT","INTmCNT","SESSCH","lgGDP_2006","lgGDP_2015")), paste0("PISA",i,"CBA.dat"))
}

#create list of plausible value datafiles
write.table(c(paste0("PISA",1:10,".dat")),"PISA input.dat",col.names=F,row.names = F,quote=F)
write.table(c(paste0("PISA",1:10,"CBA.dat")),"PISA CBA input.dat",col.names=F,row.names = F,quote=F)
######################################
####PISA Script 2006 data####
######################################
rm(list=ls())
#Note: This will download a large file ~170MB
#The data is also in the osf_data file
unzip("INT_Stu06_Dec07.sav.zip")
df.06<-subset(read.spss("INT_Stu06_Dec07.sav",use.value.labels = F,to.data.frame = T),select=c("COUNTRY","SCHOOLID","STIDSTD","W_FSTUWT","ESCS",paste0("PV",1:5,"SCIE"),paste0("ST16Q0",1:5)))
###highlight countries not included in 2015##
oldIDs<-c(31,417,438,688)

df.06$OLDCNT<-ifelse(df.06$COUNTRY %in% oldIDs,1,0)
df.06<-subset(df.06, df.06$OLDCNT != 1)
df.06$OLDCNT<-NULL
df.06$COUNTRY<-as.numeric(df.06$COUNTRY)
df.06$STIDSTD<-(df.06$COUNTRY*10000)+as.numeric(df.06$STIDSTD)

#calculate Mean score for science interest, *-1, higher score means higher interest
df.06$INTm<-(rowMeans(df.06[,11:15],na.rm=T))*-1

#calculate country-specific average for interest and SES
SESCNT<-aggregate(ESCS ~ COUNTRY, df.06, mean)
INTmCNT<-(aggregate(INTm ~ COUNTRY, df.06, mean))
names(SESCNT)[2]<-"SESCNT"
names(INTmCNT)[2]<-"INTmCNT"

#calculate school-specific average for SES and interest
df.06$SCHOOLID2 <- (df.06$COUNTRY*10000)+as.numeric(df.06$SCHOOLID)
SESSCH<-aggregate(ESCS ~ SCHOOLID2, df.06, mean)
names(SESSCH)[2]<-"SESSCH"

df.06<-merge(df.06,SESCNT,by="COUNTRY",all.x=T,all.y=F)
df.06<-merge(df.06,INTmCNT,by="COUNTRY",all.x=T,all.y=F)
df.06<-merge(df.06,SESSCH,by="SCHOOLID2",all.x=T,all.y=F)

#read in country-level indicators
df.gdp06<-subset(read.csv("data_all_withallcountry indicators0508.csv"), select=c("CNTID","lgGDP_2006"))
names(df.gdp06)[1]<-"CNT"
#merge GDP with individual-level data
df.06<-merge(df.06, df.gdp06, by.x="COUNTRY",by.y="CNT",all.x=T,all.y=F)
#creating PBA countries's file from 2015
PBA06<-subset(df.06,df.06$COUNTRY%in%c(32,360,400,634,642))
#save data for each plausible va
for (i in 1:5){
  prepareMplusData(subset(df.06,select=c("COUNTRY","SCHOOLID2","STIDSTD","W_FSTUWT","ESCS","INTm",paste0("PV",i,"SCIE"),"SESCNT","INTmCNT","SESSCH","lgGDP_2006")), paste0("PISA06",i,".dat"),inpfile="PISA template.inp")
}
#create list of plausible value datafiles
write.table(c(paste0("PISA06",1:5,".dat")),"PISA06 input.dat",col.names=F,row.names = F,quote=F)

for (i in 1:5){
  prepareMplusData(subset(PBA06,select=c("COUNTRY","SCHOOLID2","STIDSTD","W_FSTUWT","ESCS","INTm",paste0("PV",i,"SCIE"),"SESCNT","INTmCNT","SESSCH","lgGDP_2006")), paste0("PISAPBA06",i,".dat"),inpfile="PISA template.inp")
}
#create list of plausible value datafiles
write.table(c(paste0("PISAPBA06",1:5,".dat")),"PISAPBA06 input.dat",col.names=F,row.names = F,quote=F)
####Analyses#####

#This runs all mplus scripts in the working directory, wll run all models used in manuscript
runModels()

#this saves the results of each model as a list
results<-readModels()

#then you can call the elements of the list to point out things

#######################################
###      Main results_2015    #########
#######################################
rm(list=ls())
modelname<-rbind(matrix(paste0(c("53m"),rep(1:3,each=2),c("(p)","(us)"),"15.out")),
                 matrix(paste0("53m1",c("(p)","(us)"),"g06.out")),
                 matrix(paste0("72m",rep(1:3,each=2),c("(p)","(us)"),"15.out")))
df.model15<-data.frame()
for(i in 1:nrow(modelname)){
  std.i<-(readModels(modelname[i]))$parameters$unstandardized
  std.i$model<-sub("15.out","",modelname[i])
  df.model15<-rbind(df.model15,std.i)
}
df.model15<-df.model15[!(df.model15$paramHeader=="Residual.Variances"),]
model15_r<-reshape(subset(df.model15,select=c("param","est","se","model")),idvar="model",timevar="param",direction="wide")
row.names(model15_r)<-model15_r[,1]
write.csv(model15_r[2:ncol(model15_r)],file="model.all2015.csv")

##################################
###  multiple countries  #########
##################################
modelnamem<-matrix(paste0(rep(c(53,72),each=2),"multiple",c("(p)","(us)"),"15.out"))
df.multiple15<-data.frame()

for(j in 1:nrow(modelnamem)){
  std.j<-(readModels(modelnamem[j]))$parameters$unstandardized
  std.j$model<-sub("15.out","",modelnamem[j])
  df.multiple15<-rbind(df.multiple15,std.j)
}
df.multiple15<-df.multiple15[!(df.multiple15$paramHeader=="Residual.Variances"),]
df.multiple15<-subset(df.multiple15,select=c("param","est","se","Group","model"))


multiple53p<-subset(df.multiple15[df.multiple15$model%in% c('53multiple(p)'),],select=c("param","est","se","Group"))
multiple53p_r<-reshape(multiple53p,idvar="Group",timevar="param",direction="wide")
multiple53p<-NULL
names(multiple53p_r)<-c("Group","ZINT53p","seZINT53p","ZSES53p","seZSES53p","ZINTSES53p","seZINTSES53p","ZSCI53p","seZSCI53p" )

multiple53us<-subset(df.multiple15[df.multiple15$model%in% c('53multiple(us)'),],select=c("param","est","se","Group"))
multiple53us_r<-reshape(multiple53us,idvar="Group",timevar="param",direction="wide")
multiple53us<-NULL
names(multiple53us_r)<-c("Group","ZINT53us","seZINT53us","ZSES53us","seZSES53us","ZINTSES53us","seZINTSES53us","ZSCI53us","seZSCI53us" )

multiple72p<-subset(df.multiple15[df.multiple15$model%in% c('72multiple(p)'),],select=c("param","est","se","Group"))
multiple72p_r<-reshape(multiple72p,idvar="Group",timevar="param",direction="wide")
multiple72p<-NULL
names(multiple72p_r)<-c("Group","ZINT72p","seZINT72p","ZSES72p","seZSES72p","ZINTSES72p","seZINTSES72p","ZSCI72p","seZSCI72p" )

multiple72us<-subset(df.multiple15[df.multiple15$model%in% c('72multiple(us)'),],select=c("param","est","se","Group"))
multiple72us_r<-reshape(multiple72us,idvar="Group",timevar="param",direction="wide")
multiple72us<-NULL
names(multiple72us_r)<-c("Group","ZINT72us","seZINT72us","ZSES72us","seZSES72us","ZINTSES72us","seZINTSES72us","ZSCI72us","seZSCI72us" )
###country name###
CNT<-data.frame("name"=c("ARGENTINA","AUSTRALIA","AUSTRIA","BELGIUM","BRAZIL","BULGARIA","CANADA","CHILE","CHINESETAIPEI","COLOMBIA","CROATIA",
                         "CZECHREPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HONGKONGCHINA","HUNGARY","ICELAND","INDONESIA",
                         "IRELAND","ISRAEL","ITALY","JAPAN","JORDAN","KOREA","LATVIA","LITHUANIA","LUXEMBOURG","MACAOCHINA","MEXICO","MONTENEGRO",
                         "NETHERLANDS","NEWZEALAND","NORWAY","POLAND","PORTUGAL","QATAR","ROMANIA","RUSSIANFEDERATION","SLOVAKREPUBLIC",
                         "SLOVENIA","SPAIN","SWEDEN","SWITZERLAND","THAILAND","TUNISIA","TURKEY","UNITEDKINGDOM","UNITEDSTATES","URUGUAY",
                         "ALGERIA","BSJG","COSTARICA","DOMINICANREPUBLIC","GEORGIA","KOSOVO","LEBANON","MACEDONIA","MALTA","MOLDOVA",
                         "PERU","PUERTORICO","SINGAPORE","TRINIDADTOBAGO","UNITEDARABEMIRATES","VIETNAM","KAZAKHSTAN","MALAYSIA"),
                "CNT"=c(32,36,40,56,76,100,124,152,158,170,191,203,208,233,246,250,276,300,344,348,352,360,372,376,380,392,400,410,
                        428,440,442,446,484,499,528,554,578,616,620,634,642,643,703,705,724,752,756,764,788,792,826,840,858,12,970,
                        188,214,268,411,422,807,470,498,604,630,702,780,784,704,398,458))

multiple15<-merge(merge(multiple53p_r,multiple53us_r,by = "Group"),merge(multiple72p_r,multiple72us_r,by="Group"),all.y=T)
multiple15<-merge(multiple15,CNT,by.x="Group",by.y="name",all.y=T)
df.indicator<-subset(read.csv("data_all_withallcountry indicators0508.csv"),select=c("CNTID","Code","region_o","region","region_s","lgGDP_2015",
                                                                                     "Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015",
                                                                                     "SocCoh_2015","EduExp_2015","AdolFert_2015","Internet_2006","Internet_2015",
                                                                                     "lgGDP_2006","Gini_2006","Demo_2006","RnD_2006","SocJust_2006",
                                                                                     "AccEdu_2006","SocCoh_2006","EduExp_2006","AdolFert_2006"))
df.indicator[df.indicator == "#N/A"]<-NA
multiple15.all<-merge(multiple15,df.indicator,by.x="CNT",by.y="CNTID",all.y=T)
row.names(multiple15.all)<-multiple15.all[,1]
names(multiple15.all[2])<-c("CNTID")
write.csv(multiple15.all[2:ncol(multiple15.all)],file="multiple15_0521.csv")



#######################################
###      Main results_2006    #########
#######################################
modelname06<-matrix(paste0("m",rep(1:3,each=2),c("(p)","(us)"),"06.out"))
df.model06<-data.frame()
for(i in 1:nrow(modelname06)){
  std.i<-(readModels(modelname06[i]))$parameters$unstandardized
  std.i$model<-sub("06.out","",modelname06[i])
  df.model06<-rbind(df.model06,std.i)
}
df.model06<-df.model06[!(df.model06$paramHeader=="Residual.Variances"),]
model06_r<-reshape(subset(df.model06,select=c("param","est","se","model")),idvar="model",timevar="param",direction="wide")
row.names(model06_r)<-model06_r[,1]
write.csv(model06_r[2:ncol(model06_r)],file="model.all2006.csv")

##################################
###  multiple countries  #########
##################################

std.multp06<-(readModels("multiple(p)06.out"))$parameters$unstandardized
std.multp06$model<-c("multp06")
std.multus06<-(readModels("multiple(us)06.out"))$parameters$unstandardized
std.multus06$model<-c("multus06")
df.multiple06<-subset(rbind(std.multp06,std.multus06),select=c("paramHeader","param","est","se","Group","model"))
std.multp06<-NULL
std.multus06<-NULL

df.multiple06<-df.multiple06[!(df.multiple06$paramHeader=="Residual.Variances"),]
df.multiple06$param[df.multiple06$param == "ZSCI"]<-"ZSCI"
multiplep06<-subset(df.multiple06[df.multiple06$model%in% c('multp06'),],select=c("param","est","se","Group"))
multiplep06_r<-reshape(multiplep06,idvar="Group",timevar="param",direction="wide")
multiplep06<-NULL
names(multiplep06_r)<-c("Group","ZINTp06","seZINTp06","ZSESp06","seZSESp06","ZINTSESp06","seZINTSESp06","ZSCIp06","seZSCIp06" )

multipleus06<-subset(df.multiple06[df.multiple06$model%in% c('multus06'),],select=c("param","est","se","Group"))
multipleus06_r<-reshape(multipleus06,idvar="Group",timevar="param",direction="wide")
multipleus06<-NULL
names(multipleus06_r)<-c("Group","ZINTus06","seZINTus06","ZSESus06","seZSESus06","ZINTSESus06","seZINTSESus06","ZSCIus06","seZSCIus06" )

###country name###

CNT<-data.frame("name"=c("ARGENTINA","AUSTRALIA","AUSTRIA","BELGIUM","BRAZIL","BULGARIA","CANADA","CHILE","CHINESETAIPEI","COLOMBIA","CROATIA",
                         "CZECHREPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HONGKONGCHINA","HUNGARY","ICELAND","INDONESIA",
                         "IRELAND","ISRAEL","ITALY","JAPAN","JORDAN","KOREA","LATVIA","LITHUANIA","LUXEMBOURG","MACAOCHINA","MEXICO","MONTENEGRO",
                         "NETHERLANDS","NEWZEALAND","NORWAY","POLAND","PORTUGAL","QATAR","ROMANIA","RUSSIANFEDERATION","SLOVAKREPUBLIC",
                         "SLOVENIA","SPAIN","SWEDEN","SWITZERLAND","THAILAND","TUNISIA","TURKEY","UNITEDKINGDOM","UNITEDSTATES","URUGUAY"),
                "CNT"=c(32,36,40,56,76,100,124,152,158,170,191,203,208,233,246,250,276,300,344,348,352,360,372,376,380,392,400,410,428,440,
                        442,446,484,499,528,554,578,616,620,634,642,643,703,705,724,752,756,764,788,792,826,840,858))


multiple06<-merge(multiplep06_r,multipleus06_r,by = "Group",all.y=T)
multiple06<-merge(multiple06,CNT,by.x="Group",by.y="name",all.y=T)
multiple06$Group<-NULL
multiple0615<-merge(multiple15,multiple06,by.x="CNT",by.y="CNT",all.x=T)
multiple0615<-merge(multiple0615,df.indicator,by.x="CNT",by.y="CNTID",all.y=T)
newIDs<-c(12,970,188,214,268,411,422,807,470,498,604,630,702,780,784,704,398,458)
multiple0615$newCNT<-ifelse(multiple0615$CNT %in% newIDs,1,0)
multiple0615$zLogGDP<-(multiple0615$lgGDP_2015-mean(multiple0615$lgGDP_2015,na.rm=TRUE))/sd(multiple0615$lgGDP_2015,na.rm=TRUE)
multiple0615$zLogGDP2006<-(multiple0615$lgGDP_2006-mean(multiple0615$lgGDP_2006,na.rm=TRUE))/sd(multiple0615$lgGDP_2006,na.rm=TRUE)
multiple0615$diff_INT<-multiple0615$ZINT53us-multiple0615$ZINTus06
multiple0615[multiple0615 == "#N/A"]<-NA
write.csv(multiple0615,file="multiple0615_0521.csv")


################graph#############
#############9x9 grid#############
rm(list=ls())
cat("\014") 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



####9 X 9 grid####
data<-seq(from=-2,to=2,by=0.1)
ngraph<-subset(read.csv('multiple0615_0521.csv',TRUE,','),select=c("CNT","Group","ZINT72us","ZSES72us",
                                                                   "ZINTSES72us","ZSCI72us"))
ngraph<-subset(ngraph, CNT %in% c(348,410,422,442,484,604,826,840,970))
names(ngraph)<-c("CNTID","Group","INTus","SESus","INTSESus","ICus")
graphlabel<-data.frame("id"=c(1:9),"name"=c("Hungary","Korea","Lebanon","Luxembourg",
                                            "Mexico","Peru","United Kingdom","United States",
                                            "B-S-J-G"))
plots<-list()
for (i in 1:9){
  min2SE_i<-ngraph$ICus[i]+data*ngraph$INTus[i]+(-2)*ngraph$SESus[i]+(-2)*data*ngraph$INTSESus[i]
  zeroSE_i<-ngraph$ICus[i]+data*ngraph$INTus[i]
  max2SE_i<-ngraph$ICus[i]+data*ngraph$INTus[i]+(2)*ngraph$SESus[i]+(2)*data*ngraph$INTSESus[i]
  regression_i<-as.data.frame(cbind(data,min2SE_i,zeroSE_i,max2SE_i))
  names(regression_i)<-c('science interest','high family SES','average family SES','low family SES')
  r_i<-melt(regression_i,id='science interest')
  names(r_i)<-c('Science_Interest','SES','Science_Achievement')
  if(ngraph$CNTID[i]==422){
    graphi<-ggplot(r_i,aes(Science_Interest,Science_Achievement,colour=SES,linetype=SES))+geom_line(size=5)+theme_bw()+
      theme(axis.ticks = element_line(size = 6),axis.title.x=element_blank(),axis.text=element_blank(),axis.title.y=element_blank(),axis.ticks.length=unit(0.3,"cm"),
            legend.text=element_text(size=45),legend.title =element_text(size=55),legend.key.size=unit(2,'cm'),
            legend.position=c(0.3,0.65),legend.box = "horizontal",axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(colour="black",size=4))+
      annotate("text",x=0,y=1.8,label="Lebanon",size=26)+scale_x_continuous(sec.axis=dup_axis())+scale_y_continuous(limits=c(-2,2),breaks=seq(-2,2,by=0.5))+
      scale_fill_discrete(name='SES',labels=c("High Family SES","Average Family SES","Low Family SES"))
  }
  else
  {graphi<-ggplot(r_i,aes(Science_Interest,Science_Achievement,colour=SES,linetype=SES))+geom_line(size=5)+theme_bw()+
    theme(axis.ticks = element_line(size = 6),axis.title.x=element_blank(),axis.text=element_blank(),axis.title.y=element_blank(),legend.position='none',axis.line = element_line(colour = "black"),
          axis.ticks.length=unit(0.4,"cm"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(colour="black",size=4))+
    annotate("text",x=0,y=1.8,label=subset(graphlabel,id==i)$name,size=26)+scale_x_continuous(sec.axis=dup_axis())+scale_y_continuous(limits=c(-2,2),breaks=seq(-2,2,by=0.5))
  print(graphi)
  }
  ggsave(graphi,file=paste0(graphlabel$name[i],".png"),width=22,height=18,units=c("cm"))
  plots[[i]]<-graphi
}
layout<-matrix(c(2,9,6,7,8,5,3,4,1),nrow=3,byrow=F)
png(filename="multi.png",width=15000,height=11000,units="px",res=300)
multiplot(plotlist=plots,layout=layout) 
dev.off()

###################
#Figure 2&3########

#Interest & GDP#
rm(list=ls())
cat("\014") 

data72<-subset(read.csv('multiple0615_0521.csv',header=TRUE,sep=','),
               select=c("CNT","Group","Code","newCNT","region","ZINTus06","ZSESus06","ZINTSESus06",
                        "ZINT53us","ZSES53us","ZINTSES53us","ZINT72us","zLogGDP","zLogGDP2006"))
data53<-subset(data72, data72$newCNT != 1)

####################
#######Figure1######
####################

#Interest & GDP_2006#  
figure3<-ggplot(data53,aes(x=zLogGDP2006,y=ZINTus06,label=Code))+geom_point(aes(color=region),size=4)+geom_smooth(aes(group=1),method='lm')+stat_smooth(method='lm',fullrange=TRUE)+
  geom_label_repel(alpha=0.7,aes(fill=region),segment.alpha=.35,fontface = 'bold', size=5.5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey20')+theme_classic(base_size=10)+
  xlab('LogGDP(SD Units)')+ylab('Association Between Science Interest and Science Achievement')+
  ggtitle("A")+theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     legend.key.size=unit(0.9,'cm'),legend.position="none",
                     text=element_text(size=15),axis.text=element_text(size=25),axis.title=element_text(size=22))+
  scale_x_continuous(expand=c(0,0),limits=c(-3,3),breaks=seq(-2.5,2.5,by=1))+
  scale_y_continuous(limits=c(-0.1,0.5), breaks=seq(-0.1,0.5,by=0.1))+
  annotate("text",label="2006",x=2.5,y=-0.1,size=10,colour="black")+annotate("text",label="r=.68",x=2.5,y=-0.07,size=10,colour="black")
plot(figure3)


#Interest & GDP_2015#   
figure4<-ggplot(data53,aes(x=zLogGDP,y=ZINT53us,label=Code))+geom_point(aes(color=region),size=4)+geom_smooth(aes(group=1),method='lm')+stat_smooth(method='lm',fullrange=TRUE)+
  geom_label_repel(alpha=0.7,aes(fill=region),segment.alpha=.35,fontface = 'bold', size=5.5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey20')+theme_classic(base_size=10)+
  xlab('LogGDP(SD Units)')+ylab('Association between Science Interest and Science Achievement')+
  ggtitle("B")+theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     legend.key.size=unit(0.9,'cm'),text=element_text(size=15),axis.text=element_text(size=25),axis.title=element_text(size=22),legend.position="none")+
  scale_x_continuous(expand=c(0,0),limits=c(-3,3),breaks=seq(-2.5,2.5,by=1))+
  scale_y_continuous(limits=c(-0.1,0.5), breaks=seq(-0.1,0.5,by=0.1))+
  annotate("text",label="2015",x=2.5,y=-0.1,size=10,colour="black")+annotate("text",label="r=.76",x=2.5,y=-0.07,size=10,colour="black")
plot(figure4)



#Interest & GDP_2015all#
#Note: The World Bank does not report GDP for Taiwan and is therefore coded as missing. This produces a warning from ggplot, but is expected.
figure5<-ggplot(data72,aes(x=zLogGDP,y=ZINT72us,label=Code))+geom_point(aes(color=region),size=4)+geom_smooth(aes(group=1),method='lm')+stat_smooth(method='lm',fullrange=TRUE)+
  geom_label_repel(alpha=0.7,aes(fill=region),segment.alpha=.35,fontface = 'bold', size=5.5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey20')+theme_classic(base_size=10)+
  xlab('LogGDP(SD Units)')+ylab('Association between Science Interest and Science Achievement')+
  ggtitle("C")+theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     legend.key.size=unit(1.2,'cm'),legend.position=c(0.4,0.12),legend.direction="horizontal",text=element_text(size=25),axis.text=element_text(size=25),axis.title=element_text(size=22))+
  scale_fill_discrete(name='Region',breaks=c("Central/Western Asia","Eastern Asia","Eastern/Southern Europe/Africa","North/Western Europe","Latin/South America","North America","Oceania"),
                      labels=c("Central/Western Asia","Eastern Asia","Eastern/Southern Europe/Africa","North/Western Europe","Latin/South America","North America","Oceania"))+
  scale_x_continuous(expand=c(0,0),limits=c(-3,3),breaks=seq(-2.5,2.5,by=1))+
  scale_y_continuous(limits=c(-0.1,0.5), breaks=seq(-0.1,0.5,by=0.1))+annotate("text",label="PISA 2015",x=2.5,y=-0.1,size=10,colour="black")+annotate("text",label="r=.58",x=2.5,y=-0.07,size=10,colour="black")+
  scale_color_discrete(guide=FALSE)
plot(figure5)
figure1_m<-ggarrange(ggarrange(figure3,figure4,ncol=2,nrow=1),figure5,ncol=1,nrow=2)
ggsave("figure1_m.png",width=50,height=50,units="cm",dpi=600)
dev.off()

####################
###Figure 2#########
####################

#Interest & interest_2015#
figure0<-ggplot(data53,aes(x=ZINTus06,y=ZINT53us,label=Code,fill=region,color=region))+geom_point(size=5)+
  geom_label_repel(alpha=0.7,segment.alpha=.35,fontface = 'bold', size=5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey20',seed=1234)+theme_classic(base_size=10)+
  xlab('Science Interest Effect Size in 2006')+ylab('Science Interest Effect Size in 2015')+
  ggtitle("A")+theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     text=element_text(size=20),axis.text=element_text(size=30),axis.title=element_text(size=30,face="bold"))+geom_abline(slope=1,intercept=0)+
  scale_x_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+
  scale_y_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+annotate("text",label="r=.82",x=0.45,y=0,size=10,colour="black")+
  scale_fill_discrete(name='Region',breaks=c("Central/Western Asia","Eastern Asia","Eastern/Southern Europe/Africa","North/Western Europe","Latin/South America","North America","Oceania"),
                      labels=c("Central/Western Asia","Eastern Asia","Eastern/Southern Europe/Africa","North/Western Europe","Latin/South America","North America","Oceania"))+
  scale_color_discrete(guide=FALSE)+theme(legend.position="none")

################
#SES & SES_2015#
################
figure1<-ggplot(data53,aes(x=ZSESus06,y=ZSES53us,label=Code,fill=region,color=region))+geom_point(size=5)+
  geom_label_repel(alpha=0.7,segment.alpha=.35,fontface = 'bold', size=5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')+theme_classic(base_size=10)+
  xlab('Family SES Effect Size in 2006')+ylab('Family SES Effect Size in 2015')+geom_abline(slope=1,intercept=0)+
  ggtitle("B")+theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     text=element_text(size=20),axis.text=element_text(size=30),axis.title=element_text(size=30,face="bold"),legend.position="none")+geom_abline(slope=1,intercept=0)+
  scale_x_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+
  scale_y_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+annotate("text",label="r=.83",x=0.45,y=0,size=10,colour="black")
figure1  
#Interaction & Interaction_2015#
figure2.1<-ggplot(data53,aes(x=ZINTSESus06,y=ZINTSES53us,label=Code,fill=region,color=region))+geom_point(size=5)+theme_classic(base_size=10)+
  xlab('Science Interest x SES Effect Size in 2006')+ylab('Science Interest x SES Effect Size in 2015')+
  ggtitle("C")+theme(legend.position="none",plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
                     text=element_text(size=20),axis.text=element_text(size=30),axis.title=element_text(size=30,face="bold"))+geom_abline(slope=1,intercept=0)+
  scale_x_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+
  scale_y_continuous(limits=c(-0.1,0.5),breaks=seq(-0.1,0.5,by=0.1))+annotate("text",label="r=.20",x=0.45,y=0,size=10,colour="black")
figure2.1


###zoom in###
zoomtheme <- theme(legend.position="none", axis.line=element_blank(),axis.text.x=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.x=element_blank(),axis.title.y=element_blank(),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_rect(color='red', fill="white"),
                   plot.margin = unit(c(0,0,-6,-6),"mm"))


# zoomed plot
p.zoom <- ggplot(data53, aes(x = ZINTSESus06, y = ZINTSES53us, label=Code,fill=region,color=region)) +
  geom_point(size=6.5)+geom_label_repel(alpha=0.8,segment.alpha=.35,fontface = 'bold', size=5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  theme_classic(base_size=10)+coord_cartesian(xlim=c(min(data53$ZINTSESus06),max(data53$ZINTSESus06)),
                                              ylim=c(min(data53$ZINTSES53us),max(data53$ZINTSES53us)))+zoomtheme 
p.zoom
# put them together
g <- ggplotGrob(p.zoom)
plot.final <- figure2.1 + annotation_custom(grob = g, xmin = .07, xmax = .52, ymin = .07, ymax = .52)
plot.final
ggarrange(figure0,figure1,plot.final,ncol=3)
ggsave("figure2_m.png",width=90,height=30,units="cm",dpi=600,limitsize=FALSE)
dev.off()

###########################################
###########Supplementary table#############
###########################################


######################
####### S1-3 #########
######################
multi0615s<-read.csv("multiple0615_0521.csv",header=T)[c(20:79,2,3)]
multi0615<-data.frame(matrix(ncol=16,nrow=71))
for(i in 1:16){
  ci.i<-matrix(paste0("[",round((multi0615s[,2*i-1]-1.96*multi0615s[,2*i]),3),",",round((multi0615s[,2*i-1]+1.96*multi0615s[,2*i]),3),"]"))
  multi0615[,i]<-ci.i
}
colnames(multi0615)<-paste0("ci.",colnames(multi0615s)[seq(1,32,2)])
multi0615s<-cbind(multi0615s,multi0615)
multi0615[multi0615 == "[NA,NA]"]<-NA
multi0615.s1<-subset(multi0615s,select=c("Group","Code","lgGDP_2015","Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015","SocCoh_2015","EduExp_2015","AdolFert_2015","Internet_2015"))
multi0615.s2s3<-subset(multi0615s,select=c("CNT","Group","ZINT72p","ci.ZINT72p","ZSES72p","ci.ZSES72p","ZINTSES72p","ci.ZINTSES72p","ZSCI72p","ci.ZSCI72p",
                                           "ZINTp06","ci.ZINTp06","ZSESp06","ci.ZSESp06","ZINTSESp06","ci.ZINTSESp06","ZSCIp06","ci.ZSCIp06",
                                           "ZINT72us","ci.ZINT72us","ZSES72us","ci.ZSES72us","ZINTSES72us","ci.ZINTSES72us","ZSCI72us","ci.ZSCI72us",
                                           "ZINTus06","ci.ZINTus06","ZSESus06","ci.ZSESus06","ZINTSESus06","ci.ZINTSESus06","ZSCIus06","ci.ZSCIus06"))
write.csv(multi0615.s1,file="tables1_0525.csv")
write.csv(multi0615.s2s3,file="tables2s3_0525.csv")

######################
######## S4 ##########
######################

rm(list=ls())
cat("\014") 

df.mall15<-read.csv("model.all2015.csv",header=T)
df15<-data.frame(matrix(ncol=12,nrow=14))
for(i in 1:12){
  ci.i<-matrix(paste0("[",round((df.mall15[,2*i]-1.96*df.mall15[,2*i+1]),3),",",round((df.mall15[,2*i]+1.96*df.mall15[,2*i+1]),3),"]"))
  df15[,i]<-ci.i
}
colnames(df15)<-paste0("ci.",sub("est.","",colnames(df.mall15)[seq(2,25,2)]))
df15[df15 == "[NA,NA]"]<-NA
df.mall15<-cbind(df.mall15,df15)
df.mall15.report<-subset(df.mall15,select=c("est.ZSES","ci.ZSES","est.ZSESSCH","ci.ZSESSCH","est.ZGDP15","ci.ZGDP15","est.ZINT","ci.ZINT","est.ZSESSQ","ci.ZSESSQ","est.ZINTSQ","ci.ZINTSQ",
                                            "est.ZINTSES","ci.ZINTSES","est.ZINTSESSCH","ci.ZINTSESSCH","est.ZINTGDP15","ci.ZINTGDP15"))
rownames(df.mall15.report)<-df.mall15$X


df.mall06<-read.csv("model.all2006.csv",header=T)
df06<-data.frame(matrix(ncol=10,nrow=6))
for(i in 1:10){
  ci.i<-matrix(paste0("[",round((df.mall06[,2*i]-1.96*df.mall06[,2*i+1]),3),",",round((df.mall06[,2*i]+1.96*df.mall06[,2*i+1]),3),"]"))
  df06[,i]<-ci.i
}
colnames(df06)<-paste0("ci.",sub("est.","",colnames(df.mall06)[seq(2,21,2)]))
df06[df06 == "[NA,NA]"]<-NA
df.mall06<-cbind(df.mall06,df06)
df.mall06.report<-subset(df.mall06,select=c("est.ZSES","ci.ZSES","est.ZSESSCH","ci.ZSESSCH","est.ZGDP06","ci.ZGDP06","est.ZINT","ci.ZINT","est.ZSESSQ","ci.ZSESSQ","est.ZINTSQ","ci.ZINTSQ",
                                            "est.ZINTSES","ci.ZINTSES","est.ZINTSESSCH","ci.ZINTSESSCH","est.ZINTGDP06","ci.ZINTGDP06"))
rownames(df.mall06.report)<-df.mall06$X
write.csv(df.mall15.report,file="table1_2015.csv")
write.csv(df.mall06.report,file="table1_2006.csv")


############################################
######## FS2 Change in interest& GDP #######
############################################
rm(list=ls())
cat("\014")
datafs2<-read.csv('multiple0615_0521.csv',header=T,sep=",")[2:79]
datafs2<-subset(datafs2, datafs2$newCNT != 1)

figuredif<-ggplot(datafs2,aes(x=zLogGDP,y=diff_INT,label=Code))+geom_point(aes(color=region),size=4)+geom_smooth(aes(group=1),method='lm')+stat_smooth(method='lm',fullrange=TRUE)+
  geom_label_repel(alpha=0.7,aes(fill=region),segment.alpha=.35,fontface = 'bold', size=5.5,color = 'white',box.padding = 0.35, point.padding = 0.5,segment.color = 'grey20')+theme_classic(base_size=10)+
  xlab('LogGDP(SD Units)')+ylab('Change in Association between Science Interest and Science Achievement')+
  theme(plot.title=element_text(size=30,hjust=0.5),axis.line.x = element_line(colour = "black",size=2),axis.line.y = element_line(colour = "black",size=2),
        legend.key.size=unit(1,'cm'),text=element_text(size=14),axis.text=element_text(size=25),axis.title=element_text(size=20),
        legend.position=c(0.14,0.16))+
  scale_fill_discrete(name='Region',breaks=c("Central/Western Asia","Eastern Asia","Eastern/Southern Europe/Africa","North/Western Europe","Latin/South America","North America","Oceania"))+
  scale_x_continuous(expand=c(0,0),limits=c(-3,3),breaks=seq(-2.5,2.5,by=1))+
  scale_y_continuous(limits=c(-0.15,0.25), breaks=seq(-0.15,0.25,by=0.1))+
  annotate("text",label="2015",x=2.5,y=-0.1,size=8,colour="black")+annotate("text",label="r=-.45",x=2.5,y=-0.13,size=8,colour="black")+
  scale_color_discrete(guide=FALSE)
plot(figuredif)
ggsave("figuresINTdif.png",width=30,height=27,units="cm",dpi=600)

############################################
######## TS5 Commonality analysis ##########
############################################
datas5<-subset(read.csv("multiple0615_0521.csv",header=T),
               select=c("Group","ZINT72p","lgGDP_2015","Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015","SocCoh_2015",
                        "EduExp_2015","AdolFert_2015","Internet_2015"))
#Commonality
lms1 <- lapply(colnames(datas5)[4:12],function(y){
  form<-paste0("ZINT72p~",y,"+lgGDP_2015")
  lm(form,data=datas5)
})
reglms1<-sapply(lms1,function(x){
  regr(x)
})
lms2 <- lapply(colnames(datas5)[4:12],function(y){
  form2<-paste0("ZINT72p~",y)
  lm(form2,data=datas5)
})
reglms2<-sapply(lms2,function(x){
  regr(x)
})
r1<-as.data.frame(t(as.data.frame(reglms1["Beta_Weights",])))
rownames(r1)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015","SocCoh_2015","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(r1)<-c("Betav","BetaGDP")

r2<-as.data.frame(t(as.data.frame(reglms2["Beta_Weights",])))
rownames(r2)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2006","AccEdu_2006","SocCoh_2006","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(r2)<-c("Betavonly")
###commonality
r3<-reglms1["Commonality_Data",]
com<-data.frame(matrix(ncol=4,nrow=9))
rownames(com)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015","SocCoh_2015","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(com)<-c("unique to V","unique to GDP","Common","total")
for(i in 1:9){
  r3_i<-r3[[i]]$CC
  com[i,]<-as.data.frame(as.data.frame(r3[[i]]$CC))[,1]
}
commons5<-cbind(r1,r2,com)
### adjusted for OECD country ###
oecd<-datas5[,c(2:3,7:9)]
oecd<-subset(oecd,(!is.na(oecd[,3])))
reglm_oecd<-regr(lm(ZINT72p~lgGDP_2015,oecd))$Beta_Weights
reglm_all<-regr(lm(ZINT72p~lgGDP_2015,datas5))$Beta_Weights
correctindex<-reglm_all^2/reglm_oecd^2

commons5[c(4:6),c(4:6)]<-commons5[c(4:6),c(4:6)]*correctindex
commons5$percent<-commons5$`unique to V`/commons5$total
write.csv(commons5,"tables5.csv")

################################################
######## TS6 Commonality_change score ##########
################################################
datas6<-subset(read.csv("multiple0615_0521.csv",header=T),
               select=c("Group","ZINTus06","ZINT53us","ZINTp06","ZINT53p","diff_INT","lgGDP_2015","Gini_2015","Demo_2015","RnD_2015","SocJust_2015","AccEdu_2015","SocCoh_2015","EduExp_2015","AdolFert_2015",
                        "Internet_2015","lgGDP_2006","Gini_2006","Demo_2006","RnD_2006","SocJust_2006","AccEdu_2006","SocCoh_2006","EduExp_2006","AdolFert_2006","Internet_2006","newCNT"))
datas6<-subset(datas6,datas6$newCNT!=1)
data15<-datas6[7:16]
data06<-datas6[17:26]
datas6$diff_INTp<-datas6$ZINT53p-datas6$ZINTp06
#Commonality
datacommons6<-datas6[,c(4:5,7:16,28)]
#Commonality
lms1 <- lapply(colnames(datacommons6)[4:12],function(y){
  form<-paste0("diff_INTp~",y,"+lgGDP_2015")
  lm(form,data=datacommons6)
})
reglms1<-sapply(lms1,function(x){
  regr(x)
})
lms2 <- lapply(colnames(datacommons6)[4:12],function(y){
  form2<-paste0("diff_INTp~",y)
  lm(form2,data=datacommons6)
})
reglms2<-sapply(lms2,function(x){
  regr(x)
})
r1<-as.data.frame(t(as.data.frame(reglms1["Beta_Weights",])))
rownames(r1)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2006","AccEdu_2006","SocCoh_2006","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(r1)<-c("Betav","BetaGDP")

r2<-as.data.frame(t(as.data.frame(reglms2["Beta_Weights",])))
rownames(r2)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2006","AccEdu_2006","SocCoh_2006","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(r2)<-c("Betavonly")
###commonality
r3<-reglms1["Commonality_Data",]
com<-data.frame(matrix(ncol=4,nrow=9))
rownames(com)<-c("Gini_2015","Demo_2015","RnD_2015","SocJust_2006","AccEdu_2006","SocCoh_2006","EduExp_2015","AdolFert_2015","Internet_2015")
colnames(com)<-c("unique to V","unique to GDP","Common","total")
for(i in 1:9){
  r3_i<-r3[[i]]$CC
  com[i,]<-as.data.frame(as.data.frame(r3[[i]]$CC))[,1]
}
common<-cbind(r1,r2,com)
### adjusted for OECD country ###
oecd<-datas5[,c(4:5,28,7,11:13)]
oecd<-subset(oecd,(!is.na(oecd[,5])))
reglm_oecd<-regr(lm(diff_INTp~lgGDP_2015,oecd))$Beta_Weights
reglm_all<-regr(lm(diff_INTp~lgGDP_2015,datacommons6))$Beta_Weights
correctindex<-reglm_all^2/reglm_oecd^2

common[c(4:6),c(4:6)]<-common[c(4:6),c(4:6)]*correctindex
common$percent<-common$`unique to V`/common$total
write.csv(common,"tables6.csv")

############################################
######## TS7 Correlation among Vs ##########
############################################
cat("\014")

data15_06<-data15-data06
colnames(data15_06)<-paste0(sub("_2015","",colnames(data15_06)),"_c")
datacorr<-cbind(datas6[,c(1:3,6:16)],data15_06)

cor1<-cor(datacorr[2:14],use="pairwise.complete.obs")[4:13,1:3]
cor2<-cor(datacorr[,c(2:4,15:24)],use="pairwise.complete.obs")[4:13,1:3]
corrs5<-rbind(cor1,cor2)
write.csv(corrs5,"tables7.csv")
GDPcor<-cor(cbind(data15[1],data06[1]),use="pairwise.complete.obs")

##Attentuation
dataall<-subset(read.csv('multiple0615_0521.csv',header=T,sep=",")[2:79],select=c("CNT","ZINT72p","lgGDP_2015"))
datapba<-subset(dataall, dataall$CNT %in% c(32,360,400,634,642))
datacba<-subset(dataall, !(dataall$CNT %in% c(32,360,400,634,642)))
reglm_pba<-regr(lm(ZINT72p~lgGDP_2015,datapba))$Beta_Weights
reglm_cba<-regr(lm(ZINT72p~lgGDP_2015,datacba))$Beta_Weights
reglm_all<-regr(lm(ZINT72p~lgGDP_2015,dataall))$Beta_Weights
correctindex<-reglm_all^2/reglm_pba^2
correctindexcba<-reglm_all^2/reglm_cba^2


