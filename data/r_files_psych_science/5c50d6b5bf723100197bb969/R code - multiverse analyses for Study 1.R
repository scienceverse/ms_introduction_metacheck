#######SET UP#####
#set working directory

# load packages
library(lme4)
library(lmerTest)
library(psych)
library(irr)
library(haven)
library(foreign)
library(ggplot2)
library(dplyr)
library(ggpubr)

#turning off scientific notation
options(scipen=999)

#####STUDY 1#######

#####PREPROCESSING#####
#####Loading data#####
ldata = read.spss("Study 1_longit_low compliance ps removed.sav", to.data.frame=TRUE)

#####Lags####
#Lags exclude overnight, and are done wave by wave
ldata_w1 <-subset(ldata, Wave == 1)
ldata_w2 <-subset(ldata, Wave == 2)
ldata_w3 <-subset(ldata, Wave == 3)

#Wave 1 - sort by participant and beep number first!
ldata_w1 <- ldata_w1[order(ldata_w1$PpID, ldata_w1$BeepNr),]
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(rumi_1 = lag(rumi,1))
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(dist_1 = lag(dist,1))
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(reapp_1 = lag(reapp,1))
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(supp_1 = lag(supp,1))
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(ssh_1 = lag(ssh,1))

#Wave 2
ldata_w2 <- ldata_w2[order(ldata_w2$PpID, ldata_w2$BeepNr),]
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(rumi_1 = lag(rumi,1))
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(dist_1 = lag(dist,1))
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(reapp_1 = lag(reapp,1))
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(supp_1 = lag(supp,1))
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(ssh_1 = lag(ssh,1))

#Wave 3
ldata_w3 <- ldata_w3[order(ldata_w3$PpID, ldata_w3$BeepNr),]
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(rumi_1 = lag(rumi,1))
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(dist_1 = lag(dist,1))
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(reapp_1 = lag(reapp,1))
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(supp_1 = lag(supp,1))
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(ssh_1 = lag(ssh,1))

#merging datasets including lags back together
ldata <- rbind(ldata_w1,ldata_w2,ldata_w3)

#####Centering####
#making level 1 variables centered within wave and within person
ldata$crumi <- ldata$rumi - ave(ldata$rumi, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$cdist <- ldata$dist - ave(ldata$dist, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$creapp <- ldata$reapp - ave(ldata$reapp, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$csupp <- ldata$supp - ave(ldata$supp, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
ldata$cssh <- ldata$ssh - ave(ldata$ssh, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))

#Making scaled variables to use in analyses
ldata$Srumi <- scale(ldata$rumi)
ldata$Sdist <- scale(ldata$dist)
ldata$Sreapp <- scale(ldata$reapp)
ldata$Ssupp <- scale(ldata$supp)
ldata$Sssh <- scale(ldata$ssh)
ldata$Scrumi <- scale(ldata$crumi)
ldata$Scdist <- scale(ldata$cdist)
ldata$Screapp <- scale(ldata$creapp)
ldata$Scsupp <- scale(ldata$csupp)
ldata$Scssh <- scale(ldata$cssh)

#####Exporting this dataset back out again 
write_sav(ldata, "Longitudinal data pre multiverse.sav")

######MULTIVERSE######
#Here I'm aiming to see what the multiverse of values are when I omit one emotion from the differentiation index

#first making a vector of the emotions in this dataset 
emotions <- c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")

#now making a list where I leave one of the emotions out of each - so six versions of the five sets of emotions
emolist <- lapply(emotions, function (x) emotions[sapply(emotions, function(y) x != y)])

#making a datafile where I'll store my output - setting the number of columns to the number of models that are getting run
multiverse <- data.frame(matrix(NA, ncol = 31, nrow =0))

#making an index variable to use in the datafile
index <- 1

#now making my multiverse
for (i in emolist){#for each emotion set
  print(i)
  ldata = read.spss("Longitudinal data pre multiverse.sav", to.data.frame=TRUE)#load in a fresh version of the datafile with everything created I need
  
  ldata$negaff <-rowMeans(subset(ldata, select=c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")), na.rm=TRUE)
  #compute momentary NA
  
  ldata_w1 <-subset(ldata, Wave == 1)#make lags per wave
  ldata_w2 <-subset(ldata, Wave == 2)
  ldata_w3 <-subset(ldata, Wave == 3)
  ldata_w1 <- ldata_w1[order(ldata_w1$PpID, ldata_w1$BeepNr),] 
  ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
  ldata_w2 <- ldata_w2[order(ldata_w2$PpID, ldata_w2$BeepNr),]
  ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
  ldata_w3 <- ldata_w3[order(ldata_w3$PpID, ldata_w3$BeepNr),]
  ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
  ldata <- rbind(ldata_w1,ldata_w2,ldata_w3) #merging lagged datasets back together
  
  ldata_w1 <-subset(ldata, Wave == 1)#loading in wave by wave datasets to compute emotion differentiation
  ldata_w2 <-subset(ldata, Wave == 2)
  ldata_w3 <-subset(ldata, Wave == 3)
  
  subjects_w1=unique(ldata_w1$PpID)#wave 1 emotion differentiation
  icc_list_w1=NULL
  for (sub in subjects_w1){
    sub_ldata_w1=ldata_w1[ldata_w1[,1]==sub,i] 
    icc=icc(sub_ldata_w1, model="twoway",unit="average")  
    icc_temp=icc$value
    fz_iccTemp=fisherz(icc_temp)
    row_temp=c(sub,icc_temp,fz_iccTemp)
    icc_list_w1=rbind(icc_list_w1,row_temp)
    print(fz_iccTemp)}
  colnames(icc_list_w1)=c("PpID","ICCneg","ICCneg.fz")
  icc_list_w1=as.data.frame(icc_list_w1)    
  icc_list_w1=icc_list_w1[complete.cases(icc_list_w1),] 
  icc_list_w1=icc_list_w1[icc_list_w1[,2]>0,]            
  icc_list_w1$Wave <- 1
  
  subjects_w2=unique(ldata_w2$PpID)#wave 2 emotion differentiation
  icc_list_w2=NULL
  for (sub in subjects_w2){
    sub_ldata_w2=ldata_w2[ldata_w2[,1]==sub,i] 
    icc=icc(sub_ldata_w2, model="twoway",unit="average")  
    icc_temp=icc$value
    fz_iccTemp=fisherz(icc_temp)
    row_temp=c(sub,icc_temp,fz_iccTemp)
    icc_list_w2=rbind(icc_list_w2,row_temp)
    print(fz_iccTemp)}
  colnames(icc_list_w2)=c("PpID","ICCneg","ICCneg.fz")
  icc_list_w2=as.data.frame(icc_list_w2)    
  icc_list_w2=icc_list_w2[complete.cases(icc_list_w2),] 
  icc_list_w2=icc_list_w2[icc_list_w2[,2]>0,]            
  icc_list_w2$Wave <- 2
  
  subjects_w3=unique(ldata_w3$PpID)#wave 3 emotion differentiation
  icc_list_w3=NULL
  for (sub in subjects_w3){
    sub_ldata_w3=ldata_w3[ldata_w3[,1]==sub,i] 
    icc=icc(sub_ldata_w3, model="twoway",unit="average")  
    icc_temp=icc$value
    fz_iccTemp=fisherz(icc_temp)
    row_temp=c(sub,icc_temp,fz_iccTemp)
    icc_list_w3=rbind(icc_list_w3,row_temp)
    print(fz_iccTemp)}
  colnames(icc_list_w3)=c("PpID","ICCneg","ICCneg.fz")
  icc_list_w3=as.data.frame(icc_list_w3)    
  icc_list_w3=icc_list_w3[complete.cases(icc_list_w3),] 
  icc_list_w3=icc_list_w3[icc_list_w3[,2]>0,]            
  icc_list_w3$Wave <- 3
  
  icc_list_ldata <- rbind(icc_list_w1,icc_list_w2,icc_list_w3)#putting together three sets of ICCs
  ldata <- merge(ldata,icc_list_ldata,by=c("PpID","Wave")) #merging into main dataset
  
  ldata$RICCneg.fz <- -1* ldata$ICCneg.fz #reverse scoring
  ldata$RICCneg <- 1-ldata$ICCneg
  
  ldata$negaff_mean <- ave(ldata$negaff, ldata$Wave, ldata$PpID, 
                           FUN=function(x) mean(x, na.rm=TRUE)) #Compute person mean of per wave of negative emotion (control variable)

  ldata$cRICCneg.fzW <- ldata$RICCneg.fz - ave(ldata$RICCneg.fz, ldata$Wave, FUN=function(x) mean(x, na.rm=TRUE))#centering
  ldata$cnegaff_meanW <- ldata$negaff_mean - ave(ldata$negaff_mean, ldata$Wave, FUN=function(x) mean(x, na.rm=TRUE))
  ldata$cnegaff_1<- ldata$negaff_1 - ave(ldata$negaff_1, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))
  
  ldata$ScRICCneg.fzW <- scale(ldata$cRICCneg.fzW)#scaling
  ldata$Scnegaff_meanW <- scale(ldata$cnegaff_meanW)
  ldata$Snegaff <- scale(ldata$negaff)
  ldata$Scnegaff_1<- scale(ldata$cnegaff_1)
  
  lrumi <- lmer(Srumi ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)#model 1
  ldist <- lmer(Sdist ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
  lreapp <- lmer(Sreapp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
  lsupp <- lmer(Ssupp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
  lssh <- lmer(Sssh ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
  
  lrumiNA <- lmer(Snegaff ~ Scrumi*ScRICCneg.fzW
                  + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scrumi + Scnegaff_1 | PpID) 
                  + (0 + Scrumi + Scnegaff_1 | Wave) +  (0 + Scrumi + Scnegaff_1 || PpID:Wave), data=ldata) #model 2
  ldistNA <- lmer(Snegaff ~ Scdist*ScRICCneg.fzW  
                  + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scdist + Scnegaff_1 | PpID) 
                  + (0 + Scdist + Scnegaff_1 | Wave) +  (0 + Scdist + Scnegaff_1 || PpID:Wave), data=ldata)
  lreappNA <- lmer(Snegaff ~ Screapp*ScRICCneg.fzW 
                   + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Screapp + Scnegaff_1 | PpID) 
                   + (0 + Screapp + Scnegaff_1 | Wave) +  (0 + Screapp + Scnegaff_1 || PpID:Wave), data=ldata)
  lsuppNA <- lmer(Snegaff ~ Scsupp*ScRICCneg.fzW 
                  + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scsupp + Scnegaff_1 | PpID) 
                  + (0 + Scsupp + Scnegaff_1 | Wave) +  (0 + Scsupp + Scnegaff_1 || PpID:Wave), data=ldata)
  lsshNA <- lmer(Snegaff ~ Scssh*ScRICCneg.fzW 
                 + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scssh + Scnegaff_1 | PpID) 
                 + (0 + Scssh + Scnegaff_1 | Wave) +  (0 + Scssh + Scnegaff_1 || PpID:Wave), data=ldata)
  
  Plrumi <- summary(lrumi)$coefficients[2,5]#extracting p value
  Clrumi <- summary(lrumi)$coefficients[2,1]#extracting coefficient
  SElrumi <- summary(lrumi)$coefficients[2,2]#extracting standard error (repeat these steps for all model 1)
  Pldist <- summary(ldist)$coefficients[2,5]
  Cldist <- summary(ldist)$coefficients[2,1]
  SEldist <- summary(ldist)$coefficients[2,2]
  Plreapp <- summary(lreapp)$coefficients[2,5]
  Clreapp <- summary(lreapp)$coefficients[2,1]
  SElreapp <- summary(lreapp)$coefficients[2,2]
  Plsupp <- summary(lsupp)$coefficients[2,5]
  Clsupp <- summary(lsupp)$coefficients[2,1]
  SElsupp <- summary(lsupp)$coefficients[2,2]
  Plssh <- summary(lssh)$coefficients[2,5]
  Clssh <- summary(lssh)$coefficients[2,1]
  SElssh <- summary(lssh)$coefficients[2,2]
  PlrumiNA <- summary(lrumiNA)$coefficients[5,5]#same for model 2
  ClrumiNA <- summary(lrumiNA)$coefficients[5,1]
  SElrumiNA <- summary(lrumiNA)$coefficients[5,2]
  PldistNA <- summary(ldistNA)$coefficients[5,5]
  CldistNA <- summary(ldistNA)$coefficients[5,1]
  SEldistNA <- summary(ldistNA)$coefficients[5,2]
  PlreappNA <- summary(lreappNA)$coefficients[5,5]
  ClreappNA <- summary(lreappNA)$coefficients[5,1]
  SElreappNA <- summary(lreappNA)$coefficients[5,2]
  PlsuppNA <- summary(lsuppNA)$coefficients[5,5]
  ClsuppNA <- summary(lsuppNA)$coefficients[5,1]
  SElsuppNA <- summary(lsuppNA)$coefficients[5,2]
  PlsshNA <- summary(lsshNA)$coefficients[5,5]
  ClsshNA <- summary(lsshNA)$coefficients[5,1]
  SElsshNA <- summary(lsshNA)$coefficients[5,2]
  multiverse[index, ] = c(emotions[index], Plrumi,Clrumi,SElrumi, Pldist,Cldist,SEldist, Plreapp,Clreapp,SElreapp,
                          Plsupp,Clsupp,SElsupp, Plssh,Clssh,SElssh,
                          PlrumiNA,ClrumiNA,SElrumiNA, PldistNA,CldistNA,SEldistNA,PlreappNA,ClreappNA,SElreappNA,
                          PlsuppNA,ClsuppNA,SElsuppNA,PlsshNA,ClsshNA,SElsshNA) #making datafile
  names(multiverse)<-c("emo_leftout","P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
                       "P_model1_dist","Coef_model1_dist","SE_model1_dist",
                       "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
                       "P_model1_supp","Coef_model1_supp","SE_model1_supp",
                       "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
                       "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
                       "P_model2_dist","Coef_model2_dist","SE_model2_dist",
                       "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
                       "P_model2_supp","Coef_model2_supp","SE_model2_supp",
                       "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh") #naming columns in datafile
  index <- index +1
   }#end of loop

#####NOW WANT TO ADD THE FULL MODEL RESULTS TO OUR DATAFILE####
ldata = read.spss("Longitudinal data pre multiverse.sav", to.data.frame=TRUE)#load in a fresh version of data

ldata$negaff <-rowMeans(subset(ldata, select=c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")), na.rm=TRUE)

ldata_w1 <-subset(ldata, Wave == 1)#make lags per wave
ldata_w2 <-subset(ldata, Wave == 2)
ldata_w3 <-subset(ldata, Wave == 3)
ldata_w1 <- ldata_w1[order(ldata_w1$PpID, ldata_w1$BeepNr),] 
ldata_w1 <- ldata_w1 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
ldata_w2 <- ldata_w2[order(ldata_w2$PpID, ldata_w2$BeepNr),]
ldata_w2 <- ldata_w2 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
ldata_w3 <- ldata_w3[order(ldata_w3$PpID, ldata_w3$BeepNr),]
ldata_w3 <- ldata_w3 %>% group_by(PpID,DayNr) %>% mutate(negaff_1 = lag(negaff,1))
ldata <- rbind(ldata_w1,ldata_w2,ldata_w3) #merging lagged datasets back together

ldata_w1 <-subset(ldata, Wave == 1)
ldata_w2 <-subset(ldata, Wave == 2)
ldata_w3 <-subset(ldata, Wave == 3)

subjects_w1=unique(ldata_w1$PpID)

icc_list_w1=NULL
for (sub in subjects_w1){
  sub_ldata_w1=ldata_w1[ldata_w1[,1]==sub,c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")]  #selects data for the current subject
  icc=icc(sub_ldata_w1, model="twoway",unit="average")  #computes ICC
  icc_temp=icc$value
  
  fz_iccTemp=fisherz(icc_temp)
  
  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_w1=rbind(icc_list_w1,row_temp)
  print(fz_iccTemp)
}

colnames(icc_list_w1)=c("PpID","ICCneg","ICCneg.fz")
icc_list_w1=as.data.frame(icc_list_w1)    


icc_list_w1=icc_list_w1[complete.cases(icc_list_w1),]  #choose subjects where both icc and icc_fz 
icc_list_w1=icc_list_w1[icc_list_w1[,2]>0,]            #choose subjects with positive ICC - negative ICCs are uninterpretable

icc_list_w1$Wave <- 1

subjects_w2=unique(ldata_w2$PpID)

icc_list_w2=NULL
for (sub in subjects_w2){
  sub_ldata_w2=ldata_w2[ldata_w2[,1]==sub,c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")]  #selects data for the current subject
  icc=icc(sub_ldata_w2, model="twoway",unit="average")  #computes ICC
  icc_temp=icc$value
  
  fz_iccTemp=fisherz(icc_temp)
  
  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_w2=rbind(icc_list_w2,row_temp)
  print(fz_iccTemp)
}

colnames(icc_list_w2)=c("PpID","ICCneg","ICCneg.fz")
icc_list_w2=as.data.frame(icc_list_w2)    


icc_list_w2=icc_list_w2[complete.cases(icc_list_w2),]  #choose subjects where both icc and icc_fz 
icc_list_w2=icc_list_w2[icc_list_w2[,2]>0,]            #choose subjects with positive ICC - negative ICCs are uninterpretable


icc_list_w2$Wave <- 2

subjects_w3=unique(ldata_w3$PpID)

icc_list_w3=NULL
for (sub in subjects_w3){
  sub_ldata_w3=ldata_w3[ldata_w3[,1]==sub,c("droevig", "kwaad", "angstig", "depressief", "gestresseerd", "eenzaam")]  #selects data for the current subject
  icc=icc(sub_ldata_w3, model="twoway",unit="average")  #computes ICC
  icc_temp=icc$value
  
  fz_iccTemp=fisherz(icc_temp)
  
  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_w3=rbind(icc_list_w3,row_temp)
  print(fz_iccTemp)
}

colnames(icc_list_w3)=c("PpID","ICCneg","ICCneg.fz")
icc_list_w3=as.data.frame(icc_list_w3)    


icc_list_w3=icc_list_w3[complete.cases(icc_list_w3),]  #choose subjects where both icc and icc_fz 
icc_list_w3=icc_list_w3[icc_list_w3[,2]>0,]            #choose subjects with positive ICC - negative ICCs are uninterpretable
icc_list_w3$Wave <- 3 

icc_list_ldata <- rbind(icc_list_w1,icc_list_w2,icc_list_w3)
ldata <- merge(ldata,icc_list_ldata,by=c("PpID","Wave"))

ldata$RICCneg.fz <- -1* ldata$ICCneg.fz
ldata$RICCneg <- 1-ldata$ICCneg

ldata$negaff_mean <- ave(ldata$negaff, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE)) #person mean centered by wave NA

ldata$cRICCneg.fzW <- ldata$RICCneg.fz - ave(ldata$RICCneg.fz, ldata$Wave, FUN=function(x) mean(x, na.rm=TRUE))#centering
ldata$cnegaff_meanW <- ldata$negaff_mean - ave(ldata$negaff_mean, ldata$Wave, FUN=function(x) mean(x, na.rm=TRUE))
ldata$cnegaff_1<- ldata$negaff_1 - ave(ldata$negaff_1, ldata$Wave, ldata$PpID, FUN=function(x) mean(x, na.rm=TRUE))

ldata$ScRICCneg.fzW <- scale(ldata$cRICCneg.fzW)#scaling
ldata$Scnegaff_meanW <- scale(ldata$cnegaff_meanW)
ldata$Snegaff <- scale(ldata$negaff)
ldata$Scnegaff_1<- scale(ldata$cnegaff_1)

lrumi <- lmer(Srumi ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)#model 1
ldist <- lmer(Sdist ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lreapp <- lmer(Sreapp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lsupp <- lmer(Ssupp ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)
lssh <- lmer(Sssh ~ ScRICCneg.fzW + Scnegaff_meanW + (1| PpID/Wave), data=ldata)

lrumiNA <- lmer(Snegaff ~ Scrumi*ScRICCneg.fzW
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scrumi + Scnegaff_1 | PpID) 
                + (0 + Scrumi + Scnegaff_1 | Wave) +  (0 + Scrumi + Scnegaff_1 || PpID:Wave), data=ldata) #model 2
ldistNA <- lmer(Snegaff ~ Scdist*ScRICCneg.fzW  
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scdist + Scnegaff_1 | PpID) 
                + (0 + Scdist + Scnegaff_1 | Wave) +  (0 + Scdist + Scnegaff_1 || PpID:Wave), data=ldata)
lreappNA <- lmer(Snegaff ~ Screapp*ScRICCneg.fzW 
                 + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Screapp + Scnegaff_1 | PpID) 
                 + (0 + Screapp + Scnegaff_1 | Wave) +  (0 + Screapp + Scnegaff_1 || PpID:Wave), data=ldata)
lsuppNA <- lmer(Snegaff ~ Scsupp*ScRICCneg.fzW 
                + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scsupp + Scnegaff_1 | PpID) 
                + (0 + Scsupp + Scnegaff_1 | Wave) +  (0 + Scsupp + Scnegaff_1 || PpID:Wave), data=ldata)
lsshNA <- lmer(Snegaff ~ Scssh*ScRICCneg.fzW 
               + Scnegaff_1 + (1|PpID) + (1|Wave)  + (0 + Scssh + Scnegaff_1 | PpID) 
               + (0 + Scssh + Scnegaff_1 | Wave) +  (0 + Scssh + Scnegaff_1 || PpID:Wave), data=ldata)

Plrumi <- summary(lrumi)$coefficients[2,5]#extracting p value
Clrumi <- summary(lrumi)$coefficients[2,1]#extracting coefficient
SElrumi <- summary(lrumi)$coefficients[2,2]#extracting standard error (repeat these steps for all model 1)
Pldist <- summary(ldist)$coefficients[2,5]
Cldist <- summary(ldist)$coefficients[2,1]
SEldist <- summary(ldist)$coefficients[2,2]
Plreapp <- summary(lreapp)$coefficients[2,5]
Clreapp <- summary(lreapp)$coefficients[2,1]
SElreapp <- summary(lreapp)$coefficients[2,2]
Plsupp <- summary(lsupp)$coefficients[2,5]
Clsupp <- summary(lsupp)$coefficients[2,1]
SElsupp <- summary(lsupp)$coefficients[2,2]
Plssh <- summary(lssh)$coefficients[2,5]
Clssh <- summary(lssh)$coefficients[2,1]
SElssh <- summary(lssh)$coefficients[2,2]
PlrumiNA <- summary(lrumiNA)$coefficients[5,5]#same for model 2
ClrumiNA <- summary(lrumiNA)$coefficients[5,1]
SElrumiNA <- summary(lrumiNA)$coefficients[5,2]
PldistNA <- summary(ldistNA)$coefficients[5,5]
CldistNA <- summary(ldistNA)$coefficients[5,1]
SEldistNA <- summary(ldistNA)$coefficients[5,2]
PlreappNA <- summary(lreappNA)$coefficients[5,5]
ClreappNA <- summary(lreappNA)$coefficients[5,1]
SElreappNA <- summary(lreappNA)$coefficients[5,2]
PlsuppNA <- summary(lsuppNA)$coefficients[5,5]
ClsuppNA <- summary(lsuppNA)$coefficients[5,1]
SElsuppNA <- summary(lsuppNA)$coefficients[5,2]
PlsshNA <- summary(lsshNA)$coefficients[5,5]
ClsshNA <- summary(lsshNA)$coefficients[5,1]
SElsshNA <- summary(lsshNA)$coefficients[5,2]
emo_leftout <- "all emotions included"

allemotions = data.frame(emo_leftout, Plrumi,Clrumi,SElrumi, Pldist,Cldist,SEldist, Plreapp,Clreapp,SElreapp,
                        Plsupp,Clsupp,SElsupp, Plssh,Clssh,SElssh,
                        PlrumiNA,ClrumiNA,SElrumiNA, PldistNA,CldistNA,SEldistNA,PlreappNA,ClreappNA,SElreappNA,
                        PlsuppNA,ClsuppNA,SElsuppNA,PlsshNA,ClsshNA,SElsshNA)#making datafile
names(allemotions)<-c("emo_leftout","P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
                     "P_model1_dist","Coef_model1_dist","SE_model1_dist",
                     "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
                     "P_model1_supp","Coef_model1_supp","SE_model1_supp",
                     "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
                     "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
                     "P_model2_dist","Coef_model2_dist","SE_model2_dist",
                     "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
                     "P_model2_supp","Coef_model2_supp","SE_model2_supp",
                     "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh") #naming columns in datafile


#adding these to the multiverse dataset

multiverse_final <- rbind(multiverse, allemotions)

#making columns numeric so we can plot them
cols <- c("P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
          "P_model1_dist","Coef_model1_dist","SE_model1_dist",
          "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
          "P_model1_supp","Coef_model1_supp","SE_model1_supp",
          "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
          "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
          "P_model2_dist","Coef_model2_dist","SE_model2_dist",
          "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
          "P_model2_supp","Coef_model2_supp","SE_model2_supp",
          "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh")

multiverse_final[,cols] = apply(multiverse_final[,cols], 2, function(x) as.numeric(as.character(x)))

#exporting this out in case I want it later (since multiverse takes about 15 minutes to run)
write_sav(multiverse_final, "Multiverse longitudinal data.sav")

####MAKING BAR PLOTS
#reading multiverse in (only if I need to and haven't run it above)
multiverse_final = read.spss("Multiverse longitudinal data.sav", to.data.frame=TRUE)

#####Adding plot theme#####
apatheme= theme_bw()+theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           panel.border=element_blank(),
                           axis.line=element_line(),
                           text=element_text(family='Times',size=12),
                           plot.title = element_text(hjust = 0.5),
                           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#####MODEL 1 p values plots#####

Pl_rumi_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 1.00), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80, "1.00" = 1.00), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Rumination") + apatheme

Pl_dist_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 1.00), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80, "1.00" = 1.00), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Distraction") + apatheme

Pl_reapp_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 1.00), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80, "1.00" = 1.00), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Cognitive reappraisal") + apatheme

Pl_supp_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 1.00), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80, "1.00" = 1.00), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Expressive suppression") + apatheme

Pl_ssh_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 1.00), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80, "1.00" = 1.00), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Social sharing") + apatheme

#Exporting to multi panel figure
png('Model 1_p values_longitudinal data.png', units="cm", width=34, height=24, res=300)
ggarrange(Pl_rumi_model1, Pl_dist_model1, Pl_reapp_model1, Pl_supp_model1, Pl_ssh_model1,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 1 effects and SE plots#####

EFl_rumi_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.10, 0.10),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_rumi-SE_model1_rumi, ymax=Coef_model1_rumi+SE_model1_rumi), width=.2)+
  labs(title = "Rumination") + apatheme

EFl_dist_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.10, 0.10),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_dist+SE_model1_dist, ymax=Coef_model1_dist-SE_model1_dist), width=.2)+
  labs(title = "Distraction") + apatheme

EFl_reapp_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.10, 0.10),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_reapp-SE_model1_reapp, ymax=Coef_model1_reapp+SE_model1_reapp), width=.2)+
  labs(title = "Cognitive reappraisal") + apatheme

EFl_supp_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.10, 0.10),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_supp-SE_model1_supp, ymax=Coef_model1_supp+SE_model1_supp), width=.2)+
  labs(title = "Expressive suppression") + apatheme

EFl_ssh_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.10, 0.10),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_ssh+SE_model1_ssh, ymax=Coef_model1_ssh-SE_model1_ssh), width=.2)+
  labs(title = "Social sharing") + apatheme

png('Model 1_coefficients_longitudinal data.png', units="cm", width=34, height=24, res=300)
ggarrange(EFl_rumi_model1, EFl_dist_model1, EFl_reapp_model1, EFl_supp_model1, EFl_ssh_model1,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 2 p values plots#####

Pl_rumi_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.05), 
                     breaks = c("<.001" = 0.000, ".01" = 0.010, ".05" = 0.050), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Rumination") + apatheme

Pl_dist_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.05), 
                     breaks = c("<.001" = 0.000, ".01" = 0.010, ".05" = 0.050), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Distraction") + apatheme

Pl_reapp_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.05), 
                     breaks = c("<.001" = 0.000, ".01" = 0.010, ".05" = 0.050), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Cognitive reappraisal") + apatheme

Pl_supp_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.05), 
                     breaks = c("<.001" = 0.000, ".01" = 0.010, ".05" = 0.050), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Expressive suppression") + apatheme

Pl_ssh_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.05), 
                     breaks = c("<.001" = 0.000, ".01" = 0.010, ".05" = 0.050), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Social sharing") + apatheme

#Exporting to multi panel figure
png('Model 2_p values_longitudinal data.png', units="cm", width=34, height=24, res=300)
ggarrange(Pl_rumi_model2, Pl_dist_model2, Pl_reapp_model2, Pl_supp_model2, Pl_ssh_model2,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 2 effects and SE plots#####

EFl_rumi_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.12, 0.00),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_rumi+SE_model2_rumi, ymax=Coef_model2_rumi-SE_model2_rumi), width=.2)+
  labs(title = "Rumination") + apatheme

EFl_dist_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.12, 0.00),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_dist+SE_model2_dist, ymax=Coef_model2_dist-SE_model2_dist), width=.2)+
  labs(title = "Distraction") + apatheme

EFl_reapp_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.12, 0.00),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_reapp+SE_model2_reapp, ymax=Coef_model2_reapp-SE_model2_reapp), width=.2)+
  labs(title = "Cognitive reappraisal") + apatheme

EFl_supp_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.12, 0.00),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_supp+SE_model2_supp, ymax=Coef_model2_supp-SE_model2_supp), width=.2)+
  labs(title = "Expressive suppression") + apatheme

EFl_ssh_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.12, 0.00),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Anxious\nomitted", "Depressed\nomitted",
                                                  "Sad\nomitted", "Lonely\nomitted", "Stressed\nomitted",
                                                  "Angry\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_ssh+SE_model2_ssh, ymax=Coef_model2_ssh-SE_model2_ssh), width=.2)+
  labs(title = "Social sharing") + apatheme

png('Model 2_coefficients_longitudinal data.png', units="cm", width=34, height=24, res=300)
ggarrange(EFl_rumi_model2, EFl_dist_model2, EFl_reapp_model2, EFl_supp_model2, EFl_ssh_model2,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
dev.off()