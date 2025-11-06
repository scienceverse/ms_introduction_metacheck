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

#####STUDY 2#######

#####PREPROCESSING#####
#####Loading data#####
examdata = read.spss("Study 2_exam.sav", to.data.frame=TRUE)

#making a pre-post variable in the whole dataset
examdata$prepost <- ifelse(examdata$exam_beepnum >= 0, 
                                c("Post"), c("Pre")) 

#making a subset of the data with the post exam beeps only
examdata_postexam <- examdata[ which(examdata$exam_beepnum>=0), ]

#making pass variables into actual percentages
examdata_postexam$perc_pass <- examdata_postexam$perc_pass*100
examdata_postexam$perc_expass <- examdata_postexam$perc_pass*100

#####Lags####
#Lags exclude overnight
examdata_postexam <- examdata_postexam[order(examdata_postexam$Participant, examdata_postexam$exam_beepnum),]
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_rumination_1 = lag(ER_rumination,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_distraction_1 = lag(ER_distraction,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_acceptance_1 = lag(ER_acceptance,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_reapp_1 = lag(ER_reapp,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_supp_1 = lag(ER_supp,1))
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(ER_soc_sharing_1 = lag(ER_soc_sharing,1))

#####Centering####
examdata_postexam$cperc_pass <- scale(examdata_postexam$perc_pass, center = TRUE, scale = FALSE)
examdata_postexam$cER_rumination <- examdata_postexam$ER_rumination - ave(examdata_postexam$ER_rumination, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_distraction <- examdata_postexam$ER_distraction - ave(examdata_postexam$ER_distraction, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_acceptance <- examdata_postexam$ER_acceptance - ave(examdata_postexam$ER_acceptance, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_reapp <- examdata_postexam$ER_reapp - ave(examdata_postexam$ER_reapp, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_supp <- examdata_postexam$ER_supp - ave(examdata_postexam$ER_supp, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
examdata_postexam$cER_soc_sharing <- examdata_postexam$ER_soc_sharing - ave(examdata_postexam$ER_soc_sharing, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))

#Making scaled variables to use in analyses
examdata_postexam$ScER_rumination <- scale(examdata_postexam$cER_rumination)
examdata_postexam$ScER_distraction <- scale(examdata_postexam$cER_distraction)
examdata_postexam$ScER_reapp <- scale(examdata_postexam$cER_reapp)
examdata_postexam$ScER_supp <- scale(examdata_postexam$cER_supp)
examdata_postexam$ScER_soc_sharing <- scale(examdata_postexam$cER_soc_sharing)
examdata_postexam$ScER_acceptance <- scale(examdata_postexam$cER_acceptance)
examdata_postexam$SER_rumination <- scale(examdata_postexam$ER_rumination)
examdata_postexam$SER_distraction <- scale(examdata_postexam$ER_distraction)
examdata_postexam$SER_reapp <- scale(examdata_postexam$ER_reapp)
examdata_postexam$SER_supp <- scale(examdata_postexam$ER_supp)
examdata_postexam$SER_soc_sharing <- scale(examdata_postexam$ER_soc_sharing)
examdata_postexam$SER_acceptance <- scale(examdata_postexam$ER_acceptance)
examdata_postexam$Scperc_pass <- scale(examdata_postexam$cperc_pass)

#####Exporting this dataset back out again 
write_sav(examdata_postexam, "Exam data pre multiverse.sav")

######MULTIVERSE######
#Here I'm aiming to see what the multiverse of values are when I omit one emotion from the differentiation index

#first making a vector of the emotions in this dataset 
emotions <- c("emotion_sad", "emotion_angry", "emotion_disapp", "emotion_ashamed", "emotion_anxious", "emotion_stressed")

#now making a list where I leave one of the emotions out of each - so six versions of the five sets of emotions
emolist <- lapply(emotions, function (x) emotions[sapply(emotions, function(y) x != y)])

#making a datafile where I'll store my output - setting the number of columns to the number of models that are getting run
multiverse <- data.frame(matrix(NA, ncol = 37, nrow =0))

#making an index variable to use in the datafile
index <- 1

#now making my multiverse
for (i in emolist){#for each emotion set
  print(i)
  examdata_postexam = read.spss("Exam data pre multiverse.sav", to.data.frame=TRUE)#load in a fresh version of the datafile with everything created I need
  
  examdata_postexam$negaff_composite <-rowMeans(subset(examdata_postexam, select=i), na.rm=TRUE) #compute momentary NA
  
  subjects_exam<-unique(examdata_postexam$Participant)#compute emotion differentiation
  icc_list_exam=NULL
  for (sub in subjects_exam){
    sub_examdata_postexam=examdata_postexam[examdata_postexam[,1]==sub,i] 
    icc=icc(sub_examdata_postexam, model="twoway",unit="average")  
    icc_temp=icc$value
    fz_iccTemp=fisherz(icc_temp)
    row_temp=c(sub,icc_temp,fz_iccTemp)
    icc_list_exam=rbind(icc_list_exam,row_temp)
    print(fz_iccTemp)}
  colnames(icc_list_exam)=c("Participant","ICCneg_after","ICCneg_after.fz")
  icc_list_exam=as.data.frame(icc_list_exam)    
  icc_list_exam=icc_list_exam[complete.cases(icc_list_exam),] 
  icc_list_exam=icc_list_exam[icc_list_exam[,2]>0,]            
  examdata_postexam <- merge(examdata_postexam,icc_list_exam,by=c("Participant"))
  examdata_postexam$RICCneg_after.fz <- -1 *examdata_postexam$ICCneg_after.fz
  examdata_postexam$RICCneg_after <- 1 - examdata_postexam$ICCneg_after
  
  examdata_postexam$negaff_composite_meanpostexam <- 
    ave(examdata_postexam$negaff_composite, examdata_postexam$Participant, 
        FUN=function(x) mean(x, na.rm=TRUE)) #Compute person mean of negative emotion (control variable)
  
  examdata_postexam <- examdata_postexam[order(examdata_postexam$Participant, examdata_postexam$exam_beepnum),] #make lags
  examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(negaff_composite_1 = lag(negaff_composite,1))
  
  examdata_postexam$cRICCneg_after.fz <- scale(examdata_postexam$RICCneg_after.fz, center = TRUE, scale = FALSE) #centering
  examdata_postexam$cnegaff_composite_meanpostexam <-scale(examdata_postexam$negaff_composite_meanpostexam, center = TRUE, scale = FALSE)
  examdata_postexam$cnegaff_composite_1 <- examdata_postexam$negaff_composite_1 - ave(examdata_postexam$negaff_composite_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))
  
  examdata_postexam$Snegaff_composite <- scale(examdata_postexam$negaff_composite) #scaling
  examdata_postexam$ScRICCneg_after.fz <- scale(examdata_postexam$cRICCneg_after.fz)
  examdata_postexam$Scnegaff_composite_1 <- scale(examdata_postexam$cnegaff_composite_1)
  examdata_postexam$Scnegaff_composite_meanpostexam <- scale(examdata_postexam$cnegaff_composite_meanpostexam)
  
  exrumi <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam) #model 1
  exdist <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
  exreapp <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
  exsupp <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
  exssh <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
  exacc <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
  
  exrumiNA <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam) #model 2
  exreappNA <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz + Scperc_pass
                  + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
  exsuppNA <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
  exsshNA <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
  exdistNA <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
  exaccNA <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)
  Pexrumi <- summary(exrumi)$coefficients[2,5]#extracting p value
  Cexrumi <- summary(exrumi)$coefficients[2,1]#extracting coefficient
  SEexrumi <- summary(exrumi)$coefficients[2,2]#extracting standard error (repeat these steps for all model 1)
  Pexdist <- summary(exdist)$coefficients[2,5]
  Cexdist <- summary(exdist)$coefficients[2,1]
  SEexdist <- summary(exdist)$coefficients[2,2]
  Pexreapp <- summary(exreapp)$coefficients[2,5]
  Cexreapp <- summary(exreapp)$coefficients[2,1]
  SEexreapp <- summary(exreapp)$coefficients[2,2]
  Pexacc <- summary(exacc)$coefficients[2,5]
  Cexacc <- summary(exacc)$coefficients[2,1]
  SEexacc <- summary(exacc)$coefficients[2,2]
  Pexsupp <- summary(exsupp)$coefficients[2,5]
  Cexsupp <- summary(exsupp)$coefficients[2,1]
  SEexsupp <- summary(exsupp)$coefficients[2,2]
  Pexssh <- summary(exssh)$coefficients[2,5]
  Cexssh <- summary(exssh)$coefficients[2,1]
  SEexssh <- summary(exssh)$coefficients[2,2]
  PexrumiNA <- summary(exrumiNA)$coefficients[6,5]#same for model 2
  CexrumiNA <- summary(exrumiNA)$coefficients[6,1]
  SEexrumiNA <- summary(exrumiNA)$coefficients[6,2]
  PexdistNA <- summary(exdistNA)$coefficients[6,5]
  CexdistNA <- summary(exdistNA)$coefficients[6,1]
  SEexdistNA <- summary(exdistNA)$coefficients[6,2]
  PexreappNA <- summary(exreappNA)$coefficients[6,5]
  CexreappNA <- summary(exreappNA)$coefficients[6,1]
  SEexreappNA <- summary(exreappNA)$coefficients[6,2]
  PexaccNA <- summary(exaccNA)$coefficients[6,5]
  CexaccNA <- summary(exaccNA)$coefficients[6,1]
  SEexaccNA <- summary(exaccNA)$coefficients[6,2]
  PexsuppNA <- summary(exsuppNA)$coefficients[6,5]
  CexsuppNA <- summary(exsuppNA)$coefficients[6,1]
  SEexsuppNA <- summary(exsuppNA)$coefficients[6,2]
  PexsshNA <- summary(exsshNA)$coefficients[6,5]
  CexsshNA <- summary(exsshNA)$coefficients[6,1]
  SEexsshNA <- summary(exsshNA)$coefficients[6,2]
  multiverse[index, ] = c(emotions[index], Pexrumi,Cexrumi,SEexrumi, Pexdist,Cexdist,SEexdist, Pexreapp,Cexreapp,SEexreapp,
                          Pexacc,Cexacc,SEexacc, Pexsupp,Cexsupp,SEexsupp, Pexssh,Cexssh,SEexssh,
                          PexrumiNA,CexrumiNA,SEexrumiNA, PexdistNA,CexdistNA,SEexdistNA,PexreappNA,CexreappNA,SEexreappNA,
                          PexaccNA,CexaccNA,SEexaccNA, PexsuppNA,CexsuppNA,SEexsuppNA,PexsshNA,CexsshNA,SEexsshNA) #making datafile
  names(multiverse)<-c("emo_leftout","P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
                       "P_model1_dist","Coef_model1_dist","SE_model1_dist",
                       "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
                       "P_model1_acc","Coef_model1_acc","SE_model1_acc",
                       "P_model1_supp","Coef_model1_supp","SE_model1_supp",
                       "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
                       "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
                       "P_model2_dist","Coef_model2_dist","SE_model2_dist",
                       "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
                       "P_model2_acc","Coef_model2_acc","SE_model2_acc",
                       "P_model2_supp","Coef_model2_supp","SE_model2_supp",
                       "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh") #naming columns in datafile
  index <- index +1
   }#end of loop


#####NOW WANT TO ADD THE FULL MODEL RESULTS TO OUR DATAFILE####
examdata_postexam = read.spss("Exam data pre multiverse.sav", to.data.frame=TRUE)#load in a fresh version of data

examdata_postexam$negaff_composite <-rowMeans(subset(examdata_postexam, select=c("emotion_sad", "emotion_angry", "emotion_disapp", "emotion_ashamed", 
                                                                                 "emotion_anxious", "emotion_stressed")), na.rm=TRUE) #compute momentary NA

subjects_exam<-unique(examdata_postexam$Participant)#compute emotion differentiation
icc_list_exam=NULL
for (sub in subjects_exam){
  sub_examdata_postexam=examdata_postexam[examdata_postexam[,1]==sub,c("emotion_sad", "emotion_angry", "emotion_disapp", "emotion_ashamed", 
                                                                       "emotion_anxious", "emotion_stressed")] 
  icc=icc(sub_examdata_postexam, model="twoway",unit="average")  
  icc_temp=icc$value
  fz_iccTemp=fisherz(icc_temp)
  row_temp=c(sub,icc_temp,fz_iccTemp)
  icc_list_exam=rbind(icc_list_exam,row_temp)
  print(fz_iccTemp)}
colnames(icc_list_exam)=c("Participant","ICCneg_after","ICCneg_after.fz")
icc_list_exam=as.data.frame(icc_list_exam)    
icc_list_exam=icc_list_exam[complete.cases(icc_list_exam),] 
icc_list_exam=icc_list_exam[icc_list_exam[,2]>0,]            
examdata_postexam <- merge(examdata_postexam,icc_list_exam,by=c("Participant"))
examdata_postexam$RICCneg_after.fz <- -1 *examdata_postexam$ICCneg_after.fz
examdata_postexam$RICCneg_after <- 1 - examdata_postexam$ICCneg_after

examdata_postexam$negaff_composite_meanpostexam <- 
  ave(examdata_postexam$negaff_composite, examdata_postexam$Participant, 
      FUN=function(x) mean(x, na.rm=TRUE)) #Compute person mean of negative emotion (control variable)

examdata_postexam <- examdata_postexam[order(examdata_postexam$Participant, examdata_postexam$exam_beepnum),] #make lags
examdata_postexam <- examdata_postexam %>% group_by(Participant,beepday) %>% mutate(negaff_composite_1 = lag(negaff_composite,1))

examdata_postexam$cRICCneg_after.fz <- scale(examdata_postexam$RICCneg_after.fz, center = TRUE, scale = FALSE) #centering
examdata_postexam$cnegaff_composite_meanpostexam <-scale(examdata_postexam$negaff_composite_meanpostexam, center = TRUE, scale = FALSE)
examdata_postexam$cnegaff_composite_1 <- examdata_postexam$negaff_composite_1 - ave(examdata_postexam$negaff_composite_1, examdata_postexam$Participant, FUN=function(x) mean(x, na.rm=TRUE))

examdata_postexam$Snegaff_composite <- scale(examdata_postexam$negaff_composite) #scaling
examdata_postexam$ScRICCneg_after.fz <- scale(examdata_postexam$cRICCneg_after.fz)
examdata_postexam$Scnegaff_composite_1 <- scale(examdata_postexam$cnegaff_composite_1)
examdata_postexam$Scnegaff_composite_meanpostexam <- scale(examdata_postexam$cnegaff_composite_meanpostexam)

exrumi <- lmer(SER_rumination ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam) #model 1
exdist <- lmer(SER_distraction ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exreapp <- lmer(SER_reapp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exsupp <- lmer(SER_supp ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exssh <- lmer(SER_soc_sharing ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)
exacc <- lmer(SER_acceptance ~ ScRICCneg_after.fz+ Scperc_pass+ Scnegaff_composite_meanpostexam  + (1 | Participant), data=examdata_postexam)

exrumiNA <- lmer(Snegaff_composite ~ ScER_rumination*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_rumination+ Scnegaff_composite_1| Participant), data=examdata_postexam) #model 2
exreappNA <- lmer(Snegaff_composite ~ ScER_reapp*ScRICCneg_after.fz + Scperc_pass
                  + Scnegaff_composite_1 + (ScER_reapp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsuppNA <- lmer(Snegaff_composite ~ ScER_supp*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_supp+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exsshNA <- lmer(Snegaff_composite ~ ScER_soc_sharing*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_soc_sharing+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exdistNA <- lmer(Snegaff_composite ~ ScER_distraction*ScRICCneg_after.fz + Scperc_pass
                 + Scnegaff_composite_1 + (ScER_distraction+ Scnegaff_composite_1| Participant), data=examdata_postexam)
exaccNA <- lmer(Snegaff_composite ~ ScER_acceptance*ScRICCneg_after.fz + Scperc_pass
                + Scnegaff_composite_1 + (ScER_acceptance+ Scnegaff_composite_1| Participant), data=examdata_postexam)
Pexrumi <- summary(exrumi)$coefficients[2,5]#extracting p value
Cexrumi <- summary(exrumi)$coefficients[2,1]#extracting coefficient
SEexrumi <- summary(exrumi)$coefficients[2,2]#extracting standard error (repeat these steps for all models)
Pexdist <- summary(exdist)$coefficients[2,5]
Cexdist <- summary(exdist)$coefficients[2,1]
SEexdist <- summary(exdist)$coefficients[2,2]
Pexreapp <- summary(exreapp)$coefficients[2,5]
Cexreapp <- summary(exreapp)$coefficients[2,1]
SEexreapp <- summary(exreapp)$coefficients[2,2]
Pexacc <- summary(exacc)$coefficients[2,5]
Cexacc <- summary(exacc)$coefficients[2,1]
SEexacc <- summary(exacc)$coefficients[2,2]
Pexsupp <- summary(exsupp)$coefficients[2,5]
Cexsupp <- summary(exsupp)$coefficients[2,1]
SEexsupp <- summary(exsupp)$coefficients[2,2]
Pexssh <- summary(exssh)$coefficients[2,5]
Cexssh <- summary(exssh)$coefficients[2,1]
SEexssh <- summary(exssh)$coefficients[2,2]
PexrumiNA <- summary(exrumiNA)$coefficients[6,5]#same for model 2
CexrumiNA <- summary(exrumiNA)$coefficients[6,1]
SEexrumiNA <- summary(exrumiNA)$coefficients[6,2]
PexdistNA <- summary(exdistNA)$coefficients[6,5]
CexdistNA <- summary(exdistNA)$coefficients[6,1]
SEexdistNA <- summary(exdistNA)$coefficients[6,2]
PexreappNA <- summary(exreappNA)$coefficients[6,5]
CexreappNA <- summary(exreappNA)$coefficients[6,1]
SEexreappNA <- summary(exreappNA)$coefficients[6,2]
PexaccNA <- summary(exaccNA)$coefficients[6,5]
CexaccNA <- summary(exaccNA)$coefficients[6,1]
SEexaccNA <- summary(exaccNA)$coefficients[6,2]
PexsuppNA <- summary(exsuppNA)$coefficients[6,5]
CexsuppNA <- summary(exsuppNA)$coefficients[6,1]
SEexsuppNA <- summary(exsuppNA)$coefficients[6,2]
PexsshNA <- summary(exsshNA)$coefficients[6,5]
CexsshNA <- summary(exsshNA)$coefficients[6,1]
SEexsshNA <- summary(exsshNA)$coefficients[6,2]
emo_leftout <- "all emotions included"

allemotions = data.frame(emo_leftout, Pexrumi,Cexrumi,SEexrumi, Pexdist,Cexdist,SEexdist, Pexreapp,Cexreapp,SEexreapp,
                        Pexacc,Cexacc,SEexacc, Pexsupp,Cexsupp,SEexsupp, Pexssh,Cexssh,SEexssh,
                        PexrumiNA,CexrumiNA,SEexrumiNA, PexdistNA,CexdistNA,SEexdistNA,PexreappNA,CexreappNA,SEexreappNA,
                        PexaccNA,CexaccNA,SEexaccNA, PexsuppNA,CexsuppNA,SEexsuppNA,PexsshNA,CexsshNA,SEexsshNA)#making datafile
names(allemotions)<-c("emo_leftout","P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
                     "P_model1_dist","Coef_model1_dist","SE_model1_dist",
                     "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
                     "P_model1_acc","Coef_model1_acc","SE_model1_acc",
                     "P_model1_supp","Coef_model1_supp","SE_model1_supp",
                     "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
                     "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
                     "P_model2_dist","Coef_model2_dist","SE_model2_dist",
                     "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
                     "P_model2_acc","Coef_model2_acc","SE_model2_acc",
                     "P_model2_supp","Coef_model2_supp","SE_model2_supp",
                     "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh") #naming columns in datafile


#adding these to the multiverse dataset

multiverse_final <- rbind(multiverse, allemotions)


#making columns numeric so we can plot them
cols <- c("P_model1_rumi","Coef_model1_rumi","SE_model1_rumi",
          "P_model1_dist","Coef_model1_dist","SE_model1_dist",
          "P_model1_reapp","Coef_model1_reapp","SE_model1_reapp",
          "P_model1_acc","Coef_model1_acc","SE_model1_acc",
          "P_model1_supp","Coef_model1_supp","SE_model1_supp",
          "P_model1_ssh","Coef_model1_ssh","SE_model1_ssh",
          "P_model2_rumi","Coef_model2_rumi","SE_model2_rumi",
          "P_model2_dist","Coef_model2_dist","SE_model2_dist",
          "P_model2_reapp","Coef_model2_reapp","SE_model2_reapp",
          "P_model2_acc","Coef_model2_acc","SE_model2_acc",
          "P_model2_supp","Coef_model2_supp","SE_model2_supp",
          "P_model2_ssh","Coef_model2_ssh","SE_model2_ssh")

multiverse_final[,cols] = apply(multiverse_final[,cols], 2, function(x) as.numeric(as.character(x)))

#exporting this out in case I want it later (since multiverse takes about 15 minutes to run)
write_sav(multiverse_final, "Multiverse exam data.sav")

####MAKING BAR PLOTS

#reading multiverse in (only if I need to and haven't run it above)
multiverse_final = read.spss("Multiverse exam data.sav", to.data.frame=TRUE)

#####Adding plot theme#####
apatheme= theme_bw()+theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           panel.border=element_blank(),
                           axis.line=element_line(),
                           text=element_text(family='Times',size=12),
                           plot.title = element_text(hjust = 0.5),
                           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#####MODEL 1 p values plots#####

Pex_rumi_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Rumination") + apatheme

Pex_dist_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Distraction") + apatheme

Pex_reapp_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Cognitive reappraisal") + apatheme

Pex_acc_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_acc))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Acceptance") + apatheme

Pex_supp_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Expressive suppression") + apatheme

Pex_ssh_model1 <- ggplot(multiverse_final, aes(emo_leftout, P_model1_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.80), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, 
                                ".40" = 0.40, ".60" = 0.60, ".80" = 0.80), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Social sharing") + apatheme

#Exporting to multi panel figure
png('Model 1_p values_exam data.png', units="cm", width=34, height=24, res=300)
ggarrange(Pex_rumi_model1, Pex_dist_model1, Pex_reapp_model1, Pex_acc_model1, Pex_supp_model1, Pex_ssh_model1,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 1 effects and SE plots#####

EFex_rumi_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_rumi+SE_model1_rumi, ymax=Coef_model1_rumi-SE_model1_rumi), width=.2)+
  labs(title = "Rumination") + apatheme

EFex_dist_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_dist-SE_model1_dist, ymax=Coef_model1_dist+SE_model1_dist), width=.2)+
  labs(title = "Distraction") + apatheme

EFex_reapp_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_reapp-SE_model1_reapp, ymax=Coef_model1_reapp+SE_model1_reapp), width=.2)+
  labs(title = "Cognitive reappraisal") + apatheme

EFex_acc_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_acc))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_acc-SE_model1_acc, ymax=Coef_model1_acc+SE_model1_acc), width=.2)+
  labs(title = "Acceptance") + apatheme

EFex_supp_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_supp-SE_model1_supp, ymax=Coef_model1_supp+SE_model1_supp), width=.2)+
  labs(title = "Expressive suppression") + apatheme

EFex_ssh_model1 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model1_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.27, 0.27),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model1_ssh+SE_model1_ssh, ymax=Coef_model1_ssh-SE_model1_ssh), width=.2)+
  labs(title = "Social sharing") + apatheme

png('Model 1_coefficients_exam data.png', units="cm", width=34, height=24, res=300)
ggarrange(EFex_rumi_model1, EFex_dist_model1, EFex_reapp_model1, EFex_acc_model1, EFex_supp_model1, EFex_ssh_model1,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 2 p values plots#####

Pex_rumi_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Rumination") + apatheme

Pex_dist_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Distraction") + apatheme

Pex_reapp_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Cognitive reappraisal") + apatheme

Pex_acc_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_acc))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Acceptance") + apatheme

Pex_supp_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Expressive suppression") + apatheme

Pex_ssh_model2 <- ggplot(multiverse_final, aes(emo_leftout, P_model2_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("p values",limits = c(0, 0.50), 
                     breaks = c("<.001" = 0.000, ".05" = 0.050, ".10" = 0.10 ,".20" = 0.20, ".30" = 0.30,
                                ".40" = 0.40, ".50" = 0.50), 
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_hline(yintercept = .05, linetype = "dashed") + 
  labs(title = "Social sharing") + apatheme

#Exporting to multi panel figure
png('Model 2_p values_exam data.png', units="cm", width=34, height=24, res=300)
ggarrange(Pex_rumi_model2, Pex_dist_model2, Pex_reapp_model2, Pex_acc_model2, Pex_supp_model2, Pex_ssh_model2,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)
dev.off()

#####MODEL 2 effects and SE plots#####

EFex_rumi_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_rumi))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05, 0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_rumi+SE_model2_rumi, ymax=Coef_model2_rumi-SE_model2_rumi), width=.2)+
  labs(title = "Rumination") + apatheme

EFex_dist_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_dist))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05, 0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_dist+SE_model2_dist, ymax=Coef_model2_dist-SE_model2_dist), width=.2)+
  labs(title = "Distraction") + apatheme

EFex_reapp_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_reapp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05, 0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_reapp+SE_model2_reapp, ymax=Coef_model2_reapp-SE_model2_reapp), width=.2)+
  labs(title = "Cognitive reappraisal") + apatheme

EFex_acc_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_acc))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05, 0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_acc-SE_model2_acc, ymax=Coef_model2_acc+SE_model2_acc), width=.2)+
  labs(title = "Acceptance") + apatheme

EFex_supp_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_supp))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05,0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_supp+SE_model2_supp, ymax=Coef_model2_supp-SE_model2_supp), width=.2)+
  labs(title = "Expressive suppression") + apatheme

EFex_ssh_model2 <- ggplot(multiverse_final, aes(emo_leftout, Coef_model2_ssh))+
  geom_col(aes(fill = ifelse(multiverse_final$emo_leftout=='all emotions included', 'gray37', 'gray77'))) +
  scale_fill_identity() +
  scale_y_continuous("Coefficients",limits = c(-0.05,0.05),
                     expand = c(0, 0)) +
  scale_x_discrete("Emotions included",labels = c("All\nemotions\nincluded", "Angry\nomitted", "Anxious\nomitted",
                                                  "Ashamed\nomitted", "Disappointed\nomitted", "Sadness\nomitted",
                                                  "Stressed\nomitted")) +
  geom_errorbar(aes(ymin=Coef_model2_ssh+SE_model2_ssh, ymax=Coef_model2_ssh-SE_model2_ssh), width=.2)+
  labs(title = "Social sharing") + apatheme

png('Model 2_coefficients_exam data.png', units="cm", width=34, height=24, res=300)
ggarrange(EFex_rumi_model2, EFex_dist_model2, EFex_reapp_model2, EFex_acc_model2, EFex_supp_model2, EFex_ssh_model2,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)
dev.off()