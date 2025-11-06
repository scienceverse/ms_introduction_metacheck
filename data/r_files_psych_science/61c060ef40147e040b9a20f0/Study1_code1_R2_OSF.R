#######################################################
# If you use or modify this code for your own research, 
# please cite the work: 
# "The Moral Significance of Aesthetics in Nature Imagery" 
# by Eunsoo Kim, Julia Lee Cunningham and Anocha Aribarg (2022) 
#########################################################

library(data.table)
library(dummies)
library(stargazer) 

###############################################################################################
#Study 1 Table 1 Regression Results of Image Aesthetics on Social Media Engagement 
###############################################################################################
# Replace photographer dummy with # of followers for each photographer accont

#setwd("...working directory...")
load("Study1_data1.RData")           # This data is for Table 1 and SI Appendix D
         
#Text related
#DMValenceText: Demeaned text valence (Valence from EL: ELtxtValence)
#DMArousalText: Demeaned text arousal  (Arousal from Whissell: WhtxtArousal) 
#data$DMValenceText <- data$ELtxtValence - mean(data$ELtxtValence) 
#data$DMArousalText <- data$WhtxtArousal - mean(data$WhtxtArousal) 

#Image related
#DMMlaesnew811: Demeaned image aesthetic score
#DMMlvalnew811: Demeaned image valence score
#DMMlarounew811: Demeaned image arousal score

#data$DMAestheImg  <- data$Mlaesnew811 - mean(data$Mlaesnew811) 
#data$DMValenceImg  <- data$Mlvalnew811 - mean(data$Mlvalnew811) 
#data$DMArousalImg  <- data$Mlarounew811 - mean(data$Mlarounew811)

#photographer dummy file
photog <- read.csv("photograph_dummy.csv")
head(photog)
photog <-data.table(photog)
photog$X <- NULL
photog0<-photog[,c(2:161)]  
dim(photog0)
#[1] 11222   160
photog0 <-as.matrix(photog0)
#11222   161

#photographer account # of follower file 
photographerNum <- read.csv("photographer_num_follower_OSF.csv")
head(photographerNum)
photographerNum <-data.table(photographerNum)
summary(photographerNum$no_followers_xls)   
data <-data.table(data)
setkey(data, OrgImgNo)    
setkey(photographerNum, OrgImgNo)

data <- merge(data,photographerNum, all.x = TRUE)

data$ln_no_followers_xls <-  log(data$no_followers_xls +1)

dumyr <-dummy(data$yr)[,c(2:4)]
dummonth <-dummy(data$month)[,c(2:12)]

#m1 aesthetics only
m1<-(lm(LikesDivFollowers100 ~DMAestheImg + lndiffdates + lnPostorder  +dumyr + dummonth ,data))
summary(m1)

#m2 Add valence and arousal
m2<-(lm(LikesDivFollowers100 ~DMAestheImg+DMValenceImg + DMArousalImg + lndiffdates + lnPostorder  +dumyr + dummonth ,data))
summary(m2)

#m3 Add objects                 
m3<-(lm(LikesDivFollowers100 ~DMAestheImg+DMValenceImg + DMArousalImg + Animal_only+Bothsum+ Scenery_only+lndiffdates + lnPostorder  +dumyr + dummonth ,data))
summary(m3)

#m4 text  
m4<-(lm(LikesDivFollowers100 ~DMAestheImg+DMValenceImg + DMArousalImg + Animal_only+Bothsum+ Scenery_only + DMValenceText + DMArousalText  + ELwordcount+lndiffdates + lnPostorder  +dumyr + dummonth ,data))
summary(m4)

#m5 photographer account # of follower                                          
m5<-(lm(LikesDivFollowers100 ~DMAestheImg+DMValenceImg + DMArousalImg + Animal_only+Bothsum+ Scenery_only + DMValenceText + DMArousalText  + ELwordcount+lndiffdates + lnPostorder  +dumyr + dummonth+ln_no_followers_xls ,data))
summary(m5)                                                      

#m6 photographer fixed effect
m6<-(lm(LikesDivFollowers100 ~DMAestheImg+DMValenceImg + DMArousalImg + Animal_only+Bothsum+ Scenery_only + DMValenceText + DMArousalText  + ELwordcount+lndiffdates + lnPostorder  +dumyr + dummonth+photog0 ,data))
summary(m6)

#stargazer(m1,m2, m3,m4,m5,m6, type = "text", out="Study1_Table1_R2.html")


##################################################################################################################
# SI Appendix D. Table S1. Regression Results of Image Aesthetics on Social Media Engagement with Impression Data
##################################################################################################################

dataImp <- data[-which(is.na(data$impressions==TRUE)),]
dim(dataImp)
#[1] 1358   34
#dumyr <-dummy(dataImp$yr)[,c(2:4)]    #only yr2018 images so dropped year dummy
dummonth <-dummy(dataImp$month)[,c(2:8)]  #from May - Dec. 
summary(dataImp$impressions)
dataImp$Impressionsdmil<- dataImp$impressions/1000000 
 
#photographer dummy file
photog <- read.csv("photograph_dummy.csv")
head(photog)
photog <-data.table(photog)
photog$X <- NULL
photog0<-photog[,c(2:161)]  
dim(photog0)
#[1] 11222   160
photog0 <-as.matrix(photog0)
#11222   161
photog0s <- photog0[-which(is.na(data$impressions==TRUE)),]
photog0matsr <-photog0s[,-c(which(apply(photog0s, 2, sum)==0))]
photog0matsr <-as.matrix(photog0matsr)


#m1 aesthetics only
m1<-(lm(LikesDivFollowers100 ~ DMAestheImg +lndiffdates + lnPostorder + Impressionsdmil   + dummonth ,dataImp))
summary(m1)

#m2 Add valence and arousal
m2<-(lm(LikesDivFollowers100 ~ DMAestheImg +DMValenceImg + DMArousalImg  +lndiffdates + lnPostorder  + Impressionsdmil  + dummonth   ,dataImp))
summary(m2)

#m3 Add objects     
m3<-(lm(LikesDivFollowers100 ~ DMAestheImg +DMValenceImg + DMArousalImg+ Animal_only + Bothsum+ Scenery_only + lndiffdates + lnPostorder  + Impressionsdmil + dummonth,dataImp))
summary(m3)

#m4 text               
m4<-(lm(LikesDivFollowers100 ~ DMAestheImg +DMValenceImg + DMArousalImg + Animal_only + Bothsum + Scenery_only + DMValenceText + DMArousalText + ELwordcount+lndiffdates + lnPostorder  + Impressionsdmil   + dummonth, dataImp))
summary(m4)

#m5 photographer account # of followers 
m5<-(lm(LikesDivFollowers100 ~DMAestheImg +DMValenceImg + DMArousalImg + Animal_only + Bothsum+ Scenery_only + DMValenceText + DMArousalText  + ELwordcount +lndiffdates + lnPostorder  + Impressionsdmil   + dummonth +  ln_no_followers_xls, dataImp))
summary(m5)  


#m6 photographer fixed effect
m6<-(lm(LikesDivFollowers100 ~DMAestheImg +DMValenceImg + DMArousalImg + Animal_only + Bothsum+ Scenery_only + DMValenceText + DMArousalText  + ELwordcount +lndiffdates + lnPostorder  + Impressionsdmil   + dummonth +  photog0matsr, dataImp))
summary(m6)

#stargazer(m1,m2,m3,m4,m5,m6, type = "text", out="AppendixD_R2.html")

  