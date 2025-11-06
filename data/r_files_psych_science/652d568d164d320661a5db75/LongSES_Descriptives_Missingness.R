# Reading in datasets --------------------------------------------
library(dplyr)
library(tidyverse)
library(rstatix)
library(bruceR)
library(lme4)
library(lmerTest)
library(emmeans)

# set working directory to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d_white<-read.csv("white_longitSES.csv")
d_black<-read.csv("black_longitSES.csv")

# White sample descriptives & cleaning --------------------------------------------
table(d_white$Gender_1)
psych::describe(d_white$Age_1)
psych::describe(d_white$Edu_1)
psych::describe(d_white$Income_1)
table(d_white$PolParty_1)

psych::describe(d_white$Age_2)
psych::describe(d_white$Age_3)
psych::describe(d_white$Age_4)
psych::describe(d_white$Age_5)

## Ladder Dif----
d_white$LadderDif.1<-d_white$LadderGroup_1 - d_white$LadderSelf_1
d_white$LadderDif.2<-d_white$LadderGroup_2 - d_white$LadderSelf_2
d_white$LadderDif.3<-d_white$LadderGroup_3 - d_white$LadderSelf_3
d_white$LadderDif.4<-d_white$LadderGroup_4 - d_white$LadderSelf_4
d_white$LadderDif.5<-d_white$LadderGroup_5 - d_white$LadderSelf_5
psych::describe(d_white$LadderDif.1)
psych::describe(d_white$LadderDif.2)
psych::describe(d_white$LadderDif.3)
psych::describe(d_white$LadderDif.4)
psych::describe(d_white$LadderDif.5)

## Positive Emotions----
posEmo1<-dplyr::select(d_white, Amused_1, Awe_1, Grateful_1, Hopeful_1, Inspired_1, 
                       Interested_1, Joy_1, Love_1, Proud_1, Serene_1)
psych::alpha(posEmo1)
d_white$posEmo.1<-rowMeans(posEmo1, na.rm = T)
psych::describe(d_white$posEmo.1)

posEmo2<-dplyr::select(d_white, Amused_2, Awe_2, Grateful_2, Hopeful_2, Inspired_2, 
                       Interested_2, Joy_2, Love_2, Proud_2, Serene_2)
psych::alpha(posEmo2)
d_white$posEmo.2<-rowMeans(posEmo2, na.rm = T)
psych::describe(d_white$posEmo.2)

posEmo3<-dplyr::select(d_white, Amused_3, Awe_3, Grateful_3, Hopeful_3, Inspired_3, 
                       Interested_3, Joy_3, Love_3, Proud_3, Serene_3)
psych::alpha(posEmo3)
d_white$posEmo.3<-rowMeans(posEmo3, na.rm = T)
psych::describe(d_white$posEmo.3)

posEmo4<-dplyr::select(d_white, Amused_4, Awe_4, Grateful_4, Hopeful_4, Inspired_4, 
                       Interested_4, Joy_4, Love_4, Proud_4, Serene_4)
psych::alpha(posEmo4)
d_white$posEmo.4<-rowMeans(posEmo4, na.rm = T)
psych::describe(d_white$posEmo.4)


posEmo5<-dplyr::select(d_white, Amused_5, Awe_5, Grateful_5, Hopeful_5, Inspired_5, 
                       Interested_5, Joy_5, Love_5, Proud_5, Serene_5)
psych::alpha(posEmo5)
d_white$posEmo.5<-rowMeans(posEmo5, na.rm = T)
psych::describe(d_white$posEmo.5)

## Negative Emotions----

negEmo1<-dplyr::select(d_white, Angry_1, Ashamed_1, Contempt_1, Disgust_1, Guilt_1, Hate_1, Sad_1, Scared_1, Stressed_1,
                       Embarrssed_1, Resentful_1, Shame1_1, Shame2_1, Shame3_1, Shame4_1)
psych::alpha(negEmo1)
d_white$negEmo.1<-rowMeans(negEmo1, na.rm = T)
psych::describe(d_white$negEmo.1)

negEmo2<-dplyr::select(d_white, Angry_2, Ashamed_2, Contempt_2, Disgust_2, Guilt_2, Hate_2, Sad_2, Scared_2, Stressed_2,
                       Embarrssed_2, Resentful_2, Shame1_2, Shame2_2, Shame3_2, Shame4_2)
psych::alpha(negEmo2)
d_white$negEmo.2<-rowMeans(negEmo2, na.rm = T)
psych::describe(d_white$negEmo.2)

negEmo3<-dplyr::select(d_white, Angry_3, Ashamed_3, Contempt_3, Disgust_3, Guilt_3, Hate_3, Sad_3, Scared_3, Stressed_3,
                       Embarrssed_3, Resentful_3, Shame1_3, Shame2_3, Shame3_3, Shame4_3)
psych::alpha(negEmo3)
d_white$negEmo.3<-rowMeans(negEmo3, na.rm = T)
psych::describe(d_white$negEmo.3)

negEmo4<-dplyr::select(d_white, Angry_4, Ashamed_4, Contempt_4, Disgust_4, Guilt_4, Hate_4, Sad_4, Scared_4, Stressed_4,
                       Embarrssed_4, Resentful_4, Shame1_4, Shame2_4, Shame3_4, Shame4_4)
psych::alpha(negEmo4)
d_white$negEmo.4<-rowMeans(negEmo4, na.rm = T)
psych::describe(d_white$negEmo.4)

negEmo5<-dplyr::select(d_white, Angry_5, Ashamed_5, Contempt_5, Disgust_5, Guilt_5, Hate_5, Sad_5, Scared_5, Stressed_5,
                       Embarrssed_5, Resentful_5, Shame1_5, Shame2_5, Shame3_5, Shame4_5)
psych::alpha(negEmo5)
d_white$negEmo.5<-rowMeans(negEmo5, na.rm = T)
psych::describe(d_white$negEmo.5)

## Depression---- 

dep1<-dplyr::select(d_white, PROMIS_1_1, PROMIS_2_1, PROMIS_3_1, PROMIS_4_1, PROMIS_5_1, 
                    PROMIS_6_1, PROMIS_7_1, PROMIS_8_1)
psych::alpha(dep1)
d_white$dep.1<-rowMeans(dep1, na.rm = T)
psych::describe(d_white$dep.1)

dep2<-dplyr::select(d_white, PROMIS_1_2, PROMIS_2_2, PROMIS_3_2, PROMIS_4_2, PROMIS_5_2, 
                    PROMIS_6_2, PROMIS_7_2, PROMIS_8_2)
psych::alpha(dep2)
d_white$dep.2<-rowMeans(dep2, na.rm = T)
psych::describe(d_white$dep.2)

dep3<-dplyr::select(d_white, PROMIS_1_3, PROMIS_2_3, PROMIS_3_3, PROMIS_4_3, PROMIS_5_3, 
                    PROMIS_6_3, PROMIS_7_3, PROMIS_8_3)
psych::alpha(dep3)
d_white$dep.3<-rowMeans(dep3, na.rm = T)
psych::describe(d_white$dep.3)

dep4<-dplyr::select(d_white, PROMIS_1_4, PROMIS_2_4, PROMIS_3_4, PROMIS_4_4, PROMIS_5_4, 
                    PROMIS_6_4, PROMIS_7_4, PROMIS_8_4)
psych::alpha(dep4)
d_white$dep.4<-rowMeans(dep4, na.rm = T)
psych::describe(d_white$dep.4)

dep5<-dplyr::select(d_white, PROMIS_1_5, PROMIS_2_5, PROMIS_3_5, PROMIS_4_5, PROMIS_5_5, 
                    PROMIS_6_5, PROMIS_7_5, PROMIS_8_5)
psych::alpha(dep5)
d_white$dep.5<-rowMeans(dep5, na.rm = T)
psych::describe(d_white$dep.5)

## Sleep----

d_white$Sleepscale_1_1_S<- d_white$Sleepscale_1_1 - 1 # put on 5-point scale
d_white$Sleepscale_1_2_S<- d_white$Sleepscale_1_2 - 1
d_white$Sleepscale_1_3_S<- d_white$Sleepscale_1_3 - 1
d_white$Sleepscale_1_4_S<- d_white$Sleepscale_1_4 - 1
d_white$Sleepscale_1_5_S<- d_white$Sleepscale_1_5 - 1
d_white$Sleepscale_2_1_R<-7 - d_white$Sleepscale_2_1 # reverse & 5-point scale
d_white$Sleepscale_2_2_R<-7 - d_white$Sleepscale_2_2
d_white$Sleepscale_2_3_R<-7 - d_white$Sleepscale_2_3
d_white$Sleepscale_2_4_R<-7 - d_white$Sleepscale_2_4
d_white$Sleepscale_2_5_R<-7 - d_white$Sleepscale_2_5
d_white$Sleepscale_3_1_R<-7 - d_white$Sleepscale_3_1
d_white$Sleepscale_3_2_R<-7 - d_white$Sleepscale_3_2
d_white$Sleepscale_3_3_R<-7 - d_white$Sleepscale_3_3
d_white$Sleepscale_3_4_R<-7 - d_white$Sleepscale_3_4
d_white$Sleepscale_3_5_R<-7 - d_white$Sleepscale_3_5

gSleep1<-dplyr::select(d_white, Sleepscale_1_1_S, Sleepscale_2_1_R, Sleepscale_3_1_R, SleepQual_1)
psych::alpha(gSleep1)
d_white$gSleep.1<-rowMeans(gSleep1, na.rm = T)
psych::describe(d_white$gSleep.1)

gSleep2<-dplyr::select(d_white, Sleepscale_1_2_S, Sleepscale_2_2_R, Sleepscale_3_2_R, SleepQual_2)
psych::alpha(gSleep2)
d_white$gSleep.2<-rowMeans(gSleep2, na.rm = T)
psych::describe(d_white$gSleep.2)

gSleep3<-dplyr::select(d_white, Sleepscale_1_3_S, Sleepscale_2_3_R, Sleepscale_3_3_R, SleepQual_3)
psych::alpha(gSleep3)
d_white$gSleep.3<-rowMeans(gSleep3, na.rm = T)
psych::describe(d_white$gSleep.3)

gSleep4<-dplyr::select(d_white, Sleepscale_1_4_S, Sleepscale_2_4_R, Sleepscale_3_4_R, SleepQual_4)
psych::alpha(gSleep4)
d_white$gSleep.4<-rowMeans(gSleep4, na.rm = T)
psych::describe(d_white$gSleep.4)

gSleep5<-dplyr::select(d_white, Sleepscale_1_5_S, Sleepscale_2_5_R, Sleepscale_3_5_R, SleepQual_5)
psych::alpha(gSleep5)
d_white$gSleep.5<-rowMeans(gSleep5, na.rm = T)
psych::describe(d_white$gSleep.5)

## Health----

d_white$Fatigue_1_R<- 6 - d_white$Fatigue_1
d_white$Fatigue_2_R<- 6 - d_white$Fatigue_2
d_white$Fatigue_3_R<- 6 - d_white$Fatigue_3
d_white$Fatigue_4_R<- 6 - d_white$Fatigue_4
d_white$Fatigue_5_R<- 6 - d_white$Fatigue_5

d_white$Pain_1_R<- 6 - d_white$Pain_1
d_white$Pain_2_R<- 6 - d_white$Pain_2
d_white$Pain_3_R<- 6 - d_white$Pain_3
d_white$Pain_4_R<- 6 - d_white$Pain_4
d_white$Pain_5_R<- 6 - d_white$Pain_5
# scaling because all vars were on different scales 
d_white$Pain_1z<-scale(d_white$Pain_1_R)
d_white$Pain_2z<-scale(d_white$Pain_2_R)
d_white$Pain_3z<-scale(d_white$Pain_3_R)
d_white$Pain_4z<-scale(d_white$Pain_4_R)
d_white$Pain_5z<-scale(d_white$Pain_5_R)

d_white$Fatigue_1z<-scale(d_white$Fatigue_1_R)
d_white$Fatigue_2z<-scale(d_white$Fatigue_2_R)
d_white$Fatigue_3z<-scale(d_white$Fatigue_3_R)
d_white$Fatigue_4z<-scale(d_white$Fatigue_4_R)
d_white$Fatigue_5z<-scale(d_white$Fatigue_5_R)

d_white$Phys_1z<-scale(d_white$Phys_1)
d_white$Phys_2z<-scale(d_white$Phys_2)
d_white$Phys_3z<-scale(d_white$Phys_3)
d_white$Phys_4z<-scale(d_white$Phys_4)
d_white$Phys_5z<-scale(d_white$Phys_5)

d_white$Health_1z<-scale(d_white$Health_1_1)
d_white$Health_2z<-scale(d_white$Health_1_2)
d_white$Health_3z<-scale(d_white$Health_1_3)
d_white$Health_4z<-scale(d_white$Health_1_4)
d_white$Health_5z<-scale(d_white$Health_1_5)

gHealth1<-dplyr::select(d_white, Pain_1z, Fatigue_1z, Phys_1z, Health_1z)
psych::alpha(gHealth1) # .77
d_white$gHealth.1<-rowMeans(gHealth1, na.rm = T)
psych::describe(d_white$gHealth.1)

gHealth2<-dplyr::select(d_white, Pain_2z, Fatigue_2z, Phys_2z, Health_2z)
psych::alpha(gHealth2) # .78
d_white$gHealth.2<-rowMeans(gHealth2, na.rm = T)
psych::describe(d_white$gHealth.2)

gHealth3<-dplyr::select(d_white, Pain_3z, Fatigue_3z, Phys_3z, Health_3z)
psych::alpha(gHealth3) # .78
d_white$gHealth.3<-rowMeans(gHealth3, na.rm = T)
psych::describe(d_white$gHealth.3)

gHealth4<-dplyr::select(d_white, Pain_4z, Fatigue_4z, Phys_4z, Health_4z)
psych::alpha(gHealth4) # .80
d_white$gHealth.4<-rowMeans(gHealth4, na.rm = T)
psych::describe(d_white$gHealth.4)

gHealth5<-dplyr::select(d_white, Pain_5z, Fatigue_5z, Phys_5z, Health_5z)
psych::alpha(gHealth5) # .79
d_white$gHealth.5<-rowMeans(gHealth5, na.rm = T)
psych::describe(d_white$gHealth.5)

## Politics---- 

cor.test(d_white$Soc_Conserv_1, d_white$Econ_Conserv_1) # .81
cor.test(d_white$Soc_Conserv_2, d_white$Econ_Conserv_2) # .79
cor.test(d_white$Soc_Conserv_3, d_white$Econ_Conserv_3) # .80
cor.test(d_white$Soc_Conserv_4, d_white$Econ_Conserv_4) # .76
cor.test(d_white$Soc_Conserv_5, d_white$Econ_Conserv_5) # .80

d_white$PolConserv1<-rowMeans(dplyr::select(d_white, Soc_Conserv_1, Econ_Conserv_1), na.rm = T)

d_white$PolConserv1<-rowMeans(dplyr::select(d_white, Soc_Conserv_1, Econ_Conserv_1), na.rm = T)
d_white$PolParty<-ifelse(d_white$PolParty_1 == 4, NA, d_white$PolParty_1)
d_white$PolParty_F<-as.factor(d_white$PolParty)


# Black sample descriptives & cleaning -----------------------------
table(d_black$Gender_1)
psych::describe(d_black$Age_1)
psych::describe(d_black$Edu_1)
psych::describe(d_black$Income_1)
table(d_black$PolParty_1)

psych::describe(d_black$Age_2)
psych::describe(d_black$Age_3)
psych::describe(d_black$Age_4)
psych::describe(d_black$Age_5)

## Ladder Dif----
d_black$LadderDif.1<-d_black$LadderGroup_1 - d_black$LadderSelf_1
d_black$LadderDif.2<-d_black$LadderGroup_2 - d_black$LadderSelf_2
d_black$LadderDif.3<-d_black$LadderGroup_3 - d_black$LadderSelf_3
d_black$LadderDif.4<-d_black$LadderGroup_4 - d_black$LadderSelf_4
d_black$LadderDif.5<-d_black$LadderGroup_5 - d_black$LadderSelf_5
psych::describe(d_black$LadderDif.1)
psych::describe(d_black$LadderDif.2)
psych::describe(d_black$LadderDif.3)
psych::describe(d_black$LadderDif.4)
psych::describe(d_black$LadderDif.5)

## Positive Emotions----
posEmo1<-dplyr::select(d_black, Amused_1, Awe_1, Grateful_1, Hopeful_1, Inspired_1, 
                       Interested_1, Joy_1, Love_1, Proud_1, Serene_1)
psych::alpha(posEmo1)
d_black$posEmo.1<-rowMeans(posEmo1, na.rm = T)
psych::describe(d_black$posEmo.1)

posEmo2<-dplyr::select(d_black, Amused_2, Awe_2, Grateful_2, Hopeful_2, Inspired_2, 
                       Interested_2, Joy_2, Love_2, Proud_2, Serene_2)
psych::alpha(posEmo2)
d_black$posEmo.2<-rowMeans(posEmo2, na.rm = T)
psych::describe(d_black$posEmo.2)

posEmo3<-dplyr::select(d_black, Amused_3, Awe_3, Grateful_3, Hopeful_3, Inspired_3, 
                       Interested_3, Joy_3, Love_3, Proud_3, Serene_3)
psych::alpha(posEmo3)
d_black$posEmo.3<-rowMeans(posEmo3, na.rm = T)
psych::describe(d_black$posEmo.3)

posEmo4<-dplyr::select(d_black, Amused_4, Awe_4, Grateful_4, Hopeful_4, Inspired_4, 
                       Interested_4, Joy_4, Love_4, Proud_4, Serene_4)
psych::alpha(posEmo4)
d_black$posEmo.4<-rowMeans(posEmo4, na.rm = T)
psych::describe(d_black$posEmo.4)


posEmo5<-dplyr::select(d_black, Amused_5, Awe_5, Grateful_5, Hopeful_5, Inspired_5, 
                       Interested_5, Joy_5, Love_5, Proud_5, Serene_5)
psych::alpha(posEmo5)
d_black$posEmo.5<-rowMeans(posEmo5, na.rm = T)
psych::describe(d_black$posEmo.5)

## Negative Emotions----

negEmo1<-dplyr::select(d_black, Angry_1, Ashamed_1, Contempt_1, Disgust_1, Guilt_1, Hate_1, Sad_1, Scared_1, Stressed_1,
                       Embarrssed_1, Resentful_1, Shame1_1, Shame2_1, Shame3_1, Shame4_1)
psych::alpha(negEmo1)
d_black$negEmo.1<-rowMeans(negEmo1, na.rm = T)
psych::describe(d_black$negEmo.1)

negEmo2<-dplyr::select(d_black, Angry_2, Ashamed_2, Contempt_2, Disgust_2, Guilt_2, Hate_2, Sad_2, Scared_2, Stressed_2,
                       Embarrssed_2, Resentful_2, Shame1_2, Shame2_2, Shame3_2, Shame4_2)
psych::alpha(negEmo2)
d_black$negEmo.2<-rowMeans(negEmo2, na.rm = T)
psych::describe(d_black$negEmo.2)

negEmo3<-dplyr::select(d_black, Angry_3, Ashamed_3, Contempt_3, Disgust_3, Guilt_3, Hate_3, Sad_3, Scared_3, Stressed_3,
                       Embarrssed_3, Resentful_3, Shame1_3, Shame2_3, Shame3_3, Shame4_3)
psych::alpha(negEmo3)
d_black$negEmo.3<-rowMeans(negEmo3, na.rm = T)
psych::describe(d_black$negEmo.3)

negEmo4<-dplyr::select(d_black, Angry_4, Ashamed_4, Contempt_4, Disgust_4, Guilt_4, Hate_4, Sad_4, Scared_4, Stressed_4,
                       Embarrssed_4, Resentful_4, Shame1_4, Shame2_4, Shame3_4, Shame4_4)
psych::alpha(negEmo4)
d_black$negEmo.4<-rowMeans(negEmo4, na.rm = T)
psych::describe(d_black$negEmo.4)

negEmo5<-dplyr::select(d_black, Angry_5, Ashamed_5, Contempt_5, Disgust_5, Guilt_5, Hate_5, Sad_5, Scared_5, Stressed_5,
                       Embarrssed_5, Resentful_5, Shame1_5, Shame2_5, Shame3_5, Shame4_5)
psych::alpha(negEmo5)
d_black$negEmo.5<-rowMeans(negEmo5, na.rm = T)
psych::describe(d_black$negEmo.5)

## Depression---- 

dep1<-dplyr::select(d_black, PROMIS_1_1, PROMIS_2_1, PROMIS_3_1, PROMIS_4_1, PROMIS_5_1, 
                    PROMIS_6_1, PROMIS_7_1, PROMIS_8_1)
psych::alpha(dep1)
d_black$dep.1<-rowMeans(dep1, na.rm = T)
psych::describe(d_black$dep.1)

dep2<-dplyr::select(d_black, PROMIS_1_2, PROMIS_2_2, PROMIS_3_2, PROMIS_4_2, PROMIS_5_2, 
                    PROMIS_6_2, PROMIS_7_2, PROMIS_8_2)
psych::alpha(dep2)
d_black$dep.2<-rowMeans(dep2, na.rm = T)
psych::describe(d_black$dep.2)

dep3<-dplyr::select(d_black, PROMIS_1_3, PROMIS_2_3, PROMIS_3_3, PROMIS_4_3, PROMIS_5_3, 
                    PROMIS_6_3, PROMIS_7_3, PROMIS_8_3)
psych::alpha(dep3)
d_black$dep.3<-rowMeans(dep3, na.rm = T)
psych::describe(d_black$dep.3)

dep4<-dplyr::select(d_black, PROMIS_1_4, PROMIS_2_4, PROMIS_3_4, PROMIS_4_4, PROMIS_5_4, 
                    PROMIS_6_4, PROMIS_7_4, PROMIS_8_4)
psych::alpha(dep4)
d_black$dep.4<-rowMeans(dep4, na.rm = T)
psych::describe(d_black$dep.4)

dep5<-dplyr::select(d_black, PROMIS_1_5, PROMIS_2_5, PROMIS_3_5, PROMIS_4_5, PROMIS_5_5, 
                    PROMIS_6_5, PROMIS_7_5, PROMIS_8_5)
psych::alpha(dep5)
d_black$dep.5<-rowMeans(dep5, na.rm = T)
psych::describe(d_black$dep.5)

## Sleep---- 
d_black$Sleepscale_1_1_S<- d_black$Sleepscale_1_1 - 1 # put on 5-point scale
d_black$Sleepscale_1_2_S<- d_black$Sleepscale_1_2 - 1
d_black$Sleepscale_1_3_S<- d_black$Sleepscale_1_3 - 1
d_black$Sleepscale_1_4_S<- d_black$Sleepscale_1_4 - 1
d_black$Sleepscale_1_5_S<- d_black$Sleepscale_1_5 - 1
d_black$Sleepscale_2_1_R<-7 - d_black$Sleepscale_2_1 # reverse & 5-point scale
d_black$Sleepscale_2_2_R<-7 - d_black$Sleepscale_2_2
d_black$Sleepscale_2_3_R<-7 - d_black$Sleepscale_2_3
d_black$Sleepscale_2_4_R<-7 - d_black$Sleepscale_2_4
d_black$Sleepscale_2_5_R<-7 - d_black$Sleepscale_2_5
d_black$Sleepscale_3_1_R<-7 - d_black$Sleepscale_3_1
d_black$Sleepscale_3_2_R<-7 - d_black$Sleepscale_3_2
d_black$Sleepscale_3_3_R<-7 - d_black$Sleepscale_3_3
d_black$Sleepscale_3_4_R<-7 - d_black$Sleepscale_3_4
d_black$Sleepscale_3_5_R<-7 - d_black$Sleepscale_3_5

gSleep1<-dplyr::select(d_black, Sleepscale_1_1_S, Sleepscale_2_1_R, Sleepscale_3_1_R, SleepQual_1)
psych::alpha(gSleep1)
d_black$gSleep.1<-rowMeans(gSleep1, na.rm = T)
psych::describe(d_black$gSleep.1)

gSleep2<-dplyr::select(d_black, Sleepscale_1_2_S, Sleepscale_2_2_R, Sleepscale_3_2_R, SleepQual_2)
psych::alpha(gSleep2)
d_black$gSleep.2<-rowMeans(gSleep2, na.rm = T)
psych::describe(d_black$gSleep.2)

gSleep3<-dplyr::select(d_black, Sleepscale_1_3_S, Sleepscale_2_3_R, Sleepscale_3_3_R, SleepQual_3)
psych::alpha(gSleep3)
d_black$gSleep.3<-rowMeans(gSleep3, na.rm = T)
psych::describe(d_black$gSleep.3)

gSleep4<-dplyr::select(d_black, Sleepscale_1_4_S, Sleepscale_2_4_R, Sleepscale_3_4_R, SleepQual_4)
psych::alpha(gSleep4)
d_black$gSleep.4<-rowMeans(gSleep4, na.rm = T)
psych::describe(d_black$gSleep.4)

gSleep5<-dplyr::select(d_black, Sleepscale_1_5_S, Sleepscale_2_5_R, Sleepscale_3_5_R, SleepQual_5)
psych::alpha(gSleep5)
d_black$gSleep.5<-rowMeans(gSleep5, na.rm = T)
psych::describe(d_black$gSleep.5)

## Health---- 
d_black$Fatigue_1_R<- 6 - d_black$Fatigue_1
d_black$Fatigue_2_R<- 6 - d_black$Fatigue_2
d_black$Fatigue_3_R<- 6 - d_black$Fatigue_3
d_black$Fatigue_4_R<- 6 - d_black$Fatigue_4
d_black$Fatigue_5_R<- 6 - d_black$Fatigue_5

d_black$Pain_1_R<- 6 - d_black$Pain_1
d_black$Pain_2_R<- 6 - d_black$Pain_2
d_black$Pain_3_R<- 6 - d_black$Pain_3
d_black$Pain_4_R<- 6 - d_black$Pain_4
d_black$Pain_5_R<- 6 - d_black$Pain_5
# scaling because all vars were on different scales 
d_black$Pain_1z<-scale(d_black$Pain_1_R)
d_black$Pain_2z<-scale(d_black$Pain_2_R)
d_black$Pain_3z<-scale(d_black$Pain_3_R)
d_black$Pain_4z<-scale(d_black$Pain_4_R)
d_black$Pain_5z<-scale(d_black$Pain_5_R)

d_black$Fatigue_1z<-scale(d_black$Fatigue_1_R)
d_black$Fatigue_2z<-scale(d_black$Fatigue_2_R)
d_black$Fatigue_3z<-scale(d_black$Fatigue_3_R)
d_black$Fatigue_4z<-scale(d_black$Fatigue_4_R)
d_black$Fatigue_5z<-scale(d_black$Fatigue_5_R)

d_black$Phys_1z<-scale(d_black$Phys_1)
d_black$Phys_2z<-scale(d_black$Phys_2)
d_black$Phys_3z<-scale(d_black$Phys_3)
d_black$Phys_4z<-scale(d_black$Phys_4)
d_black$Phys_5z<-scale(d_black$Phys_5)

d_black$Health_1z<-scale(d_black$Health_1_1)
d_black$Health_2z<-scale(d_black$Health_1_2)
d_black$Health_3z<-scale(d_black$Health_1_3)
d_black$Health_4z<-scale(d_black$Health_1_4)
d_black$Health_5z<-scale(d_black$Health_1_5)

gHealth1<-dplyr::select(d_black, Pain_1z, Fatigue_1z, Phys_1z, Health_1z)
psych::alpha(gHealth1) # .65
d_black$gHealth.1<-rowMeans(gHealth1, na.rm = T)
psych::describe(d_black$gHealth.1)

gHealth2<-dplyr::select(d_black, Pain_2z, Fatigue_2z, Phys_2z, Health_2z)
psych::alpha(gHealth2) # .65
d_black$gHealth.2<-rowMeans(gHealth2, na.rm = T)
psych::describe(d_black$gHealth.2)

gHealth3<-dplyr::select(d_black, Pain_3z, Fatigue_3z, Phys_3z, Health_3z)
psych::alpha(gHealth3) # .72
d_black$gHealth.3<-rowMeans(gHealth3, na.rm = T)
psych::describe(d_black$gHealth.3)

gHealth4<-dplyr::select(d_black, Pain_4z, Fatigue_4z, Phys_4z, Health_4z)
psych::alpha(gHealth4) # .74
d_black$gHealth.4<-rowMeans(gHealth4, na.rm = T)
psych::describe(d_black$gHealth.4)

gHealth5<-dplyr::select(d_black, Pain_5z, Fatigue_5z, Phys_5z, Health_5z)
psych::alpha(gHealth5) # .74
d_black$gHealth.5<-rowMeans(gHealth5, na.rm = T)
psych::describe(d_black$gHealth.5)

## Politics---- 

cor.test(d_black$Soc_Conserv_1, d_black$Econ_Conserv_1) # .71
cor.test(d_black$Soc_Conserv_2, d_black$Econ_Conserv_2) # .74
cor.test(d_black$Soc_Conserv_3, d_black$Econ_Conserv_3) # .69
cor.test(d_black$Soc_Conserv_4, d_black$Econ_Conserv_4) # .72
cor.test(d_black$Soc_Conserv_5, d_black$Econ_Conserv_5) # .74

d_black$PolConserv1<-rowMeans(dplyr::select(d_black, Soc_Conserv_1, Econ_Conserv_1), na.rm = T)
d_black$PolParty<-ifelse(d_black$PolParty_1 == 4, NA, d_black$PolParty_1)
d_black$PolParty_F<-as.factor(d_black$PolParty)

# T1 analyses ---------------------------------------------------

## Status comparison frequency ----
psych::describe(d_white$frequencystatus_1_1)
psych::describe(d_black$frequencystatus_1_1)

## Comparing Group Status and Self Status, white ----
t.test(d_white$LadderGroup_1, d_white$LadderSelf_1, paired = T)
psych::describe(d_white$LadderGroup_1)
psych::describe(d_white$LadderSelf_1)

wide_w<- d_white %>% select(ID, LadderGroup_1, LadderSelf_1, Edu_1, Income_1)

long_w <- wide_w %>%
  gather(key = "item", value = "score", LadderGroup_1, LadderSelf_1) %>%
  convert_as_factor(ID, Edu_1, Income_1, item)

long_w$Edu<-as.numeric(long_w$Edu_1)
long_w$Income<-as.numeric(long_w$Income_1)

model_white<-lmer(score ~ item + Edu + Income + (1|ID), data = long_w)
summary(model_white)
anova(model_white)
emmeans(model_white, ~item)

## Comparing Group Status and Self Status, black ----
t.test(d_black$LadderGroup_1, d_black$LadderSelf_1, paired = T)
psych::describe(d_black$LadderGroup_1)
psych::describe(d_black$LadderSelf_1)

wide_b<- d_black %>% select(ID, LadderGroup_1, LadderSelf_1, Edu_1, Income_1)

long_b <- wide_b %>%
  gather(key = "item", value = "score", LadderGroup_1, LadderSelf_1) %>%
  convert_as_factor(ID, Edu_1, Income_1, item)

long_b$Edu<-as.numeric(long_b$Edu_1)
long_b$Income<-as.numeric(long_b$Income_1)

model_black<-lmer(score ~ item + Edu + Income + (1|ID), data = long_b)
summary(model_black)
anova(model_black)
emmeans(model_black, ~item)


## Time 1 Correlations - White ----

library(corrtable)
corr_vars_white<- d_white %>% select(LadderSelf_1, LadderGroup_1, LadderDif.1, Income_1, Edu_1, negEmo.1, posEmo.1, dep.1, gHealth.1, gSleep.1)
correlation_matrix(corr_vars_white, digits = 3, use = 'lower', replace_diagonal = T)

## Time 1 Correlations - Black ----

corr_vars_black<- d_black %>% select(LadderSelf_1, LadderGroup_1, LadderDif.1, Income_1, Edu_1, negEmo.1, posEmo.1, dep.1, gHealth.1, gSleep.1)
correlation_matrix(corr_vars_black, digits = 3, use = 'upper', replace_diagonal = T)

## Time 1 Plot - Race Status Comparisons ----
table(d_white$Racecomp_1)
white_comp<-dplyr::select(d_white, PSBlack, Racecomp_1)
black_comp<-dplyr::select(d_black, PSBlack, Racecomp_1)
white_comp$RaceCompF <- cut(white_comp$Racecomp_1, 6, labels=c("White", "Black", "Native American or Pacific Islander", "Asian", "Multiracial", "Another Race"))
black_comp$RaceCompF <- cut(black_comp$Racecomp_1, 6, labels=c("White", "Black", "Native American or Pacific Islander", "Asian", "Multiracial", "Another Race"))
comp_race<-rbind(white_comp, black_comp)
tb1<-as.data.frame(with(white_comp, prop.table(table(RaceCompF))))
tb1$Race<-"White"
tb2<-as.data.frame(with(black_comp, prop.table(table(RaceCompF))))
tb2$Race<-"Black"
tb3<-rbind(as.data.frame(tb1), as.data.frame(tb2))

png("compareRace.png", width = 2*6.5, height = 2*4.5, units = "in", res = 300)
ggplot(data=tb3, aes(x=Freq, y = RaceCompF, fill = Race)) +
  geom_col(position = "dodge")+
  xlab("Proportion")+
  ylab("Comparison Race") +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0.3939)+
  scale_fill_grey()+
  xlim(0, 1)+
  theme(text = element_text(size = 16, color = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.y = element_text(angle = 30),
        panel.background = element_rect(fill = 'white', color = 'white'),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white', linewidth = 2))+
  guides(fill=guide_legend(title="Participant Race"))
dev.off()

## Income Moderation at T1 ----
d_white$GenderBinary<-ifelse(d_white$Gender_1 == 1, 0,
                             ifelse(d_white$Gender_1 == 2, 1, NA))

summary(lm(posEmo.1 ~ LadderDif.1 * Income_1 + GenderBinary + Edu_1 + Age_1, data = d_white))


d_black$GenderBinary<-ifelse(d_black$Gender_1 == 1, 0,
                             ifelse(d_black$Gender_1 == 2, 1, NA))

summary(lm(posEmo.1 ~ LadderDif.1 * Income_1 + GenderBinary + Edu_1 + Age_1, data = d_black))


# Write out data for longitudinal analyses ----------------------------
write.csv(d_black, "../blackCalcVar.csv")
write.csv(d_white, "../whiteCalcVar.csv") # transpose headers from each file, label BLACK and WHITE, save that as headersBW.csv
# Assessing Missingness ----------------------------------

## Combining Black & White datasets ----

# Steps for making sure all the variables are in the same order across datasets:
# transpose headers from blackCalcVar.csv and whiteCalcVar.csv, label BLACK and WHITE, save that as headersBW.csv
# headersBW<-read.csv("~/Downloads/headersBW.csv")
# headersBW$match<-ifelse(headersBW$WHITE == headersBW$BLACK, 1, 0)
# table(headersBW$match) # great, the headers match across datasets. We can use rbind! 
d<-rbind(d_black, d_white)
write.csv(d, file = "longSESbw.csv")

# START HERE FOR MISSINGNESS -----------------------
#d<-read.csv("../Step3/longSESbw.csv")

library(naniar)
dim(d)                    # dimensions of the data
prod(dim(d))              # number of cells
n_miss(d)                 # How many are missing?
n_miss(d)/prod(dim(d))    # proportion missing (overall)

miss_var_summary(d) # sorted list by variable (lots of the optional text at top)
# gg_miss_var(d) Too many variables to visualize

################################################################################
# Missing Key Variables
################################################################################
# Which key variables are affected? (Constant stream suggests full non-response)
d %>% select(starts_with("LadderSelf")) %>% is.na() %>% colMeans()
d %>% select(starts_with("LadderGroup")) %>% is.na() %>% colMeans()
d %>% select(starts_with("LadderDif")) %>% is.na() %>% colMeans()
d %>% select(starts_with("PosEmo")) %>% is.na() %>% colMeans()
d %>% select(starts_with("dep")) %>% is.na() %>% colMeans()
d %>% select(starts_with("gSleep")) %>% is.na() %>% colMeans()
d %>% select(starts_with("gHealth")) %>% is.na() %>% colMeans()

dsub <- d %>% select(starts_with("LadderDif"),
                     starts_with("PosEmo"),
                     starts_with("dep"),
                     starts_with("gSleep"),
                     starts_with("gHealth"),
                     "PSBlack","GenderBinary",
                     starts_with("Age"),
                     starts_with("Edu"),
                     starts_with("Income")) 

dim(dsub)                    # dimensions of the data
prod(dim(dsub))              # number of cells
n_miss(dsub)                 # How many are missing?
n_miss(dsub)/prod(dim(dsub)) # proportion missing (overall)

miss_var_summary(dsub) # sorted list by variable (lots of the optional text at top)
gg_miss_var(dsub)

################################################################################
# Little's MCAR test
################################################################################
d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "PSBlack","GenderBinary",
             starts_with("Age"),
             starts_with("Edu"),
             starts_with("Income")) %>%
  mcar_test() 

################################################################################
# Predicting missingness
################################################################################

# binary variables for LadderDif missing
d$LD1_m<-ifelse(is.na(d$LadderDif.1), 1, 0) # .001% missing
d$LD2_m<-ifelse(is.na(d$LadderDif.2), 1, 0) # 30.11% missing
d$LD3_m<-ifelse(is.na(d$LadderDif.3), 1, 0) # 43.43% missing
d$LD4_m<-ifelse(is.na(d$LadderDif.4), 1, 0) # 48.26% missing
d$LD5_m<-ifelse(is.na(d$LadderDif.5), 1, 0) # 50.28% missing

# binary variables for PosEmo missing
d$PE1_m<-ifelse(is.na(d$posEmo.1), 1, 0) # missing
d$PE2_m<-ifelse(is.na(d$posEmo.2), 1, 0) # 29.97% missing
d$PE3_m<-ifelse(is.na(d$posEmo.3), 1, 0) # 43.55% missing
d$PE4_m<-ifelse(is.na(d$posEmo.4), 1, 0) # 48.26% missing
d$PE5_m<-ifelse(is.na(d$posEmo.5), 1, 0) # 50.28% missing

# binary variables for Depression missing
d$D1_m<-ifelse(is.na(d$dep.1), 1, 0) # missing
d$D2_m<-ifelse(is.na(d$dep.2), 1, 0) # 29.97% missing
d$D3_m<-ifelse(is.na(d$dep.3), 1, 0) # 43.55% missing
d$D4_m<-ifelse(is.na(d$dep.4), 1, 0) # 48.26% missing
d$D5_m<-ifelse(is.na(d$dep.5), 1, 0) # 50.28% missing

# binary variables for Sleep missing
d$S1_m<-ifelse(is.na(d$gSleep.1), 1, 0) # missing
d$S2_m<-ifelse(is.na(d$gSleep.2), 1, 0) # 29.97% missing
d$S3_m<-ifelse(is.na(d$gSleep.3), 1, 0) # 43.55% missing
d$S4_m<-ifelse(is.na(d$gSleep.4), 1, 0) # 48.26% missing
d$S5_m<-ifelse(is.na(d$gSleep.5), 1, 0) # 50.28% missing

# binary variables for Health missing
d$H1_m<-ifelse(is.na(d$gHealth.1), 1, 0) # missing
d$H2_m<-ifelse(is.na(d$gHealth.2), 1, 0) # 29.97% missing
d$H3_m<-ifelse(is.na(d$gHealth.3), 1, 0) # 43.43% missing
d$H4_m<-ifelse(is.na(d$gHealth.4), 1, 0) # 48.26% missing
d$H5_m<-ifelse(is.na(d$gHealth.5), 1, 0) # 50.39% missing

d$GenderMF<-ifelse(d$Gender_1 == 1, 0,
                   ifelse(d$Gender_1 == 2, 1, NA))

library(e1071)
modglm<-glm(LD5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d, family = "binomial")
modsvm<-svm(LD5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d)
pred <- predict(modsvm) > 0.50
tab <-table(modglm$model$LD5_m, pred)
(tab[1,1]+tab[2,2])/884 # svm -- very similar across outcomes

modglm<-glm(PE5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d, family = "binomial")
modsvm<-svm(PE5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d)
pred <- predict(modsvm) > 0.50
tab <-table(modglm$model$PE5_m, pred)
(tab[1,1]+tab[2,2])/884 # svm -- very similar across outcomes

modglm<-glm(D5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d, family = "binomial")
modsvm<-svm(D5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d)
pred <- predict(modsvm) > 0.50
tab <-table(modglm$model$D5_m, pred)
(tab[1,1]+tab[2,2])/884 # svm -- very similar across outcomes

modglm<-glm(S5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d, family = "binomial")
modsvm<-svm(S5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d)

pred <- predict(modsvm) > 0.50
tab <-table(modglm$model$S5_m, pred)
(tab[1,1]+tab[2,2])/884 # svm -- very similar across outcomes

modglm<-glm(H5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d, family = "binomial")
modsvm<-svm(H5_m ~ PSBlack + Edu_1 + Age_1 + GenderMF + Income_1+
              LadderDif.1+posEmo.1+dep.1+gSleep.1+gHealth.1, 
            data = d)
pred <- predict(modsvm) > 0.50
tab <-table(modglm$model$H5_m, pred)
(tab[1,1]+tab[2,2])/884 # svm -- very similar across outcomes

  
# Is missingness related to race? - yes: all except for 1st wave are more likely to be missing for Black respondents
summary(glm(LD1_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(LD2_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(LD3_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(LD4_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(LD5_m ~ PSBlack, data = d, family = "binomial"))

summary(glm(PE1_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(PE2_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(PE3_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(PE4_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(PE5_m ~ PSBlack, data = d, family = "binomial"))

summary(glm(D1_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(D2_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(D3_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(D4_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(D5_m ~ PSBlack, data = d, family = "binomial"))

summary(glm(S1_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(S2_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(S3_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(S4_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(S5_m ~ PSBlack, data = d, family = "binomial"))

summary(glm(H1_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(H2_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(H3_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(H4_m ~ PSBlack, data = d, family = "binomial"))
summary(glm(H5_m ~ PSBlack, data = d, family = "binomial"))

# Is missingness related to Education? - yes: all except Wave 1 are less likely to be missing as education increases

summary(glm(LD1_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(LD2_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(LD3_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(LD4_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(LD5_m ~ Edu_1, data = d, family = "binomial"))

summary(glm(PE1_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(PE2_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(PE3_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(PE4_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(PE5_m ~ Edu_1, data = d, family = "binomial"))

summary(glm(D1_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(D2_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(D3_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(D4_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(D5_m ~ Edu_1, data = d, family = "binomial"))

summary(glm(S1_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(S2_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(S3_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(S4_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(S5_m ~ Edu_1, data = d, family = "binomial"))

summary(glm(H1_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(H2_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(H3_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(H4_m ~ Edu_1, data = d, family = "binomial"))
summary(glm(H5_m ~ Edu_1, data = d, family = "binomial"))

# Is missingness related to Age? - yes: all except W1 less likely to be missing as age increases

summary(glm(LD1_m ~ Age_1, data = d, family = "binomial"))
summary(glm(LD2_m ~ Age_1, data = d, family = "binomial"))
summary(glm(LD3_m ~ Age_1, data = d, family = "binomial"))
summary(glm(LD4_m ~ Age_1, data = d, family = "binomial"))
summary(glm(LD5_m ~ Age_1, data = d, family = "binomial"))

summary(glm(PE1_m ~ Age_1, data = d, family = "binomial"))
summary(glm(PE2_m ~ Age_1, data = d, family = "binomial"))
summary(glm(PE3_m ~ Age_1, data = d, family = "binomial"))
summary(glm(PE4_m ~ Age_1, data = d, family = "binomial"))
summary(glm(PE5_m ~ Age_1, data = d, family = "binomial"))

summary(glm(D1_m ~ Age_1, data = d, family = "binomial"))
summary(glm(D2_m ~ Age_1, data = d, family = "binomial"))
summary(glm(D3_m ~ Age_1, data = d, family = "binomial"))
summary(glm(D4_m ~ Age_1, data = d, family = "binomial"))
summary(glm(D5_m ~ Age_1, data = d, family = "binomial"))

summary(glm(S1_m ~ Age_1, data = d, family = "binomial"))
summary(glm(S2_m ~ Age_1, data = d, family = "binomial"))
summary(glm(S3_m ~ Age_1, data = d, family = "binomial"))
summary(glm(S4_m ~ Age_1, data = d, family = "binomial"))
summary(glm(S5_m ~ Age_1, data = d, family = "binomial"))

summary(glm(H1_m ~ Age_1, data = d, family = "binomial"))
summary(glm(H2_m ~ Age_1, data = d, family = "binomial"))
summary(glm(H3_m ~ Age_1, data = d, family = "binomial"))
summary(glm(H4_m ~ Age_1, data = d, family = "binomial"))
summary(glm(H5_m ~ Age_1, data = d, family = "binomial"))

# Is missingness related to Gender? - No, not related to male vs female gender
summary(glm(LD1_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(LD2_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(LD3_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(LD4_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(LD5_m ~ GenderMF, data = d, family = "binomial"))

summary(glm(PE1_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(PE2_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(PE3_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(PE4_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(PE5_m ~ GenderMF, data = d, family = "binomial"))

summary(glm(D1_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(D2_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(D3_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(D4_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(D5_m ~ GenderMF, data = d, family = "binomial"))

summary(glm(S1_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(S2_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(S3_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(S4_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(S5_m ~ GenderMF, data = d, family = "binomial"))

summary(glm(H1_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(H2_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(H3_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(H4_m ~ GenderMF, data = d, family = "binomial"))
summary(glm(H5_m ~ GenderMF, data = d, family = "binomial"))

# Is missingness related to Income? - Only at Wave 5 

summary(glm(LD1_m ~ Income_1, data = d, family = "binomial"))
summary(glm(LD2_m ~ Income_1, data = d, family = "binomial"))
summary(glm(LD3_m ~ Income_1, data = d, family = "binomial"))
summary(glm(LD4_m ~ Income_1, data = d, family = "binomial"))
summary(glm(LD5_m ~ Income_1, data = d, family = "binomial")) # sig

summary(glm(PE1_m ~ Income_1, data = d, family = "binomial"))
summary(glm(PE2_m ~ Income_1, data = d, family = "binomial"))
summary(glm(PE3_m ~ Income_1, data = d, family = "binomial"))
summary(glm(PE4_m ~ Income_1, data = d, family = "binomial"))
summary(glm(PE5_m ~ Income_1, data = d, family = "binomial")) # sig

summary(glm(D1_m ~ Income_1, data = d, family = "binomial"))
summary(glm(D2_m ~ Income_1, data = d, family = "binomial"))
summary(glm(D3_m ~ Income_1, data = d, family = "binomial"))
summary(glm(D4_m ~ Income_1, data = d, family = "binomial"))
summary(glm(D5_m ~ Income_1, data = d, family = "binomial")) # sig

summary(glm(S1_m ~ Income_1, data = d, family = "binomial"))
summary(glm(S2_m ~ Income_1, data = d, family = "binomial"))
summary(glm(S3_m ~ Income_1, data = d, family = "binomial"))
summary(glm(S4_m ~ Income_1, data = d, family = "binomial"))
summary(glm(S5_m ~ Income_1, data = d, family = "binomial")) # sig

summary(glm(H1_m ~ Income_1, data = d, family = "binomial"))
summary(glm(H2_m ~ Income_1, data = d, family = "binomial"))
summary(glm(H3_m ~ Income_1, data = d, family = "binomial"))
summary(glm(H4_m ~ Income_1, data = d, family = "binomial"))
summary(glm(H5_m ~ Income_1, data = d, family = "binomial")) # sig

################################################################################
# What does it look like by demographics?
################################################################################
d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "PSBlack") %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  gg_miss_fct(fct=PSBlack)


d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "Edu_1") %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  gg_miss_fct(fct=Edu_1)

d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "Age_1") %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  gg_miss_fct(fct=AgeCat)

d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "GenderBinary") %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  gg_miss_fct(fct=GenderBinary)


d %>% select(starts_with("LadderDif"),
             starts_with("PosEmo"),
             starts_with("dep"),
             starts_with("gSleep"),
             starts_with("gHealth"),
             "Income_1") %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  gg_miss_fct(fct=Income_1)

################################################################################
# Race -- distributions of missing v non-missing
################################################################################
dLD <- d %>% 
  select(c(starts_with("LadderDif"), 
           "LD1_m","LD2_m","LD3_m","LD4_m","LD5_m",
           "PSBlack")) %>%
  mutate(LDFIRST = LadderDif.1) %>%
  pivot_longer(c(starts_with("LadderDif")),
               values_to = "LadderDif",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ LD1_m,
                             Wave==2 ~ LD2_m,
                             Wave==3 ~ LD3_m,
                             Wave==4 ~ LD4_m,
                             Wave==5 ~ LD5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  select(Wave, LDFIRST, Missing, PSBlack) %>%
  filter(Wave != "1")

ggplot(data=dLD, aes(x=Wave, y=LDFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Ladder Difference")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Ladder Difference by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)



dPE <- d %>% 
  select(c(starts_with("posEmo"), 
           "PE1_m","PE2_m","PE3_m","PE4_m","PE5_m",
           "PSBlack")) %>%
  mutate(PEFIRST = posEmo.1) %>%
  pivot_longer(c(starts_with("posEmo")),
               values_to = "posEmo",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ PE1_m,
                             Wave==2 ~ PE2_m,
                             Wave==3 ~ PE3_m,
                             Wave==4 ~ PE4_m,
                             Wave==5 ~ PE5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  select(Wave, PEFIRST, Missing, PSBlack) %>%
  filter(Wave != "1")

ggplot(data=dPE, aes(x=Wave, y=PEFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Positive Emotions")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Positive Emotions by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)


dDep <- d %>% 
  select(c(starts_with("dep"), 
           "D1_m","D2_m","D3_m","D4_m","D5_m",
           "PSBlack")) %>%
  mutate(DFIRST = dep.1) %>%
  pivot_longer(c(starts_with("dep")),
               values_to = "dep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ D1_m,
                             Wave==2 ~ D2_m,
                             Wave==3 ~ D3_m,
                             Wave==4 ~ D4_m,
                             Wave==5 ~ D5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  select(Wave, DFIRST, Missing, PSBlack) %>%
  filter(Wave != "1")

ggplot(data=dDep, aes(x=Wave, y=DFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Depression")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Depression by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)


dS <- d %>% 
  select(c(starts_with("gSleep"), 
           "S1_m","S2_m","S3_m","S4_m","S5_m",
           "PSBlack")) %>%
  mutate(SFIRST = gSleep.1) %>%
  pivot_longer(c(starts_with("gSleep")),
               values_to = "gSleep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ S1_m,
                             Wave==2 ~ S2_m,
                             Wave==3 ~ S3_m,
                             Wave==4 ~ S4_m,
                             Wave==5 ~ S5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  select(Wave, SFIRST, Missing, PSBlack) %>%
  filter(Wave != "1")

ggplot(data=dS, aes(x=Wave, y=SFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Sleep Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)

dH <- d %>% 
  select(c(starts_with("gHealth"), 
           "H1_m","H2_m","H3_m","H4_m","H5_m",
           "PSBlack")) %>%
  mutate(HFIRST = gHealth.1) %>%
  pivot_longer(c(starts_with("gHealth")),
               values_to = "gHealth",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ H1_m,
                             Wave==2 ~ H2_m,
                             Wave==3 ~ H3_m,
                             Wave==4 ~ H4_m,
                             Wave==5 ~ H5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(PSBlack=factor(PSBlack, levels=c(0,1), labels=c("White", "Black"))) %>%
  select(Wave, HFIRST, Missing, PSBlack) %>%
  filter(Wave != "1")

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Pysical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)
################################################################################
# Age  -- distributions of missing v non-missing
################################################################################
dLD <- d %>% 
  select(c(starts_with("LadderDif"), 
           "LD1_m","LD2_m","LD3_m","LD4_m","LD5_m",
           "Age_1")) %>%
  mutate(LDFIRST = LadderDif.1) %>%
  pivot_longer(c(starts_with("LadderDif")),
               values_to = "LadderDif",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ LD1_m,
                             Wave==2 ~ LD2_m,
                             Wave==3 ~ LD3_m,
                             Wave==4 ~ LD4_m,
                             Wave==5 ~ LD5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  select(Wave, LDFIRST, Missing, AgeCat) %>%
  filter(Wave != "1")

ggplot(data=dLD, aes(x=Wave, y=LDFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Ladder Difference")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Ladder Difference by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~AgeCat)


dPE <- d %>% 
  select(c(starts_with("posEmo"), 
           "PE1_m","PE2_m","PE3_m","PE4_m","PE5_m",
           "Age_1")) %>%
  mutate(PEFIRST = posEmo.1) %>%
  pivot_longer(c(starts_with("posEmo")),
               values_to = "posEmo",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ PE1_m,
                             Wave==2 ~ PE2_m,
                             Wave==3 ~ PE3_m,
                             Wave==4 ~ PE4_m,
                             Wave==5 ~ PE5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  select(Wave, PEFIRST, Missing, AgeCat) %>%
  filter(Wave != "1")

ggplot(data=dPE, aes(x=Wave, y=PEFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Positive Emotions")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Positive Emotions by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~AgeCat)


dDep <- d %>% 
  select(c(starts_with("dep"), 
           "D1_m","D2_m","D3_m","D4_m","D5_m",
           "Age_1")) %>%
  mutate(DFIRST = dep.1) %>%
  pivot_longer(c(starts_with("dep")),
               values_to = "dep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ D1_m,
                             Wave==2 ~ D2_m,
                             Wave==3 ~ D3_m,
                             Wave==4 ~ D4_m,
                             Wave==5 ~ D5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  select(Wave, DFIRST, Missing, AgeCat) %>%
  filter(Wave != "1")

ggplot(data=dDep, aes(x=Wave, y=DFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Depression")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Depression by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~AgeCat)


dS <- d %>% 
  select(c(starts_with("gSleep"), 
           "S1_m","S2_m","S3_m","S4_m","S5_m",
           "Age_1")) %>%
  mutate(SFIRST = gSleep.1) %>%
  pivot_longer(c(starts_with("gSleep")),
               values_to = "gSleep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ S1_m,
                             Wave==2 ~ S2_m,
                             Wave==3 ~ S3_m,
                             Wave==4 ~ S4_m,
                             Wave==5 ~ S5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  select(Wave, SFIRST, Missing, AgeCat) %>%
  filter(Wave != "1")

ggplot(data=dS, aes(x=Wave, y=SFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Sleep Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~AgeCat)

dH <- d %>% 
  select(c(starts_with("gHealth"), 
           "H1_m","H2_m","H3_m","H4_m","H5_m",
           "Age_1")) %>%
  mutate(HFIRST = gHealth.1) %>%
  pivot_longer(c(starts_with("gHealth")),
               values_to = "gHealth",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ H1_m,
                             Wave==2 ~ H2_m,
                             Wave==3 ~ H3_m,
                             Wave==4 ~ H4_m,
                             Wave==5 ~ H5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(AgeCat = case_when(Age_1 < 25 ~ "<25",
                            Age_1 < 35 ~ "25-34",
                            Age_1 < 45 ~ "35-44",
                            TRUE       ~ ">44")) %>%
  mutate(AgeCat=factor(AgeCat, levels=c("<25", "25-34", "35-44", ">44"))) %>%
  select(Wave, HFIRST, Missing, AgeCat) %>%
  filter(Wave != "1")

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Physical Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Physical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~AgeCat)

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Physical Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Physical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~PSBlack)

################################################################################
# Gender -- distributions of missing v non-missing
################################################################################
dLD <- d %>% 
  select(c(starts_with("LadderDif"), 
           "LD1_m","LD2_m","LD3_m","LD4_m","LD5_m",
           "GenderBinary")) %>%
  mutate(LDFIRST = LadderDif.1) %>%
  pivot_longer(c(starts_with("LadderDif")),
               values_to = "LadderDif",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ LD1_m,
                             Wave==2 ~ LD2_m,
                             Wave==3 ~ LD3_m,
                             Wave==4 ~ LD4_m,
                             Wave==5 ~ LD5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  select(Wave, LDFIRST, Missing, GenderBinary) %>%
  filter(Wave != "1")

ggplot(data=dLD, aes(x=Wave, y=LDFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Ladder Difference")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Ladder Difference by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~GenderBinary)



dPE <- d %>% 
  select(c(starts_with("posEmo"), 
           "PE1_m","PE2_m","PE3_m","PE4_m","PE5_m",
           "GenderBinary")) %>%
  mutate(PEFIRST = posEmo.1) %>%
  pivot_longer(c(starts_with("posEmo")),
               values_to = "posEmo",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ PE1_m,
                             Wave==2 ~ PE2_m,
                             Wave==3 ~ PE3_m,
                             Wave==4 ~ PE4_m,
                             Wave==5 ~ PE5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  select(Wave, PEFIRST, Missing, GenderBinary) %>%
  filter(Wave != "1")

ggplot(data=dPE, aes(x=Wave, y=PEFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Positive Emotions")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Positive Emotions by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~GenderBinary)


dDep <- d %>% 
  select(c(starts_with("dep"), 
           "D1_m","D2_m","D3_m","D4_m","D5_m",
           "GenderBinary")) %>%
  mutate(DFIRST = dep.1) %>%
  pivot_longer(c(starts_with("dep")),
               values_to = "dep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ D1_m,
                             Wave==2 ~ D2_m,
                             Wave==3 ~ D3_m,
                             Wave==4 ~ D4_m,
                             Wave==5 ~ D5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  select(Wave, DFIRST, Missing, GenderBinary) %>%
  filter(Wave != "1")

ggplot(data=dDep, aes(x=Wave, y=DFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Depression")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Depression by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~GenderBinary)


dS <- d %>% 
  select(c(starts_with("gSleep"), 
           "S1_m","S2_m","S3_m","S4_m","S5_m",
           "GenderBinary")) %>%
  mutate(SFIRST = gSleep.1) %>%
  pivot_longer(c(starts_with("gSleep")),
               values_to = "gSleep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ S1_m,
                             Wave==2 ~ S2_m,
                             Wave==3 ~ S3_m,
                             Wave==4 ~ S4_m,
                             Wave==5 ~ S5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  select(Wave, SFIRST, Missing, GenderBinary) %>%
  filter(Wave != "1")

ggplot(data=dS, aes(x=Wave, y=SFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Sleep Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~GenderBinary)

dH <- d %>% 
  select(c(starts_with("gHealth"), 
           "H1_m","H2_m","H3_m","H4_m","H5_m",
           "GenderBinary")) %>%
  mutate(HFIRST = gHealth.1) %>%
  pivot_longer(c(starts_with("gHealth")),
               values_to = "gHealth",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ H1_m,
                             Wave==2 ~ H2_m,
                             Wave==3 ~ H3_m,
                             Wave==4 ~ H4_m,
                             Wave==5 ~ H5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(GenderBinary=case_when(is.na(GenderBinary)~2,
                                TRUE ~ GenderBinary))%>%
  mutate(GenderBinary=factor(GenderBinary, levels=c(0,1,2), labels=c("Woman(0)", "Man(1)", "Another Gender"))) %>%
  select(Wave, HFIRST, Missing, GenderBinary) %>%
  filter(Wave != "1")

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Physical Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Physical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~GenderBinary)

################################################################################
# Education -- distributions of missing v non-missing
################################################################################

dLD <- d %>% 
  select(c(starts_with("LadderDif"), 
           "LD1_m","LD2_m","LD3_m","LD4_m","LD5_m",
           "Edu_1")) %>%
  mutate(LDFIRST = LadderDif.1) %>%
  pivot_longer(c(starts_with("LadderDif")),
               values_to = "LadderDif",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ LD1_m,
                             Wave==2 ~ LD2_m,
                             Wave==3 ~ LD3_m,
                             Wave==4 ~ LD4_m,
                             Wave==5 ~ LD5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  select(Wave, LDFIRST, Missing, Edu_1) %>%
  filter(Wave != "1")

ggplot(data=dLD, aes(x=Wave, y=LDFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Ladder Difference")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Ladder Difference by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Edu_1)



dPE <- d %>% 
  select(c(starts_with("posEmo"), 
           "PE1_m","PE2_m","PE3_m","PE4_m","PE5_m",
           "Edu_1")) %>%
  mutate(PEFIRST = posEmo.1) %>%
  pivot_longer(c(starts_with("posEmo")),
               values_to = "posEmo",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ PE1_m,
                             Wave==2 ~ PE2_m,
                             Wave==3 ~ PE3_m,
                             Wave==4 ~ PE4_m,
                             Wave==5 ~ PE5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  select(Wave, PEFIRST, Missing, Edu_1) %>%
  filter(Wave != "1")

ggplot(data=dPE, aes(x=Wave, y=PEFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Positive Emotions")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Positive Emotions by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Edu_1)


dDep <- d %>% 
  select(c(starts_with("dep"), 
           "D1_m","D2_m","D3_m","D4_m","D5_m",
           "Edu_1")) %>%
  mutate(DFIRST = dep.1) %>%
  pivot_longer(c(starts_with("dep")),
               values_to = "dep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ D1_m,
                             Wave==2 ~ D2_m,
                             Wave==3 ~ D3_m,
                             Wave==4 ~ D4_m,
                             Wave==5 ~ D5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  select(Wave, DFIRST, Missing, Edu_1) %>%
  filter(Wave != "1")

ggplot(data=dDep, aes(x=Wave, y=DFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Depression")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Depression by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Edu_1)


dS <- d %>% 
  select(c(starts_with("gSleep"), 
           "S1_m","S2_m","S3_m","S4_m","S5_m",
           "Edu_1")) %>%
  mutate(SFIRST = gSleep.1) %>%
  pivot_longer(c(starts_with("gSleep")),
               values_to = "gSleep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ S1_m,
                             Wave==2 ~ S2_m,
                             Wave==3 ~ S3_m,
                             Wave==4 ~ S4_m,
                             Wave==5 ~ S5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  select(Wave, SFIRST, Missing, Edu_1) %>%
  filter(Wave != "1")

ggplot(data=dS, aes(x=Wave, y=SFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Sleep Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Edu_1)

dH <- d %>% 
  select(c(starts_with("gHealth"), 
           "H1_m","H2_m","H3_m","H4_m","H5_m",
           "Edu_1")) %>%
  mutate(HFIRST = gHealth.1) %>%
  pivot_longer(c(starts_with("gHealth")),
               values_to = "gHealth",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ H1_m,
                             Wave==2 ~ H2_m,
                             Wave==3 ~ H3_m,
                             Wave==4 ~ H4_m,
                             Wave==5 ~ H5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Edu_1=factor(Edu_1, levels=1:7, labels=c("< High School", "High School", "Some College", "2-Year College", "4-Year College", "Masters-Level", "Doctorate-Level"))) %>%
  select(Wave, HFIRST, Missing, Edu_1) %>%
  filter(Wave != "1")

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Physical Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Physical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Edu_1)


################################################################################
# Income -- distributions of missing v non-missing
################################################################################

dLD <- d %>% 
  select(c(starts_with("LadderDif"), 
           "LD1_m","LD2_m","LD3_m","LD4_m","LD5_m",
           "Income_1")) %>%
  mutate(LDFIRST = LadderDif.1) %>%
  pivot_longer(c(starts_with("LadderDif")),
               values_to = "LadderDif",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ LD1_m,
                             Wave==2 ~ LD2_m,
                             Wave==3 ~ LD3_m,
                             Wave==4 ~ LD4_m,
                             Wave==5 ~ LD5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  select(Wave, LDFIRST, Missing, Income_1) %>%
  filter(Wave != "1")

ggplot(data=dLD, aes(x=Wave, y=LDFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Ladder Difference")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Ladder Difference by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Income_1)



dPE <- d %>% 
  select(c(starts_with("posEmo"), 
           "PE1_m","PE2_m","PE3_m","PE4_m","PE5_m",
           "Income_1")) %>%
  mutate(PEFIRST = posEmo.1) %>%
  pivot_longer(c(starts_with("posEmo")),
               values_to = "posEmo",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ PE1_m,
                             Wave==2 ~ PE2_m,
                             Wave==3 ~ PE3_m,
                             Wave==4 ~ PE4_m,
                             Wave==5 ~ PE5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  select(Wave, PEFIRST, Missing, Income_1) %>%
  filter(Wave != "1")

ggplot(data=dPE, aes(x=Wave, y=PEFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Positive Emotions")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Positive Emotions by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Income_1)


dDep <- d %>% 
  select(c(starts_with("dep"), 
           "D1_m","D2_m","D3_m","D4_m","D5_m",
           "Income_1")) %>%
  mutate(DFIRST = dep.1) %>%
  pivot_longer(c(starts_with("dep")),
               values_to = "dep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ D1_m,
                             Wave==2 ~ D2_m,
                             Wave==3 ~ D3_m,
                             Wave==4 ~ D4_m,
                             Wave==5 ~ D5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  select(Wave, DFIRST, Missing, Income_1) %>%
  filter(Wave != "1")

ggplot(data=dDep, aes(x=Wave, y=DFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Depression")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Depression by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Income_1)


dS <- d %>% 
  select(c(starts_with("gSleep"), 
           "S1_m","S2_m","S3_m","S4_m","S5_m",
           "Income_1")) %>%
  mutate(SFIRST = gSleep.1) %>%
  pivot_longer(c(starts_with("gSleep")),
               values_to = "gSleep",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ S1_m,
                             Wave==2 ~ S2_m,
                             Wave==3 ~ S3_m,
                             Wave==4 ~ S4_m,
                             Wave==5 ~ S5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  select(Wave, SFIRST, Missing, Income_1) %>%
  filter(Wave != "1")

ggplot(data=dS, aes(x=Wave, y=SFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Sleep Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Sleep Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Income_1)

dH <- d %>% 
  select(c(starts_with("gHealth"), 
           "H1_m","H2_m","H3_m","H4_m","H5_m",
           "Income_1")) %>%
  mutate(HFIRST = gHealth.1) %>%
  pivot_longer(c(starts_with("gHealth")),
               values_to = "gHealth",
               names_to = "Wave") %>%
  mutate(Wave = factor(substr(Wave, nchar(Wave), nchar(Wave)))) %>%
  mutate(Missing = case_when(Wave==1 ~ H1_m,
                             Wave==2 ~ H2_m,
                             Wave==3 ~ H3_m,
                             Wave==4 ~ H4_m,
                             Wave==5 ~ H5_m,
                             TRUE ~ NA_real_))%>%
  mutate(Missing=factor(Missing, levels=c(0,1), labels=c("Not Missing", "Missing"))) %>%
  mutate(Income_1=factor(Income_1, levels=1:11, labels=c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k", "125k-150k", "150k-175k", "175k-200k", "200k-225k", "225k-250k", ">250k"))) %>%
  select(Wave, HFIRST, Missing, Income_1) %>%
  filter(Wave != "1")

ggplot(data=dH, aes(x=Wave, y=HFIRST, fill=Missing)) + 
  geom_violin(position = position_dodge(1)) + 
  geom_boxplot(width=0.25, position = position_dodge(1))+
  theme_bw()+
  ylab("Physical Health")+
  scale_fill_manual(values = c("lightgrey", "grey50"))+
  ggtitle("Distributions of Physical Health by Missingness",
          subtitle = "Wave 1 values are used for missing data ") + 
  facet_wrap(.~Income_1)
