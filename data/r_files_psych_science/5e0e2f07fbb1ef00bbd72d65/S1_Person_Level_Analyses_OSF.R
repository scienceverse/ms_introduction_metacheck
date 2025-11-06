#### ***************************************************************************************
## Sample 1 Person-Level Analyses - OSF
## Author: Lauren Gazzard Kerr
## Date: 12/16/2019
## Version 2.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
# Setup Data
sd_S1 <- read.table("SDS1_OSF.csv", header = T, sep = ",")

## Save Out Accuracy
Main_Model <- lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc) * promintz + (TSelfc  + SMEANc + SDMEANc | PID) + 
               (TSelfc  + SMEANc + SDMEANc | TID) + (0 + TSelfc  + SMEANc + SDMEANc | Uniqdyad),
               data= sd_S1, control = lmerControl(optimizer = "bobyqa")) 

summary(Main_Model)

# By Perceiver
PID_Output <- as.data.frame(ranef(Main_Model)$PID)
colnames(PID_Output)<-c("PID_Intercept", "PID_D","PID_N", "PID_SD")
PID_Output$PID <- as.numeric(rownames(PID_Output))
# By Target
TID_Output <- as.data.frame(ranef(Main_Model)$TID)
colnames(TID_Output)<-c("TID_Intercept", "TID_D","TID_N")
TID_Output$TID <- as.numeric(rownames(TID_Output))

## Merge Saved Out Values Back Into Dataframe 
df_nnn <- sd_S1 %>% select(PID, TID, Uniqdyad, promintz)
df_nn  <- df_nnn[!duplicated(df_nnn), ]
df_n   <- merge(df_nn, PID_Output, by = "PID")
df     <- merge(df_n, TID_Output, by = "TID")

## Models
# Average Perceiver Accuracy
summary(lmer(promintz ~ PID_D + (1|PID) + (1|TID), data = df))
# Average Target Accuracy 
summary(lmer(promintz ~ TID_D + (1|PID) + (1|TID), data = df))
