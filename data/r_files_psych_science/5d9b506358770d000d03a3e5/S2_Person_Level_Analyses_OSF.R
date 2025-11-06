#### ***************************************************************************************
## Sample 2 Person-Level Analyses - OSF
## Author: Lauren Gazzard Kerr
## Date: 7/25/2019
## Version 1.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
# Read in data
sd_S2 <- read.table("SDS2_OSF.csv", header = T, sep = ",")

## Save Out Accuracy
Main_Model <- lmer(Ratingc ~ (TSelfc + SMEANc + IMEANc) * promintz + (TSelfc  + SMEANc + IMEANc | PID) + 
                     (TSelfc  + SMEANc + IMEANc | TID) + (0 + TSelfc  + SMEANc + IMEANc | Uniqdyad), 
                   data= sd_S2, control = lmerControl(optimizer = "bobyqa")) 
# By Perceiver
PID_Output <- as.data.frame(ranef(Main_Model)$PID)
colnames(PID_Output)<-c("PID_Intercept", "PID_D","PID_N", "PID_I")
PID_Output$PID <- as.numeric(rownames(PID_Output))
# By Target
TID_Output <- as.data.frame(ranef(Main_Model)$TID)
colnames(TID_Output)<-c("TID_Intercept", "TID_D","TID_N", "TID_I")
TID_Output$TID <- as.numeric(rownames(TID_Output))

## Merge Saved Out Values Back Into Dataframe 
df_nnn <- sd_S2 %>% select(PID, TID, Uniqdyad, promintz)
df_nn  <- df_nnn[!duplicated(df_nnn), ]
df_n   <- merge(df_nn, PID_Output, by = "PID")
df     <- merge(df_n, TID_Output, by = "TID")

## Models
# Average Perceiver Accuracy
summary(lmer(promintz ~ PID_D + (1|PID) + (1|TID), data = df))
# Average Target Accuracy 
summary(lmer(promintz ~ TID_D + (1|PID) + (1|TID), data = df))
