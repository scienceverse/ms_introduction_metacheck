#### ***************************************************************************************
## Sample 1 Wood & Furr Two-Stage Profile Correlation Approach - OSF
## Author: Lauren Gazzard Kerr
## Date: 12/16/19
## Version 2.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
library(psych)
## Setup Data
sd_S1 <- read.table("SDS1_OSF.csv", header = T, sep = ",")
 
#### Created Shared Dyad ####
sd_S1$SDyad = floor(sd_S1$PID/10 * sd_S1$TID/10)

#### Distinctive Rating ####
## Filter by each Item and take the mean. Result = Mean of Perceiver Ratings
N_Item = 12
MeanRatings = matrix(0, N_Item, 2)
for (i in 1:N_Item){
  sd_Temp = sd_S1 %>% filter(Item == i)
  MeanRatings[i,1] = i
  MeanRatings[i,2] = mean(sd_Temp$Rating, na.rm = T)
}
RMEAN = as.data.frame(MeanRatings)
colnames(RMEAN) <- c("Item", "RMEAN")

## Merge the Mean Perceiver Ratings (RMEAN) and subtract to obtain Distintice Ratings (RatingD)
sd_S1d <- merge(sd_S1, RMEAN, by = "Item")

sd_S1d$RMEANc  <- sd_S1d$RMEAN - mean(sd_S1d$RMEAN, na.rm = T)
sd_S1d$RatingD <- sd_S1d$Rating - sd_S1d$RMEAN

# Check that now the mean of each item rating is 0
for (j in 1:N_Item){
  print(paste("Item", toString(j), sep = " "))
  print(summary((sd_S1d %>% filter(Item == j))$RatingD))
}

#### Normative Profile Correlation ####
Norm_Cor <- cor(sd_S1d$RMEAN, sd_S1d$SMEAN)
Norm_Cor
#### Distinctive Profile Correlations ####
## Create an array of PIDs
aPID <- as.numeric(unique(sd_S1d$PID))

## Loop through array of PIDs and filter, then filter for each TID for the selected PID.
## For the selection of PID and TID, correlate the RatingD and the TSelfc (already Distinctive)
temp <- matrix(data = NA, nrow = NROW(aPID)*20, ncol = 5)
row  <- 1
for (i in 1:NROW(aPID)) {
  # Filter PID
  fP <- aPID[i]
  sd_filter_PID <- sd_S1d %>% filter(PID == fP) %>% dplyr::select(TID, RatingD, TSelfc, SMEANc, SDMEANc)
  
  aTID <- as.numeric(unique(sd_filter_PID$TID))
  for (j in 1:NROW(aTID)) {
    # Filter TID
    fT = aTID[j]
    sd_filtered <- sd_filter_PID %>% filter(TID == fT)
    sd_filtered <- sd_filtered[complete.cases(sd_filtered),]
    
    # Add PID and TID to matrix
    temp[row,1] <- aPID[i]
    temp[row,2] <- aTID[j]
    
    # Correlations w Fischer Z
    temp[row,3] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$TSelfc))
    temp[row,4] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$SMEANc))
    temp[row,5] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$SDMEANc))
    
    # Iterate
    row = row + 1
  }
}

Pro_Corn <- as.data.frame(temp)
Pro_Cor  <- Pro_Corn[complete.cases(Pro_Corn),]
colnames(Pro_Cor) <- c("PID", "TID", "TSelf_Cor", "SMEAN_Cor", "SDMEAN_Cor")

## Merge into data
sd_S1_Cor <- merge(sd_S1, Pro_Cor, by=c("PID", "TID"), all = T)
## Keep only unique cases
sd_S1_Cor_Unique <- as.data.frame(unique(sd_S1_Cor %>% dplyr::select(PID, TID, promintz, TSelf_Cor, SMEAN_Cor, SDMEAN_Cor, TEXz, SDyad)))

## Linear Mixed Model
summary(lmer(promintz ~ TSelf_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S1_Cor_Unique))
summary(lmer(promintz ~ SMEAN_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S1_Cor_Unique, control = lmerControl(optimizer = "bobyqa")))
summary(lmer(promintz ~ SDMEAN_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S1_Cor_Unique, control = lmerControl(optimizer = "bobyqa")))

## With Extraversion
# Average
summary(lmer(promintz ~ TSelf_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique))
summary(lmer(promintz ~ SMEAN_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique))
summary(lmer(promintz ~ SDMEAN_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique))

# Low & High
sd_S1_Cor_Unique$TEXl <- sd_S1_Cor_Unique$TEXz + 1
sd_S1_Cor_Unique$TEXh <- sd_S1_Cor_Unique$TEXz - 1
summary(lmer(promintz ~ TSelf_Cor * TEXl + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # Low
summary(lmer(promintz ~ TSelf_Cor * TEXh + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # High
summary(lmer(promintz ~ SMEAN_Cor * TEXl + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # Low
summary(lmer(promintz ~ SMEAN_Cor * TEXh + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # High
summary(lmer(promintz ~ SDMEAN_Cor * TEXl + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # Low
summary(lmer(promintz ~ SDMEAN_Cor * TEXh + (1|PID) + (1|TID) + (1|SDyad), data= sd_S1_Cor_Unique)) # High
