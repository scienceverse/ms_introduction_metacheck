#### ***************************************************************************************
## Sample 2 Wood & Furr Two-Stage Profile Correlation Approach - OSF
## Author: Lauren Gazzard
## Date: 04/23/19
## Version 1.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
library(psych)
## Read in data
sd_S2 <- read.table("S2_Profile_Correlation_Analyses.csv", header = T, sep = ",")

#### Created Shared Dyad ####
sd_S2$SDyad = floor(sd_S2$PID/10 * sd_S2$TID/10)

#### Distinctive Rating ####
## Filter by each Item and take the mean. Result = Mean of Perceiver Ratings
N_item = 26
MeanRatings = matrix(0, N_item, 2)
for (i in 1:N_item){
  sd_Temp = sd_S2 %>% filter(item == i)
  MeanRatings[i,1] = i
  MeanRatings[i,2] = mean(sd_Temp$Rating, na.rm = T)
}

RMEAN = as.data.frame(MeanRatings)
colnames(RMEAN) <- c("item", "RMEAN")

## Merge the Mean Perceiver Ratings (RMEAN) and subtract to obtain Distintice Ratings (RatingD)
sd_S2d <- merge(sd_S2, RMEAN, by = "item")

sd_S2d$RMEANc  <- sd_S2d$RMEAN - mean(sd_S2d$RMEAN, na.rm = T)
sd_S2d$RatingD <- sd_S2d$Rating - sd_S2d$RMEAN

# Check that now the mean of each item rating is 0
for (j in 1:N_item){
  print(paste("item", toString(j), sep = " "))
  print(summary((sd_S2d %>% filter(item == j))$RatingD))
}

#### Normative Profile Correlation ####
Norm_Cor <- cor(sd_S2d$RMEAN, sd_S2d$SMEAN)
Norm_Cor
#### Distinctive Profile Correlations ####
## Create array of PIDs
aPID <- as.numeric(unique(sd_S2d$PID))

## Loop through array of PIDs and filter, then filter for each TID for the selected PID.
## For the selection of PID and TID, correlate the RatingD and the TSelfc (already Distinctive)
temp <- matrix(data = NA, nrow = NROW(aPID)*20, ncol = 5)
row  <- 1
for (i in 1:NROW(aPID)) {
  # Filter PID
  fP <- aPID[i]
  sd_filter_PID <- sd_S2d %>% filter(PID == fP) %>% dplyr::select(TID, RatingD, TSelfc, SMEANc, IMEANc)
  
  aTID <- as.numeric(unique(sd_filter_PID$TID))
  for (j in 1:NROW(aTID)) {
    # Filter TID
    fT = aTID[j]
    sd_filtered <- sd_filter_PID %>% filter(TID == fT)
    sd_filtered <- sd_filtered[complete.cases(sd_filtered),]
    
    # Add PID and TID to matrix
    temp[row,1] <- aPID[i]
    temp[row,2] <- aTID[j]
    
    # Correlation w Fischer Z
    temp[row,3] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$TSelfc))
    temp[row,4] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$SMEANc))
    temp[row,5] <- fisherz(cor(sd_filtered$RatingD, sd_filtered$IMEANc))
    
    # Iterate
    row = row + 1
  }
}

Pro_Corn <- as.data.frame(temp)
Pro_Cor  <- Pro_Corn[complete.cases(Pro_Corn),]
colnames(Pro_Cor) <- c("PID", "TID", "Dist_Cor", "SMEAN_Cor", "IMEAN_Cor")

## Merge into data
sd_S2_Cor <- merge(sd_S2, Pro_Cor, by=c("PID", "TID"), all = T)
## Keep Unique 
sd_S2_Cor_Unique <- as.data.frame(unique(sd_S2_Cor %>% dplyr::select(PID, TID, promintz, Dist_Cor, SMEAN_Cor, IMEAN_Cor, TEXz, SDyad)))

## Linear Mixed Model
summary(lmer(promintz ~ Dist_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S2_Cor_Unique))
summary(lmer(promintz ~ SMEAN_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S2_Cor_Unique))
summary(lmer(promintz ~ IMEAN_Cor + (1|PID) + (1|TID) + (1|SDyad), data = sd_S2_Cor_Unique))

## With Extraversion
# Average
summary(lmer(promintz ~ Dist_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data= sd_S2_Cor_Unique))
summary(lmer(promintz ~ SMEAN_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data = sd_S2_Cor_Unique))
summary(lmer(promintz ~ IMEAN_Cor * TEXz + (1|PID) + (1|TID) + (1|SDyad), data = sd_S2_Cor_Unique))
# Low & High
sd_S2_Cor_Unique$TEXl <- sd_S2_Cor_Unique$TEXz + 1
sd_S2_Cor_Unique$TEXh <- sd_S2_Cor_Unique$TEXz - 1
summary(lmer(promintz ~ Dist_Cor * TEXl + (1|PID) + (1|TID) + (1|SDyad), data= sd_S2_Cor_Unique))
summary(lmer(promintz ~ Dist_Cor * TEXh + (1|PID) + (1|TID) + (1|SDyad), data= sd_S2_Cor_Unique))
