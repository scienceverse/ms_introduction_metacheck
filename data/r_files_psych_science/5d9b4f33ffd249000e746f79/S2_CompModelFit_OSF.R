#### ***************************************************************************************
## Sample 2 Comparing Model Fit - OSF
## Author: Dr. ?Lauren Human
## Date: 7/25/2019
## Version 1.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
# Read in data
sd_S2n <- read.table("SDS2_OSF.csv", header = T, sep = ",")
sd_S2  <- (na.omit(sd_S2n %>% select(PID, TID, Uniqdyad, TSelfc, SMEANc, IMEANc, Ratingc, promintz)))

#### Comparing Model Fit
## Simple "Accuracy is Irrelevant" Model
simple <- lmer(Ratingc ~ (SMEANc + IMEANc) * promintz + (SMEANc + IMEANc | PID) + 
                 (SMEANc + IMEANc | TID) + (SMEANc + IMEANc | Uniqdyad), 
               data= sd_S2, control = lmerControl(optimizer = "bobyqa")) 
extractAIC(simple)

## Full "Accuracy Matters" Model (SAM)
full <- lmer(Ratingc ~ (TSelfc + SMEANc + IMEANc) * promintz + (TSelfc + SMEANc + IMEANc | PID) + 
               (TSelfc + SMEANc + IMEANc | TID) + (TSelfc + SMEANc + IMEANc | Uniqdyad), 
             data= sd_S2, control = lmerControl(optimizer = "bobyqa")) 
extractAIC(full)

## Compare with Anova
anova(simple, full)
