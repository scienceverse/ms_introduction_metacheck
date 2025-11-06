#### ***************************************************************************************
## Sample 1 Comparing Model Fit - OSF
## Author: Dr. Lauren Human, and modified by Lauren Gazzard Kerr
## Date: 12/16/2019
## Version 2.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
# Setup Data
sd_S1n <- read.table("SDS1_OSF.csv", header = T, sep = ",")

sd_S1  <- (na.omit(sd_S1n %>% select(PID, TID, Uniqdyad, TSelfc, SMEANc, SDMEANc, Ratingc, promintz)))

#### Comparing Model Fit
## Simple "Accuracy is Irrelevant" Model
simple <- lmer(Ratingc ~ (SMEANc + SDMEANc) * promintz + (SMEANc + SDMEANc | PID) + 
                 (0 + SMEANc + SDMEANc | TID) + (0 + SMEANc + SDMEANc | Uniqdyad), 
               data= sd_S1, control = lmerControl(optimizer = "bobyqa"))
extractAIC(simple)

## Full "Accuracy Matters" Model (SAM)
full <- lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc) * promintz + (TSelfc + SMEANc + SDMEANc | PID) + 
               (0 + TSelfc + SMEANc + SDMEANc | TID) + (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad), 
             data= sd_S1, control = lmerControl(optimizer = "bobyqa")) 
extractAIC(full)

## Compare with Anova
anova(simple, full)
