#### ***************************************************************************************
## Archival Sample Analyses - OSF
## Author: Lauren Gazzard Kerr
## Date: 1/27/2019
## Version 1.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(lmerTest)

# Read in data
ARCH <- read.table("SDARCH_OSF.csv", header = T, sep = ",")

#### Levels of Accuracy, Normativity, and Positivity ####

summary(lmer(PRatingc ~ TValc + CMEANc + (TValc  + CMEANc    | PID) + 
               (0 + TValc  + CMEANc    | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )

#### Primary Outcome: Romantic Interest ####

summary(lmer(PRatingc ~ TValc* promintz + CMEANc* promintz + (TValc  + CMEANc    | PID) + 
               (TValc  + CMEANc    | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )

#### Target Extraversion as a Moderator ####

# Extraversion - Average

summary(lmer(PRatingc ~ (TValc + CMEANc)* promintz * TEXz +
               (TValc + CMEANc   | PID) +  
               (TValc + CMEANc  | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) ) 

# Extraversion Simple Slopes
ARCH $TEXl <- ARCH $TEXz + 1
ARCH $TEXh <- ARCH $TEXz - 1

summary(lmer(PRatingc ~ (TValc + CMEANc)* promintz * TEXl +
               (TValc + CMEANc   | PID) +  
               (TValc + CMEANc | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) ) 

summary(lmer(PRatingc ~ (TValc + CMEANc)* promintz * TEXh +
               (TValc + CMEANc  | PID) +  
               (TValc + CMEANc  | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )  

#### Additional Outcomes: Physical Attractiveness and Desire to Contact ####

# Physical Attractiveness

summary(lmer(PRatingc ~ TValc* p_goodlookingz + CMEANc* p_goodlookingz + (TValc  + CMEANc    | PID) + 
               (TValc  + CMEANc    | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )

# Accuracy, Normativity + Romantic Interest while Controlling for Perceived Physical Attractiveness

summary(lmer(PRatingc ~ TValc* promintz + CMEANc* promintz + TValc* p_goodlookingz + CMEANc* p_goodlookingz + (TValc  + CMEANc    | PID) + 
               (TValc  + CMEANc    | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )

# 3-way interaction between Accuracy, Romantic Interest and Physical Attractiveness

summary(lmer(PRatingc ~ (TValc + CMEANc)* promintz * p_goodlookingz +
               (TValc + CMEANc   | PID) +  
               (0 + TValc + CMEANc  | TID) + (0 + TValc  + CMEANc    | Uniqdyad),data= ARCH  ) ) 

# Desire to Contact

summary(lmer(PRatingc ~ TValc* pcontact + CMEANc* pcontact + (TValc  + CMEANc    | PID) + 
               (TValc  + CMEANc    | TID) + (TValc  + CMEANc    | Uniqdyad),data= ARCH  ) )

