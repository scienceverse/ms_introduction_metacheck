#### ***************************************************************************************
## Sample 1 Analyses - OSF 
## Author: Lauren Gazzard Kerr
## Date: 12/03/2019
## Version 2.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)

# Read in data
sd_S1 <- read.table("SDS1_OSF.csv", header = T, sep = ",")

#### Correlation of Profiles ####

SD_w_SD_NormCor <- unique(sd_S1[c("Item", "SMEANc", "SDMEANc")])
cor.test(SD_w_SD_NormCor$SMEANc, SD_w_SD_NormCor$SDMEANc)
SD_w_SD_DisCor <- unique(sd_S1[c("TID", "Item", "TSelfc", "SDMEANc")])
cor.test(SD_w_SD_DisCor$TSelfc, SD_w_SD_DisCor$SDMEANc)

#### Levels of Accuracy, Normativity, and Positivity ####

summary(lmer(Ratingc ~ TSelfc + SMEANc + SDMEANc + (TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc  + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")) )

#### Primary Outcome: Romantic Interest ####

# With Positivity Indicator
summary(lmer(Ratingc ~ TSelfc* promintz + SMEANc* promintz + SDMEANc* promintz + (TSelfc + SMEANc + SDMEANc | PID) + 
              (TSelfc + SMEANc + SDMEANc | TID) + (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Without Positivity Indicator
summary(lmer(Ratingc ~ TSelfc* promintz + SMEANc* promintz + (TSelfc + SMEANc | PID) + 
               (TSelfc + SMEANc | TID) + (0 + TSelfc + SMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

#### Target Extraversion as a Moderator ####

# Intercept Only Analysis

# Subset Data
sd_S1s <- sd_S1 %>%filter(Item == 1) %>% droplevels()

summary(lmer(promintz ~ TEXz + (1| PID) + 
               (1| TID),data= sd_S1s  ) )

# Extraversion - Average

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TEXz + 
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa") ) )

# Extraversion Simple Slopes
sd_S1 $TEXl <- sd_S1 $TEXz + 1
sd_S1 $TEXh <- sd_S1 $TEXz - 1

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TEXl + 
               (0 + TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TEXh +
               (0 + TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

#### Additional Outcomes: Physical Attractiveness and Desire to Contact ####

# Physical Attractiveness

summary(lmer(Ratingc ~ TSelfc* pattractz + SMEANc* pattractz + SDMEANc* pattractz +
               (TSelfc + SMEANc + SDMEANc | PID) + (TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad), data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Objective Ratings of Attractiveness - Footnote

summary(lmer(Ratingc ~ TSelfc* Mean_obs_attractz + SMEANc* Mean_obs_attractz + SDMEANc* Mean_obs_attractz + 
               (TSelfc + SMEANc + SDMEANc | PID) + (TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Accuracy, Normativity + Romantic Interest while Controlling for Perceived Physical Attractiveness

summary(lmer(Ratingc ~ TSelfc* promintz + SMEANc* promintz + SDMEANc* promintz + TSelfc* pattractz + SMEANc* pattractz + SDMEANc* pattractz +
               (TSelfc + SMEANc  + SDMEANc | PID) + (TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Accuracy, Normativity + Romantic Interest while Controlling for Objective Ratings of Attractiveness - Footnote

summary(lmer(Ratingc ~ TSelfc* promintz + SMEANc* promintz + SDMEANc* promintz + TSelfc* Mean_obs_attractz + SMEANc* Mean_obs_attractz + SDMEANc* Mean_obs_attractz +
               (0 + TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# 3-way interaction between Accuracy, Romantic Interest and Physical Attractiveness

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * pattractz + 
               (TSelfc + SMEANc + SDMEANc | PID) + (TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) ) 

# Physical Attractiveness Simple Slopes

sd_S1 $pattractl <- sd_S1 $pattractz + 1
sd_S1 $pattracth <- sd_S1 $pattractz - 1

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * pattractl + 
               (0 + TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) ) 

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * pattracth + 
               (TSelfc + SMEANc + SDMEANc | PID) + (TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) ) 

# 3-way interaction between Accuracy, Romantic Interest and Objective Attractiveness - Footnote

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * Mean_obs_attractz + 
               (0 + TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) ) 

# Desire to Contact

summary(lmer(Ratingc ~ TSelfc* pcontact + SMEANc* pcontact + SDMEANc* pcontact +
               (TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

#### Supplementary Analyses #### 

## Personality Item Analyses
# Intelligence
sd_S1_bfi2 <- sd_S1 %>%  filter(Item == 1)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi2))
# Reserved
sd_S1_bfi21 <- sd_S1 %>%  filter(Item == 2)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi21))
# Trusting
sd_S1_bfi47 <- sd_S1 %>%  filter(Item == 3)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi47))
# Lazy
sd_S1_bfi35 <- sd_S1 %>%  filter(Item == 4)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi35))
# Relaxed
sd_S1_bfi16 <- sd_S1 %>%  filter(Item == 5)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi16))
# Artistic Interest
sd_S1_bfi42 <- sd_S1 %>%  filter(Item == 6)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi42))
# Outgoing
sd_S1_bfi9 <- sd_S1 %>%  filter(Item == 7)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi9, control = lmerControl(optimizer = "bobyqa") ))
# Finds Fault
sd_S1_bfi10 <- sd_S1 %>%  filter(Item == 8)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi10))
# Thorough
sd_S1_bfi11 <- sd_S1 %>%  filter(Item == 9)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi11))
# Nervous
sd_S1_bfi40 <- sd_S1 %>%  filter(Item == 10)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi40))
# Imagination
sd_S1_bfi46 <- sd_S1 %>%  filter(Item == 11)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi46))
# Bright
sd_S1_bfi24 <- sd_S1 %>%  filter(Item == 12)
summary(lmer(promintz ~ Ratingc       + (1|PID) + (1|TID), data = sd_S1_bfi24))


## Personality Moderators

# Neuroticism

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TNEz +
               (0 + TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Neuroticism Simple Slopes
sd_S1 $TNEl <- sd_S1 $TNEz + 1
sd_S1 $TNEh <- sd_S1 $TNEz - 1

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TNEl +
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TNEh + 
               (0 + TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Openness

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TOPz +
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Openness Simple Slopes
sd_S1 $TOPl <- sd_S1 $TOPz + 1
sd_S1 $TOPh <- sd_S1 $TOPz - 1

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TOPl +
               (0 + TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TOPh +
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Agreeableness

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TAGz +
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Conscientiousness

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * TCOz +
               (TSelfc + SMEANc + SDMEANc | PID) +  
               (0 + TSelfc + SMEANc + SDMEANc | TID) + 
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

## Exploratory Analyses

# Gender

# Intercept Only Analysis

# Use Subset Data

summary(lmer(promintz ~ Gender + (1| PID) + 
               (1| TID),data= sd_S1s  ) )  

# Gender Predicting Accuracy

summary(lmer(Ratingc ~ TSelfc* Gender + SMEANc* Gender + SDMEANc* Gender + 
               (TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")   ) )

# Accuracy, Normativity and Romantic Interest While Controlling for Gender

summary(lmer(Ratingc ~ TSelfc* promintz + SMEANc* promintz + SDMEANc* promintz + TSelfc* Gender + SMEANc* Gender + SDMEANc* Gender +
               (0 + TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# 3-way Interaction between Accuracy, Romantic Interest and Gender 

summary(lmer(Ratingc ~ (TSelfc + SMEANc + SDMEANc)* promintz * Gender +
               (0 + TSelfc + SMEANc + SDMEANc | PID) + (0 + TSelfc + SMEANc + SDMEANc | TID) +
               (0 + TSelfc  + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) ) 

## Target Analyses

# Target Romantic Interest

summary(lmer(Ratingc ~ TSelfc* tromintz + SMEANc* tromintz + SDMEANc* tromintz + (TSelfc + SMEANc + SDMEANc | PID) + 
               (TSelfc + SMEANc + SDMEANc | TID) + (0 + TSelfc + SMEANc + SDMEANc | Uniqdyad),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )

# Target Desire to Contact 

summary(lmer(Ratingc ~ TSelfc* tcontact + SMEANc* tcontact + SDMEANc* tcontact + (TSelfc + SMEANc + SDMEANc | PID) + 
               (0 + TSelfc + SMEANc + SDMEANc | TID),data= sd_S1, control = lmerControl(optimizer = "bobyqa")  ) )
