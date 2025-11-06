#### ***************************************************************************************
## Sample 2 Analyses - OSF
## Author: Lauren Gazzard Kerr
## Date: 12/16/2019
## Version 2.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)

# Read in data
sd_S2 <- read.table("SDS2_OSF.csv", header = T, sep = ",")

#### Correlation of Profiles ####

SD_w_SD_NormCor <- unique(sd_S2[c("item", "SMEANc", "IMEANc")])
cor.test(SD_w_SD_NormCor$SMEANc, SD_w_SD_NormCor$IMEANc)
SD_w_SD_DisCor <- unique(sd_S2[c("TID", "item", "TSelfc", "IMEANc")])
cor.test(SD_w_SD_DisCor$TSelfc, SD_w_SD_DisCor$IMEANc)

#### Levels of Accuracy, Normativity, and Positivity ####

summary(lmer(Ratingc ~ TSelfc + IMEANc + SMEANc + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (0 + TSelfc + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")  ) )

#### Primary Outcome: Romantic Interest ####

summary(lmer(Ratingc ~ TSelfc* promintz + IMEANc* promintz + SMEANc* promintz + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")  ) )

#### Target Extraversion as a Moderator ####

# Intercept Only Analysis

# Subset Data
sd_S2s <- sd_S2 %>%filter(item == 1) %>% droplevels()

summary(lmer(promintz ~ TEXz + (1| PID) +  
               (1| TID),data= sd_S2s  ) )

# Extraversion - Average

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TEXz +
               (TSelfc + IMEANc + SMEANc   | PID) + (0 + TSelfc + IMEANc + SMEANc  | TID) + 
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")  ) ) 

# Extraversion Simple Slopes
sd_S2 $TEXl <- sd_S2 $TEXz + 1
sd_S2 $TEXh <- sd_S2 $TEXz - 1

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TEXl +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")  ) )  

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TEXh +
               (TSelfc + IMEANc + SMEANc  | PID) + (TSelfc + IMEANc + SMEANc  | TID) + 
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa") )  )   

#### Additional Outcomes: Physical Attractiveness and Desire to Contact ####

# Physical Attractiveness

summary(lmer(Ratingc ~ TSelfc* pphysattz + IMEANc* pphysattz + SMEANc* pphysattz + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa") )  ) 

# Objective Ratings of Attractiveness - Footnote

summary(lmer(Ratingc ~ TSelfc* Mean_obs_attractz + IMEANc* Mean_obs_attractz + SMEANc* Mean_obs_attractz + (0 + TSelfc  + IMEANc + SMEANc    | PID) + 
               (0 + TSelfc  + IMEANc + SMEANc    | TID) + (0 + TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# Accuracy, Normativity + Romantic Interest while Controlling for Perceived Physical Attractiveness

summary(lmer(Ratingc ~ TSelfc* promintz + IMEANc* promintz + SMEANc* promintz + TSelfc* pphysattz + IMEANc* pphysattz + SMEANc* pphysattz + 
               (TSelfc  + SMEANc    | PID) + (TSelfc  + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Accuracy, Normativity + Romantic Interest while Controlling for Objective Ratings of Attractiveness - Footnote

summary(lmer(Ratingc ~ TSelfc* promintz + IMEANc* promintz + SMEANc* promintz + TSelfc* Mean_obs_attractz + IMEANc* Mean_obs_attractz + SMEANc* Mean_obs_attractz + 
               (TSelfc  + IMEANc + SMEANc    | PID) + (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# 3-Way Interaction Between Accuracy, Romantic Interest, and Physical Attractiveness

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * pphysattz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# Physical Attractiveness Simple Slopes

sd_S2 $pphysattl <- sd_S2 $pphysattz + 1
sd_S2 $pphysatth <- sd_S2 $pphysattz - 1

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * pphysattl +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * pphysatth +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# 3-way interaction between Accuracy, Romantic Interest and Objective Attractiveness - Footnote

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * Mean_obs_attractz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) + 
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# Desire to Contact

summary(lmer(Ratingc ~ TSelfc* d1_yes + IMEANc* d1_yes + SMEANc* d1_yes + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (0 + TSelfc  + IMEANc + SMEANc    | TID) + (0 + TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa") )  ) 

#### Supplementary Analyses ####

## Personality Item Analyses

summary(lmer(promintz ~ d1_humor         + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_relaxed       + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_adventurous   + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_conversate    + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_charming      + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_openness      + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_unconventional + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_tolerant      + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_warm          + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_calmwcrit     + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_boast_low     + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_unobtrusive   + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_nonaggressive + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_honest        + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_reliable      + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_invest        + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_reveal        + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_romantic      + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_faith         + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_optimistic    + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_anxious_low   + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_mature        + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_confident    + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_assertive     + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_ambitious     + (1|PID) + (1|TID), data = sd_S2s))
summary(lmer(promintz ~ d1_intelligence  + (1|PID) + (1|TID), data = sd_S2s))

## Personality Moderators

# Neuroticism

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TNEz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Neuroticism Simple Slopes
sd_S2 $TNEl <- sd_S2 $TNEz + 1
sd_S2 $TNEh <- sd_S2 $TNEz - 1

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TNEl +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TNEh +
               (TSelfc + IMEANc + SMEANc  | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Openness

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TOPz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Openness Simple Slopes
sd_S2 $TOPl <- sd_S2 $TOPz + 1
sd_S2 $TOPh <- sd_S2 $TOPz - 1

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TOPl +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TOPh +
               (TSelfc + IMEANc + SMEANc  | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Agreeableness 

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TAGz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )
# Conscientiousness

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * TCOz +
               (TSelfc + IMEANc + SMEANc   | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

## Exploratory Analyses 

# Gender 

# Intercept Only Analysis

# Use Subset Data

summary(lmer(promintz ~ p_sex + (1| PID) +  
               (1| TID),data= sd_S2s  ) )

# Gender Predicting Accuracy

summary(lmer(Ratingc ~ TSelfc* p_sex + IMEANc* p_sex + SMEANc* p_sex + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Accuracy, Normativity + Romantic Interest while Controlling for Gender

summary(lmer(Ratingc ~ TSelfc* promintz + IMEANc* promintz + SMEANc* promintz + TSelfc* p_sex + IMEANc* p_sex + 
               SMEANc* p_sex + (TSelfc  + IMEANc + SMEANc    | PID) + (TSelfc  + IMEANc + SMEANc    | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# 3-way Interaction between Accuracy, Romantic Interest and Gender

summary(lmer(Ratingc ~ (TSelfc + IMEANc + SMEANc)* promintz * p_sex +
               (0 + TSelfc + IMEANc + SMEANc   | PID) + (0 + TSelfc + SMEANc  | TID) +
               (0 + TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

# Gender Simple Slopes
sd_S2$Genderf <- sd_S2$p_sex 
sd_S2$Genderm <- sd_S2$p_sex - 1

summary(lmer(Ratingc ~ (TSelfc + SMEANc + IMEANc)* promintz * Genderf +
               (TSelfc + + IMEANc + SMEANc  | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

summary(lmer(Ratingc ~ (TSelfc + SMEANc + IMEANc)* promintz * Genderm +
               (TSelfc + + IMEANc + SMEANc  | PID) + (TSelfc + IMEANc + SMEANc  | TID) +
               (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) ) 

## Target Analyses

# Target Romantic Interest

summary(lmer(Ratingc ~ TSelfc* tromintz + IMEANc* tromintz + SMEANc* tromintz + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )

# Target Desire to Contact 

summary(lmer(Ratingc ~ TSelfc* Td1_yes + IMEANc* Td1_yes + SMEANc* Td1_yes + (TSelfc  + IMEANc + SMEANc    | PID) + 
               (TSelfc  + IMEANc + SMEANc    | TID) + (TSelfc + IMEANc + SMEANc    | Uniqdyad),data= sd_S2, control = lmerControl(optimizer = "bobyqa")   ) )
