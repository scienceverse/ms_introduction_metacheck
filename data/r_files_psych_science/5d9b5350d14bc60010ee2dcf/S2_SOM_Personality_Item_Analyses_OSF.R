#### ***************************************************************************************
## Sample 2 - Personality Item Descriptive Statistics and Analyses - OSF
## Author: Lauren Gazzard Kerr
## Date: 08/10/2019
## Version 1.0
#### ***************************************************************************************
## Clear Global Environment
rm(list=ls())
## Load Libraries
library(tidyverse)
library(lmerTest)
# Read in data
sd_S2 <- read.table("SDS2_OSF.csv", header = T, sep = ",")

## Note: Correlating item means (values in Table S2) ) with each item’s association with romantic interest (values in Table S3)

### Get Necessary Values
## Create an Array of Items
aItems <- sort(as.numeric(unique(sd_S2$item)))
## Create New DataFrame
temp <- matrix(data = NA, nrow = NROW(aItems), ncol = 7)
row  <- 1
# Loop Through each Item and Filter Dataframe so that it Only Contains that Item
for (i in 1:NROW(aItems)) {
  # PID Column
  temp[i, 1] = aItems[i]
  # Filter by Item
  fi = aItems[i]
  filterItem <- sd_S2 %>% filter(item == fi) 
  # Mean Perceiver Rating for the Filtered Item
  temp[i, 2] = mean(filterItem$Ratingc, na.rm = T)
  # Mean Target Report
  temp[i, 3] = filterItem$SMEANc[1]
  # Mean Ideal 
  temp[i, 4] = filterItem$IMEANc[1]
}
ItemRatings <- as.data.frame(temp)
colnames(ItemRatings) <- c("Item", "PMEANc", "SMEANc", "IMEANc", "RomIntLM_b")

# Add b Value Column "RomIntLM_b
ItemRatings[ 1, 5] <- fixef(lmer(promint ~ d1_humor          + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 2, 5] <- fixef(lmer(promint ~ d1_relaxed        + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 3, 5] <- fixef(lmer(promint ~ d1_adventurous    + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 4, 5] <- fixef(lmer(promint ~ d1_conversate     + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 5, 5] <- fixef(lmer(promint ~ d1_charming       + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 6, 5] <- fixef(lmer(promint ~ d1_openness       + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 7, 5] <- fixef(lmer(promint ~ d1_unconventional + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 8, 5] <- fixef(lmer(promint ~ d1_tolerant       + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[ 9, 5] <- fixef(lmer(promint ~ d1_warm           + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[10, 5] <- fixef(lmer(promint ~ d1_calmwcrit      + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[11, 5] <- fixef(lmer(promint ~ d1_boast_low      + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[12, 5] <- fixef(lmer(promint ~ d1_unobtrusive    + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[13, 5] <- fixef(lmer(promint ~ d1_nonaggressive  + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[14, 5] <- fixef(lmer(promint ~ d1_honest         + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[15, 5] <- fixef(lmer(promint ~ d1_reliable       + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[16, 5] <- fixef(lmer(promint ~ d1_invest         + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[17, 5] <- fixef(lmer(promint ~ d1_reveal         + (1|PID), data = sd_S2))[2] # Did not converge with TID
ItemRatings[18, 5] <- fixef(lmer(promint ~ d1_romantic       + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[19, 5] <- fixef(lmer(promint ~ d1_faith          + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[20, 5] <- fixef(lmer(promint ~ d1_optimistic     + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[21, 5] <- fixef(lmer(promint ~ d1_anxious_low    + (1|PID), data = sd_S2))[2] # Did not converge with TID
ItemRatings[22, 5] <- fixef(lmer(promint ~ d1_mature         + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[23, 5] <- fixef(lmer(promint ~ d1_confident      + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[24, 5] <- fixef(lmer(promint ~ d1_assertive      + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[25, 5] <- fixef(lmer(promint ~ d1_ambitious      + (1|PID) + (1|TID), data = sd_S2))[2]
ItemRatings[26, 5] <- fixef(lmer(promint ~ d1_intelligence   + (1|PID) + (1|TID), data = sd_S2))[2]

### Correlate
# Target Self-Report Means
cor.test(ItemRatings$SMEANc, ItemRatings$RomIntLM_b, method = "pearson")
# Perceiver Rating Means
cor.test(ItemRatings$PMEANc, ItemRatings$RomIntLM_b, method = "pearson")

## Personality Items Predicting Romantic Interest (Table S3)

# Subset the Data so that it is one Item row per Dyad 
sd_S2s <- sd_S2 %>%filter(item == 1) %>% droplevels()

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