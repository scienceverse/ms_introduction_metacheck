#### ***************************************************************************************
## Sample 1 - SOM Personality Item Descriptive Statistics and Analyses - OSF
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
sd_S1 <- read.table("SDS1_OSF.csv", header = T, sep = ",")

## Note: Correlating item means (values in Table S2) ) with each item’s association with romantic interest (values in Table S3)

### Get Necessary Values
## Create an Array of Items
aItems <- sort(as.numeric(unique(sd_S1$Item)))
## Create New Dataframe
temp <- matrix(data = NA, nrow = NROW(aItems), ncol = 6)
row  <- 1
# Loop Through Each Item and Filter Dataframe so that it Only Contains that Item
for (i in 1:NROW(aItems)) {
  # PID Column
  temp[i, 1] = aItems[i]
  # Filter by Item
  fi = aItems[i]
  filterItem <- sd_S1 %>% filter(Item == fi) 
  # Mean Perceiver Rating for the Filtered Item
  temp[i, 2] = mean(filterItem$Ratingc, na.rm = T)
  # Mean Target Report
  temp[i, 3] = filterItem$SMEANc[1]
}
ItemRatings <- as.data.frame(temp)
colnames(ItemRatings) <- c("Item", "PMEANc", "SMEANc", "RomIntLM_b")

# Add b Value Column "RomIntLM_b
ItemRatings[ 1, 4] <- fixef(lmer(promint ~ bfi2_intelligent       + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 2, 4] <- fixef(lmer(promint ~ bfi21_reserved         + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 3, 4] <- fixef(lmer(promint ~ bfi47_trusting         + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 4, 4] <- fixef(lmer(promint ~ bfi35_lazy             + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 5, 4] <- fixef(lmer(promint ~ bfi16_relaxed          + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 6, 4] <- fixef(lmer(promint ~ bfi42_artisticinterest + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 7, 4] <- fixef(lmer(promint ~ bfi9_outgoing          + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 8, 4] <- fixef(lmer(promint ~ bfi10_findfault        + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[ 9, 4] <- fixef(lmer(promint ~ bfi11_thorough         + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[10, 4] <- fixef(lmer(promint ~ bfi40_nervous          + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[11, 4] <- fixef(lmer(promint ~ bfi46_imagination      + (1|PID) + (1|TID), data = sd_S1))[2]
ItemRatings[12, 4] <- fixef(lmer(promint ~ bfi24_bright           + (1|PID) + (1|TID), data = sd_S1))[2]

### Correlate
# Target Self-Report Means
cor.test(ItemRatings$SMEANc, ItemRatings$RomIntLM_b, method = "pearson")
# Perceiver Rating Means
cor.test(ItemRatings$PMEANc, ItemRatings$RomIntLM_b, method = "pearson")

## Personality Items Predicting Romantic Interest (Table S3)

# Subset the Data so that it is one item row per Dyad 
sd_S1s <- sd_S1 %>%filter(Item == 1) %>% droplevels()

summary(lmer(promintz ~ bfi2_intelligent       + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi21_reserved         + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi47_trusting         + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi35_lazy             + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi16_relaxed          + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi42_artisticinterest + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi9_outgoing          + (1|PID) + (1|TID), data = sd_S1s, control = lmerControl(optimizer = "bobyqa")))
summary(lmer(promintz ~ bfi10_findfault        + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi11_thorough         + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi40_nervous          + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi46_imagination      + (1|PID) + (1|TID), data = sd_S1s))
summary(lmer(promintz ~ bfi24_bright           + (1|PID) + (1|TID), data = sd_S1s))

