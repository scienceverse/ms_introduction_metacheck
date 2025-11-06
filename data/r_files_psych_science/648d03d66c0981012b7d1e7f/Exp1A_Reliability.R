# Exp1A_Reliability.R
# Authors: Laura Pasqualette, Edward Vessel, R. Muralikrishnan, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: Compute test-retest reliability scores for Exp. 1A
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("Experiment_1A") #set directory

library(tidyverse)
library(magrittr)
library(SciViews)
library(ggpubr)
library(PerformanceAnalytics)
library(lmtest)
library(olsrr)

SRdata <- read_delim("Exp1A_Data.txt", delim = "\t", col_names = TRUE) #33 participants
summary(SRdata)


######################## Reliability Scores #######################################

## Aesthetic

SRdata %>% filter(Block == "1") %>% select(Subj, Image, Rating) -> b1
SRdata %>% filter(Block == "3") %>% select(Subj, Image, Rating) -> b3

b1 %>% inner_join(b3, by = c("Subj", "Image")) %>%
  group_by(Subj) %>%
  group_split() -> ft13

# Create an empty tibble with two columns
ft13_cortib <- tibble(Subj = double(), Correlation_Coefficient = double())

#loop to run the reliability test
for (i in 1:length(ft13)){
  
  # Run the correlation and store it temporarily
  cortab_temp <- cor(ft13[[i]]$Rating.x, ft13[[i]]$Rating.y, use = "complete.obs", method = "pearson")
  # Add the relevant values to the tibble
  ft13_cortib %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = cortab_temp)
  
}

Correlation_Coefficients_Ae <- ft13_cortib %>%
  rename(Corr_Coeff_Ae = Correlation_Coefficient)

# plot it
Correlation_Coefficients_Ae %>%
  ggplot(aes(x = Corr_Coeff_Ae)) + geom_histogram(binwidth = 0.05)


## Self-Relevance

SRdata %>% filter(Block == "2") %>% select(Subj, Image, Rating) -> b2
SRdata %>% filter(Block == "4") %>% select(Subj, Image, Rating) -> b4

b2 %>% inner_join(b4, by = c("Subj", "Image")) %>%
  group_by(Subj) %>%
  group_split() -> ft24

# Create an empty tibble with two columns
ft24_cortib <- tibble(Subj = double(), Correlation_Coefficient = double())

#run the reliability test with the loop
for (i in 1:length(ft24)){
  
  # Run the correlation and store it temporarily
  cortab_temp <- correlation(ft24[[i]]$Rating.x, ft24[[i]]$Rating.y, use = "complete.obs", method = "pearson")
  # Add the relevant values to the tibble
  ft24_cortib %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = cortab_temp[2])
  
}

Correlation_Coefficients_SR <- ft24_cortib %>%
  rename(Corr_Coeff_SR = Correlation_Coefficient)

# plot it
Correlation_Coefficients_SR %>%
  ggplot(aes(x = Corr_Coeff_SR)) + geom_histogram(binwidth = 0.05)

# Merge, Take average
library(psych)
Reliability <- full_join(Correlation_Coefficients_Ae,Correlation_Coefficients_SR)
Reliability <- mutate(Reliability,Avg_Rel= fisherz2r((fisherz(Corr_Coeff_Ae) + fisherz(Corr_Coeff_SR)) / 2))
#plot it
Reliability %>% ggplot(aes(x = Avg_Rel)) + geom_histogram(binwidth = 0.02)

#find those below 0.5
filter(Reliability,Avg_Rel < 0.5)

#ONLY SUBJECT: SUBJ 3
