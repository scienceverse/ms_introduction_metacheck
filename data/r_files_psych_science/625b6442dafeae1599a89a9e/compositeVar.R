print("Running compositeVar.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(haven)
###############
## Load data ##
###############
loopr_S1 <- 
  read_sav("Data Analysis/Data - Survey 1.sav") %>%
  rename_with(~ sub("S1_", "", .x))
loopr_S2 <- 
  read_sav("Data Analysis/Data - Survey 2.sav") %>%
  rename_with(~ sub("S2_", "", .x)) %>%
  rename("RiskySuicide" = "rRiskySuicide")
################################
## Create Composite Variables ##
################################
# Create composite variable for CognitiveOrientationTowardsSpirituality and
# Religiousness (r = . 78)
loopr_S1 <- 
  loopr_S1 %>% 
  mutate(   # standardize variables
    COTS_z = scale(CognitiveOrientationTowardsSpirituality),
    Religiousness_z = scale(Religiousness)) %>%
  mutate(
    composite_COTS_Religiousness = rowMeans(
      select(
        .,
        c(COTS_z, Religiousness_z)))) # calculate mean
write.csv(
  loopr_S1,
  "Data Analysis/Results/loopr_S1_composites.csv")
# Create composite variables for JobAttainment and ExtrinsicSuccess (r = .78);
# IntrinsicSuccess and OccupationalSatifaction (r = .71)
loopr_S2 <- 
  loopr_S2 %>%
  mutate(   # standardize variables
    JobAttainment_z = scale(JobAttainment),
    ExtrinsicSuccess_z = scale(ExtrinsicSuccess),
    IntrinsicSuccess_z = scale(IntrinsicSuccess),
    OccupationalSatisfaction_z = scale(OccupationalSatisfaction)) %>%
  mutate(   # calculate means
    composite_JobAttainment_ExtrinsicSuccess = rowMeans(
      select(
        ., 
        c(JobAttainment_z, ExtrinsicSuccess_z))),
    composite_OccupationalSatisfaction_IntrinsicSuccess = rowMeans(
      select(
        ., 
        c(IntrinsicSuccess_z, OccupationalSatisfaction_z))))
write.csv(
  loopr_S2, 
  "Data Analysis/Results/loopr_S2_composites.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("compositeVar.R finished running!")
Sys.sleep(5)                                                           