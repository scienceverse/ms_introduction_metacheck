print("Running outcomeFrames_S2.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(haven)
###############
## Load data ##
###############
loopr_full_S2 <-
  read_sav("Data Analysis/Survey 2 - Merged - Working data file.sav")
######################
## Define variables ##
######################
traits_S2 <-
  c("S2_PersonalityNegativeEmotionality", "S2_PersonalityExtraversion",
    "S2_PersonalityOpenMindedness", "S2_PersonalityAgreeableness",
    "S2_PersonalityConscientiousness")
# ActiveCoping ####
ActiveCoping.items <- 
  c("S2_Coping_1", "S2_Coping_2", "S2_Coping_3", "S2_Coping_4", "S2_Coping_5")
# composite_JobAttainment_ExtrinsicSuccess #####
JobPrestigeExtrinsicStatusComplexity.items_raw <-
  "S2_WorkAttainment1"
JobPrestigeExtrinsicStatus.items <-
  c("S2_WorkAttainment1_Hollingshead_1", "S2_WorkAttainment1_Hollingshead_2")
JobComplexityData.items <- 
  c("S2_WorkAttainment1_DOTData_1", "S2_WorkAttainment1_DOTData_2")
JobComplexityPeople.items <- 
  c("S2_WorkAttainment1_DOTPeople_1", "S2_WorkAttainment1_DOTPeople_2")
JobComplexityThings.items <- 
  c("S2_WorkAttainment1_DOTThings_1", "S2_WorkAttainment1_DOTThings_2")
JobEducation.items_raw <- 
  "S2_WorkEducation"
JobHours.items_raw <-
  "S2_WorkAttainment2_1"
JobHours.items <- 
  "S2_WorkAttainment2_1_recode"
JobEarnings.items_raw <-
  c("S2_WorkAttainment3_1", "S2_WorkAttainment3_2")
ExtrinsicIncome.items_raw <- 
  "S2_WorkIncome_1"
ExtrinsicIncome.items <- 
  "S2_WorkIncome_1_recode"
# composite_OccupationalSatisfaction_IntrinsicSuccess ####
IntrinsicSuccess.items <- 
  c("S2_WorkIntrinsicSuccess_1", "S2_WorkIntrinsicSuccess_2",
    "S2_WorkIntrinsicSuccess_3", "S2_WorkIntrinsicSuccess_4",
    "S2_WorkIntrinsicSuccess_5", "S2_WorkIntrinsicSuccess_6")
OccupationalSatisfaction.items <- 
  c("S2_WorkSatisfaction_1", "S2_WorkSatisfaction_2", "S2_WorkSatisfaction_3",
    "S2_WorkSatisfaction_4", "S2_WorkSatisfaction_5", "S2_WorkSatisfaction_6")
# ConventionalInterests ####
ConventionalInterests.items <-
  c("S2_WorkInterests_31", "S2_WorkInterests_32", "S2_WorkInterests_33",
    "S2_WorkInterests_34", "S2_WorkInterests_35", "S2_WorkInterests_36")
# EnterprisingInterests ####
EnterprisingInterests.items <-
  c("S2_WorkInterests_25", "S2_WorkInterests_26", "S2_WorkInterests_27",
    "S2_WorkInterests_28", "S2_WorkInterests_29", "S2_WorkInterests_30")
# Leadership ####
Leadership.items <-
  c("S2_Leadership_1", "S2_Leadership_2", "S2_Leadership_3", "S2_Leadership_4",
    "S2_Leadership_5", "S2_Leadership_6", "S2_Leadership_7", "S2_Leadership_8")
# OccupationalCommitment ####
OccupationalCommitment.items_raw <- 
  c("S2_WorkCommitment_1", "S2_WorkCommitment_2", "S2_WorkCommitment_3",
    "S2_WorkCommitment_4", "S2_WorkCommitment_5", "S2_WorkCommitment_6")
OccupationalCommitment.items_R <- 
  c("S2_WorkCommitment_4", "S2_WorkCommitment_5", "S2_WorkCommitment_6")
OccupationalCommitment.items <- 
  c("S2_WorkCommitment_1", "S2_WorkCommitment_2", "S2_WorkCommitment_3",
    "S2_WorkCommitment_4_R", "S2_WorkCommitment_5_R", "S2_WorkCommitment_6_R")
# OccupationalPerformanceSalary ####
OccupationalHours.items_raw <- 
  "S2_WorkAttainment2_1"
OccupationalHours.items <- 
  "S2_WorkAttainment2_1_recode"
OccupationalSalary.items_raw <- 
  c("S2_WorkAttainment3_1", "S2_WorkAttainment3_2")
# PersonalityDisordersAggression ####
PersonalityDisordersAggression.items_raw <-
  c("S2_Personality_disorder_13", "S2_Personality_disorder_14",
    "S2_Personality_disorder_15", "S2_Personality_disorder_16")
PersonalityDisordersAggression.items_R <-
  c("S2_Personality_disorder_15", "S2_Personality_disorder_16")
PersonalityDisordersAggression.items <-
  c("S2_Personality_disorder_13", "S2_Personality_disorder_14",
    "S2_Personality_disorder_15_R", "S2_Personality_disorder_16_R")
# PersonalityDisordersDependency ####
PersonalityDisordersDependency.items_raw <-
  c("S2_Personality_disorder_25", "S2_Personality_disorder_26",
    "S2_Personality_disorder_27", "S2_Personality_disorder_28")
PersonalityDisordersDependency.items_R <-
  c("S2_Personality_disorder_27", "S2_Personality_disorder_28")
PersonalityDisordersDependency.items <-
  c("S2_Personality_disorder_25", "S2_Personality_disorder_26",
    "S2_Personality_disorder_27_R", "S2_Personality_disorder_28_R")
# PersonalityDisordersDetachment ####
PersonalityDisordersDetachment.items_raw <-
  c("S2_Personality_disorder_41", "S2_Personality_disorder_42",
    "S2_Personality_disorder_43", "S2_Personality_disorder_44")
PersonalityDisordersDetachment.items_R <-
  "S2_Personality_disorder_44"
PersonalityDisordersDetachment.items <-
  c("S2_Personality_disorder_41", "S2_Personality_disorder_42",
    "S2_Personality_disorder_43", "S2_Personality_disorder_44_R")
# PersonalityDisordersExhibitionism ####
PersonalityDisordersExhibitionism.items_raw <-
  c("S2_Personality_disorder_33", "S2_Personality_disorder_34",
    "S2_Personality_disorder_35", "S2_Personality_disorder_36")
PersonalityDisordersExhibitionism.items_R <-
  c("S2_Personality_disorder_35", "S2_Personality_disorder_36")
PersonalityDisordersExhibitionism.items <-
  c("S2_Personality_disorder_33", "S2_Personality_disorder_34",
    "S2_Personality_disorder_35_R", "S2_Personality_disorder_36_R")
# PersonalityDisordersImpulsivity ####
PersonalityDisordersImpulsivity.items_raw <-
  c("S2_Personality_disorder_49", "S2_Personality_disorder_50",
    "S2_Personality_disorder_51", "S2_Personality_disorder_52")
PersonalityDisordersImpulsivity.items_R <-
  c("S2_Personality_disorder_51", "S2_Personality_disorder_52")
PersonalityDisordersImpulsivity.items <-
  c("S2_Personality_disorder_49", "S2_Personality_disorder_50",
    "S2_Personality_disorder_51_R", "S2_Personality_disorder_52_R")
# PersonalityDisordersManipulativeness ####
PersonalityDisordersManipulativeness.items_raw <-
  c("S2_Personality_disorder_9", "S2_Personality_disorder_10",
    "S2_Personality_disorder_11", "S2_Personality_disorder_12")
PersonalityDisordersManipulativeness.items_R <-
  "S2_Personality_disorder_12"
PersonalityDisordersManipulativeness.items <-
  c("S2_Personality_disorder_9", "S2_Personality_disorder_10",
    "S2_Personality_disorder_11", "S2_Personality_disorder_12_R")
# PersonalityDisordersPropriety ####
PersonalityDisordersPropriety.items <-
  c("S2_Personality_disorder_53", "S2_Personality_disorder_54",
    "S2_Personality_disorder_55", "S2_Personality_disorder_56")
# PersonalityDisordersSelfHarm ####
PersonalityDisordersSelfHarm.items_raw <-
  c("S2_Personality_disorder_17", "S2_Personality_disorder_18",
    "S2_Personality_disorder_19", "S2_Personality_disorder_20")
PersonalityDisordersSelfHarm.items_R <- 
  "S2_Personality_disorder_20"
PersonalityDisordersSelfHarm.items <-
  c("S2_Personality_disorder_17", "S2_Personality_disorder_18",
    "S2_Personality_disorder_19", "S2_Personality_disorder_20_R")
# RealisticInterests ####
RealisticInterests.items <-
  c("S2_WorkInterests_1", "S2_WorkInterests_2", "S2_WorkInterests_3",
    "S2_WorkInterests_4", "S2_WorkInterests_5", "S2_WorkInterests_6")
# RiskyDriving ####
RiskyDriving.items <- 
  c("S2_RiskyDriving1_1", "S2_RiskyDriving1_2", "S2_RiskyDriving1_3")
# RiskyViolence ####
RiskyViolence.items_raw <- 
  c("S2_RiskyDrivingViolence_3", "S2_RiskyDrivingViolence_4",
    "S2_RiskyDrivingViolence_5", "S2_RiskyDrivingViolence_6",
    "S2_RiskyDrivingViolence_7")
RiskyViolence.items_recode <- 
  c("S2_RiskyDrivingViolence_3_recode", "S2_RiskyDrivingViolence_4_recode",
    "S2_RiskyDrivingViolence_5_recode", "S2_RiskyDrivingViolence_6_recode",
    "S2_RiskyDrivingViolence_7_recode")
RiskyViolence.items <- 
  c("S2_RiskyDrivingViolence_3_recode2", "S2_RiskyDrivingViolence_4_recode2",
    "S2_RiskyDrivingViolence_5_recode2", "S2_RiskyDrivingViolence_6_recode2",
    "S2_RiskyDrivingViolence_7_recode2")
# SeekingSupport ####
SeekingSupport.items <-
  c("S2_Coping_12", "S2_Coping_13", "S2_Coping_14", "S2_Coping_15",
    "S2_Coping_16")
# SocialInterests ####
SocialInterests.items <-
  c("S2_WorkInterests_19", "S2_WorkInterests_20", "S2_WorkInterests_21",
    "S2_WorkInterests_22", "S2_WorkInterests_23", "S2_WorkInterests_24")
####################################################
## Create Outcome-Specific Item-Level Data Frames ##
####################################################
# ActiveCoping ####
ActiveCoping_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(ActiveCoping.items), S2_ActiveCoping, 
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(ActiveCoping.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# composite_JobAttainment_ExtrinsicSuccess ####
JobPrestigeExtrinsicStatus_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobPrestigeExtrinsicStatusComplexity.items_raw),
      all_of(JobPrestigeExtrinsicStatus.items), S2_JobAttainment,
      all_of(traits_S2)))
JobComplexityData_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobPrestigeExtrinsicStatusComplexity.items_raw),
      all_of(JobComplexityData.items), S2_JobAttainment, all_of(traits_S2)))
JobComplexityPeople_full <-
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobPrestigeExtrinsicStatusComplexity.items_raw),
      all_of(JobComplexityPeople.items), S2_JobAttainment, all_of(traits_S2)))
JobComplexityThings_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobPrestigeExtrinsicStatusComplexity.items_raw),
      all_of(JobComplexityThings.items), S2_JobAttainment, all_of(traits_S2)))
JobEducation_full <-
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobEducation.items_raw), S2_JobAttainment,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(JobEducation.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(JobEducation.items_raw),
      ~ recode_factor(
        .x,
        `1` = 1,
        `2` = 2,
        `3` = 3,
        `4` = 4,
        `5` = 5,
        `6` = 5,
        `7` = 5,
        `8` = 5,
        `9` = 5,
        `10` = 5,
        .ordered = TRUE),
      .names = "{.col}_recode"))
JobHours_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobHours.items_raw), S2_JobAttainment, 
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(JobHours.items_raw),
      ~ ifelse(
        .x >= 1 & 
          .x <= 120,
        ifelse(
          .x >= 76.69, 
          75,
          .x), 
        NA),
      .names = "{.col}_recode"))
JobEarnings_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(JobEarnings.items_raw), S2_JobAttainment, 
      all_of(traits_S2))) %>%
  mutate(
    S2_WorkAttainment3_2 = as_factor(S2_WorkAttainment3_2, levels = "values",
                                     ordered = TRUE)) %>%
  mutate(
    S2_JobEarningsPerHour = ifelse(
      S2_WorkAttainment3_2 != 1,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 5 &
          S2_WorkAttainment3_1 <= 190,
        ifelse(
          S2_WorkAttainment3_1 >= 51.25,
          50, 
          S2_WorkAttainment3_1),
        NA)),
    S2_JobEarningsPerDay = ifelse(
      S2_WorkAttainment3_2 != 2,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 20 &
          S2_WorkAttainment3_1 <= 1500,
        ifelse(
          S2_WorkAttainment3_1 >= 1003.98,
          1000, 
          S2_WorkAttainment3_1),
        NA)),
    S2_JobEarningsPerWeek = ifelse(
      S2_WorkAttainment3_2 != 3,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 50 &
          S2_WorkAttainment3_1 <= 5000,
        ifelse(
          S2_WorkAttainment3_1 >= 2383.60,
          2200, 
          S2_WorkAttainment3_1),
        NA)),
    S2_JobEarningsPerMonth = ifelse(
      S2_WorkAttainment3_2 != 4,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 100 &
          S2_WorkAttainment3_1 <= 16000,
        ifelse(
          S2_WorkAttainment3_1 >= 11589.31,
          11000, 
          S2_WorkAttainment3_1),
        NA)),
    S2_JobEarningsPerYear = ifelse(
      S2_WorkAttainment3_2 != 5,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 1000 &
          S2_WorkAttainment3_1 <= 340000,
        ifelse(
          S2_WorkAttainment3_1 >= 209202.23,
          200000,
          S2_WorkAttainment3_1),
        NA)))
ExtrinsicIncome_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(ExtrinsicIncome.items_raw), S2_ExtrinsicSuccess,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(ExtrinsicIncome.items_raw),
      ~ ifelse(
        .x >= 100 &
          .x <= 300000,
        ifelse(
          .x >= 170442.23,
          170000, 
          .x), 
        NA),
      .names = "{.col}_recode")) %>%
  mutate(
    S2_ExtrinsicIncomeSplit = ifelse(
      is.na(S2_WorkIncome_1_recode),
      NA,
      ifelse(
        S2_WorkIncome_1_recode <= 12000,
        1,
        ifelse(
          S2_WorkIncome_1_recode <= 23500,
          2,
          ifelse(
            S2_WorkIncome_1_recode <= 34000,
            3,
            ifelse(
              S2_WorkIncome_1_recode <= 50000,
              4,
              ifelse(
                S2_WorkIncome_1_recode <= 75000,
                5,
                6)))))))
# composite_OccupationalSatisfaction_IntrinsicSuccess ####
IntrinsicSuccess_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(IntrinsicSuccess.items), S2_IntrinsicSuccess,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(IntrinsicSuccess.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
OccupationalSatisfaction_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(OccupationalSatisfaction.items),
      S2_OccupationalSatisfaction, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(OccupationalSatisfaction.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# ConventionalInterests ####
ConventionalInterests_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(ConventionalInterests.items), S2_ConventionalInterests,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(ConventionalInterests.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# EnterprisingInterests ####
EnterprisingInterests_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(EnterprisingInterests.items), S2_EnterprisingInterests,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(EnterprisingInterests.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# Leadership ####
Leadership_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(Leadership.items), S2_Leadership, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(Leadership.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# OccupationalCommitment ####
OccupationalCommitment_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(OccupationalCommitment.items_raw),
      S2_OccupationalCommitment, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(OccupationalCommitment.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(OccupationalCommitment.items_R),
      ~ recode_factor(
        .x,
        `7` = 1,
        `6` = 2,
        `5` = 3,
        `4` = 4,
        `3` = 5,
        `2` = 6,
        `1` = 7,
        .ordered = TRUE),
      .names = "{.col}_R"))
# OccupationalPerformanceSalary ####
OccupationalHours_full <-
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(OccupationalHours.items_raw),
      S2_OccupationalPerformanceSalary, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(OccupationalHours.items_raw),
      ~ ifelse(
        .x >= 1 &
          .x <= 120,
             ifelse(
               .x >= 76.19,
               75, 
               .x),
        NA),
      .names = "{.col}_recode"))
OccupationalSalary_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(OccupationalSalary.items_raw),
      S2_OccupationalPerformanceSalary, all_of(traits_S2))) %>%
  mutate(
    S2_WorkAttainment3_2 = as_factor(
      S2_WorkAttainment3_2, levels = "values", ordered = TRUE)) %>%
  mutate(
    S2_OccupationalSalaryPerHour = ifelse(
      S2_WorkAttainment3_2 != 1,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 5 &
          S2_WorkAttainment3_1 <= 190,
        ifelse(
          S2_WorkAttainment3_1 >= 52.43,
          50, 
          S2_WorkAttainment3_1),
        NA)),
    S2_OccupationalSalaryPerDay = ifelse(
      S2_WorkAttainment3_2 != 2,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 25 &
          S2_WorkAttainment3_1 <= 1500,
        ifelse(
          S2_WorkAttainment3_1 >= 1024.09,
          1000,
          S2_WorkAttainment3_1),
        NA)),
    S2_OccupationalSalaryPerWeek = ifelse(
      S2_WorkAttainment3_2 != 3,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 50 &
          S2_WorkAttainment3_1 <= 5000,
        ifelse(
          S2_WorkAttainment3_1 >= 2434.04,
          2200, 
          S2_WorkAttainment3_1),
        NA)),
    S2_OccupationalSalaryPerMonth = ifelse(
      S2_WorkAttainment3_2 != 4,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 100 &
          S2_WorkAttainment3_1 <= 16000,
        ifelse(
          S2_WorkAttainment3_1 >= 11525.27, 
          11000,
          S2_WorkAttainment3_1),
        NA)),
    S2_OccupationalSalaryPerYear = ifelse(
      S2_WorkAttainment3_2 != 5,
      NA,
      ifelse(
        S2_WorkAttainment3_1 >= 3000 & 
          S2_WorkAttainment3_1 <= 340000,
        ifelse(
          S2_WorkAttainment3_1 >= 209619.59,
          200000,
          S2_WorkAttainment3_1),
        NA)))
# PersonalityDisordersAggression ####
PersonalityDisordersAggression_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersAggression.items_raw),
      S2_PersonalityDisordersAggression, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersAggression.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersAggression.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersDependency ####
PersonalityDisordersDependency_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersDependency.items_raw),
      S2_PersonalityDisordersDependency, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersDependency.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersDependency.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersDetachment ####
PersonalityDisordersDetachment_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersDetachment.items_raw),
      S2_PersonalityDisordersDetachment, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersDetachment.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersDetachment.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersExhibitionism ####
PersonalityDisordersExhibitionism_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersExhibitionism.items_raw),
     S2_PersonalityDisordersExhibitionism, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersExhibitionism.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersExhibitionism.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersImpulsivity ####
PersonalityDisordersImpulsivity_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersImpulsivity.items_raw),
      S2_PersonalityDisordersImpulsivity, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersImpulsivity.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersImpulsivity.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersManipulativeness ####
PersonalityDisordersManipulativeness_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersManipulativeness.items_raw),
      S2_PersonalityDisordersManipulativeness, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersManipulativeness.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersManipulativeness.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# PersonalityDisordersPropriety ####
PersonalityDisordersPropriety_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersPropriety.items),
      S2_PersonalityDisordersPropriety, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersPropriety.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# PersonalityDisordersSelfHarm ####
PersonalityDisordersSelfHarm_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(PersonalityDisordersSelfHarm.items_raw),
      S2_PersonalityDisordersSelfHarm, all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersSelfHarm.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(PersonalityDisordersSelfHarm.items_R),
      ~ recode_factor(
        .x,
        `2` = 1,
        `1` = 2,
        .ordered = TRUE),
      .names = "{.col}_R"))
# RealisticInterests ####
RealisticInterests_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(RealisticInterests.items), S2_RealisticInterests,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(RealisticInterests.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# RiskyDriving ####
RiskyDriving_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(RiskyDriving.items), S2_RiskyDriving, 
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(RiskyDriving.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# RiskyViolence ####
RiskyViolence_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(RiskyViolence.items_raw), S2_RiskyViolence,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(RiskyViolence.items_raw),
      ~ ifelse(
        .x %in% c(0:10), 
        .x,
        NA),
      .names = "{.col}_recode")) %>%
  mutate(
    across(
      all_of(RiskyViolence.items_recode),
      ~ ifelse(
        .x %in% c(3:10),
        2, 
        .x),
      .names = "{.col}2"))
# SeekingSupport ####
SeekingSupport_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(SeekingSupport.items), S2_SeekingSupport, 
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(SeekingSupport.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# SocialInterests ####
SocialInterests_full <- 
  loopr_full_S2 %>%
  select(
    c(CaseID, all_of(SocialInterests.items), S2_SocialInterests,
      all_of(traits_S2))) %>%
  mutate(
    across(
      all_of(SocialInterests.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
###################################
## Examine Ceiling/Floor Effects ##
###################################
# ActiveCoping ####
ActiveCoping_items <- 
  tibble()
for (i in ActiveCoping.items) {
  ActiveCoping_items <-
    ActiveCoping_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(ActiveCoping_full[i])),
            names_from = i,
            values_from = Freq)))
}
ActiveCoping_items <- 
  ActiveCoping_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `4`) / n)
ActiveCoping_items
# composite_JobAttainment_ExtrinsicSuccess ####
JobPrestigeExtrinsicStatus_items <- 
  tibble()
for (i in JobPrestigeExtrinsicStatus.items) {
  JobPrestigeExtrinsicStatus_items <-
    JobPrestigeExtrinsicStatus_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobPrestigeExtrinsicStatus_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobPrestigeExtrinsicStatus_items <-
  JobPrestigeExtrinsicStatus_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `9`) / n)
JobPrestigeExtrinsicStatus_items
JobComplexityData_items <-
  tibble()
for (i in JobComplexityData.items) {
  JobComplexityData_items <- 
    JobComplexityData_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobComplexityData_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobComplexityData_items <- 
  JobComplexityData_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`0` + `6`) / n)
JobComplexityData_items
JobComplexityPeople_items <-
  tibble()
for (i in JobComplexityPeople.items) {
  JobComplexityPeople_items <- 
    JobComplexityPeople_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobComplexityPeople_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobComplexityPeople_items <- 
  JobComplexityPeople_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`0` + `8`) / n)
JobComplexityPeople_items
JobComplexityThings_items <-
  tibble()
for (i in JobComplexityThings.items) {
  JobComplexityThings_items <- 
    JobComplexityThings_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobComplexityThings_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobComplexityThings_items <- 
  JobComplexityThings_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`0` + `7`) / n)
JobComplexityThings_items
JobEducation_items <-
  tibble()
for (i in JobEducation.items_raw) {
  JobEducation_items <- 
    JobEducation_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobEducation_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobEducation_items <- 
  JobEducation_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `10`) / n)
JobEducation_items
JobHours_items <-
  tibble()
for (i in JobHours.items) {
  JobHours_items <-
    JobHours_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(JobHours_full[i])),
            names_from = i,
            values_from = Freq)))
}
JobHours_items <-
  JobHours_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `75`) / n)
JobHours_items %>% 
  select(
    c(var, ceilingFloor))
S2_WorkAttainment3_2 <- 
  tibble(var = "S2_WorkAttainment3_2") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_WorkAttainment3_2)),
      names_from = Var1,
      values_from = Freq))
S2_WorkAttainment3_2 <- 
  S2_WorkAttainment3_2 %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
S2_WorkAttainment3_2
S2_JobEarningsPerHour <- 
  tibble(var = "S2_JobEarningsPerHour") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_JobEarningsPerHour)),
      names_from = Var1,
      values_from = Freq))
S2_JobEarningsPerHour <- 
  S2_JobEarningsPerHour %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`5` + `50`) / n)
S2_JobEarningsPerHour %>% 
  select(
    c(var, ceilingFloor))
S2_JobEarningsPerDay <- 
  tibble(var = "S2_JobEarningsPerDay") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_JobEarningsPerDay)),
      names_from = Var1,
      values_from = Freq))
S2_JobEarningsPerDay <- 
  S2_JobEarningsPerDay %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`20` + `1000`) / n)
S2_JobEarningsPerDay %>% 
  select(
    c(var, ceilingFloor))
S2_JobEarningsPerWeek <- 
  tibble(var = "S2_JobEarningsPerWeek") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_JobEarningsPerWeek)),
      names_from = Var1,
      values_from = Freq))
S2_JobEarningsPerWeek <- 
  S2_JobEarningsPerWeek %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`50` + `2200`) / n)
S2_JobEarningsPerWeek %>% 
  select(
    c(var, ceilingFloor))
S2_JobEarningsPerMonth <-
  tibble(var = "S2_JobEarningsPerMonth") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_JobEarningsPerMonth)),
      names_from = Var1,
      values_from = Freq))
S2_JobEarningsPerMonth <- 
  S2_JobEarningsPerMonth %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`100` + `11000`) / n)
S2_JobEarningsPerMonth %>% 
  select(
    c(var, ceilingFloor))
S2_JobEarningsPerYear <- 
  tibble(var = "S2_JobEarningsPerYear") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(JobEarnings_full$S2_JobEarningsPerYear)),
      names_from = Var1,
      values_from = Freq))
S2_JobEarningsPerYear <-
  S2_JobEarningsPerYear %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1000` + `2e+05`) / n)
S2_JobEarningsPerYear %>% 
  select(
    c(var, ceilingFloor))
ExtrinsicIncome_items <- 
  tibble()
for (i in ExtrinsicIncome.items) {
  ExtrinsicIncome_items <- 
    ExtrinsicIncome_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(ExtrinsicIncome_full[i])),
            names_from = i,
            values_from = Freq)))
}
ExtrinsicIncome_items <- 
  ExtrinsicIncome_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`100` + `170000`) / n)
ExtrinsicIncome_items %>% 
  select(
    c(var, ceilingFloor))
S2_ExtrinsicIncomeSplit <-
  tibble(var = "S2_ExtrinsicIncomeSplit") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(ExtrinsicIncome_full$S2_ExtrinsicIncomeSplit)),
      names_from = Var1,
      values_from = Freq))
S2_ExtrinsicIncomeSplit <- 
  S2_ExtrinsicIncomeSplit %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `6`) / n)
S2_ExtrinsicIncomeSplit
# composite_OccupationalSatisfaction_IntrinsicSuccess ####
IntrinsicSuccess_items <- 
  tibble()
for (i in IntrinsicSuccess.items) {
  IntrinsicSuccess_items <- 
    IntrinsicSuccess_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IntrinsicSuccess_full[i])),
            names_from = i,
            values_from = Freq)))
}
IntrinsicSuccess_items <- 
  IntrinsicSuccess_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
IntrinsicSuccess_items
OccupationalSatisfaction_items <- 
  tibble()
for (i in OccupationalSatisfaction.items) {
  OccupationalSatisfaction_items <- 
    OccupationalSatisfaction_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(OccupationalSatisfaction_full[i])),
            names_from = i, 
            values_from = Freq)))
}
OccupationalSatisfaction_items <- 
  OccupationalSatisfaction_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
OccupationalSatisfaction_items
# ConventionalInterests ####
ConventionalInterests_items <-
  tibble()
for (i in ConventionalInterests.items) {
  ConventionalInterests_items <- 
    ConventionalInterests_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(ConventionalInterests_full[i])),
            names_from = i,
            values_from = Freq)))
}
ConventionalInterests_items <- 
  ConventionalInterests_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
ConventionalInterests_items
# EnterprisingInterests ####
EnterprisingInterests_items <- 
  tibble()
for (i in EnterprisingInterests.items) {
  EnterprisingInterests_items <-
    EnterprisingInterests_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(EnterprisingInterests_full[i])),
            names_from = i,
            values_from = Freq)))
}
EnterprisingInterests_items <- 
  EnterprisingInterests_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
EnterprisingInterests_items
# Leadership ####
Leadership_items <-
  tibble()
for (i in Leadership.items) {
  Leadership_items <- 
    Leadership_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(Leadership_full[i])),
            names_from = i,
            values_from = Freq)))
}
Leadership_items <- 
  Leadership_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
Leadership_items
# OccupationalCommitment ####
OccupationalCommitment_items <- 
  tibble()
for (i in OccupationalCommitment.items) {
  OccupationalCommitment_items <- 
    OccupationalCommitment_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(OccupationalCommitment_full[i])),
            names_from = i,
            values_from = Freq)))
}
OccupationalCommitment_items <-
  OccupationalCommitment_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `7`) / n)
OccupationalCommitment_items
# OccupationalPerformanceSalary ####
OccupationalHours_items <- 
  tibble()
for (i in OccupationalHours.items) {
  OccupationalHours_items <- 
    OccupationalHours_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(OccupationalHours_full[i])),
            names_from = i,
            values_from = Freq)))
}
OccupationalHours_items <- 
  OccupationalHours_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `75`) / n)
OccupationalHours_items %>% 
  select(
    c(var, ceilingFloor))
S2_OccupationalSalaryPerHour <-
  tibble(var = "S2_OccupationalSalaryPerHour") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(OccupationalSalary_full$S2_OccupationalSalaryPerHour)),
      names_from = Var1,
      values_from = Freq))
S2_OccupationalSalaryPerHour <- 
  S2_OccupationalSalaryPerHour %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`5` + `50`) / n)
S2_OccupationalSalaryPerHour %>% 
  select(
    c(var, ceilingFloor))
S2_OccupationalSalaryPerDay <-
  tibble(var = "S2_OccupationalSalaryPerDay") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(OccupationalSalary_full$S2_OccupationalSalaryPerDay)),
      names_from = Var1,
      values_from = Freq))
S2_OccupationalSalaryPerDay <- 
  S2_OccupationalSalaryPerDay %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`25` + `1000`) / n)
S2_OccupationalSalaryPerDay %>% 
  select(
    c(var, ceilingFloor))
S2_OccupationalSalaryPerWeek <-
  tibble(var = "S2_OccupationalSalaryPerWeek") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(OccupationalSalary_full$S2_OccupationalSalaryPerWeek)),
      names_from = Var1,
      values_from = Freq))
S2_OccupationalSalaryPerWeek <- 
  S2_OccupationalSalaryPerWeek %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`50` + `2200`) / n)
S2_OccupationalSalaryPerWeek %>% 
  select(
    c(var, ceilingFloor))
S2_OccupationalSalaryPerMonth <-
  tibble(var = "S2_OccupationalSalaryPerMonth") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(OccupationalSalary_full$S2_OccupationalSalaryPerMonth)),
      names_from = Var1,
      values_from = Freq))
S2_OccupationalSalaryPerMonth <- 
  S2_OccupationalSalaryPerMonth %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`100` + `11000`) / n)
S2_OccupationalSalaryPerMonth %>% 
  select(
    c(var, ceilingFloor))
S2_OccupationalSalaryPerYear <-
  tibble(var = "S2_OccupationalSalaryPerYear") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(OccupationalSalary_full$S2_OccupationalSalaryPerYear)),
      names_from = Var1,
      values_from = Freq))
S2_OccupationalSalaryPerYear <- 
  S2_OccupationalSalaryPerYear %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`3000` + `2e+05`) / n)
S2_OccupationalSalaryPerYear %>% 
  select(
    c(var, ceilingFloor))
# PersonalityDisordersAggression ####
PersonalityDisordersAggression_items <- 
  tibble()
for (i in PersonalityDisordersAggression.items) {
  PersonalityDisordersAggression_items <-
    PersonalityDisordersAggression_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersAggression_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersAggression_items <-
  PersonalityDisordersAggression_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersAggression_items
# PersonalityDisordersDependency ####
PersonalityDisordersDependency_items <- 
  tibble()
for (i in PersonalityDisordersDependency.items) {
  PersonalityDisordersDependency_items <-
    PersonalityDisordersDependency_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersDependency_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersDependency_items <-
  PersonalityDisordersDependency_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersDependency_items
# PersonalityDisordersDetachment ####
PersonalityDisordersDetachment_items <- 
  tibble()
for (i in PersonalityDisordersDetachment.items) {
  PersonalityDisordersDetachment_items <-
    PersonalityDisordersDetachment_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersDetachment_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersDetachment_items <-
  PersonalityDisordersDetachment_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersDetachment_items
# PersonalityDisordersExhibitionism ####
PersonalityDisordersExhibitionism_items <- tibble()
for (i in PersonalityDisordersExhibitionism.items) {
  PersonalityDisordersExhibitionism_items <-
    PersonalityDisordersExhibitionism_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersExhibitionism_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersExhibitionism_items <-
  PersonalityDisordersExhibitionism_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersExhibitionism_items
# PersonalityDisordersImpulsivity ####
PersonalityDisordersImpulsivity_items <- 
  tibble()
for (i in PersonalityDisordersImpulsivity.items) {
  PersonalityDisordersImpulsivity_items <-
    PersonalityDisordersImpulsivity_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersImpulsivity_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersImpulsivity_items <-
  PersonalityDisordersImpulsivity_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersImpulsivity_items
# PersonalityDisordersManipulativeness ####
PersonalityDisordersManipulativeness_items <- tibble()
for (i in PersonalityDisordersManipulativeness.items) {
  PersonalityDisordersManipulativeness_items <-
    PersonalityDisordersManipulativeness_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersManipulativeness_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersManipulativeness_items <-
  PersonalityDisordersManipulativeness_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersManipulativeness_items
# PersonalityDisordersPropriety ####
PersonalityDisordersPropriety_items <-
  tibble()
for (i in PersonalityDisordersPropriety.items) {
  PersonalityDisordersPropriety_items <-
    PersonalityDisordersPropriety_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersPropriety_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersPropriety_items <-
  PersonalityDisordersPropriety_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersPropriety_items
# PersonalityDisordersSelfHarm ####
PersonalityDisordersSelfHarm_items <-
  tibble()
for (i in PersonalityDisordersSelfHarm.items) {
  PersonalityDisordersSelfHarm_items <-
    PersonalityDisordersSelfHarm_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PersonalityDisordersSelfHarm_full[i])),
            names_from = i,
            values_from = Freq)))
}
PersonalityDisordersSelfHarm_items <-
  PersonalityDisordersSelfHarm_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PersonalityDisordersSelfHarm_items
# RealisticInterests ####
RealisticInterests_items <- 
  tibble()
for (i in RealisticInterests.items) {
  RealisticInterests_items <- 
    RealisticInterests_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RealisticInterests_full[i])),
            names_from = i,
            values_from = Freq)))
}
RealisticInterests_items <- 
  RealisticInterests_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
RealisticInterests_items
# RiskyDriving ####
RiskyDriving_items <- 
  tibble()
for (i in RiskyDriving.items) {
  RiskyDriving_items <- 
    RiskyDriving_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RiskyDriving_full[i])),
            names_from = i,
            values_from = Freq)))
}
RiskyDriving_items <- 
  RiskyDriving_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
RiskyDriving_items
# RiskyViolence - 5 of 5 variables show ceiling/floor effects ####
RiskyViolence_items_semiRaw <- 
  tibble()
for (i in RiskyViolence.items_recode) {
  RiskyViolence_items_semiRaw <- 
    RiskyViolence_items_semiRaw %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RiskyViolence_full[i])),
            names_from = i,
            values_from = Freq)))
}
RiskyViolence_items_semiRaw <- 
  RiskyViolence_items_semiRaw %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`0` + `10`) / n)
RiskyViolence_items_semiRaw %>% 
  select(
    c(var, n, ceilingFloor))
RiskyViolence_items <- 
  tibble()
for (i in RiskyViolence.items) {
  RiskyViolence_items <- 
    RiskyViolence_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RiskyViolence_full[i])),
            names_from = i,
            values_from = Freq)))
}
RiskyViolence_items <- 
  RiskyViolence_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`0` + `2`) / n)
RiskyViolence_items
# SeekingSupport ####
SeekingSupport_items <- 
  tibble()
for (i in SeekingSupport.items) {
  SeekingSupport_items <- 
    SeekingSupport_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(SeekingSupport_full[i])),
            names_from = i,
            values_from = Freq)))
}
SeekingSupport_items <- 
  SeekingSupport_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `4`) / n)
SeekingSupport_items
# SocialInterests ####
SocialInterests_items <- 
  tibble()
for (i in SocialInterests.items) {
  SocialInterests_items <-
    SocialInterests_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(SocialInterests_full[i])),
            names_from = i,
            values_from = Freq)))
}
SocialInterests_items <- 
  SocialInterests_items %>%
  mutate(
    n = rowSums(
      select(
        .,
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
SocialInterests_items
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("outcomeFrames_S2.R finished running!")
Sys.sleep(5)  