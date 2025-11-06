print("Running outcomeFrames_S1.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(haven)
###############
## Load data ##
###############
loopr_full_S1 <-
  read_sav("Data Analysis/Survey 1 - Merged - Working data file.sav")
######################
## Define variables ##
######################
traits_S1 <- 
  c("S1_PersonalityNegativeEmotionality", "S1_PersonalityExtraversion",
    "S1_PersonalityOpenMindedness", "S1_PersonalityAgreeableness",
    "S1_PersonalityConscientiousness")
# AntisocialBehavior ####
AntisocialQuestionnaire.items_raw <- 
  "S1_AntisocialBehavior2"
AntisocialInterview.items_raw <-
  c("S1_AntisocialBehavior1_1", "S1_AntisocialBehavior1_2",
    "S1_AntisocialBehavior3")
AntisocialHarter.items_raw <- 
  c("S1_AntisocialBehavior1_3", "S1_AntisocialBehavior1_4")
# CriminalBehavior ####
CriminalBehavior.items <-
  c("S1_CriminalBehavior_1", "S1_CriminalBehavior_2", "S1_CriminalBehavior_3",
    "S1_CriminalBehavior_4", "S1_CriminalBehavior_5", "S1_CriminalBehavior_6")
# ExistentialPhenomenologicalDimension ####
ExistentialPhenomenologicalDimension.items_raw <-
  c("S1_Spirituality_7", "S1_Spirituality_8", "S1_Spirituality_9",
    "S1_Spirituality_10", "S1_Spirituality_11", "S1_Spirituality_12")
ExistentialPhenomenologicalDimension.items_R <-
  c("S1_Spirituality_10", "S1_Spirituality_11", "S1_Spirituality_12")
ExistentialPhenomenologicalDimension.items <-
  c("S1_Spirituality_7", "S1_Spirituality_8", "S1_Spirituality_9",
    "S1_Spirituality_10_R", "S1_Spirituality_11_R", "S1_Spirituality_12_R")
# Forgiveness ####
Forgiveness.items_raw <- 
  c("S1_Forgiveness_1", "S1_Forgiveness_2", "S1_Forgiveness_3", 
    "S1_Forgiveness_4", "S1_Forgiveness_5", "S1_Forgiveness_6")
Forgiveness.items_R <- 
  c("S1_Forgiveness_4", "S1_Forgiveness_5", "S1_Forgiveness_6")
Forgiveness.items <-
  c("S1_Forgiveness_1", "S1_Forgiveness_2", "S1_Forgiveness_3",
    "S1_Forgiveness_4_R", "S1_Forgiveness_5_R", "S1_Forgiveness_6_R")
# Humor ####
Humor.items <- 
  "S1_Humor"
# IdentityAchievement ####
IdentityAchievement.items <-
  c("S1_IdentityStatus_1", "S1_IdentityStatus_2", "S1_IdentityStatus_3",
    "S1_IdentityStatus_4", "S1_IdentityStatus_5", "S1_IdentityStatus_6",
    "S1_IdentityStatus_7", "S1_IdentityStatus_8")
# IdentityDiffusion ####
IdentityDiffusion.items <-
  c("S1_IdentityStatus_9", "S1_IdentityStatus_10", "S1_IdentityStatus_11",
    "S1_IdentityStatus_12", "S1_IdentityStatus_13", "S1_IdentityStatus_14",
    "S1_IdentityStatus_15", "S1_IdentityStatus_16")
# IdentityForeclosure ####
IdentityForeclosure.items <-
  c("S1_IdentityStatus_17", "S1_IdentityStatus_18", "S1_IdentityStatus_19",
    "S1_IdentityStatus_20", "S1_IdentityStatus_21", "S1_IdentityStatus_22",
    "S1_IdentityStatus_23", "S1_IdentityStatus_24")
# IdentityIntegrationConsolidation ####
IdentityIntegrationConsolidation.items_raw <-
  c("S1_IdentityIntegration1", "S1_IdentityIntegration2",
    "S1_IdentityIntegration3", "S1_IdentityIntegration4",
    "S1_IdentityIntegration5", "S1_IdentityIntegration6")
IdentityIntegrationConsolidation.items_R <-
  c("S1_IdentityIntegration4", "S1_IdentityIntegration5",
    "S1_IdentityIntegration6")
IdentityIntegrationConsolidation.items <-
  c("S1_IdentityIntegration1", "S1_IdentityIntegration2",
    "S1_IdentityIntegration3", "S1_IdentityIntegration4_R",
    "S1_IdentityIntegration5_R", "S1_IdentityIntegration6_R")
# IdentityMoratorium ####
IdentityMoratorium.items <-
  c("S1_IdentityStatus_25", "S1_IdentityStatus_26", "S1_IdentityStatus_27",
    "S1_IdentityStatus_28", "S1_IdentityStatus_29", "S1_IdentityStatus_30",
    "S1_IdentityStatus_31", "S1_IdentityStatus_32")
# Inspiration ####
Inspiration.items <- 
  c("S1_Inspiration_1_1", "S1_Inspiration_1_2", "S1_Inspiration_1_3",
    "S1_Inspiration_1_4", "S1_Inspiration_2_1", "S1_Inspiration_2_2",
    "S1_Inspiration_2_3", "S1_Inspiration_2_4")
# LifeSatisfaction ####
LifeSatisfaction.items_raw <- 
  c("S1_Life_satisfaction_1", "S1_Life_satisfaction_2", 
    "S1_Life_satisfaction_3", "S1_Life_satisfaction_4",
    "S1_Life_satisfaction_5", "S1_Life_satisfaction_6")
LifeSatisfaction.items_recode_type1 <- 
  c("S1_Life_satisfaction_1", "S1_Life_satisfaction_2",
    "S1_Life_satisfaction_3")
LifeSatisfaction.items_recode_type2 <- 
  c("S1_Life_satisfaction_4", "S1_Life_satisfaction_5",
    "S1_Life_satisfaction_6")
LifeSatisfaction.items <- 
  c("S1_Life_satisfaction_1_recode", "S1_Life_satisfaction_2_recode",
    "S1_Life_satisfaction_3_recode", "S1_Life_satisfaction_4_recode",
    "S1_Life_satisfaction_5_recode", "S1_Life_satisfaction_6_recode")
# NegativeAffect ####
NegativeAffect.items <- 
  c("S1_PosNegAffect_6", "S1_PosNegAffect_7", "S1_PosNegAffect_8",
    "S1_PosNegAffect_9", "S1_PosNegAffect_10")
# PositiveAffect ####
PositiveAffect.items <- 
  c("S1_PosNegAffect_1", "S1_PosNegAffect_2", "S1_PosNegAffect_3",
    "S1_PosNegAffect_4", "S1_PosNegAffect_5")
# RightWingAuthoritarianism ####
RightWingAuthoritarianism.items_raw <- 
  c("S1_Authoritarianism_1", "S1_Authoritarianism_2", "S1_Authoritarianism_3",
    "S1_Authoritarianism_4", "S1_Authoritarianism_5", "S1_Authoritarianism_6")
RightWingAuthoritarianism.items_R <- 
  c("S1_Authoritarianism_4", "S1_Authoritarianism_5", "S1_Authoritarianism_6")
RightWingAuthoritarianism.items <- 
  c("S1_Authoritarianism_1", "S1_Authoritarianism_2", "S1_Authoritarianism_3",
    "S1_Authoritarianism_4_R", "S1_Authoritarianism_5_R",
    "S1_Authoritarianism_6_R")
# RomanticAbuse ####
RomanticAbuse.items <- 
  c("S1_RomanticAbuse_1", "S1_RomanticAbuse_2", "S1_RomanticAbuse_3",
    "S1_RomanticAbuse_4", "S1_RomanticAbuse_5", "S1_RomanticAbuse_6")
# RomanticConflict ####
RomanticConflict.items <-
  c("S1_RomanticConflict_1", "S1_RomanticConflict_2", "S1_RomanticConflict_3",
    "S1_RomanticConflict_4", "S1_RomanticConflict_5", "S1_RomanticConflict_6")
# RomanticSatisfaction ####
RomanticSatisfactionIntimacy.items <- 
  c("S1_RomanticSatisfaction_1", "S1_RomanticSatisfaction_2",
    "S1_RomanticSatisfaction_3")
RomanticSatisfactionConflict.items_raw <-
  c("S1_RomanticSatisfaction_4", "S1_RomanticSatisfaction_5",
    "S1_RomanticSatisfaction_6")
RomanticSatisfactionConflict.items <-
  c("S1_RomanticSatisfaction_4_R", "S1_RomanticSatisfaction_5_R",
    "S1_RomanticSatisfaction_6_R")
####################################################
## Create Outcome-Specific Item-Level Data Frames ##
####################################################
# AntisocialBehavior ####
AntisocialQuestionnaire_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(AntisocialQuestionnaire.items_raw), 
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(AntisocialQuestionnaire.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
AntisocialInterview_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(AntisocialInterview.items_raw), all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(AntisocialInterview.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
AntisocialHarter_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID,all_of(AntisocialHarter.items_raw), all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(AntisocialHarter.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# CriminalBehavior ####
CriminalBehavior_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(CriminalBehavior.items), S1_CriminalBehavior,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(CriminalBehavior.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# ExistentialPhenomenologicalDimension ####
ExistentialPhenomenologicalDimension_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(ExistentialPhenomenologicalDimension.items_raw),
      S1_ExistentialPhenomenologicalDimension, all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(ExistentialPhenomenologicalDimension.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(ExistentialPhenomenologicalDimension.items_R),
      ~ recode_factor(
        .x,
        `5` = 1,
        `4` = 2,
        `3` = 3,
        `2` = 4,
        `1` = 5,
        .ordered = TRUE),
      .names = "{.col}_R"))
# Forgiveness ####
Forgiveness_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(Forgiveness.items_raw), S1_Forgiveness, 
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(Forgiveness.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(Forgiveness.items_R),
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
# Humor ####
Humor_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(Humor.items), all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(Humor.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# IdentityAchievement ####
IdentityAchievement_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(IdentityAchievement.items), S1_IdentityAchievement,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(IdentityAchievement.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# IdentityDiffusion ####
IdentityDiffusion_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(IdentityDiffusion.items), S1_IdentityDiffusion,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(IdentityDiffusion.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# IdentityForeclosure ####
IdentityForeclosure_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(IdentityForeclosure.items), S1_IdentityForeclosure,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(IdentityForeclosure.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# IdentityIntegrationConsolidation ####
IdentityIntegrationConsolidation_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(IdentityIntegrationConsolidation.items_raw),
      S1_IdentityIntegrationConsolidation, all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(IdentityIntegrationConsolidation.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(IdentityIntegrationConsolidation.items_R),
      ~ recode_factor(
        .x,
        `9` = 1,
        `8` = 2,
        `7` = 3,
        `6` = 4,
        `5` = 5,
        `4` = 6,
        `3` = 7,
        `2` = 8,
        `1` = 9,
        .ordered = TRUE),
      .names = "{.col}_R"))
# IdentityMoratorium ####
IdentityMoratorium_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(IdentityMoratorium.items), S1_IdentityMoratorium,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(IdentityMoratorium.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# Inspiration ####
Inspiration_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(Inspiration.items), S1_Inspiration, 
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(Inspiration.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# LifeSatisfaction ####
LifeSatisfaction_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(LifeSatisfaction.items_raw), S1_LifeSatisfaction,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(LifeSatisfaction.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(LifeSatisfaction.items_recode_type1),
      ~ recode_factor(
        .x,
        `1` = 0,
        `2` = 0,
        `3` = 1,
        .ordered = TRUE),
      .names = "{.col}_recode")) %>%
  mutate(
    across(
      all_of(LifeSatisfaction.items_recode_type2),
      ~ recode_factor(
        .x,
        `2` = 0,
        `3` = 0,
        `1` = 1,
        .ordered = TRUE),
      .names = "{.col}_recode"))
# NegativeAffect ####
NegativeAffect_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(NegativeAffect.items), S1_NegativeAffect,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(NegativeAffect.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# PositiveAffect ####
PositiveAffect_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(PositiveAffect.items), S1_PositiveAffect, 
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(PositiveAffect.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# RightWingAuthoritarianism ####
RightWingAuthoritarianism_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(RightWingAuthoritarianism.items_raw),
      S1_RightWingAuthoritarianism, all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(RightWingAuthoritarianism.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(RightWingAuthoritarianism.items_R),
      ~ recode_factor(
        .x,
        `9` = 1,
        `8` = 2,
        `7` = 3,
        `6` = 4,
        `5` = 5,
        `4` = 6,
        `3` = 7,
        `2` = 8,
        `1` = 9,
        .ordered = TRUE),
      .names = "{.col}_R"))
# RomanticAbuse ####
RomanticAbuse_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(RomanticAbuse.items), S1_RomanticAbuse, 
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(RomanticAbuse.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# RomanticConflict ####
RomanticConflict_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(RomanticConflict.items), S1_RomanticConflict,
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(RomanticConflict.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
# RomanticSatisfaction ####
RomanticSatisfactionIntimacy_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(RomanticSatisfactionIntimacy.items),
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(RomanticSatisfactionIntimacy.items),
      ~ as_factor(.x, levels = "values", ordered = TRUE)))
RomanticSatisfactionConflict_full <- 
  loopr_full_S1 %>%
  select(
    c(CaseID, all_of(RomanticSatisfactionConflict.items_raw),
      all_of(traits_S1))) %>%
  mutate(
    across(
      all_of(RomanticSatisfactionConflict.items_raw),
      ~ as_factor(.x, levels = "values", ordered = TRUE))) %>%
  mutate(
    across(
      all_of(RomanticSatisfactionConflict.items_raw),
      ~ recode_factor(
        .x,
        `5` = 1,
        `4` = 2,
        `3` = 3,
        `2` = 4,
        `1` = 5),
      .names = "{.col}_R"))
###################################
## Examine Ceiling/Floor Effects ##
###################################
# AntisocialBehavior ####
AntisocialQuestionnaire_items <- 
  tibble()
for (i in AntisocialQuestionnaire.items_raw) {
  AntisocialQuestionnaire_items <- 
    AntisocialQuestionnaire_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(AntisocialQuestionnaire_full[i])),
            names_from = i,
            values_from = Freq)))
}
AntisocialQuestionnaire_items <- 
  AntisocialQuestionnaire_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
AntisocialQuestionnaire_items
AntisocialInterview_items_part1 <- 
  tibble()
for (i in AntisocialInterview.items_raw[1:2]) {
  AntisocialInterview_items_part1 <-
    AntisocialInterview_items_part1 %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(AntisocialInterview_full[i])),
            names_from = i,
            values_from = Freq)))
}
AntisocialInterview_items_part1 <-
  AntisocialInterview_items_part1 %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `4`) / n)
AntisocialInterview_items_part1
AntisocialInterview_items_part2 <-
  tibble(var = "S1_AntisocialBehavior3") %>%
  bind_cols(
    pivot_wider(
      as_tibble(
        ftable(AntisocialInterview_full$S1_AntisocialBehavior3)),
      names_from = Var1,
      values_from = Freq))
AntisocialInterview_items_part2 <-
  AntisocialInterview_items_part2 %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
AntisocialInterview_items_part2
AntisocialHarter_items <- 
  tibble()
for (i in AntisocialHarter.items_raw) {
  AntisocialHarter_items <- 
    AntisocialHarter_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(AntisocialHarter_full[i])),
            names_from = i,
            values_from = Freq)))
}
AntisocialHarter_items <- 
  AntisocialHarter_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `4`) / n)
AntisocialHarter_items
# CriminalBehavior - 3 of 6 variables show ceiling/floor effects ####
CriminalBehavior_items <- 
  tibble()
for (i in CriminalBehavior.items) {
  CriminalBehavior_items <- 
    CriminalBehavior_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(CriminalBehavior_full[i])),
            names_from = i,
            values_from = Freq)))
}
CriminalBehavior_items <- 
  CriminalBehavior_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `4`) / n)
CriminalBehavior_items
# ExistentialPhenomenologicalDimension ####
ExistentialPhenomenologicalDimension_items <-
  tibble()
for (i in ExistentialPhenomenologicalDimension.items) {
  ExistentialPhenomenologicalDimension_items <-
    ExistentialPhenomenologicalDimension_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(ExistentialPhenomenologicalDimension_full[i])),
            names_from = i,
            values_from = Freq)))
}
ExistentialPhenomenologicalDimension_items <-
  ExistentialPhenomenologicalDimension_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
ExistentialPhenomenologicalDimension_items
# Forgiveness #####
Forgiveness_items <- 
  tibble()
for (i in Forgiveness.items) {
  Forgiveness_items <- 
    Forgiveness_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(Forgiveness_full[i])),
            names_from = i,
            values_from = Freq)))
}
Forgiveness_items <- 
  Forgiveness_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `7`) / n)
Forgiveness_items
# IdentityAchievement ####
IdentityAchievement_items <- 
  tibble()
for (i in IdentityAchievement.items) {
  IdentityAchievement_items <-
    IdentityAchievement_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IdentityAchievement_full[i])),
            names_from = i,
            values_from = Freq)))
}
IdentityAchievement_items <-
  IdentityAchievement_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `6`) / n)
IdentityAchievement_items
# IdentityDiffusion ####
IdentityDiffusion_items <- 
  tibble()
for (i in IdentityDiffusion.items) {
  IdentityDiffusion_items <-
    IdentityDiffusion_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IdentityDiffusion_full[i])),
            names_from = i,
            values_from = Freq)))
}
IdentityDiffusion_items <- 
  IdentityDiffusion_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `6`) / n)
IdentityDiffusion_items
# IdentityForeclosure ####
IdentityForeclosure_items <- 
  tibble()
for (i in IdentityForeclosure.items) {
  IdentityForeclosure_items <- 
    IdentityForeclosure_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IdentityForeclosure_full[i])),
            names_from = i,
            values_from = Freq)))
}
IdentityForeclosure_items <- 
  IdentityForeclosure_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `6`) / n)
IdentityForeclosure_items
# IdentityIntegrationConsolidation ####
IdentityIntegrationConsolidation_items <- 
  tibble()
for (i in IdentityIntegrationConsolidation.items) {
  IdentityIntegrationConsolidation_items <-
    IdentityIntegrationConsolidation_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IdentityIntegrationConsolidation_full[i])),
            names_from = i,
            values_from = Freq)))
}
IdentityIntegrationConsolidation_items <-
  IdentityIntegrationConsolidation_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `9`) / n)
IdentityIntegrationConsolidation_items
# IdentityMoratorium ####
IdentityMoratorium_items <- 
  tibble()
for (i in IdentityMoratorium.items) {
  IdentityMoratorium_items <- 
    IdentityMoratorium_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(IdentityMoratorium_full[i])),
            names_from = i,
            values_from = Freq)))
}
IdentityMoratorium_items <- 
  IdentityMoratorium_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `6`) / n)
IdentityMoratorium_items
# Inspiration ####
Inspiration_items <- 
  tibble()
for (i in Inspiration.items) {
  Inspiration_items <-
    Inspiration_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(Inspiration_full[i])),
            names_from = i,
            values_from = Freq)))
}
Inspiration_items <- 
  Inspiration_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `7`) / n)
Inspiration_items
# Humor ####
Humor_items <- 
  tibble()
for (i in Humor.items) {
  Humor_items <-
    Humor_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(Humor_full[i])),
            names_from = i,
            values_from = Freq)))
}
Humor_items <- 
  Humor_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
Humor_items
# LifeSatisfaction ####
LifeSatisfaction_items <- 
  tibble()
for (i in LifeSatisfaction.items_raw) {
  LifeSatisfaction_items <- 
    LifeSatisfaction_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(LifeSatisfaction_full[i])),
            names_from = i,
            values_from = Freq)))
}
LifeSatisfaction_items <- 
  LifeSatisfaction_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `3`) / n)
LifeSatisfaction_items
# NegativeAffect ####
NegativeAffect_items <- 
  tibble()
for (i in NegativeAffect.items) {
  NegativeAffect_items <- 
    NegativeAffect_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(NegativeAffect_full[i])),
            names_from = i,
            values_from = Freq)))
}
NegativeAffect_items <- 
  NegativeAffect_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
NegativeAffect_items
# PositiveAffect ####
PositiveAffect_items <- 
  tibble()
for (i in PositiveAffect.items) {
  PositiveAffect_items <- 
    PositiveAffect_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(PositiveAffect_full[i])),
            names_from = i,
            values_from = Freq)))
}
PositiveAffect_items <- 
  PositiveAffect_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
PositiveAffect_items
# RightWingAuthoritarianism ####
RightWingAuthoritarianism_items <- 
  tibble()
for (i in RightWingAuthoritarianism.items) {
  RightWingAuthoritarianism_items <-
    RightWingAuthoritarianism_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RightWingAuthoritarianism_full[i])),
            names_from = i,
            values_from = Freq)))
}
RightWingAuthoritarianism_items <-
  RightWingAuthoritarianism_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `9`) / n)
RightWingAuthoritarianism_items
# RomanticAbuse ####
RomanticAbuse_items <- 
  tibble()
for (i in RomanticAbuse.items) {
  RomanticAbuse_items <- 
    RomanticAbuse_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RomanticAbuse_full[i])),
            names_from = i,
            values_from = Freq)))
}
RomanticAbuse_items <-
  RomanticAbuse_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `2`) / n)
RomanticAbuse_items
# RomanticConflict ####
RomanticConflict_items <- 
  tibble()
for (i in RomanticConflict.items) {
  RomanticConflict_items <- 
    RomanticConflict_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RomanticConflict_full[i])), 
            names_from = i,
            values_from = Freq)))
}
RomanticConflict_items <- 
  RomanticConflict_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `3`) / n)
RomanticConflict_items
# RomanticSatisfaction  ####
RomanticSatisfactionIntimacy_items <- 
  tibble()
for (i in RomanticSatisfactionIntimacy.items) {
  RomanticSatisfactionIntimacy_items <-
    RomanticSatisfactionIntimacy_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RomanticSatisfactionIntimacy_full[i])),
            names_from = i,
            values_from = Freq)))
}
RomanticSatisfactionIntimacy_items <-
  RomanticSatisfactionIntimacy_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
RomanticSatisfactionIntimacy_items
RomanticSatisfactionConflict_items <- 
  tibble()
for (i in RomanticSatisfactionConflict.items) {
  RomanticSatisfactionConflict_items <-
    RomanticSatisfactionConflict_items %>%
    bind_rows(
      tibble(var = i) %>%
        bind_cols(
          pivot_wider(
            as_tibble(
              ftable(RomanticSatisfactionConflict_full[i])),
            names_from = i,
            values_from = Freq)))
}
RomanticSatisfactionConflict_items <-
  RomanticSatisfactionConflict_items %>%
  mutate(
    n = rowSums(
      select(
        ., 
        -var))) %>%
  mutate(
    ceilingFloor = (`1` + `5`) / n)
RomanticSatisfactionConflict_items
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("outcomeFrames_S1.R finished running!")
Sys.sleep(5)  