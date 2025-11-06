print("Running primaryAnalyses.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
##############################
## Define Rounding Function ##
##############################
round2 <-     # Function to always round 0.5 up
  function(x, digits = 0) { 
    posneg <- 
      sign(x)
    z <- 
      abs(x) * 10 ^ digits
    z <- 
      z + 0.5
    z <- 
      trunc(z)
    z <- 
      z / 10 ^ digits
    z * posneg
  }
######################
## Define variables ##
######################
traits <- 
  c("PersonalityNegativeEmotionality", "PersonalityExtraversion",
    "PersonalityOpenMindedness", "PersonalityAgreeableness",
    "PersonalityConscientiousness")
trait.combos <-
  suppressMessages(# Suppress message confirming naming
    as_tibble(
      t(
        as_tibble(
          combn(traits, 2),
          .name_repair = "unique")),
      .name_repair = "unique"))
outcomes_S1 <-
  c("Gratitude", "Forgiveness", "Inspiration", "Humor",
    "IdentityIntegrationConsolidation", "LifeSatisfaction", "PositiveAffect",
    "NegativeAffect", "Happiness", "IdentityAchievement", "IdentityDiffusion",
    "IdentityForeclosure", "IdentityMoratorium", "ExistentialWellBeing",
    "composite_COTS_Religiousness", "ExistentialPhenomenologicalDimension",
    "ParanormalBeliefs", "RightWingAuthoritarianism", "Conservatism",
    "PeersAcceptanceFriendship", "Attractiveness", "PeerStatus",
    "RomanticSatisfaction", "RomanticConflict", "RomanticAbuse",
    "FamilyMotherAffectional", "FamilyFatherAffectional", 
    "FamilyMotherFunctional", "FamilyFatherFunctional", "AntisocialBehavior",
    "CriminalBehavior")
outcomes_S2 <- 
  c("PersonalityDisordersMistrust", "PersonalityDisordersManipulativeness",
    "PersonalityDisordersAggression", "PersonalityDisordersSelfHarm",
    "PersonalityDisordersEccentricPerceptions", 
    "PersonalityDisordersDependency", "PersonalityDisordersExhibitionism",
    "PersonalityDisordersEntitlement", "PersonalityDisordersDetachment",
    "PersonalityDisordersImpulsivity", "PersonalityDisordersPropriety",
    "RealisticInterests", "InvestigativeInterests", "ArtisticInterests",
    "SocialInterests", "EnterprisingInterests", "ConventionalInterests",
    "composite_JobAttainment_ExtrinsicSuccess",
    "composite_OccupationalSatisfaction_IntrinsicSuccess",
    "OccupationalCommitment", "OccupationalPerformanceSalary", 
    "OccupationalInvolvement", "Leadership", "FinancialSecurity", 
    "Volunteerism", "ActiveCoping", "AvoidantCoping", "SeekingSupport",
    "Resilience", "RiskyActivity", "RiskyDriving", "RiskyViolence")
#############################
## Hierarchical Regression ##
#############################
S1.results <- 
  tibble()
for (i in outcomes_S1) {   # run regression analyses for each survey 1 outcome
  df_A <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_A.csv")) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_B.csv")) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  for (j in 1:10) {
    trait1 <- 
      as.character(trait.combos[j, 1])
    trait2 <- 
      as.character(trait.combos[j, 2])
    step1.model_A <-     # Step 1 Regression (Partition A)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_A)
    step2.model_A <-     # Step 2 Regression (Partition A)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_A)
    step1.model_B <-     # Step 1 Regression (Partition B)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_B)
    step2.model_B <-     # Step 2 Regression (Partition B)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_B)
    tidy.step2_A <- # tidy step 2 (partition A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (partition B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S1.results <- # Save analysis results
      S1.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          intercept_A = tidy.step2_A$estimate[1],
          intercept_A.95L = step2.95_A[1, 1],
          intercept_A.95U = step2.95_A[1, 2],
          trait1_A = tidy.step2_A$estimate[2],
          trait1_A.95L = step2.95_A[2, 1],
          trait1_A.95U = step2.95_A[2, 2],
          trait2_A = tidy.step2_A$estimate[3],
          trait2_A.95L = step2.95_A[3, 1],
          trait2_A.95U = step2.95_A[3, 2],
          interaction_A = tidy.step2_A$estimate[4],
          interaction_A.95L = step2.95_A[4, 1],
          interaction_A.95U = step2.95_A[4, 2],
          change_A.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A.p = tidy.step2_A$p.value[4],
          intercept_B = tidy.step2_B$estimate[1],
          intercept_B.95L = step2.95_B[1, 1],
          intercept_B.95U = step2.95_B[1, 2],
          trait1_B = tidy.step2_B$estimate[2],
          trait1_B.95L = step2.95_B[2, 1],
          trait1_B.95U = step2.95_B[2, 2],
          trait2_B = tidy.step2_B$estimate[3],
          trait2_B.95L = step2.95_B[3, 1],
          trait2_B.95U = step2.95_B[3, 2],
          interaction_B = tidy.step2_B$estimate[4],
          interaction_B.95L = step2.95_B[4, 1],
          interaction_B.95U = step2.95_B[4, 2],
          change_B.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B.p = tidy.step2_B$p.value[4]))
          }
}
S2.results <- 
  tibble()
for (i in outcomes_S2) {  # run regression analyses for each survey 2 outcome
  df_A <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_A.csv")) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_B.csv")) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  for (j in 1:10) {
    trait1 <- 
      as.character(trait.combos[j, 1])
    trait2 <-
      as.character(trait.combos[j, 2])
    step1.model_A <-     # Step 1 (A)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_A)
    step2.model_A <- # Step 2 (A)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_A)
    step1.model_B <-     # Step 1 (B)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_B)
    step2.model_B <-     # Step 2 (B)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <-# CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S2.results <- #  Save analysis results
      S2.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          intercept_A = tidy.step2_A$estimate[1],
          intercept_A.95L = step2.95_A[1, 1],
          intercept_A.95U = step2.95_A[1, 2],
          trait1_A = tidy.step2_A$estimate[2],
          trait1_A.95L = step2.95_A[2, 1],
          trait1_A.95U = step2.95_A[2, 2],
          trait2_A = tidy.step2_A$estimate[3],
          trait2_A.95L = step2.95_A[3, 1],
          trait2_A.95U = step2.95_A[3, 2],
          interaction_A = tidy.step2_A$estimate[4],
          interaction_A.95L = step2.95_A[4, 1],
          interaction_A.95U = step2.95_A[4, 2],
          change_A.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A.p = tidy.step2_A$p.value[4],
          intercept_B = tidy.step2_B$estimate[1],
          intercept_B.95L = step2.95_B[1, 1],
          intercept_B.95U = step2.95_B[1, 2],
          trait1_B = tidy.step2_B$estimate[2],
          trait1_B.95L = step2.95_B[2, 1],
          trait1_B.95U = step2.95_B[2, 2],
          trait2_B = tidy.step2_B$estimate[3],
          trait2_B.95L = step2.95_B[3, 1],
          trait2_B.95U = step2.95_B[3, 2],
          interaction_B = tidy.step2_B$estimate[4],
          interaction_B.95L = step2.95_B[4, 1],
          interaction_B.95U = step2.95_B[4, 2],
          change_B.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B.p = tidy.step2_B$p.value[4]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results <- 
  S1.results %>%
  bind_rows(S2.results) %>% # Join results from surveys 1 and 2 outcomes
  mutate(
    # Is Delta R-squared significant?
    A.significant = ifelse(
      round2(change_A.p, 3) < .05, 
      1, 
      0),
    B.significant = ifelse(
      round2(change_B.p, 3) < .05, 
      1, 
      0),
    # Is Delta R-squared meaningful (i.e., >= .01)?
    A.meaningful = ifelse(
      round2(change_A.r2, 2) >= .01, 
      1, 
      0),
    B.meaningful = ifelse(
      round2(change_B.r2, 2) >= .01, 
      1, 
      0),
    signs.match = ifelse( # Do signs of step 2 est match between partitions?
      sign(interaction_A) == sign(interaction_B), 
      1, 
      0)) %>%
  mutate(
    both.significant = ifelse( # Is Delta R-squared sig in both partitions?
      A.significant == 1 & 
        B.significant == 1 & 
        signs.match == 1, 
      1, 
      0),
    both.meaningful = ifelse( # Is Delta R-squared meaningful in both parts?
      A.meaningful == 1 & 
        B.meaningful == 1 & 
        signs.match == 1, 
      1, 
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal = ifelse(
      ((A.significant == 1 & 
          interaction_B >= interaction_A.95L & 
          interaction_B <= interaction_A.95U) | 
         (B.significant == 1 & 
            interaction_A >= interaction_B.95L &
            interaction_A <= interaction_B.95U)) & 
        both.significant == 0 & 
        signs.match == 1, 
      1, 
      0)) %>%
  mutate(   
    interpretation = ifelse( # What is the final interpretation?
      both.significant == 1 & 
        (A.meaningful == 1 | 
           B.meaningful == 1),
      "Replication - Meaningful Effect",
      ifelse(
        both.significant == 1,
        "Replication - Negligible Effect",
        ifelse(
          equivocal == 1 & 
            (A.meaningful == 1 | 
               B.meaningful == 1),
          "Equivocal Evidence of Replication (Meaningful Effect)",
          ifelse(
            equivocal == 1,
            "Equivocal Evidence of Replication (Negliglble Effect)",
            ifelse(
              A.significant == 1 | 
                B.significant == 1,
              "Spurious Effect",
              "No Effect"))))),
    change.mean = (change_A.r2 + change_B.r2) / 2)
write.csv(
  results, 
  "Data Analysis/Results/results_noRound.csv")
write.csv(
  results %>%
    mutate(
      across(
        c("intercept_A", "intercept_A.95L", "intercept_A.95U", "trait1_A",
          "trait1_A.95L", "trait1_A.95U", "trait2_A", "trait2_A.95L", 
          "trait2_A.95U", "interaction_A", "interaction_A.95L", 
          "interaction_A.95U", "change_A.r2", "intercept_B", "intercept_B.95L",
          "intercept_B.95U", "trait1_B", "trait1_B.95L", "trait1_B.95U",
          "trait2_B", "trait2_B.95L", "trait2_B.95U", "interaction_B",
          "interaction_B.95L", "interaction_B.95U", "change_B.r2"),
        ~ round2(.x, 2))) %>%
    mutate(
      across(
        c("change_A.p", "change_B.p"), 
        ~ round2(.x, 3))),
  "Data Analysis/Results/results.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("primaryAnalyses.R finished running!")
Sys.sleep(5)                                                           
