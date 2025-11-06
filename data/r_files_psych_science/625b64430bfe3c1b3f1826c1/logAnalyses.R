print("Running logAnalyses.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
library(DescTools)
##############################
## Define Rounding Function ##
##############################
round2 <-
  function(x, digits = 0) {
    # Function to always round 0.5 up
    posneg <- sign(x)
    z <- abs(x) * 10 ^ digits
    z <- z + 0.5
    z <- trunc(z)
    z <- z / 10 ^ digits
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
  suppressMessages( # Suppress message confirming naming
    as_tibble(
      t(
        as_tibble(
          combn(traits, 2), 
          .name_repair = "unique")),
      .name_repair = "unique"))
log_outcome_S1 <- 
  "RomanticDissolution"
log_outcome_S2 <- 
  c("OccupationalPerformanceTurnover", "OccupationalPerformanceStatusChange",
    "RiskyEating", "RiskyAlcoholDrunkPrevalence", "RiskyTobaccoPrevalence",
    "RiskyDrugsPrevalence", "RiskySuicide", "HeartDisease", "SubstanceAbuse",
    "Anxiety", "Depression")
#############################
## Hierarchical Regression ##
#############################
set.seed(30605) # Set seed for reproducibility
S1_log.results <- 
  tibble()
for (i in log_outcome_S1) {   # run regression analyses for S1 binary outcome
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
      glm(get(i) ~ get(trait1) + get(trait2), 
          family = binomial(link = "logit"), data = df_A)
    step2.model_A <-     # Step 2 Regression (Partition A)
      glm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
          family = binomial(link = "logit"), data = df_A)
    step1.model_B <-     # Step 1 Regression (Partition B)
      glm(get(i) ~ get(trait1) + get(trait2), 
          family = binomial(link = "logit"), data = df_B)
    step2.model_B <-     # Step 2 Regression (Partition B)
      glm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
          family = binomial(link = "logit"), data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S1_log.results <- # Save analysis results
      S1_log.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          intercept_A_log = tidy.step2_A$estimate[1],
          intercept_A_log.95L = step2.95_A[1, 1],
          intercept_A_log.95U = step2.95_A[1, 2],
          trait1_A_log = tidy.step2_A$estimate[2],
          trait1_A_log.95L = step2.95_A[2, 1],
          trait1_A_log.95U = step2.95_A[2, 2],
          trait2_A_log = tidy.step2_A$estimate[3],
          trait2_A_log.95L = step2.95_A[3, 1],
          trait2_A_log.95U = step2.95_A[3, 2],
          interaction_A_log = tidy.step2_A$estimate[4],
          interaction_A_log.95L = step2.95_A[4, 1],
          interaction_A_log.95U = step2.95_A[4, 2],
          change_A_log.r2 = PseudoR2(step2.model_A, which = "AldrichNelson") -
            PseudoR2(step1.model_A, which = "AldrichNelson"), 
          change_A_log.p = tidy(step2.model_A)$p.value[4],
          intercept_B_log = tidy.step2_B$estimate[1],
          intercept_B_log.95L = step2.95_B[1, 1],
          intercept_B_log.95U = step2.95_B[1, 2],
          trait1_B_log = tidy.step2_B$estimate[2],
          trait1_B_log.95L = step2.95_B[2, 1],
          trait1_B_log.95U = step2.95_B[2, 2],
          trait2_B_log = tidy.step2_B$estimate[3],
          trait2_B_log.95L = step2.95_B[3, 1],
          trait2_B_log.95U = step2.95_B[3, 2],
          interaction_B_log = tidy.step2_B$estimate[4],
          interaction_B_log.95L = step2.95_B[4, 1],
          interaction_B_log.95U = step2.95_B[4, 2],
          change_B_log.r2 = PseudoR2(step2.model_B, which = "AldrichNelson") -
            PseudoR2(step1.model_B, which = "AldrichNelson"), 
          change_B_log.p = tidy(step2.model_B)$p.value[4]))
  }
}
S2_log.results <- 
  tibble()
for (i in log_outcome_S2) {   # run regression analyses for S2 binary outcomes
  df_A <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_A.csv")) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0("Data Analysis/Splits/", i, "_B.csv")) %>%
    mutate(
      across(all_of(traits), 
             ~ scale(.x, scale = F)))
  for (j in 1:10) {
    trait1 <- 
      as.character(trait.combos[j, 1])
    trait2 <- 
      as.character(trait.combos[j, 2])
    step1.model_A <-     # Step 1 Regression (Partition A)
      glm(get(i) ~ get(trait1) + get(trait2), 
          family = binomial(link = "logit"), data = df_A)
    step2.model_A <-     # Step 2 Regression (Partition A)
      glm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
          family = binomial(link = "logit"), data = df_A)
    step1.model_B <- # Step 1 Regression (Partition B)
      glm(get(i) ~ get(trait1) + get(trait2), 
          family = binomial(link = "logit"), data = df_B)
    step2.model_B <-     # Step 2 Regression (Partition B)
      glm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
          family = binomial(link = "logit"), data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    change_A <- # Delta R-square (A)
    S2_log.results <- # Save analysis results
      S2_log.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          intercept_A_log = tidy.step2_A$estimate[1],
          intercept_A_log.95L = step2.95_A[1, 1],
          intercept_A_log.95U = step2.95_A[1, 2],
          trait1_A_log = tidy.step2_A$estimate[2],
          trait1_A_log.95L = step2.95_A[2, 1],
          trait1_A_log.95U = step2.95_A[2, 2],
          trait2_A_log = tidy.step2_A$estimate[3],
          trait2_A_log.95L = step2.95_A[3, 1],
          trait2_A_log.95U = step2.95_A[3, 2],
          interaction_A_log = tidy.step2_A$estimate[4],
          interaction_A_log.95L = step2.95_A[4, 1],
          interaction_A_log.95U = step2.95_A[4, 2],
          change_A_log.r2 = PseudoR2(step2.model_A, which = "AldrichNelson") -
            PseudoR2(step1.model_A, which = "AldrichNelson"), 
          change_A_log.p = tidy(step2.model_A)$p.value[4],
          intercept_B_log = tidy.step2_B$estimate[1],
          intercept_B_log.95L = step2.95_B[1, 1],
          intercept_B_log.95U = step2.95_B[1, 2],
          trait1_B_log = tidy.step2_B$estimate[2],
          trait1_B_log.95L = step2.95_B[2, 1],
          trait1_B_log.95U = step2.95_B[2, 2],
          trait2_B_log = tidy.step2_B$estimate[3],
          trait2_B_log.95L = step2.95_B[3, 1],
          trait2_B_log.95U = step2.95_B[3, 2],
          interaction_B_log = tidy.step2_B$estimate[4],
          interaction_B_log.95L = step2.95_B[4, 1],
          interaction_B_log.95U = step2.95_B[4, 2],
          change_B_log.r2 = PseudoR2(step2.model_B, which = "AldrichNelson") -
            PseudoR2(step1.model_B, which = "AldrichNelson"), 
          change_B_log.p = tidy(step2.model_B)$p.value[4]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_log <- 
  S1_log.results %>%
  bind_rows(S2_log.results) %>% # Join results from surveys 1 and 2 outcomes
  mutate(     
    # Is Delta R-squared significant?
    A_log.significant = ifelse(
      round2(change_A_log.p, 3) < .05,
      1, 
      0),
    B_log.significant = ifelse(
      round2(change_B_log.p, 3) < .05, 
      1, 
      0),
    # Is Delta R-squared meaningful (i.e., >= .01)?
    A_log.meaningful = ifelse(
      round2(change_A_log.r2, 2) >= .01,
      1, 
      0),
    B_log.meaningful = ifelse(
      round2(change_B_log.r2, 2) >= .01, 
      1, 
      0),
    signs.match_log = # Do signs of step 2 estimates match between partitions?
      ifelse(
        sign(interaction_A_log) == sign(interaction_B_log), 
        1,
        0)) %>%
  mutate(
    both.significant_log = ifelse( # Is Delta R-squared sig in both partitions?
      A_log.significant == 1 &
        B_log.significant == 1 &
        signs.match_log == 1,
      1,
      0),
    both.meaningful_log = ifelse( # Is Delta R-squared meaningful in both part?
      A_log.meaningful == 1 &
        B_log.meaningful == 1 &
        signs.match_log == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_log = ifelse(
      ((A_log.significant == 1 &
          interaction_B_log >= interaction_A_log.95L &
          interaction_B_log <= interaction_A_log.95U) |
         (B_log.significant == 1 & 
            interaction_A_log >= interaction_B_log.95L &
            interaction_A_log <= interaction_B_log.95U)) &
        both.significant_log == 0 &
        signs.match_log == 1,
      1,
      0)) %>%
  mutate(   # What is the final interpretation in primary analyses?
    interpretation_log = ifelse(
      both.significant_log == 1 & 
        (A_log.meaningful == 1 |
           B_log.meaningful == 1),
      "Replication - Meaningful Effect",
      ifelse(
        both.significant_log == 1,
        "Replication - Negligible Effect",
        ifelse(
          equivocal_log == 1 & 
            (A_log.meaningful == 1 |
               B_log.meaningful == 1),
          "Equivocal Evidence of Replication (Meaningful Effect)",
          ifelse(
            equivocal_log == 1,
            "Equivocal Evidence of Replication (Negliglble Effect)",
            ifelse(
              A_log.significant == 1 | 
                B_log.significant == 1,
              "Spurious Effect",
              "No Effect"))))),
    change.mean = (change_A_log.r2 + change_B_log.r2) / 2)
write.csv(
  results_log, 
  "Data Analysis/Results/results_log_noRound.csv")
write.csv(
  results_log %>%
    mutate(
      across(
        c("intercept_A_log", "intercept_A_log.95L", "intercept_A_log.95U",
          "trait1_A_log", "trait1_A_log.95L", "trait1_A_log.95U", 
          "trait2_A_log", "trait2_A_log.95L", "trait2_A_log.95U",
          "interaction_A_log", "interaction_A_log.95L",
          "interaction_A_log.95U", "change_A_log.r2", "intercept_B_log",
          "intercept_B_log.95L", "intercept_B_log.95U", "trait1_B_log",
          "trait1_B_log.95L", "trait1_B_log.95U", "trait2_B_log",
          "trait2_B_log.95L", "trait2_B_log.95U", "interaction_B_log",
          "interaction_B_log.95L", "interaction_B_log.95U", "change_B_log.r2"),
        ~ round2(.x, 2))) %>%
    mutate(
      across(
        c("change_A_log.p", "change_B_log.p"),
        ~ round2(.x, 3))),
  "Data Analysis/Results/results_log.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("logAnalyses.R finished running!")
Sys.sleep(5)                                                           