print("Running suppAnalyses.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
##############################
## Define Rounding Function ##
##############################
round2 <-
  function(x, digits = 0) {     # Function to always round 0.5 up
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
  suppressMessages( # Suppress message confirming naming
    as_tibble(
      t(
        as_tibble(
          combn(traits, 2), 
          .name_repair = "unique")),
      .name_repair = "unique"))
supp_outcome_S1 <- 
  "DatingVariety"
supp_outcome_S2 <- 
  c("PersonalityDisordersWorkaholism", "RiskySex")
#############################
## Hierarchical Regression ##
#############################
S1_supp.results <- 
  tibble()
for (i in supp_outcome_S1) {   # run regression for each survey 1 supp. outcome
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
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <-# tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- #  CI for step 2 est. (A)
      confint(step2.model_A, level = .95)
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S1_supp.results <- # Save analysis results
      S1_supp.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_supp = tidy.step2_A$estimate[4],
          step2_A_supp.95L = step2.95_A[4, 1],
          step2_A_supp.95U = step2.95_A[4, 2],
          change_A_supp.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A_supp.p = tidy.step2_A$p.value[4],
          estimate_B_supp = tidy.step2_B$estimate[4],
          step2_B_supp.95L = step2.95_B[4, 1],
          step2_B_supp.95U = step2.95_B[4, 2],
          change_B_supp.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B_supp.p = tidy.step2_B$p.value[4]))
  }
}
S2_supp.results <- 
  tibble()
for (i in supp_outcome_S2) {   # run regression for each survey 2 supp. outcome
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
    step1.model_A <-     # Step 1 (A)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_A)
    step2.model_A <-     # Step 2 (A)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_A)
    step1.model_B <- # Step 1 (B)
      lm(get(i) ~ get(trait1) + get(trait2), data = df_B)
    step2.model_B <- # Step 2 (B)
      lm(get(i) ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S2_supp.results <- # Save analysis results
      S2_supp.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_supp = tidy.step2_A$estimate[4],
          step2_A_supp.95L = step2.95_A[4, 1],
          step2_A_supp.95U = step2.95_A[4, 2],
          change_A_supp.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A_supp.p = tidy.step2_A$p.value[4],
          estimate_B_supp = tidy.step2_B$estimate[4],
          step2_B_supp.95L = step2.95_B[4, 1],
          step2_B_supp.95U = step2.95_B[4, 2],
          change_B_supp.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B_supp.p = tidy.step2_B$p.value[4]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_supp <- 
  S1_supp.results %>%
  bind_rows(S2_supp.results) %>% # Join results from surveys 1 and 2 outcomes
  mutate(
    # Is Delta R-squared significant?
    A_supp.significant = ifelse(
      round2(change_A_supp.p, 3) < .05, 
      1, 
      0),
    B_supp.significant = ifelse(
      round2(change_B_supp.p, 3) < .05, 
      1, 
      0),
    # Is Delta R-squared meaningful (i.e., >= .01)?
    A_supp.meaningful = ifelse(
      round2(change_A_supp.r2, 2) >= .01, 
      1, 
      0),
    B_supp.meaningful = ifelse(
      round2(change_B_supp.r2, 2) >= .01, 
      1, 
      0),
    signs.match_supp = ifelse(  # Do signs of step 2 est match between parts?
      sign(estimate_A_supp) == sign(estimate_B_supp),
      1, 
      0)) %>%
  mutate(
    both.significant_supp = ifelse( # Is Delta R-squared sig in both parts?
      A_supp.significant == 1 &
        B_supp.significant == 1 &
        signs.match_supp == 1,
      1,
      0),
    both.meaningful_supp = ifelse(# Is Delta R-sq meaningful in both parts?
      A_supp.meaningful == 1 &
        B_supp.meaningful == 1 &
        signs.match_supp == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_supp = ifelse(
      ((A_supp.significant == 1 &
          estimate_B_supp >= step2_A_supp.95L &
          estimate_B_supp <= step2_A_supp.95U) |
         (B_supp.significant == 1 &
            estimate_A_supp >= step2_B_supp.95L &
            estimate_A_supp <= step2_B_supp.95U)) &
        both.significant_supp == 0 &
        signs.match_supp == 1, 
      1, 
      0)) %>%
  mutate(
    interpretation_supp = ifelse( # What is the final interpretation?
      both.significant_supp == 1 & 
        (A_supp.meaningful == 1 |
           B_supp.meaningful == 1),
      "Replication - Meaningful Effect",
      ifelse(
        both.significant_supp == 1,
        "Replication - Negligible Effect",
        ifelse(
          equivocal_supp == 1 & 
            (A_supp.meaningful == 1 |
               B_supp.meaningful == 1),
          "Equivocal Evidence of Replication (Meaningful Effect)",
          ifelse(
            equivocal_supp == 1,
            "Equivocal Evidence of Replication (Negliglble Effect)",
            ifelse(
              A_supp.significant == 1 | 
                B_supp.significant == 1,
              "Spurious Effect",
              "No Effect"))))),
    change.mean = (change_A_supp.r2 + change_B_supp.r2) / 2)
write.csv(
  results_supp %>%
    mutate(
      across(
        c("estimate_A_supp", "step2_A_supp.95L", "step2_A_supp.95U",
          "change_A_supp.r2", "estimate_B_supp", "step2_B_supp.95L",
          "step2_B_supp.95U", "change_B_supp.r2"),
        ~ round2(.x, 2))) %>%
    mutate(
      across(
        c("change_A_supp.p", "change_B_supp.p"),
        ~ round2(.x, 3))),
  "Data Analysis/Results/results_supp.csv")
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print("suppAnalyses.R finished running!")
Sys.sleep(5)  