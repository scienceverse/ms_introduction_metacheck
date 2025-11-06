print("Running tobit.R ...")
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(VGAM)
library(DescTools)
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
###############
## Load data ##
###############
replications.S1 <-
  read_csv("Data Analysis/Results/replications.S1.csv") %>%
  filter(outcome == "CriminalBehavior")
for (i in unique(replications.S1$outcome)) {
  temp <- 
    replications.S1 %>%
    filter(outcome == i)
  assign(paste0("replications.S1.", i), temp)
}
replications.S1 <-
  unique(replications.S1$outcome)
replications.S2 <-
  read_csv("Data Analysis/Results/replications.S2.csv") %>%
  filter(outcome == "RiskyViolence")
for (i in unique(replications.S2$outcome)) {
  temp <-
    replications.S2 %>%
    filter(outcome == i)
  assign(paste0("replications.S2.", i), temp)
}
replications.S2 <- 
  unique(replications.S2$outcome)
######################
## Define variables ##
######################
traits <- 
  c("PersonalityExtraversion", "PersonalityAgreeableness",
    "PersonalityConscientiousness", "PersonalityNegativeEmotionality",
    "PersonalityOpenMindedness")
trait.combos <-
  suppressMessages(# Suppress message confirming naming
    as_tibble(
      t(
        as_tibble(
          combn(traits, 2), 
          .name_repair = "unique")),
      .name_repair = "unique"))
##########################################
## Robustness Checks - Tobit Regression ##
##########################################
set.seed(30605) # Set seed for reproducibility
# CriminalBehavior ####
CriminalBehavior.tobit.results <- 
  tibble()
df_A <-
  read_csv(paste0("Data Analysis/Splits/CriminalBehavior_A.csv")) %>%
  mutate(
    across(
      all_of(traits), 
      ~ scale(.x, scale = F)))
df_B <-
  read_csv(paste0("Data Analysis/Splits/CriminalBehavior_B.csv")) %>%
  mutate(
    across(
      all_of(traits), 
      ~ scale(.x, scale = F)))
for (j in 1:nrow(replications.S1.CriminalBehavior)) {
  trait1 <- 
    as.character(replications.S1.CriminalBehavior[j, 2])
  trait2 <- 
    as.character(replications.S1.CriminalBehavior[j, 3])
  step1.model_A <-
    vglm(CriminalBehavior ~ get(trait1) + get(trait2), family = tobit, 
         Lower = 1, maxit = 500, model = TRUE, data = df_A)
  step2.model_A <-
    vglm(CriminalBehavior ~ get(trait1) + get(trait2) + 
           get(trait1):get(trait2), family = tobit, Lower = 1, maxit = 500,
         model = TRUE, data = df_A)
  step1.model_B <-
    vglm(CriminalBehavior ~ get(trait1) + get(trait2), family = tobit,
         Lower = 1, maxit = 500, model = TRUE, data = df_B)
  step2.model_B <-
    vglm(CriminalBehavior ~ get(trait1) + get(trait2) +
           get(trait1):get(trait2), family = tobit, Lower = 1, maxit = 500,
         model = TRUE, data = df_B)
  step2.95_A <- # CI for step 2 est. (A)
    confint(step2.model_A, level = .95) 
  step2.95_B <- # CI for step 2 est. (B)
    confint(step2.model_B, level = .95)
  CriminalBehavior.tobit.results <-     # Save analysis results
    CriminalBehavior.tobit.results %>%
    bind_rows(
      tibble(
        outcome = "CriminalBehavior",
        trait1 = trait1,
        trait2 = trait2,
        estimate_A_tobit = step2.model_A@coefficients[5],
        step2_A_tobit.95L = step2.95_A[5, 1],
        step2_A_tobit.95U = step2.95_A[5, 2],
        change_A_tobit.r2 = 
          PseudoR2(step2.model_A, which = "McKelveyZavoina") -
          PseudoR2(step1.model_A, which = "McKelveyZavoina"),
        change_A_tobit.p = coef(summary(step2.model_A))[5, 4],
        estimate_B_tobit = step2.model_B@coefficients[5],
        step2_B_tobit.95L = step2.95_B[5, 1],
        step2_B_tobit.95U = step2.95_B[5, 2],
        change_B_tobit.r2 =
          PseudoR2(step2.model_B, which = "McKelveyZavoina") -
          PseudoR2(step1.model_B, which = "McKelveyZavoina"),       
        change_B_tobit.p = coef(summary(step2.model_B))[5, 4]))
}
# RiskyViolence ####
RiskyViolence.tobit.results <-
  tibble()
# run tobit analyses for RiskyViolence
df_A <-
  read_csv(paste0("Data Analysis/Splits/RiskyViolence_A.csv")) %>%
  mutate(
    across(
      all_of(traits), 
      ~ scale(.x, scale = F)))
df_B <- 
  read_csv(paste0("Data Analysis/Splits/RiskyViolence_B.csv")) %>%
  mutate(
    across(
      all_of(traits), 
      ~ scale(.x, scale = F)))
for (j in 1:nrow(replications.S2.RiskyViolence)) {
  trait1 <- 
    as.character(replications.S2.RiskyViolence[j, 2])
  trait2 <- 
    as.character(replications.S2.RiskyViolence[j, 3])
  step1.model_A <-
    vglm(RiskyViolence ~ get(trait1) + get(trait2), family = tobit, Lower = 1,
         maxit = 500, model = TRUE, data = df_A)
  step2.model_A <- 
    vglm(RiskyViolence ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         family = tobit, Lower = 1, maxit = 500, model = TRUE, data = df_A)
  step1.model_B <- 
    vglm(RiskyViolence ~ get(trait1) + get(trait2), family = tobit, Lower = 1,
         maxit = 500, model = TRUE, data = df_B)
  step2.model_B <- 
    vglm(RiskyViolence ~ get(trait1) + get(trait2) + get(trait1):get(trait2),
         family = tobit, Lower = 1, maxit = 500, model = TRUE, data = df_B)
  step2.95_A <-# CI for step 2 est. (A)
    confint(step2.model_A, level = .95) 
  step2.95_B <-# CI for step 2 est. (B)
    confint(step2.model_B, level = .95) 
  RiskyViolence.tobit.results <-     # Save analysis results
    RiskyViolence.tobit.results %>%
    bind_rows(
      tibble(
        outcome = "RiskyViolence",
        trait1 = trait1,
        trait2 = trait2,
        estimate_A_tobit = step2.model_A@coefficients[5],
        step2_A_tobit.95L = step2.95_A[5, 1],
        step2_A_tobit.95U = step2.95_A[5, 2],
        change_A_tobit.r2 = 
          PseudoR2(step2.model_A, which = "McKelveyZavoina") -
          PseudoR2(step1.model_A, which = "McKelveyZavoina"),
        change_A_tobit.p = coef(summary(step2.model_A))[5, 4],
        estimate_B_tobit = step2.model_B@coefficients[5],
        step2_B_tobit.95L = step2.95_B[5, 1],
        step2_B_tobit.95U = step2.95_B[5, 2],
        change_B_tobit.r2 = 
          PseudoR2(step2.model_B, which = "McKelveyZavoina") -
          PseudoR2(step1.model_B, which = "McKelveyZavoina"),
        change_B_tobit.p = coef(summary(step2.model_B))[5, 4]))
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_tobit <- 
  CriminalBehavior.tobit.results %>%
  bind_rows(RiskyViolence.tobit.results) %>%  # join results from outcomes
  mutate(
    # Is Delta R-squared significant?
    A_tobit.significant = ifelse(
      round2(change_A_tobit.p, 3) < .05, 
      1, 
      0),
    B_tobit.significant = ifelse(
      round2(change_B_tobit.p, 3) < .05, 
      1, 
      0),
    # Is Delta R-squared meaningful?
    A_tobit.meaningful = ifelse(
      round2(change_A_tobit.r2, 2) >= .01, 
      1, 
      0),
    B_tobit.meaningful = ifelse(
      round2(change_B_tobit.r2, 2) >= .01, 
      1, 
      0),
    signs.match_tobit = ifelse( # Do signs of step 2 estimates match?
      sign(estimate_A_tobit) == sign(estimate_B_tobit), 
      1, 
      0)) %>%
  mutate(
    both.significant_tobit = ifelse(# Is Delta R-squared sig in both parts?
      A_tobit.significant == 1 &
        B_tobit.significant == 1 &
        signs.match_tobit == 1,
      1,
      0),
    both.meaningful_tobit = ifelse(# Is Delta R-sq meaningfulin both parts?
      A_tobit.meaningful == 1 &
        B_tobit.meaningful == 1 &
        signs.match_tobit == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_tobit = ifelse(
      ((A_tobit.significant == 1 &
          estimate_B_tobit >= step2_A_tobit.95L &
          estimate_B_tobit <= step2_A_tobit.95U) |
         (B_tobit.significant == 1 &
            estimate_A_tobit >= step2_B_tobit.95L &
            estimate_A_tobit <= step2_B_tobit.95U)) &
        both.significant_tobit == 0 &
        signs.match_tobit == 1, 
      1, 
      0)) %>%
  mutate(
    interpretation_tobit = ifelse(   # What is the final interpretation?
      both.significant_tobit == 1 & 
        (A_tobit.meaningful == 1 | 
           B_tobit.meaningful == 1),
      "Replication - Meaningful Effect",
      ifelse(
        both.significant_tobit == 1,
        "Replication - Negligible Effect",
        ifelse(
          equivocal_tobit == 1 & 
            (A_tobit.meaningful == 1 |
               B_tobit.meaningful == 1),
          "Equivocal Evidence of Replication (Meaningful Effect)",
          ifelse(
            equivocal_tobit == 1,
            "Equivocal Evidence of Replication (Negliglble Effect)",
            ifelse(
              A_tobit.significant == 1 |
                B_tobit.significant == 1,
              "Spurious Effect",
              "No Effect"))))))
write.csv(
  results_tobit %>%
    mutate(
      across(
        c("estimate_A_tobit", "step2_A_tobit.95L", "step2_A_tobit.95U",
          "change_A_tobit.r2", "estimate_B_tobit", "step2_B_tobit.95L",
          "step2_B_tobit.95U", "change_B_tobit.r2"),
        ~ round2(.x, 2))) %>%
    mutate(
      across(
        c("change_A_tobit.p", "change_B_tobit.p"),
        ~ round2(.x, 3))),
  "Data Analysis/Results/results_tobit.csv"
)
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
detach("package:VGAM")
print("tobit.R finished running!")
Sys.sleep(5)  