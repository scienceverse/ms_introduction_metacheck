print('Running suppAnalyses_demo.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
##############################
## Define Rounding Function ##
##############################
round2 <-
  function(x, digits = 0) { # Function to always round 0.5 up
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
###############
## Load data ##
###############
replications_supp.S1 <-
  read_csv('Data Analysis/Results/replications_supp.S1.csv')
for (i in unique(replications_supp.S1$outcome)) {
  temp <- 
    replications_supp.S1 %>%
    filter(outcome == i)
  assign(paste0('replications_supp.S1.', i), temp)
}
replications_supp.S1 <- 
  unique(replications_supp.S1$outcome)
replications_supp.S2 <-
  read_csv('Data Analysis/Results/replications_supp.S2.csv')
for (i in unique(replications_supp.S2$outcome)) {
  temp <- 
    replications_supp.S2 %>%
    filter(outcome == i)
  assign(paste0('replications_supp.S2.', i), temp)
}
replications_supp.S2 <- 
  unique(replications_supp.S2$outcome)
#############################
## Hierarchical Regression ##
#############################
S1_supp_demo.results <-
  tibble()
for (i in replications_supp.S1) { # run analyses for S1 supp outcomes
  df_A <- read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df2 <-
    get(paste0('replications_supp.S1.', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <-
      as.character(df2[j, 3])
    step1.model_A <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
         data = df_A)
    step2.model_A <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite +
           get(trait1):get(trait2), data = df_A)
    step1.model_B <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
         data = df_B)
    step2.model_B <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite + 
           get(trait1):get(trait2), data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <-# CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S1_supp_demo.results <- # Save analysis results
      S1_supp_demo.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_supp_demo = tidy.step2_A$estimate[7],
          step2_A_supp_demo.95L = step2.95_A[7, 1],
          step2_A_supp_demo.95U = step2.95_A[7, 2],
          change_A_supp_demo.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A_supp_demo.p = tidy.step2_A$p.value[7],
          estimate_B_supp_demo = tidy.step2_B$estimate[7],
          step2_B_supp_demo.95L = step2.95_B[7, 1],
          step2_B_supp_demo.95U = step2.95_B[7, 2],
          change_B_supp_demo.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B_supp_demo.p = tidy.step2_B$p.value[7]))
  }
}
S2_supp_demo.results <-
  tibble()
for (i in replications_supp.S2) { # run analyses for S2 supp outcomes
  df_A <- read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df2 <-
    get(paste0('replications_supp.S2.', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <-
      as.character(df2[j, 3])
    step1.model_A <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
         data = df_A)
    step2.model_A <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite +
           get(trait1):get(trait2), data = df_A)
    step1.model_B <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
         data = df_B)
    step2.model_B <-
      lm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite + 
           get(trait1):get(trait2), data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <-# CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S2_supp_demo.results <- # Save analysis results
      S2_supp_demo.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_supp_demo = tidy.step2_A$estimate[7],
          step2_A_supp_demo.95L = step2.95_A[7, 1],
          step2_A_supp_demo.95U = step2.95_A[7, 2],
          change_A_supp_demo.r2 = summary(step2.model_A)$r.squared - 
            summary(step1.model_A)$r.squared,
          change_A_supp_demo.p = tidy.step2_A$p.value[7],
          estimate_B_supp_demo = tidy.step2_B$estimate[7],
          step2_B_supp_demo.95L = step2.95_B[7, 1],
          step2_B_supp_demo.95U = step2.95_B[7, 2],
          change_B_supp_demo.r2 = summary(step2.model_B)$r.squared - 
            summary(step1.model_B)$r.squared,
          change_B_supp_demo.p = tidy.step2_B$p.value[7]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_supp_demo <-
  S1_supp_demo.results %>%
  bind_rows(S2_supp_demo.results) %>% # Join results for S1 and S2
  mutate(
    # Is Delta R-squared significant?
    A_supp_demo.significant = ifelse(
      round2(change_A_supp_demo.p, 3) < .05, 
      1,
      0),
    B_supp_demo.significant = ifelse(
      round2(change_B_supp_demo.p, 3) < .05, 
      1,
      0),
    # Is Delta R-squared meaningful?
    A_supp_demo.meaningful = ifelse(
      round2(change_A_supp_demo.r2, 2) >= .01,
      1,
      0),
    B_supp_demo.meaningful = ifelse(
      round2(change_B_supp_demo.r2, 2) >= .01, 
      1,
      0),
    signs.match_supp_demo = ifelse( # Do signs of step 2 est match bt parts?
      sign(estimate_A_supp_demo) == sign(estimate_B_supp_demo), 
      1, 
      0)) %>%
  mutate(
    both.significant_supp_demo = ifelse(# Is Delta R-squared sig in both parts?
      A_supp_demo.significant == 1 &
        B_supp_demo.significant == 1 &
        signs.match_supp_demo == 1,
      1,
      0),
    both.meaningful_supp_demo = ifelse(# Is delt R-sq meaningful in both parts?
      A_supp_demo.meaningful == 1 &
        B_supp_demo.meaningful == 1 &
        signs.match_supp_demo == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_supp_demo = ifelse(
      ((A_supp_demo.significant == 1 &
        estimate_B_supp_demo >= step2_A_supp_demo.95L &
        estimate_B_supp_demo <= step2_A_supp_demo.95U) |
         (B_supp_demo.significant == 1 &
            estimate_A_supp_demo >= step2_B_supp_demo.95L &
            estimate_A_supp_demo <= step2_B_supp_demo.95U)) &
        both.significant_supp_demo == 0 &
        signs.match_supp_demo == 1,
      1,
      0)) %>%
  mutate(
    interpretation_supp_demo = ifelse(# What is the final interpretation?
      both.significant_supp_demo == 1 & 
        (A_supp_demo.meaningful == 1 |
           B_supp_demo.meaningful == 1),
      'Replication - Meaningful Effect',
      ifelse(
        both.significant_supp_demo == 1,
        'Replication - Negligible Effect',
        ifelse(
          equivocal_supp_demo == 1 & 
            (A_supp_demo.meaningful == 1 |
               B_supp_demo.meaningful == 1),
          'Equivocal Evidence of Replication (Meaningful Effect)',
          ifelse(
            equivocal_supp_demo == 1,
            'Equivocal Evidence of Replication (Negliglble Effect)',
            ifelse(
              A_supp_demo.significant == 1 |
                B_supp_demo.significant == 1,
              'Spurious Effect',
              'No Effect'))))))
write.csv(
  results_supp_demo %>%
    mutate(
      across(
        c('estimate_A_supp_demo', 'step2_A_supp_demo.95L',
          'step2_A_supp_demo.95U', 'change_A_supp_demo.r2',
          'estimate_B_supp_demo', 'step2_B_supp_demo.95L',
          'step2_B_supp_demo.95U', 'change_B_supp_demo.r2'),
      ~ round2(.x, 2))) %>%
    mutate(
      across(
        c('change_A_supp_demo.p', 'change_B_supp_demo.p'),
        ~ round2(.x, 3))),
  'Data Analysis/Results/results_supp_demo.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('suppAnalyses_demo.R finished running!')
Sys.sleep(5)  