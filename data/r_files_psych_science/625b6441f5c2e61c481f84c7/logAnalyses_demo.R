print('Running logAnalyses_demo.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
library(DescTools)
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
###############
## Load data ##
###############
replications_log.S1 <- 
  read_csv('Data Analysis/Results/replications_log.S1.csv')
for (i in unique(replications_log.S1$outcome)) {
  temp <- 
    replications_log.S1 %>%
    filter(outcome == i)
  assign(paste0('replications_log.S1.', i), temp)
}
replications_log.S1 <- 
  unique(replications_log.S1$outcome)
replications_log.S2 <- 
  read_csv('Data Analysis/Results/replications_log.S2.csv')
for (i in unique(replications_log.S2$outcome)) {
  temp <- 
    replications_log.S2 %>%
    filter(outcome == i)
  assign(paste0('replications_log.S2.', i), temp)
}
replications_log.S2 <- 
  unique(replications_log.S2$outcome)
#############################
## Hierarchical Regression ##
#############################
S1_log_demo.results <- 
  tibble()
for (i in replications_log.S1) {   # run analyses for S1 binary outcomes
  df_A <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df2 <- 
    get(paste0('replications_log.S1.', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <- 
      as.character(df2[j, 3])
    step1.model_A <-     
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
          family = binomial(link = "logit"), data = df_A)
    step2.model_A <-  
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite +
            get(trait1):get(trait2), family = binomial(link = "logit"), 
          data = df_A)
    step1.model_B <-    
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite, 
          family = binomial(link = "logit"), data = df_B)
    step2.model_B <-    
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite + 
            get(trait1):get(trait2), family = binomial(link = "logit"),
          data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S1_log_demo.results <- # Save analysis results
      S1_log_demo.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_log_demo = tidy.step2_A$estimate[7],
          step2_A_log_demo.95L = step2.95_A[7, 1],
          step2_A_log_demo.95U = step2.95_A[7, 2],
          change_A_log_demo.r2 = 
            PseudoR2(step2.model_A, which = 'AldrichNelson') -
            PseudoR2(step1.model_A, which = 'AldrichNelson'),
          change_A_log_demo.p = tidy.step2_A$p.value[7],
          estimate_B_log_demo = tidy.step2_B$estimate[7],
          step2_B_log_demo.95L = step2.95_B[7, 1],
          step2_B_log_demo.95U = step2.95_B[7, 2],
          change_B_log_demo.r2 = 
            PseudoR2(step2.model_B, which = 'AldrichNelson') -
            PseudoR2(step1.model_B, which = 'AldrichNelson'),
          change_B_log_demo.p = tidy.step2_B$p.value[7]))
  }
}
S2_log_demo.results <- 
  tibble()
for (i in replications_log.S2) {   # run analyses for S2 binary outcomes
  df_A <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df2 <- 
    get(paste0('replications_log.S2.', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <- 
      as.character(df2[j, 3])
    step1.model_A <-     
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite,
          family = binomial(link = "logit"), data = df_A)
    step2.model_A <-  
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite +
            get(trait1):get(trait2), family = binomial(link = "logit"), 
          data = df_A)
    step1.model_B <-    
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite, 
          family = binomial(link = "logit"), data = df_B)
    step2.model_B <-    
      glm(get(i) ~ get(trait1) + get(trait2) + Sex + Age_1 + NonHispanicWhite + 
            get(trait1):get(trait2), family = binomial(link = "logit"),
          data = df_B)
    tidy.step2_A <- # tidy step 2 (A)
      tidy(step2.model_A) 
    tidy.step2_B <- # tidy step 2 (B)
      tidy(step2.model_B) 
    step2.95_A <- # CI for step 2 est. (A)
      confint(step2.model_A, level = .95) 
    step2.95_B <- # CI for step 2 est. (B)
      confint(step2.model_B, level = .95) 
    S2_log_demo.results <- # Save analysis results
      S2_log_demo.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_log_demo = tidy.step2_A$estimate[7],
          step2_A_log_demo.95L = step2.95_A[7, 1],
          step2_A_log_demo.95U = step2.95_A[7, 2],
          change_A_log_demo.r2 = 
            PseudoR2(step2.model_A, which = 'AldrichNelson') -
            PseudoR2(step1.model_A, which = 'AldrichNelson'),
          change_A_log_demo.p = tidy.step2_A$p.value[7],
          estimate_B_log_demo = tidy.step2_B$estimate[7],
          step2_B_log_demo.95L = step2.95_B[7, 1],
          step2_B_log_demo.95U = step2.95_B[7, 2],
          change_B_log_demo.r2 = 
            PseudoR2(step2.model_B, which = 'AldrichNelson') -
            PseudoR2(step1.model_B, which = 'AldrichNelson'),
          change_B_log_demo.p = tidy.step2_B$p.value[7]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_log_demo <- 
  S1_log_demo.results %>%
  bind_rows(S2_log_demo.results) %>% # Join results for S1 and S2
  mutate(
    # Is Delta R-squared significant?
    A_log_demo.significant = ifelse(
      round(change_A_log_demo.p, 3) < .05, 
      1, 
      0),
    B_log_demo.significant = ifelse(
      round(change_B_log_demo.p, 3) < .05, 
      1, 
      0),
    # Is Delta R-squared meaningful?
    A_log_demo.meaningful = ifelse(
      round(change_A_log_demo.r2, 2) >= .01, 
      1,
      0),
    B_log_demo.meaningful = ifelse(
      round(change_B_log_demo.r2, 2) >= .01, 
      1,
      0),
    signs.match_log_demo = ifelse(# Do signs of step 2 est match between parts?
      sign(estimate_A_log_demo) == sign(estimate_B_log_demo), 
      1, 
      0)) %>%
  mutate(
    both.significant_log_demo = ifelse(# Is Delta R-sq sig in both partitions?
      A_log_demo.significant == 1 &
        B_log_demo.significant == 1 &
        signs.match_log_demo == 1,
      1,
      0),
    both.meaningful_log_demo = ifelse(# Is Delta R-sq meaingful in both parts?
      A_log_demo.meaningful == 1 &
        B_log_demo.meaningful == 1 &
        signs.match_log_demo == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_log_demo = ifelse(
      ((A_log_demo.significant == 1 &
          estimate_B_log_demo >= step2_A_log_demo.95L &
          estimate_B_log_demo <= step2_A_log_demo.95U) |
         (B_log_demo.significant == 1 &
            estimate_A_log_demo >= step2_B_log_demo.95L &
            estimate_A_log_demo <= step2_B_log_demo.95U)) &
        both.significant_log_demo == 0 &
        signs.match_log_demo == 1,
      1,
      0)) %>%
  mutate(
    interpretation_log_demo = ifelse(# What is the final interpretation?
      both.significant_log_demo == 1 & 
        (A_log_demo.meaningful == 1 | 
           B_log_demo.meaningful == 1),
      'Replication - Meaningful Effect',
      ifelse(
        both.significant_log_demo == 1,
        'Replication - Negligible Effect',
        ifelse(
          equivocal_log_demo == 1 & 
            (A_log_demo.meaningful == 1 |
               B_log_demo.meaningful == 1),
          'Equivocal Evidence of Replication (Meaningful Effect)',
          ifelse(
            equivocal_log_demo == 1,
            'Equivocal Evidence of Replication (Negliglble Effect)',
            ifelse(
              A_log_demo.significant == 1 |
                B_log_demo.significant == 1,
              'Spurious Effect',
              'No Effect'))))))
write.csv(
  results_log_demo %>%
    mutate(
      across(
        c('estimate_A_log_demo', 'step2_A_log_demo.95L', 
          'step2_A_log_demo.95U', 'change_A_log_demo.r2', 
          'estimate_B_log_demo', 'step2_B_log_demo.95L', 
          'step2_B_log_demo.95U', 'change_B_log_demo.r2'),
        ~ round(.x, 2))) %>%
    mutate(
      across(
        c('change_A_log_demo.p', 'change_B_log_demo.p'),
        ~ round(.x, 3))),
  'Data Analysis/Results/results_log_demo.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('logAnalyses_demo.R finished running!')
Sys.sleep(5)  