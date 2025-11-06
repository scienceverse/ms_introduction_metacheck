print('Running ordinalCode.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(haven)
library(brms)
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
###############
## Load data ##
###############
replications_ordinal.S1 <-
  read_csv('Data Analysis/Results/replications.S1.csv') %>%
  filter(outcome == 'Humor')
for (i in unique(replications_ordinal.S1$outcome)) {
  temp <- 
    replications_ordinal.S1 %>%
    filter(outcome == i)
  assign(paste0('replications_ordinal.S1', i), temp)
}
replications_ordinal.S1 <- 
  unique(replications_ordinal.S1$outcome)
############################################
## Robustness Checks - Ordinal Regression ##
############################################
set.seed(30605)
S1_ordinal.results <- 
  tibble()
for (i in replications_ordinal.S1) {
  # run ordinal analyses
  df_A <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(i),
        ~ as.ordered(.x)))%>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <- 
    read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(i), 
        ~ as.ordered(.x))) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df2 <- 
    get(paste0('replications_ordinal.S1', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <- 
      as.character(df2[j, 3])
    step1.model_A <-
      brm(
        as.formula(
          paste0(i, ' ~ ', trait1, ' + ', trait2)),
        family = cumulative("probit"),
        data = df_A)
    step2.model_A <-
      brm(
        as.formula(
          paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ', trait1, ':', trait2)),
        family = cumulative("probit"),
        data = df_A)
    step1.model_B <-
      brm(
        as.formula(
          paste0(i, ' ~ ', trait1, ' + ', trait2)),
        family = cumulative("probit"),
        data = df_B)
    step2.model_B <-
      brm(
        as.formula(
          paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ', trait1, ':', trait2)),
        family = cumulative("probit"),
        data = df_B)
    S1_ordinal.results <-           # save analysis results
      S1_ordinal.results %>%
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          estimate_A_ordinal = summary(step2.model_A)$fixed$Estimate[7],
          step2_A_ordinal.95L =  summary(step2.model_A)$fixed$`l-95% CI`[7],
          step2_A_ordinal.95U = summary(step2.model_A)$fixed$`u-95% CI`[7],
          estimate_B_ordinal = summary(step2.model_B)$fixed$Estimate[7],
          step2_B_ordinal.95L =  summary(step2.model_B)$fixed$`l-95% CI`[7],
          step2_B_ordinal.95U = summary(step2.model_B)$fixed$`u-95% CI`[7]))
  }
}
#######################################
## Compare Effects Across Partitions ##
#######################################
results_ordinal <- 
  S1_ordinal.results %>%
  mutate(
    # Is Delta R-squared significant?
    A_ordinal.significant = ifelse(
      sign(step2_A_ordinal.95L) == sign(step2_A_ordinal.95U), 
      1, 
      0),
    B_ordinal.significant = ifelse(
      sign(step2_B_ordinal.95L) == sign(step2_B_ordinal.95U),
      1, 
      0),
    signs.match_ordinal = ifelse( # Do signs of step 2 estimates match?
      sign(estimate_A_ordinal) == sign(estimate_B_ordinal), 
      1, 
      0)) %>%
  mutate(
    both.significant_ordinal = ifelse(# Is Delta R-squared sig in both parts?
      A_ordinal.significant == 1 &
        B_ordinal.significant == 1 &
        signs.match_ordinal == 1,
      1,
      0),
    # Is Delta R-squared significant in either A or B (but not both) and does
    # the CI of the significant step 2 estimate contain the non-significant
    # step 2 estimate?
    equivocal_ordinal = ifelse(
      ((A_ordinal.significant == 1 &
          estimate_B_ordinal >= step2_A_ordinal.95L &
          estimate_B_ordinal <= step2_A_ordinal.95U) |
         (B_ordinal.significant == 1 &
            estimate_A_ordinal >= step2_B_ordinal.95L &
            estimate_A_ordinal <= step2_B_ordinal.95U)) &
        both.significant_ordinal == 0 &
        signs.match_ordinal == 1,
      1,
      0)) %>%
  mutate(
    interpretation_ordinal = ifelse(   # What is the final interpretation?
      both.significant_ordinal == 1,
      'Replication',
      ifelse(
        equivocal_ordinal == 1,
        'Equivocal Evidence of Replication',
        ifelse(
          A_ordinal.significant == 1 |
            B_ordinal.significant == 1,
          'Spurious Effect',
          'No Effect'))))
write.csv(
  results_ordinal %>%
    mutate(
      across(
        c('estimate_A_ordinal', 'step2_A_ordinal.95L', 'step2_A_ordinal.95U',
          'estimate_B_ordinal', 'step2_B_ordinal.95L', 'step2_B_ordinal.95U'),
        ~ round2(.x, 2))),
  'Data Analysis/Results/results_ordinal.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('ordinalCode.R finished running!')
Sys.sleep(5)  