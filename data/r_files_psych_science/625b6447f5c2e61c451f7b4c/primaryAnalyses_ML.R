print('Running primaryAnalyses_ML.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(caret)
library(DescTools)
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
replications.S1 <-
  read_csv('Data Analysis/Results/replications.S1.csv')
for (i in unique(replications.S1$outcome)) {
  temp <- 
    replications.S1 %>%
    filter(outcome == i)
  assign(paste0('replications.S1.', i), temp)
}
replications.S1 <- 
  unique(replications.S1$outcome)
replications.S2 <-
  read_csv('Data Analysis/Results/replications.S2.csv')
for (i in unique(replications.S2$outcome)) {
  temp <- 
    replications.S2 %>%
    filter(outcome == i)
  assign(paste0('replications.S2.', i), temp)
}
replications.S2 <- 
  unique(replications.S2$outcome)
###############################
## Machine Learning Analyses ##
###############################
set.seed(30605)
S1_ML.results <- 
  tibble()
for (i in replications.S1) {
  # run ML analyses for each survey 1 outcome
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
    get(paste0('replications.S1.', i))
  for (j in 1:nrow(df2)) {
    trait1 <- 
      as.character(df2[j, 2])
    trait2 <-
      as.character(df2[j, 3])
    step1.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_A,
        method = "lm")
    step2.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ', 
                                 trait1, ':', trait2)),
        data = df_A, 
        method = "lm")
    step1.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_B,
        method = "lm")
    step2.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ',
                                 trait1, ':', trait2)),
        data = df_B,
        method = "lm")
    B_from_A.step1 <-     # Predict B outcome from A step 1
      predict(object = step1.model_A, newdata = df_B)
    B_from_A.step2 <-     # Predict B outcome from A step 2
      predict(object = step2.model_A, newdata = df_B)
    A_from_B.step1 <-     # Predict A outcome from B step 1
      predict(object = step1.model_B, newdata = df_A)
    A_from_B.step2 <-     # Predict A outcome from B step 2 
      predict(object = step2.model_B, newdata = df_A)
    S1_ML.results <-  # Save analysis results
      S1_ML.results %>%
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          B_from_A.step1_cor = cor(df_B[[i]], B_from_A.step1),
          B_from_A.step1_MAE = MAE(B_from_A.step1, df_B[[i]]),
          B_from_A.step1_RMSE = RMSE(B_from_A.step1, df_B[[i]]),
          B_from_A.step2_cor = cor(df_B[[i]], B_from_A.step2),
          B_from_A.step2_MAE = MAE(B_from_A.step2, df_B[[i]]),
          B_from_A.step2_RMSE = RMSE(B_from_A.step2, df_B[[i]]),
          B_from_A_q = FisherZ(B_from_A.step2_cor) -
            FisherZ(B_from_A.step1_cor),
          A_from_B.step1_cor = cor(df_A[[i]], A_from_B.step1),
          A_from_B.step1_MAE = MAE(A_from_B.step1, df_A[[i]]),
          A_from_B.step1_RMSE = RMSE(A_from_B.step1, df_A[[i]]),
          A_from_B.step2_cor = cor(df_A[[i]], A_from_B.step2),
          A_from_B.step2_MAE = MAE(A_from_B.step2, df_A[[i]]),
          A_from_B.step2_RMSE = RMSE(A_from_B.step2, df_A[[i]]),
          A_from_B_q = FisherZ(A_from_B.step2_cor) -
            FisherZ(A_from_B.step1_cor)))
  }
}
S2_ML.results <- 
  tibble()
for (i in replications.S2) {
  # run ML analyses for each survey 2 outcome
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
    get(paste0('replications.S2.', i))
  for (j in 1:nrow(df2)) {
    trait1 <-
      as.character(df2[j, 2])
    trait2 <- 
      as.character(df2[j, 3])
    step1.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_A,
        method = "lm")
    step2.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ', 
                                 trait1, ':', trait2)),
        data = df_A,
        method = "lm")
    step1.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_B,
        method = "lm")
    step2.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ',
                                 trait1, ':', trait2)),
        data = df_B,
        method = "lm")
    # Predict B outcome from A step 1
    B_from_A.step1 = predict(object = step1.model_A, newdata = df_B)
    # Predict B outcome from A step 2
    B_from_A.step2 = predict(object = step2.model_A, newdata = df_B)
    # Predict A outcome from B step 1
    A_from_B.step1 = predict(object = step1.model_B, newdata = df_A)
    # Predict A outcome from B step 2
    A_from_B.step2 = predict(object = step2.model_B, newdata = df_A)
    S2_ML.results <-  # Save analysis results
      S2_ML.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          B_from_A.step1_cor = cor(df_B[[i]], B_from_A.step1),
          B_from_A.step1_MAE = MAE(B_from_A.step1, df_B[[i]]),
          B_from_A.step1_RMSE = RMSE(B_from_A.step1, df_B[[i]]),
          B_from_A.step2_cor = cor(df_B[[i]], B_from_A.step2),
          B_from_A.step2_MAE = MAE(B_from_A.step2, df_B[[i]]),
          B_from_A.step2_RMSE = RMSE(B_from_A.step2, df_B[[i]]),
          B_from_A_q = FisherZ(B_from_A.step2_cor) -
            FisherZ(B_from_A.step1_cor),
          A_from_B.step1_cor = cor(df_A[[i]], A_from_B.step1),
          A_from_B.step1_MAE = MAE(A_from_B.step1, df_A[[i]]),
          A_from_B.step1_RMSE = RMSE(A_from_B.step1, df_A[[i]]),
          A_from_B.step2_cor = cor(df_A[[i]], A_from_B.step2),
          A_from_B.step2_MAE = MAE(A_from_B.step2, df_A[[i]]),
          A_from_B.step2_RMSE = RMSE(A_from_B.step2, df_A[[i]]),
          A_from_B_q = FisherZ(A_from_B.step2_cor) -
            FisherZ(A_from_B.step1_cor)))
  }
}
#########################
## Examine Replication ##
#########################
ML.results <- 
  S1_ML.results %>%
  bind_rows(S2_ML.results) %>%
  mutate(  # Was predictive accuracy substantially improved?
    replication = ifelse(
      A_from_B_q >= .10 & 
        B_from_A_q >= .10, 
      1,
      0),
    equivocal = ifelse(
      A_from_B_q >= .10 |
        B_from_A_q >= .10, 
      1,
      0)) %>%
  write.csv('Data Analysis/Results/ML.results.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('primaryAnalyses_ML.R finished running!')
Sys.sleep(5)  