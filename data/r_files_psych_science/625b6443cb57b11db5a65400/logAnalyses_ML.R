print('Running logAnalyses_ML.R ...')
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
###############################
## Machine Learning Analyses ##
###############################
set.seed(30605)
S1_ML_log.results <- 
  tibble()
for (i in replications_log.S1) { # run ML analyses for each S1 binary outcome
  df_A <-
    read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(i),
        ~ as_factor(.x))) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <-
    read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(i), 
        ~ as_factor(.x))) %>%
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
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_A,
        method = "glm",
        family = 'binomial')
    step2.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ',
                                 trait1, ':', trait2)),
        data = df_A,
        method = "glm",
        family = 'binomial')
    step1.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_B,
        method = "glm",
        family = 'binomial')
    step2.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ',
                                 trait1, ':', trait2)),
        data = df_B,
        method = "glm",
        family = 'binomial')
    # Predict B outcome from A step 1
    B_from_A.step1 <-
      stats::predict(object = step1.model_A, newdata = df_B)
    B_from_A.step1 <-
      as.numeric(levels(B_from_A.step1))[B_from_A.step1]
    # Predict B outcome from A step 2
    B_from_A.step2 <-
      stats::predict(object = step2.model_A, newdata = df_B)
    B_from_A.step2 <-
      as.numeric(levels(B_from_A.step2))[B_from_A.step2]
    # Predict A outcome from B step 1
    A_from_B.step1 <-
      stats::predict(object = step1.model_B, newdata = df_A)
    A_from_B.step1 <-
      as.numeric(levels(A_from_B.step1))[A_from_B.step1]
    # Predict A outcome from B step 2
    A_from_B.step2 <-
      stats::predict(object = step2.model_B, newdata = df_A)
    A_from_B.step2 <-
      as.numeric(levels(A_from_B.step2))[A_from_B.step2]
    A <- 
      as.numeric(levels(df_A[[i]])[df_A[[i]]])
    B <- 
      as.numeric(levels(df_B[[i]])[df_B[[i]]])
    S1_ML_log.results <- # Save analysis results
      S1_ML_log.results %>% 
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          B_from_A_log.step1_cor = cor(B, B_from_A.step1),
          B_from_A_log.step1_MAE = MAE(B_from_A.step1, B),
          B_from_A_log.step1_RMSE = RMSE(B_from_A.step1, B),
          B_from_A_log.step2_cor = cor(B, B_from_A.step2),
          B_from_A_log.step2_MAE = MAE(B_from_A.step2, B),
          B_from_A_log.step2_RMSE = RMSE(B_from_A.step2, B),
          B_from_A_log_q = FisherZ(B_from_A_log.step2_cor) -
            FisherZ(B_from_A_log.step1_cor),
          A_from_B_log.step1_cor = cor(A, A_from_B.step1),
          A_from_B_log.step1_MAE = MAE(A_from_B.step1, A),
          A_from_B_log.step1_RMSE = RMSE(A_from_B.step1, A),
          A_from_B_log.step2_cor = cor(A, A_from_B.step2),
          A_from_B_log.step2_MAE = MAE(A_from_B.step2, A),
          A_from_B_log.step2_RMSE = RMSE(A_from_B.step2, A),
          A_from_B_log_q = FisherZ(A_from_B_log.step2_cor) -
            FisherZ(A_from_B_log.step1_cor)))
  }
}
S2_ML_log.results <- 
  tibble()
for (i in replications_log.S2) {# run ML analyses for each S2 binary outcome
  df_A <-
    read_csv(paste0('Data Analysis/Splits/', i, '_A.csv')) %>%
    mutate(
      across(
        all_of(i),
        ~ as_factor(.x))) %>%
    mutate(
      across(
        all_of(traits), 
        ~ scale(.x, scale = F)))
  df_B <-
    read_csv(paste0('Data Analysis/Splits/', i, '_B.csv')) %>%
    mutate(
      across(
        all_of(i), 
        ~ as_factor(.x))) %>%
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
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_A,
        method = "glm",
        family = 'binomial')
    step2.model_A <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ',
                                 trait1, ':', trait2)),
        data = df_A,
        method = "glm",
        family = 'binomial')
    step1.model_B <-
      caret::train(form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2)),
        data = df_B,
        method = "glm",
        family = 'binomial')
    step2.model_B <-
      caret::train(
        form = as.formula(paste0(i, ' ~ ', trait1, ' + ', trait2, ' + ', 
                                 trait1, ':', trait2)),
        data = df_B,
        method = "glm",
        family = 'binomial')
    # predict B from A step 1
    B_from_A.step1 <-
      stats::predict(object = step1.model_A, newdata = df_B)
    B_from_A.step1 <-
      as.numeric(levels(B_from_A.step1))[B_from_A.step1]
    # predict B from A step 2
    B_from_A.step2 <-
      stats::predict(object = step2.model_A, newdata = df_B)
    B_from_A.step2 <-
      as.numeric(levels(B_from_A.step2))[B_from_A.step2]
    # predict A from B step 1
    A_from_B.step1 <-
      stats::predict(object = step1.model_B, newdata = df_A)
    A_from_B.step1 <-
      as.numeric(levels(A_from_B.step1))[A_from_B.step1]
    # predict A from B step 2
    A_from_B.step2 <-
      stats::predict(object = step2.model_B, newdata = df_A)
    A_from_B.step2 <-
      as.numeric(levels(A_from_B.step2))[A_from_B.step2]
    A <-
      as.numeric(levels(df_A[[i]])[df_A[[i]]])
    B <- 
      as.numeric(levels(df_B[[i]])[df_B[[i]]])
    S2_ML_log.results <-  # save analysis results
      S2_ML_log.results %>%
      bind_rows(
        tibble(
          outcome = i,
          trait1 = trait1,
          trait2 = trait2,
          B_from_A_log.step1_cor = cor(B, B_from_A.step1),
          B_from_A_log.step1_MAE = MAE(B_from_A.step1, B),
          B_from_A_log.step1_RMSE = RMSE(B_from_A.step1, B),
          B_from_A_log.step2_cor = cor(B, B_from_A.step2),
          B_from_A_log.step2_MAE = MAE(B_from_A.step2, B),
          B_from_A_log.step2_RMSE = RMSE(B_from_A.step2, B),
          B_from_A_log_q = FisherZ(B_from_A_log.step2_cor) -
            FisherZ(B_from_A_log.step1_cor),
          A_from_B_log.step1_cor = cor(A, A_from_B.step1),
          A_from_B_log.step1_MAE = MAE(A_from_B.step1, A),
          A_from_B_log.step1_RMSE = RMSE(A_from_B.step1, A),
          A_from_B_log.step2_cor = cor(A, A_from_B.step2),
          A_from_B_log.step2_MAE = MAE(A_from_B.step2, A),
          A_from_B_log.step2_RMSE = RMSE(A_from_B.step2, A),
          A_from_B_log_q = FisherZ(A_from_B_log.step2_cor) -
            FisherZ(A_from_B_log.step1_cor)))
  }
}
#########################
## Examine Replication ##
#########################
ML_log.results <- 
  S1_ML_log.results %>%
  bind_rows(S2_ML_log.results) %>%
  mutate(   # Was predictive accuracy substantially improved?
    replication = ifelse(
      A_from_B_log_q >= .10 & 
        B_from_A_log_q >= .10,
      1, 
      0),
    equivocal = ifelse(
      A_from_B_log_q >= .10 |
        B_from_A_log_q >= .10,
      1,
      0)) %>%
  write.csv('Data Analysis/Results/ML_log.results.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('logAnalyses_ML.R finished running!')
Sys.sleep(5)  