print('Running table1.R ...')
#############################
## Load required libraries ##
#############################
library(tidyverse)
library(broom)
###############
## Load data ##
###############
results <- 
  read_csv('Data Analysis/Results/replications.csv') %>%
  filter(interpretation == 'Replication - Meaningful Effect')
results_log <-
  read_csv('Data Analysis/Results/replications_log.csv') %>%
  filter(interpretation_log == 'Replication - Meaningful Effect')
names(results_log) <-
  names(results)
#############
## Table 3 ##
#############
table3 <- 
  results %>%
  bind_rows(results_log) %>%
  mutate(
    trait = paste0(
      substr(str_replace(trait1, 'Personality', ''), 1, 1), 
      substr(str_replace(trait2, 'Personality', ''), 1, 1)),
    intercept_A = paste0(
      as.character(sprintf('%.2f', intercept_A)), ' (',
      as.character(sprintf('%.2f', intercept_A.95L)), ', ',
      as.character(sprintf('%.2f', intercept_A.95U)), ')'),
    intercept_B = paste0(
      as.character(sprintf('%.2f', intercept_B)), ' (',
      as.character(sprintf('%.2f', intercept_B.95L)), ', ',
      as.character(sprintf('%.2f', intercept_B.95U)), ')'),
    trait1_A = paste0(
      as.character(sprintf('%.2f', trait1_A)), ' (', 
      as.character(sprintf('%.2f', trait1_A.95L)), ', ',
      as.character(sprintf('%.2f', trait1_A.95U)), ')'),
    trait1_B = paste0(
      as.character(sprintf('%.2f', trait1_B)), ' (',
      as.character(sprintf('%.2f', trait1_B.95L)), ', ', 
      as.character(sprintf('%.2f', trait1_B.95U)), ')'),
    trait2_A = paste0(
      as.character(sprintf('%.2f', trait2_A)), ' (', 
      as.character(sprintf('%.2f', trait2_A.95L)), ', ', 
      as.character(sprintf('%.2f', trait2_A.95U)), ')'),
    trait2_B = paste0(
      as.character(sprintf('%.2f', trait2_B)), ' (', 
      as.character(sprintf('%.2f', trait2_B.95L)), ', ',
      as.character(sprintf('%.2f', trait2_B.95U)), ')'),
    interaction_A = paste0(
      as.character(sprintf('%.2f', interaction_A)), ' (', 
      as.character(sprintf('%.2f', interaction_A.95L)), ', ', 
      as.character(sprintf('%.2f', interaction_A.95U)), ')'),
    interaction_B = paste0(
      as.character(sprintf('%.2f', interaction_B)), ' (',
      as.character(sprintf('%.2f', interaction_B.95L)), ', ',
      as.character(sprintf('%.2f', interaction_B.95U)), ')'),
    r2_A = ifelse(
      change_A.p < .001,
      paste0(as.character(sprintf('%.2f', change_A.r2)), '***'),
      ifelse(
        change_A.p < .01,
        paste0(as.character(sprintf('%.2f', change_A.r2)), '**'),
        ifelse(
          change_A.p < .05,
          paste0(as.character(sprintf('%.2f', change_A.r2)), '*'),
          as.character(sprintf('%.2f', change_A.r2))))),
    r2_B = ifelse(
      change_B.p < .001,
      paste0(as.character(sprintf('%.2f', change_B.r2)), '***'),
      ifelse(
        change_B.p < .01,
        paste0(as.character(sprintf('%.2f', change_B.r2)), '**'),
        ifelse(
          change_B.p < .05,
          paste0(as.character(sprintf('%.2f', change_B.r2)), '*'), 
          as.character(sprintf('%.2f', change_B.r2)))))) %>%
  select(
    c(trait, outcome, r2_A, intercept_A, trait1_A, trait2_A, interaction_A,
      r2_B, intercept_B, trait1_B, trait2_B, interaction_B)) %>%
  arrange(trait) %>%
  write_csv('Data Analysis/Results/table3.csv')
##########################
## Clean-Up Environment ##
##########################
rm(list = ls())
print('table1.R finished running!')
Sys.sleep(5)  