# oct 2024 update aaron psycho dyads
# joseph.bulbulia@vuw.ac.nz

# data path (local - jb only)
# set path to your directory
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy"
  )
# reproducibility
set.seed(123)


# test functions ----------------------------------------------------------

# Example usage:
# detach("package:margot", unload = TRUE)
# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
# devtools::install_github("go-bayes/boilerplate")
library(margot)
#library(boilerplate)

packageVersion(pkg = "margot")
packageVersion(pkg = "boilerplate")

# load necessary libraries
pacman::p_load(
  clarify, # sensitivity analysis for causal inference
  cobalt, # covariate balance tables and plots
  DiagrammeR, # graph and network visualization
  doParallel, # parallel processing with foreach
  fastDummies, # fast creation of dummy variables
  fs, # cross-platform file system operations
  ggbeeswarm, # data visualisation
  ggplot2, # data visualisation
  glmnet, # lasso and elastic-net regularized models
  glue, # text
  grf, # generalized random forests
  gt, # html tables for data frames
  gtsummary, # summary tables for regression models
  here, # simple and robust file referencing
  janitor, # data cleaning and validation
  kableExtra, # advanced table formatting
  lmtp, # longitudinal targeted maximum likelihood estimation
  margot, # functions for casual inference
  MatchIt, # matching methods for causal inference
  MatchThem, # matching methods for multiply imputed datasets
  naniar, # handling and visualization of missing data
  parameters, # parameters and performance metrics
  policytree, # causal inference with policy trees
  progressr, # progress reporting for R
  ranger, # fast implementation of random forests
  skimr, # summary statistics for data frames
  SuperLearner, # ensemble learning
  tidyverse, # collection of R packages for data science
  WeightIt, # weighting methods for covariate balancing
  xgboost, # extreme gradient boosting
  EValue, # compute Evalues
  data.table, # fast data wrangling
  maq, # qini curves
  purrr, # data wrangling
  patchwork, # multiple plots
  labelled,
  purrr,
  boilerplate
)


# read data
dat_long_prepare <- margot::here_read_qs("dat_long_prepare", push_mods)


#view
table(dat_long_prepare$wave)

# read in variable names from 01 script
name_exposure <- margot::here_read("name_exposure")
t0_name_exposure <- here_read('t0_name_exposure')
t1_name_exposure <- here_read('t1_name_exposure')
baseline_vars <- margot::here_read("baseline_vars_binary")
exposure_var_all <- margot::here_read("exposure_var_all")
exposure_var = "aaron_psychopathy_combined"
outcome_vars <- margot::here_read("outcome_vars")
outcome_vars <- sort(outcome_vars)
baseline_wave <- margot::here_read("baseline_wave")
exposure_waves <- margot::here_read("exposure_waves")
outcome_wave <- margot::here_read("outcome_wave")
extra_vars <- margot::here_read("extra_vars")
all_vars <- margot::here_read("all_vars")
continuous_columns_keep <- margot::here_read("continuous_columns_keep")
ordinal_columns <- margot::here_read("ordinal_columns")

# if we were to use time varying confounding control
confounder_vars = c("parent_binary", "hlth_disability_binary")
confounder_vars = sort(confounder_vars)
here_save(confounder_vars, "confounder_vars")

# check names
colnames(dat_long_prepare) |>   sort(decreasing = TRUE)

# m
length(unique(dat_long_prepare$id))

# we don't need 'not lost'
dat_long_prepare_sans <- dat_long_prepare |> select(-alert_level_combined_lead)

t0_alert_level_combined_lead <- dat_long_prepare |> filter(wave == 2018) |> select(alert_level_combined_lead)
t0_alert_level_combined_lead
baseline_vars
baseline_vars_use <- setdiff(baseline_vars, c("alert_level_combined_lead"))
sort(baseline_vars_use)
baseline_vars_use
confounder_vars
outcome_vars
exposure_var_all

vis_miss(dat_long_prepare_sans, warn_large_data = F)
extra_vars <- setdiff(extra_vars, "not_lost")
# needed for imputation
dat_long_prepare_sans<- margot::remove_numeric_attributes(dat_long_prepare_sans)

## impute missing baseline data
df_wide_impute <- margot_wide_machine(
  dat_long_prepare_sans,
  id = "id",
  wave = "wave",
  baseline_vars = baseline_vars_use,
  exposure_var = exposure_var_all,
  outcome_vars = outcome_vars,  # U
  imputation_method = 'mice',  # or 'median' or 'none'
  include_exposure_var_baseline = TRUE,
  include_outcome_vars_baseline = TRUE
)
#colnames check
colnames(df_wide_impute)

# table
table(is.na(df_wide_impute$t0_aaron_antagonism_na))

# get covid back in
df_wide_impute$t0_alert_level_combined_lead <- t0_alert_level_combined_lead


# add baseline sample and gender weights (thinking about it, we could do this later...)
# # save the wide data
#cehck
df_wide_impute |> select(ends_with("_na")) |> colnames()
margot::here_save_qs(df_wide_impute, 'df_wide_impute', push_mods)

# read if needed
df_wide_impute <- margot::here_read_qs('df_wide_impute', push_mods)

length(unique(df_wide_impute$id))

miss_var_summary <- naniar::miss_var_summary(df_wide_impute)
print(miss_var_summary, n = 33)
naniar::miss_case_table(df_wide_impute)
colnames(df_wide_impute)
naniar::vis_miss(df_wide_impute, warn_large_data = FALSE)

head(df_wide_impute)



# check number of individuals per t0_rel_num
library(dplyr)
library(testthat)

# 1. check that each t0_rel_num group has exactly two individuals
group_counts <- df_wide_impute %>%
  group_by(t0_rel_num) %>%
  summarise(count = n()) %>%
  ungroup()

non_dyadic_groups <- group_counts %>%
  filter(count != 2)

if(nrow(non_dyadic_groups) > 0) {
  print("The following t0_rel_num groups do not have exactly two individuals:")
  print(non_dyadic_groups)
  # opitonal, stop execution or handle these groups accordingly
  # stop("Non-dyadic groups found. Please address them before proceeding.")
}

# create a dyadic dataframe by excluding non-dyadic groups
df_dyads <- df_wide_impute %>%
  group_by(t0_rel_num) %>%
  filter(n() == 2) %>%
  ungroup()

# 3. partner mapping
partner_mapping <- df_dyads %>%
  select(id, t0_rel_num) %>%
  arrange(t0_rel_num, id) %>%
  group_by(t0_rel_num) %>%
  mutate(partner_id = if_else(row_number() == 1, lead(id), lag(id))) %>%
  ungroup() %>%
  select(id, partner_id)

# 4. sanity checks on partner mapping
# a. self-partnering check
self_partners <- partner_mapping %>%
  filter(id == partner_id)

if(nrow(self_partners) > 0) {
  print("The following individuals are incorrectly mapped to themselves:")
  print(self_partners)
} else {
  print("No individuals are mapped to themselves.")
}

# b. bidirectional map check
bidirectional_checks <- partner_mapping %>%
  inner_join(partner_mapping, by = c("partner_id" = "id")) %>%
  filter(id != partner_id.y)

if(nrow(bidirectional_checks) > 0) {
  print("Some partner mappings are not bidirectional:")
  print(bidirectional_checks)
} else {
  print("All partner mappings are bidirectional.")
}

# 5. join partner data
prep_coop_all_with_partners <- df_dyads %>%
  left_join(partner_mapping, by = "id") %>%
  left_join(df_dyads, by = c("partner_id" = "id"), suffix = c("", "_partner")) %>%
  arrange(t0_rel_num, id) %>%
  select(-t0_rel_num_partner)  # Remove redundant t0_rel_num from partner data

# validate partner present
missing_partners <- prep_coop_all_with_partners %>%
  filter(!partner_id %in% df_dyads$id) %>%
  select(id, partner_id)

if(nrow(missing_partners) > 0) {
  print("The following partner_ids are missing in df_dyads:")
  print(missing_partners)
} else {
  print("All partner_ids are present in df_dyads.")
}

#  test partner assignment
test_that("Partner columns are correctly assigned", {
  # Define the columns to test
  columns_to_test <- c("t0_age", "t0_male_binary", "t0_education_level_coarsen")
  partner_columns_to_test <- paste0(columns_to_test, "_partner")
  
  # Iterate over each column to test
  for (i in seq_along(columns_to_test)) {
    col <- columns_to_test[i]
    partner_col <- partner_columns_to_test[i]
    
    # Fetch the partner's value from the original dyadic dataframe
    prep_coop_all_with_partners <- prep_coop_all_with_partners %>%
      mutate(
        original_partner_value = df_dyads[[col]][match(partner_id, df_dyads$id)]
      )
    
    # Compare the original_partner_value with the partner_col
    expect_equal(
      prep_coop_all_with_partners$original_partner_value,
      prep_coop_all_with_partners[[partner_col]],
      info = paste("Mismatch in", col)
    )
  }
})

#NA values in partner cols
na_counts <- sapply(prep_coop_all_with_partners, function(x) sum(is.na(x)))
na_counts_partner_columns <- na_counts[grep("_partner$", names(na_counts))]

print("NA counts in partner columns:")
print(na_counts_partner_columns)

# view
head(prep_coop_all_with_partners[ , c("id", "t0_rel_num", "t2_self_esteem", "t2_self_esteem_partner")])

# view
print( prep_coop_all_with_partners[ , c("id", "t0_rel_num", "t2_self_esteem",  "t2_self_esteem_partner")], n = 100)

# view
head(prep_coop_all_with_partners[ , c("t2_conflict_in_relationship", "t2_conflict_in_relationship_partner")])


# additional checks
# a few random dyads for manual inspection
# inspect a few more dyads to confirm the correctness of partner assignments.
sample_rel_nums <- sample(unique(prep_coop_all_with_partners$t0_rel_num), 5)

prep_coop_all_with_partners %>%
  filter(t0_rel_num %in% sample_rel_nums) %>%
  select(id, t0_rel_num, t2_self_esteem, t2_self_esteem_partner)

# ensure partner mapping is bidirectional across entire dataset.
# create a dataframe that pairs each id with their partner's id
partner_pairs <- prep_coop_all_with_partners %>%
  select(id, partner_id)

# Check for reciprocal relationships
reciprocal_pairs <- partner_pairs %>%
  inner_join(partner_pairs, by = c("partner_id" = "id")) %>%
  filter(id != partner_id.y)

# Identify any non-reciprocal mappings
non_reciprocal <- reciprocal_pairs %>%
  filter(partner_id.y != id)

if(nrow(non_reciprocal) == 0) {
  print("All partner mappings are bidirectional.")
} else {
  print("Some partner mappings are not bidirectional:")
  print(non_reciprocal)
}

# ensure that no individual is incorrectly mapped as their own partner.
self_partners <- prep_coop_all_with_partners %>%
  filter(id == partner_id)

if(nrow(self_partners) == 0) {
  print("No individuals are mapped to themselves.")
} else {
  print("The following individuals are incorrectly mapped to themselves:")
  print(self_partners$id)
}

 # compare summary statistics of t2_self_esteem and t2_self_esteem_partner to ensure they are consistent
summary_stats <- prep_coop_all_with_partners %>%
  summarise(
    mean_self_esteem = mean(t2_self_esteem, na.rm = TRUE),
    mean_partner_self_esteem = mean(t2_self_esteem_partner, na.rm = TRUE),
    median_self_esteem = median(t2_self_esteem, na.rm = TRUE),
    median_partner_self_esteem = median(t2_self_esteem_partner, na.rm = TRUE)
  )

print(summary_stats)

# visualise
library(ggplot2)

# Scatter plot of t2_self_esteem vs t2_self_esteem_partner
ggplot(prep_coop_all_with_partners, aes(x = t2_self_esteem, y = t2_self_esteem_partner)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Self Esteem vs Partner's Self Esteem",
       x = "Self Esteem",
       y = "Partner's Self Esteem") +
  theme_minimal()


# check nas
rows_with_na <- prep_coop_all_with_partners %>%
  filter(is.na(t2_self_esteem) | is.na(t2_self_esteem_partner))

print(rows_with_na)

# rename
prep_coop_all_with_partners_ordered <- prep_coop_all_with_partners %>%
  rename_with(
    .fn = function(names) {
      # Identify columns that end with "_partner"
      partner_cols <- str_detect(names, "_partner$")

      # For each partner column, rearrange its parts
      names[partner_cols] <- str_replace(names[partner_cols],
                                         pattern = "^(t[0-9]+)_(.*)_(partner)$",
                                         replacement = "\\1_\\3_\\2")

      names
    },
    .cols = everything()  # apply this function to all columns
  )
# 


# order -------------------------------------------------------------------
continuous_columns_keep_exposures <- c(continuous_columns_keep, t0_name_exposure, t1_name_exposure)
continuous_columns_keep_exposures


colnames(prep_coop_all_with_partners_ordered)
# check
naniar::miss_var_summary(prep_coop_all_with_partners_ordered)
naniar::miss_case_table(prep_coop_all_with_partners_ordered)
naniar::gg_miss_var(prep_coop_all_with_partners_ordered)


# weights -----------------------------------------------------------------
# optional remove confounders in final wave to ensure we do not encounter mediator bias
# order columns
df_wide_encoded_ordered <- prep_coop_all_with_partners_ordered %>%
  select(
    id,
    matches("^t0_"),
    matches("^t1_"),
    matches("^t2_"),
    everything()  # This ensures any columns that don't fit the pattern are still included at the end
  )

df_wide_encoded_ordered

#check must be a dataframe
str(df_wide_encoded_ordered)
nrow(df_wide_encoded_ordered)
colnames(df_wide_encoded_ordered)
here_save(df_wide_encoded_ordered, "df_wide_encoded_ordered")


df_wide_encoded_ordered <- as.data.frame(df_wide_encoded_ordered)
head(df_wide_encoded_ordered)


# save data for model interpretations
# here_save_qs(prep_coop_all_with_partners_ordered, "prep_coop_all_with_partners_ordered", push_mods)

# Order so that names resemble the previous coding: 
df_wide_encoded_ordered


library(dplyr)
library(stringr)

# create a new dataframe to hold the renamed columns
head(df_wide_encoded_ordered)

head(df_wide_encoded_ordered)
ordinal_columns_partner = c("t0_partner_education_level_coarsen", "t0_partner_eth_cat", "t0_partner_alert_level_combined_lead", ordinal_columns)
continuous_columns_keep_exposures

exposure_var_all <- c(
  "aaron_antagonism",
  "aaron_disinhibition",
  "aaron_emotional_stability",
  "aaron_narcissism",
  "aaron_psychopathy_combined"
)


# process longitudinal data
df_wide_encoded_partner <- margot::margot_process_longitudinal_data_wider(
  df_wide_encoded_ordered,
  ordinal_columns = ordinal_columns_partner, # redefined
  continuous_columns_keep = continuous_columns_keep_exposures,
  not_lost_in_following_wave = "not_lost_following_wave",
 # lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = exposure_var_all
)

# check
colnames(df_wide_encoded_partner)

# define exposure waves
exposure_waves <- c("t1")

# create a regex pattern to match partner variables in exposure waves
pattern <- paste0("^(", paste(exposure_waves, collapse = "|"), ")_partner_")

# identify partner variables to remove
partner_vars_exposure <- grep(pattern, colnames(df_wide_encoded_partner), value = TRUE)

# remove the identified partner variables from the dataframe
df_wide_encoded_partner <- df_wide_encoded_partner %>%
  select(-all_of(partner_vars_exposure))

# verify the columns have been removed
print(colnames(df_wide_encoded_partner))

# check
naniar::miss_var_summary(df_wide_encoded_partner)
naniar::miss_case_table(df_wide_encoded_partner)
naniar::vis_miss(df_wide_encoded_partner, warn_large_data = FALSE)
naniar::gg_miss_var(df_wide_encoded_partner)

# save
here_save_qs(df_wide_encoded_partner, "df_wide_encoded_partner", push_mods)


# read back in if needed
df_wide_encoded_partner<- here_read_qs("df_wide_encoded_partner", push_mods)

# order
df_wide_encoded_partner
df_lmtp <- df_wide_encoded_partner %>%
  # select(-t0_rel_num.x_z) |> 
  select(
    -starts_with("t1_"),
    matches(t1_name_exposure),
    ends_with("_following_wave")
  ) %>%   # relocate `t1_` columns immediately after `t0_` columns
  dplyr::relocate(starts_with("t1_"), .after = starts_with("t0_")) |> 
  # relocate `t2_` columns immediately after `t1_` columns
  dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_")) |> 
  relocate("t1_not_lost_following_wave", .before = starts_with("t2_"))

colnames(df_lmtp)
here_save_qs(df_lmtp,"df_lmtp", push_mods)

naniar::gg_miss_var(df_lmtp)
# define baseline vars
E_lmtp <- df_lmtp |>
  select(-"t0_rel_num") |>  # remove sample weights etc
  select(starts_with("t0_"), -ends_with("_following_wave")) |>
  # select(-all_of(t0_name_exposure)) |>  # use all_of() to avoid deprecation warning ONLY FOR GRF!
  colnames()|>
  sort()


# view
E_lmtp

# save
here_save(E_lmtp, "E_lmtp")

# health three propensity -------------------------------------------------------
# get data
df_lmtp <- here_read('df_lmtp')

# df_grf is a data frame - we must process this data frame in several steps
# user to specify which columns are outcomes, default to 'starts_with("t2_")'
df_propensity_org <- df_lmtp |> select(!starts_with("t2_"))

# Remove NAs and print message that this has been done
df_propensity <- df_propensity_org |> drop_na() |> droplevels()


# if running a baseline model - exclude outcome
# E_propensity_names_baseline =  setdiff(E, c(baseline_exposure_cat, baseline_exposure_continuous))


# set baseline variable names for the exposure model to exclude whichever of the baseline variables is not the   type of the exposure variable.  
E_propensity_names<- setdiff(E_lmtp, "baseline_exposure_cat")
df_propensity$weights_dummy = rep(1, nrow(df_propensity))

# first run model for baseline propensity if this is selected.  The default should be to not select it. 
lmtp_propensity_model_and_plots <- margot::margot_propensity_model_and_plots(
  df_propensity = df_propensity,
  exposure_variable = "t1_aaron_psychopathy_combined",
  baseline_vars = E_propensity_names,
  weights_var_name = "weights_dummy",
  estimand = "ATE",
  method = "ebal",
  focal = NULL
)

summary(lmtp_propensity_model_and_plots$match_propensity)
lmtp_propensity_model_and_plots$love_plot
lmtp_propensity_model_and_plots$summary_plot
lmtp_propensity_model_and_plots$balance_table
lmtp_propensity_model_and_plots$diagnostics


size_bytes <- object.size(lmtp_propensity_model_and_plots)
print(size_bytes, units = "auto") # 49.1 Mb
# use qs to save

##***SAVE
here_save_qs(lmtp_propensity_model_and_plots, "propensity_model_and_plots", push_mods)
