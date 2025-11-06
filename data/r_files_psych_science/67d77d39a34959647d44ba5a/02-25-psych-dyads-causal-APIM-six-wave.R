# 02-estimating-models-lmtp
# 2025 JAN
# joseph.bulbulia@gmail.com
# restart
# rstudioapi::restartSession()

# reproducibility
set.seed(123)

# set path
push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/25-aaron-psycho-wb-long')

# set up libraries
# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
library(margot)

# check version
packageVersion(pkg = "margot")

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
  boilerplate,
  data.table
)

# read data
dat_long_prepare <- here_read_qs("dat_long_prepare", push_mods)

# define wide variable names
# read exposure vars
t0_name_exposure <- here_read("t0_name_exposure")
t1_name_exposure <- here_read("t1_name_exposure")
t2_name_exposure <- here_read("t2_name_exposure")
t3_name_exposure <- here_read("t3_name_exposure")
t4_name_exposure <- here_read("t4_name_exposure")

t0_name_exposure_partner <- here_read('t0_name_exposure_partner')
t1_name_exposure_partner <- here_read('t1_name_exposure_partner')
t2_name_exposure_partner <- here_read('t2_name_exposure_partner')
t3_name_exposure_partner <- here_read('t3_name_exposure_partner')
t4_name_exposure_partner <- here_read('t4_name_exposure_partner')

# read baseline
baseline_vars <- margot::here_read("baseline_vars_binary")
baseline_vars

# read exposures
exposure_var_all <- margot::here_read("exposure_var_all")

# read outcoems
outcome_vars <- margot::here_read("outcome_vars")

# sort
outcome_vars <- sort(outcome_vars)

# weights not used
# t0_sample_weights <- margot::here_read("t0_sample_weights")
# t0_gender_weights <- margot::here_read("t0_gender_weights")

# read waves 
baseline_wave <- margot::here_read("baseline_wave")
exposure_waves <- margot::here_read("exposure_waves")
outcome_wave <- margot::here_read("outcome_wave")

# read vars
all_vars <- margot::here_read("all_vars")
continuous_columns_keep <- margot::here_read("continuous_columns_keep")
ordinal_columns <- margot::here_read("ordinal_columns")

# confounder if using time-varyin gconfounding 
confounder_vars = c("hlth_disability_binary", "parent", "employed_binary", "hours_work_log", outcome_vars) # for time varying confounding

# sort time varying confounders
confounder_vars = sort(confounder_vars)

# save them
here_save(confounder_vars, "confounder_vars")

# partner confounder vars
partner_confounder_vars =  paste0("partner_", confounder_vars)

# view
partner_confounder_vars

# save # WILL USE LATER
here_save(partner_confounder_vars, "partner_confounder_vars")

# we don't need 'not lost'
dat_long_prepare_sans <- dat_long_prepare |> select(-alert_level_combined_lead)


# sor base line vars for imputation
baseline_vars_use<- setdiff(baseline_vars, c("alert_level_combined_lead"))
baseline_vars_use <- sort(baseline_vars_use)


# arrange
dat_long_prepare_sans<- dat_long_prepare_sans |> arrange(id, wave, not_lost)


# viss missing
vis_miss(dat_long_prepare_sans, warn_large_data = F)

# check
str(dat_long_prepare_sans)

# check vars
baseline_vars_use

# check
overlapping_vars <- intersect(baseline_vars_use, outcome_vars)
print(overlapping_vars)

# no overlaps
baseline_vars_use_2 <- setdiff(baseline_vars_use, overlapping_vars)

# check
baseline_vars_use_3 <- c(baseline_vars_use_2, confounder_vars)
baseline_vars_use_3

# remove numeric attributes so missing baseline values can be imputed
dat_long_prepare_sans <- margot::remove_numeric_attributes(dat_long_prepare_sans)

# check names
colnames(dat_long_prepare_sans)

# impute
df_wide_impute <- margot_wide_machine(
  dat_long_prepare_sans,
  id = "id",
  wave = "wave",
  confounder_vars = confounder_vars,
  baseline_vars = baseline_vars_use_3,
  exposure_var = exposure_var_all,
  outcome_vars = outcome_vars,  
  imputation_method = 'mice',  
  include_exposure_var_baseline = TRUE,
  include_outcome_vars_baseline = TRUE
)

# checks
sort( colnames(df_wide_impute) )
head(df_wide_impute)

# missing
vis_miss(df_wide_impute, warn_large_data = F)
colnames(df_wide_impute)

#check
df_wide_impute |> select(ends_with("_na")) |> colnames()

# save data
margot::here_save_qs(df_wide_impute, 'df_wide_impute', push_mods)

# read if needed
df_wide_impute <- margot::here_read_qs('df_wide_impute', push_mods)

length(unique(df_wide_impute$id))

# summary
miss_var_summary <- naniar::miss_var_summary(df_wide_impute)
print(miss_var_summary, n = 33)
naniar::miss_case_table(df_wide_impute)
colnames(df_wide_impute)
naniar::vis_miss(df_wide_impute, warn_large_data = FALSE)

# check
head(df_wide_impute)
df_wide_impute <- df_wide_impute |>   dplyr::relocate(starts_with("t0_rel_num"), .after = "id") |>
arrange(t0_rel_num, id)

# partner data ------------------------------------------------------------
# check the number of individuals per t0_rel_num
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
  # Optionally, stop execution or handle these groups accordingly
  # stop("Non-dyadic groups found. Please address them before proceeding.")
}

# 2.make dyadic dataframe by excluding non-dyadic groups
df_dyads <- df_wide_impute %>%
  group_by(t0_rel_num) %>%
  filter(n() == 2) %>%
  ungroup()

# 3. partner mapping********
partner_mapping <- df_dyads %>%
  select(id, t0_rel_num) %>%
  arrange(t0_rel_num, id) %>%
  group_by(t0_rel_num) %>%
  mutate(partner_id = if_else(row_number() == 1, lead(id), lag(id))) %>%
  ungroup() %>%
  select(id, partner_id)

# 4. checks
self_partners <- partner_mapping %>%
  filter(id == partner_id)

if(nrow(self_partners) > 0) {
  print("The following individuals are incorrectly mapped to themselves:")
  print(self_partners)
} else {
  print("No individuals are mapped to themselves.")
}

# b. checks
bidirectional_checks <- partner_mapping %>%
  inner_join(partner_mapping, by = c("partner_id" = "id")) %>%
  filter(id != partner_id.y)

if(nrow(bidirectional_checks) > 0) {
  print("Some partner mappings are not bidirectional:")
  print(bidirectional_checks)
} else {
  print("All partner mappings are bidirectional.")
}

# 5. join partner data(*********)
prep_coop_all_with_partners <- df_dyads %>%
  left_join(partner_mapping, by = "id") %>%
  left_join(df_dyads, by = c("partner_id" = "id"), suffix = c("", "_partner")) %>%
  arrange(t0_rel_num, id) %>%
  select(-t0_rel_num_partner)  # Remove redundant t0_rel_num from partner data

# 6. check
missing_partners <- prep_coop_all_with_partners %>%
  filter(!partner_id %in% df_dyads$id) %>%
  select(id, partner_id)

if(nrow(missing_partners) > 0) {
  print("The following partner_ids are missing in df_dyads:")
  print(missing_partners)
} else {
  print("All partner_ids are present in df_dyads.")
}

# 7. check assignments
test_that("Partner columns are correctly assigned", {
  # Define the columns to test
  columns_to_test <- c("t0_age", "t0_male_binary", "t0_education_level_coarsen")
  partner_columns_to_test <- paste0(columns_to_test, "_partner")
  
  # interate over cols to check
  for (i in seq_along(columns_to_test)) {
    col <- columns_to_test[i]
    partner_col <- partner_columns_to_test[i]
    
    # get partner dat from orig datframe
    prep_coop_all_with_partners <- prep_coop_all_with_partners %>%
      mutate(
        original_partner_value = df_dyads[[col]][match(partner_id, df_dyads$id)]
      )
    
    # compare the original_partner_value with the partner_col
    expect_equal(
      prep_coop_all_with_partners$original_partner_value,
      prep_coop_all_with_partners[[partner_col]],
      info = paste("Mismatch in", col)
    )
  }
})

# 8.check
na_counts <- sapply(prep_coop_all_with_partners, function(x) sum(is.na(x)))
na_counts_partner_columns <- na_counts[grep("_partner$", names(na_counts))]

print("NA counts in partner columns:")
print(na_counts_partner_columns)

prep_coop_all_with_partners$t1_aaron_disinhibition_partner
head(prep_coop_all_with_partners[ , c("id", "t0_rel_num", "t1_aaron_disinhibition", "t1_aaron_disinhibition_partner", "t3_pwi_partner", "t3_pwi")])


# view
head(prep_coop_all_with_partners[ , c("t1_aaron_antagonism", "t2_aaron_antagonism")])


# # identify any non-reciprocal mappings ( will return error if none)
# non_reciprocal <- reciprocal_pairs %>%
#   filter(partner_id.y != id)
# 
# if(nrow(non_reciprocal) == 0) {
#   print("All partner mappings are bidirectional.")
# } else {
#   print("Some partner mappings are not bidirectional:")
#   print(non_reciprocal)
# }

#  no individual is incorrectly mapped as their own partner.
self_partners <- prep_coop_all_with_partners %>%
  filter(id == partner_id)

if(nrow(self_partners) == 0) {
  print("No individuals are mapped to themselves.")
} else {
  print("The following individuals are incorrectly mapped to themselves:")
  print(self_partners$id)
}


# visualise
library(ggplot2)

# Scatter plot of t2_self_esteem vs t2_self_esteem_partner
ggplot(prep_coop_all_with_partners, aes(x = t1_aaron_disinhibition, y = t2_aaron_disinhibition_partner)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Self Esteem vs Partner's Self Esteem",
       x = "Self Esteem",
       y = "Partner's Self Esteem") +
  theme_minimal()


# check nas
rows_with_na <- prep_coop_all_with_partners %>%
  filter(is.na(t1_aaron_disinhibition) | is.na(t1_aaron_disinhibition_partner))

# view
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

# check all colnames -- we have everything for partners, which is is good
colnames(prep_coop_all_with_partners_ordered)

# save
here_save_qs(prep_coop_all_with_partners_ordered, "prep_coop_all_with_partners_ordered", push_mods)

# read if needed
prep_coop_all_with_partners_ordered<- here_read_qs("prep_coop_all_with_partners_ordered", push_mods)

# order -------------------------------------------------------------------
continuous_columns_keep_exposures <- c(
  continuous_columns_keep,
  t0_name_exposure_partner,
  t0_name_exposure,
  t1_name_exposure_partner,
  t1_name_exposure,
  t2_name_exposure_partner,
  t2_name_exposure,
  t2_name_exposure_partner,
  t3_name_exposure,
  t4_name_exposure
)


# view
continuous_columns_keep_exposures

# view
colnames(prep_coop_all_with_partners_ordered)

# check
naniar::miss_var_summary(prep_coop_all_with_partners_ordered)
naniar::miss_case_table(prep_coop_all_with_partners_ordered)
naniar::gg_miss_var(prep_coop_all_with_partners_ordered)

# save data for model interpretations
# here_save_qs(prep_coop_all_with_partners_ordered, "prep_coop_all_with_partners_ordered", push_mods)
# weights -----------------------------------------------------------------

# order
# order columns - old approach
df_wide_encoded_ordered <- prep_coop_all_with_partners_ordered %>%
  select(
    id,
    matches("^t0_"),
    matches("^t1_"),
    matches("^t2_"),
    matches("^t3_"),
    matches("^t4_"),
    matches("^t5_partner")
  #  everything()  # This ensures any columns that don't fit the pattern are still included at the end
  ) |>
  dplyr::relocate(starts_with("t0_partner"), .before = starts_with("t0")) |>
  dplyr::relocate(starts_with("t1_partner"), .before = starts_with("t1")) |>
  dplyr::relocate(starts_with("t2_partner"), .before = starts_with("t2")) |>
  dplyr::relocate(starts_with("t3_partner"), .before = starts_with("t3")) |>
  dplyr::relocate(starts_with("t4_partner"), .before = starts_with("t4")) |>
  dplyr::relocate(starts_with("t0_rel_num"), .before = starts_with("t0_partner")) |>
  dplyr::relocate(starts_with("partner_id"), .before = starts_with("t0_rel_num")) |>
  dplyr::relocate(starts_with("t1_"), .after = starts_with("t0_")) |>
  # relocate `t2_` columns immediately after `t1_` columns
  dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  # relocate `t3_` columns immediately after `t2_` columns
  dplyr::relocate(starts_with("t3_"), .after = starts_with("t2_")) |>
  # relocate `t4_` columns immediately after `t3_` columns
  dplyr::relocate(starts_with("t4_"), .after = starts_with("t3_")) |>
  dplyr::relocate(starts_with("t5_"), .after = starts_with("t4_"))

#checks
colnames(df_wide_encoded_ordered)
naniar::gg_miss_var(df_wide_encoded_ordered)


#check must be a dataframe
str(df_wide_encoded_ordered)
nrow(df_wide_encoded_ordered)

# ensure data is a data frame
df_wide_encoded_ordered <- as.data.frame(df_wide_encoded_ordered)

# save
here_save(df_wide_encoded_ordered, "df_wide_encoded_ordered")



# Create a new dataframe to hold the renamed columns
head(df_wide_encoded_ordered)


#"t0_partner_alert_level_combined_lead"
ordinal_columns_partner = c("t0_partner_education_level_coarsen", "t0_partner_eth_cat", ordinal_columns)

# view
ordinal_columns_partner

# view
exposure_var_all

# checks
# exposure_vars_t4 <- grep("^t4_", colnames(df_wide_encoded_ordered), value = TRUE)
# print(exposure_vars_t4)

# individual exposure variables
exposure_var_all <- c(
  "aaron_antagonism",
  "aaron_disinhibition",
  "aaron_emotional_stability",
  "aaron_narcissism",
  "aaron_psychopathy_combined"
)

#  partner exposure variables
partner_exposure_vars <- c(
  "partner_aaron_antagonism",
  "partner_aaron_disinhibition",
  "partner_aaron_emotional_stability",
  "partner_aaron_narcissism",
  "partner_psychopathy_combined"
)


# make all confounder vars
all_confounder_vars <-  c(partner_confounder_vars, partner_exposure_vars, confounder_vars)

# check # we will need to change this variable later ****
all_confounder_vars

#  exposure waves
exposure_waves <- c("t1", "t2", "t3", "t4")

#  a regex pattern to match partner variables in exposure waves
pattern <- paste0("^(", paste(exposure_waves, collapse = "|"), ")_partner_")

#  partner variables checks
partner_vars_tv <- grep(pattern, colnames(df_wide_encoded_ordered), value = TRUE)
partner_vars_tv # 


# remove identified partner variables from  dataframe

## NOTE THAT WE NO LONGER DO THIS STEP
# df_wide_encoded_ordered <- df_wide_encoded_ordered %>%
#   select(-all_of(partner_vars_exposure))

# view
df_wide_encoded_ordered


# validate the columns have been removed
print(colnames(df_wide_encoded_ordered))

# view
all_confounder_vars

# impute carry forward
df_wide_encoded_ordered

# var names
exposure_var_all

# use "all_confounder_vars"
exposure_vars_all_imp <- c(all_confounder_vars, exposure_var_all)
exposure_vars_all_imp

# check
exposure_vars_all_imp
exposure_var_all
colnames(df_wide_encoded_ordered)


# hack --------------------------------------------------------------------


# hack. Remove "aaron" from partner exposures
names(df_wide_encoded_ordered) <- gsub("partner_aaron_", "partner_", names(df_wide_encoded_ordered))
# check
colnames(df_wide_encoded_ordered)

# hack, do same for all_confounders list
all_confounder_vars <- gsub("partner_aaron_", "partner_", all_confounder_vars)
exposure_vars_all_imp <- gsub("partner_aaron_", "partner_", exposure_vars_all_imp)

# view
all_confounder_vars
exposure_vars_all_imp

# save
margot::here_save(all_confounder_vars, "all_confounder_vars")
all_confounder_vars
# end hack ----------------------------------------------------------------

# combine var so that we can impute carryign forward
exposure_vars_all_imp <- c(exposure_var_all, all_confounder_vars)

# carry forward imputations
df_wide_ordered_imp <- margot_impute_carry_forward(
  df_wide_encoded_ordered,
  columns_to_impute = exposure_vars_all_imp,  # changed
  columns_no_future_required = all_confounder_vars,
  require_one_observed = TRUE,
  max_carry_forward = 6
  )

# order
df_wide_ordered_imp <- df_wide_ordered_imp %>%
  select(
    id,
    matches("^t0_"),
    matches("^t1_"),
    matches("^t2_"),
    matches("^t3_"),
    matches("^t4_"),
    matches("^t5_partner")
    #  everything()  # This ensures any columns that don't fit the pattern are still included at the end
  ) |>
  dplyr::relocate(starts_with("t0_partner"), .before = starts_with("t0")) |>
  dplyr::relocate(starts_with("t1_partner"), .before = starts_with("t1")) |>
  dplyr::relocate(starts_with("t2_partner"), .before = starts_with("t2")) |>
  dplyr::relocate(starts_with("t3_partner"), .before = starts_with("t3")) |>
  dplyr::relocate(starts_with("t4_partner"), .before = starts_with("t4")) |>
  dplyr::relocate(starts_with("t0_rel_num"), .before = starts_with("t0_partner")) |>
  dplyr::relocate(starts_with("partner_id"), .before = starts_with("t0_rel_num")) |>
  dplyr::relocate(starts_with("t1_"), .after = starts_with("t0_")) |>
  # relocate `t2_` columns immediately after `t1_` columns
  dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  # relocate `t3_` columns immediately after `t2_` columns
  dplyr::relocate(starts_with("t3_"), .after = starts_with("t2_")) |>
  # relocate `t4_` columns immediately after `t3_` columns
  dplyr::relocate(starts_with("t4_"), .after = starts_with("t3_")) |>
  dplyr::relocate(starts_with("t5_"), .after = starts_with("t4_"))


# view
naniar::gg_miss_var(df_wide_ordered_imp)
naniar::vis_miss(df_wide_ordered_imp)

# checks
table(is.na(df_wide_ordered_imp$t0_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t1_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t2_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t3_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t4_aaron_psychopathy_combined))

table(is.na(df_wide_ordered_imp$t0_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t1_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t2_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t3_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t4_aaron_antagonism))


# colnames
colnames(df_wide_ordered_imp)

# hack
continuous_columns_keep_exposures_2 <- gsub("partner_aaron_", "partner_", continuous_columns_keep_exposures)
# view
continuous_columns_keep_exposures_2


# view
head(df_wide_ordered_imp)
exposure_var_all

# checks
table(is.na(df_wide_ordered_imp$t0_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t1_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t2_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t3_aaron_psychopathy_combined))
table(is.na(df_wide_ordered_imp$t4_aaron_psychopathy_combined))


#checks
table(is.na(df_wide_ordered_imp$t0_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t1_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t2_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t3_aaron_antagonism))
table(is.na(df_wide_ordered_imp$t4_aaron_antagonism))


# checks
exposures_t1 <- c(
  "t1_aaron_antagonism",
  "t1_aaron_disinhibition",
  "t1_aaron_emotional_stability",
  "t1_aaron_narcissism",
  "t1_aaron_psychopathy_combined"
)

count_nonmissing_t1 <- rowSums(!is.na(df_wide_ordered_imp[, exposures_t1]))
table(count_nonmissing_t1)


# checks
skimr::skim(
  df_wide_ordered_imp
)

# exposures (there are several)
exposure_vars_all <- exposure_var_all
exposure_vars_all

# other vars
continuous_columns_keep_exposures_2
ordinal_columns_partner

# run function
processed_data <- margot_process_longitudinal_data_wider(
  df_wide_ordered_imp,
  relationship_id = "t0_rel_num",
  ordinal_columns = ordinal_columns_partner,
  continuous_columns_keep = continuous_columns_keep_exposures_2,
  not_lost_in_following_wave = "not_lost_following_wave",
  lost_in_following_wave = "lost_following_wave",
  remove_selected_columns = TRUE,
  exposure_var = exposure_var_all  # <--- HERE
)

# 
# # function in experimental functions
# processed_data <- strict_exposure_outcome_censoring(
#   df_wide = df_wide_ordered_imp,
#   exposure_vars  = exposure_var_all,
#   ordinal_columns = ordinal_columns_partner,
#   continuous_columns_keep = continuous_columns_keep_exposures_2,
#   scale_exposure = FALSE,
#   not_lost_in_following_wave = "not_lost_following_wave",
#   lost_in_following_wave     = "lost_following_wave",
#   remove_selected_columns    = TRUE
# )

# processed_data_strict <- strict_exposure_outcome_censoring(
#   df_wide = df_wide_ordered_imp,
#   exposure_vars  = exposure_var_all,
#   ordinal_columns = ordinal_columns_partner,
#   continuous_columns_keep = continuous_columns_keep_exposures_2,
#   scale_exposure = FALSE,
#   not_lost_in_following_wave = "not_lost_following_wave",
#   lost_in_following_wave     = "lost_following_wave", # not working
#   remove_selected_columns    = TRUE
# )
# checks

# checks
table(is.na(processed_data$t0_aaron_antagonism))
table(is.na(processed_data$t1_aaron_antagonism))
table(is.na(processed_data$t2_aaron_antagonism))
table(is.na(processed_data$t3_aaron_antagonism))
table(is.na(processed_data$t4_aaron_antagonism))

table(is.na(processed_data$t0_aaron_disinhibition))
table(is.na(processed_data$t1_aaron_disinhibition))
table(is.na(processed_data$t2_aaron_disinhibition))
table(is.na(processed_data$t3_aaron_disinhibition))
table(is.na(processed_data$t4_aaron_disinhibition))


table(is.na(processed_data$t0_aaron_emotional_stability))
table(is.na(processed_data$t1_aaron_emotional_stability))
table(is.na(processed_data$t2_aaron_emotional_stability))
table(is.na(processed_data$t3_aaron_emotional_stability))
table(is.na(processed_data$t4_aaron_emotional_stability))


table(is.na(processed_data$t0_aaron_narcissism))
table(is.na(processed_data$t1_aaron_narcissism))
table(is.na(processed_data$t2_aaron_narcissism))
table(is.na(processed_data$t3_aaron_narcissism))
table(is.na(processed_data$t4_aaron_narcissism))

table(is.na(processed_data$t0_aaron_psychopathy_combined))
table(is.na(processed_data$t1_aaron_psychopathy_combined))
table(is.na(processed_data$t2_aaron_psychopathy_combined))
table(is.na(processed_data$t3_aaron_psychopathy_combined))
table(is.na(processed_data$t4_aaron_psychopathy_combined))


# checks
table(processed_data$t0_not_lost_following_wave)
table(processed_data$t0_lost_following_wave)

table(processed_data$t1_not_lost_following_wave)
table(processed_data$t2_not_lost_following_wave)
table(processed_data$t3_not_lost_following_wave)
table(processed_data$t4_not_lost_following_wave)
table(is.na(processed_data$t5_partner_lifesat_z))

table(processed_data$t0_not_lost_following_wave)
table(processed_data$t1_not_lost_following_wave)
table(processed_data$t2_not_lost_following_wave)
table(processed_data$t3_not_lost_following_wave)
table(processed_data$t4_not_lost_following_wave)

hist(processed_data$t0_not_lost_following_wave)
hist(processed_data$t1_not_lost_following_wave)
hist(processed_data$t2_not_lost_following_wave)
hist(processed_data$t3_not_lost_following_wave)
hist(processed_data$t4_not_lost_following_wave)



table(is.na(processed_data$t5_partner_lifesat_z))
# check
colnames(processed_data)

# checks
processed_data$t4_partner_hlth_disability_binary



# checks
table(is.na(processed_data$t5_partner_kessler_latent_depression_z))
table(is.na(processed_data$t4_aaron_antagonism))


# order -----------------------------------------------------------------

# check
naniar::miss_var_summary(processed_data)
naniar::miss_case_table(processed_data)
naniar::vis_miss(processed_data, warn_large_data = FALSE)
naniar::gg_miss_var(processed_data)
naniar::gg_miss_which(processed_data)


#checks
table(is.na(processed_data$t4_conflict_in_relationship_z))
table(is.na(processed_data$t4_partner_kessler_latent_anxiety_z))
table(is.na(processed_data$t4_kessler_latent_anxiety_z))

# save
here_save_qs(processed_data, "processed_data", push_mods)
processed_data <- here_read_qs("processed_data", push_mods)

colnames(processed_data)
# get N at baseline

N <- nrow(processed_data)
N
# check
processed_data_test <- processed_data |> select(t0_rel_num, id, t0_partner_male_binary, t0_male_binary, t0_partner_male_binary, t0_not_lost_following_wave, t1_partner_sat_relationship_z, t1_sat_relationship_z, t2_partner_self_esteem_z) |> arrange(t0_rel_num,id) 


head(processed_data_test)


# save baseline vars ------------------------------------------------------

E_lmtp_pred <- processed_data |>
  # select(-"t0_sample_weights", -"t0_gender_weights") |> # remove sample weights etc
  select(starts_with("t0_"), -ends_with("_following_wave")) |>
  # select(-all_of(t0_name_exposure)) |>  # use all_of() to avoid deprecation warning ONLY FOR GRF!
  colnames() |>
  sort()

# viewbaseline covariates
E_lmtp_pred

# check

# save baseline covariates
margot::here_save(E_lmtp_pred, "E_lmtp_pred")

# get key variables and wrangle into shape for prediction model
# t0_sample_weights <- df_wide_ordered$t0_sample_weights
# t0_gender_weights <- df_wide_ordered$t0_gender_weights
# D_0<- as.factor(df_wide_encoded_partner_ordered$t0_lost)
# method 1 ----------------------------------------------------------------
# probability forest


# not needed unless using experimental function
table(processed_data$t0_lost_following_wave)

# step 2: calculate weights for t0
D_0 <- as.factor(processed_data$t0_lost_following_wave )

# get covariates
cen_0 <- data.frame(processed_data[, E_lmtp_pred])

# view
str(D_0)
str(cen_0)

#  necessary libraries
library(dplyr)

# find factor columns
# factor_cols <- sapply(cen_0, is.factor)
# table(factor_cols)
# # sep numeric and factor columns
# numeric_cols <- cen_0[, !factor_cols]
# factor_data <- cen_0[, factor_cols]
# factor_data
# # one hot encode categorical variables without order
# dummy_matrix <- model.matrix(~ . -1, data = factor_data)
# dummy_df <- as.data.frame(dummy_matrix)

# combine with numeric columns
# cen_0_numeric <- cbind(numeric_cols, dummy_df)
# 
# # verify all columns are numeric
# if(all(sapply(cen_0_numeric, is.numeric))) {
#   message("All features are numeric. Proceeding to build the probability forest.")
# } else {
#   stop("There are still non-numeric features in cen_0_numeric.")
# }

# nrow(cen_0_numeric)

# get probabilities for attrition
library(grf)
cen_forest_0 <- probability_forest(X = cen_0, Y = D_0)
predictions_grf_0 <- predict(cen_forest_0, newdata = cen_0, type = "response")
pscore_0 <- predictions_grf_0$pred[, 2]

# extract the predicted probability of being censored (P(censored))
pscore_0 <- predictions_grf_0$pred[, 2]

# check
hist(pscore_0)
# use the margot_adjust_weights function for t0
t0_weights <- margot_adjust_weights(
  pscore = pscore_0,
  censoring_indicator = processed_data$t0_lost_following_wave,
  sample_weights = NULL, #df_wide_ordered$t0_sample_weights, # Use sample weights as per the question
  trim = FALSE,
  normalize = TRUE,
  lower_percentile = 0.01,
  upper_percentile = 0.99
)

summary(t0_weights$adjusted_weights)
# not that those who drop out do not contribute to weights
hist(t0_weights$adjusted_weights)

# save adjusted weights ---------------------------------------------------
t0_adjusted_weights <- t0_weights$adjusted_weights
here_save(t0_adjusted_weights, "t0_adjusted_weights")

# get weights
processed_data$t0_adjusted_weights <- t0_adjusted_weights

# check
exposure_vars_all

# order data correctly for lmtp -------------------------------------------
df_wide_encoded_partner_ordered_1 <- processed_data %>%
  select(-starts_with("t1_") | matches(exposure_vars_all) | matches(exposure_vars_all_imp)| ends_with("_following_wave")) |>
  select(-starts_with("t2_") | matches(exposure_vars_all) | matches(exposure_vars_all_imp)| ends_with("_following_wave")) |>
  select(-starts_with("t3_") | matches(exposure_vars_all) | matches(exposure_vars_all_imp)| ends_with("_following_wave")) |>
  select(-starts_with("t4_") | matches(exposure_vars_all) | matches(exposure_vars_all_imp)| ends_with("_following_wave")) |>
  #relocate `t1_` columns immediately after `t0_` columns
  # dplyr::relocate(starts_with("name_exposure"), .before = ends_with("t0_")) |> 
  dplyr::relocate(starts_with("t0_") & matches(exposure_vars_all), .before = ("t0_not_lost_following_wave")) |> 
  dplyr::relocate(starts_with("t1_") & matches(exposure_vars_all), .before = ("t1_not_lost_following_wave")) |> 
  # relocate `t2_` columns immediately after `t1_` columns
  dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_")) |> 
  dplyr::relocate(starts_with("t2_") & matches(exposure_vars_all), .before = ("t2_not_lost_following_wave")) |> 
  # relocate `t3_` columns immediately after `t2_` columns
  dplyr::relocate(starts_with("t3_"), .after = starts_with("t2_")) |> 
  dplyr::relocate(starts_with("t3_") & matches(exposure_vars_all), .before = starts_with("t3_not_lost_following_wave")) |> 
  # relocate `t4_` columns immediately after `t3_` columns
  dplyr::relocate(starts_with("t4_"), .after = starts_with("t3_")) |> 
  dplyr::relocate(starts_with("t4_") & matches(exposure_vars_all), .before = starts_with("t4_not_lost_following_wave")) |> 
  # relocate `t4_` columns immediately after `t3_` columns
  dplyr::relocate(starts_with("t5_"), .after = starts_with("t4_"))

naniar::gg_miss_var(df_wide_encoded_partner_ordered_1)

df_lmtp <- df_wide_encoded_partner_ordered_1 |>
  filter(t0_lost_following_wave == 0) |> # for lmtp models
  # ensure factors remain as factors
  mutate(
    across(where(is.factor), ~ factor(as.character(.), levels = levels(.), ordered = is.ordered(.)))
  ) |>
  # Select columns
  select(
    where(is.factor),
    ends_with("_binary"),
    ends_with("_following_wave"),
    ends_with("_z"),
    ends_with("_weights"),
    starts_with("t0_"),
    starts_with("t1_"),
    starts_with("t2_"),
    starts_with("t3_"),
    starts_with("t4_")
  ) |>
  # Relocate columns
  dplyr::relocate(starts_with("t0_partner"), .before = starts_with("t0")) |>
  dplyr::relocate(starts_with("t1_partner"), .before = starts_with("t1")) |>
  dplyr::relocate(starts_with("t2_partner"), .before = starts_with("t2")) |>
  dplyr::relocate(starts_with("t3_partner"), .before = starts_with("t3")) |>
  dplyr::relocate(starts_with("t4_partner"), .before = starts_with("t4")) |>
  dplyr::relocate(starts_with("t0_rel_num"), .after= starts_with("id")) |>
  # dplyr::relocate(starts_with("partner_id"), .before = starts_with("t0_rel_num")) |>
  dplyr::relocate(starts_with("t1_"), .after = starts_with("t0_")) |>
  # relocate `t2_` columns immediately after `t1_` columns
  dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_")) |>
  # relocate `t3_` columns immediately after `t2_` columns
  dplyr::relocate(starts_with("t3_"), .after = starts_with("t2_")) |>
  # relocate `t4_` columns immediately after `t3_` columns
  dplyr::relocate(starts_with("t4_"), .after = starts_with("t3_")) |>
  dplyr::relocate(starts_with("t5_"), .after = starts_with("t4_")) |> 
  relocate("t0_not_lost_following_wave", .before = starts_with("t1_")) |>
  relocate("t1_not_lost_following_wave", .before = starts_with("t2_")) |>
  relocate("t2_not_lost_following_wave", .before = starts_with("t3_")) |>
  relocate("t3_not_lost_following_wave", .before = starts_with("t4_")) |>
  relocate("t4_not_lost_following_wave", .before = starts_with("t5_")) |>
  droplevels()

# check
colnames(df_lmtp)

# check weights exist
hist(df_lmtp$t0_adjusted_weights)

# check
table(is.na(df_lmtp$t1_aaron_disinhibition))

# check
colnames(df_lmtp)

# vis
naniar::gg_miss_var(df_lmtp)

here_save_qs(df_lmtp, "df_lmtp", push_mods)

