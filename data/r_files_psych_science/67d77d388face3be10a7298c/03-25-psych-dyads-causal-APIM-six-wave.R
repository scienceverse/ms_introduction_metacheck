# 03-estimating-models-lmtp
# 2024 oct
# joseph.bulbulia@gmail.com
# reproducibility
# set data paths
# data path (local - jb only)

# # restart if needed
# rstudioapi::restartSession()
# this analysis is for censoring on individuals

# set path
push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/25-aaron-psycho-wb-long')

# reproducibility
set.seed(123)

# libraries
# devtools::install_github("go-bayes/margot")
# detach("package:margot", unload = TRUE)

library(margot)
packageVersion(pkg = "margot")

# load necessary libraries
pacman::p_load(
  clarify, # sensitivity analysis for causal inference
  cobalt, # covariate balance tables and plots
  DiagrammeR, # graph and network visualization
  doParallel, # parallel processing with foreach
  future, # multi-core
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
  glmnet, # learner
  xgboost, # learner
  nnls, # learner
  ranger, # learner
  cli,
  crayon,
  rlang,
  margot,
  boilerplate,
  future, # multi-core
  zoo
)


# import untransformed data
original_df <- margot::here_read("df_wide_encoded_ordered")

# get personal outcomes
# label mappings for  outcomes
label_mapping_partner <- list(
  "t5_partner_conflict_in_relationship_z" = "Relationship Conflict",
  "t5_partner_kessler_latent_anxiety_z" = "Anxiety",
  "t5_partner_kessler_latent_depression_z" = "Depression",
  "t5_partner_kessler6_sum_z" = "Distress",
  "t5_partner_lifesat_z" = "Life Satisfaction",
  "t5_partner_pwi_z" = "Personal Well-Being",
  "t5_partner_sat_relationship_z" = "Relationship Satisfaction",
  "t5_partner_self_esteem_z" = "Self Esteem"
)

# read data 
df_lmtp <- here_read_qs("df_lmtp", push_mods)

#check
df_lmtp <- df_lmtp |> arrange(t0_rel_num, id)
head(df_lmtp)

# make character
df_lmtp$t0_rel_num <- as.character(df_lmtp$t0_rel_num)

# check
vis_miss(df_lmtp)

# read health outcomes
outcome_vars <- here_read("outcome_vars")
outcome_vars <- sort(outcome_vars)

# set fold
my_folds = 5

# set trim
set_trim = .95

# set shift null value
shift_null_value = TRUE

# read exposure vars
t0_name_exposure <- here_read("t0_name_exposure")
t1_name_exposure <- here_read("t1_name_exposure")
t2_name_exposure <- here_read("t2_name_exposure")
t3_name_exposure <- here_read("t3_name_exposure")
t4_name_exposure <- here_read("t4_name_exposure")

# view
outcome_vars

# get partner outcomes
t5_partner_outcomes_z <- paste0("t5_partner_", outcome_vars, "_z")

# check
t5_partner_outcomes_z

# general partner outcomes
partner_outcomes <- paste0("partner_", outcome_vars)

# combined outcome vars
combo_outcomes <- c(outcome_vars, partner_outcomes)


# test functions ----------------------------------------------------------
# multisession
plan(multisession, workers = 5)

# needed for progress bars
progressr::handlers(global = TRUE)

# learners
library("SuperLearner")
library("ranger")
library("xgboost")

# learners used
# not that stacked learners will only work theoretically under large N. 
# this is a small N, with many covariates, so we use regression forests which have 
# better properties in this setting (see Athfield & Wager)

#
sl_lib <- c("SL.ranger")

# start analysis here ----------------------------------------------------

# double check that relationship number is a character
str(df_lmtp$t0_rel_num)


# check
naniar::gg_miss_case(df_lmtp)
naniar::vis_miss(df_lmtp)

# check
# get confounder vars
all_confounder_vars <- here_read("all_confounder_vars")
hlth_disability_vars <- c("hlth_disability", "partner_hlth_disability")

# check
colnames(df_lmtp)

# get n


# # lagged confounders time 1 (same as baseline confounders)
L1 <- df_lmtp |>
  select(
  matches(paste0("^t1_", hlth_disability_vars)),
) |> colnames()

# view
L1

# lagged confounders time 2
L2 <- df_lmtp |>
  select(
    matches(paste0("^t1_", partner_outcomes)),
    matches(paste0("^t2_", hlth_disability_vars))
  ) |>
  colnames() 

# view
L2



# lagged confounders time 3
L3 <- df_lmtp |>
  select(
    matches(paste0("^t2_", partner_outcomes)),
    matches(paste0("^t3_", hlth_disability_vars))
    
  ) |>
  colnames() 

# view
L3

# lagged confounders time 4
L4 <- df_lmtp |>
  select(
    matches(paste0("^t3_", partner_outcomes)),
    matches(paste0("^t4_", hlth_disability_vars))
  ) |>
  colnames() 

# view
L4

str(df_lmtp)
str(df_lmtp, list.len = ncol(df_lmtp))

# check
length(L3)

# basseline
# import names of baseline covariates
B <- margot::here_read("E_lmtp_pred")

# check
B
length(B)

#  ordinary exposures
A <- c(t1_name_exposure, t2_name_exposure, t3_name_exposure, t4_name_exposure)

# check exposures
A

# bind censoring indicators
C <- c(
  "t1_not_lost_following_wave",  # Time Point 1
  "t2_not_lost_following_wave",  # Time Point 2
  "t3_not_lost_following_wave",   # Time Point 3
  "t4_not_lost_following_wave"   # Time Point 3
)

# check
C

# combine time-varying confounding
L <- list(c(L1), c(L2), c(L3), c(L4))

# check
L


# weights are same for all analyses
weights <- df_lmtp$t0_adjusted_weights # t0_adjusted_combo_gender_weights # which weights depnds on the questions

# view weights
hist(weights)

# shift function ----------------------------------------------------------
max_score = 7
min_score = 1

# shift up
shift_up <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 1, data[[trt]] + 1,  max_score)
}

# make weights if used
weights <- df_lmtp$t0_adjusted_weights

# shift interventions for psychopathy -------------------------------------
# make times
times <- c("t1_", "t2_", "t3_", "t4_")

# treatments 
A_antagonism <-  paste0(times, "aaron_antagonism")
A_disinhibition <-  paste0(times, "aaron_disinhibition")
A_emotional_stability <- paste0(times, "aaron_emotional_stability")
A_narcissism <- paste0(times, "aaron_narcissism")
A_psychopathy_combined <- paste0(times, "aaron_psychopathy_combined")

# combined
A_psychopathy_combined

# make all treatment indicators
A_antagonism <-  paste0(times, "aaron_antagonism")
A_disinhibition <-  paste0(times, "aaron_disinhibition")
A_emotional_stability <- paste0(times, "aaron_emotional_stability")
A_narcissism <- paste0(times, "aaron_narcissism")
A_psychopathy_combined <- paste0(times, "aaron_psychopathy_combined")

# define vectorised shift function.  
shift_up_psychopathy <- function(data, trt_list) {
  shifted_treatments <- list()
  
  # flatten the list of treatment variables
  all_trts <- unlist(trt_list)
  
  # apply shift logic/ error handling
  shifted_treatments <- lapply(all_trts, function(trt_var) {
    if (!trt_var %in% names(data)) {
      stop(paste("Treatment variable", trt_var, "not found in the data."))
    }
    
    if (grepl("antagonism", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] <= 6, data[[trt_var]] + 1, 7))
    } else if (grepl("disinhibition", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] <= 6, data[[trt_var]] + 1, 7))
    } else if (grepl("emotional_stability", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] <= 6, data[[trt_var]] + 1, 7))
    } else if (grepl("narcissism", trt_var)) {
      ifelse(is.na(data[[trt_var]]), NA, ifelse(data[[trt_var]] <= 6, data[[trt_var]] + 1, 7))
    } else {
      stop(paste("Unexpected treatment variable:", trt_var))
    }
  })
  
  # name
  names(shifted_treatments) <- all_trts
  
  return(shifted_treatments)
}

# list treatments if using interaction approach
A_list <- list(
  c("t1_aaron_antagonism", "t1_aaron_disinhibition", "t1_aaron_emotional_stability", "t1_aaron_narcissism"),  # Time Point 1
  c("t2_aaron_antagonism", "t2_aaron_disinhibition", "t2_aaron_emotional_stability", "t2_aaron_narcissism"),  # Time Point 2
  c("t3_aaron_antagonism", "t3_aaron_disinhibition", "t3_aaron_emotional_stability", "t3_aaron_narcissism"),
  c("t4_aaron_antagonism", "t4_aaron_disinhibition", "t4_aaron_emotional_stability", "t4_aaron_narcissism")
)

#  test ------------------------------------------------------------
# using sample of data
# get 500 obs
df_slice <- df_lmtp |>
  slice_head(n = 500) |>
  as.data.frame()

# checks
all_trts <- unlist(A_list)
missing_trts <- setdiff(all_trts, names(df_slice))
if(length(missing_trts) > 0){
  stop(paste("Missing treatment variables in data:", paste(missing_trts, collapse = ", ")))
}

missing_cens <- setdiff(C, names(df_slice))
if(length(missing_cens) > 0){
  stop(paste("Missing censoring variables in data:", paste(missing_cens, collapse = ", ")))
}

# needs to be nrows of df_slice
weights_test <- df_slice$t0_adjusted_weights

# run test -------------------------------------------------------------------
fit_test_speed <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t5_partner_conflict_in_relationship_z",
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy),
                      #   shift_down_psychopathy = shift_down_psychopathy),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_test_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights_test,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)
 
# view results
fit_test_speed$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null

# test
# xgboost won't work because some predictors prob have little or no variation in certain cross‐validation folds. in such cases, $xgboost$ may drop those columns during training, so that when it later receives new data with all predictors present the feature names don’t match. so we replace xgboost with randomForest.

fit_test_speed_0 <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t5_partner_conflict_in_relationship_z",
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up),
  #   shift_down_psychopathy = shift_down_psychopathy),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_test_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights_test,
    learners_trt = c("SL.ranger", "SL.xgboost"),
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# view results
fit_test_speed_0$combined_tables$combined_outcomes_shift_up_vs_null

# test multi-learners -----------------------------------------------------
sl_lib_2 = c("SL.ranger", "SL.randomForest", "SL.mean")

# test model
fit_test_speed_2 <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t5_partner_conflict_in_relationship_z",
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up),
  #   shift_down_psychopathy = shift_down_psychopathy), # test just using one
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_test_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights_test,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# view results
fit_test_speed_2$combined_tables$combined_outcomes_shift_up_vs_null

# lmtp models -------------------------------------------------------------
# models ------------------------------------------------------------------
# effects on partner antagonism
antagonism_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_antagonism_partner_",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner disinhibition
disinhibition_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_disinhibition,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_disinhibition_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on emotional stability
emotional_stability_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_emotional_stability,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_emotional_stability_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner narcissism
narcissism_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_narcissism,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_narcissism_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_o_psychopathy_combined_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# view results
psychopathy_combined_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null

# experimental model ------------------------------------------------------

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_x <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy), # not using shift down 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_x_psychopathy_combined_partner",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# check density ratios
hist(psychopathy_combined_partner_lmtp_output$models$t5_partner_conflict_in_relationship_z$t5_partner_conflict_in_relationship_z_shift_up$density_ratios)
hist(psychopathy_combined_partner_lmtp_output$models$t5_partner_conflict_in_relationship_z$t5_partner_conflict_in_relationship_z_null$density_ratios)


# lmtp models -------------------------------------------------------------
# models ------------------------------------------------------------------
# effects on partner antagonism
antagonism_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_antagonism_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner disinhibition
disinhibition_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_disinhibition,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_disinhibition_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on emotional stability
emotional_stability_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_emotional_stability,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_emotional_stability_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner narcissism
narcissism_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_narcissism,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_narcissism_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_o_psychopathy_combined_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

psychopathy_combined_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up),
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_sdr,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_o_psychopathy_combined_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

x_psychopathy_combined_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp,
  outcome_vars = t5_partner_outcomes_z,
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "c_x_psychopathy_combined_partner_2",
  lmtp_defaults = list(
    baseline = B,
    mtp = TRUE,
    folds = my_folds,
    time_vary = L, # use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# view results
psychopathy_combined_partner_lmtp_output_3$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null


# graphs ------------------------------------------------------------------
# create the titles with automatic exposure insertion
title_up <- glue("six-wave")

# create  options
options_base_partner_up <- list(
  type = "RD",
  #  title = title_up,
  order = "default",
  colors = c(
    "positive" = "#E69F00",
    "not reliable" = "grey50",
    "negative" = "#56B4E9"
  ),
  x_offset = -0.5,  # will be set based on type
  x_lim_lo = -0.5, # will be set based on type
  x_lim_hi = 0.5, # will be set based on type
  text_size = 4,
  linewidth = 0.5,
  estimate_scale = 1,
  base_size = 16,
  point_size = 3,
  title_size = 16,
  subtitle_size = 16,
  legend_text_size = 8,
  legend_title_size = 8,
  label_mapping_partner = label_mapping_partner
)


options_psychopathy_combined_partner_up<- margot_plot_create_options(
  title = "Global Psychopathy",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "psychopathy_combined_partner_up"
)


options_psychopathy_experimental_combined_partner_up<- margot_plot_create_options(
  title = "Global Psychopathy: shift each + 1",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "psychopathy_x_combined_partner_up"
)

options_antagonism_partner_up<- margot_plot_create_options(
  title = "Antagonism",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "antagonism_partner_up"
)
options_disinhibition_partner_up<- margot_plot_create_options(
  title = "Disinhibition",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "disinhibition_partner_up"
)
options_emotional_stability_partner_up<- margot_plot_create_options(
  title = "Emotional Stability",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "emotional_stability_partner_up"
)

options_narcissism_partner_up<- margot_plot_create_options(
  title = "Narcissism",
  subtitle = "",
  base_defaults = options_base_partner_up,
  filename_prefix = "narcissism_partner_up"
)

options_narcissism_partner_up<- margot_plot_create_options(
  subtitle = "Narcissism",
  base_defaults = options_base_partner_up,
  filename_prefix = "narcissism_partner_up"
)

# results -----------------------------------------------------------------


# results antagonism ------------------------------------------------------
# antagonism_partner_lmtp_output <- here_read_qs('c_antagonism_partner__lmtp_output', push_mods

# use full learner set (inference improved)
antagonism_partner_lmtp_output <- here_read_qs('c_antagonism_partner_2_lmtp_output', push_mods)

# checks
antagonism_partner_lmtp_output$combined_tables
hist(antagonism_partner_lmtp_output$models$t5_partner_conflict_in_relationship_z$t5_partner_conflict_in_relationship_z_shift_up$density_ratios)

antagonism_partner_shift_up_vs_null <- margot_plot(
  antagonism_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_antagonism_partner_up,
  save_output = TRUE,
  save_path = here::here(push_mods),
  original_df = original_df,
  interpret_all_E_gt1 = TRUE, 
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
  )


# results
antagonism_partner_shift_up_vs_null$plot
antagonism_partner_shift_up_vs_null$transformed_table
cat(antagonism_partner_shift_up_vs_null$interpretation)

# disinhibition
# disinhibition_partner_lmtp_output <- here_read_qs('c_disinhibition_partner_lmtp_output', push_mods)
disinhibition_partner_lmtp_output <- here_read_qs('c_disinhibition_partner_2_lmtp_output', push_mods)

#plot
disinhibition_partner_shift_up_vs_null <- margot_plot(
  disinhibition_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_disinhibition_partner_up,
  save_output = TRUE,
  save_path = here::here(push_mods),
  original_df = original_df,
  interpret_all_E_gt1 = TRUE, 
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE  # set to FALSE to exclude coefficients
)
# results
disinhibition_partner_shift_up_vs_null$plot
disinhibition_partner_shift_up_vs_null$transformed_table
cat(disinhibition_partner_shift_up_vs_null$interpretation)


# emotional stability
# emotional_stability_partner_lmtp_output <- here_read_qs('c_emotional_stability_partner_lmtp_output', push_mods)
emotional_stability_partner_lmtp_output <- here_read_qs('c_emotional_stability_partner_2_lmtp_output', push_mods)

# make plot
emotional_stability_partner_shift_up_vs_null <- margot_plot(
  emotional_stability_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_emotional_stability_partner_up,
  save_output = TRUE,
  save_path = here::here(push_mods),
  original_df = original_df,
  interpret_all_E_gt1 = TRUE, 
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
  )


# results
emotional_stability_partner_shift_up_vs_null$plot
emotional_stability_partner_shift_up_vs_null$transformed_table
cat(emotional_stability_partner_shift_up_vs_null$interpretation)

# narcissims
#narcissism_partner_lmtp_output <- here_read_qs('c_narcissism_partner_lmtp_output', push_mods)
narcissism_partner_lmtp_output <- here_read_qs('c_narcissism_partner_2_lmtp_output', push_mods)

narcissism_partner_shift_up_vs_null <- margot_plot(
  narcissism_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_narcissism_partner_up,
  save_output = TRUE,
  save_path = here::here(push_mods),
  interpret_all_E_gt1 = TRUE, 
  original_df = original_df,
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)
narcissism_partner_shift_up_vs_null$plot
narcissism_partner_shift_up_vs_null$transformed_table
cat(narcissism_partner_shift_up_vs_null$interpretation)



# psychopathy -------------------------------------------------------------
# original
# psychopathy_combined_partner_lmtp_output <- here_read_qs('c_o_psychopathy_combined_partner_lmtp_output', push_mods)
psychopathy_combined_partner_lmtp_output <- here_read_qs('c_x_psychopathy_combined_partner_3_lmtp_output', push_mods)

hist(psychopathy_combined_partner_lmtp_output$models$t5_partner_conflict_in_relationship_z$t5_partner_conflict_in_relationship_z_shift_up$density_ratios)

# make plot and combine results 
psychopathy_combined_partner_shift_up_vs_null <- margot_plot(
  psychopathy_combined_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null,
  options = options_psychopathy_combined_partner_up,
  save_output = FALSE,
  save_path = here::here(push_mods),
  original_df = original_df,
  interpret_all_E_gt1 = TRUE, 
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)


psychopathy_combined_partner_shift_up_vs_null$plot
psychopathy_combined_partner_shift_up_vs_null$transformed_table
cat(psychopathy_combined_partner_shift_up_vs_null$interpretation)


psychopathy_combined_partner_shift_up_vs_null_3 <- margot_plot(
  psychopathy_combined_partner_lmtp_output_2$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_psychopathy_combined_partner_up,
  save_output = FALSE,
  save_path = here::here(push_mods),
  original_df = original_df,
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)

# results
psychopathy_combined_partner_shift_up_vs_null$plot/psychopathy_combined_partner_shift_up_vs_null_2$plot

psychopathy_combined_partner_shift_up_vs_null$transformed_table
cat(psychopathy_combined_partner_shift_up_vs_null$interpretation)

cat(psychopathy_combined_partner_shift_up_vs_null$interpretation)


# experimental ------------------------------------------------------------
# experimental. # NOT YET RUN
#x_psychopathy_combined_partner_lmtp_output <-  here_read_qs('c_x_psychopathy_combined_partner_lmtp_output', push_mods)
x_psychopathy_combined_partner_lmtp_output <-  here_read_qs('c_x_psychopathy_combined_partner_3_lmtp_output', push_mods)

x_psychopathy_combined_partner_lmtp_output$models$t5_partner_conflict_in_relationship_z$t5_partner_conflict_in_relationship_z_shift_up_psychopathy$fits_r

x_psychopathy_combined_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null
x_psychopathy_combined_partner_shift_up_vs_null <- margot_plot(
  x_psychopathy_combined_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null,
  options = options_psychopathy_experimental_combined_partner_up,
  save_output = FALSE,
  save_path = here::here(push_mods),
  original_df = original_df,
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)

# results
x_psychopathy_combined_partner_shift_up_vs_null$plot/
psychopathy_combined_partner_shift_up_vs_null$plot

x_psychopathy_combined_partner_shift_up_vs_null$transformed_table
cat(x_psychopathy_combined_partner_shift_up_vs_null$interpretation)


x_psychopathy_combined_partner_shift_up_vs_null$plot/psychopathy_combined_partner_shift_up_vs_null$plot

L
# combined graph ----------------------------------------------------------

# make combined graphs
library(patchwork)



# experimental graph

# compare_graph <- (psychopathy_combined_partner_shift_up_vs_null$plot &  theme(legend.position = "none")) +  (x_psychopathy_combined_partner_shift_up_vs_null$plot ) +
#   plot_layout(guides = 'collect', heights = c(4, 4)) +
#   plot_annotation(
#     title = 'Psychopathic Personality Effects on Partner Well-being: Four-Year Intervention',
#     tag_levels = "A",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 14),
#       legend.position = "bottom",
#       legend.key.size = unit(1.5, "cm"),
#       # Adjust size of legend keys
#       legend.text = element_text(size = 14),
#       # Adjust legend text size
#       legend.title = element_text(size = 14)
#     ))
# 
# # view
# compare_graph




combined_plot_five_wave <- psychopathy_combined_partner_shift_up_vs_null$plot  /
  (
    (
      antagonism_partner_shift_up_vs_null$plot &
        theme(legend.position = "none")
    ) + (
      disinhibition_partner_shift_up_vs_null$plot &
        theme(legend.position = "none")
    )
  ) /
  (
    (
      emotional_stability_partner_shift_up_vs_null$plot &
        theme(legend.position = "none")
    ) + (
      narcissism_partner_shift_up_vs_null$plot
      &
        theme(legend.position = "none")
    )
  ) +
  plot_layout(guides = 'collect', heights = c(2, 2, 2)) +
  plot_annotation(
    title = 'Psychopathic Personality Effects on Partner Well-being: Four-Year Intervention',
    subtitle = "Confounding Control at baseline wave; outcomes measured + 1 wave after interventions (N = 1594)",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 16),
      legend.position = "bottom"
    )
  )
nrow(df_lmtp)
# view
combined_plot_five_wave

# perfect
combined_plot_five_wave_2 <- combined_plot_five_wave +
  plot_layout(guides = "collect", heights = c(2, 2, 2)) &
  theme(
    legend.key.size = unit(1.5, "cm"),
    # Adjust size of legend keys
    legend.text = element_text(size = 12),
    # Adjust legend text size
    legend.title = element_text(size = 14)
  ) #

# view
combined_plot_five_wave_2

# save
margot::margot_save_png(
  combined_plot_five_wave_2,
  base_filename = "pub-causal_apim_five_wave_pyschopathy_relationships",
  save_path = here::here(push_mods),
  width = 14,
  height = 20
)



# 
# compare_graph <- #psychopathy_combined_partner_shift_up_vs_null$plot 
#   ((psychopathy_combined_partner_shift_up_vs_null$plot &  theme(legend.position = "none")) +  (x_psychopathy_combined_partner_shift_up_vs_null$plot ) ) /
#   (
#     (
#       antagonism_partner_shift_up_vs_null$plot &
#         theme(legend.position = "none")
#     ) + (
#       disinhibition_partner_shift_up_vs_null$plot &
#         theme(legend.position = "none")
#     )
#   ) /
#   (
#     (
#       emotional_stability_partner_shift_up_vs_null$plot &
#         theme(legend.position = "none")
#     ) + (
#       narcissism_partner_shift_up_vs_null$plot
#       &
#         theme(legend.position = "none")
#     )
#   ) +
#   plot_layout(guides = 'collect', heights = c(2, 1, 1)) +
#   plot_annotation(
#     title = 'Psychopathic Personality Effects on Partner Well-being: Four-Year Intervention',
#     tag_levels = "A",
#     theme = theme(
#       plot.title = element_text(face = "bold", size = 20),
#       legend.position = "bottom"
#     )
#   )

# view
compare_graph

# perfect
combined_plot_five_wave_1 <- compare_graph +
  plot_layout(guides = "collect", heights = c(2, 2, 2)) &
  theme(
    legend.key.size = unit(1.5, "cm"),
    # Adjust size of legend keys
    legend.text = element_text(size = 12),
    # Adjust legend text size
    legend.title = element_text(size = 14)
  ) #

# view
combined_plot_five_wave_1  

# save
margot::margot_save_png(
  combined_plot_five_wave_1,
  base_filename = "causal_apim_five_wave_compare_psychopathy_graph_including_shift_individual_elements",
  save_path = here::here(push_mods),
  width = 11,
  height = 16
)



# regressions
# test --------------------------------------------------------------------
summary(lm(
  t5_partner_self_esteem_z ~ t1_aaron_psychopathy_combined ,
  df_lmtp
))


# test learner ------------------------------------------------------------
