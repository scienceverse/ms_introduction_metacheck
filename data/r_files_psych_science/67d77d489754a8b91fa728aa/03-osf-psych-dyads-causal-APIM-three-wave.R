# 2025 FEB
# joseph.bulbulia@gmail.com
# reproducibility
# set data paths
# data path (local - jb only)

# do Aaron's three-wave study according to this: 
# In each model, we will control for the psychopathy variable, wellbeing indicator (e.g., life satisfaction), age and gender for both members of a couple at Time 0. For models examining psychopathy facets (e.g., Antagonism), we will additionally control for the other psychopathy facets (e.g., Emotional Stability, Disinhibition, Narcissism) for both members of a couple at Time 0.

# clear session
rstudioapi::restartSession()

# set path (jb note to self: data wrangling is from 24 "aaron psychopathy" folder)
push_mods <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/data/lmtp/24/aaron_psychopathy"
  )

# reproducibility
set.seed(123)

# libraries
# install `margot` if not already installed
# devtools::install_github("go-bayes/margot")
# detach("package:margot", unload = TRUE)
library(margot)

# check version
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
  purrr, #wrangling
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
  xgboost, # learner
  nnls, # learner
  ranger, # learner
  randomForest, # learner
  cli,
  crayon,
  rlang,
  margot,
  future, # multi-core
  zoo
)

# note: can't run "SL.xgboost" - some predictors might have little or no variation in certain cross‐validation folds. In such cases, $xgboost$ drops those columns during training, so that when it later receives new data with all predictors present the feature names don’t match. So we substitute "SL.randomForest"

# list learners
listWrappers()

# import untransformed data
original_df <- margot::here_read("df_wide_encoded_ordered")

# label mappings for outcomes
label_mapping_partner <- list(
  "t2_partner_conflict_in_relationship_z" = "Relationship Conflict",
  "t2_partner_kessler_latent_anxiety_z" = "Anxiety",
  "t2_partner_kessler_latent_depression_z" = "Depression",
  "t2_partner_kessler6_sum_z" = "Distress",
  "t2_partner_lifesat_z" = "Life Satisfaction",
  "t2_partner_pwi_z" = "Personal Well-Being",
  "t2_partner_sat_relationship_z" = "Relationship Satisfaction",
  "t2_partner_self_esteem_z" = "Self Esteem"
)


# model specifications ----------------------------------------------------
# set fold
my_folds = 5

# set trim (trim is not specified in OSF, but standard in epi to trim between 95-99%. We check densisty ratios to ensure that we don't get extreme values)

# lowest threshold
set_trim = .95

# standard threshold in NZAVS causal inference studies with many observations (used here)
set_trim_3 = .98

# set shift null value
shift_null_value = TRUE

# read exposure vars
t0_name_exposure <- here_read("t0_name_exposure")
t1_name_exposure <- here_read("t1_name_exposure")

# check
t0_name_exposure
t1_name_exposure

# read data # note data has five waves
df_lmtp <- here_read_qs("df_lmtp", push_mods)

# read health outcomes
outcome_vars <- here_read("outcome_vars")
outcome_vars <- sort(outcome_vars)

# view
outcome_vars

# get partner outcomes # third wave
t2_partner_outcomes_z <- paste0("t2_partner_", outcome_vars, "_z")


# get na vals
t2_partner_outcomes_na <- paste0("t2_partner_", outcome_vars, "_na")
t2_partner_outcomes_na

# make vars that have na for exposure (later to remove)
t0_name_exposure_na <- paste0(t0_name_exposure, "_na")
t0_name_exposure_na

t1_name_exposure_na <- paste0(t1_name_exposure, "_na")
t1_name_exposure_na

# check
t2_partner_outcomes_z


# test functions ----------------------------------------------------------
# multisession
plan(multisession, workers = 5)

# needed for progress bars
progressr::handlers(global = TRUE)

# learners
library("SuperLearner")
library("ranger")
library("xgboost")

# learners used for intial exploration (ranger only cause ranger is fast, and plays well with high-dimensional data)
sl_lib <- c("SL.ranger")

# start analysis here ----------------------------------------------------
# import data
df_lmtp<- margot::here_read_qs("df_lmtp", push_mods)


#check
colnames(df_lmtp)
nrow(df_lmtp)

# check
naniar::gg_miss_var(df_lmtp)

# for consistency and avoiding errors
df_lmtp_three <- df_lmtp

#check
naniar::vis_miss(df_lmtp_three)

# check
table((df_lmtp_three$t1_not_lost_following_wave))


# end wrangling -----------------------------------------------------------
# check
colnames(df_lmtp_three)

# check N
nrow(df_lmtp_three)/2

# basseline
# import names of baseline covariates
B <- margot::here_read("E_lmtp")

# check
B

# ensure that variables match OSF statement -------------------------------
# remove elements that end with "_na" or "na_binary"
b_osf <- B[!grepl("_na$|na_binary|t0_hlth_disability_binary|t0_partner_rel_length_year_z|t0_rel_length_year_z|^t0_partner_alert_level_combined_lead", B)]

# check
print(b_osf)

# how many baseline covariates
length(b_osf)

#  ordinary exposures
A <- c(t1_name_exposure)

# check exposures
A


# censoring indicator
C <- c(
  "t1_not_lost_following_wave"  # Time Point 1
)

# check
C

# NO weights as the osf specified that the exposure must be observed in t1
# weights <- df_lmtp$t0_adjusted_weights # t0_adjusted_combo_gender_weights # which weights depnds on the questions

# # view weights
# hist(weights)

# # check
# t2_partner_outcomes_z
colnames(df_lmtp_three)

# view
table(is.na(df_lmtp_three$t1_aaron_disinhibition))


# shift function ----------------------------------------------------------
max_score = 7
min_score = 1

# shift up
shift_up <- function(data, trt) {
  ifelse(data[[trt]] <= max_score - 1, data[[trt]] + 1,  max_score)
}

# rel number -- make character (actually doesn't matter, but good hygiene)
df_lmtp_three$t0_rel_num <- as.character(df_lmtp_three$t0_rel_num)

# check
str(df_lmtp_three$t0_rel_num)

# shift interventions for psychopathy -------------------------------------
# make times
times <- c("t1_")

# treatments 
A_antagonism <-  paste0(times, "aaron_antagonism")
A_disinhibition <-  paste0(times, "aaron_disinhibition")
A_emotional_stability <- paste0(times, "aaron_emotional_stability")
A_narcissism <- paste0(times, "aaron_narcissism")
A_psychopathy_combined <- paste0(times, "aaron_psychopathy_combined")

# combined
A_psychopathy_combined

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

# test a shift in which we independently vary the individual personality variables
A_list <- list(
  c(
    "t1_aaron_antagonism",
    "t1_aaron_disinhibition",
    "t1_aaron_emotional_stability",
    "t1_aaron_narcissism"
  )  # Time Point 1
)

# perform test ------------------------------------------------------------
# using sample of data
# test --------------------------------------------------------------------
df_slice <- df_lmtp_three |>
  slice_head(n = 500) |>
  droplevels() |> 
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

# checks
hist(df_lmtp_three$t2_partner_kessler6_sum_z)

# view
A_antagonism


# learners ----------------------------------------------------------------
# test multi-learners -----------------------------------------------------
# we do not specify learners in the OSF statement. 
# generally good to have multiple learners, but the theory only works for large data
# this dataset is not large, so we focus primarily on tree based methods, and here, also boosting. 

library(randomForest)
library(xgboost) 

# create learners 
sl_lib_2 = c("SL.ranger", "SL.xgboost")

# corresponding to the five wave study, we include the mean as a sanity check.
sl_lib_3 = c("SL.ranger", "SL.xgboost", "SL.mean")

# test new function -------------------------------------------------------
vis_miss(df_slice)

#. review 
# # test model
fit_test_speed <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t2_partner_conflict_in_relationship_z",
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = FALSE,
  save_path = here::here(push_mods),
  prefix = "test_partner_3_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # no time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    # weights = weights_test,
    learners_trt = sl_lib,
    learners_outcome = sl_lib,
    control = lmtp_control(.trim = set_trim)
  )
)

# view results
fit_test_speed$combined_tables$combined_outcomes_shift_up_vs_null

fit_test_speed_2 <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t2_partner_conflict_in_relationship_z",
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = FALSE,
  save_path = here::here(push_mods),
  prefix = "test_partner_3_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # no time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    # weights = weights_test,
    learners_trt = sl_lib_3,
    learners_outcome =  sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# view results
fit_test_speed_2$combined_tables$combined_outcomes_shift_up_vs_null
hist(fit_test_speed_2$models$t2_partner_conflict_in_relationship_z$t2_partner_conflict_in_relationship_z_shift_up$density_ratios)


fit_test_speed_2 <- margot::margot_lmtp(
  data = df_slice,
  outcome_vars = "t2_partner_conflict_in_relationship_z",
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = FALSE,
  save_path = here::here(push_mods),
  prefix = "test_partner_3_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # no time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    # weights = weights_test, # not used in OSF papar
    learners_trt = sl_lib_3,
    learners_outcome =  sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

#checks
fit_test_speed$combined_tables
fit_test_speed_2$combined_tables

# lmtp models -------------------------------------------------------------
# FAST MODELS ------------------------------------------------------------------
# effects on partner antagonism
antagonism_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "antagonism_partner_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    # weights = weights,
    learners_trt = "SL.ranger",
    learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)
# effects on partner disinhibition
disinhibition_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_disinhibition,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "disinhibition_partner_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
   learners_trt = "SL.ranger",
   learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on emotional stability
emotional_stability_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_emotional_stability,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "emotional_stability_partner_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
   learners_trt = "SL.ranger",
   learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner narcissism
narcissism_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_narcissism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "narcissism_partner_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
   learners_trt = "SL.ranger",
   learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "o_psychopathy_combined_partner_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
   learners_trt = "SL.ranger",
   learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)

# experimental model ------------------------------------------------------

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy), # not using shift down 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "x_psychopathy_combined_partner_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
   learners_trt = "SL.ranger",
   learners_outcome = "SL.ranger",
    control = lmtp_control(.trim = set_trim)
  )
)

# STACKED LEARNERS --------------------------------------------------------
# lmtp models -------------------------------------------------------------
# # models ------------------------------------------------------------------
# effects on partner antagonism
antagonism_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "antagonism_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner disinhibition
disinhibition_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_disinhibition,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "disinhibition_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on emotional stability
emotional_stability_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_emotional_stability,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "emotional_stability_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# effects on partner narcissism
narcissism_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_narcissism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "narcissism_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "o_psychopathy_combined_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

# experimental model ------------------------------------------------------

# note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_2 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy), # not using shift down
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "x_psychopathy_combined_partner_2_three_wave",
  lmtp_defaults = list(
     baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
   #  weights = weights,
    learners_trt = sl_lib_2,
    learners_outcome = sl_lib_2,
    control = lmtp_control(.trim = set_trim)
  )
)

psychopathy_combined_partner_lmtp_output_2$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null



# ROBUST STACKED LEARNERS -------------------------------------------------
# # lmtp models -------------------------------------------------------------
# # models ------------------------------------------------------------------
# effects on partner antagonism
antagonism_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_antagonism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "antagonism_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# effects on partner disinhibition
disinhibition_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_disinhibition,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "disinhibition_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# effects on emotional stability
emotional_stability_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_emotional_stability,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "emotional_stability_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# effects on partner narcissism
narcissism_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_narcissism,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "narcissism_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_psychopathy_combined,
  shift_functions = list(shift_up = shift_up), 
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "o_psychopathy_combined_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

# experimental model ------------------------------------------------------

# Note that one can use a shift model in which we move each variable up independently, rather than the combined score
psychopathy_combined_partner_lmtp_output_3 <- margot_lmtp(
  data = df_lmtp_three,
  outcome_vars = t2_partner_outcomes_z,
  trt = A_list,
  shift_functions = list(shift_up_psychopathy= shift_up_psychopathy), # not using shift down
  include_null_shift = shift_null_value,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = "pairwise",
  contrast_scale = "additive",
  save_output = TRUE,
  save_path = here::here(push_mods),
  prefix = "x_psychopathy_combined_partner_3_three_wave",
  lmtp_defaults = list(
    baseline = b_osf,
    mtp = TRUE,
    folds = my_folds,
    # time_vary = L, # NO NEED use time-vary confounding
    outcome_type = "continuous",
    id = "t0_rel_num",
    cens = C,
    #  weights = weights,
    learners_trt = sl_lib_3,
    learners_outcome = sl_lib_3,
    control = lmtp_control(.trim = set_trim_3)
  )
)

psychopathy_combined_partner_lmtp_output_3$combined_tables$combined_outcomes_shift_up_psychopathy_vs_null




# graphs ------------------------------------------------------------------
# create the titles with automatic exposure insertion
title_up <- glue("Three-wave")

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
# antagonism_partner_lmtp_output <- here_read_qs('antagonism_partner_three_wave_lmtp_output', push_mods)
antagonism_partner_lmtp_output <- here_read_qs('antagonism_partner_3_three_wave_lmtp_output', push_mods)


antagonism_partner_shift_up_vs_null <- margot_plot(
  antagonism_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_antagonism_partner_up,
  save_output = TRUE,
  interpret_all_E_gt1 = TRUE,  # all parameter
  save_path = here::here(push_mods),
  original_df = original_df,
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)


# results
antagonism_partner_shift_up_vs_null$plot
antagonism_partner_shift_up_vs_null$transformed_table
cat(antagonism_partner_shift_up_vs_null$interpretation)



# disinhibition
# disinhibition_partner_lmtp_output <- here_read_qs('disinhibition_partner_three_wave_lmtp_output', push_mods)
disinhibition_partner_lmtp_output <- here_read_qs('disinhibition_partner_3_three_wave_lmtp_output', push_mods)

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
# emotional_stability_partner_lmtp_output <- here_read_qs('emotional_stability_partner_three_wave_lmtp_output', push_mods)
emotional_stability_partner_lmtp_output <- here_read_qs('emotional_stability_partner_3_three_wave_lmtp_output', push_mods)

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
# narcissism_partner_lmtp_output <- here_read_qs('narcissism_partner_three_wave_lmtp_output', push_mods)
 narcissism_partner_lmtp_output <- here_read_qs('narcissism_partner_3_three_wave_lmtp_output', push_mods)

narcissism_partner_shift_up_vs_null <- margot_plot(
  narcissism_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_narcissism_partner_up,
  save_output = TRUE,
  save_path = here::here(push_mods),
  original_df = original_df,
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)
narcissism_partner_shift_up_vs_null$plot
narcissism_partner_shift_up_vs_null$transformed_table
cat(narcissism_partner_shift_up_vs_null$interpretation)

# psychopathy -------------------------------------------------------------
# original
# psychopathy_combined_partner_lmtp_output <- here_read_qs('o_psychopathy_combined_partner_three_wave_lmtp_output', push_mods)
psychopathy_combined_partner_lmtp_output <- here_read_qs('o_psychopathy_combined_partner_3_three_wave_lmtp_output', push_mods)
# psychopathy_combined_partner_lmtp_output_2
# make plot and combine results 
psychopathy_combined_partner_shift_up_vs_null <- margot_plot(
  psychopathy_combined_partner_lmtp_output$combined_tables$combined_outcomes_shift_up_vs_null,
  options = options_psychopathy_combined_partner_up,
  save_output = FALSE,
  save_path = here::here(push_mods),
  original_df = original_df,
  interpret_all_E_gt1 = TRUE, 
  label_mapping = label_mapping_partner,
  include_coefficients = FALSE 
)
# results
psychopathy_combined_partner_shift_up_vs_null$plot
psychopathy_combined_partner_shift_up_vs_null$transformed_table
cat(psychopathy_combined_partner_shift_up_vs_null$interpretation)


# experimental ------------------------------------------------------------
# experimental
x_psychopathy_combined_partner_lmtp_output <-  here_read_qs('x_psychopathy_combined_partner_3_three_wave_lmtp_output', push_mods)

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
x_psychopathy_combined_partner_shift_up_vs_null$plot/psychopathy_combined_partner_shift_up_vs_null$plot
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




combined_plot_three_wave <- psychopathy_combined_partner_shift_up_vs_null$plot  /
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
  plot_layout(guides = 'collect', heights = c(2, 1, 1)) +
  plot_annotation(
    title = 'Psychopathic Personality Effects on Partner Well-being: Single Intervention',
    subtitle = "Confounding Control at baseline wave; outcomes measured + 1 wave after intervention (N = 1012)",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 16),
      legend.position = "bottom"
    )
  )

nrow(df_lmtp_three)

# view
combined_plot_three_wave

# perfect
combined_plot_three_wave_2 <- combined_plot_three_wave +
  plot_layout(guides = "collect", heights = c(2, 2, 2)) &
  theme(
    legend.key.size = unit(1.5, "cm"),
    # Adjust size of legend keys
    legend.text = element_text(size = 12),
    # Adjust legend text size
    legend.title = element_text(size = 14)
  ) #

# view
combined_plot_three_wave_2

# save
margot::margot_save_png(
  combined_plot_three_wave_2,
  base_filename = "pub-osf-three_wave_causal_apim_pyschopathy_relationships",
  save_path = here::here(push_mods),
  width = 14,
  height = 20
)

push_mods

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
# 
# # view
# compare_graph
# 
# # perfect
# combined_plot_five_wave_1 <- compare_graph +
#   plot_layout(guides = "collect", heights = c(2, 2, 2)) &
#   theme(
#     legend.key.size = unit(1.5, "cm"),
#     # Adjust size of legend keys
#     legend.text = element_text(size = 12),
#     # Adjust legend text size
#     legend.title = element_text(size = 14)
#   ) #
# 
# # view
# combined_plot_five_wave_1  
# 
# # save
# margot::margot_save_png(
#   combined_plot_five_wave_1,
#   base_filename = "causal_apim_five_wave_compare_psychopathy_graph_including_shift_individual_elements",
#   save_path = here::here(push_mods),
#   width = 18,
#   height = 18
# )
# 


# regressions
# test --------------------------------------------------------------------
summary(lm(
  t2_partner_self_esteem_z ~ t1_aaron_psychopathy_combined ,
  df_lmtp
))
summary(
  lm(
    t2_partner_conflict_summary(
      lm(t4_partner_conflict_summary(
        lm(
          t4_partner_conflict_in_relationship_z ~ t1_aaron_antagonism ,
          df_lmtp
        )
      )
      summary(
        lm(
          t2_partner_conflict_in_relationship_z ~ t1_aaron_antagonism ,
          df_lmtp
        )
      )
      summary(
        lm(
          t2_partner_sat_relationship_z ~ t0_conflict_in_relationship_z ,
          df_lmtp
        )
      )
      
      
      