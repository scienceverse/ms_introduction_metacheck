# original-analysis.R
# cleaned version of the original muslim diversity study analysis
# this shows the exact analytical workflow used in the paper
# 
# note: actual participant data not included for privacy
# to reproduce with real data, apply for access at: https://osf.io/75snb/
#
# **author**: Joseph Bulbulia
# **contact**: joseph.bulbulia@vuw.ac.nz
# **date**: 2025-JUL-11

# ============================================================================
# SETUP
# ============================================================================

# load packages
# margot is off cran
# install and load 'margot' package
# if (!require(margot, quietly = TRUE)) {
#   devtools::install_github("go-bayes/margot")
# }

library(margot)
library(mice)
library(survey)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(flextable)
library(skimr)

# set seed for reproducibility
set.seed(123)

# ============================================================================
# DATA LOADING (PRIVATE - NOT INCLUDED)
# ============================================================================

# in the actual analysis, data was loaded from secure location:
# dat <- margot::here_read_qs("nzavs_data", data_dir)

# data contains NZAVS longitudinal survey data with:
# - wave 15 (2023) baseline data
# - muslim booster sample included
# - post-stratification weights calibrated to 2018 census

# ============================================================================
# VARIABLE DEFINITIONS
# ============================================================================

# define baseline covariates
baseline_vars <- c(
  "age",
  "eth_cat",  # ethnicity categories
  "male_binary",
  "born_nz_binary"
)

# define outcome variables by domain

# religion outcomes (7 variables)
raw_religion_outcome <- c(
  "religion_believe_god_binary",
  "religion_believe_spirit_binary",
  "religion_church",
  "religion_identification_level",
  "religion_prayer",
  "religion_scripture",
  "religion_spiritual_identification"
)

# discrimination outcomes (4 variables)
raw_discrim_outcome <- c(
  "perc_age_discrim",
  "perc_discrim",
  "perc_gend_discrim",
  "perc_religious_discrim"
)

# health outcomes (3 variables)
raw_outcomes_health <- c(
  "hlth_sleep_hours",
  "log_hours_exercise",
  "short_form_health"
)

# psychological outcomes (4 variables)
raw_outcomes_psych <- c(
  "hlth_fatigue",
  "kessler_latent_anxiety",
  "kessler_latent_depression",
  "rumination"
)

# life-reflective outcomes (4 variables)
raw_outcomes_life <- c(
  "gratitude",
  "lifesat",
  "meaning_purpose",
  "meaning_sense"
)

# social outcomes (7 variables)
raw_outcomes_social <- c(
  "belong_accept",
  "belong_beliefs",
  "belong_outsider_reversed",  # note: fixed typo from original
  "neighbourhood_community",
  "support_help",
  "support_noguidance_reversed",
  "support_turnto"
)

# present-reflective outcomes (7 variables)
raw_outcomes_present <- c(
  "forgiveness",
  "perfectionism",
  "pwb_standard_living",
  "pwb_your_future_security",
  "pwb_your_health",
  "pwb_your_relationships",
  "self_esteem"
)

# ============================================================================
# LABEL MAPPINGS FOR PLOTS AND TABLES
# ============================================================================

label_mapping_religion <- list(
  "religion_believe_god_binary" = "Belief in God",
  "religion_believe_spirit_binary" = "Belief in Spirit",
  "religion_church" = "Monthly Religious Service Attendance",
  "religion_identification_level" = "Religious Identification Level",
  "religion_prayer" = "Weekly Prayer",
  "religion_scripture" = "Weekly Scripture",
  "religion_spiritual_identification" = "Spiritual Identification Level"
)

label_mapping_discrim <- list(
  "perc_age_discrim" = "Perceived Age Discrimination",
  "perc_discrim" = "Perceived Ethnic Discrimination",
  "perc_gend_discrim" = "Perceived Gender Discrimination",
  "perc_religious_discrim" = "Perceived Religious Discrimination"
)

label_mapping_health <- list(
  "hlth_sleep_hours" = "Sleep Hours",
  "log_hours_exercise" = "Hours of Exercise (log)",
  "short_form_health" = "Subjective Health"
)

label_mapping_psych <- list(
  "hlth_fatigue" = "Fatigue",
  "kessler_latent_anxiety" = "Anxiety",
  "kessler_latent_depression" = "Depression",
  "rumination" = "Rumination"
)

label_mapping_life <- list(
  "gratitude" = "Gratitude",
  "lifesat" = "Life Satisfaction",
  "meaning_purpose" = "Meaning & Purpose",
  "meaning_sense" = "Sense of Meaning"
)

label_mapping_social <- list(
  "belong_accept" = "Belonging: Acceptance",
  "belong_beliefs" = "Belonging: Shared Beliefs",
  "belong_outsider_reversed" = "Belonging: Not an Outsider",
  "neighbourhood_community" = "Neighbourhood Community",
  "support_help" = "Support: Help Available",
  "support_noguidance_reversed" = "Support: Guidance Available",
  "support_turnto" = "Support: Someone to Turn To"
)

label_mapping_present <- list(
  "forgiveness" = "Forgiveness",
  "perfectionism" = "Perfectionism",
  "pwb_standard_living" = "PWB: Standard of Living",
  "pwb_your_future_security" = "PWB: Future Security",
  "pwb_your_health" = "PWB: Your Health",
  "pwb_your_relationships" = "PWB: Your Relationships",
  "self_esteem" = "Self-Esteem"
)

# ============================================================================
# DATA PREPARATION WORKFLOW
# ============================================================================

# in the actual analysis:

# 1. filter to wave 15 (2023) baseline data
# df_15 <- dat |> 
#   filter(wave == "2023" & year_measured == 1) |>
#   filter(!is.na(census_religion_l2))  # require religion data

# 2. create muslim indicator
# df_15b <- df_15 |>
#   mutate(muslim = as.integer(census_religion_l2 == 401))

# 3. log transform exercise hours
# df_15a <- margot_log_transform_vars(
#   df_15b,
#   vars = "hours_exercise",
#   prefix = "log_",
#   keep_original = FALSE
# )

# 4. extract analysis variables
# all_vars <- c(baseline_vars, "muslim", all_outcome_vars, "adjusted_weights", "id")
# df_analysis <- df_15a |> select(all_of(all_vars))

# ============================================================================
# GROUP-AWARE MULTIPLE IMPUTATION
# ============================================================================

# key innovation: muslim identity used as auxiliary variable
# this preserves group-specific distributions in imputation

prepare_data_group_aware_imputation <- function(df_models, 
                                               group_var = "muslim",
                                               weight_col = "adjusted_weights", 
                                               m = 20, 
                                               maxit = 10, 
                                               method = "pmm", 
                                               seed = 123, 
                                               printFlag = TRUE) {
  
  # create mice object
  init_mice <- mice(df_models, maxit = 0, printFlag = FALSE)
  
  # get predictor matrix
  pred_matrix <- init_mice$predictorMatrix
  
  # ensure muslim predicts all other variables (group-aware)
  if (group_var %in% colnames(pred_matrix)) {
    pred_matrix[, group_var] <- 1  # muslim predicts everything
    pred_matrix[group_var, ] <- 0  # nothing predicts muslim
    pred_matrix[group_var, group_var] <- 0
  }
  
  # exclude weights from imputation
  pred_matrix[weight_col, ] <- 0
  pred_matrix[, weight_col] <- 0
  
  # set up methods
  imputation_method <- init_mice$method
  
  # don't impute muslim or weights
  imputation_method[group_var] <- ""
  imputation_method[weight_col] <- ""
  
  # run imputation
  mice_result <- mice(
    df_models,
    m = m,
    maxit = maxit,
    method = imputation_method,
    predictorMatrix = pred_matrix,
    seed = seed,
    printFlag = printFlag
  )
  
  return(mice_result)
}

# ============================================================================
# ANALYSIS USING ENHANCED FUNCTION
# ============================================================================

# source the enhanced analysis function
# source("margot_batch_glm_mice_enhanced.R")

# define model specification
exposure_formula <- "~ muslim + age + eth_cat + male_binary + born_nz_binary"

# specify GLM families for outcomes
family_list <- list(
  # binary outcomes use quasibinomial
  "religion_believe_god_binary" = quasibinomial(),
  "religion_believe_spirit_binary" = quasibinomial()
  # all others use gaussian (default)
)

# ============================================================================
# RUN ANALYSES BY DOMAIN
# ============================================================================

# example for religion domain:
# results_religion <- margot_batch_glm_mice_enhanced(
#   mids_obj = mids_data,
#   outcome_vars = raw_religion_outcome,
#   exposure_formula = exposure_formula,
#   terms = "muslim",
#   label_mapping = label_mapping_religion,
#   weight_col = "adjusted_weights",
#   family_list = family_list,
#   fdr_method = "bonferroni",  # correction for 39 tests
#   alpha = 0.05,
#   plot_params = list(
#     show_data = FALSE,  # don't show individual points
#     show_ci = TRUE,
#     ci_style = "errorbar"
#   )
# )

# repeat for all domains:
# - results_discrimination
# - results_health  
# - results_psychological
# - results_present_reflective
# - results_life_reflective
# - results_social

# ============================================================================
# SAMPLE SIZE REPORTING
# ============================================================================

# in the actual analysis:
# - total n = 32,202 (wave 15 with religion data)
# - muslims n = 667 (including booster sample)
# - non-muslims n = 31,535

# effective sample sizes after weighting were:
# - muslims: ~400-500 (varies by outcome due to missingness)
# - non-muslims: ~25,000-28,000

# ============================================================================
# KEY ANALYTICAL DECISIONS
# ============================================================================

# 1. post-stratification weights
#    - variable: adjusted_weights
#    - calibrated to 2018 nz census
#    - critical for population-representative estimates

# 2. multiple imputation
#    - 20 imputations
#    - group-aware: muslim as auxiliary variable
#    - predictive mean matching for continuous
#    - logistic regression for binary

# 3. model specification
#    - adjusted for: age, ethnicity, gender, nz-born
#    - survey-weighted glms using svyglm
#    - quasibinomial for binary outcomes (handles overdispersion)

# 4. multiple testing correction
#    - bonferroni correction for 39 outcomes
#    - α = 0.05 / 39 = 0.00128
#    - 99.87% confidence intervals

# 5. effect sizes
#    - marginal means comparison
#    - differences on original scale
#    - percentage differences for binary outcomes

# ============================================================================
# RESULTS AGGREGATION
# ============================================================================

# combine all results
# all_results <- list(
#   results_religion = results_religion,
#   results_discrimination = results_discrimination,
#   results_health = results_health,
#   results_psychological = results_psychological,
#   results_present_reflective = results_present_reflective,
#   results_life_reflective = results_life_reflective,
#   results_social = results_social
# )

# save for table/plot generation
# saveRDS(all_results, "output/all_results.rds")

# ============================================================================
# NOTES FOR REPRODUCIBILITY
# ============================================================================

# to reproduce this analysis:
# 1. obtain nzavs data access (https://osf.io/75snb/)
# 2. use wave 15 (2023) data with muslim booster
# 3. apply post-stratification weights
# 4. run group-aware imputation as shown
# 5. use enhanced mice function for proper inference
# 6. apply bonferroni correction consistently

# the osf code repository provides:
# - all analysis functions
# - synthetic data for testing
# - complete workflow examples
# - publication-ready output functions