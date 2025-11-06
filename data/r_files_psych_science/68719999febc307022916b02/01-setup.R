# 01-setup.R
# setup script for muslim diversity study (they are us) analysis
# this script loads required packages, sets parameters, and defines variables
# **author**: Joseph Bulbulia
# **contact**: joseph.bulbulia@vuw.ac.nz
# **date**: 2024-JUL-12

# set seed for reproducibility
set.seed(123)

# load required packages
library(mice)         # multiple imputation
library(survey)       # survey-weighted analysis
library(ggeffects)    # marginal effects
library(ggplot2)      # plotting
library(dplyr)        # data manipulation
library(tidyr)        # data reshaping
library(patchwork)    # combining plots
library(flextable)    # publication tables
library(here)         # project paths

# check package versions
required_versions <- list(
  mice = "3.0.0",
  ggeffects = "2.3.0",  # critical for empirical marginalization
  survey = "4.0"
)

for (pkg in names(required_versions)) {
  if (packageVersion(pkg) < required_versions[[pkg]]) {
    warning(paste0("Package ", pkg, " version ", packageVersion(pkg), 
                   " is older than recommended ", required_versions[[pkg]]))
  }
}

# source helper functions
# if project_root is defined (from run_analysis_example.R), use it
# otherwise try here::here() for when this is sourced independently
if (exists("project_root")) {
  source(file.path(project_root, "functions", "helper_functions.R"))
  source(file.path(project_root, "functions", "margot_poststrat_contrast_mi.R"))
} else {
  source(here::here("functions", "helper_functions.R"))
  source(here::here("functions", "margot_poststrat_contrast_mi.R"))
}

# define analysis parameters
ANALYSIS_PARAMS <- list(
  # imputation settings
  n_imputations = 20,
  imputation_method = "pmm",  # predictive mean matching
  
  # model settings
  exposure_var = "muslim",
  covariates = c("age", "eth_cat", "male_binary", "born_nz_binary"),
  weight_col = "adjusted_weights",
  
  # multiple testing correction
  fdr_method = "bonferroni",
  alpha = 0.05,
  
  # plotting defaults
  plot_params = list(
    show_data = TRUE,
    jitter = 0.4,
    dot_alpha = 0.1,
    show_ci = TRUE,
    ci_style = "errorbar",
    dot_size = 2,
    colors = NULL
  ),
  
  # output settings
  digits = 2,
  save_plots = TRUE,
  plot_width = 16,
  plot_height = 9
)

# define outcome variables by domain
OUTCOME_DOMAINS <- list(
  # religion outcomes
  religion = c(
    "religion_believe_god_binary",
    "religion_believe_spirit_binary",
    "religion_church",
    "religion_identification_level",
    "religion_prayer",
    "religion_scripture",
    "religion_spiritual_identification"
  ),
  
  # discrimination outcomes
  discrimination = c(
    "perc_age_discrim",
    "perc_discrim",
    "perc_gend_discrim",
    "perc_religious_discrim"
  ),
  
  # health outcomes
  health = c(
    "hlth_sleep_hours",
    "log_hours_exercise",
    "short_form_health"
  ),
  
  # psychological outcomes
  psychological = c(
    "hlth_fatigue",
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    "rumination"
  ),
  
  # present-reflective outcomes
  present_reflective = c(
    "forgiveness",
    "perfectionism",
    "self_esteem",
    "pwb_your_future_security",
    "pwb_standard_living",
    "pwb_your_health",
    "pwb_your_relationships"
  ),
  
  # life-reflective outcomes
  life_reflective = c(
    "gratitude",
    "lifesat",
    "meaning_purpose",
    "meaning_sense"
  ),
  
  # social outcomes
  social = c(
    "belong_accept",
    "belong_beliefs",
    "belong_outsider_reversed",
    "neighbourhood_community",
    "support_help",
    "support_noguidance_reversed",
    "support_turnto"
  )
)

# define glm families for specific outcomes
OUTCOME_FAMILIES <- list(
  # binary outcomes use quasibinomial
  religion_believe_god_binary = quasibinomial(),
  religion_believe_spirit_binary = quasibinomial()
  # all others use gaussian (default)
)

# create exposure formula
EXPOSURE_FORMULA <- paste("~", ANALYSIS_PARAMS$exposure_var, "+", 
                         paste(ANALYSIS_PARAMS$covariates, collapse = " + "))

# create label mapping for nice display
LABEL_MAPPING <- create_plot_name_mapping()

# print setup summary
cat("=== Muslim Diversity Study Analysis Setup ===\n")
cat("Exposure variable:", ANALYSIS_PARAMS$exposure_var, "\n")
cat("Covariates:", paste(ANALYSIS_PARAMS$covariates, collapse = ", "), "\n")
cat("Weight variable:", ANALYSIS_PARAMS$weight_col, "\n")
cat("Number of imputations:", ANALYSIS_PARAMS$n_imputations, "\n")
cat("FDR method:", ANALYSIS_PARAMS$fdr_method, "\n")
cat("Alpha level:", ANALYSIS_PARAMS$alpha, "\n")
cat("\nOutcome domains:\n")
for (domain in names(OUTCOME_DOMAINS)) {
  cat(sprintf("  %s: %d outcomes\n", domain, length(OUTCOME_DOMAINS[[domain]])))
}
cat("\nTotal outcomes:", sum(sapply(OUTCOME_DOMAINS, length)), "\n")
cat("==========================================\n")

