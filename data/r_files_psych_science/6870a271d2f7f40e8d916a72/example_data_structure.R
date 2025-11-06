# example_data_structure.R
# this file documents the expected data structure for the muslim diversity study analysis
# actual participant data is not included for privacy reasons

# ============================================================================
# EXPECTED DATA STRUCTURE
# ============================================================================

# the analysis expects a 'mids' object from the mice package
# this object should contain multiply imputed data with the following variables:

# EXPOSURE VARIABLE
# muslim: binary indicator (0 = non-muslim, 1 = muslim)
#   - this variable should NOT be imputed
#   - used as auxiliary variable in group-aware imputation

# COVARIATES (adjusted for in all models)
# age: continuous, age in years
# eth_cat: categorical ethnicity variable
#   - levels typically include: "euro", "maori", "pacific", "asian", "other"
# male_binary: binary indicator (0 = not male, 1 = male)
# born_nz_binary: binary indicator (0 = not born in nz, 1 = born in nz)

# WEIGHT VARIABLE
# adjusted_weights: post-stratification weights
#   - benchmarked to 2018 nz census
#   - accounts for age × gender × ethnicity

# OUTCOME VARIABLES BY DOMAIN

# religion outcomes (7 variables)
# - religion_believe_god_binary: belief in god (0/1)
# - religion_believe_spirit_binary: belief in spirit/life force (0/1)
# - religion_church: monthly religious service attendance (count)
# - religion_identification_level: religious identification (1-7 scale)
# - religion_prayer: weekly prayer frequency (count)
# - religion_scripture: weekly scripture reading (count)
# - religion_spiritual_identification: spiritual identification (1-7 scale)

# discrimination outcomes (4 variables)
# - perc_age_discrim: perceived age discrimination (1-7 scale)
# - perc_discrim: perceived ethnic discrimination (1-7 scale)
# - perc_gend_discrim: perceived gender discrimination (1-7 scale)
# - perc_religious_discrim: perceived religious discrimination (1-7 scale)

# health outcomes (3 variables)
# - hlth_sleep_hours: hours of sleep per night
# - log_hours_exercise: log-transformed hours of exercise per week
# - short_form_health: self-rated health (1-7 scale)

# psychological outcomes (4 variables)
# - hlth_fatigue: fatigue symptoms (1-5 scale)
# - kessler_latent_anxiety: anxiety symptoms (latent score)
# - kessler_latent_depression: depression symptoms (latent score)
# - rumination: rumination scale (1-7 scale)

# present-reflective outcomes (7 variables)
# - forgiveness: forgiveness scale (1-7 scale)
# - perfectionism: perfectionism scale (1-7 scale)
# - self_esteem: self-esteem scale (1-7 scale)
# - pwb_your_future_security: personal wellbeing - future security (0-10 scale)
# - pwb_standard_living: personal wellbeing - standard of living (0-10 scale)
# - pwb_your_health: personal wellbeing - health satisfaction (0-10 scale)
# - pwb_your_relationships: personal wellbeing - relationships (0-10 scale)

# life-reflective outcomes (4 variables)
# - gratitude: gratitude scale (1-7 scale)
# - lifesat: life satisfaction scale (1-7 scale)
# - meaning_purpose: meaning and purpose scale (1-7 scale)
# - meaning_sense: sense of meaning scale (1-7 scale)

# social outcomes (7 variables)
# - belong_accept: belonging - acceptance (1-7 scale)
# - belong_beliefs: belonging - shared beliefs (1-7 scale)
# - belong_outsider_reversed: belonging - not an outsider (1-7 scale, reversed)
# - neighbourhood_community: neighbourhood community connection (1-7 scale)
# - support_help: social support - help available (1-7 scale)
# - support_noguidance_reversed: social support - guidance available (1-7 scale, reversed)
# - support_turnto: social support - someone to turn to (1-7 scale)

# ============================================================================
# EXAMPLE CODE TO CREATE COMPATIBLE DATA
# ============================================================================

# this example shows how to structure your data for the analysis
# replace with your actual data loading and preparation code

create_example_data_structure <- function(n = 1000) {
  
  # create example data frame
  example_data <- data.frame(
    # id
    id = 1:n,
    
    # exposure
    muslim = sample(0:1, n, replace = TRUE, prob = c(0.98, 0.02)),
    
    # covariates
    age = rnorm(n, mean = 45, sd = 15),
    eth_cat = sample(c("euro", "maori", "pacific", "asian", "other"), 
                     n, replace = TRUE, prob = c(0.7, 0.15, 0.05, 0.08, 0.02)),
    male_binary = sample(0:1, n, replace = TRUE),
    born_nz_binary = sample(0:1, n, replace = TRUE, prob = c(0.2, 0.8)),
    
    # weight
    adjusted_weights = runif(n, 0.5, 2.0),
    
    # religion outcomes (7 variables)
    religion_believe_god_binary = sample(0:1, n, replace = TRUE),
    religion_believe_spirit_binary = sample(0:1, n, replace = TRUE),
    religion_church = rpois(n, lambda = 1),
    religion_identification_level = sample(1:7, n, replace = TRUE),
    religion_prayer = rpois(n, lambda = 3),
    religion_scripture = rpois(n, lambda = 1),
    religion_spiritual_identification = sample(1:7, n, replace = TRUE),
    
    # discrimination outcomes (4 variables)
    perc_age_discrim = sample(1:7, n, replace = TRUE),
    perc_discrim = sample(1:7, n, replace = TRUE),
    perc_gend_discrim = sample(1:7, n, replace = TRUE),
    perc_religious_discrim = sample(1:7, n, replace = TRUE),
    
    # health outcomes (3 variables)
    hlth_sleep_hours = rnorm(n, mean = 7, sd = 1.5),
    log_hours_exercise = log(rgamma(n, shape = 2, rate = 1) + 0.1),
    short_form_health = sample(1:7, n, replace = TRUE),
    
    # psychological outcomes (4 variables)
    hlth_fatigue = sample(1:5, n, replace = TRUE),
    kessler_latent_anxiety = rnorm(n, mean = 0, sd = 1),
    kessler_latent_depression = rnorm(n, mean = 0, sd = 1),
    rumination = sample(1:7, n, replace = TRUE),
    
    # present-reflective outcomes (7 variables)
    forgiveness = sample(1:7, n, replace = TRUE),
    perfectionism = sample(1:7, n, replace = TRUE),
    self_esteem = sample(1:7, n, replace = TRUE),
    pwb_your_future_security = sample(0:10, n, replace = TRUE),
    pwb_standard_living = sample(0:10, n, replace = TRUE),
    pwb_your_health = sample(0:10, n, replace = TRUE),
    pwb_your_relationships = sample(0:10, n, replace = TRUE),
    
    # life-reflective outcomes (4 variables)
    gratitude = sample(1:7, n, replace = TRUE),
    lifesat = sample(1:7, n, replace = TRUE),
    meaning_purpose = sample(1:7, n, replace = TRUE),
    meaning_sense = sample(1:7, n, replace = TRUE),
    
    # social outcomes (7 variables)
    belong_accept = sample(1:7, n, replace = TRUE),
    belong_beliefs = sample(1:7, n, replace = TRUE),
    belong_outsider_reversed = sample(1:7, n, replace = TRUE),
    neighbourhood_community = sample(1:7, n, replace = TRUE),
    support_help = sample(1:7, n, replace = TRUE),
    support_noguidance_reversed = sample(1:7, n, replace = TRUE),
    support_turnto = sample(1:7, n, replace = TRUE),
    
    stringsAsFactors = FALSE
  )
  
  # introduce some missing data (except for muslim and id)
  vars_to_miss <- setdiff(names(example_data), c("id", "muslim"))
  for (var in vars_to_miss) {
    miss_idx <- sample(1:n, size = floor(n * 0.05))  # 5% missing
    example_data[miss_idx, var] <- NA
  }
  
  return(example_data)
}

# ============================================================================
# GROUP-AWARE IMPUTATION SETUP
# ============================================================================

# example of how to set up group-aware imputation
setup_group_aware_imputation <- function(data) {
  
  library(mice)
  
  # create predictor matrix
  pred_matrix <- quickpred(data, exclude = c("id"))
  
  # ensure muslim predicts all other variables
  pred_matrix[, "muslim"] <- 1
  
  # ensure muslim is never imputed
  pred_matrix["muslim", ] <- 0
  
  # set up imputation methods
  imp_methods <- make.method(data)
  imp_methods["muslim"] <- ""  # don't impute muslim
  
  # for binary variables, ensure they are factors for logreg
  binary_vars <- c("religion_believe_god_binary", "religion_believe_spirit_binary",
                   "male_binary", "born_nz_binary")
  
  # convert binary variables to factors if they aren't already
  for (var in binary_vars) {
    if (var %in% names(data) && !is.factor(data[[var]])) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  
  imp_methods[binary_vars] <- "logreg"
  
  # for categorical variables
  imp_methods["eth_cat"] <- "polyreg"
  
  # for continuous variables, use pmm (default)
  
  return(list(
    predictorMatrix = pred_matrix,
    method = imp_methods
  ))
}

# ============================================================================
# EXAMPLE IMPUTATION WORKFLOW
# ============================================================================

# # 1. load your data
# raw_data <- read.csv("your_data.csv")  # replace with actual data loading
# 
# # 2. set up group-aware imputation
# imp_setup <- setup_group_aware_imputation(raw_data)
# 
# # 3. run mice with group-aware settings
# mids_data <- mice(
#   raw_data,
#   m = 20,  # 20 imputations
#   predictorMatrix = imp_setup$predictorMatrix,
#   method = imp_setup$method,
#   seed = 123,
#   printFlag = TRUE
# )
# 
# # 4. check convergence
# plot(mids_data)
# 
# # 5. save for analysis
# saveRDS(mids_data, "imputed_data.rds")