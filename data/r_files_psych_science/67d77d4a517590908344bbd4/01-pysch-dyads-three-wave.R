# oct 2024 update aaron psycho dyads

# data path (local - jb only)
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
# library(boilerplate)

packageVersion(pkg = "margot")
# packageVersion(pkg = "boilerplate")

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
  data.table
  )
## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
# # nzavs data
# pull_path <-
#   fs::path_expand(
#     "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
#   )
# 
# # read data: note that you need use the arrow package in R
# dat <- arrow::read_parquet(pull_path)

# updated data
pull_path <- fs::path_expand("/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph Bulbulia/00Bulbulia Pubs/DATA/nzavs-current/r-data/nzavs_data")
dat <- qs::qread(here::here(pull_path))

str(dat)

###dat### WARNING: THIS PATH WILL NOT WORK FOR YOU. PLEASE SET A PATH TO YOUR OWN COMPUTER!! ###
push_mods <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/lmtp/24/aaron_psychopathy"
  )

# check path:is this correct?  check so you know you are not overwriting other directors
push_mods

# data table -----------------------------------------------------

library(data.table) # fast
library(psych) # measures
library(dplyr) # ensure called should be there
library(ggplot2) # graphs, should be there
library(tidyr) # for pivot_longer

# convert to data.table
dt <- as.data.table(dat)

# arrange the data by id and wave
setorder(dt, id, wave)

# rename columns to avoid conflicts
setnames(dt, 
         old = c("support_help", "self_control_have_lots", "belong_routside_reversed"),
         new = c("rel_support01", "self_control01", "rel_belong02r")
         )

# define variables for each scale
# *r* denotes requested reverse score


# define original variables for each scale
antagonism_vars_orig <- c(
  "pers_a_ipip02r",    # *r* "Am not interested in other people's problems" 
  "pers_a_ipip04r",    # *r* "Am not really interested in others"
  "pers_a_ipip01",     # *r* "Sympathise with others' feelings"
  "sdo03",             # "To get ahead in life, it is sometimes okay to step on other groups"
  "sdo02",             # "Inferior groups should stay in their place"
  "rel_support01"      # *r* "There are people I can depend on to help me if I really need it"
)

emotional_stability_vars_orig <- c(
  "pers_n_ipip02r",    # *r* "Am relaxed most of the time"
  "pers_n_ipip04r",    # *r* "Seldom feel blue"
  "vengeful_rumination01", # "Sometimes I can't sleep because of thinking about past wrongs I have suffered"
  "vengeful_rumination03", # "I find myself regularly thinking about past times that I have been wronged"
  "self_control01",    # "In general, I have a lot of self-control"
  "rel_belong02r"      # "Feel like an outsider"
)

disinhibition_vars_orig <- c(
  "pers_c_ipip03r",    # *r* "Make a mess of things"
  "pers_n_ipip01",     # "Have frequent mood swings"
  "pers_c_ipip04r",    # *r* "Often forget to put things back in their proper place"
  "pers_c_ipip01",     # *r* "Get chores done right away"
  "police_engagement02", # *r* "I would always provide information to the police to help them find someone suspected of committing a crime"
  "pers_hon_hum04r"    # *r* "Would get a lot of pleasure from owning expensive luxury goods"
)

narcissism_vars_orig <- c(
  "pers_e_ipip01",     # "Am the life of the party"
  "pers_e_ipip04",     # "Talk to a lot of different people at parties"
  "pers_modesty01r",   # *r* "I want people to know that I am an important person of high status"
  "pers_modesty04r",   # *r* "I think that I am entitled to more respect than the average person is"
  "pers_narc01r",      # *r* "Feel entitled to more of everything"
  "pers_modesty02"     # "I am an ordinary person who is no better than others"
)

# combine all original scale variables
all_scale_vars_orig <- unique(c(
  antagonism_vars_orig, 
  emotional_stability_vars_orig, 
  disinhibition_vars_orig, 
  narcissism_vars_orig
))

# define variables to reverse score
vars_to_reverse <- c(
  "pers_a_ipip02r",    # *r* "Am not interested in other people's problems"
  "pers_a_ipip04r",    # *r* "Am not really interested in others"
  "pers_a_ipip01",     # *r* "Sympathise with others' feelings" 
  "rel_support01",     # *r* "There are people I can depend on to help me if I really need it"
  "pers_n_ipip02r",    # *r* "Am relaxed most of the time"
  "pers_n_ipip04r",    # *r* "Seldom feel blue"
  "pers_c_ipip03r",    # *r* "Make a mess of things"
  "pers_c_ipip04r",    # *r* "Often forget to put things back in their proper place"
  "pers_c_ipip01",     # *r* "Get chores done right away"
  "police_engagement02", # *r* "I would always provide information to the police to help them find someone suspected of committing a crime"
  "pers_hon_hum04r",   # *r* "Would get a lot of pleasure from owning expensive luxury goods"
  "pers_modesty01r",   # *r* "I want people to know that I am an important person of high status"
  "pers_modesty04r",   # *r* "I think that I am entitled to more respect than the average person is"
  "pers_narc01r"       # *r* "Feel entitled to more of everything"
)

# create scored variables: reverse score if needed, else copy
# name them 'aaron_var'
for(var in all_scale_vars_orig){
  aaron_var <- paste0("aaron_", var)
  if(var %in% vars_to_reverse){
    dt[, (aaron_var) := 8 - get(var)]
  } else {
    dt[, (aaron_var) := get(var)]
  }
}

# define scale variables using 'aaron_' prefixed variables
antagonism_vars <- c(
  "aaron_pers_a_ipip02r",    # *r* "Am not interested in other people's problems" 
  "aaron_pers_a_ipip04r",    # *r* "Am not really interested in others"
  "aaron_pers_a_ipip01",     # *r* "Sympathise with others' feelings"
  "aaron_sdo03",             # "To get ahead in life, it is sometimes okay to step on other groups"
  "aaron_sdo02",             # "Inferior groups should stay in their place"
  "aaron_rel_support01"      # *r* "There are people I can depend on to help me if I really need it"
)

emotional_stability_vars <- c(
  "aaron_pers_n_ipip02r",    # *r* "Am relaxed most of the time"
  "aaron_pers_n_ipip04r",    # *r* "Seldom feel blue"
  "aaron_vengeful_rumination01", # "Sometimes I can't sleep because of thinking about past wrongs I have suffered"
  "aaron_vengeful_rumination03", # "I find myself regularly thinking about past times that I have been wronged"
  "aaron_self_control01",    # "In general, I have a lot of self-control"
  "aaron_rel_belong02r"      # "Feel like an outsider"
)

disinhibition_vars <- c(
  "aaron_pers_c_ipip03r",    # *r* "Make a mess of things"
  "aaron_pers_n_ipip01",     # "Have frequent mood swings"
  "aaron_pers_c_ipip04r",    # *r* "Often forget to put things back in their proper place"
  "aaron_pers_c_ipip01",     # *r* "Get chores done right away"
  "aaron_police_engagement02", # *r* "I would always provide information to the police to help them find someone suspected of committing a crime"
  "aaron_pers_hon_hum04r"    # *r* "Would get a lot of pleasure from owning expensive luxury goods"
)

narcissism_vars <- c(
  "aaron_pers_e_ipip01",     # "Am the life of the party"
  "aaron_pers_e_ipip04",     # "Talk to a lot of different people at parties"
  "aaron_pers_modesty01r",   # *r* "I want people to know that I am an important person of high status"
  "aaron_pers_modesty04r",   # *r* "I think that I am entitled to more respect than the average person is"
  "aaron_pers_narc01r",      # *r* "Feel entitled to more of everything"
  "aaron_pers_modesty02"     # "I am an ordinary person who is no better than others"
)

# combine all 'aaron_' scale variables
all_scale_vars <- unique(c(
  antagonism_vars, 
  emotional_stability_vars, 
  disinhibition_vars, 
  narcissism_vars
))

# verify all scored variables created
missing_scored_vars <- setdiff(all_scale_vars, names(dt))
if(length(missing_scored_vars) > 0){
  print(paste("missing scored variables:", paste(missing_scored_vars, collapse = ", ")))
} else {
  print("all scored variables are present")
}

# ensure all scored variables are numeric
non_numeric_scored_vars <- all_scale_vars[!sapply(all_scale_vars, function(x) is.numeric(dt[[x]]))]

# print results
if(length(non_numeric_scored_vars) > 0){
  print(paste("non-numeric scored variables:", paste(non_numeric_scored_vars, collapse = ", ")))
} else {
  print("all scored variables are numeric")
}

# compute scales using 'aaron_' prefixed 
dt[, aaron_antagonism := rowMeans(.SD, na.rm = TRUE), .SDcols = antagonism_vars]
dt[, aaron_emotional_stability := rowMeans(.SD, na.rm = TRUE), .SDcols = emotional_stability_vars]
dt[, aaron_disinhibition := rowMeans(.SD, na.rm = TRUE), .SDcols = disinhibition_vars]
dt[, aaron_narcissism := rowMeans(.SD, na.rm = TRUE), .SDcols = narcissism_vars]

# create global psychopathy score
dt[, aaron_psychopathy_combined := (aaron_antagonism + aaron_emotional_stability + 
                                      aaron_disinhibition + aaron_narcissism) / 4]

print("scales created successfully")

#  checks for original scale variables
non_numeric_vars <- all_scale_vars[!sapply(dt[, ..all_scale_vars], is.numeric)]
if(length(non_numeric_vars) > 0){
  print(paste("non-numeric variables:", paste(non_numeric_vars, collapse = ", ")))
} else {
  print("all_scale_vars are numeric")
}
# compute validation statistics using data.table
validation_stats <- dt[, .(
  antagonism_mean = mean(aaron_antagonism, na.rm = TRUE),
  antagonism_sd = sd(aaron_antagonism, na.rm = TRUE),
  antagonism_min = min(aaron_antagonism, na.rm = TRUE),
  antagonism_max = max(aaron_antagonism, na.rm = TRUE),
  antagonism_n = sum(!is.na(aaron_antagonism)),
  
  emotional_stability_mean = mean(aaron_emotional_stability, na.rm = TRUE),
  emotional_stability_sd = sd(aaron_emotional_stability, na.rm = TRUE),
  emotional_stability_min = min(aaron_emotional_stability, na.rm = TRUE),
  emotional_stability_max = max(aaron_emotional_stability, na.rm = TRUE),
  emotional_stability_n = sum(!is.na(aaron_emotional_stability)),
  
  disinhibition_mean = mean(aaron_disinhibition, na.rm = TRUE),
  disinhibition_sd = sd(aaron_disinhibition, na.rm = TRUE),
  disinhibition_min = min(aaron_disinhibition, na.rm = TRUE),
  disinhibition_max = max(aaron_disinhibition, na.rm = TRUE),
  disinhibition_n = sum(!is.na(aaron_disinhibition)),
  
  narcissism_mean = mean(aaron_narcissism, na.rm = TRUE),
  narcissism_sd = sd(aaron_narcissism, na.rm = TRUE),
  narcissism_min = min(aaron_narcissism, na.rm = TRUE),
  narcissism_max = max(aaron_narcissism, na.rm = TRUE),
  narcissism_n = sum(!is.na(aaron_narcissism)),
  
  psychopathy_combined_mean = mean(aaron_psychopathy_combined, na.rm = TRUE),
  psychopathy_combined_sd = sd(aaron_psychopathy_combined, na.rm = TRUE),
  psychopathy_combined_min = min(aaron_psychopathy_combined, na.rm = TRUE),
  psychopathy_combined_max = max(aaron_psychopathy_combined, na.rm = TRUE),
  psychopathy_combined_n = sum(!is.na(aaron_psychopathy_combined))
), by = wave]

# data.table = fast!
print(validation_stats)

# quick visualisation of distributions by wave
plot_scales <- dt %>%
  filter(wave %in% c(2018:2020)) |> # only waves used in the study
  pivot_longer(
    cols = c(aaron_antagonism, aaron_emotional_stability, 
             aaron_disinhibition, aaron_narcissism, aaron_psychopathy_combined),
    names_to = "scale",
    values_to = "score"
  ) %>%
  ggplot(aes(x = score, fill = factor(wave))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~scale, scales = "free") +
  theme_minimal() +
  labs(fill = "Wave",
       x = "Score",
       y = "Density") +
  ggtitle("Distribution of Scale Scores by Wave")

# show
print(plot_scales)


# check range of scored variables
range_scored <- dt[, lapply(.SD, range, na.rm=TRUE), .SDcols = all_scale_vars]
print(range_scored)

# check distribution of scale scores
summary_scales <- dt[, .(aaron_antagonism, aaron_emotional_stability, 
                         aaron_disinhibition, aaron_narcissism, aaron_psychopathy_combined)]
print(summary(summary_scales))

# check correlations between subscales
correlations <- cor(dt[, .(aaron_antagonism, aaron_emotional_stability, 
                           aaron_disinhibition, aaron_narcissism, aaron_psychopathy_combined)], 
                    use = "pairwise.complete.obs")
print(correlations)

# save
margot::here_save_qs(dt, "dt", push_mods)

# proceed with omega reliabilities

# define reversed variables for each scale
antagonism <-  antagonism_vars
emotional_stability <-  emotional_stability_vars
disinhibition <- disinhibition_vars
narcissism <- narcissism_vars


# do 2018 wave
dt_2018 <- dt[wave %in% "2018"]

# compute reliabilities
antagonism_reliability <- psych::omega(dt_2018[, ..antagonism])$omega.tot
emotional_stability_reliability <- psych::omega(dt_2018[, ..emotional_stability])$omega.tot
disinhibition_reliability <- psych::omega(dt_2018[, ..disinhibition])$omega.tot
narcissism_reliability <- psych::omega(dt_2018[, ..narcissism])$omega.tot



# compute total psychopathy reliability
all_vars_use<- c(antagonism, emotional_stability, 
                       disinhibition, narcissism)

total_psychopathy_reliability <- psych::omega(dt_2018[, ..all_vars_use], check.keys=TRUE)$omega.tot

# display results
cat("Reliabilities (omega):\n")
cat("Antagonism: ", round(antagonism_reliability, 2), "\n")
cat("Emotional Stability: ", round(emotional_stability_reliability, 2), "\n")
cat("Disinhibition: ", round(disinhibition_reliability, 2), "\n")
cat("Narcissism: ", round(narcissism_reliability, 2), "\n")
cat("Total Psychopathy Modified: ", round(total_psychopathy_reliability, 2), "\n")



# more tests --------------------------------------------------------------
library(psych)

# for each scale separately
scales_list <- list(
  antagonism ,
  emotional_stability,
  disinhibition,
  narcissism
)

for(scale_name in names(scales_list)) {
  vars <- scales_list[[scale_name]]
  
  # Calculate multiple reliability indices
  alpha_result <- psych::alpha(dt[, ..vars])
  
  cat("\nScale:", scale_name, "\n")
  cat("Cronbach's alpha:", round(alpha_result$total$std.alpha, 3), "\n")
  cat("Item correlations range:", 
      round(min(alpha_result$item.stats$r.drop), 3), "to",
      round(max(alpha_result$item.stats$r.drop), 3), "\n")
}

disinhibition_items <- dt_2018[, ..disinhibition]
cor_matrix <- cor(disinhibition_items, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# check for potential problematic items
item_stats <- psych::alpha(disinhibition_items)$item.stats
print(item_stats[order(item_stats$r.drop), ])
# 
# scales <- c("aaron_antagonism", "aaron_emotional_stability", 
#             "aaron_disinhibition", "aaron_narcissism")
# 
# for(scale in scales) {
#   p <- ggplot(dt, aes_string(x = scale)) +
#     geom_histogram(bins = 30) +
#     theme_minimal() +
#     ggtitle(paste("Distribution of", scale))
#   print(p)
# }
# 
# p


# these not working well ********
# c("aaron_pers_hon_hum04r", "aaron_police_engagement02") # not looking good

library(performance)
performance::item_reliability(dt[, ..antagonism])
performance::item_reliability(dt[, ..emotional_stability])
performance::item_reliability(dt[, ..disinhibition])
performance::item_reliability(dt[, ..narcissism])


# end ---------------------------------------------------------------------
# 
# # psychometric validation -------------------------------------------------
# # deep dive
# library(psych)
# library(lavaan)
# library(performance)
# library(knitr)
# library(datawizard)
# 
# to fit with earlier workflow
# dat_init_use_2 <- dt
# # version 1 Variables
# vars_version_1 <-all_vars
# 
# # version 2 Variables
# vars_subscale <- c('aaron_antagonism', 'aaron_emotional_stability', 'aaron_disinhibition', 'aaron_narcissism')
# 
# 
# # data frame
# dat_init_use_2$wave
# dat_init_use_2$rel_status_l3
# psycho_dt <- dat_init_use_2 |> filter(wave == "2018" & year_measured == 1)
# 
# hist(psycho_dt$aaron_antagonism)
# hist(psycho_dt$aaron_narcissism)
# hist(psycho_dt$aaron_emotional_stability)
# hist(psycho_dt$aaron_disinhibition)
# hist(psycho_dt$aaron_psychopathy)
# hist(psycho_dt$aaron_psychopathy_test)
# 
# 
# # extract relevant variables
# dt_antagonism <- psycho_dt |> select(all_of(antagonism_vars_reversed))
# dt_emotional_stability <- psycho_dt |> select(all_of(emotional_stability_vars_reversed))
# dt_disinhibition<- psycho_dt |> select(all_of(disinhibition_vars_reversed))
# dt_narcissism<- psycho_dt |> select(all_of(narcissism_vars_reversed))
# dt_all_vars<- psycho_dt |> select(all_of(all_vars))
# dt_subscale_vars <- psycho_dt |> select(all_of(vars_subscale))
# 
# dt_subscale_vars
# narcissism_vars_reversed
# 
# # check factor structure for each version
# performance::check_factorstructure(dt_antagonism)
# performance::check_factorstructure(dt_emotional_stability)
# performance::check_factorstructure(dt_disinhibition)
# performance::check_factorstructure(dt_narcissism)
# performance::check_factorstructure(dt_all_vars)
# performance::check_factorstructure(dt_subscale_vars)
# 
# 
# # EFA for each version
# efa_dt_antagonism <- psych::fa(dt_antagonism, nfactors = 1) %>%
#   model_parameters(sort = TRUE, threshold = "max")   
# 
# efa_dt_emotional_stability <- psych::fa(dt_emotional_stability, nfactors = 1) %>%
#   model_parameters(sort = TRUE, threshold = "max")
# 
# efa_dt_disinhibition <- psych::fa(dt_disinhibition, nfactors = 1) %>%
#   model_parameters(sort = TRUE, threshold = "max")
# 
# efa_dt_narcissism <- psych::fa(dt_narcissism, nfactors = 1) %>%
#   model_parameters(sort = TRUE, threshold = "max")
# 
# efa_dt_all_vars <- psych::fa(dt_all_vars, nfactors = 4) %>%
#   model_parameters(sort = TRUE, threshold = "max")
# efa_dt_all_vars
# 
# #efa_dt_subscale_vars <- psych::fa(dt_subscale_vars, nfactors = 3) %>%
# model_parameters(sort = TRUE, threshold = "max")
# 
# # display EFA results
# efa_dt_antagonism
# efa_dt_emotional_stability
# efa_dt_disinhibition
# efa_dt_narcissism
# 
# 
# # parallel analysis to determine the number of factors
# n_dt_antagonism <- n_factors(dt_antagonism)
# n_dt_emotional_stability <- n_factors(dt_emotional_stability)
# n_dt_disinhibition <- n_factors(dt_disinhibition)
# n_dt_narcissism <- n_factors(dt_narcissism)
# n_dt_all_vars <- n_factors(dt_all_vars)
# n_dt_subscale_vars <- n_factors(dt_subscale_vars)
# 
# 
# # plot results
# p_factors_antagonism <- plot(n_dt_antagonism) + theme_classic()
# p_factors_emotional_stability <- plot(n_dt_emotional_stability) + theme_classic()
# p_factors_disinhibition <- plot(n_dt_disinhibition) + theme_classic()
# p_factors_narcissism <- plot(n_dt_narcissism) + theme_classic()
# p_factors_antagonism + p_factors_emotional_stability + p_factors_disinhibition + p_factors_narcissism
# 
# # split data for training and testing (for CFA)
# part_dt_antagonism <- data_partition(dt_antagonism, training_proportion = .7, seed = 1234)
# part_dt_emotional_stability <- data_partition(dt_emotional_stability, training_proportion = .7, seed = 1234)
# part_dt_disinhibition<- data_partition(dt_disinhibition, training_proportion = .7, seed = 1234)
# part_dt_narcissism <- data_partition(dt_narcissism, training_proportion = .7, seed = 1234)
# # Define CFA models based on EFA results
# 
# # code for making model to do cfa
# create_lavaan_model <- function(var_list, latent_factor_name = "latent_factor") {
#   # concatenate variable names with ' + '
#   var_string <- paste(var_list, collapse = " + ")
#   
#   # construct the model string
#   model_string <- paste(latent_factor_name, "=~", var_string)
#   
#   return(model_string)
# }
# 
# 
# # create the model string
# model_string_antagonism <- create_lavaan_model(antagonism_vars_reversed)
# model_string_emotional_stability <- create_lavaan_model(emotional_stability_vars_reversed)
# model_string_disinhibition <- create_lavaan_model(disinhibition_vars_reversed)
# model_string_narcissism <- create_lavaan_model(narcissism_vars_reversed)
# model_string_all_vars <- create_lavaan_model(all_vars)
# 
# 
# # model_string_subscale <- create_lavaan_model(vars_version_2)
# 
# 
# # print the model strings
# model_string_antagonism
# model_string_emotional_stability
# model_string_disinhibition
# model_string_narcissism
# model_string_all_vars
# 
# model_string_antagonism
# model_string_emotional_stability
# model_string_disinhibition
# model_string_narcissism
# 
# # perform CFA on test data for each version
# cfa_antagonism <- suppressWarnings(lavaan::cfa(model_string_antagonism, data = part_dt_antagonism$test))
# 
# cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))
# 
# cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))
# 
# cfa_disinhibition <- suppressWarnings(lavaan::cfa(model_string_disinhibition, data = part_dt_disinhibition$test))
# 
# cfa_emotional_stability <- suppressWarnings(lavaan::cfa(model_string_emotional_stability, data = part_dt_emotional_stability$test))
# 
# cfa_narcissism  <- suppressWarnings(lavaan::cfa(model_string_narcissism, data = part_dt_narcissism $test))
# 
# 
# # summaries
# 
# summary(cfa_antagonism, fit.measures = TRUE)
# summary(cfa_emotional_stability, fit.measures = TRUE)
# summary(cfa_disinhibition, fit.measures = TRUE)
# summary(cfa_narcissism, fit.measures = TRUE)
# 
# # fit_antagonism <- fitMeasures(cfa_antagonism)
# # fit_emotional_stability <- fitMeasures(cfa_emotional_stability)
# # fit_disinhibition <- fitMeasures(cfa_disinhibition)
# # fit_narcissism <- fitMeasures(cfa_narcissism)
# 
# 
# 

# data wrangling ----------------------------------------------------------
# read data if needed i.e. if you come back to the analysis
dt <- margot::here_read_qs("dt", push_mods)


# data and wrangle-------------------------------------------------
# check 
table(dt$rel_num)
table(dt$rel_status_l3)
table(dt$rel_complete)

# views
hist(dt$aaron_antagonism)
hist(dt$aaron_emotional_stability)
hist(dt$aaron_narcissism)
hist(dt$aaron_disinhibition)
hist(dt$aaron_psychopathy_combined)

# checks
min(dt$aaron_antagonism, na.rm=TRUE)
max(dt$aaron_antagonism, na.rm=TRUE)

# checks
min(dt$aaron_emotional_stability, na.rm=TRUE)
max(dt$aaron_emotional_stability, na.rm=TRUE)

# checks
min(dt$aaron_disinhibition, na.rm=TRUE)
max(dt$aaron_disinhibition, na.rm=TRUE)

# checks
min(dt$aaron_narcissism, na.rm=TRUE)
max(dt$aaron_narcissism, na.rm=TRUE)

# checks
min(dt$aaron_psychopathy_combined, na.rm=TRUE)
max(dt$aaron_psychopathy_combined, na.rm=TRUE)

# check
skimr::n_unique(dt$id)
skimr::n_unique(dt$rel_num)

# check
table(dt$rel_status_l3)

# function to count dyads -------------------------------------------------

# using 'rel_num'
count_dyads <- function(dat, waves = c(2018), year_measured_val = 1) {
  rel_count_test <- dat %>%
    #mutate(rel_num = factor(rel_num)) |> 
    filter(wave %in% waves, year_measured == year_measured_val, !is.na(rel_num)) %>%
    group_by(rel_num) %>%
    summarise(n_in_couple = n(), .groups = 'drop') %>%
    ungroup()
  
  rel_count_1a <- rel_count_test %>% 
    filter(n_in_couple == 1)
  
  rel_count_2a <- rel_count_test %>% 
    filter(n_in_couple == 2)
  
  list(
    total_dyads = nrow(rel_count_2a) + nrow(rel_count_1a),
    singletons = nrow(rel_count_1a),
    complete_dyads = nrow(rel_count_2a),
    unique_complete_dyads = n_distinct(rel_count_2a$rel_num) # Corrected function for counting unique values
  )
}

# cound dyads
dyad_counts <- count_dyads(dat, waves = c(2018))
dyad_counts



# dyads in all waves ------------------------------------------------------
# now part of the margot package
count_dyads_multi_wave <- function(dat, start_wave = 2009, end_wave = 2022, year_measured_val = 1) {
  results <- tibble()
  
  for(wave in seq(start_wave, end_wave)) {
    dyad_counts <- count_dyads(dat, waves = c(wave), year_measured_val = year_measured_val)
    temp_tibble <- tibble(
      wave = wave,
      total_dyads = dyad_counts$total_dyads,
      singletons = dyad_counts$singletons,
      complete_dyads = dyad_counts$complete_dyads,
      unique_dyads = dyad_counts$unique_dyads
    )
    
    results <- bind_rows(results, temp_tibble)
  }
  
  return(results)
}

# wrangling ---------------------------------------------------------------
# define waves
library(dplyr)

baseline_wave <- "2018"
exposure_waves <- "2019"
outcome_wave <- "2020"

here_save(baseline_wave, "baseline_wave")
here_save(exposure_waves, "exposure_waves")
here_save(outcome_wave, "outcome_wave")


# need to load haven to change labels (!)
library(haven)
# dt_t <- dt
# process data

dt <- here_read_qs("dt")
dt_t_final <- dt %>%
  mutate(rel_status_l3 = factor(
    rel_status_l3,
    levels = c(1001, 1002, 1003, 1004, 1005, 1006, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 3001, 3002),
    labels = c(
      "Other Single incl. Casual", "Dating/Girl- or Boy- Friend", "Divorced/Separated",
      "Widow/Widower", "Separated but living together", "Single",
      "Partner/Committed/Relationship", "De-Facto", "Civil Partnership", "Engaged",
      "Married", "Polyamorous", "Open Relationship", "Not Specified", "Outside Scope"
    )
  ))

filter_statuses <- c("Other Single incl. Casual", "Single", "Divorced/Separated", "Engaged", "Widow/Widower")

# filter function
filter_relationship_status <- function(data, specified_wave) {
  data %>%
    filter(wave == specified_wave) %>%
    filter(
      !(rel_status_l3 %in% filter_statuses),
      !is.na(aaron_psychopathy_combined),
      !is.na(aaron_disinhibition),
      !is.na(aaron_narcissism)
    ) %>%
    pull(id)
}
table(dt_t_final$rel_complete)


#  id's for baseline (2018)
ids_baseline <- dt_t_final %>%
  filter(year_measured == 1, 
         wave == baseline_wave, 
         !is.na(rel_num),
         !(rel_status_l3 %in% filter_statuses),
         !is.na(aaron_psychopathy_combined),
         !is.na(aaron_disinhibition),
         !is.na(aaron_narcissism),
         rel_complete == 1) %>%
  filter_relationship_status(baseline_wave)

#  id's for exposure (2019)
ids_exposure <- dt_t_final %>%
  filter(year_measured == 1, 
         wave == exposure_waves, 
         !is.na(rel_num),
         !(rel_status_l3 %in% filter_statuses),
         !is.na(aaron_psychopathy_combined),
         !is.na(aaron_disinhibition),
         !is.na(aaron_narcissism),
         rel_complete == 1) %>% # added
  filter_relationship_status(exposure_waves)

#  final study IDs (participants in both 2018 and 2019)
ids_study <- intersect(ids_baseline, ids_exposure)

#  number of participants
cat("Number of participants in both 2018 and 2019:", length(ids_study), "\n")

# dataset for 2018 and 2019
dat_long_1819 <- dt_t_final %>%
  filter(id %in% ids_study, wave %in% c(baseline_wave, exposure_waves)) %>%
  droplevels()

# couples with exactly two individuals in 2018 and 2019
rel_count <- dat_long_1819 %>%
  filter(!is.na(rel_num)) %>%
  group_by(rel_num, wave) %>%
  summarise(n_in_wave = n(), .groups = 'drop')

couple_rel_nums <- rel_count %>%
  group_by(rel_num) %>%
  filter(all(n_in_wave == 2)) %>%
  filter(n_distinct(wave) == 2) %>%  # Ensure data for both 2018 and 2019
  ungroup() %>%
  pull(rel_num) %>%
  unique()

# create dataset with couples for 2018 and 2019
dat_couples_1819 <- dat_long_1819 %>%
  filter(rel_num %in% couple_rel_nums)

# get 2020 data for these couples (allowing for attrition)
dat_2020 <- dt_t_final %>%
  filter(id %in% unique(dat_couples_1819$id),
         wave == outcome_wave) %>%
  droplevels()

# Standardise levels for `alert_level` column
dat_2020$alert_level <- factor(
  dat_2020$alert_level,
  levels = levels(dat_couples_1819$alert_level),
  ordered = TRUE
)

# Standardise levels for `alert_level_combined` column
dat_2020$alert_level_combined <- factor(
  dat_2020$alert_level_combined,
  levels = levels(dat_couples_1819$alert_level_combined),
  ordered = TRUE
)

# Standardise levels for `alert_level_lead` column
dat_2020$alert_level_lead <- factor(
  dat_2020$alert_level_lead,
  levels = levels(dat_couples_1819$alert_level_lead),
  ordered = TRUE
)
# Standardise levels for `alert_level_lead` column
dat_2020$alert_level_combined_lead <- factor(
  dat_2020$alert_level_combined_lead,
  levels = levels(dat_couples_1819$alert_level_combined_lead),
  ordered = TRUE
)




# combine 2018-2019 couple data with 2020 data
dat_final_dyadic <- bind_rows(dat_couples_1819, dat_2020)

# checks
dt_20 <- dat_final_dyadic |> filter(wave== 2020)
table(dt_20$rel_complete)

head(dat_couples_1819$alert_level)
head(dat_2020$alert_level)


# dat_final_dyadic <- dat_final_dyadic |> droplevels()
length(unique(dat_final_dyadic$id))

# function to count participants per relationship per wave
count_participants <- function(data) {
  data %>%
    group_by(rel_num, wave) %>%
    summarise(n_participants = n_distinct(id), .groups = 'drop')
}

# count participants for each relationship in each wave
participant_counts <- count_participants(dat_final_dyadic)

# identify relationships present in 2018 and 2019
relationships_1819 <- participant_counts %>%
  filter(wave %in% c(baseline_wave, exposure_waves)) %>%
  group_by(rel_num) %>%
  filter(all(n_participants == 2)) %>%
  summarise(n_waves = n_distinct(wave)) %>%
  filter(n_waves == 2) %>%
  pull(rel_num)

# check status in 2020
status_2020 <- participant_counts %>%
  filter(wave == outcome_wave, rel_num %in% relationships_1819)

# calculate loss to follow-up (should be zero because we require follow up in 2019)
total_relationships <- length(relationships_1819)
complete_loss <- total_relationships - n_distinct(status_2020$rel_num)
partial_loss <- sum(status_2020$n_participants == 1)
no_loss <- sum(status_2020$n_participants == 2)

# print results
cat("Total dyadic relationships in 2018-2019:", total_relationships, "\n")
cat("Complete loss to follow-up in 2020:", complete_loss, "\n")
cat("Partial loss to follow-up in 2020 (one member missing):", partial_loss, "\n")
cat("No loss to follow-up in 2020:", no_loss, "\n")

# percentage calculations
cat("Percentage of complete loss:", round(complete_loss / total_relationships * 100, 2), "%\n")
cat("Percentage of partial loss:", round(partial_loss / total_relationships * 100, 2), "%\n")
cat("Percentage of no loss:", round(no_loss / total_relationships * 100, 2), "%\n")

# summary by wave
summary_by_wave <- dat_final_dyadic %>%
  group_by(wave) %>%
  summarise(
    n_relationships = n_distinct(rel_num),
    n_participants = n_distinct(id)
  )
print(summary_by_wave)

n_distinct(status_2020$rel_num)

# define exposure
nzavs_exposure <- "aaron_psychopathy_combined"
exposure_var = c("aaron_psychopathy_combined", "not_lost") #

# data
dat_final_dyadic$wave

# test
test_18 <- dat_final_dyadic |> filter(wave == 2018) |> droplevels()
sum(is.na(test_18$aaron_psychopathy_combined))

# table
table(test_18$rel_status_l3)

test_19 <- dat_final_dyadic |> filter(wave == 2019)
sum(is.na(test_19$aaron_psychopathy_combined))
table(test_19$rel_status_l3)
length(unique(dat_final_dyadic$id))

# save data 
here_save_qs(dat_final_dyadic, "dat_final_dyadic", push_mods)

# read if needed
dat_final_dyadic <- here_read_qs("dat_final_dyadic", push_mods)

length(unique(dat_final_dyadic$id))

table(dat_final_dyadic$rel_status_l3)
# check mismatches
# load required libraries
library(dplyr)
library(tidyr)

# function to find mismatched couples by wave
find_mismatched_couples <- function(data) {
  # Group by rel_num and wave, then check if rel_status_l3 is not the same for all members
  mismatched_couples <- data %>%
    group_by(rel_num, wave) %>%
    filter(n_distinct(rel_status_l3) > 1) %>%
    ungroup()
  
  return(mismatched_couples)
}

# apply function
mismatched_couples <- find_mismatched_couples(dat_final_dyadic)

# check
mismatched_couples

# order / display all results
ordered_results <- mismatched_couples %>%
  select(rel_num, id, wave, rel_status_l3) %>%
  arrange( wave,rel_num, id)

# print rows
print(ordered_results, n = Inf)

# count of mismatched couples by wave
mismatched_summary <- mismatched_couples %>%
  group_by(wave) %>%
  summarise(n_mismatched = n_distinct(rel_num))

print("Number of couples with mismatched status by wave:")
print(mismatched_summary)

# total count of unique mismatched couples across all waves
n_total_mismatched <- n_distinct(mismatched_couples$rel_num)
print(paste("Total number of unique couples with mismatched status across all waves:", n_total_mismatched))

library(dplyr)
## tests
length(unique(dat_final_dyadic$id))
dat_long <- dat_final_dyadic |> 
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    "aaron_psychopathy_combined", #aaron
    "aaron_antagonism",#aaron
    "aaron_emotional_stability",#aaron
    "aaron_disinhibition", #aaron
    "aaron_narcissism",#aaron
    "aaron_psychopathy_combined",#aaron
    "sat_relationship", # Relationship satisfication
    "sample_origin_names_combined",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    "age",
    "born_nz",
    "hlth_disability",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei13",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "religion_church",
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "urban",
    # see NZAVS,
    "has_siblings",
    # How many children have you given birth to, fathered, or adopted?
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    "hours_exercise",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    "self_esteem",
    "pwi",
    "lifesat",
    "sat_relationship",
    "conflict_in_relationship",
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    "alert_level_combined_lead",
    "rel_num",
    "rel_length_year",
    "rel_status_l3"
  ) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  ungroup() |>
  dplyr::mutate(
    household_inc_log = log(household_inc + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1)#,
    # hours_exercise_log = log(hours_exercise + 1)
  ) |>
  select(-c("household_inc", "hours_housework", "hours_work"))|>
  # dplyr::rename(sample_weights = w_gend_age_euro) |>
  # dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  mutate(
    religion_church_coarsen = cut(
      religion_church,
      breaks = c(-Inf, 0, 1, 3.99, Inf),
      labels = c("zero", "one", "less_four", "four_up"),
      include.lowest = TRUE,
      right = TRUE,
      ordered = TRUE
    )) %>%
  select(-religion_church) |> 
  mutate(
    # eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    # education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

naniar::vis_miss(dat_long, warn_large_data = TRUE)


# check sample 
n_participants <-n_unique(dat_long$id) #
n_participants

here_save(n_participants, "n_participants")


# dt_19 <- dat_long |>
#   filter(year_measured == 1 & wave == 2019) |>
#   mutate(aaron_psychopathy_combined_z = scale(aaron_psychopathy_combined))
# here_save_arrow(dt_19, "dt_19")
# 
# min_score <- min(dt_19$aaron_psychopathy_combined, na.rm = TRUE)
# min_score
# 
# max_score <- max(dt_19$aaron_psychopathy_combined, na.rm = TRUE)
# max_score
# # 
# # sd_exposure <- sd(dt_19$aaron_psychopathy_combined,
# #                   na.rm = TRUE)
# # sd_exposure
# # 
# # one_point_in_sd_units <- 1/sd_exposure
# # one_point_in_sd_units
# 
# 
# 
# # check function logic
# max_score
# max_score - 1
# min_score + 1
# min_score
# 
# # double check path
# push_mods
# 
# # check col names
# colnames(dat)




# set variables for baseline exposure and outcome -------------------------
baseline_vars = c(
  "male",
  "age",
  "rel_length_year", # added 
  "rel_status_l3",  # added
  "education_level_coarsen",
  # factors
  "eth_cat",
  "parent",
  "nz_dep2018",
  "hlth_disability",
  # "nzsei13",
  # "born_nz",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  # social belonging
  # "household_inc_log",
  # added: measured with error but OK for imputations
  # "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  "religion_church_coarsen",
  "parent",
  "hours_housework_log",
  "hours_work_log",
  "sample_weights",
  "alert_level_combined_lead",
  "rel_num"
)

# check
baseline_vars

# check
exposure_vars_all = c(
  "aaron_antagonism",
  "aaron_emotional_stability",
  "aaron_disinhibition", 
  "aaron_narcissism",
  "aaron_psychopathy_combined"
)

# outcomes
outcome_vars = c(
  "sat_relationship",
  "conflict_in_relationship",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  #  "pwb_your_health",
  # #Your health.
  #  "pwb_your_relationships",
  # #Your personal relationships.
  #  "pwb_your_future_security",
  # #Your future security.
  #  "pwb_standard_living",
  "lifesat",
  "pwi",
  "self_esteem"
)



# summary tables start  ------------------------------------------------------------------

dat_table <- dat_final_dyadic |> 
  # ungroup
  select(
    "wave",
    "year_measured",
    "id",
    "aaron_psychopathy_combined", #aaron
    "aaron_antagonism",#aaron
    "aaron_emotional_stability",#aaron
    "aaron_disinhibition", #aaron
    "aaron_narcissism",#aaron
    "aaron_psychopathy_combined",#aaron
    "sat_relationship", # Relationship satisfication
    "sample_origin_names_combined",
    # Sample origin names combined
    #"alert_level_combined_lead",  not needed because all receive all levels by the point the outcome is measured
    # covid alert levels -> 2019-2020
    "education_level_coarsen",
    # Ordinal-Rank 0-10 NZREG codes (with overseas school quals coded as Level 3, and all other ancillary categories coded as missing)  Combined highschool levels See:https://www.nzqa.govt.nz/assets/Studying-in-NZ/New-Zealand-Qualification-Framework/requirements-nzqf.pdf
    "male",
    "age",
    "born_nz",
    "hlth_disability",
    # value label 0    No 1   Yes
    "eth_cat",
    #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    "employed",
    # Are you currently employed? (this includes self-employment or casual work)
    # "gen_cohort",
    "household_inc",
    # Please estimate your total household income (before tax) for the last year.
    "nz_dep2018",
    # see nzavs materials
    "nzsei13",
    # see nzavs materials
    "partner",
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
    "religion_church",
    "political_conservative",
    #Please rate how politically liberal versus conservative you see yourself as being.
    "pol_wing",
    # Please rate how politically left-wing versus right-wing you see yourself as being.
    "urban",
    # see NZAVS,
    "have_siblings",
    # How many children have you given birth to, fathered, or adopted?
    "hours_children",
    #Hours - Looking after children
    "hours_work",
    #Hours - Working in paid employment
    "hours_housework",
    # Hours - Housework/cooking
    # "w_gend_age_euro",
    "sample_weights",
    "modesty",
    "hours_exercise",
    "kessler6_sum",
    # depression constructs,
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    # During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
    "bodysat",
    ## Am satisfied with the appearance, size and shape of my body.
    "vengeful_rumin",
    "self_esteem",
    "pwi",
    "lifesat",
    "sat_relationship",
    "conflict_in_relationship",
    "kessler_latent_anxiety",
    "kessler_latent_depression",
    "alert_level_combined_lead",
    "rel_num",
    "rel_length_year",
    "rel_status_l3"
  ) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # not_lost = ifelse(lead(year_measured)== -1, 0, not_lost,
    # not_lost = ifelse(lead(year_measured) == 0, 0, not_lost,
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  dplyr::mutate(
    # friends_money = ifelse(friends_money < 0, 0, friends_money),
    # someone gave neg number
    household_inc_log = log(household_inc + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1)#,
    # hours_exercise_log = log(hours_exercise + 1)
  ) |>
  # dplyr::rename(sample_weights = w_gend_age_euro) |>
  # dplyr::mutate(sample_origin = sample_origin_names_combined) |>  #shorter name
  arrange(id, wave) |>
  droplevels() |>
  ungroup() %>%
  #  mutate(wave = as.numeric(wave)) |>#### CHANGED
  arrange(id, wave) |>
  mutate(
    religion_church_coarsen = cut(
      religion_church,
      breaks = c(-Inf, 0, 1, 3.99, Inf),
      labels = c("zero", "one", "less_four", "four_up"),
      include.lowest = TRUE,
      right = TRUE,
      ordered = TRUE
    )) %>%
  select(-religion_church) |> 
  mutate(
    # eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    # education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame()

baseline_wave = "2018"
exposure_waves = c("2019")
outcome_wave = c("2018","2020")
# table_baseline
str(dat_table$rel_status_l3)
colnames(dat_table)
baseline_vars
basline_table_vars <- setdiff(baseline_vars, c("sample_weights", "rel_num", "alert_level_combined_lead"))

# for imputation model
here_save(basline_table_vars, "basline_table_vars")


# needed
library(haven)
# mutate haven labels
dat_table_converted <- dat_table %>%
  mutate(rel_status_l3 = as_factor(rel_status_l3)) |> 
  select(-c(sample_weights, rel_num))
basline_table_vars
dat_table_converted$rel_status_l3

# # dat_table <- margot::remove_numeric_attributes(dat_table)
# summary_tables <- margot::margot_summary_tables(
#   data = dat_table_converted,
#   baseline_wave = baseline_wave,
#   exposure_waves = exposure_waves,
#   outcome_wave = outcome_wave,
#   name_exposure = "aaron_antagonism",
#   baseline_vars = basline_table_vars,
#   outcome_vars = outcome_vars,
#   extra_vars = extra_vars,
#   create_plots = FALSE,
#   plot_type = "boxplot"
# )      

# summary_tables$outcome_table

dat_table_converted$rel_status_l3
dat_single_dyads <- dat_table_converted |> filter(rel_status_l3 == "Single")
dat_single_dyads


exposure_vars_all = c(
  "antagonism",
  "emotional_stability",
  "disinhibition", 
  "narcissism",
  "psychopathy_combined"
)


library(margot)

# baseline_vars_use <- setdiff(baseline_vars, c("rel_num", "sample_weights"))


dat_table_converted <- dat_table_converted |> 
  rename(antagonism = aaron_antagonism,
         emotional_stability = aaron_emotional_stability,
         disinhibition = aaron_disinhibition, 
         narcissism = aaron_narcissism, 
         psychopathy_combined = aaron_psychopathy_combined)



# graphs for exposures ----------------------------------------------------
# general histogram
# another approach (more robust)
histogram_exposure_by_wave <- margot_plot_histogram(
  data = dat_table_converted,
  col_names = exposure_vars_all,
  id_col = "id",
  waves = c(2018),
  binwidth = 0.5,
  facet_scales = "free",
  color_palette = "gray",
  mean_line_color = "darkred", #
  sd_line_color = "darkred",
  add_timestamp = FALSE,
  save_path = here::here(push_mods),
  vertical_facets = TRUE
)

# view
histogram_exposure_by_wave


histogram_exposure_by_wave <- margot_plot_histogram(
  data = dat_table_converted,
  col_names = exposure_vars_all,
  id_col = "id",
  waves = c(2018),
  binwidth = 0.5,
  facet_scales = "free",
  color_palette = "gray",
  mean_line_color = "darkred", #
  sd_line_color = "darkred",
  add_timestamp = FALSE,
  save_path = here::here(push_mods),
  vertical_facets = TRUE
)

margot_save_png(histogram_exposure_by_wave, base_filename = "histogram_exposures", save_path = here::here(push_mods),
                width = 12, height = 18)

# end general graphs ---------------------------------------------

# new graphs ---------------------------------------------------------
# evaluate exposure variable/s
dat_long_exposure <- dat_table_converted |>
  filter(wave == exposure_waves) |>
  droplevels()

head(dat_long_exposure)


# check
exposure_var

# you can check quantile breaks this way
# ********* CHECK ************** #
result <- lapply(exposure_vars_all, function(var) {
  quantiles <- quantile(dat_long_exposure[[var]], na.rm = TRUE, probs = seq(0, 1, .25))
  setNames(list(quantiles), var)
})

print(result)


# binary_exposure <- here_read_qs("binary_categorical_political_conservative", push_mods)



# for manuscript
# plot_histogram_binary_exposure <- here_read_qs('binary_categorical_trust_science_high_confidence_scientific_community', push_mods)

exposure_vars_all


# antagonism --------------------------------------------------------------


# save plot
shift_exposure_up_antagonism <- margot_plot_shift(
  dat_long_exposure,
  col_name = "antagonism",
  shift = "up",
  range_highlight = c(6.1, 7),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)
shift_exposure_up_antagonism

# view
shift_exposure_up_antagonism

# shift_exposure_up <- here_read_qs('shift_political_conservative_up', push_mods)


shift_exposure_down_antagonism <- margot_plot_shift(
  dat_long_exposure,
  col_name = "antagonism",
  shift = "down",
  range_highlight = c(1, 1.9),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)

shift_exposure_down_antagonism




# shift disinhibition ------------------------------------------------------
shift_exposure_up_disinhibition <- margot_plot_shift(
  dat_long_exposure,
  col_name = "disinhibition",
  shift = "up",
  range_highlight = c(6.1, 7),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)
shift_exposure_up_disinhibition



# shift_exposure_up <- here_read_qs('shift_political_conservative_up', push_mods)


shift_exposure_down_disinhibition <- margot_plot_shift(
  dat_long_exposure,
  col_name = "disinhibition",
  shift = "down",
  range_highlight = c(1, 1.9),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)

shift_exposure_down_disinhibition



# shift emotional stability -----------------------------------------------

# save plot
shift_exposure_up_emotional_stability <- margot_plot_shift(
  dat_long_exposure,
  col_name = "emotional_stability",
  shift = "up",
  range_highlight = c(6.1, 7),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)
shift_exposure_up_emotional_stability



# shift_exposure_up <- here_read_qs('shift_political_conservative_up', push_mods)


shift_exposure_down_emotional_stability <- margot_plot_shift(
  dat_long_exposure,
  col_name = "emotional_stability",
  shift = "down",
  range_highlight = c(1, 1.9),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)

shift_exposure_down_emotional_stability





# shift narcissism --------------------------------------------------------
# save plot
shift_exposure_up_narcissism <- margot_plot_shift(
  dat_long_exposure,
  col_name = "narcissism",
  shift = "up",
  range_highlight = c(6.1, 7),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)
shift_exposure_up_narcissism



# shift_exposure_up <- here_read_qs('shift_political_conservative_up', push_mods)


shift_exposure_down_narcissism <- margot_plot_shift(
  dat_long_exposure,
  col_name = "narcissism",
  shift = "down",
  range_highlight = c(1, 1.9),
  binwidth = .25,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)

shift_exposure_down_narcissism


# shift psychopathy_combined ----------------------------------------------
shift_exposure_up_psychopathy_combined <- margot_plot_shift(
  dat_long_exposure,
  col_name = "psychopathy_combined",
  shift = "up",
  range_highlight = c(6.05, 7),
  binwidth = .1,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)
shift_exposure_up_psychopathy_combined



shift_exposure_down_psychopathy_combined <- margot_plot_shift(
  dat_long_exposure,
  col_name = "psychopathy_combined",
  shift = "down",
  range_highlight = c(1, 1.91),
  binwidth = .1,
  save_path = here::here(push_mods),
  show_avg_line = TRUE
)

shift_exposure_down_psychopathy_combined

# summary tables ----------------------------------------------------------

# check
baseline_vars
basline_table_vars

summary_tables <- margot_summary_tables(
  data = dat_table_converted,
  baseline_wave = baseline_wave,
  exposure_waves = exposure_waves,
  outcome_wave = outcome_wave,
  name_exposure = exposure_vars_all,
  baseline_vars = basline_table_vars,
  outcome_vars = outcome_vars,
  extra_vars = extra_vars,
  create_plots = FALSE,
  plot_type = "boxplot"
)               


summary_tables$exposure_tables$antagonism
summary_tables$baseline_table
margot::here_save_qs(summary_tables, "summary_tables", push_mods)

summary_tables$baseline_table

# tables end  ------------------------------------------------------------------

# transition matrices -----------------------------------------------------
# create transition matrix ------------------------------------------------
dt_positivity <- dat_table_converted |>
  filter(wave == baseline_wave | wave %in% exposure_waves) |>
  select(all_of(exposure_vars_all), id, wave) |>
  droplevels() |>
  # use across to mutate over all exposure variables and round to nearest whole number
  mutate(across(all_of(exposure_vars_all), ~ round(as.numeric(.))))
# check
head(dt_positivity)

# to batch process
library(purrr)

# function to create transition matrices
create_transition_matrix_for_var <- function(var_name) {
  # create transition matrix data
  transition_matrix_data <- margot::create_transition_matrix(data = dt_positivity, state_var = var_name, id_var = "id")
  
  # create transition table
  transition_matrix <- margot::transition_table(transition_matrix_data)
  
  # return the transition matrix
  return(transition_matrix)
}


# map: batch process for all exposure variables
transition_matrices <- map(exposure_vars_all, create_transition_matrix_for_var)

transition_matrices$antagonism

# name the list elements with the variable names
names(transition_matrices) <- exposure_vars_all

# check the structure of the first matrix
str(transition_matrices[[1]])

# save 
here_save(transition_matrices, "transition_matrices")



# save data for next step in processing -----------------------------------


# prepare data for imputation an wide format in script 2 -----------------------


# custom function

dat_long <- as.data.frame(dat_long)
dat_long <- haven::zap_formats(dat_long)
dat_long <- haven::zap_label(dat_long)
dat_long <- haven::zap_widths(dat_long)
here_save_qs(dat_long, "dat_long", push_mods)


# baseline vars with binary names
baseline_vars_binary = c(
  "male_binary",
  "age",
  "rel_length_year", # added 
  "education_level_coarsen",
  # factors
  "eth_cat",
  "parent_binary",
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  # "sample_origin",
  "nz_dep2018",
  "hlth_disability_binary",
  # "nzsei13",
  # "born_nz",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  # social belonging
  # "household_inc_log",
  # added: measured with error but OK for imputations
  # "partner",
  # "parent",  # newly changed - have information in child number
  "political_conservative",
  # "religion_church_coarsen_n",
  "religion_church_coarsen",
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban_binary",
  "hours_housework_log",
  "hours_work_log",
  # "sample_weights",
  "alert_level_combined_lead",
  "rel_num")
baseline_vars_binary <- sort(baseline_vars_binary)
baseline_vars_binary
here_save(baseline_vars_binary, "baseline_vars_binary")
baseline_vars_binary

# make binary vars
dat_long_prepare<- margot::margot_process_binary_vars( dat_long, exceptions = "not_lost" )
sort(colnames( dat_long_prepare))
here_save_qs(dat_long_prepare, "dat_long_prepare", push_mods)

# define extra vars for data wrangling later
extra_vars <- c("id", "wave", "year_measured", "not_lost") # gender_weights not used/ just an option

# save
here_save(extra_vars, "extra_vars")

# define all vars
all_vars <- c(baseline_vars_binary, exposure_var, outcome_vars, extra_vars)

# save
here_save(all_vars, "all_vars")

# define columns that will later be handled in a special way
# define continuous columns that we will not standardise
continuous_columns_keep <- c("t0_rel_num", "t0_alert_level_combined_lead", "t0_religion_church_coarsen")
here_save(continuous_columns_keep, "continuous_columns_keep")

here_save(basline_table_vars, "basline_table_vars")
basline_table_vars

# define ordinal columns that we will expand into binary variables
ordinal_columns <- c("t0_education_level_coarsen", "t0_eth_cat")

margot::here_save(ordinal_columns, "ordinal_columns")

# name exposure
exposure_var_all <- c("aaron_antagonism",  # only for tables
                      "aaron_disinhibition",
                      "aaron_emotional_stability",
                      "aaron_narcissism",
                      "aaron_psychopathy_combined") # redundant
# save
here_save(exposure_var_all, "exposure_var_all")

# names of exposures
t0_name_exposure <- paste0("t0_", exposure_var_all)
t1_name_exposure <- paste0("t1_", exposure_var_all)

t0_name_exposure_partner <- paste0("t0_partner_", exposure_var_all)
t1_name_exposure_partner <- paste0("t1_partner_", exposure_var_all)
# save names
here_save(t0_name_exposure , 't0_name_exposure')
here_save(t1_name_exposure , 't1_name_exposure')

here_save(t0_name_exposure_partner , 't0_name_exposure_partner')
here_save(t1_name_exposure_partner , 't1_name_exposure_partner')
t1_name_exposure_partner

# put in alphabetical order
outcome_vars <- sort(outcome_vars)

# save
here_save(outcome_vars, "outcome_vars")

# save weights NO WEIGHTS USED IN THIS STUDY
here_save_qs(dat_long_prepare, "dat_long_prepare", push_mods)








