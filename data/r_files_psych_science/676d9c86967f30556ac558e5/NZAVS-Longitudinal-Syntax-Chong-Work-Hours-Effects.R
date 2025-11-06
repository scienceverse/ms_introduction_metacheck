# Bella work hour 27 June 2024
# by joseph.bulbulia@vuw.ac.nz

# The Causal Effects of Work on Multi-Dimensional Well-Being: Evidence From A National Longitudinal Panel Study in New Zealand
# notes
# this script is from a previous workflow.  The margot_plot() function in the margot package introduced breaking changes to this workflow. We have replaced `margot_plot()` with a function called `old_margot_plot()` which will generate the graphs. This function is in the current directory.
# The data are part of the New Zealand and Attitudes and Values Study. They are not supplied for participant privacy, but may be requested to reproduce this analysis from joseph.bulbulia@vuw.ac.nz or Chris G Sibley (lead NZAVS researcher) or anyone on the NZAVS board.
# note that much of the commented out code belongs to workflows we used at the time of this study. we have kept it for archival purposes, but it is irrelevant to this study, (e.g. the coding of religious service attendance.)
# finally the code in this old workflow is tedious. Updates to the margot package have streamlined it, to avoid repetition.


# reproducibility
set.seed(123)

# set path to save models
push_mods <-
  here::here('/Users/joseph/Library/CloudStorage/Dropbox-v-project/data/24-bella-work')

# load packages
# get devtools
if (!require(devtools, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(devtools, quietly = TRUE)) {
  devtools::install_github("go-bayes/margot")
  library(devtools)
}

# check if pacman is installed; if not, install it
if (!require(pacman, quietly = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

# use p_load to load / install the packages
pacman::p_load(
  skimr,
  naniar,
  WeightIt,
  clarify,
  MatchThem,
  cobalt,
  MatchIt,
  kableExtra,
  janitor,
  lmtp,
  SuperLearner,
  ranger,
  margot,
  xgboost,
  glmnet,
  doParallel,
  ggplot2,
  here,
  naniar,
  gtsummary,
  grf,
  gt,
  progressr,
  tidyverse,
  ggplot2,
  parameters,
  kableExtra,
  xgboost,
  glmnet,
  ranger,
  future
)

# wrangling ---------------------------------------------------------------
pull_path <-
  fs::path_expand(
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data_qs"
  )

# read data: note that you need use the arrow package in R
dat <- qs::qread(here::here(pull_path))

# total nzavs participants
n_total <- skimr::n_unique(dat$id)

# get comma in number
n_total <- margot::pretty_number(n_total)

# check
n_total

# save for manuscript
margot::here_save(n_total, "n_total")

# eliminate haven labels
dat <- as.data.frame(dat)
dat <- haven::zap_formats(dat)
dat <- haven::zap_label(dat)
dat <- haven::zap_widths(dat)

# name of exposure
name_exposure <-  "hours_work" # EXPOSURE VARIABLE

# get names
colnames(dat)

# define data ------------------------------------------------------------
baseline_wave <- "2020"
exposure_wave <- "2021"
outcome_wave <- "2022"

# obtain ids for individuals who participated in baseline and have no missing baseline exposure
# not retired
ids_baseline <- dat |>
  dplyr::filter(year_measured == 1, wave == baseline_wave) |>
  dplyr::filter(!is.na(!!sym(name_exposure))) |> # criteria, no missing in baseline exposure
  dplyr::filter(!is.na(employed)) |> # criteria, no missing in baseline exposure
  pull(id)
! is_na(employed)
length(ids_baseline)

# general code for selection with two waves: not used in this study
# obtain ids for individuals who participated in exposure
# ids_exposure <- dat |>
#   dplyr::filter(
#     year_measured == 1,
#     wave == exposure_wave) |>
#   dplyr::filter(!is.na(!!sym(name_exposure))) |>
#   pull(id)# criteria, no missing in baseline exposure
#
# # intersect IDs from baseline and exposure to ensure participation in both years
# ids_study <- intersect(ids_baseline, ids_exposure)
# length( ids_study )


# checks -----------------------------------------------------------------
df_all_in <- dat |>
  filter(year_measured == 1)

# check column names for covariates
sort(colnames(dat))


# prepare data -----------------------------------------------------------
dat_long_0 <- dat |>
  dplyr::filter(id %in% ids_baseline &
                  wave %in% c(baseline_wave, exposure_wave, outcome_wave)) |>
  arrange(id, wave) |>
  dplyr::select(
    "id",
    "wave",
    "age",
    "male",
    "w_gend_age_euro",
    "year_measured",
    "sample_origin_names_combined",
    "sample_frame_opt_in",
    "hlth_disability",
    "born_nz",
    "male",
    "eth_cat",
    "employed",
    #
    "hours_commute",
    "education_level_coarsen",
    "household_inc",
    "nz_dep2018",
    "nzsei_13_l",
    "partner",
    "parent",
    "political_conservative",
    "hours_children",
    "hours_work",
    "hours_housework",
    "hours_charity",
    "hours_exercise",
    "religion_identification_level",
    "religion_believe_spirit",
    "religion_believe_god",
    "charity_donate",
    "agreeableness",
    "conscientiousness",
    "extraversion",
    "honesty_humility",
    "openness",
    "neuroticism",
    "rural_gch_2018_l",
    "charity_donate",
    "hours_charity",
    "alcohol_frequency",
    "alcohol_intensity",
    "smoker",
    "hlth_bmi",
    "alcohol_frequency",
    "alcohol_intensity",
    "hlth_fatigue",
    "rumination",
    "kessler_latent_depression",
    "kessler_latent_anxiety",
    "sexual_satisfaction",
    "hlth_sleep_hours",
    "hours_exercise",
    "sfhealth",
    "hlth_sleep_hours",
    "bodysat",
    "perfectionism",
    "self_esteem",
    "self_control_have_lots",
    "self_control_wish_more_reversed",
    "vengeful_rumin",
    "gratitude",
    "pwb_your_health",
    "pwb_your_relationships",
    "pwb_your_future_security",
    "pwb_standard_living",
    "lifesat",
    "meaning_purpose",
    "meaning_sense",
    "neighbourhood_community",
    "support",
    "belong"
  )

# re name indicators
dat_long <- dat_long_0 |>
  rename(short_form_health = sfhealth) |> # better name
  mutate(male = as.numeric(male)) |>
  mutate(
    #    religion_church_binary = ifelse(religion_church > 0, 1, 0), # not in these data :(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  dplyr::rename(sample_weights = w_gend_age_euro) |>
  # note used in this study
  arrange(id, wave) |>
  #   mutate(
  #   religion_church_coarsen = cut(
  #     religion_church,
  #     breaks = c(-Inf, 0, 1, 3.99, Inf),
  #     labels = c("zero", "one", "less_four", "four_up"),
  #     include.lowest = TRUE,
  #     right = TRUE,
  #     ordered = TRUE
  #   )
  # ) |>
  arrange(id, wave) |>
  # note used in this study
  # mutate(religion_church_round = round(ifelse(religion_church >= 8, 8, religion_church), 0)) |>
  #  mutate(hours_community_round = round(ifelse(hours_community >= 24, 24, hours_community), 0)) |>
  mutate(
    #initialize 'not_lost'
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    # modify 'not_lost' based on the condition; no need to check for NA here as 'not_lost' is already defined in the previous step
    not_lost =  ifelse(is.na(not_lost) &
                         year_measured == 1, 1, not_lost)
  ) |>
  select(-year_measured) |>
  ungroup() |>
  dplyr::mutate(
    hours_housework_log = log(hours_housework + 1),
    household_inc_log = log(household_inc + 1),
    hours_charity_log = log(hours_charity + 1),
    hours_exercise_log = log(hours_exercise + 1),
    hours_children_log = log(hours_children + 1),
    charity_donate_log = log(charity_donate + 1),
    hours_commute_log = log(hours_commute + 1)
  ) |>
  dplyr::select(
    -c(
      charity_donate,
      hours_housework,
      household_inc,
      hours_exercise,
      hours_commute,
      hours_children,
      hours_charity,
      charity_donate
    )
  ) |>
  droplevels() |>
  dplyr::rename(sample_origin =  sample_origin_names_combined) |>
  dplyr::mutate(
    parent = as.numeric(as.character(parent)),
    partner = as.numeric(as.character(partner)),
    born_nz = as.numeric(as.character(born_nz)),
    not_lost = as.numeric(as.character(not_lost)),
    employed = as.numeric(as.character(employed)),
    hlth_disability = as.numeric(as.character(hlth_disability))
  ) |>
  droplevels() |>
  arrange(id, wave) |>
  data.frame() |>
  droplevels()

# check missing
vis_miss(dat_long, warn_large_data = F)

# check # need to fix employment variable
table1::table1( ~ employed | wave, data = dat_long)

# n participants
n_participants <- skimr::n_unique(dat_long$id)

# get comma in number
n_participants <- margot::pretty_number(n_participants)

# check
n_participants

# save number for manuscript
margot::here_save(n_participants, "n_participants")

# inspect data
skimr::skim(dat_long)

# check names
sort(colnames(dat_long))

# set baseline variables --------------------------------------------------
# for confounding control
baseline_vars = c(
  "male",
  "age",
  "employed",
  "sample_frame_opt_in",
  "education_level_coarsen",
  "eth_cat",
  "sample_origin",
  "nz_dep2018",
  "nzsei_13_l",
  "rural_gch_2018_l",
  "born_nz",
  "hlth_disability",
  "short_form_health",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "support",
  "belong",
  "household_inc_log",
  "partner",
  "parent",
  "political_conservative",
  "hours_children_log",
  "religion_identification_level",
  "agreeableness",
  "conscientiousness",
  "extraversion",
  "honesty_humility",
  "openness",
  "neuroticism",
  "charity_donate_log",
  "hours_charity_log",
  "hours_housework_log",
  "hours_exercise_log",
  "hours_commute_log",
  "sample_weights"
)
baseline_vars <- sort(baseline_vars)

#exposure variable
exposure_var = c(name_exposure, "not_lost") # we will use the not_lost variable later

# outcomes
outcome_vars = c(
  "alcohol_frequency",
  "alcohol_intensity",
  "hlth_bmi",
  "hours_exercise_log",
  "short_form_health",
  "hlth_sleep_hours",
  "smoker",
  "hlth_fatigue",
  "rumination",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "bodysat",
  "perfectionism",
  "household_inc_log",
  "self_esteem",
  "sexual_satisfaction",
  "self_control_have_lots",
  "self_control_wish_more_reversed",
  "vengeful_rumin",
  "gratitude",
  "short_form_health",
  "pwb_your_health",
  "pwb_your_relationships",
  "pwb_your_future_security",
  "pwb_standard_living",
  "lifesat",
  "meaning_purpose",
  "meaning_sense",
  "neighbourhood_community",
  "belong",
  "support",
  "household_inc_log"
)
outcome_vars <- sort(outcome_vars)

# visualise missing
naniar::vis_miss(dat_long, warn_large_data = F)

# make tables --------------------------------------------------------
dt_baseline <- dat_long |>
  filter(wave == baseline_wave)

# variables for table
base_vars <- setdiff(baseline_vars, c("not_lost", "sample_weights", outcome_vars))

# check
colnames(dt_baseline)

# get baseline variables
selected_base_cols <-
  dt_baseline |> select(all_of(base_vars))

# missing values visualisation
viss_miss_baseline <- naniar::vis_miss(dt_baseline, warn_large_data = F)

#view
viss_miss_baseline

# save
here_save(viss_miss_baseline, "base_var")

# prepare table
selected_base_cols <- selected_base_cols |>
  select(sort(names(selected_base_cols)))

# the setdiff command allows us to remove names from the baseline vars list that we do not
table_baseline <- selected_base_cols |>
  janitor::clean_names(case = "title") |>
  tbl_summary(
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")
    ),
    type = list(all_continuous() ~ "continuous2")
  ) |>
  modify_header(label = "**Exposure + Demographic Variables**") |>
  bold_labels()

# baseline
table_baseline

# make tables -------------------------------------------------------------
summary_tables_bella <- margot::margot_summary_tables(
  data = dat_long,
  name_exposure = name_exposure,
  baseline_wave = baseline_wave,
  exposure_waves = exposure_wave,
  outcome_wave = outcome_wave,
  baseline_vars = baseline_vars,
  outcome_vars = outcome_vars,
  extra_vars = "sample_weights",
  create_plots = FALSE
)

# save baseline table
here_save_qs(summary_tables_bella, "summary_tables_bella", push_mods) # save your baseline table)

# view statistics e.g.
summary_tables_bella$baseline_table
summary_tables_bella$exposure_tables$hours_work
summary_tables_bella$outcome_table

# save your tables
here_save(summary_tables_bella, "summary_tables_bella", push_mods)


# order names correctly
selected_outcome_cols <- selected_outcome_cols |>
  select(sort(names(selected_outcome_cols)))


# histogram of the exposure -----------------------------------------------
# select exposure wave
dt_exposure_wave  <- dat_long |> dplyr::filter(wave == exposure_wave)

# mean of exposure
mean_exposure <- mean(dt_exposure_wave[[name_exposure]], na.rm = TRUE)

# view
mean_exposure

# save
here_save(mean_exposure, "mean_exposure")

# sd of exposure
sd_exposure <- sd(dt_exposure_wave[[name_exposure]], na.rm = TRUE)
# check
sd_exposure
# save
here_save(sd_exposure, "sd_exposure")

sd_exposure <- here_read("sd_exposure")

# check
sd_exposure

# median
median_exposure <- median(dt_exposure_wave[[name_exposure]], na.rm = TRUE)

# get missingness at baseline
dat_long_baseline <- dat_long |> 
  filter(wave == baseline_wave)

dat_long_baseline <-naniar::pct_miss(dat_long_baseline)
naniar::gg_miss_var(dat_long_baseline)
  dat_long_baseline

# change in exposure ------------------------------------------------------
# check to verify positivity
dt_positivity <- dat_long |>
  dplyr::filter(wave == baseline_wave |
                  wave == exposure_wave) |>
  mutate(working = ifelse(hours_work > 0, 1, 0)) |>   # EXPOSURE VARIABLE
  dplyr::select(working, id, wave) |>
  droplevels()


# make 
dt_positivity_2 <- dat_long |>
  dplyr::select(hours_work, id, wave) |>
  dplyr::filter(hours_work <= 120) |> 
  dplyr::mutate(hours_work_units_ten = round(hours_work / 20) * 20) |> 
  droplevels()


# make matrix for those working
out <- margot::create_transition_matrix(data = dt_positivity_2,
                                        state_var = "hours_work_units_ten",
                                        id_var = "id")

# make table transition table
transition_table  <- margot::transition_table(out)

transition_table$table

# save
margot::here_save_qs(transition_table, "transition_table", push_mods)

# view
print(transition_table$table)
print(transition_table$explanation)


# compute how many work zero hours. ---------------------------------------
dat_long_exposure <- dat_long |> 
  filter(wave == exposure_wave) |> 
  select(hours_work) |> 
  drop_na()


nrowdat_long_exposure<- nrow(dat_long_exposure)
nrowdat_long_exposure
proportion_zero_hours <- round( sum(dat_long_exposure$hours_work == 0) / nrowdat_long_exposure, 3 )
proportion_zero_hours
here_save(proportion_zero_hours, "proportion_zero_hours")

# example of a cross sectional analysis -----------------------------------

# impute missing values at baseline ---------------------------------------
exposure_vars <- c(exposure_var, "not_lost")
baseline_vars <- setdiff(baseline_vars, "sample_weights")

# check
baseline_vars


# prepare for imputation --------------------------------
dat_long <- margot::remove_numeric_attributes(dat_long)

# check
str(dat_long)

# here we impute the baseline
df_impute_base <- margot::margot_wide_impute_baseline(
  dat_long,
  baseline_vars = baseline_vars,
  exposure_var = exposure_vars,
  outcome_vars = outcome_vars
)

# get sample weights back to data
dt_baseline <- dat_long |> filter(wave == baseline_wave)

# add sample weights
df_impute_base$t0_sample_weights = dt_baseline$sample_weights

table(is.na(df_impute_base$t0_sample_weights))

# make factor for modesl
df_impute_base$t0_rural_gch_2018_l <- as.factor(df_impute_base$t0_rural_gch_2018_l)

# save
margot::here_save(df_impute_base, "df_impute_base")


# data wrangling for censoring weights ------------------------------------
df_impute_base <- margot::here_read("df_impute_base")

# check data types
str(df_impute_base)

df_wide_not_lost <- df_impute_base |>
  relocate("t0_not_lost", .before = starts_with("t1_")) |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))


# save
here_save(df_wide_not_lost, "df_wide_not_lost")


# read if needed
df_wide_not_lost <- here_read("df_wide_not_lost")

# check missing values
naniar::vis_miss(df_wide_not_lost, warn_large_data = FALSE)

# check those lost
table(df_wide_not_lost$t0_not_lost)

# compute the conditions before the mutate steps
t0_na_condition <- rowSums(is.na(select(df_wide_not_lost, starts_with("t1_")))) > 0


# used if you did not censor at exposure wave
#t1_na_condition <- rowSums(is.na(select(df_wide_not_lost, starts_with("t2_")))) > 0

df_clean <- df_wide_not_lost |>
  dplyr::mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) |>
  dplyr::mutate(t0_lost = 1 - t0_not_lost) |>
  # not needed in this study
  # mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) |>. # use if not_lost at t1
  # mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
  #        across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) |>
  # mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .)))|>
  # select variables
  dplyr::mutate(
    across(
      .cols = where(is.numeric) &
        !t0_not_lost &
        !t0_lost &
        !t0_smoker &
        !t0_sample_weights &
        !t0_hours_work & # exposure var at baseline!t0_sample_origin  &
        !t0_rural_gch_2018_l  &
        !t1_hours_work &  # exposure var !t1_not_lost &
        !t2_smoker,
      .fns = ~ scale(.),
      .names = "{.col}_z"
    )
  ) |>
  select(
    where(is.factor),
    t0_not_lost,
    t0_lost,
    t0_smoker,
    t0_sample_weights,
    t0_sample_origin ,
    t0_rural_gch_2018_l  ,
    t0_hours_work,
    # exposure var at baseline
    t0_not_lost,
    t1_hours_work,
    # exposure var
    t1_not_lost,
    t2_smoker,
    ends_with("_z")
  ) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check
colnames(df_clean)

# get rid of attributes
df_clean <- margot::remove_numeric_attributes(df_clean)
# check again
naniar::vis_miss(df_clean, warn_large_data = FALSE)

# checks
table(df_clean$t0_not_lost)
table(df_clean$t1_not_lost)

# checks
table(df_clean$t0_not_lost)

# checks
test <- df_wide_not_lost |> filter(t0_not_lost == 1)
nrow(test)

# weights for treatment ----------------------------------------------------
baseline_vars_models = df_clean |>  # post process of impute and combine
  dplyr::select(starts_with("t0_"), -t0_not_lost, -t0_sample_weights) |> colnames()

# create fresh dataset
df_clean_pre <- df_clean[baseline_vars_models]

# checks
str(df_clean_pre)

# perform one-hot encoding using model.matrix
# we need factors to be 0 or 1
encoded_vars <- model.matrix(
  ~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l  - 1,
  data = df_clean_pre
)

# check
head(encoded_vars)

# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# bind the new one-hot encoded variables back to the original dataframe
# ensure to remove original categorical variables to avoid duplication
df_clean_hot <- df_clean |>
  select(-c(
    t0_eth_cat,
    t0_education_level_coarsen,
    t0_sample_origin,
    t0_rural_gch_2018_l
  )) |>
  bind_cols(encoded_df)

# extract and print the new column names for encoded variables
new_encoded_colnames <- colnames(encoded_df)
print(new_encoded_colnames)


# get baseline variable set without factors
baseline_vars_set <- setdiff(
  names(df_clean_pre),
  c(
    "t0_lost",
    "id",
    "t0_eth_cat",
    "t0_education_level_coarsen",
    "t0_sample_origin",
    "t0_rural_gch_2018_l"
  )
)

# check
baseline_vars_set

# add the new encoded column names
full_predictor_vars <- c(baseline_vars_set, new_encoded_colnames)

# check
full_predictor_vars

# check
str(df_clean_hot)

# set up super learner

# predict prob of drop out using learners
library(SuperLearner)

# library for multicore processing
library(doParallel)

# learners
listWrappers()

# set up superlearner
cv_control <- list(V = 10, stratifyCV = TRUE)  # 10-fold CV with stratification


# Set up parallel back end
no_cores <- detectCores()
cl <- makeCluster(no_cores - 1)
registerDoParallel(cl)


# stacked learners
match_lib = c("SL.glmnet", "SL.xgboost", "SL.ranger")

# run super learner
sl <- SuperLearner(
  Y = df_clean_hot$t0_lost,
  X = df_clean_hot[full_predictor_vars],
  # use specified predictors
  SL.library = match_lib,
  family = binomial(),
  method = "method.NNloglik",
  cvControl = list(V = 10)
)

# stop the cluster
stopCluster(cl)

# save your super learner model
here_save(sl, "sl")

# read
sl <- here_read("sl")

# summary of the SuperLearner output
print(sl)

# detailed summary, including cross-validated risks
summary(sl)

# examination of cross-validated performance
# cross-validated risks for each learner
sl$cvRisk

# weights assigned to each learner in the final ensemble
sl$coef

# generate predictions
predictions <- predict(sl, newdata = df_clean_hot[full_predictor_vars], type = "response")

# extract predictions from the 'pred' component and ensure it's a vector
df_clean_hot$pscore <- predictions$pred[, 1]

# check the structure of the predictions
str(df_clean_hot$pscore)

# check pscore
hist(df_clean_hot$pscore)

# make censoring weights
df_clean_hot$weights <- ifelse(df_clean_hot$t0_lost == 1,
                               1 / df_clean_hot$pscore,
                               1 / (1 - df_clean_hot$pscore))

# check
hist(df_clean_hot$weights)# nothing extreme

# obtain stablise weights
# marginal_not_lost <- mean(df_clean_hot$t0_lost)
# save output of hot code dataset

# save data
here_save(df_clean_hot, "df_clean_hot")

# import if you are picking up the analysis later
df_clean_hot <- here_read("df_clean_hot")

# compute new weights by combining censor and sample weights, using stabalised weights
df_clean$t0_combo_weights = df_clean_hot$weights * df_clean$t0_sample_weights

# checks
min(df_clean$t0_combo_weights)
max(df_clean$t0_combo_weights)

# check distrobution of weights
hist(df_clean$t0_combo_weights)

# next remove those who were lost between t0 and t1
df_clean_t1 <- df_clean |> filter(t0_lost == 0) |>
  select(-t0_lost, -t0_sample_weights) |>
  relocate("t0_combo_weights", .before = starts_with("t1_"))

# check
hist(df_clean_t1$t0_combo_weights)

# checks
max(df_clean_t1$t0_combo_weights)
min(df_clean_t1$t0_combo_weights)

# number of weighted sample at t1, again check
n_not_lost_sample <- nrow(df_clean_t1)
n_not_lost_sample <- prettyNum(n_not_lost_sample, big.mark = ",")

# save output for manuscript
here_save(n_not_lost_sample, "n_not_lost_sample")

# check
n_not_lost_sample

# no one missing in exposure
# check
table(is.na(df_clean_t1$t1_hours_work)) # none
table((df_clean_t1$t1_not_lost)) # none

# check column order and missing ness
naniar::vis_miss(df_clean_t1, warn_large_data = FALSE)

#check
nrow(df_clean_t1)

# next get data for t1
hist(df_clean_t1$t0_combo_weights)

# save data
margot::here_save(df_clean_t1, "df_clean_t1")

# pick up here if returning to anlysis
df_clean_t1 <- margot::here_read("df_clean_t1")

# get correct censoring -----------------------------------------
# na condition at baseline
t0_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t1_")))) > 0

# na condition at exposure
t1_na_condition <-
  rowSums(is.na(select(df_clean_t1, starts_with("t2_")))) > 0

df_clean_t2 <- df_clean_t1 %>%
  mutate(t0_not_lost = ifelse(t0_na_condition, 0, t0_not_lost)) %>%
  mutate(t1_not_lost = ifelse(t1_na_condition, 0, t1_not_lost)) %>%
  mutate(across(starts_with("t1_"), ~ ifelse(t0_not_lost == 0, NA_real_, .)),
         across(starts_with("t2_"), ~ ifelse(t0_not_lost == 0, NA_real_, .))) %>%
  mutate(across(starts_with("t2_"), ~ ifelse(t1_not_lost == 0, NA_real_, .))) |>
  mutate(t1_lost = 1 - t1_not_lost) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_")) |>
  select(-t1_lost, -t0_not_lost)

# test
nrow(df_clean_t2)
colnames(df_clean_t2)

# checks
hist(df_clean_t2$t0_combo_weights)

# check outcomes
naniar::vis_miss(df_clean_t2, warn_large_data = F)

# save
here_save(df_clean_t2, "df_clean_t2")


# analysis ----------------------------------------------------------------
# read data --  start here if previous work already done
df_clean_t2 <- here_read("df_clean_t2")

# check
colnames(df_clean_t2)

# check
str(df_clean_t2)

# names of vars for modelling
names_base <-
  df_clean_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
names_base

# get outcome names for checks
names_outcomes <-
  df_clean_t2 |> select(starts_with("t2")) |> colnames()

# check
names_outcomes

# obsessively check
names(df_clean_t2)

# check against this
names_base

df_final_base  <- df_clean_t2[names_base]


# one hot encode any categorical vars, here only t0_eth_cat
# this code is the same as above
encoded_vars <- model.matrix(
  ~ t0_education_level_coarsen + t0_eth_cat + t0_sample_origin + t0_rural_gch_2018_l - 1,
  data = df_clean_t2
)

# view
head(encoded_vars)

# convert matrix to data frame
encoded_df <- as.data.frame(encoded_vars)

# make better names
encoded_df <- encoded_df |>
  janitor::clean_names()

# view the first few rows to confirm structure
head(encoded_df)

# ensure to remove original categorical variables to avoid duplication
# note new data `df_clean_t2`
df_clean_hot_t2 <- df_clean_t2 |>
  select(-c(
    t0_education_level_coarsen,
    t0_eth_cat,
    t0_sample_origin,
    t0_rural_gch_2018_l
  )) |>
  bind_cols(encoded_df) |>
  relocate(starts_with("t0_"), .before = starts_with("t1_")) |>
  #  relocate("t0_not_lost", .before = starts_with("t1_"))  |>
  relocate("t1_not_lost", .before = starts_with("t2_"))

# check names
colnames(df_clean_hot_t2)

# # extract and print the new column names for encoded variables
new_encoded_colnames_t2 <- colnames(encoded_df)
print(new_encoded_colnames_t2)

# get baseline variable set without factors
baseline_vars_set_t2 <- setdiff(names(df_clean_hot_t2), c("id", "t0_eth_cat"))
set_final_names <- c(baseline_vars_set_t2, new_encoded_colnames_t2)

# set names for analysis
set_final_names <-
  df_clean_hot_t2 |> select(starts_with("t0"), -t0_combo_weights) |> colnames()

# check
set_final_names

# add the new encoded column names

# check
colnames(df_clean_hot_t2)



# model estimation --------------------------------------------------------
# estimate models ---------------------------------------------------------
library(lmtp)
library(SuperLearner)
library(xgboost)
library(ranger)
library(future)

# progress bars
progressr::handlers(global = TRUE)

# set seed for reproducing results
plan(multisession)

# set cores
n_cores <- parallel::detectCores() - 2

#  model variable names
A <- c("t1_hours_work")  # exposure
C <- c("t1_not_lost") # censored
W <- set_final_names # baseline covariates

# make data_final
df_final <- df_clean_hot_t2

# last checks
colnames(df_final)
naniar::vis_miss(df_final, warn_large_data = F)

# save
here_save(df_final, "df_final")
here_save(W, "W")


# models start here -------------------------------------------------------

# read coveriate names
W <- margot::here_read("W")

# read final data
df_final <- margot::here_read("df_final")


# shift functions :

# shift work up  1 sd (20.2 hours) to max of 80.
gain_A <- function(data, trt) {
  ifelse(data[[trt]]  + sd_exposure < 80, data[[trt]] + sd_exposure, 80)
  
}

# shift work down by 1 sd (20.2 hours) to min of 0.
loss_A <- function(data, trt) {
  ifelse(data[[trt]]  - sd_exposure > 0, data[[trt]] - sd_exposure, 0)
}

# set learners
sl_lib <- c("SL.glmnet", "SL.ranger", "SL.xgboost") #

# smaller subset for test
sl_lib_test <-  c("SL.glmnet", "SL.ranger") #

df_clean_slice <- df_final |>
  slice_head(n = 500) |>
  as.data.frame()


sd_exposure <- here_read("sd_exposure")

fit_test <- margot_lmtp(
  data = df_clean_slice,
  outcome_vars = outcome_vars,
  trt = A,
  shift_functions = list(gain = gain_A, loss = loss_A),
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = 'pairwise',
  contrast_scale = 'additive',
  lmtp_defaults = list(
    baseline = baseline_names,
    mtp = TRUE,
    folds = 10,
    outcome_type = "continuous",
    cens = "t1_not_lost",
    weights = df_clean_slice$t0_combo_weights,
    learners_trt = "SL.ranger",
    learners_outcome = "SL.ranger"
  )
)

# functions
shift_functions <- list(gain = gain_A, loss = loss_A)

lmtp_defaults = list(
  baseline = baseline_names,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger"
)

# shift function
gain_A <- function(data, trt) {
  ifelse(data[[trt]]  + sd_exposure < 80, data[[trt]] + sd_exposure, 80)
  
}

loss_A <- function(data, trt) {
  ifelse(data[[trt]]  - sd_exposure > 0, data[[trt]] - sd_exposure, 0)
}


A = "t1_hours_work"

outcome_vars <- c("t2_kessler_latent_anxiety_z", "t2_alcohol_frequency_z")

fit_test <- margot_lmtp(
  data = df_clean_slice,
  outcome_vars = outcome_vars,
  trt = A,
  shift_functions = list(gain = gain_A, loss = loss_A),
  include_null_shift = TRUE,
  lmtp_model_type = lmtp::lmtp_tmle,
  contrast_type = 'pairwise',
  contrast_scale = 'additive',
  lmtp_defaults = list(
    baseline = baseline_names,
    cens = "t1_not_lost",
    # Change this to a character vector of censoring variable names if you have any
    mtp = TRUE,
    folds = 10,
    outcome_type = "continuous",
    weights = df_clean_slice$t0_combo_weights,
    learners_trt = "SL.ranger",
    learners_outcome = "SL.ranger"
  )
)


# trim baseline co-variates -- including all in all models tends to be over-kill
W_1 <- margot::select_and_rename_cols(
  W,
  baseline_vars,
  outcome = 't2_kessler_latent_anxiety_z',
  from_prefix = "t2",
  to_prefix = "t0"
)
W_1

t2_kessler_latent_anxiety_z_null_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = NULL,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = "SL.ranger",
  learners_outcome = "SL.ranger"
)

# test gain
t2_kessler_latent_anxiety_z_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = loss_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view

# view learner performance
t2_kessler_latent_anxiety_z_test$fits_r
t2_kessler_latent_anxiety_z_test$fits_m

# save
here_save(t2_kessler_latent_anxiety_z_null_test,
          "t2_kessler_latent_anxiety_z_null_test")

t2_kessler_latent_anxiety_z_lose_test <- lmtp_tmle(
  outcome = "t2_kessler_latent_anxiety_z",
  baseline = W,
  shift = loss_A,
  data = df_clean_slice,
  trt = A,
  cens = C,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_clean_slice$t0_combo_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)
# save test
here_save(t2_kessler_latent_anxiety_z_lose_test,
          "t2_kessler_latent_anxiety_z_lose_test")

# test contrast
test_contrast_anxiety <- lmtp::lmtp_contrast(t2_kessler_latent_anxiety_z_lose_test , ref = t2_kessler_latent_anxiety_z_null_test)

# check
test_contrast_anxiety


# tests look good, let's run the model ------------------------------------
# models ------------------------------------------------------------------
# health models -----------------------------------------------------------
outcome_vars

# simplify covariates but ensure that outcome is measured at baseline
W_t2_smoker <- select_and_rename_cols(names_base = W,
                                      baseline_vars = baseline_vars,
                                      outcome = "t2_smoker")
# check
W_t2_smoker

# model
t2_smoker_binary <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# result
t2_smoker_binary

# save results
here_save(t2_smoker_binary, "t2_smoker_binary")

# model on diff scale
t2_smoker_binary_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_smoker_binary_1
here_save(t2_smoker_binary_1, "t2_smoker_binary_1")

# control
t2_smoker_binary_null  <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_smoker",
  cens = C,
  shift = NULL,
  mtp = FALSE,
  folds = 10,
  outcome_type = "binomial",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

t2_smoker_binary_null
here_save(t2_smoker_binary_null, "t2_smoker_binary_null")


# alcohol freq model
t2_alcohol_frequency_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view
t2_alcohol_frequency_z
here_save(t2_alcohol_frequency_z, "t2_alcohol_frequency_z")


#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view
t2_alcohol_frequency_z_1
# save
here_save(t2_alcohol_frequency_z_1, "t2_alcohol_frequency_z_1")

#"How often do you have a drink containing alcohol?"
t2_alcohol_frequency_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_frequency_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_alcohol_frequency_z_null

# save
here_save(t2_alcohol_frequency_z_null, "t2_alcohol_frequency_z_null")


# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_alcohol_intensity_z

# save
here_save(t2_alcohol_intensity_z, "t2_alcohol_intensity_z")

# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_alcohol_intensity_z_1

#save
here_save(t2_alcohol_intensity_z_1, "t2_alcohol_intensity_z_1")


# How many drinks containing alcohol do you have on a typical day when drinking?
t2_alcohol_intensity_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_alcohol_intensity_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_alcohol_intensity_z_null
# save
here_save(t2_alcohol_intensity_z_null, "t2_alcohol_intensity_z_null")



# "In general, would you say your health is...
# "I seem to get sick a little easier than other people."
# "I expect my health to get worse." ****
# "In general, would you say your health is...

t2_short_form_health_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_short_form_health_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)

# view
t2_short_form_health_z

# save
here_save(t2_short_form_health_z, "t2_short_form_health_z")


# "In general, would you say your health is...
t2_short_form_health_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_short_form_health_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_short_form_health_z_1

# save
here_save(t2_short_form_health_z_1, "t2_short_form_health_z_1")

# model
t2_short_form_health_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_short_form_health_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_short_form_health_z_null

# save
here_save(t2_short_form_health_z_null, "t2_short_form_health_z_null")

# Hours spent … exercising/physical activity
t2_hours_exercise_log_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hours_exercise_log_z

# save
here_save(t2_hours_exercise_log_z, "t2_hours_exercise_log_z")

# Hours spent … exercising/physical activity
t2_hours_exercise_log_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hours_exercise_log_z_1

# save
here_save(t2_hours_exercise_log_z_1, "t2_hours_exercise_log_z_1")

# Hours spent … exercising/physical activity
t2_hours_exercise_log_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hours_exercise_log_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hours_exercise_log_z_null

# save
here_save(t2_hours_exercise_log_z_null,
          "t2_hours_exercise_log_z_null")


#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hlth_sleep_hours_z

# save
here_save(t2_hlth_sleep_hours_z, "t2_hlth_sleep_hours_z")



#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hlth_sleep_hours_z_1

# save
here_save(t2_hlth_sleep_hours_z_1, "t2_hlth_sleep_hours_z_1")


#During the past month, on average, how many hours of actual sleep did you get per night?
t2_hlth_sleep_hours_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_sleep_hours_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hlth_sleep_hours_z_null

# save
here_save(t2_hlth_sleep_hours_z_null, "t2_hlth_sleep_hours_z_null")


# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view
t2_hlth_bmi_z

# save
here_save(t2_hlth_bmi_z, "t2_hlth_bmi_z")


# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_hlth_bmi_z_1
here_save(t2_hlth_bmi_z_1, "t2_hlth_bmi_z_1")



# " What is your height? (metres)\nWhat is your weight? (kg)\nKg
t2_hlth_bmi_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_bmi_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_hlth_bmi_z_null
here_save(t2_hlth_bmi_z_null, "t2_hlth_bmi_z_null")



# embodied models ----------------------------------------------------------------

## Am satisfied with the appearance, size and shape of my body.

t2_bodysat_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_bodysat_z
here_save(t2_bodysat_z, "t2_bodysat_z")


## Am satisfied with the appearance, size and shape of my body.
t2_bodysat_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_bodysat_z_1
here_save(t2_bodysat_z_1, "t2_bodysat_z_1")


## Am satisfied with the appearance, size and shape of my body.
t2_bodysat_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_bodysat_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_bodysat_z_null
here_save(t2_bodysat_z_null, "t2_bodysat_z_null")



# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel that everything was an effort?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel nervous?
# During the last 30 days, how often did.... you feel restless or fidgety?
# During the last 30 days, how often did.... you feel worthless?

t2_kessler_latent_anxiety_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_kessler_latent_anxiety_z
here_save(t2_kessler_latent_anxiety_z, "t2_kessler_latent_anxiety_z")




t2_kessler_latent_anxiety_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W_t2_kessler_latent_anxiety_z,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_kessler_latent_anxiety_z_1
here_save(t2_kessler_latent_anxiety_z_1,
          "t2_kessler_latent_anxiety_z_1")

t2_kessler_latent_anxiety_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_anxiety_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
here_save(t2_kessler_latent_anxiety_z_null,
          "t2_kessler_latent_anxiety_z_null")


# depression
# During the last 30 days, how often did.... you feel so depressed that nothing could cheer you up?
# During the last 30 days, how often did.... you feel hopeless?
# During the last 30 days, how often did.... you feel worthless?

t2_kessler_latent_depression_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_kessler_latent_depression_z
here_save(t2_kessler_latent_depression_z,
          "t2_kessler_latent_depression_z")




t2_kessler_latent_depression_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_kessler_latent_depression_z_1
here_save(t2_kessler_latent_depression_z_1,
          "t2_kessler_latent_depression_z_1")


t2_kessler_latent_depression_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_kessler_latent_depression_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_kessler_latent_depression_z_null
here_save(t2_kessler_latent_depression_z_null,
          "t2_kessler_latent_depression_z_null")


# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_hlth_fatigue_z
here_save(t2_hlth_fatigue_z, "t2_hlth_fatigue_z")

# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_hlth_fatigue_z_1
here_save(t2_hlth_fatigue_z_1, "t2_hlth_fatigue_z_1")




# During the last 30 days, how often did.... you feel exhausted?
t2_hlth_fatigue_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_hlth_fatigue_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_hlth_fatigue_z_null
here_save(t2_hlth_fatigue_z_null, "t2_hlth_fatigue_z_null")

# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
)
# view and save
t2_rumination_z
here_save(t2_rumination_z, "t2_rumination_z")


# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_rumination_z_1
here_save(t2_rumination_z_1, "t2_rumination_z_1")




# During the last 30 days, how often did.... you have negative thoughts that repeated over and over?
t2_rumination_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_rumination_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_rumination_z_null
here_save(t2_rumination_z_null, "t2_rumination_z_null")




# ego models --------------------------------------------------------------
#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_sexual_satisfaction_z
here_save(t2_sexual_satisfaction_z, "t2_sexual_satisfaction_z")


#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_sexual_satisfaction_z_1
here_save(t2_sexual_satisfaction_z_1, "t2_sexual_satisfaction_z_1")

#  How satisfied are you with your sex life?
t2_sexual_satisfaction_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_sexual_satisfaction_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_sexual_satisfaction_z_null
here_save(t2_sexual_satisfaction_z_null,
          "t2_sexual_satisfaction_z_null")

#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_esteem_z
here_save(t2_self_esteem_z, "t2_self_esteem_z")



t2_self_esteem_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_esteem_z_1
here_save(t2_self_esteem_z_1, "t2_self_esteem_z_1")




#  On the whole am satisfied with myself.
#  Take a positive attitude toward myself
#  Am inclined to feel that I am a failure.
t2_self_esteem_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_esteem_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_esteem_z_null
here_save(t2_self_esteem_z_null, "t2_self_esteem_z_null")

# In general, I have a lot of self-control.
t2_self_control_have_lots_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_have_lots_z
here_save(t2_self_control_have_lots_z, "t2_self_control_have_lots_z")


t2_self_control_have_lots_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_have_lots_z_1
here_save(t2_self_control_have_lots_z_1,
          "t2_self_control_have_lots_z_1")




#In general, I have a lot of self-control.
t2_self_control_have_lots_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_have_lots_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_have_lots_z_null
here_save(t2_self_control_have_lots_z_null,
          "t2_self_control_have_lots_z_null")

# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_wish_more_reversed_z
here_save(t2_self_control_wish_more_reversed_z,
          "t2_self_control_wish_more_reversed_z")



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_wish_more_reversed_z_1
here_save(
  t2_self_control_wish_more_reversed_z_1,
  "t2_self_control_wish_more_reversed_z_1"
)



# I wish I had more self-discipline.(r)
t2_self_control_wish_more_reversed_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_self_control_wish_more_reversed_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_self_control_wish_more_reversed_z_null
here_save(
  t2_self_control_wish_more_reversed_z_null,
  "t2_self_control_wish_more_reversed_z_null"
)


# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_perfectionism_z
here_save(t2_perfectionism_z, "t2_perfectionism_z")



# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_perfectionism_z_1
here_save(t2_perfectionism_z_1, "t2_perfectionism_z_1")


# # Doing my best never seems to be enough./# My performance rarely measures up to my standards.
# I am hardly ever satisfied with my performance.
t2_perfectionism_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_perfectionism_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_perfectionism_z_null
here_save(t2_perfectionism_z_null, "t2_perfectionism_z_null")


# reflective models --------------------------------------------------------------
W_t2_gratitude_z <- select_and_rename_cols(names_base = W,
                                           baseline_vars = baseline_vars,
                                           outcome = "t2_gratitude_z")

## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_gratitude_z
here_save(t2_gratitude_z, "t2_gratitude_z")


## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_gratitude_z_1
here_save(t2_gratitude_z_1, "t2_gratitude_z_1")



## I have much in my life to be thankful for. # When I look at the world, I don’t see much to be grateful for. # I am grateful to a wide variety of people.
t2_gratitude_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_gratitude_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_gratitude_z_null
here_save(t2_gratitude_z_null, "t2_gratitude_z_null")

# # Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_vengeful_rumin_z
here_save(t2_vengeful_rumin_z, "t2_vengeful_rumin_z")

# Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_vengeful_rumin_z_1
here_save(t2_vengeful_rumin_z_1, "t2_vengeful_rumin_z_1")

# Sometimes I can't sleep because of thinking about past wrongs I have suffered.//# I can usually forgive and forget when someone does me wrong.# I find myself regularly thinking about past times that I have been wronged.
t2_vengeful_rumin_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_vengeful_rumin_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_vengeful_rumin_z_null
here_save(t2_vengeful_rumin_z_null, "t2_vengeful_rumin_z_null")

# Your health.
t2_pwb_your_health_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

t2_pwb_your_health_z
here_save(t2_pwb_your_health_z, "t2_pwb_your_health_z")



# Your health.
t2_pwb_your_health_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_health_z_1
here_save(t2_pwb_your_health_z_1, "t2_pwb_your_health_z_1")



# Your health.
t2_pwb_your_health_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_health_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_pwb_your_health_z_null
here_save(t2_pwb_your_health_z_null, "t2_pwb_your_health_z_null")

# #Your future security.
t2_pwb_your_future_security_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_future_security_z
here_save(t2_pwb_your_future_security_z,
          "t2_pwb_your_future_security_z")


# #Your future security.
t2_pwb_your_future_security_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_future_security_z_1
here_save(t2_pwb_your_future_security_z_1,
          "t2_pwb_your_future_security_z_1")



# #Your future security.
t2_pwb_your_future_security_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_future_security_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_future_security_z_null
here_save(t2_pwb_your_future_security_z_null,
          "t2_pwb_your_future_security_z_null")


# Your personal relationships.
t2_pwb_your_relationships_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_relationships_z
here_save(t2_pwb_your_relationships_z, "t2_pwb_your_relationships_z")


# Your personal relationships.
t2_pwb_your_relationships_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_relationships_z_1
here_save(t2_pwb_your_relationships_z_1,
          "t2_pwb_your_relationships_z_1")


# Your personal relationships.
t2_pwb_your_relationships_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_your_relationships_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_your_relationships_z_null
here_save(t2_pwb_your_relationships_z_null,
          "t2_pwb_your_relationships_z_null")


# Your standard of living.
t2_pwb_standard_living_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_standard_living_z
here_save(t2_pwb_standard_living_z, "t2_pwb_standard_living_z")


# Your standard of living.
t2_pwb_standard_living_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_standard_living_z_1
here_save(t2_pwb_standard_living_z_1, "t2_pwb_standard_living_z_1")


# Your standard of living.
t2_pwb_standard_living_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_pwb_standard_living_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_pwb_standard_living_z_null
here_save(t2_pwb_standard_living_z_null,
          "t2_pwb_standard_living_z_null")


# My life has a clear sense of purpose.
t2_meaning_purpose_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_meaning_purpose_z
here_save(t2_meaning_purpose_z, "t2_meaning_purpose_z")


t2_meaning_purpose_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_meaning_purpose_z_1
here_save(t2_meaning_purpose_z_1, "t2_meaning_purpose_z_1")




# My life has a clear sense of purpose.
t2_meaning_purpose_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_purpose_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_meaning_purpose_z_null
here_save(t2_meaning_purpose_z_null, "t2_meaning_purpose_z_null")



# I have a good sense of what makes my life meaningful.
t2_meaning_sense_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_meaning_sense_z
here_save(t2_meaning_sense_z, "t2_meaning_sense_z")


t2_meaning_sense_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_meaning_sense_z_1
here_save(t2_meaning_sense_z_1, "t2_meaning_sense_z_1")



# I have a good sense of what makes my life meaningful.
t2_meaning_sense_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_meaning_sense_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_meaning_sense_z_null
here_save(t2_meaning_sense_z_null, "t2_meaning_sense_z_null")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_lifesat_z
here_save(t2_lifesat_z, "t2_lifesat_z")


t2_lifesat_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_lifesat_z_1
here_save(t2_lifesat_z_1, "t2_lifesat_z_1")

# I am satisfied with my life.
# In most ways my life is close to ideal.
t2_lifesat_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_lifesat_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_lifesat_z_null
here_save(t2_lifesat_z_null, "t2_lifesat_z_null")



# social models -----------------------------------------------------------
# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_support_z
here_save(t2_support_z, "t2_support_z")


t2_support_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_support_z_1
here_save(t2_support_z_1, "t2_support_z_1")




# There are people I can depend on to help me if I really need it.
# There is no one I can turn to for guidance in times of stress (r)
# There is no one I can turn to for guidance in times of stress.
t2_support_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_support_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_support_z_null
here_save(t2_support_z_null, "t2_support_z_null")

# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_neighbourhood_community_z
here_save(t2_neighbourhood_community_z,
          "t2_neighbourhood_community_z")


t2_neighbourhood_community_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_neighbourhood_community_z_1
here_save(t2_neighbourhood_community_z_1,
          "t2_neighbourhood_community_z_1")


# I feel a sense of community with others in my local neighbourhood.
t2_neighbourhood_community_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_neighbourhood_community_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_neighbourhood_community_z_null
here_save(t2_neighbourhood_community_z_null,
          "t2_neighbourhood_community_z_null")


# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = gain_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_belong_z
here_save(t2_belong_z, "t2_belong_z")



t2_belong_z_1 <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = loss_A,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)
# view and save
t2_belong_z_1
here_save(t2_belong_z_1, "t2_belong_z_1")


# Know that people in my life accept and value me.
# Feel like an outsider.
# Know that people around me share my attitudes and beliefs.
t2_belong_z_null <- lmtp_tmle(
  data = df_final,
  trt = A,
  baseline = W,
  outcome = "t2_belong_z",
  cens = C,
  shift = NULL,
  mtp = TRUE,
  folds = 10,
  outcome_type = "continuous",
  weights = df_final$t0_sample_weights,
  learners_trt = sl_lib,
  learners_outcome = sl_lib
  
)

# view and save
t2_belong_z_null
here_save(t2_belong_z_null, "t2_belong_z_null")




# contrasts health ---------------------------------------------------------------
# smoker
t2_smoker_binary <- here_read("t2_smoker_binary")
t2_smoker_binary_1 <- here_read("t2_smoker_binary_1")
t2_smoker_binary_null <-
  here_read("t2_smoker_binary_null")


# first contrast
contrast_t2_smoker_binary <-
  lmtp_contrast(t2_smoker_binary, ref = t2_smoker_binary_null, type = "rr")


tab_contrast_t2_smoker_binary <-
  margot::margot_lmtp_evalue(contrast_t2_smoker_binary,
                             scale = "RR",
                             new_name = "Smoker")
# view and save
tab_contrast_t2_smoker_binary
here_save(tab_contrast_t2_smoker_binary,
          "ttab_contrast_t2_smoker_binary")

# second contrast
contrast_t2_smoker_binary_1 <-
  lmtp_contrast(t2_smoker_binary_1, ref = t2_smoker_binary_null, type = "rr")



tab_contrast_t2_smoker_binary_1 <-
  margot::margot_lmtp_evalue(contrast_t2_smoker_binary_1,
                             scale = "RR",
                             new_name = "Smoker")

# view
tab_contrast_t2_smoker_binary_1
# save
here_save(tab_contrast_t2_smoker_binary_1,
          "tab_contrast_t2_smoker_binary_1")



# sf health, your health
t2_short_form_health_z <- here_read("t2_short_form_health_z")
t2_short_form_health_z_1 <- here_read("t2_short_form_health_z_1")

t2_short_form_health_z_null <-
  here_read("t2_short_form_health_z_null")

# first contrast
contrast_t2_short_form_health_z <-
  lmtp_contrast(t2_short_form_health_z, ref = t2_short_form_health_z_null, type = "additive")

tab_contrast_t2_short_form_health_z <-
  margot::margot_lmtp_evalue(contrast_t2_short_form_health_z,
                             scale = "RD",
                             new_name = "Short form health, your health")
# view
tab_contrast_t2_short_form_health_z

# second contrast
contrast_t2_short_form_health_z_1 <-
  lmtp_contrast(t2_short_form_health_z_1, ref = t2_short_form_health_z_null, type = "additive")


tab_contrast_t2_short_form_health_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_short_form_health_z_1,
                             scale = "RD",
                             new_name = "Short form health, your health")

# view
tab_contrast_t2_short_form_health_z_1

# exercise
t2_hours_exercise_log_z <-
  here_read("t2_hours_exercise_log_z")
t2_hours_exercise_log_z_1 <-
  here_read("t2_hours_exercise_log_z_1")

t2_hours_exercise_log_z_null <-
  here_read("t2_hours_exercise_log_z_null")

# first contrast
contrast_t2_hours_exercise_log_z <-
  lmtp_contrast(t2_hours_exercise_log_z, ref = t2_hours_exercise_log_z_null, type = "additive")

tab_contrast_t2_hours_exercise_log_z <-
  margot::margot_lmtp_evalue(contrast_t2_hours_exercise_log_z,
                             scale = "RD",
                             new_name = "Hours excercise")

# view
tab_contrast_t2_hours_exercise_log_z

# second contrast
contrast_t2_hours_exercise_log_z_1 <-
  lmtp_contrast(t2_hours_exercise_log_z_1, ref = t2_hours_exercise_log_z_null, type = "additive")

tab_contrast_t2_hours_exercise_log_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_hours_exercise_log_z_1,
                             scale = "RD",
                             new_name = "Hours excercise")
# view
tab_contrast_t2_hours_exercise_log_z_1

# alcohol freq
t2_alcohol_frequency_z <-
  here_read("t2_alcohol_frequency_z")


t2_alcohol_frequency_z_1 <-
  here_read("t2_alcohol_frequency_z_1")

t2_alcohol_frequency_z_null <-
  here_read("t2_alcohol_frequency_z_null")

# first contrast
contrast_t2_alcohol_frequency_z <-
  lmtp_contrast(t2_alcohol_frequency_z, ref = t2_alcohol_frequency_z_null, type = "additive")


tab_contrast_t2_alcohol_frequency_z <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_frequency_z ,
                             scale = "RD",
                             new_name = "Alcohol frequency")
tab_contrast_t2_alcohol_frequency_z

# second contrast
contrast_t2_alcohol_frequency_z_1 <-
  lmtp_contrast(t2_alcohol_frequency_z_1, ref = t2_alcohol_frequency_z_null, type = "additive")


tab_contrast_t2_alcohol_frequency_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_frequency_z_1,
                             scale = "RD",
                             new_name = "Alcohol frequency")

# alcohol intensity
t2_alcohol_intensity_z <-
  here_read("t2_alcohol_intensity_z")

t2_alcohol_intensity_z_1 <-
  here_read("t2_alcohol_intensity_z_1")

t2_alcohol_intensity_z_null <-
  here_read("t2_alcohol_intensity_z_null")

# first contrast
contrast_t2_alcohol_intensity_z <-
  lmtp_contrast(t2_alcohol_intensity_z, ref = t2_alcohol_intensity_z_null, type = "additive")

contrast_t2_alcohol_intensity_z_1 <-
  lmtp_contrast(t2_alcohol_intensity_z_1, ref = t2_alcohol_intensity_z_null, type = "additive")

tab_contrast_t2_alcohol_intensity_z <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_intensity_z,
                             scale = "RD",
                             new_name = "Alcohol intensity")


# view
tab_contrast_t2_alcohol_intensity_z


tab_contrast_t2_alcohol_intensity_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_alcohol_intensity_z_1,
                             scale = "RD",
                             new_name = "Alcohol intensity")
# view
tab_contrast_t2_alcohol_intensity_z_1



# hour sleep
t2_hlth_sleep_hours_z <- here_read("t2_hlth_sleep_hours_z")
t2_hlth_sleep_hours_z_1 <- here_read("t2_hlth_sleep_hours_z_1")


t2_hlth_sleep_hours_z_null <-
  here_read("t2_hlth_sleep_hours_z_null")

# first contrast
contrast_t2_hours_sleep_z <-
  lmtp_contrast(t2_hlth_sleep_hours_z, ref = t2_hlth_sleep_hours_z_null, type = "additive")

tab_contrast_t2_hours_sleep_z <-
  margot::margot_lmtp_evalue(contrast_t2_hours_sleep_z,
                             scale = "RD",
                             new_name = "Hours sleep")

# view
tab_contrast_t2_hours_sleep_z

# second contrast
contrast_t2_hours_sleep_z_1 <-
  lmtp_contrast(t2_hlth_sleep_hours_z_1, ref = t2_hlth_sleep_hours_z_null, type = "additive")

tab_contrast_t2_hours_sleep_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_hours_sleep_z_1,
                             scale = "RD",
                             new_name = "Hours sleep")

# view
tab_contrast_t2_hours_sleep_z_1


# bmi
t2_hlth_bmi_z <- here_read("t2_hlth_bmi_z")
t2_hlth_bmi_z_1 <- here_read("t2_hlth_bmi_z_1")

t2_hlth_bmi_z_null <- here_read("t2_hlth_bmi_z_null")

# first contrast
contrast_t2_bmi_z <- lmtp_contrast(t2_hlth_bmi_z, ref = t2_hlth_bmi_z_null, type = "additive")

tab_contrast_t2_bmi_z <-
  margot::margot_lmtp_evalue(contrast_t2_bmi_z, scale = "RD", new_name = "BMI")



# second contrast
contrast_t2_bmi_z_1 <- lmtp_contrast(t2_hlth_bmi_z_1, ref = t2_hlth_bmi_z_null, type = "additive")

tab_contrast_t2_bmi_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_bmi_z_1, scale = "RD", new_name = "BMI")

# view
tab_contrast_t2_bmi_z_1


# contrast embodied -------------------------------------------------------

# bodysat
t2_bodysat_z <- here_read("t2_bodysat_z")
t2_bodysat_z_1 <- here_read("t2_bodysat_z_1")

t2_bodysat_z_null <- here_read("t2_bodysat_z_null")

# first contrast
contrast_t2_bodysat_z <- lmtp_contrast(t2_bodysat_z, ref = t2_bodysat_z_null, type = "additive")

tab_contrast_t2_bodysat_z <-
  margot::margot_lmtp_evalue(contrast_t2_bodysat_z, scale = "RD", new_name = "Body satisfaction")

tab_contrast_t2_bodysat_z

# second contrast
contrast_t2_bodysat_z_1 <- lmtp_contrast(t2_bodysat_z_1, ref = t2_bodysat_z_null, type = "additive")

tab_contrast_t2_bodysat_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_bodysat_z_1,
                             scale = "RD",
                             new_name = "Body satisfaction")

# view
tab_contrast_t2_bodysat_z_1

# depression
t2_kessler_latent_depression_z <-
  here_read("t2_kessler_latent_depression_z")

t2_kessler_latent_depression_z_1 <-
  here_read("t2_kessler_latent_depression_z_1")

t2_kessler_latent_depression_z_null <-
  here_read("t2_kessler_latent_depression_z_null")

t2_kessler_latent_depression_z_null

# first contrast
contrast_t2_kessler_latent_depression_z <-
  lmtp_contrast(t2_kessler_latent_depression_z,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z,
                             scale = "RD",
                             new_name = "Kessler 6 depression")

# view
tab_contrast_t2_kessler_latent_depression_z


# second contrast
contrast_t2_kessler_latent_depression_z_1 <-
  lmtp_contrast(t2_kessler_latent_depression_z_1,
                ref = t2_kessler_latent_depression_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_depression_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_depression_z_1,
                             scale = "RD",
                             new_name = "Kessler 6 depression")
# view
tab_contrast_t2_kessler_latent_depression_z_1

#anxiety

t2_kessler_latent_anxiety_z <-
  here_read("t2_kessler_latent_anxiety_z")

t2_kessler_latent_anxiety_z_1 <-
  here_read("t2_kessler_latent_anxiety_z_1")

t2_kessler_latent_anxiety_z_null <-
  here_read("t2_kessler_latent_anxiety_z_null")

# first contrast
contrast_t2_kessler_latent_anxiety_z <-
  lmtp_contrast(t2_kessler_latent_anxiety_z, ref = t2_kessler_latent_anxiety_z_null, type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z,
                             scale = "RD",
                             new_name = "Kessler 6 anxiety")
# view
tab_contrast_t2_kessler_latent_anxiety_z

# second contrast
contrast_t2_kessler_latent_anxiety_z_1 <-
  lmtp_contrast(t2_kessler_latent_anxiety_z_1,
                ref = t2_kessler_latent_anxiety_z_null,
                type = "additive")

tab_contrast_t2_kessler_latent_anxiety_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_kessler_latent_anxiety_z_1,
                             scale = "RD",
                             new_name = "Kessler 6 anxiety")
# view
tab_contrast_t2_kessler_latent_anxiety_z_1


# fatigue
t2_hlth_fatigue_z <- here_read("t2_hlth_fatigue_z")

t2_hlth_fatigue_z_1 <- here_read("t2_hlth_fatigue_z_1")

t2_hlth_fatigue_z_null <-
  here_read("t2_hlth_fatigue_z_null")

# first contrast
contrast_t2_hlth_fatigue_z <-
  lmtp_contrast(t2_hlth_fatigue_z, ref = t2_hlth_fatigue_z_null, type = "additive")


tab_contrast_t2_hlth_fatigue_z <-
  margot::margot_lmtp_evalue(contrast_t2_hlth_fatigue_z ,
                             scale = "RD",
                             new_name = "Fatigue")

# view
tab_contrast_t2_hlth_fatigue_z


# second contrast
contrast_t2_hlth_fatigue_z_1 <-
  lmtp_contrast(t2_hlth_fatigue_z_1, ref = t2_hlth_fatigue_z_null, type = "additive")


tab_contrast_t2_hlth_fatigue_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_hlth_fatigue_z_1,
                             scale = "RD",
                             new_name = "Fatigue")
# view
tab_contrast_t2_hlth_fatigue_z_1

# rumination
t2_rumination_z <- here_read("t2_rumination_z")
t2_rumination_z_1 <- here_read("t2_rumination_z_1")
t2_rumination_z_null <-
  here_read("t2_rumination_z_null")

# first contrast
contrast_t2_rumination_z <-
  lmtp_contrast(t2_rumination_z, ref = t2_rumination_z_null, type = "additive")

tab_contrast_t2_rumination_z <-
  margot::margot_lmtp_evalue(contrast_t2_rumination_z ,
                             scale = "RD",
                             new_name = "Rumination")
# view
tab_contrast_t2_rumination_z
# second contrast

contrast_t2_rumination_z_1 <-
  lmtp_contrast(t2_rumination_z_1, ref = t2_rumination_z_null, type = "additive")

tab_contrast_t2_rumination_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_rumination_z_1 ,
                             scale = "RD",
                             new_name = "Rumination")

# view
tab_contrast_t2_rumination_z_1

# sex sat
t2_sexual_satisfaction_z <-
  here_read("t2_sexual_satisfaction_z")

t2_sexual_satisfaction_z_1 <-
  here_read("t2_sexual_satisfaction_z_1")

t2_sexual_satisfaction_z_null <-
  here_read("t2_sexual_satisfaction_z_null")

# first contrast
contrast_t2_sexual_satisfaction_z <-
  lmtp_contrast(t2_sexual_satisfaction_z, ref = t2_sexual_satisfaction_z_null, type = "additive")


tab_contrast_t2_sexual_satisfaction_z <-
  margot::margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z,
                             scale = "RD",
                             new_name = "Sexual satisfaction")
# view
tab_contrast_t2_sexual_satisfaction_z


# second contrast
contrast_t2_sexual_satisfaction_z_1 <-
  lmtp_contrast(t2_sexual_satisfaction_z_1, ref = t2_sexual_satisfaction_z_null, type = "additive")


tab_contrast_t2_sexual_satisfaction_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_sexual_satisfaction_z_1,
                             scale = "RD",
                             new_name = "Sexual satisfaction")
# view
tab_contrast_t2_sexual_satisfaction_z_1



# contrasts ego -----------------------------------------------------------
# self esteem

t2_self_esteem_z <- here_read("t2_self_esteem_z")
t2_self_esteem_z_1 <- here_read("t2_self_esteem_z_1")
t2_self_esteem_z_null <-
  here_read("t2_self_esteem_z_null")


# first contrast
contrast_t2_self_esteem_z <-
  lmtp_contrast(t2_self_esteem_z, ref = t2_self_esteem_z_null, type = "additive")


tab_contrast_t2_self_esteem_z <-
  margot::margot_lmtp_evalue(contrast_t2_self_esteem_z,
                             scale = "RD",
                             new_name = "Self esteem")
# view
tab_contrast_t2_self_esteem_z



# second contrast
contrast_t2_self_esteem_z_1 <-
  lmtp_contrast(t2_self_esteem_z_1, ref = t2_self_esteem_z_null, type = "additive")

tab_contrast_t2_self_esteem_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_self_esteem_z_1,
                             scale = "RD",
                             new_name = "Self esteem")

tab_contrast_t2_self_esteem_z_1

# perfectionism
t2_perfectionism_z <- here_read("t2_perfectionism_z")
t2_perfectionism_z_1 <- here_read("t2_perfectionism_z_1")
t2_perfectionism_z_null <-
  here_read("t2_perfectionism_z_null")

# first contrast
contrast_t2_perfectionism_z <-
  lmtp_contrast(t2_perfectionism_z, ref = t2_perfectionism_z_null, type = "additive")


tab_contrast_t2_perfectionism_z <-
  margot::margot_lmtp_evalue(contrast_t2_perfectionism_z ,
                             scale = "RD",
                             new_name = "Perfectionism")

# view
tab_contrast_t2_perfectionism_z


# second contrast
contrast_t2_perfectionism_z_1 <-
  lmtp_contrast(t2_perfectionism_z_1, ref = t2_perfectionism_z_null, type = "additive")


tab_contrast_t2_perfectionism_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_perfectionism_z_1,
                             scale = "RD",
                             new_name = "Perfectionism")
# view
tab_contrast_t2_perfectionism_z_1


# self control have
t2_self_control_have_lots_z <-
  here_read("t2_self_control_have_lots_z")

t2_self_control_have_lots_z_1 <-
  here_read("t2_self_control_have_lots_z_1")

t2_self_control_have_lots_z_null <-
  here_read("t2_self_control_have_lots_z_null")

# first contrast
contrast_t2_self_control_have_lots_z <-
  lmtp_contrast(t2_self_control_have_lots_z, ref = t2_self_control_have_lots_z_null, type = "additive")


tab_contrast_t2_self_control_have_lots_z <-
  margot::margot_lmtp_evalue(contrast_t2_self_control_have_lots_z ,
                             scale = "RD",
                             new_name = "Self control have")

tab_contrast_t2_self_control_have_lots_z


# second contrast
contrast_t2_self_control_have_lots_z_1 <-
  lmtp_contrast(t2_self_control_have_lots_z_1,
                ref = t2_self_control_have_lots_z_null,
                type = "additive")


tab_contrast_t2_self_control_have_lots_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_self_control_have_lots_z_1,
                             scale = "RD",
                             new_name = "Self control have")

# view
tab_contrast_t2_self_control_have_lots_z_1


# self control wish
t2_self_control_wish_more_reversed_z <-
  here_read("t2_self_control_wish_more_reversed_z")

t2_self_control_wish_more_reversed_z_1 <-
  here_read("t2_self_control_wish_more_reversed_z_1")

t2_self_control_wish_more_reversed_z_null <-
  here_read("t2_self_control_wish_more_reversed_z_null")

# first contrast
contrast_t2_self_control_wish_more_reversed_z <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z <-
  margot::margot_lmtp_evalue(
    contrast_t2_self_control_wish_more_reversed_z,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )

# view
tab_contrast_t2_self_control_wish_more_reversed_z


# seoind contrast
contrast_t2_self_control_wish_more_reversed_z_1 <-
  lmtp_contrast(t2_self_control_wish_more_reversed_z_1,
                ref = t2_self_control_wish_more_reversed_z_null,
                type = "additive")

tab_contrast_t2_self_control_wish_more_reversed_z_1 <-
  margot::margot_lmtp_evalue(
    contrast_t2_self_control_wish_more_reversed_z_1,
    scale = "RD",
    new_name = "Self control wish more (reversed)"
  )

# view
tab_contrast_t2_self_control_wish_more_reversed_z_1


# contrasts reflective ----------------------------------------------------

# gratitude
t2_gratitude_z <- here_read("t2_gratitude_z")
t2_gratitude_z_1 <- here_read("t2_gratitude_z_1")

t2_gratitude_z_null <- here_read("t2_gratitude_z_null")
t2_gratitude_z_null
# first contrast
contrast_t2_gratitude_z <- lmtp_contrast(t2_gratitude_z, ref = t2_gratitude_z_null, type = "additive")
tab_contrast_t2_gratitude_z <-
  margot::margot_lmtp_evalue(contrast_t2_gratitude_z,
                             scale = "RD",
                             new_name = "Gratitude")
# view
tab_contrast_t2_gratitude_z

# second contrast
contrast_t2_gratitude_z_1 <- lmtp_contrast(t2_gratitude_z_1, ref = t2_gratitude_z_null, type = "additive")
tab_contrast_t2_gratitude_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_gratitude_z_1 ,
                             scale = "RD",
                             new_name = "Gratitude")
# view
tab_contrast_t2_gratitude_z_1


# vengence / forgive
t2_vengeful_rumin_z <- here_read("t2_vengeful_rumin_z")
t2_vengeful_rumin_z_1 <- here_read("t2_vengeful_rumin_z_1")

t2_vengeful_rumin_z_null <-
  here_read("t2_vengeful_rumin_z_null")

# first contrast
contrast_t2_vengeful_rumin_z <-
  lmtp_contrast(t2_vengeful_rumin_z, ref = t2_vengeful_rumin_z_null, type = "additive")

tab_contrast_t2_vengeful_rumin_z <-
  margot_lmtp_tab(contrast_t2_vengeful_rumin_z,
                  scale = "RD",
                  new_name = "Vengefulness (forgiveness)")


tab_contrast_t2_vengeful_rumin_z <-
  lmtp_evalue_tab(tab_contrast_t2_vengeful_rumin_z, scale = c("RD"))
# view
tab_contrast_t2_vengeful_rumin_z


# second contrast
contrast_t2_vengeful_rumin_z_1 <-
  lmtp_contrast(t2_vengeful_rumin_z_1, ref = t2_vengeful_rumin_z_null, type = "additive")

tab_contrast_t2_vengeful_rumin_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_vengeful_rumin_z_1 ,
                             scale = "RD",
                             new_name = "Vengefulness (forgiveness")

# view
tab_contrast_t2_vengeful_rumin_z_1


# pwb your health

t2_pwb_your_health_z <-
  here_read("t2_pwb_your_health_z")

t2_pwb_your_health_z_1 <-
  here_read("t2_pwb_your_health_z_1")

t2_pwb_your_health_z_null <-
  here_read("t2_pwb_your_health_z_null")

# first contrast
contrast_t2_pwb_your_health_z <-
  lmtp_contrast(t2_pwb_your_health_z, ref = t2_pwb_your_health_z_null, type = "additive")

tab_contrast_t2_pwb_your_health_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_health_z,
                             scale = "RD",
                             new_name = "PWB your health")

# view
tab_contrast_t2_pwb_your_health_z

# second contrast
contrast_t2_pwb_your_health_z_1 <-
  lmtp_contrast(t2_pwb_your_health_z_1, ref = t2_pwb_your_health_z_null, type = "additive")

tab_contrast_t2_pwb_your_health_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_health_z_1,
                             scale = "RD",
                             new_name = "PWB your health")


# view
tab_contrast_t2_pwb_your_health_z_1

# pwb your furture security

t2_pwb_your_future_security_z <-
  here_read("t2_pwb_your_future_security_z")

t2_pwb_your_future_security_z_1 <-
  here_read("t2_pwb_your_future_security_z_1")

t2_pwb_your_future_security_z_null <-
  here_read("t2_pwb_your_future_security_z_null")

# first contrast
contrast_t2_pwb_your_future_security_z <-
  lmtp_contrast(t2_pwb_your_future_security_z,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_future_security_z,
                             scale = "RD",
                             new_name = "PWB your future security")

tab_contrast_t2_pwb_your_future_security_z

# second contrast
contrast_t2_pwb_your_future_security_z_1 <-
  lmtp_contrast(t2_pwb_your_future_security_z_1,
                ref = t2_pwb_your_future_security_z_null,
                type = "additive")

tab_contrast_t2_pwb_your_future_security_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_future_security_z_1,
                             scale = "RD",
                             new_name = "PWB your future security")


# pwb your relationships
t2_pwb_your_relationships_z <-
  here_read("t2_pwb_your_relationships_z")

t2_pwb_your_relationships_z_1 <-
  here_read("t2_pwb_your_relationships_z_1")

t2_pwb_your_relationships_z_null <-
  here_read("t2_pwb_your_relationships_z_null")

# first contrast
contrast_t2_pwb_your_relationships_z <-
  lmtp_contrast(t2_pwb_your_relationships_z, ref = t2_pwb_your_relationships_z_null, type = "additive")


tab_contrast_t2_pwb_your_relationships_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_relationships_z ,
                             scale = "RD",
                             new_name = "PWB your relationships")

# view
tab_contrast_t2_pwb_your_relationships_z


# second contrast
contrast_t2_pwb_your_relationships_z_1 <-
  lmtp_contrast(t2_pwb_your_relationships_z_1 ,
                ref = t2_pwb_your_relationships_z_null,
                type = "additive")


tab_contrast_t2_pwb_your_relationships_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_your_relationships_z_1 ,
                             scale = "RD",
                             new_name = "PWB your relationships")
# view
tab_contrast_t2_pwb_your_relationships_z_1


# pwb your standard of living
t2_pwb_standard_living_z <-
  here_read("t2_pwb_standard_living_z")

t2_pwb_standard_living_z_1 <-
  here_read("t2_pwb_standard_living_z_1")

t2_pwb_standard_living_z_null <-
  here_read("t2_pwb_standard_living_z_null")

# first contrast
contrast_t2_pwb_standard_living_z <-
  lmtp_contrast(t2_pwb_standard_living_z, ref = t2_pwb_standard_living_z_null, type = "additive")

tab_contrast_t2_pwb_standard_living_z <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_standard_living_z ,
                             scale = "RD",
                             new_name = "PWB your standard living")

# view
tab_contrast_t2_pwb_standard_living_z


# second contrast
contrast_t2_pwb_standard_living_z_1 <-
  lmtp_contrast(t2_pwb_standard_living_z_1, ref = t2_pwb_standard_living_z_null, type = "additive")

tab_contrast_t2_pwb_standard_living_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_pwb_standard_living_z_1 ,
                             scale = "RD",
                             new_name = "PWB your standard living")
# view
tab_contrast_t2_pwb_standard_living_z_1

t2_meaning_purpose_z <- here_read("t2_meaning_purpose_z")
t2_meaning_purpose_z_1 <- here_read("t2_meaning_purpose_z_1")
t2_meaning_purpose_z_null <-
  here_read("t2_meaning_purpose_z_null")

# first contrast
contrast_t2_meaning_purpose_z <-
  lmtp_contrast(t2_meaning_purpose_z, ref = t2_meaning_purpose_z_null, type = "additive")

tab_contrast_t2_meaning_purpose_z <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_purpose_z,
                             scale = "RD",
                             new_name = "Meaning: clear sense of purpose")
# view
tab_contrast_t2_meaning_purpose_z

# second contrast
contrast_t2_meaning_purpose_z_1 <-
  lmtp_contrast(t2_meaning_purpose_z_1 , ref = t2_meaning_purpose_z_null, type = "additive")

tab_contrast_t2_meaning_purpose_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_purpose_z_1 ,
                             scale = "RD",
                             new_name = "Meaning: clear sense of purpose")
# meaning sense
t2_meaning_sense_z <- here_read("t2_meaning_sense_z")
t2_meaning_sense_z_1 <- here_read("t2_meaning_sense_z_1")

t2_meaning_sense_z_null <-
  here_read("t2_meaning_sense_z_null")

# first contrast
contrast_t2_meaning_sense_z <-
  lmtp_contrast(t2_meaning_sense_z, ref = t2_meaning_sense_z_null, type = "additive")

tab_contrast_t2_meaning_sense_z <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_sense_z,
                             scale = "RD",
                             new_name = "Meaning: good sense of what makes my life meaningful")

# view
tab_contrast_t2_meaning_sense_z


# second contrast
contrast_t2_meaning_sense_z_1 <-
  lmtp_contrast(t2_meaning_sense_z_1, ref = t2_meaning_sense_z_null, type = "additive")

tab_contrast_t2_meaning_sense_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_meaning_sense_z_1,
                             scale = "RD",
                             new_name = "Meaning: good sense of what makes my life meaningful")
# view
tab_contrast_t2_meaning_sense_z_1



# lifesat
t2_lifesat_z <- here_read("t2_lifesat_z")
t2_lifesat_z_1 <- here_read("t2_lifesat_z_1")
t2_lifesat_z_null <- here_read("t2_lifesat_z_null")


# first contrast
contrast_t2_lifesat_z <- lmtp_contrast(t2_lifesat_z, ref = t2_lifesat_z_null, type = "additive")

tab_contrast_t2_lifesat_z <-
  margot::margot_lmtp_evalue(contrast_t2_lifesat_z, scale = "RD", new_name = "Satisfaction with life")

# view
tab_contrast_t2_lifesat_z

# second contrast
contrast_t2_lifesat_z_1 <- lmtp_contrast(t2_lifesat_z_1, ref = t2_lifesat_z_null, type = "additive")

tab_contrast_t2_lifesat_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_lifesat_z_1,
                             scale = "RD",
                             new_name = "Satisfaction with life")

# view
tab_contrast_t2_lifesat_z_1

# contrasts social --------------------------------------------------------
# social support
t2_support_z <- here_read("t2_support_z")
t2_support_z_1 <- here_read("t2_support_z_1")
t2_support_z_null <- here_read("t2_support_z_null")

# first contrast
contrast_t2_support_z <- lmtp_contrast(t2_support_z, ref = t2_support_z_null, type = "additive")
tab_contrast_t2_support_z <-
  margot::margot_lmtp_evalue(contrast_t2_support_z, scale = "RD", new_name = "Social support")

tab_contrast_t2_support_z

# second contrast
contrast_t2_support_z_1 <- lmtp_contrast(t2_support_z_1, ref = t2_support_z_null, type = "additive")
tab_contrast_t2_support_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_support_z_1,
                             scale = "RD",
                             new_name = "Social support")
# view
tab_contrast_t2_support_z_1

# neighbourhood community

t2_neighbourhood_community_z <-
  here_read("t2_neighbourhood_community_z")

t2_neighbourhood_community_z_1 <-
  here_read("t2_neighbourhood_community_z_1")

t2_neighbourhood_community_z_null <-
  here_read("t2_neighbourhood_community_z_null")

# first contrast
contrast_t2_neighbourhood_community_z <-
  lmtp_contrast(t2_neighbourhood_community_z,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z <-
  margot::margot_lmtp_evalue(contrast_t2_neighbourhood_community_z,
                             scale = "RD",
                             new_name = "Neighbourhood community")

# view
tab_contrast_t2_neighbourhood_community_z

# second contrast
contrast_t2_neighbourhood_community_z_1 <-
  lmtp_contrast(t2_neighbourhood_community_z_1,
                ref = t2_neighbourhood_community_z_null,
                type = "additive")

tab_contrast_t2_neighbourhood_community_z_1 <-
  margot::margot_lmtp_evalue(contrast_t2_neighbourhood_community_z_1,
                             scale = "RD",
                             new_name = "Neighbourhood community")

tab_contrast_t2_neighbourhood_community_z_1

# social belong
t2_belong_z <- here_read("t2_belong_z")
t2_belong_z_1 <- here_read("t2_belong_z_1")
t2_belong_z_null <- here_read("t2_belong_z_null")

# first contrast
contrast_t2_belong_z <- lmtp_contrast(t2_belong_z, ref = t2_belong_z_null, type = "additive")


tab_contrast_t2_belong_z <-
  margot::margot_lmtp_evalue(contrast_t2_belong_z, scale = "RD", new_name = "Social belonging")

tab_contrast_t2_belong_z

# second contrast
contrast_t2_belong_z_1 <- lmtp_contrast(t2_belong_z_1 , ref = t2_belong_z_null, type = "additive")


tab_contrast_t2_belong_z_1  <-
  margot::margot_lmtp_evalue(contrast_t2_belong_z_1 ,
                             scale = "RD",
                             new_name = "Social belonging")

# view
tab_contrast_t2_belong_z_1


# make tables -------------------------------------------------------------

# don't forget to report smoking
# bind individual tables

tab_health_smoker <- rbind(tab_contrast_t2_smoker_binary)
tab_health


tab_health <- rbind(
  tab_contrast_t2_hours_exercise_log_z,
  tab_contrast_t2_alcohol_frequency_z,
  tab_contrast_t2_alcohol_intensity_z,
  tab_contrast_t2_bmi_z,
  tab_contrast_t2_hours_sleep_z,
  tab_contrast_t2_short_form_health_z
)
here_save(tab_health, "tab_health")
tab_health <- here_read("tab_health")


here_save(tab_health , "tab_health")
tab_health <- here_read("tab_health")

tab_body <- rbind(
  tab_contrast_t2_bodysat_z,
  tab_contrast_t2_kessler_latent_anxiety_z,
  tab_contrast_t2_kessler_latent_depression_z,
  tab_contrast_t2_hlth_fatigue_z,
  tab_contrast_t2_rumination_z,
  tab_contrast_t2_sexual_satisfaction_z
)
tab_body


here_save(tab_body, "tab_body")
tab_body <- here_read("tab_body")


tab_ego <- rbind(
  #  tab_contrast_t2_emotion_regulation_out_control_z,
  #  tab_contrast_t2_permeability_individual_z,
  tab_contrast_t2_perfectionism_z,
  # tab_contrast_t2_power_no_control_composite_z,
  tab_contrast_t2_self_control_have_lots_z,
  tab_contrast_t2_self_control_wish_more_reversed_z,
  tab_contrast_t2_self_esteem_z
)

tab_ego

here_save(tab_ego, "tab_ego")
tab_ego <- here_read("tab_ego")

tab_reflective <- rbind(
  tab_contrast_t2_gratitude_z,
  tab_contrast_t2_meaning_purpose_z,
  tab_contrast_t2_meaning_sense_z,
  tab_contrast_t2_pwb_your_future_security_z,
  tab_contrast_t2_pwb_your_health_z,
  tab_contrast_t2_pwb_your_relationships_z,
  tab_contrast_t2_pwb_standard_living_z,
  tab_contrast_t2_lifesat_z,
  tab_contrast_t2_vengeful_rumin_z
)
tab_reflective


here_save(tab_reflective, "tab_reflective")
tab_reflective <- here_read("tab_reflective")

tab_social <- rbind(
  tab_contrast_t2_belong_z,
  tab_contrast_t2_neighbourhood_community_z,
  tab_contrast_t2_support_z
)
tab_social

here_save(tab_social, "tab_social")
tab_social <- here_read("tab_social")


str(tab_health)

head(tab_health)
# make group table
group_tab_health <- group_tab(tab_health  , type = "RD")

# save
here_save(group_tab_health, "group_tab_health")


# make group table
group_tab_body <- group_tab(tab_body , type = "RD")

# save
here_save(group_tab_body, "group_tab_body")

# make group table
group_tab_ego <- group_tab(tab_ego, type = "RD")

# save
here_save(group_tab_ego, "group_tab_ego")


# make group table
group_tab_reflective <- group_tab(tab_reflective, type = "RD")
tab_reflective
# save
here_save(group_tab_reflective, "group_tab_reflective")

# make group table
group_tab_social <- group_tab(tab_social, type = "RD")

# save
here_save(group_tab_social, "group_tab_social")

group_tab_health <- here_read("group_tab_health")
group_tab_body <- here_read("group_tab_body")
group_tab_ego <- here_read("group_tab_ego")
group_tab_reflective <- here_read("group_tab_reflective")
group_tab_social <- here_read("group_tab_social")

group_tab_reflective <- group_tab_reflective |>
  mutate(
    outcome = case_when(
      outcome == "Vengefulness (forgiveness)" ~ "Vengefulness",
      outcome == "Meaning: good sense of what makes my life meaningful" ~ "Meaning: Sense",
      outcome == "PWB your future security" ~ "PWB Security",
      outcome == "PWB your standard living" ~ "PWB Std Living",
      outcome == "PWB your relationships" ~ "PWB Relationships",
      outcome == "Meaning: clear sense of purpose" ~ "Meaning: Purpose",
      outcome == "Satisfaction with life" ~ "Life Satisfaction",
      TRUE ~ outcome # keep other labels unchanged
    )
  )

group_tab_reflective
# create plots -------------------------------------------------------------
# define plot params:
base_size = 18
text_size = 4.5
point_size = 3
title_size = 20
subtitle_size = 15
legend_text_size = 8
legend_title_size = 10
x_offset = -1.25
x_lim_lo = -1.20
x_lim_hi =  .25

# check N
n_participants <- here_read("n_participants")
N = n_participants
sub_title = "Heavy Work vs Status Quo"

# graph health
plot_group_tab_health <- old_margot_plot(
  group_tab_health,
  type = "RD",
  title = "Biological Health",
  subtitle = sub_title,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)

# save graph
ggsave(
  plot_group_tab_health,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_health.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
# graph body
plot_group_tab_body <- old_margot_plot(
  group_tab_body,
  type = "RD",
  title = "Embodied Well-Being",
  subtitle = sub_title,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
)
# view
plot_group_tab_body

# save graph
ggsave(
  plot_group_tab_body,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
# view
plot_group_tab_body
group_tab_ego
# graph ego
plot_group_tab_ego <- old_margot_plot(
  group_tab_ego,
  type = "RD",
  title = "Person-Centred Well-Being",
  subtitle = sub_title,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_ego

# save graph
ggsave(
  plot_group_tab_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
# view
plot_group_tab_ego

# graph reflective
plot_group_tab_reflective <- old_margot_plot(
  group_tab_reflective,
  type = "RD",
  title = "Life-Centred Well-Being",
  subtitle = sub_title,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_reflective

# save graph
ggsave(
  plot_group_tab_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social <- old_margot_plot(
  group_tab_social,
  type = "RD",
  title = "Social Well-Being",
  subtitle = sub_title,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_social

# save graph
ggsave(
  plot_group_tab_social,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# make tables shift one up -------------------------------------------------------------

# bind individual tables
tab_health_1 <- rbind(
  tab_contrast_t2_hours_exercise_log_z_1,
  tab_contrast_t2_alcohol_frequency_z_1,
  tab_contrast_t2_alcohol_intensity_z_1,
  tab_contrast_t2_bmi_z_1,
  tab_contrast_t2_hours_sleep_z_1,
  tab_contrast_t2_short_form_health_z_1
)
# view
tab_health

here_save(tab_health_1, "tab_health_1")

tab_body_1 <- rbind(
  tab_contrast_t2_bodysat_z_1,
  tab_contrast_t2_kessler_latent_anxiety_z_1,
  tab_contrast_t2_kessler_latent_depression_z_1,
  tab_contrast_t2_hlth_fatigue_z_1,
  tab_contrast_t2_rumination_z_1,
  tab_contrast_t2_sexual_satisfaction_z_1
)
# view
tab_body_1

here_save(tab_body_1, "tab_body_1")

tab_ego_1 <- rbind(
  #  tab_contrast_t2_emotion_regulation_out_control_z_1,
  #  tab_contrast_t2_permeability_individual_z_1,
  tab_contrast_t2_perfectionism_z_1,
  #  tab_contrast_t2_power_no_control_composite_z_1,
  tab_contrast_t2_self_control_have_lots_z_1,
  tab_contrast_t2_self_control_wish_more_reversed_z_1,
  tab_contrast_t2_self_esteem_z_1
)
# view
tab_ego_1

here_save(tab_ego_1, "tab_ego_1")
tab_contrast_t2_vengeful_rumin_z_1
tab_reflective_1 <- rbind(
  tab_contrast_t2_gratitude_z_1,
  tab_contrast_t2_meaning_purpose_z_1,
  tab_contrast_t2_meaning_sense_z_1,
  tab_contrast_t2_pwb_your_future_security_z_1,
  tab_contrast_t2_pwb_your_health_z_1,
  tab_contrast_t2_pwb_your_relationships_z_1,
  tab_contrast_t2_pwb_standard_living_z_1,
  tab_contrast_t2_lifesat_z_1,
  tab_contrast_t2_vengeful_rumin_z_1
)# view
tab_reflective_1

here_save(tab_reflective_1, "tab_reflective_1")

tab_social_1 <- rbind(
  tab_contrast_t2_belong_z_1,
  tab_contrast_t2_neighbourhood_community_z_1,
  tab_contrast_t2_support_z_1
)
# view
tab_social_1
here_save(tab_social_1, "tab_social_1")




# make group table
group_tab_health_1 <- group_tab(tab_health_1, type = "RD")

# save
here_save(group_tab_health_1, "group_tab_health_1")


# make group table
group_tab_body_1 <- group_tab(tab_body_1 , type = "RD")

# save
here_save(group_tab_body_1, "group_tab_body_1")

# make group table
group_tab_ego_1 <- group_tab(tab_ego_1, type = "RD")

# save
here_save(group_tab_ego_1, "group_tab_ego_1")

# make group table
group_tab_reflective_1 <-
  group_tab(tab_reflective_1, type = "RD")

# save
here_save(group_tab_reflective_1, "group_tab_reflective_1")

# make group table
group_tab_social_1 <- group_tab(tab_social_1, type = "RD")

# save
here_save(group_tab_social_1, "group_tab_social_1")

# read tables
group_tab_health_1 <- here_read("group_tab_health_1")
group_tab_body_1 <- here_read("group_tab_body_1")
group_tab_ego_1 <- here_read("group_tab_ego_1")
group_tab_reflective_1 <- here_read("group_tab_reflective_1")
group_tab_social_1 <- here_read("group_tab_social_1")
# view
group_tab_reflective_1
# change labels
group_tab_reflective_1 <- group_tab_reflective_1 %>%
  mutate(
    outcome = case_when(
      outcome == "Vengefulness (forgiveness" ~ "Vengefulness",
      outcome == "Meaning: good sense of what makes my life meaningful" ~ "Meaning: Sense",
      outcome == "PWB your future security" ~ "PWB Security",
      outcome == "PWB your standard living" ~ "PWB Std Living",
      outcome == "PWB your relationships" ~ "PWB Relationships",
      outcome == "Meaning: clear sense of purpose" ~ "Meaning: Purpose",
      outcome == "Satisfaction with life" ~ "Life Satisfaction",
      TRUE ~ outcome # keep other labels unchanged
    )
  )
# view
group_tab_reflective_1
# create plots -------------------------------------------------------------
# USE OLD MARGOT PLOT FUNCTION IN THIS DIRECTORY
sub_title_1 = "Reduce Work vs Status Quo"
tab_health_1 <- here_read("tab_health_1")

# graph health
plot_group_tab_health_1 <- old_margot_plot(
  tab_health_1,
  type = "RD",
  title = "Biological Health",
  subtitle = sub_title_1,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_health_1

# save graph
ggsave(
  plot_group_tab_health_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_health_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph body
plot_group_tab_body_1 <- old_margot_plot(
  group_tab_body_1,
  type = "RD",
  title = "Embodied Well-Being",
  subtitle = sub_title_1,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_body_1
# save graph
ggsave(
  plot_group_tab_body_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_body_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
# graph ego
plot_group_tab_ego_1 <- old_margot_plot(
  group_tab_ego_1,
  type = "RD",
  title = "Person-Centred Well-Being",
  subtitle = sub_title_1,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_ego_1

# save graph
ggsave(
  plot_group_tab_ego_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_ego_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)


# graph reflective
plot_group_tab_reflective_1 <- old_margot_plot(
  group_tab_reflective_1,
  type = "RD",
  title = "Life-Centred Well-Being",
  subtitle = sub_title_1,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_reflective_1

# save graph
ggsave(
  plot_group_tab_reflective_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_reflective_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# graph social
plot_group_tab_social_1 <- old_margot_plot(
  group_tab_social_1,
  type = "RD",
  title = "Social Well-Being",
  subtitle = sub_title_1,
  estimate_scale = 1,
  base_size = base_size,
  text_size = text_size,
  point_size = point_size,
  title_size = title_size,
  subtitle_size = subtitle_size,
  legend_text_size = legend_text_size,
  legend_title_size = legend_title_size,
  x_offset = x_offset,
  x_lim_lo = x_lim_lo,
  x_lim_hi =  x_lim_hi,
  order = "alphabetical"
)
# view
plot_group_tab_social_1

# save graph
ggsave(
  plot_group_tab_social_1,
  path = here::here(here::here(push_mods, "figs")),
  width = 12,
  height = 12,
  units = "in",
  filename = "plot_group_tab_social_1.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# comparative intervention graphs -----------------------------------------
# combo graph parameters
width = 16
height = 16

# plots
plot_compare_health <- plot_group_tab_health + plot_group_tab_health_1 +
  plot_annotation(title = "Shift Intervention Comparisions", tag_level = "A")
# view
plot_compare_health
ggsave(
  plot_compare_health,
  path = here::here(here::here(push_mods, "figs")),
  width = width,
  height = height,
  units = "in",
  filename = "plot_compare_health.png",
  device = 'png',
  dpi = 600
)
# make
plot_compare_body <- plot_group_tab_body + plot_group_tab_body_1  +
  plot_annotation(title = "Shift Intervention Comparisions", tag_level = "A")
# view
plot_compare_body
ggsave(
  plot_compare_body,
  path = here::here(here::here(push_mods, "figs")),
  width = width,
  height = height,
  units = "in",
  filename = "plot_compare_body.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)
dev.off()


# make
plot_compare_ego <- plot_group_tab_ego + plot_group_tab_ego_1 +
  plot_annotation(title = "Shift Intervention Comparisions", tag_level = "A")

# view
plot_compare_ego
ggsave(
  plot_compare_ego,
  path = here::here(here::here(push_mods, "figs")),
  width = width,
  height = height,
  units = "in",
  filename = "plot_compare_ego.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# make
plot_compare_reflective <- plot_group_tab_reflective + plot_group_tab_reflective_1 +
  plot_annotation(title = "Shift Intervention Comparisions", tag_level = "A")

# view
plot_compare_reflective
ggsave(
  plot_compare_reflective,
  path = here::here(here::here(push_mods, "figs")),
  width = width,
  height = height,
  units = "in",
  filename = "plot_compare_reflective.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

# make
plot_compare_social  <- plot_group_tab_social + plot_group_tab_social_1 +
  plot_annotation(title = "Shift Intervention Comparisions", tag_level = "A")

# view
plot_compare_social
ggsave(
  plot_compare_social,
  path = here::here(here::here(push_mods, "figs")),
  width = width,
  height = height,
  units = "in",
  filename = "plot_compare_social.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

group_tab_health
group_tab_body
group_tab_ego
group_tab_reflective
group_tab_ego

