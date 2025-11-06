# 01-estimating-models-lmtp
# 2025 jan
# joseph.bulbulia@gmail.com
# data preparation for six wave analysis (four exposure waves) for psychopathy effects on partner well-being
# restart in fresh session
rstudioapi::restartSession()

# data path (local - jb only)
push_mods <- here::here('/Users/joseph/v-project\ Dropbox/data/25-aaron-psycho-wb-long')

# reproducibility
set.seed(123)

# essential library - install if needed. 
# detach("package:margot", unload = TRUE)
# devtools::install_github("go-bayes/margot")
library(margot)

# check package versionn
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
  data.table,
  psych
  )
## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE. DO NOT USE THIS PATH
# pull path (customise for your own computer) ----------------------------
pull_path <-here::here("/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00-nazvs-data-backup/r/2015")

# read data
dat <- margot::here_read_qs("nzavs_data", pull_path)

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

# emotional stability
emotional_stability_vars_orig <- c(
  "pers_n_ipip02r",    # *r* "Am relaxed most of the time"
  "pers_n_ipip04r",    # *r* "Seldom feel blue"
  "vengeful_rumination01", # "Sometimes I can't sleep because of thinking about past wrongs I have suffered"
  "vengeful_rumination03", # "I find myself regularly thinking about past times that I have been wronged"
  "self_control01",    # "In general, I have a lot of self-control"
  "rel_belong02r"      # "Feel like an outsider"
)

# disinhibition
disinhibition_vars_orig <- c(
  "pers_c_ipip03r",    # *r* "Make a mess of things"
  "pers_n_ipip01",     # "Have frequent mood swings"
  "pers_c_ipip04r",    # *r* "Often forget to put things back in their proper place"
  "pers_c_ipip01",     # *r* "Get chores done right away"
  "police_engagement02", # *r* "I would always provide information to the police to help them find someone suspected of committing a crime"
  "pers_hon_hum04r"    # *r* "Would get a lot of pleasure from owning expensive luxury goods"
)

# narcissism  
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

# calculate scales using 'aaron_' prefixed scored variables
dt[, aaron_antagonism := rowMeans(.SD, na.rm = TRUE), .SDcols = antagonism_vars]
dt[, aaron_emotional_stability := rowMeans(.SD, na.rm = TRUE), .SDcols = emotional_stability_vars]
dt[, aaron_disinhibition := rowMeans(.SD, na.rm = TRUE), .SDcols = disinhibition_vars]
dt[, aaron_narcissism := rowMeans(.SD, na.rm = TRUE), .SDcols = narcissism_vars]

# create global psychopathy score
dt[, aaron_psychopathy_combined := (aaron_antagonism + aaron_emotional_stability + 
                                      aaron_disinhibition + aaron_narcissism) / 4]

print("scales created successfully")

#  validation checks for original scale variables
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
here_save_qs(dt, "dt", push_mods)

# proceed with omega reliabilities

# define reversed variables for each scale
antagonism <-  antagonism_vars
emotional_stability <-  emotional_stability_vars
disinhibition <- disinhibition_vars
narcissism <- narcissism_vars

# get baseline wave
dt_2018 <- dt[wave %in% "2018"]

# compute reliabilities
antagonism_reliability <- psych::omega(dt_2018[, ..antagonism])$omega.tot
emotional_stability_reliability <- psych::omega(dt_2018[, ..emotional_stability])$omega.tot
disinhibition_reliability <- psych::omega(dt_2018[, ..disinhibition])$omega.tot
narcissism_reliability <- psych::omega(dt_2018[, ..narcissism])$omega.tot



# compute total psychopathy reliability
all_vars_use<- c(antagonism, emotional_stability, 
                       disinhibition, narcissism)

# check total
total_psychopathy_reliability <- psych::omega(dt_2018[, ..all_vars_use], check.keys=TRUE)$omega.tot

# display results
cat("Reliabilities 2018 (omega):\n")
cat("Antagonism: ", round(antagonism_reliability, 2), "\n")
cat("Emotional Stability: ", round(emotional_stability_reliability, 2), "\n")
cat("Disinhibition: ", round(disinhibition_reliability, 2), "\n")
cat("Narcissism: ", round(narcissism_reliability, 2), "\n")
cat("Total Psychopathy: ", round(total_psychopathy_reliability, 2), "\n")



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
  
# calculate multiple reliability indices
  alpha_result <- psych::alpha(dt[, ..vars])
  
  cat("\nScale:", scale_name, "\n")
  cat("Cronbach's alpha:", round(alpha_result$total$std.alpha, 3), "\n")
  cat("Item correlations range:", 
      round(min(alpha_result$item.stats$r.drop), 3), "to",
      round(max(alpha_result$item.stats$r.drop), 3), "\n")
}

# get items
disinhibition_items <- dt_2018[, ..disinhibition]
cor_matrix <- cor(disinhibition_items, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# check for potential problematic items
item_stats <- psych::alpha(disinhibition_items)$item.stats
print(item_stats[order(item_stats$r.drop), ])

# check item reliabilities
library(performance)
performance::item_reliability(dt[, ..antagonism])
performance::item_reliability(dt[, ..emotional_stability])
performance::item_reliability(dt[, ..disinhibition])
performance::item_reliability(dt[, ..narcissism])

# end ---------------------------------------------------------------------

# data wrangling ----------------------------------------------------------
# read data if needed
dt <- margot::here_read_qs("dt", push_mods)

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

# check numbers
counts <- margot::margot_count_dyads(dat, start_wave = 2009, end_wave = 2023, year_measured_val = 1, prev_wave_counts = c(1:14))

# define waves
library(dplyr)

# define waves and save
baseline_wave <- "2018"
exposure_waves <- c("2019", "2020", "2021","2022")
outcome_wave <- "2023"

# all waves
all_waves = c(baseline_wave, exposure_waves, outcome_wave)

# save vars
here_save(all_waves, "all_waves")
here_save(baseline_wave, "baseline_wave")
here_save(exposure_waves, "exposure_waves")
here_save(outcome_wave, "outcome_wave")

# need to load haven to change labels (!)
library(haven)

# start here if needed ----------------------------------------------------
# read back data
dt <- here_read_qs("dt")

# make data selecting correct status of partner
# this is important because to emulate an experiment, we cannot know in advance who will break up
# we will censor those that do

# make data
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


# status to filter
filter_statuses <- c("Other Single incl. Casual", "Single", "Divorced/Separated", "Engaged", "Widow/Widower")

# filter function
filter_relationship_status <- function(data, specified_wave) {
  data %>%
    filter(wave == specified_wave) %>%
    #filter(
     # !(rel_status_l3 %in% filter_statuses),
      # !is.na(aaron_psychopathy_combined),
      # !is.na(aaron_disinhibition),
      # !is.na(aaron_narcissism)
    #) %>%
    pull(id)
}

# check
table(dt_t_final$rel_complete)


#  id's for baseline (2018)
ids_baseline <- dt_t_final %>%
  filter(year_measured == 1, 
         wave == baseline_wave, 
        !is.na(rel_num),
        # !(rel_status_l3 %in% filter_statuses),
        # !is.na(aaron_psychopathy_combined),
        # !is.na(aaron_disinhibition),
        # !is.na(aaron_narcissism),
         rel_complete == 1)%>%
  pull(id)


#  final study IDs (participants in both 2018 and 2019)
ids_study <- ids_baseline #intersect(ids_baseline, ids_exposure)

#  number of participants
cat("Number of participants in 2018", length(ids_study), "\n")

# dataset for 2018 and 2019
dat_long_init <- dt_t_final %>%
  filter(id %in% ids_study, wave %in% all_waves) %>%
  droplevels()

# summary of n by each wave -----------------------------------------------
# summary by wave
summary_by_wave <- dat_long_init %>%
  group_by(wave) %>%
  summarise(
    n_relationships = n_distinct(rel_num),
    n_participants = n_distinct(id)
  )


# this shows us the N by each wave
print(summary_by_wave)

# checks
n_distinct(dat_long_init$rel_num)

# define exposure
nzavs_exposure <- "aaron_psychopathy_combined"
exposure_var = c("aaron_psychopathy_combined", "not_lost") #

# data
table(dat_long_init$wave)


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
mismatched_couples <- find_mismatched_couples(dat_long_init)

# check
mismatched_couples

# order / display all results
ordered_results <- mismatched_couples %>%
  select(rel_num, id, wave, rel_status_l3, rel_complete) %>%
  arrange( wave,rel_num, id)

# print rows
print(ordered_results, n = Inf)


# count of mismatched couples by wave
mismatched_summary <- mismatched_couples %>%
  group_by(wave) %>%
  summarise(n_mismatched = n_distinct(rel_num))

print("Number of couples with mismatched status by wave:")
print(mismatched_summary)

# total count of unique mismatched coup ched_couples$rel_num)
# print(paste("Total number of unique couples with mismatched status across all waves:", n_total_mismatched))


# create long data --------------------------------------------------------
library(dplyr)
dat_long <- dat_long_init |> 
  # ungroup
  select(
    "id",
    "rel_complete",
    "wave",
    "year_measured",
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
    #"partner", # not relevant because  we are only looking at those with partners at baseline ******* 
    # 0 = no, 1 = yes
    "parent",
    # 0 = no, 1 = yes
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
    "rel_status_l3",
    "religion_church_binary"
  ) |>
  mutate(
    eth_cat = as.integer(eth_cat),
    urban = as.numeric(urban),
    education_level_coarsen = as.integer(education_level_coarsen)
  ) |>
  mutate(
    not_lost = ifelse(lead(year_measured) == 1, 1, 0),
    not_lost = ifelse(is.na(not_lost) &
                        year_measured == 1, 1, not_lost),
    not_lost = ifelse(is.na(not_lost), 0, not_lost)
  ) |>
  ungroup() |>
  dplyr::mutate(
    household_inc_log = log(household_inc + 1),
    hours_work_log = log(hours_work + 1),
    hours_housework_log = log(hours_housework + 1)#,
  ) |>
  select(-c("household_inc", "hours_housework", "hours_work"))|>
  arrange(id, wave) |>
  droplevels() |>
  mutate(
    urban = as.numeric(urban)
      ) |>
  arrange(id, wave) |>
  droplevels() |>
  data.frame()

# check missing data
naniar::vis_miss(dat_long, warn_large_data = TRUE)

# levels
levels(dat_long$wave)

# table of waves
table(dat_long$wave)

# check sample 
n_participants <-n_unique(dat_long$id) #

# participants in the study
n_participants

# save value
here_save(n_participants, "n_participants")

test<- dat_long |> 
  filter(wave==2018) |> select(aaron_psychopathy_combined)

# check missing
table(is.na(test$aaron_psychopathy_combined))

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
  #factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
  #"bigger_doms", #religious denomination
  # "sample_origin",
  "nz_dep2018",
  "hlth_disability",
  # "nzsei13",
  # "born_nz",
  "kessler6_sum",
  "kessler_latent_depression",
  "kessler_latent_anxiety",
  "political_conservative",
  "religion_church_binary",
  "urban",
  "parent",
  "hours_housework_log",
  "hours_work_log",
  "sample_weights",
  "alert_level_combined_lead",
  "rel_num"
)

# check (NOTE - removed 'partner' as not needed or control)
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
  "lifesat",
  "pwi",
  "self_esteem"
)

# conditional censoring ---------------------------------------------------------

## get rid of haven labels
dat_long <- as.data.frame(dat_long)
dat_long <- haven::zap_formats(dat_long)
dat_long <- haven::zap_label(dat_long)
dat_long <- haven::zap_widths(dat_long)

str(dat_long)
dat_long[, "rel_complete"]
dat_long[, "year_measured"]


# conditional censor if attrition
dat_long <- dat_long |> droplevels() |> arrange(id, wave)


# dat_long to a data.table in place
setDT(dat_long)
setorder(dat_long, id, rel_num, wave)
dat_long[, rel_num := as.factor(rel_num)]

# count of missing 'rel_num' entries
missing_rel_num <- dat_long[is.na(rel_num), .N]
cli::cli_alert_info("There are {missing_rel_num} missing values in the 'rel_num' variable.")

# check whether 'dat_long' is a data.table
if (!is.data.table(dat_long)) {
  setDT(dat_long)  # Convert to data.table by reference
  cli::cli_alert_info("Converted 'dat_long' to a data.table.")
} else {
  cli::cli_alert_info("'dat_long' is already a data.table.")
}

# verify that the 'wave' variable exists
if (!"wave" %in% names(dat_long)) {
  cli::cli_abort("The 'wave' variable does not exist in 'dat_long'. Please check the variable name.")
} else {
  cli::cli_alert_info("'wave' variable found in 'dat_long'.")
}

# check the type of 'wave' variable
if (!is.numeric(dat_long$wave) && !is.factor(dat_long$wave) && !is.character(dat_long$wave)) {
  cli::cli_warn("'wave' variable is not of type numeric, factor, or character. Consider converting it for accurate analysis.")
}

total_missing_rel_num <- dat_long[is.na(rel_num), .N]
cli::cli_alert_info("Total missing 'rel_num' values: {total_missing_rel_num}")

# count of missing 'rel_num' values grouped by each distinct wave value
missing_rel_num_by_wave <- dat_long[, .(Missing_rel_num_Count = sum(is.na(rel_num))), by = wave]

# result
missing_rel_num_by_wave

# OR, inspect rows with missing 'rel_num'
losses <-missing_rel_num_by_wave$Missing_rel_num_Count
abs(losses- 1832)

# use margot package conditional censoring
dat_long_censored  <- margot_censor(
  dt = dat_long,
  id_var = "id",
  cluster_id = "rel_num",
  wave_var = "wave",
  condition_var = "rel_complete",
  condition_value = 0,
  year_measured_var = "year_measured",
  censor_final_wave = TRUE
  )

# check
table(is.na(dat_long_censored$year_measured))

# check
table(dat_long_censored$year_measured)

# checks
cols_test_orig_1 <- dat_long_censored[, .(cols_test = sum(rel_complete==0)), by = wave]
cols_test_orig_1

cols_test_orig_1a <- dat_long_censored[, .(cols_test = sum(rel_complete==1)), by = wave]
cols_test_orig_1a

cols_test_orig_1a[ ,cols_test] - cols_test_orig_1[, cols_test]


cols_test_orig_2 <- dat_long_censored[, .(cols_test = sum(year_measured ==1)), by = wave]
cols_test_orig_2

cols_test <- dat_long_censored[, .(cols_test = sum(rel_complete==1)), by = wave]
cols_test

cols_test_2 <- dat_long_censored[, .(cols_test = sum(year_measured ==1)), by = wave]
cols_test_2

head(dat_long_censored)


# check 
naniar::vis_miss(dat_long_censored)

# compare
naniar::vis_miss(dat_long)

# save censored data
dat_long_censored<- as.data.frame(dat_long_censored)
dat_long_censored <- dat_long_censored |> arrange(id, rel_num, wave)
head(dat_long_censored)

# save
here_save_qs(dat_long_censored, "dat_long_censored", push_mods)

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
  #Please rate how politically liberal versus conservative you see yourself as being.
  # Sample origin names combined
  "urban_binary",
  "hours_housework_log",
  "hours_work_log",
  # "sample_weights",
  "alert_level_combined_lead",
  "rel_num")
baseline_vars
# sort and save
baseline_vars_binary <- sort(baseline_vars_binary)
baseline_vars_binary
here_save(baseline_vars_binary, "baseline_vars_binary")

# use correct df ----------------------------------------------------------
# df_long_censored if that's needed 
dat_long_prepare<- margot::margot_process_binary_vars(dat_long_censored, exceptions = c("not_lost", "rel_complete", "religion_church_binary" ))
colnames(dat_long_prepare)

# check
vis_miss(dat_long_prepare)

# save weights NO WEIGHTS USED IN THIS STUDY
sort(colnames( dat_long_prepare))
dat_long_prepare <- as.data.frame(dat_long_prepare)
here_save_qs(dat_long_prepare, "dat_long_prepare", push_mods)

# define extra vars for data wrangling later
extra_vars <- c("id", "wave", "year_measured", "not_lost") # gender_weights not used/ just an option


# define columns that will later be handled in a special way
continuous_columns_keep <- c("t0_rel_num", "t0_alert_level_combined_lead")
here_save(continuous_columns_keep, "continuous_columns_keep")

# save
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
t2_name_exposure <- paste0("t2_", exposure_var_all)
t3_name_exposure <- paste0("t3_", exposure_var_all)
t4_name_exposure <- paste0("t4_", exposure_var_all)


t0_name_exposure_partner <- paste0("t0_partner_", exposure_var_all)
t1_name_exposure_partner <- paste0("t1_partner_", exposure_var_all)
t2_name_exposure_partner <- paste0("t2_partner_", exposure_var_all)
t3_name_exposure_partner <- paste0("t3_partner_", exposure_var_all)
t4_name_exposure_partner <- paste0("t4_partner_", exposure_var_all)


# save names
here_save(t0_name_exposure , 't0_name_exposure')
here_save(t1_name_exposure , 't1_name_exposure')
here_save(t2_name_exposure , 't2_name_exposure')
here_save(t3_name_exposure , 't3_name_exposure')
here_save(t4_name_exposure , 't4_name_exposure')


here_save(t0_name_exposure_partner , 't0_name_exposure_partner')
here_save(t1_name_exposure_partner , 't1_name_exposure_partner')
here_save(t2_name_exposure_partner , 't2_name_exposure_partner')
here_save(t3_name_exposure_partner , 't3_name_exposure_partner')
here_save(t4_name_exposure_partner , 't4_name_exposure_partner')

# save
here_save(extra_vars, "extra_vars")
here_save(exposure_var_all, "exposure_var_all")
here_save(outcome_vars, "outcome_vars")
exposure_var
# define all vars
all_vars <- c(baseline_vars_binary, exposure_var_all, outcome_vars, extra_vars)

# save
here_save(all_vars, "all_vars")

# view
t1_name_exposure_partner

# put in alphabetical order
outcome_vars <- sort(outcome_vars)

# save
here_save(outcome_vars, "outcome_vars")

# save wave values
here_save(baseline_wave , 'baseline_wave')
here_save(exposure_waves , 'exposure_waves')
here_save(outcome_wave , 'outcome_wave')

# checks
exposure_var_all <- here_read("exposure_var_all")
exposure_var_all
dat_long_prepare <- here_read_qs("dat_long_prepare", push_mods)
baseline_vars_binary
scale_range_exposure <- c(1, 7)
scale_range_exposure


individual_plot_aaron_antagonism <- margot::margot_plot_individual_responses(
  dat_long_prepare,
  y_vars = "aaron_antagonism",
  id_col = "id",
  waves = c(2018:2022),
  theme = theme_classic(),
  random_draws =81,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)


individual_plot_aaron_disinhibition <- margot::margot_plot_individual_responses(
  dat_long_prepare,
  y_vars = "aaron_disinhibition",
  id_col = "id",
  waves = c(2018:2022),
  theme = theme_classic(),
  random_draws =81,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)
individual_plot_aaron_disinhibition



individual_plot_aaron_emotional_stability <- margot::margot_plot_individual_responses(
  dat_long_prepare,
  y_vars = "aaron_emotional_stability",
  id_col = "id",
  waves = c(2018:2022),
  theme = theme_classic(),
  random_draws =81,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)


individual_plot_aaron_narcissism <- margot_plot_individual_responses(
  dat_long_prepare,
  y_vars = "aaron_narcissism",
  id_col = "id",
  waves = c(2018:2022),
  theme = theme_classic(),
  random_draws =81,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)


individual_plot_aaron_psychopathy_combined <- margot_plot_individual_responses(
  dat_long_prepare,
  y_vars = "aaron_psychopathy_combined",
  id_col = "id",
  waves = c(2018:2022),
  theme = theme_classic(),
  random_draws =81,
  title = NULL,
  y_label = NULL,
  x_label = NULL,
  color_palette = NULL,
  include_timestamp = FALSE,
  save_path = here::here(push_mods),
  width = 16,
  height = 8,
  seed = 123,
  full_response_scale = TRUE,
  scale_range = scale_range_exposure
)

# view
hist(dat_long_prepare$aaron_psychopathy_combined)

# calculate mean and SD
mean_score <- mean(dat_long_prepare$aaron_psychopathy_combined, na.rm = TRUE)
sd_score <- sd(dat_long_prepare$aaron_psychopathy_combined, na.rm = TRUE)

histogram_aaron_psychopathy_combined <- ggplot(data = dat_long_prepare, aes(x = aaron_psychopathy_combined)) +
  geom_histogram(fill = "steelblue", color = "black") +
  # Scale limits as solid pale lines
  geom_vline(xintercept = c(1, 7), color = "gold", linewidth = 1) +
  # Mean as dashed red line
  geom_vline(xintercept = mean_score, color = "red", linetype = "dashed") +
  # Standard deviations as dashed red lines
  geom_vline(xintercept = c(mean_score - sd_score, mean_score + sd_score), 
             color = "red", linetype = "dashed") +
  # Annotations
  annotate("text", x = mean_score, y = Inf, label = paste("Mean =", round(mean_score, 2)),
           vjust = 2, color = "red") +
  annotate("text", x = mean_score - sd_score, y = Inf, 
           label = paste("-1 SD =", round(mean_score - sd_score, 2)),
           vjust = 2, color = "red") +
  annotate("text", x = mean_score + sd_score, y = Inf,
           label = paste("+1 SD =", round(mean_score + sd_score, 2)),
           vjust = 2, color = "red") +
  xlim(1, 7) +
  theme_minimal() +
  labs(x = "Aaron Psychopathy Score",
       y = "Count",
       title = "Distribution of Aaron Psychopathy Scores",
       subtitle = "Yellow marks theoretical scale range")

histogram_aaron_psychopathy_combined

margot_save_png(histogram_aaron_psychopathy_combined, width = 16, height = 9, base_filename = "histogram_aaron_psychopathy_combined", save_path = here::here(push_mods))

# view graphs
individual_plot_aaron_antagonism
individual_plot_aaron_disinhibition
individual_plot_aaron_emotional_stability
individual_plot_aaron_narcissism
individual_plot_aaron_psychopathy_combined


basline_table_vars <- setdiff(baseline_vars, c("sample_weights", "rel_num", "alert_level_combined_lead"))

# for imputation model
here_save(basline_table_vars, "basline_table_vars")


# needed
library(haven)

# mutate haven labels
dat_table_converted <- dat_long %>%
  mutate(rel_status_l3 = as_factor(rel_status_l3)) |> 
  select(-rel_num) |> 
  rename(antagonism = aaron_antagonism, 
         disinhibition = aaron_disinhibition,
         emotional_stability = aaron_emotional_stability, 
         narcissism = aaron_narcissism, 
         psychopathy_combined = aaron_psychopathy_combined
  )


exposure_vars_all = c(
  "antagonism",
  "emotional_stability",
  "disinhibition", 
  "narcissism",
  "psychopathy_combined"
)


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
table(dat_table_converted$wave)
exposure_waves
# new graphs ---------------------------------------------------------
# evaluate exposure variable/s
dat_long_exposure <- dat_table_converted |>
  filter(wave %in% exposure_waves) |>
  droplevels()

# view
head(dat_long_exposure)


# you can check quantile breaks this way
# ********* CHECK ************** #
result <- lapply(exposure_vars_all, function(var) {
  quantiles <- quantile(dat_long_exposure[[var]], na.rm = TRUE, probs = seq(0, 1, .25))
  setNames(list(quantiles), var)
})

print(result)


# antagonism --------------------------------------------------------------
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

# shift_exposure_up <- here_read_qs('disinhibition_up', push_mods)



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


# summary tables ----------------------------------------------------------
# check
baseline_vars
basline_table_vars

exposure_vars_table = c(exposure_vars_all, "rel_complete")

# old tables
summary_tables <- margot::margot_summary_tables(
  data = dat_table_converted,
  baseline_wave = baseline_wave,
  exposure_waves = exposure_waves,
  outcome_wave = outcome_wave,
  name_exposure = exposure_vars_table,
  baseline_vars = basline_table_vars,
  outcome_vars = outcome_vars,
  extra_vars = extra_vars,
  create_plots = FALSE,
  plot_type = "boxplot"
)               


summary_tables$outcome_table |> kbl(formate = "pipe")
summary_tables$exposure_tables$antagonism
margot::here_save_qs(summary_tables, "summary_tables", push_mods)

summary_tables$exposure_tables$rel_complete


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


push_mods
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

transition_matrices[[4]]