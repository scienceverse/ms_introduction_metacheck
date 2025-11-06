# ============= Run a Bayesian hierarchical model =============

# Following Oberauer et al. (2017), the current script performs the 
# two-parameter mixture model (Zhang & Luck, 2008) in a hierarchical manner
# This code includes condition type (e.g., WM vs LTM) within the model.
# We run 6 chains with 6000 iterations each (3000 warmup iterations per chain)
# Note that each model takes about a hour to run.

# The code was written by Dan Biderman and Natalie Biderman, June 2018

# -----------------------------------------------------------

rm(list=ls())
current_folder = getwd() # to get the correct working directory, you will need to restart RStudio and open this script from the folder of Experiment 2
setwd(current_folder)

# load packages
library("rstan")

# Load data
Data_Exp1 = read.csv(paste0(current_folder,'/Data/Data_Exp1.csv'),header = TRUE)
Data_Exp2 = read.csv(paste0(current_folder,'/Data/Data_Exp2_WM_LTM.csv'),header = TRUE)
Data_Exp2_WMreports = read.csv(paste0(current_folder,'/Data/Data_Exp2_WM_reports.csv'),header = TRUE)
Data_Exp3 = read.csv(paste0(current_folder,'/Data/Data_Exp3.csv'),header = TRUE)

# Becuase of rounding, we have 7 trials with color larger than 2*pi, so we turn them to 2*pi
Data_Exp1[which(c(Data_Exp1[,3]>2*pi)),3] = 2*pi

# Remove outliers from data
remove_outliers = FALSE
if (remove_outliers==TRUE) {
  # Remove outliers (found separately in the Matlab analysis code) from data
  Data_Exp1 = Data_Exp1[!Data_Exp1$subject %in% c(22), ]
  Data_Exp2 = Data_Exp2[!Data_Exp2$subject %in% c(1,5,22), ]
  Data_Exp2_WMreports = Data_Exp2_WMreports[!Data_Exp2_WMreports$subject %in% c(1,5,22), ]
  Data_Exp3 = Data_Exp3[!Data_Exp3$subject %in% c(21), ]
  # Reorder subjects numbers to comform the model requirements
  subs_exp1 = unique(Data_Exp1[,1])
  for (sub in 1:length(subs_exp1)){
    Data_Exp1[Data_Exp1$subject==subs_exp1[sub],1] = sub
  }
  subs_exp2 = unique(Data_Exp2[,1])
  for (sub in 1:length(subs_exp2)){
    Data_Exp2[Data_Exp2$subject==subs_exp2[sub],1] = sub
    Data_Exp2_WMreports[Data_Exp2_WMreports$subject==subs_exp2[sub],1] = sub
  }
  subs_exp3 = unique(Data_Exp3[,1])
  for (sub in 1:length(subs_exp3)){
    Data_Exp3[Data_Exp3$subject==subs_exp3[sub],1] = sub
  }
}

# Run the model while including condition type. 

# -----------------------------------------------------------
# Experiment 1 : WM vs. LTM
# -----------------------------------------------------------

# Perpare data
data_list_Exp1 = as.list(Data_Exp1)
data_list_Exp1$N_trials = length(data_list_Exp1$subject) # N trials
data_list_Exp1$n_subjects = length(unique(data_list_Exp1$subject)) # n subjects
data_list_Exp1$n_cond = length(unique(data_list_Exp1$cond)) # n conditions

# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp1 <- stan(file = "mixture_multi_level_cond.stan", 
                               data = data_list_Exp1, iter=6000, chains = 6)
save(mixture_multi_level_Exp1, file = "Stan_objects/mixture_multi_level_Exp1.RData")
remove("data_list_Exp1")
remove("mixture_multi_level_Exp1")

# -----------------------------------------------------------
# Experiment 2 - WM vs. LTM
# -----------------------------------------------------------

# Perpare data
data_list_Exp2 = as.list(Data_Exp2)
data_list_Exp2$N_trials = length(data_list_Exp2$subject) # N trials
data_list_Exp2$n_subjects = length(unique(data_list_Exp2$subject)) # n subjects
data_list_Exp2$n_cond = length(unique(data_list_Exp2$cond)) # n conditions

# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2 <- stan(file = "mixture_multi_level_cond.stan", 
                                     data = data_list_Exp2, iter=6000, chains = 6)
save(mixture_multi_level_Exp2, file = "Stan_objects/mixture_multi_level_Exp2.RData")
remove("data_list_Exp2")
remove("mixture_multi_level_Exp2")

# -----------------------------------------------------------
# Experiment 2 - WM reports
# -----------------------------------------------------------

# Perpare data
data_list_Exp2_WMreports = as.list(Data_Exp2_WMreports)
data_list_Exp2_WMreports$N_trials = length(data_list_Exp2_WMreports$subject) # N trials
data_list_Exp2_WMreports$n_subjects = length(unique(data_list_Exp2_WMreports$subject)) # n subjects
data_list_Exp2_WMreports$n_cond = length(unique(data_list_Exp2_WMreports$cond)) # n conditions

# Run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WMreports <- stan(file = "mixture_multi_level_cond.stan", 
                                    data = data_list_Exp2_WMreports, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WMreports, file = "Stan_objects/mixture_multi_level_Exp2_WMreports.RData")
remove("data_list_Exp2_WMreports")
remove("mixture_multi_level_Exp2_WMreports")

# -----------------------------------------------------------
# Experiment 3 - Short vs. Long lists
# -----------------------------------------------------------

# Perpare data
data_list_Exp3 = as.list(Data_Exp3)
data_list_Exp3$N_trials = length(data_list_Exp3$subject) # N trials
data_list_Exp3$n_subjects = length(unique(data_list_Exp3$subject)) # n subjects
data_list_Exp3$n_cond = length(unique(data_list_Exp3$cond)) # n conditions

# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp3 <- stan(file = "mixture_multi_level_cond.stan", 
                                     data = data_list_Exp3, iter=6000, chains = 6)
save(mixture_multi_level_Exp3, file = "Stan_objects/mixture_multi_level_Exp3.RData")
remove("data_list_Exp3")
remove("mixture_multi_level_Exp3")
