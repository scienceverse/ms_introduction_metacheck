# ============= Run a Bayesian hierarchical model =============

# Following Oberauer et al. (2017), the current script performs the 
# two-parameter mixture model (Zhang & Luck, 2008) in a hierarchical manner
# We run 6 chains with 6000 iterations each (3000 warmup iterations per chain)
# Note that each model takes about an 40 min to run.

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
remove_outliers = TRUE
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

# Run the model seperately for each condition in each experiment and save in designated folder

# -----------------------------------------------------------

# Experiment 1 - LTM
data_list_Exp1_LTM = as.list(subset(Data_Exp1,cond==1))
data_list_Exp1_LTM$N = length(data_list_Exp1_LTM$subject) # N trials
data_list_Exp1_LTM$n_subjects = length(unique(data_list_Exp1_LTM$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp1_LTM <- stan(file = "mixture_multi_level.stan", 
                               data = data_list_Exp1_LTM, iter=6000, chains = 6)
save(mixture_multi_level_Exp1_LTM, file = "Stan_objects/mixture_multi_level_Exp1_LTM.RData")
remove("data_list_Exp1_LTM")
remove("mixture_multi_level_Exp1_LTM")

# -----------------------------------------------------------

# Experiment 1 - WM
data_list_Exp1_WM = as.list(subset(Data_Exp1,cond==2))
data_list_Exp1_WM$N = length(data_list_Exp1_WM$subject) # N trials
data_list_Exp1_WM$n_subjects = length(unique(data_list_Exp1_WM$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp1_WM <- stan(file = "mixture_multi_level.stan", 
                                     data = data_list_Exp1_WM, iter=6000, chains = 6)
save(mixture_multi_level_Exp1_WM, file = "Stan_objects/mixture_multi_level_Exp1_WM.RData")
remove("data_list_Exp1_WM")
remove("mixture_multi_level_Exp1_WM")

# -----------------------------------------------------------

# Experiment 2 - LTM
data_list_Exp2_LTM = as.list(subset(Data_Exp2,cond==1))
data_list_Exp2_LTM$N = length(data_list_Exp2_LTM$subject) # N trials
data_list_Exp2_LTM$n_subjects = length(unique(data_list_Exp2_LTM$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_LTM <- stan(file = "mixture_multi_level.stan", 
                                     data = data_list_Exp2_LTM, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_LTM, file = "Stan_objects/mixture_multi_level_Exp2_LTM.RData")
remove("data_list_Exp2_LTM")
remove("mixture_multi_level_Exp2_LTM")

# -----------------------------------------------------------

# Experiment 2 - WM
data_list_Exp2_WM = as.list(subset(Data_Exp2,cond==2))
data_list_Exp2_WM$N = length(data_list_Exp2_WM$subject) # N trials
data_list_Exp2_WM$n_subjects = length(unique(data_list_Exp2_WM$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WM <- stan(file = "mixture_multi_level.stan", 
                                    data = data_list_Exp2_WM, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WM, file = "Stan_objects/mixture_multi_level_Exp2_WM.RData")
remove("data_list_Exp2_WM")
remove("mixture_multi_level_Exp2_WM")

# -----------------------------------------------------------

# Experiment 2 - aggregated WM estimate across two WM blocks
data_list_Exp2_WM_agg_2blocks = as.list(Data_Exp2_WMreports)
data_list_Exp2_WM_agg_2blocks$N = length(data_list_Exp2_WM_agg_2blocks$subject) # N trials
data_list_Exp2_WM_agg_2blocks$n_subjects = length(unique(data_list_Exp2_WM_agg_2blocks$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WM_agg_2blocks <- stan(file = "mixture_multi_level.stan", 
                                    data = data_list_Exp2_WM_agg_2blocks, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WM_agg_2blocks, file = "Stan_objects/mixture_multi_level_Exp2_WM_agg_2blocks.RData")
remove("data_list_Exp2_WM_agg_2blocks")
remove("mixture_multi_level_Exp2_WM_agg_2blocks")

# -----------------------------------------------------------

# Experiment 2 - WM 1st report
data_list_Exp2_WMreport1 = as.list(subset(Data_Exp2_WMreports,cond==1))
data_list_Exp2_WMreport1$N = length(data_list_Exp2_WMreport1$subject) # N trials
data_list_Exp2_WMreport1$n_subjects = length(unique(data_list_Exp2_WMreport1$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WM_report1 <- stan(file = "mixture_multi_level.stan", 
                                    data = data_list_Exp2_WMreport1, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WM_report1, file = "Stan_objects/mixture_multi_level_Exp2_WM_report1.RData")
remove("data_list_Exp2_WMreport1")
remove("mixture_multi_level_Exp2_WM_report1")

# -----------------------------------------------------------

# Experiment 2 - WM 2nd report
data_list_Exp2_WMreport2 = as.list(subset(Data_Exp2_WMreports,cond==2))
data_list_Exp2_WMreport2$N = length(data_list_Exp2_WMreport2$subject) # N trials
data_list_Exp2_WMreport2$n_subjects = length(unique(data_list_Exp2_WMreport2$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WM_report2 <- stan(file = "mixture_multi_level.stan", 
                                            data = data_list_Exp2_WMreport2, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WM_report2, file = "Stan_objects/mixture_multi_level_Exp2_WM_report2.RData")
remove("data_list_Exp2_WMreport2")
remove("mixture_multi_level_Exp2_WM_report2")

# -----------------------------------------------------------

# Experiment 2 - WM 3rd report
data_list_Exp2_WMreport3 = as.list(subset(Data_Exp2_WMreports,cond==3))
data_list_Exp2_WMreport3$N = length(data_list_Exp2_WMreport3$subject) # N trials
data_list_Exp2_WMreport3$n_subjects = length(unique(data_list_Exp2_WMreport3$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp2_WM_report3 <- stan(file = "mixture_multi_level.stan", 
                                            data = data_list_Exp2_WMreport3, iter=6000, chains = 6)
save(mixture_multi_level_Exp2_WM_report3, file = "Stan_objects/mixture_multi_level_Exp2_WM_report3.RData")
remove("data_list_Exp2_WMreport3")
remove("mixture_multi_level_Exp2_WM_report3")

# -----------------------------------------------------------

# Experiment 3 - Short list
data_list_Exp3_Short = as.list(subset(Data_Exp3,cond==1))
data_list_Exp3_Short$N = length(data_list_Exp3_Short$subject) # N trials
data_list_Exp3_Short$n_subjects = length(unique(data_list_Exp3_Short$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp3_Short <- stan(file = "mixture_multi_level.stan", 
                                     data = data_list_Exp3_Short, iter=6000, chains = 6)
save(mixture_multi_level_Exp3_Short, file = "Stan_objects/mixture_multi_level_Exp3_Short.RData")
remove("data_list_Exp3_Short")
remove("mixture_multi_level_Exp3_Short")

# -----------------------------------------------------------

# Experiment 3 - Long list
data_list_Exp3_Long = as.list(subset(Data_Exp3,cond==2))
data_list_Exp3_Long$N = length(data_list_Exp3_Long$subject) # N trials
data_list_Exp3_Long$n_subjects = length(unique(data_list_Exp3_Long$subject)) # n subjects
# run the model
options(mc.cores = parallel::detectCores()) # detect cores and run in parallel
mixture_multi_level_Exp3_Long <- stan(file = "mixture_multi_level.stan", 
                                    data = data_list_Exp3_Long, iter=6000, chains = 6)
save(mixture_multi_level_Exp3_Long, file = "Stan_objects/mixture_multi_level_Exp3_Long.RData")
remove("data_list_Exp3_Long")
remove("mixture_multi_level_Exp3_Long")
