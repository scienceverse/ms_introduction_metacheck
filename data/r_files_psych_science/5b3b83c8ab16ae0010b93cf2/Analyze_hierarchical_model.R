# ============= Analysis of the Bayesian hierarchical model =============

# Analysis of the ZL Bayesian hierarchical model (Oberauer et al., 2017)
# The current code reads Stan objects and performes the following analyses
# for each condition separately:
#         (1) Conversion of kappa values to SD (degrees) and Pmem to PG
#         (2) Computation of mean and 95% HDI of group-level parameters
#         (3) Computation of mean and 95% HDI of the difference between conditions
# The code was written by Dan Biderman and Natalie Biderman, June 2018

# --------------------------------------------------------

rm(list=ls())
current_folder = getwd() # to get the correct working directory, you will need to restart RStudio and open this script from the folder of Experiment 2
setwd(current_folder)

# Load packages and functions
library("rstan") # install.packages(c("rstan","rstanarm","shinystan","bayesplot","ggplot2","BayesFactor"))
library("rstanarm") 
library("shinystan")
library("bayesplot")
library("ggplot2")
source("Functions.R")

# --------------------------------------------------------
# Experiment 1 : LTM vs. WM
# --------------------------------------------------------

# Load stan objects
stan_object_Exp1_LTM = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp1_LTM.RData")))
posterior_mat_Exp1_LTM_temp = as.matrix(stan_object_Exp1_LTM)
stan_object_Exp1_WM = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp1_WM.RData")))
posterior_mat_Exp1_WM_temp = as.matrix(stan_object_Exp1_WM)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp1_LTM = SD_PG_conversion_mat(posterior_mat_Exp1_LTM_temp,"precision",c("pm","p_mem"))
posterior_mat_Exp1_WM = SD_PG_conversion_mat(posterior_mat_Exp1_WM_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp1_LTM = posterior_summary_table(posterior_mat_Exp1_LTM,c("mean_precision","p_mem"),c("LTM_SD","LTM_PG"))
descriptive_Exp1_WM = posterior_summary_table(posterior_mat_Exp1_WM,c("mean_precision","p_mem"),c("WM_SD","WM_PG"))

# Display posteriors of conditions
# Density
create_non_ggplot_density(list(posterior_mat_Exp1_LTM[,"mean_precision"],
                               posterior_mat_Exp1_WM[,"mean_precision"]))

# Calulate mean_precision difference
SD_diff = posterior_mat_Exp1_LTM[,"mean_precision"] - posterior_mat_Exp1_WM[,"mean_precision"]
mean(SD_diff)
HDIofMCMC(SD_diff)

# --------------------------------------------------------
# Experiment 2 : LTM vs. WM
# --------------------------------------------------------

# Load stan objects
stan_object_Exp2_LTM = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_LTM.RData")))
posterior_mat_Exp2_LTM_temp = as.matrix(stan_object_Exp2_LTM)
stan_object_Exp2_WM = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WM.RData")))
posterior_mat_Exp2_WM_temp = as.matrix(stan_object_Exp2_WM)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp2_LTM = SD_PG_conversion_mat(posterior_mat_Exp2_LTM_temp,"precision",c("pm","p_mem"))
posterior_mat_Exp2_WM = SD_PG_conversion_mat(posterior_mat_Exp2_WM_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp2_LTM = posterior_summary_table(posterior_mat_Exp2_LTM,c("mean_precision","p_mem"),c("LTM_SD","LTM_PG"))
descriptive_Exp2_WM = posterior_summary_table(posterior_mat_Exp2_WM,c("mean_precision","p_mem"),c("WM_SD","WM_PG"))

# Display posteriors of conditions
# Density
create_non_ggplot_density(list(posterior_mat_Exp2_LTM[,"mean_precision"],
                               posterior_mat_Exp2_WM[,"mean_precision"]))

# Calulate mean_precision difference
SD_diff_exp2 = posterior_mat_Exp2_LTM[,"mean_precision"] - posterior_mat_Exp2_WM[,"mean_precision"]
mean(SD_diff_exp2)
HDIofMCMC(SD_diff_exp2)

# --------------------------------------------------------
# Experiment 2 : WM reports
# --------------------------------------------------------

# Load stan objects
stan_object_Exp2_WM_report1 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WM_report1.RData")))
posterior_mat_Exp2_WM_report1_temp = as.matrix(stan_object_Exp2_WM_report1)
stan_object_Exp2_WM_report2 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WM_report2.RData")))
posterior_mat_Exp2_WM_report2_temp = as.matrix(stan_object_Exp2_WM_report2)
stan_object_Exp2_WM_report3 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WM_report3.RData")))
posterior_mat_Exp2_WM_report3_temp = as.matrix(stan_object_Exp2_WM_report3)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp2_WM_report1 = SD_PG_conversion_mat(posterior_mat_Exp2_WM_report1_temp,"precision",c("pm","p_mem"))
posterior_mat_Exp2_WM_report2 = SD_PG_conversion_mat(posterior_mat_Exp2_WM_report2_temp,"precision",c("pm","p_mem"))
posterior_mat_Exp2_WM_report3 = SD_PG_conversion_mat(posterior_mat_Exp2_WM_report3_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp2_WM_report1 = posterior_summary_table(posterior_mat_Exp2_WM_report1,c("mean_precision","p_mem"),c("WMreport1_SD","WMreport1_PG"))
descriptive_Exp2_WM_report2 = posterior_summary_table(posterior_mat_Exp2_WM_report2,c("mean_precision","p_mem"),c("WMreport2_SD","WMreport2_PG"))
descriptive_Exp2_WM_report3 = posterior_summary_table(posterior_mat_Exp2_WM_report3,c("mean_precision","p_mem"),c("WMreport3_SD","WMreport3_PG"))

# Compute difference between the first and third report 
# SD
SD_diff_exp2_reports_1_3 = posterior_mat_Exp2_WM_report3[,"mean_precision"] - posterior_mat_Exp2_WM_report1[,"mean_precision"]
mean(SD_diff_exp2_reports_1_3)
HDIofMCMC(SD_diff_exp2_reports_1_3)
# PG
PG_diff_exp2_reports_1_3 = posterior_mat_Exp2_WM_report3[,"p_mem"] - posterior_mat_Exp2_WM_report1[,"p_mem"]
mean(PG_diff_exp2_reports_1_3)
HDIofMCMC(PG_diff_exp2_reports_1_3)

# Compute difference between the third WM report and LTM 
SD_diff_exp2_3rdReport_LTM = posterior_mat_Exp2_LTM[,"mean_precision"] - posterior_mat_Exp2_WM_report3[,"mean_precision"] 
mean(SD_diff_exp2_3rdReport_LTM)
HDIofMCMC(SD_diff_exp2_3rdReport_LTM)

# --------------------------------------------------------
# Experiment 2 : aggregated WM and the first WM report
# --------------------------------------------------------

# Load stan objects
stan_object_Exp2_WM_agg_2blocks = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WM_agg_2blocks.RData")))
posterior_mat_Exp2_WM_agg_2blocks_temp = as.matrix(stan_object_Exp2_WM_agg_2blocks)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp2_WM_agg_2blocks = SD_PG_conversion_mat(posterior_mat_Exp2_WM_agg_2blocks_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp2_WM_agg_2blocks = posterior_summary_table(posterior_mat_Exp2_WM_agg_2blocks,c("mean_precision","p_mem"),c("LTM_SD","LTM_PG"))

# Calulate difference between WM agg and the first WM report
SD_diff_aggWM_1stWM = posterior_mat_Exp2_WM_agg_2blocks[,"mean_precision"] - posterior_mat_Exp2_WM_report1[,"mean_precision"]
mean(SD_diff_aggWM_1stWM)
HDIofMCMC(SD_diff_aggWM_1stWM)
PG_diff_aggWM_1stWM = posterior_mat_Exp2_WM_agg_2blocks[,"p_mem"] - posterior_mat_Exp2_WM_report1[,"p_mem"]
mean(PG_diff_aggWM_1stWM)
HDIofMCMC(PG_diff_aggWM_1stWM)

# --------------------------------------------------------
# Experiment 3 : Short vs. Long
# --------------------------------------------------------

# Load stan objects
stan_object_Exp3_Short = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp3_Short.RData")))
posterior_mat_Exp3_Short_temp = as.matrix(stan_object_Exp3_Short)
stan_object_Exp3_Long = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp3_Long.RData")))
posterior_mat_Exp3_Long_temp = as.matrix(stan_object_Exp3_Long)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp3_Short = SD_PG_conversion_mat(posterior_mat_Exp3_Short_temp,"precision",c("pm","p_mem"))
posterior_mat_Exp3_Long = SD_PG_conversion_mat(posterior_mat_Exp3_Long_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp3_Short = posterior_summary_table(posterior_mat_Exp3_Short,c("mean_precision","p_mem"),c("Short_SD","Short_PG"))
descriptive_Exp3_Long = posterior_summary_table(posterior_mat_Exp3_Long,c("mean_precision","p_mem"),c("Long_SD","Long_PG"))

# Compute difference between conditions 
SD_diff_exp3 = posterior_mat_Exp3_Long[,"mean_precision"] - posterior_mat_Exp3_Short[,"mean_precision"]
mean(SD_diff_exp3)
HDIofMCMC(SD_diff_exp3)
PG_diff_exp3 = posterior_mat_Exp3_Long[,"p_mem"] - posterior_mat_Exp3_Short[,"p_mem"]
mean(PG_diff_exp3)
HDIofMCMC(PG_diff_exp3)

# Compute the difference between the third WM report and the short-list condition
SD_diff_short_3rdWM = posterior_mat_Exp3_Short[,"mean_precision"] - posterior_mat_Exp2_WM_report3[,"mean_precision"]
mean(SD_diff_short_3rdWM)
HDIofMCMC(SD_diff_short_3rdWM)
