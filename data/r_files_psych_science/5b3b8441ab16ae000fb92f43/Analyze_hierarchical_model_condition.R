# ============= Analysis of the Bayesian hierarchical model =============

# Analysis of the ZL Bayesian hierarchical model (Oberauer et al., 2017)
# while including condition type (e.g., LTM vs WM) within the model.
# The current code reads Stan objects and performes the following analyses:
#         (1) Conversion of kappa values to SD (degrees) and Pmem to PG
#         (2) Computation of mean and 95% HDI of group-level parameters
#         (3) Computation of mean and 95% HDI of the difference between conditions
# The code was written by Dan Biderman and Natalie Biderman, June 2018

# --------------------------------------------------------

rm(list=ls())
current_folder = getwd() # to get the correct working directory, you will need to restart RStudio and open this script from the folder of Experiment 2
setwd(current_folder)

# Load packages
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
stan_object_Exp1 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp1.RData")))
posterior_mat_Exp1_temp = as.matrix(stan_object_Exp1)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp1 = SD_PG_conversion_mat(posterior_mat_Exp1_temp,"precision",c("pm","p_mem"))
                                         
# Show descriptive statistics
descriptive_Exp1 = posterior_summary_table(posterior_mat_Exp1,c("mean_precision[1]","p_mem[1]",
                                                                "mean_precision[2]","p_mem[2]"),
                                           c("SD_LTM","PG_LTM","SD_WM","PG_WM"))

# Display posteriors of conditions
create_non_ggplot_density(list(posterior_mat_Exp1[,"mean_precision[1]"],
                               posterior_mat_Exp1[,"mean_precision[2]"]))

# Calulate the difference between conditions
SD_diff = posterior_mat_Exp1[,"mean_precision[1]"] - posterior_mat_Exp1[,"mean_precision[2]"]
mean(SD_diff)
HDIofMCMC(SD_diff)
PG_diff = posterior_mat_Exp1[,"p_mem[1]"] - posterior_mat_Exp1[,"p_mem[2]"]
mean(PG_diff)
HDIofMCMC(PG_diff)

# --------------------------------------------------------
# Experiment 2 : LTM vs. WM
# --------------------------------------------------------

# Load stan objects
stan_object_Exp2 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2.RData")))
posterior_mat_Exp2_temp = as.matrix(stan_object_Exp2)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp2 = SD_PG_conversion_mat(posterior_mat_Exp2_temp,"precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp2 = posterior_summary_table(posterior_mat_Exp2,c("mean_precision[1]","p_mem[1]",
                                                                "mean_precision[2]","p_mem[2]"),
                                           c("SD_LTM","PG_LTM","SD_WM","PG_WM"))

# Display posteriors of conditions
create_non_ggplot_density(list(posterior_mat_Exp2[,"mean_precision[1]"],
                               posterior_mat_Exp2[,"mean_precision[2]"]))

# Calulate the difference between conditions
SD_diff = posterior_mat_Exp2[,"mean_precision[1]"] - posterior_mat_Exp2[,"mean_precision[2]"]
mean(SD_diff)
HDIofMCMC(SD_diff)
PG_diff = posterior_mat_Exp2[,"p_mem[1]"] - posterior_mat_Exp2[,"p_mem[2]"]
mean(PG_diff)
HDIofMCMC(PG_diff)

# --------------------------------------------------------
# Experiment 2 : WM reports
# --------------------------------------------------------

# Load stan objects
stan_object_Exp2_WMreports = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp2_WMreports.RData")))
posterior_mat_Exp2_WMreports_temp = as.matrix(stan_object_Exp2_WMreports)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp2_WMreports = SD_PG_conversion_mat(posterior_mat_Exp2_WMreports_temp,
                                                    "precision",c("pm","p_mem"))

# Show descriptive statistics
descriptive_Exp2_WMreports = posterior_summary_table(posterior_mat_Exp2_WMreports,c("mean_precision[1]","p_mem[1]",
                                                                                    "mean_precision[2]","p_mem[2]",
                                                                                    "mean_precision[3]","p_mem[3]"),
                                                     c("SD_WMreport1","PG_WMreport1","SD_WMreport2","PG_WMreport2",
                                                       "SD_WMreport3","PG_WMreport3"))

# Calulate the difference between the third and first WM report
SD_diff = posterior_mat_Exp2_WMreports[,"mean_precision[3]"] - posterior_mat_Exp2_WMreports[,"mean_precision[1]"]
mean(SD_diff)
HDIofMCMC(SD_diff)
PG_diff = posterior_mat_Exp2_WMreports[,"p_mem[3]"] - posterior_mat_Exp2_WMreports[,"p_mem[1]"]
mean(PG_diff)
HDIofMCMC(PG_diff)

# --------------------------------------------------------
# Experiment 3 : Short vs. Long
# --------------------------------------------------------

# Load stan objects
stan_object_Exp3 = get(load(paste0(current_folder,"/Stan_objects/mixture_multi_level_Exp3.RData")))
posterior_mat_Exp3_temp = as.matrix(stan_object_Exp3)

# Turn all kappa values to degrees and PM to PG
posterior_mat_Exp3 = SD_PG_conversion_mat(posterior_mat_Exp3_temp,
                                          "precision",c("pm","p_mem"))
                                          
# Show descriptive statistics
descriptive_Exp3 = posterior_summary_table(posterior_mat_Exp3,c("mean_precision[1]","p_mem[1]",
                                                                "mean_precision[2]","p_mem[2]"))

# Calulate mean_precision difference
SD_diff = posterior_mat_Exp3[,"mean_precision[2]"] - posterior_mat_Exp3[,"mean_precision[1]"]
mean(SD_diff)
HDIofMCMC(SD_diff)
PG_diff = posterior_mat_Exp3[,"p_mem[2]"] - posterior_mat_Exp3[,"p_mem[1]"]
mean(PG_diff)
HDIofMCMC(PG_diff)