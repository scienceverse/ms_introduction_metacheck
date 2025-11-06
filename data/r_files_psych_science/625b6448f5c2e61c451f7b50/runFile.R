#################################
## Run Zero Order Correlations ##
#################################
source("Data Analysis/zeroOrder.R")
################################
## Create Composite Variables ##
################################
source("Data Analysis/compositeVar.R")
#######################################
## Split Sample Per Outcome Variable ##
#######################################
source("Data Analysis/split.R")
#############################
## Hierarchical Regression ##
#############################
# Primary Analyses ####
source("Data Analysis/primaryAnalyses.R")
# Primary Analyses (Log Models for Binary Outcomes) ####
source("Data Analysis/logAnalyses.R")
# Supplemental Outcome Analyses ####
source("Data Analysis/suppAnalyses.R")
############################################
## Create Tibbles for Replicating Effects ##
############################################
source("Data Analysis/replications.R")
######################################
## Robustness Checks - Demographics ##
######################################
# Primary Analyses ####
source("Data Analysis/primaryAnalyses_demo.R")
# Primary Analyses (Log Models for Binary Outcomes) ####
source("Data Analysis/logAnalyses_demo.R")
# Supplemental Outcome Analyses ####
source("Data Analysis/suppAnalyses_demo.R")
###############################################
## Robustness Checks - Ceiling/Floor Effects ##
###############################################
# Survey 1 ####
source("Data Analysis/outcomeFrames_S1.R")
# Survey 2 ####
source("Data Analysis/outcomeFrames_S2.R")
# Tobit Regression ####
source("Data Analysis/tobit.R")
########################################
## Robustness Checks - Ordinal Models ##
########################################
source("Data Analysis/ordinalCode.R")
###############################
## Machine Learning Analyses ##
###############################
# Primary Analyses ####
source("Data Analysis/primaryAnalyses_ML.R")
# Primary Analyses (Log Models for Binary Outcomes) ####
source("Data Analysis/logAnalyses_ML.R")
# Supplemental Outcome Analyses ####
source("Data Analysis/suppAnalyses_ML.R")
#############
## Table 1 ##
#############
source("Data Analysis/table1.R")
##############
## Figure 2 ##
##############
source("Data Analysis/figure1.R")
sessionInfo()