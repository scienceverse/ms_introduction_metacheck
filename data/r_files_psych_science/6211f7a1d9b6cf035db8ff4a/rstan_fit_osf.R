#####################################################################
# rstan_fit_osf.R                                                   #
# Author: Yang Hu                                                   #
# Date: Sept. 1, 2018                                               #
# Date of last change: Nov. 10, 2019                                #
# Project: Corruption with tDCS (Corr_tDCS)                         #
# PI: Dr.Yang Hu                                                    #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                 #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao      #
#                                                                   #
# This script aims to wrap up the preparation for the stan fit. It  #
# is cleaned and simplified based on the latest version of the      #
# code, and is uploaded to the Open Science Framework (OSF) to      #
# reproduce the main analyses.                                      #
#####################################################################

# main function #
rstan_fit <- function(modelStr, groupStr, IsZscore = 0, test = NULL, saveFit = NULL, 
                      suffix = NULL, nIter = NULL, nwarmup = NULL,
                      adapt = 0.8, treedepth = 10, nThinn = 1 ) {
    
    #******************************************************#
    #                  Step 1: Preparation                 #
    #******************************************************#
    
    library(readxl)
    library(tidyverse) 
    library(rstan)
    library(ggplot2)
    library(loo)

    # source("./code/code_rstan/create_pois.R")
    # source("./code/code_rstan/prep_data4stan.R")
    
    L <- list()

    dataList <- prep_data4stan(modelStr, groupStr) # IsZscore
    poi      <- create_pois(modelStr)
    model_name <- modelStr
    
    #******************************************************#
    #               Step 2: model estimate                 #
    #******************************************************#
    
    # For estimation based on mean-centered data,
    # the model per se will not change.
    # if (IsZscore == 1) {
    #   z_pos <- regexpr("z",modelStr)
    #   modelStr <- paste0(substr(modelStr, 1, z_pos-1),"hbm")
    # }
    
    modelFile <- paste0("./code/code_rstan/models_stan/", modelStr,".stan")
    
    if (test == T) {
        options(mc.cores = 1)
        nIter     <- 20
        nChains   <- 1 
        nWarmup   <- floor(nIter/2)
        nThin     <- 1
        suffix = paste("_", as.character(nIter), "_test", sep = "")
        
    } else {
        options(mc.cores = 4)
        nIter     <- nIter
        nChains   <- 4 
        if (is.null(nwarmup)) {
            nWarmup <- floor(nIter/2)
        } else {
            nWarmup = nwarmup
        }
        nThin     <- nThinn
        suffix = paste("_", as.character(nIter), sep = "")
    }
    
    cat("Estimating", modelStr, "model... \n")
    cat("Variant: ", modelStr, suffix, " ... \n", sep = "")
    startTime = Sys.time(); print(startTime)
    cat("Calling", nChains, "simulations in Stan... \n")
    
    rstan_options(auto_write = TRUE)
    
    fit = stan(file = modelFile, 
               data = dataList,
               pars = poi,
               chains  = nChains,
               iter    = nIter,
               warmup  = nWarmup, 
               thin    = nThin,
               # init    = "random", # default option is "random"
               # seed    = 1362405440,
               control = list(adapt_delta = adapt, max_treedepth = treedepth),
               # open_progress = T,
               verbose = FALSE) 
    
    cat("Finishing", modelStr, "model simulation ... \n")
    endTime = Sys.time(); print(endTime)  
    cat("It took",as.character.Date(endTime - startTime), "\n")
    
    L$data <- dataList
    L$fit  <- fit
    
    if (saveFit == TRUE) {
      saveRDS(L, file = paste0('./output/stanfits/', groupStr, '_', model_name, suffix, '.RData'))
    }
    
    return(L)
} # end of main function


# prep_data4stan
# Goal: to prepare the data for stan analyses

prep_data4stan <- function(modelstr, groupstr){
  
  #%%%%%%%%%%% Read-in and clean the data %%%%%%%%%%%%%#
  df_merge <- read.table("./data/analyses/Corr_tDCS_merge_osf.csv", header = T, sep = ",") %>%
    group_by(SbjID) %>%
    mutate(TrialNo_Check = 1:n()) %>%
    ungroup() %>%
    mutate(TrialNo = as.numeric(TrialNo),
           IsValid = ifelse((SbjID == 28 & TrialNo_Check <= 28) | SbjID == 117, 0, 1))
  
  # sanity check
  table(df_merge$Group, df_merge$SbjID)
  
  df_valid <- df_merge %>% filter(IsValid == 1) %>% 
    filter(Group == groupstr) %>%
    mutate(IsAccept = ifelse(Decision == "accept", 1, 0))
  
  sbjs  <- unique(df_valid$SbjID)
  sbjno <- length(sbjs)
  
  # if (IsZscore == 1) {
  #   df_valid <- df_valid %>%
  #               mutate(pakeep = scale(pakeep), paafter = scale(paafter),
  #                      pbkeep = scale(pbkeep), pbafter = scale(pbafter),
  #                      ppkeep = scale(ppkeep), ppafter = scale(ppafter)) 
  #   
  # }
  
  #%%%%%%%%%%% Re-format the data %%%%%%%%%%%%%#
  # This is recommended by hBayesDM package
  Tsbj <- as.vector(rep(0, sbjno)) # number of valid trials for each subject
  for (i in 1:sbjno)  {
    curSbj  <- sbjs[i]
    Tsbj[i] <- sum(df_valid$SbjID == curSbj)  # Tsbj[N]
  }
  
  # Setting maxTrials
  maxTrials <- max(Tsbj)
  
  # Information for user continued
  cat(" # of (max) trials per subject = ", maxTrials, "\n\n")
  
  validTrialNo <- array(0, c(sbjno, maxTrials))
  IsAccept <- array(0, c(sbjno, maxTrials))
  Condition <- array(0, c(sbjno, maxTrials))
  pay_total <- array(0, c(sbjno, maxTrials))
  pay_you <- array(0, c(sbjno, maxTrials))
  pay_briber <- array(0, c(sbjno, maxTrials))
  
  dataList <- list()
  
  # m <- as.numeric(substr(modelstr,11,11))
  
  for (i in 1:sbjno) {
    curSbj       <- sbjs[i]
    useTrials    <- Tsbj[i]
    tmp          <- subset(df_valid, df_valid$SbjID == curSbj)
    validTrialNo[i, 1:useTrials] <- tmp$TrialNo
    IsAccept[i, 1:useTrials] <- tmp$IsAccept
    Condition[i, 1:useTrials] <- tmp$CondNo
    pay_total[i, 1:useTrials] <- tmp$Sender_Payoff
    pay_you[i, 1:useTrials] <- tmp$Sender_Bribe
    pay_briber[i, 1:useTrials] <- tmp$Sender_Kept
    
  }
  
  dataList <- list(
    S             = sbjno,
    T             = maxTrials,
    Tsbj          = Tsbj,
    validTrialNo  = validTrialNo,
    IsAccept      = IsAccept,
    Condition     = Condition,
    pay_total     = pay_total,
    pay_you       = pay_you,
    pay_briber    = pay_briber
  )
  
} 

# create_pois
# Goal: to prepare the parameters that we want to save for each model

create_pois <- function(model){
  
  pois <- list() # pois: parameters of interest
  
  if (substr(model,1,24) == 'Corr_tDCS_MC2cond_m2_hbm' ) {
    pois <- c("beta_t","beta_l","lambda_t","lambda_l","IT",
              "mu_beta_t","mu_beta_l","mu_lambda_t","mu_lambda_l","mu_IT",
              "log_lik","U_Accept","Accept_pred")
  } else if (substr(model,1,24) == 'Corr_tDCS_MC2cond_m3_hbm' ) {
    pois <- c("beta_t","beta_l","gamma_t","gamma_l","IT",
              "mu_beta_t","mu_beta_l","mu_gamma_t","mu_gamma_l","mu_IT",
              "log_lik","U_Accept","Accept_pred")
  } else if (substr(model,1,24) == 'Corr_tDCS_MC2cond_m4_hbm') {
    pois <- c("beta_t","beta_l","lambda_t","lambda_l","gamma_t","gamma_l","IT",
              "mu_beta_t","mu_beta_l","mu_lambda_t","mu_lambda_l","mu_gamma_t","mu_gamma_l","mu_IT",
              "log_lik","U_Accept","Accept_pred")
  } else if (substr(model,1,21) == 'Corr_tDCS_FSIA_m4_hbm' ) {
    pois <- c("alpha_t","beta_t","alpha_l","beta_l","IT",
              "mu_alpha_t","mu_beta_t","mu_alpha_l","mu_beta_l","mu_IT",
              "log_lik","U_Accept","Accept_pred")
  } 
  
  return(pois)
}

