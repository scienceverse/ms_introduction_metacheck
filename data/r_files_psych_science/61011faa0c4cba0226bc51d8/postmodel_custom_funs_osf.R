#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# postmodel_custom_funs_osf.R                                       #
# Author: Yang Hu                                                   #
# Established Date: Aug. 31, 2017                                   #
# Last Change: Nov. 14, 2019                                        #
# Project: Corruption with tDCS (Corr_tDCS)                         #
# PI: Dr.Yang Hu                                                    #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                 #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao      #
#                                                                   #
# This is a collection of custom R functions to perform analyses    #
# and make figures. It is cleaned and simplified based on the       # 
# latest version of the code, and is uploaded to the Open Science   #
# Framework (OSF) to reproduce the main analyses.                   #                              #                                                 #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

library(Rmisc) # # for computing btw-sbj and within-sbj se and 95% ci
library(readxl)
library(rstan)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(loo)
library(psych)
library(reshape2)
library(lme4)
library(lmerTest)
library(cowplot)
require(scales)
require(ggbeeswarm)
# library(bayesplot)
# require(R.matlab)

myconfig <- theme(#axis.line.x = element_line(size = 2),
                  #axis.line.y = element_line(size = 2),
                  axis.text = element_text(size = 18),
                  axis.title.x = element_text(size = 22,vjust = -1),
                  axis.title.y = element_text(size = 22,vjust = 1),
                  legend.text = element_text(size = 18),
                  legend.title = element_text(size = 18),
                  strip.background = element_blank(),
                  strip.text = element_text(size = 18),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.spacing = unit("0.5", "cm"),
                  plot.title = element_text(size = 25, hjust = 0.5, vjust = 3),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"))

# pred_data
# Goal: to prepare the data for later analyses
prep_data <- function(filename){
  
  #%%%%%%%%%%% Read-in and clean the data %%%%%%%%%%%%%#
  df_valid <- read.table(filename, header = T, sep = ",") %>%
              group_by(SbjID) %>%
              dplyr::mutate(TrialNo_Check = 1:n()) %>%
              ungroup() %>%
              mutate(TrialNo = as.numeric(TrialNo),
                     IsValid = ifelse((SbjID == 28 & TrialNo_Check <= 28) | SbjID == 117, 0, 1)) %>%
              filter(IsValid == 1) %>%
              mutate(IsAccept = ifelse(Decision == "accept", 1, 0))

  return(df_valid)
  
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

# rhat
# Goal: to calculate the rhat of all parameters
# modified based the rhat.m in hBayesDM package
rhat <- function(fit = NULL, less = NULL, paraidx = NULL) {
  
  if (class(fit$fit) != "stanfit") {
    stop("Error: The 'fit' object is not of class stanfit!")
  } else {
    summaryData <- rstan::summary(fit$fit)
    if ("Rhat" %in% colnames(summaryData[["summary"]])) {
      rhatData <- data.frame(Rhat = summaryData[["summary"]][, "Rhat"])
    } else {
      stop("\r The 'fit' object is estimated with variational inference! Rhat values cannot be computed.")
    }
  }
  if (!is.null(less)) {
    output_L <- list()
    cat("The max. Rhat of individual- and hyper-paras is ", max(rhatData$Rhat[1:paraidx]), "\n", sep = "")
    if (all(rhatData$Rhat[1:paraidx] <= less)) {
      cat("TRUE: All Rhat values of individual- and hyper-paras are less than ", less, "\n", sep = "")
      rhat_IsValid <- 1
      
    } else {
      cat("FALSE: Some Rhat values are greater than ", less, "\n", sep = "")
      rhat_IsValid <- 0
    }
    output_L <- list(rhat_IsValid = rhat_IsValid, rhat_max = max(rhatData$Rhat[1:paraidx]))
  } else {
    return(rhatData)
  }
}

# HDIofMCMC
# Goal: to do bayesian t-test (whether 95% HDI of two post. distr. overlaps)
# Directly copied from hBayesDM toolbox
HDIofMCMC <- function(sampleVec,
                      credMass = 0.95) {
  
  sortedPts = sort(sampleVec)
  ciIdxInc = floor(credMass * length(sortedPts))
  nCIs = length(sortedPts) - ciIdxInc
  ciWidth = rep(0 , nCIs)
  for (i in 1:nCIs) {
    ciWidth[i] = sortedPts[i + ciIdxInc] - sortedPts[i]
  }
  HDImin = sortedPts[which.min(ciWidth)]
  HDImax = sortedPts[which.min(ciWidth) + ciIdxInc]
  HDIlim = c(HDImin , HDImax)
  return(HDIlim)
}

# plotHDI
# Goal: to plot HDI
# Directly copied from hBayesDM toolbox
plotHDI <- function(sample   = NULL,
                    credMass = 0.95,
                    Title    = NULL,
                    xLab     = "Value",
                    yLab     = "Density",
                    fontSize = NULL,
                    binSize  = 30,
                    ...) {
  
  # To pass R CMD Checks (serves no other purpose than to create binding)
  ..density.. <- NULL
  
  HDI <- HDIofMCMC(as.vector(t(sample)), credMass = credMass)  # 'sample' w/ data.frame class is also fine..
  sample_df <- data.frame(sample)
  
  h1 <- ggplot(sample_df, aes(x = sample)) +
        ggplot2::theme_bw() +
        geom_histogram(aes(y = ..density..), colour = "black", fill = "grey", bins = binSize, ...) +
        ggtitle(Title) + xlab(xLab) + ylab(yLab) +
        geom_segment(aes(x = HDI[1], y = 0, xend = HDI[2], yend = 0), size = 1.5, colour = "red") +
        theme(axis.text.x = ggplot2::element_text(size = fontSize)) +
        theme(axis.text.y = ggplot2::element_text(size = fontSize)) +
        theme(axis.title.y = ggplot2::element_text(size = fontSize)) +
        theme(axis.title.x = ggplot2::element_text(size = fontSize)) +
        theme(plot.title = ggplot2::element_text(size = fontSize))
  
  print(paste0(credMass*100, "% Highest Density Interval (HDI):"))
  print(paste0("Lower bound = ", round(HDI[1], 4), ", Upper bound = ", round(HDI[2], 4)))
  return(h1)
}

# extract_mu_pd
# Goal: to extract group-level para posterior distribution (pd)
extract_mu_pd <- function(fit, Group_label, para_label) {
  
  if (length(para_label) == 1) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label, permuted = T))
    mu_pd <- data.frame(para1 = para1_pd)
  } else if (length(para_label) == 2) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label[1], permuted = T))
    para2_pd = unlist(rstan::extract(fit$fit,  pars = para_label[2], permuted = T))
    mu_pd <- data.frame(para1 = para1_pd, para2 = para2_pd)
  } else if (length(para_label) == 4) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label[1], permuted = T))
    para2_pd = unlist(rstan::extract(fit$fit,  pars = para_label[2], permuted = T))
    para3_pd = unlist(rstan::extract(fit$fit,  pars = para_label[3], permuted = T))
    para4_pd = unlist(rstan::extract(fit$fit,  pars = para_label[4], permuted = T))
    mu_pd <- data.frame(para1 = para1_pd, para2 = para2_pd, para3 = para3_pd, para4 = para4_pd)
  } else if (length(para_label) == 5) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label[1], permuted = T))
    para2_pd = unlist(rstan::extract(fit$fit,  pars = para_label[2], permuted = T))
    para3_pd = unlist(rstan::extract(fit$fit,  pars = para_label[3], permuted = T))
    para4_pd = unlist(rstan::extract(fit$fit,  pars = para_label[4], permuted = T))
    para5_pd = unlist(rstan::extract(fit$fit,  pars = para_label[5], permuted = T))
    mu_pd <- data.frame(para1 = para1_pd, para2 = para2_pd, para3 = para3_pd,
                        para4 = para4_pd, para5 = para5_pd)
  } else if (length(para_label) == 7) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label[1], permuted = T))
    para2_pd = unlist(rstan::extract(fit$fit,  pars = para_label[2], permuted = T))
    para3_pd = unlist(rstan::extract(fit$fit,  pars = para_label[3], permuted = T))
    para4_pd = unlist(rstan::extract(fit$fit,  pars = para_label[4], permuted = T))
    para5_pd = unlist(rstan::extract(fit$fit,  pars = para_label[5], permuted = T))
    para6_pd = unlist(rstan::extract(fit$fit,  pars = para_label[6], permuted = T))
    para7_pd = unlist(rstan::extract(fit$fit,  pars = para_label[7], permuted = T))
    mu_pd <- data.frame(para1 = para1_pd, para2 = para2_pd, para3 = para3_pd,
                        para4 = para4_pd, para5 = para5_pd, para6 = para6_pd, para7 = para7_pd)
  } else if (length(para_label) == 9) {
    para1_pd = unlist(rstan::extract(fit$fit,  pars = para_label[1], permuted = T))
    para2_pd = unlist(rstan::extract(fit$fit,  pars = para_label[2], permuted = T))
    para3_pd = unlist(rstan::extract(fit$fit,  pars = para_label[3], permuted = T))
    para4_pd = unlist(rstan::extract(fit$fit,  pars = para_label[4], permuted = T))
    para5_pd = unlist(rstan::extract(fit$fit,  pars = para_label[5], permuted = T))
    para6_pd = unlist(rstan::extract(fit$fit,  pars = para_label[6], permuted = T))
    para7_pd = unlist(rstan::extract(fit$fit,  pars = para_label[7], permuted = T))
    para8_pd = unlist(rstan::extract(fit$fit,  pars = para_label[8], permuted = T))
    para9_pd = unlist(rstan::extract(fit$fit,  pars = para_label[9], permuted = T))
    mu_pd <- data.frame(para1 = para1_pd, para2 = para2_pd, para3 = para3_pd,
                        para4 = para4_pd, para5 = para5_pd, para6 = para6_pd, 
                        para7 = para7_pd, para8 = para8_pd, para9 = para9_pd)
  } 
  
  mu_pd$Group <- Group_label
  
  return(mu_pd)
  
}

# Built-in function: pm_ttest
# type: "one", "pair"
pm_ttest <- function (data_input, type) {
  data <- data_input
  if (type == "one") {
    results <- t.test(data$pars_value, mu = 0)
    output <- data.frame(results$statistic,results$p.value)
    
  } else if (type == "two") {
    results <- t.test(pars_value ~ Group, data = data_input)
    output <- data.frame(results$statistic,results$p.value)
    
  } else if (type == "pair") {
    data <- dcast(data_input, SbjID ~ pars_name, value.var = "pars_value")
    results <- t.test(data[,2], data[,3], paired = T)
    output_alpha <- data.frame(results$statistic,results$p.value)
    results <- t.test(data[,4], data[,5], paired = T)
    output_beta <- data.frame(results$statistic,results$p.value)
    results <- t.test(data[,6], data[,7], paired = T)
    output_theta <- data.frame(results$statistic,results$p.value)
    # results <- pairwise.t.test(data$pars_value, data$Bene, paired = T, p.adj = "none") # this function cannot provide t values
    output <- rbind(output_alpha, output_beta, output_theta)
  }
  
  return(output)
} 



# get_upper_tri
# Goal: to Get upper triangle of the correlation matrix
get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

# Built-in function: mypmcorr #
mypmcorr <- function(df_pm_input, model_name, para_name, input_type) {
  
  #### Prep data ###
  # n_name <- nchar(para_name)
  # idx_start <- n_name + 2
  # idx_end <- n_name + 3
  # df_pm_input$cond <- substr(df_pm_input$pars_name, idx_start, idx_end)
  # df_pm_input$cond <- factor(df_pm_input$cond, levels = c("SH","SL","DH","DL"))
  df_pm_input_wide <- dcast(df_pm_input, SbjID ~ pars_name, value.var = "pars_value")
  
  #### Calculate r and p (bivariate correlation) ###
  # cor_pm <- round(cor(df_pm_input_wide[,c(2:5)]),3)
  res <- corr.test(df_pm_input_wide[,c(2:ncol(df_pm_input_wide))], 
                   adjust = "none") # default is with correction
  if (input_type == "p") {
    drange <- c(0,1)
    direction_label <- 1
    legend_label <- "p-value\n(Correlation)\n"
    input_mat <- res$p
  } else {
    drange <- c(-1,1)
    direction_label <- -1
    legend_label <- "r-value\n(Correlation)\n"
    input_mat <- res$r
  } 
  uptri_cor <- get_upper_tri(input_mat)
  uptri_cor_modi <- melt(uptri_cor, na.rm = T)
  # uptri_cor_modi$Var1 <- paste0("theta_", uptri_cor_modi$Var1)
  # uptri_cor_modi$Var2 <- paste0("theta_", uptri_cor_modi$Var2)
  # uptri_cor_modi$Var1 <- factor(uptri_cor_modi$Var1, levels = c("theta_SH","theta_SL","theta_DH","theta_DL"))
  # uptri_cor_modi$Var2 <- factor(uptri_cor_modi$Var2, levels = c("theta_SH","theta_SL","theta_DH","theta_DL"))
  
  #### Make plot ###
  g1 <- ggplot(data = uptri_cor_modi, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    # scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
    #                      midpoint = 0, limit = c(-1,1), space = "Lab", 
    #                      name = "Pearson\nCorrelation") +
    scale_fill_distiller(palette = "Spectral", limits=drange, direction = direction_label, 
                         name = legend_label) +
    labs(x = "", y = "") + 
    # ggtitle(para_name) + 
    theme_bw() + myconfig + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) + coord_fixed()
  ggsave(paste0(plot_savedir,"heatmap_",model_name,"_",para_name,"_",input_type,".jpeg"), 
         width = 7, height = 4, dpi = 600)
  print(g1)
  
}

# mypmbar #
# Goal: to make bar plots
mypmbar <- function(df_pm_input, parlabel, para_input) {
  
  #### Prep data ###
  
  if (parlabel == "theta") {
    df_pm_X <- df_pm_input[df_pm_input$pars_name == paste0(parlabel,"_S") | 
                             df_pm_input$pars_name == paste0(parlabel,"_D"), ]
    attach(df_pm_X)
    df_pm_X$f1[pars_name == paste0(parlabel,"_S")] <- "solo"
    df_pm_X$f1[pars_name == paste0(parlabel,"_D")] <- "dyad"
    detach(df_pm_X)
    df_pm_X$f1 <- factor(df_pm_X$f1, levels = c("solo","dyad"))
    df_pm_X$para <- rep("theta", times = nrow(df_pm_X))
    df_pm_X$sbjpars <- as.factor(paste(df_pm_X$SbjNo, df_pm_X$para, sep = ""))
    X_summary <- ddply(df_pm_X, c("f1"), summarise,
                       N = length(pars_name), mean = mean(pars_value), 
                       sd = sd(pars_value), se = sd/sqrt(N), .drop = FALSE)
    g1 <- ggplot(data = X_summary, 
                 aes(x = f1, y = mean, color = f1, fill = f1)) + 
          geom_bar(position = position_dodge(), stat = "identity") +
          geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = "black",
                        width = .1, size = 1, position = position_dodge(.9)) +
          geom_point(data = df_pm_X, aes(x = f1, y = pars_value),
                     color = "black", alpha = 0.2, position = "jitter") +
          geom_line(data = df_pm_X, aes(x = f1, y = pars_value, group = sbjpars), 
                    color = "gray", size = 0.5) + # individual line ("spaghetti plot")
          scale_fill_manual(name = "", values=c("white", "skyblue")) + 
          scale_color_manual(name = "", values=c("skyblue", "skyblue")) + 
          scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5)) + 
          ggtitle("theta")
    
  } else {
    df_pm_X <- df_pm_input
    df_pm_X$pars_name <- factor(df_pm_X$pars_name, 
                                levels = para_input)
    # df_pm_X$pars_name <- droplevels(df_pm_X$pars_name)
    X_summary <- ddply(df_pm_X, c("pars_name"), summarise,
                       N = length(pars_name), mean = mean(pars_value), 
                       sd = sd(pars_value), se = sd/sqrt(N), .drop = FALSE)
    g1 <- ggplot(data = X_summary, aes(x = pars_name, y = mean)) +
      geom_bar(position = position_dodge(), color = "red", fill = "red", stat = "identity") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = "black",
                    width = .1, size = 1, position = position_dodge(.9)) +
      geom_point(data = df_pm_X, aes(x = pars_name, y = pars_value),
                 color = "black", alpha = 0.2, position = "jitter") +
      scale_y_continuous(limits = c(-20,20), breaks = seq(-20,20,2))
  }
  
  #### Make plot ###
  g1 <- g1 + labs(x = "", y = "mean of posterior estimates\n") +
    theme_bw() + myconfig
  
  ggsave(paste0(plot_savedir, "bar_CR_modi_m2_", parlabel, ".jpeg"), 
         width = 8, height = 5, dpi = 600)
  
  print(g1)
}