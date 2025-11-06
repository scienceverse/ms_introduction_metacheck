#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# rstan_ppc_osf.R                                                   #
# Author: Yang Hu                                                   #
# Established Date: Aug. 31, 2017                                   #
# Last Change: May 19, 2021                                         #
# Project: Corruption with tDCS (Corr_tDCS)                         #
# PI: Dr.Yang Hu                                                    #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                 #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao      #
#                                                                   #
# This is a custom R function to compare models and do other        #
# operations. It is cleaned and simplified based on the latest      #
# version of the code, and is uploaded to the Open Science          #
# Framework (OSF) to reproduce the main analyses.                   #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#*******************************************************************#
#                       Step 1: Preparation                         #
#*******************************************************************#
library(Rmisc) # # for computing btw-sbj and within-sbj se and 95% ci
library(tidyverse)
library(tidyr)
library(rstan)
library(ggplot2)
library(loo)
library(psych)
library(lme4)
library(lmerTest)
require(scales)

rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS"
plot_savedir <- paste0(rootdir,"/output/figures/stanplots/202103/MC2cond_m4/")

source(paste0(rootdir,"/code/code_rstan/postmodel_custom_funs_osf.R"))

models <- c("Corr_tDCS_MC2cond_m4") 
modelno <- length(models)
tDCS_groups <- c("Anodal","Cathodal","Sham")

df_valid <- prep_data(filename = paste0(rootdir,"/data/analyses/Corr_tDCS_merge_osf.csv")) %>% 
            group_by(SbjID) %>% 
            dplyr::mutate(TrialNo_ppc = 1:n(),
                          Condition = ifelse(Sender_Act == "honesty", "Truth", "Lie"),
                          Condition = factor(Condition, levels = c("Truth", "Lie")))

# sanity check
table_freq <- df_valid %>%
              filter(TrialNo == 1) %>%
              group_by(Group) %>%
              dplyr::summarise(Freq = n())
print(table_freq)

nIter <- 2500
nwarmup <- 1500
nPostsample <- nIter - nwarmup 
nChains   <- 4
m_win <- 1
nTrial <- 144

#**************************************************************************#
#           Step 2: Posterior predictive check (ppc) by conditions         #
#**************************************************************************#

choice_ind_summary <- df_valid %>%
                      group_by(SbjID, Group, Condition, Decision) %>%
                      dplyr::summarise(Freq = n()) %>%
                      mutate(Prop = Freq/72) %>%
                      ungroup() %>%
                      complete(SbjID, Condition, Decision, 
                               fill = list(Freq = 0, Prop = 0)) %>%
                      mutate(Group = ifelse(SbjID <= 38 | SbjID == 127 | SbjID == 128, "Sham",
                                            ifelse((SbjID >= 41 & SbjID <= 79) | SbjID == 125, "Anodal", "Cathodal")))

acceptprop <- choice_ind_summary %>% filter(Decision == "accept")

#sbjno_all <- length(unique(df_valid$SbjID))
accept_pred_all <- data.frame() # 2 refers to two conditions (truth, lie)

### Be really careful using extract!!!
### Always check the order of the stored data after extract

for (n in 1:length(tDCS_groups)) {
  
  fitname <- paste0(rootdir,"/output/stanfits/",tDCS_groups[n],"_",models[m_win],"_hbm_",toString(nIter),".RData")
  fit <- readRDS(fitname)
  sbjs_tmp <- unique(df_valid[df_valid$Group == tDCS_groups[n],]$SbjID)
  sbjno_tmp <- length(sbjs_tmp)
  accept_pred_all_tmp <- data.frame(row.names = 1:(2*sbjno_tmp)) # 2 refers to two conditions (truth, lie)
  accept_pred <- as.data.frame(rstan::extract(fit$fit, pars = "Accept_pred", permuted = F)) # To keep the original sequence, set permuted to F
  
  # nPostsample <- 10 # for testing 
  
  df_valid_tmp <- df_valid %>% 
                  filter(Group == tDCS_groups[n])
  i_col <- 0
  
  for (i in 1:nChains) {
    
      if (i == 1) {
        col_chain <- seq(1, dim(accept_pred)[2], by = nChains)
      } else if (i == 2) {
        col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 1
      } else if (i == 3) {
        col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 2
      } else {
        col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 3
      }
      
      for (i_sample in 1:nPostsample) {
          colname <- paste0("pred_chain",toString(i),"_sample",toString(i_sample))
          print(colname)
          # Extract the original data and transform in the right order 
          pred_sample <- accept_pred[i_sample, col_chain]
          pred_sample.T <- as.data.frame(t(pred_sample[,1:ncol(pred_sample)]))
          colnames(pred_sample.T)[1] <- "IsAccept_pred"
          pred_sample.T$SbjID <- rep(sbjs_tmp, times = nTrial)
          pred_sample.T$TrialNo_ppc <- rep(seq(1, nTrial, by = 1), each = sbjno_tmp)
          
          # Attach the predicted response to the conditions and calculate the proportion
          df_merge_tmp <- merge(df_valid_tmp, pred_sample.T, by = c("SbjID", "TrialNo_ppc"))
          
          acceptprop_pred <- df_merge_tmp %>%
                             group_by(SbjID, Condition, IsAccept_pred) %>%
                             summarise(Freq_pred = n()) %>%
                             mutate(Prop_pred = Freq_pred/72) %>%
                             ungroup() %>%
                             complete(SbjID, Condition, IsAccept_pred, 
                                      fill = list(Freq_pred = 0, Prop_pred = 0)) %>%
                             filter(IsAccept_pred == 1)
          accept_pred_all_tmp <- cbind(accept_pred_all_tmp, acceptprop_pred[,5])
          i_col <- i_col + 1
          colnames(accept_pred_all_tmp)[i_col] <- colname
      }
    
  }
  accept_pred_all_tmp$SbjID <- acceptprop_pred$SbjID
  accept_pred_all_tmp$Group <- tDCS_groups[n]
  accept_pred_all_tmp$Condition <- acceptprop_pred$Condition
  accept_pred_all <- rbind(accept_pred_all, accept_pred_all_tmp)
}


write.table(accept_pred_all, paste0(rootdir,"/output/estimation/202103/MC2cond_m4/winmodel_accept_pred.csv"), row.names = F, quote = F, sep=",")

#%%%%%%%%  Figure S4: Scatter plot of reject rate between predicted and real data  %%%%%%%%%#
accept_pred_all <- read.table(paste0(rootdir,"/output/estimation/202103/MC2cond_m4/winmodel_accept_pred.csv"), header = T, sep = ",")
# acceptprop$pred_mean <- rowMeans(accept_pred_all)
accept_pred_all$pred_mean <- rowMeans(accept_pred_all[,c(1:4000)])
acceptprop_plot <- accept_pred_all %>%
                   select(4001:4004) %>%
                   full_join(acceptprop, by = c("SbjID","Group","Condition")) %>%
                   mutate(Condition = factor(Condition, levels = c("Truth","Lie")))

post_hoc <- acceptprop_plot %>%
            group_by(Group, Condition) %>%
            nest() %>%
            mutate(stat = map(data, ~cor.test(.x$Prop, .x$pred_mean, data = .x)),
                      t = map(stat, ~.x$statistic[[1]]),
                      p = map(stat, ~.x$p.value[[1]]),
                      r = map(stat, ~.x$estimate[[1]]))

# save a data frame with list-columns as the csv file
# useful link: https://stackoverflow.com/questions/48024266/save-a-data-frame-with-list-columns-as-csv-file/50281576
write.table(unnest(post_hoc[,c(1,2,5,6,7)]), 
            paste0(rootdir,"/output/estimation/202103/MC2cond_m4/corr_pred_actual_accept_winmodel.csv"), 
            row.names = F, quote = F, sep=",")

g1 <- ggplot(data = acceptprop_plot, aes(x = Prop, y = pred_mean, color = Group)) + 
      geom_point(size = 4, alpha = .5) +
      facet_wrap(~ Condition, ncol = 3) +
      geom_abline(intercept = 0, slope = 1, size = 0.5, linetype = "dashed", color = "gray") + 
      # geom_smooth(method = lm, se = T) +
      scale_color_manual(name = "", values = c("magenta","skyblue","gray"), # red
                         labels = c("Anodal","Cathodal","Sham")) +
      scale_x_continuous(labels = percent) +
      scale_y_continuous(labels = percent) +
      labs(x = "\nactual acceptance", y = "predicted acceptance\n") +
      theme_bw() + myconfig
print(g1)
ggsave(paste0(plot_savedir,"winmodel_ppc_scatter.jpeg"), width = 12, height = 5, dpi = 600)

#%%%%%%%%  Figure S3A: Bar plot of accept rate between predicted and real data (pool all pps) %%%%%%%%%#

acceptprop_ind_summary <- acceptprop_plot %>%
                          group_by(SbjID, Group, Condition) %>%
                          dplyr::summarise(mean_actual_ind = mean(Prop),
                                           mean_pred_ind = mean(pred_mean)) %>%
                          ungroup()

actual_mean_summary <- summarySEwithin(acceptprop_ind_summary,
                                measurevar = "mean_actual_ind",
                                withinvars = c("Condition"),
                                betweenvars = c("Group"),
                                idvar = "SbjID") %>%
                 dplyr::rename(mean_actual = 4, sd_actual = 5,
                               se_actual = 6, ci_actual = 7)

pred_mean_summary <- summarySEwithin(acceptprop_ind_summary,
                                measurevar = "mean_pred_ind",
                                withinvars = c("Condition"),
                                betweenvars = c("Group"),
                                idvar = "SbjID") %>%
                     dplyr::rename(mean_pred = 4, sd_pred = 5,
                                   se_pred = 6, ci_pred = 7)

acceptprop_summary <- actual_mean_summary %>% 
                      merge(pred_mean_summary, by = c("Group","Condition","N")) %>%
                      mutate(x_pred_pos = c(1.8,0.8,2.1,1.1,2.4,1.4))

g1 <- ggplot(data = acceptprop_summary, 
             aes(x = Condition, y = mean_actual, color = Group, fill = Group)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = .5) +
  geom_errorbar(aes(ymin = mean_actual - ci_actual, 
                    ymax = mean_actual + ci_actual), color = "black",
                width = .1, size = 1, position = position_dodge(.9)) +
  geom_point(aes(x = x_pred_pos, y = mean_pred), 
             shape = 1, size = 4, color = "red") +
  geom_errorbar(aes(x = x_pred_pos, ymin = mean_pred - ci_pred, 
                    ymax = mean_pred + ci_pred), color = "red",
                width = .1, size = 1) + # position = position_dodge(.7)
  scale_fill_manual(name = "", values=c("magenta","skyblue","gray")) + # light green/magenta
  scale_color_manual(name = "", values=c("magenta","skyblue","gray")) + # dark green/magenta
  scale_y_continuous(labels = percent, limits = c(0,1), breaks = seq(0,1,0.2))+
  labs(x = "", y = "Acceptance\n") +
  theme_bw() + myconfig
print(g1)
ggsave(paste0(plot_savedir, "winmodel_ppc_bar.jpeg"), width = 8, height = 5, dpi = 600)


#**************************************************************************#
#   Step 3: Posterior predictive check (ppc) by conditions and proportion  #
#**************************************************************************#

choice_ind_summary <- df_valid %>%
  group_by(SbjID, Group, Condition, Bribeprop, Decision) %>%
  dplyr::summarise(Freq = n()) %>%
  mutate(Prop = Freq/8) %>%
  ungroup() %>%
  complete(SbjID, Condition, Bribeprop, Decision, 
           fill = list(Freq = 0, Prop = 0)) %>%
  mutate(Group = ifelse(SbjID <= 38 | SbjID == 127 | SbjID == 128, "Sham",
                        ifelse((SbjID >= 41 & SbjID <= 79) | SbjID == 125, "Anodal", "Cathodal")))

acceptprop <- choice_ind_summary %>% filter(Decision == "accept")

accept_pred_all <- data.frame()


for (n in 1:length(tDCS_groups)) {
  
  fitname <- paste0(rootdir,"/output/stanfits/",tDCS_groups[n],"_",models[m_win],"_hbm_",toString(nIter),".RData")
  fit <- readRDS(fitname)
  sbjs_tmp <- unique(df_valid[df_valid$Group == tDCS_groups[n],]$SbjID)
  sbjno_tmp <- length(sbjs_tmp)
  accept_pred_all_tmp <- data.frame(row.names = 1:(2*9*sbjno_tmp)) # 2 refers to two conditions (truth, lie);9 refers to 9 bribeprop
  accept_pred <- as.data.frame(rstan::extract(fit$fit, pars = "Accept_pred", permuted = F)) # To keep the original sequence, set permuted to F
  
  df_valid_tmp <- df_valid %>% 
    filter(Group == tDCS_groups[n])
  i_col <- 0
  
  for (i in 1:nChains) {
    
    if (i == 1) {
      col_chain <- seq(1, dim(accept_pred)[2], by = nChains)
    } else if (i == 2) {
      col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 1
    } else if (i == 3) {
      col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 2
    } else {
      col_chain <- seq(1, dim(accept_pred)[2], by = nChains) + 3
    }
    
    for (i_sample in 1:nPostsample) {
      colname <- paste0("pred_chain",toString(i),"_sample",toString(i_sample))
      print(colname)
      # Extract the original data and transform in the right order 
      pred_sample <- accept_pred[i_sample, col_chain]
      pred_sample.T <- as.data.frame(t(pred_sample[,1:ncol(pred_sample)]))
      colnames(pred_sample.T)[1] <- "IsAccept_pred"
      pred_sample.T$SbjID <- rep(sbjs_tmp, times = nTrial)
      pred_sample.T$TrialNo_ppc <- rep(seq(1, nTrial, by = 1), each = sbjno_tmp)
      
      # Attach the predicted response to the conditions and calculate the proportion
      df_merge_tmp <- merge(df_valid_tmp, pred_sample.T, by = c("SbjID", "TrialNo_ppc"))
      
      acceptprop_pred <- df_merge_tmp %>%
        group_by(SbjID, Condition, Bribeprop, IsAccept_pred) %>%
        dplyr::summarise(Freq_pred = n()) %>%
        mutate(Prop_pred = Freq_pred/8) %>%
        ungroup() %>%
        complete(SbjID, Condition, Bribeprop, IsAccept_pred, 
                 fill = list(Freq_pred = 0, Prop_pred = 0)) %>%
        filter(IsAccept_pred == 1)
      accept_pred_all_tmp <- cbind(accept_pred_all_tmp, acceptprop_pred[,6])
      i_col <- i_col + 1
      colnames(accept_pred_all_tmp)[i_col] <- colname
    }
    
  }
  accept_pred_all_tmp$SbjID <- acceptprop_pred$SbjID
  accept_pred_all_tmp$Group <- tDCS_groups[n]
  accept_pred_all_tmp$Condition <- acceptprop_pred$Condition
  accept_pred_all_tmp$Bribeprop <- acceptprop_pred$Bribeprop
  accept_pred_all <- rbind(accept_pred_all, accept_pred_all_tmp)
}

write.table(accept_pred_all, paste0(rootdir,"/output/estimation/202103/MC2cond_m4/winmodel_accept_pred_byProp.csv"), row.names = F, quote = F, sep=",")


#%%%%%%%%  Point plot of accept rate between predicted and real data (pool all pps) %%%%%%%%%#
# by proportion
accept_pred_all <- read.table(paste0(rootdir,"/output/estimation/202103/MC2cond_m4/winmodel_accept_pred_byProp.csv"), header = T, sep = ",")
# accept_pred_all$pred_mean <- rowMeans(accept_pred_all[,c(1:4000)])
# we could use the following tidy way (producing the same results as above)
accept_pred_all <- accept_pred_all %>% rowwise() %>%
                   mutate(pred_mean = mean(c_across(pred_chain1_sample1:pred_chain4_sample1000)),
                          pred_sd = sd(c_across(pred_chain1_sample1:pred_chain4_sample1000))) %>%
                   ungroup() %>%
                   mutate(pred_se = pred_sd/sqrt(4000),
                          pred_95tvalue = qt(0.95 + (1 - 0.95)/2, df = 4000-1),
                          pred_95ci_lower = pred_mean - pred_se*pred_95tvalue,
                          pred_95ci_upper = pred_mean + pred_se*pred_95tvalue)
                   

acceptprop_plot <- accept_pred_all %>%
  select(4001:4010) %>%
  full_join(acceptprop, by = c("SbjID","Group","Condition","Bribeprop")) %>%
  mutate(Condition = factor(Condition, levels = c("Truth","Lie")))

### Figure S5-S7: Individual-level ppc ###
g1 <- acceptprop_plot %>% filter(Group == "Sham") %>%
  ggplot(aes(x = as.factor(Bribeprop), y = Prop, group = Condition, color = Condition, fill = Condition)) + 
  facet_wrap( ~ SbjID, ncol = 5) +
  geom_point(position = position_dodge(.9), stat = "identity", alpha = .5, size = 2) +
  geom_line(position = position_dodge(.9), linetype = "dashed", size = 1) + 
  geom_point(aes(y = pred_mean), position = position_dodge(.5), # x = x_pred_pos,
             shape = 5, size = 1, color = "red") +
  geom_ribbon(aes(ymin = pred_95ci_lower,
                  ymax = pred_95ci_upper),
              alpha = 0.1, size = 0.5) + # position = position_dodge(.5), x = x_pred_pos,
  scale_fill_manual(name = "", values=c("green","orange")) + # truth (control)/lie (bribe)
  scale_color_manual(name = "", values=c("green","orange")) + 
  scale_x_discrete(labels = c("10%","","30%","","50%","","70%","","90%"))+ # for a better visualization
  scale_y_continuous(labels = percent, limits = c(0,1), breaks = seq(0,1,0.5))+ # for a better visualization
  labs(x = "Offer Proportion", y = "Acceptance\n") +
  theme_bw() + myconfig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "winmodel_ppc_bar_byProp_ind-level_anodal.jpeg"), width = 16, height = 12, dpi = 600)
ggsave(paste0(plot_savedir, "winmodel_ppc_bar_byProp_ind-level_cathodal.jpeg"), width = 16, height = 12, dpi = 600)
ggsave(paste0(plot_savedir, "winmodel_ppc_bar_byProp_ind-level_sham.jpeg"), width = 16, height = 12, dpi = 600)

### Figure S3B: Group-level ppc ###
acceptprop_ind_summary <- acceptprop_plot %>%
  group_by(SbjID, Group, Condition, Bribeprop) %>%
  dplyr::summarise(mean_actual_ind = mean(Prop),
                   mean_pred_ind = mean(pred_mean)) %>%
  ungroup()

actual_mean_summary <- summarySEwithin(acceptprop_ind_summary,
                                       measurevar = "mean_actual_ind",
                                       withinvars = c("Condition","Bribeprop"),
                                       betweenvars = c("Group"),
                                       idvar = "SbjID") %>%
  dplyr::rename(mean_actual = 5, sd_actual = 6,
                se_actual = 7, ci_actual = 8)

pred_mean_summary <- summarySEwithin(acceptprop_ind_summary,
                                     measurevar = "mean_pred_ind",
                                     withinvars = c("Condition","Bribeprop"),
                                     betweenvars = c("Group"),
                                     idvar = "SbjID") %>%
  dplyr::rename(mean_pred = 5, sd_pred = 6,
                se_pred = 7, ci_pred = 8)

acceptprop_summary <- actual_mean_summary %>% 
  merge(pred_mean_summary, by = c("Group","Condition","Bribeprop","N")) %>%
  mutate(x_pred_pos = c(1.8,0.8,2.1,1.1,2.4,1.4))

g1 <- ggplot(data = acceptprop_summary, 
             aes(x = as.factor(Bribeprop), y = mean_actual, group = Group, color = Group, fill = Group)) + 
  facet_wrap(~ Condition, ncol = 2) +
  geom_point(position = position_dodge(.9), stat = "identity", alpha = .5, size = 2) +
  geom_line(position = position_dodge(.9), linetype = "dashed", size = 0.5) + 
  geom_errorbar(aes(ymin = mean_actual - ci_actual, 
                    ymax = mean_actual + ci_actual),
                width = .1, size = 1, position = position_dodge(.9)) +
  geom_point(aes(y = mean_pred), position = position_dodge(.5), # x = x_pred_pos,
             shape = 5, size = 4, color = "red", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_pred - ci_pred,
                    ymax = mean_pred + ci_pred), 
                position = position_dodge(.5), color = "red",alpha = 0.5,
                width = .1, size = 1) + # x = x_pred_pos, 
  scale_fill_manual(name = "", values=c("magenta","skyblue","gray")) + # light green/magenta
  scale_color_manual(name = "", values=c("magenta","skyblue","gray")) + # dark green/magenta
  scale_x_discrete(labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%"))+
  scale_y_continuous(labels = percent)+ # limits = c(0,1), breaks = seq(0,1,0.2)
  labs(x = "Offer Proportion", y = "Acceptance\n") +
  theme_bw() + myconfig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "winmodel_ppc_bar_byProp.jpeg"), width = 12, height = 5, dpi = 600)
