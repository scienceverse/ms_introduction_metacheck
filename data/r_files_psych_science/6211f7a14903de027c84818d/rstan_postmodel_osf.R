#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# rstan_postmodel_osf.R                                             #
# Author: Yang Hu                                                   #
# Established Date: Aug. 31, 2017                                   #
# Last Change: May 18, 2021                                         #
# Project: Corruption with tDCS (Corr_tDCS)                         #
# PI: Dr.Yang Hu                                                    #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                 #
# Collborator: Remi Philippe, Valentin Guigon, Sasa Zhao            #
#                                                                   #
# Note: This is a custom R function to compare models and do other  #
# operations. It is cleaned and simplified based on the latest      #
# version of the code, and is uploaded to the Open Science          #
# Framework (OSF) to reproduce the main analyses.                   #                              #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#*******************************************************************#
#                       Step 1: Preparation                         #
#*******************************************************************#

nIter <- 2500
nwarmup <- 1500
nChains   <- 4
# We finally decided to define our model space with these models,
# corresponding to Model 1-4 in the manuscript
models <- c("Corr_tDCS_MC2cond_m4","Corr_tDCS_MC2cond_m2",
            "Corr_tDCS_MC2cond_m3","Corr_tDCS_FSIA_m4") 

modelno <- length(models)
tDCS_groups <- c("Anodal","Cathodal","Sham") # 

rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS"
plot_savedir <- paste0(rootdir,"/output/figures/stanplots/202103/") #  MC2cond2IA_m5/
est_savedir <- paste0(rootdir,"/output/estimation/202103/") #  MC2cond2IA_m5/

source(paste0(rootdir,"/code/code_rstan/postmodel_custom_funs_osf.R"))
source(paste0(rootdir,"/code/code_R/summarySEwithin2.R"))

df_valid <- prep_data(filename = paste0(rootdir,"/data/analyses/Corr_tDCS_merge_all.csv"))

# sanity check
table_freq <- df_valid %>%
              filter(TrialNo == 1) %>%
              group_by(Group) %>%
              dplyr::summarise(Freq = n())
print(table_freq)

#*******************************************************************#
#                     Step 2: Model Comparison                      #
#*******************************************************************#
warnings()
k <- 0
model_tmp <- array(0, c(modelno*length(tDCS_groups)))
group_tmp <- model_tmp
rhat_IsValid <- model_tmp
rhat_max <- model_tmp
myloo <- model_tmp
mywaic <- model_tmp

for (n in 1:length(tDCS_groups)) {
    sbjs  <- unique(df_valid[df_valid$Group == tDCS_groups[n],]$SbjID)
    sbjno <- length(sbjs)
    for (m in 1:modelno) {
        k <- k + 1  
        # fitname <- paste0("./output/stanfits/",tDCS_groups[n],"_",models[m],"_hbm_",toString(nIter_actual[k]),".RData")
        fitname <- paste0(rootdir,"/output/stanfits/",tDCS_groups[n],"_",models[m],"_hbm_",toString(nIter),".RData")
        
        fit <- readRDS(fitname)
        fit$fit@.MISC <- new.env(parent=globalenv())
        options(max.print = 5000) # in this way we can print as many rows as possibles
        model_tmp1 <- paste(models[m],"_hbm",sep = "")
        pois_tmp <- create_pois(model_tmp1)
        # try(print(fit$fit, pars = pois_tmp))
        paraendidx <- which("log_lik" == pois_tmp) - 1
        paraidx <- (paraendidx/2)*sbjno + (paraendidx/2)
        
        model_tmp[[k]] <- models[m]
        group_tmp[[k]] <- tDCS_groups[n]
        rhat_output_tmp <- rhat(fit = fit, less = 1.1, paraidx = paraidx)
        rhat_IsValid[[k]] <- rhat_output_tmp[[1]]
        rhat_max[[k]] <- rhat_output_tmp[[2]]
        mylog_lik <- extract_log_lik(fit$fit, parameter_name = "log_lik")
        myloo[[k]]  <- loo(mylog_lik)$looic
        mywaic[[k]] <- waic(mylog_lik)$waic
        
        print(paste0(models[m],": ",tDCS_groups[n]))
        print(paste0("The seed is ", get_seed(fit$fit)))
        print(loo(mylog_lik))
        print(waic(mylog_lik))

  }
  
}

# df_mc: data frame of model comparison
df_mc <- data.frame(model = model_tmp, Group = group_tmp, rhat_IsValid = rhat_IsValid, 
                    rhat_max = rhat_max, looic = myloo, waic = mywaic) %>%
         mutate(looic = as.numeric(looic), 
                waic = as.numeric(waic),
                model = factor(model, levels = models))

# df_mc_long <- melt(df_mc, id.vars = c("model"))
write.table(df_mc, paste0(est_savedir,"modelcomparison_MC2cond+FSIA.csv"), row.names = F, quote = F, sep=",") # Moral Cost models

# #%%%%%%%%%%%           Figure 4A: plots for looic             %%%%%%%%%%%%%#

df_mc <- read.table(paste0(est_savedir, "modelcomparison_MC2cond+FSIA.csv"), header = T, sep = ",")

g1 <- df_mc %>% 
      # filter(model != "Corr_tDCS_FSIA_m1") %>%
      ggplot(aes(x = model, y = looic - max(looic),
                        group = Group, fill = Group)) +
      geom_bar(stat = "identity",position=position_dodge(), alpha = .4) +
      coord_flip() +
      # scale_y_reverse(limits = c(0,-4000), breaks=c(0,-1000,-2000,-3000,-4000)) +
      scale_y_reverse(limits = c(0,-1500), breaks=c(0,-500,-1000,-1500)) +
      scale_fill_manual(name = "", values=c("magenta","skyblue","grey")) +
      labs(x = "", y = "\ndelta log model evidence (LOOIC)") +
      # scale_y_continuous(limits = c(2000,3500), breaks = seq(2000,3500,300))+
      theme_bw() + myconfig
print(g1)

ggsave(paste0(plot_savedir, "modelfit_LOOIC_MC2cond+FSIA_4m.jpeg"), width = 10, height = 5, dpi = 600)

#**************************************************************************#
#             Step 3: Extract the parameter of the winning model           #
#**************************************************************************#

m_win <- 1 # MC2cond_m4
winmodel_plot_savedir <- paste0(rootdir,"/output/figures/stanplots/202103/", substr(models[m_win],11,nchar(models[m_win])),"/")
winmodel_est_savedir <- paste0(rootdir,"/output/estimation/202103/", substr(models[m_win],11,nchar(models[m_win])),"/")
pars_win <- create_pois(paste0(models[m_win],"_hbm"))
df_mu_pm_all <- data.frame()
df_pm_all <- data.frame()

for (n in 1:length(tDCS_groups)) {
  
    fitname <- paste0(rootdir,"/output/stanfits/",tDCS_groups[n],"_",models[m_win],"_hbm_",toString(nIter),".RData")
    fit <- readRDS(fitname)
    sbjs_tmp <- unique(df_valid[df_valid$Group == tDCS_groups[n],]$SbjID)
    # sbjno_tmp <- table_freq$Freq[n]
    sbjno_tmp <- length(sbjs_tmp)
    
    # Extract posterior distribution (pd) of group-level pm (for later plots)
    
    df_mu_pm_tmp <- extract_mu_pd(fit = fit, Group_label = tDCS_groups[n],
                                  para_label = c("mu_beta_t","mu_beta_l","mu_lambda_t","mu_lambda_l",
                                                 "mu_gamma_t","mu_gamma_l","mu_IT")) # MC2cond_m4
    
    df_mu_pm_all <- rbind(df_mu_pm_all,df_mu_pm_tmp)
    
    # Save estimates of poseterior means (pm) of individual-level parameter in the winning model #
    pars_value_tmp <- get_posterior_mean(fit$fit, pars = pars_win[1:5])[,nChains+1] # This column is the mean of all chains 
    pars_name_tmp  <- as.factor(rep(pars_win[1:5], each = sbjno_tmp))
    df_pm_tmp <- data.frame(pars_value = pars_value_tmp, pars_name = pars_name_tmp)
    df_pm_tmp$SbjID <- rep(sbjs_tmp, times = length(pars_win[1:5]))
    df_pm_tmp$Group <- tDCS_groups[n]
    
    df_pm_all <- rbind(df_pm_all,df_pm_tmp)
}

# MC2cond_m4
# df_mu_pm_all <- df_mu_pm_all %>%
#   dplyr::rename(mu_beta_t = 1, mu_beta_l = 2,
#                 mu_lambda_t = 3, mu_lambda_l = 4, mu_gamma_t = 5, mu_gamma_l = 6, mu_IT = 7)
# 
# write.table(df_mu_pm_all, paste0(winmodel_est_savedir,"winmodel_mu_pm.csv"), 
#             row.names = F, quote = F, sep=",")

write.table(df_pm_all, paste0(winmodel_est_savedir,"winmodel_ind_pm.csv"), 
            row.names = F, quote = F, sep=",")


#**************************************************************************#
#     Step 4: Plot for the individual-level posterior mean of parameters   #
#             of interest in the winning model                             #
#**************************************************************************#     

### Read-in data file ###
# Individual-level pm
df_pm_all <- read.table(paste0(winmodel_est_savedir,"winmodel_ind_pm.csv"),
                        header = T, sep = ",") 
# all paras except IT
df_pm_other <- df_pm_all %>%
               filter(pars_name != "IT") %>%
               mutate(pars_name = droplevels(pars_name),
                      para = ifelse(str_detect(pars_name,"beta"), "beta",
                                    ifelse(str_detect(pars_name,"lambda"),"lambda","gamma")), # MC2cond_m4
                      para = factor(para, levels = c("beta","lambda","gamma")),
                      Condition = ifelse(str_detect(pars_name,"_t"),"Truth","Lie"),
                      Condition = factor(Condition, levels = c("Truth","Lie")))

# pm_other_summary <- df_pm_other %>%
#                     group_by(Group, pars_name) %>%
#                     summarise(mean = mean(pars_value),
#                               sd = sd(pars_value)) %>%
#                     ungroup() %>%
#                     mutate(para = ifelse(str_detect(pars_name,"beta"),"beta",
#                                          ifelse(str_detect(pars_name,"gamma"),"gamma","lambda")))


df_pm_wide <- df_pm_other %>%
  dplyr::select(-para, -Condition) %>%
  spread(pars_name, pars_value) %>%
  mutate(beta_lt = beta_l - beta_t,
         lambda_lt = lambda_l - lambda_t,
         gamma_lt = gamma_l - gamma_t,
         Group_new = ifelse(Group == "Sham","Sham","Stimulation"))

# IT
df_pm_IT <- df_pm_all  %>%
  filter(pars_name == "IT")

# pm_IT_summary <- df_pm_IT %>%
#   group_by(Group, pars_name) %>%
#   summarise(mean = mean(pars_value),
#             sd = sd(pars_value)) %>%
#   ungroup() 


### beta ###
df_pm_other$Group <- factor(df_pm_other$Group, levels = c("Sham", "Anodal","Cathodal")) # 
para_fit <- lmer(pars_value ~ 1 + Group*Condition + (1|SbjID), data = df_pm_other[df_pm_other$para == "beta",])
# para_fit <- aov(pars_value ~ Group*Condition + Error(SbjID/Condition),
#                 data = df_pm_all[df_pm_all$para == "beta",])
AIC(para_fit)
BIC(para_fit)
summary(para_fit)
anova(para_fit)
sjstats::eta_sq(para_fit, partial = TRUE)
emmeans::emmeans(para_fit, list(pairwise ~ Group|Condition), adjust = "fdr")

# post-hoc: to get 95% CI and the Cohen's d
# For details, see: https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
tt <- emmeans::emmeans(para_fit, ~ Group|Condition)
# pairs(tt)
emmeans::eff_size(tt, sigma = sigma(para_fit), edf = 213) #equivalent degree of freedom for the sigma (same as the df in the pairwise comparison output)

# df_beta <- df_pm_all %>% filter(para == "beta")
# # Truth (control)
# # anodal vs. sham
# t.test(pars_value ~ Group, data = df_beta[df_beta$Condition == "Truth" &
#                                             df_beta$Group != "Cathodal",])
# effectsize::cohens_d(pars_value ~ Group, data = df_beta[df_beta$Condition == "Truth" &
#                                                           df_beta$Group != "Cathodal",])

### lambda ###
para_fit <- lmer(pars_value ~ 1 + Group*Condition + (1|SbjID), data = df_pm_other[df_pm_other$para == "lambda",])
# para_fit <- aov(pars_value ~ Group*Condition + Error(SbjID/Condition),
#                 data = df_pm_all[df_pm_all$para == "lambda",])
AIC(para_fit)
BIC(para_fit)
summary(para_fit)
anova(para_fit)
sjstats::eta_sq(para_fit, partial = TRUE)
emmeans::emmeans(para_fit, list(pairwise ~ Group|Condition), adjust = "fdr")

### gamma ###
para_fit <- lmer(pars_value ~ 1 + Group*Condition + (1|SbjID), 
                 data = df_pm_other[df_pm_other$para == "gamma",])
# para_fit <- aov(pars_value ~ Group*Condition + Error(SbjID/Condition),
#                 data = df_pm_all[df_pm_all$para == "gamma",])
AIC(para_fit)
BIC(para_fit)
summary(para_fit)
anova(para_fit)
sjstats::eta_sq(para_fit, partial = TRUE)

# post-hoc analyses: to get corrected p-value
emmeans::emmeans(para_fit, list(pairwise ~ Group), adjust = "fdr")
emmeans::emmeans(para_fit, list(pairwise ~ Group|Condition), adjust = "fdr")

# post-hoc: to get 95% CI and the Cohen's d
tt <- emmeans::emmeans(para_fit, ~ Group)
emmeans::eff_size(tt, sigma = sigma(para_fit), edf = 116)

tt <- emmeans::emmeans(para_fit, ~ Group|Condition)
emmeans::eff_size(tt, sigma = sigma(para_fit), edf = 228)

# Directly testing the Group*Condition interaction on each parameter
df_pm_wide$Group <- factor(df_pm_wide$Group, levels = c("Sham","Anodal", "Cathodal")) 
para_lt_fit <- lm(beta_lt ~ 1 + Group, data = df_pm_wide)
para_lt_fit <- lm(lambda_lt ~ 1 + Group, data = df_pm_wide)
para_lt_fit <- lm(gamma_lt ~ 1 + Group, data = df_pm_wide)
summary(para_lt_fit)

require(lm.beta)
confint(fit)
lm.beta(fit)

car::Anova(inter_fit)
emmeans::emmeans(fit, list(pairwise ~ Group), adjust = "none")

# library(permuco)
# lmp_fit_beta <- lmperm(beta_lt ~ 1 + Group, data = df_pm_wide, np = 10000)
# summary(lmp_fit_beta)

# df_pm_long1 <- df_pm_wide %>%
#                select(1,2,7,8) %>%
#                gather(pars_name, pm, alpha_lt:beta_lt)
# 
# aov_fit <- aov(pm ~ Group*pars_name +
#                     Error(SbjID/pars_name), data = df_pm_long1)
# summary(aov_fit)

#%%%%%%%%%%% Figure 4B: Bar plot of mean of pm %%%%%%%%%%%%%#
# For Table S5, we reported the original mean and sd
ind_pm_summary_table <- df_pm_other %>%
                  group_by(Group, para, Condition) %>%
                  summarise(mean = mean(pars_value),
                            sd = sd(pars_value)) %>%
                  ungroup()

# we adjusted sd, se and ci deu to the repeated measures in the within-subject design
# keep the original mean (rather than the normalized mean)
ind_pm_summary <- summarySEwithin2(df_pm_other,
                                  measurevar = "pars_value",
                                  withinvars = c("para","Condition"),
                                  betweenvars = c("Group"),
                                  idvar = "SbjID") %>%
                   dplyr::rename(mean = 5)

g1 <- ggplot(data = ind_pm_summary,
             aes(x = Condition, y = mean, color =  Group, fill =  Group)) +
      facet_wrap(~ para, ncol = 3) +
      geom_point(position = position_dodge(0.9),  size = 4) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = Group),
                    width = .5, size = 2, position = position_dodge(0.9)) +
      geom_beeswarm(data = df_pm_other, aes(x = Condition, y = pars_value, group = Group, color = Group),
                    fill = "black", alpha = 0.2, dodge.width = .9) +
      scale_fill_manual(name = "", values = c("magenta","skyblue","gray")) + 
      scale_color_manual(name = "", values = c("magenta","skyblue","gray")) +  
      # scale_y_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 5))+
      labs(x = "", y = "Individual-level \nposterior estimates\n", size = "") +
      theme_bw() + myconfig
print(g1)
ggsave(paste0(winmodel_plot_savedir, "bar_ind_pm_winmodel_2conds.jpeg"), 
       width = 12, height = 5, dpi = 600)


#%%%%%%%%%%% Figure S8: Bar plot of mean of dealta pm (lie - truth) %%%%%%%%%%%%%#
df_pm_lt_long <- df_pm_wide %>%
                 select(1,2,9:12) %>%
                 gather(para, pars_value, 3:5) %>%
                 mutate(para = factor(para, levels = c("beta_lt","lambda_lt","gamma_lt")))

ind_pm_summary <- summarySEwithin(df_pm_lt_long,
                                  measurevar = "pars_value",
                                  withinvars = c("para"),
                                  betweenvars = c("Group"),
                                  idvar = "SbjID") %>%
                  dplyr::rename(mean = 4)

g1 <- ggplot(data = ind_pm_summary,
             aes(x = para, y = mean, color =  Group, fill =  Group)) +
  geom_point(position = position_dodge(0.9),  size = 4) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = Group),
                width = .5, size = 2, position = position_dodge(0.9)) +
  geom_beeswarm(data = df_pm_lt_long, aes(x = para, y = pars_value, group = Group, color = Group),
                fill = "black", alpha = 0.2, dodge.width = .9) +
  scale_fill_manual(name = "", values = c("magenta","skyblue","gray")) + # labels = c("Help", "Punish")
  scale_color_manual(name = "", values = c("magenta","skyblue","gray")) + 
  # scale_y_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 5))+
  labs(x = "", y = "Individual-level \nposterior estimates\n", size = "") +
  theme_bw() + myconfig
print(g1)
ggsave(paste0(winmodel_plot_savedir, "bar_ind_pm_lt_winmodel.jpeg"), 
       width = 8, height = 5, dpi = 600)


### To address the PsySci reviewer's concern ###
# For between-sbj design, we use summarySE
ind_pm_summary <- summarySE(df_pm_IT,
                            measurevar = "pars_value",
                            groupvars = c("Group")) %>%
                  dplyr::rename(mean = 3)

g1 <- ggplot(data = ind_pm_summary,
             aes(x = Group, y = mean, color =  Group, fill =  Group)) +
  geom_point(position = position_dodge(0.9),  size = 4) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = Group),
                width = .5, size = 2, position = position_dodge(0.9)) +
  geom_beeswarm(data = df_pm_IT, aes(x = Group, y = pars_value, group = Group, color = Group),
                fill = "black", alpha = 0.2, dodge.width = .9) +
  scale_fill_manual(name = "", values = c("magenta","skyblue","gray")) + # labels = c("Help", "Punish")
  scale_color_manual(name = "", values = c("magenta","skyblue","gray")) + #
  # scale_y_continuous(limits = c(-10, 20), breaks = seq(-10, 20, 5))+
  labs(x = "", y = "Individual-level \nposterior estimates\n", size = "") +
  theme_bw() + myconfig
print(g1)
ggsave(paste0(winmodel_plot_savedir, "bar_ind_pm_winmodel_IT.jpeg"),
       width = 7, height = 5, dpi = 600)

df_pm_IT <- df_pm_IT %>%
  mutate(Group = factor(Group, levels = c("Sham","Anodal","Cathodal")))
para_fit <- lm(pars_value ~ 1 + Group, data = df_pm_IT)
summary(para_fit)
anova(para_fit)
emmeans::emmeans(para_fit, list(pairwise ~ Group), adjust = "fdr")

#### control analyses ###
# Whether IT influences our results
df_pm_control <- df_pm_IT %>% dplyr::select(-pars_name) %>%
  rename(IT = 1) %>%
  merge(df_pm_other,by = c("SbjID","Group")) %>%
  mutate(Group = factor(Group, levels = c("Sham","Anodal","Cathodal")))

para_fit <- lmer(pars_value ~ 1 + Group*Condition + IT + (1|SbjID), 
                 data = df_pm_control[df_pm_control$para == "beta",])
# para_fit <- aov(pars_value ~ Group*Condition + Error(SbjID/Condition),
#                 data = df_pm_all[df_pm_all$para == "beta",])
summary(para_fit)
anova(para_fit)
AIC(para_fit)
BIC(para_fit)
sjstats::eta_sq(para_fit, partial = TRUE)
emmeans::emmeans(para_fit, list(pairwise ~ Group|Condition), adjust = "fdr")

para_fit <- lmer(pars_value ~ 1 + Group*Condition + IT + (1|SbjID), 
                 data = df_pm_control[df_pm_control$para == "gamma",])
summary(para_fit)
anova(para_fit)
AIC(para_fit)
BIC(para_fit)

#**************************************************************************#
#                    Step 6: Explorative mediation analyses                #
#**************************************************************************#
df_demo_raw <- read_excel(paste0(rootdir, "/data/analyses/Corr_tDCS_basicinfo_osf.xlsx"), sheet = "modi") # 0: female; 1: male

df_Q_wide <- read.table(paste0(rootdir,"/data/analyses/Corr_tDCS_Q_wide.csv"), header = T, sep = ",") %>%
             filter(SbjID != 117)

df_pmQ_wide <- df_pm_wide %>% 
               merge(df_Q_wide[,c(1,9:11,15,16,19:24)], by = c("SbjID")) %>%
               mutate(z.Mach_sum = as.numeric(scale(Mach_sum)),
                      z.PBS_Empathy = as.numeric(scale(PBS_Empathy)),
                      z.PBS_Help = as.numeric(scale(PBS_Help)),
                      Group = factor(Group, levels = c("Sham","Anodal","Cathodal"))) %>% # scale will automatically drop na
               merge(df_demo_raw[,c(2,4,5)], by = c("SbjID"))

accept_ind_summary4med <- read.table(paste0(rootdir,"/data/analyses/accept_ind_summary4med.csv"),
                                     header = T, sep=",")

accept_ind_wide <- accept_ind_summary4med %>%
  dplyr::select(-Group,-Freq) %>%
  spread(Condition, Prop) %>%
  rename(Prop_t = 2, Prop_l = 3) %>%
  mutate(Prop_lt = Prop_l - Prop_t)

df_pmQ_wide <- df_pmQ_wide %>% merge(accept_ind_wide, by = "SbjID")

# The 'mediation' package cannot handle categorical X with more than two levels
# Thus we switch to 'MeMoBootR' package
# tutorial video: https://www.youtube.com/watch?v=bvn1sqTb74s 
library(MeMoBootR)
### beta ###
beta_Med_main <- mediation1(y = "Prop_lt",
                            x = "Group",
                            m = "beta_lt",
                            cvs = NULL, # covariates
                            df = df_pmQ_wide,
                            with_out = F, # do not exclude outliers
                            nboot = 5000,
                            conf_level = .95)

### Decompose the outputs ###
## view data screening
# View(beta_Med_main$datascreening$fulldata)

## additivity
# beta_Med_main$datascreening$correl

## linearity
# beta_Med_main$datascreening$linearity

## normality
# beta_Med_main$datascreening$normality

## homogeneity
# beta_Med_main$datascreening$homogen

## regression results
summary(beta_Med_main$model1) # c path (total effect)
summary(beta_Med_main$model2) # a path
summary(beta_Med_main$model3) # b path and c' path (direct effect); a*b path (indirect effect)

# total, direct, indirect effects
beta_Med_main$total.effect
beta_Med_main$direct.effect
beta_Med_main$indirect.effect

# # Sobel test (not used often)
# beta_Med_main$z.score; beta_Med_main$p.value

# Bootstrapped statistics (indirect effect)
beta_Med_main$boot.results
beta_Med_main$boot.ci

# # control analysis
# beta_Med_c1 <- mediation1(y = "Prop_lt",
#                           x = "Group",
#                           m = "beta_lt",
#                           cvs = c("Age","Gender"), # covariates
#                           df = df_pmQ_wide,
#                           with_out = F, # do not exclude outliers
#                           nboot = 5000,
#                           conf_level = .95)
# 
# beta_Med_c1$boot.results
# beta_Med_c1$boot.ci

# ### lambda ###
# lambda_Med_main <- mediation1(y = "Prop_lt",
#                              x = "Group",
#                              m = "lambda_lt",
#                              cvs = NULL, # covariates
#                              df = df_pmQ_wide,
#                              with_out = F, # do not exclude outliers
#                              nboot = 5000,
#                              conf_level = .95)
# 
# lambda_Med_main$boot.results
# lambda_Med_main$boot.ci

### gamma ###
gamma_Med_main <- mediation1(y = "Prop_lt",
                             x = "Group",
                             m = "gamma_lt",
                             cvs = NULL, # covariates
                             df = df_pmQ_wide,
                             with_out = F, # do not exclude outliers
                             nboot = 5000,
                             conf_level = .95)

summary(gamma_Med_main$model1)
summary(gamma_Med_main$model2)
summary(gamma_Med_main$model3)
gamma_Med_main$boot.results
gamma_Med_main$boot.ci

# gamma_Med_c1 <- mediation1(y = "Prop_lt",
#                            x = "Group",
#                            m = "gamma_lt",
#                            cvs = c("Age","Gender"), # covariates
#                            df = df_pmQ_wide,
#                            with_out = F, # do not exclude outliers
#                            nboot = 1000,
#                            conf_level = .95)
# gamma_Med_c1$boot.results
# gamma_Med_c1$boot.ci