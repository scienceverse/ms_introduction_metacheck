#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Corr_tDCS_analyses_osf.R                                           #
# Author: Yang Hu                                                    #
# Date: Sept. 1, 2018                                                #
# Date of last change: Dec. 23, 2021                                 #
# Project: Corruption with tDCS (Corr_tDCS)                          #
# PI: Dr.Yang Hu                                                     #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                  #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao       # 
#                                                                    #
# This script aims to manage behavioral data, and perform            #
# statistical analyses. It is cleaned and simplified based on the    #
# latest version of the code, and is uploaded to the Open Science    #
# Framework (OSF) to reproduce the main analyses.                    #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################################################################
###                      Step 1: Preparation                       ###
######################################################################

rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS"
# source("./code/code_R/Corr_tDCS_custom_funs.R")
# source("./code/code_R/tidy_lmer.R")
source(paste0(rootdir,"/code/code_R/Corr_tDCS_custom_funs.R"))
source(paste0(rootdir,"/code/code_R/tidy_lmer.R"))

options(scipen = 999) # turn off scientific notations and modify the decimal

# data_path <- paste0(rootdir,"/data/backup/")
plot_savedir <- paste0(rootdir,"/output/figures/")

###  Merge individual data files from presentation logfile  ###
# Once this has been done, we can directly start from below
# df_merge <- myreadin(data_path) 
# Missing SbjID due to schedule: 39, 40, 80, 89, 90, 91, 92, 123 (123 and 124 are the same person, but mis-indexed in behavioral data and Q data)

# sbj28 (sham) restarted the task after completing a few trials (due to technical error); we kept him/her
# as sbj28 still completed the whole task in the second time
# sbj117 (cathodal) did not complete the whole task and we excluded him from further analyses

###  Read merged data ###
# Behavioral data recorded from the experiment
df_merge <- read.table(paste0(rootdir,"/data/analyses/Corr_tDCS_merge_osf.csv"), header = T, sep = ",") %>%
            group_by(SbjID) %>%
            mutate(TrialNo_Check = 1:n(), 
                   Condition = ifelse(Sender_Act == "honesty", "Truth", "Lie"),
                   Condition = factor(Condition, levels = c("Truth", "Lie"))) %>%
            ungroup() %>%
            mutate(TrialNo = as.numeric(TrialNo),
                   IsValid = ifelse((SbjID == 28 & TrialNo_Check <= 28) | SbjID == 117, 0, 1))

# Demographical data
# The raw file additionally includes 3 SbjID (89,90,91)
# as these three pps did not attend the task at all
df_demo_raw <- read_excel(paste0(rootdir, "/data/analyses/Corr_tDCS_basicinfo_osf.xlsx"), sheet = "modi") # 0: female; 1: male
df_demo_all <- df_demo_raw %>% filter(SbjID != 89 & SbjID != 90 & SbjID != 91) # these 120 pps participated our task
table(df_demo_all$Gender)
describe(df_demo_all$Age)

table(df_demo_all$Group, df_demo_all$Gender)
describeBy(df_demo_all$Age, df_demo_all$Group)
                                                                                                 
df_demo_valid <- df_merge %>% filter(IsValid == 1) %>%
                 filter(TrialNo == 1) %>%
                 dplyr::select(Group, SbjID, Decision) %>%
                 merge(df_demo_raw, by = c("Group","SbjID")) %>%
                 # full_join(df_demo, by = c("Group","SbjID")) %>%
                 arrange(SbjID) %>%
                 mutate(Group = factor(Group, levels = c("Sham","Anodal","Cathodal")))
                 # replace_na(list(Age = 20, Gender = 1)) %>%
                 # filter(is.na(Decision) == F)

table(df_demo_valid$Gender)
describe(df_demo_valid$Age)

table(df_demo_valid$Group, df_demo_valid$Gender)
describeBy(df_demo_valid$Age, df_demo_valid$Group)

tbl_gen <- table(df_demo_valid$Group, df_demo_valid$Gender)
chisq.test(tbl_gen)

describe(df_demo_valid$Age)
age_fit <- lm(Age ~ 1 + Group, data = df_demo_valid)
Anova(age_fit)
summary(age_fit)
eta_sq(age_fit, partial = T)

#####################################################################
###                     Step 2: Analyses on choice                ###
#####################################################################
# Sham: sbj1-38, 127, 128
# Anodal: sbj41-79, 125
# Cathodal: sbj81-123, 126 (without 89, 90, 91, 92)
df_analyses <- df_merge %>%
               filter(IsValid == 1) %>%
               mutate(SbjID = as.factor(SbjID),
                      Group = factor(Group, levels = c("Sham","Anodal","Cathodal")),
                      Group_new = ifelse(Group == "Sham","Sham","Stimulation"),
                      z.Sender_Payoff = as.numeric(scale(Sender_Payoff, center = T, scale = T)),
                      IsAccept = ifelse(Decision == "accept", 1, 0),
                      Equal = ifelse(Bribeprop < 0.5, "disadv", 
                                     ifelse(Bribeprop == 0.5, "eq","adv")),
                      Equal = factor(Equal, levels = c("adv","eq","disadv"))) %>%
               mutate(c.Bribeprop = as.numeric(scale(Bribeprop, center = T, scale = F)))

# For the generalized linear mixed models, we use odds ratio as the effect size 
# (see: https://stats.stackexchange.com/questions/226545/effect-size-in-glmm)

accept_fit1 <- glmer(IsAccept ~ 1 + Group*Condition*c.Bribeprop + z.Sender_Payoff + (1 + Condition*c.Bribeprop|SbjID), 
                      data = df_analyses, family = "binomial",
                      control = glmerControl(optimizer = c("optimx"), calc.derivs = F,
                                             optCtrl = list(method = "nlminb")))
# control = glmerControl(optimizer = c("bobyqa"))

summary(accept_fit1)
Anova(accept_fit1)
confint(accept_fit1)

fixcoef <- fixef(accept_fit1)
OR <- exp(fixcoef) 
print(OR)

# Using emmeans package to run post-hoc analyses (recommended by Jimmy)
pw<-emtrends(accept_fit1,pairwise~Condition,var="c.Bribeprop")
summary(pw, infer = c(TRUE, TRUE)) 

# pw<-emtrends(accept_fit1,pairwise~Group|Group*Condition,var="Bribeprop")
# summary(pw, infer = c(TRUE, TRUE)) # lie/true: anodal vs. cathodal vs. sham

# pw<-emtrends(accept_fit1,pairwise~Condition|Group*Condition,var="Bribeprop")
pw<-emtrends(accept_fit1,pairwise~Condition|Group,var="c.Bribeprop") # output same as above
summary(pw, infer = c(TRUE, TRUE)) # anodal/cathodal/sham: lie vs. true


### Additional Analysis 1 ###
# To address the PS reviewer #2 concern:
# whether inverse temperature plays a role in influencing behaviors
winmodel_est_savedir <- paste0(rootdir,"/output/estimation/202103/MC2cond_m4/")
df_pm_all <- read.table(paste0(winmodel_est_savedir,"winmodel_ind_pm.csv"),
                        header = T, sep = ",") 

df_pm_IT <- df_pm_all  %>%
  filter(pars_name == "IT") %>%
  dplyr::select(SbjID, pars_value) %>%
  rename(IT = 2)

df_analyses <- merge(df_analyses, df_pm_IT, by = "SbjID")

accept_fit1_c <- glmer(IsAccept ~ 1 + Group*Condition*c.Bribeprop + z.Sender_Payoff + IT +
                                 (1 + Condition*c.Bribeprop|SbjID), 
                       data = df_analyses, family = "binomial",
                       control = glmerControl(optimizer = c("optimx"), calc.derivs = F,
                                              optCtrl = list(method = "nlminb")))
summary(accept_fit1_c) # The tDCS*Condition*OfferProp three-way interaction still holds
Anova(accept_fit1_c)

#####################################################################
###              Step 3: Analyses on decision time (DT)           ###
#####################################################################
ad.test(df_analyses$DT) # A = 1411.1, p-value < 2.2e-16
df_analyses <- df_analyses %>% mutate(logDT = log(DT))

DT_fit1 <- lmer(logDT ~ 1 + Group*Condition*c.Bribeprop + Decision + z.Sender_Payoff +
                       (1 + Condition|SbjID), data = df_analyses) 
# The other formats of random effects failed to converge, inc:
# (1 +Condition*Bribeprop|SbjID), (1+Bribeprop|SbjID)
# Anova(DT_fit1) # only Condition:Bribeprop interaction
# for lmer results, we could use anova function to get the classical ANOVA table
anova(DT_fit1)
summary(DT_fit1)
AIC(DT_fit1)
BIC(DT_fit1)
sjstats::eta_sq(DT_fit1, partial = T)
# eta_squared(anova(DT_fit1), partial = T)

# Using emmeans package to run post-hoc analyses

pw<-emtrends(DT_fit1,pairwise~Condition,var="Bribeprop")
summary(pw, infer = c(TRUE, TRUE)) 

### regression analyses for each condition ###
### We did not incorporate interactions between tDCS Group and offer proportion, 
# as neither the two-way interaction nor the three-way interaction (with condition) 
# was significant in the regression using all trials.
DT_control_fit1 <- lmer(logDT ~ 1 + Group + c.Bribeprop + Decision + z.Sender_Payoff + (1|SbjID), 
                        data = df_analyses[df_analyses$Condition == "Truth",]) 

DT_bribe_fit1 <- lmer(logDT ~ 1 + Group + c.Bribeprop + Decision + z.Sender_Payoff + (1|SbjID), 
                      data = df_analyses[df_analyses$Condition == "Lie",]) 
