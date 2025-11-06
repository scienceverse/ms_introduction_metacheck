#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Corr_tDCS_Q.R                                                      #
# Author: Yang Hu                                                    #
# Date: Sept. 1, 2018                                                #
# Date of last change: Apr. 2, 2020                                  #
# Project: Corruption with tDCS (Corr_tDCS)                          #
# PI: Dr.Yang Hu                                                     #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                  #
# Collborator: Remi Philippe, Valentin Guigon, Sasa Zhao             #
#                                                                    #
# This script aims to manage the questionnaire data. Results of      #
# Mach-IV and PSB are only mentioned in the response letter due to   #
# the space limitation. It is cleaned and simplified based on the    #
# latest version of the code, and is uploaded to the Open Science    #
# Framework (OSF) to reproduce the main analyses.                    #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################################################################
###                       Step 1: Preparation                      ###
######################################################################

library(tidyverse)
library(readxl)
library(car)
library(lme4)
library(lmerTest)

rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS"
# data_path <- paste0(rootdir,"/data/backup/")
plot_savedir <- paste0(rootdir,"/output/figures/")

# Behavioral Data (To get the group information)
# Missing SbjID due to schedule: 39, 40, 80, 89, 90, 91, 92, 123 (123 and 124 are the same person, but mis-indexed in behavioral data and Q data)
# sbj28 (sham) restarted the task after completing a few trials (due to technical error); we kept him/her
# as sbj28 still completed the whole task in the second time
# sbj117 (cathodal) did not complete the whole task and we excluded him from further analyses

df_merge <- read.table(paste0(rootdir,"/data/analyses/Corr_tDCS_merge_osf.csv"), header = T, sep = ",") %>%
            group_by(SbjID) %>%
            mutate(TrialNo_Check = 1:n()) %>%
            ungroup() %>%
            mutate(TrialNo = as.numeric(TrialNo),
                   IsValid = ifelse((SbjID == 28 & TrialNo_Check <= 28) | SbjID == 117, 0, 1))

df_SbjGroup <- df_merge %>% filter(TrialNo == 100) %>% select(SbjID, Group) # sbj28 repeated first 28 trials; sbj117 only had 116 trials

# Post-task questionnaire data
# Be careful: sbj126: only preMDMQ is missing; but we still keep him for analysis
df_Q <- read_excel(paste0(rootdir,"/data/analyses/Corr_tDCS_Q_all_osf.xlsx"), sheet = "Modi") %>%
        select(1:5) %>%
        filter(SbjNo != 39 & SbjNo != 40 & SbjNo != 80 & SbjNo != 89 &
                 SbjNo != 90 & SbjNo != 91 & SbjNo != 92 & SbjNo != 124) %>% # remove missing SbjID
        mutate(SbjID = ifelse(SbjNo == 123, 124, SbjNo)) %>% # consistent with the index of behavioral data
        select(-SbjNo) 

######################################################################
###          Step 2: Calculate Scores for Each Q and Merge         ###
######################################################################
# MDMQ
# Subscales: GB = good-bad, AT = awake-tired, CN = calm-nervous
# French version: 5-point scale
df_MDMQ <- df_Q %>% filter(Q_name == "MDMQ_pre" | Q_name == "MDMQ_post") %>%
            mutate(Q_sub = ifelse(ItemNo == 1 | ItemNo == 4 | ItemNo == 8 |
                                    ItemNo == 11 | ItemNo == 14 | ItemNo == 17 |
                                    ItemNo == 19 | ItemNo == 21 | ItemNo == 24 | ItemNo == 29, "GB",
                                  ifelse(ItemNo == 2 | ItemNo == 5 | ItemNo == 7 |
                                           ItemNo == 10 | ItemNo == 13 | ItemNo == 16 |
                                           ItemNo == 20 | ItemNo == 23 | ItemNo == 26 | ItemNo == 28,"AT","CN"))) %>% 
            mutate(Score_orig = as.numeric(Score_orig),
                   Score_modi = ifelse(ItemNo == 3 | ItemNo == 4 | ItemNo == 5 |
                                         ItemNo == 7 | ItemNo == 9 | ItemNo == 11 |
                                         ItemNo == 16 | ItemNo == 19 | ItemNo == 21 | 
                                         ItemNo == 22 | ItemNo == 25 | ItemNo == 26, 6-Score_orig, Score_orig)) %>% # reversal score
            group_by(SbjID, Q_name, Q_sub) %>%
            summarise(Score_sum = sum(Score_modi)) %>%
            ungroup() 

df_MDMQ_wide <- df_MDMQ %>%  
                mutate(Q_name_modi = paste0(Q_name,"_",Q_sub)) %>%
                select(-Q_name, -Q_sub) %>%
                spread(Q_name_modi, Score_sum)

# Sensation Check & Guess
# no subjects here reported any irregular feelings (we only have the paper version of this questionnaire)
df_Guess <- df_Q %>% filter(Q_name == "Guess") %>% select(4,5) %>% rename(Guess = Score_orig)  # 1 = tDCS, 2 = sham (placebo), 3 = not sure

# Cognitive Reflective Task (CRT)
df_CRT <- df_Q %>% filter(Q_name == "CRT") %>%
          mutate(Answer = ifelse(ItemNo == 1, 5, 
                                 ifelse(ItemNo == 2, 5, 47)),
                 IsCorrect = ifelse(as.character(Score_orig) == Answer,1,0)) %>%
          group_by(SbjID) %>%
          summarise(CRT_sum = sum(IsCorrect)) %>%
          ungroup()

# Task-Relevant Rating          
df_Rating_wide <- df_Q %>% filter(Q_name == "Q_tDCS") %>% 
                  mutate(Q_sub = ifelse(ItemNo == 1, "Interesting", 
                                        ifelse(ItemNo == 2, "PartnerNo", 
                                               ifelse(ItemNo == 3 | ItemNo == 4, "PartnerInap", 
                                                      ifelse(ItemNo == 5 | ItemNo == 8, "SelfConflict", 
                                                             ifelse(ItemNo == 6 | ItemNo == 9, "SelfInap", 
                                                                    ifelse(ItemNo == 7 | ItemNo == 10, "SelfGuilt",
                                                                           ifelse(ItemNo == 11, "Power","BribePercept"))))))),
                         Q_cond = ifelse(ItemNo < 3 | ItemNo > 10, "",
                                         ifelse(ItemNo == 3 | ItemNo == 5 | ItemNo == 6 | ItemNo == 7, "Truth","Lie")),
                         Q_sub_modi = paste0("Rating_", Q_sub, "_", Q_cond)) %>% 
                  select(SbjID, Q_sub_modi, Score_orig) %>%
                  spread(Q_sub_modi, as.numeric(Score_orig))

df_Q_wide <- df_MDMQ_wide %>% merge(df_Guess, by = "SbjID") %>% merge(df_Mach, by = "SbjID") %>% 
             merge(df_PBS_wide[,c(1,9,10)], by = "SbjID") %>% merge(df_CRT, by = "SbjID") %>% merge(df_Rating_wide, by = "SbjID") %>%
             merge(df_SbjGroup, by = "SbjID") %>%
             mutate(Group = factor(Group, levels = c("Sham","Anodal","Cathodal")))

write.table(df_Q_wide, paste0(rootdir,"/data/analyses/Corr_tDCS_Q_wide.csv"), row.names = F, quote = F, sep=",")

######################################################################
###                    Step 3: Statistical Tests                   ###
######################################################################
# For statistical analyses, we can directly start from here
df_Q_wide <- read.table(paste0(rootdir,"/data/analyses/Corr_tDCS_Q_wide.csv"), header = T, sep = ",") %>%
             filter(SbjID != 117)

# Note: lm deletes missing case automatically
# MDMQ
# Note: sbj126: the data of MDMQ_pre is missing
require(sjstats)
MDMQ_summary <- df_Q_wide %>%
                select(1:7,25) %>%
                gather(Q_sub, Score, 2:7) %>%
                drop_na() %>%
                group_by(Q_sub, Group) %>%
                summarise(mean = mean(Score), sd = sd(Score), N = n())
                
MDMQ_pre_AT_stats <- lm(MDMQ_pre_AT ~ 1 + Group, df_Q_wide)
anova(MDMQ_pre_AT_stats)
eta_sq(MDMQ_pre_AT_stats, partial = T)
MDMQ_pre_CN_stats <- lm(MDMQ_pre_CN ~ 1 + Group, df_Q_wide)
anova(MDMQ_pre_CN_stats)
eta_sq(MDMQ_pre_CN_stats, partial = T)
MDMQ_pre_GB_stats <- lm(MDMQ_pre_GB ~ 1 + Group, df_Q_wide)
anova(MDMQ_pre_GB_stats)
eta_sq(MDMQ_pre_GB_stats, partial = T)

MDMQ_post_AT_stats <- lm(MDMQ_post_AT ~ 1 + Group, df_Q_wide)
anova(MDMQ_post_AT_stats)
eta_sq(MDMQ_post_AT_stats, partial = T)
MDMQ_post_CN_stats <- lm(MDMQ_post_CN ~ 1 + Group, df_Q_wide)
anova(MDMQ_post_CN_stats)
eta_sq(MDMQ_post_CN_stats, partial = T)
MDMQ_post_GB_stats <- lm(MDMQ_post_GB ~ 1 + Group, df_Q_wide)
anova(MDMQ_post_GB_stats)
eta_sq(MDMQ_post_GB_stats, partial = T)

# Guess
df_Guess_Actual <- df_Q_wide %>% select(SbjID, Group, Guess) %>%
                   mutate(Group_modi = ifelse(Group == "Anodal" | Group == "Cathodal","tDCS","sham"),
                          IsCorrect = ifelse((Group_modi == "tDCS" & Guess == 1) |
                                              (Group_modi == "sham" & Guess == 2), 1, 0))
tbl_Guess <- table(df_Guess_Actual$IsCorrect) 
chisq.test(tbl_Guess) # Across three groups, pps guess randomly (p = 0.144)

# CRT
df_CRT <- df_Q_wide %>% select(SbjID, Group, CRT_sum) # no missing data
# CRT_stats <- lm(CRT_sum ~ 1 + Group, df_Q_wide)
# anova(CRT_stats)
tbl_CRT <- table(df_CRT$Group, df_CRT$CRT_sum)
chisq.test(tbl_CRT)
CRT_summary <- df_CRT %>% group_by(Group) %>%
               summarise(mean = mean(CRT_sum),sd = sd(CRT_sum),N = n())

# Task-Relevant Rating
sum(is.na(df_Q_wide$Rating_PartnerNo)) # two pps missed this item
sum(df_Q_wide$Rating_PartnerNo == "0")

OtherRating_summary <- df_Q_wide %>% select(1,13,14,18,25) %>%
                       drop_na() %>%
                       gather(Item, Score, 2:4) %>%
                       group_by(Item, Group) %>%
                       summarise(mean = mean(Score), sd = sd(Score), N = n())

# Interesting_stats <- lm(as.numeric(Rating_Interesting) ~ 1 + Group, df_Q_wide)
# Anova(Interesting_stats)
Power_stats <- lm(Rating_Power_ ~ 1 + Group, df_Q_wide)
Anova(Power_stats)
eta_sq(Power_stats, partial = T)
BribePercept_stats <- lm(Rating_BribePercept_ ~ 1 + Group, df_Q_wide)
Anova(BribePercept_stats)
eta_sq(BribePercept_stats, partial = T)

df_MoralRating <- df_Q_wide %>% select(1,15,16,19:25) %>% rename(PInap_L = 2, PInap_T = 3, Confl_L = 4, Confl_T = 5,
                                                                 Guilt_L = 6, Guilt_T = 7, SInap_L = 8, SInap_T = 9) %>%
                  gather(Item, Score, PInap_L:SInap_T) %>%
                  mutate(Item_Cat = substr(Item, 1, 5),
                         Condition = substr(Item, 7, 7),
                         Score = as.numeric(Score)) %>%
                  mutate(Condition = factor(Condition, levels = c("T","L")),
                         SbjID = as.factor(SbjID))
# missing rating: sbj101: Guilt_L and SInap_L
# for later analyses, we removed sbj101 on Guilt and SInap
MoralRating_summary <- df_MoralRating %>% drop_na() %>%
                       mutate(flag = paste0(as.character(SbjID),"_",Item_Cat)) %>%
                       filter(flag != "101_Guilt" & flag != "101_SInap") %>%
                       group_by(Item, Group) %>%
                       summarise(mean = mean(Score), sd = sd(Score), N = n())

PInap_stats <- aov(Score ~ Group*Condition + Error(SbjID/Condition),
                    data = df_MoralRating[df_MoralRating$Item_Cat == "PInap",])
summary(PInap_stats)
eta_sq(PInap_stats, partial = T)

# Sbj101 missed this item,
# SInap_stats <- lmer(Score ~ 1 + Group*Condition + (1|SbjID), 
#                     data = df_MoralRating[df_MoralRating$Item_Cat == "SInap",]) 
# Anova(SInap_stats)
SInap_stats <- aov(Score ~ Group*Condition + Error(SbjID/Condition), 
                    data = df_MoralRating[df_MoralRating$Item_Cat == "SInap" & df_MoralRating$SbjID != 101,])
summary(SInap_stats)
eta_sq(SInap_stats, partial = T)

# Confl_stats <- lmer(Score ~ 1 + Group*Condition + (1|SbjID), 
#                     data = df_MoralRating[df_MoralRating$Item_Cat == "Confl",])
# Anova(Confl_stats)

Confl_stats <- aov(Score ~ Group*Condition + Error(SbjID/Condition), 
                    data = df_MoralRating[df_MoralRating$Item_Cat == "Confl",])
summary(Confl_stats)
eta_sq(Confl_stats, partial = T)

# Sbj101 missed this item, automatically deleted by lmer
# Guilt_stats <- lmer(Score ~ 1 + Group*Condition + (1|SbjID),
#                     data = df_MoralRating[df_MoralRating$Item_Cat == "Guilt",])
# Anova(Guilt_stats)

Guilt_stats <- aov(Score ~ Group*Condition + Error(SbjID/Condition), 
                   data = df_MoralRating[df_MoralRating$Item_Cat == "Guilt" & df_MoralRating$SbjID != 101,])
summary(Guilt_stats)
eta_sq(Guilt_stats, partial = T)
