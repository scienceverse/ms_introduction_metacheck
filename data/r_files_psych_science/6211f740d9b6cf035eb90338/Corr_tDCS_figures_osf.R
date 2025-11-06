#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Corr_tDCS_figures_osf.R                                            #
# Author: Yang Hu                                                    #
# Date: Sept. 1, 2018                                                #
# Date of last change: Apr. 16, 2021                                 #
# Project: Corruption with tDCS (Corr_tDCS)                          #
# PI: Dr.Yang Hu                                                     #
# Co-PI: Prof.Dr.Jean-Claude Dreher                                  #
# Main Collborators: Remi Philippe, Valentin Guigon, Sasa Zhao       #
#                                                                    #
# This script aims to manage behavioral data and make plots. It is   #
# cleaned and simplified based on the latest version of the code,    #
# and is uploaded to the Open Science Framework (OSF) to reproduce   #
# the main analyses.                                                 #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

######################################################################
###                       Step 1: Preparation                      ###
######################################################################

rootdir <- "/gpfs/share/home/1906388343/CBCS_Projects/2019_Corruption_tDCS"
# source("./code/code_R/Corr_tDCS_custom_funs.R")
source(paste0(rootdir,"/code/code_R/Corr_tDCS_custom_funs.R"))
source(paste0(rootdir,"/code/code_R/summarySEwithin2.R")) # To get the original mean and corrected SE/CI

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
                   IsValid = ifelse((SbjID == 28 & TrialNo_Check <= 28) | SbjID == 117, 0, 1),
                   Equal = ifelse(Bribeprop < 0.5, "disadv", 
                                  ifelse(Bribeprop == 0.5, "eq","adv")),
                   Equal = factor(Equal, levels = c("disadv","eq","adv")))

df_analyses <- df_merge %>% filter(IsValid == 1)

df_groupinfo <- df_analyses %>%
                 filter(TrialNo == 1) %>%
                 dplyr::select(SbjID, Group)

#####################################################################
###               Step 2: Plots and Analyses on choice            ###
#####################################################################

# Sham: sbj1-38, 127, 128
# Anodal: sbj41-79, 125
# Cathodal: sbj81-123, 126 (without 89, 90, 91, 92)

choice_ind_summary <- df_analyses %>%
                      group_by(SbjID, Group, Condition, Decision) %>%
                      summarise(Freq = n()) %>%
                      mutate(Prop = Freq/72) %>%
                      ungroup() %>%
                      complete(SbjID, Condition, Decision, 
                               fill = list(Freq = 0, Prop = 0)) %>%
                      mutate(Group = ifelse(SbjID <= 38 | SbjID == 127 | SbjID == 128, "Sham",
                                     ifelse((SbjID >= 41 & SbjID <= 79) | SbjID == 125, "Anodal", "Cathodal")))
                      # group_by(SbjID, Group, Condition, Decision, Prop) %>%
                      # summarise(Count = n())

# accept_ind_summary4med <- choice_ind_summary %>%
#   filter(Decision == "accept") %>%
#   dplyr::select(-Decision)
# write.table(accept_ind_summary4med, paste0(rootdir,"/data/analyses/accept_ind_summary4med.csv"), 
#             row.names = F, quote = F, sep=",")


# summarySEwithin2 can return both the original and normalized mean

choice_summary <- summarySEwithin2(choice_ind_summary,
                                  measurevar = "Prop",
                                  withinvars = c("Condition","Decision"),
                                  betweenvars = c("Group"),
                                  idvar = "SbjID") %>%
                  dplyr::rename(mean = 5)

# Figure 3: relationship between acceptance rate and bribe proportion #
# continuous; group-level
choice_payoff_ind_summary <- df_analyses %>%
                             group_by(SbjID, Condition, Decision, Bribeprop) %>%
                             summarise(n = n()) %>%
                             ungroup() %>% # this is to keep the 0 count
                             complete(SbjID, Condition, Decision, Bribeprop, fill = list(n = 0)) %>%
                             mutate(prop = n/8,
                                    Group = ifelse(SbjID <= 38 | SbjID == 127 | SbjID == 128, "Sham",
                                                   ifelse((SbjID >= 41 & SbjID <= 79) | SbjID == 125, "Anodal", "Cathodal")))

choice_payoff_summary <- summarySEwithin2(choice_payoff_ind_summary,
                measurevar = "prop",
                withinvars = c("Condition","Decision","Bribeprop"),
                betweenvars = c("Group"),
                idvar = "SbjID") %>%
                dplyr::rename(mean = 6)

g1 <- choice_payoff_summary %>% filter(Decision == "accept") %>% 
      ggplot(aes(x = as.factor(Bribeprop), y = mean, group = Group, color =  Group, fill =  Group)) + 
      # ggplot(aes(x = Bribeprop, y = mean, group = Group, color =  Group, fill =  Group)) + 
      facet_wrap(~ Condition, ncol = 2) +
      # geom_bar(position = position_dodge(), stat = "identity", size = 1.5, alpha = .4) +
      geom_point(position = position_dodge(.9), size = 4, alpha = .4) +
      geom_line(position = position_dodge(.9), linetype = "dashed", size = 0.5) + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = .1, size = 1, position = position_dodge(.9)) +
      scale_fill_manual(name = "", values=c("magenta","skyblue","gray")) + # light magenta/green
      scale_color_manual(name = "", values=c("magenta","skyblue","gray")) + # dark magenta/green
      scale_x_discrete(labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%"))+
      scale_y_continuous(labels = percent, limits = c(0,1), breaks = seq(0,1,0.2))+
      labs(x = "\nOffer Proportion", y = "Acceptance\n", size = "") +
      theme_bw() + myconfig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "acceptprop_point_bribeprop.jpeg"), width = 12, height = 5, dpi = 600)

# To respond to the PS reviewer #1 concern:
# whether our results replicated previous findings about the effect of dlPFC on respondent's behavior in UG:
choice_payoff_ind_summary1 <- df_analyses %>%
                              filter(Condition == "Truth" & Bribeprop < 0.6) %>%
                              mutate(UnfLevel = 6 - 10*Bribeprop) %>% # transform offer prop into unfairness level
                              group_by(SbjID, Decision, UnfLevel) %>%
                              summarise(n = n()) %>%
                              ungroup() %>% # this is to keep the 0 count
                              complete(SbjID, Decision, UnfLevel, fill = list(n = 0)) %>%
                              mutate(prop = n/8,
                                     Group = ifelse(SbjID <= 38 | SbjID == 127 | SbjID == 128, "Sham",
                                                    ifelse((SbjID >= 41 & SbjID <= 79) | SbjID == 125, "Anodal", "Cathodal")))

choice_payoff_summary1 <- summarySEwithin2(choice_payoff_ind_summary1,
                                          measurevar = "prop",
                                          withinvars = c("Decision","UnfLevel"),
                                          betweenvars = c("Group"),
                                          idvar = "SbjID") %>%
                          dplyr::rename(mean = 5)

g1 <- choice_payoff_summary1 %>% filter(Decision == "accept" & Group != "Anodal") %>% 
  ggplot(aes(x = as.factor(UnfLevel), y = mean, group = Group, color =  Group, fill =  Group)) + 
  geom_point(position = position_dodge(.9), size = 4, alpha = .4) +
  geom_line(position = position_dodge(.9), linetype = "dashed", size = 0.5) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = .1, size = 1, position = position_dodge(.9)) +
  scale_fill_manual(name = "", values=c("skyblue","gray")) + # light magenta/green
  scale_color_manual(name = "", values=c("skyblue","gray")) + # dark magenta/green
  scale_x_discrete(labels = c("1 (50%)","2 (40%)","3 (30%)","4 (20%)","5 (10%)"))+
  scale_y_continuous(labels = percent, limits = c(0.4,1), breaks = seq(0.4,1,0.2))+
  labs(x = "\nInequity Level \n(Offer Proportion)", y = "Acceptance\n", size = "") +
  theme_bw() + myconfig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "acceptprop_point_bribeprop_onlyTruth_noAnodal.jpeg"), width = 8, height = 6, dpi = 600)


#####################################################################
###        Step 3: Plots and Analyses on decision time (DT)       ###
#####################################################################
# Use all trials (these dropped trials with an extreme DT did not influence
# the results; so here we used all trials)
# Without considering bribeprop

# Figure S2: line plot on DT #
# Considering bribeprop: seperate choices (accept or reject)
DT_prop_ind_summary <- df_analyses %>%
                       group_by(SbjID, Group, Condition, Decision, Bribeprop) %>%
                       summarise(DT_ind_mean = mean(DT)) %>% 
                       ungroup()

DT_prop_summary <- summarySEwithin2(DT_prop_ind_summary,
                               measurevar = "DT_ind_mean",
                               withinvars = c("Decision","Condition","Bribeprop"),
                               betweenvars = c("Group"),
                               idvar = "SbjID") %>%
                   dplyr::rename(mean = 6)

g1 <- ggplot(data = DT_prop_summary, aes(x = as.factor(Bribeprop), y = mean, 
                                         group = Group, color = Group, fill = Group)) +
      facet_wrap(Condition ~ Decision, ncol = 2) +
      geom_point(position = position_dodge(.9), size = 4, alpha = .4) +
      geom_line(position = position_dodge(.9), linetype = "dashed", size = 0.5) + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = .1, size = 1, position = position_dodge(.9)) +
      # geom_beeswarm(data = DT_prop_ind_summary, 
      #               aes(x = as.factor(Bribeprop), y = DT_ind_mean, group = Group), 
      #               color = "black", alpha = 0.2, dodge.width = .9) +
      scale_fill_manual(name = "", values=c("magenta","skyblue","gray")) + # light green/magenta
      scale_color_manual(name = "", values=c("magenta","skyblue","gray")) + # dark green/magenta
      scale_x_discrete(labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%"))+
      scale_y_continuous(limits = c(2000,10000), breaks = seq(2000,10000,2000), oob = rescale_none)+
      labs(x = "\nOffer Proportion", y = "Decision Time (ms)\n", size = "") +
      theme_bw() + myconfig +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "DT_linepoint_prop_sepchoice.jpeg"), width = 12, height = 8, dpi = 600)

# Considering bribeprop: without considering specific choices
DT_prop_ind_summary <- df_analyses %>%
                       group_by(SbjID, Group, Condition, Bribeprop) %>%
                       summarise(DT_ind_mean = mean(DT)) %>% 
                       ungroup()

DT_prop_summary <- summarySEwithin2(DT_prop_ind_summary,
                                    measurevar = "DT_ind_mean",
                                    withinvars = c("Condition","Bribeprop"),
                                    betweenvars = c("Group"),
                                    idvar = "SbjID") %>%
                   dplyr::rename(mean = 5)

g1 <- ggplot(data = DT_prop_summary, aes(x = as.factor(Bribeprop), y = mean, 
                                         group = Group, color = Group, fill = Group)) +
      facet_wrap(~ Condition, ncol = 2) +
      geom_point(position = position_dodge(.9), size = 4, alpha = .4) +
      geom_line(position = position_dodge(.9), linetype = "dashed", size = 0.5) + 
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = .1, size = 1, position = position_dodge(.9)) +
      scale_fill_manual(name = "", values=c("magenta","skyblue","gray")) + # light green/magenta
      scale_color_manual(name = "", values=c("magenta","skyblue","gray")) + # dark green/magenta
      scale_x_discrete(labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%"))+
      scale_y_continuous(limits = c(1000,5000), breaks = seq(1000,5000,1000), oob = rescale_none)+
      labs(x = "\nOffer Proportion", y = "Decision Time (ms)\n", size = "") +
      theme_bw() + myconfig +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g1)
ggsave(paste0(plot_savedir, "DT_linepoint_prop_all.jpeg"), width = 12, height = 5, dpi = 600)

