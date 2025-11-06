#This script visualises the treatment effect conferred by the accuracy nudge
#by headline display order, which varies from 1-30 (as each participant
#was shown 30 headlines). The output of this script is shown in Figure S1 
#in the supplement.

#Load libraries:
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggeffects)
library(rstatix)
library(sjPlot)
library(jtools)
library(ggpubr)

#Set working directory
setwd("~/University of Cambridge/Psychology/Winton replication study")


#Load data (downloadable from OSF):
df <- read_csv("Pennycook et al (2020) - replication - final.csv")


#First we need to convert the format from wide to long, like the STATA script.

#Trim df to variables of interest (short)
short<- 
df %>% select(Discernment , `Condition-text`, 
              starts_with("Fake"), 
              starts_with("Real"),
              contains("_DispOrder_"))

#check
head(short)

#rename vars for melt/pivot (relab)
relab <-
short %>%
  mutate(cond = factor(`Condition-text`)) %>% #easier variable name, as factor
  select(-`Condition-text`) %>% #throw away old one
  mutate(id = factor(row_number())) %>% #unique id
  rename_at(vars(Fake1_1:Fake1_15), ~ str_replace(., "Fake1_", "rating_F")) %>% 
  rename_at(vars(Real1_1:Real1_15), ~ str_replace(., "Real1_", "rating_R")) %>%
  rename_at(vars(F_DispOrder_1:F_DispOrder_15), ~ str_replace(., "F_DispOrder_", "DispOrder_F")) %>%
  rename_at(vars(R_DispOrder_1:R_DispOrder_15), ~ str_replace(., "R_DispOrder_", "DispOrder_R"))

relab %>% names

#pivot long/melt
long <- 
relab %>% pivot_longer(
  rating_F1:DispOrder_R15,
      names_to = c(".value", "item"), #
      names_sep = "_"
    ) %>% 
  mutate(item = factor(item))

#check
long %>% head

#separate real and fake item ratings into separate dfs

long_real <- 
long %>% filter(str_detect(item, "R"))

long_fake <- 
  long %>% filter(str_detect(item, "F"))

#look at mean sharing rating across display order, using a loess smoothing

#Figure S1a:
real_plot <- long_real %>% group_by(cond, DispOrder) %>% 
  get_summary_stats(rating, type = "mean_ci") %>% 
  ggplot(aes(x = DispOrder, y = mean, group = cond, colour = cond))+
  geom_smooth(method ="loess")+ theme_apa() + 
  scale_y_continuous(name="Mean sharing rating (real headlines)", breaks=c(2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5), labels=c(2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5), limits=c(2.5, 3.5)) +
  scale_x_continuous(name="Headline display order", breaks=c(1:30), labels=c(1:30),limits=c(1,30))+labels +theme(text=element_text(size=8))

#Figure S1b:
fake_plot <- long_fake %>% group_by(cond, DispOrder) %>% 
  get_summary_stats(rating, type = "mean_ci") %>% 
  ggplot(aes(x = DispOrder, y = mean, group = cond, colour = cond))+
  geom_smooth(method ="loess")+ theme_apa() + 
  scale_y_continuous(name="Mean sharing rating (false headlines)", breaks=c(2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5), labels=c(2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5), limits=c(2.5, 3.5)) +
  scale_x_continuous(name="Headline display order", breaks=c(1:30), labels=c(1:30),limits=c(1,30))+labels +theme(text=element_text(size=8))

#calculating discernment score for each display point
#Avg sharing score for real items - avg for fake items
#(loses participant and headline variables)

mean_ratings <- long %>% 
  mutate(real_fake = case_when(str_detect(item, "F") ~ "fake", #create variable denoting if item is real or fake
                               str_detect(item, "R") ~ "real")) %>% 
  group_by(cond, real_fake, DispOrder) %>% 
  summarise(m = mean(rating)) #compute mean rating of real and fake items, at each display point, for treatment and control

#check
head(mean_ratings)

#pivot long, and calculate 'discernment' score (dscrn): avg real - avg fake 

dscrn_scores <- 
mean_ratings %>% pivot_wider(names_from = real_fake, values_from = m)%>% 
  mutate(dscrn = real-fake)

head(dscrn_scores)

#plot discernment by display order

#base plot
labels <- labs(x = "Display order", y = "Discernment", colour = "Condition")

dscrn_plot <- dscrn_scores %>% 
  ggplot(aes(x = DispOrder, y = dscrn, colour = cond, group = cond))+labels

#points, lines & loess
dscrn_plot + geom_point()+labels
dscrn_plot + geom_line()+labels
discernment_plot <- dscrn_plot + geom_smooth(method="loess") + theme_apa() + scale_y_continuous(name="Sharing discernment", breaks=c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), limits=c(-0.1,0.6)) +
  scale_x_continuous(name="Headline display order", breaks=c(1:30), labels=c(1:30),limits=c(1,30))+labels +theme(text=element_text(size=8))

#Figure S1c:
discernment_plot

#Plot the final figure S1:
bigplot <-  ggarrange(real_plot, fake_plot, discernment_plot, ncol=3, nrow=1, common.legend=TRUE)
bigplot

