setwd("insertlocaldirectoryhere") # Change working directory based on local folders

library(tidyverse)
library(emmeans)
library(miceadds)
library(margins)


data2 <- read_csv("study 1 final data.csv")


tables <- data2 %>% 
  # Remove people with corrupted location/description fields
  filter(bad_loc_desc != 1) %>%  
  # Isolate to day of eclipse
  filter(daysfromevent == 0) %>% 
  # Code people as in the path if they listed a path locale in their location or description fields
  mutate(present = case_when(cities_locale == 1 | cities_locale2 == 1 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(present = as.factor(present)) %>% 
  # code for when both fields contain NA
  mutate(na_locale = as.integer(as.logical(na_locale))) %>% 
  mutate(na_locale2 = as.integer(as.logical(na_locale2))) %>% 
  mutate(present_r = case_when(na_locale == 1 & na_locale2 == 1 ~ 2,
                               present == 1 ~ 1, 
                               present == 0 ~ 0)) %>% 
  mutate(present_r = as.factor(present_r)) 

# Check things
tables %>% 
  select(cities_locale, cities_locale2, present_r) %>% 
  View()

#n per group
tables %>% 
  group_by(present_r) %>% 
  summarise(count = n_distinct(id))


# Use these data when running mediation analyses
tables_new2 <- tables %>%
  select(id, present_r, awewords, we_new, i_new, certain_full, posemo_new, presentfocus,
         tenta_full, proso_grati, affiliation_s, affiliation_full, proso_no_ovlp)

#write_csv(tables_new2, "study 1 mediation final3.csv")

###########################
# Analyses
###########################
library(miceadds)

awe <- miceadds::glm.cluster(formula = awewords ~ present_r, data = tables,
                             cluster="id", family="binomial")

posemo <- miceadds::glm.cluster(formula = posemo_new ~ present_r, data = tables, 
                                cluster="id", family="binomial")

i_new <- miceadds::glm.cluster(formula = i_new ~ present_r, data = tables, 
                               cluster="id", family="binomial")

we <- miceadds::glm.cluster(formula = we_new ~ present_r, data = tables, 
                            cluster="id", family="binomial")

tenta <- miceadds::glm.cluster(formula = tenta_full ~ present_r, data = tables, 
                               cluster="id", family="binomial")

proso <- miceadds::glm.cluster(formula = proso_grati ~ present_r, data = tables, 
                               cluster="id", family="binomial")

affili_full <- miceadds::glm.cluster(formula = affiliation_full ~ present_r, data = tables, 
                                     cluster="id", family="binomial") 



# Put model results into object that can be used in emmeans
grid_awe <- qdrg(object = awe$glm_res, data = tables)
grid_posemo <- qdrg(object = posemo$glm_res, data = tables)
grid_i <- qdrg(object = i_new$glm_res, data = tables)
grid_we <- qdrg(object = we$glm_res, data = tables)
grid_tenta <- qdrg(object = tenta$glm_res, data = tables)
grid_proso <- qdrg(object = proso$glm_res, data = tables)
grid_affili <- qdrg(object = affili_full$glm_res, data = tables)

# Type = response doesn't work for the cluster models -- results are still in log odds
# So, results from the following contrast code are also still in log odds 
# Exponentiate the contrast results to get the OR
margins_awe <- emmeans(grid_awe, ~ present_r, type = "response")
margins_posemo <- emmeans(grid_posemo, ~ present_r, type = "response")
margins_i <- emmeans(grid_i, ~ present_r, type = "response")
margins_we <- emmeans(grid_we, ~ present_r, type = "response")
margins_tenta <- emmeans(grid_tenta, ~ present_r, type = "response")
margins_proso <- emmeans(grid_proso, ~ present_r, type = "response")
margins_affili <- emmeans(grid_affili, ~ present_r, type = "response")

# Define contrast (based on ordering from margins output)
present_vs_not <- list("present_vs_not" = c(-1, 1, 0))

# Gather contrasts, including CIs
results_awe <- confint(contrast(margins_awe, present_vs_not))
results_posemo <- confint(contrast(margins_posemo, present_vs_not))
results_i <- confint(contrast(margins_i, present_vs_not))
results_we <- confint(contrast(margins_we, present_vs_not))
results_tenta <- confint(contrast(margins_tenta, present_vs_not))
results_proso <- confint(contrast(margins_proso, present_vs_not))
results_affili <- confint(contrast(margins_affili, present_vs_not))


# Exponentiate model parameters
exp_results_awe <- exp(c(results_awe$estimate, results_awe$lower.CL, results_awe$upper.CL))
exp_results_posemo <- exp(c(results_posemo$estimate, results_posemo$lower.CL, results_posemo$upper.CL))
exp_results_i <- exp(c(results_i$estimate, results_i$lower.CL, results_i$upper.CL))
exp_results_we <- exp(c(results_we$estimate, results_we$lower.CL, results_we$upper.CL))
exp_results_tenta <- exp(c(results_tenta$estimate, results_tenta$lower.CL, results_tenta$upper.CL))
exp_results_proso <- exp(c(results_proso$estimate, results_proso$lower.CL, results_proso$upper.CL))
exp_results_affili <- exp(c(results_affili$estimate, results_affili$lower.CL, results_affili$upper.CL))
# Put contrast results (with CI) into it a dataframe (the t() function puts it into a matrix, correctly splitting the columns)
awe_con <- as.data.frame(t(exp_results_awe))
posemo_con <- as.data.frame(t(exp_results_posemo))
i_con <- as.data.frame(t(exp_results_i))
we_con <- as.data.frame(t(exp_results_we))
tenta_con <- as.data.frame(t(exp_results_tenta))
proso_con <- as.data.frame(t(exp_results_proso))
affili_con <- as.data.frame(t(exp_results_affili))

# Name the columns
colnames(awe_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(posemo_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(i_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(we_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(tenta_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(proso_con) <- c("odds_ratio", "lower_ci","upper_ci")
colnames(affili_con) <- c("odds_ratio", "lower_ci","upper_ci")

# Combine all indiviual contrast dataframes into one
all_cons_merge <- bind_rows(affili_con, awe_con, posemo_con, we_con, proso_con, tenta_con, i_con)

all_cons_merge <- all_cons_merge %>% 
  mutate(contrast = c("Affiliation", "Awe", "Positive Emotion","Collective Focus", "Prosociality", "Tentativeness", "Self Focus"))

rownames(all_cons_merge) <- c("Affiliation", "Awe", "Positive Emotion","Collective Focus", "Prosociality", "Tentativeness", "Self Focus")


# Graph odds ratios
ggplot(all_cons_merge) +
  aes(x = odds_ratio, y = forcats::fct_rev(contrast), group = 1) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = .2, color = "gray50") +
  geom_point(size = 1.7) +
  labs(color = "Present") +
  scale_y_discrete(name = "Word Categories") + 
  scale_x_continuous(name = "Odds Ratios") +
  coord_cartesian(xlim = c(.8, 2.5)) +
  ggtitle("Relationships between Location and Word Use") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),)


########################################################################
### Supplement
########################################################################

# Present and certain dictionaries
present <- miceadds::glm.cluster(formula = presentfocus ~ present_r, data = tables, 
                                 cluster="id", family="binomial") 

cert <- miceadds::glm.cluster(formula = certain_full ~ present_r, data = tables, 
                              cluster="id", family="binomial") 

# Affiliation and prosocial dicitonaries with no overlap
affili_short <- miceadds::glm.cluster(formula = affiliation_s ~ present_r, data = tables, 
                                      cluster="id", family="binomial") 

proso_short <- miceadds::glm.cluster(formula = proso_no_ovlp ~ present_r, data = tables, 
                                     cluster="id", family="binomial") 


grid_present <- qdrg(object = present$glm_res, data = tables)
grid_cert <- qdrg(object = cert$glm_res, data = tables)
grid_affili_short <- qdrg(object = affili_short$glm_res, data = tables)
grid_proso_short <- qdrg(object = proso_short$glm_res, data = tables)

margins_present <- emmeans(grid_present, ~ present_r, type = "response")
margins_certain <- emmeans(grid_cert, ~ present_r, type = "response")
margins_affili_short <- emmeans(grid_affili_short, ~ present_r, type = "response")
margins_proso_short <- emmeans(grid_proso_short, ~ present_r, type = "response")

results_present <- confint(contrast(margins_present, present_vs_not))
results_certain <- confint(contrast(margins_certain, present_vs_not))
results_affili_short <- confint(contrast(margins_affili_short, present_vs_not))
results_proso_short <- confint(contrast(margins_proso_short, present_vs_not))

exp(c(results_present$estimate, results_present$lower.CL, results_present$upper.CL))
exp(c(results_certain$estimate, results_certain$lower.CL, results_certain$upper.CL))
exp(c(results_affili_short$estimate, results_affili_short$lower.CL, results_affili_short$upper.CL))
exp(c(results_proso_short$estimate, results_proso_short$lower.CL, results_proso_short$upper.CL))

