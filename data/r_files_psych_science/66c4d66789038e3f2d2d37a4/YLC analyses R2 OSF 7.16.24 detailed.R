# YLC Study analyses
# Predicting well-being and depression from traits and NI

require(psych)
require(gridExtra)
require(tidyverse)
require(lme4)
require(lmerTest)
require(ggExtra)
require(effects)
require(ggeffects)
require(GGally)
require(gridExtra)
require(patchwork)
require(sjPlot)

# Load data ====
ylc <- read_csv("YLC processed data R2 OSF 7.16.24.csv")

# Descriptives ====
## Find IDs with narrative data to include in study ----
IDs <- ylc %>% 
  drop_na(ac_val) %>% 
  select(ID) %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  distinct(ID) %>%
  slice(1) %>%
  ungroup() 

## Demographics ----
# See manuscript "Participants and procedures"
bl_data <- ylc %>% 
  filter(ID %in% IDs$ID) %>%
  select(ID, year, "t1: date of birth (dd-mmm-year)", "date completed interview (dd-mmm-year)",
         "t1: sex of respondent", "highest level of education completed", "race/ethnicity") %>%
  filter(year==1) 

# Age at baseline
# See manuscript "Participants and procedures"
lubridate::time_length(difftime(bl_data$`date completed interview (dd-mmm-year)`, bl_data$`t1: date of birth (dd-mmm-year)`), unit = "years") %>%
  round(1) %>%
  describe()

# Sex
# See manuscript "Participants and procedures"
rbind(table(bl_data$`t1: sex of respondent`), round(table(bl_data$`t1: sex of respondent`)/nrow(bl_data)*100, 1)) #Table with count and %

# Education
# See manuscript "Participants and procedures"
rbind(table(bl_data$`highest level of education completed`), round(table(bl_data$`highest level of education completed`)/nrow(bl_data)*100, 1)) #Table with count and %

# Race/ethnicity
# See manuscript "Participants and procedures"
rbind(table(bl_data$`race/ethnicity`), round(table(bl_data$`race/ethnicity`)/nrow(bl_data)*100, 1)) #Table with count and %

## Describe time-invariant data at baseline ----
# See manuscript Table 2
mean_data <- ylc %>% 
  filter(ID %in% IDs$ID) %>%
  select(ID, year, bdi_pmean_uc, swls_pmean_uc, pwb_pmean_uc, wb_comp_pmean_uc, 
         extra, neur, open, consc, agree, 
         end_val_pmean_uc, ac_val_pmean_uc) %>% #Note descriptive stats report uncentered data ("_uc")
  group_by(ID) %>%
  slice(1) %>% #Obtain first observation for each pt
  ungroup()

describe(select(mean_data, -ID, -year)) 
cors1 <- corr.test(select(mean_data, -ID, -year), adjust = "fdr")
round(cors1$r, 2)
round(cors1$p, 3) #Note FDR-corrected p appears above the diagonal

## Describe time-variant data ----
# See manuscript Table 2
yearly_data <- ylc %>% 
  select(ID, year, bdi_pmc, swls_pmc, pwb_pmc, wb_comp, 
         end_val_pmc, ac_val_pmc,
         agn_val_pmc, com_val_pmc) %>%
  drop_na(ac_val_pmc) %>% 
  group_by(ID) %>%
  filter(n() > 1) %>%
  ungroup() 

describe(select(yearly_data, -ID, -year))
cors2 <- corr.test(select(yearly_data, -ID, -year), adjust = "fdr")
round(cors2$r, 2)
round(cors2$p, 3) #Note FDR-corrected p appears above the diagonal

# Participants per year with codable narratives
# See manuscript "Materials > Narrative Identity > Yearly Life Challenges"
rbind(table(yearly_data$year), round(table(yearly_data$year)/length(unique(yearly_data$ID))*100, 1))

# Number of codable narratives per participant
# See manuscript "Materials > Narrative Identity > Yearly Life Challenges"
table(yearly_data$ID, yearly_data$year) %>% 
  rowSums() %>% 
  describe()

# Word counts
# See manuscript "Materials > Narrative Identity > Yearly Life Challenges"
ylc %>%
  drop_na(ac_val) %>% 
  select(WC) %>% 
  unlist() %>%
  describe()

# Growth curve models ====

## Define function to run MLM model sequence for a given predictor and outcome ----
ylc_models <- function(outcome, predictor, data){
  # Select complete data for models
  data <- data  %>% 
    select(ID, year_cent5, extra_z, neur_z, open_z, consc_z, agree_z, outcome, 
           paste0(predictor, "_pmean"), paste0(predictor, "_pmc"),
           end_val_pmean, end_val_pmc) %>%
    drop_na() %>% 
    group_by(ID) %>%
    filter(n() > 1) %>%
    ungroup()
  output <- list()
  # Build list of models
  output$gc <- lmer(as.formula(paste0(outcome, "~ year_cent5 + (1 + year_cent5 | ID)")), data, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))
  output$ni_int <- lmer(as.formula(paste0(outcome, "~ year_cent5 * ", paste0(predictor, "_pmean"), " + ", paste0(predictor, "_pmc"),  "+ end_val_pmean * year_cent5 + end_val_pmc + (1 + year_cent5 | ID)")), data, 
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))
  output$tr_int <- lmer(as.formula(paste0(outcome, "~ year_cent5 * extra_z + year_cent5 * neur_z + year_cent5 * open_z + year_cent5 * consc_z + year_cent5 * agree_z + (1 + year_cent5 | ID)")), data, 
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))
  output$all_int <- lmer(as.formula(paste0(outcome, "~ year_cent5 * ",paste0(predictor, "_pmean"), " + ", paste0(predictor, "_pmc"), "+ end_val_pmean * year_cent5 + end_val_pmc + year_cent5 * extra_z + year_cent5 * neur_z + year_cent5 * open_z + year_cent5 * consc_z + year_cent5 * agree_z + (1 + year_cent5 | ID)")), data, 
                         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))
  # Return list of models
  return(output)
}

## Combined agency/communion models ----

### Run all models using the function we defined above ----
bdi_ac_mods <- ylc_models("bdi_sz", "ac_val", ylc)
wb_ac_mods <- ylc_models("wb_comp", "ac_val", ylc)

### Compare model fit ----
# See manuscript Table 3
# Note that the table reports 4 sets of chi squared comparisons:
# Model 1 > Model 2 > Model 4 for depression
# Model 1 > Model 3 > Model 4 for depression
# Model 1 > Model 2 > Model 4 for well-being
# Model 1 > Model 3 > Model 4 for well-being
# The four blocks of code below produce each of these model sequences
bdi_ac_mods[setdiff(names(bdi_ac_mods), "ni_int")] %>% #Drop model "ni_int" from model sequence
  with(do.call(anova, lapply(names(.), as.name))) #Compare models 
bdi_ac_mods[setdiff(names(bdi_ac_mods), "tr_int")] %>% #Drop model "tr_int" from model sequence
  with(do.call(anova, lapply(names(.), as.name))) #Compare models

wb_ac_mods[setdiff(names(wb_ac_mods), "ni_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))
wb_ac_mods[setdiff(names(wb_ac_mods), "tr_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))

### Examine models ----
# Table 4, depression model
# Note: cent5 = "centered on Year 5"; "pmean" = person-mean, aka. between-person;
# "pmc" = person-mean-centered, aka. within-person
tab_model(bdi_ac_mods$all_int, show.intercept = F, collapse.ci = F)
# Table 4, well-being model
tab_model(wb_ac_mods$all_int, show.intercept = F, collapse.ci = F)


## Agency only models ----

### Run all models using the function we defined above ----
bdi_agn_mods <- ylc_models("bdi_sz", "agn_val0", ylc)
wb_agn_mods <- ylc_models("wb_comp", "agn_val0", ylc)

### Compare model fit ----
# See supplement, Table S1.1
# Note that the table reports 4 sets of chi squared comparisons:
# Model 1 > Model 2 > Model 4 for depression
# Model 1 > Model 3 > Model 4 for depression
# Model 1 > Model 2 > Model 4 for well-being
# Model 1 > Model 3 > Model 4 for well-being
# The four blocks of code below produce each of these model sequences
bdi_agn_mods[setdiff(names(bdi_agn_mods), "ni_int")] %>% #Drop model "ni_int" from comparison
  with(do.call(anova, lapply(names(.), as.name))) #Compare models using anova() function
bdi_agn_mods[setdiff(names(bdi_agn_mods), "tr_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))

wb_agn_mods[setdiff(names(wb_agn_mods), "ni_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))
wb_agn_mods[setdiff(names(wb_agn_mods), "tr_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))

### Examine agency models ----
# See supplement, Table S1.2, depression model
# Note: cent5 = "centered on Year 5"; "pmean" = person-mean, aka. between-person;
# "pmc" = person-mean-centered, aka. within-person
# "agn0" = agency with zeroes added for absence of agency (see manuscript "Statistical Analysis > Growth Curve Modeling > Preprocessing")
tab_model(bdi_agn_mods$all_int, show.intercept = F, collapse.ci = F)
# See supplement, Table S1.2, well-being model
tab_model(wb_agn_mods$all_int, show.intercept = F, collapse.ci = F)


## Communion only models ----

### Run all models using the function we defined above ----
bdi_com_mods <- ylc_models("bdi_sz", "com_val0", ylc)
wb_com_mods <- ylc_models("wb_comp", "com_val0", ylc)

### Compare model fit ----
# See supplement, Table S2.1
# Note that the table reports 4 sets of chi squared comparisons:
# Model 1 > Model 2 > Model 4 for depression
# Model 1 > Model 3 > Model 4 for depression
# Model 1 > Model 2 > Model 4 for well-being
# Model 1 > Model 3 > Model 4 for well-being
# The four blocks of code below produce each of these model sequences
bdi_com_mods[setdiff(names(bdi_com_mods), "ni_int")] %>% #Drop model "ni_int" from comparison
  with(do.call(anova, lapply(names(.), as.name))) #Compare models using anova() function
bdi_com_mods[setdiff(names(bdi_com_mods), "tr_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))

wb_com_mods[setdiff(names(wb_com_mods), "ni_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))
wb_com_mods[setdiff(names(wb_com_mods), "tr_int")] %>%
  with(do.call(anova, lapply(names(.), as.name)))

### Examine communion models ----
# See supplement, Table S2.2, depression model
# Note: cent5 = "centered on Year 5"; "pmean" = person-mean, aka. between-person;
# "pmc" = person-mean-centered, aka. within-person
# "com0" = communion with zeroes added for absence of agency (see manuscript "Statistical Analysis > Growth Curve Modeling > Preprocessing")
tab_model(bdi_com_mods$all_int, show.intercept = F, collapse.ci = F)
# See supplement, Table S2.2, well-being model
tab_model(wb_com_mods$all_int, show.intercept = F, collapse.ci = F)



# Plot findings ====

## Fig 1. Change over time with individual pts and group trends ----
# Recalculate models outside model function for easier plotting
bdi_plot_gc <- lmer(bdi_sz ~ year_cent5 + (1 + year_cent5 | ID), ylc, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))
wb_plot_gc <- lmer(wb_comp ~ year_cent5 + (1 + year_cent5 | ID), ylc, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 99999)))

# Extract BDI fit line to superimpose on spaghetti plot
bdi_gc_plot_data <- Effect("year_cent5",
                           mod=bdi_plot_gc) %>%
  as.data.frame() %>%
  mutate(fit = (((fit*0.264) + .508) ^2) - .01) # Backtransform and unstandardize BDI

# Extract WB fit line to superimpose on spaghetti plot
wb_gc_plot_data <- Effect("year_cent5",
                          mod=wb_plot_gc) %>%
  as.data.frame()

# BDI plot
dep_sp_tr <- ggplot(ylc, aes(x = year_cent5+5, y = bdi, color = ID, group = ID)) +
  geom_line(alpha = .3) +
  labs(title="A. Depression", 
       y = "BDI Mean", 
       x = "Year in Study") +
  theme_light() +
  scale_x_continuous(breaks = c(1,3,5,7,9)) +
  theme(text=element_text(size=10), aspect.ratio=1.2, panel.grid = element_blank(),
        legend.position = "none") +
  geom_segment(x = 1, xend = 9, 
               y = bdi_gc_plot_data$fit[[1]], yend = bdi_gc_plot_data$fit[[5]], 
               linewidth=1.2, color = "black") 

# WB plot
wb_sp_tr <- ggplot(ylc, aes(x = year_cent5+5, y = wb_comp, color = ID, group = ID)) +
  geom_line(alpha = .3) +
  labs(title="B. Well-Being", 
       y = "Well-Being Composite", 
       x = "Year in Study") +
  theme_light() +
  scale_x_continuous(breaks = c(1,3,5,7,9)) +
  theme(text=element_text(size=10), aspect.ratio=1.2, panel.grid = element_blank(),
        legend.position = "none") +
  geom_segment(x = 1, xend = 9, 
               y = wb_gc_plot_data$fit[[1]], yend = wb_gc_plot_data$fit[[5]], 
               linewidth=1.2, color = "black") 

# Display plots
grid.arrange(dep_sp_tr, wb_sp_tr, nrow = 1)


## Fig 2. Interaction plots ----

# Plot +/-1 SD for each variable:
ylc %>% 
  select(ac_val_pmean, agn_val0_pmean, com_val0_pmean) %>% 
  describe()
# A/C -0.41, 0.41
# Agn -0.36, 0.36
# Com -0.31, 0.31

a <- ggpredict(bdi_ac_mods$all_int, c("year_cent5", "ac_val_pmean[-0.41, 0.41]")) %>% 
  plot(alpha = 0.2, show_data = F, colors = "system", limits = c(-.55, .45)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "A. Depression",
       subtitle = "As a Function of \nMotivational Fulfillment",
       y = "Depression",
       x = "Year",
       color = "Motivational Fulfillment \n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = F)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

b <- ggpredict(wb_ac_mods$all_int, c("year_cent5", "ac_val_pmean[-0.36, 0.36]")) %>% 
  plot(alpha = 0.2, show_data = F, limits = c(-.40, .60)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "B. Well-Being",
       subtitle = "As a Function of \nMotivational Fulfillment",
       y = "Well-Being Composite",
       x = "Year",
       color = "Motivational Fulfillment \n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = T)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

c <- ggpredict(bdi_agn_mods$all_int, c("year_cent5", "agn_val0_pmean[-0.31, 0.31]")) %>% 
  plot(alpha = 0.2, show_data = F, colors = "system", limits = c(-.55, .45)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "C. Depression",
       subtitle = "As a Function of \nAgency Fulfillment",
       y = "Depression",
       x = "Year",
       color = "Agency Fulfillment        \n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = F)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

d <- ggpredict(wb_agn_mods$all_int, c("year_cent5", "agn_val0_pmean[-0.41, 0.41]")) %>% 
  plot(alpha = 0.2, show_data = F, limits = c(-.40, .60)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "D. Well-Being",
       subtitle = "As a Function of \nAgency Fulfillment",
       y = "Well-Being Composite",
       x = "Year",
       color = "Agency Fulfillment        \n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = T)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

e <- ggpredict(bdi_com_mods$all_int, c("year_cent5", "com_val0_pmean[-0.36, 0.36]")) %>% 
  plot(alpha = 0.2, show_data = F, colors = "system", limits = c(-.55, .45)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "E. Depression",
       subtitle = "As a Function of \nCommunion Fulfillment",
       y = "Depression",
       x = "Year",
       color = "Communion Fulfillment\n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = F)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

f <- ggpredict(wb_com_mods$all_int, c("year_cent5", "com_val0_pmean[-0.31, 0.31]")) %>% 
  plot(alpha = 0.2, show_data = F, limits = c(-.40, .60)) + 
  scale_x_continuous(labels=function(x) x+5) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  labs(title = "F. Well-Being",
       subtitle = "As a Function of \nCommunion Fulfillment",
       y = "Well-Being Composite",
       x = "Year",
       color = "Communion Fulfillment\n(Between-Person)") +
  theme_bw() + 
  scale_colour_brewer(palette = "Set1",
                      labels = c("-1 SD", "+1 SD"),
                      guide = guide_legend(reverse = T)) +
  scale_fill_brewer(palette = "Set1") +
  theme(panel.grid = element_blank())

grid.arrange(a,b,c,d,e,f, nrow = 3)
