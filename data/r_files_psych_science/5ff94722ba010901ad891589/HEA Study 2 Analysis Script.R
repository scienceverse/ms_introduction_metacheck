if(!(require(pacman))) install.packages('pacman')

p_load(tidyverse, data.table, lme4, lmerTest,
       performance, sjPlot, apaTables,
       psych, lubridate, brms,
       labelled, effsize)

theme_set(theme_minimal())

set.seed(17)

##################################
# Data Analysis
##################################

# Read in data and create factors

daily_raw <- read_csv('HEA 2 data clean.csv', 
                      col_types = cols(), guess_max = 3000)

daily <- daily_raw %>% 
  mutate(coffee.bin = factor(coffee.bin, levels = c('nodrink','drink')),
         soft.bin = factor(soft.bin, ),
         coffee.lagged = factor(coffee.lagged, levels = c('nodrink','drink')),
         coffee.next = factor(coffee.next, levels = c('nodrink','drink')),
         soft.next = factor(soft.next, levels = c('nonsit','sit')))

dailyrep <- daily %>% 
  count(part.num)

daily.bet <- daily %>% 
  group_by(part.num) %>% 
  slice_sample(n = 1) 

# Participants

nrow(daily.bet)

describe(daily.bet$age)
table(daily.bet$gender.cur)

# Single event attribution measure - % who recalled the event

daily.sing <- daily %>% # Create dataframe with one row per participant
  group_by(part.num) %>% 
  filter(!(is.na(recall))) %>%
  slice_sample(n = 1) %>% 
  rename(attsing.fat = specific.att_1,
         attsing.hab = specific.att_2) %>% 
  mutate(attsing.diff = attsing.fat - attsing.hab)

#How many people remembered the single event?

length(daily.sing$recall[daily.sing$recall==1])
length(daily.sing$recall[daily.sing$recall==1]) / nrow(daily.bet)

# Number of participants who completed the follow up survey

nrow(daily.bet) - sum(is.na(daily.bet$fu.att.fat))
(nrow(daily.bet) - sum(is.na(daily.bet$fu.att.fat))) / nrow(daily.bet)

# Sample sizes

length(unique(daily$part.num)) # Level 2 (participants)
nrow(daily) # Level 1 (prompts)

# Descriptives and correlations table

cor.all <- daily.bet %>%
  ungroup() %>% 
  dplyr::select(bfcs, srhi.avg, fatig.mean, att.habit, att.fat, ncof, n)

apa.cor.table(cor.all)

##################################
# Habit and Fatigue Effects on Coffee Drinking - Actual 
##################################

# Uncomment to run Bayesian Analyses

# mb <- brm(coffee.next ~ bfcs.std * fatig.cmc.std + fatig.mean.std +
#              att.fat.std + att.fat.std:fatig.cmc.std + (fatig.cmc.std | part.num),
#              set_prior("normal(0,100)", class = "b"),
#            data = daily, family = "bernoulli",
#            file = 'HEA Study 2 Main Analysis BFIC')

mb <- readRDS('.rds files for Bayesian analyses/HEA Study 2 Main Analysis BFIC.rds') # Read analysis files

summary(mb)

tab_model(mb,
          pred.labels = c("Intercept","Habit strength","Fatigue (within)", "Fatigue (between)",
                          "F. attribution","Habit strength X fatigue (within)",
                          "F. attribution X fatigue (within)"),
          dv.labels = "Likelihood of Drinking Coffee")


plot_model(mb, type = "pred", terms = c("bfcs.std","fatig.cmc.std"),
           line.size = 2, legend.title = "Fatigue (within)",
           axis.title = c("Habit strength",
                          "Coffee drinking likelihood 
in next prompt"), title = "") +
  theme_sjplot(base_size = 18) +
  aes(linetype=group) +
  scale_x_continuous(breaks = c(-2.09739670, -1.317655, -0.3429768, 0.63170101, 1.60637876, 2.58105652),
                     labels = c(1, 5, 10, 15, 20, 25)) +
  scale_color_discrete(labels = c("Low","Average","High"), name = 'Fatigue (Within)') +
  scale_linetype_discrete(labels = c("Low","Average","High"), name = 'Fatigue (Within)') +
  theme(legend.key.width=unit(3, 'line'),
        legend.key = element_rect(color = 'white', fill = 'white')) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

###################
#Morning Analyses
###################

# Create dataframe of just morning analyses

daily.morn <- daily %>%
  arrange(part.num, DAY, SIG) %>% 
  rename(datetime.morn = date.morn) %>% 
  separate(datetime.morn, into = c("date.morn", "time.morn"),
           sep = " ", remove = F) %>% 
  mutate(datetime.morn = as.POSIXct(datetime.morn, 
                                    format = "%m/%d/%Y %H:%M")) %>%
  group_by(part.num, DAY) %>%
  filter(row_number(SIG) == 1, !(is.na(datetime.morn)), !(is.na(coffee.morn))) %>% # Capture only first prompt of the day and remove incomplete responses
  ungroup() %>% 
  mutate(startdatetime = as.POSIXct(startdatetime,
                                    format = '%m/%d/%Y %H:%M'),
         invalid = datetime.morn > startdatetime,
         toolate = ifelse(coffee.morn == "1", 0, 1),
         fatig.morn = as.numeric(fatig.morn),
         fatig.morn.c = scale(fatig.morn)[,1],
         coffee.bin = factor(coffee.bin, labels = c("No","Yes"))
  ) 

nrow(daily.morn) # number of participant-days before exclusions

table(daily.morn$toolate, daily.morn$invalid) # Number of responses made after having already drunk coffee (toolate) and after the first regular EMA prompt of the day (invalid)

daily.morn <- daily.morn %>% 
  filter(toolate == 0, invalid == F) # Remove invalid responses

length(unique(daily.morn$part.num)) # Number of unique participants
nrow(daily.morn) # number of participant-days

m.morn <- glmer(coffee.bin ~ fatig.morn.c * bfcs.std + (1 | part.num),
                data = daily.morn, family = binomial)

summary(m.morn)
tab_model(m.morn)

##################################
# Habit and Fatigue Effects on Coffee Drinking - Attributed 
##################################

# Main test (self-attribution)

t.test(daily.bet$att.fat, daily.bet$att.habit, paired = T)
cohen.d(daily.bet$att.fat, daily.bet$att.habit, paired = T, na.rm = T)

# Single-event attribution - Include only participants who recalled event

test.sing <- filter(daily.sing, recall == 1)

t.test(test.sing$attsing.fat, test.sing$attsing.hab,
       paired = T)
cohen.d(test.sing$attsing.fat, test.sing$attsing.hab,
        paired = T)

t.test(daily.bet$fu.att.fat, daily.bet$fu.att.hab, paired = T) # Incentivized Attribution ratings
cohen.d(daily.bet$fu.att.fat, daily.bet$fu.att.hab, na.rm = T, paired = T)

# [not in manuscript] - Sensitivity analysis - qualitatively identical results when scaling attribution responses with sum > 100% to sum = 100%

t.test(daily.bet$att.fat.adj, daily.bet$att.habit.adj, paired = T) 
cohen.d(daily.bet$att.fat.adj, daily.bet$att.habit.adj, na.rm = T, paired = T) 

###########################
# Effects of Alternative Habit Measure: Context-Specific Habit 
###########################

# Create data set of only situations that were included in the context-specific habit measure

daily.fin <- daily %>% 
  filter(!(is.na(bfcs.fin)), possible == 1 | is.na(possible)) %>% # Remove impossible observations and ones that weren't included in the habit measure
  group_by(part.num) %>% 
  mutate(bfcs.fin.range = range(bfcs.fin)[2]-range(bfcs.fin)[1],
         bfcs.ful = ifelse(bfcs.fin.range > 0, 1, 0))

# Remove participants that only rated a single coffee-drinking situation

unsit <- daily.fin %>% 
  group_by(part.num, coffee.bin, sitid) %>% 
  count() %>% 
  group_by(part.num, coffee.bin) %>% 
  summarize(num = n()) %>%
  pivot_wider(id_cols = 'part.num', names_from = coffee.bin, values_from = num) %>% 
  filter(drink > 1)

length(unique(daily.fin$part.num)) - nrow(unsit) # Number of participants with only 1 coffee drinking situation rating

daily.fin <- filter(daily.fin, part.num %in% unsit$part.num)

# Remove participants with no variance on the context-specific habit measure

length(unique(daily.fin$part.num[daily.fin$bfcs.ful == 0])) # Number of participants with no variance on habit measure

daily.fin <- filter(daily.fin, bfcs.ful == 1)

# Main analysis using context-specific habit measure - uncomment to run

# rep.modb <- brm(coffee.bin ~ bfcs.fin.std * fatig.cmc.lagged.std +
#                   (bfcs.fin.std + fatig.cmc.lagged.std | part.num),
#                 data = daily.fin, family = "bernoulli", cores = parallel::detectCores(),
#                 set_prior("normal(0,100)", class = "b"), iter = 8000,
#                 file = '.rds files for Bayesian analyses/HEA Context-Specific Habit Measure Analyses')

rep.modb <- readRDS('.rds files for Bayesian analyses/HEA Context-Specific Habit Measure Analyses.rds')

tab_model(rep.modb,
          pred.labels = c('Intercept','Habit Strength (Within)','Fatigue (within)', 'Habit X Fatigue'),
          dv.labels = NULL)


#####################################################################
# Exploratory Analyses - Intensity of Experience and Attributions
#####################################################################

cor.test(daily.bet$att.fat, daily.bet$fatig.mean)
cor.test(daily.bet$att.habit, daily.bet$bfcs)
cor.test(daily.bet$att.habit, daily.bet$srhi.avg)

# Extract each participant's habit and fatigue coefficients from the model

coefr_raw <- as.data.frame(coef(rep.modb, pars = c('bfcs.fin.std','fatig.cmc.lagged.std'))$part.num)

coefr <- coefr_raw %>% 
  mutate(part.num = as.numeric(rownames(coefr_raw))) %>%
  rename(rancor_hab = Estimate.bfcs.fin.std,
         rancor_fat = Estimate.fatig.cmc.lagged.std) %>% 
  dplyr::select(part.num, rancor_hab, rancor_fat) %>% 
  mutate(rancor_fat = abs(rancor_fat), # Get absolute value of slope
         rancor_fat = rancor_fat / max(rancor_hab, na.rm = T), # Standardize slope values to 0-1
         rancor_hab = abs(rancor_hab),
         rancor_hab = rancor_hab / max(rancor_hab, na.rm = T),
         coef_diff = rancor_fat - rancor_hab) # Get fatigue - habit difference score of slopes

# Join model coefficients with rest of data

daily.fin <- left_join(daily.fin, coefr, by = 'part.num', all.x = T)

# Create data frame with one row per participant.

daily.fin.bet <- daily.fin %>% 
  group_by(part.num) %>% 
  slice_sample(n = 1) %>% 
  dplyr::select(part.num, bfcs, bfcs.tri, bfcs.std, att.fat, att.habit, att.diff, rancor_hab, rancor_fat, coef_diff) %>% 
  mutate(att.diff = att.diff / 100, # scale attribution scores to better fit actual scores
         att.fat = att.fat / 100,
         att.habit = att.habit / 100,
         bfcs.tri = case_when(
           bfcs.tri == 0 ~ 'Weak',
           bfcs.tri == 1 ~ 'Moderate',
           bfcs.tri == 2 ~ 'Strong'
         )
         )

daily.fin.bet$bfcs.tri <- fct_relevel(daily.fin.bet$bfcs.tri, c('Weak','Moderate','Strong'))

daily.fin.bet %>% 
  group_by(bfcs.tri) %>% 
  summarize(avg = mean(coef_diff),
            stdev = sd(coef_diff),
            avg.att = mean(att.diff, na.rm = T),
            stdev.att = sd(att.diff, na.rm = T)) %>% 
  mutate(overalldiff = avg - avg.att) %>% 
  mutate_at(vars(avg:overalldiff), round, digits = 2)

#######################################################################################
# Exploratory Analyses - Downstream Effects of Attribution Accuracy on Well-Being 
#######################################################################################

cor.test(daily.bet$meanmood, daily.bet$att.diff)

summary(attmood <- lm(meanmood ~ fatig.mean.std + bfcs.std + att.diff.std, data = daily.bet))
tab_model(attmood, show.std = T)


########################################
# Supplemental Materials Analysis
########################################

# Main analysis using SRBAI

# mb.srb <- brm(coffee.next ~ srbai.avg.std * fatig.cmc.std + fatig.mean.std +
#                      att.fat + att.fat.std:fatig.cmc.std + (fatig.cmc.std | part.num),
#                    set_prior("normal(0,100)", class = "b"),
#                    data = daily, family = "bernoulli",
#                    file = 'HEA Study 2 Main Analysis SRBAI')

mb.srb <- readRDS('HEA Study 2 Main Analysis SRBAI.rds')

tab_model(mb.srb,
          pred.labels = c("Intercept","Habit strength","Fatigue (within)", "Fatigue (between)",
                          "F. attribution","Habit strength X fatigue (within)",
                          "F. attribution X fatigue (within)"),
          dv.labels = "Likelihood of Drinking Coffee",
          file = 'HEA Study 2 SRHI Analysis Table.doc')

# Main analysis with soft drink consumption

# mb.sd <- brm(soft.next ~ sd.bfcs.std * fatig.cmc.std + fatig.mean.std +
#                      sd.att.fat.std + sd.att.fat.std:fatig.cmc.std + (fatig.cmc.std | part.num),
#                    set_prior("normal(0,100)", class = "b"),
#                    data = daily, family = "bernoulli",
#                    file = 'HEA Study 2 Soft Drink Analysis')

mb.sd <- readRDS('HEA Study 2 Soft Drink Analysis.rds')

tab_model(mb.sd,
          pred.labels = c("Intercept","Habit strength","Fatigue (within)", "Fatigue (between)",
                          "F. attribution","Habit strength X fatigue (within)",
                          "F. attribution X fatigue (within)"),
          dv.labels = "Likelihood of Drinking Soft Drinks",
          file = 'Soft drinks analyses.doc')

plot_model(mb.sd, type = 'pred', terms = c('sd.bfcs.std','fatig.cmc.std'))

# Main analysis with covariates (intentions to drink coffee and liking of coffee)

# mcov <- brm(coffee.next ~ bfcs.std * fatig.cmc.std + fatig.mean.std +
#              att.fat.std + att.fat.std:fatig.cmc.std + cofint.std + cofenj.std + (fatig.cmc.std | part.num),
#              set_prior("normal(0,100)", class = "b"),
#            data = daily, family = "bernoulli",
#            file = 'HEA Study 2 Main Analysis with Covariates')

mcov <- readRDS('HEA Study 2 Main Analysis with Covariates.rds')

tab_model(mcov,
          pred.labels = c("Intercept","Habit strength","Fatigue (within)", "Fatigue (between)",
                          "F. attribution","Intention","Liking","Habit strength X fatigue (within)",
                          "F. attribution X fatigue (within)"),
          dv.labels = 'Likelihood of Drinking Coffee',
          file = 'Covariate Analysis.doc')
