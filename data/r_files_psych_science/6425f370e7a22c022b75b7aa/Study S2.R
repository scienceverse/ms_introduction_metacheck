library(groundhog)

groundhog_day_lmer = "2020-01-01"
groundhog.library('nlme',groundhog_day_lmer)
groundhog.library('MASS',groundhog_day_lmer)
groundhog.library('lattice',groundhog_day_lmer)
groundhog.library('Matrix',groundhog_day_lmer)
groundhog.library(lmerTest, groundhog_day_lmer)
groundhog.library(lme4, groundhog_day_lmer)


groundhog_day = "2020-01-01"

groundhog.library(tidyverse, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(ggrepel, groundhog_day)
groundhog.library(formattable, groundhog_day)
groundhog.library(ggeffects, groundhog_day)
groundhog.library(sjPlot, groundhog_day)
groundhog.library(ggpubr, groundhog_day)

library(Hmisc)


#Get rid of test runs and Qualtrics cruft
raw = raw[5:nrow(raw),]

raw$id = 1:nrow(raw)

names(raw)
raw$englishCheck
#71 fails

raw$fail_english_check = ifelse(raw$id == 71, 1,0)

#Create a lookup table for start and end values of form;
# start question_num end
# xx%    1           uu%
lookup_raw = raw[1,138:175]
lookup_raw = t(lookup_raw)
lookup = data.frame('start_prob' = as.numeric(t(lookup_raw)), 'wtp_question' = parse_number(row.names(lookup_raw)))
lookup$end_prob = Hmisc::Lag(lookup$start, -1)
lookup = lookup[seq(1, nrow(lookup), 2),]


# Go wide to long
long = raw %>% pivot_longer(cols = starts_with('X'), names_to = 'wtp_question', names_prefix = "X", values_to = 'wtp')

keeps = c("wtp_question", "wtp", "fail_english_check", "attentionCheck", "wtp60to60_1", "WTP30to30_1", "id")
long = long[,keeps]
long$wtp_question = substr(long$wtp_question, 1, nchar(long$wtp_question)-1)
long$wtp_question =parse_number(long$wtp_question)

#Merge on start/end values
long = merge(long, lookup, by = "wtp_question")


#Create perc reduction, make percents percents
long$start_prob = long$start_prob/100
long$end_prob = long$end_prob/100
long$perc_reduction = 1 - (1.00001-long$end_prob)/(1.000001-long$start_prob)


#More exclusion
long$pass_attn = ifelse(long$attentionCheck == "Indicate your willingness to pay to change your chances of winning a lottery", 1, 0)

long$wtp_control_60to60 = long$wtp60to60_1
long$wtp_control_30to30 = long$WTP30to30_1


#TODO - check this
wtp_cutoff = 40

long$pass_all_wtp_controls = ifelse(long$wtp_control_30to30< wtp_cutoff & long$wtp_control_60to60 < wtp_cutoff, 1, 0)

#Make it all numeric
long = data.frame(lapply(long, as.numeric))

#create dataset with and without exclusions
long_nox = long
long = long[long$pass_attn==1 & long$fail_english_check==0 & long$pass_all_wtp_controls == 1,]


#Analysis
null_mod = lmer(wtp ~ 1 + (1 | id), data=long[long$end_prob < 1,])
fr_mod = lmer(wtp ~ perc_reduction + (1 | id), data=long[long$end_prob < 1,])
ob_mod = lmer(wtp ~ start_prob + (1 | id), data=long[long$end_prob < 1,])

anova(null_mod, fr_mod)
anova(ob_mod, fr_mod)


null_modnox = lmer(wtp ~ 1 + (1 | id), data=long_nox[long_nox$end_prob < 1,])
fr_modnox = lmer(wtp ~ perc_reduction + (1 | id), data=long_nox[long_nox$end_prob < 1,])
ob_modnox = lmer(wtp ~ start_prob + (1 | id), data=long_nox[long_nox$end_prob < 1,])

anova(null_modnox, fr_modnox)
anova(ob_modnox, fr_modnox)


#Graph
sqrt_n = sqrt(length(unique(long$id)))
wtp_by_start = long %>% group_by(start_prob) %>% summarise(wtp = mean(wtp),se = sd(wtp, na.rm = T)/sqrt_n, sd = sd(wtp, na.rm=T))

grps = 'start_prob'
wtp_table = desc_statby(long, 'wtp', grps, ci = 0.95)


title = "Mean WTP for a 10 Percentage Point Increase in Winning Probability by Starting Probability"
xLab = "Starting probability of winning lottery"
yLab = "Mean WTP"

ggplot(data=wtp_table, aes(x = start_prob, y = mean)) + geom_point() + scale_y_continuous(labels = scales::dollar_format()) + scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1.1,.1)) + ggtitle(title) + xlab(xLab) + ylab(yLab) + geom_errorbar(aes(x = start_prob, ymin = mean - se, ymax = mean + se)) + theme_light()


#
# Run from here to get AICcModAvg results
#

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(AICcmodavg)

raw = read.csv(here("study_s5_data.csv"))

#Get rid of test runs and Qualtrics cruft
raw = raw[5:nrow(raw),]

raw$id = 1:nrow(raw)

names(raw)
raw$englishCheck
#71 fails

raw$fail_english_check = ifelse(raw$id == 71, 1,0)

#Create a lookup table for start and end values of form;
# start question_num end
# xx%    1           uu%
lookup_raw = raw[1,138:175]
lookup_raw = t(lookup_raw)
lookup = data.frame('start_prob' = as.numeric(t(lookup_raw)), 'wtp_question' = parse_number(row.names(lookup_raw)))
lookup$end_prob = Hmisc::Lag(lookup$start, -1)
lookup = lookup[seq(1, nrow(lookup), 2),]


# Go wide to long
long = raw %>% pivot_longer(cols = starts_with('X'), names_to = 'wtp_question', names_prefix = "X", values_to = 'wtp')

keeps = c("wtp_question", "wtp", "fail_english_check", "attentionCheck", "wtp60to60_1", "WTP30to30_1", "id")
long = long[,keeps]
long$wtp_question = substr(long$wtp_question, 1, nchar(long$wtp_question)-1)
long$wtp_question =parse_number(long$wtp_question)

#Merge on start/end values
long = merge(long, lookup, by = "wtp_question")


#Create perc reduction, make percents percents
long$start_prob = long$start_prob/100
long$end_prob = long$end_prob/100
long$perc_reduction = 1 - (1.00001-long$end_prob)/(1.000001-long$start_prob)


#More exclusion
long$pass_attn = ifelse(long$attentionCheck == "Indicate your willingness to pay to change your chances of winning a lottery", 1, 0)

long$wtp_control_60to60 = long$wtp60to60_1
long$wtp_control_30to30 = long$WTP30to30_1


wtp_cutoff = 40

long$pass_all_wtp_controls = ifelse(long$wtp_control_30to30< wtp_cutoff & long$wtp_control_60to60 < wtp_cutoff, 1, 0)

#Make it all numeric
long = data.frame(lapply(long, as.numeric))

#create dataset with and without exclusions
long_nox = long
long = long[long$pass_attn==1 & long$fail_english_check==0 & long$pass_all_wtp_controls == 1,]


#Analysis
null_mod = lmer(wtp ~ 1 + (1 | id), data=long[long$end_prob < 1,])
fr_mod = lmer(wtp ~ perc_reduction + (1 | id), data=long[long$end_prob < 1,])
ob_mod = lmer(wtp ~ start_prob + (1 | id), data=long[long$end_prob < 1,])

anova(null_mod, fr_mod)
anova(ob_mod, fr_mod)

models = list(fr_mod, ob_mod)
aic_comp = aictab(cand.set = models)
print(aic_comp)

null_modnox = lmer(wtp ~ 1 + (1 | id), data=long_nox[long_nox$end_prob < 1,])
fr_modnox = lmer(wtp ~ perc_reduction + (1 | id), data=long_nox[long_nox$end_prob < 1,])
ob_modnox = lmer(wtp ~ start_prob + (1 | id), data=long_nox[long_nox$end_prob < 1,])

anova(null_modnox, fr_modnox)
anova(ob_modnox, fr_modnox)

models = list(fr_mod, ob_mod)
aic_comp = aictab(cand.set = models)
print(aic_comp)


#Graph
sqrt_n = sqrt(length(unique(long$id)))
wtp_by_start = long %>% group_by(start_prob) %>% summarise(wtp = mean(wtp),se = sd(wtp, na.rm = T)/sqrt_n, sd = sd(wtp, na.rm=T))

grps = 'start_prob'
wtp_table = desc_statby(long, 'wtp', grps, ci = 0.95)


title = "Mean WTP for a 10 Percentage Point Increase in Winning Probability by Starting Probability"
xLab = "Starting probability of winning lottery"
yLab = "Mean WTP"

ggplot(data=wtp_table, aes(x = start_prob, y = mean)) + geom_point() + scale_y_continuous(labels = scales::dollar_format()) + scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1.1,.1)) + ggtitle(title) + xlab(xLab) + ylab(yLab) + geom_errorbar(aes(x = start_prob, ymin = mean - se, ymax = mean + se)) + theme_light()


