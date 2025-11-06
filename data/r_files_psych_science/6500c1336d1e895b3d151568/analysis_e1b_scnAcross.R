library(tidyverse)
library(lme4)
library(ggthemes)

# clear env & import data
rm(list=ls())
d <- read_csv("../2 - data/subjdata_e1b_scnAcross.csv",
              col_types = cols(id = 'f'))


# Outlier Responses
#--------------------------------------------------------------~

# accuracy
d <- 
  d %>% 
  group_by(id) %>% 
  mutate(meanAcc = mean(trialaccuracy, na.rm=T)) %>% 
  filter(meanAcc > 0.65)

# Basic RT removal
d <- 
  d %>% 
  group_by(id) %>% 
  mutate(visionRT =
           case_when(
             visionRT < 200 ~ NA_real_,
             visionRT < 8000 ~ visionRT,
             visionRT > 8000 ~ NA_real_
           ))


# individual RT removal
d <- 
  d %>% 
  group_by(id) %>% 
  mutate(mRT = mean(visionRT, na.rm=T),
         sRT = sd(visionRT, na.rm=T),
         hi = mRT + 3*sRT,
         lo = mRT - 3*sRT,
         visionRT = ifelse(visionRT>=hi, NA, visionRT),
         visionRT = ifelse(visionRT<=lo, NA, visionRT))


# overall RT removal
d <- 
  d %>% 
  group_by(id) %>% 
  mutate(m = mean(visionRT, na.rm=T),
         s = sd(visionRT, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(mGroup = mean(visionRT, na.rm=T),
         sGroup = sd(visionRT, na.rm=T),
         coLow  = ifelse(m < (mGroup - 3*sGroup), T, F),
         coHigh = ifelse(m > (mGroup + 3*sGroup), T, F)) %>% 
  filter(!id %in% id[coLow | coHigh]) %>% 
  select(-c(hi, lo, m, s, mGroup, sGroup, coLow, coHigh))


# Statistical Analyses
#--------------------------------------------------------------~

# get average RT
d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T))

# Mixed effect analysis with "sound" and image "group" as factors 
iaData <- d %>% filter(trialaccuracy == 1)
iaNull <- lmer(visionRT ~ 1 + (1|id) + (1|audName) + (1|groupName), iaData)
iaFull <- lmer(visionRT ~ validity + (1|id) + (1|audName) + (1|groupName), iaData)
# anova(iaNull, iaFull)
pbkrtest::KRmodcomp(iaNull, iaFull) ## recommended, ~always agrees with anova


# run analysis statistics
stats <- 
  d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T)) %>% 
  {t.test(.$rt ~ .$validity, paired = T)}; stats

# effect size
dz <- 
  d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T)) %>% 
  summarize(eff = diff(rt)) %>% 
  summarize(dz = round(abs(mean(eff) / sd(eff)),2)) %>% 
  as.numeric(); dz


accStats <- 
  d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(trialaccuracy, na.rm=T)) %>% 
  {t.test(.$acc ~ .$validity, paired = T)}; accStats

# Bayes Factor (in favor of null: Accuracy)
1/BayesFactor::ttest.tstat(t = accStats$statistic[[1]], 
                         n1=accStats$parameter[[1]]+1, 
                         simple=T)

# effect size
d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(trialaccuracy, na.rm=T)) %>% 
  summarize(eff = diff(acc)) %>% 
  summarize(dz = round(abs(mean(eff) / sd(eff)),2)) %>% 
  as.numeric()


#  run d' analysis
dpStats <- 
  d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(c(trialaccuracy, 0.5), na.rm=T),
            dp  = sqrt(2) * qnorm(acc)) %>% 
  {t.test(.$dp ~ .$validity, paired = T)}; dpStats

# Bayes Factor (in favor of null; dprime)
1/BayesFactor::ttest.tstat(t = dpStats$statistic[[1]], 
                           n1=dpStats$parameter[[1]]+1, 
                           simple=T)

# effect size over d'
d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(c(trialaccuracy, 0.5), na.rm=T),
            dp  = sqrt(2) * qnorm(acc)) %>% 
  summarize(eff = diff(dp)) %>% 
  summarize(dz = round(abs(mean(eff) / sd(eff)), 3)) %>% 
  as.numeric()


# Visualize Data
#--------------------------------------------------------------~

# Response Time data
yMin = 3600
yMax = 4400
d %>% 
  group_by(id, validity) %>% 
  summarize(cm = mean(visionRT, na.rm=T)) %>% 
  group_by(id) %>% 
  mutate(sm = mean(cm)) %>% 
  group_by(validity) %>% 
  summarize(rt = mean(cm),
            rtMax = rt + (sd(cm - sm) / sqrt(n())),
            rtMin = rt - (sd(cm - sm) / sqrt(n()))) %>%
  ggplot(aes(x = as.factor(validity), y = rt)) +
  geom_errorbar(aes(ymin = rtMin, ymax = rtMax),
                width = 0.65,
                linewidth = 2.6) +
  geom_col(fill = c("#598C59", "#A6C9A6"),
           width = 0.85) +
  geom_segment(aes(x = 1, xend = 2, y = yMax-200, yend = yMax-200),
               linewidth = 1.8) +
  annotate('text', x=1.5, y=yMax-170, size = 16,
           label = "°°°") +
  theme_classic() +
  coord_cartesian(ylim = c(yMin,yMax)) +
  labs(x = "Condition",
       y = "Response Time (ms)") +
  scale_x_discrete(labels = c("Incongruent", "Congruent")) +
  scale_y_continuous(breaks = seq(yMin,yMax,200)) +
  theme(axis.line = element_line(size = 2),
        axis.ticks = element_blank(),
        axis.text = element_text(face = 'bold'),
        text = element_text(size = 22))

# Plot Accuracy data
d %>% 
  group_by(id, validity) %>% 
  summarize(cm = mean(trialaccuracy, na.rm=T)) %>% 
  group_by(id) %>% 
  mutate(sm = mean(cm)) %>% 
  group_by(validity) %>% 
  summarize(rt = mean(cm),
            rtMax = rt + (sd(cm - sm) / sqrt(n())),
            rtMin = rt - (sd(cm - sm) / sqrt(n()))) %>% 
  ggplot(aes(x = as.factor(validity), y = rt)) +
  geom_errorbar(aes(ymin = rtMin, ymax = rtMax),
                width = 0.65,
                linewidth = 2.6) +
  geom_col(fill = c("skyblue4", "skyblue"),
           width = 0.85) +
  geom_segment(aes(x = 1, xend = 2, y = .99, yend = .99),
               linewidth = 1.8) +
  annotate('text', x=1.5, y=1.005, size = 8,
           label = "n.s.") +
  theme_classic() +
  coord_cartesian(ylim = c(.8,1.02)) +
  labs(x = "Condition",
       y = "Accuracy") +
  scale_x_discrete(labels = c("Incongruent", "Congruent")) +
  scale_y_continuous(breaks = seq(.75,1,.05)) +
  theme(axis.line = element_line(size = 2),
        axis.ticks = element_blank(),
        axis.text = element_text(face = 'bold'),
        text = element_text(size = 22))


# Plot d' data
d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(c(trialaccuracy, 0.5), na.rm=T),
            dp  = sqrt(2) * acc) %>% 
  group_by(id) %>% 
  mutate(sm = mean(dp)) %>% 
  group_by(validity) %>% 
  summarize(m = mean(dp),
            mMax = m + (sd(dp - sm) / sqrt(n())),
            mMin = m - (sd(dp - sm) / sqrt(n()))) %>% 
  ggplot(aes(x = as.factor(validity), y = m)) +
  geom_errorbar(aes(ymin = mMin, ymax = mMax),
                width = 0.65,
                linewidth = 2.6) +
  geom_col(fill = c("skyblue4", "skyblue"),
           width = 0.85) +
  geom_segment(aes(x = 1, xend = 2, y = 1.4, yend = 1.4),
               linewidth = 1.8) +
  annotate('text', x=1.5, y= 1.42, size = 8,
           label = "n.s.") +
  theme_classic() +
  coord_cartesian(ylim = c(1.1, 1.43)) +
  labs(x = "Condition",
       y = "d'") +
  scale_x_discrete(labels = c("Incongruent", "Congruent")) +
  scale_y_continuous(breaks = round(seq(1.1, 1.5, length.out = 5), 2)) +
  theme(axis.line = element_line(size = 2),
        axis.ticks = element_blank(),
        axis.text = element_text(face = 'bold'),
        text = element_text(size = 22))

# Plot Accuracy by RT type
d %>% 
  filter(!is.na(afcRT), !is.na(visionRT)) %>%
  mutate(trialaccuracy = factor(trialaccuracy, levels = rev(c("1","0")), labels = rev(c("Correct", "Incorrect"))),
         validity = factor(validity, levels = c("1", "0"), labels = c("Congruent", "Incongruent"))) %>% 
  ggplot(aes(x = visionRT/1000, y = afcRT/1000)) +
  stat_density_2d(bins = 23, adjust = 1, geom = "polygon", aes(alpha = (..level..), fill = trialaccuracy)) +
  scale_fill_manual(values = rev(c("#8ea4d2", "#EF626C"))) +
  coord_cartesian(xlim =c(.75,6.8), ylim = c(0.5,3.25)) +
  scale_x_continuous(breaks = round(seq(1.5, 6, length.out = 4),1)) +
  scale_y_continuous(breaks = seq(.5, 3.2, length.out = 4)) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 28),
        axis.line = element_line(size = 1.3),
        panel.spacing = unit(3, "lines")) +
  facet_wrap(~validity, nrow=1)


