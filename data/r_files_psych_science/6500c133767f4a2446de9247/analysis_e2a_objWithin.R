library(tidyverse)
library(lme4)

# clear env & import
rm(list=ls())
d <- read_csv("../2 - data/subjdata_e2a_objWithin.csv",
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

# Mixed effect analysis
iaData <-   d %>% filter(trialaccuracy == 1)
iaNull <- lmer(visionRT ~ 1 + (1|id) + (1|sound) + (1|groupImg), iaData)
iaFull <- lmer(visionRT ~ validity + (1|id) + (1|sound) + (1|groupImg), iaData)
anova(iaNull, iaFull)
# pbkrtest::KRmodcomp(iaNull, iaFull) ## recommended, ~always agrees with anova

# get average RT
d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T))

# run analysis statistics
stats <- 
  d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T)) %>% 
  {t.test(.$rt ~ .$validity, paired = T)}; stats

accStats <- 
  d %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(trialaccuracy, na.rm=T)) %>% 
  {t.test(.$rt ~ .$validity, paired = T)}; accStats

1/BayesFactor::ttest.tstat(t = accStats$statistic[[1]], 
                               n1=accStats$parameter[[1]]+1, 
                               simple=T)

# effect size
dz <- 
  d %>% 
  filter(trialaccuracy==1) %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(visionRT, na.rm=T)) %>% 
  summarize(eff = diff(rt)) %>% 
  summarize(dz = round(abs(mean(eff) / sd(eff)),2)) %>% 
  as.numeric(); dz

d %>% 
  group_by(id, validity) %>% 
  summarize(rt = mean(trialaccuracy, na.rm=T)) %>% 
  summarize(eff = diff(rt)) %>% 
  summarize(dz = round(abs(mean(eff) / sd(eff)),2)) %>% 
  as.numeric()


#  run d' analysis
dpStats <- 
  d %>% 
  group_by(id, validity) %>% 
  summarize(acc = mean(c(trialaccuracy, 0.5), na.rm=T),
            dp  = sqrt(2) * qnorm(acc)) %>% 
  {t.test(.$dp ~ .$validity, paired = T)}; dpStats

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

# Plot Response Time data
yMin = 4000
yMax = 4800
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
  geom_col(fill = c("tomato3", "lightsalmon"),
           width = 0.85) +
  geom_segment(aes(x = 1, xend = 2, y = yMax-100, yend = yMax-100),
               linewidth = 1.8) +
  annotate('text', x=1.5, y=yMax-90, size = 16,
           label = "°°") +
  theme_classic() +
  coord_cartesian(ylim = c(yMin,yMax)) + 
  labs(x = "Condition",
       y = "Response Time (ms)") +
  scale_x_discrete(labels = c("Incongruent", "Congruent")) +
  scale_y_continuous(breaks = seq(yMin, yMax,200)) +
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
  ggplot(aes(x = validity, y = rt)) +
  geom_errorbar(aes(ymin = rtMin, ymax = rtMax),
                width = 0.65,
                linewidth = 2.6) +
  geom_col(fill = c("skyblue4", "skyblue"),
           width = 0.85) +
  geom_segment(aes(x = 1, xend = 2, y = .95, yend = .95),
               linewidth = 1.8) +
  annotate('text', x=1.5, y=.956 + .01, size = 8,
           label = "n.s.") +
  theme_classic() +
  coord_cartesian(ylim = c(.75,1)) +
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
  geom_segment(aes(x = 1, xend = 2, y = 1.36, yend = 1.36),
               linewidth = 1.8) +
  annotate('text', x=1.5, y= 1.38, size = 8,
           label = "n.s.") +
  theme_classic() +
  coord_cartesian(ylim = c(1, 1.42)) +
  labs(x = "Condition",
       y = "d'") +
  scale_x_discrete(labels = c("Incongruent", "Congruent")) +
  scale_y_continuous(breaks = round(seq(1, 1.4, length.out = 5), 2)) +
  theme(axis.line = element_line(size = 2),
        axis.ticks = element_blank(),
        axis.text = element_text(face = 'bold'),
        text = element_text(size = 22))