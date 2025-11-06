
#### analysis code ####
#### study 2b ####
#### last updated 2-27-23 ####

#### load all packages ####
library(groundhog)
groundhog.day="2022-05-20"
pkgs=c("readxl","dplyr","tidyr","lme4","ggplot2",
       "plotrix","devtools","effsize","miceadds","bda","lmerTest","cowplot",
       "effects","ggpubr","multcomp","medmod","rstatix",
       "gmodels","crosstable", "diffcor", "cocron", "ltm", "sjstats", "pwr",
       "AICcmodavg")
groundhog.library(pkgs, groundhog.day)
groundhog.library('github::michaelkardas/bwrappers','2022-05-20')

### load in the data itself ###
rm(list = ls()) # clean environment #
imp = read_excel("study2b.xlsx")
sapply(imp, class)

### get the DV and regret measures in columns ###
imp = imp %>%
  rowwise() %>%
  mutate(wtp_dv = sum(`60_50_mild_wtp_4`, `10_1_mild_wtp_4`, `60_50_severe_wtp_4`, 
                        `10_1_severe_wtp_4`, na.rm=T),
         regret1 = sum(`60_50_mild_blame_4`, `10_1_mild_blame_4`, `60_50_severe_blame_4`, 
                       `10_1_severe_blame_4`, na.rm=T),
         regret2 = sum(`60_50_mild_blame_7`, `10_1_mild_blame_7`, `60_50_severe_blame_7`, 
                       `10_1_severe_blame_7`, na.rm=T))

### calculate condition variables ###
imp = imp %>%
  mutate(mild = ifelse(condition==1, "1",
                       ifelse(condition==2,"1", "-1")),
         sixty_fifty = ifelse(condition==1,"1",
                              ifelse(condition==3,"1","-1")))

### compute AC variable and finish variable ###
imp = imp %>%
  mutate(passac = ifelse(AC1==1,"1","0"),
         finish = ifelse(AC1>=1,"1","0"))

### add an ID variable to make everything easier to call ###
imp$id = seq.int(nrow(imp))
imp$id = as.factor(imp$id)
nlevels(imp$id)

### remove people who did not finish ###
imp_cl_finish = imp[ which(imp$finish==1), ] # 1607 people finished
imp_cl = imp_cl_finish[ which(imp_cl_finish$passac==1), ] # 1588 finished and passed the AC
# so, 19 people failed the attention check Q #

### prepare data for main test ###
imp_cl$mild = as.factor(imp_cl$mild)
nlevels(imp_cl$mild)
imp_cl$sixty_fifty = as.factor(imp_cl$sixty_fifty)
nlevels(imp_cl$sixty_fifty)

### look at the distributions ###

# just for the purposes of easy visualization, create a new variable #
imp_cl$cond_label = imp_cl$condition
imp_cl$cond_label = factor(imp_cl$cond_label,
                    levels = c(1,2,3,4),
                    labels = c("mild_60_50", "mild_10_1", "severe_60_50", "severe_10_1"))

# for wtp #
ggplot(data=imp_cl, aes(x=wtp_dv, group=cond_label, fill=cond_label)) +
  geom_density(adjust=1.5) +
  facet_wrap(~cond_label) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank())

# for regret1 (this is self-blame) #
ggplot(data=imp_cl, aes(x=regret1, group=cond_label, fill=cond_label)) +
  geom_density(adjust=1.5) +
  facet_wrap(~cond_label) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank())

# for regret2 (this is regret) #
ggplot(data=imp_cl, aes(x=regret2, group=cond_label, fill=cond_label)) +
  geom_density(adjust=1.5) +
  facet_wrap(~cond_label) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank())

### compute a regret measure that is the average of the two measures ###
# first, look at the correlation #
cor.test(imp_cl$regret1, imp_cl$regret2) # r = 0.71 #
### average the two measures ###
imp_cl$regret_comp = rowMeans(imp_cl[ , c(40,41)], na.rm=TRUE)

### look at summary stats for WTP ###
group_by(imp_cl, cond_label) %>%
  summarise(
    count = n(),
    mean = mean(wtp_dv, na.rm = TRUE),
    sd = sd(wtp_dv, na.rm = TRUE))

### ANOVA predicting WTP from the two conditions and interaction between two conditions ###
mod1 = aov(wtp_dv ~ mild * sixty_fifty, data = imp_cl)
summary(mod1)
anova_stats(mod1)

### unpack the two simple effects ###
imp_mild = filter(imp_cl, condition==1 | condition==2)
imp_severe = filter(imp_cl, condition==3 | condition==4)

# when cold is mild #
wrap.t.ind(imp_mild$wtp_dv, imp_mild$cond_label)
wrap.desc(imp_mild$wtp_dv, imp_mild$cond_label)

# when cold is severe #
wrap.t.ind(imp_severe$wtp_dv, imp_severe$cond_label)
wrap.desc(imp_severe$wtp_dv, imp_severe$cond_label)

### ANOVA predicting regret composite from the two conditions and interaction between two conditions ###
mod2 = aov(regret_comp ~ mild * sixty_fifty, data = imp_cl)
summary(mod2)
anova_stats(mod2)

# when cold is mild #
wrap.t.ind(imp_mild$regret_comp, imp_mild$cond_label)
wrap.desc(imp_mild$regret_comp, imp_mild$cond_label)

# when cold is severe #
wrap.t.ind(imp_severe$regret_comp, imp_severe$cond_label)
wrap.desc(imp_severe$regret_comp, imp_severe$cond_label)

### look at summary stats for regret ###
group_by(imp_cl, cond_label) %>%
  summarise(
    count = n(),
    mean = mean(regret_comp, na.rm = TRUE),
    sd = sd(regret_comp, na.rm = TRUE))

### correlation between regret composite and WTP ###
cor.test(imp_cl$regret_comp, imp_cl$wtp_dv)

### ANOVA predicting WTP, but with regret included ###
mod3 = aov(wtp_dv ~ mild * sixty_fifty + regret_comp, data = imp_cl)
summary(mod3) 
anova_stats(mod3)

### for purely exploratory purposes, do a basic model comparison ###
model.set = list(mod1, mod3)
model.names <- c("mod1", "mod3")
aictab(model.set, modnames = model.names)
# model3 (which includes regret composite) is the better model, which is unsurprising #

#### Demographics ####

# age #
imp_cl$age_num = as.numeric(imp_cl$age)
mean(imp_cl$age_num,na.rm=T)

# gender #
# 1 = Male, 2 = Female, 3 = Non-Binary #
imp_cl %>% count(gender) 
imp_cl_finish %>% count(gender) 
# note that a few respondents did not answer this question #

#### Exploratory Analyses Looking at Order Effects ####

imp_cl = imp_cl %>%
  mutate(order_one = ifelse(FL_64_DO=='60_50_mild_wtp|60_50_mild_blame',"1","-1"),
         order_two = ifelse(FL_66_DO=='10_1_mild_wtp|10_1_mild_blame',"1","-1"),
         order_three = ifelse(FL_68_DO=='60_50_severe_wtp|60_50_severe_blame',"1","-1"),
         order_four = ifelse(FL_70_DO=='10_1_severe_wtp|10_1_severe_blame',"1","-1"))

imp_cl$order_one = as.numeric(imp_cl$order_one)
imp_cl$order_two = as.numeric(imp_cl$order_two)
imp_cl$order_three = as.numeric(imp_cl$order_three)
imp_cl$order_four = as.numeric(imp_cl$order_four)

imp_cl = imp_cl %>%
  rowwise() %>%
  mutate(wtp_first = sum(order_one,order_two,order_three,order_four, na.rm=T))

imp_cl$wtp_first = as.factor(imp_cl$wtp_first)
nlevels(imp_cl$wtp_first)

### Does order of WTP/Regret measures have an effect? ###

### Look at summary stats first ###
group_by(imp_cl, wtp_first, cond_label) %>%
  summarise(
    count = n(),
    mean = mean(wtp_dv, na.rm = TRUE),
    sd = sd(wtp_dv, na.rm = TRUE))

### Run a full model to test this directly ###
mod4 = aov(wtp_dv ~ mild * sixty_fifty * wtp_first, data = imp_cl)
summary(mod4)
anova_stats(mod4)

# unpack the marginal three-way a little further #
# do this by looking at the two two-way interactions #
imp_cl_wtp_first = imp_cl %>%
  filter(wtp_first == 1)

imp_cl_wtp_second = imp_cl %>%
  filter(wtp_first == -1)

# when WTP is first (before regret measures) #
mod5 = aov(wtp_dv ~ mild * sixty_fifty, data = imp_cl_wtp_first)
summary(mod5)
anova_stats(mod5)

# when WTP is second (after regret measures) #
mod6 = aov(wtp_dv ~ mild * sixty_fifty, data = imp_cl_wtp_second)
summary(mod6)
anova_stats(mod6)

#### Redoing Main Analyses, But With All Participants ####

# this code includes the N = 19 participants who failed the attention check #

# the model predicting WTP #
mod7 = aov(wtp_dv ~ mild * sixty_fifty, data = imp_cl_finish)
summary(mod7)
anova_stats(mod7)

# the same model, but with regret composite added #
imp_cl_finish$regret_comp = rowMeans(imp_cl_finish[ , c(40,41)], na.rm=TRUE) # first, create composite #
mod8 = aov(wtp_dv ~ mild*sixty_fifty + regret_comp, data = imp_cl_finish)
summary(mod8)
anova_stats(mod8)

# the model just predicting regret #
mod9 = aov(regret_comp ~ mild * sixty_fifty, data = imp_cl_finish)
summary(mod9)
anova_stats(mod9)

### new condition label for finish data ###
imp_cl_finish$cond_label = imp_cl_finish$condition
imp_cl_finish$cond_label = factor(imp_cl_finish$cond_label,
                           levels = c(1,2,3,4),
                           labels = c("mild_60_50", "mild_10_1", "severe_60_50", "severe_10_1"))

### unpack the two simple effects ###
imp_mild_fin = filter(imp_cl_finish, condition==1 | condition==2)
imp_severe_fin = filter(imp_cl_finish, condition==3 | condition==4)

### for WTP ###

# when cold is mild #
wrap.t.ind(imp_mild_fin$wtp_dv, imp_mild_fin$cond_label)
wrap.desc(imp_mild_fin$wtp_dv, imp_mild_fin$cond_label)

# when cold is severe #
wrap.t.ind(imp_severe_fin$wtp_dv, imp_severe_fin$cond_label)
wrap.desc(imp_severe_fin$wtp_dv, imp_severe_fin$cond_label)

### for Regret ###

# when cold is mild #
wrap.t.ind(imp_mild_fin$regret_comp, imp_mild_fin$cond_label)
wrap.desc(imp_mild_fin$regret_comp, imp_mild_fin$cond_label)

# when cold is severe #
wrap.t.ind(imp_severe_fin$regret_comp, imp_severe_fin$cond_label)
wrap.desc(imp_severe_fin$regret_comp, imp_severe_fin$cond_label)
 


