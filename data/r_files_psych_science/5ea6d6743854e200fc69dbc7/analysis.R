### analysis script for Sherman & Grange (2019)


# comments are included throughout the script
# any questions about the scripts, please email Jim Grange:
# grange.jim@gmail.com
# www.jimgrange.org

#------------------------------------------------------------------------------
### load required libraries & function file
library(tidyverse) # for data tidying and plotting
library(BayesFactor) # for Bayes factor analyses
library(here) # for easy directory control
source("functions.R") # load custom functionms
library(lsr)  # for eta squared
library(MBESS) # for confidence intervals on eta squared
library(psych)

### contents of the file (with associated line numbers)

# section 1: load the data files & tidy them up a bit (line 47) 
# section 2: check for participant removal (line 101)
# section 3: manipulation checks (line 141)
# section 4: correct recognition analysis (line 668)
# section 5: false recognition analysis (line 745)
# section 6: filler recognition analysis (line 822)
# section 7: remember responses - correct recognition (line 899)
# section 8: remember responses - false recognition (line 978)
# section 9: remember responses - filler recognition (line 1057)
# section 10: know responses - correct recognition (line 1136)
# section 11: know responses - false recognition (line 1215)
# section 12: know responses - filler recognition (line 1294)
# section 13: correct recall analysis (line 1373)
# section 14: false recall analysis (line 1453)
# section 15: plots of standardised effect sizes (line 1582)
# section 16: regressions involving recognition responses (line 2483)
# section 17: regressions involving recall responses (line 2577)
# section 18: sensitivity analysis of achieved power (line 2670)
# section 19: Omnibus tests adding change in mindfulness as covariate (line 2737)
# section 20: Omnibus tests adding mindwandering TRI and TUT (line 2819)
# section 21: Omnibus tests with participant removal (line 2928)





#------------------------------------------------------------------------------
### section 1: load the data files & tidy them up a bit


##--- recognition data

# recogition data import
setwd(here("data"))
recognition_data <- read.csv("recognition_data.csv")
setwd(here())

# change the condition column to a factor
# 1 = mindfulness
# 2 = mind_wandering
# 3 = join_dots
recognition_data$condition <- factor(recognition_data$condition, 
                                     levels = c(1, 2, 3), 
                                     labels = c("mindfulness", 
                                                "mind_wandering", 
                                                "join_dots"))

# change the compensation column to a factor
# 1 = course_credit
# 2 = cash
recognition_data$compensation <- factor(recognition_data$compensation, 
                                        levels = c(1, 2), 
                                        labels = c("course_credit", "cash"))


##--- recall data
setwd(here("data"))
recall_data <- read.csv("free_recall_data.csv")
setwd(here())

# change the condition column to a factor
# 1 = mindfulness
# 2 = mind_wandering
# 3 = join_dots
recall_data$condition <- factor(recall_data$condition, 
                                labels = c("mindfulness", 
                                           "mind_wandering", 
                                           "join_dots"))

# change the compensation column to a factor
# 1 = course_credit
# 2 = cash
recall_data$compensation <- factor(recall_data$compensation, 
                                   levels = c(1, 2), 
                                   labels = c("course_credit", "cash"))

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 2: check for participant removal

# in the pre-registration, we stated that we would remove any participant who:
# - does not complete all items on the Mindful Attention Awareness Scale; 
# - had correct recognition scores more than 3 standard deviations from the 
#   sample mean, as this suggests disengagement with the study.

# calculate mean, sd, and cutoff criterion
sample_sd <- sd(recognition_data$list_total)
sample_mean <- mean(recognition_data$list_total)
cutoff <- sample_mean - (3 * sample_sd)

# find participants who score below this (participants 150, 216, 222)
removed_participants <- recognition_data %>% 
  filter(list_total < cutoff) %>% 
  pull(participant)

# in our stopping rule, we also stated:
# "To be considered complete participants need to complete all items on both 
# presentations of the State Mindfulness Scale (Tanay & Bernstein, 2013), 
# all items on the retrospective mindwandering scale, the free recall task, 
# and the recognition task"

# participant 42 didn't make any free-recall response
# participants 76 and 90 had missing data for at least 1 of the free recall 
# lists
removed_participants <- c(removed_participants, 42, 76, 90)


# remove these from the data sets
recognition_data <- recognition_data %>% 
  filter(participant %!in% removed_participants)

recall_data <- recall_data %>% 
  filter(participant %!in% removed_participants)
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 3: manipulation checks


##--- 
# SMS scale - did minfulness work?

# get the difference score (post- minus pre-manipulation)
sms_total <- recognition_data %>% 
  select(participant, condition, pre_manip_total_sms, post_manip_total_sms) %>%
  mutate(sms_difference = post_manip_total_sms - pre_manip_total_sms) %>% 
  select(participant, condition, sms_difference)

sms_total_percent <- recognition_data %>% 
  mutate(pre_manip_total_sms_percent = ((pre_manip_total_sms / 120) * 100)) %>% 
  mutate(post_manip_total_sms_percent = ((post_manip_total_sms / 120) * 100)) %>% 
  select(participant, condition, pre_manip_total_sms_percent, 
         post_manip_total_sms_percent) %>%
  mutate(sms_difference = post_manip_total_sms_percent - 
           pre_manip_total_sms_percent) %>% 
  select(participant, condition, sms_difference)


# conduct the anova & follow-up tests
sms_total_aov <- aov(sms_difference ~ condition, data = sms_total)
summary(sms_total_aov)
etaSquared(sms_total_aov)
ci_eta <- ci.pvaf(F.value = 28.11, 
                  df.1 = 2, 
                  df.2 = 281, 
                  N = 284)
sms_total_hsd <- TukeyHSD(sms_total_aov)
sms_total_hsd




# Bayesian analysis
# order restriction = mindfulness > control > mind-wandering
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)

# 1)
full_null <- anovaBF(sms_difference ~ condition, data = sms_total)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null



# do a plot
theme_set(theme_light())

sms_total_plot_data <- sms_total %>%
  group_by(condition) %>% 
  summarise(mean = mean(sms_difference), 
            se = sd(sms_difference) / sqrt(length(sms_difference))) %>% 
  mutate(dv = "sms_total")


# set the working directory to the figures folder
setwd(here("figures"))

# mean scores
sms_total_plot <- ggplot(sms_total_plot_data, aes(condition, mean, group = 1)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                width = .14, size = .5) +
  labs(y = "SMS score Post - SMS score Pre",
       x = "Condition")

### uncomment the following to show individual data points
# # do the bar plot
# sms_total_plot <- ggplot(sms_total_plot_data, aes(condition, mean, group = 1)) + 
#   geom_bar(stat = "identity", width = 0.4, fill = "grey") + 
#   geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
#                 width = .08, size = 0.5) + 
#   geom_hline(yintercept = 0, size = 0.65) + 
#   labs(y = "SMS score Post - SMS score Pre", 
#        x = "Condition")
# 
# # add individual data points
# sms_total_plot <- sms_total_plot + 
#   geom_point(data = sms_total, aes(y = sms_difference, x = condition),
#              colour = "#e79f00", size = 3, alpha = 0.3, 
#              position = position_jitter(width = 0.15, height = 0.1))

pdf("sms_total.pdf", width = 5, height = 5)
sms_total_plot
dev.off()

# set the working directory back to the home folder
setwd(here())


##--- 
# SMS-mind subscale
sms_mind <- recognition_data %>% 
  select(participant, condition, pre_manip_subscale_mind, 
         post_manip_subscale_mind) %>% 
  mutate(sms_difference = post_manip_subscale_mind - 
           pre_manip_subscale_mind) %>% 
  select(participant, condition, sms_difference)

# conduct the anova & follow-up tests
sms_mind_aov <- aov(sms_difference ~ condition, data = sms_mind)
summary(sms_mind_aov)
ci.pvaf(F.value = 23.9, 
        df.1 = 2, 
        df.2 = 281, 
        N = 284)
etaSquared(sms_mind_aov)
sms_mind_hsd <- TukeyHSD(sms_mind_aov) 
sms_mind_hsd





# Bayesian analysis
# order restriction = mindfulness > control > mind-wandering
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)

# 1)
full_null <- anovaBF(sms_difference ~ condition, data = sms_mind)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null


# do a plot
sms_mind_plot_data <- sms_mind %>%
  group_by(condition) %>%
  summarise(mean = round(mean(sms_difference), 2),
            se = sd(sms_difference) / sqrt(length(sms_difference))) %>% 
  mutate(dv = "sms_mind")

# set the working directory to the figures folder
setwd(here("figures"))

# mean scores
sms_mind_plot <- ggplot(sms_mind_plot_data, aes(condition, mean, group = 1)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                width = .14, size = .5) +
  labs(y = "Mind score Post - Mind score Pre",
       x = "Condition")

### uncomment the following to show individual data points
# # do the bar plot
# sms_mind_plot <- ggplot(sms_mind_plot_data, aes(condition, mean, group = 1)) + 
#   geom_bar(stat = "identity", width = 0.4, fill = "grey") + 
#   geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
#                 width = .08, size = 0.5) + 
#   geom_hline(yintercept = 0, size = 0.65) +
#   labs(y = "Mind score Post - Mind score Pre", 
#        x = "Condition")
# 
# # add individual data points
# sms_mind_plot <- sms_mind_plot + 
#   geom_point(data = sms_mind, aes(y = sms_difference, x = condition),
#              colour = "#e79f00", size = 3, alpha = 0.3, 
#              position = position_jitter(width = 0.15, height = 0.1))

pdf("sms_mind.pdf", width = 5, height = 5)
sms_mind_plot
dev.off()

# set the working directory back to the home folder
setwd(here())


##--- 
# SMS-body subscale
sms_body <- recognition_data %>% 
  select(participant, condition, pre_manip_subscale_body, 
         post_manip_subscale_body) %>% 
  mutate(sms_difference = post_manip_subscale_body - 
           pre_manip_subscale_body) %>% 
  select(participant, condition, sms_difference)

# conduct the anova & follow-up tests
sms_body_aov <- aov(sms_difference ~ condition, data = sms_body)
summary(sms_body_aov)
ci.pvaf(F.value = 27.81, 
        df.1 = 2, 
        df.2 = 281, 
        N = 284)
etaSquared(sms_body_aov)
sms_body_hsd <- TukeyHSD(sms_body_aov) 
sms_body_hsd



# Bayesian analysis
# order restriction = mindfulness > control > mind-wandering
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)

# 1)
full_null <- anovaBF(sms_difference ~ condition, data = sms_body)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null

# do a plot
sms_body_plot_data <- sms_body %>%
  group_by(condition) %>% 
  summarise(mean = mean(sms_difference), 
            se = sd(sms_difference) / sqrt(length(sms_difference))) %>% 
  mutate(dv = "sms_body")

correct_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(list_total), 
            se = sd(list_total) / sqrt(length(list_total)))

# set the working directory to the figures folder
setwd(here("figures"))

# mean scores
sms_body_plot <- ggplot(sms_body_plot_data, aes(condition, mean, group = 1)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                width = .14, size = .5) +
  labs(y = "Body score Post - Body score Pre",
       x = "Condition")

### uncomment the following to show individual data points
# # do the bar plot
# sms_body_plot <- ggplot(sms_body_plot_data, aes(condition, mean, group = 1)) + 
#   geom_bar(stat = "identity", width = 0.4, fill = "grey") + 
#   geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
#                 width = .08, size = 0.5) + 
#   geom_hline(yintercept = 0, size = 0.65) +
#   labs(y = "Body score Post - Body score Pre", 
#        x = "Condition")
# 
# # add individual data points
# sms_body_plot <- sms_body_plot + 
#   geom_point(data = sms_body, aes(y = sms_difference, x = condition),
#              colour = "#e79f00", size = 3, alpha = 0.3, 
#              position = position_jitter(width = 0.15, height = 0.1))


pdf("sms_body.pdf", width = 5, height = 5)
sms_body_plot
dev.off()

# set the working directory back to the home folder
setwd(here())


##--- 
# TRI scale - task related interference
tri <- recognition_data %>% 
  select(participant, condition, tri) 

# conduct the anova & follow-up tests
tri_aov <- aov(tri ~ condition, data = tri)
summary(tri_aov)
ci.pvaf(F.value = 27.54, 
        df.1 = 2, 
        df.2 = 281, 
        N = 284)
etaSquared(tri_aov)
tri_hsd <- TukeyHSD(tri_aov) 
tri_hsd





# Bayesian analysis
# prediction = mindwandering > control > mindfulness
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)

# 1)
full_null <- anovaBF(tri ~ condition, data = tri)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null


# set the working directory to the figures folder
setwd(here("figures"))

# do the plot
tri_plot_data <- tri %>% 
  group_by(condition) %>% 
  summarise(mean = mean(tri), 
            se = sd(tri) / sqrt(length(tri))) %>% 
  mutate(dv = "TRI")

tri_plot <- ggplot(tri_plot_data, aes(condition, mean, group = 1)) + 
  geom_point() + 
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "TRI Scores", 
       x = "Condition")

pdf("tri.pdf", width = 5, height = 5)
tri_plot
dev.off()

# set the working directory back to the home folder
setwd(here())



##--- 
# TUT scale - task unrelated thoughts
tut <- recognition_data %>% 
  select(participant, condition, tut) 

# conduct the anova & follow-up tests
tut_aov <- aov(tut ~ condition, data = tut)
summary(tut_aov)
ci.pvaf(F.value = 35.18, 
        df.1 = 2, 
        df.2 = 281, 
        N = 284)
etaSquared(tut_aov)
tut_hsd <- TukeyHSD(tut_aov) 
tut_hsd



# Bayesian analysis
# prediction = mindwandering > control > mindfulness
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)

# 1)
full_null <- anovaBF(tut ~ condition, data = tut)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null

# set the working directory to the figures folder
setwd(here("figures"))

# do the plot
tut_plot_data <- tut %>% 
  group_by(condition) %>% 
  summarise(mean = mean(tut), 
            se = sd(tut) / sqrt(length(tut))) %>% 
  mutate(dv = "TUT")

tut_plot <- ggplot(tut_plot_data, aes(condition, mean, group = 1)) + 
  geom_point() + 
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "TUT Scores", 
       x = "Condition")

pdf("tut.pdf", width = 5, height = 5)
tut_plot
dev.off()

# set the working directory back to the home folder
setwd(here())



#----
## put all manipulation checks into one graph

# store all in one data frame
manipulation_checks <- rbind(sms_total_plot_data, 
                             sms_body_plot_data, 
                             sms_mind_plot_data, 
                             tri_plot_data, 
                             tut_plot_data)

# do the plot
manipulation_check_plot <- ggplot(manipulation_checks, 
                                  aes(condition, mean, group = 1)) + 
  geom_point() + 
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Difference Scores (post - pre)", 
       x = "Condition") + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14,face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) + 
  facet_wrap( ~ dv, scales = "free") 

setwd(here("figures"))
pdf("manipulation_checks.pdf", width = 12, height = 8)
manipulation_check_plot
dev.off()
setwd(here())

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 4: correct recognition analysis

# prediction: mindfulness > control > mind_wandering

setwd(here("figures"))

#---
# plot the data
correct_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = round(mean(list_total), 3), 
            se = sd(list_total) / sqrt(length(list_total))) %>% 
  mutate(dv = "Total_Correct_Recognition")

correct_recognition_plot <- ggplot(correct_recognition_plot_data, 
                                   aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Total Correct Recognition", 
       x = "Condition")

pdf("correct_recognition.pdf", width = 5, height = 5)
correct_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
correct_recognition_aov <- aov(list_total ~ condition, data = recognition_data)
summary(correct_recognition_aov)
etaSquared(correct_recognition_aov)
correct_recognition_hsd <- TukeyHSD(correct_recognition_aov)
correct_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(list_total ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
correct_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 5: false recognition analysis

# prediction: mind_wandering > control > mindfulness

setwd(here("figures"))

#---
# plot the data
false_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(lure_total), 
            se = sd(lure_total) / sqrt(length(lure_total))) %>% 
  mutate(dv = "Total_False_Recognition")

false_recognition_plot <- ggplot(false_recognition_plot_data, 
                                 aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Total False Recognition", 
       x = "Condition")

pdf("false_recognition.pdf", width = 5, height = 5)
false_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
false_recognition_aov <- aov(lure_total ~ condition, data = recognition_data)
summary(false_recognition_aov)
etaSquared(false_recognition_aov)
false_recognition_hsd <- TukeyHSD(false_recognition_aov)
false_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(lure_total ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 6: filler recognition analysis

# prediction: mind_wandering > control > mindfulness

setwd(here("figures"))

#---
# plot the data
filler_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(filler_total), 
            se = sd(filler_total) / sqrt(length(filler_total))) %>% 
  mutate(dv = "Total_Filler")

filler_recognition_plot <- ggplot(filler_recognition_plot_data, 
                                 aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Total Filler Recognition", 
       x = "Condition")

pdf("filler_recognition.pdf", width = 5, height = 5)
filler_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
filler_recognition_aov <- aov(filler_total ~ condition, data = recognition_data)
summary(filler_recognition_aov)
etaSquared(filler_recognition_aov)
filler_recognition_hsd <- TukeyHSD(filler_recognition_aov)
filler_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(filler_total ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
filler_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 7: remember responses - correct recognition

# prediction: mindfulness > control > mind_wandering

setwd(here("figures"))

#---
# plot the data
remember_correct_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(list_r), 
            se = sd(list_r) / sqrt(length(list_r))) %>% 
  mutate(dv = "Remember_Correct_Recognition")

remember_correct_recognition_plot <- 
  ggplot(remember_correct_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Correct Recognition (Remember Responses)", 
       x = "Condition")

pdf("remember_correct_recognition.pdf", width = 5, height = 5)
remember_correct_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
remember_correct_recognition_aov <- 
  aov(list_r ~ condition, data = recognition_data)
summary(remember_correct_recognition_aov)
etaSquared(remember_correct_recognition_aov)
remember_correct_recognition_hsd <- TukeyHSD(remember_correct_recognition_aov)
remember_correct_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(list_r ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
remember_correct_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 8: remember responses - false recognition

# prediction: control > mind_wandering > mindfulness

setwd(here("figures"))

#---
# plot the data
remember_false_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(lure_r), 
            se = sd(lure_r) / sqrt(length(lure_r))) %>% 
  mutate(dv = "Remember_False_Recognition")

remember_false_recognition_plot <- 
  ggplot(remember_false_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "False Recognition (Remember Responses)", 
       x = "Condition")

pdf("remember_false_recognition.pdf", width = 5, height = 5)
remember_false_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
remember_false_recognition_aov <- 
  aov(lure_r ~ condition, data = recognition_data)
summary(remember_false_recognition_aov)
etaSquared(remember_false_recognition_aov)
remember_false_recognition_hsd <- TukeyHSD(remember_false_recognition_aov)
remember_false_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(lure_r ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-mind_wandering"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-join_dots"] > samples[, "condition-mind_wandering"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
remember_false_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 9: remember responses - filler recognition

# prediction: control > mind_wandering > mindfulness

setwd(here("figures"))

#---
# plot the data
remember_filler_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(filler_r), 
            se = sd(filler_r) / sqrt(length(filler_r))) %>% 
  mutate(dv = "Remember_Filler")

remember_filler_recognition_plot <- 
  ggplot(remember_filler_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Filler Recognition (Remember Responses)", 
       x = "Condition")

pdf("remember_filler_recognition.pdf", width = 5, height = 5)
remember_filler_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
remember_filler_recognition_aov <- 
  aov(filler_r ~ condition, data = recognition_data)
summary(remember_filler_recognition_aov)
etaSquared(remember_filler_recognition_aov)
remember_filler_recognition_hsd <- TukeyHSD(remember_filler_recognition_aov)
remember_filler_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(filler_r ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-mind_wandering"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-join_dots"] > samples[, "condition-mind_wandering"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
remember_filler_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 10: know responses - correct recognition

# prediction: mindfulness > control > mind_wandering

setwd(here("figures"))

#---
# plot the data
know_correct_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(list_k), 
            se = sd(list_k) / sqrt(length(list_k))) %>% 
  mutate(dv = "Know_Correct_Recognition")

know_correct_recognition_plot <- 
  ggplot(know_correct_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Correct Recognition (Know Responses)", 
       x = "Condition")

pdf("know_correct_recognition.pdf", width = 5, height = 5)
know_correct_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
know_correct_recognition_aov <- 
  aov(list_k ~ condition, data = recognition_data)
summary(know_correct_recognition_aov)
etaSquared(know_correct_recognition_aov)
know_correct_recognition_hsd <- TukeyHSD(know_correct_recognition_aov)
know_correct_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(list_k ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
know_correct_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 11: know responses - false recognition

# prediction: mind_wandering > control > mindfulness

setwd(here("figures"))

#---
# plot the data
know_false_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(lure_k), 
            se = sd(lure_k) / sqrt(length(lure_k))) %>% 
  mutate(dv = "Know_False_Recognition")

know_false_recognition_plot <- 
  ggplot(know_false_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "False Recognition (Know Responses)", 
       x = "Condition")

pdf("know_false_recognition.pdf", width = 5, height = 5)
know_false_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
know_false_recognition_aov <- 
  aov(lure_k ~ condition, data = recognition_data)
summary(know_false_recognition_aov)
etaSquared(know_false_recognition_aov)
know_false_recognition_hsd <- TukeyHSD(know_false_recognition_aov)
know_false_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(lure_k ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-mindfulness"] > 
                 samples[, "condition-join_dots"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
know_false_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 12: know responses - filler recognition

# prediction: mind_wandering > control > mindfulness

setwd(here("figures"))

#---
# plot the data
know_filler_recognition_plot_data <- recognition_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(filler_k), 
            se = sd(filler_k) / sqrt(length(filler_k))) %>% 
  mutate(dv = "Know_Filler")

know_filler_recognition_plot <- 
  ggplot(know_filler_recognition_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Filler Recognition (Know Responses)", 
       x = "Condition")

pdf("know_filler_recognition.pdf", width = 5, height = 5)
know_filler_recognition_plot
dev.off()


#---
# NHST analysis

# conduct the anova & follow-up tests
know_filler_recognition_aov <- 
  aov(filler_k ~ condition, data = recognition_data)
summary(know_filler_recognition_aov)
etaSquared(know_filler_recognition_aov)
know_filler_recognition_hsd <- TukeyHSD(know_filler_recognition_aov)
know_filler_recognition_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(filler_k ~ condition, data = recognition_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
know_filler_recognition_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 13: correct recall analysis

# prediction: mind_wandering > control > minfdulness

setwd(here("figures"))

#---
# plot the data
correct_recall_plot_data <- recall_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(list_total), 
            se = sd(list_total) / sqrt(length(list_total))) %>% 
  mutate(dv = "Correct_Recall")

correct_recall_plot <- 
  ggplot(correct_recall_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "Correct Free Recall", 
       x = "Condition")

pdf("correct_recall.pdf", width = 5, height = 5)
correct_recall_plot
dev.off()

setwd(here())

#---
# NHST analysis

# conduct the anova & follow-up tests
correct_recall_aov <- 
  aov(list_proportion ~ condition, data = recall_data)
summary(correct_recall_aov)
etaSquared(correct_recall_aov)
correct_recall_hsd <- TukeyHSD(correct_recall_aov)
correct_recall_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(list_proportion ~ condition, data = recall_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mindfulness"]) & 
  (samples[, "condition-mind_wandering"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recall_bf <- restricted_null
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 14: false recall analysis


# prediction: mindfulness > control > mind_wandering

setwd(here("figures"))

#---
# plot the data
false_recall_plot_data <- recall_data %>% 
  group_by(condition) %>% 
  summarise(mean = mean(lure_total), 
            se = sd(lure_total) / sqrt(length(lure_total))) %>% 
  mutate(dv = "False_Recall")

false_recall_plot <- 
  ggplot(false_recall_plot_data, 
         aes(condition, mean, group = 1)) +
  geom_point() + 
  geom_line(linetype = "dashed") + 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), 
                width = .14, size = .5) + 
  labs(y = "False Free Recall", 
       x = "Condition")

pdf("false_recall.pdf", width = 5, height = 5)
false_recall_plot
dev.off()

setwd(here())

#---
# NHST analysis

# conduct the anova & follow-up tests
false_recall_aov <- 
  aov(lure_proportion ~ condition, data = recall_data)
summary(false_recall_aov)
etaSquared(false_recall_aov)
false_recall_hsd <- TukeyHSD(false_recall_aov)
false_recall_hsd

#---
# Bayesian analysis
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

set.seed(42)
# 1)
full_null <- anovaBF(lure_proportion ~ condition, data = recall_data)


# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "condition-join_dots"] > 
                 samples[, "condition-mind_wandering"]) & 
  (samples[, "condition-mindfulness"] > samples[, "condition-join_dots"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_recall_bf <- restricted_null


### put all into same plot
all_plot_data <- rbind(correct_recognition_plot_data, 
                       false_recognition_plot_data, 
                       filler_recognition_plot_data, 
                       remember_correct_recognition_plot_data, 
                       remember_false_recognition_plot_data, 
                       remember_filler_recognition_plot_data, 
                       know_correct_recognition_plot_data, 
                       know_false_recognition_plot_data, 
                       know_filler_recognition_plot_data, 
                       correct_recall_plot_data, 
                       false_recall_plot_data)

all_plot_data$dv = factor(all_plot_data$dv, 
                          levels = c ("Total_Correct_Recognition", 
                                      "Total_False_Recognition", 
                                      "Total_Filler", 
                                      "Remember_Correct_Recognition", 
                                      "Remember_False_Recognition", 
                                      "Remember_Filler", 
                                      "Know_Correct_Recognition", 
                                      "Know_False_Recognition", 
                                      "Know_Filler", 
                                      "Correct_Recall", 
                                      "False_Recall"))

# do the plot
all_plot <- ggplot(all_plot_data, aes(condition, mean, group = 1)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se),
                width = .14, size = .5) +
  labs(y = "Number of Items Recognised / Recalled",
       x = "Condition") +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14,face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  facet_wrap( ~ dv, ncol = 3, scales = "free")



setwd(here("figures"))
pdf("all_plot.pdf", width = 12, height = 14)
all_plot
dev.off()
setwd(here())
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 15: plots of standardised effect sizes


##--- manipulation checks
# first, create data frame to store all cohen-d values in
manipulation_check_d <- data.frame(
  Measure = rep(0, 15), 
  Comparison = rep(0, 15), 
  d = rep(0, 15), 
  lower_ci = rep(0, 15), 
  upper_ci = rep(0, 15)
)

# SMS total
# mindfulness vs. mind_wandering
sms_total_mindful_wandering <- sms_total %>% 
  select(-participant) %>% 
  filter(condition != "join_dots")
sms_total_mindful_wandering_d <- 
  cohen.d(sms_total_mindful_wandering, "condition")

manipulation_check_d$Measure[1] <- "SMS_Total"
manipulation_check_d$Comparison[1] <- "Mindfulness vs. Wandering"
manipulation_check_d$d[1] <- -sms_total_mindful_wandering_d$cohen.d[2]
manipulation_check_d$lower_ci[1] <- -sms_total_mindful_wandering_d$cohen.d[3]
manipulation_check_d$upper_ci[1] <- -sms_total_mindful_wandering_d$cohen.d[1]

# mindfulness vs. join_the_dots
sms_total_mindful_dots <- sms_total %>% 
  select(-participant) %>% 
  filter(condition != "mind_wandering")
sms_total_mindful_dots_d <- 
  cohen.d(sms_total_mindful_dots, "condition")

manipulation_check_d$Measure[2] <- "SMS_Total"
manipulation_check_d$Comparison[2] <- "Mindfulness vs. Dots"
manipulation_check_d$d[2] <- -sms_total_mindful_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[2] <- -sms_total_mindful_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[2] <- -sms_total_mindful_dots_d$cohen.d[1]

# mindwandering vs. join_the_dots
sms_total_wandering_dots <- sms_total %>% 
  select(-participant) %>% 
  filter(condition != "mindfulness")
sms_total_wandering_dots <- 
  cohen.d(sms_total_wandering_dots, "condition")

manipulation_check_d$Measure[3] <- "SMS_Total"
manipulation_check_d$Comparison[3] <- "Wandering vs. Dots"
manipulation_check_d$d[3] <- -sms_total_wandering_dots$cohen.d[2]
manipulation_check_d$lower_ci[3] <- -sms_total_wandering_dots$cohen.d[3]
manipulation_check_d$upper_ci[3] <- -sms_total_wandering_dots$cohen.d[1]

# SMS mind
# mindfulness vs. mind_wandering
sms_mind_mindful_wandering <- sms_mind %>% 
  select(-participant) %>% 
  filter(condition != "join_dots")
sms_mind_mindful_wandering_d <- 
  cohen.d(sms_mind_mindful_wandering, "condition")

manipulation_check_d$Measure[4] <- "SMS_Mind"
manipulation_check_d$Comparison[4] <- "Mindfulness vs. Wandering"
manipulation_check_d$d[4] <- -sms_mind_mindful_wandering_d$cohen.d[2]
manipulation_check_d$lower_ci[4] <- -sms_mind_mindful_wandering_d$cohen.d[3]
manipulation_check_d$upper_ci[4] <- -sms_mind_mindful_wandering_d$cohen.d[1]

# mindfulness vs. join_the_dots
sms_mind_mindful_dots <- sms_mind %>% 
  select(-participant) %>% 
  filter(condition != "mind_wandering")
sms_mind_mindful_dots_d <- 
  cohen.d(sms_mind_mindful_dots, "condition")

manipulation_check_d$Measure[5] <- "SMS_Mind"
manipulation_check_d$Comparison[5] <- "Mindfulness vs. Dots"
manipulation_check_d$d[5] <- -sms_mind_mindful_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[5] <- -sms_mind_mindful_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[5] <- -sms_mind_mindful_dots_d$cohen.d[1]

# mindwandering vs. join_the_dots
sms_mind_wandering_dots <- sms_mind %>% 
  select(-participant) %>% 
  filter(condition != "mindfulness")
sms_mind_wandering_dots_d <- 
  cohen.d(sms_mind_wandering_dots, "condition")

manipulation_check_d$Measure[6] <- "SMS_Mind"
manipulation_check_d$Comparison[6] <- "Wandering vs. Dots"
manipulation_check_d$d[6] <- -sms_mind_wandering_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[6] <- -sms_mind_wandering_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[6] <- -sms_mind_wandering_dots_d$cohen.d[1]


# SMS body
# mindfulness vs. mind_wandering
sms_body_mindful_wandering <- sms_body %>% 
  select(-participant) %>% 
  filter(condition != "join_dots")
sms_body_mindful_wandering_d <- 
  cohen.d(sms_body_mindful_wandering, "condition")

manipulation_check_d$Measure[7] <- "SMS_Body"
manipulation_check_d$Comparison[7] <- "Mindfulness vs. Wandering"
manipulation_check_d$d[7] <- -sms_body_mindful_wandering_d$cohen.d[2]
manipulation_check_d$lower_ci[7] <- -sms_body_mindful_wandering_d$cohen.d[3]
manipulation_check_d$upper_ci[7] <- -sms_body_mindful_wandering_d$cohen.d[1]

# mindfulness vs. join_the_dots
sms_body_mindful_dots <- sms_body %>% 
  select(-participant) %>% 
  filter(condition != "mind_wandering")
sms_body_mindful_dots_d <- 
  cohen.d(sms_body_mindful_dots, "condition")

manipulation_check_d$Measure[8] <- "SMS_Body"
manipulation_check_d$Comparison[8] <- "Mindfulness vs. Dots"
manipulation_check_d$d[8] <- -sms_body_mindful_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[8] <- -sms_body_mindful_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[8] <- -sms_body_mindful_dots_d$cohen.d[1]

# mindwandering vs. join_the_dots
sms_body_wandering_dots <- sms_body %>% 
  select(-participant) %>% 
  filter(condition != "mindfulness")
sms_body_wandering_dots_d <- 
  cohen.d(sms_body_wandering_dots, "condition")

manipulation_check_d$Measure[9] <- "SMS_Body"
manipulation_check_d$Comparison[9] <- "Wandering vs. Dots"
manipulation_check_d$d[9] <- -sms_body_wandering_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[9] <- -sms_body_wandering_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[9] <- -sms_body_wandering_dots_d$cohen.d[1]


# TRI
# mindfulness vs. mind_wandering
tri_mindful_wandering <- tri %>% 
  select(-participant) %>% 
  filter(condition != "join_dots")
tri_mindful_wandering_d <- 
  cohen.d(tri_mindful_wandering, "condition")

manipulation_check_d$Measure[10] <- "TRI"
manipulation_check_d$Comparison[10] <- "Mindfulness vs. Wandering"
manipulation_check_d$d[10] <- -tri_mindful_wandering_d$cohen.d[2]
manipulation_check_d$lower_ci[10] <- -tri_mindful_wandering_d$cohen.d[3]
manipulation_check_d$upper_ci[10] <- -tri_mindful_wandering_d$cohen.d[1]

# mindfulness vs. join_the_dots
tri_mindful_dots <- tri %>% 
  select(-participant) %>% 
  filter(condition != "mind_wandering")
tri_mindful_dots_d <- 
  cohen.d(tri_mindful_dots, "condition")

manipulation_check_d$Measure[11] <- "TRI"
manipulation_check_d$Comparison[11] <- "Mindfulness vs. Dots"
manipulation_check_d$d[11] <- -tri_mindful_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[11] <- -tri_mindful_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[11] <- -tri_mindful_dots_d$cohen.d[1]

# mindwandering vs. join_the_dots
tri_wandering_dots <- tri %>% 
  select(-participant) %>% 
  filter(condition != "mindfulness")
tri_wandering_dots_d <- 
  cohen.d(tri_wandering_dots, "condition")

manipulation_check_d$Measure[12] <- "TRI"
manipulation_check_d$Comparison[12] <- "Wandering vs. Dots"
manipulation_check_d$d[12] <- -tri_wandering_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[12] <- -tri_wandering_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[12] <- -tri_wandering_dots_d$cohen.d[1]


# TUT
# mindfulness vs. mind_wandering
tut_mindful_wandering <- tut %>% 
  select(-participant) %>% 
  filter(condition != "join_dots")
tut_mindful_wandering_d <- 
  cohen.d(tut_mindful_wandering, "condition")

manipulation_check_d$Measure[13] <- "TUT"
manipulation_check_d$Comparison[13] <- "Mindfulness vs. Wandering"
manipulation_check_d$d[13] <- -tut_mindful_wandering_d$cohen.d[2]
manipulation_check_d$lower_ci[13] <- -tut_mindful_wandering_d$cohen.d[3]
manipulation_check_d$upper_ci[13] <- -tut_mindful_wandering_d$cohen.d[1]

# mindfulness vs. join_the_dots
tut_mindful_dots <- tut %>% 
  select(-participant) %>% 
  filter(condition != "mind_wandering")
tut_mindful_dots_d <- 
  cohen.d(tut_mindful_dots, "condition")

manipulation_check_d$Measure[14] <- "TUT"
manipulation_check_d$Comparison[14] <- "Mindfulness vs. Dots"
manipulation_check_d$d[14] <- -tut_mindful_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[14] <- -tut_mindful_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[14] <- -tut_mindful_dots_d$cohen.d[1]

# mindwandering vs. join_the_dots
tut_wandering_dots <- tut %>% 
  select(-participant) %>% 
  filter(condition != "mindfulness")
tut_wandering_dots_d <- 
  cohen.d(tut_wandering_dots, "condition")

manipulation_check_d$Measure[15] <- "TUT"
manipulation_check_d$Comparison[15] <- "Wandering vs. Dots"
manipulation_check_d$d[15] <- -tut_wandering_dots_d$cohen.d[2]
manipulation_check_d$lower_ci[15] <- -tut_wandering_dots_d$cohen.d[3]
manipulation_check_d$upper_ci[15] <- -tut_wandering_dots_d$cohen.d[1]


# Do the plot of all standardised effect sizes

# declare colour palette
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
palette <- c("#08306B", "#08519C", "#2171B5", "#4292C6",
             "#6BAED6", "#9ECAE1", "#CC79A7")


pd <- position_dodge(0.3)
manipulation_d_plot <- ggplot(manipulation_check_d, 
                              aes(x = Comparison, 
                                  y = d, 
                                  group = Measure)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = Measure, colour = Measure), size = 3, position = pd) + 
  geom_errorbar(aes(colour = Measure, 
                    ymin = lower_ci, ymax = upper_ci), 
                width = .14, size = .5, position = pd) + 
  scale_color_manual(values = palette) +
  ggtitle("Manipulation Checks") + 
  theme_bw()
pdf("manipulation_d_plot.pdf", width = 7.5, height = 6)
manipulation_d_plot
dev.off()








##--- total responses
# first, create data frame to store all cohen-d values in
total_recognition_d <- data.frame(
  Measure = rep(0, 9), 
  Comparison = rep(0, 9), 
  d = rep(0, 9), 
  lower_ci = rep(0, 9), 
  upper_ci = rep(0, 9)
)

# correct total recognition
# mindfulness vs. mind_wandering
correct_total_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "join_dots")
correct_total_mindful_wandering_d <- 
  cohen.d(correct_total_mindful_wandering , "condition")

total_recognition_d$Measure[1] <- "Total Rec."
total_recognition_d$Comparison[1] <- "Mindfulness vs. Wandering"
total_recognition_d$d[1] <- -correct_total_mindful_wandering_d$cohen.d[2]
total_recognition_d$lower_ci[1] <- -correct_total_mindful_wandering_d$cohen.d[3]
total_recognition_d$upper_ci[1] <- -correct_total_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
correct_total_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "mind_wandering")
correct_total_mindful_dots_d <- 
  cohen.d(correct_total_mindful_dots , "condition")

total_recognition_d$Measure[2] <- "Total Rec."
total_recognition_d$Comparison[2] <- "Mindfulness vs. Dots"
total_recognition_d$d[2] <- -correct_total_mindful_dots_d$cohen.d[2]
total_recognition_d$lower_ci[2] <- -correct_total_mindful_dots_d$cohen.d[3]
total_recognition_d$upper_ci[2] <- -correct_total_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
correct_total_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "mindfulness")
correct_total_wandering_dots_d <- 
  cohen.d(correct_total_wandering_dots , "condition")

total_recognition_d$Measure[3] <- "Total Rec."
total_recognition_d$Comparison[3] <- "Wandering vs. Dots"
total_recognition_d$d[3] <- -correct_total_wandering_dots_d$cohen.d[2]
total_recognition_d$lower_ci[3] <- -correct_total_wandering_dots_d$cohen.d[3]
total_recognition_d$upper_ci[3] <- -correct_total_wandering_dots_d$cohen.d[1]


# false total recognition
# mindfulness vs. mind_wandering
false_total_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "join_dots")
false_total_mindful_wandering_d <- 
  cohen.d(false_total_mindful_wandering , "condition")

total_recognition_d$Measure[4] <- "False Rec."
total_recognition_d$Comparison[4] <- "Mindfulness vs. Wandering"
total_recognition_d$d[4] <- -false_total_mindful_wandering_d$cohen.d[2]
total_recognition_d$lower_ci[4] <- -false_total_mindful_wandering_d$cohen.d[3]
total_recognition_d$upper_ci[4] <- -false_total_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
false_total_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "mind_wandering")
false_total_mindful_dots_d <- 
  cohen.d(false_total_mindful_dots , "condition")

total_recognition_d$Measure[5] <- "False Rec."
total_recognition_d$Comparison[5] <- "Mindfulness vs. Dots"
total_recognition_d$d[5] <- -false_total_mindful_dots_d$cohen.d[2]
total_recognition_d$lower_ci[5] <- -false_total_mindful_dots_d$cohen.d[3]
total_recognition_d$upper_ci[5] <- -false_total_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
false_total_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "mindfulness")
false_total_wandering_dots_d <- 
  cohen.d(false_total_wandering_dots , "condition")

total_recognition_d$Measure[6] <- "False Rec."
total_recognition_d$Comparison[6] <- "Wandering vs. Dots"
total_recognition_d$d[6] <- -false_total_wandering_dots_d$cohen.d[2]
total_recognition_d$lower_ci[6] <- -false_total_wandering_dots_d$cohen.d[3]
total_recognition_d$upper_ci[6] <- -false_total_wandering_dots_d$cohen.d[1]


# filler total recognition
# mindfulness vs. mind_wandering
filler_total_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_total) %>% 
  filter(condition != "join_dots")
filler_total_mindful_wandering_d <- 
  cohen.d(filler_total_mindful_wandering , "condition")

total_recognition_d$Measure[7] <- "Filler Rec."
total_recognition_d$Comparison[7] <- "Mindfulness vs. Wandering"
total_recognition_d$d[7] <- -filler_total_mindful_wandering_d$cohen.d[2]
total_recognition_d$lower_ci[7] <- -filler_total_mindful_wandering_d$cohen.d[3]
total_recognition_d$upper_ci[7] <- -filler_total_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
filler_total_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_total) %>% 
  filter(condition != "mind_wandering")
filler_total_mindful_dots_d <- 
  cohen.d(filler_total_mindful_dots , "condition")

total_recognition_d$Measure[8] <- "Filler Rec."
total_recognition_d$Comparison[8] <- "Mindfulness vs. Dots"
total_recognition_d$d[8] <- -filler_total_mindful_dots_d$cohen.d[2]
total_recognition_d$lower_ci[8] <- -filler_total_mindful_dots_d$cohen.d[3]
total_recognition_d$upper_ci[8] <- -filler_total_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
filler_total_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_total) %>% 
  filter(condition != "mindfulness")
filler_total_wandering_dots_d <- 
  cohen.d(filler_total_wandering_dots, "condition")

total_recognition_d$Measure[9] <- "Filler Rec."
total_recognition_d$Comparison[9] <- "Wandering vs. Dots"
total_recognition_d$d[9] <- -filler_total_wandering_dots_d$cohen.d[2]
total_recognition_d$lower_ci[9] <- -filler_total_wandering_dots_d$cohen.d[3]
total_recognition_d$upper_ci[9] <- -filler_total_wandering_dots_d$cohen.d[1]


# Do the plot of all standardised effect sizes
palette <- c("#08306B", "#4292C6", "#9ECAE1")
pd <- position_dodge(0.3)
total_recognition_d_plot <- ggplot(total_recognition_d, 
                              aes(x = Comparison, 
                                  y = d, 
                                  group = Measure)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = Measure, colour = Measure), size = 3, position = pd) + 
  geom_errorbar(aes(colour = Measure, 
                    ymin = lower_ci, ymax = upper_ci), 
                width = .14, size = .5, position = pd) + 
  scale_color_manual(values = palette) +
  ggtitle("Total Recognition") + 
  theme_bw()
pdf("total_recognition_d_plot.pdf", width = 7.5, height = 6)
total_recognition_d_plot
dev.off()




##--- remember responses
# first, create data frame to store all cohen-d values in
remember_recognition_d <- data.frame(
  Measure = rep(0, 9), 
  Comparison = rep(0, 9), 
  d = rep(0, 9), 
  lower_ci = rep(0, 9), 
  upper_ci = rep(0, 9)
)

# correct remember recognition
# mindfulness vs. mind_wandering
correct_remember_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_r) %>% 
  filter(condition != "join_dots")
correct_remember_mindful_wandering_d <- 
  cohen.d(correct_remember_mindful_wandering , "condition")

remember_recognition_d$Measure[1] <- "Correct Rec."
remember_recognition_d$Comparison[1] <- "Mindfulness vs. Wandering"
remember_recognition_d$d[1] <- -correct_remember_mindful_wandering_d$cohen.d[2]
remember_recognition_d$lower_ci[1] <- -correct_remember_mindful_wandering_d$cohen.d[3]
remember_recognition_d$upper_ci[1] <- -correct_remember_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
correct_remember_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_r) %>% 
  filter(condition != "mind_wandering")
correct_remember_mindful_dots_d <- 
  cohen.d(correct_remember_mindful_dots , "condition")

remember_recognition_d$Measure[2] <- "Correct Rec."
remember_recognition_d$Comparison[2] <- "Mindfulness vs. Dots"
remember_recognition_d$d[2] <- -correct_remember_mindful_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[2] <- -correct_remember_mindful_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[2] <- -correct_remember_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
correct_remember_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_r) %>% 
  filter(condition != "mindfulness")
correct_remember_wandering_dots_d <- 
  cohen.d(correct_remember_wandering_dots , "condition")

remember_recognition_d$Measure[3] <- "Correct Rec."
remember_recognition_d$Comparison[3] <- "Wandering vs. Dots"
remember_recognition_d$d[3] <- -correct_remember_wandering_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[3] <- -correct_remember_wandering_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[3] <- -correct_remember_wandering_dots_d$cohen.d[1]


# false remember recognition
# mindfulness vs. mind_wandering
false_remember_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_r) %>% 
  filter(condition != "join_dots")
false_remember_mindful_wandering_d <- 
  cohen.d(false_remember_mindful_wandering , "condition")

remember_recognition_d$Measure[4] <- "False Rec."
remember_recognition_d$Comparison[4] <- "Mindfulness vs. Wandering"
remember_recognition_d$d[4] <- -false_remember_mindful_wandering_d$cohen.d[2]
remember_recognition_d$lower_ci[4] <- -false_remember_mindful_wandering_d$cohen.d[3]
remember_recognition_d$upper_ci[4] <- -false_remember_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
false_remember_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_r) %>% 
  filter(condition != "mind_wandering")
false_remember_mindful_dots_d <- 
  cohen.d(false_remember_mindful_dots , "condition")

remember_recognition_d$Measure[5] <- "False Rec."
remember_recognition_d$Comparison[5] <- "Mindfulness vs. Dots"
remember_recognition_d$d[5] <- -false_remember_mindful_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[5] <- -false_remember_mindful_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[5] <- -false_remember_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
false_remember_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_r) %>% 
  filter(condition != "mindfulness")
false_remember_wandering_dots_d <- 
  cohen.d(false_remember_wandering_dots , "condition")

remember_recognition_d$Measure[6] <- "False Rec."
remember_recognition_d$Comparison[6] <- "Wandering vs. Dots"
remember_recognition_d$d[6] <- -false_remember_wandering_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[6] <- -false_remember_wandering_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[6] <- -false_remember_wandering_dots_d$cohen.d[1]


# filler remember recognition
# mindfulness vs. mind_wandering
filler_remember_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_r) %>% 
  filter(condition != "join_dots")
filler_remember_mindful_wandering_d <- 
  cohen.d(filler_remember_mindful_wandering , "condition")

remember_recognition_d$Measure[7] <- "Filler Rec."
remember_recognition_d$Comparison[7] <- "Mindfulness vs. Wandering"
remember_recognition_d$d[7] <- -filler_remember_mindful_wandering_d$cohen.d[2]
remember_recognition_d$lower_ci[7] <- -filler_remember_mindful_wandering_d$cohen.d[3]
remember_recognition_d$upper_ci[7] <- -filler_remember_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
filler_remember_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_r) %>% 
  filter(condition != "mind_wandering")
filler_remember_mindful_dots_d <- 
  cohen.d(filler_remember_mindful_dots , "condition")

remember_recognition_d$Measure[8] <- "Filler Rec."
remember_recognition_d$Comparison[8] <- "Mindfulness vs. Dots"
remember_recognition_d$d[8] <- -filler_remember_mindful_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[8] <- -filler_remember_mindful_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[8] <- -filler_remember_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
filler_remember_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_r) %>% 
  filter(condition != "mindfulness")
filler_remember_wandering_dots_d <- 
  cohen.d(filler_remember_wandering_dots, "condition")

remember_recognition_d$Measure[9] <- "Filler Rec."
remember_recognition_d$Comparison[9] <- "Wandering vs. Dots"
remember_recognition_d$d[9] <- -filler_remember_wandering_dots_d$cohen.d[2]
remember_recognition_d$lower_ci[9] <- -filler_remember_wandering_dots_d$cohen.d[3]
remember_recognition_d$upper_ci[9] <- -filler_remember_wandering_dots_d$cohen.d[1]


# Do the plot of all standardised effect sizes
pd <- position_dodge(0.3)
remember_recognition_d_plot <- ggplot(remember_recognition_d, 
                                   aes(x = Comparison, 
                                       y = d, 
                                       group = Measure)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = Measure, colour = Measure), size = 3, position = pd) + 
  geom_errorbar(aes(colour = Measure, 
                    ymin = lower_ci, ymax = upper_ci), 
                width = .14, size = .5, position = pd) + 
  scale_color_manual(values = palette) +
  ggtitle("Remember Recognition") + 
  theme_bw()
pdf("remember_recognition_d_plot.pdf", width = 7.5, height = 6)
remember_recognition_d_plot
dev.off()





##--- know responses

# first, create data frame to store all cohen-d values in
know_recognition_d <- data.frame(
  Measure = rep(0, 9), 
  Comparison = rep(0, 9), 
  d = rep(0, 9), 
  lower_ci = rep(0, 9), 
  upper_ci = rep(0, 9)
)

# correct know recognition
# mindfulness vs. mind_wandering
correct_know_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_k) %>% 
  filter(condition != "join_dots")
correct_know_mindful_wandering_d <- 
  cohen.d(correct_know_mindful_wandering , "condition")

know_recognition_d$Measure[1] <- "Correct Rec."
know_recognition_d$Comparison[1] <- "Mindfulness vs. Wandering"
know_recognition_d$d[1] <- -correct_know_mindful_wandering_d$cohen.d[2]
know_recognition_d$lower_ci[1] <- -correct_know_mindful_wandering_d$cohen.d[3]
know_recognition_d$upper_ci[1] <- -correct_know_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
correct_know_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_k) %>% 
  filter(condition != "mind_wandering")
correct_know_mindful_dots_d <- 
  cohen.d(correct_know_mindful_dots , "condition")

know_recognition_d$Measure[2] <- "Correct Rec."
know_recognition_d$Comparison[2] <- "Mindfulness vs. Dots"
know_recognition_d$d[2] <- -correct_know_mindful_dots_d$cohen.d[2]
know_recognition_d$lower_ci[2] <- -correct_know_mindful_dots_d$cohen.d[3]
know_recognition_d$upper_ci[2] <- -correct_know_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
correct_know_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, list_k) %>% 
  filter(condition != "mindfulness")
correct_know_wandering_dots_d <- 
  cohen.d(correct_know_wandering_dots , "condition")

know_recognition_d$Measure[3] <- "Correct Rec."
know_recognition_d$Comparison[3] <- "Wandering vs. Dots"
know_recognition_d$d[3] <- -correct_know_wandering_dots_d$cohen.d[2]
know_recognition_d$lower_ci[3] <- -correct_know_wandering_dots_d$cohen.d[3]
know_recognition_d$upper_ci[3] <- -correct_know_wandering_dots_d$cohen.d[1]


# false remember recognition
# mindfulness vs. mind_wandering
false_know_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_k) %>% 
  filter(condition != "join_dots")
false_know_mindful_wandering_d <- 
  cohen.d(false_know_mindful_wandering , "condition")

know_recognition_d$Measure[4] <- "False Rec."
know_recognition_d$Comparison[4] <- "Mindfulness vs. Wandering"
know_recognition_d$d[4] <- -false_know_mindful_wandering_d$cohen.d[2]
know_recognition_d$lower_ci[4] <- -false_know_mindful_wandering_d$cohen.d[3]
know_recognition_d$upper_ci[4] <- -false_know_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
false_know_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_k) %>% 
  filter(condition != "mind_wandering")
false_know_mindful_dots_d <- 
  cohen.d(false_know_mindful_dots , "condition")

know_recognition_d$Measure[5] <- "False Rec."
know_recognition_d$Comparison[5] <- "Mindfulness vs. Dots"
know_recognition_d$d[5] <- -false_know_mindful_dots_d$cohen.d[2]
know_recognition_d$lower_ci[5] <- -false_know_mindful_dots_d$cohen.d[3]
know_recognition_d$upper_ci[5] <- -false_know_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
false_know_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, lure_k) %>% 
  filter(condition != "mindfulness")
false_know_wandering_dots_d <- 
  cohen.d(false_know_wandering_dots , "condition")

know_recognition_d$Measure[6] <- "False Rec."
know_recognition_d$Comparison[6] <- "Wandering vs. Dots"
know_recognition_d$d[6] <- -false_know_wandering_dots_d$cohen.d[2]
know_recognition_d$lower_ci[6] <- -false_know_wandering_dots_d$cohen.d[3]
know_recognition_d$upper_ci[6] <- -false_know_wandering_dots_d$cohen.d[1]


# filler remember recognition
# mindfulness vs. mind_wandering
filler_know_mindful_wandering <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_k) %>% 
  filter(condition != "join_dots")
filler_know_mindful_wandering_d <- 
  cohen.d(filler_know_mindful_wandering , "condition")

know_recognition_d$Measure[7] <- "Filler Rec."
know_recognition_d$Comparison[7] <- "Mindfulness vs. Wandering"
know_recognition_d$d[7] <- -filler_know_mindful_wandering_d$cohen.d[2]
know_recognition_d$lower_ci[7] <- -filler_know_mindful_wandering_d$cohen.d[3]
know_recognition_d$upper_ci[7] <- -filler_know_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
filler_know_mindful_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_k) %>% 
  filter(condition != "mind_wandering")
filler_know_mindful_dots_d <- 
  cohen.d(filler_know_mindful_dots , "condition")

know_recognition_d$Measure[8] <- "Filler Rec."
know_recognition_d$Comparison[8] <- "Mindfulness vs. Dots"
know_recognition_d$d[8] <- -filler_know_mindful_dots_d$cohen.d[2]
know_recognition_d$lower_ci[8] <- -filler_know_mindful_dots_d$cohen.d[3]
know_recognition_d$upper_ci[8] <- -filler_know_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
filler_know_wandering_dots <- recognition_data %>% 
  select(-participant) %>% 
  select(condition, filler_k) %>% 
  filter(condition != "mindfulness")
filler_know_wandering_dots_d <- 
  cohen.d(filler_know_wandering_dots, "condition")

know_recognition_d$Measure[9] <- "Filler Rec."
know_recognition_d$Comparison[9] <- "Wandering vs. Dots"
know_recognition_d$d[9] <- -filler_know_wandering_dots_d$cohen.d[2]
know_recognition_d$lower_ci[9] <- -filler_know_wandering_dots_d$cohen.d[3]
know_recognition_d$upper_ci[9] <- -filler_know_wandering_dots_d$cohen.d[1]


# Do the plot of all standardised effect sizes
pd <- position_dodge(0.3)
know_recognition_d_plot <- ggplot(know_recognition_d, 
                                      aes(x = Comparison, 
                                          y = d, 
                                          group = Measure)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = Measure, colour = Measure), size = 3, position = pd) + 
  geom_errorbar(aes(colour = Measure, 
                    ymin = lower_ci, ymax = upper_ci), 
                width = .14, size = .5, position = pd) + 
  scale_color_manual(values = palette) +
  ggtitle("Know Recognition") + 
  theme_bw()
pdf("know_recognition_d_plot.pdf", width = 7.5, height = 6)
know_recognition_d_plot
dev.off()




##--- recall responses
# first, create data frame to store all cohen-d values in
recall_d <- data.frame(
  Measure = rep(0, 6), 
  Comparison = rep(0, 6), 
  d = rep(0, 6), 
  lower_ci = rep(0, 6), 
  upper_ci = rep(0, 6)
)

# correct recall
# mindfulness vs. mind_wandering
correct_recall_mindful_wandering <- recall_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "join_dots")
correct_recall_mindful_wandering_d <- 
  cohen.d(correct_recall_mindful_wandering , "condition")

recall_d$Measure[1] <- "Correct Recall"
recall_d$Comparison[1] <- "Mindfulness vs. Wandering"
recall_d$d[1] <- -correct_recall_mindful_wandering_d$cohen.d[2]
recall_d$lower_ci[1] <- -correct_recall_mindful_wandering_d$cohen.d[3]
recall_d$upper_ci[1] <- -correct_recall_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
correct_recall_mindful_dots <- recall_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "mind_wandering")
correct_recall_mindful_dots_d <- 
  cohen.d(correct_recall_mindful_dots , "condition")

recall_d$Measure[2] <- "Correct Recall"
recall_d$Comparison[2] <- "Mindfulness vs. Dots"
recall_d$d[2] <- -correct_recall_mindful_dots_d$cohen.d[2]
recall_d$lower_ci[2] <- -correct_recall_mindful_dots_d$cohen.d[3]
recall_d$upper_ci[2] <- -correct_recall_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
correct_recall_wandering_dots <- recall_data %>% 
  select(-participant) %>% 
  select(condition, list_total) %>% 
  filter(condition != "mindfulness")
correct_recall_wandering_dots_d <- 
  cohen.d(correct_recall_wandering_dots , "condition")

recall_d$Measure[3] <- "Correct Recall"
recall_d$Comparison[3] <- "Wandering vs. Dots"
recall_d$d[3] <- -correct_recall_wandering_dots_d$cohen.d[2]
recall_d$lower_ci[3] <- -correct_recall_wandering_dots_d$cohen.d[3]
recall_d$upper_ci[3] <- -correct_recall_wandering_dots_d$cohen.d[1]



# false recall
# mindfulness vs. mind_wandering
false_recall_mindful_wandering <- recall_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "join_dots")
false_recall_mindful_wandering_d <- 
  cohen.d(false_recall_mindful_wandering , "condition")

recall_d$Measure[4] <- "False Recall"
recall_d$Comparison[4] <- "Mindfulness vs. Wandering"
recall_d$d[4] <- -false_recall_mindful_wandering_d$cohen.d[2]
recall_d$lower_ci[4] <- -false_recall_mindful_wandering_d$cohen.d[3]
recall_d$upper_ci[4] <- -false_recall_mindful_wandering_d$cohen.d[1]


# mindfulness vs. join-the-dots
false_recall_mindful_dots <- recall_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "mind_wandering")
false_recall_mindful_dots_d <- 
  cohen.d(false_recall_mindful_dots , "condition")

recall_d$Measure[5] <- "False Recall"
recall_d$Comparison[5] <- "Mindfulness vs. Dots"
recall_d$d[5] <- -false_recall_mindful_dots_d$cohen.d[2]
recall_d$lower_ci[5] <- -false_recall_mindful_dots_d$cohen.d[3]
recall_d$upper_ci[5] <- -false_recall_mindful_dots_d$cohen.d[1]


# mindwandering vs. join-the-dots
false_recall_wandering_dots <- recall_data %>% 
  select(-participant) %>% 
  select(condition, lure_total) %>% 
  filter(condition != "mindfulness")
false_recall_wandering_dots_d <- 
  cohen.d(false_recall_wandering_dots , "condition")

recall_d$Measure[6] <- "False Recall"
recall_d$Comparison[6] <- "Wandering vs. Dots"
recall_d$d[6] <- -false_recall_wandering_dots_d$cohen.d[2]
recall_d$lower_ci[6] <- -false_recall_wandering_dots_d$cohen.d[3]
recall_d$upper_ci[6] <- -false_recall_wandering_dots_d$cohen.d[1]



# Do the plot of all standardised effect sizes
pd <- position_dodge(0.3)
recall_d_plot <- ggplot(recall_d, 
                        aes(x = Comparison, 
                            y = d, 
                            group = Measure)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(shape = Measure, colour = Measure), size = 3, position = pd) + 
  geom_errorbar(aes(colour = Measure, 
                    ymin = lower_ci, ymax = upper_ci), 
                width = .14, size = .5, position = pd) + 
  scale_color_manual(values = palette) +
  ggtitle("Recall") + 
  theme_bw()
pdf("recall_d_plot.pdf", width = 7.5, height = 6)
recall_d_plot
dev.off()



# plot of all plots
library(gridExtra)
pdf("all_effect_sizes.pdf", width = 13, height = 13)
grid.arrange(manipulation_d_plot,  
             total_recognition_d_plot, 
             remember_recognition_d_plot,
             know_recognition_d_plot, 
             recall_d_plot, 
             ncol = 2)
dev.off()


#------------------------------------------------------------------------------




#------------------------------------------------------------------------------
### section 16: regressions involving recognition responses


##--- list total

# list total vs. sms_total
recognition_sms_total_list_total_lm <- 
  lm(list_total ~ post_manip_total_sms, data = recognition_data)
summary(recognition_sms_total_list_total_lm)

# list total vs. sms_mind
recognition_sms_mind_list_total_lm <- 
  lm(list_total ~ post_manip_subscale_mind, data = recognition_data)
summary(recognition_sms_mind_list_total_lm)


# list total vs. sms_body
recognition_sms_body_list_total_lm <-
  lm(list_total ~ post_manip_subscale_body, 
     data = recognition_data)
summary(recognition_sms_body_list_total_lm)


# list total vs. tri
recognition_tri_list_total_lm <-
  lm(list_total ~ tri, data = recognition_data)
summary(recognition_tri_list_total_lm)

# this was significant, so do a plot
tri_list_total_plot <- recognition_data %>% 
  ggplot(aes(x = tri, y = list_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "TRI Score", y = "List (Total)")


# list total vs. tut
recognition_tut_list_total_lm <-
  lm(list_total ~ tut, 
     data = recognition_data)
summary(recognition_tut_list_total_lm)



##--- lure total

# lure total vs. sms_total
recognition_sms_total_lure_total_lm <-
  lm(lure_total ~ post_manip_total_sms, 
     data = recognition_data)
summary(recognition_sms_total_lure_total_lm)

# this was significant, so do a plot
total_sms_lure_total_plot <- recognition_data %>% 
  ggplot(aes(x = post_manip_total_sms, y = lure_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "SMS (Total)", y = "Lure (Total)")

# lure total vs. sms_mind
recognition_sms_mind_lure_total_lm <- 
  lm(lure_total ~ post_manip_subscale_mind, data = recognition_data)
summary(recognition_sms_mind_lure_total_lm)

# this was significant, so do a plot
mind_sms_lure_total_plot <- recognition_data %>% 
  ggplot(aes(x = post_manip_subscale_mind, y = lure_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "SMS (Mind)", y = "Lure (Total)")

# lure total vs. sms_body
recognition_sms_body_lure_total_lm <-
  lm(lure_total ~ post_manip_subscale_body, 
     data = recognition_data)
summary(recognition_sms_body_lure_total_lm)


# lure total vs. tri
recognition_tri_lure_total_lm <-
  lm(lure_total ~ tri, data = recognition_data)
summary(recognition_tri_lure_total_lm)

# lure total vs. tut
recognition_tut_lure_total_lm <-
  lm(lure_total ~ tut, 
     data = recognition_data)
summary(recognition_tut_lure_total_lm)


#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
### section 17: regressions involving recall responses


##--- list total

# list total vs. sms_total
recall_sms_total_list_total_lm <- 
  lm(list_total ~ post_manip_total_sms, data = recall_data)
summary(recall_sms_total_list_total_lm)

# list total vs. sms_mind
recall_sms_mind_list_total_lm <- 
  lm(list_total ~ post_manip_subscale_mind, data = recall_data)
summary(recall_sms_mind_list_total_lm)

# list total vs. sms_body
recall_sms_body_list_total_lm <-
  lm(list_total ~ post_manip_subscale_body, 
     data = recall_data)
summary(recall_sms_body_list_total_lm)


# list total vs. tri
recall_tri_list_total_lm <-
  lm(list_total ~ tri, data = recall_data)
summary(recall_tri_list_total_lm)

# this was significant, so do a plot
recall_tri_list_total_plot <- recall_data %>% 
  ggplot(aes(x = tri, y = list_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "TRI Score", y = "List (Total) - Recall")


# list total vs. tut
recall_tut_list_total_lm <-
  lm(list_total ~ tut, 
     data = recall_data)
summary(recall_tut_list_total_lm)



##--- lure total

# lure total vs. sms_total
recall_sms_total_lure_total_lm <-
  lm(lure_total ~ post_manip_total_sms, 
     data = recall_data)
summary(recall_sms_total_lure_total_lm)

# this was significant, so do a plot
total_sms_lure_total_plot <- recall_data %>% 
  ggplot(aes(x = post_manip_total_sms, y = lure_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "SMS (Total)", y = "Lure (Total) - Recall")

# lure total vs. sms_mind
recall_sms_mind_lure_total_lm <- 
  lm(lure_total ~ post_manip_subscale_mind, data = recall_data)
summary(recall_sms_mind_lure_total_lm)

# lure total vs. sms_body
recall_sms_body_lure_total_lm <-
  lm(lure_total ~ post_manip_subscale_body, 
     data = recall_data)
summary(recall_sms_body_lure_total_lm)

# this was significant, so do a plot
body_sms_lure_total_plot <- recall_data %>% 
  ggplot(aes(x = post_manip_subscale_body, y = lure_total)) + 
  geom_smooth(method = "lm") + 
  geom_point(alpha = 0.6, colour = "#67a9cf", size = 2) + 
  labs(x = "SMS (Body)", y = "Lure (Total) - Recall")

# lure total vs. tri
recall_tri_lure_total_lm <-
  lm(lure_total ~ tri, data = recall_data)
summary(recall_tri_lure_total_lm)

# lure total vs. tut
recall_tut_lure_total_lm <-
  lm(lure_total ~ tut, 
     data = recall_data)
summary(recall_tut_lure_total_lm)


#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 18: sensitivity analysis of achieved power

library(pwr)

# study power analysis based on original study's effect size
study_power <- pwr.anova.test(k = 3, f = 0.25, power = 0.95)

## sensitivity analysis

# average n per group
n_per_group <- floor(287 / 3)

power_range <- round(seq(from = 0.7, to = 1.0, length.out = 100), 4)
effect_size <- numeric(length(power_range))

# loop over all power possibilities
for(i in 1:length(power_range)){
  
  # get the effect size for current level of power
  curr_effect_size <- pwr.anova.test(k = 3, n = n_per_group, 
                                     sig.level = 0.05,
                                     power = power_range[i])
  
  effect_size[i] <- curr_effect_size$f
}


# plot of sensitivity analysis
setwd(here("figures"))
pdf("power_sensitivty_plot.pdf", width = 6, height = 6)
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), 
          cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(power_range, effect_size, ylim = c(0.15, 0.30), xlim = c(0.75, 1), 
      pch = 19, col = "gray30",
     ylab = "Effect Size (f)", 
     xlab = "Power") 
rug(x = seq(from = 0.15, to = 0.30, by = 0.01),
    ticksize = -0.02, 
    side = 2)

lines(c(0.80, 0.80), c(0.15, effect_size[which(power_range == 0.8)]), 
      col = "gray30", lty = 2, lwd = 2)
lines(c(0.75, 0.80), 
      c(effect_size[which(power_range == 0.8)], 
        effect_size[which(power_range == 0.8)]), 
      col = "gray30", lty = 2, lwd = 2)

lines(c(0.90, 0.90), c(0.15, effect_size[which(power_range == 0.9)]), 
      col = "gray30", lty = 2, lwd = 2)
lines(c(0.75, 0.90), 
      c(effect_size[which(power_range == 0.9)], 
        effect_size[which(power_range == 0.9)]), 
      col = "gray30", lty = 2, lwd = 2)

lines(c(0.95, 0.95), c(0.15, effect_size[84]), 
      col = "gray30", lty = 2, lwd = 2)
lines(c(0.75, 0.95), 
      c(effect_size[84], 
        effect_size[84]), 
      col = "gray30", lty = 2, lwd = 2)

dev.off()
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 19: Omnibus tests adding change in mindfulness as covariate

## recognition
recognition_data <- recognition_data %>% 
  mutate(sms_difference = post_manip_total_sms - 
           pre_manip_total_sms)

# total correct
x <- lm(list_total ~ as.numeric(condition) + sms_difference, 
        data = recognition_data)
summary(x)


# total false recognition
x <- lm(lure_total ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)



# total filler
x <- lm(filler_total ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)


# remember correct recognition
x <- lm(list_r ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)


# remember false recognition
x <- lm(lure_r ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)



# remember filler
x <- lm(filler_r ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)



# know correct recognition
x <- lm(list_k ~ as.numeric(condition) + sms_difference, 
             data = recognition_data)
summary(x)


# know false recognition
x <- lm(lure_k ~ as.numeric(condition) + sms_difference, 
        data = recognition_data)
summary(x)

# know filler recognition
x <- lm(filler_k ~ as.numeric(condition) + sms_difference, 
        data = recognition_data)
summary(x)


## recall 

recall_data <- recall_data %>% 
  mutate(sms_difference = post_manip_total_sms - 
           pre_manip_total_sms)

# correct recall
x <- lm(list_total ~ as.numeric(condition) + sms_difference, 
        data = recall_data)
summary(x)

# false recall
x <- lm(lure_total ~ as.numeric(condition) + sms_difference, 
        data = recall_data)
summary(x)
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
### section 20: Omnibus tests adding mindwandering TRI and TUT 
# score as covariate


# total correct
x <- lm(list_total ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(list_total ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


# total false recognition
x <- lm(lure_total ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(lure_total ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)



# total filler
x <- lm(filler_total ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(filler_total ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


# remember correct recognition
x <- lm(list_r ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(list_r ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


# remember false recognition
x <- lm(lure_r ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(lure_r ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)



# remember filler
x <- lm(filler_r ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(filler_r ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)



# know correct recognition
x <- lm(list_k ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(list_k ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


# know false recognition
x <- lm(lure_k ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(lure_k ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


# know filler recognition
x <- lm(filler_k ~ as.numeric(condition) + tri, 
        data = recognition_data)
summary(x)
x <- lm(filler_k ~ as.numeric(condition) + tut, 
        data = recognition_data)
summary(x)


## recall 
# correct recall
x <- lm(list_total ~ as.numeric(condition) + tri, 
        data = recall_data)
summary(x)
x <- lm(list_total ~ as.numeric(condition) + tut, 
        data = recall_data)
summary(x)

# false recall
x <- lm(lure_total ~ as.numeric(condition) + tri, 
        data = recall_data)
summary(x)
x <- lm(lure_total ~ as.numeric(condition) + tut, 
        data = recall_data)
summary(x)
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### section 21: Omnibus tests with participants removed who don't show
### changed in mindfulness in the expected direction

# find participants in the mindfulness condition who showed no change
# or a decrease in mindfulness
mindfulness_participants <- recognition_data %>% 
  filter(condition == "mindfulness") %>% 
  filter(sms_difference <= 0) %>% 
  pull(participant)

# find participants in the control & mind-wandering conditions who showed an 
# increase in mindfulness
other_participants <- recognition_data %>% 
  filter(condition != "mindfulness") %>% 
  filter(sms_difference > 0) %>% 
  pull(participant)

# density plot of the change in mindfulness scores for these "other" participants
other_plot_data <- recognition_data %>% 
  filter(participant %in% other_participants) %>% 
  pull(sms_difference)

# too many peple need to be removed from the mind-wandering condition for the 
# planned analysis to be viable.
x <- recognition_data %>% 
  filter(participant %in% other_participants) %>% 
  filter(condition == "mind_wandering") 
