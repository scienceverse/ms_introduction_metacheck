#------------------------------------------------------------------------------
### "Mindfulness and False Memory" - Analysis Script
### Written by Jim Grange, 5th August, 2016.
### Any corrections/questions about the code, please email grange.jim@gmail.com

### The analysis uses standard ANOVA with Tukey's HSD for planned 
### comparisons. It also uses Bayes Factor ANOVA using the BayesFactor R 
### package. For the planned comparisons, we use "order-restricted" multiple
### comparisons, as presented by Richard Morey here: http://bit.ly/2aAuKam
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### general set-up for analysis

# clear the workspace
rm(list = ls())

# set the random seed so the Bayesian posterior samples are reproducible
set.seed(42)

# set the working directory
setwd("~/Git/lab-book/Sues Project")

# load required packages
library(BayesFactor)
library(dplyr)
library(ggplot2)
library(tidyr)

# load the data files
recall <- read.csv("dummy_recall_data.csv")
recognition <- read.csv("dummy_recognition_data.csv")
remember <- read.csv("dummy_remember_data.csv")
know <- read.csv("dummy_know_data.csv")

# how many subjects are there?
n_subjects <- length(unique(recall$subject))
#------------------------------------------------------------------------------


# go through each data file and do the analysis
#------------------------------------------------------------------------------
### RECALL ###

# reshape data to wide format
recall <- gather(recall, response, accuracy, correctRecall:falseRecall)

# get the means and standard error for each condition and response type
mean_data_recall <- recall %>%
  group_by(state, response) %>%
  summarise(mean = round(mean(accuracy), 2), se = round(sd(accuracy) / 
                                                          sqrt(n_subjects), 2))

# plot the data and print to PDF
pdf("recall.pdf", width = 6, height = 8)
p <- ggplot(mean_data_recall, aes(x = state, y = mean, 
                                  group = response))
p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.0, 
                       size = 0.5)
p <- p + geom_point(size = 3)
p <- p + geom_line(aes(x = state, y = mean), linetype = "dashed")
p <- p + facet_grid(response ~ .)
p <- p + theme(panel.background = element_rect(fill = "grey86"))
p <- p + labs(x = "State of Mind", y = "Mean Recall (Proportion)")
# p <- p + scale_y_continuous(limits = c(0.6, 1))
p
dev.off()


##------------------- 
## CORRECT RECALL 
## prediction = mindwandering < no-manipulation < mindfulness

# get the data
correct_recall<- subset(recall, recall$response == "correctRecall")

# standard ANOVA & post-hoc
correct_recall_aov <- aov(accuracy ~ state, data = correct_recall)
correct_recall_HSD <- TukeyHSD(correct_recognition_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = correct_recall)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindwandering"]) & 
  (samples[, "state-mindfulness"] > samples[, "state-none"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
correct_recall_bf <- restricted_null
##-------------------


##-------------------
## FALSE RECALL 
## prediction = mindwandering < no-manipulation < mindfulness

# get the data
false_recall <- subset(recall, recall$response == "falseRecall")

# standard ANOVA & post-hoc
false_recall_aov <- aov(accuracy ~ state, data = false_recall)
false_recall_HSD <- TukeyHSD(false_recall_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = false_recall)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindwandering"]) & 
  (samples[, "state-mindfulness"] > samples[, "state-none"])
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
### RECOGNITION ###

# reshape data to wide format
recognition <- gather(recognition, response, accuracy, 
                      correctRecognition:fillerRecognition)

# get the means and standard error for each condition and response type
mean_data_recognition <- recognition %>%
  group_by(state, response) %>%
  summarise(mean = round(mean(accuracy), 2), se = round(sd(accuracy) / 
                                                          sqrt(n_subjects), 2))

# plot the data and print to PDF
pdf("recognition.pdf", width = 6, height = 8)
p <- ggplot(mean_data_recognition, aes(x = state, y = mean, 
                                       group = response))
p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.0, 
                       size = 0.5)
p <- p + geom_point(size = 3)
p <- p + geom_line(aes(x = state, y = mean), linetype = "dashed")
p <- p + facet_grid(response ~ .)
p <- p + theme(panel.background = element_rect(fill = "grey86"))
p <- p + labs(x = "State of Mind", y = "Mean Recognition (Proportion)")
# p <- p + scale_y_continuous(limits = c(0.6, 1))
p
dev.off()


##------------------- 
## CORRECT RECOGNITION 
## prediction = mindwandering < no-manipulation < mindfulness

# get the data
correct_recognition <- subset(recognition, 
                              recognition$response == "correctRecognition")

# standard ANOVA & post-hoc
correct_recognition_aov <- aov(accuracy ~ state, data = correct_recognition)
correct_recognition_HSD <- TukeyHSD(correct_recognition_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = correct_recognition)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindwandering"]) & 
               (samples[, "state-mindfulness"] > samples[, "state-none"])
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
##-------------------


##-------------------
## FALSE RECOGNITION 
## prediction = mindfulness< no-manipulation < mindwandering 

# get the data
false_recognition <- subset(recognition, 
                            recognition$response == "falseRecognition")

# standard ANOVA & post-hoc
false_recognition_aov <- aov(accuracy ~ state, data = false_recognition)
false_recognition_HSD <- TukeyHSD(false_recognition_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = false_recognition)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-mindwandering"] > samples[, "state-none"])
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


##-------------------
## FILLER RECOGNITION 
## prediction = mindfulness < no-manipulation < mindwandering

# get the data
filler_recognition <- subset(recognition, 
                             recognition$response == "fillerRecognition")

# standard ANOVA & post-hoc
filler_recognition_aov <- aov(accuracy ~ state, data = filler_recognition)
filler_recognition_HSD <- TukeyHSD(filler_recognition_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = filler_recognition)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-mindwandering"] > samples[, "state-none"])
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
##-------------------

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### REMEMBER RESPONSES ###

# reshape data to wide format
remember <- gather(remember, response, accuracy, correctRemember:fillerRemember)

# get the means and standard error for each condition and response type
mean_data_remember <- remember %>%
  group_by(state, response) %>%
  summarise(mean = round(mean(accuracy), 2), se = round(sd(accuracy) / 
                                                          sqrt(n_subjects), 2))

# plot the data and print to PDF
pdf("remember.pdf", width = 6, height = 8)
p <- ggplot(mean_data_remember, aes(x = state, y = mean, 
                                       group = response))
p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.0, 
                       size = 0.5)
p <- p + geom_point(size = 3)
p <- p + geom_line(aes(x = state, y = mean), linetype = "dashed")
p <- p + facet_grid(response ~ .)
p <- p + theme(panel.background = element_rect(fill = "grey86"))
p <- p + labs(x = "State of Mind", y = "Mean `Remember' Responses (Proportion)")
# p <- p + scale_y_continuous(limits = c(0.6, 1))
p
dev.off()


##------------------- 
## CORRECT REMEMBER 
## prediction = mindwandering < no-manipulation < mindfulness

# get the data
correct_remember <- subset(remember, remember$response == "correctRemember")

# standard ANOVA & post-hoc
correct_remember_aov <- aov(accuracy ~ state, data = correct_remember)
correct_remember_HSD <- TukeyHSD(correct_remember_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = correct_remember)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindwandering"]) & 
  (samples[, "state-mindfulness"] > samples[, "state-none"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
correct_remember_bf <- restricted_null
##-------------------


##-------------------
## FALSE REMEMBER 
## prediction = mindfulness < mindwandering < no-manipulation

# get the data
false_remember <- subset(remember, remember$response == "falseRemember")

# standard ANOVA & post-hoc
false_remember_aov <- aov(accuracy ~ state, data = false_remember)
false_remember_HSD <- TukeyHSD(false_remember_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = false_remember)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-mindwandering"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-none"] > samples[, "state-mindwandering"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_remember_bf <- restricted_null


##-------------------
## FILLER REMEMVER
## prediction = mindfulness < mindwandering < no-manipulation

# get the data
filler_remember <- subset(remember, remember$response == "fillerRemember")

# standard ANOVA & post-hoc
filler_remember_aov <- aov(accuracy ~ state, data = filler_remember)
filler_remember_HSD <- TukeyHSD(filler_remember_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = filler_remember)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-mindwandering"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-none"] > samples[, "state-mindwandering"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
filler_remember_bf <- restricted_null
##-------------------

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
### KNOW RESPONSES ###

# reshape data to wide format
know <- gather(know, response, accuracy, correctKnow:fillerKnow)

# get the means and standard error for each condition and response type
mean_data_know <- know %>%
  group_by(state, response) %>%
  summarise(mean = round(mean(accuracy), 2), se = round(sd(accuracy) / 
                                                          sqrt(n_subjects), 2))

# plot the data and print to PDF
pdf("know.pdf", width = 6, height = 8)
p <- ggplot(mean_data_know, aes(x = state, y = mean, 
                                    group = response))
p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.0, 
                       size = 0.5)
p <- p + geom_point(size = 3)
p <- p + geom_line(aes(x = state, y = mean), linetype = "dashed")
p <- p + facet_grid(response ~ .)
p <- p + theme(panel.background = element_rect(fill = "grey86"))
p <- p + labs(x = "State of Mind", y = "Mean `Know' Responses (Proportion)")
# p <- p + scale_y_continuous(limits = c(0.6, 1))
p
dev.off()


##------------------- 
## CORRECT KNOW 
## prediction = mindwandering < no-manipulation < mindfulness

# get the data
correct_know <- subset(know, know$response == "correctKnow")

# standard ANOVA & post-hoc
correct_know_aov <- aov(accuracy ~ state, data = correct_know)
correct_know_HSD <- TukeyHSD(correct_know_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = correct_know)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindwandering"]) & 
  (samples[, "state-mindfulness"] > samples[, "state-none"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
correct_know_bf <- restricted_null
##-------------------


##-------------------
## FALSE KNOW 
## prediction = mindfulness < no-manipulation < mindwandering

# get the data
false_know <- subset(know, know$response == "falseKnow")

# standard ANOVA & post-hoc
false_know_aov <- aov(accuracy ~ state, data = false_know)
false_know_HSD <- TukeyHSD(false_know_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = false_know)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-mindwandering"] > samples[, "state-none"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
false_know_bf <- restricted_null


##-------------------
## FILLER KNOW
## prediction = mindfulness < no-manipulation < mindwandering

# get the data
filler_know <- subset(know, know$response == "fillerKnow")

# standard ANOVA & post-hoc
filler_know_aov <- aov(accuracy ~ state, data = filler_know)
filler_know_HSD <- TukeyHSD(filler_know_aov)

# Bayes Factor ANOVA. This is done in 3 stages:
# 1) compare the unrestricted "full" model to the null
# 2) compare the unrestricted "full" model to an order restricted model
# 3) use the resulting Bayes factors to compare the null to the order 
# restriction

# 1)
full_null <- anovaBF(accuracy ~ state, data = filler_know)

# 2)
# sample from the posterior distribution of the full model
samples <- posterior(full_null, iterations = 10000)

# check the predicted order constraint in the posterior samples
consistent <- (samples[, "state-none"] > samples[, "state-mindfulness"]) & 
  (samples[, "state-mindwandering"] > samples[, "state-none"])
n_consistent <- sum(consistent)

# calculate the Bayes factor for restricted vs. full
restricted_full <- (n_consistent / 10000) / (1 / 6)

# 3)
# convert full_null to a number so it can be multiplied
full_null <- as.vector(full_null)

# compute final Bayes factor for restricted vs. null
restricted_null <- restricted_full * full_null

# transfer it to a more meaningful variable name
filler_know_bf <- restricted_null
##-------------------

#------------------------------------------------------------------------------