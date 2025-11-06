#Analysis of Experiment 3

library(readxl)
library(lme4)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(knitr)

#read in data
res1 <- read_excel("Experiment_3_results.xlsx")
names(res1) <- c("Subject","word","category","response")

#fit glmm
g1 <- glmmTMB(response ~ category + (1|Subject) + (1|word), data=res1,family=binomial)

summary(g1)

#group means on the probability scale
kable(summary(emmeans(g1,"category"),infer=TRUE,type="response"))

#pairwise differences, HSD adjustment for alpha level
kable(summary(emmeans(g1,"category",contr = "pairwise"),infer=TRUE)$contrasts)
