# Power Analysis

library(readxl)
library(rstatix)

data <- read_excel('fixation maps/IA_fixationVariables_semanticSimilarity.xlsx')

data$group <- ifelse(seq(1,44,1) %% 2,'CON','SEM')

# power for ttests
t.test(proportionFixations_adjusted ~ group, data = data, paired = TRUE)
cohens_d(proportionFixations_adjusted ~ group, data = data, paired = TRUE)

power_propFix <- wp.t(n1=22,n2=22, d=-1.28, type = "paired")
power_propFix$power

t.test(fixationDuration_adjusted ~ group, data = data, paired = TRUE)
cohens_d(fixationDuration_adjusted ~ group, data = data, paired = TRUE)

power_fixDur <-wp.t(n1=22,n2=22, d=-1.14, type = "paired")
power_fixDur$power

t.test(semSim_scoreCategory ~ group, data = data, paired = TRUE)
cohens_d(semSim_scoreCategory ~ group, data = data, paired = TRUE)

power_semSim_category <- wp.t(n1=22,n2=22, d=0.912, type = "paired")
power_semSim_category$power

t.test(semSim_scoreObjects ~ group, data = data, paired = TRUE)
cohens_d(semSim_scoreObjects ~ group, data = data, paired = TRUE)

power_semSim_objects <- wp.t(n1=22,n2=22, d=1.05, type = "paired")
power_semSim_objects$power

# power for correlations
power_propFix_semSimCat <- wp.correlation(n=44, r=-.37, alternative = "less")
power_propFix_semSimCat$power

power_fixDur_semSimCat <- wp.correlation(n=44, r=-.35, alternative = "less")
power_fixDur_semSimCat$power 

power_propFix_semSimObj <- wp.correlation(n=44, r=-.34, alternative = "less")
power_propFix_semSimObj$power

power_fixDur_semSimObj <- wp.correlation(n=44, r=-.36, alternative = "less")
power_fixDur_semSimObj$power

# power for exploratory analysis
#proportion fixations
data_all <- read_excel('~/Dropbox/Postdoc/Projects/Meaning Maps/fixation maps/allObjects_fixationVariables_semanticSimilarity.xlsx')
data_all <- data_all %>% filter(proportionFixations != 0)

linCat <- lm(sqrt(proportionFixations_adjusted) ~ semSim_scoreCategory, data = data_all)
summary(linCat)

power_linear_semSimCat <- wp.regression(n=504, p1 = 1, f2 = 0.1192)
power_linear_semSimCat$power

fix <- sqrt(data_all$proportionFixations_adjusted)
score <- data_all$semSim_scoreCategory
score2 <- data_all$semSim_scoreCategory^2
quadCat <- lm(fix ~ score2 + score)
summary(quadCat)

power_quadratic_semSimCat <- wp.regression(n=504, p1 = 2, f2 = 0.1317)
power_quadratic_semSimCat$power

linCat <- lm(sqrt(proportionFixations_adjusted) ~ semSim_scoreObjects, data = data_all)
summary(linCat)

power_linear_semSimObj <- wp.regression(n=504, p1 = 1, f2 = 0.027)
power_linear_semSimObj$power

fix <- sqrt(data_all$proportionFixations_adjusted)
score <- data_all$semSim_scoreObjects
score2 <- data_all$semSim_scoreObjects^2
quadCat <- lm(fix ~ score2 + score)
summary(quadCat)

power_quadratic_semSimObj <- wp.regression(n=504, p1 = 2, f2 = 0.055)
power_quadratic_semSimObj$power


# fixation duration
linCat <- lm(sqrt(fixationDuration_adjusted) ~ semSim_scoreCategory, data = data_all)
summary(linCat)

power_linear_semSimCat <- wp.regression(n=504, p1 = 1, f2 = 0.1163)
power_linear_semSimCat$power

fix <- sqrt(data_all$fixationDuration_adjusted)
score <- data_all$semSim_scoreCategory
score2 <- data_all$semSim_scoreCategory^2
quadCat <- lm(fix ~ score2 + score)
summary(quadCat)

power_quadratic_semSimCat <- wp.regression(n=504, p1 = 2, f2 = 0.1302)
power_quadratic_semSimCat$power

linCat <- lm(sqrt(fixationDuration_adjusted) ~ semSim_scoreObjects, data = data_all)
summary(linCat)

power_linear_semSimObj <- wp.regression(n=504, p1 = 1, f2 = 0.034)
power_linear_semSimObj$power

fix <- sqrt(data_all$fixationDuration_adjusted)
score <- data_all$semSim_scoreObjects
score2 <- data_all$semSim_scoreObjects^2
quadCat <- lm(fix ~ score2 + score)
summary(quadCat)

power_quadratic_semSimObj <- wp.regression(n=504, p1 = 2, f2 = 0.0687)
power_quadratic_semSimObj$power
