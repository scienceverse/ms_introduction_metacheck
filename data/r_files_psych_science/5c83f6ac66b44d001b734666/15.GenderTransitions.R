####################################
# File 14 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: October 15 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
rm(list=ls())
library(mice)
library(ggplot2)
library(extrafont)
library(viridis)
library(brms)
library(bridgesampling)
library(arm)
library(Hmisc)
library(brms)
library(coda)
library(dplyr)
library(tidybayes)
library(bayesplot)

# Set working directory
setwd("")

# Read in data
data <- read.table("GenderTransition.csv",header=TRUE,sep = "")
load(file = "data3.short.rda")

# Remove participant with 3 month follow-up
data3.long <- mice::complete(data3.short, action='long',include = TRUE)
dim(data3.long) # Correct N
data3.long <- subset(data3.long,data3.long$id !=77) # Correct N because reduced by 21 (20 imputations plus original data)

# Make short again
data3.short.no77 <- as.mids(data3.long)
  
##########################################
# No covariates - gender cognition only
##########################################

# Set prior on intercept and on regression ------------------------
# Use only standardized variable so can make reasonable choices on b priors
m1priors = c(prior(student_t(4,0, 10), class = "Intercept"),
             prior(student_t(4,0, 2.5), class = "b"))

# Fit model with gender cognition
a1.m1.PCTSI <- brm_multiple(transition ~ PCTSI.z,
                            family="bernoulli",
                            data=data3.short.no77,
                            save_all_pars=TRUE,
                            prior=m1priors,
                            seed=1234)
plot(a1.m1.PCTSI)
pp_check(a1.m1.PCTSI) # looks good
#save(a1.m1.PCTSI,file = "a1no77.m1.PCTSI.rda")

##########################################
# Covariate + gender cognition model
##########################################

# Fit model with covariates and gender cognition using M1 priors
a1.m3.PCTSI <- brm_multiple(transition ~ PCTSI.z + sex.z + agemos.z + racecat.z + MonthsTest.z
                            + polid.z  + income.z,
                            family="bernoulli",
                            data=data3.short.no77,
                            save_all_pars=TRUE,
                            prior=m1priors,
                            seed=1234)
plot(a1.m3.PCTSI)
pp_check(a1.m3.PCTSI)
#save(a1.m3.PCTSI,file = "a1no77.m3.PCTSI.rda")

###########################################
# Estimates to report
###########################################

# M1
parameters <- a1.m1.PCTSI %>%
  gather_draws(b_PCTSI.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios

# M3
parameters <- a1.m3.PCTSI %>%
  gather_draws(b_PCTSI.z, b_sex.z,b_agemos.z , b_racecat.z , b_MonthsTest.z
               , b_polid.z  , b_income.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios
