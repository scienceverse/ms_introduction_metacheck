####################################
# File 4 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: October 24 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
rm(list=ls())
library(rstan)
library(mice)
library(ggplot2)
library(extrafont)
library(viridis)
library(brms)
library(bridgesampling)
library(arm)
library(Hmisc)
library(brms)
library(cowplot)
library(coda)
library(dplyr)
library(tidybayes)
library(bayesplot)
library(loo)

# Set working directory
setwd("")

# Read in data for analyses
load(file = "data3.short.rda")

# Read in complete data for plot
data <- read.table("complete.data.csv",sep=",",header=TRUE)

# Set up for multiple cores
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores()) # Run on multiple cores

#################################################################################################
# First, run models, get estimates, and make plots for models presented in-text
#################################################################################################

##########################################
# No covariates - gender cognition only
##########################################

# Set prior on intercept and on predictors  ------------------------
m1priors = c(prior(student_t(4,0, 10), class = "Intercept"),
             prior(student_t(4,0, 2.5), class = "b"))

# Fit model with gender cognition
a1.m1.PCTSI <- brm_multiple(transition ~ PCTSI.z,
                   family="bernoulli",
                   data=data3.short,
                   save_all_pars=TRUE,
                   prior=m1priors,
                   seed=1234)
plot(a1.m1.PCTSI)
pp_check(a1.m1.PCTSI) # looks good
#save(a1.m1.PCTSI,file = "a1.m1.PCTSI.rda")

##########################################
# Covariate + gender cognition model
##########################################

# Note: Called M3 so that have logical ordering for model comparison approach below

# Fit model with covariates and gender cognition using M1 priors
a1.m3.PCTSI <- brm_multiple(transition ~ PCTSI.z + sex.z + agemos.z + racecat.z + MonthsTest.z
                   + polid.z  + income.z,
                   family="bernoulli",
                   data=data3.short,
                   save_all_pars=TRUE,
                   prior=m1priors,
                   seed=1234)
plot(a1.m3.PCTSI)
pp_check(a1.m3.PCTSI)
#save(a1.m3.PCTSI,file = "a1.m3.PCTSI.rda")

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

############################################
# Plot for no covariates - gender cognition only
############################################

# Plot 2a in paper
x <- complete(data3.short,action = "long")
m1fit <- round(data.frame(fitted(object = a1.m1.PCTSI,robust=TRUE,newdata =data.frame(PCTSI.z = seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05)),allow_new_levels = TRUE)),2)
m1fit$PCTSI.z <- seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05)
m1fit$PCTSI.p <- round(m1fit$PCTSI.z*2*sd(x$PCTSI.p) + mean(x$PCTSI.p),2)
m1HPD <- as.mcmc(fitted(object = a1.m1.PCTSI,newdata =data.frame(PCTSI.z = seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05),allow_new_levels = TRUE), summary = FALSE))
m1HPDinterval <- HPDinterval(m1HPD)
m1fit$lower <- m1HPDinterval[,1]
m1fit$upper <- m1HPDinterval[,2]

# Only GNC
data <- subset(data,data$group== "Gender Nonconforming")
data$plot <- "p"
c <- rep("p",85)

png("fig2a.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data,aes(y=transition,x=PCTSI.p)) + theme_gray() #+ geom_point(aes(color=c))
p <- p + geom_point(aes(color=c),alpha=.5,size=6,shape=16,position = position_jitter(h = .08,w=.01))
p <- p + scale_fill_viridis(discrete = T,direction = -1,option="B",begin=.2,end=.2)+ scale_color_viridis(discrete = T,direction = -1,option="B",begin=.2,end=.2)
p <- p + labs(x="Cross-Sex Gender Identity and Preferences", y="Probability of Transition") 
p <- p + theme(text=element_text(size=22))
p <- p + scale_x_continuous(limits = c(0,1))#,labels = function(x){ paste0(x, "%") }) 
p <- p + scale_y_continuous(breaks = c(0,.25,.5,.75,1),limits = c(-.08,1.08))
p2a <- p + theme(legend.position="none") + geom_ribbon(data=m1fit, aes(y=Estimate, ymin=lower, ymax=upper),alpha=.3) + 
  geom_line(data=m1fit, aes(y=Estimate))
#p1b <- p + annotate("text", x = 2, y = 1, label = "b",family="Gill Sans MT",size=10)
#p1 <- p1 + geom_segment(aes(x=50,xend=50,y=0,yend=.3),linetype=2) + geom_segment(aes(x=0,xend=50,y=.3,yend=.3),linetype=2)
#p1 <- p1 + geom_segment(aes(x=75,xend=75,y=0,yend=.49),linetype=2) + geom_segment(aes(x=0,xend=75,y=.49,yend=.49),linetype=2)
p2a
dev.off()

############################################
# Plot for covariate + gender cognition model
############################################

# Plot 2b in paper
m3fit <- data.frame(fitted(object = a1.m3.PCTSI,robust=TRUE,
                           newdata =data.frame(PCTSI.z = seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05),
                                               sex.z =0, agemos.z =0, racecat.z =0, MonthsTest.z =0, polid.z  =0, income.z=0),
                           allow_new_levels = TRUE))
m3fit$PCTSI.z <- seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05)
m3fit$PCTSI.p <- round(m3fit$PCTSI.z*2*sd(x$PCTSI.p) + mean(x$PCTSI.p),2)
m3HPD <- as.mcmc(fitted(object = a1.m3.PCTSI,newdata =data.frame(PCTSI.z = seq(min(x$PCTSI.z),max(x$PCTSI.z), 0.05),
                                                        sex.z =0, agemos.z =0, racecat.z =0, MonthsTest.z =0, polid.z  =0, income.z=0), summary = FALSE))
m3HPDinterval <- HPDinterval(m3HPD)
m3fit$lower <- m3HPDinterval[,1]
m3fit$upper <- m3HPDinterval[,2]

png("fig2b.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data,aes(y=transition,x=PCTSI.p)) + theme_gray() #+ geom_point(aes(color=c))
p <- p + geom_point(aes(color=c),alpha=.5,size=6,shape=16,position = position_jitter(h = .08,w=.01))
p <- p + scale_fill_viridis(discrete = T,direction = -1,option="B",begin=.2,end=.2)+ scale_color_viridis(discrete = T,direction = -1,option="B",begin=.2,end=.2)
p <- p + labs(x="Cross-Sex Gender Identity and Preferences", y="Probability of Transition") 
p <- p + theme(text=element_text(size=22))
p <- p + scale_x_continuous(limits = c(0,1))#,labels = function(x){ paste0(x, "%") }) 
p <- p + scale_y_continuous(breaks = c(0,.25,.5,.75,1),limits = c(-.08,1.08))
p2b <- p + theme(legend.position="none") + geom_ribbon(data=m3fit, aes(y=Estimate, ymin=lower, ymax=upper),alpha=.3) + 
  geom_line(data=m3fit, aes(y=Estimate))
#p1b <- p + annotate("text", x = 2, y = 1, label = "b",family="Gill Sans MT",size=10)
#p1 <- p1 + geom_segment(aes(x=50,xend=50,y=0,yend=.3),linetype=2) + geom_segment(aes(x=0,xend=50,y=.3,yend=.3),linetype=2)
#p1 <- p1 + geom_segment(aes(x=75,xend=75,y=0,yend=.49),linetype=2) + geom_segment(aes(x=0,xend=75,y=.49,yend=.49),linetype=2)
p2b
dev.off()

####################################
# Combine 2a and 2b into a single plot
####################################

theme_set(theme_grey())
png("~/Projects/Current/GenderTransition/Data/fig2.png", family =  "Gill Sans MT"    ,width = 16, height = 8, units = 'in', res = 1200)
plot_grid(p2a,p2b,labels = "auto",label_size = 30,ncol = 2,nrow = 1)
dev.off()

#################################################################################################
# Now run models and do analyses for Supplemental Material 
#################################################################################################

###########################################
# Null model - no predictors
############################################

# Set prior on intercept ------------------------
m0priors = c(prior(student_t(4,0, 10), class = "Intercept"))

# Fit model with intercept only
a1.m0.PCTSI <- brm_multiple(transition ~ 1,
                   family="bernoulli",
                   data=data3.short,
                   save_all_pars=TRUE,
                   prior=m0priors,
                   seed=1234)
plot(a1.m0.PCTSI)
pp_check(a1.m0.PCTSI) # looks good
save(a1.m0.PCTSI,file = "a1.m0.PCTSI.rda")

##########################################
# Covariate only model
##########################################

# Fit model with covariates using M1 priors
a1.m2.PCTSI <- brm_multiple(transition ~ sex.z + agemos.z + racecat.z + MonthsTest.z
                   + polid.z  + income.z,
                   family="bernoulli",
                   data=data3.short,
                   save_all_pars=TRUE,
                   prior=m1priors,
                   seed=1234)
plot(a1.m2.PCTSI)
pp_check(a1.m2.PCTSI) # looks good
#save(a1.m2.PCTSI,file = "a1.m2.PCTSI.rda")

##########################################
# Model comparison approach instead of focusing on estimates
##########################################

# Get values for comparisons
loo0 <- loo(a1.m0.PCTSI)
loo31 <- loo(a1.m1.PCTSI) # 31 for use in a later file
loo2 <- loo(a1.m2.PCTSI)
loo3 <- loo(a1.m3.PCTSI)

# Save
#save(loo0,file = "a1.loo0.PCTSI.rda")
#save(loo31,file = "a1.loo1.PCTSI.rda")
#save(loo2,file = "a1.loo2.PCTSI.rda")
#save(loo3,file = "a1.loo3.PCTSI.rda")

# See if influential data points
which(loo0$diagnostics$pareto_k > .7)
which(loo31$diagnostics$pareto_k > .7)
which(loo2$diagnostics$pareto_k > .7)
which(loo3$diagnostics$pareto_k > .7)

# Make a list of loo 
loo_list <- list(loo0,loo31,loo2,loo3)

# Loo
compare_ic(loo0,loo31,loo2,loo3)

# Pseudo-BMA
loo_model_weights(loo_list,method = "pseudobma")

# Stacking
loo_model_weights(loo_list,method = "stacking")

#########################################
# Fit M1 with different priors (both more and less informative)
#########################################

# Note: Not necessary to change priors on M0 because intercept prior is constant

# Set priors -------------------------
m1priors_5 = c(prior(student_t(4,0,10), class = "Intercept"), 
               prior(student_t(4,0,5),class="b")) # Wider

m1priors_1 = c(prior(student_t(4,0,10), class = "Intercept"), 
               prior(student_t(4,0,1),class="b")) # Narrower

# Re-fit m1
a1.m1.PCTSI_5 <- brm_multiple(transition ~ PCTSI.z,
                            family="bernoulli",
                            data=data3.short,
                            save_all_pars=TRUE,
                            prior=m1priors_5,
                            seed=1234)
plot(a1.m1.PCTSI_5)
pp_check(a1.m1.PCTSI_5) # looks good
#save(a1.m1.PCTSI_5,file = "a1.m1.PCTSI_5.rda")

a1.m1.PCTSI_1 <- brm_multiple(transition ~ PCTSI.z,
                              family="bernoulli",
                              data=data3.short,
                              save_all_pars=TRUE,
                              prior=m1priors_1,
                              seed=1234)
plot(a1.m1.PCTSI_1)
pp_check(a1.m1.PCTSI_1) # looks good
#save(a1.m1.PCTSI_1,file = "a1.m1.PCTSI_1.rda")

#########################################
# Fit M3 with different priors (both more and less informative)
#########################################

# Re-fit m3
a1.m3.PCTSI_5 <- brm_multiple(transition ~ PCTSI.z + sex.z + agemos.z + racecat.z + MonthsTest.z
                            + polid.z  + income.z,
                            family="bernoulli",
                            data=data3.short,
                            save_all_pars=TRUE,
                            prior=m1priors_5,
                            seed=1234)
plot(a1.m3.PCTSI_5)
pp_check(a1.m3.PCTSI_5)
#save(a1.m3.PCTSI_5,file = "a1.m3.PCTSI_5.rda")

a1.m3.PCTSI_1 <- brm_multiple(transition ~ PCTSI.z + sex.z + agemos.z + racecat.z + MonthsTest.z
                              + polid.z  + income.z,
                              family="bernoulli",
                              data=data3.short,
                              save_all_pars=TRUE,
                              prior=m1priors_1,
                              seed=1234)
plot(a1.m3.PCTSI_1)
pp_check(a1.m3.PCTSI_1)
#save(a1.m3.PCTSI_1,file = "a1.m3.PCTSI_1.rda")

###########################################
# Comparing M1 parameter estimates with those using original priors
###########################################

# m1_5
parameters <- a1.m1.PCTSI_5 %>%
  gather_draws(b_PCTSI.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios

# m1_1
parameters <- a1.m1.PCTSI_1 %>%
  gather_draws(b_PCTSI.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios

###########################################
# Comparing M3 parameter estimates with those using original priors
###########################################

# m3_5
parameters <- a1.m3.PCTSI_5 %>%
  gather_draws(b_PCTSI.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios 

# m3_1
parameters <- a1.m3.PCTSI_1 %>%
  gather_draws(b_PCTSI.z) %>%
  median_hdi()
print(cbind(parameters$.variable,round(parameters$.value,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.lower,2))) # Log odds scale
print(cbind(parameters$.variable,round(parameters$.upper,2))) # Log odds scale
print(cbind(parameters$.variable,round(exp(parameters$.value),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.lower),2))) # Odds ratios
print(cbind(parameters$.variable,round(exp(parameters$.upper),2))) # Odds ratios


