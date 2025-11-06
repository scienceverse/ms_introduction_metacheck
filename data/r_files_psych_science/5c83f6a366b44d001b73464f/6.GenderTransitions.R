####################################
# File 5 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: October 24 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
rm(list=ls())
library(mice)
library(miceadds)
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
library(pwr)
library(HDInterval)
library(loo)
library(rstan)

# Set working directory
setwd("")

# Read in data
load(file = "data2.short.rda")

# Set up for multiple cores
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores()) # Run on multiple cores

#################################################################################################
# First, run models, get estimates, and make plots for models presented in-text
#################################################################################################

##########################################
# Group as predictor for unadjusted effects
##########################################

# Set prior -------------------------
m1priors = c(prior(student_t(4,0,10), class = "Intercept"),
             prior(gamma(2.5,7.5),class="sd"))

# Fit null model with only group as predictor
a3.m1.PCTSI <- brm_multiple(PCTSI.p.r ~ (1|group), 
                            family=Beta(),
                            data=data2.short,
                            prior=m1priors,
                            seed=1234, 
                            control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTSI)
pp_check(a3.m1.PCTSI) # looks good
#save(a3.m1.PCTSI,file = "a3.m1.PCTSI.rda")

##########################################
# Add coviarates for adjusted effects
##########################################

# Set prior -------------------------
m3priors = c(prior(student_t(4,0,10), class = "Intercept"),
             prior(gamma(2.5,7.5),class="sd"),
             prior(student_t(4, 0, 2.5), class = "b"))

# Fit null model with group and covariates as predictors
a3.m3.PCTSI <- brm_multiple(PCTSI.p.r ~ (1|group) + sex.z + agemos.z + racecat.z + 
                              + polid.z  + income.z,
                            family=Beta(),
                            data=data2.short,
                            prior=m3priors,
                            seed=1234,
                            control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m3.PCTSI)
pp_check(a3.m3.PCTSI) # looks good
#save(a3.m3.PCTSI,file = "a3.m3.PCTSI.rda")

###########################################
# Estimates to report
###########################################

# M1 --------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m1.PCTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 

# M3 ---------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m3.PCTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 

#################################################################################################
# Now run models and do analyses for Supplemental Material 
#################################################################################################

###########################################
# Null model - no predictors
############################################

# Set prior -------------------------
m0priors = c(prior(student_t(4,0,10), class = "Intercept"))

# Fit null model with only group as predictor
a3.m0.PCTSI <- brm_multiple(PCTSI.p.r ~ 1, 
                            family=Beta(),
                            data=data2.short,
                            prior=m0priors,
                            seed=1234, 
                            control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m0.PCTSI)
pp_check(a3.m0.PCTSI) # looks good
#save(a3.m0.PCTSI,file = "a3.m0.PCTSI.rda")

##########################################
# Covariate only model
##########################################

# Set prior -------------------------
m2priors = c(prior(student_t(4,0,10), class = "Intercept"),
             prior(student_t(4, 0, 2.5), class = "b"))

# Fit null model with group and covariates as predictors
a3.m2.PCTSI <- brm_multiple(PCTSI.p.r ~ sex.z + agemos.z + racecat.z + 
                              + polid.z  + income.z,
                            family=Beta(),
                            data=data2.short,
                            prior=m2priors,
                            seed=1234,
                            control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m2.PCTSI)
pp_check(a3.m2.PCTSI) # looks good
#save(a3.m2.PCTSI,file = "a3.m2.PCTSI.rda")

##########################################
# Model comparison approach instead of focusing on estimates
##########################################

# Get values for comparisons
loo0 <- loo(a3.m0.PCTSI)
loo31 <- loo(a3.m1.PCTSI) # This is numbered 31 for use in a later file
loo2 <- loo(a3.m2.PCTSI)
loo3 <- loo(a3.m3.PCTSI, reloo = TRUE) # Removing 1 observation

# Save
#save(loo0,file = "a3.loo0.PCTSI.rda")
#save(loo31,file = "a3.loo1.PCTSI.rda")
#save(loo2,file = "a3.loo2.PCTSI.rda")
#save(loo3,file = "a3.loo3.PCTSI.rda")

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
m1priors_10 = c(prior(student_t(4,0,10), class = "Intercept"), 
                prior(gamma(2.5,10),class="sd")) # Narrower

m1priors_5 = c(prior(student_t(4,0,10), class = "Intercept"), 
               prior(gamma(2.5,5),class="sd")) # Wider

# Re-fit m1
a3.m1.PCTSI_10 <- brm_multiple(PCTSI.p.r ~ (1|group), 
                               family=Beta(),
                               data=data2.short,
                               prior=m1priors_10,
                               seed=1234, 
                               control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTSI_10)
pp_check(a3.m1.PCTSI_10) # looks good
save(a3.m1.PCTSI_10,file = "a3.m1.PCTSI_10.rda")

a3.m1.PCTSI_5 <- brm_multiple(PCTSI.p.r ~ (1|group), 
                              family=Beta(),
                              data=data2.short,
                              prior=m1priors_5,
                              seed=1234, 
                              control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTSI_5)
pp_check(a3.m1.PCTSI_5) # looks good
save(a3.m1.PCTSI_5,file = "a3.m1.PCTSI_5.rda")

#########################################
# Fit M3 with different priors (both more and less informative)
#########################################

# Set priors -------------------------
m3priors_10 = c(prior(student_t(4,0,10), class = "Intercept"), 
                prior(student_t(4, 0, 1), class = "b"),
                prior(gamma(2.5,10),class="sd")) # Narrower

m3priors_5 = c(prior(student_t(4,0,10), class = "Intercept"), 
               prior(student_t(4, 0, 5), class = "b"),
               prior(gamma(2.5,5),class="sd")) # Wider

# Refit m3
a3.m3.PCTSI_10 <- brm_multiple(PCTSI.p.r ~ (1|group) + sex.z + agemos.z + racecat.z + 
                                 + polid.z  + income.z,
                               family=Beta(),
                               data=data2.short,
                               prior=m3priors_10,
                               seed=1234,
                               control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m3.PCTSI_10)
pp_check(a3.m3.PCTSI_10) # looks good
save(a3.m3.PCTSI_10,file = "a3.m3.PCTSI_10.rda")

a3.m3.PCTSI_5 <- brm_multiple(PCTSI.p.r ~ (1|group) + sex.z + agemos.z + racecat.z + 
                                + polid.z  + income.z,
                              family=Beta(),
                              data=data2.short,
                              prior=m3priors_5,
                              seed=1234,
                              control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m3.PCTSI_5)
pp_check(a3.m3.PCTSI_5) # looks good
save(a3.m3.PCTSI_5,file = "a3.m3.PCTSI_5.rda")

###########################################
# Comparing M1 parameter estimates with those using original priors
###########################################

# 10 --------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m1.PCTSI_10))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC)

# 5 --------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m1.PCTSI_5))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC)

###########################################
# Comparing M3 parameter estimates with those using original priors
###########################################

# 10 --------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m3.PCTSI_10))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC)

# 5 --------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m3.PCTSI_5))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# What is the median and hdi
round(median_hdi(ControlTrans)[1:3],2)
round(median_hdi(ControlGNC)[1:3],2)
round(median_hdi(TransGNC)[1:3],2)

# Proportion of the posterior in the ROPE
length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC)

##############################################
# Proportion of Posterior in ROPE as function of effect size width 
##############################################

# M1 --------------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m1.PCTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# Set effect size width 
EF <- seq(from = 0,to = -1, by = -0.001)

# Control - Trans
ROPEControlTrans <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(ControlTrans[ControlTrans > i & ControlTrans < abs(i)]))/length(ControlTrans),"\n")
}))
ROPEControlTransdat <- cbind.data.frame(EF,ROPEControlTrans)
ROPEControlTransdat$EF <- -1*ROPEControlTransdat$EF
png("ROPEControlTrans.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPEControlTransdat,aes(y=ROPEControlTrans,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Control - Transgender") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

# Control - GNC
ROPEControlGNC <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(ControlGNC[ControlGNC > i & ControlGNC < abs(i)]))/length(ControlGNC),"\n")
}))
ROPEControlGNCdat <- cbind.data.frame(EF,ROPEControlGNC)
ROPEControlGNCdat$EF <- -1*ROPEControlGNCdat$EF
png("ROPEControlGNC.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPEControlGNCdat,aes(y=ROPEControlGNC,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Control - Future Transitioners") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

# Trans - GNC
ROPETransGNC <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(TransGNC[TransGNC > i & TransGNC < abs(i)]))/length(TransGNC),"\n")
}))
ROPETransGNCdat <- cbind.data.frame(EF,ROPETransGNC)
ROPETransGNCdat$EF <- -1*ROPETransGNCdat$EF
png("ROPETransGNC.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPETransGNCdat,aes(y=ROPETransGNC,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Transgender - Future Transitioners") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

# M3 --------------------------------------------

# Get posterior samples
post <- data.frame(posterior_samples(a3.m3.PCTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)

# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)

# Set effect size width 
EF <- seq(from = 0,to = -1, by = -0.001)

# Control - Trans
ROPEControlTrans <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(ControlTrans[ControlTrans > i & ControlTrans < abs(i)]))/length(ControlTrans),"\n")
}))
ROPEControlTransdat <- cbind.data.frame(EF,ROPEControlTrans)
ROPEControlTransdat$EF <- -1*ROPEControlTransdat$EF
png("ROPEControlTransCovariates.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPEControlTransdat,aes(y=ROPEControlTrans,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Control - Transgender") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

# Control - GNC
ROPEControlGNC <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(ControlGNC[ControlGNC > i & ControlGNC < abs(i)]))/length(ControlGNC),"\n")
}))
ROPEControlGNCdat <- cbind.data.frame(EF,ROPEControlGNC)
ROPEControlGNCdat$EF <- -1*ROPEControlGNCdat$EF
png("ROPEControlGNCCovariates.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPEControlGNCdat,aes(y=ROPEControlGNC,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Control - Future Transitioners") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

# Trans - GNC
ROPETransGNC <- as.numeric(capture.output(for(i in seq(from = 0,to = -1, by = -0.001)){
  cat((length(TransGNC[TransGNC > i & TransGNC < abs(i)]))/length(TransGNC),"\n")
}))
ROPETransGNCdat <- cbind.data.frame(EF,ROPETransGNC)
ROPETransGNCdat$EF <- -1*ROPETransGNCdat$EF
png("ROPETransGNCCovariates.png", family =  "Gill Sans MT"    ,width = 8, height = 8, units = 'in', res = 1200)
p <- ggplot(data=ROPETransGNCdat,aes(y=ROPETransGNC,x=EF)) + geom_point(color="darkblue") 
p <- p + scale_y_continuous(name = expression(Proportion~of~Posterior~inside~ROPE),breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) + scale_x_continuous(name = my_x_title,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) 
p <- p + theme(text=element_text(size=22))+ labs(title="Transgender - Future Transitioners") + theme(plot.title = element_text(hjust=.5))
p
dev.off()

