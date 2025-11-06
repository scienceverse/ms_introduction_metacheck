####################################
# File 9 of 15
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

# Read in data
load(file = "data2.short.rda")
load(file = "data2.noimpute.rda")

# Set up for multiple cores
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores()) # Run on multiple cores

#################################################################################################
# Run models and get relevant fits on imputed data sets
#################################################################################################

##########################################
# Run models 
##########################################

# One variable ------------------------------------------------------

# Set prior on intercept and on predictors  ------------------------
m1priors = c(prior(student_t(4,0,10), class = "Intercept"),
             prior(gamma(2.5,7.5),class="sd"))

# Fit model 
a3.m1.P <- brm_multiple(P.p.r ~ (1|group), 
                        family=Beta(),
                        data=data2.short,
                        prior=m1priors,
                        seed=1234, 
                        control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.P)
pp_check(a3.m1.P) # looks good
save(a3.m1.P,file = "a3.m1.P.rda")

# Fit model 
a3.m1.C <- brm_multiple(C.p.r ~ (1|group), 
                        family=Beta(),
                        data=data2.short,
                        prior=m1priors,
                        seed=1234, 
                        control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.C)
pp_check(a3.m1.C) # looks good
save(a3.m1.C,file = "a3.m1.C.rda")

# Fit model 
a3.m1.T <- brm_multiple(T.p.r ~ (1|group), 
                        family=Beta(),
                        data=data2.short,
                        prior=m1priors,
                        seed=1234, 
                        control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.T)
pp_check(a3.m1.T) # looks good
save(a3.m1.T,file = "a3.m1.T.rda")

# Fit model 
a3.m1.S <- brm_multiple(S.p.r ~ (1|group), 
                        family=Beta(),
                        data=data2.short,
                        prior=m1priors,
                        seed=1234, 
                        control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.S)
pp_check(a3.m1.S) # looks good
save(a3.m1.S,file = "a3.m1.S.rda")

# Fit model 
a3.m1.I <- brm_multiple(I.p.r ~ (1|group), 
                        family=Beta(),
                        data=data2.short,
                        prior=m1priors,
                        seed=1234, 
                        control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.I)
pp_check(a3.m1.I) # looks good
save(a3.m1.I,file = "a3.m1.I.rda")

# Two variables ------------------------------------------------------

# Fit model 
a3.m1.PC <- brm_multiple(PC.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PC)
pp_check(a3.m1.PC) # looks good
save(a3.m1.PC,file = "a3.m1.PC.rda")

# Fit model 
a3.m1.PT <- brm_multiple(PT.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PT)
pp_check(a3.m1.PT) # looks good
save(a3.m1.PT,file = "a3.m1.PT.rda")

# Fit model 
a3.m1.PS <- brm_multiple(PS.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PS)
pp_check(a3.m1.PS) # looks good
save(a3.m1.PS,file = "a3.m1.PS.rda")

# Fit model 
a3.m1.PI <- brm_multiple(PI.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PI)
pp_check(a3.m1.PI) # looks good
save(a3.m1.PI,file = "a3.m1.PI.rda")

# Fit model 
a3.m1.CT <- brm_multiple(CT.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CT)
pp_check(a3.m1.CT) # looks good
save(a3.m1.CT,file = "a3.m1.CT.rda")

# Fit model 
a3.m1.CS <- brm_multiple(CS.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CS)
pp_check(a3.m1.CS) # looks good
save(a3.m1.CS,file = "a3.m1.CS.rda")

# Fit model 
a3.m1.CI <- brm_multiple(CI.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CI)
pp_check(a3.m1.CI) # looks good
save(a3.m1.CI,file = "a3.m1.CI.rda")

# Fit model 
a3.m1.TS <- brm_multiple(TS.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TS)
pp_check(a3.m1.TS) # looks good
save(a3.m1.TS,file = "a3.m1.TS.rda")

# Fit model 
a3.m1.TI <- brm_multiple(TI.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TI)
pp_check(a3.m1.TI) # looks good
save(a3.m1.TI,file = "a3.m1.TI.rda")

# Fit model 
a3.m1.SI <- brm_multiple(SI.p.r ~ (1|group), 
                         family=Beta(),
                         data=data2.short,
                         prior=m1priors,
                         seed=1234, 
                         control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.SI)
pp_check(a3.m1.SI) # looks good
save(a3.m1.SI,file = "a3.m1.SI.rda")

# Three variables ------------------------------------------------------

# Fit model 
a3.m1.PCT <- brm_multiple(PCT.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCT)
pp_check(a3.m1.PCT) # looks good
save(a3.m1.PCT,file = "a3.m1.PCT.rda")

# Fit model 
a3.m1.PCS <- brm_multiple(PCS.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCS)
pp_check(a3.m1.PCS) # looks good
save(a3.m1.PCS,file = "a3.m1.PCS.rda")

# Fit model 
a3.m1.PCI <- brm_multiple(PCI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCI)
pp_check(a3.m1.PCI) # looks good
save(a3.m1.PCI,file = "a3.m1.PCI.rda")

# Fit model 
a3.m1.PTS <- brm_multiple(PTS.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTS)
pp_check(a3.m1.PTS) # looks good
save(a3.m1.PTS,file = "a3.m1.PTS.rda")

# Fit model 
a3.m1.PTI <- brm_multiple(PTI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTI)
pp_check(a3.m1.PTI) # looks good
save(a3.m1.PTI,file = "a3.m1.PTI.rda")

# Fit model 
a3.m1.PSI <- brm_multiple(PSI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PSI)
pp_check(a3.m1.PSI) # looks good
save(a3.m1.PSI,file = "a3.m1.PSI.rda")

# Fit model 
a3.m1.CTS <- brm_multiple(CTS.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTS)
pp_check(a3.m1.CTS) # looks good
save(a3.m1.CTS,file = "a3.m1.CTS.rda")

# Fit model 
a3.m1.CTI <- brm_multiple(CTI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTI)
pp_check(a3.m1.CTI) # looks good
save(a3.m1.CTI,file = "a3.m1.CTI.rda")

# Fit model 
a3.m1.CSI <- brm_multiple(CSI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CSI)
pp_check(a3.m1.CSI) # looks good
save(a3.m1.CSI,file = "a3.m1.CSI.rda")

# Fit model 
a3.m1.TSI <- brm_multiple(TSI.p.r ~ (1|group), 
                          family=Beta(),
                          data=data2.short,
                          prior=m1priors,
                          seed=1234, 
                          control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TSI)
pp_check(a3.m1.TSI) # looks good
save(a3.m1.TSI,file = "a3.m1.TSI.rda")

# Four variables ------------------------------------------------------

# Fit model 
a3.m1.PCTS <- brm_multiple(PCTS.p.r ~ (1|group), 
                           family=Beta(),
                           data=data2.short,
                           prior=m1priors,
                           seed=1234, 
                           control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTS)
pp_check(a3.m1.PCTS) # looks good
save(a3.m1.PCTS,file = "a3.m1.PCTS.rda")

# Fit model 
a3.m1.PCTI <- brm_multiple(PCTI.p.r ~ (1|group), 
                           family=Beta(),
                           data=data2.short,
                           prior=m1priors,
                           seed=1234, 
                           control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTI)
pp_check(a3.m1.PCTI) # looks good
save(a3.m1.PCTI,file = "a3.m1.PCTI.rda")

# Fit model 
a3.m1.PCSI <- brm_multiple(PCSI.p.r ~ (1|group), 
                           family=Beta(),
                           data=data2.short,
                           prior=m1priors,
                           seed=1234, 
                           control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCSI)
pp_check(a3.m1.PCSI) # looks good
save(a3.m1.PCSI,file = "a3.m1.PCSI.rda")

# Fit model 
a3.m1.PTSI <- brm_multiple(PTSI.p.r ~ (1|group), 
                           family=Beta(),
                           data=data2.short,
                           prior=m1priors,
                           seed=1234, 
                           control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTSI)
pp_check(a3.m1.PTSI) # looks good
save(a3.m1.PTSI,file = "a3.m1.PTSI.rda")

# Fit model 
a3.m1.CTSI <- brm_multiple(CTSI.p.r ~ (1|group), 
                           family=Beta(),
                           data=data2.short,
                           prior=m1priors,
                           seed=1234, 
                           control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTSI)
pp_check(a3.m1.CTSI) # looks good
save(a3.m1.CTSI,file = "a3.m1.CTSI.rda")

# Five variables -------------------------------------------------------

# Not necessary - original model had all 5 variables. Load existing
load("a3.m1.PCTSI.rda")

##########################################
# Get Loo
##########################################

# Get loo for comparisons
loo1 <- loo(a3.m1.P)
loo2 <- loo(a3.m1.C)
loo3 <- loo(a3.m1.T)
loo4 <- loo(a3.m1.S)
loo5 <- loo(a3.m1.I)

loo6 <- loo(a3.m1.PC)
loo7 <- loo(a3.m1.PT)
loo8 <- loo(a3.m1.PS)
loo9 <- loo(a3.m1.PI)
loo10 <- loo(a3.m1.CT)
loo11 <- loo(a3.m1.CS)
loo12 <- loo(a3.m1.CI)
loo13 <- loo(a3.m1.TS)
loo14 <- loo(a3.m1.TI)
loo15 <- loo(a3.m1.SI)

loo16 <- loo(a3.m1.PCT)
loo17 <- loo(a3.m1.PCS)
loo18 <- loo(a3.m1.PCI)
loo19 <- loo(a3.m1.PTS)
loo20 <- loo(a3.m1.PTI)
loo21 <- loo(a3.m1.PSI)
loo22 <- loo(a3.m1.CTS)
loo23 <- loo(a3.m1.CTI)
loo24 <- loo(a3.m1.CSI)
loo25 <- loo(a3.m1.TSI)

loo26 <- loo(a3.m1.PCTS)
loo27 <- loo(a3.m1.PCTI)
loo28 <- loo(a3.m1.PCSI)
loo29 <- loo(a3.m1.PTSI)
loo30 <- loo(a3.m1.CTSI)

load("a3.loo1.PCTSI.rda")

# Save
#save(loo1, file = "a3.loo1.P.rda")
#save(loo2, file = "a3.loo1.C.rda")
#save(loo3, file = "a3.loo1.T.rda")
#save(loo4, file = "a3.loo1.S.rda")
#save(loo5, file = "a3.loo1.I.rda")

#save(loo6, file= "a3.loo1.PC.rda")
#save(loo7, file= "a3.loo1.PT.rda")
#save(loo8, file = "a3.loo1.PS.rda")
#save(loo9, file = "a3.loo1.PI.rda")
#save(loo10, file = "a3.loo1.CT.rda")
#save(loo11, file = "a3.loo1.CS.rda")
#save(loo12, file = "a3.loo1.CI.rda")
#save(loo13, file = "a3.loo1.TS.rda")
#save(loo14, file = "a3.loo1.TI.rda")
#save(loo15, file = "a3.loo1.SI.rda")

#save(loo16, file = "a3.loo1.PCT.rda")
#save(loo17, file = "a3.loo1.PCS.rda")
#save(loo18, file = "a3.loo1.PCI.rda")
#save(loo19, file = "a3.loo1.PTS.rda")
#save(loo20, file = "a3.loo1.PTI.rda")
#save(loo21, file = "a3.loo1.PSI.rda")
#save(loo22, file = "a3.loo1.CTS.rda")
#save(loo23, file = "a3.loo1.CTI.rda")
#save(loo24, file = "a3.loo1.CSI.rda")
#save(loo25, file = "a3.loo1.TSI.rda")

#save(loo26, file = "a3.loo1.PCTS.rda")
#save(loo27, file = "a3.loo1.PCTI.rda")
#save(loo28, file = "a3.loo1.PCSI.rda")
#save(loo29, file = "a3.loo1.PTSI.rda")
#save(loo30, file = "a3.loo1.CTSI.rda")

# See if influential data points
which(loo1$diagnostics$pareto_k > .7)
which(loo2$diagnostics$pareto_k > .7)
which(loo3$diagnostics$pareto_k > .7)
which(loo4$diagnostics$pareto_k > .7)
which(loo5$diagnostics$pareto_k > .7)
which(loo6$diagnostics$pareto_k > .7)
which(loo7$diagnostics$pareto_k > .7)
which(loo8$diagnostics$pareto_k > .7)
which(loo9$diagnostics$pareto_k > .7)
which(loo10$diagnostics$pareto_k > .7)
which(loo11$diagnostics$pareto_k > .7)
which(loo12$diagnostics$pareto_k > .7)
which(loo13$diagnostics$pareto_k > .7)
which(loo14$diagnostics$pareto_k > .7)
which(loo15$diagnostics$pareto_k > .7)
which(loo16$diagnostics$pareto_k > .7)
which(loo17$diagnostics$pareto_k > .7)
which(loo18$diagnostics$pareto_k > .7)
which(loo19$diagnostics$pareto_k > .7)
which(loo20$diagnostics$pareto_k > .7)
which(loo21$diagnostics$pareto_k > .7)
which(loo22$diagnostics$pareto_k > .7)
which(loo23$diagnostics$pareto_k > .7)
which(loo24$diagnostics$pareto_k > .7)
which(loo25$diagnostics$pareto_k > .7)
which(loo26$diagnostics$pareto_k > .7)
which(loo27$diagnostics$pareto_k > .7)
which(loo28$diagnostics$pareto_k > .7)
which(loo29$diagnostics$pareto_k > .7)
which(loo30$diagnostics$pareto_k > .7)
which(loo31$diagnostics$pareto_k > .7)

# Note: No flagged data points

#################################################################################################
# Run models and get relevant fits on data sets that didn't use multiple imputation
#################################################################################################

##########################################
# Run models 
##########################################

# One variable ------------------------------------------------------

# Set prior on intercept and on predictors  ------------------------
m1priors = c(prior(student_t(4,0,10), class = "Intercept"),
             prior(gamma(2.5,7.5),class="sd"))

# Fit model 
a3.m1.P.ni <- brm(P.p.r ~ (1|group), 
                  family=Beta(),
                  data=data2,
                  prior=m1priors,
                  seed=1234, 
                  control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.P.ni)
pp_check(a3.m1.P.ni) # looks good
save(a3.m1.P.ni,file = "a3.m1.P.ni.rda")

# Fit model 
a3.m1.C.ni <- brm(C.p.r ~ (1|group), 
                  family=Beta(),
                  data=data2,
                  prior=m1priors,
                  seed=1234, 
                  control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.C.ni)
pp_check(a3.m1.C.ni) # looks good
save(a3.m1.C.ni,file = "a3.m1.C.ni.rda")

# Fit model 
a3.m1.T.ni <- brm(T.p.r ~ (1|group), 
                  family=Beta(),
                  data=data2,
                  prior=m1priors,
                  seed=1234, 
                  control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.T.ni)
pp_check(a3.m1.T.ni) # looks good
save(a3.m1.T.ni,file = "a3.m1.T.ni.rda")

# Fit model 
a3.m1.S.ni <- brm(S.p.r ~ (1|group), 
                  family=Beta(),
                  data=data2,
                  prior=m1priors,
                  seed=1234, 
                  control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.S.ni)
pp_check(a3.m1.S.ni) # looks good
save(a3.m1.S.ni,file = "a3.m1.S.ni.rda")

# Fit model 
a3.m1.I.ni <- brm(I.p.r ~ (1|group), 
                  family=Beta(),
                  data=data2,
                  prior=m1priors,
                  seed=1234, 
                  control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.I.ni)
pp_check(a3.m1.I.ni) # looks good
save(a3.m1.I.ni,file = "a3.m1.I.ni.rda")

# Two variables ------------------------------------------------------

# Fit model 
a3.m1.PC.ni <- brm(PC.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PC.ni)
pp_check(a3.m1.PC.ni) # looks good
save(a3.m1.PC.ni,file = "a3.m1.PC.ni.rda")

# Fit model 
a3.m1.PT.ni <- brm(PT.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PT.ni)
pp_check(a3.m1.PT.ni) # looks good
save(a3.m1.PT.ni,file = "a3.m1.PT.ni.rda")

# Fit model 
a3.m1.PS.ni <- brm(PS.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PS.ni)
pp_check(a3.m1.PS.ni) # looks good
save(a3.m1.PS.ni,file = "a3.m1.PS.ni.rda")

# Fit model 
a3.m1.PI.ni <- brm(PI.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PI.ni)
pp_check(a3.m1.PI.ni) # looks good
save(a3.m1.PI.ni,file = "a3.m1.PI.ni.rda")

# Fit model 
a3.m1.CT.ni <- brm(CT.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CT.ni)
pp_check(a3.m1.CT.ni) # looks good
save(a3.m1.CT.ni,file = "a3.m1.CT.ni.rda")

# Fit model 
a3.m1.CS.ni <- brm(CS.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CS.ni)
pp_check(a3.m1.CS.ni) # looks good
save(a3.m1.CS.ni,file = "a3.m1.CS.ni.rda")

# Fit model 
a3.m1.CI.ni <- brm(CI.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CI.ni)
pp_check(a3.m1.CI.ni) # looks good
save(a3.m1.CI.ni,file = "a3.m1.CI.ni.rda")

# Fit model 
a3.m1.TS.ni <- brm(TS.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TS.ni)
pp_check(a3.m1.TS.ni) # looks good
save(a3.m1.TS.ni,file = "a3.m1.TS.ni.rda")

# Fit model 
a3.m1.TI.ni <- brm(TI.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TI.ni)
pp_check(a3.m1.TI.ni) # looks good
save(a3.m1.TI.ni,file = "a3.m1.TI.ni.rda")

# Fit model 
a3.m1.SI.ni <- brm(SI.p.r ~ (1|group), 
                   family=Beta(),
                   data=data2,
                   prior=m1priors,
                   seed=1234, 
                   control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.SI.ni)
pp_check(a3.m1.SI.ni) # looks good
save(a3.m1.SI.ni,file = "a3.m1.SI.ni.rda")

# Three variables ------------------------------------------------------

# Fit model 
a3.m1.PCT.ni <- brm(PCT.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCT.ni)
pp_check(a3.m1.PCT.ni) # looks good
save(a3.m1.PCT.ni,file = "a3.m1.PCT.ni.rda")

# Fit model 
a3.m1.PCS.ni <- brm(PCS.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCS.ni)
pp_check(a3.m1.PCS.ni) # looks good
save(a3.m1.PCS.ni,file = "a3.m1.PCS.ni.rda")

# Fit model 
a3.m1.PCI.ni <- brm(PCI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCI.ni)
pp_check(a3.m1.PCI.ni) # looks good
save(a3.m1.PCI.ni,file = "a3.m1.PCI.ni.rda")

# Fit model 
a3.m1.PTS.ni <- brm(PTS.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTS.ni)
pp_check(a3.m1.PTS.ni) # looks good
save(a3.m1.PTS.ni,file = "a3.m1.PTS.ni.rda")

# Fit model 
a3.m1.PTI.ni <- brm(PTI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTI.ni)
pp_check(a3.m1.PTI.ni) # looks good
save(a3.m1.PTI.ni,file = "a3.m1.PTI.ni.rda")

# Fit model 
a3.m1.PSI.ni <- brm(PSI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PSI.ni)
pp_check(a3.m1.PSI.ni) # looks good
save(a3.m1.PSI.ni,file = "a3.m1.PSI.ni.rda")

# Fit model 
a3.m1.CTS.ni <- brm(CTS.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTS.ni)
pp_check(a3.m1.CTS.ni) # looks good
save(a3.m1.CTS.ni,file = "a3.m1.CTS.ni.rda")

# Fit model 
a3.m1.CTI.ni <- brm(CTI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTI.ni)
pp_check(a3.m1.CTI.ni) # looks good
save(a3.m1.CTI.ni,file = "a3.m1.CTI.ni.rda")

# Fit model 
a3.m1.CSI.ni <- brm(CSI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CSI.ni)
pp_check(a3.m1.CSI.ni) # looks good
save(a3.m1.CSI.ni,file = "a3.m1.CSI.ni.rda")

# Fit model 
a3.m1.TSI.ni <- brm(TSI.p.r ~ (1|group), 
                    family=Beta(),
                    data=data2,
                    prior=m1priors,
                    seed=1234, 
                    control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.TSI.ni)
pp_check(a3.m1.TSI.ni) # looks good
save(a3.m1.TSI.ni,file = "a3.m1.TSI.ni.rda")

# Four variables ------------------------------------------------------

# Fit model 
a3.m1.PCTS.ni <- brm(PCTS.p.r ~ (1|group), 
                     family=Beta(),
                     data=data2,
                     prior=m1priors,
                     seed=1234, 
                     control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTS.ni)
pp_check(a3.m1.PCTS.ni) # looks good
save(a3.m1.PCTS.ni,file = "a3.m1.PCTS.ni.rda")

# Fit model 
a3.m1.PCTI.ni <- brm(PCTI.p.r ~ (1|group), 
                     family=Beta(),
                     data=data2,
                     prior=m1priors,
                     seed=1234, 
                     control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTI.ni)
pp_check(a3.m1.PCTI.ni) # looks good
save(a3.m1.PCTI.ni,file = "a3.m1.PCTI.ni.rda")

# Fit model 
a3.m1.PCSI.ni <- brm(PCSI.p.r ~ (1|group), 
                     family=Beta(),
                     data=data2,
                     prior=m1priors,
                     seed=1234, 
                     control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCSI.ni)
pp_check(a3.m1.PCSI.ni) # looks good
save(a3.m1.PCSI.ni,file = "a3.m1.PCSI.ni.rda")

# Fit model 
a3.m1.PTSI.ni <- brm(PTSI.p.r ~ (1|group), 
                     family=Beta(),
                     data=data2,
                     prior=m1priors,
                     seed=1234, 
                     control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PTSI.ni)
pp_check(a3.m1.PTSI.ni) # looks good
save(a3.m1.PTSI.ni,file = "a3.m1.PTSI.ni.rda")

# Fit model 
a3.m1.CTSI.ni <- brm(CTSI.p.r ~ (1|group), 
                     family=Beta(),
                     data=data2,
                     prior=m1priors,
                     seed=1234, 
                     control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.CTSI.ni)
pp_check(a3.m1.CTSI.ni) # looks good
save(a3.m1.CTSI.ni,file = "a3.m1.CTSI.ni.rda")

# Five variables -------------------------------------------------------

a3.m1.PCTSI.ni <- brm(PCTSI.p.r ~ (1|group), 
                      family=Beta(),
                      data=data2,
                      prior=m1priors,
                      seed=1234, 
                      control=list(adapt_delta=.999,max_treedepth = 15))
plot(a3.m1.PCTSI.ni)
pp_check(a3.m1.PCTSI.ni) # looks good
save(a3.m1.PCTSI.ni,file = "a3.m1.PCTSI.ni.rda")

##########################################
# Get Loo
##########################################

# Get loo for comparisons
loo1.ni <- loo(a3.m1.P.ni)
loo2.ni <- loo(a3.m1.C.ni)
loo3.ni <- loo(a3.m1.T.ni)
loo4.ni <- loo(a3.m1.S.ni)
loo5.ni <- loo(a3.m1.I.ni)

loo6.ni <- loo(a3.m1.PC.ni)
loo7.ni <- loo(a3.m1.PT.ni)
loo8.ni <- loo(a3.m1.PS.ni)
loo9.ni <- loo(a3.m1.PI.ni)
loo10.ni <- loo(a3.m1.CT.ni)
loo11.ni <- loo(a3.m1.CS.ni)
loo12.ni <- loo(a3.m1.CI.ni)
loo13.ni <- loo(a3.m1.TS.ni)
loo14.ni <- loo(a3.m1.TI.ni)
loo15.ni <- loo(a3.m1.SI.ni)

loo16.ni <- loo(a3.m1.PCT.ni)
loo17.ni <- loo(a3.m1.PCS.ni)
loo18.ni <- loo(a3.m1.PCI.ni)
loo19.ni <- loo(a3.m1.PTS.ni)
loo20.ni <- loo(a3.m1.PTI.ni)
loo21.ni <- loo(a3.m1.PSI.ni)
loo22.ni <- loo(a3.m1.CTS.ni)
loo23.ni <- loo(a3.m1.CTI.ni)
loo24.ni <- loo(a3.m1.CSI.ni)
loo25.ni <- loo(a3.m1.TSI.ni)

loo26.ni <- loo(a3.m1.PCTS.ni)
loo27.ni <- loo(a3.m1.PCTI.ni)
loo28.ni <- loo(a3.m1.PCSI.ni)
loo29.ni <- loo(a3.m1.PTSI.ni)
loo30.ni <- loo(a3.m1.CTSI.ni)

loo31.ni <- loo(a3.m1.PCTSI.ni)

# Save
save(loo1.ni, file = "a3.loo1.P.ni.rda")
save(loo2.ni, file = "a3.loo1.C.ni.rda")
save(loo3.ni, file = "a3.loo1.T.ni.rda")
save(loo4.ni, file = "a3.loo1.S.ni.rda")
save(loo5.ni, file = "a3.loo1.I.ni.rda")

save(loo6.ni, file= "a3.loo1.PC.ni.rda")
save(loo7.ni, file= "a3.loo1.PT.ni.rda")
save(loo8.ni, file = "a3.loo1.PS.ni.rda")
save(loo9.ni, file = "a3.loo1.PI.ni.rda")
save(loo10.ni, file = "a3.loo1.CT.ni.rda")
save(loo11.ni, file = "a3.loo1.CS.ni.rda")
save(loo12.ni, file = "a3.loo1.CI.ni.rda")
save(loo13.ni, file = "a3.loo1.TS.ni.rda")
save(loo14.ni, file = "a3.loo1.TI.ni.rda")
save(loo15.ni, file = "a3.loo1.SI.ni.rda")

save(loo16.ni, file = "a3.loo1.PCT.ni.rda")
save(loo17.ni, file = "a3.loo1.PCS.ni.rda")
save(loo18.ni, file = "a3.loo1.PCI.ni.rda")
save(loo19.ni, file = "a3.loo1.PTS.ni.rda")
save(loo20.ni, file = "a3.loo1.PTI.ni.rda")
save(loo21.ni, file = "a3.loo1.PSI.ni.rda")
save(loo22.ni, file = "a3.loo1.CTS.ni.rda")
save(loo23.ni, file = "a3.loo1.CTI.ni.rda")
save(loo24.ni, file = "a3.loo1.CSI.ni.rda")
save(loo25.ni, file = "a3.loo1.TSI.ni.rda")

save(loo26.ni, file = "a3.loo1.PCTS.ni.rda")
save(loo27.ni, file = "a3.loo1.PCTI.ni.rda")
save(loo28.ni, file = "a3.loo1.PCSI.ni.rda")
save(loo29.ni, file = "a3.loo1.PTSI.ni.rda")
save(loo30.ni, file = "a3.loo1.CTSI.ni.rda")

save(loo31.ni, file = "a3.loo1.PCTSI.ni.rda")

# See if influential data points
which(loo1.ni$diagnostics$pareto_k > .7)
which(loo2.ni$diagnostics$pareto_k > .7)
which(loo3.ni$diagnostics$pareto_k > .7)
which(loo4.ni$diagnostics$pareto_k > .7)
which(loo5.ni$diagnostics$pareto_k > .7)
which(loo6.ni$diagnostics$pareto_k > .7)
which(loo7.ni$diagnostics$pareto_k > .7)
which(loo8.ni$diagnostics$pareto_k > .7)
which(loo9.ni$diagnostics$pareto_k > .7)
which(loo10.ni$diagnostics$pareto_k > .7)
which(loo11.ni$diagnostics$pareto_k > .7)
which(loo12.ni$diagnostics$pareto_k > .7)
which(loo13.ni$diagnostics$pareto_k > .7)
which(loo14.ni$diagnostics$pareto_k > .7)
which(loo15.ni$diagnostics$pareto_k > .7)
which(loo16.ni$diagnostics$pareto_k > .7)
which(loo17.ni$diagnostics$pareto_k > .7)
which(loo18.ni$diagnostics$pareto_k > .7)
which(loo19.ni$diagnostics$pareto_k > .7)
which(loo20.ni$diagnostics$pareto_k > .7)
which(loo21.ni$diagnostics$pareto_k > .7)
which(loo22.ni$diagnostics$pareto_k > .7)
which(loo23.ni$diagnostics$pareto_k > .7)
which(loo24.ni$diagnostics$pareto_k > .7)
which(loo25.ni$diagnostics$pareto_k > .7)
which(loo26.ni$diagnostics$pareto_k > .7)
which(loo27.ni$diagnostics$pareto_k > .7)
which(loo28.ni$diagnostics$pareto_k > .7)
which(loo29.ni$diagnostics$pareto_k > .7)
which(loo30.ni$diagnostics$pareto_k > .7)
which(loo31.ni$diagnostics$pareto_k > .7)
# Note: No flagged data points
