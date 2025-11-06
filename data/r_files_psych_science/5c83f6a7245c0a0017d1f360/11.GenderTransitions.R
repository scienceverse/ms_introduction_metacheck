####################################
# File 11 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: September 14 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
rm(list=ls())
library(ggplot2)
library(extrafont)
library(viridis)
library(tidybayes)
library(dplyr)
library(brms)
library(arm)
library(pwr)

# Set working directory
setwd("")

#################################################################################################
# Load data from two different types of models: those that analyzed imputed data and those that didn't
#################################################################################################

###########################
# Models on imputed data
###########################

# One variable ----------------------------------
load("a2.m1.P.rda")
load("a2.m1.C.rda")
load("a2.m1.T.rda")
load("a2.m1.S.rda")
load("a2.m1.I.rda")

# Two variables ----------------------------------
load("a2.m1.PC.rda")
load("a2.m1.PT.rda")
load("a2.m1.PS.rda")
load("a2.m1.PI.rda")
load("a2.m1.CT.rda")
load("a2.m1.CS.rda")
load("a2.m1.CI.rda")
load("a2.m1.TS.rda")
load("a2.m1.TI.rda")
load("a2.m1.SI.rda")

# Three variables ----------------------------------
load("a2.m1.PCT.rda")
load("a2.m1.PCS.rda")
load("a2.m1.PCI.rda")
load("a2.m1.PTS.rda")
load("a2.m1.PTI.rda")
load("a2.m1.PSI.rda")
load("a2.m1.CTS.rda")
load("a2.m1.CTI.rda")
load("a2.m1.CSI.rda")
load("a2.m1.TSI.rda")

# Four variables ----------------------------------
load("a2.m1.PCTS.rda")
load("a2.m1.PCTI.rda")
load("a2.m1.PCSI.rda")
load("a2.m1.PTSI.rda")
load("a2.m1.CTSI.rda")

# Five variables ----------------------------------
load("a2.m1.PCTSI.rda")

###########################
# Models on data that ignored missingness
###########################

# One variable ----------------------------------
load("a2.m1.P.ni.rda")
load("a2.m1.C.ni.rda")
load("a2.m1.T.ni.rda")
load("a2.m1.S.ni.rda")
load("a2.m1.I.ni.rda")

# Two variables ----------------------------------
load("a2.m1.PC.ni.rda")
load("a2.m1.PT.ni.rda")
load("a2.m1.PS.ni.rda")
load("a2.m1.PI.ni.rda")
load("a2.m1.CT.ni.rda")
load("a2.m1.CS.ni.rda")
load("a2.m1.CI.ni.rda")
load("a2.m1.TS.ni.rda")
load("a2.m1.TI.ni.rda")
load("a2.m1.SI.ni.rda")

# Three variables ----------------------------------
load("a2.m1.PCT.ni.rda")
load("a2.m1.PCS.ni.rda")
load("a2.m1.PCI.ni.rda")
load("a2.m1.PTS.ni.rda")
load("a2.m1.PTI.ni.rda")
load("a2.m1.PSI.ni.rda")
load("a2.m1.CTS.ni.rda")
load("a2.m1.CTI.ni.rda")
load("a2.m1.CSI.ni.rda")
load("a2.m1.TSI.ni.rda")

# Four variables ----------------------------------
load("a2.m1.PCTS.ni.rda")
load("a2.m1.PCTI.ni.rda")
load("a2.m1.PCSI.ni.rda")
load("a2.m1.PTSI.ni.rda")
load("a2.m1.CTSI.ni.rda")

# Five variables ----------------------------------
load("a2.m1.PCTSI.ni.rda")

#################################################################################################
# First, run models, get estimates, and make plots for models presented in-text
#################################################################################################

######################
# Set up data frame to hold estimates
#####################

# Make data set
plot.data <- data.frame(matrix(NA,nrow = 186,ncol=7))

# Give names
names(plot.data) <- c("DV","Missing","Comparison","Proportion","Estimate","Lower","Upper") # Change these later

# Assign levels of outcome variable
plot.data$DV <- rep(c("P","C","T","S","I",
                      "PC","PT","PS","PI","CT","CS","CI","TS","TI","SI",
                      "PCT","PCS","PCI","PTS","PTI","PSI","CTS","CTI","CSI","TSI",
                      "PCTS","PCTI","PCSI","PTSI","CTSI",
                      "PCTSI"),6)
plot.data$Number <- nchar(plot.data$DV)
plot.data$Number <- ifelse(plot.data$Number==1,"One Variable",
                           ifelse(plot.data$Number==2,"Two Variables",
                                  ifelse(plot.data$Number==3,"Three Variables","Four or Five Variables")))
plot.data$Number <- factor(plot.data$Number, ordered = TRUE, 
                           levels = c("One Variable",
                                      "Two Variables",
                                      "Three Variables",
                                      "Four or Five Variables"))
plot.data$DV <- factor(plot.data$DV, ordered = TRUE, 
                       levels = c("P","C","T","S","I",
                                  "PC","PT","PS","PI","CT","CS","CI","TS","TI","SI",
                                  "PCT","PCS","PCI","PTS","PTI","PSI","CTS","CTI","CSI","TSI",
                                  "PCTS","PCTI","PCSI","PTSI","CTSI",
                                  "PCTSI"))
plot.data$Missing[1:93] <- "Multiple Imputation"
plot.data$Missing[94:186] <- "Ignore Missingness"
plot.data$Comparison[1:31] <- "Control-Transgender"
plot.data$Comparison[32:62] <- "Control-GNC"
plot.data$Comparison[63:93] <- "Transgender-GNC"
plot.data$Comparison[94:124] <- "Control-Transgender"
plot.data$Comparison[125:155] <- "Control-GNC"
plot.data$Comparison[156:186] <- "Transgender-GNC"

########################################
# Insert proportions for multiple imputation data
########################################

# One variable ----------------------------------

# P - get posterior samples
post <- data.frame(posterior_samples(a2.m1.P))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# C - get posterior samples
post <- data.frame(posterior_samples(a2.m1.C))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# T - get posterior samples
post <- data.frame(posterior_samples(a2.m1.T))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# S - get posterior samples
post <- data.frame(posterior_samples(a2.m1.S))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# I - get posterior samples
post <- data.frame(posterior_samples(a2.m1.I))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Two variables ----------------------------------

# PC - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PC))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PT))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CT))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# P - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# TI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# SI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.SI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Three variables ----------------------------------

# PCT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCT))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# TSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Four variables ----------------------------------

# PCTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTS))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Five variables ----------------------------------

# PCTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTSI))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

########################################
# Insert proportions for data that ignores missingness
########################################

# One variable ----------------------------------

# P - get posterior samples
post <- data.frame(posterior_samples(a2.m1.P.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# C - get posterior samples
post <- data.frame(posterior_samples(a2.m1.C.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# T - get posterior samples
post <- data.frame(posterior_samples(a2.m1.T.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# S - get posterior samples
post <- data.frame(posterior_samples(a2.m1.S.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# I - get posterior samples
post <- data.frame(posterior_samples(a2.m1.I.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Two variables ----------------------------------

# PC - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PC.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PT.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CT.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# P - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# TI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# SI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.SI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Three variables ----------------------------------

# PCT - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCT.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# TSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.TSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Four variables ----------------------------------

# PCTS - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTS.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCTI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PCSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# PTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PTSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# CTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.CTSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

# Five variables ----------------------------------

# PCTSI - get posterior samples
post <- data.frame(posterior_samples(a2.m1.PCTSI.ni))
post$Control<- invlogit(post$b_Intercept + post$`r_group.Control.Intercept.`)
post$GNC <- invlogit(post$b_Intercept + post$`r_group.Gender.Nonconforming.Intercept.`)
post$Trans <- invlogit(post$b_Intercept + post$`r_group.Transgender.Intercept.`)
# Calculate effect size
TransGNC <- ES.h(post$Trans,post$GNC)
ControlGNC <- ES.h(post$Control,post$GNC)
ControlTrans <- ES.h(post$Control,post$Trans)
# Proportion of the posterior in the ROPE
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- length(ControlTrans[ControlTrans >= -.2 & ControlTrans <= .2])/length(ControlTrans)
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- length(ControlGNC[ControlGNC >= -.2 & ControlGNC <= .2])/length(ControlGNC)
plot.data$Proportion[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- length(TransGNC[TransGNC >= -.2 & TransGNC <= .2])/length(TransGNC) 
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-Transgender"] <- as.numeric(median_hdi(ControlTrans)[3])
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Control-GNC"] <- as.numeric(median_hdi(ControlGNC)[3])
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[1])
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[2])
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness" & plot.data$Comparison=="Transgender-GNC"] <- as.numeric(median_hdi(TransGNC)[3])

###############################################################################
# Summarize results and make plot for paper 
##############################################################################

# Summaries -----------------------------------------------------------

# Median differences
summary(plot.data$Estimate)
summary(plot.data$Upper)
summary(plot.data$Lower)

# Number of comparisons
plot.data$Inside <- ifelse(plot.data$Upper> .2 | plot.data$Lower < -.2,"No","Yes")
table(plot.data$Inside) # 130/186
130/186
table(plot.data$Inside,plot.data$Comparison) # by group
48/62 # Control vs. GNC
52/62 # Control vs. Transgender
30/62 # Transgender vs. GNC


# Proportion inside the ROPE
summary(plot.data$Proportion)
length(plot.data$Proportion[plot.data$Proportion > .95])
162/186

# Plot 1 --------------------------------------------------------------

# Get colon for figure
plot.data$`Comparison:` <- plot.data$Comparison

# Make y-axis title
my_y_title <- expression(paste("Effect Size (Cohen's ", italic("h"), ")"))

# Make plot
png("Analysis2aMultiverse.png", family= "Gill Sans MT",width = 10, height = 6.25, units = 'in', res = 1200)
p <- ggplot(plot.data)+ ggtitle("Multiverse Analysis 2: Do Future Transitioners, Transgender Children, \nand Controls Differ in their Gender Identity and Preferences? ")
p <- p + geom_hline(yintercept = .2, colour = gray(1/2), lty = 3) 
p <- p + geom_hline(yintercept = -.2, colour = gray(1/2), lty = 3)
p <- p + geom_hline(yintercept = 0, colour = gray(1/2), lty = 1)
p <- p + geom_pointrange(aes(x = DV, y = Estimate, ymin = Lower,
                             ymax = Upper,group=`Comparison:`,color=`Comparison:`),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 16)
p <- p + scale_y_continuous(name = my_y_title,limits = c(-.5,.5),labels=c(-.5,-.2,0,.2,.5),breaks=c(-.5,-.2,0,.2,.5))+ theme_grey()
p <- p + xlab(label="Outcome Variable") + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=.6))+theme(strip.text.x = element_blank())
p <- p  + facet_grid(vars(Missing),vars(Number),scales = "free_x",space="free_x") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p <- p + scale_fill_viridis(direction = -1,begin = c(.8),option="B",discrete=TRUE)+ scale_color_viridis(direction = -1,end=c(.8),option="B",discrete = TRUE) 
p <- p + theme(text=element_text(size=14,family="Gill Sans MT"))
p <- p + theme(legend.position = "bottom")
p <- p + theme(plot.title = element_text(family = "Gill Sans MT", face = "bold",hjust = .5,margin = margin(t = 10, r = 0, b = 10, l = 0)))
p
dev.off()

# Save as PDF
ggsave("Analysis2Multiverse.pdf", p, width=10, height=6, 
       device = cairo_pdf, units = "in")

# Plot 2 --------------------------------------------------------------------

png("Analysis2bMultiverse.png", family= "Gill Sans MT",width = 10, height = 5, units = 'in', res = 1200)
p<-ggplot(plot.data, aes(x=DV,y=Proportion,group=`Comparison:`)) + theme_gray() +
  geom_line(aes(color=`Comparison:`))+ scale_y_continuous(limits = c(0,1)) +
  facet_grid(vars(Missing),vars(Number),scales = "free_x",space="free_x")+ scale_fill_viridis(direction = -1,begin = c(.8),option="B",discrete=TRUE)+ scale_color_viridis(direction = -1,end=c(.8),option="B",discrete = TRUE) +
  ylab(label = "% of Posterior in ROPE") + xlab(label="Outcome Variable") + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=.6)) +theme(strip.text.x = element_blank())
p <- p + theme(text=element_text(size=14)) + theme(legend.position = "top")
p
dev.off()

