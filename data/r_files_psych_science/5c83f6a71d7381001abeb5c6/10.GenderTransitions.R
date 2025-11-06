####################################
# File 10 of 15
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

# Set working directory
setwd("")

#################################################################################################
# Load data from two different types of models: those that analyzed imputed data and those that didn't
#################################################################################################

###########################
# Models on imputed data
###########################

# One variable ----------------------------------
load("a1.m1.P.rda")
load("a1.m1.C.rda")
load("a1.m1.T.rda")
load("a1.m1.S.rda")
load("a1.m1.I.rda")

# Two variables ----------------------------------
load("a1.m1.PC.rda")
load("a1.m1.PT.rda")
load("a1.m1.PS.rda")
load("a1.m1.PI.rda")
load("a1.m1.CT.rda")
load("a1.m1.CS.rda")
load("a1.m1.CI.rda")
load("a1.m1.TS.rda")
load("a1.m1.TI.rda")
load("a1.m1.SI.rda")

# Three variables ----------------------------------
load("a1.m1.PCT.rda")
load("a1.m1.PCS.rda")
load("a1.m1.PCI.rda")
load("a1.m1.PTS.rda")
load("a1.m1.PTI.rda")
load("a1.m1.PSI.rda")
load("a1.m1.CTS.rda")
load("a1.m1.CTI.rda")
load("a1.m1.CSI.rda")
load("a1.m1.TSI.rda")

# Four variables ----------------------------------
load("a1.m1.PCTS.rda")
load("a1.m1.PCTI.rda")
load("a1.m1.PCSI.rda")
load("a1.m1.PTSI.rda")
load("a1.m1.CTSI.rda")

# Five variables ----------------------------------
load("a1.m1.PCTSI.rda")

###########################
# Models on data that ignored missingness
###########################

# One variable ----------------------------------
load("a1.m1.P.ni.rda")
load("a1.m1.C.ni.rda")
load("a1.m1.T.ni.rda")
load("a1.m1.S.ni.rda")
load("a1.m1.I.ni.rda")

# Two variables ----------------------------------
load("a1.m1.PC.ni.rda")
load("a1.m1.PT.ni.rda")
load("a1.m1.PS.ni.rda")
load("a1.m1.PI.ni.rda")
load("a1.m1.CT.ni.rda")
load("a1.m1.CS.ni.rda")
load("a1.m1.CI.ni.rda")
load("a1.m1.TS.ni.rda")
load("a1.m1.TI.ni.rda")
load("a1.m1.SI.ni.rda")

# Three variables ----------------------------------
load("a1.m1.PCT.ni.rda")
load("a1.m1.PCS.ni.rda")
load("a1.m1.PCI.ni.rda")
load("a1.m1.PTS.ni.rda")
load("a1.m1.PTI.ni.rda")
load("a1.m1.PSI.ni.rda")
load("a1.m1.CTS.ni.rda")
load("a1.m1.CTI.ni.rda")
load("a1.m1.CSI.ni.rda")
load("a1.m1.TSI.ni.rda")

# Four variables ----------------------------------
load("a1.m1.PCTS.ni.rda")
load("a1.m1.PCTI.ni.rda")
load("a1.m1.PCSI.ni.rda")
load("a1.m1.PTSI.ni.rda")
load("a1.m1.CTSI.ni.rda")

# Five variables ----------------------------------
load("a1.m1.PCTSI.ni.rda")

#################################################################################################
# First, run models, get estimates, and make plots for models presented in-text
#################################################################################################

######################
# Set up data frame to hold estimates
#####################

# Make data set
plot.data <- data.frame(matrix(NA,nrow = 62,ncol=5))

# Give names
names(plot.data) <- c("DV","Missing","Estimate","Lower","Upper") # Change these later

# Assign levels of outcome variable
plot.data$DV <- rep(c("P","C","T","S","I",
                      "PC","PT","PS","PI","CT","CS","CI","TS","TI","SI",
                      "PCT","PCS","PCI","PTS","PTI","PSI","CTS","CTI","CSI","TSI",
                      "PCTS","PCTI","PCSI","PTSI","CTSI",
                      "PCTSI"),2)
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
plot.data$Missing[1:31] <- "Multiple Imputation"
plot.data$Missing[32:62] <- "Ignore Missingness"

########################################
# Insert odds ratios for multiple imputation data
########################################

# One variable ----------------------------------

# P
parameters <- a1.m1.P %>%
  spread_draws(b_P.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# C
parameters <- a1.m1.C %>%
  spread_draws(b_C.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# T
parameters <- a1.m1.T %>%
  spread_draws(b_T.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# S
parameters <- a1.m1.S %>%
  spread_draws(b_S.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# I
parameters <- a1.m1.I %>%
  spread_draws(b_I.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# Two variables ----------------------------------

# PC
parameters <- a1.m1.PC %>%
  spread_draws(b_PC.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PT
parameters <- a1.m1.PT %>%
  spread_draws(b_PT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PS
parameters <- a1.m1.PS %>%
  spread_draws(b_PS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PI
parameters <- a1.m1.PI %>%
  spread_draws(b_PI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CT
parameters <- a1.m1.CT %>%
  spread_draws(b_CT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CS
parameters <- a1.m1.CS %>%
  spread_draws(b_CS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CI
parameters <- a1.m1.CI %>%
  spread_draws(b_CI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# TS
parameters <- a1.m1.TS %>%
  spread_draws(b_TS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# TI
parameters <- a1.m1.TI %>%
  spread_draws(b_TI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# SI
parameters <- a1.m1.SI %>%
  spread_draws(b_SI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# Three variables ----------------------------------

# PCT
parameters <- a1.m1.PCT %>%
  spread_draws(b_PCT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PCS
parameters <- a1.m1.PCS %>%
  spread_draws(b_PCS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PCI
parameters <- a1.m1.PCI %>%
  spread_draws(b_PCI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PTS
parameters <- a1.m1.PTS %>%
  spread_draws(b_PTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PTI
parameters <- a1.m1.PTI %>%
  spread_draws(b_PTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PSI
parameters <- a1.m1.PSI %>%
  spread_draws(b_PSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CTS
parameters <- a1.m1.CTS %>%
  spread_draws(b_CTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CTI
parameters <- a1.m1.CTI %>%
  spread_draws(b_CTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CSI
parameters <- a1.m1.CSI %>%
  spread_draws(b_CSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# TSI
parameters <- a1.m1.TSI %>%
  spread_draws(b_TSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# Four variables ----------------------------------

# PCTS
parameters <- a1.m1.PCTS %>%
  spread_draws(b_PCTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PCTI
parameters <- a1.m1.PCTI %>%
  spread_draws(b_PCTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PCSI
parameters <- a1.m1.PCSI %>%
  spread_draws(b_PCSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# PCSI
parameters <- a1.m1.PTSI %>%
  spread_draws(b_PTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# CTSI
parameters <- a1.m1.CTSI %>%
  spread_draws(b_CTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

# Five variables ----------------------------------

# CTSI
parameters <- a1.m1.PCTSI %>%
  spread_draws(b_PCTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Multiple Imputation"] <- parameters[3]

########################################
# Insert odds ratios for data that ignores missingness
########################################

# One variable ----------------------------------

# P
parameters <- a1.m1.P.ni %>%
  spread_draws(b_P.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="P" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# C
parameters <- a1.m1.C.ni %>%
  spread_draws(b_C.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="C" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# T
parameters <- a1.m1.T.ni %>%
  spread_draws(b_T.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="T" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# S
parameters <- a1.m1.S.ni %>%
  spread_draws(b_S.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="S" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# I
parameters <- a1.m1.I.ni %>%
  spread_draws(b_I.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="I" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# Two variables ----------------------------------

# PC
parameters <- a1.m1.PC.ni %>%
  spread_draws(b_PC.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PC" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PT
parameters <- a1.m1.PT.ni %>%
  spread_draws(b_PT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PT" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PS
parameters <- a1.m1.PS.ni %>%
  spread_draws(b_PS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PI
parameters <- a1.m1.PI.ni %>%
  spread_draws(b_PI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CT
parameters <- a1.m1.CT.ni %>%
  spread_draws(b_CT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CT" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CS
parameters <- a1.m1.CS.ni %>%
  spread_draws(b_CS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CI
parameters <- a1.m1.CI.ni %>%
  spread_draws(b_CI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# TS
parameters <- a1.m1.TS.ni %>%
  spread_draws(b_TS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# TI
parameters <- a1.m1.TI.ni %>%
  spread_draws(b_TI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# SI
parameters <- a1.m1.SI.ni %>%
  spread_draws(b_SI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="SI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# Three variables ----------------------------------

# PCT
parameters <- a1.m1.PCT.ni %>%
  spread_draws(b_PCT.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCT" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PCS
parameters <- a1.m1.PCS.ni %>%
  spread_draws(b_PCS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PCI
parameters <- a1.m1.PCI.ni %>%
  spread_draws(b_PCI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PTS
parameters <- a1.m1.PTS.ni %>%
  spread_draws(b_PTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PTI
parameters <- a1.m1.PTI.ni %>%
  spread_draws(b_PTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PSI
parameters <- a1.m1.PSI.ni %>%
  spread_draws(b_PSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CTS
parameters <- a1.m1.CTS.ni %>%
  spread_draws(b_CTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CTI
parameters <- a1.m1.CTI.ni %>%
  spread_draws(b_CTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CSI
parameters <- a1.m1.CSI.ni %>%
  spread_draws(b_CSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# TSI
parameters <- a1.m1.TSI.ni %>%
  spread_draws(b_TSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="TSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# Four variables ----------------------------------

# PCTS
parameters <- a1.m1.PCTS.ni %>%
  spread_draws(b_PCTS.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTS" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PCTI
parameters <- a1.m1.PCTI.ni %>%
  spread_draws(b_PCTI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PCSI
parameters <- a1.m1.PCSI.ni %>%
  spread_draws(b_PCSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# PCSI
parameters <- a1.m1.PTSI.ni %>%
  spread_draws(b_PTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# CTSI
parameters <- a1.m1.CTSI.ni %>%
  spread_draws(b_CTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="CTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

# Five variables ----------------------------------

# CTSI
parameters <- a1.m1.PCTSI.ni %>%
  spread_draws(b_PCTSI.z) %>%
  median_hdi()
plot.data$Estimate[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[1]
plot.data$Lower[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[2]
plot.data$Upper[plot.data$DV=="PCTSI" & plot.data$Missing=="Ignore Missingness"] <- parameters[3]

###############################################################################
# Summarize results and make plot for paper 
##############################################################################

# Summaries -------------------------------------------------------------

# Get some values for comparison
plot.data$Estimate <- as.numeric(as.character(plot.data$Estimate))
plot.data$Lower <- as.numeric(as.character(plot.data$Lower))
plot.data$Upper <- as.numeric(as.character(plot.data$Upper))
plot.data$Below <- ifelse(plot.data$Lower>0,"above","below")

# Proportion
table(plot.data$Below)
50/62

# Figure -----------------------------------------------------------------
plot.data$f <- "c"
png("Analysis1Multiverse.png", family= "Gill Sans MT",width = 10, height = 6, units = 'in', res = 1200)
p <- ggplot(plot.data)+ scale_y_continuous(breaks = c(0, 1, 2),labels = c("1","2.7","7.4"),name="Odds Ratio") + ggtitle("Multiverse Analysis 1: Do Gender Identity and Preferences Predict Social Transitions?")
p <- p + geom_hline(yintercept = 0, colour = gray(1/3), lty = 3)
p <- p + geom_pointrange(aes(x = DV, y = Estimate, ymin = Lower,
                             ymax = Upper,color=f),
                         lwd = 1/2, position = position_dodge(width = 1/2),
                         shape = 16) + scale_color_viridis(discrete = T,direction = 1,option="B",begin=.2,end=.7)#+ facet_grid(Missing ~.)+ theme_classic()
p <- p + xlab(label="Predictor Variable") + theme(axis.text.x=element_text(angle=45,hjust=1,vjust=.6))+theme(strip.text.x = element_blank())
p <- p + theme(legend.position="none") + facet_grid(vars(Missing),vars(Number),scales = "free_x",space="free_x") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p <- p + theme(text=element_text(size=14, family = "Gill Sans MT"))
p <- p + theme(plot.title = element_text(family = "Gill Sans MT", face = "bold",hjust = .5,margin = margin(t = 10, r = 0, b = 10, l = 0)))
p
dev.off()

# Save as PDF
ggsave("Analysis1Multiverse.pdf", p, width=10, height=6, 
       device = cairo_pdf, units = "in")
