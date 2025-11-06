####################################
# File 13 of 15
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

# Change plot
ggplot2::theme_set(ggplot2::theme_gray())

# Set working directory
setwd("")

# Read in data
data <- read.table("complete.data.csv",sep=",",header=TRUE)

#################################
# Demographics
#################################

# sample size
table(data$group,data$transition)

# sex
round(tapply(data$sex,list(data$group,data$transition),mean),2)

# age
round(tapply(data$agemos,list(data$group,data$transition),mean),1)
round(tapply(data$agemos,list(data$group,data$transition),sd),1)

# race
round(tapply(data$racecat,list(data$group,data$transition),mean),2)

# time since test
round(tapply(data$MonthsTest,list(data$group,data$transition),mean),1)
round(tapply(data$MonthsTest,list(data$group,data$transition),sd),1)

# liberalism
summary(data$polid)
round(tapply(data$polid,list(data$group,data$transition),mean,na.rm=T),1)
round(tapply(data$polid,list(data$group,data$transition),sd,na.rm=T),1)

# income
summary(data$income)
round(tapply(data$income,list(data$group,data$transition),mean,na.rm=T),1)
round(tapply(data$income,list(data$group,data$transition),sd,na.rm=T),1)

#################################
# Means (and correlations among) on gender cognition measures
#################################

# Put on 0-1 metric
data$GenderCognition <- data$PCTSI.p
data$PeerGenderPOMP <- data$PeerGenderPOMP/100
data$ToyGenderPOMP <- data$ToyGenderPOMP/100
data$ClothingGenderPOMP <- data$ClothingGenderPOMP/100
data$SimilarityGenderPOMP <- data$SimilarityGenderPOMP/100
data$IdentityGenderPOMP <- data$IdentityGenderPOMP/100

# Measure correlations
myvars <- c("PeerGenderPOMP","ToyGenderPOMP","ClothingGenderPOMP","SimilarityGenderPOMP","IdentityGenderPOMP")

GenderCog <- data[myvars]

# Get correlations
rcorr(as.matrix(GenderCog))

# Peer
round(tapply(data$PeerGenderPOMP,list(data$group,data$transition),mean),2)
round(tapply(data$PeerGenderPOMP,list(data$group,data$transition),sd),2)

# Toy
round(tapply(data$ToyGenderPOMP,list(data$group,data$transition),mean),2)
round(tapply(data$ToyGenderPOMP,list(data$group,data$transition),sd),2)

# Clothing
round(tapply(data$ClothingGenderPOMP,list(data$group,data$transition),mean),2)
round(tapply(data$ClothingGenderPOMP,list(data$group,data$transition),sd),2)

# Similarity
round(tapply(data$SimilarityGenderPOMP,list(data$group,data$transition),mean),2)
round(tapply(data$SimilarityGenderPOMP,list(data$group,data$transition),sd),2)

# Identity
round(tapply(data$IdentityGenderPOMP,list(data$group,data$transition),mean),2)
round(tapply(data$IdentityGenderPOMP,list(data$group,data$transition),sd),2)

# Composite
round(tapply(data$GenderCognition,list(data$group,data$transition),mean),2)
round(tapply(data$GenderCognition,list(data$group,data$transition),sd),2)

###########################
# Zero-order correlations entire sample
###########################

# Selecting data want
myvars <- c("GenderCognition","sex", "agemos", "racecat", "polid_imp"  , "income_imp")

# Subsetting data
cor.data <- data[myvars]

# Get correlations
rcorr(as.matrix(cor.data))

###########################
# Zero-order correlations subset in analysis 1
###########################

# Get data want with just GNC kids -------------------------

# Only GNC
analysis1.data <- subset(data,data$group== "Gender Nonconforming")

# Selecting data want
myvars <- c("transition","GenderCognition","sex", "agemos", "racecat", "MonthsTest", "polid_imp"  , "income_imp")

# Subsetting data
analysis1.data <- analysis1.data[myvars]

# Get correlations
rcorr(as.matrix(analysis1.data))

###########################
# Zero-order correlations subset in analysis 2
###########################

# Get data want with future transitioners or matched children -------------------------

# Only GNC
analysis2.data <- subset(data,data$transition== 1)

# Selecting data want
myvars <- c("GenderCognition","sex", "agemos", "racecat", "polid_imp"  , "income_imp")

# Subsetting data
analysis2.data <- analysis2.data[myvars]

# Get correlations
rcorr(as.matrix(analysis2.data))

###########################
# Zero-order correlations subset in analysis 3
###########################

# Get data want with non-transitioners or matched children -------------------------

# Only GNC
analysis3.data <- subset(data,data$transition== 0)

# Selecting data want
myvars <- c("GenderCognition","sex", "agemos", "racecat", "polid_imp"  , "income_imp")

# Subsetting data
analysis3.data <- analysis3.data[myvars]

# Get correlations
rcorr(as.matrix(analysis3.data))
