######################### FACE SPACE ACROSS GROUPS - R code & data at [osf.io/dytxs] #########################

# libraries
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(psych)
library(tidyr)
library(car)
options(contrasts=c("contr.sum","contr.poly"))

# load in data
setwd("Z:/Studies/085_RaceDimensionsBifactor/PublicData") ## CHANGE ME

# face-trait space in long and wide format, indicating which social groups (WM,WF,...,AM)
# were rated on which traits. Values are z-transformed correlations between trait-pairs.
facesL <- read.csv("FaceTraitMatrix - longformat.csv", header=TRUE)
facesW <- read.csv("FaceTraitMatrix - wideformat.csv", header=TRUE) 

# data for RSA:
# stereotype-trait space combined with face-trait space in long format. 
# Values are z-transformed trait-pair correlations with the mean correlation (across all 6 groups) subtracted out.
RSA1 <- read.csv("RSA [faces~stereotypes] dataset1.csv", header=TRUE)
RSA2 <- read.csv("RSA [faces~stereotypes] dataset2directreplication.csv", header=TRUE) # direct replication


################## Study 1 - RMANOVA of 'Face-Trait Space' Correlation Matrix #######################

# set up a matrix of factor codes for the repeated measures variable to use inside Anova
factorcodes <- as.data.frame(rbind(c(0,1),c(1,1),c(2,1),c(0,0),c(1,0),c(2,0)))
factorcodes[,1:2] <- lapply(factorcodes[,1:2], as.factor)

faces <- facesW[,c(3:8)]
attach(faces)

# define multivariate linear model with only the intercept as a predictor
multmodel <- lm(cbind(WM,BM,AM,WF,BF,AF)~1)
multmodel

# build rmanova model
blargh <- Anova(multmodel, idata=factorcodes, idesign =~ V1*V2, type="III")
summary(blargh)




################## Study 2 - RSA of Face-Trait Space & Stereotype-Trait-Space #######################

# Representational Similarity Analysis 
# bootstrap spearman correlations between FACETRAIT matrix (mean-subtracted) & STEREOTYPE matrix (dataset 1 and then 2)

# dataset 1
library(boot)
cor(RSA1[,4:5], use = "complete.obs", method = "spearman")
boote <- function(data,i) cor(data[i,1], data[i,2], use = "complete.obs", method = "spearman")
bootmom <- boot(RSA1[,4:5], boote, 5000)
boot.ci(bootmom)

# dataset 2
cor(RSA2[,4:5], use = "complete.obs", method = "spearman")
boote <- function(data,i) cor(data[i,1], data[i,2], use = "complete.obs", method = "spearman")
bootmom <- boot(RSA2[,4:5], boote, 5000)
boot.ci(bootmom)  




