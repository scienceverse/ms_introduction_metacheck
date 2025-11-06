####################################
# File 3 of 15
# R code for: Predicting Early Childhood Gender Transitions
# Updated: October 24 2018
# Contact James Rae [jrrae@umass.edu] with any questions
####################################

###################################
# Set up
###################################

# Read in libraries
rm(list=ls())
library(arm)
library(mice)
library(plyr)
library(doBy)
library(sjmisc)

# Set working directory
setwd("")

# Read in original data
data <- read.table("GenderTransition.csv",header=TRUE,sep = "")
head(data)

# Load imputated data 
load("imp.rda")

# Make long so can compute variables
long <- mice::complete(imp, action='long',include = TRUE)

######################################
# Getting data for plots and descriptive statistics- For simplicity, use mean imputed value across datasets
######################################

# Get plot data ------------------------------------------

# Make mids object
long.mids <- as.mids(long)

# Get mean of imputed datasets from sjmisc package
long.mids.mean <- merge_imputations(dat = data,imp = long.mids,ori=data)

#########################################
# Compute each gender cognition variable
#########################################

# Note: Same as above for imputed data

# Sum boy items
long.mids.mean$PeerBoyNum <- rowSums(cbind(long.mids.mean$Peer1_imp,long.mids.mean$Peer2_imp,long.mids.mean$Peer3_imp,long.mids.mean$Peer4_imp,long.mids.mean$Peer5_imp,long.mids.mean$Peer6_imp) > 0,na.rm=T)
# Sum girl items
long.mids.mean$PeerGirlNum <- rowSums(cbind(long.mids.mean$Peer1_imp,long.mids.mean$Peer2_imp,long.mids.mean$Peer3_imp,long.mids.mean$Peer4_imp,long.mids.mean$Peer5_imp,long.mids.mean$Peer6_imp) < 1,na.rm=T)
# Getting POMP score
long.mids.mean$PeerGirlPOMP <- ((long.mids.mean$PeerGirlNum-0)/((long.mids.mean$PeerBoyNum + long.mids.mean$PeerGirlNum)-0))*100
# Coding in terms of gender
long.mids.mean$PeerGenderPOMP <- ifelse(long.mids.mean$gender==0,long.mids.mean$PeerGirlPOMP,
                                        ifelse(long.mids.mean$gender==1,100-long.mids.mean$PeerGirlPOMP,NA))

# Mean of items
long.mids.mean$ToyGirlMean <- rowMeans(cbind(long.mids.mean$toy1_imp,long.mids.mean$toy2_imp,long.mids.mean$toy3_imp,long.mids.mean$toy4_imp),na.rm=T)
# Getting POMP score
long.mids.mean$ToyGirlPOMP <- ((long.mids.mean$ToyGirlMean-1)/(5-1))*100
# Coding in terms of gender
long.mids.mean$ToyGenderPOMP <- ifelse(long.mids.mean$gender==0,long.mids.mean$ToyGirlPOMP,
                                       ifelse(long.mids.mean$gender==1,100-long.mids.mean$ToyGirlPOMP,NA))

# Mean of items
long.mids.mean$ClothingGirlMean <- rowMeans(cbind(long.mids.mean$cloth1_imp,long.mids.mean$cloth2_imp,long.mids.mean$cloth3_imp,long.mids.mean$cloth4_imp),na.rm=T)
# Getting POMP score
long.mids.mean$ClothingGirlPOMP <- ((long.mids.mean$ClothingGirlMean-1)/(5-1))*100
# Coding in terms of gender
long.mids.mean$ClothingGenderPOMP <- ifelse(long.mids.mean$gender==0,long.mids.mean$ClothingGirlPOMP,
                                            ifelse(long.mids.mean$gender==1,100-long.mids.mean$ClothingGirlPOMP,NA))

# Mean for girl items
long.mids.mean$SimilarityGirlMean <- rowMeans(cbind(long.mids.mean$sim1_imp,long.mids.mean$sim3_imp,long.mids.mean$sim5_imp,long.mids.mean$sim7_imp,long.mids.mean$sim9_imp),na.rm=TRUE)
# Mean for boy items
long.mids.mean$SimilarityBoyMean <- rowMeans(cbind(long.mids.mean$sim2_imp,long.mids.mean$sim4_imp,long.mids.mean$sim6_imp,long.mids.mean$sim8_imp,long.mids.mean$sim10_imp),na.rm=TRUE)
# Mean difference between boy and girl items
long.mids.mean$SimilarityMeanDiff <- long.mids.mean$SimilarityGirlMean - long.mids.mean$SimilarityBoyMean
# Getting POMP score
long.mids.mean$SimilarityGirlPOMP <- ((long.mids.mean$SimilarityMeanDiff+4)/(4+4))*100
# Coding in terms of gender
long.mids.mean$SimilarityGenderPOMP <- ifelse(long.mids.mean$gender==0,long.mids.mean$SimilarityGirlPOMP,
                                              ifelse(long.mids.mean$gender==1,100-long.mids.mean$SimilarityGirlPOMP,NA))

# Recoding values -1 (male),1 (female), or 0 (anything else)
long.mids.mean$Idadult.recode <- ifelse(long.mids.mean$Idadult_imp ==1,-1,
                                        ifelse(long.mids.mean$Idadult_imp==2,1,0))
# Recoding values -1 (male),1 (female), or 0 (anything else)
long.mids.mean$Idnow.recode <- ifelse(long.mids.mean$Idnow_imp ==1,-1,
                                      ifelse(long.mids.mean$Idnow_imp==2,1,0))
# Take row sums 
long.mids.mean$IdentityGirlSum <- rowSums(cbind(long.mids.mean$Idadult.recode,long.mids.mean$Idnow.recode),na.rm=T)
# Getting POMP score
long.mids.mean$IdentityGirlPOMP <- ((long.mids.mean$IdentityGirlSum+2)/(2+2))*100
# Code in terms of gender
long.mids.mean$IdentityGenderPOMP <- ifelse(long.mids.mean$gender==0,long.mids.mean$IdentityGirlPOMP,
                                            ifelse(long.mids.mean$gender==1,100-long.mids.mean$IdentityGirlPOMP,NA))

# Mean across 5 POMP scores
long.mids.mean$PCTSI.p <- rowMeans(cbind(long.mids.mean$PeerGenderPOMP,long.mids.mean$ClothingGenderPOMP,
                                         long.mids.mean$ToyGenderPOMP,long.mids.mean$SimilarityGenderPOMP,
                                         long.mids.mean$IdentityGenderPOMP),na.rm=T)/100

######################################
# Save output and data for plots and descriptive statistics
######################################

write.table(long.mids.mean,file="complete.data.csv",sep=",",row.names = F)
