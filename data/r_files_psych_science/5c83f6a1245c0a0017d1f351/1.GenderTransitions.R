####################################
# File 1 of 15
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

# Set working directory
setwd("")

# Read in data
data <- read.table("GenderTransition.csv",header=TRUE,sep = "")
head(data)

#########################################
# Look at missingness rate
#########################################

# Get overall sense of data
summary(data)

# Get missing pattern
tail(md.pattern(data))

#########################################
# Imputation
#########################################

# Max imputation
40/length(data$id) # 16%

# Imputation by predictive mean matching
imp <- mice(data, m=20, maxit=20, seed = 1234)
save(imp,file="imp.rda")

# Look for convergence by examining trace lines
plot(imp)

# Looking at plots for variables with missing values
stripplot(imp, income~.imp, pch=20, cex=2)
stripplot(imp, polid~.imp, pch=20, cex=2)
stripplot(imp, Idnow~.imp, pch=20, cex=2)
stripplot(imp, Idadult~.imp, pch=20, cex=2)
stripplot(imp, Peer1~.imp, pch=20, cex=2)
stripplot(imp, Peer2~.imp, pch=20, cex=2)
stripplot(imp, Peer3~.imp, pch=20, cex=2)
stripplot(imp, Peer4~.imp, pch=20, cex=2)
stripplot(imp, Peer5~.imp, pch=20, cex=2)
stripplot(imp, Peer6~.imp, pch=20, cex=2)
stripplot(imp, toy1~.imp, pch=20, cex=2)
stripplot(imp, toy2~.imp, pch=20, cex=2)
stripplot(imp, toy3~.imp, pch=20, cex=2)
stripplot(imp, toy4~.imp, pch=20, cex=2)
stripplot(imp, cloth1~.imp, pch=20, cex=2)
stripplot(imp, cloth2~.imp, pch=20, cex=2)
stripplot(imp, cloth3~.imp, pch=20, cex=2)
stripplot(imp, cloth4~.imp, pch=20, cex=2)
stripplot(imp, sim1~.imp, pch=20, cex=2)
stripplot(imp, sim2~.imp, pch=20, cex=2)
stripplot(imp, sim3~.imp, pch=20, cex=2)
stripplot(imp, sim4~.imp, pch=20, cex=2)
stripplot(imp, sim5~.imp, pch=20, cex=2)
stripplot(imp, sim6~.imp, pch=20, cex=2)
stripplot(imp, sim7~.imp, pch=20, cex=2)
stripplot(imp, sim8~.imp, pch=20, cex=2)
stripplot(imp, sim9~.imp, pch=20, cex=2)
stripplot(imp, sim10~.imp, pch=20, cex=2)
# Plots show that seem to have imputed reasonable values for all variables

# Make long so can compute variables
long <- mice::complete(imp, action='long',include = TRUE)

#########################################
# Compute each gender cognition variable
#########################################

# Peer ----------------------------------
# Sum boy items
long$PeerBoyNum <- rowSums(cbind(long$Peer1,long$Peer2,long$Peer3,long$Peer4,long$Peer5,long$Peer6) > 0,na.rm=T)
# Sum girl items
long$PeerGirlNum <- rowSums(cbind(long$Peer1,long$Peer2,long$Peer3,long$Peer4,long$Peer5,long$Peer6) < 1,na.rm=T)
# Getting POMP score
long$PeerGirlPOMP <- ((long$PeerGirlNum-0)/((long$PeerBoyNum + long$PeerGirlNum)-0))*100
# Coding in terms of gender
long$PeerGenderPOMP <- ifelse(long$gender==0,long$PeerGirlPOMP,
                              ifelse(long$gender==1,100-long$PeerGirlPOMP,NA))

# Toy -----------------------------------
# Mean of items
long$ToyGirlMean <- rowMeans(cbind(long$toy1,long$toy2,long$toy3,long$toy4),na.rm=T)
# Getting POMP score
long$ToyGirlPOMP <- ((long$ToyGirlMean-1)/(5-1))*100
# Coding in terms of gender
long$ToyGenderPOMP <- ifelse(long$gender==0,long$ToyGirlPOMP,
                             ifelse(long$gender==1,100-long$ToyGirlPOMP,NA))

# Clothing -------------------------------
# Mean of items
long$ClothingGirlMean <- rowMeans(cbind(long$cloth1,long$cloth2,long$cloth3,long$cloth4),na.rm=T)
# Getting POMP score
long$ClothingGirlPOMP <- ((long$ClothingGirlMean-1)/(5-1))*100
# Coding in terms of gender
long$ClothingGenderPOMP <- ifelse(long$gender==0,long$ClothingGirlPOMP,
                                  ifelse(long$gender==1,100-long$ClothingGirlPOMP,NA))

# Similarity -------------------------------
# Mean for girl items
long$SimilarityGirlMean <- rowMeans(cbind(long$sim1,long$sim3,long$sim5,long$sim7,long$sim9),na.rm=TRUE)
# Mean for boy items
long$SimilarityBoyMean <- rowMeans(cbind(long$sim2,long$sim4,long$sim6,long$sim8,long$sim10),na.rm=TRUE)
# Mean difference between boy and girl items
long$SimilarityMeanDiff <- long$SimilarityGirlMean - long$SimilarityBoyMean
# Getting POMP score
long$SimilarityGirlPOMP <- ((long$SimilarityMeanDiff+4)/(4+4))*100
# Coding in terms of gender
long$SimilarityGenderPOMP <- ifelse(long$gender==0,long$SimilarityGirlPOMP,
                                    ifelse(long$gender==1,100-long$SimilarityGirlPOMP,NA))

# Explicit identity ---------------------------
# Recoding values -1 (male),1 (female), or 0 (anything else)
long$Idadult.recode <- ifelse(long$Idadult ==1,-1,
                              ifelse(long$Idadult==2,1,0))
# Recoding values -1 (male),1 (female), or 0 (anything else)
long$Idnow.recode <- ifelse(long$Idnow ==1,-1,
                            ifelse(long$Idnow==2,1,0))
# Take row sums 
long$IdentityGirlSum <- rowSums(cbind(long$Idadult.recode,long$Idnow.recode),na.rm=T)
# Getting POMP score
long$IdentityGirlPOMP <- ((long$IdentityGirlSum+2)/(2+2))*100
# Code in terms of gender
long$IdentityGenderPOMP <- ifelse(long$gender==0,long$IdentityGirlPOMP,
                                  ifelse(long$gender==1,100-long$IdentityGirlPOMP,NA))

##################################
# Checking values 
#################################

# Define values want
myfun1 <- function(x){c(m=mean(x), max=max(x), min=min(x))}

# Get summaries by imputation. Imputation 0 should have NA's but others not.
summaryBy(cbind(PeerGenderPOMP) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(ClothingGenderPOMP) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(ToyGenderPOMP) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(SimilarityGenderPOMP) ~ .imp, data=long, FUN=myfun1) # looks ok
summaryBy(cbind(IdentityGenderPOMP) ~ .imp,data=long, FUN=myfun1) # looks ok

##################################
# Compute 31 possible dependent variables
#################################

# one variable --------------------
long$P.p <- long$PeerGenderPOMP/100
long$C.p <- long$ClothingGenderPOMP/100
long$T.p <- long$ToyGenderPOMP/100
long$S.p <- long$SimilarityGenderPOMP/100
long$I.p <- long$IdentityGenderPOMP/100

# two variables -------------------------
long$PC.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP),na.rm=T)/100
long$PT.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ToyGenderPOMP),na.rm=T)/100
long$PS.p <- rowMeans(cbind(long$PeerGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$PI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$CT.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$ToyGenderPOMP),na.rm=T)/100
long$CS.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$CI.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$TS.p <- rowMeans(cbind(long$ToyGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$TI.p <- rowMeans(cbind(long$ToyGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$SI.p <- rowMeans(cbind(long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100

# three variables -------------------------
long$PCT.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,long$ToyGenderPOMP),na.rm=T)/100
long$PCS.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$PCI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$PTS.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ToyGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$PTI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ToyGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$PSI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$CTS.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$ToyGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$CTI.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$ToyGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$CSI.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$TSI.p <- rowMeans(cbind(long$ToyGenderPOMP,long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100

# four variables -------------------------
long$PCTS.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,
                              long$ToyGenderPOMP,long$SimilarityGenderPOMP),na.rm=T)/100
long$PCTI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,
                              long$ToyGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$PCSI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,
                              long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$PTSI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ToyGenderPOMP,
                              long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100
long$CTSI.p <- rowMeans(cbind(long$ClothingGenderPOMP,long$ToyGenderPOMP,
                              long$SimilarityGenderPOMP,long$IdentityGenderPOMP),na.rm=T)/100

# five variables -------------------------
long$PCTSI.p <- rowMeans(cbind(long$PeerGenderPOMP,long$ClothingGenderPOMP,
                              long$ToyGenderPOMP,long$SimilarityGenderPOMP,
                              long$IdentityGenderPOMP),na.rm=T)/100

# Get summaries by imputation. Imputation 0 should have NA's but others not.
summaryBy(cbind(P.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(C.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(T.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(S.p) ~ .imp, data=long, FUN=myfun1) # looks ok
summaryBy(cbind(I.p) ~ .imp,data=long, FUN=myfun1) # looks ok

summaryBy(cbind(PC.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PT.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CT.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(TS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(TI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(SI.p) ~ .imp,data=long, FUN=myfun1) # looks ok

summaryBy(cbind(PCT.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PCS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PCI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PTS.p) ~ .imp, data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PTI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PSI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CTS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CTI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CSI.p) ~ .imp, data=long, FUN=myfun1) # looks ok
summaryBy(cbind(TSI.p) ~ .imp,data=long, FUN=myfun1) # looks ok

summaryBy(cbind(PCTS.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PCTI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PCSI.p) ~ .imp,data=long, FUN=myfun1) # looks ok
summaryBy(cbind(PTSI.p) ~ .imp, data=long, FUN=myfun1) # looks ok
summaryBy(cbind(CTSI.p) ~ .imp,data=long, FUN=myfun1) # looks ok

summaryBy(cbind(PCTSI.p) ~ .imp,data=long, FUN=myfun1) # looks ok

##################################
# Put into separate data sets
#################################

# Making subsets for each analysis
data1 <-  subset(long,long$transition==1)
data2 <-  subset(long,long$transition==0)
data3 <- subset(long,long$group == "Gender Nonconforming")

##################################
# Scaling variables for analyses
#################################

# Data set 1---------------------------

# Scaled so can be compared
data1 <- ddply(data1, c(".imp"), transform, P.p.r = (P.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, C.p.r = (C.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, T.p.r = (T.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, S.p.r = (S.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, I.p.r = (I.p*(107-1) + 0.5)/107)

data1 <- ddply(data1, c(".imp"), transform, PC.p.r = (PC.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PT.p.r = (PT.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PS.p.r = (PS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PI.p.r = (PI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CT.p.r = (CT.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CS.p.r = (CS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CI.p.r = (CI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, TS.p.r = (TS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, TI.p.r = (TI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, SI.p.r = (SI.p*(107-1) + 0.5)/107)

data1 <- ddply(data1, c(".imp"), transform, PCT.p.r = (PCT.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PCS.p.r = (PCS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PCI.p.r = (PCI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PTS.p.r = (PTS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PTI.p.r = (PTI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PSI.p.r = (PSI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CTS.p.r = (CTS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CTI.p.r = (CTI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CSI.p.r = (CSI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, TSI.p.r = (TSI.p*(107-1) + 0.5)/107)

data1 <- ddply(data1, c(".imp"), transform, PCTS.p.r = (PCTS.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PCTI.p.r = (PCTI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PCSI.p.r = (PCSI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, PTSI.p.r = (PTSI.p*(107-1) + 0.5)/107)
data1 <- ddply(data1, c(".imp"), transform, CTSI.p.r = (CTSI.p*(107-1) + 0.5)/107)

data1 <- ddply(data1, c(".imp"), transform, PCTSI.p.r = (PCTSI.p*(107-1) + 0.5)/107)

data1 <- ddply(data1, c(".imp"), transform, sex.z = rescale(sex))
data1 <- ddply(data1, c(".imp"), transform, agemos.z = rescale(agemos))
data1 <- ddply(data1, c(".imp"), transform, racecat.z = rescale(racecat))
data1 <- ddply(data1, c(".imp"), transform, polid.z = rescale(polid))
data1 <- ddply(data1, c(".imp"), transform, income.z = rescale(income))

# Data set 2---------------------------

# Scaled so can be compared
data2 <- ddply(data2, c(".imp"), transform, P.p.r = (P.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, C.p.r = (C.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, T.p.r = (T.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, S.p.r = (S.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, I.p.r = (I.p*(147-1) + 0.5)/147)

data2 <- ddply(data2, c(".imp"), transform, PC.p.r = (PC.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PT.p.r = (PT.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PS.p.r = (PS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PI.p.r = (PI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CT.p.r = (CT.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CS.p.r = (CS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CI.p.r = (CI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, TS.p.r = (TS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, TI.p.r = (TI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, SI.p.r = (SI.p*(147-1) + 0.5)/147)

data2 <- ddply(data2, c(".imp"), transform, PCT.p.r = (PCT.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PCS.p.r = (PCS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PCI.p.r = (PCI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PTS.p.r = (PTS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PTI.p.r = (PTI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PSI.p.r = (PSI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CTS.p.r = (CTS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CTI.p.r = (CTI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CSI.p.r = (CSI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, TSI.p.r = (TSI.p*(147-1) + 0.5)/147)

data2 <- ddply(data2, c(".imp"), transform, PCTS.p.r = (PCTS.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PCTI.p.r = (PCTI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PCSI.p.r = (PCSI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, PTSI.p.r = (PTSI.p*(147-1) + 0.5)/147)
data2 <- ddply(data2, c(".imp"), transform, CTSI.p.r = (CTSI.p*(147-1) + 0.5)/147)

data2 <- ddply(data2, c(".imp"), transform, PCTSI.p.r = (PCTSI.p*(147-1) + 0.5)/147)

data2 <- ddply(data2, c(".imp"), transform, sex.z = rescale(sex))
data2 <- ddply(data2, c(".imp"), transform, agemos.z = rescale(agemos))
data2 <- ddply(data2, c(".imp"), transform, racecat.z = rescale(racecat))
data2 <- ddply(data2, c(".imp"), transform, polid.z = rescale(polid))
data2 <- ddply(data2, c(".imp"), transform, income.z = rescale(income))

# Dataset 3 ---------------------------

# Scaled so can be compared
data3 <- ddply(data3, c(".imp"), transform, P.z = rescale(P.p))
data3 <- ddply(data3, c(".imp"), transform, C.z = rescale(C.p))
data3 <- ddply(data3, c(".imp"), transform, T.z = rescale(T.p))
data3 <- ddply(data3, c(".imp"), transform, S.z = rescale(S.p))
data3 <- ddply(data3, c(".imp"), transform, I.z = rescale(I.p))

data3 <- ddply(data3, c(".imp"), transform, PC.z = rescale(PC.p))
data3 <- ddply(data3, c(".imp"), transform, PT.z = rescale(PT.p))
data3 <- ddply(data3, c(".imp"), transform, PS.z = rescale(PS.p))
data3 <- ddply(data3, c(".imp"), transform, PI.z = rescale(PI.p))
data3 <- ddply(data3, c(".imp"), transform, CT.z = rescale(CT.p))
data3 <- ddply(data3, c(".imp"), transform, CS.z = rescale(CS.p))
data3 <- ddply(data3, c(".imp"), transform, CI.z = rescale(CI.p))
data3 <- ddply(data3, c(".imp"), transform, TS.z = rescale(TS.p))
data3 <- ddply(data3, c(".imp"), transform, TI.z = rescale(TI.p))
data3 <- ddply(data3, c(".imp"), transform, SI.z = rescale(SI.p))

data3 <- ddply(data3, c(".imp"), transform, PCT.z = rescale(PCT.p))
data3 <- ddply(data3, c(".imp"), transform, PCS.z = rescale(PCS.p))
data3 <- ddply(data3, c(".imp"), transform, PCI.z = rescale(PCI.p))
data3 <- ddply(data3, c(".imp"), transform, PTS.z = rescale(PTS.p))
data3 <- ddply(data3, c(".imp"), transform, PTI.z = rescale(PTI.p))
data3 <- ddply(data3, c(".imp"), transform, PSI.z = rescale(PSI.p))
data3 <- ddply(data3, c(".imp"), transform, CTS.z = rescale(CTS.p))
data3 <- ddply(data3, c(".imp"), transform, CTI.z = rescale(CTI.p))
data3 <- ddply(data3, c(".imp"), transform, CSI.z = rescale(CSI.p))
data3 <- ddply(data3, c(".imp"), transform, TSI.z = rescale(TSI.p))

data3 <- ddply(data3, c(".imp"), transform, PCTS.z = rescale(PCTS.p))
data3 <- ddply(data3, c(".imp"), transform, PCTI.z = rescale(PCTI.p))
data3 <- ddply(data3, c(".imp"), transform, PCSI.z = rescale(PCSI.p))
data3 <- ddply(data3, c(".imp"), transform, PTSI.z = rescale(PTSI.p))
data3 <- ddply(data3, c(".imp"), transform, CTSI.z = rescale(CTSI.p))

data3 <- ddply(data3, c(".imp"), transform, PCTSI.z = rescale(PCTSI.p))

data3 <- ddply(data3, c(".imp"), transform, sex.z = rescale(sex))
data3 <- ddply(data3, c(".imp"), transform, agemos.z = rescale(agemos))
data3 <- ddply(data3, c(".imp"), transform, racecat.z = rescale(racecat))
data3 <- ddply(data3, c(".imp"), transform, MonthsTest.z = rescale(MonthsTest))
data3 <- ddply(data3, c(".imp"), transform, polid.z = rescale(polid))
data3 <- ddply(data3, c(".imp"), transform, income.z = rescale(income))

# Getting out of long format ---------------------
data1.short <- as.mids(data1)
data2.short <- as.mids(data2)
data3.short <- as.mids(data3)

######################################
# Save output and data for plots
######################################

save(data1.short,file="data1.short.rda")
save(data2.short,file="data2.short.rda")
save(data3.short,file="data3.short.rda")




