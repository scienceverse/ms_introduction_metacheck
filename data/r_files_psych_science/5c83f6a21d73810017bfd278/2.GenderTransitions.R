####################################
# File 2 of 15
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
library(plyr)

# Set working directory
setwd("")

# Read in data
data <- read.table("GenderTransition.csv",header=TRUE,sep = "")
head(data)

#########################################
# Compute each gender cognition variable
#########################################

# Peer ----------------------------------
# Sum boy items
data$PeerBoyNum <- rowSums(cbind(data$Peer1,data$Peer2,data$Peer3,data$Peer4,data$Peer5,data$Peer6) > 0,na.rm=T)
# Sum girl items
data$PeerGirlNum <- rowSums(cbind(data$Peer1,data$Peer2,data$Peer3,data$Peer4,data$Peer5,data$Peer6) < 1,na.rm=T)
# Getting POMP score
data$PeerGirlPOMP <- ((data$PeerGirlNum-0)/((data$PeerBoyNum + data$PeerGirlNum)-0))*100
# Coding in terms of gender
data$PeerGenderPOMP <- ifelse(data$gender==0,data$PeerGirlPOMP,
                              ifelse(data$gender==1,100-data$PeerGirlPOMP,NA))

# Toy -----------------------------------
# Mean of items
data$ToyGirlMean <- rowMeans(cbind(data$toy1,data$toy2,data$toy3,data$toy4),na.rm=T)
# Getting POMP score
data$ToyGirlPOMP <- ((data$ToyGirlMean-1)/(5-1))*100
# Coding in terms of gender
data$ToyGenderPOMP <- ifelse(data$gender==0,data$ToyGirlPOMP,
                             ifelse(data$gender==1,100-data$ToyGirlPOMP,NA))

# Clothing -------------------------------
# Mean of items
data$ClothingGirlMean <- rowMeans(cbind(data$cloth1,data$cloth2,data$cloth3,data$cloth4),na.rm=T)
# Getting POMP score
data$ClothingGirlPOMP <- ((data$ClothingGirlMean-1)/(5-1))*100
# Coding in terms of gender
data$ClothingGenderPOMP <- ifelse(data$gender==0,data$ClothingGirlPOMP,
                                  ifelse(data$gender==1,100-data$ClothingGirlPOMP,NA))

# Similarity -------------------------------
# Mean for girl items
data$SimilarityGirlMean <- rowMeans(cbind(data$sim1,data$sim3,data$sim5,data$sim7,data$sim9),na.rm=TRUE)
# Mean for boy items
data$SimilarityBoyMean <- rowMeans(cbind(data$sim2,data$sim4,data$sim6,data$sim8,data$sim10),na.rm=TRUE)
# Mean difference between boy and girl items
data$SimilarityMeanDiff <- data$SimilarityGirlMean - data$SimilarityBoyMean
# Getting POMP score
data$SimilarityGirlPOMP <- ((data$SimilarityMeanDiff+4)/(4+4))*100
# Coding in terms of gender
data$SimilarityGenderPOMP <- ifelse(data$gender==0,data$SimilarityGirlPOMP,
                                    ifelse(data$gender==1,100-data$SimilarityGirlPOMP,NA))

# Explicit identity ---------------------------
# Recoding values -1 (male),1 (female), or 0 (anything else)
data$Idadult.recode <- ifelse(data$Idadult ==1,-1,
                              ifelse(data$Idadult==2,1,0))
# Recoding values -1 (male),1 (female), or 0 (anything else)
data$Idnow.recode <- ifelse(data$Idnow ==1,-1,
                            ifelse(data$Idnow==2,1,0))
# Take row sums 
data$IdentityGirlSum <- rowSums(cbind(data$Idadult.recode,data$Idnow.recode),na.rm=T)
# Getting POMP score
data$IdentityGirlPOMP <- ((data$IdentityGirlSum+2)/(2+2))*100
# Code in terms of gender
data$IdentityGenderPOMP <- ifelse(data$gender==0,data$IdentityGirlPOMP,
                                  ifelse(data$gender==1,100-data$IdentityGirlPOMP,NA))

##################################
# Checking values 
#################################

# Get summaries by imputation. Imputation 0 should have NA's but others not.
summary(data$PeerGenderPOMP) # looks ok
summary(data$ClothingGenderPOMP) # looks ok
summary(data$ToyGenderPOMP) # looks ok
summary(data$SimilarityGenderPOMP) # looks ok
summary(data$IdentityGenderPOMP) # looks ok

# Some NA's so make sure that those cases where child completed no items 
PeerNA <- subset(data,is.na(data$PeerGenderPOMP)==TRUE)
PeerNA # NA on all items
ClothingNA <- subset(data,is.na(data$ClothingGenderPOMP)==TRUE)
ClothingNA # NA on all items
ToyNA <- subset(data,is.na(data$ToyGenderPOMP)==TRUE)
ToyNA # NA on all items
SimilarityNA <- subset(data,is.na(data$SimilarityGenderPOMP)==TRUE)
SimilarityNA # NA on all items
IdentityNA <- subset(data,is.na(data$IdentityGenderPOMP)==TRUE)
IdentityNA # NA on all items

##################################
# Compute 31 possible dependent variables
#################################

# one variable --------------------
data$P.p <- data$PeerGenderPOMP/100
data$C.p <- data$ClothingGenderPOMP/100
data$T.p <- data$ToyGenderPOMP/100
data$S.p <- data$SimilarityGenderPOMP/100
data$I.p <- data$IdentityGenderPOMP/100

# two variables -------------------------
data$PC.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP),na.rm=T)/100
data$PT.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ToyGenderPOMP),na.rm=T)/100
data$PS.p <- rowMeans(cbind(data$PeerGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$PI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$CT.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$ToyGenderPOMP),na.rm=T)/100
data$CS.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$CI.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$TS.p <- rowMeans(cbind(data$ToyGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$TI.p <- rowMeans(cbind(data$ToyGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$SI.p <- rowMeans(cbind(data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100

# three variables -------------------------
data$PCT.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,data$ToyGenderPOMP),na.rm=T)/100
data$PCS.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$PCI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$PTS.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ToyGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$PTI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ToyGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$PSI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$CTS.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$ToyGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$CTI.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$ToyGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$CSI.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$TSI.p <- rowMeans(cbind(data$ToyGenderPOMP,data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100

# four variables -------------------------
data$PCTS.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,
                              data$ToyGenderPOMP,data$SimilarityGenderPOMP),na.rm=T)/100
data$PCTI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,
                              data$ToyGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$PCSI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,
                              data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$PTSI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ToyGenderPOMP,
                              data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100
data$CTSI.p <- rowMeans(cbind(data$ClothingGenderPOMP,data$ToyGenderPOMP,
                              data$SimilarityGenderPOMP,data$IdentityGenderPOMP),na.rm=T)/100

# five variables -------------------------
data$PCTSI.p <- rowMeans(cbind(data$PeerGenderPOMP,data$ClothingGenderPOMP,
                               data$ToyGenderPOMP,data$SimilarityGenderPOMP,
                               data$IdentityGenderPOMP),na.rm=T)/100

# Get summaries by imputation. Imputation 0 should have NA's but others not.
summary(data$P.p) # looks ok
summary(data$C.p) # looks ok
summary(data$T.p) # looks ok
summary(data$S.p) # looks ok
summary(data$I.p) # looks ok

summary(data$PC.p) # looks ok
summary(data$PT.p) # looks ok
summary(data$PS.p) # looks ok
summary(data$PI.p) # looks ok
summary(data$CT.p) # looks ok
summary(data$CS.p) # looks ok
summary(data$CI.p) # looks ok
summary(data$TS.p) # looks ok
summary(data$TI.p) # looks ok
summary(data$SI.p) # looks ok

summary(data$PCT.p) # looks ok
summary(data$PCS.p) # looks ok
summary(data$PCI.p) # looks ok
summary(data$PTS.p) # looks ok
summary(data$PTI.p) # looks ok
summary(data$PSI.p) # looks ok
summary(data$CTS.p) # looks ok
summary(data$CTI.p) # looks ok
summary(data$CSI.p) # looks ok
summary(data$TSI.p) # looks ok

summary(data$PCTS.p) # looks ok
summary(data$PCTI.p) # looks ok
summary(data$PCSI.p) # looks ok
summary(data$PTSI.p) # looks ok
summary(data$CTSI.p) # looks ok

summary(data$PCTSI.p) # looks ok

##################################
# Put into separate data sets
#################################

# Making subsets for each analysis
data1 <-  subset(data,data$transition==1)
data2 <-  subset(data,data$transition==0)
data3 <- subset(data,data$group == "Gender Nonconforming")

##################################
# Scaling variables for analyses
#################################

# Data set 1---------------------------

# Scaled so can be compared
data1$P.p.r <- (data1$P.p*(107-1) + 0.5)/107
data1$C.p.r <- (data1$C.p*(107-1) + 0.5)/107
data1$T.p.r <- (data1$T.p*(107-1) + 0.5)/107
data1$S.p.r <- (data1$S.p*(107-1) + 0.5)/107
data1$I.p.r <- (data1$I.p*(107-1) + 0.5)/107

data1$PC.p.r <- (data1$PC.p*(107-1) + 0.5)/107
data1$PT.p.r <- (data1$PT.p*(107-1) + 0.5)/107
data1$PS.p.r <- (data1$PS.p*(107-1) + 0.5)/107
data1$PI.p.r <- (data1$PI.p*(107-1) + 0.5)/107
data1$CT.p.r <- (data1$CT.p*(107-1) + 0.5)/107
data1$CS.p.r <- (data1$CS.p*(107-1) + 0.5)/107
data1$CI.p.r <- (data1$CI.p*(107-1) + 0.5)/107
data1$TS.p.r <- (data1$TS.p*(107-1) + 0.5)/107
data1$TI.p.r <- (data1$TI.p*(107-1) + 0.5)/107
data1$SI.p.r <- (data1$SI.p*(107-1) + 0.5)/107

data1$PCT.p.r <- (data1$PCT.p*(107-1) + 0.5)/107
data1$PCS.p.r <- (data1$PCS.p*(107-1) + 0.5)/107
data1$PCI.p.r <- (data1$PCI.p*(107-1) + 0.5)/107
data1$PTS.p.r <- (data1$PTS.p*(107-1) + 0.5)/107
data1$PTI.p.r <- (data1$PTI.p*(107-1) + 0.5)/107
data1$PSI.p.r <- (data1$PSI.p*(107-1) + 0.5)/107
data1$CTS.p.r <- (data1$CTS.p*(107-1) + 0.5)/107
data1$CTI.p.r <- (data1$CTI.p*(107-1) + 0.5)/107
data1$CSI.p.r <- (data1$CSI.p*(107-1) + 0.5)/107
data1$TSI.p.r <- (data1$TSI.p*(107-1) + 0.5)/107

data1$PCTS.p.r <- (data1$PCTS.p*(107-1) + 0.5)/107
data1$PCTI.p.r <- (data1$PCTI.p*(107-1) + 0.5)/107
data1$PCSI.p.r <- (data1$PCSI.p*(107-1) + 0.5)/107
data1$PTSI.p.r <- (data1$PTSI.p*(107-1) + 0.5)/107
data1$CTSI.p.r <- (data1$CTSI.p*(107-1) + 0.5)/107

data1$PCTSI.p.r <- (data1$PCTSI.p*(107-1) + 0.5)/107

data1$sex.z <- rescale(data1$sex)
data1$agemos.z <- rescale(data1$agemos)
data1$racecat.z <- rescale(data1$racecat)
data1$polid.z <- rescale(data1$polid)
data1$income.z <- rescale(data1$income)

# Data set 2---------------------------

# Scaled so can be compared
data2$P.p.r <- (data2$P.p*(147-1) + 0.5)/147
data2$C.p.r <- (data2$C.p*(147-1) + 0.5)/147
data2$T.p.r <- (data2$T.p*(147-1) + 0.5)/147
data2$S.p.r <- (data2$S.p*(147-1) + 0.5)/147
data2$I.p.r <- (data2$I.p*(147-1) + 0.5)/147

data2$PC.p.r <- (data2$PC.p*(147-1) + 0.5)/147
data2$PT.p.r <- (data2$PT.p*(147-1) + 0.5)/147
data2$PS.p.r <- (data2$PS.p*(147-1) + 0.5)/147
data2$PI.p.r <- (data2$PI.p*(147-1) + 0.5)/147
data2$CT.p.r <- (data2$CT.p*(147-1) + 0.5)/147
data2$CS.p.r <- (data2$CS.p*(147-1) + 0.5)/147
data2$CI.p.r <- (data2$CI.p*(147-1) + 0.5)/147
data2$TS.p.r <- (data2$TS.p*(147-1) + 0.5)/147
data2$TI.p.r <- (data2$TI.p*(147-1) + 0.5)/147
data2$SI.p.r <- (data2$SI.p*(147-1) + 0.5)/147

data2$PCT.p.r <- (data2$PCT.p*(147-1) + 0.5)/147
data2$PCS.p.r <- (data2$PCS.p*(147-1) + 0.5)/147
data2$PCI.p.r <- (data2$PCI.p*(147-1) + 0.5)/147
data2$PTS.p.r <- (data2$PTS.p*(147-1) + 0.5)/147
data2$PTI.p.r <- (data2$PTI.p*(147-1) + 0.5)/147
data2$PSI.p.r <- (data2$PSI.p*(147-1) + 0.5)/147
data2$CTS.p.r <- (data2$CTS.p*(147-1) + 0.5)/147
data2$CTI.p.r <- (data2$CTI.p*(147-1) + 0.5)/147
data2$CSI.p.r <- (data2$CSI.p*(147-1) + 0.5)/147
data2$TSI.p.r <- (data2$TSI.p*(147-1) + 0.5)/147

data2$PCTS.p.r <- (data2$PCTS.p*(147-1) + 0.5)/147
data2$PCTI.p.r <- (data2$PCTI.p*(147-1) + 0.5)/147
data2$PCSI.p.r <- (data2$PCSI.p*(147-1) + 0.5)/147
data2$PTSI.p.r <- (data2$PTSI.p*(147-1) + 0.5)/147
data2$CTSI.p.r <- (data2$CTSI.p*(147-1) + 0.5)/147

data2$PCTSI.p.r <- (data2$PCTSI.p*(147-1) + 0.5)/147

data2$sex.z <- rescale(data2$sex)
data2$agemos.z <- rescale(data2$agemos)
data2$racecat.z <- rescale(data2$racecat)
data2$polid.z <- rescale(data2$polid)
data2$income.z <- rescale(data2$income)

# Dataset 3 ---------------------------

# Scaled so can be compared
data3$P.z <- rescale(data3$P.p)
data3$C.z <- rescale(data3$C.p)
data3$T.z <- rescale(data3$T.p)
data3$S.z <- rescale(data3$S.p)
data3$I.z <- rescale(data3$I.p)

data3$PC.z <- rescale(data3$PC.p)
data3$PT.z <- rescale(data3$PT.p)
data3$PS.z <- rescale(data3$PS.p)
data3$PI.z <- rescale(data3$PI.p)
data3$CT.z <- rescale(data3$CT.p)
data3$CS.z <- rescale(data3$CS.p)
data3$CI.z <- rescale(data3$CI.p)
data3$TS.z <- rescale(data3$TS.p)
data3$TI.z <- rescale(data3$TI.p)
data3$SI.z <- rescale(data3$SI.p)

data3$PCT.z <- rescale(data3$PCT.p)
data3$PCS.z <- rescale(data3$PCS.p)
data3$PCI.z <- rescale(data3$PCI.p)
data3$PTS.z <- rescale(data3$PTS.p)
data3$PTI.z <- rescale(data3$PTI.p)
data3$PSI.z <- rescale(data3$PSI.p)
data3$CTS.z <- rescale(data3$CTS.p)
data3$CTI.z <- rescale(data3$CTI.p)
data3$CSI.z <- rescale(data3$CSI.p)
data3$TSI.z <- rescale(data3$TSI.p)

data3$PCTS.z <- rescale(data3$PCTS.p)
data3$PCTI.z <- rescale(data3$PCTI.p)
data3$PCSI.z <- rescale(data3$PCSI.p)
data3$PTSI.z <- rescale(data3$PTSI.p)
data3$CTSI.z <- rescale(data3$CTSI.p)

data3$PCTSI.z <- rescale(data3$PCTSI.p)

data3$sex.z <- rescale(data3$sex)
data3$agemos.z <- rescale(data3$agemos)
data3$racecat.z <- rescale(data3$racecat)
data3$MonthsTest.z <- rescale(data3$MonthsTest)
data3$polid.z <- rescale(data3$polid)
data3$income.z <- rescale(data3$income)

######################################
# Save output 
######################################

save(data1,file="data1.noimpute.rda")
save(data2,file="data2.noimpute.rda")
save(data3,file="data3.noimpute.rda")