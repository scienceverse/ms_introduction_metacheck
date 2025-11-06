#### ============================================================
## This file contains R code that was used to analyze the data for
##
## Salzar Kämpf, M., Liebermann, H., Kerschreiter, R., Krause, S., 
## Nestler, S. & Schmukle, S.C.(2017).
## Disentangling the Sources of Mimicry: Social Relations Analyses 
## of the Link Between Mimicry and Liking.
## This script file is licensed under a CC-BY 4.0 license. 
## For a human-readable summary of that license, 
## see http://creativecommons.org/licenses/by/4.0/
##
#### ============================================================
## !!!!!!!!!!!!!!!!!!!!
## Creating Data for Prediction Models
## !!!!!!!!!!!!!!!!!!!!

## Import Mimicry data

mimicry <- read.table("RawData_Mimicry.dat", header=T)

## Import Liking/Metaliking - T1 data and merge it with the mimicry data
 
act.lml <- read.table("ActorEffectsLikingMetaLikingT1.dat",header=T)
par.lml <- read.table("PartnerEffectsLikingMetaLikingT1.dat",header=T)
rel.lml <- read.table("RelationshipEffectsLikingMetaLikingT1.dat",header=T)

mimicry <- merge(mimicry,act.lml,by=c("Group","Actor"))
mimicry <- merge(mimicry,par.lml,by.x=c("Group","Actor"),by.y=c("Group","Partner"))
mimicry <- merge(mimicry,rel.lml,by=c("Group","Actor","Partner","Dyad"))
mimicry <- merge(mimicry,rel.lml,by.x=c("Group","Actor","Partner","Dyad"),by.y=c("Group","Partner","Actor","Dyad"))

## Select the important variables and rename them

mimicry <- mimicry[,c(1:6,7,9,11,13,8,10,12,14)]
names(mimicry)[7:14] <- c("lT1.AA","lT1.AP","lT1.Rel.intra","lT1.Rel.inter",
                          "mlT1.AA","mlT1.AP","mlT1.Rel.intra","mlT1.Rel.inter")


## Import Liking/Metaliking T2 data and merge it with the mimicry data

data <- read.table("RawData_LikingMetaLikingT2.dat", header=T)

# Merge with liking and meta-liking T1

data <- merge(data,act.lml,by=c("Group","Actor"))   
data <- merge(data,par.lml,by=c("Group","Partner")) 
data <- merge(data,rel.lml,by.x=c("Group","Dyad","Actor","Partner"),by.y=c("Group","Dyad","Actor","Partner")) 

# Merge this with the other dataframe

act.mim <- read.table("ActorEffectsMimicry.dat",header=T)
par.mim <- read.table("PartnerEffectsMimicry.dat",header=T)
rel.mim <- read.table("RelationshipEffectsMimicry.dat",header=T)

data <- merge(data,act.mim,by=c("Group","Actor"))   
names(data)[15] <- "mim.AA" 
data <- merge(data,act.mim,by.x=c("Group","Partner"),by.y=c("Group","Actor"))   
names(data)[16] <- "mim.PA" 
data <- merge(data,rel.mim,by.x=c("Group","Dyad","Actor","Partner"),by.y=c("Group","Dyad","Actor","Partner")) 
names(data)[17] <- "mim.Rel.intra" #Doing mimicry
data <- merge(data,rel.mim,by.x=c("Group","Dyad","Partner","Actor"),by.y=c("Group","Dyad","Actor","Partner")) 
names(data)[18] <- "mim.Rel.inter" #Being mimicked

data <- data[,c(1,4,3,2,5:18)]

## Final merging

data <- merge(data,mimicry,by=c("Group","Actor","Partner","Dyad","Occasion"))

## Save the data for the prediction models

write.table(data,"DataPredictionModels.dat",row.names=F,col.names=T)

##Please go on with the 5th R code 5.Data_Analysis_Prediction_Models_Liking.r for prediction models of Liking and mimicry.