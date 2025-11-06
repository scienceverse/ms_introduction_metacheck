
#LOAD PJ TRIMMING ROUTLINE
#...see van Selst and Jolicoeur (1994)
source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  

#...open the data file
#...set the working directory
setwd("~/Documents/Research/papers/in progress/StandingStroopPaper/RevisedSupplementalMaterials/standingStroopE03/")

#...open the concatenated data
mergedDataSet = read.delim("Experiment03Data.txt",header=TRUE,sep="")

#...check that the data opened correctly
View(mergedDataSet)

#check variable coding
str(mergedDataSet)


#...remove subject who responded to word instead of colour
mergedDataSet = mergedDataSet[mergedDataSet$sj!=6,]

#...get number of subjects
length(unique(mergedDataSet$sj))

#...check for missing data
mergedDataSet[!complete.cases(mergedDataSet),]


#...remove practice trials
mergedDataSet <- mergedDataSet[!mergedDataSet$blockType=="practice",]
mergedDataSet <- mergedDataSet[mergedDataSet$blockType=="experimental",]


#count trials
totalTrials = dim(mergedDataSet)[1]

#...get error data
sjDataPE = ddply(mergedDataSet, 
                     .(sj, cb,blockNumber,age, gender,handedness,congruency,posture), 
                     summarise, 
                     meanPE = 100 - (mean(ac)*100))

head(sjDataPE)



#...ANALYZE RT DATA
mergedDataSet = mergedDataSet[mergedDataSet$ac ==1,] #...select only correct trials
errorsRemoved = dim(mergedDataSet)[1]


trimInfo = data.frame(totalTrials, errorsRemoved)
head(trimInfo)

#...report %trials removed due to errors
paste("Trials removed due to errors",round(((totalTrials-errorsRemoved)/totalTrials)*100,3),"%")


library(plyr)

#...GET RT DATA
#...RUN TRIMMING PROCEDURE
tempList = pjRecursiveTrim2(mergedDataSet, #...dataset
                            "RT", #...dependent variables
                            c("sj", #..subject ID for each participant.
                              "cb", #..counterbalance of posture order. 1 = start sitting, 2 = start standing
                              "age", #..age in years
                              "gender", #..preferred gender assignment f= female, m=male, o = other
                              "handedness", # dominant hand of the particpiant
                              "blockNumber", # The position the participant started in
                              "congruency",#...the congruency of the trial
                              "posture")) #.independent variables

trimmedData=tempList[[1]]
totalN = tempList[[2]]
rejected = tempList[[3]]
percentTrimmed = tempList[[4]]
Ncells = tempList[[5]]

output.out= data.frame(totalN, rejected,percentTrimmed,Ncells)
head(output.out)
#write.table(trimmedData, "TrentExperiment03.trimmedData.txt",row.names = FALSE)


#...Get the cell means for each subject
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sj, cb,blockNumber,age, gender,handedness,congruency,posture), 
                 summarise, 
                 meanRT = mean(RT))

#...combine the RT and error data
sjData = cbind(sjDataRT,meanPE =sjDataPE$meanPE)

head(sjData)

#set as factors
str(sjData)
sjData$sj = factor(sjData$sj)
sjData$cb = factor(sjData$cb)
sjData$blockNumber = factor(sjData$blockNumber)


####################################
#...RUN THE ANOVA
####################################

options(scipen= 999)
library(ez)
rtModel <- ezANOVA(sjData, 
                   dv = .(meanRT),
                   wid=.(sj),
                   within=.(posture,congruency), 
                   between = .(cb),
                   detailed=TRUE, 
                   type=3,
                   return_aov=TRUE)

#...print ANOVA in nice format
paste(rtModel$ANOVA$Effect,":  F(",
      rtModel$ANOVA$DFn,
      ", ",
      rtModel$ANOVA$DFd,
      ") = ",
      round(rtModel$ANOVA$F,3),
      ", MSE = ",
      round(rtModel$ANOVA$SSd/rtModel$ANOVA$DFd,3),
      ", p = ",
      round(rtModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(rtModel$ANOVA$SSn/(rtModel$ANOVA$SSn+rtModel$ANOVA$SSd),4),sep="")



#...ERROR ANALYSIS
peModel <- ezANOVA(sjData, 
                   dv = .(meanPE),
                   wid=.(sj),
                   within=.(posture,congruency), 
                   between = .(cb),
                   detailed=TRUE, 
                   type=3,
                   return_aov = TRUE)

#...print ANOVA in nice format
paste(peModel$ANOVA$Effect,":  F(",
      peModel$ANOVA$DFn,
      ", ",
      peModel$ANOVA$DFd,
      ") = ",
      round(peModel$ANOVA$F,3),
      ", MSE = ",
      round(peModel$ANOVA$SSd/peModel$ANOVA$DFd,3),
      ", p = ",
      round(peModel$ANOVA$p,3),
      ", partialEtaSq = ",
      round(peModel$ANOVA$SSn/(peModel$ANOVA$SSn+peModel$ANOVA$SSd),4),sep="")


#...CALCULATE THE BAYES FACTORS FOR THE RT ANALYSIS
library(BayesFactor)
bfValues = anovaBF(meanRT~cb*congruency*posture+sj,
                   data = sjData, 
                   whichRandom = "sj", 
                   method = "laplace")
bfValues

#...get the Bayes factor for the Null Interaction
bfValues[7]/bfValues[13]
1/(bfValues[7]/bfValues[13])


#...Calcualte the differences scores for the interaction
#... Used for the Cohen's D measure of effect size as reported by Rosenbaum et al.
#....Used for Violin Plots

library(reshape2)
wideRTData = dcast(sjData, #the name of the dataframe you want to reshape
                 sj+cb+gender+handedness #row variables 
                 ~posture+congruency, #row variables ~ column variables
                 value.var = "meanRT") 
View(wideRTData)


#...get difference scores

wideRTData$sittingEffect = wideRTData$SITTING_incongruent-wideRTData$SITTING_congruent
wideRTData$standingEffect = wideRTData$STANDING_incongruent-wideRTData$STANDING_congruent
wideRTData$interaction = wideRTData$sittingEffect - wideRTData$standingEffect

sdValue = sd(wideRTData$interaction)
mValue = mean(wideRTData$interaction)
mValue
sdValue
d = mValue /sdValue

library(psych)
length(unique(sjData$sj))
cohen.d.ci(d,n=98,n1=98,alpha=.05)


#...save the data for the violin plots
violinData = wideRTData[,c("sj","interaction")]
violinData$experiment = "Experiment03"
names(violinData) = c("sjID","interaction","experiment")

write.table(violinData,"Exp03.violinData.txt",row.names = FALSE)



####################################
#...THE CREATE THE TABLE OF MEANS
#...USE THE LOFTUS AND MASSON CONFIDENCE INTERVALS
####################################
library(psych)
tableRT = describeBy(sjData$meanRT, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tableRT)
tablePE = describeBy(sjData$meanPE, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tablePE)

tableRT = tableRT[,c("group1","group2","group3","mean","se")]
tablePE = tablePE[,c("group1","group2","group3","mean","se")]

names(tableRT) = c("cb","posture","congruency","mean","se")
names(tablePE) = c("cb","posture","congruency","mean","se")
tableRT$se = sqrt((4769.204)/length(unique(sjData$sj)))  #...THE VALUE HERE IS THE MSE FROM THE INTERACTION
tablePE$se= sqrt((9.793)/length(unique(sjData$sj))) #...THE VALUE HERE IS THE MSE FROM THE INTERACTION

critT = qt(p=.025,df=length(unique(sjData$sj))-2,lower.tail =FALSE)

tableRT$posture = str_to_upper(tableRT$posture)
#---add the min and max for the confidence intervals
tableRT$min = tableRT$mean - (tableRT$se*critT)
tableRT$max = tableRT$mean + (tableRT$se*critT)

####GET AC DATA FROM twoAnimalWordsPRPac.R
tableRT$ac = paste("(",format(round(tablePE$mean,digits=1),nsmall = 1),")",sep="")
head(tableRT, dim(tableRT)[1])






####################################
#....CALCULATE ROBUST ANOVA
#....WITH WINDSORIZED MEANS
####################################

source("//Users/michael/Desktop/rFolder/rallfun-v25.txt")

cleanFactors = fac2list(sjData$meanRT, 
                        cbind(sjData$cb,sjData$posture ,sjData$congruency))
bwwtrim(2,2,2,cleanFactors,tr=.2)



