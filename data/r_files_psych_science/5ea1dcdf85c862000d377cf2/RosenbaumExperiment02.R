#LOAD PJ TRIMMING ROUTLINE
#...see van Selst and Jolicoeur (1994)
source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  

#OPEN DATA
#...set the working directory
setwd("~/Documents/Research/papers/in progress/StandingStroopPaper/RosenbaumData/")

library(openxlsx)
rData = read.xlsx("Rosenbaum.raw_data_Stroop.xlsx",sheet ="Experiment 2")
View(rData)

#...practice trials already removed???

#...remove instruction screen
rData = rData[rData$Name!="SPACE",]

#...remove extra embedded headers
rData = rData[rData$Subj!="Subj",]
#View(rData)

#...remove blank lines
rData = rData[complete.cases(rData),]

#...code congruency
rData$congruency = "incongruent"
rData$congruency[rData$comdition=="uu"]="congruent"
rData$congruency[rData$comdition=="dd"]="congruent"

#...match subject and posture values
library(stringr)
rData$sjID = str_sub(rData$Subj,start=2,end=3)
rData$blockType = str_sub(rData$Subj,start=1,end=1)

rData$posture = "SITTING"
rData$posture[rData$blockType ==2]="STANDING"
str(rData)

#...convert stimulus types
rData$Subj = factor(rData$Subj)
rData$Cond = factor(rData$Cond)
rData$Block = as.numeric(rData$Block)
rData$Trial = as.numeric(rData$Trial)
rData$Correct = as.logical(rData$Correct)
rData$RT = as.numeric(rData$RT)
rData$sjID = factor(rData$sjID)
rData$posture = factor(rData$posture)
rData$congruency = factor(rData$congruency)
rData$blockType = factor(rData$blockType)

#...check equal N per condition
ftable(congruency+blockType~sjID,data=rData)

#...check for missing data
rData[!complete.cases(rData),]

#count trials
totalTrials = dim(rData)[1]

#...get error data

#...calculate accuracy
rData$ac = 0
rData$ac[rData$Correct] = 1 

#View(rData)
sjDataPE = ddply(rData, 
                     .(sjID,congruency,posture), 
                     summarise, 
                     meanPE = 100 - (mean(ac)*100))

head(sjDataPE)
#View(sjDataPE)

#...ANALYZE RT DATA
rData = rData[rData$ac ==1,]
errorsRemoved = dim(rData)[1]
trimInfo = data.frame(totalTrials, errorsRemoved)
head(trimInfo)

#...number of trials lost due to errors
((totalTrials-errorsRemoved)/totalTrials)*100

#get number of observations per condition
library(plyr)

ftable(posture~congruency, data=rData)

#...GET RT DATA
#...RUN TRIMMING PROCEDURE
tempList = pjRecursiveTrim2(rData, #...dataset
                            "RT", #...dependent variables
                            c("sjID", 
                              "congruency",
                              "posture")) #.independent variables

trimmedData=tempList[[1]]
totalN = tempList[[2]]
rejected = tempList[[3]]
percentTrimmed = tempList[[4]]
Ncells = tempList[[5]]

output.out= data.frame(totalN, rejected,percentTrimmed,Ncells)
head(output.out)
#write.table(output.out, "output.txt",row.names = FALSE)
write.table(trimmedData, "RosenbaumE02trimmedData.txt",row.names = FALSE)


#...Get the cell means for each subject
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sjID, congruency,posture), 
                 summarise, 
                 meanRT = mean(RT))
head(sjDataRT)

#...combine the RT and error data
sjData = cbind(sjDataRT,meanPE =sjDataPE$meanPE)
head(sjData)

#set as factors
str(sjData)
sjData$sjID = factor(sjData$sjID)

View(sjData)


#...RUN THE ANOVA

options(scipen= 999)
library(ez)
rtModel <- ezANOVA(sjData,
                   dv = .(meanRT),
                   wid=.(sjID),
                   within=.(posture,congruency), 
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
                   wid=.(sjID),
                   within=.(posture,congruency), 
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
bfValues = anovaBF(meanRT~congruency*posture+sjID,
                   data = sjData,
                   whichRandom = "sjID", 
                   method = "laplace")
bfValues

#...get the Bayes factor for the Null Interaction
bfValues[3]/bfValues[4]
1/(bfValues[3]/bfValues[4])



#...Calcualte the differences scores for the interaction
#... Used for the Cohen's D measure of effect size as reported by Rosenbaum et al.
#....Used for Violin Plots
#ACTUAL
library(reshape2)
wideData = dcast(sjData, #the name of the dataframe you want to reshape
                 sjID #row variables 
                 ~posture+congruency, #row variables ~ column variables
                 value.var = "meanRT") 
#View(wideData)


#...get difference scores

wideData$sittingEffect = wideData$SITTING_incongruent-wideData$SITTING_congruent
wideData$standingEffect = wideData$STANDING_incongruent-wideData$STANDING_congruent
wideData$interaction = wideData$sittingEffect - wideData$standingEffect

sdValue = sd(wideData$interaction)
mValue = mean(wideData$interaction)
d = mValue /sdValue

library(psych)

#...get the number of subjects
length(unique(sjData$sjID))

#...calculate effect size with confidence interval
cohen.d.ci(d,n=16,n1=16,alpha=.05)

####################################
#...THE CREATE THE TABLE OF MEANS
#...USE THE LOFTUS AND MASSON CONFIDENCE INTERVALS
####################################
library(psych)
tableRT = describeBy(sjData$meanRT, list(sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tableRT)
tablePE = describeBy(sjData$meanPE, list(sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tablePE)

tableRT = tableRT[,c("group1","group2","mean","se")]
tablePE = tablePE[,c("group1","group2","mean","se")]

names(tableRT) = c("posture","congruency","mean","se")
names(tablePE) = c("posture","congruency","mean","se")
tableRT$se = sqrt((3133.874)/length(unique(sjData$sjID)))  #...THE VALUE HERE IS THE MSE FROM THE INTERACTION
tablePE$se= sqrt((29.785)/length(unique(sjData$sjID))) #...THE VALUE HERE IS THE MSE FROM THE INTERACTION

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
                        cbind(sjData$posture ,sjData$congruency))
wwtrim(2,2,cleanFactors,tr=.2)
