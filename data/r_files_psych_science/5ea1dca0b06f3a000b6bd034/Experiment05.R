#LOAD PJ TRIMMING ROUTLINE

#...see van Selst and Jolicoeur (1994)
source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  

#...OPEN DATA
#...Set the directory where the data are located
setwd("~/Documents/Research/papers/in progress/StandingStroopPaper/WaterlooStroop05/")

#...open the concatenated data
rData = read.table("Experiment05Data.txt",header = TRUE,stringsAsFactors = FALSE,sep = "\t")

#...check that the data opened correctly
View(rData)



#...VARIABLE NAMES
# names(rData) = c("sjID",  #..subject ID for each participant.
#                  "cb",  #..counterbalance of posture order. 1 = start sitting, 2 start standing
#                  "age",  #..age in years
#                  "gender",  #..preferred gender assignment f= female, m=male, o = other
#                  "blockList",  #...cb again
#                  "cbDefinition", #...block type e.g., sit2Stand vs. stand2sit
#                  "trialNum",  #... the trial number in each block
#                  "rgb", #..the RGB values used for the ink colour               
#                  "congruency",  #...the congruency of the trial
#                  "Accuracy",
#                  "posture",  #...sitting vs. standing
#                  "RT",  #..the response time for the trial
#                  "Resp",
#                  "word",  #the word stimulus shown on the trial
#                  "blockNum") #...the block number


#check variable coding
str(rData)

#...get the number of subjects
length(unique(rData$sjID))


#...check number of observations
ftable(congruency~sjID, data=rData)  

#....PREPARE DATA FOR ANALYSIS

# remove bad subjects 
rData = rData[rData$sjID!= 5,]  #the data file did not save, there is no file 
rData = rData[rData$sjID!= 12,]  #Said the word instead of the colour in block 1
rData = rData[rData$sjID!= 15,]  #the data file did not save, there is no file 
rData = rData[rData$sjID!= 17,]  #the data file did not save, there is no file 
rData = rData[rData$sjID!= 51,]  #Said the word instead of the colour in block 1



#. To match experiments 1 & 2 recode any vocal RTs faster than or equal to 200 ms as a mistrial.
rData$Accuracy[rData$RT<=200] = 999

#...check accuracy coding
unique(rData$Accuracy)  #... determine the unique values crorectly associated with this variable

#####################################################
#...CHECK THE MISTRIAL DATA
#####################################################
#...get the number of mistrials for each participant
library(plyr)
mistrialData = ddply(rData, 
                     .(sjID,cb,Accuracy), 
                     summarise, 
                     num=length(Accuracy))

#...convert the data from long format to wide format for easy reading
library(reshape2)
mistrialData = dcast(mistrialData, #the name of the dataframe you want to reshape
                     sjID+cb #row variables 
                     ~Accuracy, #row variables ~ column variables
                     value.var = "num") 

#...rename the columns
names(mistrialData) <- c("sjID","cb","incorrect","correct","mistrial")

#...convert NA values to 0
mistrialData[is.na(mistrialData)] <- 0

#..add the total number of trials to the data.drame
mistrialData$totalTrials = mistrialData$incorrect+mistrialData$correct+mistrialData$mistrial

#...calculate the percentage mistrial for each participant
mistrialData$percentMistrial = round(((mistrialData$mistrial)/mistrialData$totalTrials)*100,3)

View(mistrialData)



#...REMOVE SUBJECTS WITH TOO MANY MISTRIALS
#...set mistrial criterion (%)
mistrialCriterion = 20

#...Exclude participants with greater than 10% mistrials from hardware error
excludedSjIDs = mistrialData[mistrialData$percentMistrial>mistrialCriterion,]
View(excludedSjIDs)


#...return the number of subjects excluded due to mistrials
paste("The number of subjects excluded due to mistrial criterion:",dim(excludedSjIDs)[1]) #number excluded

#...Exclude the subjects exhibiting greater than 20% mistrials, this for readability.
for(excludedSJcount in 1:dim(excludedSjIDs)[1]){
  rData = rData[rData$sjID!= excludedSjIDs$sjID[excludedSJcount],]
}



#...get the number of remaining subjects
length(unique(rData$sjID))



#...check for missing data
rData[!complete.cases(rData),]



totalTrials = dim(rData)[1]   #...get the total number of experimental trials

rData = rData[rData$Accuracy!=999,] #....Remove mistrials

missingTrials = dim(rData)[1]  #...get the number of remaining trials after removing mistrials


###################################
#...CALCULATE THE PE MEANS
###################################

sjDataPE = ddply(rData, 
                 .(sjID, cb,congruency,posture,blockNum), 
                 summarise, 
                 meanPE = round(mean(1-Accuracy)*100,2))


###################################
#...CALCULATE THE RT MEANS
###################################



rData = rData[rData$Accuracy ==1,]  #...select only correct trials
errorsRemoved = dim(rData)[1]  #...get the number of remaining trials after removing ERRORS


trimInfo = data.frame(totalTrials, missingTrials,errorsRemoved)  #combine the trial counts into one variable
head(trimInfo)


#...report %mistrials with hardware failure
#((totalTrials-misTrials)/totalTrials)*100
paste("Trials removed due to hardware mistrials",round(((totalTrials-missingTrials)/totalTrials)*100,3),"%")

#...report %trials removed due to errors
paste("Trials removed due to errors",round(((missingTrials-errorsRemoved)/totalTrials)*100,3),"%")





##################################
#...RUN PJ TRIMMING PROCEDURE
##################################
tempList = pjRecursiveTrim2(rData, #...dataset
                            "RT", #...dependent variables
                            c("sjID","cb",
                              "congruency",
                              "posture","blockNum")) #.independent variables

trimmedData=tempList[[1]]
totalN = tempList[[2]]
rejected = tempList[[3]]
percentTrimmed = tempList[[4]]
Ncells = tempList[[5]]

output.out= data.frame(totalN, rejected,percentTrimmed,Ncells)
head(output.out)
#write.table(output.out, "output.txt",row.names = FALSE)
#write.table(trimmedData, "Experiment05trimmedData.txt",row.names = FALSE)


#...GET THE CELL MEANS FOR EACH SUBJECT
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sjID, cb,congruency,posture,blockNum), 
                 summarise, 
                 meanRT = round(mean(RT),0))

head(sjDataRT)
#View(sjDataRT)
head(sjDataPE)


#...combine the RT and error data
sjData = cbind(sjDataRT,meanPE =sjDataPE$meanPE)

head(sjData)

#...get the total number of subjects analyzed
length(unique(rData$sjID))
#...Recode variable types
str(sjData)
sjData$sjID = factor(sjData$sjID)
sjData$cb = factor(sjData$cb)
sjData$congruency = factor(sjData$congruency)
sjData$posture = factor(sjData$posture)
sjData$blockNum = factor(sjData$blockNum)

View(sjData)

summary(sjData)


##################################
#...GET MEANS FOR WRITEUP
##################################

#...RUN THE ANOVA
options(scipen= 999)

#...RUN THE FULL ANOVA
library(ez)
rtModel <- ezANOVA(sjData,
                   dv = .(meanRT),
                   wid=.(sjID),
                   within=.(congruency,posture), 
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
                   wid=.(sjID),
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
bfValues = anovaBF(meanRT~cb*congruency*posture+sjID,
                   data = sjData,
                   whichRandom = "sjID", 
                   method="laplace")
bfValues
warnings()
#...get the Bayes factor for the Null Interaction
bfValues[7]/bfValues[13]
1/(bfValues[7]/bfValues[13])


#...Calculate the differences scores for the interaction
#... Used for the Cohen's D measure of effect size as reported by Rosenbaum et al.
#....Used for Violin Plots

library(reshape2)
wideRTData = dcast(sjData, #the name of the dataframe you want to reshape
                 sjID+cb #row variables 
                 ~posture+congruency, #row variables ~ column variables
                 value.var = "meanRT") 
View(wideRTData)


#...get difference scores

wideRTData$sittingEffect = wideRTData$sitting_inc-wideRTData$sitting_con
wideRTData$standingEffect = wideRTData$standing_inc-wideRTData$standing_con
wideRTData$interaction = wideRTData$sittingEffect - wideRTData$standingEffect

mean(wideRTData$standingEffect)
mean(wideRTData$sittingEffect)
(mean(wideRTData$standingEffect)+mean(wideRTData$sittingEffect))/2
sdValue = sd(wideRTData$interaction)
mValue = mean(wideRTData$interaction)
mValue
sdValue
d = mValue /sdValue


library(psych)

cohen.d.ci(d,n=51,n1=51,alpha=.05)



#...save the data for the violin plots
violinData = wideRTData[,c("sjID","interaction")]
violinData$experiment = "Experiment05"

write.table(violinData,"exp05.violinData.txt",row.names = FALSE)


####################################
#...THE CREATE THE TABLE OF MEANS
#...USE THE LOFTUS AND MASSON CONFIDENCE INTERVALS
####################################

#...THE REST IS FOR THE TABLE WITH COUNTERBALANCE
library(psych)
tableRT = describeBy(sjData$meanRT, list(sjData$blockNum,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tableRT)
tablePE = describeBy(sjData$meanPE, list(sjData$blockNum,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
#head(tablePE)

tableRT = tableRT[,c("group1","group2","group3","mean","se")]
tablePE = tablePE[,c("group1","group2","group3","mean","se")]

names(tableRT) = c("cb","posture","congruency","mean","se")
names(tablePE) = c("cb","posture","congruency","mean","se")

tableRT$se = sqrt((674.819)/length(unique(sjData$sjID)))
tablePE$se= sqrt((9.166)/length(unique(sjData$sjID)))

critT = qt(p=.025,df=length(unique(sjData$sjID))-2,lower.tail =FALSE)

#---add the min and max for the confidence intervals
tableRT$min = tableRT$mean - (tableRT$se*critT)
tableRT$max = tableRT$mean + (tableRT$se*critT)

####GET AC DATA FROM twoAnimalWordsPRPac.R
tableRT$ac = paste("(",format(round(tablePE$mean,digits=1),nsmall = 1),")",sep="")
head(tableRT)



####################################
#....CALCULATE ROBUST ANOVA
#....WITH WINDSORIZED MEANS
####################################
source("//Users/michael/Desktop/rFolder/rallfun-v25.txt")

cleanFactors = fac2list(sjData$meanRT, 
                        cbind(sjData$cb,sjData$posture ,sjData$congruency))
bwwtrim(2,2,2,cleanFactors,tr=.2)




