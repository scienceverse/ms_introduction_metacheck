#LOAD PJ TRIMMING ROUTLINE

#...see van Selst and Jolicoeur (1994)
source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  

#...OPEN DATA
#...Set the directory where the data are located
setwd("~/Documents/Research/papers/in progress/StandingStroopPaper/RevisedSupplementalMaterials/standingStroopE02/")

#...open the data file
rData = read.table("Experiment02Data.txt",header=TRUE,sep="",stringsAsFactors = FALSE)
#...view the data to make sure that it openned correctly
View(rData)

#...check the structure of the data to check the variable names and
str(rData)

#...rename the varibles to be more consistent(ish) with Trent Studies
names(rData) = c("sjID",  #..subject ID for each participant.
                 "age",  #..age in years
                 "gender",  #..preferred gender assignment f= female, m=male, o = other
                 "cb",  #..counterbalance of posture order. 1 = start sitting, 2 = start standing
                 "phase",  #...block type e.g., standing / sitting
                 "trialNum",  #... the trial number in each block
                 "visualWordPairing",  #..the stimuli on each trial WORD-COLOUR
                 "word",  #..the word presented on the trial
                 "colour", #..the ink colour on the trial
                 "rgb",  #..the RGB values used for the ink colour
                 "congruency",  #...the congruency of the trial 
                 "RT",  #..the response time for the trial
                 "preference",  #..sitting vs. standing preference (in another file)
                 "ac")  #... the accuracy coding 0=incorrect, 1 = correct, 999 = mistrial


#....PREPARE DATA FOR ANALYSIS

#...get the total number of subjects

length(unique(rData$sjID))

# remove subject 55 & 121  because they read the word instead of naming the colour
rData = rData[rData$sjID!=55,]
rData = rData[rData$sjID!=121,]

#...get the number of remaining subjects
length(unique(rData$sjID))

#...remove the preference column
rData = rData[,-13]
#View(rData)


#...remove lines with phase = "Questionnaires". This line contained responses to preference of sitting or standing.
rData= rData[rData$phase !="Questionnaires",]

#...check that only practice and experimental trials are remaining
unique(rData$phase)  #... determine the unique values correctly associated with this variable


#...check accuracy coding
unique(rData$ac)  #... determine the unique values correctly associated with this variable

#...fix accurracy coding errors
rData$ac[rData$ac==9991]=999
rData$ac[rData$ac==11]=1
rData$ac[rData$ac==2]=1
rData$ac[is.na(rData$ac)] = 999

unique(rData$ac)  #... determine the unique values correctly associated with this variable

#...fix posture coding errors for subject 60
rData$posture = rData$phase  #...create new posture variable
rData$posture[rData$sjID==60 & rData$phase=="sitting"] ="standing"
rData$posture[rData$sjID==60 & rData$phase=="standing"] = "sitting"



#...add blockNumbers
rData$blockNum = 0
rData$blockNum[rData$cb==1 & rData$posture=="sitting"] = 1
rData$blockNum[rData$cb==1 & rData$posture=="standing"] = 2
rData$blockNum[rData$cb==2 & rData$posture=="sitting"] = 2
rData$blockNum[rData$cb==2 & rData$posture=="standing"] = 1


#...CLEAN UP THE RT VARIABLE
#...the RT values are bracketed.
#...some RT values on mistrials are coded as FALSE
#...remove brackets from RT
library(stringr)
rData$RT = str_remove(rData$RT,"\\[")
rData$RT = str_remove(rData$RT,"\\]")
#View(rData)  #...check the the RT values are correct


#...check RT data for commas 
library(stringr)
rData[str_detect(rData$RT,","),]
which(str_detect(rData$RT,","))

#...remove the rt values with commas
rData = rData[-28202,]
rData = rData[-25932,]
rData = rData[-19403,]
rData = rData[-18585,]
rData = rData[-4036,]

#...convert FALSE RTs to the minimum RT value - 200.  used to indicate early trigger of response key
rData$RT[rData$RT==FALSE]=200


#...check for missing data
rData[!complete.cases(rData),]


#...IDENTIFY MISTRIALS
#...definition:  999 = no recording of vocalization

#...get the number of mistrials for each participant
mistrialData = ddply(rData, 
                 .(sjID,cb,ac), 
                 summarise, 
                 num=length(ac))
#View(mistrialData)



#...convert the data from long format to wide format for easy reading
library(reshape2)
mistrialData = dcast(mistrialData, #the name of the dataframe you want to reshape
                 sjID+cb #row variables 
                 ~ac, #row variables ~ column variables
                 value.var = "num") 
#View(mistrialData)

#...rename the columns
names(mistrialData) <- c("sjID","cb","incorrect","correct","mistrial")

#...convert NA values to 0
mistrialData[is.na(mistrialData)] <- 0

#..add the total number of trials to the dataframe
mistrialData$totalTrials = mistrialData$incorrect+mistrialData$correct+mistrialData$mistrial

#...calculate the percentage mistrial for each participant
mistrialData$percentMistrial = round(((mistrialData$mistrial)/mistrialData$totalTrials)*100,3)
#View(mistrialData)

#...REMOVE SUBJECTS WITH TOO MANY MISTRIALS

#...set mistrial criterion (%)
mistrialCriterion = 20

#...Exclude participants with greater than 20% mistrials from hardware error
excludedSjIDs = mistrialData[mistrialData$percentMistrial>mistrialCriterion,]
View(excludedSjIDs)


#...return the number of subjects excluded due to mistrials
paste("The number of subjects excluded due to mistrial criterion:",dim(excludedSjIDs)[1]) #number excluded

#...Exclude the subjects exhibiting greater than 20% mistrials,this is for readability.
for(excludedSJcount in 1:dim(excludedSjIDs)[1]){
  rData = rData[rData$sjID!= excludedSjIDs$sjID[excludedSJcount],]
}

#...get number of remaining SJ
length(unique(rData$sjID))

#...remove practice Trials
unique(rData$phase)  #... determine the unique values correctly associated with this variable
rData= rData[rData$phase !="practice",]  #.. N.B., this only applies to sjID 2.1, 2.2
rData= rData[rData$phase !="practice_sitting",]
rData= rData[rData$phase !="practice_standing",]


#...Check that only experimental trials are remaining
unique(rData$phase)  #... determine the unique values correctly associated with this variable


#...check accuracy coding
unique(rData$ac)  #... determine the unique values correctly associated with this variable (1, 999, 0)

#...check the number of observations per subject
ftable(congruency+posture~sjID,data=rData)

#...count trials
totalTrials = dim(rData)[1]

#..count mistrials
rData = rData[rData$ac!="999",]
misTrials = dim(rData)[1]



#...ANALYZE THE ACCURACY DATA
library(plyr)
sjDataPE = ddply(rData, 
                 .(sjID,cb,congruency,posture,blockNum), 
                 summarise, 
                 meanPE = 100 - (mean(ac)*100))

View(sjDataPE)


###################################
#...ANALYZE RT DATA
###################################

#...select only correct trials
rData = rData[rData$ac ==1,]
errorsRemoved = dim(rData)[1]


#rData[rData$RT <=205,]
#...remove correct mistrials (RT <= 205)
rData$RT = as.numeric(rData$RT)
rData = rData[rData$RT >205,]
correctMistrialsRemoved = dim(rData)[1]

trimInfo = data.frame(totalTrials, misTrials, errorsRemoved,correctMistrialsRemoved)
head(trimInfo)


#...report %mistrials with hardware failure
((totalTrials-misTrials)/totalTrials)*100
paste("Trials removed due to hardware mistrials",round(((totalTrials-misTrials)/totalTrials)*100,3),"%")

#...report %trials removed due to errors
paste("Trials removed due to errors",round(((misTrials-errorsRemoved)/totalTrials)*100,3),"%")


#...mistrials due to early trigger
paste("Trials removed due to early trigger of voice key",round(((errorsRemoved-correctMistrialsRemoved)/totalTrials)*100,3),"%")

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

#...write the trimmed data
#write.table(trimmedData,"waterlooExperiment02TrimmedData.txt",row.names = FALSE,sep="")

#...GET THE CELL MEANS FOR EACH SUBJECT
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sjID, cb,congruency,posture,blockNum), 
                 summarise, 
                 meanRT = round(mean(RT),0))

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


##################################
#...RUN THE ANOVA
##################################

options(scipen= 999)

#...RUN THE FULL ANOVA
library(ez)
rtModel <- ezANOVA(sjData,
                   dv = .(meanRT),
                   wid=.(sjID),
                   within=.(congruency,posture), 
                   between=.(cb),
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

#...get the Bayes factor for the Null Interaction
bfValues[7]/bfValues[13]
1/(bfValues[7]/bfValues[13])


#...Calcualte the differences scores for the interaction
#... Used for the Cohen's D measure of effect size as reported by Rosenbaum et al.
#....Used for Violin Plots

library(reshape2)
wideRTData = dcast(sjData, #the name of the dataframe you want to reshape
                 sjID #row variables 
                 ~posture+congruency, #row variables ~ column variables
                 value.var = "meanRT") 
View(wideRTData)


#...get difference scores
wideRTData$sittingEffect = wideRTData$sitting_incongruent-wideRTData$sitting_congruent
wideRTData$standingEffect = wideRTData$standing_incongruent-wideRTData$standing_congruent
wideRTData$interaction = wideRTData$sittingEffect - wideRTData$standingEffect


mean(wideRTData$standingEffect)  #...stroop effect in the standing condition
mean(wideRTData$sittingEffect)  #...stroop effect in the sitting condition
(mean(wideRTData$standingEffect)+mean(wideRTData$sittingEffect))/2  #..average Stroop effect
sdValue = sd(wideRTData$interaction)
mValue = mean(wideRTData$interaction)
mValue
sdValue
d = mValue /sdValue

library(psych)

cohen.d.ci(d,n=108,n1=108,alpha=.05)






#...save the data for the violin plots
violinData = wideRTData[,c("sjID","interaction")]
violinData$experiment = "Experiment02"

#write.table(violinData,"exp02.violinData.txt",row.names = FALSE)



#...CREATE THE TABLE OF MEANS
#...USE THE LOFTUS AND MASSON CONFIDENCE INTERVALS
library(psych)
tableRT = describeBy(sjData$meanRT, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
head(tableRT)
tablePE = describeBy(sjData$meanPE, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
head(tablePE)

tableRT = tableRT[,c("group1","group2","group3","mean","se")]
tablePE = tablePE[,c("group1","group2","group3","mean","se")]

names(tableRT) = c("cb","posture","congruency","mean","se")
names(tablePE) = c("cb","posture","congruency","mean","se")
tableRT$se = sqrt((507.195)/length(unique(sjData$sjID)))  #...THE VALUE HERE IS THE MSE FROM THE INTERACTION
tablePE$se= sqrt((1.168)/length(unique(sjData$sjID))) #...THE VALUE HERE IS THE MSE FROM THE INTERACTION

critT = qt(p=.025,df=length(unique(sjData$sjID))-2,lower.tail =FALSE)

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

