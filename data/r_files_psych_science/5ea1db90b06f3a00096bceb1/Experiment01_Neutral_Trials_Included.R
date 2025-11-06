#LOAD PJ TRIMMING ROUTLINE

#...see van Selst and Jolicoeur (1994)
source("//Users/michael/Desktop/rFolder/pjTrimmingV2.R")  


#...OPEN DATA
#...set the working directory
setwd("~\Psych Science\\Final Materials submitted to Psych Science\\April 23rd\\Supplemental Materials\\Experiment 1\\")


#...open the concatenated data
rData = read.delim("Experiment01Data.txt",
                   header=TRUE,
                   sep = "",
                   stringsAsFactors = FALSE)

#...check that the data opened correctly
View(rData)

#...check variable names and coding
str(rData)

#...change the variable names to be consistent(ish) with Trent Studies
names(rData) = c("sjID",
                 "age",
                 "gender",
                 "cb",
                 "phase",
                 "trialNum",
                 "stimulus",
                 "congruency",
                 "targetColour",
                 "RT",
                 "ac"
                 )

#...EXAMINE MISTRIALS...DUE TO PROBLEMS RECORDING VOCAL RESPONSES
library(plyr)

#...calculate the number of mistrials per SJ
#...this returns the number of correct (1), incorrect(0) and mistrials(999)
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
#... total number of subjects
length(unique(rData$sjID))
#...set mistrial criterion (%)
mistrialCriterion = 20

#...Exclude participants with greater than 20% mistrials from hardware error
excludedSjIDs = mistrialData[mistrialData$percentMistrial>mistrialCriterion,]
View(excludedSjIDs)


#...return the number of subjects excluded due to mistrials
paste("The number of subjects excluded due to mistrial criterion:",dim(excludedSjIDs)[1]) #number excluded

#...Exclude the subjects exhibiting greater than 20% mistrials for readability.
for(excludedSJcount in 1:dim(excludedSjIDs)[1]){
  rData = rData[rData$sjID!= excludedSjIDs$sjID[excludedSJcount],]
}

#...Remove practice Trials
unique(rData$phase) #... determine the unique values correctly associated with this variable
rData= rData[rData$phase !="practice",]

#...Check that only experimental trials are remaining
rData = droplevels(rData)
unique(rData$phase)  #... determine the unique values correctly associated with this variable


#...check accuracy coding
unique(rData$ac)  #... determine the unique values correctly associated with this variable (1, 999, 0)

#...Correct counterbalance coding error
#...Recode Posture for counterbalance 1
rData$posture = rData$phase
rData$posture[rData$cb==1 & rData$phase=="sitting"] ="standing"
rData$posture[rData$cb==1 & rData$phase=="standing"] = "sitting"


#...add blockNumbers
rData$blockNum = 0
rData$blockNum[rData$cb==1 & rData$posture=="sitting"] = 1
rData$blockNum[rData$cb==1 & rData$posture=="standing"] = 2
rData$blockNum[rData$cb==2 & rData$posture=="sitting"] = 2
rData$blockNum[rData$cb==2 & rData$posture=="standing"] = 1


#...check the number of observations per subject
ftable(posture+congruency~sjID,rData)

#...INITIAL CLEAN UP DATA

#...CLEAN UP THE RT VARIABLE
#...check RT data for commas
library(stringr)
rData[str_detect(rData$RT,","),]
which(str_detect(rData$RT,","))

# remove the rt values with commas
rData = rData[-22072,]
rData = rData[-11739,]
rData = rData[-10282,]
rData = rData[-1100,]

#...FALSE VALUES FOUND IN EXPERIMENT 1
#...HARDWARE CODING OF VOCALIZATIONs LEAD TO MISTRIALS
#...definition:  999 = no recording of vocalization
rData[str_detect(rData$RT,"FALSE"),]
which(str_detect(rData$RT,"FALSE"))

#...convert FALSE RTs to the minimum RT value - 200.  used to indicate early trigger of response key
rData$RT[rData$RT==FALSE]=200
length(rData$RT[rData$RT<205]) #166 scores
rData$RT=as.numeric(rData$RT)
#...check for missing data
rData[!complete.cases(rData),]


#...count trials
totalTrials = dim(rData)[1]

#...count misTrials
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
paste("Trials removed due to hardware mistrials",round(((totalTrials-misTrials)/totalTrials)*100,3),"%")

#...report %trials removed due to errors
paste("Trials removed due to errors",round(((misTrials-errorsRemoved)/totalTrials)*100,3),"%")

#...mistrials due to early trigger
paste("Trials removed due to early trigger of voice key",round(((errorsRemoved-correctMistrialsRemoved)/totalTrials)*100,3),"%")




#get number of observations per condition
library(plyr)

ftable(posture+congruency~sjID, data=rData)



##################################
#...RUN PJ TRIMMING PROCEDURE
##################################
tempList = pjRecursiveTrim2(rData, #...dataset
                            "RT", #...dependent variables
                            c("sjID",
                              "cb",
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
#write.table(trimmedData, "WaterlooExperiment01trimmedData.txt",row.names = FALSE)


#...GET THE CELL MEANS FOR EACH SUBJECT
library(plyr)
sjDataRT = ddply(trimmedData, 
                 .(sjID, cb,congruency,posture,blockNum), 
                 summarise, 
                 meanRT = mean(RT))

#...COMBINE THE RT AND THE ERROR
sjData = cbind(sjDataRT,meanPE =sjDataPE$meanPE)

#total number of subject used in Analysis 
length(unique(sjData$sjID))
#set as factors
str(sjData)
sjData$sjID = factor(sjData$sjID)
sjData$blockNum = factor(sjData$blockNum)
sjData$posture = factor(sjData$posture)
sjData$congruency = factor(sjData$congruency)
sjData$cb = factor(sjData$cb)


########################
#...RUN THE ANOVA
########################
options(scipen= 999)

#...RUN THE FULL ANOVA
library(ez)
rtModel <- ezANOVA(sjData,
                   dv = .(meanRT),
                   wid=.(sjID),
                   between = .(cb),
                   within=.(congruency,posture), 
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



#...THE REST IS FOR THE TABLE
library(psych)
tableRT = describeBy(sjData$meanRT, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
head(tableRT)
tablePE = describeBy(sjData$meanPE, list(sjData$cb,sjData$posture,sjData$congruency), mat=TRUE, digits = 1)
head(tablePE)

tableRT = tableRT[,c("group1","group2","group3","mean","se")]
tablePE = tablePE[,c("group1","group2","group3","mean","se")]

names(tableRT) = c("cb","posture","congruency","mean","se")
names(tablePE) = c("cb","posture","congruency","mean","se")
tableRT$se = sqrt((679.41)/length(unique(sjData$sjID)))
tablePE$se= sqrt((3.213)/length(unique(sjData$sjID)))

critT = qt(p=.025,df=length(unique(sjData$sjID))-2,lower.tail =FALSE)

tableRT$posture = str_to_upper(tableRT$posture)
#---add the min and max for the confidence intervals
tableRT$min = tableRT$mean - (tableRT$se*critT)
tableRT$max = tableRT$mean + (tableRT$se*critT)

####GET AC DATA FROM twoAnimalWordsPRPac.R
tableRT$ac = paste("(",format(round(tablePE$mean,digits=1),nsmall = 1),")",sep="")
head(tableRT, dim(tableRT)[1])




##########################################
#...FACILITATION EFFECT
##########################################
facilitation_sjData= sjData[sjData$congruency!="incongruent",]
str(facilitation_sjData)
facilitation_sjData = droplevels(facilitation_sjData)

library(ez)
rtModel <- ezANOVA(facilitation_sjData,
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
peModel <- ezANOVA(facilitation_sjData, 
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

##########################################
#...INTERFERENCE EFFECT
##########################################
interference_sjData= sjData[sjData$congruency!="congruent",]
str(interference_sjData)
interference_sjData = droplevels(interference_sjData)

library(ez)
rtModel <- ezANOVA(interference_sjData,
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
peModel <- ezANOVA(interference_sjData, 
                   dv = .(meanPE),
                   wid=.(sjID),
                   within=.(posture,congruency), 
                   between= .(cb),
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
