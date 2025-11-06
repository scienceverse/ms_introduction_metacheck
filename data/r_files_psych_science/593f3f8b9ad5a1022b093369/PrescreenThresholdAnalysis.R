library(plyr)
library(lmSupport)

# load the data
screeningData = read.csv('VTlogsall.csv', header=TRUE) 

# recode learning condition to be centered around zero so that Comprehension = -0.5 and Production = 0.5
screeningData$learningCondition <- varRecode(screeningData$conditionnr,c(1,2), c(-0.5,0.5))

# summarize how many trials each participant got correct
screeningDataSummary <- ddply(screeningData,~subjectnr + learningCondition,summarise,sumCorrectScreening=sum(correctanswer))

# look at the mean presceening test score by learning condition
varDescribeBy(screeningDataSummary$sumCorrectScreening, screeningDataSummary$learningCondition)

# regress prethreshold test scores on learning condition to see whether the mean prescreen scores differ by condition
mPreThreshold = lm(sumCorrectScreening ~ learningCondition, data = screeningDataSummary) 
modelSummary(mPreThreshold)

# record in a new variable whether a partcipants data is usable or not (1 = usable, so score > 14 on threshold test)
screeningDataSummary$usable = 0
screeningDataSummary$usable[screeningDataSummary$sumCorrectScreening > 14] = 1

# get a frequency table for usable/unusable by learning condition and run a chi square test on it
freqTable = table(screeningDataSummary$usable, screeningDataSummary$learningCondition)
chisq.test(freqTable)

# now make a new dataframe with only usable partcipants
screeningDataUsable <- subset(screeningDataSummary, usable ==1)

# look at the mean screening test score by learning condition again
varDescribeBy(screeningDataUsable$sumCorrectScreening, screeningDataUsable$learningCondition)

# regress post threshold test scores on learning condition to see whether this differs
mPostThreshold = lm(sumCorrectScreening ~ learningCondition, data = screeningDataUsable) 
modelSummary(mPostThreshold) 