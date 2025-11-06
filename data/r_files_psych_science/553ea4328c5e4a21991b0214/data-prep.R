########## RISEN & GILOVICH REPLICATION - Data Cleaning ##########

### Input data file d1 was in its original format (as downloaded from Qualtrics) except that the headers were renamed as in the second step below. 

#read in raw data
d1 = read.csv(file = "data_prepped_1.csv", header = TRUE)

#put in relabeled data frame
d2 = data.frame("WorkerID" = d1$mtwid, "ValidCode" = d1$mTurkCode, "Load" = d1$Cognitive.load, "Had.read" = d1$Had.read, "Likelihood1" = d1$Likelihood1, "End.num1" = d1$End.num1, "End.num2" = d1$End.num2, "Effort1" = d1$Effort1, "Effort2" = d1$Effort2, "Likelihood2" = d1$Likelihood2, "Likelihood3" = d1$Likelihood3, "Likelihood4" = d1$Likelihood4)

#merge the 4 likelihood variables into one vector
Likelihood.merged = d2$Likelihood1
ind = is.na(d2$Likelihood1) & !is.na(d2$Likelihood2)
Likelihood.merged[ind] = d2$Likelihood2[ind]
ind = is.na(Likelihood.merged) & !is.na(d2$Likelihood3)
Likelihood.merged[ind] = d2$Likelihood3[ind]
ind = is.na(Likelihood.merged) & !is.na(d2$Likelihood4)
Likelihood.merged[ind] = d2$Likelihood4[ind]

#merge end number variables
End.num.merged = d2$End.num1
ind = is.na(d2$End.num1) & !is.na(d2$End.num2)
End.num.merged[ind] = d2$End.num2[ind]

#merge effort variables
Effort.merged = d2$Effort1
ind = is.na(d2$Effort1) & !is.na(d2$Effort2)
Effort.merged[ind] = d2$Effort2[ind]

#dataset with merged variables
d3 = data.frame("WorkerID" = d2$WorkerID, "ValidCode" = d2$ValidCode, "Load" = d2$Load, "Had.read" = d2$Had.read, "Likelihood" = Likelihood.merged, "End.num" = End.num.merged, "Effort" = Effort.merged)

#check to see if there are any unexpected NA values
subset(d3, is.na(d3$Likelihood)) #should have no NA values
subset(d3, is.na(d3$End.num))  #should be NA only for load=0 people
subset(d3, is.na(d3$Effort))  #should be NA only for load=0 people

#exclude people with end number > 561 and/or effort = 0
subset(d3, d3$Effort == 0) #who has effort = 0?
subset(d3, d3$End.num > 561) #who ended on number > 561?
d4 = subset( d3, ( d3$End.num < 562 | is.na(d3$End.num) ) & ( d3$Effort > 0 | is.na(d3$Effort) ) )

#see if any repeated responses
d3$WorkerID
duplicated(d3$WorkerID)
#problem: many people have blank ID or 999999

#export prepped data into csv
write.csv(d4, file = "data_prepped_2.csv")
