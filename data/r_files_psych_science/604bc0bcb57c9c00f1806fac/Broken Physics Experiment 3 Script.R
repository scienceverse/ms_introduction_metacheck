
# Broken Physics: A Conjunction-Fallacy Effect in Intuitive Physical Reasoning
# Ethan Ludwin-Peery, Neil R. Bramley, Ernest Davis, and Todd M. Gureckis

# Script for Experiment 3

if (!require(BayesianFirstAid)) {install.packages("BayesianFirstAid"); require(BayesianFirstAid)}

column.greppr = function(DATA, GREP){
  return(DATA[, grep(GREP, names(DATA))])
}

physicalCF3 = read.csv(file.choose()) # Import the data for Experiment 3

physicalCF3.1 = physicalCF3 # Makes a copy of the original that won't be affected by exclusions



##### Exclusions ------------------------------------------------------------------------

physicalCF3.1 = physicalCF3.1[physicalCF3.1$Finished == 1,] # Gotta have finished it! 

physicalCF3.1 = physicalCF3.1[physicalCF3.1$Duration..in.seconds. >= 180,] # If their time is under 3 minutes 

# In all studies, participants will be asked to answer a series of comprehension questions about the task 
# once they have read the instructions. If they fail any of these comprehension questions, they will be 
# shown the instructions again and given the opportunity to change their answers to the comprehension questions. 
# If they fail to answer all the comprehension questions correctly three times in a row, they will be allowed to 
# continue with the experiment but excluded from the analysis.

physicalCF3.1 = physicalCF3.1[physicalCF3.1$numFAIL != 3,]

# For the CONJUNCTION FALLACY study, there will be one question where the true outcome participants are asked 
# about appears near certain, and another where it appears near impossible. 

#Participants that report that the near certain outcome is less than 90% likely 

physicalCF3.1 = physicalCF3.1[physicalCF3.1$check.hundred_5 >= 80,]

# or that the near impossible outcome is more than 10% likely will be excluded from the analysis.

physicalCF3.1 = physicalCF3.1[physicalCF3.1$check.zero_5 <= 20,]

# First 100 only
if(nrow(physicalCF3.1) > 100){
  physicalCF3.1 = physicalCF3.1[1:100,]
}

physicalCF3.1 = droplevels(physicalCF3.1)



##### Analysis ------------------------------------------------------------------------

# Calculate CF error magnitude: conjunction minus single event -----

# >>> Tubes #

physicalCF3.1$CF.B.tubes_2 = physicalCF3.1$Tubes_2_Conj_3 - physicalCF3.1$Tubes_2_B_2
physicalCF3.1$CF.B.tubes_3 = physicalCF3.1$Tubes_3_Conj_3 - physicalCF3.1$Tubes_3_B_2
physicalCF3.1$CF.B.tubes_4 = physicalCF3.1$Tubes_4_Conj_3 - physicalCF3.1$Tubes_4_B_2
physicalCF3.1$CF.B.tubes_5 = physicalCF3.1$tubes_5_Conj_3 - physicalCF3.1$tubes_5_B_2

PCFstudy3_tubes = column.greppr(physicalCF3.1, "CF.B.tubes")

physicalCF3.1$PCFstudy3_tubes = apply(PCFstudy3_tubes, 1, mean)

t.test(physicalCF3.1$PCFstudy3_tubes)
bayes.t.test(physicalCF3.1$PCFstudy3_tubes)


# >>> Baskettube #

physicalCF3.1$CF.Baskettube_1 = physicalCF3.1$Baskettube_1_Conj_3 - physicalCF3.1$Baskettube_1_A_2
physicalCF3.1$CF.Baskettube_3 = physicalCF3.1$Baskettube_3_Conj_3 - physicalCF3.1$Baskettube_3_A_2
physicalCF3.1$CF.Baskettube_4 = physicalCF3.1$Baskettube_4_Conj_3 - physicalCF3.1$Baskettube_4_A_2
physicalCF3.1$CF.Baskettube_5 = physicalCF3.1$Baskettube_5_Conj_3 - physicalCF3.1$Baskettube_5_A_2

PCFstudy3_baskettube = column.greppr(physicalCF3.1, "CF.Baskettube")

physicalCF3.1$PCFstudy3_baskettube = apply(PCFstudy3_baskettube, 1, mean)

t.test(physicalCF3.1$PCFstudy3_baskettube)
bayes.t.test(physicalCF3.1$PCFstudy3_baskettube)


# >>> Stillweighttower #

physicalCF3.1$CF.Stillweighttower_1 = physicalCF3.1$Stillweighttower_1_C_2 - physicalCF3.1$Stillweighttower_1_A_2
physicalCF3.1$CF.Stillweighttower_2 = physicalCF3.1$Stillweighttower_2_C_2 - physicalCF3.1$Stillweighttower_2_A_2
physicalCF3.1$CF.Stillweighttower_3 = physicalCF3.1$Stillweighttower_3_C_2 - physicalCF3.1$Stillweighttower_3_A_2
physicalCF3.1$CF.Stillweighttower_4 = physicalCF3.1$Stillweighttower_4_C_2 - physicalCF3.1$Stillweighttower_4_A_2

PCFstudy3_stillweighttower = column.greppr(physicalCF3.1, "CF.Stillweighttower")

physicalCF3.1$PCFstudy3_stillweighttower = apply(PCFstudy3_stillweighttower, 1, mean)

t.test(physicalCF3.1$PCFstudy3_stillweighttower)
bayes.t.test(physicalCF3.1$PCFstudy3_stillweighttower)











