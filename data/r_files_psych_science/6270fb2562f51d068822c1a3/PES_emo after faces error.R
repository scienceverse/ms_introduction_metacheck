library(tidyverse)
library(glba) 
library(rtdists)
library(retimes)

#clean the enviorment:
rm(list = ls())

#import an external csv file:
my_data<-read.csv(header=TRUE,file.choose())

#creating an additional colum:
my_data$pes_ref_time <- NA

#creating subjects vector:
subjects <- c(101:140) #exp 4
#subjects <- c(10,12,14,16,18,2,20,22,24,26,28,30,32,34,36,38,4,40,42,44,46,48,50,6,8,52,54,56,58) #exp 3

#creating a variable that indicates the first trial of a specific subject:
first_trial <- 1

#creating a variable that indicates the end of the data (last trial of the last subject):
last_all <- 1
while(!is.na(my_data$subject_nr[last_all]))
{
  last_all <- last_all + 1
}
last_all <- last_all - 1


#Function of calculating average RTs for pes_ref_time:

finding_RTs <- function(last_trial, first_trial, current_trial){
  #finding before trials:
  sum <- 0
  before_trial <- current_trial-1     #the before trial itself
  before_bound <- 0      #setting the counter for before trials
  while ((before_trial>=first_trial) & (before_bound<2)){
    if ((my_data$previous.faces.trial[before_trial]==1) & (my_data$acc_emo[before_trial]==1)){
      sum <- sum + my_data$rt_emo[before_trial]
      before_bound <- before_bound+1
    }
    before_trial <- before_trial-1
  }
  #finding after trials:
  after_trial <- current_trial+1    #the after trial itself
  after_bound <- 0            #setting the counter for after trials 
  while ((after_trial<=last_trial) & (after_bound<2)){
    if ((my_data$previous.faces.trial[after_trial]==1) & (my_data$acc_emo[after_trial]==1)){
      sum <- sum + my_data$rt_emo[after_trial]
      after_bound <- after_bound+1
    }
    after_trial <- after_trial+1
  }
  return(sum/(before_bound+after_bound))  
}



#loop to run on all subjects, first we defint the boounds (first and last trials) of each subject:
for (number in 1:length(subjects)){
  last_trial <- first_trial
  
  
  #We are running last_trial from first_trial until it reaches the end of the current subject number:
  while((my_data$subject_nr[last_trial] == subjects[number])&(last_trial<=last_all))
  {
    last_trial <- last_trial+1
  }
  
  
  # we have the upper bound of the current subject:
  
  last_trial <- last_trial - 1
  
  
  #place for Logic to fill the new column
  #analyzing rt_faces post emo-errors:
  
  trial <- first_trial+1
  
  while (trial<=last_trial){
    if (my_data$previous.faces.trial[trial] == 0) {
      my_data$pes_ref_time[trial]<- finding_RTs(last_trial, first_trial, trial)
    }
    trial<- trial+1
  }
  
  first_trial <- last_trial+1 #moving to the first trial of the next subject
  
} 


write.csv(my_data, "PES emo after faces error EXP 4.csv")
