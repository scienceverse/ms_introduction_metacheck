
## Analysis Script for: "Mental imagery induces cross-modal sensory plasticity and changes future auditory perceptioon" ##

#>>>>>  Experiment 2 Behavioral Data Analysis Individual Participant Psychometric Curve fitting and PSE Extraction Script  <<<<<<#

# Step 1 #      This script was/can be used to fit the psychometric curve to the data for each individual participant, and will save the PSE data as a text file in the 
#               directory called "adapatation_data" the script called "Exper1_Beh_Data_analysis.R" will then look for and analyze the PSE data from the directory called "adaptation_data". 
#               this script can be modified by doing find and replace in this script for "s00x" for each subject (or modified as a foor loop for all participants). 

# Step 2 #      After the curves are fit to each individual participants' data and the PSE values are saved in the "adaptation_data" directory, the script called "2_Exp2_Beh_Data_Analysis.R" 
#               can be used to perform group analyses (both by fitting hte psychometric curves to raw group data in the directory "Behavioral_Data", and by analyzing the PSEs saved by this 
#               script for each participant in the "adaptation_data" directory. 


rm(list = setdiff(ls(), lsf.str()))


require("ggplot2")
require("plyr")
library(ggplot2)

se <- function(x) sqrt(var(x)/length(x))#creates a function for standard error of the mean


mainDir<-"~/Exp2/Behavioral_Data"
setwd(mainDir)


datafile.list<-list.files(path = "~/Exp2/Behavioral_Data/", 
                          pattern = "s00x.*", include.dirs=TRUE)#this creates a list of the data for the specified subject

datafile.list
df.list <- lapply(datafile.list, function(i) {
        r <- readLines(i)
        read.table(text=r , header=TRUE, sep="" , blank.lines.skip=FALSE)
})

df.list

pretest<-df.list[[4]]#makes a dataframe outa the subs data
ses1<-df.list[[1]]#
ses2<-df.list[[2]]#
ses3<-df.list[[3]]#

data<-rbind(pretest, ses1, ses2, ses3)

data["right_left_response_binary_code"] <- NA #this creates a new column for right=1 and left=0 binary code for analysis
data$right_left_response_binary_code[
        data$Response == "right" 
        ] <- 1
data$right_left_response_binary_code[
        data$Response == "left" 
        ] <- 0

data["Sound_Condition_Normalized"] <- NA
data$Sound_Condition_Normalized[
        data$Sound_Condition == "Sound_at_30_Right" 
        ] <- 2
data$Sound_Condition_Normalized[
        data$Sound_Condition == "Sound_at_30_Left" 
        ] <- -2
data$Sound_Condition_Normalized[
        data$Sound_Condition == "Sound_at_15_Right" 
        ] <- 1
data$Sound_Condition_Normalized[
        data$Sound_Condition == "Sound_at_15_Left" 
        ] <- -1
data$Sound_Condition_Normalized[
        data$Sound_Condition == "Sound_at_Center" 
        ] <- 0

data["Block_reOrder"]<-NA
data$Block_reOrder[
        data$Block == "PreTest" 
        ] <- 1
data$Block_reOrder[
        data$Block == "Left_Adpt.xlsx" 
        ] <- 4
data$Block_reOrder[
        data$Block == "Control_Adpt.xlsx" 
        ] <- 3
data$Block_reOrder[
        data$Block == "Right_Adpt.xlsx" 
        ] <- 2

split1<-split(data, data$Block_reOrder)
pre_test_split<-split1[[1]]
right_sound_adapt_split<-split1[[2]]
left_sound_adapt_split<-split1[[4]]
control_sound_adapt_split<-split1[[3]]

t <- function(y) (sum(y)/length(y))

pretestdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = pre_test_split, t)
rightadaptdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = right_sound_adapt_split, t)
leftadaptdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = left_sound_adapt_split, t)
control2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = control_sound_adapt_split, t)



#data$Coherence <- factor(data$Coherence)#converts the coherence values into a categorical variable
rightlogit1 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = right_sound_adapt_split, family = "binomial")
summary(rightlogit1)
#confint(mylogit1)
leftlogit1<-glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = left_sound_adapt_split, family = "binomial")
summary(leftlogit1)
controllogit1 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = control_sound_adapt_split, family = "binomial")
summary(controllogit1)
pretestlogit1 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = pre_test_split, family = "binomial")
summary(pretestlogit1)


rightlogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = rightadaptdata2, family = "binomial")
summary(rightlogit2)
leftlogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = leftadaptdata2, family = "binomial")
summary(leftlogit2)
controllogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = control2, family = "binomial")
summary(controllogit2)
pretestlogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = pretestdata2, family = "binomial")
summary(pretestlogit2)

setwd(working_dir)

#this is the inverse log function
logit<-function(x) log(x/(1-x))

#this fucntion gives the predicted x values (along the fitted curve) that correspond to specific y values (or probabilities).
f <- function (y,m) {
        (logit(y/1) - coef(m)[["(Intercept)"]]) / coef(m)[["Sound_Condition_Normalized"]]
}

left_adapt<-f(.5, leftlogit2)
right_adapt<-f(.5, rightlogit2)
control_adapt<-f(.5, controllogit2)
pretest<-f(.5, pretestlogit2)

aftereffect_data<-data.frame(Sub=c("s00x"), left_adapt=left_adapt, right_adapt=right_adapt, control_adapt=control_adapt, pretest=pretest)

#savesubData to a new DF--can be used later to do group analysis on PSEs
dir.create(("~/Exp2/adaptation_data"), showWarnings = TRUE)
working_dir2<- "~/Exp2/adaptation_data"
setwd(working_dir2)
write.table(aftereffect_data, "s00x_aftereffect.txt", sep="\t", row.names = FALSE)
setwd(mainDir)























