gonogo <- read.table("nogo_flowheart_R.txt", 
                     header=TRUE, sep="\t", na.strings="NA", dec=",", fill=TRUE, strip.white=TRUE)
#factorize time and group variables and subset data to include only T1 data
gonogo$Time <- factor(gonogo$Time, labels=c('T1','T2'))
gonogo$group <- factor(gonogo$group, labels=c('school','kindergarten'))
gonogo <- subset(gonogo,Time!='T2')

#age descriptives for whole sample (n=62)
mean(gonogo$ExactAge_T1)
range(gonogo$ExactAge_T1)

#age descriptives & group differences for final HFT sample
library(psych)
gonogo_HFT <- subset(gonogo,ID!='1162') ##very low incon_acc at T1 (0,2!!)
gonogo_HFT <- subset(gonogo_HFT,ID!='1169') ##very low con_acc at T1 (0,7!!)
describeBy(gonogo_HFT,group=gonogo_HFT$group)
t.test(gonogo_HFT$ExactAge_T1 ~ gonogo_HFT$group)

#age descriptives & group differences for final CDT behavioral sample
gonogo <- subset(gonogo,ID!='1138')
gonogo <- subset(gonogo,ID!='1151')
gonogo <- subset(gonogo,ID!='1155')
gonogo <- subset(gonogo,ID!='1236')
gonogo <- subset(gonogo,ID!='1260')
gonogo <- subset(gonogo,ID!='1133')
gonogo <- subset(gonogo,ID!='1219')
describeBy(gonogo,group=gonogo$group)
t.test(gonogo$ExactAge_T1 ~ gonogo$group)

#age descriptives & group differences for final CDT fMRI sample
gonogo <- subset(gonogo,ID!='1134')
gonogo <- subset(gonogo,ID!='1140')
gonogo <- subset(gonogo,ID!='1142')
gonogo <- subset(gonogo,ID!='1157')
gonogo <- subset(gonogo,ID!='1231')
gonogo <- subset(gonogo,ID!='1233')
gonogo <- subset(gonogo,ID!='1234')
gonogo <- subset(gonogo,ID!='1240')
gonogo <- subset(gonogo,ID!='1247')
gonogo <- subset(gonogo,ID!='1253')
gonogo <- subset(gonogo,ID!='1257')
describeBy(gonogo,group=gonogo$group)
t.test(gonogo$ExactAge_T1 ~ gonogo$group)

