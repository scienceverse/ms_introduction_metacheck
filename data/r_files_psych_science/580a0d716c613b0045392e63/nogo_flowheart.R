gonogo <- read.table("nogo_flowheart_R.txt", 
                     header=TRUE, sep="\t", na.strings="NA", dec=",", fill=TRUE, strip.white=TRUE)

#factorize Time and group variables
gonogo$Time <- factor(gonogo$Time, labels=c('T1','T2'))
gonogo$group <- factor(gonogo$group, labels=c('school','kindergarten'))
#gonogo$wave <- factor(gonogo$wave, labels=c('wave1','wave2'))
#gonogo$gender <- factor(gonogo$gender, labels=c('female','male'))

#exclude subjects for Hearts&Flowers Task (HFT)
gonogo <- subset(gonogo,ID!='1162') ##did not follow instructions, resulting in very low incon_acc at T1 (0.2)
gonogo <- subset(gonogo,ID!='1169') ##did not follow instructions, resulting in very low con_acc at T1 (0.7)

#exclude subjects for behavioral analyses of the Cats&Dogs Task (CDT) because of a lack of valid data at at least one timepoint
#gonogo <- subset(gonogo,ID!='1138')
#gonogo <- subset(gonogo,ID!='1151')
#gonogo <- subset(gonogo,ID!='1155')
#gonogo <- subset(gonogo,ID!='1236')
#gonogo <- subset(gonogo,ID!='1260')
#gonogo <- subset(gonogo,ID!='1133')
#gonogo <- subset(gonogo,ID!='1219')

#exclude subjects for CDT fMRI analyses because of too much movement
#fMRI movement
#gonogo <- subset(gonogo,ID!='1134')
#gonogo <- subset(gonogo,ID!='1140')
#gonogo <- subset(gonogo,ID!='1142')
#gonogo <- subset(gonogo,ID!='1157')
#gonogo <- subset(gonogo,ID!='1231')
#gonogo <- subset(gonogo,ID!='1233')
#gonogo <- subset(gonogo,ID!='1234')
#gonogo <- subset(gonogo,ID!='1240')
#gonogo <- subset(gonogo,ID!='1247')
#gonogo <- subset(gonogo,ID!='1253')
#gonogo <- subset(gonogo,ID!='1257')

attach(gonogo)

#perform mixed ANOVA for HFT accuracy
library(tidyr)
options(contrasts=c("contr.sum","contr.poly"))
gonogo_acc <- gather(gonogo,conditions,accuracy,c(Con_acc_t1:Mix_acc_t1))
aov.con <- ezANOVA(gonogo_acc, dv=accuracy, wid=ID, within=c(Time,conditions), between=group,type=3)
aov.con_stats <- ezStats(gonogo_acc, dv=accuracy, wid=ID, within=c(Time,conditions), between=group)

#perform mixed ANOVA for HFT RTs
gonogo_rt <- gather(gonogo,conditions,accuracy,c(Con_CorrRTmean_t1:Mix_CorrRTmean_t1))
aov.con_RT_stats <- ezStats(gonogo_rt, dv=accuracy, wid=ID, within=c(Time,conditions), between=group, type=3)
aov.con_RT <- ezANOVA(gonogo_rt, dv=accuracy, wid=ID, within=c(Time,conditions), between=group)

#perform mixed ANOVA for CDT accuracy
gonogo <- gather(gonogo,conditions,accuracy,c(acc_go:acc_nogo))
aov.gonogo <- ezANOVA(gonogo, dv=accuracy, wid=ID, within=c(Time,conditions), between=group, type=3)
aov.gonogo_stats <- ezStats(gonogo, dv=accuracy, wid=ID, within=c(Time,conditions), between=group)

#perform mixed ANOVAs for CDT go RTs
library(ez)
aov.acc_go_RT <- ezANOVA(gonogo, dv=rt_go, wid=ID, within=Time, between=group, type=3)
aov.acc_go_RT_stats <- ezStats(gonogo, dv=rt_go, wid=ID, within=Time, between=group)