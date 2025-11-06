## Analysis Script for: "Mental imagery induces cross-modal sensory plasticity and changes future auditory perceptioon" 
        
#Experiment 2 Behavioral Data Analysis
        
rm(list = setdiff(ls(), lsf.str()))

se <- function(x) sqrt(var(x)/length(x))#creates a function for standard error of the mean--we use this later

require("ggplot2")
require("plyr")
library(ggplot2)
library(compute.es) 
library (lsr)


mainDir<-"~/Exp2/Behavioral_Data"
setwd(mainDir)

datafile.list<-list.files(path = "~/Exp2/Behavioral_Data", 
                          pattern = "s0.*", include.dirs=TRUE)#this creates a list of the data for the specified subject

datafile.list
df.list <- lapply(datafile.list, function(i) {
        r <- readLines(i)
        read.table(text=r , header=TRUE, sep="" , blank.lines.skip=FALSE)
})

df.list
#install.packages('data.table')
library(data.table)
real_df <- as.data.frame(rbindlist(df.list))


real_df["right_left_response_binary_code"] <- NA #this creates a new column for right=1 and left=0 binary code for analysis
real_df$right_left_response_binary_code[
        real_df$Response == "right" 
        ] <- 1
real_df$right_left_response_binary_code[
        real_df$Response == "left" 
        ] <- 0

real_df["Sound_Condition_Normalized"] <- NA
real_df$Sound_Condition_Normalized[
        real_df$Sound_Condition == "Sound_at_30_Right" 
        ] <- 2
real_df$Sound_Condition_Normalized[
        real_df$Sound_Condition == "Sound_at_30_Left" 
        ] <- -2
real_df$Sound_Condition_Normalized[
        real_df$Sound_Condition == "Sound_at_15_Right" 
        ] <- 1
real_df$Sound_Condition_Normalized[
        real_df$Sound_Condition == "Sound_at_15_Left" 
        ] <- -1
real_df$Sound_Condition_Normalized[
        real_df$Sound_Condition == "Sound_at_Center" 
        ] <- 0

real_df["Block_reOrder"]<-NA
real_df$Block_reOrder[
        real_df$Block == "PreTest" 
        ] <- 1
real_df$Block_reOrder[
        real_df$Block == "Left_Adpt.xlsx" 
        ] <- 4
real_df$Block_reOrder[
        real_df$Block == "Control_Adpt.xlsx" 
        ] <- 3
real_df$Block_reOrder[
        real_df$Block == "Right_Adpt.xlsx" 
        ] <- 2

split1<-split(real_df, real_df$Block_reOrder)
pre_test_split<-split1[[1]]
right_sound_adapt_split<-split1[[2]]
left_sound_adapt_split<-split1[[4]]
control_sound_adapt_split<-split1[[3]]

t <- function(y) (sum(y)/length(y))

pretestdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = pre_test_split, t)
rightadaptdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = right_sound_adapt_split, t)
leftadaptdata2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = left_sound_adapt_split, t)
control2<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = control_sound_adapt_split, t)


pretestdata2_sub<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized + SubNum,  data = pre_test_split, t)
pretestdata2_se<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = pretestdata2_sub, se)

rightadaptdata2_sub<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized + SubNum,  data = right_sound_adapt_split, t)
rightadaptdata2_se<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = rightadaptdata2_sub, se)

leftadaptdata2_sub<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized + SubNum,  data = left_sound_adapt_split, t)
leftadaptdata2_se<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = leftadaptdata2_sub, se)

control2_sub<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized + SubNum,  data = control_sound_adapt_split, t)
control2_se<-aggregate(right_left_response_binary_code ~  Sound_Condition_Normalized,  data = control_sound_adapt_split, se)




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

#install.packages('rms')
require(rms)
rightlogit2_new<-lrm(formula =right_left_response_binary_code ~ Sound_Condition_Normalized, data = rightadaptdata2)
print(rightlogit2_new)

leftlogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = leftadaptdata2, family = "binomial")
summary(leftlogit2)

leftlogit2_new<-lrm(formula =right_left_response_binary_code ~ Sound_Condition_Normalized, data = leftadaptdata2)
print(leftlogit2_new)

controllogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = control2, family = "binomial")
summary(controllogit2)

controllogit2_new<-lrm(formula =right_left_response_binary_code ~ Sound_Condition_Normalized, data = leftadaptdata2)
print(controllogit2_new)

pretestlogit2 <- glm(right_left_response_binary_code ~ Sound_Condition_Normalized, data = pretestdata2, family = "binomial")
summary(pretestlogit2)

pretestlogit2_new<-lrm(formula =right_left_response_binary_code ~ Sound_Condition_Normalized, data = pretestdata2)
print(pretestlogit2_new)


##Adaptation analysis

working_dir2<-"~/Exp2/adaptation_data"


datafile.list2<-list.files(path = "~/Exp2/adaptation_data/", 
                           pattern = "s.*", include.dirs=TRUE)#this creates a list of the data for the specified subject
setwd(working_dir2)
datafile.list2
df.list2 <- lapply(datafile.list2, function(i) {
        r <- readLines(i)
        read.table(text=r , header=TRUE, sep="" , blank.lines.skip=FALSE)
})

df.list2


library(data.table)
real_adaptation_df <- as.data.frame(rbindlist(df.list2))

real_adaptation_df$pretest<-NULL

##convert to long format to analyze using ANOVA
long_real_adaptation_df <- reshape(real_adaptation_df, 
                                   varying = c("left_adapt","right_adapt","control_adapt"),                                 				
                                   v.names = "Means",
                                   timevar = "Condition", 
                                   times = c("left_adapt","right_adapt","control_adapt"), 
                                   new.row.names = 1:200,
                                   direction = "long")
long_real_adaptation_df<- long_real_adaptation_df[order(long_real_adaptation_df$Sub),] ##this orders everythign by subject

#repeated measures ANVOA using ezAnova
library("ez")

aov2<-ezANOVA(data=long_real_adaptation_df, dv=Means, wid=Sub, within=.(Condition)) 
aov2

anov_2<-aov(Means~Condition + Error(Sub/Condition), data=long_real_adaptation_df)
summary(anov_2)
#resid<-residuals(anov_2)

t.test(real_adaptation_df$left_adapt, real_adaptation_df$control_adapt, paired=TRUE, var.equal=FALSE)
cohensD(real_adaptation_df$left_adapt, real_adaptation_df$control_adapt, method="paired")

t.test(real_adaptation_df$right_adapt, real_adaptation_df$control_adapt, paired=TRUE, var.equal=FALSE)
cohensD(real_adaptation_df$right_adapt, real_adaptation_df$control_adapt, method="paired")

t.test(real_adaptation_df$right_adapt, real_adaptation_df$left_adapt, paired=TRUE, var.equal=FALSE)
cohensD(real_adaptation_df$right_adapt, real_adaptation_df$left_adapt, method="paired")


aov3<-aov(Means~Condition + Error(Sub/Condition), data=long_real_adaptation_df)
summary(aov3)
aov_resid<-proj(aov3)
aov_resid_test<-aov_resid[[3]][,"Residuals"]#set the residuals from this to a  
shapiro.test(aov_resid_test)#test the residuals 
qqnorm(aov_resid_test); qqline(aov_resid_test)
#identify(Q <- qqnorm(aov_resid_test, label=TRUE,  cex=0.5))

mean_left<-mean(real_adaptation_df$left_adapt)
mean_right<-mean(real_adaptation_df$right_adapt)
mean_same<-mean(real_adaptation_df$control_adapt)
#mean_pre<-mean(real_adaptation_df$pretest)

se_left<-se(real_adaptation_df$left_adapt)
se_right<-se(real_adaptation_df$right_adapt)
se_same<-se(real_adaptation_df$control_adapt)
#se_pre<-se(imagery_adaptation_df$pretest)

plot_data<-data.frame(condition=c("left_sound", "sound_same", "right_sound"), means=c(mean_left, mean_same, mean_right), ses=c(se_left,  se_same, se_right))

plot_data$Order<-NA
plot_data$Order[
        plot_data$condition=="left_sound"
        ]<-1
plot_data$Order[
        plot_data$condition=="sound_same"
        ]<-2
plot_data$Order[
        plot_data$condition=="right_sound"
        ]<-3

#####The proportion data plot
ggplot(rightadaptdata2, aes(x=rightadaptdata2$Sound_Condition_Normalized, y=rightadaptdata2$right_left_response_binary_code)) +
        scale_y_continuous(limits = c(0, 1)) + 
        geom_point(data=rightadaptdata2, aes(x=rightadaptdata2$Sound_Condition_Normalized, y=rightadaptdata2$right_left_response_binary_code, colour="red")) +
        geom_errorbar(data=rightadaptdata2_se, ymin=rightadaptdata2$right_left_response_binary_code-rightadaptdata2_se$right_left_response_binary_code, 
                      ymax=rightadaptdata2$right_left_response_binary_code+rightadaptdata2_se$right_left_response_binary_code, width=0.0, colour="#ff6666") +
        geom_point(data=leftadaptdata2, aes(x=leftadaptdata2$Sound_Condition_Normalized, y=leftadaptdata2$right_left_response_binary_code, colour="blue")) +
        geom_errorbar(data=leftadaptdata2_se, ymin=leftadaptdata2$right_left_response_binary_code-leftadaptdata2_se$right_left_response_binary_code, 
                      ymax=leftadaptdata2$right_left_response_binary_code+leftadaptdata2_se$right_left_response_binary_code, width=0.0, colour="#56B4E9") +
        geom_point(data=control2, aes(x=control2$Sound_Condition_Normalized, y=control2$right_left_response_binary_code, colour="black")) +
        geom_errorbar(data=control2_se, ymin=control2$right_left_response_binary_code-control2_se$right_left_response_binary_code, 
                      ymax=control2$right_left_response_binary_code+control2_se$right_left_response_binary_code, width=0.0, colour="#666666") +
        #geom_point(data=pretestdata2, aes(x=pretestdata2$Sound_Condition_Normalized, y=pretestdata2$right_left_response_binary_code, colour="grey")) +
        labs(x= "Nomalized Sound Locations (-/+ denotes left/right)", y="Probability of a Rightward Response") +
        stat_smooth(data=rightadaptdata2_sub, aes(x=rightadaptdata2_sub$Sound_Condition_Normalized, y=rightadaptdata2_sub$right_left_response_binary_code, colour ="red"),  method="glm", family="binomial", se=FALSE, size=1) +
        stat_smooth(data=leftadaptdata2_sub, aes(x=leftadaptdata2_sub$Sound_Condition_Normalized, y=leftadaptdata2_sub$right_left_response_binary_code, colour ="blue"),  method="glm", family="binomial", se=FALSE, size=1) +
        stat_smooth(data=control2_sub, aes(x=control2_sub$Sound_Condition_Normalized, y=control2_sub$right_left_response_binary_code, colour ="black"),  method="glm", family="binomial", se=FALSE, size=1) +
        #stat_smooth(data=pretestdata2, aes(x=pretestdata2$Sound_Condition_Normalized, y=pretestdata2$right_left_response_binary_code, colour ="grey"),  method="glm", family="binomial", se=FALSE) +
        geom_vline(aes(xintercept=mean_right), colour="#ff6666",linetype="dashed", size=.5) +
        geom_vline(aes(xintercept=mean_left), colour="#56B4E9",linetype="dashed", size=.5) +
        geom_vline(aes(xintercept=mean_same), colour="#666666", linetype="dashed", size=.5) +
        geom_hline(aes(yintercept=.5), colour="black", linetype="dashed", size=.5) +
        
        scale_colour_manual(name = 'Sound Adaptation', 
                            values =c('red'='#ff6666','blue'='#56B4E9', 'black'='#666666'), 
                            breaks =c("blue", "red", 'black'),
                            labels = c('Sound Left Adapt','Sound Right Adapt', 'Same Adapt')
        ) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#PSE PLot
pg<-ggplot(plot_data, aes(as.factor(Order), means, fill =as.factor(Order))) +
        ylim(-.3, .3) +
        labs(x="Adaptation Condition", y="Mean (a.u)") + 
        geom_bar(stat= "identity", position ="dodge") + theme(axis.text.x=element_text(angle=45, hjust=1)) + 
        geom_errorbar(aes(ymin=means-ses, ymax=means+ses), 
                      size=.3, width=.2, 
                      position=position_dodge(.9)) +
        coord_flip() + 
        theme_classic() +
        geom_hline(aes(yintercept=0)) +
        scale_fill_manual(values=c("#56B4E9", "#666666", "#ff6666"), 
                          name="Experimental\nCondition",
                          breaks=c("Left Sound", "Control Sound", "Right Sound"),
                          labels=c("Left Sound", "Right Sound", "Control Sound"))











