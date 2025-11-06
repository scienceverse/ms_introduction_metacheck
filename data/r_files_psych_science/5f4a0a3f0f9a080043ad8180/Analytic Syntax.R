#############################################################################################
#
# R Code to reproduce the analyses and figures in Harder than you think: How outside assistance leads to overconfidence
# Last update: 08-04-2020
# Author: Anonymized for Review

############################################################################################


if(!require("psych")) {install.packages("psych"); require("psych")}
if(!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if(!require("effsize")) {install.packages("effsize"); require("effsize")}
if(!require("stats")) {install.packages("stats"); require("stats")}
if(!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
if(!require("svglite")) {install.packages("svglite"); require("svglite")}
if(!require("mediation")) {install.packages("mediation"); require("mediation")}

# Set working directory
## setwd("...")
# Enter file path to data files

#### FUNCTIONS ####
published_plot_ExD <- function (df){
  ## Plot Results
  positions <- c("No Help", "Help", "Button")
  main_plot <- ggboxplot(df, x = "Condition", y = "Predict", fill = "Condition",
                   palette = c("#636363", "#b1b1b1", "#f6f6f6"),
                   add = "jitter",outlier.shape = NA, shape = "Condition") +
    ylab("Predicted Future Performance (%)") +
    scale_x_discrete(limits = positions) +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position="none")
  
  # Add p-values comparing groups
  my_comparisons <- list( c("Button", "No Help"), c("Help", "No Help"), c("Button", "Help") )
  symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c( "***", "**", "*", "ns"))
  final <- main_plot + stat_compare_means(comparisons = my_comparisons,symnum.args = symnum.args, method = "t.test") # Add pairwise comparisons p-value
  final

}
published_plot_ExI <- function (df){
  ## Plot Results
  positions <- c("No Help", "Help", "Delay")
  main_plot <- ggboxplot(df, x = "Condition", y = "Predict", fill = "Condition",
                         palette = c("#636363", "#b1b1b1", "#f6f6f6"),
                         add = "jitter",outlier.shape = NA, shape = "Condition") +
    ylab("Predicted Future Performance (%)") +
    scale_x_discrete(limits = positions) +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position="none")
  
  # Add p-values comparing groups
  my_comparisons <- list( c("Delay", "No Help"), c("Help", "No Help"), c("Delay", "Help") )
  symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c( "***", "**", "*", "ns"))
  final <- main_plot + stat_compare_means(comparisons = my_comparisons,symnum.args = symnum.args, method = "t.test") # Add pairwise comparisons p-value
  final
  
}
#### Experiment 1a: Immediacy Trivia (IT) ####
IT.df<-read.csv("S1a_Data.csv")
# Convert Score to % correct
IT.df$Score <- (IT.df$Score/8)*100
headTail(IT.df)

## Participant Info
IT.df$Gender <- factor(IT.df$Gender,labels=c("Male","Female"))
table(IT.df$Gender)
mean(IT.df$Age)
sd(IT.df$Age)

## Descriptive statistics
IT.cond.difs <- IT.df %>% group_by(Condition) %>% summarise(PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
IT.cond.difs

## Standardized Linear Regression Models
IT.df$Condition <- relevel(IT.df$Condition, ref="Delay")
#Score
model.IT.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=IT.df)
summary(model.IT.score)
#Prediction
model.IT <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=IT.df)
summary(model.IT)

IT.t.df <- IT.df %>% filter(Condition != "No Help")
t.test(Predict ~ Condition, var.equal=TRUE, data=IT.t.df)

## Plot Results
save.pIT<-published_plot_ExI(IT.df)
## Save Image
#ggsave(file="FisherFigure1.svg", plot=save.pIT, width=5, height=5)

#### Experiment 1b: Default Trvia (DT) #####
DT.df<-read.csv("S1b_Data.csv")
# Convert Score to % correct
DT.df$Score <- (DT.df$Score/8)*100
headTail(DT.df)

## Participant Info
DT.df$Gender <- factor(DT.df$Gender,labels=c("Male","Female"))
table(DT.df$Gender)
mean(DT.df$Age)
sd(DT.df$Age)

## Exclusion (Preregistered)
sum(DT.df$OffPage>15)
DT.df.op <- DT.df %>% filter(OffPage <= 15)

## Descriptive statistics
DT.cond.difs <- DT.df.op %>% group_by(Condition) %>% summarise(TIME = mean(Time),PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
DT.cond.difs

## Standardized Linear Regression Models
DT.df.op$Condition <- relevel(DT.df.op$Condition, ref="Button")
#Score
model.DT.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=DT.df.op)
summary(model.DT.score)
#Prediction
model.DT <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=DT.df.op)
summary(model.DT)

## Exclusion (Button Press)
DT.df.b <- DT.df.op %>% filter(ButtonClicks > 4 | is.na(ButtonClicks))
#Prediction
model.DT.b <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=DT.df.b)
summary(model.DT.b)

## Plot Results
save.pDT <- published_plot_ExD(DT.df.op)
## Save Image
#ggsave(file="FisherFigure2.svg", plot=save.pDT, width=5, height=5)

#### Experiment 2a: Immediacy Memory (IM) ####
IM.df<-read.csv("S2a_Data.csv")
# Convert Score to % correct
IM.df$Score <- (IM.df$Score/15)*100
headTail(IM.df)

## Participant Info
IM.df$Gender <- factor(IM.df$Gender,labels=c("Male","Female"))
table(IM.df$Gender)
mean(IM.df$Age)
sd(IM.df$Age)

## Descriptive statistics
IM.cond.difs <- IM.df %>% group_by(Condition) %>% summarise(PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
IM.cond.difs

## Standardized Linear Regression Models
IM.df$Condition <- relevel(IM.df$Condition, ref="Delay Clue")
#Score
model.IM.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=IM.df)
summary(model.IM.score)
#Prediction
model.IM <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=IM.df)
summary(model.IM)

#Compare Delay Only to No Help
IM.df$Condition <- relevel(IM.df$Condition, ref="Delay Only")
model.IM.control <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=IM.df)
summary(model.IM.control)

## Plot
## Plot Results
positions <- c("Delay Only", "No Help", "Help", "Delay Clue")
IM.df$Condition <- factor(IM.df$Condition, levels = c("Delay Only", "No Help", "Help", "Delay Clue"))
main_plot <- ggboxplot(IM.df, x = "Condition", y = "Predict", fill = "Condition",
                       palette = c("white",  "#f6f6f6", "#b1b1b1", "#636363" ),
                       add = "jitter",outlier.shape = NA, shape = "Condition") +
  scale_shape_manual(values=c(3, 15, 17, 16)) +
  ylab("Predicted Future Performance (%)") +
  scale_x_discrete(limits = positions) +
  theme(legend.position = "none",
    axis.title.x = element_blank())

# Add p-values comparing groups
my_comparisons <- list( c("No Help","Delay Only"), c("Delay Clue", "No Help"), c("Help", "No Help"), c("Delay Clue", "Help"))
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c( "***", "**", "*", "ns"))
save.pIM <- main_plot + stat_compare_means(comparisons = my_comparisons,symnum.args = symnum.args, method = "t.test") # Add pairwise comparisons p-value

## Save Image
#ggsave(file="FisherFigure3.svg", plot=save.pIM, width=5.5, height=5)

#### Experiment 2b: Default  Memory (DM) ####
DM.df<-read.csv("S2b_Data.csv")
# Convert Score to % correct
DM.df$Score <- (DM.df$Score/15)*100
headTail(DM.df)

## Participant Info
DM.df$Gender <- factor(DM.df$Gender,labels=c("Male","Female"))
table(DM.df$Gender)
mean(DM.df$Age)
sd(DM.df$Age)

## Descriptive statistics
DM.cond.difs <- DM.df %>% group_by(Condition) %>% summarise(TIME = mean(Time),PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
DM.cond.difs

## Standardized Linear Regression Models
DM.df$Condition <- relevel(DM.df$Condition, ref="Button")
#Score
model.DM.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=DM.df)
summary(model.DM.score)
#Prediction
model.DM <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=DM.df)
summary(model.DM)

## Exclusion (Button Press)
DM.df.b <- DM.df %>% filter(ButtonClicks > 7 | is.na(ButtonClicks))
#Predict
model.DM.b <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=DM.df.b)
summary(model.DM.b)

## Plot Results
save.pDM<-published_plot_ExD(DM.df)
## Save Image
#ggsave(file="FisherFigure4.svg", plot=save.pDM, width=5, height=5)

#### Experiment 3a: Help vs. No Help Anagram (A) ####
A.df<-read.csv("S3a_Data.csv")
# Convert Score to % correct
A.df$Score <- (A.df$Score/8)*100

headTail(A.df)

## Participant Info
A.df$Gender <- factor(A.df$Gender,labels=c("Male","Female"))
table(A.df$Gender)
mean(A.df$Age)
sd(A.df$Age)

##Descriptive statistics
A.cond.difs <- A.df %>% group_by(Condition) %>% dplyr::summarise(TIME=mean(Time),PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
A.cond.difs

##Plots 
ggplot(A.df, aes(x=Score, fill=Condition)) + geom_density(alpha=.35)
ggplot(A.df, aes(x=Predict, fill=Condition)) + geom_density(alpha=.35)

## Independent sample t-tests
t.test(Score ~ Condition, var.equal=TRUE, data=A.df)
cohen.d(Score ~ Condition, data = A.df)

t.test(Predict ~ Condition, var.equal=TRUE, data=A.df)
cohen.d(Predict ~ Condition, data = A.df)

## Standardized Linear Regression Model
model.A <- lm(scale(Predict)~Condition + scale(Age) + Gender + scale(Education), data=A.df)
summary(model.A)

#### Experiment 3b: Immediacy Anagram (IA) ####
IA.df<-read.csv("S3b_Data.csv")
# Convert Score to % correct
IA.df$Score <- (IA.df$Score/8)*100
headTail(IA.df)

## Participant Info
IA.df$Gender <- factor(IA.df$Gender,labels=c("Male","Female"))
table(IA.df$Gender)
mean(IA.df$Age)
sd(IA.df$Age)

## Descriptive statistics
IA.cond.difs <- IA.df %>% group_by(Condition) %>% summarise(PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
IA.cond.difs

## Standardized Linear Regression Models
IA.df$Condition <- relevel(IA.df$Condition, ref="Delay")
#Score
model.IA.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=IA.df)
summary(model.IA.score)
#Predict
model.IA <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=IA.df)
summary(model.IA)

## Plot Results
save.pIA<-published_plot_ExI(IA.df)
## Save Image
#ggsave(file="FisherFigure5.svg", plot=save.pIA, width=5, height=5)

#### Experiment 3c: Default Anagram (DA) #### 
DA.df<-read.csv("S3c_Data.csv")
# Convert Score to % correct
DA.df$Score <- (DA.df$Score/8)*100
headTail(DA.df)

## Participant Info
DA.df$Gender <- factor(DA.df$Gender,labels=c("Male","Female"))
table(DA.df$Gender)
mean(DA.df$Age)
sd(DA.df$Age)

##Descriptive statistics
DA.cond.difs <- DA.df %>% group_by(Condition) %>% summarise(TIME = mean(Time),PREDICT = mean(Predict), PREDICT_SD = sd(Predict), SCORE = mean(Score), SCORE_SD = sd(Score))
DA.cond.difs

## Stanardized Linear Regression Models
DA.df$Condition <- relevel(DA.df$Condition, ref="Button")
#Score
model.DA.score <- lm(scale(Score)~Condition + Gender + scale(Age) + scale(Education), data=DA.df)
summary(model.DA.score)
#Prediction
model.DA <- lm(scale(Predict)~Condition + Gender + scale(Age) + scale(Education), data=DA.df)
summary(model.DA)

## Plot Results
save.pDA <- published_plot_ExD(DA.df)
## Save Image
ggsave(file="FisherFigure6.svg", plot=save.pDA, width=5, height=5)

#### Experiment 3d: Default Anagram (DA) with follow-up #### 
DA2.df<-read.csv("S3d_Data.csv")
# Convert Scores to % correct
DA2.df$Score1 <- (DA2.df$Score1/8)*100
DA2.df$Score2 <- (DA2.df$Score2/8)*100
headTail(DA2.df)

## Participant Info
DA2.df$Gender <- factor(DA2.df$Gender,labels=c("Male","Female"))
table(DA2.df$Gender)
mean(DA2.df$Age)
sd(DA2.df$Age)

#Compute Difference Score
DA2.df$Difference <- DA2.df$Prediction - DA2.df$Score2

##Descriptive statistics
DA2.cond.difs <- DA2.df %>% group_by(Condition) %>% summarise(DIFFERENCE = mean(Difference), DIFFERENCE_SD = sd(Difference), PREDICT = mean(Prediction), PREDICT_SD = sd(Prediction), SCORE1 = mean(Score1), SCORE1_SD = sd(Score1), SCORE2 = mean(Score2), SCORE2_SD = sd(Score2),DIFFICULTY = mean(Difficulty), DIFFICULTY_SD = sd(Difficulty), COUNT = unique(Condition))
DA2.cond.difs

## Stanardized Linear Regression Models
DA2.df$Condition <- relevel(DA2.df$Condition, ref="Button")
#Difference Score
model.DA2.score <- lm(scale(Difference)~Condition + Gender + scale(Age) + scale(Education), data=DA2.df)
summary(model.DA2.score)
#Difficulty
model.DA2.difficulty <- lm(scale(Difficulty)~Condition + Gender + scale(Age) + scale(Education), data=DA2.df)
summary(model.DA2.difficulty)
#Predictions 
model.DA2.predict <- lm(scale(Prediction)~Condition + Gender + scale(Age) + scale(Education), data=DA2.df)
summary(model.DA2.predict)

## Exclusion (Button Press)
DA2.df.b <- DA2.df %>% filter(ButtonClicks > 3 | is.na(ButtonClicks))
DA2.button.include <- count(DA2.df.b[DA2.df.b$Condition == "Button", ]) #amount who clicked more than half the available buttons
DA2.button.total <- count(DA2.df[DA2.df$Condition == "Button", ])
DA2.button.include/DA2.button.total #71.71%

#Replicate with Button Press Exclusion
DA2.model.b <- lm(scale(Difference)~Condition + Gender + scale(Age) + scale(Education), data=DA2.df.b)
summary(DA2.model.b)

## Mediation Analysis, reported values from SPSS PROCESS MACRO
mediate <- mediation::mediate
DA2.df$Condition <- relevel(DA2.df$Condition, ref="Button")
b.med.fit <- lm(scale(Difficulty) ~  Condition, data=DA2.df)
summary(b.med.fit)

b.y.fit <-  lm(scale(Difference) ~  Condition + scale(Difficulty), data=DA2.df)
summary(b.y.fit)

b.med.out.full = mediate(b.med.fit, b.y.fit, treat="Condition", mediator="scale(Difficulty)",control.value = "Help", treat.value = "Button") #note: lmerTest may need to be uninstalled for this to work
summary(b.med.out.full)

## Plot Results
positions <- c("No Help", "Help", "Button")
DA2.main_plot <- ggboxplot(DA2.df, x = "Condition", y = "Difference", fill = "Condition",
                       palette = c("#636363", "#b1b1b1", "#f6f6f6"),
                       add = "jitter",outlier.shape = NA, shape = "Condition") +
  ylab("Overconfidence (%)") +
  scale_x_discrete(limits = positions) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position="none") + 
  geom_hline(yintercept=0) + 
  scale_y_continuous(expand = c(0,0), breaks=seq(-100, 100, by = 50)) +
  coord_cartesian(ylim=c(-100, 155))

# Add p-values comparing groups
my_comparisons <- list( c("Button", "No Help"), c("Help", "No Help"), c("Button", "Help") )
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c( "***", "**", "*", "ns"))
DA2.final <- DA2.main_plot + stat_compare_means(comparisons = my_comparisons,symnum.args = symnum.args, method = "t.test") # Add pairwise comparisons p-value
DA2.final

#ggsave(file="FisherFigure7.svg", plot=DA2.final, width=5, height=5)


