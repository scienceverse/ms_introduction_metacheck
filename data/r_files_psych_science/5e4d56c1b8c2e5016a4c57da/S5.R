#load packages----
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plotrix)) {install.packages("plotrix"); require(plotrix)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
if (!require(devtools)) {install.packages("devtools"); require(devtools)} # for install github
if (!require(yarrr)) {install_github("ndphillips/yarrr"); require(yarrr)} # pirate plots
if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)} 
if (!require(ordinal)) {install.packages("ordinal"); require(ordinal)} ## package for ordinal logistic regression
if (!require(effects)) {install.packages("effects"); require(effects)} 
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(car)) {install.packages("car"); require(car)}
if (!require(mosaic)) {install.packages("mosaic"); require(mosaic)}
if (!require(pwr)) {install.packages("pwr"); require(pwr)}                       ## package for power calculation
if (!require(vcd)) {install.packages("vcd"); require(vcd)}          ## for visualizing categorical data
if (!require(effsize)) {install.packages("effsize"); require(effsize)}  ## for coheh.d
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
#require(lattice)
require(MASS)
#require(devtools)
# clean data----
# rawData <- read.csv("rawData.csv", header = TRUE)
# ## 758 rows
# # repeated responses??
# # remove duplicate mTurk IDs
# (greedy <- table(rawData$workerId)) 
# (greedy <- as.data.frame(greedy[greedy > 1])) #19 duplicate mTurk IDs
# sum(greedy$Freq) # 41 tainted responses
# remove.list <- paste(greedy$Var1, collapse = '|')
# rawData <- filter(rawData, !grepl(remove.list, workerId)) #717 responses remaining
# rawData$workerId <- 1:nrow(rawData)
# rawData$V6 <- NA
# rawData$BonusID <- NA
# write_csv(rawData, "rawDataAnon.csv")

rawData <- read.csv("rawDataAnon.csv", header = T)
# Include only completed responses
rawData<- rawData[!is.na(rawData$ed), ]  ## 692 complete.  
names(rawData)
# exclude IAW preregistration
rawData <- subset(rawData, AttnCheck1 == 1) 
692 - 676 # 16 failed attention checks

# Survey mis-scored AttnCheck2 for easy condition
for (i in 1:nrow(rawData)){  ## row
  if (rawData$condition[i] == "easy" & rawData$AttnCk2[i] == 3) rawData$AttnCheck2[i] <- 1
  if (rawData$condition[i] == "easy" & rawData$AttnCk2[i] != 3) rawData$AttnCheck2[i] <- 0
}

compHE <- table(rawData$AttnCheck2, rawData$condition)
mar_table(compHE)
prop.table(compHE, 2) 
# 8.5% fail rate
prop.test(x = c(29, 29), n = c(339, 337))
# Participants did not perform significantly better in any one condition
# exclude incorrect responses per pre-reg.
rawData <- subset(rawData, AttnCheck2 == 1) 
# N=618
676-618 # 58 exclusions
rawData <- droplevels(rawData)
# rawData$condition <- relevel(rawData$condition, ref = "hard")
table(rawData$condition)

#Factor Gender
rawData$gender <- factor(rawData$gender, levels = c(1,2,3), labels = c("male", "female", "other"))
table(rawData$gender)
table(rawData$gender)[[2]]/nrow(rawData)
# 56% female

mean(rawData$age)
sd(rawData$age)
median(rawData$age)

#Factor Education level
rawData$ed <- factor(rawData$ed, levels = c(1:6), labels = c("less than HS", "HS", "Some college", "Associates", "Bachelor's", "Graduate or Professional Degree"))
table(rawData$ed)

#  Pull the needed data into new dataframe "Data"
Data <- rawData[,c("workerId", "condition", "TotalScore")]
Data$ScoreEst <- rawData$Q33
Data$age <- rawData$age
Data$gender <- rawData$gender
Data$ed <- rawData$ed
rawData <- dplyr::select(rawData, -contains("Q")) 
colnames(rawData)
Data <- cbind(Data, rawData[,34:69])
Data <- cbind(Data, rawData[,21:26])

for (i in 1:nrow(Data)){  ## row
  Data$A1PercentErr[i] <- abs(Data$A1est[i]- 100)
  Data$A2PercentErr[i] <- abs(Data$A2est[i]- 100)
  Data$A3PercentErr[i] <- abs(Data$A3est[i]- 100)
  Data$A4PercentErr[i] <- abs(Data$A4est[i]- 100)
  Data$A5PercentErr[i] <- abs(Data$A5est[i]- 100)
  Data$A6PercentErr[i] <- abs(Data$A6est[i]- 100)
}
truthData <- read.csv("Truth data.csv")
## compute raw error for each stimulous
rawerrmat <- data.frame()
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    rawerrmat[i,j] <- Data[i,paste0(truthData$Item[j], "est")] - truthData$Count[j]
  }
}
colnames(rawerrmat) <- c(paste0(truthData$Item, "err"))
rawerrmat <- abs(rawerrmat)

x2 <- c("hard" , "easy")
x3 <- c("DO.BR.FL_88") #display order column in rawdata

## agreggate estimates and confidence questions for each question sequence across conditions
Q1 <- NULL
Q2 <- NULL
Q3 <- NULL
Q4 <- NULL
Q5 <- NULL
Q6 <- NULL

for (i in 1:nrow(rawData)) { # row
    
      Q1[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][1]  
      Q2[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][2]
      Q3[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][3]
      Q4[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][4]
      Q5[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][5]
      Q6[i] <- strsplit(as.character(rawData[i,x3[1]]), ",")[[1]][6]
    
}  
   

Data$Q1 <- Q1
Data$Q2 <- Q2
Data$Q3 <- Q3
Data$Q4 <- Q4
Data$Q5 <- Q5
Data$Q6 <- Q6

# error for each stimulous/color-------
summary(rawerrmat) # there are some ridiculously high estimates!!!
boxplot(rawerrmat)

# replace errors greater than 1000% with NA
for (i in 1:nrow(rawerrmat)) { # row
  for (j in 1:ncol(rawerrmat)) { # column
    if (rawerrmat[i,j] > 1000) rawerrmat[i,j] <- NA
  }
}

# zscore all error by stimulous/color
errmat <- data.frame()
for (i in 1:nrow(rawerrmat)){
  for (j in 1:ncol(rawerrmat)) {
    errmat[i,j] <- zscore(rawerrmat[,j], na.rm = TRUE)[i]
  }
}
colnames(errmat) <- c(paste0(truthData$Item, "err"))
hist(errmat[,4])

## z scored Error (scored within stimulous/color) organized by question order
err1 <- NULL # the errors for the first question asked, z scored by color/stimulous
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) { ## 6 stimuli (A1-A6)
    if (grepl(pattern = truthData$Item[j], x = Data$Q1[i])) {err1[i] <- errmat[i,j]}  
  }
}

err2 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (grepl(pattern = truthData$Item[j], x = Data$Q2[i])) {err2[i] <- errmat[i,j]}  
  }
}
err2

err3 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (grepl(pattern = truthData$Item[j], x = Data$Q3[i])) {err3[i] <- errmat[i,j]}  
  }
}
err3

err4 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (grepl(pattern = truthData$Item[j], x = Data$Q4[i])) {err4[i] <- errmat[i,j]}  
  }
}
err4

err5 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (grepl(pattern = truthData$Item[j], x = Data$Q5[i])) {err5[i] <- errmat[i,j]}  
  }
}

err6 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (grepl(pattern = truthData$Item[j], x = Data$Q6[i])) {err6[i] <- errmat[i,j]}  
  }
}

colnames(rawData)

### Create a more compact data set to analize. Get rid of extraneous data
CleanData <- Data # used to be Null
CleanData <- cbind.data.frame(CleanData, err1, err2, err3, err4, err5, err6 ) 
colnames((CleanData))
CleanData$MeanConf <- rowMeans(CleanData[,c(13,19,25,31,37,43)], na.rm = T)
hist(CleanData$MeanConf)
CleanData$ScoreEstPerc <- CleanData$ScoreEst/6*100
CleanData$TotalScorePerc <- CleanData$TotalScore/6*100
names(CleanData)
write_csv(CleanData, "CleanDataAnon.csv")

## Long format
head(CleanData)
LongData <- NULL
LongData <- melt(CleanData, measure.vars = c(paste0("Q", c(1:6))), variable_name = "QuestionOrder" )
colnames(LongData)[length(LongData)] <- "QuestionID"
## Question order as numberic so we can run linear regression
LongData$QuestionOrder <- as.numeric(LongData$QuestionOrder)
names(LongData)

for (i in 1:nrow(LongData)) {
  LongData$Confidence[i] <- LongData[i,paste0(LongData$QuestionID[i], "conf")]
}
for (i in 1:nrow(LongData)) {
  LongData$time[i] <- LongData[i,paste0(LongData$QuestionID[i], "time_3")]
}
LongData$time
for (i in 1:nrow(LongData)) {
  LongData$est[i] <- LongData[i,paste0(LongData$QuestionID[i], "est")]
}
for (i in 1:nrow(LongData)) {
  LongData$score[i] <- LongData[i,paste0("R", substring(LongData$QuestionID[i],2), "Score")]
}
LongData$score <- as.factor(LongData$score)
for (i in 1:nrow(LongData)) {
  LongData$PercentErr[i] <- LongData[i,paste0(LongData$QuestionID[i], "PercentErr")]
}
for (i in 1:nrow(LongData)) {
  LongData$zErr[i] <- LongData[i,paste0("err", LongData$QuestionOrder[i])]
}
LongData <- LongData[order(LongData$workerId), ]

# Eliminate the redundant estimate and error Data that has been melted
colnames(LongData)
LongData <- LongData[,-c(8:61)]
#zscore all confidences BY PARTICIPANT
LongData <- ddply(LongData, "workerId", mutate, zconf = scale(Confidence))
LongData[is.nan(LongData$zconf),"zconf"] <- 0 #replace NaN (created by div by 0 variance for participants who never changed conf) with 0 
LongData$zconf <- as.numeric(LongData$zconf)

write_csv(LongData, "LongDataAnon.csv")
## load data------
CleanData <- read.csv("CleanDataAnon.CSV", header = TRUE)
LongData <- read.csv("LongDataAnon.CSV", header = TRUE)
truthData <- read.csv("Truth data.csv")
table(CleanData$condition)

head(LongData)
tail(LongData)
names(LongData)
head(CleanData)
tail(CleanData)
names(CleanData)

#common subsets-----
EasyData = LongData[LongData$condition == "easy",]
HardData = LongData[LongData$condition == "hard",]

#### Descriptive stats
summary(LongData)
summary(CleanData)

with(CleanData, tapply(MeanConf, list(condition), mean))  ## group means
with(CleanData, tapply(MeanConf, list(condition), sd))  ## group means
t.test(CleanData[CleanData$condition == "hard","MeanConf"], CleanData[CleanData$condition == "easy","MeanConf"])

with(CleanData, tapply(ScoreEstPerc, list(condition), mean))  ## group means

with(CleanData, tapply(TotalScorePerc, list(condition), mean))  ## group performance

t.test(CleanData[CleanData$condition == "hard","TotalScore"], CleanData[CleanData$condition == "easy","TotalScore"])

# overall calibration by conditioin-----
# Easy condition, does overall performance exceed average confidence indicating underconfidence?
t.test(CleanData[CleanData$condition == "easy","MeanConf"],CleanData[CleanData$condition == "easy","TotalScorePerc"], paired = T)
# Hard condition
t.test(CleanData[CleanData$condition == "hard","MeanConf"],CleanData[CleanData$condition == "hard","TotalScorePerc"], paired = T)

#####  Visualize data-------------
## Visualize the data for a few workerIds in each condition____________________________________________________________________________________________
set.seed(123)
ind <- sample(unique(LongData$workerId), 12)  ## pick 12 random sumjects
DataSub <- LongData[LongData$workerId %in% ind, ]  ## pull the rows for those workerIds
xyplot(Confidence ~ QuestionOrder|workerId, data = DataSub, mainv = "Individual Regressions (workerIds)", 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y, lty = 2)})

ind2 <- sample(unique(CleanData$workerId), 20)  ## pick 12 random sumjects
DataSub2 <- CleanData[CleanData$workerId %in% ind2, ]  ## pull the rows for those workerIds
matplot(t(DataSub2[DataSub2$condition == "easyA",c("MeanConf","ScoreEstPerc", "TotalScorePerc")]), pch = 20, type = "b", xlab = "Conf", xaxt = "n", ylab = "Confidence", lty = 1, main = "Confidence vs performance (EasyA)")
axis(1, at = 1:3, labels = c("Mean Confidence", "Overall Conf (% of Questions)", "Actual % Correct"))
matplot(t(DataSub2[DataSub2$condition == "easy",c("MeanConf","ScoreEstPerc", "TotalScorePerc")]), pch = 20, type = "b", xlab = "Conf", xaxt = "n", ylab = "Confidence", lty = 1, main = "Confidence vs performance (EasyB)")
axis(1, at = 1:3, labels = c("Mean Confidence", "Overall Conf (% of Questions)", "Actual % Correct"))
matplot(t(DataSub2[DataSub2$condition == "hard",c("MeanConf","ScoreEstPerc", "TotalScorePerc")]), pch = 20, type = "b", xlab = "Conf", xaxt = "n", ylab = "Confidence", lty = 1, main = "Confidence vs performance (Hard)")
axis(1, at = 1:3, labels = c("Mean Confidence", "Overall Conf (% of Questions)", "Actual % Correct"))

hist(LongData$zErr)  ## right skewed.  

## Within workerIds
pirateplot(formula = Confidence ~ QuestionOrder + condition,
           data = LongData,
           main = "Confidence by Question Order",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

pirateplot(formula = zconf ~ QuestionOrder + condition,
           data = LongData,
           main = "Confidence by Question Order (with FEEDBACK)",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

pirateplot(formula = Confidence ~ QuestionID,
           data = LongData,
           main = "Confidence for each question",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)
text(5,1,"line is the mean and box is the 95% CI", cex = 0.5)

# time-------
hist(LongData$time, xlim = c(0,80), breaks = 200)
median(LongData$time)
mean(LongData$time > 45) # 12% of responses were > 45s or about 3x above the mean
timeTrim <- LongData[LongData$time < 45,]
hist(timeTrim$time)
nrow(LongData)-nrow(timeTrim) # 443 questions dropped
pirateplot(formula = time ~ QuestionID,
           data = LongData,
           main = "Time on each question",
           jitter.val = .1,
           ylab = "Time (sec)",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)
pirateplot(formula = time ~ QuestionOrder,
           data = timeTrim,
           main = "Time on each question",
           jitter.val = .1,
           ylab = "Time (sec)",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

meanTimeByOrder <- ddply(LongData, .(QuestionOrder), summarise, meantime = mean(time, na.rm = TRUE))
plot(meanTimeByOrder) 
abline(lm(meanTimeByOrder$meantime ~ meanTimeByOrder$QuestionOrder))

fitTime <- lm(time ~ QuestionOrder, data = timeTrim)
summary(fitTime) 
plot(fitTime)  # normality is a terrible assumption for response time

# try gamma dist
timefit <- glmer(time ~ QuestionOrder + (1|workerId), data = timeTrim, family = Gamma(link = log))
summary(timefit) # reported
exp(-.1578)  # about 1 sec less per question.  Decline is actually more than that from Q1 to Q2 and less from 2-9.
plot(allEffects(timefit))
plot(timefit)
plot(LongData$QuestionOrder, LongData$time)

# Conf~(log)time?-------
myColVec2 <- rainbow(10, alpha = 0.7)
plot(1:10, col = myColVec2, pch = 20, cex = 3)
plot(log(LongData$time), LongData$Confidence, xlab = "Response Time (log seconds)", ylab = "Confidence", main = "Confidence and response time",
     pch = 20, col = myColVec2[c(1:9)][LongData$QuestionOrder])
legend("bottomright", paste0("Q", 1:6), pch = 20, col = myColVec2[c(1:6)])

fitConfTime <- lmer(Confidence ~ log(time) + (1|workerId) + (1|QuestionID), data = timeTrim)
summary(fitConfTime) #reported 
plot(allEffects(fitConfTime))

fitConfTime1a <- lmer(Confidence ~ (time) + (1|workerId) + (1|QuestionID), data = timeTrim)
summary(fitConfTime1a) 
plot(allEffects(fitConfTime1a))
# ns

fitConfTime2 <- lmer(Confidence ~ log(time) + QuestionOrder + (1|workerId) + (1|QuestionID), data = timeTrim)
summary(fitConfTime2) 
plot(allEffects(fitConfTime2))
# log(time) ns; conf declining significantly w/ Qorder
fitConfTime2a <- lmer(Confidence ~ log(time) + QuestionOrder*condition + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
summary(fitConfTime2a) 
plot(allEffects(fitConfTime2a), ylab = "Confidence (%)")
# time not at all significant when add question order as covariate.  

cor(log(LongData$time), LongData$Confidence)
cor(LongData$QuestionOrder, LongData$Confidence)
cor(log(LongData$time), LongData$QuestionOrder)

# score~confidence--------
fitScore <- glmer(score ~ Confidence + (1|workerId), family = binomial, data = LongData[LongData$condition == "easy",])
summary(fitScore)
plot(allEffects(fitScore))
# marginal increase in score as conf increases
fitScore <- glmer(score ~ Confidence + (1|workerId), family = binomial, data = LongData[LongData$condition == "hard",])
summary(fitScore)
plot(allEffects(fitScore))
# ns decline in score as conf increases

# confidence ~ question order------
hist(LongData$Confidence, breaks = c(-10,15,40,65,90,115))

## random effect for animal and workerId
fitConfTreat1 <- lmer(Confidence ~ (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
ranef(fitConfTreat1) ## accounting for variation in confidence based on different cities and workerIds as random effects
# because of the random effect for workerId and the between subjects design, most of the variation due to differences in the difficulty between A and B stimuli is being attributed to the specific individuals
## Add Order
fitConfTreat2 <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
summary(fitConfTreat2) # ns
plot(allEffects(fitConfTreat2))
## Add condition----
fitConfTreatE <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "easy",])
summary(fitConfTreatE) #
plot(allEffects(fitConfTreatE, x.var = "QuestionOrder"))
# ns in easy condition

fitConfTreatH <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "hard",])
summary(fitConfTreatH) # 
plot(allEffects(fitConfTreatH, x.var = "QuestionOrder"))
# significant decline in hard condition 

fitOrderHard <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = HardData)
summary(fitOrderHard) # reported
confint(fitOrderHard)
plot(allEffects(fitOrderHard, x.var = "QuestionOrder"), ylim = c(-.25,.25), rug = F, main = "hard")

fitOrderEasy <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = EasyData)
summary(fitOrderEasy) #reported
confint(fitOrderEasy)
plot(allEffects(fitOrderEasy, x.var = "QuestionOrder"), ylim = c(-.25,.25), rug = F, main = "easy")

#interaction of question order and condition-------
contrasts(LongData$condition)

fitOrderTreat1 <- lmer(Confidence ~ QuestionOrder*condition + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
Anova(fitOrderTreat1)
summary(fitOrderTreat1) #not a significant interaction
plot(allEffects(fitOrderTreat1, x.var = "QuestionOrder"))

fitOrderTreat3 <- lmer(zconf ~ QuestionOrder*condition + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
summary(fitOrderTreat3) #reported
confint(fitOrderTreat3)
plot(allEffects(fitOrderTreat3, x.var = "QuestionOrder"), main = "Confidence trends by condition")
# significant interaction

# control of time and error for Supplement
fitOrderTreat3 <- lmer(zconf ~ QuestionOrder*condition + log(time) + zErr + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData)
summary(fitOrderTreat3) 
plot(allEffects(fitOrderTreat3), main = "Confidence trends by condition")
# significant interaction


par(mfcol = c(1,1))
pirateplot(formula = zconf ~ QuestionOrder + condition ,
           data = LongData,
           main = "Confidence by Question Order (by condition)",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

# Figure 7------
apatheme=theme_bw()+
  theme(#panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        title = element_text(size = 12),
        axis.title.y = element_text(angle=90, vjust = .5, size = 10),
        axis.title.x = element_text(size = 10),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        text = element_text(size = 9, family = "Helvetica")
  )

levels(LongData$condition)
LongData$condition0 <- factor(LongData$condition, labels = c( "Easy", "Hard"))
df <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(Confidence, na.rm = T), 
                                                                            sd = sd(Confidence, na.rm = T), 
                                                                            se = plotrix::std.error(Confidence, na.rm = T))
ggplot(df, aes(x=QuestionOrder, y=mean)) + facet_grid(. ~ condition0, labeller = label_value) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05) +
  geom_line(lty = 5) + geom_point(size = 2) + coord_cartesian(ylim = c(10,80)) + 
  labs(x = "\nQuestion Order", y = "\n\n\n\nConfidence") + 
  guides(fill = guide_legend(reverse=F)) + 
  scale_x_continuous(breaks=c(1:6)) +
  # scale_color_manual(name="Prior Investment", values = c("darkblue", "red")) + 
  theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman"))+  
  ggsave("EasyHard.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("EasyHard.jpeg", width=8, height=5, device="jpeg")
#z
dfz <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(zconf, na.rm = T), 
                                                                            sd = sd(zconf, na.rm = T), 
                                                                            se = plotrix::std.error(zconf, na.rm = T))
ggplot(dfz, aes(x=QuestionOrder, y=mean)) + facet_grid(. ~ condition0, labeller = label_value) + 
  # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.05) +
  # geom_line(lty = 5) + geom_point(size = 2) + 
  coord_cartesian(ylim = c(-.25,.25)) + 
  labs(x = "\nQuestion Order", y = "\n\n\n\nConfidence\nStandardized\nby Participant") + 
  scale_x_continuous(breaks=c(1:6)) +
  # scale_color_manual(name="Prior Investment", values = c("darkblue", "red")) + 
  theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman"))+  
  ggsave("EasyHardz0.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("EasyHardz0.jpeg", width=8, height=5, device="jpeg")

ggplot(LongData, aes(x=QuestionOrder, y=zconf)) + facet_wrap( ~ condition0, labeller = label_value) + 
  stat_summary(fun.y = mean, geom = "point", alpha = 4/5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, alpha = 4/5, position = position_dodge(.5)) + 
  geom_lm(interval = "confidence") +
  labs(x = "\nQuestion Order", y = "Z-Scored Confidence\n") + 
  apatheme +
  ggsave("MinsonUmphresFig7.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig7.jpeg", width=8, height=5, device="jpeg") #reported

## Demographic influences on Confidence??--------------------------------------------------------------------------------------------------------------
# add gender to within workerIds
fitConfGender <- lmer(Confidence ~ gender + (1|QuestionID), data = LongData)
ranef(fitConfGender)
summary(fitConfGender)
plot(allEffects(fitConfGender))
## Men more confident than women

## add age
hist(CleanData$age)
abline(v = mean(CleanData$age), col = "red")
## Between workerIds
fitConfAge <- lmer(Confidence ~ age + (1|QuestionID), data = LongData)
ranef(fitConfAge)
summary(fitConfAge)
plot(allEffects(fitConfAge))
## Age appears to have a small, significant impact on confidence. 
# conf increases .12% each year of age, p = .009

## education level
plot(CleanData$ed)
lmer1.2 <- lm(Confidence ~ ed , data = LongData)
Anova(lmer1.2) ## Education level marginal
summary(lmer1.2)    
plot(allEffects(lmer1.2))
## See the same pattern we see in self reported baseline knowledge.  People get more and more confident until they finally realize how much they suck and then confidence resets

# resolution-----
scatterplot(LongData[, "PercentErr"], LongData[, "Confidence"])

# let's trim some of these outliers.  Say get rid of any error greater than 600% (the error is 6x the size of the actual value) 
LongDataTrim <- LongData[LongData$PercentErr < 600,]
nrow(LongData)-nrow(LongDataTrim) # cut 15 observations
scatterplot(LongDataTrim[LongDataTrim$condition == "easyA", "PercentErr"], LongDataTrim[LongDataTrim$condition == "easyA", "Confidence"])
scatterplot(LongDataTrim[LongDataTrim$condition == "hard", "PercentErr"], LongDataTrim[LongDataTrim$condition == "hard", "Confidence"])

cor(LongDataTrim[,c("Confidence" , "zErr")])
cor(LongDataTrim[,c("Confidence" , "PercentErr")]) #reported
# gamma correlations
CorrBySub <- ddply(LongDataTrim, .(workerId), summarise,  GammaCor = rcorr.cens(score, Confidence, outx = TRUE)[2])
mean(CorrBySub$GammaCor, na.rm = T) # .17 when computed by participant. relatively good #reported
CorrBySub2 <- ddply(LongDataTrim, .(workerId), summarise,  GammaCor = rcorr.cens(PercentErr, Confidence, outx = TRUE)[2]) 
mean(CorrBySub2$GammaCor, na.rm = T) # -.12.  Participants were more confident when their errors were smaller (barely) #reported

CorrBySubMix <- ddply(subset(LongDataTrim, condition == "hard"), .(workerId), summarise,  GammaCor = rcorr.cens(PercentErr, Confidence, outx = TRUE)[2]) 
mean(CorrBySubMix$GammaCor, na.rm = T) # -.12.  Participants were more confident when their errors were smaller (barely) #reported
CorrBySubMM <- ddply(subset(LongDataTrim, condition == "easyA"), .(workerId), summarise,  GammaCor = rcorr.cens(PercentErr, Confidence, outx = TRUE)[2]) 
mean(CorrBySubMM$GammaCor, na.rm = T) # -.19.  Participants were more confident when their errors were smaller (barely) #reported
CorrBySubA <- ddply(subset(LongDataTrim, condition == "easyB"), .(workerId), summarise,  GammaCor = rcorr.cens(PercentErr, Confidence, outx = TRUE)[2]) 
mean(CorrBySubA$GammaCor, na.rm = T) # -.07.  Participants were more confident when their errors were smaller (barely) #reported

lm2 <- lm(Confidence ~ PercentErr, data = LongData)
summary(lm2)
plot(LongData$PercentErr, LongData$Confidence)  
abline(lm2) # fit driven by outliers
plot(allEffects(lm2)) 

lm2a <- lm(Confidence ~ PercentErr, data = LongDataTrim)
summary(lm2a) # with trimmed data and PercentError
plot(LongDataTrim$PercentErr, LongDataTrim$Confidence)  
abline(lm2a)
plot(allEffects(lm2a)) 
#  ns

# re-analize using zErr------
cor(LongData$Confidence, LongData$zErr)
cor(LongDataTrim$Confidence, LongDataTrim$zErr)

lm2 <- lm(Confidence ~ zErr, data = LongData)
summary(lm2)
plot(LongData$zErr, LongData$Confidence)  
abline(lm2) # fit driven by outliers
plot(allEffects(lm2)) 

lm2a <- lm(Confidence ~ zErr, data = LongDataTrim)
summary(lm2a) # with trimmed data and zError
plot(LongDataTrim$zErr, LongDataTrim$Confidence)  
abline(lm2a)
plot(allEffects(lm2a)) 
# nothing

## w/ random effects for workerId and city variation
fitConfErr1 <- lmer(Confidence ~ zErr + (1|workerId) + (1|QuestionID), data = LongDataTrim)
summary(fitConfErr1)
plot(allEffects(fitConfErr1))
plot(LongDataTrim$zErr, LongDataTrim$Confidence)  
abline(a = 62, b = -.0288)
#  nothing

hist(LongDataTrim$PercentErr)
hist(log(LongData$PercentErr))

## Effects of order on error----
scatterplot(LongData[LongData$condition == "easy", "QuestionOrder"], LongData[LongData$condition == "easy", "PercentErr"])
scatterplot(LongData[LongData$condition == "hard", "QuestionOrder"], LongData[LongData$condition == "hard", "PercentErr"])

pirateplot(formula = PercentErr ~ QuestionOrder + condition,
           data = LongDataTrim,  # Trimmed data
           main = "Percent Error in Estimation/Prediction",
           jitter.val = .1,
           ylab = "Error in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5,
           avg.line.fun = median ### note the change to median indicator
)
##  Looks pretty steady accross conditions indicating whatever causes the decline of confidence does not negatively effect accuracy

sd(LongData$PercentErr)
sd(LongDataTrim$PercentErr)
set.seed(12)
ind <- sample(unique(LongDataTrim$workerId), 12)  ## pick 12 random sumjects
DataSub <- LongDataTrim[LongDataTrim$workerId %in% ind, ]  ## pull the rows for those workerIds
xyplot(zErr ~ QuestionOrder|workerId, data = DataSub, mainv = "Individual Regressions (workerIds)", 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y, lty = 2)})

# Start with a within workerIds lme model
fitErrTreat1t <- lmer(PercentErr ~ QuestionOrder + (1|workerId) , data = LongDataTrim)
summary(fitErrTreat1t) #reported
plot(allEffects((fitErrTreat1t)))

fitErrTreat2at <- lmer(PercentErr ~ QuestionOrder + (1|workerId), data = LongDataTrim[LongDataTrim$condition == "easy",])
summary(fitErrTreat2at) 
plot(allEffects(fitErrTreat2at))

fitErrTreat2ct <- lmer(PercentErr ~ QuestionOrder + (1|workerId), data = LongDataTrim[LongDataTrim$condition == "hard",])
summary(fitErrTreat2ct)
plot(allEffects(fitErrTreat2ct))

fitErrTreat4 <- lmer(PercentErr ~ condition + (1|workerId), data = LongDataTrim)
summary(fitErrTreat4)
plot(allEffects(fitErrTreat4))

fitErrTreat3 <- lmer(PercentErr ~ QuestionOrder*condition + (1|workerId), data = LongDataTrim)
summary(fitErrTreat3)
plot(allEffects(fitErrTreat3, x.var = "QuestionOrder"))
# main effect of question and marginal effect of condition, no interaction

# Confidence decline controling for log(time) and PercentErr-----
# excluding both error and time outliers
LongDataTrim2 <-  timeTrim[timeTrim$PercentErr < 600,]
fitOrderHard2 <- lmer(zconf ~ QuestionOrder + log(time) + PercentErr + (1 + QuestionOrder|workerId) + (1|QuestionID), data = subset(LongDataTrim2, condition == "hard"))
summary(fitOrderHard2) # reported
confint(fitOrderHard2)
plot(allEffects(fitOrderHard2), rug = F, main = "hard")

fitOrderEasy2 <- lmer(zconf ~ QuestionOrder + log(time) + PercentErr + (1 + QuestionOrder|workerId) + (1|QuestionID), data = subset(LongDataTrim2, condition == "easy"))
summary(fitOrderEasy2) #reported
confint(fitOrderEasy2)
plot(allEffects(fitOrderEasy2), rug = F, main = "easy")

#interaction of question order and condition-------
fitOrderTreat32 <- lmer(zconf ~ QuestionOrder*condition + log(time) + PercentErr + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongDataTrim2)
summary(fitOrderTreat32) 
confint(fitOrderTreat32)
plot(allEffects(fitOrderTreat32), main = "Confidence trends by condition")
# significant interaction

#calibarion-----------
pirateplot(formula = score ~ QuestionOrder+condition+FBcondition,
           data = LongData,
           main = "Score",
           jitter.val = .1,
           ylab = "Accuracy",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

# aggregate score by question and condition
ScoreByOrder <- ddply(LongData, .(QuestionOrder, condition), summarise, percCorrect = mean(as.numeric(score), na.rm = TRUE))
ScoreByOrder$percCorrect <- 100*ScoreByOrder$percCorrect
plot(percCorrect ~ QuestionOrder, ScoreByOrder[ScoreByOrder$condition == "easy",]) # big jump after 1st question, then flat at high level
plot(percCorrect ~ QuestionOrder, ScoreByOrder[ScoreByOrder$condition == "hard",]) # gradual improvement to mediocre

meanConfByOrder <- ddply(LongData, .(QuestionOrder, condition), summarise, meanConf = mean(Confidence, na.rm = TRUE))
# plot(meanConfByOrder, ylim = c(0, 75)) 
# points(ScoreByOrder$QuestionOrder, ScoreByOrder$percCorrect, pch = 22, col = "red")
# overall underconfident

OverConf <- meanConfByOrder$meanConf - ScoreByOrder$percCorrect
plot(OverConf)

# Look at easy condition only and use fitted values like Sanchez and Dunning
fitConfTreatx <- lmer(Confidence ~ QuestionOrder*FBcondition + (1+QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "easy",])
summary(fitConfTreatx)
fittedFixedConf <- model.matrix(fitConfTreatx, type = "fixed")  %*% fixef(fitConfTreatx)
fittedFixedConf <- fittedFixedConf[1:12,]
plot(fittedFixedConf, ylim = c(0,75))
# Confidence if fairly steady at 60% (hard) or 70% (easy)

fitScoreTrend <- glmer(score ~ QuestionOrder + (1+QuestionOrder|workerId) + (1|QuestionID), family = binomial, data = LongData[LongData$condition == "easy",])
summary(fitScoreTrend)
plot(allEffects(fitScoreTrend)) # score trending positive but ns
fittedFixedLogOdds <- model.matrix(fitScoreTrend, type = "fixed")  %*% fixef(fitScoreTrend)
fittedFixedLogOdds <- fittedFixedLogOdds[1:6,]
fittedFixedOdds <- exp(fittedFixedLogOdds)
fittedAccProb <- fittedFixedOdds/(1+fittedFixedOdds)*100
plot(fittedAccProb, ylim = c(0,100), xlab = "Question Order", ylab = "Proability (%)", main = "Confidence vs. Accuracy (Easy 100 Condition)", type = "b")
points(c(1:6), fittedFixedConf[1:6],pch = 23, type = "b")
points(c(1:6), fittedFixedConf[7:12],pch = 23, type = "b")
legend("bottomright", c("Confidence", "Accuracy"), pch = c(23, 21), col = "black", bty = "n")
# Consistently underconfident in the Easy Condition

OverConfFitted <- fittedFixedConf - fittedAccProb
plot(OverConfFitted, ylim = c(0,50))

rac <- cor(LongControl$score, LongControl$Confidence) 
#SEoc <- sqrt(SEc^2+SEa^2-2*rac*SEc*SEa)

# Look at hard condition only
fitConfTreatx1 <- lmer(Confidence ~ QuestionOrder + (1+QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "hard",])
summary(fitConfTreatx1)
plot(allEffects(fitConfTreatx1)) # declining conf in hard condition
fittedFixedConf1 <- model.matrix(fitConfTreatx1, type = "fixed")  %*% fixef(fitConfTreatx1)
fittedFixedConf1 <- fittedFixedConf1[1:6,]
plot(fittedFixedConf1, ylim = c(0,60))

#score should be improving from about 8% up to 30% based on overall trend data
fitsimple <- glm(score~QuestionOrder*condition, family = binomial, data = LongData)
plot(allEffects(fitsimple))

fitScoreTrend1 <- glmer(score ~ QuestionOrder +  (1|QuestionID), family = binomial, data = LongData[LongData$condition == "hard",])
summary(fitScoreTrend1)
plot(allEffects(fitScoreTrend1)) # something here is messed up. Way off of the simple model. Predicting super low scores when including random slopes or intercepts for participant.
fittedFixedLogOdds1 <- model.matrix(fitScoreTrend1, type = "fixed")  %*% fixef(fitScoreTrend1)
fittedFixedLogOdds1 <- fittedFixedLogOdds1[1:6,]
fittedFixedOdds1 <- exp(fittedFixedLogOdds1)
fittedAccProb1 <- fittedFixedOdds1/(1+fittedFixedOdds1)*100
plot(fittedAccProb1, ylim = c(0,75), xlab = "Question Order", ylab = "Proability (%)", main = "Confidence vs. Accuracy (Hard Condition)", type = "b")
points(c(1:6), fittedFixedConf1,pch = 23, type = "b")
legend("topright", c("Confidence", "Accuracy"), pch = c(23, 21), col = "black", bty = "n")
# start grossly overconfident, end reasonably calibrated.

OverConfFittedBDM <- fittedFixedConf1 - fittedAccProb1
plot(OverConfFittedBDM, ylim = c(0,50), type = "l")

#ScoreEst vs Total Score----
pirateplot(formula = TotalScorePerc ~ condition,
           data = CleanData,
           main = "% Correct by condition",
           jitter.val = .1,
           ylab = "% of 6 Questions Correct",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)
pirateplot(formula = ScoreEstPerc ~ condition,
           data = CleanData,
           main = "Estimated % Correct by condition",
           jitter.val = .1,
           ylab = "% of 6 Questions Predicted Correct",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)

hist(CleanData$TotalScore) # right skewed bounded (0,9).  Violates parametric assumptions.
summary(CleanData$TotalScorePerc)
hist(CleanData$ScoreEstPerc)
summary(CleanData$ScoreEstPerc)
t.test(CleanData$TotalScore, CleanData$ScoreEst, paired = T)  # not really valid.  Violated assumptions of normality.
cohen.d(CleanData$TotalScore, CleanData$ScoreEst, paired = T) #small effect
hist(LongData$Confidence)
summary(LongData$Confidence)

# Hard
plot(fittedAccProb1, ylim = c(0,100), xlab = "Question Order", ylab = "Proability (%)", main = "Confidence vs. Accuracy (Hard Condition)", pch = 21, type = "b")
points(c(1:6), fittedFixedConf1,pch = 22, type = "b")
abline(h = mean(CleanData[CleanData$condition == "hard", "TotalScorePerc"]), lty = 1)
abline(h = mean(CleanData[CleanData$condition == "hard", "ScoreEstPerc"]), lty = 2)
legend("top", c("Confidence", "% of participants who answered correctly"), pch = c(22, 21 ),  bty = "n")
legend("right", c( "% out of 6 Q actually correct","Median % prediced Correct"),lty = c(1,2),  bty = "n")
# overconfident by any measure.  item level confidence improved over time. 

# Easy
plot(fittedAccProb, ylim = c(0,100), xlab = "Question Order", ylab = "Proability (%)", main = "Confidence vs. Accuracy (Easy Condition)", pch = 21, type = "b")
points(c(1:6), fittedFixedConf,pch = 22, type = "b")
abline(h = mean(CleanData[CleanData$condition == "easy", "TotalScorePerc"]), lty = 1)
abline(h = mean(CleanData[CleanData$condition == "easy", "ScoreEstPerc"]), lty = 2)
legend("right", c("Confidence", "% of participants who answered correctly"), pch = c(22, 21 ),  bty = "n")
legend("bottom", c( "% out of 9 Q actually correct","Median % prediced Correct"),lty = c(1,2),  bty = "n")
# well calibrated overall. Underconfident for item level.

# single animal over time-------
A1Long <- LongData[LongData$QuestionID == "A1",]
pirateplot(formula = Confidence ~ QuestionOrder + condition,
           data = A1Long,
           main = "Confidence by Question Order (by condition)",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)
pirateplot(formula = Confidence ~ QuestionOrder + condition + QuestionID,
           data = LongData,
           main = "Confidence by Question Order (by condition)",
           jitter.val = .1,
           ylab = "Confidence in Prediction",
           pal = "black",
           inf.method = "ci",
           inf.f.o = .5
)
op <- par(mfrow = c(1,1))
# GLM with gamma dist.  -----------------------------------------------------------------------------------------------------------------------------------------
##As we showed above, distribution of absolute error is right skewed and positive
## Are there error differences across question? treatments?
## Let's use workerId and QuestionCity as random effects because we are only interested in effects of Treatments, not animal or workerId variations
## Start with null model
fitErrTreat1a <- glmer(PercentErr ~ (1|workerId) + (1|QuestionID), family = Gamma(link = identity), data = LongDataTrim)

fitErrTreat1b <- glmer(I(PercentErr+1) ~ (1|workerId) + (1|QuestionID), family = Gamma(link = identity), data = LongDataTrim)
## I had to add 1 to the error to get the model to run.  There were no negative values but I think it didn't like the errors between 0 and 1.
ranef(fitErrTreat1b)  ## I don't think I fully understand what these are.  Intercepts for regressions across individual workerIds or cities, but in the Null model there is nothing to regress across?
## add Treatment
fitErrTreat2b <- glmer(I(PercentErr+1) ~ QuestionOrder + (1|workerId) + (1|QuestionID), family = Gamma(link = identity), data = LongDataTrim)
summary(fitErrTreat2b)   ## question order is significant
plot(fitErrTreat2b) # this looks much healtier than untrimmed model
plot(allEffects(fitErrTreat2b))  ## error decreases slightly with subsequent questions.  Effect is small but significant
## with log link--------------------------------------------------------------------------------------------------------------------------------------------
fitErrTreat1c <- glmer(I(PercentErr+1) ~ (1|workerId) + (1|QuestionID), family = Gamma(link = log), data = LongDataTrim)
## I had to add 1 to the error to get the model to run.  There were no negative values but I think it didn't like the errors between 0 and 1.
## warnings still suggest rescaling variables depite log link
ranef(fitErrTreat1c)  ## these would need to be "unlinked" by taking exp to be meaningful
## add Treatment
fitErrTreat2c <- glmer(I(PercentErr+1) ~ QuestionOrder + (1|workerId) + (1|QuestionID), family = Gamma(link = log), data = LongDataTrim)
summary(fitErrTreat2c)   
plot(fitErrTreat2c)
plot(allEffects(fitErrTreat2c))  
## Q order still decreasing (log) error

# condition
fitErrTreat3 <- glmer(I(PercentErr+1) ~ condition + (1|workerId) + (1|QuestionID), family = Gamma(link = log), data = LongDataTrim)
summary(fitErrTreat3)   
plot(fitErrTreat3)
plot(allEffects(fitErrTreat3))  

# interaction
fitErrTreat4 <- glmer(I(PercentErr+1) ~ QuestionOrder*condition + (1|workerId) + (1|QuestionID), family = Gamma(link = log), data = LongDataTrim)
summary(fitErrTreat4)   
plot(fitErrTreat4)
plot(allEffects(fitErrTreat4))  
