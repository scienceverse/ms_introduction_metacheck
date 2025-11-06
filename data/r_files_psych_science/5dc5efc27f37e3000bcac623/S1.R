# S1
# load packages----
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
if (!require(plyr)) {install.packages("plyr"); require(plyr)}  #ddply
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ggformula)) {install.packages("ggformula"); require(ggformula)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
# Clean Data---- 
# You can skip to load data if you don't want to make any changes to exclusions
Data <- read.csv("HandCleanedDataAnon.CSV", header = TRUE) # this data set manually selected and renamed columns, deleted previews and incomplete responses, removed identifying information and created new participant IDs to replace MTurk IDs
## 202 rows

#Factor Gender
Data$gender <- factor(Data$gender, levels = c(1,2,3), labels = c("male", "female", "other"))
#Factor Education level
Data$ed <- factor(Data$ed, levels = c(1:8), labels = c("less than HS", "HS", "Some college", "Associates", "Bachelor's", "Mater's", "PhD", "Prof"))

## remove people who looked up info on the web. 
Data <- Data[Data$cheat == 0,] #198 - 4 cheaters removed.  Comment out this line to run analysis including these participants.
## remove failed attention checks
Data <- Data[is.na(Data$smack),] #192 - 6 failed attention checks removed. Comment out this line to run analysis including these participants.
# create vectors to hold unscrambled data
Q1 <- NULL
Q2 <- NULL
Q3 <- NULL
Q4 <- NULL
Q5 <- NULL
Q6 <- NULL
Q7 <- NULL
Q8 <- NULL
Q9 <- NULL

est1 <- NULL
est2 <- NULL
est3 <- NULL
est4 <- NULL
est5 <- NULL
est6 <- NULL
est7 <- NULL
est8 <- NULL
est9 <- NULL

conf1 <- NULL
conf2 <- NULL
conf3 <- NULL
conf4 <- NULL
conf5 <- NULL
conf6 <- NULL
conf7 <- NULL
conf8 <- NULL
conf9 <- NULL

time1 <- NULL
time2 <- NULL
time3 <- NULL
time4 <- NULL
time5 <- NULL
time6 <- NULL
time7 <- NULL
time8 <- NULL
time9 <- NULL

score1 <- NULL
score2 <- NULL
score3 <- NULL
score4 <- NULL
score5 <- NULL
score6 <- NULL
score7 <- NULL
score8 <- NULL
score9 <- NULL

# Which stimulous was presented first, second, third...
for (i in 1:nrow(Data)) {
  Q1[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][1]  
  Q2[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][2]
  Q3[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][3]
  Q4[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][4]
  Q5[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][5]
  Q6[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][6]  
  Q7[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][7]
  Q8[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][8]
  Q9[i] <- strsplit(as.character(Data[i,"Question.Order"]), ",")[[1]][9]
}

Data <- cbind(Data, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9)

# pull the estimate for the specifid question ex if Q1 = "A3", pull value in collumn "A3est" for est1 etc.
for (i in 1:nrow(Data)){
  for (l in 1:9){ # l is the question version in the category 1-9
    if (Q1[i]== paste0("A", l)) {
      est1[i] <- Data[i,paste0("A", l, "est")]
      conf1[i] <- Data[i,paste0("A", l, "conf")]
      time1[i] <- Data[i,paste0("A", l, "time")]
      score1[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q2[i]== paste0("A", l)) {
      est2[i] <- Data[i,paste0("A", l, "est")]
      conf2[i] <- Data[i,paste0("A", l, "conf")]
      time2[i] <- Data[i,paste0("A", l, "time")]
      score2[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q3[i]== paste0("A", l)) {
      est3[i] <- Data[i,paste0("A", l, "est")]
      conf3[i] <- Data[i,paste0("A", l, "conf")]
      time3[i] <- Data[i,paste0("A", l, "time")]
      score3[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q4[i]== paste0("A", l)) {
      est4[i] <- Data[i,paste0("A", l, "est")]
      conf4[i] <- Data[i,paste0("A", l, "conf")]
      time4[i] <- Data[i,paste0("A", l, "time")]
      score4[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q5[i]== paste0("A", l)) {
      est5[i] <- Data[i,paste0("A", l, "est")]
      conf5[i] <- Data[i,paste0("A", l, "conf")]
      time5[i] <- Data[i,paste0("A", l, "time")]
      score5[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q6[i]== paste0("A", l)) {
      est6[i] <- Data[i,paste0("A", l, "est")]
      conf6[i] <- Data[i,paste0("A", l, "conf")]
      time6[i] <- Data[i,paste0("A", l, "time")]
      score6[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q7[i]== paste0("A", l)) {
      est7[i] <- Data[i,paste0("A", l, "est")]
      conf7[i] <- Data[i,paste0("A", l, "conf")]
      time7[i] <- Data[i,paste0("A", l, "time")]
      score7[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q8[i]== paste0("A", l)) {
      est8[i] <- Data[i,paste0("A", l, "est")]
      conf8[i] <- Data[i,paste0("A", l, "conf")]
      time8[i] <- Data[i,paste0("A", l, "time")]
      score8[i] <- Data[i,paste0("R", l, "Score")]
    }
    if (Q9[i]== paste0("A", l)) {
      est9[i] <- Data[i,paste0("A", l, "est")]
      conf9[i] <- Data[i,paste0("A", l, "conf")]
      time9[i] <- Data[i,paste0("A", l, "time")]
      score9[i] <- Data[i,paste0("R", l, "Score")]
    }
  }
}
# add the place holder vectors to the data.frame
Data <- cbind(Data, est1, est2, est3, est4, est5, est6, est7, est8, est9)
Data <- cbind(Data, conf1, conf2, conf3, conf4, conf5, conf6, conf7, conf8, conf9)
Data <- cbind(Data, time1, time2, time3, time4, time5, time6, time7, time8, time9)
Data <- cbind(Data, score1, score2, score3, score4, score5, score6, score7, score8, score9)

names(Data)

## load truth data
truthData <- read.csv("Truth data.csv", header = TRUE)

## compute raw error for each question 
PercErrmat <- data.frame()
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    PercErrmat[i,j] <- (Data[i,paste0(truthData$Item[j], "est")] - truthData$Weight[j])/truthData$Weight[j]*100
  }
}
colnames(PercErrmat) <- c(paste0(truthData$Item, "err"))  
PercErrmat <- abs(PercErrmat)

## Abs % Error vectors by question
err1 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q1[i] == truthData$Item[j]) {err1[i] <- PercErrmat[i,j]}  
  }
}

err2 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q2[i] == truthData$Item[j]) {err2[i] <- PercErrmat[i,j]}  
  }
}
err2

err3 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q3[i] == truthData$Item[j]) {err3[i] <- PercErrmat[i,j]}  
  }
}
err3

err4 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q4[i] == truthData$Item[j]) {err4[i] <- PercErrmat[i,j]}  
  }
}
err4

err5 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q5[i] == truthData$Item[j]) {err5[i] <- PercErrmat[i,j]}  
  }
}

err6 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q6[i] == truthData$Item[j]) {err6[i] <- PercErrmat[i,j]}  
  }
}

err7 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q7[i] == truthData$Item[j]) {err7[i] <- PercErrmat[i,j]}  
  }
}

err8 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q8[i] == truthData$Item[j]) {err8[i] <- PercErrmat[i,j]}  
  }
}

err9 <- NULL 
for (i in 1:nrow(Data)) {
  for (j in 1:nrow(truthData)) {
    if (Data$Q9[i] == truthData$Item[j]) {err9[i] <- PercErrmat[i,j]}  
  }
}

### Create a more compact data set to analize. Get rid of extraneous data
CleanData <- NULL
CleanData <- cbind.data.frame(Subject = Data$workerId, Data[,48:92], err1, err2, err3, err4, err5, err6, err7, err8, err9, Age = Data$age, Gender = Data$gender, Edlevel = Data$ed ) 
names(CleanData)
write.csv(CleanData, "CleanDataAnon.csv", row.names = F)

## Long format
head(CleanData)
LongData <- NULL
LongData <- melt(CleanData, measure.vars = c(paste0("Q", c(1:9))), variable_name = "QuestionOrder" )
colnames(LongData)[length(LongData)] <- "QuestionID"
## Question order as numberic so we can run linear regression
LongData$QuestionOrder <- as.numeric(LongData$QuestionOrder)
# add estimate and error to the long data frame for each city/question
err <- melt(CleanData, measure.vars = c(paste0("err", 1:9)))
LongData$PercError <- err$value
conf <- (melt(CleanData, measure.vars = c(paste0("conf", c(1:9)))))
LongData$Confidence <- conf$value
est <- (melt(CleanData, measure.vars = c(paste0("est", c(1:9)))))
LongData$est <- est$value
time <- (melt(CleanData, measure.vars = c(paste0("time", c(1:9)))))
LongData$time <- time$value
score <- (melt(CleanData, measure.vars = c(paste0("score", c(1:9)))))
LongData$Accurate <- score$value

# Eliminate the redundant estimate and error Data that has been melted
LongData <- LongData[,-c(2:46)]
LongData <- LongData[order(LongData$Subject), ]
LongData <- ddply(LongData, "Subject", mutate, zconf = scale(Confidence))
LongData[is.na(LongData)] <- 0 #replace NaN (created by div by 0 variance for participants who never changed conf) with 0 
LongData$set <- ifelse(LongData$QuestionOrder <= 5, 1, 2)
write.csv(LongData, "LongDataAnon.csv", row.names = F)

# Load data----
CleanData <- read.csv("CleanDataAnon.csv", header = T) # These data sets exclude 4 participants who looked up information online & 6 participants who missed an attention check. They do not exclude 
LongData <- read.csv("LongDataAnon.csv", header = T)
LongData1 <- subset(LongData, set == 1)
LongData2 <- subset(LongData, set == 2)
# Demographics----
mean(CleanData$Age)
table(CleanData$Gender)[[1]]/nrow(CleanData)
#### Descriptive stats-------
t.test(LongData$Confidence)
mean(LongData$Accurate) # 9% of forecasts accurate
# Conf over time-----
fitConfTreat2 <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
summary(fitConfTreat2) #reported
plot(allEffects(fitConfTreat2))
#zconf
zfitConfTreat2 <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
summary(zfitConfTreat2) #reported
confint(zfitConfTreat2)
plot(allEffects(zfitConfTreat2))

# #figure----
# df <- LongData %>% group_by(QuestionOrder) %>% dplyr::summarise(n = n(), mean = mean(Confidence, na.rm = T), 
#                                                                             sd = sd(Confidence, na.rm = T), 
#                                                                             se = plotrix::std.error(Confidence, na.rm = T))
# 
# ggplot(df, aes(x=QuestionOrder, y=mean)) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
#   geom_point(size = 2) + 
#   geom_lm(interval = "confidence") +
#   labs(x = "\nQuestion Order", y = "Confidence") + 
#   guides(fill = guide_legend(reverse=F)) + 
#   scale_x_continuous(breaks=c(1:9)) +
#   theme_bw() + 
#   theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman")) +
#   ggsave("Figure.pdf", width=8, height=5, device=cairo_pdf) +
#   ggsave("Figure.jpeg", width=8, height=5, device="jpeg")

#z figure----
apatheme=theme_bw()+
  theme(#panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        title = element_text(size = 12),
        axis.title.y = element_text(angle=90, vjust = .5, size = 10),
        axis.title.x = element_text(size = 10),
        text = element_text(size = 9, family = "Helvetica")
  )

df4 <- LongData %>% group_by(QuestionOrder) %>% dplyr::summarise(n = n(), mean = mean(zconf, na.rm = T), se = plotrix::std.error(zconf, na.rm = T))

ggplot(df4, aes(x=QuestionOrder, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25) +
  geom_point(size = 2) + 
  geom_lm(interval = "confidence") +
  labs(x = "\nQuestion Order", y = 'Z-Scored Confidence\n') + 
  scale_x_continuous(breaks=c(1:9)) +
  apatheme + 
  ggsave("MinsonUmphresFig1.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig1", width=8, height=5, device="jpeg")

# response time over time----
plot(LongData$time)
hist(LongData$time, xlim = c(0,80), breaks = 200)
median(LongData$time) # ~11 seconds
mean(LongData$time > 33) # 8% of responses were > 22s or about 3x above the mean
TimeTrim <- LongData[LongData$time < 33,] 
nrow(LongData) - nrow(TimeTrim) # 145 outliers
hist(TimeTrim$time)
timefit <- glmer(time ~ QuestionOrder + (1|Subject), data = TimeTrim, family = Gamma(link = log))
summary(timefit) #reported
exp(-0.0627)
plot(allEffects(timefit))
# Error over time-----
hist(LongData$PercError)
LongDataTrim <- LongData[LongData$PercError < 1000,]
hist(LongDataTrim$PercError)
nrow(LongData) - nrow(LongDataTrim)
fitErrTreat2a <- lmer(PercError ~ QuestionOrder + (1|Subject), data = LongDataTrim)
summary(fitErrTreat2a) # reported
plot(allEffects(fitErrTreat2a))

# Response time and confidence
fitConfTime <- lmer(Confidence ~ log(time) + QuestionOrder + (1|Subject), data = TimeTrim)
summary(fitConfTime) # response time ns predictor of confidence
plot(allEffects(fitConfTime))
# does (log)time predict accuracy?-------
fitScoreTime <- glm(Accurate ~ log(time), data = TimeTrim, family = "binomial")
summary(fitScoreTime) 
plot(allEffects(fitScoreTime))
#nope
# does (log)time predict error?-------
fitErrTime <- lm(PercError ~ log(time), data = TimeTrim)
summary(fitErrTime) 
plot(allEffects(fitErrTime))
#nope

# gamma correlation of confidence and error (resolution)
CorrBySub2 <- ddply(LongDataTrim, .(Subject), summarise,  GammaCor = rcorr.cens(PercError, Confidence, outx = TRUE)[2]) 
mean(CorrBySub2$GammaCor, na.rm = T) # -.04.  Participants were more confident when their errors were smaller (barely) #reported
hist(CorrBySub2)
sd(CorrBySub2$GammaCor, na.rm = T)

# question order controling for response time and error
LongDataTrim2 <- TimeTrim[TimeTrim$PercError < 1000,]
zfitConfTreat3 <- lmer(zconf ~ QuestionOrder + log(time) + PercError + (QuestionOrder|Subject) + (1|QuestionID), data = LongDataTrim2)
summary(zfitConfTreat3) #reported
confint(zfitConfTreat3)
plot(allEffects(zfitConfTreat3))

