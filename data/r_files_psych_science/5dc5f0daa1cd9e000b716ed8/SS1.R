# S1A
# load packages----
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
if (!require(plyr)) {install.packages("plyr"); require(plyr)}  #ddply
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}

# Load data----
CleanData <- read.csv("CleanDatav3.csv", header = T)
LongData <- read.csv("LongDatav3.csv", header = T)
# Demographics----
mean(CleanData$Age)
table(CleanData$Gender)[[1]]/nrow(CleanData)
#### Descriptive stats-----
mean(LongData$Confidence)
sd(LongData$Confidence)
mean(LongData$Accurate) # 14% of forecasts accurate

# Conf over time----
fitConfTreat2a <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionCity), data = LongData)
summary(fitConfTreat2a) # reported
confint(fitConfTreat2a)
plot(allEffects(fitConfTreat2a))
# with z scored confidence
zfitConfTreat2a <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionCity), data = LongData)
summary(zfitConfTreat2a) # reported
confint(zfitConfTreat2a)
plot(allEffects(zfitConfTreat2a))
df <- group_by(LongData, QuestionOrder) %>% dplyr::summarise(n = n(), mean = mean(Confidence, na.rm = T), 
                                                                            sd = sd(Confidence, na.rm = T), 
                                                                            se = plotrix::std.error(Confidence, na.rm = T))
#How often is the last estimate lower than the first?
CleanData$Delta <- CleanData$conf4 - CleanData$conf1
prop.table(table(sign(CleanData$Delta)))

# Error over time----
fitErrTime2 <- lmer(Error ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionCity), data = LongData)
summary(fitErrTime2) 
plot(allEffects(fitErrTime2))

# gamma correlation of confidence and error (resolution)
CorrBySub2 <- ddply(LongData, .(Subject), summarise,  GammaCor = rcorr.cens(Error, Confidence, outx = TRUE)[2]) 
hist(CorrBySub2)
mean(CorrBySub2$GammaCor, na.rm = T) # -.02.  On average, participants were more confident when their errors were smaller (barely) #reported

# effect of question order on z scored confidence controling for error
zfitConfTreat <- lmer(zconf ~ QuestionOrder + Error + (QuestionOrder|Subject) + (1|QuestionCity), data = LongData)
summary(zfitConfTreat) 
confint(zfitConfTreat)
plot(allEffects(zfitConfTreat))
