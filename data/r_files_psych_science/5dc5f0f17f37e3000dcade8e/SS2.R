# SS2 - Feedback
# load packages----
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plotrix)) {install.packages("plotrix"); require(plotrix)}
if (!require(reshape)) {install.packages("reshape"); require(reshape)}
if (!require(yarrr)) {install_github("ndphillips/yarrr"); require(yarrr)} 
if (!require(VGAM)) {install.packages("VGAM"); require(VGAM)} #vglm for orginal w/ p values
if (!require(ordinal)) {install.packages("ordinal"); require(ordinal)} ## package for ordinal logistic regression
if (!require(effects)) {install.packages("effects"); require(effects)} 
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(car)) {install.packages("car"); require(car)}
if (!require(mosaic)) {install.packages("mosaic"); require(mosaic)}
if (!require(MASS)) {install.packages("MASS"); require(MASS)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
if (!require(plyr)) {install.packages("plyr"); require(plyr)}  #ddply

# load data----
CleanData <- read.csv("CleanDatav3.csv")
LongData <- read.csv("LongDatav3.csv")

# Demographics----
mean(CleanData$Age)
table(CleanData$Gender)[[1]]/nrow(CleanData)

# Descriptive stats-------
summary(LongData)
t.test(LongData$Confidence, na.rm = T)
sd(LongData$Confidence, na.rm = T)
t.test(LongData$score, na.rm = T)

fitScore <- glm(score ~ condition , data = LongData, family = "binomial")
summary(fitScore)
confint(fitScore)
plot(allEffects(fitScore))
# no difference in accuracy between conditions

# Confidence over time----
# with standardized confidence
fitConfFBz <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData[LongData$condition == "FB",])
summary(fitConfFBz) #reported
confint(fitConfFBz)
fitConfnoFBz <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData[LongData$condition == "noFB",])
summary(fitConfnoFBz) #reported
confint(fitConfnoFBz)
fitConfTreat4z <- lmer(zconf ~ condition*QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
Anova(fitConfTreat4z) # Significant interaction
summary(fitConfTreat4z) #interaction reported
confint(fitConfTreat4z)
plot(allEffects(fitConfTreat4z, x.var = "QuestionOrder"), main = "Confidence (standardized within participant) as a function of Question Order", xlab = "Question Order", rug = FALSE)

# with raw confidence ratings
fitConfFB <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData[LongData$condition == "FB",])
summary(fitConfFB) #reported
fitConfnoFB <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData[LongData$condition == "noFB",])
summary(fitConfnoFB) #reported
fitConfTreat4 <- lmer(Confidence ~ condition*QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
Anova(fitConfTreat4) # Significant interaction
summary(fitConfTreat4) #interaction reported
plot(allEffects(fitConfTreat4, x.var = "QuestionOrder"), main = "Confidence as a function of Question Order", xlab = "Question Order", rug = FALSE)

#Figure------
LongData$condition0 <- factor(LongData$condition, labels = c("Feedback", "No Feedback"))
ggplot(LongData, aes(x=QuestionOrder, y=zconf)) + facet_wrap( ~ condition0, labeller = label_value) + 
  stat_summary(fun.y = mean, geom = "point", alpha = 4/5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2, alpha = 4/5, position = position_dodge(.5)) + 
  geom_lm(interval = "confidence") + scale_x_continuous(breaks=c(1:9)) +
  labs(x = "\nQuestion Order", y = "Z-Scored\nConfidence") + 
  theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman")) +   
  theme(strip.background = element_rect(fill="white")) +
  ggsave("Feedback.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("Feedback.jpeg", width=8, height=5, device="jpeg") #reported

# Error over time------
TrimData <- subset(LongData, PercentErr < 1000)
5022-4999 # exclued 23 excessively large estimates
# Does error increase/decrease over time?
fitErrTreat2a <- lmer(PercentErr ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = TrimData[TrimData$condition == "FB",])
summary(fitErrTreat2a) # reported
confint(fitErrTreat2a)
plot(allEffects(fitErrTreat2a))

fitErrTreat2b <- lmer(PercentErr ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = TrimData[TrimData$condition == "noFB",])
summary(fitErrTreat2b) # ns reported
plot(allEffects(fitErrTreat2b))

fitErrTreat3 <- lmer(PercentErr ~ QuestionOrder*condition + (QuestionOrder|Subject) + (1|QuestionID), data = TrimData)
summary(fitErrTreat3) # reported
confint(fitErrTreat3)
plot(allEffects(fitErrTreat3, x.var = "QuestionOrder"))
#  Interaction is significant

# gamma correlation of confidence and error (resolution)
CorrBySub2 <- ddply(LongData, .(Subject), summarise,  GammaCor = rcorr.cens(PercentErr, Confidence, outx = TRUE)[2]) 
mean(CorrBySub2$GammaCor, na.rm = T) # .02.  Participants were more confident when their errors were smaller (barely) #reported

# Is confidence decline mediated by error? No.
# with standardized confidence
fitConfTreat4zErr <- lmer(zconf ~ condition*QuestionOrder + PercentErr*condition+ (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
Anova(fitConfTreat4zErr) 
summary(fitConfTreat4zErr) 
confint(fitConfTreat4zErr)
plot(allEffects(fitConfTreat4zErr), main = "Confidence (standardized within participant) as a function of Question Order", rug = FALSE)

