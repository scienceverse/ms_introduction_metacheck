#S4
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
CleanData <- read.csv("CleanDataAnon.csv")
LongData <- read.csv("LongDataAnon.csv")

# Demographics----
mean(CleanData$Age)
table(CleanData$Gender)[[1]]/nrow(CleanData)

# Descriptive stats-------
summary(LongData)
nrow(LongData[complete.cases(LongData),])

mean(LongData$Confidence, na.rm = T)
sd(LongData$Confidence, na.rm = TRUE)
ddply(LongData, "condition", summarise, conf = mean(Confidence, na.rm = T))
ddply(LongData, "condition", summarise, conf = sd(Confidence, na.rm = T))

mean(LongData$score, na.rm = T)
ddply(LongData, "condition", summarise, score = mean(score, na.rm = T))
ddply(LongData, "condition", summarise, score = sd(score, na.rm = T))

# Confidence over time----
# with z-scored confidence
#M&Ms
zfitConfTreatMM <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "mm")
summary(zfitConfTreatMM) #  b = -0.071, se = .023
confint(zfitConfTreatMM)
#Food
zfitConfTreatFood <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "food")
summary(zfitConfTreatFood) # b = -0.084, se = .029
confint(zfitConfTreatFood)
#Animals
zfitConfTreatAn <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "animal")
summary(zfitConfTreatAn) # b = -0.123, se = .030
confint(zfitConfTreatAn)
#People
zfitConfTreatP <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "person")
summary(zfitConfTreatP) # b = -0.129, se = .027
confint(zfitConfTreatP)
#Wx
zfitConfTreatWx <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "city")
summary(zfitConfTreatWx) # b = -0.080, se = .03
confint(zfitConfTreatWx)
#Mix
zfitConfTreatMix <- lmer(zconf ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "mix")
summary(zfitConfTreatMix) # b = -0.086, se = .023
confint(zfitConfTreatMix)
#Interaction
fitzConfx <- lmer(zconf ~ condition*QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
Anova(fitzConfx)
summary(fitzConfx) # main effect for question order.  No difference between conditioins.  

#collapse across conditions and control for error
fitzConf <- lmer(zconf ~ QuestionOrder + PercentErr + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
summary(fitzConf) # main effect for question order.  No difference between conditioins.  
confint(fitzConf)

# with raw confidence ratings
#M&Ms
fitConfTreatMM <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "mm")
summary(fitConfTreatMM) # b = -0.049, se = .014
#Food
fitConfTreatFood <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "food")
summary(fitConfTreatFood) # b = -0.057, se = .021
#Animals
fitConfTreatAn <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "animal")
summary(fitConfTreatAn) # b = -0.073, se = .019
#People
fitConfTreatP <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "person")
summary(fitConfTreatP) # b = -0.083, se = .018
#Wx
fitConfTreatWx <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "city")
summary(fitConfTreatWx) # b = -0.049, se = .022
#Mixed
fitConfTreatMix <- lmer(Confidence ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData, subset = condition == "mix")
summary(fitConfTreatMix) # b = -.078, se = .019
plot(allEffects(fitConfTreatMix))
#Interaction
fitConfTreatx <- lmer(Confidence ~ condition*QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongData)
summary(fitConfTreatx) # main effect for question order.  No difference between conditioins.  
plot(allEffects(fitConfTreatx, x.var = "QuestionOrder"), main = "Confidence as a function of Question Order", xlab = "Question Order", rug = FALSE, ylim = c(2,3))

#Figure-----
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

LongData$condition0 <- factor(LongData$condition, labels = c("Animal Weight", "City Temp Forecast", "Food Calories", "Mix of Domains","Number of M&Ms", "Weights of People"))
LongData$condition0 <- factor(LongData$condition0, levels = c("Number of M&Ms", "Animal Weight", "Food Calories", "Weights of People", "City Temp Forecast", "Mix of Domains"))
ggplot(LongData, aes(x=QuestionOrder, y=zconf)) + facet_wrap( ~ condition0, labeller = label_value) + 
  stat_summary(fun.y = mean, geom = "point", alpha = 4/5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, alpha = 4/5, position = position_dodge(.5)) + 
  geom_lm(interval = "confidence") +
  labs(x = "\nQuestion Order", y = "Z-Scored Confidence\n") + 
  apatheme +
  ggsave("MinsonUmphresFig5.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig5.jpeg", width=8, height=5, device="jpeg") #reported

# Error over time----
LongNoNA <- LongData[complete.cases(LongData),]
# exclude > 5 sd above average error
LongNoNA <- subset(LongNoNA, ErrorZ < 5)
3671-3660 # 11 observations eliminated

fitErrTreat2 <- lmer(PercentErr ~ QuestionOrder + (QuestionOrder|Subject) + (1|QuestionID), data = LongNoNA)
summary(fitErrTreat2)
#ns
fitErrTreat3 <- lmer(PercentErr ~ QuestionOrder*condition + (QuestionOrder|Subject) + (1|QuestionID), data = LongNoNA)
summary(fitErrTreat3)
plot(allEffects(fitErrTreat3, x.var = "QuestionOrder"))
#  Not significant


