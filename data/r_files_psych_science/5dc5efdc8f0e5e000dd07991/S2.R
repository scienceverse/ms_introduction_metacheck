# S2
# load packages----
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)} # for gamma correlation
if (!require(plyr)) {install.packages("plyr"); require(plyr)}  #ddply
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ggformula)) {install.packages("ggformula"); require(ggformula)}

#load data----
CleanData <- read.csv("CleanDataAnon.CSV", header = TRUE) #excludes 29 cheaters, 11 attention check failures, and 4 duplicate ID.
LongData <- read.csv("LongDataAnon.CSV", header = TRUE)
#completion rates
## 178 of 220 participants in BDM complete (p1); 201 out of 220 control (p2); 197/220 in likert complete (p3)
## Are completion rates in each condition equally good? (H0: p1 = p2 = p3)
prop.test(x = c(178, 193, 191), n = c(220, 220, 220)) # overall survival rate marginally lower for BDM condition. 
# Demographics----
mean(CleanData$age)
table(CleanData$gender)[[1]]/nrow(CleanData)
#### Descriptive stats-------
ddply(LongData, "condition", summarise, mean = mean(Confidence))
ddply(LongData, "condition", summarise, sd = sd(Confidence))
ddply(LongData, "condition", summarise, Accuracy = mean(score))
# Conf over time-----
fitConfLikert <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "likert",])
summary(fitConfLikert) # b = -.026 (5pt scale) #reported
plot(allEffects(fitConfLikert, x.var = "QuestionOrder"))
# significant decline in conf w/ likert scale

fitConfControl <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "control",])
summary(fitConfControl) # b = -.62 (100 pt scale) #reported
plot(allEffects(fitConfControl, x.var = "QuestionOrder"))

fitConfBDM <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "BDM",])
summary(fitConfBDM) # b = -.4 (100 pt scale) #reported
plot(allEffects(fitConfBDM, x.var = "QuestionOrder"))

# z conf
zfitConfLikert <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "likert",])
summary(zfitConfLikert) # reported
confint(zfitConfLikert)
plot(allEffects(zfitConfLikert, x.var = "QuestionOrder"), rug = F, main = "Likert", ylim = c(-.25,.25))
# significant decline in conf w/ likert scale

zfitConfControl <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "control",])
summary(zfitConfControl) # reported
confint(zfitConfControl)
plot(allEffects(zfitConfControl, x.var = "QuestionOrder"), rug = F, main = "Prob", ylim = c(-.25,.25))

zfitConfBDM <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition == "BDM",])
summary(zfitConfBDM) # reported
confint(zfitConfBDM)
plot(allEffects(zfitConfBDM, x.var = "QuestionOrder"), rug = F, main = "BDM", ylim = c(-.25,.25))

# interaction btw question and condition----
fitOrderTreat3 <- lmer(zconf ~ QuestionOrder*condition + (1|workerId) + (1|QuestionID), data = LongData)
summary(fitOrderTreat3) 
plot(allEffects(fitOrderTreat3, x.var = "QuestionOrder"))
# no significant interactions

# Response time and confidence----
plot(LongData$time)
hist(LongData$time, xlim = c(0,80), breaks = 200)
median(LongData$time)
mean(LongData$time > 33) # 6% of responses were > 33s or about 3x above the mean
TimeTrim <- LongData[LongData$time < 33,] 
nrow(LongData) - nrow(TimeTrim) # 348 outliers
hist(TimeTrim$time)
timefit2 <- glmer(time ~ QuestionOrder + (1|workerId), data = TimeTrim, family = Gamma(link = log))
summary(timefit2)
exp(-0.059)  # about 1 sec less per question.  Decline is actually more than that from Q1 to Q2 and less from 2-9.
plot(allEffects(timefit2))
plot(TimeTrim$QuestionOrder, TimeTrim$time)

fitConfTimeTrim <- lmer(Confidence100 ~ log(time) + (1|workerId) + (1|QuestionID), data = TimeTrim)
summary(fitConfTimeTrim) # reported
plot(allEffects(fitConfTimeTrim))

# Error over time-----
LongDataTrim <- LongData[LongData$PercentErr < 1000,]
nrow(LongData) - nrow(LongDataTrim)
fitErrTreat1tmID <- lmer(PercentErr ~ QuestionOrder*condition + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongDataTrim)
summary(fitErrTreat1tmID) #reported
plot(allEffects((fitErrTreat1tmID)))
# basically no change

# Question order when controling for response time and error 
fitOrderTreat4 <- lmer(zconf ~ QuestionOrder + (1|workerId) + (1|QuestionID), data = LongData)
summary(fitOrderTreat4) 
plot(allEffects(fitOrderTreat4, x.var = "QuestionOrder"))

LongDataTrim2 <- TimeTrim[TimeTrim$PercentErr < 1000,]
fitOrderTreat4 <- lmer(zconf ~ QuestionOrder + (1|workerId) + (1|QuestionID), data = LongDataTrim2)
summary(fitOrderTreat4) 
plot(allEffects(fitOrderTreat4, x.var = "QuestionOrder"))

fitOrderTreat5 <- lmer(zconf ~ QuestionOrder + log(time) + PercentErr + (1|workerId) + (1|QuestionID), data = LongDataTrim2)
summary(fitOrderTreat5) # reported
confint(fitOrderTreat5)
plot(allEffects(fitOrderTreat5))


#figure----
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

LongData$condition0 <- factor(LongData$condition, labels = c("BDM", "Probability", "Likert"))
df <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(score, na.rm = T), se = plotrix::std.error(score, na.rm = T))
df$Measure <- "Accuracy"
df$mean <- df$mean*100
df$se <- df$se*100
df2 <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(Confidence100, na.rm = T), se = plotrix::std.error(Confidence100, na.rm = T))
df2$Measure <- "Confidence"
df3 <- df[,1:3]
df3$mean <- df2$mean-df$mean
df3$se <- 0
df3$Measure <- "Overconfidence"
df <- rbind(df, df2)
df$Measure <- factor(df$Measure, c("Confidence", "Accuracy"))

ggplot(df, aes(x=QuestionOrder, y=mean, pch = Measure)) + facet_grid(. ~ condition0, labeller = label_value) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
  geom_point(size = 2) + 
  geom_lm(interval = "confidence") +
  coord_cartesian(ylim = c(0,65)) + 
  labs(x = "\nQuestion Order", y = "Probability\n") + 
  scale_x_continuous(breaks=c(1:9)) +
  scale_shape_manual(values = c(1,17,8)) +
  apatheme + 
  theme(legend.position = c(.8,.8)) + guides( pch = guide_legend(title.theme = element_text(size = 9))) +
  scale_y_continuous(sec.axis = sec_axis(~./25+1, name = "", breaks = c(1:4), 
                                         labels = c('Not at all', 'Slightly', 'Moderately', 'Quite a bit'))) +
  ggsave("MinsonUmphresFig3.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig3.jpeg", width=8, height=5, device="jpeg")

#z figure----
df4 <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(zconf, na.rm = T), se = plotrix::std.error(zconf, na.rm = T))

ggplot(df4, aes(x=QuestionOrder, y=mean)) + facet_grid(. ~ condition0, labeller = label_value) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25) +
  geom_point(size = 2) + 
  geom_lm(interval = "confidence") +
  labs(x = "\nQuestion Order", y = 'Z-Scored Confidence\n') + 
  scale_x_continuous(breaks=c(1:9)) +
  apatheme + 
  ggsave("MinsonUmphresFig2.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig2.jpeg", width=8, height=5, device="jpeg")
