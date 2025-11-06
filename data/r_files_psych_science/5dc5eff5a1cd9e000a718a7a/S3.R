# S3
#load packages----
if (!require(dplyr)) {install.packages("dplyr"); require(dplyr)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}
if (!require(readr)) {install.packages("readr"); require(readr)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
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
if (!require(plotrix)) {install.packages("plotrix"); require(plotrix)}  ## for std error breakdown

#load data----
CleanData <- read.csv("CleanDataAnon.CSV", header = TRUE)
LongData <- read.csv("LongDataAnon.CSV", header = TRUE)
LongData$condition0 <- factor(LongData$condition, labels = c("Forecast", "Control"))
LongDataHard <- LongData[LongData$condition == "hard",]
LongDataForecast <- LongData[LongData$condition == "forecast",]

# Demographics----
mean(CleanData$age)
table(CleanData$gender)[[1]]/nrow(CleanData)

# Descriptive stats-------
summary(LongData)
mean(LongData$Confidence, na.rm = T)
sd(LongData$Confidence, na.rm = T)
mean(LongData$score, na.rm = T)

# What did participants in the forcast condition predict their confidence would do over time?----
skippers <- as.character(LongDataForecast[LongDataForecast$QuestionOrder == 5 & is.na(LongDataForecast$Confidence), "workerId"]) # 3 participants did not provide a prediction.
ind <- which(LongDataForecast$workerId %in% skippers)
LongDataForecast <- LongDataForecast[-ind,]
t.test(LongDataForecast[LongDataForecast$QuestionOrder==1, "Confidence"], LongDataForecast[LongDataForecast$QuestionOrder==5, "Confidence"], paired = T)
cohen.d(LongDataForecast[LongDataForecast$QuestionOrder==1, "Confidence"], LongDataForecast[LongDataForecast$QuestionOrder==5, "Confidence"], paired = T)
# significant increase in forecast condition
# when asked explicitly to predict if their confidence would increase or decrease
table2 <- table(CleanData$expectation)
prop.table(table2)
# By this measure, only 23 forecasted a decrease.  The remaining 173 forecasted the same or increasing confidence

# Confidence over time in the control condition----
fitConf <- lmer(Confidence ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongDataHard)
summary(fitConf) # b = -.047 (5 pt scale) p < .0001
plot(allEffects(fitConf, x.var = "QuestionOrder"))

fitConfForecast <- lmer(Confidence ~ QuestionOrder + (1|workerId), data = LongDataForecast) # no QuestionID for forecast and only two points per person so only random effect is for participant
summary(fitConfForecast) # b = .035 (5 pt scale) p = .006
plot(allEffects(fitConfForecast, x.var = "QuestionOrder"))

# with z scored confidence
fitzConf <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongDataHard)
summary(fitzConf) 
confint(fitzConf)
plot(allEffects(fitzConf, x.var = "QuestionOrder"))
#interaction
fitConfx <- lmer(Confidence ~ QuestionOrder*condition + (1 + QuestionOrder|workerId), data = LongData)
summary(fitConfx) # b = -.081 (5 pt scale) p < .0001
plot(allEffects(fitConfx, x.var = "QuestionOrder"))

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

ggplot(LongData, aes(x=QuestionOrder, y=Confidence)) + facet_grid(. ~ condition0, labeller = label_value) + 
  stat_summary(fun.y = mean, geom = "point", alpha = 4/5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, alpha = 4/5, position = position_dodge(.5)) + 
  geom_lm(interval = "confidence") +
  coord_cartesian(ylim = c(2.25,3.0)) + 
  labs(x = "\nQuestion Order", y = "Confidence\n") + 
  apatheme +
  ggsave("MinsonUmphresFig4.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig4.jpeg", width=8, height=5, device="jpeg")

