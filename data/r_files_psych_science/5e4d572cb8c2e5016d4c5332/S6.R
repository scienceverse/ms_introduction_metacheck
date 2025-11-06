#S6
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
if (!require(effsize)) {install.packages("effsize"); require(effsize)}  ## for coheh.d

# load data----
CleanData <- read.csv("CleanDataAnon.csv")
LongData <- read.csv("LongDataAnon.csv")
LongData$set <- factor(LongData$set)
LongDataRptConf <- LongData[LongData$condition=="repeatConf", ]
LongDataCtrl <- LongData[LongData$condition=="control", ]
DataSub1617 <- LongData[LongData$QuestionOrder == "16" | LongData$QuestionOrder == "17",]

# Demographics----
mean(CleanData$age)
table(CleanData$gender)[[1]]/nrow(CleanData)

# Descriptive stats-------
summary(LongData)
mean(LongData$Confidence, na.rm = T)
sd(LongData$Confidence, na.rm = TRUE)

# Confidence over time----
#set 1 contains only the first 4 questions. 
# with z-scored confidence
fitctrlz <- lmer(zconf ~ QuestionOrder.c + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongDataCtrl[LongDataCtrl$set == 1,])
summary(fitctrlz) # beta = -.11***
confint(fitctrlz)

fitrptz <- lmer(zconf ~ QuestionOrder.c + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongDataRptConf[LongDataRptConf$set == 1,])
summary(fitrptz) # beta = -.10***
confint(fitrptz)

# with raw confidence ratings
fitctrl <- lmer(Confidence ~ QuestionOrder.c + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongDataCtrl[LongDataCtrl$set == 1,])
summary(fitctrl) # b = -1.73***

fitrpt <- lmer(Confidence ~ QuestionOrder.c + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongDataRptConf[LongDataRptConf$set == 1,])
summary(fitrpt) # b = -1.58***

# examine changes from question 16 to question 17-----
# control condition should be fairly flat
t.test(LongDataCtrl[LongDataCtrl$QuestionOrder == 17,"zconf"], LongDataCtrl[LongDataCtrl$QuestionOrder == 16,"zconf"], paired = T) 
cohen.d(LongDataCtrl[LongDataCtrl$QuestionOrder == 17,"zconf"], LongDataCtrl[LongDataCtrl$QuestionOrder == 16,"zconf"], paired = T)
# reapeat conition should be big jump up
t.test(LongDataRptConf[LongDataRptConf$QuestionOrder == 17,"zconf"], LongDataRptConf[LongDataRptConf$QuestionOrder == 16,"zconf"], paired = T) 
cohen.d(LongDataRptConf[LongDataRptConf$QuestionOrder == 17,"zconf"], LongDataRptConf[LongDataRptConf$QuestionOrder == 16,"zconf"], paired = T)

# interaction
fit1617 <- lmer(zconf ~ QuestionOrder*condition + (1|workerId) + (1|QuestionID), data = DataSub1617)
summary(fit1617)
confint(fit1617)
plot(allEffects(fit1617, x.var = "QuestionOrder"))

# using raw confidences
# control condition should be fairly flat
t.test(LongDataCtrl[LongDataCtrl$QuestionOrder == 16,"Confidence"], LongDataCtrl[LongDataCtrl$QuestionOrder == 17,"Confidence"], paired = T) 
cohen.d(LongDataCtrl[LongDataCtrl$QuestionOrder == 16,"Confidence"], LongDataCtrl[LongDataCtrl$QuestionOrder == 17,"Confidence"], paired = T)
# reapeat conition should be big jump up
t.test(LongDataRptConf[LongDataRptConf$QuestionOrder == 16,"Confidence"], LongDataRptConf[LongDataRptConf$QuestionOrder == 17,"Confidence"], paired = T) 
cohen.d(LongDataRptConf[LongDataRptConf$QuestionOrder == 16,"Confidence"], LongDataRptConf[LongDataRptConf$QuestionOrder == 17,"Confidence"], paired = T)
# interaction
fit1617 <- lmer(Confidence ~ QuestionOrder*condition + (1|workerId) + (1|QuestionID), data = DataSub1617)
summary(fit1617)
plot(allEffects(fit1617, x.var = "QuestionOrder"))

# second set comparison of means and slopes between conditions-----
# Control Conf set 2 slope
fitCtrlSet2 <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition=="control" & LongData$set == 2,])
summary(fitCtrlSet2) # marginally significant increase in control condition
plot(allEffects(fitCtrlSet2))
# rptConf set 2 slope
fitRptSet2 <- lmer(zconf ~ QuestionOrder + (1 + QuestionOrder|workerId) + (1|QuestionID), data = LongData[LongData$condition=="repeatConf" & LongData$set == 2,])
summary(fitRptSet2) # no order effect in set 2 in Rpt Condition
# Interaction
Main2 <- lmer(zconf ~ QuestionOrder.c*condition + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongData[LongData$set==2,])
summary(Main2) # interaction ns
plot(allEffects(Main2))
Main3 <- lmer(zconf ~ QuestionOrder.c + condition + (1 + QuestionOrder.c|workerId) + (1|QuestionID), data = LongData[LongData$set==2,])
summary(Main3) # effect of condition significant
confint(Main3)
plot(allEffects(Main3))

# slope attenuated in repeat condition for repeat questions in set 2?
Exp1 <- lmer(zconf ~ set*QuestionOrder.c + (1|workerId) + (1|QuestionID), data = LongDataRptConf)
summary(Exp1)
confint(Exp1)
plot(allEffects(Exp1, x.var = "QuestionOrder.c"))


colnames(CleanData)
a1 <- CleanData[CleanData$condition == "repeatConf",c("A1conf", "A12conf")]
colnames(a1) <- c("c1", "c2")
a2 <- CleanData[CleanData$condition == "repeatConf",c("A2conf", "A22conf")]
colnames(a2) <- c("c1", "c2")
a3 <- CleanData[CleanData$condition == "repeatConf",c("A3conf", "A32conf")]
colnames(a3) <- c("c1", "c2")
a4 <- CleanData[CleanData$condition == "repeatConf",c("A4conf", "A42conf")]
colnames(a4) <- c("c1", "c2")
a5 <- CleanData[CleanData$condition == "repeatConf",c("A5conf", "A52conf")]
colnames(a5) <- c("c1", "c2")
a6 <- CleanData[CleanData$condition == "repeatConf",c("A6conf", "A62conf")]
colnames(a6) <- c("c1", "c2")
a7 <- CleanData[CleanData$condition == "repeatConf",c("A7conf", "A72conf")]
colnames(a7) <- c("c1", "c2")
a8 <- CleanData[CleanData$condition == "repeatConf",c("A8conf", "A82conf")]
colnames(a8) <- c("c1", "c2")
Rpt <- rbind(a1,a2,a3,a4,a5,a6,a7,a8) # paired comparisons of all initial and revisited confidences
t.test(Rpt$c1, Rpt$c2, paired = T) # The first confidence assessment was marginally higher than the second
mean(Rpt$c1, na.rm = T)
mean(Rpt$c2, na.rm = T)

# figures----
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

LongData$condition0 <- factor(LongData$condition, labels = c("Control", "Repeated Confidence"))
dfz <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(zconf, na.rm = T), 
                                                                             sd = sd(zconf, na.rm = T), 
                                                                             se = plotrix::std.error(zconf, na.rm = T))
dfz$Type <- 'New'
dfz[dfz$condition0 == 'Repeated Confidence' & dfz$QuestionOrder >= 17, 'Type'] <- 'Revisited'
dfz$rep <- factor(dfz$Type)
ggplot(dfz, aes(x=QuestionOrder, y=mean)) + facet_grid(. ~ condition0, labeller = label_value) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) +
  geom_line(lty = 5) + geom_point(size = 2) + coord_cartesian(ylim = c(-.7,.7)) + 
  labs(x = "\nQuestion Order", y = "Z-Scored Confidence\n") + 
  scale_x_continuous(breaks=c(1,4,8,12,16,20)) +
  # theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman"))+  
  # theme(panel.grid.minor = element_blank(), 
  #       strip.background = element_rect(fill="white")) +
  apatheme + 
  ggsave("MinsonUmphresFig6.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("MinsonUmphresFig6.jpeg", width=8, height=5, device="jpeg")

df <- LongData %>% group_by(QuestionOrder, condition0) %>% dplyr::summarise(n = n(), mean = mean(Confidence, na.rm = T), 
                                                                             sd = sd(Confidence, na.rm = T), 
                                                                             se = plotrix::std.error(Confidence, na.rm = T))
df$Type <- 'New'
df[df$condition0 == 'Repeated Confidence' & df$QuestionOrder >= 17, 'Type'] <- 'Revisited'
df$rep <- factor(df$Type)
ggplot(df, aes(x=QuestionOrder, y=mean)) + facet_grid(. ~ condition0, labeller = label_value) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) +
  geom_line(lty = 5) + geom_point(size = 2, aes(col = Type)) + 
  labs(x = "\nQuestion Order", y = "Confidence (%)", col = "Estimate Type") + 
  scale_color_manual(values = c(16,17)) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20)) +
  theme_bw() + theme(axis.title.y = element_text(angle=0, vjust = .5), text = element_text(size = 16, family = "Times New Roman"))+  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white"),
        legend.position = c(.75,.8)) +
  ggsave("Repeat.pdf", width=8, height=5, device=cairo_pdf) +
  ggsave("Repeat.jpeg", width=8, height=5, device="jpeg")

