# -----------Aug 24,2021-------------------------
# -----------Author: Rui Sun---------------------
# -----------Data Analysis for Study 1-----------


rm(list = ls())
# setwd("")
library(psych)
library(dplyr)
library(nlme)
library(reshape2)
library(stargazer)
library(readr)
library(MuMIn)
library(buildmer)
library(lme4)
library(optimx)
library(ggplot2)
library(ggpubr)
library(papeR)
library(reghelper)
library(gridExtra)
library(merTools)
require(ggiraph)
require(ggiraphExtra)
require(careless)
require(mediation)
require(lavaan)
require(tidyverse)


# -----------------------------------------
#-----------0.data preparation--------------
# -----------------------------------------

cc <- read.csv("study1_data.csv") #in this dataset death per million and policty stringency were entered by date participants answered the questionnaire, therefore they are Level 1 variables
ctrl <- read.csv("control_global_figures_sr.csv")
ctrl$log_gdp <- log(ctrl$GDP_per_capita_PPP)

cc <- merge.data.frame(cc,ctrl,by="countryname")

# =============== data cleaning ======================

# 1. remove those who don't have age data
# remove age == 999, that is, prefer not to say
describe(cc$age)
cc$age[cc$age == 999] <- NA
cc <- cc[which(!is.na(cc$age)),] #n=23842

# 2. only include people who answered all 20 emotions
cc1 <- cc[complete.cases(cc[,c("admiration","calm","compassion","determination","moved","gratitude","hope","love","relief","pleasure","anger","anxiety","boredom","confusion", "disgust","fear","frustration","loneliness","regret", "sadness" )]),] #N=23562


# 3. do some data cleaning of this dataset, remove those who didn't answer carefully (on emotion questions)
cc1$long_str <- longstring(cc1[,c("admiration","calm","compassion","determination","moved","gratitude","hope","love","relief","pleasure","anger","anxiety","boredom","confusion", "disgust","fear","frustration","loneliness","regret", "sadness" )], avg = FALSE)
table(cc1$long_str)
# remove those that have more than 10 continuous same emotion answers
cc1 <- cc1[which(cc1$long_str<11),] #23350
table(cc1$countryname)

# 77 participants are needed if meeting this criteria: Tables 3- 6 (R2 = .18), α = .05, power = .8, Npredictor = 8, as criteria
country <- as.data.frame(table(cc1$countryname))
# 63 countries, with Poland fewest, 96, so all 63 countries were included in the analyses

# =============create outcomes=================
cc <- cc1
rm(cc1,ctrl)

cc$pos <- rowMeans(cc[,c("admiration", "calm", "compassion","determination","moved","gratitude","hope", "love","relief","pleasure")],na.rm=TRUE)
cc$neg <- rowMeans(cc[,c( "anger","anxiety","boredom", "confusion","disgust","fear","frustration","loneliness","regret","sadness")],na.rm=TRUE)

# =============create control variables=================
table(cc$gender)
# 1: male, 2: female, 3:other, 4:don't want to disclose
# similar to Carstensen, we will code female = 1, male + other = 0, do not want to disclose=NA

cc$gender[cc$gender>3] <- NA #code do not want to disclose=NA
cc$gender[cc$gender==3] <- 1

# coding female =1 male =0
cc$gender <- as.numeric(cc$gender)
cc$gender <- cc$gender-1
cc$gender <- as.factor(cc$gender)
table(cc$gender,useNA = "always")
# 0       1     <NA> 
# 6941  16254   155 

describe(cc$education)
cc$education[cc$education == 999] <- NA
table(cc$education,useNA = "always")

# code personality
cc$reserved_recode <- 8-cc$reserved
cc$critical_recode <- 8-cc$critical
cc$disorganised_recode <- 8-cc$disorganised
cc$anxious_recode <- 8-cc$anxious
cc$conventional_recode <- 8-cc$conventional

cc$Extraversion <- rowMeans(cc[,c("extravert","reserved_recode")],na.rm=TRUE)
cc$Agreeableness <- rowMeans(cc[,c("critical_recode","sympathy_personality")],na.rm=TRUE)
cc$Conscientiousness <- rowMeans(cc[,c("dependent","disorganised_recode")],na.rm=TRUE)
cc$EmoStability <-  rowMeans(cc[,c("anxious_recode","calm_personality")],na.rm=TRUE)
cc$Openness <- rowMeans(cc[,c("open","conventional_recode")],na.rm=TRUE)

# deal with employment
# 1-full time 2-parttime 3-retired 4-student 5-unemployed
table(cc$employment,useNA = "always")
# create"working"
cc$working[cc$employment=="1"|cc$employment=="1,2"|cc$employment=="1,2,4"|cc$employment=="1,3"|cc$employment=="1,4"|cc$employment=="2"|cc$employment=="2,4"|cc$employment=="2,3"|cc$employment=="2,3,4"|cc$employment=="2, 4"|cc$employment=="2,4,5"|cc$employment=="2,5"]<- 1 #full time, parttime 

cc$working[cc$employment=="3"|cc$employment=="3,4"|cc$employment=="3,4,5"|cc$employment=="3,5"|cc$employment=="4"|cc$employment=="4,5"|cc$employment=="5"] <- 0 #not working

cc$working[cc$working>1] <- NA 

table(cc$working,useNA = "always")
# 0     1      <NA> 
# 8800 14402   148   

cc$working <- as.factor(cc$working)

# health
cc$health <- rowMeans(cc[,c("phy_healthy","mentally_healthy")],na.rm = TRUE)
describe(cc$health)

# living alone
# 7：living alone
table(cc$livingstatus,useNA = "always")
cc$living_alone[cc$livingstatus=="7"]<- 1
cc$living_alone[is.na(cc$living_alone)] <- 0
table(cc$living_alone,useNA = "always")
cc$living_alone <- as.factor(cc$living_alone)
# 0        1     <NA> 
# 21228  2122     0

# -----------------------------------------
#----------1. Descriptive Results-------------------
# -----------------------------------------
# Table S1

describe(cc[,c("admiration","calm","compassion","determination","moved","gratitude","hope","love","relief","pleasure","anger","anxiety","boredom","confusion", "disgust","fear","frustration","loneliness","regret", "sadness")])
emotion_cc <- cc[,c("admiration","calm","compassion","determination","moved","gratitude","hope","love","relief","pleasure","anger","anxiety","boredom","confusion", "disgust","fear","frustration","loneliness","regret", "sadness")]
library(Rmisc) # to get mean confidence interval
library(magicfor)  # to save for loop result easier

magic_for(print, silent = TRUE) # call magic_for()

for (i in 1:20) {
  mean_res <- CI(na.omit(emotion_cc[[i]]), ci=0.95)[2]
  sd <- sd(emotion_cc[[i]],na.rm = TRUE)
  ci_low <- CI(na.omit(emotion_cc[[i]]), ci=0.95)[3]
  ci_high <- CI(na.omit(emotion_cc[[i]]), ci=0.95)[1]
  print(format(round(mean_res, 2), nsmall = 2))
  print(format(round(sd, 2), nsmall = 2))
  print(format(round(ci_low, 2), nsmall = 2))
  print(format(round(ci_high, 2), nsmall = 2))
}
emotion_descriptive <- magic_result_as_dataframe() 
colnames(emotion_descriptive) <- c("emotion","mean","sd","ci_low","ci_high")
emotion_descriptive$emotion <- names(emotion_cc)

# write output if wanting to export the results
# emotion_descriptive$mean_emo <- paste(emotion_descriptive$mean,"(",emotion_descriptive$sd,")")
# emotion_descriptive$CI <- paste("[",emotion_descriptive$ci_low,",",emotion_descriptive$ci_high,"]")
# write.csv(emotion_descriptive,"emotion_descriptive.csv")

# prepare .dat file for Level-1 Zero-order Correlations to be run in mplus "zero_order_continuous.inp"
cc2 <- cc[,c("country","age","Extraversion","Agreeableness","Conscientiousness","EmoStability","Openness","self_vulnerable","gender","working","health","living_alone","ladder","education","pos","neg")]
cc2$gender <- as.numeric(cc2$gender)
cc2$living_alone <- as.numeric(cc2$living_alone)
cc2$working <- as.numeric(cc2$working)
cc2[is.na(cc2)] <- -999
library(multiplex)
# write.dat(cc2,"for_mplus.dat")

# -----------------------------------------
#----------2. run models-------------------
# -----------------------------------------
# ICC
VarCorr(lme(pos~1,random = ~1|countryname,data=cc,na.action = na.omit))
# icc for positive emotions: 0.06078276/(0.06078276+0.98009562) = 0.058

VarCorr(lme(neg~1,random = ~1|countryname,data=cc,na.action = na.omit))
# icc for negative emotions: 0.1018806/(0.1018806+1.6278752) = 0.059

# In Carstensen's paper, continuous variables were standardised. But standardising in multi-level is not so straightforward, so we didn't standardise when running the model

# ===============POSITIVE emotions as outcome============

# building up models step by step
cc_m0_pos <- lme(pos~1,random=~1|countryname,data=cc,na.action=na.omit)
cc_m1_pos <- lme(pos~age,random=~1|countryname,data=cc,na.action=na.omit)
cc_m2_pos <- lme(pos~age,random=~1+age|countryname,data=cc,na.action=na.omit)

anova(cc_m0_pos,cc_m1_pos)
anova(cc_m1_pos,cc_m2_pos)

cc_m3_pos <- lme(pos~age+
                   health+gender+education+ladder+working+self_vulnerable, random=~1+age|countryname,data=cc,na.action=na.omit)
cc_m4_pos <- lme(pos~age+
                   health+gender+education+ladder+working+self_vulnerable, random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable)),data=cc,na.action = na.omit)
anova(cc_m3_pos,cc_m4_pos)
# adding random slope is better than no random slope

cc_m5_pos <- lme(pos~age+
                   health+gender+education+ladder+working+self_vulnerable+
                   Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone,# only a subset has this data
                 random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable+Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone)),data=cc,na.action = na.omit)

cc_m6_pos <- lme(pos~age+
                   health+gender+education+ladder+working+self_vulnerable+
                   Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone+
                   stringency+deaths_per_million+log_gdp+Gini_Index, #adding covid-related variables and country-level control variables
                  random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable+Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone)),data=cc,na.action = na.omit)


# getting fixed-effect R squared (results reported in Table S4, R2m)
r.squaredGLMM(cc_m1_pos)
r.squaredGLMM(cc_m2_pos)
r.squaredGLMM(cc_m3_pos)
r.squaredGLMM(cc_m4_pos)
r.squaredGLMM(cc_m5_pos)
r.squaredGLMM(cc_m6_pos)

summary(cc_m1_pos)
summary(cc_m2_pos)
summary(cc_m3_pos)
summary(cc_m4_pos)
summary(cc_m5_pos)
summary(cc_m6_pos)

# Table S4
stargazer(cc_m1_pos,cc_m2_pos,cc_m3_pos,cc_m4_pos,cc_m5_pos,cc_m6_pos,
          type = "text",star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
          notes.align = "c",
          notes.append = F,
          title = 'cc_age_pos',
          out ="cc_age_pos_mar19_2021.txt")


#================== NEGATIVE emotions as outcome================

cc_m0_neg <- lme(neg~1,random=~1|countryname,data=cc,na.action=na.omit)
cc_m1_neg <- lme(neg~age,random=~1|countryname,data=cc,na.action=na.omit)
cc_m2_neg <- lme(neg~age,random=~1+age|countryname,data=cc,na.action=na.omit, control = lmeControl(gradHess = FALSE))
# gradHess = T uses numerical derivative，= F uses analytical derivative

anova(cc_m0_neg,cc_m1_neg)
anova(cc_m1_neg,cc_m2_neg)


cc_m3_neg <- lme(neg~age+
                   health+gender+education+ladder+working+self_vulnerable, random=~1+age|countryname,data=cc,na.action=na.omit)
cc_m4_neg <- lme(neg~age+
                   health+gender+education+ladder+working+self_vulnerable, random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable)),data=cc,na.action = na.omit)
anova(cc_m3_neg,cc_m4_neg)
# model has better fit when adding random slopes for the control variables

cc_m5_neg <- lme(neg~age+
                   health+gender+education+ladder+working+self_vulnerable+
                   Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone,# only a subset has this data
                 random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable+Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone)),data=cc,na.action = na.omit)

cc_m6_neg <- lme(neg~age+
                   health+gender+education+ladder+working+self_vulnerable+
                   Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone+
                   stringency+deaths_per_million+log_gdp+Gini_Index,random= list(countryname = pdDiag(~1+age+health+gender+education+ladder+working+self_vulnerable+Extraversion+Agreeableness+Conscientiousness+EmoStability+Openness+living_alone)),data=cc,na.action = na.omit)

# this is to get the fixed-effect r square (results reported in Table S3, R2m)
r.squaredGLMM(cc_m1_neg)
r.squaredGLMM(cc_m2_neg)
r.squaredGLMM(cc_m3_neg)
r.squaredGLMM(cc_m4_neg)
r.squaredGLMM(cc_m5_neg)
r.squaredGLMM(cc_m6_neg)

summary(cc_m1_neg)
summary(cc_m2_neg)
summary(cc_m3_neg)
summary(cc_m4_neg)
summary(cc_m5_neg)
summary(cc_m6_neg)

stargazer(cc_m1_neg,cc_m2_neg,cc_m3_neg,cc_m4_neg,cc_m5_neg,cc_m6_neg,
          type = "text",star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
          notes.align = "c",
          notes.append = F,
          title = 'cc_age_neg',
          out ="cc_age_neg_mar19_2021.txt")


# -----------------------------------------
#----------3. Visualisation-------------------
# -----------------------------------------

require(lme4)
require(merTools)

VarCorr(lme(age~1,random=~1|countryname,data=cc,na.action=na.omit))
# ICC = 14.65%, there's high between-country variance for age

# ============POS================
# https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
pos_model <- lmer(pos~age+(age|countryname),data=cc,na.action=na.omit,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# this model equals to cc_m2_pos

summary(pos_model)
coef(pos_model)

p_pos_model <- REsim(pos_model)
p_pos_model <- p_pos_model[-c(1:63), ]
coef_p_pos_model<-as.data.frame(coef(pos_model)$countryname)
coef_p_pos_model <- cbind(rownames(coef_p_pos_model), coef_p_pos_model)
rownames(coef_p_pos_model) <- NULL
colnames(coef_p_pos_model)[1] <-"groupID"
coef_p_pos_model$`(Intercept)` <- NULL

p_pos_model2 <- p_pos_model[,c(2,6)]
pos_for_plot <- merge.data.frame(coef_p_pos_model,p_pos_model2,by="groupID")

pos_for_plot <- pos_for_plot[order(pos_for_plot$age), ]
pos_for_plot$groupID <- factor(pos_for_plot$groupID, levels = pos_for_plot$groupID)
pos_plot <- ggplot(pos_for_plot,aes(x=groupID,y=age)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=age-1.96*sd, ymax=age+1.96*sd))+
  coord_flip() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 8, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  ylim(-0.01,0.05)+
  geom_hline(yintercept = 0, 
             color = "blue", size=0.5)+
  ylab("Coefficient of Age and Positive Emotions")
pos_plot



# ============NEG================

neg_model <- lmer(neg~age+(age|countryname),data=cc,na.action=na.omit,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
# this model equals to cc_m2_neg
coef(neg_model)

p_neg_model <- REsim(neg_model)
p_neg_model <- p_neg_model[-c(1:63), ]
coef_p_neg_model<-as.data.frame(coef(neg_model)$countryname)
coef_p_neg_model <- cbind(rownames(coef_p_neg_model), coef_p_neg_model)
rownames(coef_p_neg_model) <- NULL
colnames(coef_p_neg_model)[1] <-"groupID"
coef_p_neg_model$`(Intercept)` <- NULL

p_neg_model2 <- p_neg_model[,c(2,6)]
neg_for_plot <- merge.data.frame(coef_p_neg_model,p_neg_model2,by="groupID")

neg_for_plot <- neg_for_plot[order(neg_for_plot$age), ]
neg_for_plot$groupID <- factor(neg_for_plot$groupID, levels = neg_for_plot$groupID)
neg_plot <- ggplot(neg_for_plot,aes(x=groupID,y=age)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=age-1.96*sd, ymax=age+1.96*sd))+
  coord_flip() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(size = 8, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  ylim(-0.05,0.01)+
  geom_hline(yintercept = 0, 
             color = "blue", size=0.5)+
  ylab("Coefficient of Age and Negative Emotions")
neg_plot




# =====put POS and NEG pic together ========
require(ggpubr)
figure_cc <- ggarrange(neg_plot,pos_plot,
                           ncol = 2, nrow = 1)

ggsave(
  "figure_cc_mar24_2021.pdf",
  plot = figure_cc,
  device = NULL,
  path = NULL,
  scale = 2.8,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300)


























