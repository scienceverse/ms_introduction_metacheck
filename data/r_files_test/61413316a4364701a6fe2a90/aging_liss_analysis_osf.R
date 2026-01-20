# --------------------------Sep 13,2021-------------------------------
# -------------------------Author: Rui Sun----------------------------
# -----------Study 2 LISS Additional Analyses (2008-2020)-------------

rm(list = ls())
setwd("")

library(nlme)
library(data.table)
library(MuMIn) #for rsquare 
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)
library(multcomp) #for glht

data <- read.csv("Study2_liss.csv")

# ----getting demographics
y2008 <- data[which(data$year==2008),] #1629 ppts
table(y2008$gender,useNA = "always") #816 male, 813 female
table(y2008$age,useNA = "always") 
describe(y2008$age)
# initial age: 16-85, mean = 50.79 (sd = 13.19)

# get initial age
ini_age <- y2008[,c("id","age")]
colnames(ini_age)[2] <- "ini_age"
ini_age$scale_ini_age <- scale(ini_age$ini_age)

### add the initial age to the dataset
data <- merge.data.frame(data,ini_age,by="id",all.x = TRUE)

data$covid <- as.factor(data$covid)
data$gender <- as.factor(data$gender)

# =================================
# ---------------------------------
# -----descriptive results---------
# ---------------------------------
# =================================

# ---------------------------------
# ----1 emotion by year -----------
# ---------------------------------
# -------Table S9------------------
# Participants’ Yearly Emotional Experiences Between 2008 and 2020 (N = 1629)

descriptive_panas_mean <- aggregate(data[,c("Pos","Neg")], by=list(year=data$year), FUN=mean,na.rm=TRUE)
descriptive_panas_sd <- aggregate(data[,c("Pos","Neg")], by=list(year=data$year), FUN=sd,na.rm=TRUE)
descriptive_panas <- cbind.data.frame(descriptive_panas_mean,descriptive_panas_sd)

descriptive_panas_mean_covid <- aggregate(data[,c("Pos","Neg")], by=list(covid=data$covid), FUN=mean,na.rm=TRUE)
descriptive_panas_sd_covid <- aggregate(data[,c("Pos","Neg")], by=list(covid=data$covid), FUN=sd,na.rm=TRUE)
descriptive_panas_covid <- cbind.data.frame(descriptive_panas_mean_covid,descriptive_panas_sd_covid)

# whether pos/neg differ because of covid
summary(lme(Neg~covid,random=~1+covid|id,data=data,na.action = na.omit)) #sig 
summary(lme(Pos~covid,random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim'))) #non-sig

# --------------------------------------------------------------------------
# ----2. Relationship between age and emotions in different years ----------
# --------------------------------------------------------------------------

# ---------Table S10---------
# whether emotional experience change because of covid

data$year <- as.factor(data$year)

negemo0 <- lme(Neg~ year, #Level 1: Neg, year
               random=~1|id,data=data,na.action = na.omit)  #random intercept=id
summary(negemo0)

wald_neg_emo <- glht(model = negemo0, 
                     linfct = c("year2009 - year2020 = 0",
                                "year2011 - year2020 = 0",
                                "year2013 - year2020 = 0",
                                "year2014 - year2020 = 0",
                                "year2017 - year2020 = 0",
                                "year2019 - year2020 = 0"))

summary(wald_neg_emo) #unadjusted p value
summary(wald_neg_emo, test = adjusted("BH")) #adjusted p value for multiple comparison

# Linear Hypotheses:
# Estimate Std. Error z value Pr(>|z|)    
# year2009 - year2020 == 0 -0.11731    0.02452  -4.783 2.68e-06 ***
#   year2011 - year2020 == 0 -0.11713    0.02452  -4.776 2.68e-06 ***
#   year2013 - year2020 == 0 -0.16888    0.02452  -6.886 3.44e-11 ***
#   year2014 - year2020 == 0 -0.13702    0.02452  -5.587 6.94e-08 ***
#   year2017 - year2020 == 0 -0.09398    0.02452  -3.832 0.000127 ***
#   year2019 - year2020 == 0 -0.11136    0.02452  -4.541 6.73e-06 ***
# MEANING: Year 2020, the negative emotions are higher than any year before covid

posemo0 <- lme(Pos~ year, #Level 1: pos, year
               random=~1|id,data=data,na.action = na.omit)  #random intercept=id
summary(posemo0)

wald_pos_emo <- glht(model = posemo0, 
                     linfct = c("year2009 - year2020 = 0",
                                "year2011 - year2020 = 0",
                                "year2013 - year2020 = 0",
                                "year2014 - year2020 = 0",
                                "year2017 - year2020 = 0",
                                "year2019 - year2020 = 0"))
summary(wald_pos_emo)
summary(wald_pos_emo, test = adjusted("BH")) 

# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)  
# year2009 - year2020 == 0  0.054205   0.022549   2.404   0.0585 .
# year2011 - year2020 == 0 -0.052670   0.022549  -2.336   0.0585 .
# year2013 - year2020 == 0 -0.046347   0.022549  -2.055   0.0797 .
# year2014 - year2020 == 0  0.024125   0.022549   1.070   0.3416  
# year2017 - year2020 == 0 -0.001596   0.022549  -0.071   0.9436  
# year2019 - year2020 == 0 -0.035605   0.022549  -1.579   0.1715  

# MEANING: Year 2020, the positive emotions are lower than 2008, 2009, 2011 and 2013 (2009/2011/2013 marginally sig after BH correction); positive emotions in 2020 didn't differ from 2014, 2017 and 2019

# ---------Table S11---------
# Age-Emotion Coefficients of Linear Regression Analysis by Year (N= 1629)

model_neg_by_year <- with(data,
                  by(data, year,
                     function(x) lm(Neg~scale(age), data = x,na.action = na.omit)))
df_neg_by_year <- as.data.frame(sapply(model_neg_by_year, coef)) #Table S11
df_neg_t_by_year <- as.data.frame(t(df_neg_by_year))

model_neg_by_covid <- with(data,
                           by(data, covid,
                              function(x) lm(Neg~scale(age), data = x,na.action = na.omit)))
df_neg_by_covid <- as.data.frame(sapply(model_neg_by_covid, coef))
df_neg_t_by_covid <- as.data.frame(t(df_neg_by_covid))

# ---------------------------------------------------------
# for each year get correlation between variables
# for covid/non-covid, get correlation between variables
# exploratory, not reported in the manuscript
# ---------------------------------------------------------
# data$gender <- as.numeric(data$gender)
# require(plyr)
# func <- function(xx) #get estimate of correlation
# {
#   # return(data.frame(COR_pos = cor(xx$scale_ini_age, xx$Pos)))
#   cor_data <- list()
#   cor_data$COR_age_pos <-  cor(xx$scale_ini_age, xx$Pos)
#   cor_data$COR_age_neg <-  cor(xx$scale_ini_age, xx$Neg)
#   cor_data$COR_age_health <-  cor(xx$scale_ini_age, xx$sub_health)
#   cor_data$COR_age_gender <-  cor(xx$scale_ini_age, xx$gender)
#   cor_data$COR_age_extra <-  cor(xx$scale_ini_age, xx$Extraversion)
#   cor_data$COR_age_agree <-  cor(xx$scale_ini_age, xx$Agreeable)
#   cor_data$COR_age_cons <-  cor(xx$scale_ini_age, xx$Conscientiousness)
#   cor_data$COR_age_emosta <-  cor(xx$scale_ini_age, xx$EmoStability)
#   cor_data$COR_age_openness <-  cor(xx$scale_ini_age, xx$Openness)
#   
#   cor_data$COR_pos_health <-  cor(xx$Pos, xx$sub_health)
#   cor_data$COR_pos_gender <-  cor(xx$Pos, xx$gender)
#   cor_data$COR_pos_extra <-  cor(xx$Pos, xx$Extraversion)
#   cor_data$COR_pos_agree <-  cor(xx$Pos, xx$Agreeable)
#   cor_data$COR_pos_cons <-  cor(xx$Pos, xx$Conscientiousness)
#   cor_data$COR_pos_emosta <-  cor(xx$Pos, xx$EmoStability)
#   cor_data$COR_pos_openness <-  cor(xx$Pos, xx$Openness)
#   
#   cor_data$COR_neg_health <-  cor(xx$Neg, xx$sub_health)
#   cor_data$COR_neg_gender <-  cor(xx$Neg, xx$gender)
#   cor_data$COR_neg_extra <-  cor(xx$Neg, xx$Extraversion)
#   cor_data$COR_neg_agree <-  cor(xx$Neg, xx$Agreeable)
#   cor_data$COR_neg_cons <-  cor(xx$Neg, xx$Conscientiousness)
#   cor_data$COR_neg_emosta <-  cor(xx$Neg, xx$EmoStability)
#   cor_data$COR_neg_openness <-  cor(xx$Neg, xx$Openness)
#   
#   return(ldply (cor_data, data.frame))
# }
# 
# 
# correlations <- ddply(data, .(year), func)
# correlations$v1 <- substring(correlations[,2], 5,7) 
# correlations$v2 <- substring(correlations[,2], 9,) 
# correlations1 <- correlations[,c("year","v1","v2","X..i..")]
# names(correlations1)[4] <- "value"
# 
# 
# correlations_covid <- ddply(data, .(covid), func)
# correlations_covid$v1 <- substring(correlations_covid[,2], 5,7) 
# correlations_covid$v2 <- substring(correlations_covid[,2], 9,) 
# correlations_covid1 <- correlations_covid[,c("covid","v1","v2","X..i..")]
# names(correlations_covid1)[4] <- "value"
# 
# summary(lme(EmoStability~Neg,random=~1|id,data=data[which(data$covid==0),],na.action = na.omit)) #sig 
# 
# # getting p values
# func_p <- function(xx) #get estimate of correlation
# {
#   # return(data.frame(COR_pos = cor(xx$scale_ini_age, xx$Pos)))
#   cor_data <- list()
#   cor_data$COR_age_pos <-  cor.test(xx$scale_ini_age, xx$Pos)$p.value
#   cor_data$COR_age_neg <-  cor.test(xx$scale_ini_age, xx$Neg)$p.value
#   cor_data$COR_age_health <-  cor.test(xx$scale_ini_age, xx$sub_health)$p.value
#   cor_data$COR_age_gender <-  cor.test(xx$scale_ini_age, xx$gender)$p.value
#   cor_data$COR_age_extra <-  cor.test(xx$scale_ini_age, xx$Extraversion)$p.value
#   cor_data$COR_age_agree <-  cor.test(xx$scale_ini_age, xx$Agreeable)$p.value
#   cor_data$COR_age_cons <-  cor.test(xx$scale_ini_age, xx$Conscientiousness)$p.value
#   cor_data$COR_age_emosta <-  cor.test(xx$scale_ini_age, xx$EmoStability)$p.value
#   cor_data$COR_age_openness <-  cor.test(xx$scale_ini_age, xx$Openness)$p.value
#   
#   cor_data$COR_pos_health <-  cor.test(xx$Pos, xx$sub_health)$p.value
#   cor_data$COR_pos_gender <-  cor.test(xx$Pos, xx$gender)$p.value
#   cor_data$COR_pos_extra <-  cor.test(xx$Pos, xx$Extraversion)$p.value
#   cor_data$COR_pos_agree <-  cor.test(xx$Pos, xx$Agreeable)$p.value
#   cor_data$COR_pos_cons <-  cor.test(xx$Pos, xx$Conscientiousness)$p.value
#   cor_data$COR_pos_emosta <-  cor.test(xx$Pos, xx$EmoStability)$p.value
#   cor_data$COR_pos_openness <-  cor.test(xx$Pos, xx$Openness)$p.value
#   
#   cor_data$COR_neg_health <-  cor.test(xx$Neg, xx$sub_health)$p.value
#   cor_data$COR_neg_gender <-  cor.test(xx$Neg, xx$gender)$p.value
#   cor_data$COR_neg_extra <-  cor.test(xx$Neg, xx$Extraversion)$p.value
#   cor_data$COR_neg_agree <-  cor.test(xx$Neg, xx$Agreeable)$p.value
#   cor_data$COR_neg_cons <-  cor.test(xx$Neg, xx$Conscientiousness)$p.value
#   cor_data$COR_neg_emosta <-  cor.test(xx$Neg, xx$EmoStability)$p.value
#   cor_data$COR_neg_openness <-  cor.test(xx$Neg, xx$Openness)$p.value
#   
#   return(ldply (cor_data, data.frame))
# }
# correlations_covid_p <- ddply(data, .(covid), func_p)
# correlations_covid_p$v1 <- substring(correlations_covid_p[,2], 5,7) 
# correlations_covid_p$v2 <- substring(correlations_covid_p[,2], 9,) 
# correlations_covid_p1 <- correlations_covid_p[,c("covid","v1","v2","X..i..")]
# names(correlations_covid_p1)[4] <- "p_value"
# 
# 
# 
# func_age_emo <- function(xx) #get estimate of correlation
# {
#   # return(data.frame(COR_pos = cor(xx$scale_ini_age, xx$Pos)))
#   cor_data <- list()
#   cor_data$COR_age_pos_est <-  cor.test(xx$scale_ini_age, xx$Pos)$estimate
#   cor_data$COR_age_neg_est <-  cor.test(xx$scale_ini_age, xx$Neg)$estimate
#   
#   cor_data$COR_age_pos_ci <-  cor.test(xx$scale_ini_age, xx$Pos)$conf.int[1:2] 
#   cor_data$COR_age_neg_ci <-  cor.test(xx$scale_ini_age, xx$Neg)$conf.int[1:2] 
#   
#   cor_data$COR_age_pos_p <-  cor.test(xx$scale_ini_age, xx$Pos)$p.value
#   cor_data$COR_age_neg_p <-  cor.test(xx$scale_ini_age, xx$Neg)$p.value 
#  
#   return(ldply (cor_data, data.frame))
# }
# correlations_age_emo_year <- ddply(data, .(year), func_age_emo)
# correlations_age_emo_year$v1 <- substring(correlations_age_emo_year[,2], 5,7) 
# correlations_age_emo_year$v2 <- substring(correlations_age_emo_year[,2], 9,11) 
# correlations_age_emo_year$v3 <- substring(correlations_age_emo_year[,2], 13,) 
# age_emo_year <- correlations_age_emo_year[,c(1,4:6,3)]
# names(age_emo_year)[5] <- "value"
# age_pos_year <- age_emo_year[which(age_emo_year$v2=="pos"),]
# age_neg_year <- age_emo_year[which(age_emo_year$v2=="neg"),]





# ========================================================================
# ------------------------------------------------------------------------
# testing hypothesis: data analysis without adding any control variables
# ------------------------------------------------------------------------
# ========================================================================

# ====age * covid on neg/pos emotions===
# purpose: covid vs non-covid difference

# -----------NegativeEmotionResults
data$year <- as.numeric(data$year)
data$year_recode <- data$year-2007

neg0 <- lme(Neg~ scale_ini_age+year_recode, #Level 1: Neg, year; Level 2: scale_ini_age;
            random=~1|id,data=data,na.action = na.omit)  #random intercept=id
summary(neg0)
r.squaredGLMM(neg0)

# testing hypothesis
neg1 <- lme(Neg~ scale_ini_age*covid+year_recode,#Level 1: Neg, year,covid condition; Level 2: scale_ini_age;
            random=~1+covid|id,data=data,na.action = na.omit)  #random intercept=id;random slope=covid
summary(neg1)
r.squaredGLMM(neg1)

# neg2 <- lme(Neg~ scale_ini_age*covid+year_recode,#Level 1: Neg, year,covid condition; Level 2: scale_ini_age;
#             random= list(id = pdDiag(~1+scale_ini_age*covid)),data=data,na.action = na.omit) #random intercept=id;random slope=covid*scale_ini_age
# summary(neg2)
# anova(neg1,neg2) #no difference; so for the rest of analyses, we only used covid as random slope

neg2_c1 <- lme(Neg~ scale_ini_age*covid+year_recode  #Level 1: Neg, year,covid condition; Level 2: scale_ini_age;
               +gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness #Level 2 control variables 
               ,random=~1+covid|id,data=data,na.action = na.omit) #random intercept=id;random slope=covid
summary(neg2_c1)
r.squaredGLMM(neg2_c1)

neg2_c2 <- lme(Neg~ scale_ini_age*covid+year_recode  #Level 1: Neg, year,covid condition; Level 2: scale_ini_age;
               +gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness #Level 2 control variables 
               +sub_health #level 1 control variable
               ,random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) #random intercept=id;random slope=covid
summary(neg2_c2)
r.squaredGLMM(neg2_c2)

neg2_c3 <- lme(Neg~ (scale_ini_age+gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness+sub_health) *covid+year_recode,random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) #random intercept=id;random slope=covid
summary(neg2_c3)
r.squaredGLMM(neg2_c2)

# Table S12
stargazer(neg0,neg1,neg2_c1,neg2_c2,neg2_c3,
          type = "text",star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
          notes.align = "c",
          notes.append = F,
          title = 'LISS_neg',
          out ="LISS_neg.txt")

# -----------PositiveEmotionResults
pos0 <- lme(Pos~ scale_ini_age+year_recode, #Level 1: pos, year; Level 2: scale_ini_age;
            random=~1|id,data=data,na.action = na.omit)  #random intercept=id
summary(pos0)
r.squaredGLMM(pos0)

# testing hypothesis
pos1 <- lme(Pos~ scale_ini_age*covid+year_recode,#Level 1: pos, year,covid condition; Level 2: scale_ini_age;
            random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(pos1)
r.squaredGLMM(pos1)

pos2_c1 <- lme(Pos~ scale_ini_age*covid+year_recode  #Level 1: pos, year,covid condition; Level 2: scale_ini_age;
               +gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness #Level 2 control variables 
               ,random=~1+covid|id,data=data,control = lmeControl(opt = 'optim'))#random intercept=id;random slope=covid
summary(pos2_c1)
r.squaredGLMM(pos2_c1)

pos2_c2 <- lme(Pos~ scale_ini_age*covid+year_recode  #Level 1: pos, year,covid condition; Level 2: scale_ini_age;
               +gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness #Level 2 control variables 
               +sub_health #level 1 control variable
               ,random=~1+covid|id,data=data,na.action = na.omit) #random intercept=id;random slope=covid
summary(pos2_c2)
r.squaredGLMM(pos2_c2)

pos2_c3 <- lme(Pos~ (scale_ini_age+gender+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness+sub_health) *covid+year_recode,random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) #random intercept=id;random slope=covid
summary(pos2_c3)
r.squaredGLMM(pos2_c3)

stargazer(pos0,pos1,pos2_c1,pos2_c2,pos2_c3,
          type = "text",star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
          notes.align = "c",
          notes.append = F,
          title = 'LISS_pos',
          out ="LISS_pos.txt")

# -----------------------
# ------PLOTS------------
# -----------------------

# --------Fig S2. plot standard coefficient of age on emotion by year, line plot ------
data$year <- as.factor(data$year)

# NEG
year_neg_plot <- data %>%
  ggplot(aes(x=age, 
             y=Neg, 
             color=year))+
  geom_smooth(method="lm",se = TRUE,fill="grey80")+
  ylab("Negative Emotions")+
  # xlab("Age")+
  xlim(16,100)+
  theme_bw() + 
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 16,hjust=0.5),
    axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 5, b = 0, l = 0), size=14),
    axis.title.x =element_blank(),
    legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.x = element_blank() ,
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5,2,0.1,0.1), "lines")
  )
year_neg_plot


# POS
year_pos_plot <- data %>%
  ggplot(aes(x=age, 
             y=Pos, 
             color=year))+
  geom_smooth(method="lm",se = TRUE,fill="grey80")+
  ylab("Positive Emotions")+
  # xlab("Age")+
  xlim(16,100)+
  theme_bw() + 
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 16,hjust=0.5),
    axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 5, b = 0, l = 0),size=14),
    axis.title.x =element_blank(),
    legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.x = element_blank() ,
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5,1,0.1,0.1), "lines")
  )
year_pos_plot

require(ggpubr)
line_plot <- ggarrange( year_neg_plot, year_pos_plot,ncol =2, nrow = 1,common.legend = TRUE,legend = "right")
line_plot
line_plot1 <- annotate_figure(line_plot,
                              bottom = text_grob("Age(Years)", 
                                                 hjust = 1, x = 0.55, size = 16))
line_plot1


# ------------------Fig S3 age * covid interaction ----------------
# for plot reason, it's easier to create ppts' mean age as x-axis
data$m_age <- data$ini_age+6
data$c_age <- data$age-data$m_age

# neg_plot <- lme(Neg~ m_age*covid+year_recode,#Level 1: Neg, year,covid condition; Level 2: m_age;
#             random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) #random intercept=id;random slope=covid

neg1 <- lme(Neg~ scale_ini_age*covid+year_recode,#Level 1: Neg, year,covid condition; Level 2: scale_ini_age;
            random=~1+covid|id,data=data,na.action = na.omit)  #random intercept=id;random slope=covid


ef_neg <- effect(term="scale_ini_age*covid", mod=neg1)
ef_neg <-as.data.frame(ef_neg) #convert the effects list to a data frame

# year 2008, mean age = 50.79, SD = 13.19
# year 2020 mean age = 62.79, SD = 13.19
# year 2020, age 30, in 2008, age 18, z = (18-50.79)/13.19 = -2.485974
# year 2020, age 50, in 2008, age 38, z = (38-50.79)/13.19 = -0.969674
# year 2020, age 70, in 2008, age 58, z = (58-50.79)/13.19 =  0.5466262
# year 2020, age 90, in 2008, age 78, z = (78-50.79)/13.19 =  2.062926

interaction_plot_neg <- ggplot(ef_neg,aes(x=scale_ini_age,y=fit,color=covid,group=covid))+
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=covid),alpha=0.3)+
  ylab("Negative Emotions")+
  # xlab("Age")+
  theme_bw() + 
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 14,hjust=0.5),
    axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0), size=14),
    legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.x = element_blank() ,
    axis.title.x =element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    plot.margin = unit(c(0.5,2,0.1,0.1), "lines")
  )+
  scale_colour_manual(values=c('0'="red",'1'="blue"),labels=c("Before","During"))+
  scale_fill_manual(values=c('0'="#FF9999",'1'="#99CCFF"),labels=c("Before","During"))+
  scale_x_continuous(breaks=c(-2.485974,-0.969674,0.5466262,2.062926),
                     labels=c("30", "50", "70","90"))
interaction_plot_neg

# pos_plot <- lme(Pos~ m_age*covid+year_recode,#Level 1: Pos, year,covid condition; Level 2: m_age;
# random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) #random intercept=id;random slope=covid

pos1 <- lme(Pos~ scale_ini_age*covid+year_recode,#Level 1: pos, year,covid condition; Level 2: scale_ini_age;
            random=~1+covid|id,data=data,na.action = na.omit,control = lmeControl(opt = 'optim')) 

require(effects)
ef_pos <- effect(term="scale_ini_age*covid", mod=pos1)
ef_pos <-as.data.frame(ef_pos) #convert the effects list to a data frame
interaction_plot_pos <- ggplot(ef_pos,aes(x=scale_ini_age,y=fit,color=covid,group=covid))+
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=covid),alpha=0.3)+
  ylab("Positive Emotions")+
  # xlab("Age")+
  theme_bw() + 
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 14,hjust=0.5),
    axis.title.y = element_text(colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0), size=14),
    legend.justification = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2),
    panel.grid.major.x = element_blank() ,
    panel.grid.minor = element_blank(),
    axis.title.x =element_blank(),
    legend.title = element_blank(),
    plot.margin = unit(c(0.5,1,0.1,0.1), "lines")
  )+
  scale_colour_manual(values=c('0'="red",'1'="blue"),labels=c("Before","During"))+
  scale_fill_manual(values=c('0'="#FF9999",'1'="#99CCFF"),labels=c("Before","During"))+
  scale_x_continuous(breaks=c(-2.485974,-0.969674,0.5466262,2.062926),
                     labels=c("30", "50", "70","90"))
interaction_plot_pos

covid_interaction_plot <- ggarrange(interaction_plot_neg,interaction_plot_pos,  ncol = 2, nrow = 1, common.legend = TRUE,legend = "right")
covid_interaction_plot1 <- annotate_figure(covid_interaction_plot,
                                           bottom = text_grob("Age(Years)", 
                                                              hjust = 1, x = 0.55, size = 16))
covid_interaction_plot1