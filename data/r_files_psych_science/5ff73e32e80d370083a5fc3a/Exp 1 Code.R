#loading packages
library("psych")
library("lmerTest")
library('effects')

#load data from multi-site study
mcdata <- read.csv("Exp1data.csv")

#recode condition as a factor
mcdata$Condition <-as.factor(mcdata$Condition)

#subset data by wave in order to standardize the persistence  outcome within each wave

waveAdata <- mcdata[ which(mcdata$wave== "A"), ]
waveBdata <- mcdata[ which(mcdata$wave== "B"), ]
waveCdata <- mcdata[ which(mcdata$wave== "C"), ]
waveDdata <- mcdata[ which(mcdata$wave== "D"), ]

#calculate standardized selection outcome for each wave 
Pers_zscore_A  <- scale(waveAdata$Avg_Time_combined); Pers_zscore_A
Pers_zscore_B  <- scale(waveBdata$Avg_Time_combined); Pers_zscore_B
Pers_zscore_C  <- scale(waveCdata$Avg_Time_combined); Pers_zscore_C
Pers_zscore_D  <- scale(waveDdata$Avg_Time_combined); Pers_zscore_D

#these values are saved in data set as "avg_time_combined_zscore_withinwave

### average time
model1 <-lmer(avg_time_combined_zscore_withinwave ~ Condition + (1|class), data=mcdata, weights = weights)
summary(model1)
plot(effect(term="Condition", mod=model1), multiline=TRUE)
# the effect of discomfort vs. control B = 0.44, p < .001. 

# "confint"" can't calculate 95% CI when using weights, so we'll use 1.96*SE for the 95% CI. 

#95% CI of difference between discomfort and control: 
1.96*.06295
# so 95% CI is 0.44163 +/- 0.1234 = [.32, .57]
.44163 - .1234
.44163 + .1234

### risk taking
model2 <-lmer(risk_combined_zscore_withinwave ~ Condition + (1|class), data=mcdata, weights = weights)
summary(model2)
plot(effect(term="Condition", mod=model2), multiline=TRUE)
# the effect of discomfort vs. control B = 0.24, p < .001. 

#95% CI of difference between discomfort and control: 
1.96*0.06155
# so 95% CI is 0.44163 +/- 0.1234 = [.12, .36]
0.24255 - 0.120638
0.24255 + 0.120638

# Selection outcome effects split by Wave

#Wave A
modelA <-lmer(avg_time_combined_zscore_withinwave ~ Condition + (1|class), data=waveAdata)
summary(modelA); confint(modelA)
## For Wave A, the effect of discomfort vs. control is B = 0.45, 95% CI: [.23, .67], p < .001.

#Wave B
modelB <-lmer(avg_time_combined_zscore_withinwave ~ Condition + (1|class), data=waveBdata)
summary(modelB); confint(modelB)
## for Wave B, the effect of discomfort vs. control is B = 0.44, 95% CI: [0.22, 0.67], p < .001. 

#Wave C
modelC <-lmer(avg_time_combined_zscore_withinwave ~ Condition + (1|class), data=waveCdata)
summary(modelC); confint(modelC)
## for Wave C, the effect of discomfort vs. control is B = 0.33, 95% CI: [.12, .54], p = .002. 

#Wave D
modelD <-lmer(avg_time_combined_zscore_withinwave ~ Condition + (1|class), data=waveDdata)
summary(modelD); confint(modelD)
## for Wave C, the effect of discomfort vs. control is B = 0.57, 95% CI: [.20, .93], p = .003. 

#risk taking outcome

#Wave A
modelA2 <-lmer(risk_combined_zscore_withinwave ~ Condition + (1|class), data=waveAdata)
summary(modelA2); confint(modelA2)
## For Wave A, the effect of discomfort vs. control is B = 0.28, 95% CI: [.13, .43], p < .001.

#Wave B
modelB2 <-lmer(risk_combined_zscore_withinwave ~ Condition + (1|class), data=waveBdata)
summary(modelB2); confint(modelB2)
## for Wave B, the effect of discomfort vs. control is B = -.13, 95% CI: [-.36, .10], p = .270 

#Wave C
modelC2 <-lmer(risk_combined_zscore_withinwave ~ Condition + (1|class), data=waveCdata)
summary(modelC2); confint(modelC2)
## for Wave C, the effect of discomfort vs. control is B = 0.47, 95% CI: [.23, .71], p < .001. 

#Wave D
modelD2 <-lmer(risk_combined_zscore_withinwave ~ Condition + (1|class), data=waveDdata)
summary(modelD2); confint(modelD2)
## for Wave D, the effect of discomfort vs. control is B = 0.38, 95% CI: [.001, .76], p = .053 

#growth achievement

#Wave D
modelD3 <-lmer(Growthachievement ~ Condition + (1|class), data=waveDdata)
summary(modelD3); confint(modelD3)
## for growth achievement in Wave D, the effect of discomfort vs. control is B = .84, 95% CI: [.13, 1.54], p = .022
