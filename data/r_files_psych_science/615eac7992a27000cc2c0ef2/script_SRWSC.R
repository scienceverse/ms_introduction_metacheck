
# This script reproduces the findings reported in the manuscript "Spatial representations without spatial computations"

require('lme4')
require('lmerTest')
require('ggpubr')
require('effects')
require('emmeans')
require('ggplot2')
require('plyr')
require('Rmisc')
require('sjstats')
require('effsize')
require('reshape2')
require('jtools')
require('MuMIn')
require('ggeffects')
require('car')
require('olsrr')
require('BiDimRegression')
require('rstatix')

options(scipen=999, digits=3)

#########################################################################################################

# how to compute vector cosines

#########################################################################################################

require('proxy')

# load the dataset containing the vectors
df <- read.csv2("Google Drive/vectors.csv",header=T,sep=";",dec=",",fill = T)

# compute cosine matrix
df_cosine <- simil(df, method="cosine", by_rows = F, diag = T)

# OR
# compute distance matrix
df_cosine <- dist(df, method="cosine", by_rows = F, diag = T)

#########################################################################################################

# Experiment 1

#########################################################################################################

# load the dataset for Experiment 1 and Figure 1
data_EXP1 <- read.csv2("Google Drive/1_PhD/1_progetti/10_city_semantics/DATASETS/data_EXP1.csv",header=T,sep=";",dec=",",fill = T)

# compute linguistic latitude and longitude
data_EXP1$abs_lat <- data_EXP1$South - data_EXP1$North
data_EXP1$abs_long <- data_EXP1$West - data_EXP1$East

# note that in this case the order of the operators is reversed as linguistic values are expressed as distances 
# (i.e. 1 - cosine, the lower the value the linguistically closer the words) rather than cosines

# for Fig. 1A and 1B
maps <- read.csv2("Google Drive/1_PhD/1_progetti/10_city_semantics/DATASETS/real_ling_map.csv",header=T,sep=";",dec=",",fill = T)

# real geographic map
ggscatter(maps, x = "Longitude", y = "Latitude", 
          color = "black", fill = "lightgray", shape = 21, size = 3,
          add.params = list(color = "blue"), 
          xlab = "Longitude", ylab = "Latitude",
          label = "City", repel = TRUE ) + theme_bw(base_size = 15)

# linguistic map
ggscatter(maps, x = "abs_long", y = "abs_lat", 
          color = "black", fill = "lightgray", shape = 21, size = 3,
          add.params = list(color = "blue"), 
          xlab = "Longitude", ylab = "Latitude",
          label = "City", repel = TRUE ) + theme_bw(base_size = 15)

# Fig 1C and 1D
ggplot(data_EXP1,
       aes(x = Latitude,y = abs_lat, fill = Language)) +
  geom_point(aes(x = Latitude,y = abs_lat, fill = Language, color = Language)) +
  geom_smooth(method = 'lm',
              aes(x = Latitude,y = abs_lat, fill = Language, color = Language), 
              alpha = 0) +
  theme_bw(base_size = 15) + scale_color_brewer(palette="Spectral")

ggplot(data_EXP1,
       aes(x = Longitude,y = abs_long, fill = Language)) +
  geom_point(aes(x = Longitude,y = abs_long, fill = Language, color = Language)) +
  geom_smooth(method = 'lm',
              aes(x = Longitude,y = abs_long, fill = Language, color = Language), 
              alpha = 0) +
  theme_bw(base_size = 15) + scale_color_brewer(palette="Spectral")

# analyses on latitude
fit.abs <- lmer(abs_lat ~ Latitude + (1|Language) + (1|City), data = data_EXP1)

fit.abs_min <- lmer(abs_lat_MIN ~ Latitude + (1|Language) + (1|City), data = data_EXP1)

summ(fit.abs)
summ(fit.abs_min)

# analyses on longitude
fit.abs <- lmer(abs_long ~ Longitude + (1|Language) + (1|City), data = data_EXP1)

fit.abs_min <- lmer(abs_long_MIN ~ Longitude + (1|Language) + (1|City), data = data_EXP1)

summ(fit.abs)
summ(fit.abs_min)

# across both latitude and longitude and for both uppercase and lowercase cardinal points
# geographic coordinates predict linguistic coordinates

#########################################################################################################

# Experiment 2

#########################################################################################################

# stimuli selected
geo <- read.csv2("Google Drive/1_PhD/1_progetti/10_city_semantics/DATASETS/italy_map.csv",header=T,sep=";",dec=",",fill = T)

ggscatter(geo, x = "Longitude", y = "Latitude", 
          color = "black", fill = "lightgray", shape = 21, size = 3,
          add.params = list(color = "blue"), 
          xlab = "Longitude", ylab = "Latitude",
          label = "Comune", repel = TRUE ) + theme_bw()

# load dataset
data_EXP2 <- read.csv2("Google Drive/1_PhD/1_progetti/10_city_semantics/DATASETS/data_EXP2.csv",header=T,sep=";",dec=",",fill = T)

# compute participants accuracy
data2.group <- ddply(data_EXP2,
                     c('ID'),
                     summarize,
                     ACC = mean(accuracy,na.rm=T))

data2.group

# no participants answered at chance level

# 4730 starting trials (110 trials * 43 participants)

# use milliseconds for RTs
data_EXP2$RT <- data_EXP2$RT* 1000

# remove trials responded too fast
data_EXP2 <- subset(data_EXP2, RT > 300)

# 4629 final trials (2% of trials removed)

# compute correlations between predictors
data.group <- ddply(data_EXP2,
                    c('trial_fin'),
                    summarize,
                    LLat = mean(LLat ,na.rm=T),
                    LLatLow = mean(LLatLow ,na.rm=T),
                    cosine_simil = mean(cosine_simil ,na.rm=T),
                    GLat = mean(GLat ,na.rm=T),
                    der_cor_1 = mean(der_cor_1 ,na.rm=T),
                    der_cor_2 = mean(der_cor_2 ,na.rm=T))

mydata <- data.group %>%
  select(GLat, LLat, LLatLow, der_cor_1, der_cor_2, cosine_simil)
cor.mat <- mydata %>% cor_mat()
cor.mat

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

# analyses on accuracy

# Geographic latitude
acc.geo2 <- glmer(accuracy ~ GLat + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.geo2)

# Linguistic latitude
acc.ling2 <- glmer(accuracy ~ LLat + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.ling2)

# Linguistic latitude (lowercase)
acc.ling2low <- glmer(accuracy ~ LLatLow + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.ling2low)

# Derived coordinates
acc.der2.1 <- glmer(accuracy ~ der_cor_1 + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
acc.der2.2 <- glmer(accuracy ~ der_cor_2 + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.der2.1)
summ(acc.der2.2)

# Word similarity
acc.WS2 <- glmer(accuracy ~ cosine_simil + (1|ID) + (1|trial_fin), data = data_EXP2, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.WS2)

# compare models based on AIC
AIC(acc.geo2,acc.ling2,acc.ling2low,acc.der2.1,acc.der2.2,acc.WS2)

# plot best model
plot(predictorEffects(acc.geo2,~ GLat, residuals=F),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response", lim = c(.6,1))),
     partial.residuals = list(smooth = F,
                              col = "grey",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

# The best model is the one including geographic latitude

# analyses on RTs

# Geographic latitude
rts.geo2 <- lmer(log(RT) ~ GLat + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
summ(rts.geo2)

# Linguistic latitude
rts.ling2 <- lmer(log(RT) ~ LLat + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
summ(rts.ling2)

# Linguistic latitude (lowercase)
rts.ling2low <- lmer(log(RT) ~ LLatLow + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
summ(rts.ling2low)

# Derived coordinates
rts.der2.1 <- lmer(log(RT) ~ der_cor_1 + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
rts.der2.2 <- lmer(log(RT) ~ der_cor_2 + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
summ(rts.der2.1)
summ(rts.der2.2)

# Word similarity
rts.WS2 <- lmer(log(RT) ~ cosine_simil + (1|ID) + (1|trial_fin), data = data_EXP2[data_EXP2$accuracy == '1',])
summ(rts.WS2)

# compare models based on AIC
AIC(rts.geo2,rts.ling2,rts.ling2low,rts.der2.1,rts.der2.2,rts.WS2)

# The best models are the ones including linguistic latitude

# plot best models

# uppercase cardinal points
plot(predictorEffects(rts.ling2,~ LLat, residuals=T),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response")),
     partial.residuals = list(smooth = F,
                              col = "black",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

# lowercase cardinal points
plot(predictorEffects(rts.ling2low,~ LLatLow, residuals=T),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response")),
     partial.residuals = list(smooth = F,
                              col = "black",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

#########################################################################################################

# Experiment 3

#########################################################################################################

# stimuli selected
geo2 <- geo[geo$Comune != 'Bologna' & geo$Comune != 'Pisa',]

ggscatter(geo2, x = "Longitude", y = "Latitude", 
          color = "black", fill = "lightgray", shape = 21, size = 3,
          add.params = list(color = "blue"), 
          xlab = "Longitude", ylab = "Latitude",
          label = "Comune", repel = TRUE ) + theme_bw()

# load dataset
data_EXP3 <- read.csv2("Google Drive/1_PhD/1_progetti/10_city_semantics/DATASETS/data_EXP3.csv",header=T,sep=";",dec=",",fill = T)

# compute participants accuracy
data3.group <- ddply(data_EXP3,
                     c('ID'),
                     summarize,
                     ACC = mean(accuracy,na.rm=T))

data3.group

# no participants answered at chance level

# 5904 starting trials (144 trials * 41 participants)

# use milliseconds for RTs
data_EXP3$RT <- data_EXP3$RT* 1000

# remove trials responded too fast
data_EXP3 <- subset(data_EXP3, RT > 300)

# 5839 final trials (1% of trials removed)

# compute correlations between predictors
data.group <- ddply(data_EXP3,
                    c('trial_fin'),
                    summarize,
                    GLat = mean(GLat,na.rm=T),
                    GLon = mean(GLon,na.rm=T),
                    LLat = mean(LLat,na.rm=T),
                    LLon = mean(LLon,na.rm=T),
                    LLatLow = mean(LLatLow,na.rm=T),
                    LLonLow = mean(LLonLow,na.rm=T),
                    der_cor_1 = mean(der_cor_1,na.rm=T),
                    der_cor_2 = mean(der_cor_2,na.rm=T),
                    cosine_simil = mean(cosine_simil,na.rm=T))

mydata <- data.group %>%
  select(GLat, GLon, LLat, LLon, LLatLow, LLonLow, der_cor_1, der_cor_2, cosine_simil)
cor.mat <- mydata %>% cor_mat()
cor.mat

cor.mat %>% cor_get_pval()

cor.mat %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

# analyses on accuracy

# Geographic coordinates
acc.geo3 <- glmer(accuracy ~ GLat + GLon + (1|ID) + (1|trial_fin), data = data_EXP3, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.geo3)

# Linguistic coordinates
acc.ling3 <- glmer(accuracy ~ LLat + LLon + (1|ID) + (1|trial_fin), data = data_EXP3, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.ling3)

# Linguistic coordinates (lowercase)
acc.ling3low <- glmer(accuracy ~ LLatLow + LLonLow + (1|ID) + (1|trial_fin), data = data_EXP3, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.ling3low)

# Derived coordinates
acc.der3 <- glmer(accuracy ~ der_cor_1 + der_cor_2 + (1|ID) + (1|trial_fin), data = data_EXP3, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.der3)

# Word Similarity
acc.WS3 <- glmer(accuracy ~ cosine_simil + (1|ID) + (1|trial_fin), data = data_EXP3, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summ(acc.WS3)

# compare models based on AIC
AIC(acc.geo3,acc.ling3,acc.ling3low,acc.der3,acc.WS3)

# The best models are the one including geographic coordinates and the one including WordSim

# plot best models

# geographic
plot(predictorEffects(acc.geo3,~ GLat + GLon, residuals=F),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response", lim = c(.9,1))),
     partial.residuals = list(smooth = F,
                              col = "grey",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

# word similarity
plot(predictorEffects(acc.WS3,~ cosine_simil, residuals=F),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response", lim = c(.9,1))),
     partial.residuals = list(smooth = F,
                              col = "grey",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

# analyses on RTs

# Geographic coordinates
rts.geo3 <- lmer(log(RT) ~ GLat + GLon + (1|ID) + (1|trial_fin), data = data_EXP3[data_EXP3$accuracy == '1',])
summ(rts.geo3)

# Linguistic coordinates
rts.ling3 <- lmer(log(RT) ~ LLat + LLon + (1|ID) + (1|trial_fin), data = data_EXP3[data_EXP3$accuracy == '1',])
summ(rts.ling3)

# Linguistic coordinates (lowercase)
rts.ling3low <- lmer(log(RT) ~ LLatLow + LLonLow + (1|ID) + (1|trial_fin), data = data_EXP3[data_EXP3$accuracy == '1',])
summ(rts.ling3low)

# Derived coordinates
rts.der3 <- lmer(log(RT) ~ der_cor_1 + der_cor_2 + (1|ID) + (1|trial_fin), data = data_EXP3[data_EXP3$accuracy == '1',])
summ(rts.der3)

# Word Similarity
rts.WS3 <- lmer(log(RT) ~ cosine_simil + (1|ID) + (1|trial_fin), data = data_EXP3[data_EXP3$accuracy == '1',])
summ(rts.WS3)

# compare models based on AIC
AIC(rts.geo3,rts.ling3,rts.ling3low,rts.der3,rts.WS3)

# The best models are the ones including linguistic coordinates

# plot best models

# uppercase cardinal points
plot(predictorEffects(rts.ling3,~ LLat + LLon, residuals=T),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response")),
     partial.residuals = list(smooth = F,
                              col = "black",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))

# lowercase cardinal points
plot(predictorEffects(rts.ling3low,~ LLatLow + LLonLow, residuals=T),
     main = "",
     axes = list(
       grid = T,
       x = list(rotate = 30,
                rug = F),
       y = list(rotate = 30,
                rug = F,
                type = "response")),
     partial.residuals = list(smooth = F,
                              col = "black",
                              cex = .2,
                              pch = 16),
     lattice=list(layout=c(1, 1)))
