library(tidyverse)
rm(list = ls())

#------------------------------------------------------------~
# import data
data <- read_csv('../2 - data/subjdata_correlations.csv')
srvy <- read_csv('../2 - data/subjdata_survey.csv')



#------------------------------------------------------------~
# Outlier removal

data <- 
  data %>% 
  group_by(subNo, validity) %>% 
  mutate(
    visionRT = case_when(
      is.na(visionRT) ~ NA_real_,
      visionRT < 200 ~ NA_real_,
      visionRT >= 8000 ~ NA_real_,
      visionRT > (mean(visionRT, na.rm=T) + 3*sd(visionRT, na.rm = T)) ~ NA_real_,
      visionRT < (mean(visionRT, na.rm=T) - 3*sd(visionRT, na.rm = T)) ~ NA_real_,
      visionRT < (mean(visionRT, na.rm=T) + 3*sd(visionRT, na.rm = T)) ~ visionRT,
      visionRT < (mean(visionRT, na.rm=T) - 3*sd(visionRT, na.rm = T)) ~ visionRT),
    
    afcRT = case_when(
      afcRT < 200 ~ NA_real_,
      afcRT > (mean(afcRT, na.rm = T) + 3*sd(afcRT, na.rm = T)) ~ NA_real_,
      afcRT < (mean(afcRT, na.rm = T) + 3*sd(afcRT, na.rm = T)) ~ afcRT)) 


# remove RT at incorrect responses
data$visionRT[data$accuracy == 0] <- NA_real_
data$afcRT[data$accuracy == 0] <- NA_real_



#------------------------------------------------------------~
mdiff <- function(x, y){
  return(mean(x, na.rm = T) - mean(y, na.rm = T))
}

# create a data frame that normalizes the RT for an image, and then asks whether that image
# shows an effect and averages across the image group 
normCongruencyEffect <- 
  data %>% 
  group_by(targetimg) %>% 
  mutate(imgMean = mean(visionRT, na.rm=T), imgSd = sd(visionRT, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(targetimg) %>% 
  mutate(zDiff = (imgMean - visionRT) / imgSd) %>% 
  ungroup() %>% 
  group_by(imgGrp) %>% 
  summarize(Effect = mdiff(zDiff[validity==1], zDiff[validity==0]))

normEffectplusSurvey <- 
  cbind(normCongruencyEffect, 
        propCorrectID = srvy$propCorrectID,
        propSimilarID = srvy$propSimilarID)

  
# "grDevices::Purple-Yellow", "ggthemes::Classic Area Green", "grDevices::Green-Yellow",
mp <- paletteer::paletteer_c("grDevices::TealRose", n = 10, direction = -1)
sc <- scale_colour_gradientn(colours = mp, limits = c(-.75, .5), guide = "none") 

# -----------------------------------------------~
# -- plot without names
# Effect on the y axis
tibble(normEffectplusSurvey) %>% 
  ggplot(aes(y = Effect, x = propSimilarID, color = Effect)) +
  geom_point(size = 5) + 
  coord_cartesian(ylim = c(-.6, .6), xlim = c(.59, 1.01)) +
  scale_y_continuous(breaks = round(seq(-.6, .6, length.out = 4),1)) +
  labs(x = "Proportion Correct ID",
       y = "Effect of Congruency") +
  theme_linedraw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(size = 3),
        axis.ticks = element_line(size = 2),
        text = element_text(size = 22)) +
  sc


# get statistics on correlations / linear model
# --- exact id match
cor.test(normEffectplusSurvey$Effect, normEffectplusSurvey$propCorrectID)
summary(lm(Effect ~ propCorrectID, data = normEffectplusSurvey))

# --- similar id match
cor(normEffectplusSurvey$Effect, normEffectplusSurvey$propSimilarID)
summary(lm(Effect ~ propSimilarID, data = normEffectplusSurvey))
