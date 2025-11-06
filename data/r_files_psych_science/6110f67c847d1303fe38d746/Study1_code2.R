#######################################################
# If you use or modify this code for your own research, 
# please cite the work: 
# "The Moral Significance of Aesthetics in Nature Imagery" 
# by Eunsoo Kim, Julia Lee Cunningham and Anocha Aribarg (2022) 
#########################################################

##############################
# Study 1 Image characteristic analysis
# Code for OSF  
##############################
library(stargazer)  
library(data.table)

setwd("..working directory..")
load("Study1_data2.RData")

data$DMwarm_hue <-data$warm_hue-mean(data$warm_hue) 
data$DMwarm_hue <-data$warm_hue-mean(data$warm_hue)
data$DMaverage_saturation <-data$average_saturation-mean(data$average_saturation)
data$DMaverage_brightness <-data$average_brightness-mean(data$average_brightness)
data$DMcontrast_level <-data$contrast_level-mean(data$contrast_level)
data$DMimage_clarity <-data$image_clarity-mean(data$image_clarity)
data$DMsym_intensity <-data$sym_intensity-mean(data$sym_intensity)
data$DMsym_color <-data$sym_color-mean(data$sym_color)
data$DMdiagonal_dominance <-data$diagonal_dominance-mean(data$diagonal_dominance)
data$DMrule_of_third <-data$rule_of_third-mean(data$rule_of_third)
data$DMvb_center <-data$vb_center-mean(data$vb_center)            
data$DMvb_horizontal <-data$vb_horizontal-mean(data$vb_horizontal)


#Only color 
m1 <- lm(Aesthetics~DMwarm_hue+DMaverage_saturation+DMimage_clarity+DMcontrast_level,data)
summary(m1) 

#Only compositional
m2 <- lm(Aesthetics~DMdiagonal_dominance+DMrule_of_third+DMvb_center+DMvb_horizontal+DMsym_intensity+DMsym_color,data)
summary(m2) 

#All     
m3 <- lm(Aesthetics~DMwarm_hue+DMaverage_saturation+DMimage_clarity+DMcontrast_level+ DMdiagonal_dominance+DMrule_of_third+DMvb_center+DMvb_horizontal+DMsym_intensity+DMsym_color+only_ani+ both + landscape,data)
summary(m3)   

#stargazer(m1,m2,m3, type = "text", out="Study1_Image_attribute.html")
