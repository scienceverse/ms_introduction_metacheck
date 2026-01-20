library(here)
library(tidyverse)
library(ggplot2)

#Read in data
raw = read.csv('final_data.csv')


#Clean data
names(raw)

temp = raw

temp = temp[3:nrow(temp),]

temp = temp[raw$DistributionChannel=="anonymous",]

temp = type.convert(temp)

temp$avg_choice = ifelse(temp$avg100.1 =="", temp$avg100, temp$avg100.1)


temp$one_choice = ifelse(temp$one.1 =="", temp$one, temp$one.1)

temp$id = 1:nrow(temp)

#Exclusions not possible due to incorrect coding of attention check

long = temp %>% pivot_longer(cols = contains("choice"), values_to = "choice", names_to = "cond_name")

long = long[long$choice != "",]

#Create a choice variable
long$choose_low = ifelse((long$choice == "Group A" & long$a_hi == 1) | (long$choice == "Group B" & long$a_hi == 0),1,0)


#run regression
summary(glm(choose_low ~ condition, data = long, family = "binomial"))



prop.table(table(long$choose_low, long$condition), 2)

length(unique(long$id))
