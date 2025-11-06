# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 
library(dplyr)

# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 4 data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$valence = factor(dt$valence, c('negative', 'positive'))
dt$reference = factor(dt$reference, c('low', 'high'))
dt$id = as.factor(dt$id)

# Analysis #### 

anova1 = ezANOVA(
        data = dt, 
        dv = experience, 
        wid = id, 
        between = list(reference, valence), 
        type = 3
        )
aovRoundDigits(anova1, nsmall = 3)

groupwiseMean(experience ~ reference,
              data   = subset(dt, valence == 'negative'),
              conf   = 0.95,
              digits = 4)

sd(dt$experience[dt$valence == 'negative' & dt$reference == 'low'])
sd(dt$experience[dt$valence == 'negative' & dt$reference == 'high'])

t.test(
        dt$experience[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$experience[dt$valence == 'negative' & dt$reference == 'high'], 
        var.equal = T
)

cohen.d(
        dt$experience[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$experience[dt$valence == 'negative' & dt$reference == 'high'], 
        na.rm = T
)

groupwiseMean(experience ~ reference,
              data   = subset(dt, valence == 'positive'),
              conf   = 0.95,
              digits = 4)

sd(dt$experience[dt$valence == 'positive' & dt$reference == 'low'])
sd(dt$experience[dt$valence == 'positive' & dt$reference == 'high'])

t.test(
        dt$experience[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$experience[dt$valence == 'positive' & dt$reference == 'high'], 
        var.equal = T
)

cohen.d(
        dt$experience[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$experience[dt$valence == 'positive' & dt$reference == 'high'], 
        na.rm = T
)