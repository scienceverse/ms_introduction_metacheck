# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 
library(dplyr, warn.conflicts = FALSE)

# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 1B data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$reference = factor(dt$reference, levels = c('low', 'high'))
dt$valence = factor(dt$valence, levels = c('negative', 'positive'))
dt$id = as.factor(dt$id)

dt$forecast[dt$valence == 'negative'] = - dt$forecast[dt$valence == 'negative']

# Analysis ####

anova1 = ezANOVA(
        data = dt, 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence),
        type = 3
)
aovRoundDigits(anova1, nsmall = 3)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'negative'),
              conf = 0.95,
              digits = 3)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high'])

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high'],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positive'),
              conf = 0.95,
              digits = 3)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high'])

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high'],
        var.equal = T
)