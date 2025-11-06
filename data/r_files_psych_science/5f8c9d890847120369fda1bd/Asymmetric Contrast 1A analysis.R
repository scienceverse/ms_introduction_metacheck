# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 
library(dplyr, warn.conflicts = FALSE)

# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 1A data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$reference = factor(dt$reference, levels = c('low', 'control', 'high'))
dt$valence = factor(dt$valence, levels = c('negative', 'positive'))
dt$id = as.factor(dt$id)

# Analysis #### 

aovRoundDigits(
        ezANOVA(
                data = dt,
                dv = forecast, 
                wid = id, 
                between = list(reference, valence), 
                type = 3
        ), 
        nsmall = 3)

aovRoundDigits(
        ezANOVA(
                data = subset(dt, valence == 'negative'),
                dv = forecast, 
                wid = id, 
                between = list(reference), 
                type = 3
        ), 
        nsmall = 3)

aovRoundDigits(
        ezANOVA(
                data = subset(dt, valence == 'positive'),
                dv = forecast, 
                wid = id, 
                between = list(reference), 
                type = 3
        ), 
        nsmall = 3)

# Additional analysis ####

aovRoundDigits(
        ezANOVA(
                data = subset(dt, reference != 'high'),
                dv = forecast, 
                wid = id, 
                between = list(reference, valence), 
                type = 3
        ), 
        nsmall = 3)

aovRoundDigits(
        ezANOVA(
                data = subset(dt, reference != 'low'),
                dv = forecast, 
                wid = id, 
                between = list(reference, valence), 
                type = 3
        ), 
        nsmall = 3)

groupwiseMean(forecast ~ reference,
               data   = subset(dt, valence == 'negative'),
               conf   = 0.95,
               digits = 3)

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'control'], 
        var.equal = T
)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'control'])

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'control']
)

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high'],
        dt$forecast[dt$valence == 'negative' & dt$reference == 'control'], 
        var.equal = T
)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high'])

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'control'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high']
)

groupwiseMean(forecast ~ reference,
              data   = subset(dt, valence == 'positive'),
              conf   = 0.95,
              digits = 3)

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'control'], 
        var.equal = T
)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'control'])

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'control']
)

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high'],
        dt$forecast[dt$valence == 'positive' & dt$reference == 'control'], 
        var.equal = T
)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high'])

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'control'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high']
)