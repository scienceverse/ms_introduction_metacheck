# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 

# load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast attention manipulation studies data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$id = as.factor(dt$id)
dt$reference = as.factor(dt$reference)
dt$valence = as.factor(dt$valence)
dt$condition = as.factor(dt$condition)

dt$valence = ordered(dt$valence, levels = c('negative', 'positive'))
dt$condition = ordered(dt$condition, levels = c('control', 'test'))
dt$reference = ordered(dt$reference, levels = c('low', 'high'))

# Analysis ####

anovaO = ezANOVA(
        data = dt, 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference, condition), 
        type = 3)
aovRoundDigits(anovaO, nsmall = 4)

anova1 = ezANOVA(
        data = subset(dt, condition == 'control'), 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence), 
        type = 3)
aovRoundDigits(anova1, nsmall = 4)

anova2 = ezANOVA(
        data = subset(dt, condition == 'test'), 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence), 
        type = 3)
aovRoundDigits(anova2, nsmall = 4)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'negative' & condition == 'control'),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'control'])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'control'])

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'control'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'control'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'control'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'control'],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positive' & condition == 'control'),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'control'])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'control'])

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'control'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'control'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'control'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'control'],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'negative' & condition == 'test'),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'test'])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'test'])

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'test'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'test'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$condition == 'test'], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$condition == 'test'],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positive' & condition == 'test'),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'test'])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'test'])

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'test'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'test'],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$condition == 'test'], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$condition == 'test'],
        var.equal = T
)