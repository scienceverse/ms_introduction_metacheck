# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 

# load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast Supplemental 1 data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$reference = factor(dt$reference, levels = c('low', 'high'))
dt$valence = factor(dt$valence, levels = c('negative', 'positiveControl', 'positiveAttention'))

# Analysis #### 

anova2 = ezANOVA(
        data = subset(dt, valence != 'positiveAttention'), 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference), 
        type = 3)
aovRoundDigits(anova2, nsmall = 3)

anova2 = ezANOVA(
        data = subset(dt, valence != 'positiveControl'), 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference), 
        type = 3)
aovRoundDigits(anova2, nsmall = 3)

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
              data = subset(dt, valence == 'positiveControl'),
              conf = 0.95,
              digits = 3)

sd(dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'high'])

t.test(
        dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'high'], 
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positiveControl' & dt$reference == 'high'], 
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positiveAttention'),
              conf = 0.95,
              digits = 3)

sd(dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'low'])
sd(dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'high'])

t.test(
        dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'high'], 
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'low'], 
        dt$forecast[dt$valence == 'positiveAttention' & dt$reference == 'high'], 
        var.equal = T
)