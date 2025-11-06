# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 

# load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 3A data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$id = as.factor(dt$id)
dt$reference = as.factor(dt$reference)
dt$valence = as.factor(dt$valence)
dt$attention = as.factor(dt$attention)

dt$valence = ordered(dt$valence, levels = c('negative', 'positive'))
dt$attention = ordered(dt$attention, levels = c(0, 1))
dt$reference = ordered(dt$reference, levels = c('low', 'high'))

# Analysis ####

anovaO = ezANOVA(
        data = dt, 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference, attention), 
        type = 3)
aovRoundDigits(anovaO, nsmall = 3)

anova2 = ezANOVA(
        data = subset(dt, attention == 0), 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence), 
        type = 3)
aovRoundDigits(anova2, nsmall = 3)

anova2 = ezANOVA(
        data = subset(dt, attention == 1), 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence), 
        type = 3)
aovRoundDigits(anova2, nsmall = 3)

# Additional analysis ####

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'negative' & attention == 0),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 0])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 0])

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 0], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 0],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 0], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 0],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positive' & attention == 0),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 0])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 0])

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 0], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 0],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 0], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 0],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'negative' & attention == 1),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 1])
sd(dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 1])

t.test(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 1], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 1],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'negative' & dt$reference == 'low' & dt$attention == 1], 
        dt$forecast[dt$valence == 'negative' & dt$reference == 'high' & dt$attention == 1],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(dt, valence == 'positive' & attention == 1),
              conf = 0.95,
              digits = 4)

sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 1])
sd(dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 1])

t.test(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 1], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 1],
        var.equal = T
)

cohen.d(
        dt$forecast[dt$valence == 'positive' & dt$reference == 'low' & dt$attention == 1], 
        dt$forecast[dt$valence == 'positive' & dt$reference == 'high' & dt$attention == 1],
        var.equal = T
)