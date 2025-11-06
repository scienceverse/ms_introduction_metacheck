# Load libraries ####

library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 
library(reshape2)
library(dplyr)

# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 3B data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$id = as.factor(dt$id)

dt$reference = factor(dt$reference, levels = c('low', 'high'))

lg = subset(dt, select = c('id', 'reference', 'negative', 'positive', 'first'))

lg = melt(lg, id.vars=c('id', 'reference', 'first'), measure.vars = c('negative', 'positive'), variable.name = 'valence', value.name = 'forecast')

lg$first = ifelse(lg$first == lg$valence, 1, 0)

# Analysis ####

anova1 = ezANOVA(
        data = lg, 
        dv = forecast, 
        wid = id, 
        between = list(reference, valence, first),
        type = 3
)
aovRoundDigits(anova1, nsmall = 3)

anova1 = ezANOVA(
        data = subset(lg, first == 1), 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference),
        type = 3
)
aovRoundDigits(anova1, nsmall = 3)

anova1 = ezANOVA(
        data = subset(lg, first == 0), 
        dv = forecast, 
        wid = id, 
        between = list(valence, reference),
        type = 3
)
aovRoundDigits(anova1, nsmall = 3)

# Additional analysis ####

groupwiseMean(forecast ~ reference,
              data = subset(lg, valence == 'negative' & first == 1),
              conf = 0.95,
              digits = 4)

sd(lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 1])
sd(lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 1])

t.test(
        lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 1], 
        lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 1],
        var.equal = T
)

cohen.d(
        lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 1], 
        lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 1],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(lg, valence == 'positive' & first == 1),
              conf = 0.95,
              digits = 4)

sd(lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 1])
sd(lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 1])

t.test(
        lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 1], 
        lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 1],
        var.equal = T
)

cohen.d(
        lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 1], 
        lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 1],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(lg, valence == 'negative' & first == 0),
              conf = 0.95,
              digits = 4)

sd(lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 0])
sd(lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 0])

t.test(
        lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 0], 
        lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 0],
        var.equal = T
)

cohen.d(
        lg$forecast[lg$valence == 'negative' & lg$reference == 'low' & lg$first == 0], 
        lg$forecast[lg$valence == 'negative' & lg$reference == 'high' & lg$first == 0],
        var.equal = T
)

groupwiseMean(forecast ~ reference,
              data = subset(lg, valence == 'positive' & first == 0),
              conf = 0.95,
              digits = 4)

sd(lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 0])
sd(lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 0])

t.test(
        lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 0], 
        lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 0],
        var.equal = T
)

cohen.d(
        lg$forecast[lg$valence == 'positive' & lg$reference == 'low' & lg$first == 0], 
        lg$forecast[lg$valence == 'positive' & lg$reference == 'high' & lg$first == 0],
        var.equal = T
)