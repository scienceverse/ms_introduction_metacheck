# Load libraries ####

library(mediation)
library(ez)
library(psychReport)
library(rcompanion)
library(effsize) 

# Load data ####

pathname = "PATH TO FILE"
filename = "Asymmetric Contrast 2C data.csv"

setwd(pathname)
dt = read.csv(filename, header = T)

dt$id = as.factor(dt$id)

dt$reference = factor(dt$reference, levels = c('low', 'high'))

# Analysis ####

tbl = table(context = dt$reference, gambled = dt$gambled)
print(aggregate(dt$gambled == 1, list(context = dt$reference), mean, na.rm=T), digits = 2)
chisq.test(tbl)

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
              digits = 4)

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
              digits = 4)

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

negA = lm(forecast ~ reference, data = subset(dt, valence == 'negative'))
negB = glm(gambled ~ forecast + reference, family = binomial(link = "probit"), data = subset(dt, valence == 'negative'))
mediationNeg = mediate(negA, negB, treat = "reference", mediator = "forecast", robustSE = TRUE, sims = 10000)
sensNeg = medsens(mediationNeg, rho.by = 0.01, effect.type = "indirect", sims = 10000)

summary(mediationNeg)
summary(negA)
summary(negB)
summary(sensNeg)

posA = lm(forecast ~ reference, data = subset(dt, valence == 'positive'))
posB = glm(gambled ~ forecast + reference, family = binomial(link = "probit"), data = subset(dt, valence == 'positive'))
mediationPos = mediate(posA, posB, treat = "reference", mediator = "forecast", robustSE = TRUE, sims = 10000)
sensPos = medsens(mediationPos, rho.by = 0.01, effect.type = "indirect", sims = 10000)

summary(mediationPos)
summary(posA)
summary(posB)
summary(sensPos)