#load libraries
library(pwr)

#F statistic from article
F <- 103.7

#convert to t statistic (Lakens, 2013)
t <- sqrt(F)
n <- 100

#convert to d (Lakens, 2013)
d <- t / sqrt(n); d

#use d to compute N for power of .95
pwr.t.test(d=d, power=.95, n=NULL, type="paired")
