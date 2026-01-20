library(pwr)
pwr.r.test(n = 8458, sig.level = .05, power = .80) 
pwr.f2.test(u =5, v = 8452, f2 = , sig.level = .008, power = .80) 
#where u and v are the numerator and denominator degrees of freedom. We use f2 as the effect size measure. 

summary(lm(loginc.res ~  per_e + ba2_e + per_e*ba2_e + per_e2 + ba2_e2, data = valid))

#https://math.stackexchange.com/questions/617735/multiple-regression-degrees-of-freedom-f-test
#The correct approach is to use p−1 in the numerator (degrees of freedom of the model) and n−p in the denominator (degrees of freedom of the error), where p is the number of predictors and n is the number of observations. 