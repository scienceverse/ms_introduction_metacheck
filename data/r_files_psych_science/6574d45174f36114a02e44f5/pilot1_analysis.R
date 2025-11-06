###Data Description:
'
Description: This script performs a power analysis for E1 and
grid searches for UPCOs free parameter using pilot data.
'

###Necessary Libraries: [Import with 'install.packages("package_name")' if library missing]

#data wrangling
library(qualtRics)
library(data.table)
library(boot)
library(pwr)
library(gmp)
library(MASS)

set.seed(47)

#Set your directory here
setwd(r"(YOUR PATH HERE)")

#Import data here
Raw <- read_survey("pilot1_data.csv")
DT <- data.table(copy(Raw))

#Assign Subject #s
DT[,Subject := seq(1:nrow(DT))]
DT[,Subject := as.factor(Subject)]

#remove unrealistic completion times
DT = DT[which(DT$`Duration (in seconds)`>120),]
#remove unused attention check
DT$`19_trial_1` = NULL

mdt = melt.data.table(DT,id.vars = 
                        c('Subject'),
                measure.vars = list(
                  belief = c(paste0(seq(1,18),'_trial_1'))
))


###Mean Correlations
ofq = mdt[,mean(value,na.rm = T)/100,variable]$V1

#Estimate confidence intervals for means
trials = c(paste0(seq(1,18),'_trial_1'))
cis = data.frame('ci_low' = numeric(), 'ci_high' = numeric())
for(i in seq(1,18)){
  corr.fun <- function(data, idx)
  {
    df <- mdt[idx, ]
    df[variable ==trials[i],mean(value,na.rm = T)]
  }
  
  bootstrap <- boot(mdt, corr.fun, R = 5000)
  cib = boot.ci(bootstrap, type ='perc')
  cis[i, 'ci_low'] = cib$percent[4]
  cis[i, 'ci_high'] = cib$percent[5]
}
cis$ci_dif = cis$ci_high - cis$ci_low
#mean difference in 95% confidence intervals
summary(cis$ci_dif)

cis$est = ofq
cis$prop = cis$ci_dif / (cis$est*100)
summary(cis$prop)


nrow(DT)


#simulate upco
n = 100
a = seq(1,n,1)
b = seq(1,n,1)
m = expand.grid(a, b)
m$text = paste0(m$Var1,'\n',m$Var2)
m = data.table(m)

v1 = c(17, 20, 39, 33, 50, 41, 11, 3, 15, 7, 19, 11, 7, 11, 30, 22, 
       47, 33)
v2 = c(12, 28, 28, 46, 36, 57, 1 , 13, 5 , 17, 9 , 21, 1, 23, 16, 
       38, 29, 52)

points = paste(v1,v2,sep = '\n')

m_test = m[text %in% points, ]
m_test = m_test[match(points, m_test$text),]

upco = function(p,a,b){
  p^(a-b) / ( p^(a-b) + (1-p)^(a-b))
}

devs = c()
grid = seq(.01,.99,.01)
for(i in grid){
  m_test$UPCO = upco(i, m_test$Var1, m_test$Var2)
  devs = c(devs, sum(abs(ofq-m_test$UPCO)))
}
grid[which.min(devs)]
#.51 minimizes absolute deviations. 

#now estimate power for correlations
m_test$UPCO = upco(.49, m_test$Var1, m_test$Var2)
m_test$Bayes =  (m_test$Var2 + 1) / (m_test$Var1 + m_test$Var2 + 2)

competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}
for(n in 1:18){
  m_test[n, 'comp':= competence(Var2,Var1)]
}
m_test$par = ofq

c1 = cor.test(m_test$Bayes,m_test$par)
c2 = cor.test(m_test$UPCO,m_test$par)
c3 = cor.test(m_test$comp,m_test$par)

pwr.r.test(n = 18,sig.level = .001,r = c1$estimate)
pwr.r.test(n = 18,sig.level = .001,r = c2$estimate)
pwr.r.test(n = 18,sig.level = .001,r = c3$estimate)

c4 = cor.test(m_test$Bayes,m_test$UPCO)
c5 = cor.test(m_test$Bayes,m_test$comp)

p1= paired.r(c1$estimate,c2$estimate,c4$estimate,18)
p2= paired.r(c1$estimate,c3$estimate,c5$estimate,18)

#power to detect difference between Bayes and UPCO
ps = c()
for(sample in 1:20000){
  set.seed(sample)
  # Generate correlated samples using a multivariate normal for power analyses
  #using the statistics of the data for the covariance matrix
  sample <- mvrnorm(18, mu = c(0, 0, 0), # means of the populations, doesn't matter 
                      Sigma = matrix(c(1,           c1$estimate, c2$estimate,
                                       c1$estimate, 1,            c4$estimate,
                                       c2$estimate, c4$estimate, 1), ncol = 3))
  # check correlation matrix
  r12 = cor(sample[,1], sample[,2]) #participant / Bayes
  r13 = cor(sample[,1], sample[,3]) #participant / UPCO 
  r23 = cor(sample[,2], sample[,3]) # bayes / UPCO
  
  paired_r_test = paired.r(n = 18, r12, r13, r23)
  ps = c(ps, paired_r_test$p)
}
#power at alpha = .01
sum(ps < .01) / length(ps)

#power to detect difference between Bayes and Competence
ps = c()
for(sample in 1:20000){
  set.seed(sample)
  # Generate samples for correlations
  sample <- mvrnorm(18, mu = c(0, 0, 0), # means of the populations, doesn't matter 
                    Sigma = matrix(c(1,           c1$estimate, c3$estimate,
                                     c1$estimate, 1,            c5$estimate,
                                     c3$estimate, c5$estimate, 1), ncol = 3))
  # check correlation matrix
  r12 = cor(sample[,1], sample[,2]) #participant / Bayes
  r13 = cor(sample[,1], sample[,3]) #participant / Competence 
  r23 = cor(sample[,2], sample[,3]) # bayes / Competence
  
  paired_r_test = paired.r(n = 18, r12, r13, r23)
  ps = c(ps, paired_r_test$p)
}
#power at alpha = .01
sum(ps < .01) / length(ps)


