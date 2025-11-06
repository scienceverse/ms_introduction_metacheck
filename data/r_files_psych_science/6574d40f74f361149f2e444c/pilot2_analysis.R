#Simulations
library(data.table)
library(ggplot2)

####MODEL FIRST, DATA SECOND
####SET PARAMETER SPACE
theta = seq(0,1,.01)
n = 25
a = seq(1,n,1)
b = seq(1,n,1)
m = expand.grid(a, b)


####BINOMIAL MODEL PREDICTIONS
m$b_pm = (m$Var1 + 1) / (m$Var1 + m$Var2 + 2) #this is using the conjugate prior

####UPCO MODEL PREDICTIONS
upco = function(p,a,b){
  p^(a-b) / ( p^(a-b) + (1-p)^(a-b))
}

####COMPETENCE MODEL PREDICTIONS
library(gmp)
competence = function(a,b){
  n = a + b
  cons = factorialZ(n+1) / (factorialZ(a)*factorialZ(b))
  integrand = function(x){(x^b)*((1-x)^a)}
  integ = integrate(integrand, lower = 0, upper = .5)
  res = as.numeric(cons)*integ$value
  res
}

m$dif = apply(m,1,function(X) competence(X[1],X[2]))
m$UPCO = apply(m,1,function(X) upco(.51,X[1],X[2]))



#data wrangling
library(qualtRics)
library(data.table)
library(pwr)
library(psych)


#Set your directory here
setwd(r"(C:\Users\Kerem Oktar\OneDrive\Research\Princeton2019-2024\Disagreement\Computational\OSF)")

#Import data here
Raw <- read_survey("pilot2_data.csv")
dt <- data.table(copy(Raw))

####Demographics
dt$Age <- as.numeric(dt$Age)
dt[,mean(Age,na.rm = T)]
dt$Sex <- as.numeric(dt$Sex)
dt[,mean(Sex-1,na.rm = T)]

#Assign Subject #s
dt[,Subject := seq(1:nrow(dt))]
dt[,Subject := as.factor(Subject)]

#remove unrealistic completion times
dt = dt[which(dt$`Duration (in seconds)`>180),]
dt = dt[,-(StartDate:Q11)]
nrow(dt[example_1 > 50,]) # 44 failed instructions check

mdt = melt(dt, id.vars = 'Subject',measure.vars = paste0(seq(1,169),"_t_1"))


#m is the data table that holds the model predictions
# skip one sampling
library(stringr)

#description of sampling:there are 225 total cells to sample; each of them 
#from a square skip-1 from 0-30; but we need to skip a bunch of cells
sk_code = rep(seq(1,25,2),length.out = 169) + 
  rep(seq(0,13), each = 13, #this is the max Var1 that you go to /2 but round down
      length.out = 169)*50 #need to skip 29; but already went 29; so need to skip 29*2
m_sk = m[sk_code,]

dt$points = 0
for(sub in dt$Subject){
  ids = mdt[Subject == sub & !is.na(value), str_match(variable, pattern = '^[0-9]+')]
  
  mdt[Subject == sub & !is.na(value), Var1 := m_sk[as.numeric(ids), 'Var1']]
  mdt[Subject == sub & !is.na(value), Var2 := m_sk[as.numeric(ids), 'Var2']]
}

mdt = mdt[!is.na(mdt$value),]
means = mdt[,mean(value),.(Var1,Var2)]
means$V1 =means$V1/100 

m_sk$Participant = means$V1
mm = melt.data.table(as.data.table(m_sk), id.vars = c('Var1','Var2'))

attributes(m_sk$Participant) <- NULL
names(m_sk) <- c('True','False',"Bayes", "Competence", "UPCO",'Participant')
  
mb = lm(formula = Participant ~ Bayes, data = m_sk)
mu = lm(formula = Participant ~ Competence, data = m_sk)
mc = lm(formula = Participant ~ UPCO, data = m_sk)

mub = lm(formula = Participant ~ Bayes + UPCO, data = m_sk)
mcb = lm(formula = Participant ~ Bayes + Competence, data = m_sk)
mucb = lm(formula = Participant ~ Bayes + Competence  + UPCO, data = m_sk)

#individual model power
r2 = as.numeric(summary(mb)[8])
pwr.f2.test(u = 1, v= 167, f2 = r2 / (1-r2), sig.level = .001)

r2 = as.numeric(summary(mu)[8])
pwr.f2.test(u = 1, v= 167, f2 = r2 / (1-r2), sig.level = .001)

r2 = as.numeric(summary(mc)[8])
pwr.f2.test(u = 1, v= 167, f2 = r2 / (1-r2), sig.level = .001)

##model comparison power
#bayes vs bayes + upco
r2a = as.numeric(summary(mb)[8])
r2ab = as.numeric(summary(mub)[8])
pwr.f2.test(u = 3, v= 165, f2 = (r2ab-r2a) / (1-r2ab), sig.level = .001)

#bayes vs bayes + competence
r2a = as.numeric(summary(mb)[8])
r2ab = as.numeric(summary(mcb)[8])
pwr.f2.test(u = 3, v= 165, f2 = (r2ab-r2a) / (1-r2ab), sig.level = .001)

#bayes vs bayes + competence
r2a = as.numeric(summary(mub)[8])
r2ab = as.numeric(summary(mucb)[8])
pwr.f2.test(u = 3, v= 165, f2 = (r2ab-r2a) / (1-r2ab), sig.level = .001)

#bayes vs bayes + competence
r2a = as.numeric(summary(mcb)[8])
r2ab = as.numeric(summary(mucb)[8])
pwr.f2.test(u = 3, v= 165, f2 = (r2ab-r2a) / (1-r2ab), sig.level = .001)



