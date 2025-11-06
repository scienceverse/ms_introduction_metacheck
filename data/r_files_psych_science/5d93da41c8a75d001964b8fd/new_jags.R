rm(list=ls())
library(R2jags)

#######################################
#This first part loads in both type 1 and type 2 choices
#This code generates Figure 2
#######################################

load("data.RData")
#cond is '1 - present' '2 - absent' '3-present/salience' '4-absent/salience'

#re-order subject numbers to be consequetive
subj = array(dim = dim(data)[1])
subj[1] = 1
s = 1
for (i in 2:dim(data)[1]){
  if (data$subjID[i] != data$subjID[i - 1]) s = s + 1
  subj[i] = s
}
data$subjID = subj

#get K, the number of risky choices
tmpK = tapply(data$riskyChoice,list(data$cond,data$type,data$subjID,data$exp),sum,na.rm=T)
#out of N trials
tmpN = tapply(data$riskyChoice,list(data$cond,data$type,data$subjID,data$exp),length)

#reformat the data into something that is easy to work with in JAGS
ks = NA; trials = NA; cond = NA; type = NA; exp = NA; subject = NA; ns = 0;
for (e in 1:4){
  for (c in 1:2){
    for (t in 1:2){
      tmp1 = tmpK[c,t,,e]
      tmp1 = tmp1[!is.na(tmpK[c,t,,e])]
      tmp2 = tmpN[c,t,,e]
      tmp2 = tmp2[!is.na(tmpN[c,t,,e])]
      
      if (length(tmp1) > 0){
        s = 1:length(tmp1) + ns
        
        ks = c(ks, tmp1)
        trials = c(trials, tmp2)
        cond = c(cond, rep(c, length(tmp1)))
        type = c(type, rep(t, length(tmp1)))
        exp = c(exp, rep(e, length(tmp1)))
        subject = c(subject, s)
        ns = length(tmp1) + ns
      }
    }
  }
}
ks = ks[-1]; trials = trials[-1]; type = type[-1]; cond = cond[-1]; subject = subject[-1]; exp = exp[-1];
tmpdat = as.data.frame(cbind(exp, subject, cond, type, ks, trials))

#define variables for JAGS code
nsubjs= length(unique(tmpdat$subject))
sub = tmpdat$subject
ks = tmpdat$ks
ns = tmpdat$trials
cond = tmpdat$cond
type = tmpdat$type

data = list("cond","ns","ks","nsubjs")
parameters=c('Val','sdV')

#call JAGS to fit model
samples <- jags(data, inits=NULL,parameters.to.save=parameters,
                model.file ="model-collapse.txt", n.chains=3, n.iter=5000, 
                n.burnin=1000, n.thin=1, DIC=T)

Vals = samples$BUGSoutput$sims.list$Val
#use the empirical CDF function on the posterior samples so that we can later get quantiles.
cdfVA = ecdf(Vals[,2])
cdfVP = ecdf(Vals[,1])

diffSAallTypes = Vals[,2] - Vals[,1]

intVA = quantile(cdfVA,c(.025,0.5,0.975))
intVP = quantile(cdfVP,c(.025,0.5,0.975))

#Make Figure 2 - plot of pooled population-level estimated risky choice rates and histogram of difference between value conditions
pdf(file = 'pooledThetaPlot.pdf', height = 4, width = 7)
layout(m = array(c(1,2),dim=c(1,2)))
par(mar = c(4,4,1,1))
plot(x = c(1,2), y = c(intVP[2],intVA[2]), ylim = c(0.2,0.55), pch = 16, cex = 1.3, axes = F, ylab = '', xlab = '', xlim = c(0.75,2.25))
axis(side = 1, at = c(1,2), labels = F);
mtext(side = 1, at = c(1), c('Standard'), line = 1.2)
mtext(side = 1, at = c(2), c('Value\nIgnorance'), line = 1.7)
arrows(x0 = c(1,2), x1=c(1,2), y0=c(intVP[1],intVA[1]), y1=c(intVP[3],intVA[3]), code = 3, length = 0.05, angle = 90)
axis(side = 2)
mtext(side = 2, 'Proportion Risky Choices', line = 2.5, cex = 1.2)

xmin = min(diffSAallTypes)
xmax = max(diffSAallTypes)

xrange = c(xmin - 0.05 * (xmax-xmin), xmax + 0.05 * (xmax-xmin))
xbreaks = seq(xrange[1],xrange[2],length.out = 32)

histDiffSA = hist(diffSAallTypes, breaks = xbreaks, plot = F)

plot(-1e10,-1e10, xlim = xrange, ylim = c(-1,13), xlab = '', ylab = '', axes = F)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSA$density, type = 'h', col = rgb(0,0,1), lwd = 4, lty = 1)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSA$density, type = 'l', col = rgb(0,0,1,0.5), lwd = 1, lty = 1)
mtext(side = 2, 'Density', line = 2.5, cex = 1.3)
mtext(side = 1, expression(paste(theta, " Value-Ignorance ", - theta, " Standard")), line = 2.5, cex = 1)
axis(side = 1); axis(side = 2, at = c(-10,-5,0,5,10), lab = c(10,5,0,5,10), cex = 1.2); #box()
abline(h = 0) 
abline(v = 0, col = 'red', lty = 2, lwd = 2)
dev.off()



#################################################
#This is the same as above, but only type 1 choices. 
#This code generates Figure 3
#################################################

load("data.RData")
subj = array(dim = dim(data)[1])
subj[1] = 1
s = 1
for (i in 2:dim(data)[1]){
  if (data$subjID[i] != data$subjID[i - 1]) s = s + 1
  subj[i] = s
}
data$subjID = subj

tmpK = tapply(data$riskyChoice,list(data$cond,data$type,data$subjID,data$exp),sum,na.rm=T)
tmpN = tapply(data$riskyChoice,list(data$cond,data$type,data$subjID,data$exp),length)

ks = NA; trials = NA; cond = NA; type = NA; exp = NA; subject = NA; ns = 0;
for (e in 1:4){
  for (c in 1:4){
    for (t in 1:2){
      tmp1 = tmpK[c,t,,e]
      tmp1 = tmp1[!is.na(tmpK[c,t,,e])]
      tmp2 = tmpN[c,t,,e]
      tmp2 = tmp2[!is.na(tmpN[c,t,,e])]
      
      if (length(tmp1) > 0){
        s = 1:length(tmp1) + ns
        
        ks = c(ks, tmp1)
        trials = c(trials, tmp2)
        cond = c(cond, rep(c, length(tmp1)))
        type = c(type, rep(t, length(tmp1)))
        exp = c(exp, rep(e, length(tmp1)))
        subject = c(subject, s)
        ns = length(tmp1) + ns
      }
    }
  }
}

ks = ks[-1]; trials = trials[-1]; type = type[-1]; cond = cond[-1]; subject = subject[-1]; exp = exp[-1];

tmpdat = as.data.frame(cbind(exp, subject, cond, type, ks, trials))

expt = tmpdat$exp
type = tmpdat$type
use = expt >=3 & type == 1
expt = tmpdat$exp[use]
type = tmpdat$type[use]
sub = tmpdat$subject[use]
ks = tmpdat$ks[use]
ns = tmpdat$trials[use]
cond = tmpdat$cond[use]
nsubjs = length(unique(tmpdat$subject[use]))

data = list("expt","cond","ns","ks","nsubjs")

parameters=c('Val','sdV')

# The following command calls JAGS with specific options.
# For a detailed description the R2jags documentation.
samples <- jags(data, inits=NULL,parameters.to.save=parameters,
                model.file ="model-type1.txt", n.chains=3, n.iter=5000, 
                n.burnin=1000, n.thin=1, DIC=T)

Vals = samples$BUGSoutput$sims.list$Val

diffSA = Vals[,2] - Vals[,1]
diffSP = Vals[,4] - Vals[,3]

cdfSA = ecdf(diffSA); 
cdfSP = ecdf(diffSP);
cdfInt = ecdf(diffSA-diffSP);

#get information for each condition for plotting
cdfVASA = ecdf(Vals[,2])
cdfVPSA = ecdf(Vals[,1])
cdfVASP = ecdf(Vals[,4])
cdfVPSP = ecdf(Vals[,3])

intVASA = quantile(cdfVASA,c(.025,0.5,0.975))
intVPSA = quantile(cdfVPSA,c(.025,0.5,0.975))
intVASP = quantile(cdfVASP,c(.025,0.5,0.975))
intVPSP = quantile(cdfVPSP,c(.025,0.5,0.975))

#create Figure 3
pdf(file = 'exp34Plot.pdf', height = 8, width = 7)
layout(m = array(c(1,2,1,3),dim=c(2,2)))
par(mar = c(4,4,1,1))

plot(x = c(1,2,3.5,4.5), y = c(intVPSA[2],intVASA[2],intVPSP[2],intVASP[2]), ylim = c(0,0.6), pch = 16, cex = 1.3, axes = F, ylab = '', xlab = '', xlim = c(0.75,4.75))
axis(side = 1, at = c(1,2), labels = F); axis(side = 1, at = c(3.5,4.5), labels = F)
mtext(side = 1, at = c(1,3.5), c('Standard', 'Standard'), line = 1.2)
mtext(side = 1, at = c(2,4.5), c('Value-\nIgnorance','Value-\nIgnorance'), line = 1.7)
arrows(x0 = c(1,2,3.5,4.5), x1=c(1,2,3.5,4.5), y0=c(intVPSA[1],intVASA[1],intVPSP[1],intVASP[1]), y1=c(intVPSA[3],intVASA[3],intVPSP[3],intVASP[3]), code = 3, length = 0.05, angle = 90)
axis(side = 2)
mtext(side = 2, 'Proportion Risky Choices', line = 2.5, cex = 1.2)
mtext(side = 3, 'No-Salience', at = 1.5, line = -1, cex = 1.1)
mtext(side = 3, 'Salience', at = 4, line = -1, cex = 1.1)

xmin = min(diffSA, diffSP)
xmax = max(diffSA, diffSP)

xrange = c(xmin - 0.05 * (xmax-xmin), xmax + 0.05 * (xmax-xmin))
xbreaks = seq(xrange[1],xrange[2],length.out = 32)

histDiffSA = hist(diffSA, breaks = xbreaks, plot = F)
histDiffSP = hist(diffSP, breaks = xbreaks, plot = F)

yrange = c(-max(histDiffSP$density) - 0.15*max(histDiffSP$density),max(histDiffSA$density) + 0.15*max(histDiffSA$density))

plot(-1e10,-1e10, xlim = xrange, ylim = c(-1,6), xlab = '', ylab = '', axes = F)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSA$density, type = 'h', col = rgb(0,0,1), lwd = 4, lty = 1)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSA$density, type = 'l', col = rgb(0,0,1,0.5), lwd = 1, lty = 1)
mtext(side = 2, 'Density', line = 2.5, cex = 1.3)
mtext(side = 1, expression(paste(theta, " Value-Ignorance ", - theta, " Standard")), line = 2.5, cex = 1)
axis(side = 1); axis(side = 2, at = c(-10,-5,0,5,10), lab = c(10,5,0,5,10), cex = 1.2); #box()
abline(h = 0) 
abline(v = 0, col = 'red', lty = 2, lwd = 2)

plot(-1e10,-1e10, xlim = xrange, ylim = c(-1,6), xlab = '', ylab = '', axes = F)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSP$density, type = 'h', col = rgb(0,0,1), lwd = 4, lty = 1)
lines(x = xbreaks[-1] - diff(xbreaks)[1]/2, y = histDiffSP$density, type = 'l', col = rgb(0,0,1,0.5), lwd = 1, lty = 1)
mtext(side = 1, expression(paste(theta, " Value-Ignorance ", - theta, " Standard")), line = 2.5, cex = 1)
axis(side = 1); #axis(side = 2, at = c(-10,-5,0,5,10), lab = c(10,5,0,5,10), cex = 1.2); #box()
abline(h = 0) 
abline(v = 0, col = 'red', lty = 2, lwd = 2)
dev.off()
