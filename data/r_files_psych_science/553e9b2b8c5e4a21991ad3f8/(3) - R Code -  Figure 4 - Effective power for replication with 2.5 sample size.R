#R Code behind Figure 4 in "Small Telescopes"
#Written by Uri Simonsohn (uws@wharton.upenn.edu)
#Last update: 2014 12 15
#
##################################################################
#Overview:
#This program identifies the effective power of rejecting d33% (an effect giving the original sample 33% power)
#when replications are run with 2.5 times the sample size of the origional
####################################################################################

library(metafor)
library(pwr)


#These functions use as input a sample size of the original research.
#They then compute the effect size that gives that sample size 33% power
#Then they estiate the probability of rejecting the corresponding d33 (or h33 for proportions) 
#if the true effect is 0.

#cHI-2
  #function computes power from using the 2.5 rule to detect the h33 effect
    chi25=function(nori) {
      #Find h33 for nori=n (this is the measure of effect size for proportions tests)
              h33=pwr.2p.test(n=nori,sig.level=0.05,power=1/3,alternative="two.sided")$h
      #Find power for nrep
              nrep=round(2.5*nori)  #rounds sample size 
              power25=pwr.2p.test(n=nrep,sig.level=0.05,h=h33,alternative="greater")$power 
              #Note: here I am using the effect size as a pivotal quantity, computing power to reject 0, one sided, 
              #if the true effect is d33. For the chi2 that's teh same value as probaility of rejecting d33 if the
              #true effect is 0.
              return(c(nori,nrep,h33,power25))
            }


#ttest
  t25=function(nori) {
           d33=pwr.t.test(n=nori,sig.level=0.05,power=1/3,type="two.sample")$d
           nrep=round(2.5*nori)
           power25=pwr.t.test(n=nrep,sig.level=0.05,d=d33,type="two.sample",alternative="greater")$power
      return(c(nori,nrep,d33,power25))
  }


#create vector with sample size from n=10 to 100
  nori=seq(from=10,to=1000,by=1)
  
#apply function to each sample size consisered for t,chi
  results_t25=t(matrix(sapply(nori,t25),4,991))
  results_chi25=t(matrix(sapply(nori,chi25),4,991))
#Write results to .csv file to run graphs in excel
  write.table(results_t25,file="Power 2.5 for t-test.csv",sep=",")
  write.table(results_chi25,file="Power 2.5 for chi-test.csv",sep=",")
  
#Quick look at results within R
  plot(results_t25[,4],ylim=c(0,1),cex=.25,main="Results for the Student distribution",ylab="Power to reject d33%")
  plot(results_chi25[,4],ylim=c(0,1), cex=.25, main="Results for the chi-2 distribution",ylab="Power to reject d33%")
  
