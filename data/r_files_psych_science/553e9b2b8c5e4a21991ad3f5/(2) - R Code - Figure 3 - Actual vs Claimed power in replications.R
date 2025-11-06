#R Code behind Figures 3 in "Small Telescopes"
#Written by Uri Simonsohn (uws@wharton.upenn.edu)
#Last update: 2014 12 15
#
#This code was also published alongside blogpost "DataColada[4]" in October of 2013 see: http://datacolada.org/wp-content/uploads/2013/10/R-Powering-Replications.txt
##################################################################


library(pwr)    #Need to install this package

#Syntax of <pwr> is a bit convoluted, so I create simpler function off it
#Function getn() gets sample size for a given effect size and desired power
  getn = function(d,power) {
    n=pwr.t.test(d=d,power=power)$n
    return(round(n,digits=0))
    }

#Function getpower() for given sample and effect size
  getpower = function(d,n) {return(pwr.t.test(d=d,n=n)$power)}

#THIS IS THE KEY FUNCTION, GOES FROM TRUE EFFECT SIZE, ORIGINAL SAMPLE SIZE AND CLAIMED POWER, TO ACTUAL POWER OF REPLCIATION
  realpower=function(nori,powerori,powerrep) {
  #SYNTAX:
  #nori: per cell sample size of original study (comparing two means)
  #powerori: true power of original study
  #powerrep: claimed power of repliation
  
  #compute df, ncp, tc, true effect size
    dfori=2*nori-2                            #degrees of freedom
    dori=pwr.t.test(n=nori,power=powerori)$d  #true effect size leading to the set power for the original study given its sample size
    ncpori=sqrt(nori)*dori                    #noncentrality parameter for student distribution given true effect and original sample size
    tcrossprod=qt(.975,dfori)                         #critical t-value for p=.05 given original sample size
  
  #generate original studies
  #the way I do this is to get the distribution of possible t-values obtained, I start creating a vector with percentiles between 1-power and 99.9%
    rp=seq(from=1-powerori+.001, to=.999,by=.001)               
  #I then find the noncentral t-values associated with each of those percentiles, effectively the noncentral distribution every 1/1000 point
    t_publish=qt(p=rp,df=dfori,ncp=sqrt(nori/2)*dori)
  #conver that distribution of t-values into effect sizes
    d_publish=(2*t_publish)/sqrt(dfori)
  
  #find nrep for powerrep
  #for each of the possible values obtained, I compute the sample size a replicator would set seeking the claimed level of power
    nrep=mapply(getn,d=d_publish,power=powerrep)  #nrep: sample size of replication
  
  #find actual power
  #the true level of power is different from the published, that's why actual power is not claimed power, i here compute actual power
    real=mapply(getpower,d=dori,n=nrep)  #for every sample size run by the replicator, i compute the effective power
  
    m=mean(real)  #every sample size in the vector is equally likely (by construction, becuase of line 38 of code above) so the simple mean
    cat("Mean power:       ",m,"\n")
    
  
  #return(c(nori,powerori,powerrep,m,p33,p40,p50,p60,p80,p90))
    return(m)  #so the output is a vector with K elements, where K=(1-power)*999, so if 50% power, there are 499 elements
  }


#50% line in Figure 3
  m1=realpower(nori=20,powerori=.5,powerrep=.80)
  m2=realpower(nori=20,powerori=.5,powerrep=.90)
  m3=realpower(nori=20,powerori=.5,powerrep=.95)

#33% line in Figure 3
  m4=realpower(nori=20,powerori=.35,powerrep=.80)
  m5=realpower(nori=20,powerori=.35,powerrep=.90)
  m6=realpower(nori=20,powerori=.35,powerrep=.95)


#Figure itself made in Excel with those values (means)
 #50 power line
  c(m1,m2,m3)
 #33% power line
  c(m4,m5,m6)
  
