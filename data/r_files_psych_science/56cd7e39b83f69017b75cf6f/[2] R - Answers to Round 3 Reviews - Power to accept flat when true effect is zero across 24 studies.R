###################################################################################
#  This program computes how much power p-curve analysis has to conclude a given p-curve
#  is flat, when all 24 of them are null. 
#  Written for Round 3 review for Psych Science of power-posing p-curve paper
#  By Uri Simonsohn, uws@wharton.upenn.edu
#  Last update: 2016 02 24
#
###################################################################################

#Load power library
    library(pwr)
    
#Function 1. Commputes the stouffer test on a set of pp-values
    stouffer=function(pp) sum(qnorm(pp),na.rm=TRUE)/sqrt(sum(!is.na(pp)))
  
#Function 2. Estimate power for flat for 24 null studies
  power.flat=function(n,simtot,seed)
      {
      #n: sample size of each study
      #simtot: number of simulations
      #seed
    
    #1 Compute d33, ncp33 and df for the original sample
        d33=pwr.t.test(n=n,sig.level=0.05,power=1/3,type="two.sample")$d
        ncp33=sqrt(n/2)*d33      
        df=2*n-2                 
      
    #2 Simulate studies 
        set.seed(seed)
        p33=prs=c(length=simtot)            #Empty vectors to store overall p-value for flatt p-curve for each simulation
                                            #also saving it for right-skew to verify simulations are going ok.
        #Loop simulating simk studies
        for (simk in 1:simtot)
          {
          p=runif(n=24)*.05                  # Draw 24 significant p-values under the null, flat.
          t=abs(qt(p/2,df=df))               # Convert them to t stats
          #pp33
            pp33=(1-pt(t,df=df,ncp=ncp33))*3   # Compute pp33 for those t-stats
            p33[simk]=1-pnorm(stouffer(pp33))  # Compute stouffer test for flatter than 33% test and store it
          #Right skew
            pprs=20*p                          # Compute right-skew tests (again, just to verify, should be 5%)
            prs[simk]=1-pnorm(stouffer(pprs))
          }
        
     #3 Report share that are p<.05
          power33=sum(p33<.05)/simtot
          powerrs=sum(prs<.05)/simtot
          cat("\nPower of flatter than 33% test: ",round(power33,4)*100,"%")
          cat("\nPower of right skew: ",round(powerrs,4)*100,"%")
        }
        
        
    #Results depend slightly on sample size, power of p-curve is 86% to accept null with n=20 and 91% with n=500
      power.flat(n=20,simtot=10000,seed=112233)  
      power.flat(n=30,simtot=10000,seed=112233)
      power.flat(n=100,simtot=10000,seed=112233)
      power.flat(n=500,simtot=10000,seed=112233)  
      
      
  
  
  
  

    