###################################################################################
#  This program computes how much power a p-curve based on  24 studies has
#  if k of them have `powerk`, and 24-k of them are  null.
#  Written for Round 3 review for Psych Science of power-posing p-curve paper
#  By Uri Simonsohn, uws@wharton.upenn.edu
#  Last update: 2016 02 24
#
#  Note: as elsewhere, the simulations use n=20 throughout. This is without loss of 
#      generality. Because the analyses assume a level of power, sample size
#      affects minimally, only to the degree of variability of expected p-curves
#      for a given level of power but different sample sizes. The consequences
#      are second order.
###################################################################################


#Load power library
    library(pwr)
#Function 1. Commputes the stouffer test on a set of pp-values
    stouffer=function(pp) sum(qnorm(pp),na.rm=TRUE)/sqrt(sum(!is.na(pp)))

#Function 2. Computes power of p-curve, for right skew, if k of 24 studies have powerk
    rspower=function(k,powerk,simtot)
      {
      n=20  #See note above
    #Draw large sets of significant p-values
      #True effect size
          dk=pwr.t.test(n=20,power=powerk)$d
      #Draw the ts
          t=rt(n=10000/powerk,ncp=sqrt(n/2)*dk,df=2*n-2)
      #Make p-values
            p=2*(1-pt(t,df=2*n-2))
      #Keep the significant ones
            p.sig=p[p<.05]      #will draw from these in the simualtions
      #Empty vector with restuls to be stored in
            rs=c(length=simtot)
      #Loop
        for (i in 1:simtot)
            {
            pk=sample(p.sig,k)              #Draw k significant real ones
            pk=c(pk,runif(n=24-k)*.05)      #Add 24-k significant uniform 0,.05 ones
              pp=20*pk                        #pp-values
              Z=stouffer(pp)                  #combine them
              rs[i]=pnorm(Z)                  #Store p-value from full p-curve test
              }
     return(sum(rs<.05)/simtot) 
    }  
          

#Function 3. Finds the number of studies with powerk, out of 24,m needed for p-curve to have powerk overall
      getk=function(powerk)
      {
        for (i in 1:23)
          {
          power.i=rspower(simtot=10000,k=i,powerk=powerk)  
          if (power.i>powerk) break
         }
     return(i)
    }


################## 
#ANALYSES
#1. Find power of p-curve if all studies have a given level of power
#
    set.seed(123321)
    rspower(simtot=10000,k=24,powerk=.8)  #If 24 of the 24 are powered to 80%, p-curve has >99.99% power
    rspower(simtot=10000,k=24,powerk=.5)  #If 24 of the 24 are powered to 50%, p-curve has >98.5% power
    rspower(simtot=10000,k=24,powerk=1/3) #if 24 of the 24 are powered to 1/3, p-curve has 89% power.
  
#2 If k studies are real and have power=powerk (constant) 
#     and 24-k are d=0, how big isk so that p-curve's power for right-skew is >=powerk?
    
    set.seed(112233)
    getk(powerk=.25)  #if 10 studies have power=25%, and 14 studies have d=0, p-curved has 25% power
    getk(powerk=.33)  #if 10 studies have power=25%, and 14 studies have d=0, p-curved has 33% power
    getk(powerk=.5)   #'''
    getk(powerk=.8)   #if 9 studies have power=80%, and 15 studies have d=0, p-curved has 80% power
    getk(powerk=.99)
