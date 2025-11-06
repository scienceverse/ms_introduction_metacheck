#R Code behind Figures S1 in online supplement of  "Small Telescopes"
#Written by Uri Simonsohn (uws@wharton.upenn.edu)
#Last update: 2014 12 15
##################################################################



#Function that computes variance of d estimate
vard=function(d,n) 2/n+((d**2)/(4*n))

#Function used to compute effect size needed for the replication to differ p<.05 from original
pQ=function(p1,d2,n)
  #SYNTAX:
  #p1: p-value of original finding (Example in paper is p1=.025)
  #n:  sample size of original finding (and replication)
  #d2: effect size of the replication, this is the variable we will solve  for
  
{
  #Compute the t-value implied by the entered p-value and sample size
  t1=qt(1-p1/2,df=2*n-2)
  #Convert the t-value to an effect size for the original study
  d1=2*t1/sqrt(2*n)
  #Compute the variance of both studies
  v1=vard(d1,n)
  v2=vard(d2,n)
  #Compute the average effect size
  dbar=(d1/v1+d2/v2)/(1/v1+1/v2)
  #Compute the heterogeneity test
  Q=(d1-dbar)**2/v1+(d2-dbar)**2/v2
  #Compute difference between p-value for that test and .05
  #This allows solving for the d2 that makes this funciton return 0 via uniroot()
  p=1-pchisq(Q,1)-.05
  return(p)
}

#With uniroot we solve for d2:
#Example: if p1=.025 and n=20, we find the d2 that is significantly different from d1
  uniroot(pQ, c(-1,.5), p1=.025,n = 20)$root

#Compute needed d2 in replication for every sample size between 10 and 100, if original had p=.025 or p=.049
  d25=d49=c()  #store results here
  for (i in 10:100)
  {
  #Compute this d2
  #if original was p=.025
    d25.k=uniroot(pQ, c(-1,.5), p1=.025,n = i)$root 
  #if original was p=.049
    d49.k=uniroot(pQ, c(-1,.5), p1=.049,n = i)$root 
  #Archive it with all d2s
    d25=c(d25, d25.k) 
    d49=c(d49, d49.k) 
}

#Now compute how likely each of those d-values are
#Create vectors with sample sizes n=10 to n=100
  n.all=seq(from=10, to=100)
#Convert d to t
  t25=(d25/2)*sqrt(2*n.all)
  t49=(d49/2)*sqrt(2*n.all)

#How likely is this each t under null of d2=0?
  p25=pt(t25,df=2*n.all-2)
  p49=pt(t49,df=2*n.all-2)

#plot results (figure in supplement made in Excel, but similar to this one)
  plot(n.all ,p25,col="red",ylim=c(0,.4),type="l",
     main="Probability Replication is significantly different from original\nwhen true effect is 0",
    xlab="Sample size of original and replication studies \n(per cell)",
    ylab="Percentage of replications that 'fail'")
    lines(n.all,p49,type="l",col="blue",lty=2)
    legend(60,.4,col=c("red","blue"),lty=c(1,2),
           title="P-value of original study",cex=1.2,
            legend=c("p=.025"," p=.049"))
