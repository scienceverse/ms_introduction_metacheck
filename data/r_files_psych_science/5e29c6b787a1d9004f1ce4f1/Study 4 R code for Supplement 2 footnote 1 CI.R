#R code to reproduce analysis in Supplement 4 footnote 1 in "People Reject Algorithms in Uncertain Decision Domains Because They Have Diminishing Sensitivity to Forecasting Error"
#This version: 2020 1 18
#########################################################################################################

  library(car)

  #1. Read the data
    a=read.csv("...[file location here].../Study 4 Data for R.csv")

  #2. Create vectors
    y=a$ChoseModel
    m=a$ModMoreNearPerfect
    c1=a$d1
    c2=a$d2
    
  
  #3 Analyze observed data
    #3.1 Run regression with and without mediator
    
      lm1=lm(y~c1+c2-1)
      
      lm2=lm(y~c1+c2+m-1)
  
    #3.3 F test on equality of three means
      F1.obs=linearHypothesis(lm1, c("c1 = c2"))$F[2]
      F2.obs=linearHypothesis(lm2, c("c1 = c2"))$F[2]
      F1.obs
      F2.obs
      
    #3.4 DIfference
      dF.obs=F1.obs-F2.obs
      
  #Bootstrap
    simtot=1000
    set.seed(123)
    res=matrix(nrow=simtot,ncol=1)
    for (simk in 1:simtot)
      {
      #Draw bootstrapped sample
          draw=sample(length(y),replace=T)    #Which random observations to select, with replacement
          
          y.b=y[draw]
          c1.b=c1[draw]
          c2.b=c2[draw]
          m.b=m[draw]
          
          
      #Repeat calculations
          #4.1 Run regression with and without mediator
            
          lm1.b=lm(y.b~c1.b+c2.b-1)
          lm2.b=lm(y.b~c1.b+c2.b+m.b-1)

            
          #4.2 F test on equality of three means
            
            F1.b=linearHypothesis(lm1.b, c("c1.b = c2.b"))$F[2]
            F2.b=linearHypothesis(lm2.b, c("c1.b = c2.b"))$F[2]
            
          
          #Compute difference
            dF.b=F1.b-F2.b
            
            res[simk,1]=c(dF.b)
            if (simk%%100==0) cat("...",simk)
          
    }
    #2.5th percentile    
      i025=apply(res,2,quantile,.025)  #2.5th percentile of each difference
      i975=apply(res,2,quantile,.975)  #97.5th percentile of each difference
      
    #RESULTS
        names=c("Joint test")
        
      #Point estimates for change in F-value
        point=c(dF.obs)
        rbind(names,point)
        
      #95% CI
        results=rbind(i025,i975)
        colnames(results)=names
        results


      