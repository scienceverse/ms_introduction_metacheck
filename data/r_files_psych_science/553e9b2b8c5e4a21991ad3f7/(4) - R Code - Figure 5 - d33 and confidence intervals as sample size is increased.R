##################################################################
#R Code behind Figures 5 in "Small Telescopes"
#Written by Uri Simonsohn (uws@wharton.upenn.edu)
#Last update: 2014 12 15
##################################################################

#Function 1 - Used by function 2 to find the confidence interval for a given effect size
pgap = function(ncp_est, d, n, p)
{
  #compute d.f.
    df=2*n-2
  #convert d to t
    t=(d*sqrt(df+2))/2
  #compute p-value given noncentrality assumption ncp_est
    return(pt(t,df=df,ncp=ncp_est)-p)
}


#FUnction 2 (Compute 95% ci  for given d and n)
  er=function(d,n)
  {
    ci95=(uniroot(pgap, c(-34, 34), p=.05, d = d, n = n)$root)/sqrt(n/2)
    return(ci95-d)
  }


#Function 3: find d33
  d33=function(n) pwr.t.test(power=1/3,n=n)$d

#########################################################

options(warn=-1)

#Error bars (as used in Excel terminology, meaning, 95% ci, for original studies
  e1=  er(d=.6,n=20)
  e2=  er(d=.6,n=50)
  e3=  er(d=.6,n=100)
  e4= er(d=.6,n=250)
  e5=er(d=.6,n=1000)
  e6=er(d=.6,n=3000)
  es=c(e1,e2,e3,e4,e5,e6)

#d33 for original studies
  d1=d33(n=20)
  d2=d33(n=50)
  d3=d33(n=100)
  d4=d33(n=250)
  d5=d33(n=1000)
  d6=d33(n=3000)
  ds=c(d1,d2,d3,d4,d5,d6)


#Copy paste these into Excel
  ds  #the green lines in the Figure for d33%
  es  #the "error" bars


#NOTE: The excel file with the figures is also publicaly available.

