#Evaluating our replication results from different recent perspectives
require(compute.es)
require(pwr)

#########################################################################
#Safeguard-Power-Analaysis APPROACH following Perugini et al. (2014, PPS)
#########################################################################
#Correll's original effect size
fes(5.52,47,24) #d=.59
#but for 80% protection, need to calculate 60% C.I.
fes(5.52,47,24,level=60) #d=.59 CI60%=[.37,.81]
#so d_s_lower_bound=.37
#now calculate required N_s to achieve 80% power to detect d_s=.37
pwr.t.test(d=.37,power=0.80,sig.level=0.05,type="two.sample",alternative="two.sided") 
#N=232, so we're definitely safeguarded at 80% for our combined sample of N=296

#########################################################
#Minimum-effect-size APPROACH (LeBel, 2014) (e.g., d_min)
#########################################################
#Correll's orginal sample
pwr.t.test(n=35,d=NULL,power=0.95,sig.level=0.05,type="two.sample",alternative="two.sided") #n=35/cell!!
#so minimum effect size Correll could reliably (95% power) detect with N=71 is d=.87

#Our individual samples (N=148)
pwr.t.test(n=75,d=NULL,power=0.95,sig.level=0.05,type="two.sample",alternative="two.sided") 
#In our samples, we could reliably (95% power) have detected a d=.59 if it existed

#Our combined sample (N=296)
pwr.t.test(n=148,d=NULL,power=0.95,sig.level=0.05,type="two.sample",alternative="two.sided") 
#In our combined sample, we could reliably (95% power) have detected a d=.42 if it existed

#this is a much simpler and intuitively understandable approach to making sense of our replication results!
#CONCLUSION: we had the sensitivity (at 95% level) to detect an effect as small as d=.42
#An effect smaller than this *COULD* exist, but you would need a much larger N to reliably detect it!! 
#(e.g., an N=600 to reliably detect a d=.30)!!

###################################################################
#SIMONSOHN'S SMALL-TELESCOPE APPROACH (Simonsohn, 2013, e.g., d33%)
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2259879
###################################################################

#d33% for Correll's (2008) Study 2 sample size of N=71
pwr.t.test(n=71,d=NULL,power=0.33,sig.level=0.05,type="two.sample",alternative="two.sided") 
#d33%=.257  Hence, Correll had 33% power to detect d=.257


#Now we test whether our combined-sample d=.03 is statistically significantly smaller than this d=.257
#first compute ncp according to Simonsohn's equation 2
#ncp = sqrt(296/2)*.257 
#or more precisely because of unequal cell sizes
ncp = sqrt((198*98)/(198+98))*.257 #with the .257 being the d33%=.257 from above!
#ncp = 2.0808
#then this next line to calculate p-value
pt(.277,df=294,ncp=2.0808)
#p<.0356
