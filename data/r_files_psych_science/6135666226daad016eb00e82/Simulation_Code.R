##Simulation Code for RR Stage 1: LOOPR Trait-by-Trait Interactions

devtools::install_github("dbaranger/InteractionPoweR")
library(InteractionPoweR) #package for simulation-based power analyses of interaction effects
library(dplyr)

set.seed(412)

#Run simulations varying sample size, predictor intercorrelations, internal consistencies, main effects, and interaction effects

Two_way_detailed<-power_interaction(n.iter = 1000, N=c(700,1000,1500),r.x1.y=c(-.43,-.20,.20,.43),r.x2.y=c(-.43,-.20,.20,.43),r.x1x2.y=c(.05,.10,.15),
                      r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.4,.6,.8),
                  skew.x1 = 0,
                  skew.x2 = 0,
                  skew.y = 0,
                  transform.x1 = "default",
                  transform.x2 = "default",
                  transform.y = "default",
                  adjust.correlations = T,
                  alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

#Collapsing across main effects and predictor correlations
power.summary.r<-Two_way_detailed%>%
  group_by(N, r.x1x2.y, rel.y) %>%
  summarise(
    power_int.mean=round(mean(pwr), digits=2),
    power_int.med=round(median(pwr), digits=2),
    r2=round(mean(x1x2_r2_mean), digits=3)) 

#Get min and max values (Table 2)
min.max_pwr<-Two_way_detailed%>%
  group_by(N, r.x1x2.y, rel.y)%>%
  summarise(
    min.pow=round(min(pwr),digits=2),
    max.pow=round(max(pwr), digits=2)) 

write.csv(power.summary, "power_summaryInt.csv")
write.csv(min.max_pwr, "min_maxInt.csv")
write.csv(Two_way_detailed, "Two_way_results_detailed.csv")

###Specific Outcomes

##Additional power analyses were conducted for the specific outcomes below because the outcome variables
##diverged enough from our simulated parameters in terms of skew, internal consistency values, or sample size  
##to warrant further analyses.   

Job_attain_2way<-power_interaction(n.iter = 1000, N=c(823),r.x1.y=c(-.16,.05),r.x2.y=c(.07),r.x1x2.y=c(.05,.10,.15),
                                    r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.71),
                                    skew.x1 = 0,
                                    skew.x2 = 0,
                                    skew.y = 0,
                                    transform.x1 = "default",
                                    transform.x2 = "default",
                                    transform.y = "default",
                                    adjust.correlations = T,
                                    alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

RiskySex_2way<-power_interaction(n.iter = 1000, N=c(940),r.x1.y=c(-.03,.05),r.x2.y=c(.15),r.x1x2.y=c(.05,.10,.15),
                            r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.51),
                            skew.x1 = 0,
                            skew.x2 = 0,
                            skew.y = 2.09,
                            transform.x1 = "default",
                            transform.x2 = "default",
                            transform.y = "default",
                            adjust.correlations = T,
                            alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

DateVar_2way<-power_interaction(n.iter = 1000, N=c(1017),r.x1.y=c(-.13),r.x2.y=c(-.08,.04),r.x1x2.y=c(.05,.10,.15),
                                 r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.65),
                                 skew.x1 = 0,
                                 skew.x2 = 0,
                                 skew.y = 14.66,
                                 transform.x1 = "default",
                                 transform.x2 = "default",
                                 transform.y = "default",
                                 adjust.correlations = T,
                                 alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

Violence_2way<-power_interaction(n.iter = 1000, N=c(1495),r.x1.y=c(-.16),r.x2.y=c(-.13,.05),r.x1x2.y=c(.05,.10,.15),
                                r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.90),
                                skew.x1 = 0,
                                skew.x2 = 0,
                                skew.y = 6.6,
                                transform.x1 = "default",
                                transform.x2 = "default",
                                transform.y = "default",
                                adjust.correlations = T,
                                alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

Suicide_2way<-power_interaction(n.iter = 1000, N=c(1505),r.x1.y=c(-.17,.02),r.x2.y=c(.28),r.x1x2.y=c(.05,.10,.15),
                                 r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.65),
                                 skew.x1 = 0,
                                 skew.x2 = 0,
                                 skew.y = 1.99,
                                 transform.x1 = "default",
                                 transform.x2 = "default",
                                 transform.y = "default",
                                 adjust.correlations = T,
                                 alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

ExtrSuccess_2way<-power_interaction(n.iter = 1000, N=c(675),r.x1.y=c(-.20),r.x2.y=c(.02,.12),r.x1x2.y=c(.05,.10,.15),
                                r.x1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.54),
                                skew.x1 = 0,
                                skew.x2 = 0,
                                skew.y = 0,
                                transform.x1 = "default",
                                transform.x2 = "default",
                                transform.y = "default",
                                adjust.correlations = T,
                                alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)

RomDiss_2way<-power_interaction(n.iter = 1000, N=c(679),r.x1.y=c(-.04),r.x2.y=c(-.02,.05),r.x1x2.y=c(.05,.10,.15),
                                r.versx1.x2=c(-.49,.28,.52),rel.x1=c(.79),rel.x2=c(.81),rel.y=c(.65),
                                skew.x1 = 0,
                                skew.x2 = 0,
                                skew.y = 0,
                                transform.x1 = "default",
                                transform.x2 = "default",
                                transform.y = "default",
                                adjust.correlations = T,
                                alpha=0.05,q=2,cl=6,ss.IQR=1.5,detailed_results=T,full_simulation=FALSE)



