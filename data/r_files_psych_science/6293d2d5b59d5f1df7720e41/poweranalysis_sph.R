# use mixedpower to simulate the result
library(mixedpower)
library(BayesFactor)
library(lme4)
# first load the pilot data and do some preproces
combineData<-read.csv(file='../data/pilotData_triangle_combine_clean.csv')
combineData_rescaled<-combineData;
combineData_rescaled$inboundTurnInDeg<-combineData$inboundTurnInDeg/180*pi # let's convert degree to radian to avoid the large eigenvalue error for lmer function
combineData_rescaled$idealPlane_inboundTurn<-combineData$idealPlane_inboundTurn/180*pi# let's convert degree to radian to avoid the large eigenvalue error for lmer function
combineData_rescaled$idealSph_inboundTurn<-combineData$idealSph_inboundTurn/180*pi# let's convert degree to radian to avoid the large eigenvalue error for lmer function
combineData_rescaled$subInt<-factor(combineData$sub)
combineData_rescaled$sub<-factor(combineData$sub)
combineData_rescaled$sub0<-factor(combineData$sub) # a trick to fix a known issue of overparametrization for continuous predictor in lmBF https://github.com/richarddmorey/BayesFactor/issues/141
combineData_rescaled$env<-factor(combineData$env)
combineData_sphOnly<-combineData_rescaled[combineData_rescaled$env=='sph',]

lme_idealPlaneTurn_pilot<-lmer(inboundTurnInDeg~idealPlane_inboundTurn+(1+idealPlane_inboundTurn|sub),data=combineData_sphOnly)
lme_idealSphTurn_pilot<-lmer(inboundTurnInDeg~idealSph_inboundTurn+(1+idealSph_inboundTurn|sub),data=combineData_sphOnly)
lme_idealMixedTurn_pilot<-lmer(inboundTurnInDeg~idealPlane_inboundTurn+idealSph_inboundTurn+(1+idealPlane_inboundTurn+idealSph_inboundTurn|sub),data=combineData_sphOnly)

lme_idealPlaneTurnSharedRandom_pilot<-lmer(inboundTurnInDeg~idealPlane_inboundTurn+(1+idealPlane_inboundTurn+idealSph_inboundTurn|sub),data=combineData_sphOnly)

lme_idealPlaneDist_pilot<-lmer(inboundDistInDeg~idealPlane_inboundDist+(1+idealPlane_inboundDist|sub),data=combineData_sphOnly)
lme_idealSphDist_pilot<-lmer(inboundDistInDeg~idealSph_inboundDist+(1+idealSph_inboundDist|sub),data=combineData_sphOnly)
lme_idealMixedDist_pilot<-lmer(inboundDistInDeg~idealPlane_inboundDist+idealSph_inboundDist+(1+idealPlane_inboundDist+idealSph_inboundDist|sub),data=combineData_sphOnly)

lm_bayes_planeTurn1<-lmBF(inboundTurnInDeg~sub+idealPlane_inboundTurn+idealPlane_inboundTurn:sub0,data=combineData_sphOnly, whichRandom=c('sub') ) 
lm_bayes_planeTurn2<-lmBF(inboundTurnInDeg~sub+idealPlane_inboundTurn:sub0,data=combineData_sphOnly, whichRandom=c('sub') ) 
lm_bayes_planeTurn3<-lmBF(inboundTurnInDeg~sub+idealPlane_inboundTurn:sub,data=combineData_sphOnly, whichRandom=c('sub') )# this is almost identical sub+idealPlane_inboundTurn+idealPlane_inboundTurn:sub0
lm_bayes_planeTurn4<-lmBF(inboundTurnInDeg~sub,data=combineData_sphOnly, whichRandom=c('sub') )# this is almost identical sub+idealPlane_inboundTurn+idealPlane_inboundTurn:sub0

lm_bayes_mixedTurn<-lmBF(inboundTurnInDeg~sub+idealPlane_inboundTurn:sub+idealSph_inboundTurn:sub, data=combineData_sphOnly, whichRandom='sub')

nSim=1000;
bf_simPlane_PlaneVsSph=rep(0,nSim)
bic_simDataTurn_truePlane<-matrix(0,nSim,5)
bic_simDataTurn_trueSph<-matrix(0,nSim,5)
bic_simDataDist_truePlane<-matrix(0,nSim,5)
bic_simDataDist_trueSph<-matrix(0,nSim,5)

Sys.time()
for (nSub in seq(8,20,by=1)){
  for (i in 1:nSim){
    print(i)
    simDataTurn_truePlane<-simulateDataset(n_want=nSub,data=combineData_sphOnly, model=lme_idealPlaneTurn_pilot, simvar="sub",fixed_effects=c("idealPlane_inboundTurn","idealSph_inboundTurn"))
    simDataTurn_trueSph<-simulateDataset(n_want=nSub,data=combineData_sphOnly, model=lme_idealSphTurn_pilot, simvar="sub",fixed_effects=c("idealPlane_inboundTurn","idealSph_inboundTurn"))
    simDataDist_truePlane<-simulateDataset(n_want=nSub,data=combineData_sphOnly, model=lme_idealPlaneDist_pilot, simvar="sub",fixed_effects=c("idealPlane_inboundDist","idealSph_inboundDist"))
    simDataDist_trueSph<-simulateDataset(n_want=nSub,data=combineData_sphOnly, model=lme_idealSphDist_pilot, simvar="sub",fixed_effects=c("idealPlane_inboundDist","idealSph_inboundDist"))
    

    bic_simDataTurn_truePlane[i,1]<-BIC(lmer(inboundTurnInDeg~idealPlane_inboundTurn+(1+idealPlane_inboundTurn|sub),data=simDataTurn_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_truePlane[i,2]<-BIC(lmer(inboundTurnInDeg~idealSph_inboundTurn+(1+idealSph_inboundTurn|sub),data=simDataTurn_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_truePlane[i,3]<-BIC(lmer(inboundTurnInDeg~1+(idealPlane_inboundTurn|sub),data=simDataTurn_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_truePlane[i,4]<-BIC(lmer(inboundTurnInDeg~1+(idealSph_inboundTurn|sub),data=simDataTurn_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_truePlane[i,5]<-BIC(lmer(inboundTurnInDeg~idealPlane_inboundTurn+idealSph_inboundTurn+(1+idealPlane_inboundTurn+idealSph_inboundTurn|sub),data=simDataTurn_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
      
    bic_simDataTurn_trueSph[i,1]<-BIC(lmer(inboundTurnInDeg~idealPlane_inboundTurn+(1+idealPlane_inboundTurn|sub),data=simDataTurn_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_trueSph[i,2]<-BIC(lmer(inboundTurnInDeg~idealSph_inboundTurn+(1+idealSph_inboundTurn|sub),data=simDataTurn_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_trueSph[i,3]<-BIC(lmer(inboundTurnInDeg~1+(idealPlane_inboundTurn|sub),data=simDataTurn_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_trueSph[i,4]<-BIC(lmer(inboundTurnInDeg~1+(idealSph_inboundTurn|sub),data=simDataTurn_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataTurn_trueSph[i,5]<-BIC(lmer(inboundTurnInDeg~idealPlane_inboundTurn+idealSph_inboundTurn+(1+idealPlane_inboundTurn+idealSph_inboundTurn|sub),data=simDataTurn_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    
    bic_simDataDist_truePlane[i,1]<-BIC(lmer(inboundDistInDeg~idealPlane_inboundDist+(1+idealPlane_inboundDist|sub),data=simDataDist_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_truePlane[i,2]<-BIC(lmer(inboundDistInDeg~idealSph_inboundDist+(1+idealSph_inboundDist|sub),data=simDataDist_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_truePlane[i,3]<-BIC(lmer(inboundDistInDeg~1+(idealPlane_inboundDist|sub),data=simDataDist_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_truePlane[i,4]<-BIC(lmer(inboundDistInDeg~1+(idealSph_inboundDist|sub),data=simDataDist_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_truePlane[i,5]<-BIC(lmer(inboundDistInDeg~idealPlane_inboundDist+idealSph_inboundDist+(1+idealPlane_inboundDist+idealSph_inboundDist|sub),data=simDataDist_truePlane, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    
    bic_simDataDist_trueSph[i,1]<-BIC(lmer(inboundDistInDeg~idealPlane_inboundDist+(1+idealPlane_inboundDist|sub),data=simDataDist_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_trueSph[i,2]<-BIC(lmer(inboundDistInDeg~idealSph_inboundDist+(1+idealSph_inboundDist|sub),data=simDataDist_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_trueSph[i,3]<-BIC(lmer(inboundDistInDeg~1+(idealPlane_inboundDist|sub),data=simDataDist_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_trueSph[i,4]<-BIC(lmer(inboundDistInDeg~1+(idealSph_inboundDist|sub),data=simDataDist_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    bic_simDataDist_trueSph[i,5]<-BIC(lmer(inboundDistInDeg~idealPlane_inboundDist+idealSph_inboundDist+(1+idealPlane_inboundDist+idealSph_inboundDist|sub),data=simDataDist_trueSph, REML=FALSE,control=lmerControl(optimizer="Nelder_Mead")))
    
           #    lm1_Bayes=lmBF(inboundTurnInDeg~sub+idealPlane_inboundTurn:sub,data=simDataTurn_truePlane, whichRandom=c('sub') )
#    lm2_Bayes=lmBF(inboundTurnInDeg~sub+idealSph_inboundTurn:sub,data=simDataTurn_truePlane, whichRandom=c('sub') )
#    bf_simPlane_PlaneVsSph[i]<-lm1_Bayes/lm2_Bayes;

  }
  bic_turn_truePlane_PlaneVsSph[nSub,]<-bic_simDataTurn_truePlane[,1]-bic_simDataTurn_truePlane[,2]
  bic_turn_trueSph_SphVsPlane[nSub,]<-bic_simDataTurn_trueSph[,2]-bic_simDataTurn_trueSph[,1]
  bic_Dist_truePlane_PlaneVsSph[nSub,]<-bic_simDataDist_truePlane[,1]-bic_simDataDist_truePlane[,2]
  bic_Dist_trueSph_SphVsPlane[nSub,]<-bic_simDataDist_trueSph[,2]-bic_simDataDist_trueSph[,1]
  
  bic_turn_trueSph_SphVsPlane[nSub,]<-bic_simDataTurn_trueSph[,2]-bic_simDataTurn_trueSph[,1]
  bic_turn_trueSph_SphVsReduced[nSub,]<-bic_simDataTurn_trueSph[,2]-bic_simDataTurn_trueSph[,4]
  bic_turn_trueSph_SphVsMixed[nSub,]<-bic_simDataTurn_trueSph[,2]-bic_simDataTurn_trueSph[,5]
  
  bic_Dist_truePlane_PlaneVsSph[nSub,]<-bic_simDataDist_truePlane[,1]-bic_simDataDist_truePlane[,2]
  bic_Dist_truePlane_PlaneVsReduced[nSub,]<-bic_simDataDist_truePlane[,1]-bic_simDataDist_truePlane[,3]
  bic_Dist_truePlane_PlaneVsMixed[nSub,]<-bic_simDataDist_truePlane[,1]-bic_simDataDist_truePlane[,5]
  
  bic_Dist_trueSph_SphVsPlane[nSub,]<-bic_simDataDist_trueSph[,2]-bic_simDataDist_trueSph[,1]
  bic_Dist_trueSph_SphVsReduced[nSub,]<-bic_simDataDist_trueSph[,2]-bic_simDataDist_trueSph[,4]
  bic_Dist_trueSph_SphVsMixed[nSub,]<-bic_simDataDist_trueSph[,2]-bic_simDataDist_trueSph[,5]
  
}


Sys.time()

printBICrange<-function(x){
  x_bic<-x
  x<-exp(-x/2) #BIC to BF
  sprintf("BIC=[%.1f,%.1f],median=%.1f,n(BF>10)=%d, n(10>BF>3)=%d, n(3>BF>1/3)=%d, n(1/3>BF>1/10)=%d, n(1/10>BF)=%d",min(x_bic),max(x_bic),median(x_bic),sum(x>10), sum(x>3 & x<10), sum(x>1/3 & x<3), sum(x>1/10&x<1/3), sum(x<1/10))
}

nSub=10
sprintf("when nSub=%d",nSub)
sprintf("turn simPlane: BIC Plane vs Sph,%s",printBICrange(bic_turn_truePlane_PlaneVsSph[nSub,]))
sprintf("turn simPlane: BIC Plane vs Reduced,%s",printBICrange(bic_turn_truePlane_PlaneVsReduced[nSub,]))
sprintf("turn simPlane: BIC Plane vs Mixed,%s",printBICrange(bic_turn_truePlane_PlaneVsMixed[nSub,]))


sprintf("turn simSph: BIC Sph vs Plane,%s",printBICrange(bic_turn_trueSph_SphVsPlane[nSub,]))
sprintf("turn simSph: BIC Sph vs Reduced,%s",printBICrange(bic_turn_trueSph_SphVsReduced[nSub,]))
sprintf("turn simSph: BIC Sph vs Mixed,%s",printBICrange(bic_turn_trueSph_SphVsMixed[nSub,]))

sprintf("Dist simPlane: BIC Plane vs Sph,%s",printBICrange(bic_Dist_truePlane_PlaneVsSph[nSub,]))
sprintf("Dist simPlane: BIC Plane vs Reduced,%s",printBICrange(bic_Dist_truePlane_PlaneVsReduced[nSub,]))
sprintf("Dist simPlane: BIC Plane vs Mixed,%s",printBICrange(bic_Dist_truePlane_PlaneVsMixed[nSub,]))

sprintf("Dist simSph: BIC Sph vs Plane,%s",printBICrange(bic_Dist_trueSph_SphVsPlane[nSub,]))
sprintf("Dist simSph: BIC Sph vs Reduced,%s",printBICrange(bic_Dist_trueSph_SphVsReduced[nSub,]))
sprintf("Dist simSph: BIC Sph vs Mixed,%s",printBICrange(bic_Dist_trueSph_SphVsMixed[nSub,]))
