### ============================================================
## This file contains R code that was used to analyze the data for
##
## Salzar Kämpf, M., Liebermann, H., Kerschreiter, R., Krause, S., 
## Nestler, S. & Schmukle, S.C.(2017).
## Disentangling the Sources of Mimicry: Social Relations Analyses 
## of the Link Between Mimicry and Liking.
## This script file is licensed under a CC-BY 4.0 license. 
## For a human-readable summary of that license, 
## see http://creativecommons.org/licenses/by/4.0/
##
## (c) 2016 Steffen Nestler (steffen.nestler@uni-leipzig.de) 
## 
## If you have any questions, please mail Steffen Nestler.
##
## ============================================================

LMM.SRM_MultivariatMultipleGroups <- function(starts,y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20) { 

   # 1.  Compute matrices
   # 1a. Groups stable and instable
   gs.M <- rbind(cbind(tcrossprod(Zgs.1),tcrossprod(Zgs.1,Zgs.2)),cbind(tcrossprod(Zgs.2,Zgs.1),tcrossprod(Zgs.2)))
   gi.M <- rbind(cbind(tcrossprod(Zgi.1),tcrossprod(Zgi.1,Zgi.2)),cbind(tcrossprod(Zgi.2,Zgi.1),tcrossprod(Zgi.2)))
   
   # 1b. Individuals stable and instable
   as.M  <- rbind(cbind(tcrossprod(Z1s.1),tcrossprod(Z1s.1,Z2s.2)),cbind(tcrossprod(Z2s.2,Z1s.1),tcrossprod(Z2s.2)))
   ps.M  <- rbind(cbind(tcrossprod(Z2s.1),tcrossprod(Z2s.1,Z1s.2)),cbind(tcrossprod(Z1s.2,Z2s.1),tcrossprod(Z1s.2)))
   aps.M <- rbind(cbind(tcrossprod(Z2s.1,Z1s.1)+tcrossprod(Z1s.1,Z2s.1),tcrossprod(Z2s.1,Z2s.2)+tcrossprod(Z1s.1,Z1s.2)),
                 cbind(tcrossprod(Z1s.2,Z1s.1)+tcrossprod(Z2s.2,Z2s.1),tcrossprod(Z1s.2,Z2s.2)+tcrossprod(Z2s.2,Z1s.2)))
   
   ai.M  <- rbind(cbind(tcrossprod(Z1i.1),tcrossprod(Z1i.1,Z2i.2)),cbind(tcrossprod(Z2i.2,Z1i.1),tcrossprod(Z2i.2)))
   pi.M  <- rbind(cbind(tcrossprod(Z2i.1),tcrossprod(Z2i.1,Z1i.2)),cbind(tcrossprod(Z1i.2,Z2i.1),tcrossprod(Z1i.2)))
   api.M <- rbind(cbind(tcrossprod(Z2i.1,Z1i.1)+tcrossprod(Z1i.1,Z2i.1),tcrossprod(Z2i.1,Z2i.2)+tcrossprod(Z1i.1,Z1i.2)),
                 cbind(tcrossprod(Z1i.2,Z1i.1)+tcrossprod(Z2i.2,Z2i.1),tcrossprod(Z1i.2,Z2i.2)+tcrossprod(Z2i.2,Z1i.2)))
   
   # 1c. Dyads
   r.M  <- adiag(tcrossprod(Z3s.1),tcrossprod(Z3s.2))
   rr.M <- rbind(cbind(diag(0,ncol(tcrossprod(Z3s.1))),tcrossprod(Z3s.1,Z3s.2)),cbind(tcrossprod(Z3s.2,Z3s.1),diag(0,ncol(tcrossprod(Z3s.2)))))
   
   # 1d. Errors     
   e.M  <- diag(1,length(y))
   ee.M <- matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(r.M)/2) # this has to be changed in the future
     
   mlist <- list(gs.M,gi.M,as.M,ps.M,aps.M,ai.M,pi.M,api.M,r.M,rr.M,e.M,ee.M)
   sp.mlist <- lapply(mlist,function(x) x <- Matrix(x,sparse=TRUE))
    
   # 2. Define starting values and start number for iterations
   Var <- starts        
   l <- 1
   dif <- 100
   
   # 3. Fisher scoring  for variance parameters
   while (dif > difcrit & l <= maxit) {
	
     l  <- l + 1  
     V <- Reduce('+',Map('*',mlist,Var)) 
     Vi <- solve(V)  # this is the slowest part of the algorithm
     P <- Vi - (Vi%*%X)%*%(solve(crossprod(X,Vi)%*%X))%*%(crossprod(X,Vi))   
     
     # Score vector
     sc <- matrix(0, ncol=1, nrow=length(Var))
     sc <- as.matrix(unlist(lapply(mlist,function(x) -sum(P*t(x))+((crossprod(y,P)%*%x)%*%(P%*%y)))),ncol=1,nrow=length(Var))

     # Expected information matrix
     In  <- matrix(0, ncol=length(starts), nrow=length(starts))
     for (i in 1:length(Var)) {
         pa.i <- P%*%sp.mlist[[i]]          
         for (j in i:length(Var)) {
            pb.j <- P%*%sp.mlist[[j]] 
            In[i,j] <- sum(pa.i*t(pb.j))                  
          } 
     }
     In <- In + t(In) - diag(diag(In)) 
     
     # Compute new variance-covariance estimates   
     newVar <- Var + (solve(In)%*%sc)
     dif <- (t(Var-newVar)%*%(Var-newVar))/(t(Var)%*%Var) 
     Var <- newVar

   }
           
   # 4. Get standard errors of the variance parameters
   # by computing the observed information matrix
   # a. Compute fixed effects
   b <- solve(t(X)%*%Vi%*%X)%*%t(X)%*%(Vi%*%y)
   
   # b. Observed information matrix
   In  <- matrix(0, ncol=length(starts), nrow=length(starts))
   for (i in 1:length(Var)) {      
       pa.i <- P%*%sp.mlist[[i]]       
       for (j in i:length(Var)) {       
            pa.j <- P%*%sp.mlist[[j]]
            In[i,j] <- ((t(y-X%*%b)%*%Vi)%*%sp.mlist[[i]]%*%P%*%sp.mlist[[j]]%*%(Vi%*%(y-X%*%b))-0.5*(sum(pa.i*t(pa.j))))[1]                  
       } 
   }
   In <- In + t(In) - diag(diag(In)) 
   s.In <- solve(In) 
   ses <- sqrt(diag(s.In))
   if (min(eigen(In)$values) <= 0) { error <- 1; } else { error <- 0; }
   
   # 5. Compute standard errors for the fixed effects
   # 5a. Compute standard error of fixed effects and t-values
   sigma.b <- solve(t(X)%*%Vi%*%X)
   ses.b <- sqrt(diag(sigma.b))
   ts <- b/ses.b

   # 5b. Satterthwaite approximation of dfs
   cma <- diag(1,ncol(X)) 
   dfs <- matrix(0,nrow=ncol(X),ncol=1)
   for (k in 1:ncol(cma)) {
      cs <- cma[,k]
      # Gradient
      gr  <- as.matrix(unlist(lapply(mlist,function(x) (t(cs)%*%sigma.b)%*%t(X)%*%Vi%*%x%*%Vi%*%X%*%(sigma.b%*%cs))),ncol=1,nrow=length(Var))
      # Dfs
      dfs[k,1] <- (2*(t(cs)%*%sigma.b%*%cs)^2)/(t(gr)%*%solve(In)%*%gr)
   } 
   pss <- 2*pt(-abs(ts),df=dfs)
   
   # 6.   Compute random effects for stable actor, partner, and relationship effects
   Daps <- adiag(kronecker(Var[3],diag(1,ncol(Z1s.1))),kronecker(Var[4],diag(1,ncol(Z2s.1))))+Var[5]*matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(Z1s.1))
   Dres <- kronecker(Var[9],diag(1,ncol(Z3s.1)+ncol(Z3s.2)))+Var[10]*matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(Z3s.1))
   Dt  <- adiag(Daps,Dres)
   bt  <- Dt%*%t(cbind(rbind(Z1s.1,Z2s.2),rbind(Z2s.1,Z1s.2),adiag(Z3s.1,Z3s.2)))%*%Vi%*%(y-X%*%b)  
   
   ids.aps <- ddply(data,.(Group),.fun=function(x) { a <- sort(unique(x$Actor)); p <- sort(unique(x$Partner)); out <- data.frame(id.a = a,id.p = p); })
   ids.dy1 <- ddply(data.1,.(Group),.fun=function(x) { x <- subset(x,x$Occasion==1); out <- data.frame(Actor = x$Actor, Partner = x$Partner, Dyad = x$Dyad); })
   ids.dy2 <- ddply(data.2,.(Group),.fun=function(x) { x <- subset(x,x$Occasion==1); out <- data.frame(Actor = x$Actor, Partner = x$Partner, Dyad = x$Dyad); })
   ids.dyd <- rbind(ids.dy1,ids.dy2)
   ran.a <- data.frame(Group=ids.aps[,1],Actor=ids.aps$id.a,Effects=bt[1:ncol(Z1s.1)])
   ran.p <- data.frame(Group=ids.aps[,1],Partner=ids.aps$id.p,Effects=bt[(ncol(Z1s.1)+1):(ncol(Z1s.1)+ncol(Z2s.1))])
   ran.d <- data.frame(Group=ids.dyd[,1],Actor=ids.dyd$Actor,Partner=ids.dyd$Partner,Dyad=ids.dyd$Dyad,Effects=bt[(ncol(Z1s.1)+ncol(Z2s.1)+1):length(bt)])
   
   # 7. Standardized variance and covariance estimates following SOREMO's and TripleR's output
   Vark <- c(Var[3],Var[4],Var[9],sum(Var[c(6,7,11)]),Var[5],Var[10])
   sVar <- Vark
   sVar[5] <- sVar[5]/sqrt(sVar[1]*sVar[2])
   sVar[6] <- sVar[6]/sVar[3]
   sVar[c(1,2,3,4)] <- (1/sum(sVar[c(1,2,3,4)]))*sVar[c(1,2,3,4)] 
   

   # 8. Export results
   OutRan <- data.frame(Parameter=c("Group variance stable","Group variance unstable",
                                    "Actor variance stable","Parner variance stable","Actor-Partner covariance stable",
                                    "Actor variance unstable","Parner variance unstable","Actor-Partner covariance unstable",
                                    "Relationship variance","Relationship covariance",
                                    "Error variance","Error covariance"),
                        Estimate=round(Var,3),
                        SE=round(ses,3))
   OutTripleR <- data.frame(Parameter=c("Actor variance","Parner variance","Relationship variance","Error variance",
                                        "Actor-partner covariance","Relationship covariance"),
                            Estimate=round(Vark,3),
                            SE=round(c(ses[3],ses[4],ses[9],NA,ses[5],ses[10]),3),
                            Standardized=round(sVar,3))
   
   OutFix <- data.frame(Effects=round(b,4),Standard_errors=round(ses.b,4),ts=round(ts,4),
                        app_dfs=round(dfs,4),app_ps=round(pss,4))
   OutAlg <- data.frame(NoOfIterations=l,ConvergeCriterion=dif,Error=error)
   OutEff <- list(ran.a,ran.p,ran.d)
   
   result=list(OutAlg,OutFix,OutRan,OutTripleR,OutEff)
   names(result)=c("Information_algorithm","Fixed_effects","Variance_Random_effects_(LMM)","Variance_Random_effects_(TripleR/SOREMO)","Raneffects")
   return(result) 
   
} # End of function

## %%%%%%%%%%%%%%%%%%
## 
##   Main part
## 
## %%%%%%%%%%%%%%%%%%

library(Matrix)
library(plyr)
library(magic)

# 1. Import data

data <- read.table("DataPredictionModels.dat", header=T)
no.oc <- length(unique(data$Occasion)) # number of measurements

# The next three steps are preparations for the algorithm; do not change any code here
# 2. Compute indicators for the instable effects

data$gi <- no.oc*(data$Group-1)+data$Occasion
data$Ai <- no.oc*(data$Actor-1)+data$Occasion
data$Pi <- no.oc*(data$Partner-1)+data$Occasion

# 3. Sort the data according to the matrix formulation of the SRM  (see Nestler, in press)

data <- data[order(data$Group,data$Actor,data$Partner,data$Dyad,data$Occasion),]
data.1 <- subset(data,data$Actor < data$Partner)
data.1 <- data.1[order(data.1$Group,data.1$Actor,data.1$Partner,data.1$Dyad,data.1$Occasion),]
data.2 <- subset(data,data$Actor > data$Partner)
data.2 <- data.2[order(data.2$Group,data.2$Dyad,data.2$Occasion),]
data   <- rbind(data.1,data.2)                                                

# 4. Compute design matrices for the random effects

Zgs.1 <- with(data.1, outer(Group, unique(Group), '==')*1)
Zgs.2 <- with(data.2, outer(Group, unique(Group), '==')*1)

Zgi.1 <- with(data.1, outer(gi, unique(gi), '==')*1)
Zgi.2 <- with(data.2, outer(gi, unique(gi), '==')*1)
groups <- unique(data.1$Group)
for (i in 1:length(groups)) {    
   g1 <- subset(data.1,data.1$Group==groups[i])
   g2 <- subset(data.2,data.2$Group==groups[i])
   mZ1s.1 <- with(g1, outer(Actor, sort(unique(data[data$Group==groups[i],]$Actor)), '==')*1)
   mZ2s.1 <- with(g1, outer(Partner, sort(unique(data[data$Group==groups[i],]$Partner)), '==')*1)
   mZ2s.2 <- with(g2, outer(Actor, sort(unique(data[data$Group==groups[i],]$Actor)), '==')*1)
   mZ1s.2 <- with(g2, outer(Partner, sort(unique(data[data$Group==groups[i],]$Partner)), '==')*1)
   mZ3s.1 <- with(g1, outer(Dyad, sort(unique(data[data$Group==groups[i],]$Dyad)), '==')*1)
   mZ3s.2 <- with(g2, outer(Dyad, sort(unique(data[data$Group==groups[i],]$Dyad)), '==')*1)
   mZ1i.1 <- with(g1, outer(Ai, sort(unique(data[data$Group==groups[i],]$Ai)), '==')*1)
   mZ2i.1 <- with(g1, outer(Pi, sort(unique(data[data$Group==groups[i],]$Pi)), '==')*1)
   mZ2i.2 <- with(g2, outer(Ai, sort(unique(data[data$Group==groups[i],]$Ai)), '==')*1)
   mZ1i.2 <- with(g2, outer(Pi, sort(unique(data[data$Group==groups[i],]$Pi)), '==')*1)
   if (i == 1) { Z1s.1 <- mZ1s.1; Z2s.1 <- mZ2s.1; Z2s.2 <- mZ2s.2; Z1s.2 <- mZ1s.2;
                 Z1i.1 <- mZ1i.1; Z2i.1 <- mZ2i.1; Z2i.2 <- mZ2i.2; Z1i.2 <- mZ1i.2;  
                 Z3s.1 <- mZ3s.1; Z3s.2 <- mZ3s.2; 
   } else { Z1s.1 <- adiag(Z1s.1, mZ1s.1); Z2s.1 <- adiag(Z2s.1,mZ2s.1); Z2s.2 <- adiag(Z2s.2,mZ2s.2); 
            Z1s.2 <- adiag(Z1s.2, mZ1s.2); 
            Z1i.1 <- adiag(Z1i.1, mZ1i.1); Z2i.1 <- adiag(Z2i.1,mZ2i.1); Z2i.2 <- adiag(Z2i.2,mZ2i.2); 
            Z1i.2 <- adiag(Z1i.2, mZ1i.2);
            Z3s.1 <- adiag(Z3s.1,mZ3s.1); Z3s.2 <- adiag(Z3s.2,mZ3s.2)}
}

## 5.   Now, we start with the analysis for the manuscript

head(data)

## 6. Compute a model in which the actor and partner effects in liking at the first time point
##    and the liking relationship effects of the first time-point are used to predict mimicry. Results in Table S4


y <- as.matrix(data$Mimicry)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.AA","lT1.AP","lT1.Rel.intra","lT1.Rel.inter")]))
  
fit.Mimicry.prediction.lT1 <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.Mimicry.prediction.lT1$Information_algorithm
fit.Mimicry.prediction.lT1$Fixed_effects

##Standardization of fixed effects (you need to have the results for fit.LikingT1 and fit.Mimicry from analysis steps 1 and 2 stored in R workspace)

lT1.AA.stand <-fit.Mimicry.prediction.lT1$Fixed_effects[2,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])
lT1.AP.stand <-fit.Mimicry.prediction.lT1$Fixed_effects[3,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])/sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])
lT1.Rel.intra.stand <-fit.Mimicry.prediction.lT1$Fixed_effects[4,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])
lT1.Rel.inter.stand <-fit.Mimicry.prediction.lT1$Fixed_effects[5,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

lT1.AA.stand 
lT1.AP.stand 
lT1.Rel.intra.stand 
lT1.Rel.inter.stand 


## 7. Compute a model with Mimicry effects and LikingT1 effects as predictors of the random effects of the Liking T2 SRM Model
##    (again, estimation takes a few moments) - Results in Table S5

y <- as.matrix(data$Liking_T2)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.actor","mim.AA","lT1.partner","mim.PA","lT1.rel","mim.Rel.intra","mim.Rel.inter")]))

fit.LikingT2.prediction.lT1.mim <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.LikingT2.prediction.lT1.mim$Information_algorithm
fit.LikingT2.prediction.lT1.mim$Fixed_effects

##Standardization of fixed effects (you need to have the results for fit.Mimicry, fit.LikingT1, and fit.LikingT2 from analysis steps 1 to 3 stored in R workspace)
#Liking effects:

lT1.actor.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[2,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2]) 
lT1.partner.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[4,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
lT1.rel.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[6,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

lT1.actor.stand 
lT1.partner.stand 
lT1.rel.stand

# Mimicry effects:
mim.AA.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[3,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])
mim.PA.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[5,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
mim.Rel.intra.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[7,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])
mim.Rel.inter.stand <- fit.LikingT2.prediction.lT1.mim$Fixed_effects[8,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

mim.AA.stand 
mim.PA.stand 
mim.Rel.intra.stand 
mim.Rel.inter.stand 


## 8. Compute an alternative model with Mimicry effects and LikingT1 effects as predictors of the random effects of the Liking T2 SRM Model
##    (again, estimation takes a few moments) but on the relationship level only with intrapersonal relationship ("doing mimicry") without interpersonal relationship ("being mimicked") - Results in Table S8

y <- as.matrix(data$Liking_T2)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.actor","mim.AA","lT1.partner","mim.PA","lT1.rel","mim.Rel.intra")]))

fit.LikingT2.prediction.lT1.mim.a <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.LikingT2.prediction.lT1.mim.a$Information_algorithm
fit.LikingT2.prediction.lT1.mim.a$Fixed_effects

##Standardization of fixed effects (you need to have the results for fit.Mimicry, fit.LikingT1, and fit.LikingT2 from Analysis Steps 1 to 3 stored in R workspace)
#Liking effects:
lT1.actor.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[2,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2]) 
lT1.partner.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[4,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
lT1.rel.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[6,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

lT1.actor.a.stand 
lT1.partner.a.stand 
lT1.rel.a.stand 

# Mimicry effects:
mim.AA.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[3,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])
mim.PA.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[5,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
mim.Rel.intra.a.stand <- fit.LikingT2.prediction.lT1.mim.a$Fixed_effects[7,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

mim.AA.a.stand 
mim.PA.a.stand
mim.Rel.intra.a.stand 

## 9.Compute an alternative model with Mimicry-actor effects and Liking_T1-actor effects and so on as predictors of the random effects of the random effects of the Liking_T2 SRM Model
##(again, estimation takes a few moments) but on the relationship level only with interpersonal relationship ("being mimicked") without intrapersonal relationship ("doing mimicry") - Results in Table S9

y <- as.matrix(data$Liking_T2)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.actor","mim.AA","lT1.partner","mim.PA","lT1.rel","mim.Rel.inter")]))

fit.LikingT2.prediction.lT1.mim.b <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.LikingT2.prediction.lT1.mim.b$Information_algorithm
fit.LikingT2.prediction.lT1.mim.b$Fixed_effects

##Standardization of fixed effects (you need to have the results for fit.Mimicry, fit.LikingT1, and fit.LikingT2 from Analysis Steps 1 to 3 stored in R workspace)
#Liking effects:
lT1.actor.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[2,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2]) 
lT1.partner.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[4,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
lT1.rel.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[6,1]*sqrt(fit.LikingT1$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

lT1.actor.b.stand 
lT1.partner.b.stand 
lT1.rel.b.stand 

# Mimicry effects:
mim.AA.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[3,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])
mim.PA.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[5,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[1,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2])
mim.Rel.inter.b.stand <- fit.LikingT2.prediction.lT1.mim.b$Fixed_effects[7,1]*sqrt(fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])/sqrt(fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2])

mim.AA.b.stand 
mim.PA.b.stand 
mim.Rel.inter.b.stand 



## +++++++
## 10. Gender analyses of the liking prediction models
## +++++++

##  Compute a model in which the actor and partner effects in liking at the first time point and gender main effects and interactions 
##  are predictors of the random effects of mimicry

data$int_gender_lT1.AA <- data$gender*data$lT1.AA
data$int_gender_lT1.AP <- data$gender*data$lT1.AP
data$int_gender_lT1.Rel.intra <- data$gender*data$lT1.Rel.intra
data$int_gender_lT1.Rel.inter <- data$gender*data$lT1.Rel.inter

y <- as.matrix(data$Mimicry)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.AA","lT1.AP","lT1.Rel.intra","lT1.Rel.inter",
                                                                           "gender","int_gender_lT1.AA","int_gender_lT1.AP","int_gender_lT1.Rel.intra","int_gender_lT1.Rel.inter")]))

fit.Mimicry.prediction.lT1.int_gender <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.Mimicry.prediction.lT1.int_gender$Information_algorithm
fit.Mimicry.prediction.lT1.int_gender$Fixed_effects



## Compute a model with Mimicry-actor effects, LikingT1-actor effects and gender main effects and interactions as predictors of the random effects of liking T2
## (again, estimation takes a few moments) - 

data$int_gender_mim.AA <- data$gender*data$mim.AA
data$int_gender_mim.PA <- data$gender*data$mim.PA
data$int_gender_mim.Rel.intra <- data$gender*data$mim.Rel.intra
data$int_gender_mim.Rel.inter <- data$gender*data$mim.Rel.inter


y <- as.matrix(data$Liking_T2)
X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.actor","mim.AA","lT1.partner","mim.PA","lT1.rel","mim.Rel.intra","mim.Rel.inter", 
                                                                           "gender","int_gender_mim.AA","int_gender_mim.PA","int_gender_mim.Rel.intra","int_gender_mim.Rel.inter")]))

fit.LikingT2.prediction.lT1.mim.int_gender <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.LikingT2.prediction.lT1.mim.int_gender$Information_algorithm
fit.LikingT2.prediction.lT1.mim.int_gender$Fixed_effects 


##Please go on with the 6th R code 6.Data_Analysis_Prediction_Models_Metaliking.r



