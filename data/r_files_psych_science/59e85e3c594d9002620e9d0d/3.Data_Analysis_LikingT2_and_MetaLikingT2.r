## ============================================================
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

## The following function implements a Fisher scoring algorithm to estimate the SRM parameters
## with a restricted maximum likelihood approach. It is a slightly adapted version of the algorithm
## introduced in Nestler (in press) [Restricted maximum likelhood estimation of the parameters of the social
## relation model. Psychmetrika]. Nestler (in press) also discusses the REML approach.
## Please, do not make any changes to the function. Concrete usage of it is explained in the "Main program" part below.

LMM.SRM_MultivariatMultipleGroups <- function(starts,y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20) { 

   # 1.  Compute matrices
   # 1a. Groups stable and instable
   gs.M <- rbind(cbind(tcrossprod(Zgs.1),tcrossprod(Zgs.1,Zgs.2)),cbind(tcrossprod(Zgs.2,Zgs.1),tcrossprod(Zgs.2)))
   gi.M <- rbind(cbind(tcrossprod(Zgi.1),tcrossprod(Zgi.1,Zgi.2)),cbind(tcrossprod(Zgi.2,Zgi.1),tcrossprod(Zgi.2)))
   
   # 1b. individuals stable and instable
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
   
   # 1d. errors     
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
     
     # score vector
     sc <- matrix(0, ncol=1, nrow=length(Var))
     sc <- as.matrix(unlist(lapply(mlist,function(x) -sum(P*t(x))+((crossprod(y,P)%*%x)%*%(P%*%y)))),ncol=1,nrow=length(Var))

     # expected information matrix
     In  <- matrix(0, ncol=length(starts), nrow=length(starts))
     for (i in 1:length(Var)) {
         pa.i <- P%*%sp.mlist[[i]]          
         for (j in i:length(Var)) {
            pb.j <- P%*%sp.mlist[[j]] 
            In[i,j] <- sum(pa.i*t(pb.j))                  
          } 
     }
     In <- In + t(In) - diag(diag(In)) 
     
     # compute new variance-covariance estimates   
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
      # gradient
      gr  <- as.matrix(unlist(lapply(mlist,function(x) (t(cs)%*%sigma.b)%*%t(X)%*%Vi%*%x%*%Vi%*%X%*%(sigma.b%*%cs))),ncol=1,nrow=length(Var))
      # dfs
      dfs[k,1] <- (2*(t(cs)%*%sigma.b%*%cs)^2)/(t(gr)%*%solve(In)%*%gr)
   } 
   pss <- 2*pt(-abs(ts),df=dfs)
   
   # 6.   Compute random effects for stable actor-, partner-, and relationship effects
    Daps <- adiag(kronecker(Var[3],diag(1,ncol(Z1s.1))),kronecker(Var[4],diag(1,ncol(Z2s.1))))+Var[5]*matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(Z1s.1))
   Dres <- kronecker(Var[9],diag(1,ncol(Z3s.1)+ncol(Z3s.2)))+Var[10]*matrix(c(0,1,1,0),2,2)%x%diag(1,ncol(Z3s.1))
   Dt  <- adiag(Daps,Dres)
   bt  <- Dt%*%t(cbind(rbind(Z1s.1,Z2s.2),rbind(Z2s.1,Z1s.2),adiag(Z3s.1,Z3s.2)))%*%Vi%*%(y-X%*%b)  
   
   ids.aps <- ddply(daten,.(Group),.fun=function(x) { a <- sort(unique(x$Actor)); p <- sort(unique(x$Partner)); out <- data.frame(id.a = a,id.p = p); })
   ids.dy1 <- ddply(daten.1,.(Group),.fun=function(x) { x <- subset(x,x$Occasion==1); out <- data.frame(Actor = x$Actor, Partner = x$Partner, Dyad = x$Dyad); })
   ids.dy2 <- ddply(daten.2,.(Group),.fun=function(x) { x <- subset(x,x$Occasion==1); out <- data.frame(Actor = x$Actor, Partner = x$Partner, Dyad = x$Dyad); })
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
   
   # 8. export results
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
   
} # end of function

## %%%%%%%%%%%%%%%%%%
## 
##   Main part
## 
## %%%%%%%%%%%%%%%%%%

library(Matrix)
library(plyr)
library(magic)

## 1. Import data

setwd("") # change to your working directory
daten <- read.table("RawData_LikingMetaLikingT2.dat", header=T)
no.oc <- length(unique(daten$Occasion)) # number of measurements

# The next three steps are preparations for the algorithm; do not change any code here
# 2. Compute indicators for the instable effects

daten$gi <- no.oc*(daten$Group-1)+daten$Occasion
daten$Ai <- no.oc*(daten$Actor-1)+daten$Occasion
daten$Pi <- no.oc*(daten$Partner-1)+daten$Occasion

# 3. Sort the data according to the matrix formulation of the SRM  (see Nestler, in press)

daten <- daten[order(daten$Group,daten$Actor,daten$Partner,daten$Dyad,daten$Occasion),]
daten.1 <- subset(daten,daten$Actor < daten$Partner)
daten.1 <- daten.1[order(daten.1$Group,daten.1$Actor,daten.1$Partner,daten.1$Dyad,daten.1$Occasion),]
daten.2 <- subset(daten,daten$Actor > daten$Partner)
daten.2 <- daten.2[order(daten.2$Group,daten.2$Dyad,daten.2$Occasion),]
daten   <- rbind(daten.1,daten.2)                                                

# 4. Compute design matrices for the random effects

Zgs.1 <- with(daten.1, outer(Group, unique(Group), '==')*1)
Zgs.2 <- with(daten.2, outer(Group, unique(Group), '==')*1)

Zgi.1 <- with(daten.1, outer(gi, unique(gi), '==')*1)
Zgi.2 <- with(daten.2, outer(gi, unique(gi), '==')*1)
groups <- unique(daten.1$Group)
for (i in 1:length(groups)) {    
   g1 <- subset(daten.1,daten.1$Group==groups[i])
   g2 <- subset(daten.2,daten.2$Group==groups[i])
   mZ1s.1 <- with(g1, outer(Actor, sort(unique(daten[daten$Group==groups[i],]$Actor)), '==')*1)
   mZ2s.1 <- with(g1, outer(Partner, sort(unique(daten[daten$Group==groups[i],]$Partner)), '==')*1)
   mZ2s.2 <- with(g2, outer(Actor, sort(unique(daten[daten$Group==groups[i],]$Actor)), '==')*1)
   mZ1s.2 <- with(g2, outer(Partner, sort(unique(daten[daten$Group==groups[i],]$Partner)), '==')*1)
   mZ3s.1 <- with(g1, outer(Dyad, sort(unique(daten[daten$Group==groups[i],]$Dyad)), '==')*1)
   mZ3s.2 <- with(g2, outer(Dyad, sort(unique(daten[daten$Group==groups[i],]$Dyad)), '==')*1)
   mZ1i.1 <- with(g1, outer(Ai, sort(unique(daten[daten$Group==groups[i],]$Ai)), '==')*1)
   mZ2i.1 <- with(g1, outer(Pi, sort(unique(daten[daten$Group==groups[i],]$Pi)), '==')*1)
   mZ2i.2 <- with(g2, outer(Ai, sort(unique(daten[daten$Group==groups[i],]$Ai)), '==')*1)
   mZ1i.2 <- with(g2, outer(Pi, sort(unique(daten[daten$Group==groups[i],]$Pi)), '==')*1)
   if (i == 1) { Z1s.1 <- mZ1s.1; Z2s.1 <- mZ2s.1; Z2s.2 <- mZ2s.2; Z1s.2 <- mZ1s.2;
                 Z1i.1 <- mZ1i.1; Z2i.1 <- mZ2i.1; Z2i.2 <- mZ2i.2; Z1i.2 <- mZ1i.2;  
                 Z3s.1 <- mZ3s.1; Z3s.2 <- mZ3s.2; 
   } else { Z1s.1 <- adiag(Z1s.1, mZ1s.1); Z2s.1 <- adiag(Z2s.1,mZ2s.1); Z2s.2 <- adiag(Z2s.2,mZ2s.2); 
            Z1s.2 <- adiag(Z1s.2, mZ1s.2); 
            Z1i.1 <- adiag(Z1i.1, mZ1i.1); Z2i.1 <- adiag(Z2i.1,mZ2i.1); Z2i.2 <- adiag(Z2i.2,mZ2i.2); 
            Z1i.2 <- adiag(Z1i.2, mZ1i.2);
            Z3s.1 <- adiag(Z3s.1,mZ3s.1); Z3s.2 <- adiag(Z3s.2,mZ3s.2)}
}

## 5.   Now, we compute the latent SRMs for Liking_T2 and MetaLiking_T2

head(daten)

## 5.1  We start with Liking_T2 (see Table S2)
## Define the outcome vector y and the fixed effect matrix (here the mean; hence only a column of ones)

y <- as.matrix(daten$Liking_T2)
X <- matrix(data=1, ncol=1, nrow=length(y)) 
  
# Call the function to estimate the SRM parameters. See the mimicry-file for more information concerning the function.

fit.LikingT2 <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)

# We just want to see Information concerning the performance of the algoirthm, the fixed effects and the variance of the random
# effects (based on the computations performed in TripleR or SOREMO):

fit.LikingT2$Information_algorithm
fit.LikingT2$Fixed_effects
fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'

# We use the standard errors of the variance and covariance parameters to test the significance of these parameters (i.e., this test is called a Wald test).
# The test statistic is approximately standard normally distributed. Note, that this test is debatable in case of variance parameters (see e.g., Verbeke & Molenbergh, 2009).

z.actor <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2]/fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,3]
1-pnorm(abs(z.actor),0,1)

z.partner <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2]/fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,3]
1-pnorm(abs(z.partner),0,1)

z.relation <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2]/fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,3]
1-pnorm(abs(z.relation),0,1)

z.actor_partner <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[5,2]/fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[5,3]
(1-pnorm(abs(z.actor_partner),0,1))*2

z.relationship <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[6,2]/fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[6,3]
(1-pnorm(abs(z.relationship),0,1))*2 


## 5.2 The same analysis is performed for Meta-Liking T2

y <- as.matrix(daten$MetaLiking_T2)
X <- matrix(data=1, ncol=1, nrow=length(y)) 
  
fit.MetaLikingT2 <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
fit.MetaLikingT2$Information_algorithm
fit.MetaLikingT2$Fixed_effects
fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'

# We use the standard errors of the variance and covariance parameters to test the significance of these parameters (i.e., this test is called a Wald test).
# The test statistic is approximately standard normally distributed. Note, that this test is debatable in case of variance parameters (see e.g., Verbeke & Molenbergh, 2009).

z.actor <- fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,2]/fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[1,3]
1-pnorm(abs(z.actor),0,1)

z.partner <- fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,2]/fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2,3]
1-pnorm(abs(z.partner),0,1)

z.relation <- fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,2]/fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[3,3]
1-pnorm(abs(z.relation),0,1)

z.actor_partner <- fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[5,2]/fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[5,3]
(1-pnorm(abs(z.actor_partner),0,1))*2

z.relationship <- fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[6,2]/fit.MetaLikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[6,3]
(1-pnorm(abs(z.relationship),0,1))*2 


#### Please use the 4th R code saved in 4.Make_Data_Prediction_Models.r

