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
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##    Bootstrapping the Mediation Effect for Liking
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##           SRM function
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

LMM.SRM_MultivariatMultipleGroups <- function(starts,y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z1i.1,Z2i.1,Z2i.2,Z1i.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20) {                                                                                            

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
                                    "Actor variance stable","Partner variance stable","Actor-Partner covariance stable",
                                    "Actor variance unstable","Partner variance unstable","Actor-Partner covariance unstable",
                                    "Relationship variance","Relationship covariance",
                                    "Error variance","Error covariance"),
                        Estimate=round(Var,3),
                        SE=round(ses,3))
   OutTripleR <- data.frame(Parameter=c("Actor variance","Partner variance","Relationship variance","Error variance",
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
   
} # End of function SRM

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##   This function makes the bootstrap samples 
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## As we want to test the indirect effect on the level of the dyad
## we decided to sample groups and then dyads

newData <- function(datas) {
   
   out <- data.frame()
   for (k in 1:length(unique(datas$Group))) {
   
       data <- subset(datas,datas$Group==unique(datas$Group)[k])
       data <- data[order(data$Dyad),]
       zahl <- sort(sample(unique(data$Dyad),length(unique(data$Dyad)),replace=T))
       dyad <- sort(unique(data$Dyad))

       for (i in 1:length(zahl)) {
    
           zw <- subset(data,data$Dyad==zahl[i])
           zw$Dyadnew  <- i
           zw$Actornew <- data[data$Dyad==dyad[i],]$Actor
           zw$Partnernew <- data[data$Dyad==dyad[i],]$Partner
           out <- rbind(out,zw) 

       }
   
   }
   return(out) 

} # End function bootstrap samples

## 
##             Main program
## 

library(Matrix)
library(plyr)
library(magic)

# Import raw data file

data <- read.table("DataPredictionModels.dat",header=T)

# Make bootstrap files, save them in a folder specified in namess

nrep <- 2000
for (z in 1:nrep) {

    samples <- newData(data)
    namess <- paste("XXX/Sample",z,".dat",sep="") # substitute XXX
    write.table(samples,namess,row.names=F,col.names=T)

}

# Now, we have to fit two SRMs to each Bootstrap sample. This takes a while 
# (on my [SN] computer about three days), so we do not recommend to use this function.

# We have uploaded the results for the 2,000 samples that we generated and used
# for the analyses. If you are interested in these 2,000 files, please write an 
# email to SN (steffen.nestler@uni-leipzig.de).

out1 <- data.frame(samp=numeric(),b0=numeric(),b1=numeric(),b2=numeric(),b3=numeric(),b4=numeric(),
                  avar=numeric(),pvar=numeric(),rvar=numeric(),
                  apcov=numeric(),rrcov=numeric())


out2 <- data.frame(samp=numeric(),b0=numeric(),b1=numeric(),b2=numeric(),b3=numeric(),b4=numeric(),
                   b5=numeric(),b6=numeric(),b7=numeric(),
                   avar=numeric(),pvar=numeric(),rvar=numeric(),
                   apcov=numeric(),rrcov=numeric())

no.samples <- 2000

for (z in 1:no.samples) {

    namess <-paste("BootstrapSamples2/Sample",z,".dat",sep="")
    data <- read.table(namess,header=T)
    
    ## Prerequisites for the function
    no.oc <- length(unique(data$Occasion))
    
    ## Rename
    names(data)[2:4] <- c("Actor2","Partner2","Dyad2")
    names(data)[19:21] <- c("Dyad","Actor","Partner")

    data$gi <- no.oc*(data$Group-1)+data$Occasion
    data$Ai <- no.oc*(data$Actor-1)+data$Occasion
    data$Pi <- no.oc*(data$Partner-1)+data$Occasion

    ## Sort the data according to the matrix formulation of the SRM  (see Nestler, in press)
    data <- data[order(data$Group,data$Actor,data$Partner,data$Dyad,data$Occasion),]
    data.1 <- subset(data,data$Actor < data$Partner)
    data.1 <- data.1[order(data.1$Group,data.1$Actor,data.1$Partner,data.1$Dyad,data.1$Occasion),]
    data.2 <- subset(data,data$Actor > data$Partner)
    data.2 <- data.2[order(data.2$Group,data.2$Dyad,data.2$Occasion),]
    data   <- rbind(data.1,data.2)                                                

    ## Compute design matrices for the random effects
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

    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## Fit the SRM to the mimicry variable
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    y <- as.matrix(data$Mimicry)
    X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.AA","lT1.AP","lT1.Rel.intra","lT1.Rel.inter")]))
  
    fit.Mimicry <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z1i.1,Z2i.1,Z2i.2,Z1i.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
    
    ## Save the results
    feff <- fit.Mimicry$'Fixed_effect'[1]$Effects
    vars <- fit.Mimicry$'Variance_Random_effects_(TripleR/SOREMO)'[2]$Estimate
    zwis <- data.frame(samp=z,b0=feff[1],b1=feff[2],b2=feff[3],b3=feff[4],b4=feff[5],
                       avar=vars[1],pvar=vars[2],rvar=vars[3],evar=vars[4],apcov=vars[5],rrcov=vars[6])
    out1 <- rbind(out1,zwis)
    
    ## 
    ## Fit the SRM to the liking T2 variable
    ## 
    
    y <- as.matrix(data$Liking_T2)
    X <- as.matrix(cbind(Inter=matrix(data=1, ncol=1, nrow=length(y)),data[,c("lT1.actor","mim.AA","lT1.partner","mim.PA","lT1.rel","mim.Rel.intra","mim.Rel.inter")]))
    fit.LikingT2 <- LMM.SRM_MultivariatMultipleGroups(starts=c(1,1,1,1,0,1,1,0,1,0,1,0),y,X,Zgs.1,Zgs.2,Zgi.1,Zgi.2,Z1s.1,Z2s.1,Z2s.2,Z1s.2,Z1i.1,Z2i.1,Z2i.2,Z1i.2,Z3s.1,Z3s.2,difcrit=10e-5,maxit=20)
    
    ## Save the results
    feff <- fit.LikingT2$'Fixed_effect'[1]$Effects
    vars <- fit.LikingT2$'Variance_Random_effects_(TripleR/SOREMO)'[2]$Estimate
    zwis <- data.frame(samp=z,b0=feff[1],b1=feff[2],b2=feff[3],b3=feff[4],b4=feff[5],b5=feff[6],b6=feff[7],b7=feff[8],
                       avar=vars[1],pvar=vars[2],rvar=vars[3],evar=vars[4],apcov=vars[5],rrcov=vars[6])
    out2 <- rbind(out2,zwis)
    
}

write.table(out1,"BootstrapResultsMimicry.dat",row.names=F,col.names=T)
write.table(out2,"BootstrapResultsLiking.dat",row.names=F,col.names=T)  


#
# The results for the 2,000 samples have been saved. Now, we import these files
# and compute the bias-corrected bootstrap confidence interval:
#

datLT2 <- read.table("BootstrapResultsLiking.dat",header=T)[,c(1,9)]
datMim <- read.table("BootstrapResultsMimicry.dat",header=T)[,c(1,5)]
dat <- merge(datMim,datLT2,by="samp")

# Compute the indirect effect
dat$ab <- dat$b3*dat$b7

# Some descriptives for the a-, b-, and ab-path
apply(dat[,c("b3","b7","ab")],2,mean)

# Bias corrected bootstrap ci for the indirect effect

z0 <- qnorm(sum(dat$ab <= 0.03227247)/nrow(dat))
alph1 <- pnorm(2*z0-1.96)
alph2 <- pnorm(2*z0+1.96)
bootKIab <- c(quantile(dat$ab,alph1),quantile(dat$ab,alph2))
bootKIab

## Bias corrected bootstrap ci for the a path

z0 <- qnorm(sum(dat$b3 <= 0.115)/nrow(dat))
alph1 <- pnorm(2*z0-1.96)
alph2 <- pnorm(2*z0+1.96)
bootKIa <- c(quantile(dat$b3,alph1),quantile(dat$b3,alph2))
bootKIa 

## Bias corrected bootstrap ci for the b path

z0 <- qnorm(sum(dat$b7 <= 0.280)/nrow(dat))
alph1 <- pnorm(2*z0-1.96)
alph2 <- pnorm(2*z0+1.96)
bootKIb <- c(quantile(dat$b7,alph1),quantile(dat$b7,alph2))
bootKIb

##Please go on with the 8th R code Data_Analysis_SRM_Mimicry_Gender_Differences.r for controlling for gender in the univariate analyses of mimicry.