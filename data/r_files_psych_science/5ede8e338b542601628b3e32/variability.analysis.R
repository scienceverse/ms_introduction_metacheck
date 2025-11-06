# Load results:
setwd("/Users/blakemcshane/Documents/Academic Research/MetaAnalysis/CRT/Analysis2")
load("crt.results.Rdata")

# Parameter names and packaging:
theta.name <- c("CQ.IDPlacebo*QCRT1", "CQ.IDPlacebo*QCRT2", "CQ.IDPlacebo*QCRT3", 
		"CQ.IDTestosterone*QCRT1", "CQ.IDTestosterone*QCRT2", "CQ.IDTestosterone*QCRT3",
		"Sigma[EQ.ID:(Intercept),(Intercept)]", "Sigma[Experiment.ID:(Intercept),(Intercept)]",
		"Sigma[GQ.ID:(Intercept),(Intercept)]", "Sigma[Group.ID:(Intercept),(Intercept)]",
		"Sigma[Subject.ID:(Intercept),(Intercept)]")
package.theta <- function(stan.model){
	tmp1 <- colnames(as.matrix(stan.model))
	tmp2 <- grep("b\\[", tmp1)
	tmp3 <- as.matrix(stan.model)[,tmp1[setdiff(1:length(tmp1),tmp2)]]
	theta.median <- apply(tmp3, 2, median)
	if(length(theta.median)!=length(theta.name)){ print("Error1"); return(NA) }
	if( all(sort(names(theta.median))!=sort(theta.name)) ){ print("Error2"); return(NA) }

	bb <- tmp3[,theta.name[1:6]]
	te <- rowMeans(tmp3[,theta.name[4:6]] - tmp3[,theta.name[1:3]])
	sigmaI <- sqrt(tmp3[,theta.name[11]])

	SigmaE <- array(0, c(nrow(tmp3),3,3))
	SigmaE[,1,1] <- SigmaE[,2,2] <- SigmaE[,3,3] <- tmp3[,theta.name[7]]
	SigmaE <- SigmaE + array(tmp3[,theta.name[8]], c(nrow(tmp3),3,3))

	SigmaG <- array(0, c(nrow(tmp3),3,3))
	SigmaG[,1,1] <- SigmaG[,2,2] <- SigmaG[,3,3] <- tmp3[,theta.name[9]]
	SigmaG <- SigmaG + array(tmp3[,theta.name[10]], c(nrow(tmp3),3,3))

	list(te=te, bb=bb, SigmaE=SigmaE, SigmaG=SigmaG, sigmaI=sigmaI)
}
theta.list <- list( package.theta(mm.4), package.theta(mm.3),
					package.theta(mm.subgroup.4), package.theta(mm.subgroup.3) )
MM <- length(theta.list[[1]][[1]])

# Variability: Experiment to Experiment
probs <- c(0.025, 0.25, 0.50, 0.75, 0.975)
point.estimate <- matrix(NA, 3, length(theta.list))
colnames(point.estimate) <- c("All4", "New3", "All4.Subgroup", "New3.Subgroup")
cc <- rep(c(-1,1), each=3)/3
v1 <- matrix(NA, MM, length(theta.list))
colnames(v1) <- c("All4", "New3", "All4.Subgroup", "New3.Subgroup")
for(i in 1:length(theta.list)){
	tmp.theta <- theta.list[[i]]
	for(j in 1:MM){
		tmpm <- kronecker(diag(2), tmp.theta$SigmaE[j,,] + tmp.theta$SigmaG[j,,])
		v1[j,i] <- t(cc) %*% tmpm %*% cc	
	}
}
point.estimate[1,] <- apply(sqrt(v1), 2, median)
round(t(apply(sqrt(v1), 2, quantile, probs)), 3)

# Variability: Group to Group
v2 <- matrix(NA, MM, length(theta.list))
colnames(v2) <- c("All4", "New3", "All4.Subgroup", "New3.Subgroup")
for(i in 1:length(theta.list)){
	tmp.theta <- theta.list[[i]]
	for(j in 1:MM){
		tmpm <- kronecker(diag(2), tmp.theta$SigmaG[j,,])
		v2[j,i] <- t(cc) %*% tmpm %*% cc	
	}
}
point.estimate[2,] <- apply(sqrt(v2), 2, median)
round(t(apply(sqrt(v2), 2, quantile, probs)), 3)

# Variability: Individual to Individual
v3 <- matrix(NA, MM, length(theta.list))
colnames(v3) <- c("All4", "New3", "All4.Subgroup", "New3.Subgroup")
for(i in 1:length(theta.list)){
	tmp.theta <- theta.list[[i]]
	for(j in 1:MM){
		tmpm <- kronecker(diag(2), matrix(tmp.theta$sigmaI[j]^2,3,3))
		v3[j,i] <- t(cc) %*% tmpm %*% cc	
	}
}
point.estimate[3,] <- apply(sqrt(v3), 2, median)
round(t(apply(sqrt(v3), 2, quantile, probs)), 3)

# Ratio
te.point.estimate <- sapply(theta.list, function(x){median(x[[1]])})
ratio.point.estimate <- point.estimate / matrix(te.point.estimate, 3, 4, byrow=TRUE)
round(ratio.point.estimate, 2)