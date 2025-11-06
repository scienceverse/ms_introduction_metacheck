############################
# READ IN AND PREPARE DATA #
############################
# Load libraries and read in data:
library(rstanarm)
set.seed(1234)
setwd("/Users/blakemcshane/Documents/Academic Research/MetaAnalysis/CRT/Analysis2")
dat <- read.csv("CRT_finaldata.csv", header=TRUE)

# Sampling parameters
n.chains <- 4
n.iter <- 10000
n.core <- 4
a.delta <- 0.99

# Prep data:
dat$Subject.ID <- paste(dat$Experiment.ID, dat$Subject.ID, sep="*")
dat$Question.ID <- factor(dat$Question.ID)
dat$ConditionT <- 1*(dat$Condition=="Testosterone")
dat$Group.ID <- paste(dat$Experiment.ID, dat$Condition, dat$Blind, dat$Stress, sep="*")
dat$CQ.ID <- paste(dat$Condition, dat$Question.ID, sep="*Q")
dat$EQ.ID <- paste(dat$Experiment.ID, dat$Question.ID, sep="*Q")
dat$GQ.ID <- paste(dat$Group.ID, dat$Question.ID, sep="*Q")
dat$NCQ.ID <- paste(paste0("Nave",dat$Experiment.ID==4), dat$CQ.ID, sep="*")

nave.sel <- dat$Experiment.ID==4

dat$TrtExp <- 1*(dat$Treatment_Expectancy_dichot=="Testosterone")
dat$TrtExp[nave.sel] <- (dat$Treatment_Expectancy[nave.sel] - 1) / 4
dat$Time <- dat$CRT_Time_minnoon / 60

#################
# PRIMARY MODEL #
#################
mm.4 <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=5230)

mm.3 <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=6023)

mm.4.expanded <- stan_glmer(y~NCQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=4235)

###########################
# INDIVIDUAL STUDY MODELS #
#      FOR THE FIGURE     #
###########################
mi <- list(NULL)
sel <- dat$Experiment.ID==1
mi[[1]] <- stan_glmer(y~CQ.ID-1 + (1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9896)

sel <- dat$Experiment.ID==2
mi[[2]] <- stan_glmer(y~CQ.ID-1 + (1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=7626)

sel <- dat$Experiment.ID==3
mi[[3]] <- stan_glmer(y~CQ.ID-1 + (1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9835)

sel <- dat$Experiment.ID==4
mi[[4]] <- stan_glmer(y~CQ.ID-1 + (1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3207)

###########################
# SUM SCORE LINEAR MODELS #
###########################
sdat <- unique(dat[,c("Experiment.ID","Group.ID","Subject.ID","ConditionT")])
sdat$y <- NA
for(i in 1:nrow(sdat)){
	sel <- dat$Subject.ID==sdat$Subject.ID[i]
	if(sum(sel)!=3){print("Error")
	}else{
		sdat$y[i] <- sum(dat$y[sel])
	}	
}

sm.4 <- stan_glmer(y~ConditionT + (1|Experiment.ID)+(1|Group.ID),
				data=sdat, family="gaussian",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9207)

sel <- sdat$Experiment.ID!=4
sm.3 <- stan_glmer(y~ConditionT + (1|Experiment.ID)+(1|Group.ID),
				data=sdat[sel,], family="gaussian",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9745)

#############
# COVARATES #
#############
# Covariates: age, treatment expectancy, right-hand 2D:4D, cortisol 
mm.4.cov1 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+
				(1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=7655)

mm.3.cov1 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+
				(1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=8097)

# Covariates: age, treatment expectancy, right-hand 2D:4D, cortisol, pos & neg affect 
sel <- dat$Experiment.ID %in% c(1,3,4)
mm.134.cov2 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+Positive.Affect+Negative.Affect+
				(1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=8567)

sel <- dat$Experiment.ID %in% c(1,3)
mm.13.cov2 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+Positive.Affect+Negative.Affect+
				(1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=8106)

# Covariates: age, treatment expectancy, right-hand 2D:4D, cortisol, pos & neg affect, math ability
sel <- dat$Experiment.ID %in% c(1,4)
mm.14.cov3 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+Positive.Affect+Negative.Affect+Math_z+
				(1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=7069)

sel <- dat$Experiment.ID %in% c(1)
mm.1.cov3 <- stan_glmer(y~CQ.ID-1 + Age+TrtExp+R2d4d+Pre.C+Positive.Affect+Negative.Affect+Math_z+
				(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=4074)

###########################
# INTUITIVE-BUT-INCORRECT #
###########################
mm.ibi.4 <- stan_glmer(CRT_IBI~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3039)

mm.ibi.3 <- stan_glmer(CRT_IBI~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=6254)

################################################
# METHODOLOGICAL DIFFERENCES: REMOVE SUBGROUPS #
################################################
sel <- dat$Blind=="Double" & dat$Stress=="Control"
mm.subgroup.4 <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3951)


sel <- !nave.sel & dat$Blind=="Double" & dat$Stress=="Control"
mm.subgroup.3 <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=4886)

############################################
# METHODOLOGICAL DIFFERENCES: INTERACTIONS #
############################################
# Experimenter sex
mm.sex.4 <- stan_glmer(y~CQ.ID*Experimenter_Gender-1-CQ.ID-Experimenter_Gender + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=6142)


mm.sex.3 <- stan_glmer(y~CQ.ID*Experimenter_Gender-1-CQ.ID-Experimenter_Gender + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=2924)

# Time of day
mm.time.4 <- stan_glmer(y~CQ.ID*Time-1-Time + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9513)

mm.time.3 <- stan_glmer(y~CQ.ID*Time-1-Time + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=6543)

# Blinding
mm.blind.4 <- stan_glmer(y~CQ.ID*Blind-1-CQ.ID-Blind + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3767)

mm.blind.3 <- stan_glmer(y~CQ.ID*Blind-1-CQ.ID-Blind + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=1391)

# Stress
mm.stress.4 <- stan_glmer(y~CQ.ID*Stress-1-CQ.ID-Stress + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=2643)

mm.stress.3 <- stan_glmer(y~CQ.ID*Stress-1-CQ.ID-Stress + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=1087)

########################################
# INDIVIDUAL DIFFERENCES: INTERACTIONS #
########################################
# Cortisol
mm.cortisol.4 <- stan_glmer(y~CQ.ID*Pre.C-1-Pre.C + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=7351)

mm.cortisol.3 <- stan_glmer(y~CQ.ID*Pre.C-1-Pre.C + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3420)

# R:2D4D
mm.r2d4d.4 <- stan_glmer(y~CQ.ID*R2d4d-1-R2d4d + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=9132)

mm.r2d4d.3 <- stan_glmer(y~CQ.ID*R2d4d-1-R2d4d + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=2131)

# L:2D4D
mm.l2d4d.4 <- stan_glmer(y~CQ.ID*L2d4d-1-L2d4d + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat, family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=1735)

mm.l2d4d.3 <- stan_glmer(y~CQ.ID*L2d4d-1-L2d4d + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=3759)

# Trait impulsivity (not available in Nave et al.)
mm.impulsivity.3 <- stan_glmer(y~CQ.ID*Impulsivity-1-Impulsivity + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
				data=dat[!nave.sel,], family="binomial",
				cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=8230)

save.image("crt.results.Rdata")
