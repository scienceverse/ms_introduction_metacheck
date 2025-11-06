############### calculating R2 for cross-classified mlm with 2 clusters
# formulas & code adapted from Rights & Sterba (2019), Table 5, Appendix Section A2
# 2020/08/27

library(lme4)
library(dplyr)
library(psych)

# load in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("study2data_StereotypeFaceTraitSpace.csv")

# center TraitSimilarity within cluster
data <- data %>%
  mutate(TraitSimilarity_gmc = TraitSimilarity-mean(TraitSimilarity)) %>% # grand mean centering
  group_by(ParticipantID) %>% 
  mutate(TraitSimilarity_cm = mean(TraitSimilarity), # cluster mean
         TraitSimilarity_cwc = TraitSimilarity-TraitSimilarity_cm) %>% # centered within cluster
  ungroup %>%
  mutate(TraitSimilarity_cmc = TraitSimilarity_cm-mean(TraitSimilarity_cm))

# build conditional lme4 model
model <- lmer(faceCor ~ TraitSimilarity_cwc + 
                (TraitSimilarity_cwc | ParticipantID) + (TraitSimilarity_cwc | Traitpair), data=data, REML=TRUE)
print(summary(model))

# gamma_w, fixed slopes for L1 variables
L1fixedslope = as.numeric(fixef(model)[2])
gamma_w = c(1, L1fixedslope)

# gamma_b, fixed intercept estimate (entered first), followed by fixed slope
#   estimates for all level-2 predictors (in the order listed by between_covs)
fixedintercept = as.numeric(fixef(model)[1])
gamma_b = c(fixedintercept)

# define gamma, given that we have an intercept in the model
gamma = matrix(c(1, L1fixedslope))

# within_covs, list of numbers corresponding to the columns in the dataset of
#   the level-1 predictors used in the MLM. if none used, set to NULL
within_covs = c(10) # CHANGE ME

# between_covs, list of numbers corresponding to the columns in the
#   dataset of the level-2 predictors used in the MLM (if none, set to NULL)
between_covs = NULL

# compute phi, covariance matrix of x_ij, which is a vector of 1 and all predictors
phi <- matrix(var(cbind(1,data[,c(within_covs)],data[,c(between_covs)]),na.rm=T),ncol=2)

# look at var-cov matrix for random effects
print(VarCorr(model),comp="Variance",digits=6)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# create tau matrix, random effect covariance matrix
#   first row/column denotes the intercept variance and covariances (if intercept is fixed, set to 0)
#   each subsequent row/column denotes a given random slope's variance and covariances

# structure of tau matrix for 2-cluster cross-classified model with random slope in both clusters:
#   intercept var A     int-slope cov A       0                    0
#   int-slope cov A     slope var A           0                    0
#   0                   0                     intercept var B      int-slope cov B
#   0                   0                     int-slope cov B      slope var B

Tau <- matrix(0, nrow=4,ncol=4)
Tau[1,1] = 0.01254962
Tau[1,2] = -0.00038
Tau[2,1] = -0.00038
Tau[2,2] = 0.00218989
Tau[3,3] = 0.05343443
Tau[3,4] = 0.00041
Tau[4,3] = 0.00041
Tau[4,4] = 0.00106053
Tau

# automated
Tau[1,1] = unclass(VarCorr(model))$ParticipantID[1]
Tau[1,2] = unclass(VarCorr(model))$ParticipantID[2]
Tau[2,1] = unclass(VarCorr(model))$ParticipantID[2]
Tau[2,2] = unclass(VarCorr(model))$ParticipantID[4]
Tau[3,3] = unclass(VarCorr(model))$Traitpair[1]
Tau[3,4] = unclass(VarCorr(model))$Traitpair[2]
Tau[4,3] = unclass(VarCorr(model))$Traitpair[2]
Tau[4,4] = unclass(VarCorr(model))$Traitpair[4]
Tau

# create sigma matrix
# structure of sigma matrix with only 1 L1 predictor:
#   0           0                     0              0
#   0           var(L1 predictor)     0              var(L1 predictor) 
#   0           0                     0              0  
#   0           var(L1 predictor)     0              var(L1 predictor) 

Sigma <- matrix(0, nrow=4,ncol=4)
Sigma[2,2] = 2.316417
Sigma[4,2] = 2.316417
Sigma[2,4] = 2.316417
Sigma[4,4] = 2.316417

# automated
Sigma[2,2] = var(data[,c(within_covs)])[1]
Sigma[4,2] = var(data[,c(within_covs)])[1]
Sigma[2,4] = var(data[,c(within_covs)])[1]
Sigma[4,4] = var(data[,c(within_covs)])[1]
Sigma

# residual
sigma2 = sigma(model)^2 # this was 0.20180902

# enter the mean using this structure:
#   1
#   mean(L1 predictor)
#   1 
#   mean(L1 predictor)
m = matrix(c(1,
             mean(as.matrix(data[,within_covs])),
             1,
             mean(as.matrix(data[,within_covs]))))

# Compute R2 ####
# formulas from Rights & Sterba (2019),
# Table 5: Total R2 Formulas for MLMs Not Employing Cluster-Mean-Centering

# R2_f = Proportion of total outcome variance explained by predictors via fixed slopes
# R2_v = Proportion of total outcome variance explained by predictors via random slope variation/covariation
# R2_m = Proportion of total outcome variance explained by cluster-specific outcome means (beyond that
#        attributable to predictors via fixed slopes and random slope variation/covariation)
# R2_fv = Proportion of total outcome variance explained by predictors via fixed slopes and random slope
#         variation/covariation
# R2_fvm = Proportion of total outcome variance explained by predictors via fixed slopes and random slope
#          variation/covariation and by cluster-specific outcome means

totalvar_notdecomp = (t(gamma)%*%phi%*%gamma) + tr(Tau%*%Sigma) + t(m)%*%Tau%*%m + sigma2
decomp_fixed_notdecomp = (t(gamma)%*%phi%*%gamma) / totalvar_notdecomp
decomp_varslopes_notdecomp = tr(Tau%*%Sigma) / totalvar_notdecomp
decomp_varmeans_notdecomp = t(m)%*%Tau%*%m / totalvar_notdecomp
decomp_sigma_notdecomp <- sigma2/totalvar_notdecomp


R2_f <- decomp_fixed_notdecomp  
R2_v <- decomp_varslopes_notdecomp
R2_m <- decomp_varmeans_notdecomp
R2_fv <- decomp_fixed_notdecomp + decomp_varslopes_notdecomp
R2_fvm <- decomp_fixed_notdecomp + decomp_varslopes_notdecomp + decomp_varmeans_notdecomp

# figs ####

decomp_table <- matrix(c(decomp_fixed_notdecomp,decomp_varslopes_notdecomp,decomp_varmeans_notdecomp,decomp_sigma_notdecomp),ncol=1)
rownames(decomp_table) <- c("fixed","slope variation","mean variation","sigma2")
colnames(decomp_table) <- c("total")
R2_table <- matrix(c(R2_f,R2_v,R2_m,R2_fv,R2_fvm),ncol=1)
rownames(R2_table) <- c("f","v","m","fv","fvm")
colnames(R2_table) <- c("total")
##barchar
contributions_stacked <- matrix(c(decomp_fixed_notdecomp,decomp_varslopes_notdecomp,decomp_varmeans_notdecomp,decomp_sigma_notdecomp),4,1)
colnames(contributions_stacked) <- c("total")
rownames(contributions_stacked) <- c("fixed slopes",
                                     "slope variation",
                                     "intercept variation",
                                     "residual")
barplot(contributions_stacked, main="Decomposition", horiz=FALSE,
        ylim=c(0,1),col=c("darkblue","darkblue","darkblue","white"),ylab="proportion of variance",
        density=c(NA,30,40,NA),angle=c(0,0,135,0),xlim=c(0,1),width=c(.6))
legend(.30,-.1,legend=rownames(contributions_stacked),fill=c("darkblue","darkblue","darkblue","white"),
       cex=.7, pt.cex = 1,xpd=TRUE,density=c(NA,30,40,NA),angle=c(0,0,135,0))
