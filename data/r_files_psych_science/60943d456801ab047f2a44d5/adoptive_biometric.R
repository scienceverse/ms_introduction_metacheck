########################## Biometrical model for adoptive and biological families ###################

# Adapted from equations in:
# Keller, M. C., Medland, S. E., Duncan, L. E., Hatemi, P. K., Neale, M. C., Maes, H. H.
# & Eaves, L. J. (2009). Modeling extended twin family data I: Description of the
# cascade model. Twin Research and Human Genetics, 12(1), 8–18.

library(ICC)
library(nloptr)
library(ROI)

# https://rdrr.io/rforge/Rnlminb2/src/R/nlminb2.R 
source("~/nlminb2.R")

# We need equations for the following covariances:

# mother-father
# biological mother-child
# biological father-child
# adoptive-adoptive siblings
# adoptive-biological siblings
# biological-biological siblings
# adoptive mother-child
# adoptive father-child

# MOTHER-FATHER
# mu*sigma^4 (typo in Keller et al. 2009?)

# BIOLOGICAL MOTHER-CHILD
# (1/2)*a*(q*a + w) + (1/2)*a*(q*a + w)*mu*sigma^2 + m*sigma^2 + p*mu*sigma^4

# BIOLOGICAL FATHER-CHILD
# (1/2)*a*(q*a + w) + (1/2)*a*(q*a + w)*mu*sigma^2 + p*sigma^2 + m*mu*sigma^4

# ADOPTIVE-ADOPTIVE SIBLINGS
# x + s^2

# ADOPTIVE-BIOLOGICAL SIBLINGS
# x + s^2 + a*w  # I THINK

# BIOLOGICAL-BIOLOGICAL SIBLINGS
# a^2*(q - (1/2)) + (1/4)*d^2 + 2*a*w + x + s^2

# ADOPTIVE MOTHER-CHILD
# m*sigma^2 + p*mu*sigma^4

# ADOPTIVE FATHER-CHILD
# p*sigma^2 + m*mu*sigma^4

estimate_mod_param <- function(df, B=200, x0){
  # I only use the columns FAMID, IDAB, IDFAMAB, and the three phenotypes (child, mom, dad)
  # in the code above. Make "df" a dataframe with only these columns. Make the three phenotype
  # columns last, in this order, so that I can apply the analysis to any trait without calling it
  # by name. So, to be clear,
  # column 1 - FAMID
  # column 2 - IDAB
  # column 3 - IDFAMAB
  # column 4 - phenotype of offspring
  # column 5 - phenotype of mom
  # column 6 - phenotype of dad
  
  # First, get the point estimates. Use different names to make absolutely sure there are no
  # naming conflicts.
  
  mom_child_bio <- cor.test(df[df$IDAB==2,5], df[df$IDAB==2,4])
  corr_mom_child_bio <- mom_child_bio$estimate
  N_mom_child_bio <- mom_child_bio$parameter + 2
  
  dad_child_bio <- cor.test(df[df$IDAB==2,6], df[df$IDAB==2,4])
  corr_dad_child_bio <- dad_child_bio$estimate
  N_dad_child_bio <- dad_child_bio$parameter + 2	
  
  adopt_fam <- subset(df, IDFAMAB==1)
  bio_fam <- subset(df, IDFAMAB==2)
  mixed_fam <- subset(df, IDFAMAB==3)	
  
  siblings_adopt_adopt <- cor.test(adopt_fam[seq(1, nrow(adopt_fam), 2),4], 
                                   adopt_fam[seq(2, nrow(adopt_fam), 2),4])
  corr_siblings_adopt_adopt <- siblings_adopt_adopt$estimate
  N_siblings_adopt_adopt <- siblings_adopt_adopt$parameter + 2
  
  siblings_adopt_bio <- cor.test(mixed_fam[mixed_fam$IDAB==1,4],
                                 mixed_fam[mixed_fam$IDAB==2,4])
  corr_siblings_adopt_bio <- siblings_adopt_bio$estimate    
  N_siblings_adopt_bio <- siblings_adopt_bio$parameter + 2                                	   
  
  siblings_bio_bio <- cor.test(bio_fam[seq(1, nrow(adopt_fam), 2),4],
                               bio_fam[seq(2, nrow(adopt_fam), 2),4])
  corr_siblings_bio_bio <- siblings_bio_bio$estimate  
  N_siblings_bio_bio <- siblings_bio_bio$parameter + 2  
  
  mom_child_adopt <- cor.test(df[df$IDAB==1,5], df[df$IDAB==1,4])
  corr_mom_child_adopt <- mom_child_adopt$estimate
  N_mom_child_adopt <- mom_child_adopt$parameter + 2
  
  dad_child_adopt <- cor.test(df[df$IDAB==1,6], df[df$IDAB==1,4])
  corr_dad_child_adopt <- dad_child_adopt$estimate
  N_dad_child_adopt <- dad_child_adopt$parameter + 2
  
  dad_mom <- cor.test(df[,5], df[,6])
  corr_dad_mom <- dad_mom$estimate 
  N_dad_mom <- dad_mom$parameter + 2
  
  # x is a vector with the following components:
  # (1) a: the effect of the genetic score on the phenotype
  # (2) q: the variance of the genetic score
  # (3) dsq: dominance genetic variance
  # (4) ssq: variance of environmental factors shared by siblings reared together
  # (5) m: direct effect of maternal phenotype on offspring phenotype
  # (6) p: direct effect of paternal phenotype on offspring phenotype
  # (7) w: covariance between additive genetic factors and family environment
  # (8) x: variance of the shared environment induced by parental phenotypes
  # (9) mu: correlation between spouses	
  
  OBJ_FUNCTION <- function(x){
    w_sum_sq <-
      
      (N_mom_child_bio - 3)*(atanh(corr_mom_child_bio) - atanh(
        (1/2)*x[1]*(x[2]*x[1] + x[7]) + (1/2)*x[1]*(x[2]*x[1] + x[7])*x[9] + x[5] + x[6]*x[9] ) )^2 +
      
      (N_dad_child_bio - 3)*(atanh(corr_dad_child_bio) - atanh(
        (1/2)*x[1]*(x[2]*x[1] + x[7]) + (1/2)*x[1]*(x[2]*x[1] + x[7])*x[9] + x[6] + x[5]*x[9] ) )^2 +
      
      (N_siblings_adopt_adopt - 3)*(atanh(corr_siblings_adopt_adopt) - atanh(
        (x[8] + x[4])/(1 - 2*x[1]*x[7]) ) )^2 + # now dividing by the adopted var
      
      (N_siblings_adopt_bio - 3)*(atanh(corr_siblings_adopt_bio) - atanh(
        (x[8] + x[4] + x[1]*x[7])/sqrt(1 - 2*x[1]*x[7]) ) )^2 + # now dividing by the adopted SD
      
      (N_siblings_bio_bio - 3)*(atanh(corr_siblings_bio_bio) - atanh(
        x[1]^2*(x[2] - (1/2)) + (1/4)*x[3] + 2*x[1]*x[7] + x[8] + x[4] ) )^2 +
      
      (N_mom_child_adopt - 3)*(atanh(corr_mom_child_adopt) - atanh(
        (x[5] + x[6]*x[9])/sqrt(1 - 2*x[1]*x[7]) ) )^2 + # now dividing by the adopted SD
      
      (N_dad_child_adopt - 3)*(atanh(corr_dad_child_adopt) - atanh(
        (x[6] + x[5]*x[9])/sqrt(1 - 2*x[1]*x[7]) ) )^2 + # now dividing by the adopted SD
      
      (N_dad_mom - 3)*(atanh(corr_dad_mom) - atanh(x[9]))^2					
    
    return(w_sum_sq) }	
  
  best_fit <- nlminb2(x0, OBJ_FUNCTION, constr_function,
                      lower=c(0, 0, 0, 0, -Inf, -Inf, -Inf, 0, -Inf))
  est <- best_fit$par
  
  a <- est["a"]
  q <- est["q"]
  dsq <- est["dsq"]
  ssq <- est["ssq"]
  m <- est["m"]
  p <- est["p"]
  w <- est["w"]
  x <- est["x"]
  mu <- est["mu"]
  
  exp_mom_child_bio <- (1/2)*a*(q*a + w) + (1/2)*a*(q*a + w)*mu + m + p*mu
  exp_dad_child_bio <- (1/2)*a*(q*a + w) + (1/2)*a*(q*a + w)*mu + p + m*mu
  exp_siblings_adopt_adopt <- (x + ssq)/(1 - 2*a*w) # now dividing by adopted var
  exp_siblings_adopt_bio <- (x + ssq + a*w)/sqrt(1 - 2*a*w) # now dividing by adopted SD
  exp_siblings_bio_bio <- a^2*(q - (1/2)) + (1/4)*dsq + 2*a*w + x + ssq
  exp_mom_child_adopt <- (m + p*mu)/sqrt(1 - 2*a*w) # now dividing by adopted SD
  exp_dad_child_adopt <- (p + m*mu)/sqrt(1 - 2*a*w) # now dividing by adopted SD
  exp_dad_mom <- mu	
  
  corr_obs <- c(corr_mom_child_bio, corr_dad_child_bio, corr_siblings_adopt_adopt,
                corr_siblings_adopt_bio, corr_siblings_bio_bio, corr_mom_child_adopt,
                corr_dad_child_adopt, corr_dad_mom)
  
  corr_exp <- c(exp_mom_child_bio, exp_dad_child_bio, exp_siblings_adopt_adopt,
                exp_siblings_adopt_bio, exp_siblings_bio_bio, exp_mom_child_adopt,
                exp_dad_child_adopt, exp_dad_mom)
  
  corr_names <- c("mom_child_bio", "dad_child_bio", "siblings_adopt_adopt",
                  "siblings_adopt_bio", "siblings_bio_bio", "mom_child_adopt",
                  "dad_child_adopt", "corr_dad_mom")
  
  names(corr_obs) <- corr_names
  names(corr_exp) <- corr_names	                	              
  
  boot_est <- matrix(NA, nrow=B, ncol=length(est))
  colnames(boot_est) <- names(est)	              
  
  for(b in 1:B){
    print(b)
    
    bootFAMID <- sample(df$FAMID, replace=TRUE)
    bootd <- subset(df, FAMID==bootFAMID[1])
    for(i in 2:length(bootFAMID)){
      bootd <- rbind(bootd, subset(df, FAMID==bootFAMID[i])) }	        
    
    mom_child_bio <- cor.test(bootd[bootd$IDAB==2,5], bootd[bootd$IDAB==2,4])
    corr_mom_child_bio <- mom_child_bio$estimate
    N_mom_child_bio <- mom_child_bio$parameter + 2
    
    dad_child_bio <- cor.test(bootd[bootd$IDAB==2,6], bootd[bootd$IDAB==2,4])
    corr_dad_child_bio <- dad_child_bio$estimate
    N_dad_child_bio <- dad_child_bio$parameter + 2	
    
    adopt_fam <- subset(bootd, IDFAMAB==1)
    bio_fam <- subset(bootd, IDFAMAB==2)
    mixed_fam <- subset(bootd, IDFAMAB==3)	
    
    siblings_adopt_adopt <- cor.test(adopt_fam[seq(1, nrow(adopt_fam), 2),4], 
                                     adopt_fam[seq(2, nrow(adopt_fam), 2),4])
    corr_siblings_adopt_adopt <- siblings_adopt_adopt$estimate
    N_siblings_adopt_adopt <- siblings_adopt_adopt$parameter + 2
    
    siblings_adopt_bio <- cor.test(mixed_fam[mixed_fam$IDAB==1,4],
                                   mixed_fam[mixed_fam$IDAB==2,4])
    corr_siblings_adopt_bio <- siblings_adopt_bio$estimate    
    N_siblings_adopt_bio <- siblings_adopt_bio$parameter + 2                                	   
    
    siblings_bio_bio <- cor.test(bio_fam[seq(1, nrow(adopt_fam), 2),4],
                                 bio_fam[seq(2, nrow(adopt_fam), 2),4])
    corr_siblings_bio_bio <- siblings_bio_bio$estimate  
    N_siblings_bio_bio <- siblings_bio_bio$parameter + 2  
    
    mom_child_adopt <- cor.test(bootd[bootd$IDAB==1,5], bootd[bootd$IDAB==1,4])
    corr_mom_child_adopt <- mom_child_adopt$estimate
    N_mom_child_adopt <- mom_child_adopt$parameter + 2
    
    dad_child_adopt <- cor.test(bootd[bootd$IDAB==1,6], bootd[bootd$IDAB==1,4])
    corr_dad_child_adopt <- dad_child_adopt$estimate
    N_dad_child_adopt <- dad_child_adopt$parameter + 2
    
    dad_mom <- cor.test(bootd[,5], bootd[,6])
    corr_dad_mom <- dad_mom$estimate 
    N_dad_mom <- dad_mom$parameter + 2
    
    boot_best_fit <- nlminb2(x0, OBJ_FUNCTION, constr_function,
                             lower=c(0, 0, 0, 0, -Inf, -Inf, -Inf, 0, -Inf))
    boot_est[b,] <- boot_best_fit$par } 		
  
  output <- list("est"=est, "corr_obs"=corr_obs, "corr_exp"=corr_exp, "boot_est"=boot_est)
  return(output) } 	                             

# Now for the constraints.
constr_function <- function(x){
  constr <- rbind( x[5]^2 + x[6]^2 + 2*x[5]*x[6]*x[9] - x[8],
                   (1/2)*(x[2]*x[1] + x[7])*x[5] + (1/2)*(x[2]*x[1] + x[7])*x[6] + 
                     (1/2)*(x[2]*x[1] + x[7])*x[9]*x[5] + (1/2)*(x[2]*x[1] + x[7])*x[9]*x[6] - x[7],
                   x[2] - 1 - x[9]*(x[2]*x[1] + x[7])^2,
                   x[3]
  )
  
  # grad <- rbind( c(0, 0, 0, 0, 2*x[5] + 2*x[6]*mu, 2*x[6] + 2*x[5]*mu, 0, -1),
  # c((1/2)*mu*x[2]*x[5] + (1/2)*mu*x[2]*x[6] + (1/2)*x[2]*x[5] + (1/2)*x[2]*x[6],
  # (1/2)*mu*x[1]*x[5] + (1/2)*mu*x[1]*x[6] + (1/2)*x[1]*x[5] + (1/2)*x[1]*x[6],
  # 0, 0, (1/2)*mu*x[1]*x[2] + (1/2)*mu*x[7] + (1/2)*x[1]*x[2] + x[7]/2,
  # (1/2)*mu*x[1]*x[2] + (1/2)*mu*x[7] + (1/2)*x[1]*x[2] + x[7]/2,
  # (1/2)*mu*x[5] + (1/2)*mu*x[6] + x[5]/2 + x[6]/2 - 1, 0),
  # c(-2*mu*x[2]^2*x[1] - 2*mu*x[2]*x[7], 1 - 2*mu*x[1]^2*x[2] - 2*mu*x[1]*x[7],
  # 0, 0, 0, 0, -2*mu*x[1]*x[2] - 2*mu*x[7], 0) )
  
  return(constr) }

# Starting values:

x0 = c(0.55, 1, 0, 0.2, 0.1, 0.1, 0, 0.05, 0.5)
names(x0) <- c("a", "q", "dsq", "ssq", "m", "p", "w", "x", "mu")



