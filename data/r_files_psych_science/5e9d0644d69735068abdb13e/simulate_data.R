# To run a section: ctrl + alt + t
# To run current line / selection: ctrl + enter (alt + enter, stays on current line)
# To run everything : Ctrl + Alt + R
# Comment selection: Ctrl + Shift + C ----
library(rstanarm)
library(rstan)
setwd("D:/Data/Gidi/CRT")
dat <- read.csv("CRT_finaldata.csv", header=TRUE)
set.seed(123)

# Prep data:
dat$Subject.ID <- paste(dat$Experiment.ID, dat$Subject.ID, sep="*")
dat$Question.ID <- factor(dat$Question.ID)
dat$ConditionT <- 1*(dat$Condition=="Testosterone")
dat$Group.ID <- paste(dat$Experiment.ID, dat$Condition, dat$Blind, dat$Stress, sep="*")
dat$Group.nTE <- paste(dat$Experiment.ID, dat$Blind, dat$Stress, sep="*")
dat$CQ.ID <- paste(dat$Condition, dat$Question.ID, sep="*Q")
dat$EQ.ID <- paste(dat$Experiment.ID, dat$Question.ID, sep="*Q")
dat$GQ.ID <- paste(dat$Group.ID, dat$Question.ID, sep="*Q")
dat$GQ.nTE <- paste(dat$Group.nTE , dat$Question.ID, sep="*Q")
dat$NCQ.ID <- paste(paste0("Nave",dat$Experiment.ID==4), dat$CQ.ID, sep="*")

nave.sel <- dat$Experiment.ID==4

dat$TrtExp <- 1*(dat$Treatment_Expectancy_dichot=="Testosterone")
dat$TrtExp[nave.sel] <- (dat$Treatment_Expectancy[nave.sel] - 1) / 4
dat$Time <- dat$CRT_Time_minnoon / 60

# get proba for treatment and control
p_T = mean(dat$y[dat$ConditionT==1])
p_C = mean(dat$y[dat$ConditionT==0])

logit_p = function(mu){
  return(1/(1+exp(-mu)))
}

# solve mean util for logit 
mu_T = -log( (1/p_T) - 1 )
mu_C = -log( (1/p_C) - 1 )

logit_p(mu_T)
logit_p(mu_C)

T_effect = mu_T-mu_C

# Init result data frame ----
sigma_list = c(0,0.25,0.5,0.75,1.0,1.5)
results <- data.frame(matrix(ncol = 5, nrow = length(sigma_list)))
colnames(results) <- c("q_0.025","q_0.25","q_0.5","q_0.75","q_0.975")

theta.name <- c("CQ.IDPlacebo*QCRT1", "CQ.IDPlacebo*QCRT2", "CQ.IDPlacebo*QCRT3", 
                "CQ.IDTestosterone*QCRT1", "CQ.IDTestosterone*QCRT2", "CQ.IDTestosterone*QCRT3",
                "Sigma[EQ.ID:(Intercept),(Intercept)]", "Sigma[Experiment.ID:(Intercept),(Intercept)]",
                "Sigma[GQ.ID:(Intercept),(Intercept)]", "Sigma[Group.ID:(Intercept),(Intercept)]",
                "Sigma[Subject.ID:(Intercept),(Intercept)]")

# Run models ----
# for(r in 1:length(sigma_list)){
#   # Model + data ----
#   n_T = 439
#   n_C = 432
#   x_T  = c(rep(1,n_T),rep(0,n_C))
#   intercept = sigma_list[r] * rnorm(n_T + n_C)
#   mu_all = intercept + T_effect * x_T
#   prob_all = logit_p(mu_all)
#   obs_1 = runif(n_T+n_C, min = 0, max = 1) < prob_all
#   obs_2 = runif(n_T+n_C, min = 0, max = 1) < prob_all
#   obs_3 = runif(n_T+n_C, min = 0, max = 1) < prob_all
#   
#   dataf = data.frame(y=c(obs_1,obs_2,obs_3),treatment=rep(x_T,3),sub_id = rep(1:(n_T+n_C),3))
#   
#   # Run estim ----
#   library(rstanarm)
#   set.seed(1234)
#   sim_model = stan_glmer(y~ treatment -1 + (1|sub_id),
#                        data=dataf, family="binomial",
#                        cores=n.core, chains=n.chains, iter=n.iter, adapt_delta=a.delta, seed=5230)
#   
#   # process result ----
#   sim_T = extract(sim_model$stanfit,'treatment')
#   median(sim_T$treatment)
#   quantile(sim_T$treatment, probs = c(0.025, 0.25, 0.5,0.75, 0.975))
#   results[r,] = quantile(sim_T$treatment, probs = c(0.025, 0.25, 0.5,0.75, 0.975))
# }
##########################
# Original data without random treatment effect:
a.delta <- 0.99
mm.4.noRandTE <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Subject.ID),
                            data=dat, family="binomial",
                            cores=4, chains=4, iter=5000, adapt_delta=a.delta, seed=5230)

mm.4.noRandTE.GQ <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Subject.ID)+(1|Group.nTE)+(1|GQ.nTE),
                            data=dat, family="binomial",
                            cores=4, chains=4, iter=5000, adapt_delta=a.delta, seed=5230)


NRTE.theta = c("CQ.IDPlacebo*QCRT1", "CQ.IDPlacebo*QCRT2", "CQ.IDPlacebo*QCRT3", 
               "CQ.IDTestosterone*QCRT1", "CQ.IDTestosterone*QCRT2", "CQ.IDTestosterone*QCRT3",
               "Sigma[EQ.ID:(Intercept),(Intercept)]", "Sigma[Experiment.ID:(Intercept),(Intercept)]",
               "Sigma[Subject.ID:(Intercept),(Intercept)]")

summary(mm.4.noRandTE)
NRTE_T = extract(mm.4.noRandTE.GQ$stanfit ,NRTE.theta) 
ch_T <- cbind( NRTE_T[[theta.name[4]]], NRTE_T[[theta.name[5]]], NRTE_T[[theta.name[6]]] )
ch_C <- cbind( NRTE_T[[theta.name[1]]], NRTE_T[[theta.name[2]]], NRTE_T[[theta.name[3]]] )
sim_te =  apply(ch_T - ch_C,1, FUN = mean)
quantile(sim_te, probs = c(0.025, 0.25, 0.5,0.75, 0.975))

##########################
# Run models from modified original data ----


te_list = c(0.0,-0.25,-0.5,-1.0)
sigma_list = c(0,0.25,0.5,0.75,1.0,1.5,2.0)
df_col =  sigma_list[rep(1:7,each=10)]
quant_cols = c("q_0.025","q_0.25","q_0.5","q_0.75","q_0.975")
qsigma_cols = c("sig_0.025","sig_0.25","sig_0.5","sig_0.75","sig_0.975")
full_results <- data.frame(matrix(ncol = 3+5+5+length(theta.name), nrow = length(df_col) * length(te_list)))
colnames(full_results) <- c("sim_n","teff","sigma",quant_cols,qsigma_cols,theta.name)

cur_row = 0

for(t in 1:length(te_list)){
  for(r in 1:length(df_col)){
    cur_row = cur_row + 1
    print(df_col[r])
    set.seed(12345+cur_row)
    
    # Copy data
    modified_dat = dat
    IDs_T = unique(modified_dat$Subject.ID[modified_dat$ConditionT==1])
    IDs_C = unique(modified_dat$Subject.ID[modified_dat$ConditionT==0])
    
    n_T = length(IDs_T)
    n_C = length(IDs_C)
    intercept = df_col[r] * rnorm(n_T + n_C)
    mu_all = intercept
    mu_all[1:n_T] = mu_all[1:n_T] + te_list[t]
    
    df_effect = data.frame(id=c(IDs_T,IDs_C),intercept=intercept,mu=mu_all)
    
    # generate obs
    for(i in 1: nrow(df_effect)){
      ID_i = df_effect$id[i]
      obs_i = runif(3, min = 0, max = 1) < logit_p(df_effect$mu[i])
      modified_dat$y[modified_dat$Subject.ID == ID_i] = obs_i
    }
    
    mm.4.sim <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Group.ID)+(1|GQ.ID)+(1|Subject.ID),
                       data=modified_dat, family="binomial",
                       cores=4, chains=4, iter=5000, adapt_delta=0.995, seed=5230)
    
    # compute quantiles and save
    full_results[cur_row,"sim_n"] = cur_row
    full_results[cur_row,"teff"] = te_list[t]
    full_results[cur_row,"sigma"] = df_col[r]
    sim_T = extract(mm.4.sim$stanfit ,theta.name) 
    ch_T <- cbind( sim_T[[theta.name[4]]], sim_T[[theta.name[5]]], sim_T[[theta.name[6]]] )
    ch_C <- cbind( sim_T[[theta.name[1]]], sim_T[[theta.name[2]]], sim_T[[theta.name[3]]] )
    sim_te =  apply(ch_T - ch_C,1, FUN = mean)
    full_results[cur_row,quant_cols] = quantile(sim_te, probs = c(0.025, 0.25, 0.5,0.75, 0.975))
    full_results[cur_row,qsigma_cols] = quantile(sim_T[["Sigma[Subject.ID:(Intercept),(Intercept)]"]], probs = c(0.025, 0.25, 0.5,0.75, 0.975))
    
    for(th in 1:length(theta.name)){
      th_chain = sim_T[[theta.name[th]]]
      full_results[cur_row,theta.name[th]] = median(th_chain)
    }
    
    print(full_results[cur_row,])
    
    # save
    save(full_results, file = "sim.quant_results.Rdata")
    write.csv(full_results, file = "sim.quant_results.csv",row.names=FALSE)
  }
}

################################
# No random treatment effect----
te_list = c(0.0,-0.25,-0.5,-1.0)
sigma_list = c(0,0.25,0.5,0.75,1.0,1.5,2.0)
df_col =  sigma_list[rep(1:7,each=10)]
quant_cols = c("q_0.025","q_0.25","q_0.5","q_0.75","q_0.975")
qsigma_cols = c("sig_0.025","sig_0.25","sig_0.5","sig_0.75","sig_0.975")
full_results <- data.frame(matrix(ncol = 3+5+5+length(theta.name), nrow = length(df_col) * length(te_list)))
colnames(full_results) <- c("sim_n","teff","sigma",quant_cols,qsigma_cols,theta.name)

cur_row = 0
re_run = 111:120
for(t in 1:length(te_list)){
  for(r in 1:length(df_col)){
    cur_row = cur_row + 1
    if(cur_row %in% re_run){
      print(df_col[r])
      set.seed(12345+cur_row)
      
      # Copy data
      modified_dat = dat
      IDs_T = unique(modified_dat$Subject.ID[modified_dat$ConditionT==1])
      IDs_C = unique(modified_dat$Subject.ID[modified_dat$ConditionT==0])
      
      n_T = length(IDs_T)
      n_C = length(IDs_C)
      intercept = df_col[r] * rnorm(n_T + n_C)
      mu_all = intercept
      mu_all[1:n_T] = mu_all[1:n_T] + te_list[t]
      
      df_effect = data.frame(id=c(IDs_T,IDs_C),intercept=intercept,mu=mu_all)
      
      # generate obs
      for(i in 1: nrow(df_effect)){
        ID_i = df_effect$id[i]
        obs_i = runif(3, min = 0, max = 1) < logit_p(df_effect$mu[i])
        modified_dat$y[modified_dat$Subject.ID == ID_i] = obs_i
      }
      
      mm.4.sim <- stan_glmer(y~CQ.ID-1 + (1|Experiment.ID)+(1|EQ.ID)+(1|Subject.ID)+(1|Group.nTE)+(1|GQ.nTE),
                             data=modified_dat, family="binomial",
                             cores=4, chains=4, iter=5000, adapt_delta=0.995, seed=5230)
      
      # compute quantiles and save
      full_results[cur_row,"sim_n"] = cur_row
      full_results[cur_row,"teff"] = te_list[t]
      full_results[cur_row,"sigma"] = df_col[r]
      sim_T = extract(mm.4.sim$stanfit ,NRTE.theta) 
      ch_T <- cbind( sim_T[[NRTE.theta[4]]], sim_T[[NRTE.theta[5]]], sim_T[[NRTE.theta[6]]] )
      ch_C <- cbind( sim_T[[NRTE.theta[1]]], sim_T[[NRTE.theta[2]]], sim_T[[NRTE.theta[3]]] )
      sim_te =  apply(ch_T - ch_C,1, FUN = mean)
      full_results[cur_row,quant_cols] = quantile(sim_te, probs = c(0.025, 0.25, 0.5,0.75, 0.975))
      full_results[cur_row,qsigma_cols] = quantile(sim_T[["Sigma[Subject.ID:(Intercept),(Intercept)]"]], probs = c(0.025, 0.25, 0.5,0.75, 0.975))
      
      for(th in 1:length(NRTE.theta)){
        th_chain = sim_T[[NRTE.theta[th]]]
        full_results[cur_row,NRTE.theta[th]] = median(th_chain)
      }
      
      print(full_results[cur_row,])
      
      # save
      save(full_results, file = "sim.quant_results.NRTE.Rdata")
      write.csv(full_results, file = "sim.quant_results.NRTE.csv",row.names=FALSE)
    }
  }
}


