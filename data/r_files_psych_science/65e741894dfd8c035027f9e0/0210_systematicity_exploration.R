# systematicity -> exploration 
# NEED to be added to supplementary materials. 
library(tidyr)
library(rstan)
library(bridgesampling) # for bf 
library(dplyr)
library("loo")
options(mc.cores = parallel::detectCores()) 
rstan_options(auto_write = TRUE)

burnin = 1500
niter = 6000
nchains = 4
data1 <- read.csv("/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/processed_data/sys_ent.csv", row.names=1)
data1 <- data1 %>%
  mutate(age_code = as.integer(factor(Age)))


stan_data1 <- list(
  N = nrow(data1),
  Exploration = data1$ent,
  Systematicity = data1$sys_ent,
  Age = data1$age_code,
  K=4
)

model1<- stan_model(file = '/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/systematicity/sys_exp.stan',model_name = 'sys_exp')
fit1 <- sampling(object = model1,
                 data = stan_data1,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE)
summary(fit1)$summary

model2<- stan_model(file = '/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/systematicity/exp_only.stan',model_name = 'exp_only')
fit2 <- sampling(object = model2,
                 data = stan_data1,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE)

model3<- stan_model(file = '/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/systematicity/exp_age.stan',model_name = 'exp_only')
fit3 <- sampling(object = model3,
                 data = stan_data1,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE)

model3a<- stan_model(file = '/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/systematicity/exp_noage.stan',model_name = 'exp_only')
fit3a <- sampling(object = model3a,
                 data = stan_data1,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE)



bridge_null1 <- bridge_sampler(fit2)
bridge_alternative1 <- bridge_sampler(fit1)

# Compute Bayes Factor
bf1 <- bf(bridge_alternative1, bridge_null1)
print(bf1)

bridge_null1 <- bridge_sampler(fit3a)
bridge_alternative1 <- bridge_sampler(fit3)

# Compute Bayes Factor
bf2 <- bf(bridge_alternative1, bridge_null1)
print(bf2)

log_lik_1 <- extract_log_lik(fit1, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_1), cores = 4) 
loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 4)


log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_2), cores = 4) 
loo_2 <- loo(log_lik_2, r_eff = r_eff, cores = 4)

log_lik_3 <- extract_log_lik(fit3, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_3), cores = 4) 
loo_3 <- loo(log_lik_3, r_eff = r_eff, cores = 4)

# comp <- loo_compare(loo_1,loo_2)
# print(comp,simplify=FALSE) 

comp <- loo_compare(loo_2,loo_3)
print(comp,simplify=FALSE) 


# elpd_diff <- loo_1$elpd - loo_2$elpd
# se_elpd_diff <- sqrt(loo_1$se_elpd^2 + loo_2$se_elpd^2)
# bayes_factor <- exp(elpd_diff)
# se_bayes_factor <- bayes_factor * sqrt((exp(se_elpd_diff^2) - 1))
# print(bayes_factor)



# prioritization and exploration 

data2 <- read.csv("/Users/qq/Library/Mobile Documents/com~apple~CloudDocs/feature_sampling/journal/dataAnalysis/1009_freq/processed_data/pri_ent.csv", row.names=1)
data2 <- data2 %>%
  mutate(age_code = as.integer(factor(Age)))


stan_data2 <- list(
  N = nrow(data2),
  Exploration = data2$normed_ent,
  Systematicity = data2$priority_score,
  Age = data2$age_code,
  K=4
)

fit1_pri <- sampling(object = model1,
                 data = stan_data2,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE,
                 control = list(max_treedepth = 15
                 #               adapt_delta=0.99),
                 ))

summary(fit1_pri)
fit2_pri <- sampling(object = model2,
                 data = stan_data2,
                 iter = burnin + niter,
                 warmup = burnin,
                 chains = nchains,
                 verbose = TRUE,
                 control = list(max_treedepth = 15
                                               #               adapt_delta=0.99),
                 ))


log_lik_1 <- extract_log_lik(fit1_pri, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_1), cores = 4) 
loo_1 <- loo(log_lik_1, r_eff = r_eff, cores = 4)


log_lik_2 <- extract_log_lik(fit2_pri, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_2), cores = 4) 
loo_2 <- loo(log_lik_2, r_eff = r_eff, cores = 4)


comp <- loo_compare(loo_1,loo_2)
print(comp,simplify=FALSE) 

elpd_diff <- loo_2$elpd - loo_1$elpd
se_elpd_diff <- sqrt(loo_1$se_elpd^2 + loo_2$se_elpd^2)
bayes_factor <- exp(elpd_diff)
se_bayes_factor <- bayes_factor * sqrt((exp(se_elpd_diff^2) - 1))
print(bayes_factor)