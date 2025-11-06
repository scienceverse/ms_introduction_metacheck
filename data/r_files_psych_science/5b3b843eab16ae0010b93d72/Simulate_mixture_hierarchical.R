# ============= Data simulation for a Bayesian hierarchical model =============

# The current script simulates continuous reponses as generated from
# two components: 
#     (1) a von-Mises distribution around a study color, with precision varying 
#         per subject and optionally, per experimental condition.
#     (2) a uniform distribution on the circle, signifying guesses.
# mixing parameter pm indicates the probability that a response comes from a von mises, 
# i.e., from memory. pm varies per subject and optionally, per experimental condition.
# The aim of the Bayesian hierarchical model is to recover two parameters:
# (1) sample_mean_p_mem, (2) sample_mean_kappa, and these parameters can also vary 
# per condition

# The code was written by Dan Biderman, June-July 2018, and follows the 
# simulations conducted by Oberauer et al. (2017) and published at 
# OSF (osf.io/wjg7y/).

# -------------------------------------------------------------------------------------

rm(list=ls())
current_folder = getwd() # to get the correct working directory, you will need to restart RStudio and open this script from the folder of Experiment 2
setwd(current_folder)

# Load packages
library(circular)
library(reshape2)

# Assign parameters for simulation
n_subjects <- 20
n_trials <- 100 # in each condition, for each subject
n_cond <- 1 # insert "1" for a single condition, as in Oberauer et al., (2017). 
# when you insert an integer "n", conditions will be coded 1:n. 
p_mem_value <- c(0.60, 0.65, 0.70, 0.75) # cond 1,..., cond n. Number of elements should match n_cond.
kappa_value <- c(10, 15, 20, 25) # cond 1,..., cond n. Number of elements should match n_cond.
kappa_var <- 25   # variance of kappa (across subjects), same for all conditions

# save room:
p_mem_per_sub <- matrix(nrow = n_cond, ncol = n_subjects)
kappa_per_sub <- p_mem_per_sub

for (cond in 1:n_cond) {
  # draw individual p_mem from a beta distribion centered around the group p_mem
  p_mem_per_sub[cond,] <- rbeta(n_subjects, p_mem_value[cond]*10, (1-p_mem_value[cond])*10)
  # draw individual kappas from a gamma distribion using group mean and variance
  kappa_per_sub[cond,] <- rgamma(n_subjects, (kappa_value[cond]^2)/kappa_var, 
                                 kappa_value[cond]/kappa_var)  # shape = M²/var, rate = M/var
} # cond

# true group values that we need to recover (vector with one value per cond)
sample_mean_p_mem <- apply(p_mem_per_sub, FUN = mean, MARGIN = 1)
sample_mean_kappa <- apply(kappa_per_sub, FUN = mean, MARGIN = 1)

# save room in an array: 
# in each matrix: (1) n_subjects; (2) n_trials; (3) n_cond
study_colors <- array(data = 0, dim=c(n_subjects, n_trials, n_cond))
response_colors <- study_colors # and for response colors

for (c in 1:n_cond) {
  for (s in 1:n_subjects) { 
    study_colors[s,,c] <- runif(n_trials, 0, 2*pi)  # studied colors are uniformly distributed
    binary_ind <- rbinom(n_trials,1,prob = p_mem_per_sub[c,s]) # 1's with prob p_mem, 0's otherwise
    for (t in 1:n_trials) {
      response_colors[s,t,c] <- rvonmises(1, circular(study_colors[s,t,c]), 
                                          binary_ind[t]* kappa_per_sub[c,s])
      # when binary_ind ==0, response is sampled from a uniform. 
      # otherwise, from a von Mises distribution with the current subject's kappa.
    } # n_trials
  } # n_subjects
} # n_cond

# Organize data for stan framework (to run the Bayesian hierarchical model)
melted_resp = melt(response_colors) # cols: (1) sub; (2) trial; (3) cond
melted_study = melt(study_colors) # same
combined_df = cbind(melted_study[c("Var1","Var3", "value")], melted_resp["value"])
colnames(combined_df) = c("subject", "cond", "study_color", "response_color")
mixture_dat_list=as.list(combined_df)
mixture_dat_list$N_trials = length(mixture_dat_list$subject) # n. of trials
mixture_dat_list$n_subjects = length(unique(mixture_dat_list$subject)) # n. of subjects
mixture_dat_list$n_cond = length(unique(mixture_dat_list$cond)) # n. of conditions

# Run stan model
library(rstan)
options(mc.cores = parallel::detectCores())

if (n_cond>1) { # run a Stan model with condition
mixture_multi_level <- stan(file = "mixture_multi_level_cond.stan", 
                            data = mixture_dat_list, iter=2000, chains = 4)
} else { # run a Stan model without condition
  mixture_multi_level <- stan(file = "mixture_multi_level.stan", 
                              data = mixture_dat_list, iter=2000, chains = 4)
}

# PP check
response_color_vec = mixture_dat_list$response_color # vector of observations
post_mat = as.matrix(mixture_multi_level)
y_tilde_mat= post_mat[, which(colnames(post_mat) %in% 
                                grep("y_tilde", colnames(post_mat), value=TRUE))]

# plot simulated data from the model versus original data
library(bayesplot)
color_scheme_set("brightblue")
ppc_dens_overlay(response_color_vec, y_tilde_mat[450:500,])

