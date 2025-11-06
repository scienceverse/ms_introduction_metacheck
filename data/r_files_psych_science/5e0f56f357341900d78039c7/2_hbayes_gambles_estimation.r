
#rm(list=ls())
#gc(reset=T)
# Load the rstan and other package
library(Rcpp)
library(inline)
library(rstan)
library(MASS)
library(ggmcmc)
library(coda)
library(data.table)
library(bayesplot)
library(ggplot2)
library(yarrr)

n_iter <- 5000 #number of iterations for each model

exp_vector <- c('images','money')
for (i in 1:length(exp_vector)){
current_exp <- exp_vector[i]

included_ids <- read.csv(paste0('./data/included_ids_list_',current_exp,'.csv'))

nsubj <- nrow(included_ids)
print(paste0('Total number of included subjects is: ',nsubj))

gambles_reduced <- read.csv(paste0('./data/gambles_all_',current_exp,'.csv'),header=T)

gains <- subset(gambles_reduced,lottery_payoff>0)#choice: 0=safe, 1=lottery
losses <- subset(gambles_reduced, lottery_payoff<0)

#renumerate ids (for fitting)
gains_new <- c()
losses_new <- c()

for (index in 1:nrow(included_ids)){
  id <- included_ids$ID[index]
  print(id)
  gains_subj <- subset(gains, ID==id)
  gains_subj$ID <- index
  gains_new <- rbind(gains_new,gains_subj)
  
  losses_subj <- subset(losses, ID==id)
  losses_subj$ID <- index
  losses_new <- rbind(losses_new,losses_subj)
}

player_dat_gains  = list("amount_lottery_gains"=as.vector(gains_new$lottery_payoff),
                     "prob_lottery_gains"=as.vector(gains_new$lottery_prob),
                     "amb_lottery_gains"=as.vector(gains_new$lottery_amb),
                     "sv_safe"=5,
                   "rawdata"=gains_new$choice, 
                   "items_n_gains"=nrow(gains_new),
                   "id"=gains_new$ID,
                   "nsubj"=nsubj) 

player_dat_losses  = list("amount_lottery_losses"=as.vector(losses_new$lottery_payoff),
                         "prob_lottery_losses"=as.vector(losses_new$lottery_prob),
                         "amb_lottery_losses"=as.vector(losses_new$lottery_amb),
                         "sv_safe"=5,
                         "rawdata"=losses_new$choice, 
                         "items_n_losses"=nrow(losses_new),
                         "id"=losses_new$ID,
                         "nsubj"=nsubj) 

starting_values_fun <- function(){list(alphamu=0,alphasigma=0.5,betamu=0,betasigma=0.5,gammamu=0,gammasigma=0.5)}

outfile <- paste0('./results/results_gambles/bayesian/estimated_gains_bayesian_',current_exp,'.rds')

if(!file.exists(outfile)){
    gains_model<-stanc(file="./stan_models/gains_h_model.stan",model_name='gains') # sadd an empty line to get rid of warning
    gains_compiled<-stan_model(stanc_ret=gains_model,verbose=FALSE)

    estimated_gains= sampling(gains_compiled,data=player_dat_gains, iter=n_iter, chains=2, seed=2,
                         control = list(adapt_delta = 0.999,max_treedepth=15),init=starting_values_fun)

    saveRDS(estimated_gains, outfile)

    fit_summary_gains <- summary(estimated_gains)
    fit_summary_gains <- as.data.frame(fit_summary_gains$summary)

    parameters <- c('alpha','beta','gamma')
    interest <- paste0(parameters,'[',rep(1:nsubj,each=length(parameters)),']')
    estimates_gains <- subset(fit_summary_gains, rownames(fit_summary_gains) %in% interest)

    write.csv(estimates_gains,paste0('./results/results_gambles/bayesian/estimates_gains_',current_exp,'.csv'),row.names=T)

    convergence_check <- with(estimates_gains, Rhat <= 1.05 & Rhat >= (0.95))
    ifelse('FALSE' %in% convergence_check ,'check_convergence_gains','convergence_ok_gains')
}else {
    print('File already exists. Skipping estimation.')
}

outfile <- paste0('./results/results_gambles/bayesian/estimated_losses_bayesian_',current_exp,'.rds')

if(!file.exists(outfile)){
    losses_model <- stanc(file="./stan_models/losses_h_model.stan",model_name='losses') 

    losses_compiled<-stan_model(stanc_ret=losses_model,verbose=FALSE)

    estimated_losses= sampling(losses_compiled,data=player_dat_losses, iter=n_iter,init=starting_values_fun,chains=2,
                           control = list(adapt_delta = 0.99,max_treedepth=15))

    saveRDS(estimated_losses, outfile)

    fit_summary_losses <- summary(estimated_losses)
    fit_summary_losses <- as.data.frame(fit_summary_losses$summary)

    estimates_losses <- subset(fit_summary_losses, rownames(fit_summary_losses) %in% interest)

    write.csv(estimates_losses,paste0('./results/results_gambles/bayesian/estimates_losses_',current_exp,'.csv'),row.names=T)

    convergence_check <- with(estimates_losses, Rhat <= 1.05 & Rhat >= (0.95))
    ifelse('FALSE' %in% convergence_check ,'check_convergence_losses','convergence_ok_losses')
}else {
    print('File already exists. Skipping estimation.')
}
}
# plot(estimated_gains, plotfun = "trace", pars = c("alphamu",'betamu','gammamu'), inc_warmup = TRUE)
# 
# posterior <- as.matrix(estimated_gains)
# 
# plot_title <- ggtitle("Posterior distributions",
#                       "with medians and 90% intervals")
# mcmc_areas(posterior,
#            pars = c("alphamu",'betamu','gammamu'),
#            prob = 0.9) + plot_title
# 
# pairs(estimated_gains, pars=c('alphamu','alphasigma','betamu','betasigma','gammamu','gammasigma','lp__'))

# plot(estimated_losses, plotfun = "trace", pars = c("alphamu",'betamu','gammamu'), inc_warmup = TRUE)
# 
# posterior <- as.matrix(estimated_losses)
# 
# plot_title <- ggtitle("Posterior distributions",
#                       "with medians and 90% intervals")
# mcmc_areas(posterior,
#            pars = c("alphamu",'betamu','gammamu'),
#            prob = 0.9) + plot_title
# 
# pairs(estimated_losses, pars=c('alphamu','alphasigma','betamu','betasigma','gammamu','gammasigma','lp__'))
