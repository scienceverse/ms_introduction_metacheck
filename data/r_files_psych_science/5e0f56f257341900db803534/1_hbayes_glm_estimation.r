
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #or: setwd(getSrcDirectory()[1])

# Load needed libraries
library(Rcpp)
library(inline)
library(rstan)
library(MASS)
library(ggmcmc)
library(coda)
library(data.table)
library(bayesplot)
library(ggplot2)
library(bayesplot)
library(psych)

n_iter <- 10000*2 #number of samples (warm-up and sampling, 50/50)

#options(repr.matrix.max.cols=50, repr.matrix.max.rows=100) #this option allows to show more lines/rows in jupyter notebook

exp_vector <- c('images','money')
for (i in 1:length(exp_vector)){
  current_exp <- exp_vector[i]

included_ids <- read.csv(paste0('./data/included_ids_list_',current_exp,'.csv'))
print(paste0('Number of included subjects (experiment ', current_exp, ') is: ', nrow(included_ids)))

remfor_all <- read.csv(paste0('./data/remembered_forgotten_allIds_',current_exp,'.csv'))
remfor_pos <- subset(remfor_all,data_type=='positive')
remfor_neg <- subset(remfor_all,data_type=='negative')

remfor_pos_new <- c()
remfor_neg_new <- c()

for (index in 1:nrow(included_ids)){
    id <- included_ids$ID[index]
    print(id)
    remfor_pos_subj <- subset(remfor_pos, ID==id)
    remfor_pos_subj$ID <- index
    remfor_pos_new <- rbind(remfor_pos_new,remfor_pos_subj)
    
    remfor_neg_subj <- subset(remfor_neg, ID==id)
    remfor_neg_subj$ID <- index
    remfor_neg_new <- rbind(remfor_neg_new,remfor_neg_subj)
}

#table(remfor_pos_new$ID)
#table(remfor_neg_new$ID)

nsubj <- nrow(included_ids)

translatedModel<-stanc(file=paste0('./stan_models/glm_h_model.stan'),model_name='my_model') #add an empty line to get rid of warning
compiledModel<-stan_model(stanc_ret=translatedModel,verbose=FALSE)

outfile <- paste0('./results/results_memory_bias/bayesian/estimated_glm_pos_',current_exp,'.rds')

if(!file.exists(outfile)){
    x_pre <- list(rep(1,nrow(remfor_pos_new)),remfor_pos_new$corrected_z_value)
    c <- transpose(x_pre)

    # Define what information should be passed into Stan
    player_dat_pos  = list("D"=2,
                     "L"=nsubj,
                     "N"=nrow(remfor_pos_new),
                     "y"=remfor_pos_new$decided_remembered,
                   "x"=c, 
                   "ll"=remfor_pos_new$ID) 

    starting_values_fun <- function(){list(mu=c(0,0.5),sigma=c(0.3,0.3))}
    estimated_pos= sampling(compiledModel,data=player_dat_pos, iter=n_iter, chains=2, seed=2,
                        control = list(adapt_delta = 0.999),init=starting_values_fun)


    saveRDS(estimated_pos, outfile)
    
    fit_summary_pos <- summary(estimated_pos)
    fit_summary_pos <- as.data.frame(fit_summary_pos$summary)
    fit_summary_pos_focus <- fit_summary_pos[-c(1:4),]
    
    write.csv(fit_summary_pos,paste0('./results/results_memory_bias/bayesian/estimates_mb_pos_',current_exp,'.csv'),
              row.names=T)
    
    convergence_check <- with(fit_summary_pos, Rhat <= 1.05 & Rhat >= (0.95))
    ifelse('FALSE' %in% convergence_check ,'check_convergence_negative','convergence_ok_negative')
    
}else {
    print('File already exists. Skipping estimation.')
}

outfile <- paste0('./results/results_memory_bias/bayesian/estimated_glm_neg_',current_exp,'.rds')

if(!file.exists(outfile)){
    x_pre <- list(rep(1,nrow(remfor_neg_new)),remfor_neg_new$corrected_z_value)
    c <- transpose(x_pre)

    # Define what information should be passed into Stan
    player_dat_neg  = list("D"=2,
                     "L"=nsubj,
                     "N"=nrow(remfor_neg_new),
                     "y"=remfor_neg_new$decided_remembered,
                   "x"=c, 
                   "ll"=remfor_neg_new$ID) 

    starting_values_fun <- function(){list(mu=c(0,0.5),sigma=c(0.3,0.3))}
    estimated_neg= sampling(compiledModel,data=player_dat_neg, iter=n_iter, chains=2, seed=2,control = list(adapt_delta = 0.999),init=starting_values_fun)#,estimated

    saveRDS(estimated_neg, outfile)
    
    fit_summary_neg <- summary(estimated_neg)
    fit_summary_neg <- as.data.frame(fit_summary_neg$summary)
    fit_summary_neg_focus <- fit_summary_neg[-c(1:4),]
    
    write.csv(fit_summary_neg,paste0('./results/results_memory_bias/bayesian/estimates_mb_neg_',current_exp,'.csv'),
              row.names=T)
    
    convergence_check <- with(fit_summary_neg, Rhat <= 1.05 & Rhat >= (0.95))
    ifelse('FALSE' %in% convergence_check ,'check_convergence_positive','convergence_ok_positive')
     
}else {
    print('File already exists. Skipping estimation.')
}

#plot(estimated_pos, plotfun = "trace", pars = c("mu"), inc_warmup = TRUE)

#posterior <- as.matrix(estimated_pos)

#plot_title <- ggtitle("Posterior distributions",
#                      "with medians and 90% intervals")
#mcmc_areas(posterior,
#           pars = c("mu[1]", "mu[2]","sigma[1]","sigma[2]"),
#          prob = 0.9) + plot_title

#pairs(estimated_pos, pars=c('mu','sigma','lp__'))

#plot(estimated_neg, plotfun = "trace", pars = c("mu"), inc_warmup = TRUE)

#posterior <- as.matrix(estimated_neg)

#plot_title <- ggtitle("Posterior distributions",
#                      "with medians and 90% intervals")
#mcmc_areas(posterior,
#           pars = c("mu[1]", "mu[2]","sigma[1]","sigma[2]"),
#           prob = 0.9) + plot_title

#pairs(estimated_neg, pars=c('mu','sigma','lp__'))
}