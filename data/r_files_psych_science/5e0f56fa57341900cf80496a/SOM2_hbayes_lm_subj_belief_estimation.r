
rm(list=ls())
gc(reset=T)
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

n_iter <- 10000*2

exp_vector <- c('images','money')
for (i in 1:length(exp_vector)){
  current_exp <- exp_vector[i]

model_dir <- paste0('./stan_models/h_lm_model/')

included_ids <- read.csv(paste0('./data/included_ids_list_',
                                current_exp,'.csv'))

subjective_belief_all <- read.csv(paste0('./data/subjective_value_allIds_', current_exp,'.csv'))
 
subjective_belief_all_noNA <- na.omit(subjective_belief_all)
sb_pos <- subset(subjective_belief_all_noNA, data_type=='positive')
sb_neg <- subset(subjective_belief_all_noNA, data_type=='negative')

#renumerate ids (for fitting)
sb_pos_new <- c()
sb_neg_new <- c()

for (index in 1:nrow(included_ids)){
  id <- included_ids$ID[index]
  print(id)
  sb_pos_subj <- subset(sb_pos, ID==id)
  sb_pos_subj$ID <- index
  sb_pos_new <- rbind(sb_pos_new,sb_pos_subj)
  
  sb_neg_subj <- subset(sb_neg, ID==id)
  sb_neg_subj$ID <- index
  sb_neg_new <- rbind(sb_neg_new,sb_neg_subj)
}

nsubj <- nrow(included_ids)

if (current_exp=='money'){
    sb_pos_new$mean_preference <- sb_pos_new$Number
    sb_neg_new$mean_preference <- sb_neg_new$Number
}

head(sb_pos_new)

names(sb_neg_new)

df_pos <- data.frame(matrix(nrow=nsubj, ncol=7))
colnames(df_pos) <- c('ID','intercept','slope','CI_lower',
                  'CI_upper','CI_lower_intercept','CI_upper_intercept')
df_pos$ID <- 1:nsubj

for (id in 1:nsubj){
      sb_reduced <- subset(sb_pos_new,ID==id)

      lm_sb <- lm(sb_reduced$mean_preference_scaled ~ sb_reduced$SubjectiveBelief)

      CI=confint(lm_sb)
      df_pos$CI_upper[id]= CI[2,2]
      df_pos$CI_lower[id]=CI[2,1]
      df_pos$CI_upper_intercept[id]=CI[1,1]
      df_pos$CI_lower_intercept[id]=CI[1,2]
      df_pos$intercept[id] <- lm_sb$coefficients[1] # intercept
      df_pos$slope[id] <- lm_sb$coefficients[2] # slope
        
  }

write.csv(df_pos,paste0('./results/results_subjective_belief/frequentist/belief_bias_gains_frequentist_', current_exp,'.csv'),row.names=F)

df_neg <- data.frame(matrix(nrow=nsubj, ncol=7))
colnames(df_neg) <- c('ID','intercept','slope','CI_lower',
                  'CI_upper','CI_lower_intercept','CI_upper_intercept')
df_neg$ID <- 1:nsubj

for (id in 1:nsubj){
      sb_reduced <- subset(sb_neg_new,ID==id)

      lm_sb <- lm(sb_reduced$mean_preference_scaled ~ sb_reduced$SubjectiveBelief)

      CI=confint(lm_sb)
      df_neg$CI_upper[id]= CI[2,2]
      df_neg$CI_lower[id]=CI[2,1]
      df_neg$CI_upper_intercept[id]=CI[1,1]
      df_neg$CI_lower_intercept[id]=CI[1,2]
      df_neg$intercept[id] <- lm_sb$coefficients[1] # intercept
      df_neg$slope[id] <- lm_sb$coefficients[2] # slope
        
  }

write.csv(df_neg,paste0('./results/results_subjective_belief/frequentist/belief_bias_losses_frequentist_', current_exp,'.csv'),row.names=F)


x_pre <- list(rep(1,nrow(sb_pos_new)),sb_pos_new$SubjectiveBelief_scaled)
c <- transpose(x_pre)

# Define what information should be passed into Stan
player_dat_pos  = list("D"=2,
                    "L"=nsubj,
                    "N"=nrow(sb_pos_new),
                    "y"=sb_pos_new$mean_preference_scaled, #"y"=sb_pos_new$mean_preference_scaled,
                    "x"=c, 
                    "ll"=sb_pos_new$ID) 

x_pre <- list(rep(1,nrow(sb_neg_new)),sb_neg_new$SubjectiveBelief_scaled)
c <- transpose(x_pre)

# Define what information should be passed into Stan
player_dat_neg  = list("D"=2,
                    "L"=nsubj,
                    "N"=nrow(sb_neg_new),
                    "y"=sb_neg_new$mean_preference_scaled, 
                    "x"=c, 
                    "ll"=sb_neg_new$ID) 

lm_model<-stanc(file="./stan_models/lm_h_model.stan",model_name='my_model') # add an empty line to get rid of warning
compiled_lm_model<-stan_model(stanc_ret=lm_model,verbose=FALSE)

starting_values_fun <- function(){list(mu=c(0,0.5),betasigma=c(0.3,0.3),sigma=(0.3))}

outfile <- paste0('./results/results_subjective_belief/bayesian/estimated_lm_positive_', current_exp,'.rds')

if(!file.exists(outfile)){
    estimated_pos= sampling(compiled_lm_model,data=player_dat_pos, iter=n_iter, chains=2, seed=2,control = list(adapt_delta = 0.99))
    saveRDS(estimated_pos, outfile)

    fit_summary_pos <- summary(estimated_pos)
    fit_summary_pos <- as.data.frame(fit_summary_pos$summary)

    write.csv(fit_summary_pos,paste0('./results/results_subjective_belief/bayesian/estimates_sb_pos_', current_exp,'.csv'),row.names=T)

}else {
    print('File already exists. Skipping estimation.')
}

outfile <- paste0('./results/results_subjective_belief/bayesian/estimated_lm_negative_', current_exp,'.rds')

if(!file.exists(outfile)){
estimated_neg= sampling(compiled_lm_model,data=player_dat_neg, iter=n_iter, chains=2, seed=2,control = list(adapt_delta = 0.99))
saveRDS(estimated_neg, outfile)

fit_summary_neg <- summary(estimated_neg)
fit_summary_neg <- as.data.frame(fit_summary_neg$summary)

convergence_check <- with(fit_summary_neg, Rhat <= 1.05 & Rhat >= (0.95))
ifelse('FALSE' %in% convergence_check ,'check_convergence_negative','convergence_ok_negative')

write.csv(fit_summary_neg,paste0('./results/results_subjective_belief/bayesian/estimates_sb_neg_', current_exp,'.csv'),row.names=T)

convergence_check <- with(fit_summary_pos, Rhat <= 1.05 & Rhat >= (0.95))
ifelse('FALSE' %in% convergence_check ,'check_convergence_positive','convergence_ok_positive')
}else {
    print('File already exists. Skipping estimation.')
}
}
# plot(estimated_pos, plotfun = "trace", pars = c("mu"), inc_warmup = TRUE)
# plot(estimated_pos, plotfun = "hist", pars = c("mu"), inc_warmup = TRUE)
# pairs(estimated_pos, pars=c('beta','lp__'))
# 
# posterior <- as.matrix(estimated_pos)

# plot_title <- ggtitle("Posterior distributions",
#                       "with medians and 90% intervals")
# mcmc_areas(posterior,
#            pars = c("mu[1]", "mu[2]","betasigma[1]","betasigma[2]"),
#            prob = 0.9) + plot_title

# plot(estimated_neg, plotfun = "trace", pars = c("mu"), inc_warmup = TRUE)
# plot(estimated_neg, plotfun = "hist", pars = c("mu"), inc_warmup = TRUE)
# pairs(estimated_neg, pars=c('beta','lp__'))

# posterior <- as.matrix(estimated_neg)

# plot_title <- ggtitle("negterior distributions",
#                       "with medians and 90% intervals")
# mcmc_areas(posterior,
#            pars = c("mu[1]", "mu[2]","betasigma[1]","betasigma[2]"),
#            prob = 0.9) + plot_title
