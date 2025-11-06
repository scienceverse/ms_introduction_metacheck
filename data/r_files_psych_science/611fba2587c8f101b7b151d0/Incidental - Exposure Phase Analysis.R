################################################
################ DESCRIPTION ###################
################################################

#Data come from a set of experiments. In total, this research consisted of 5 experiments. 
#However, only Experiments 1 - 3 included an "Exposure Phase" that was analysed using this
#script.
#In the Exposure phase, participants completed a cover task. During the cover task, participants saw a
#stimulus "jump" to either the left or right. After the stimulus had jumped, participants responded
#indicating whether it jumped to the left or right.
#Across experiments, participants completed this cover task under one of two exposure conditions: 
#Baseline, in which the stimuli had no category structure, and Incidental,
#in which, unbeknownst to participants, the stimuli belonged to one of two categories.
#In Experiment 1, categories were either Dense or Sparse. Data for these two "category structure" conditions
#are stored as Experiment1A and Experiment1B. Only Dense categories were used in Experiments 2 and 3. 

#Goal of analyses: Test whether exposure condition affected performance on the cover task. 
#Specifically, test whether incidental exposure to dense or sparse categories caused participants to speed 
#up more rapidly than baseline exposure.

#Data for each experiment are stored in a long- format dataframe. Important columns are:
#ID: randomly assigned ID of the participant as an alphanumeric string
#Condition: combined exposure and category structure condition info
#Accuracy: 0 or 1
#Running.Trial: Trial 1-80 of Exposure phase  
#RT: Reaction time in milliseconds

# Other columns not used for analyses include:
# Block: Block 1-8 of exposure phase
# Trial: Trial 1-10 within Block
# Limit: Upper limit on RT before trial ended (Experiments 1-2 only)

#Steps currently in script:
#(1) Define a set of new functions that will be used in the script
#(2) Define a set of hierarchical Bayesian models that will be used to analyze the data. In these models,
#    the outcome variable (RT or Accuracy) is predicted by trial number. Each participant has a slope for 
#    change in the outcome variable across trials. Participant slopes are drawn from one of two distributions,
#    depending on whether they belonged to the Baseline or Incidental condition. For each iteration, the model
#    compares the Baseline and Incidental slope distributions. This comparison is used to compare whether changes
#    in RT or Accuracy across trials vary across the two conditions.
#(3) Load and format RT and Accuracy data from each experiment.
#(4) Fit the models to the data
#(5) From the models, extract differences between the distributions for slopes in the two
#    conditions for each sample from the posterior. 
#(6) Evaluate differences between slope distributions between the two conditions for each
#    experiment.
#(7) Genrate graphs, including graphs presented in the manuscript, and additional graphs that can be used to
#    explore the data, the predictions of the models, and comparisons between observed & predicted data.

################################################
################# PACKAGES #####################
################################################

library(stringr)
library(psycho)

library(plyr)
library(dplyr)
library(tidyr)
library(plyr)

library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpubr)

library(rstan)
library(bayestestR)
library(ggmcmc)

rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
options(mc.cores = 3)

################################################
################# FUNCTIONS ####################
################################################

# This section defines functions that will be used in the script.

############## FORMATTING DATA #################

# Replace inaccurate and long RTs (> 2000ms) with NA for experiments with no limits
# on RT
rt.filter <- function(input.sub) {
  upper.limit <- 2000
  input.sub$RT.filter <- ifelse(input.sub$RT > upper.limit | input.sub$Accuracy == 0, NA, input.sub$RT)
  return(input.sub)
}

# Generate object in which RT and acc data used to fit models is in list
# format used as input for stan models
gen.rt.data <- function(experiment) {
  dat_experiment <- list(num_sub = length(unique(experiment$ID.num)), #number of participants
                         num_trials = length(unique(experiment$Running.Trial)), #number of trials for each participant
                         num_conditions = length(unique(experiment$Condition.num)), #number of conditions
                         sub_condition = as.integer(ddply(experiment, .(ID), summarise, Cond = unique(Condition.num))$Cond), #condition for each participant
                         trial = matrix(experiment$Running.Trial, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)),byrow=T), #matrix of trial numbers for each participant
                         trial_acc = matrix(experiment$Acc.num, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)),byrow=T), #matrix of trial accuracies for each participant
                         rt = matrix(experiment$RT, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)), byrow=T) #matrix of rts for each participant
  )
}

gen.acc.data <- function(experiment) {
  dat_experiment <- list(num_sub = length(unique(experiment$ID.num)), #number of participants
                         num_trials = length(unique(experiment$Running.Trial)), #number of trials for each participant
                         num_conditions =length(unique(experiment$Condition.num)), #number of conditions
                         sub_condition = as.integer(ddply(experiment, .(ID), summarise, Cond = unique(Condition.num))$Cond), #condition for each participant
                         trial = matrix(experiment$Running.Trial, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)),byrow=T), #matrix of trial numbers for each participant
                         acc = matrix(experiment$Accuracy, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)), byrow=T) #matrix of accuracies for each participant
  )
}

######### SIGNAL DETECTION MEASURES ############

# For experiments that used a 1-back task, for each participant,
# calculate numbers of hits (detecting repetitions),
# correct rejections (correct rejections of non-repetitions), 
# false alarms (identifying non-repetitions as repetitions), and
# misses (failing to detect repetitions)
# Use to calculate hit rates and false alarm rates, hit - false alarms,
# and dprime

extract_signal <- function(input) {
  n_hit <- length(input$Accuracy[input$Rep==1&input$Acc==1])
  n_cr <- length(input$Accuracy[input$Rep==0&input$Acc==1])
  n_fa <- length(input$Accuracy[input$Rep==0&input$Acc==0])
  n_miss <- length(input$Accuracy[input$Rep==1&input$Acc==0])
  hit <- n_hit/(n_hit+n_miss)
  fa <- n_fa/(n_fa+n_cr)
  indeces <- psycho::dprime(n_hit, n_fa, n_miss, n_cr)
  d.prime <- indeces$dprime
  hit_fa <- hit-fa
  output <- data.frame(hit, fa, d.prime, hit_fa)
  return(output)
}


####### EXTRACT MODEL-PREDICTED OUTCOMES #######

# This function takes data from an experiment, and the model fit for RT for an experiment, 
# and matches up observed with model predicted RTs
obs_pred <- function(input.data = NULL, input.fit, downsample.size = 500, RT.filter = FALSE) {
  pred_rt <- data.frame(as.matrix(input.fit, pars = "pred_rt"))
  pred_rt <- pred_rt[sample(1:nrow(pred_rt), downsample.size),]
  pred_rt <- gather(pred_rt, Obs, RT.pred)
  pred_rt$Obs <- str_replace_all(pred_rt$Obs, c("pred_rt." = "", ".$" = ""))
  
  if(RT.filter) {
    key <- subset(input.data, !is.na(RT.filter), select = c(ID, Condition, Condition.num, Block, Running.Trial, RT.filter))
    key$Obs <- 1:nrow(key) 
    data_pred <- merge(pred_rt, key, by = "Obs")
  } else {
    key <- subset(input.data, select = c(ID, ID.num, Condition, Condition.num, Block, Running.Trial, RT))
    pred_rt$Running.Trial <- as.integer(str_replace_all(pred_rt$Obs, c("^[[:digit:]]{1,2}." = "")))
    pred_rt$ID.num <- as.integer(str_replace_all(pred_rt$Obs, c(".[[:digit:]]{1,2}$" = "")))
    pred_rt$Obs <- NULL
    data_pred <- merge(key, pred_rt, by = c("ID.num", "Running.Trial"))
  }
  return(data_pred)
}

# Function to get credible interval of outcomes predicted by models to use
# when plotting predictions against observed data
credible <- function(x) {
  data.frame(ymin=bayestestR::ci(x, ci=.1)$CI_low,
             ymax=bayestestR::ci(x, ci=.1)$CI_high)
}

# Generate graphs of posterior distributions of differences between conditions
plot_diff <- function(input.fit, parameter, conditions, density.color) {
  input.name <- deparse(substitute(input.fit))
  experiment <- str_replace_all(input.name, "fit_slope_rt_|A|B", "")
  experiment <- paste("(", experiment, ")", sep="")
  post_diff <- data.frame(as.matrix(input.fit, pars=parameter))
  names(post_diff) <- c("post_diff")
  density_diff <- with(density(post_diff$post_diff), data.frame(x, y))
  ci_diff <- bayestestR::ci(post_diff$post_diff, method="HDI")
  ggplot(post_diff, aes(x=post_diff)) +
    geom_density(color=rgb(0,0,0,0), fill=density.color, size=1, alpha=.2) +
    scale_x_continuous(name=paste("RT Advantage for Incidental Exposure")) +
    scale_y_continuous(name=paste(conditions, experiment, sep=" ")) +
    geom_area(data= density_diff[density_diff$x>ci_diff$CI_low & density_diff$x<ci_diff$CI_high,], aes(x=x, y=y), fill = density.color, alpha=.5) +
    geom_vline(xintercept=0, linetype="dashed", color="black", size = .9) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=8),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

# Generate graphs of posterior distributions of differences between conditions
# to embed in graphs of data
plot_diff_embed <- function(input.fit, parameter, conditions, density.color) {
  post_diff <- data.frame(as.matrix(input.fit, pars=parameter))
  names(post_diff) <- c("post_diff")
  density_diff <- with(density(post_diff$post_diff), data.frame(x, y))
  ci_diff <- bayestestR::ci(post_diff$post_diff, method="HDI", ci = .89)
  ggplot(post_diff, aes(x=post_diff)) +
    geom_density(color=rgb(0,0,0,0), fill=density.color, size=1, alpha=.1) +
    scale_x_continuous(name=paste("Category vs Baseline")) +
    scale_y_continuous(name="") +
    geom_area(data= density_diff[density_diff$x>ci_diff$CI_low & density_diff$x<ci_diff$CI_high,], aes(x=x, y=y), fill = density.color, alpha=.8) +
    geom_vline(xintercept=0, linetype="dashed", color="black", size = .7) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"), 
          axis.title=element_text(size=8, face="bold"),
          axis.text=element_text(size=8),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}


plot_data <- function(input.data, outcome, incidental.color, rt.limits) {
  input.name <- deparse(substitute(input.data))
  experiment <- str_replace_all(input.name, "exp", "Experiment ")
  experiment <- str_replace_all(experiment, "A|B", "")
  #experiment <- paste("(", experiment, ")", sep="")
  #incidental <- ifelse(grepl("B", input.name), "- Sparse", "- Dense")
  #experiment <- paste(experiment, incidental, " vs Baseline")
  ggplot(input.data[input.data$Accuracy == 1,], aes_string(x = "Block", y = outcome)) +
    stat_summary(aes(color = Condition), geom = "line", fun = "mean", position = position_dodge(width = .1), show.legend=F) +
    stat_summary(aes(group = Condition), geom = "linerange", fun.data = "mean_se", position = position_dodge(width = .1), alpha = .8, color = "black", show.legend=F) +
    #stat_summary(aes(group = Condition, color = Condition), geom = "point", fun = "mean", position = position_dodge(width = .1), show.legend=F) +
    scale_x_continuous(name = "Exposure Phase Block", breaks = 1:8) +
    scale_y_continuous(name = paste(experiment, "\nRT on Accurate Trials")) +
    scale_color_manual(values = c(incidental.color, "grey")) +
    scale_fill_manual(values = c(incidental.color, "grey")) +
    coord_cartesian(ylim = rt.limits) +
    #geom_hline(yintercept = .5, linetype = "dashed") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=8))
}

################################################
################## MODELS ######################
################################################
# Model for change in accuracy across trials in experiments with limits on RT, in which
# slopes for change in accuracy accuracy across trials are drawn from different distributions 
# based on between-subjects condition.
model_slope_acc = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> sub_condition[num_sub]; // condition for each subject
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor
  int<lower=0,upper=1> acc[num_sub,num_trials]; // accuracies (binary integers - 0 or 1)
  }
parameters {
  real sub_alpha[num_sub]; // Intercept for each participant
  real sub_slope[num_sub]; // Slope for trial number for each particpant. To be drawn from one of two distributions, based on sub_condition
  
  real hyper_alpha;  // mean of the single distribution for subject intercepts
  real<lower=0> hyper_alpha_sigma2; // variance of the single distribution for subject intercepts
  
  real condition_slope_mean[num_conditions];   // means of the 2 distributions for for subject trial slopes
  real<lower=0> condition_slope_sigma2[num_conditions];  // variances of the 2 distributions for subject trial slopes
  }

transformed parameters {
  real<lower=0> hyper_alpha_sigma;
  real<lower=0> condition_slope_sigma[num_conditions];
  
  hyper_alpha_sigma = sqrt(hyper_alpha_sigma2);
  condition_slope_sigma = sqrt(condition_slope_sigma2);
  }

model {
// Priors for hyperparameters
  hyper_alpha ~ normal(0,1/.01);
  condition_slope_mean ~ normal(0,1/.01);
  
  hyper_alpha_sigma2 ~ inv_gamma(.01,.01);
  condition_slope_sigma2 ~ inv_gamma(.01,.01);


// Priors for intercepts and slopes for each participant
// Use this to draw intercept from one distribution, and slope from one of two distributions based on participant condition
  for (i in 1:num_sub){                           
    sub_alpha[i] ~ normal(hyper_alpha,hyper_alpha_sigma);
    sub_slope[i] ~ normal(condition_slope_mean[sub_condition[i]],condition_slope_sigma[sub_condition[i]]);

// Likelihood for accuracy: Predicted by participant's intercept, their slope for trial type, and their slope for trial num
    for (j in 1:num_trials){                         
      acc[i,j] ~ bernoulli_logit(sub_alpha[i] + sub_slope[i] * trial[i,j]);
    }
  }
}

generated quantities {
  real post_slope_diff; // difference between means for slopes for baseline vs dense or sparse conditions
  real ppd_mean_acc[num_sub]; // vector of predicted mean accuracies
  
  post_slope_diff = condition_slope_mean[2] - condition_slope_mean[1];
   
  for (i in 1:num_sub) {
    vector[num_trials] ppd_accuracies; //vector of predicted accuracies for each trial
    for (j in 1:num_trials) {
      ppd_accuracies[j] = bernoulli_logit_rng(sub_alpha[i] + sub_slope[i] * trial[i,j] ); //predicted acc on the jth trial for the ith subject
    }
    ppd_mean_acc[i] = mean(ppd_accuracies); //predicted mean acc for the ith subject
  }
}
"

# Model for change in RT across trials in experiments with progressively decreasing limits on RT, 
# in which changes in RT across trials were approximately linear. 
# Due to linear changes in RT, this model predicts RT for each trial for each participant as the 
# outcome of a linear regression with an intercept and slope for trial number. To test condition
# differences, slopes for change in RT across trials are sampled from different distributions based
# on between-subjects condition.
model_slope_rt = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> sub_condition[num_sub]; // condition for each subject to be used as *indeces* to identify params for different conditions
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor - matrix of trial numbers for each subject
  int<lower=0> trial_acc[num_sub,num_trials]; // accuracies for each trial to be used as *indeces* to identify separate params for acc & inacc trials. So acc = 1, inacc = 2
  real<lower=0> rt[num_sub,num_trials]; // reaction times
  }
parameters {
  real sub_alpha[num_sub];  // Intercept for each participant
  vector[num_sub] sub_condition_slope; // Participants' trial number slopes for accurate trials only. To be drawn from one of two distributions, based on sub_condition
  vector[num_sub] sub_inacc_slope; // Participants' trial number slopes for inaccurate trials
  
  real hyper_alpha_mean;  // mean of the distribution for subject intercepts
  real<lower=0> hyper_alpha_sigma2; // variance of the distribution for subject intercepts
  
  real condition_slope_mean[num_conditions]; // means of the 2 distributions for particpant trial number slopes***
  real<lower=0> condition_slope_sigma2[num_conditions]; // variances of the 2 distributions for particpant trial number slopes
  
  real inacc_slope_mean; // Mean of distribution for participant trial number slopes on *inaccurate* trials
  real<lower=0> inacc_slope_sigma2; // Variance of distribution for participant trial number slopes on *inaccurate* trials
  
  real<lower=0> rt_sigma2; // Error term for predicting trial-level RTs
  }

transformed parameters {
  real<lower=0> hyper_alpha_sigma;
  real<lower=0> condition_slope_sigma[num_conditions];
  real<lower=0> inacc_slope_sigma;
  
  real<lower=0> rt_sigma;
  
  matrix[num_sub, 2] sub_slope; // Trial number slopes for each participant. [,1] = slope for accurate trials based on condition; [,2] = slope for inaccurate trials
 
  hyper_alpha_sigma = sqrt(hyper_alpha_sigma2);
  condition_slope_sigma = sqrt(condition_slope_sigma2);
  inacc_slope_sigma = sqrt(inacc_slope_sigma2);
  
  rt_sigma = sqrt(rt_sigma2);
  
  sub_slope = append_col(sub_condition_slope, sub_inacc_slope); // For each subject, combine their slope for accurate trials based on condition, and their slope for inaccurate trials
  }

model {
// Declare predicted rt variable
  real rt_mu[num_sub, num_trials];

// Priors for hyperparameters
  hyper_alpha_mean ~ normal(0,1/.01);
  condition_slope_mean ~ normal(0,1/.01);
  inacc_slope_mean ~ normal(0,1/.01);
  
  hyper_alpha_sigma2 ~ inv_gamma(.01,.01);
  condition_slope_sigma2 ~ inv_gamma(.01,.01);
  inacc_slope_sigma2 ~ inv_gamma(.01,.01);
  rt_sigma2 ~ inv_gamma(.01,.01);
  
// Priors for subject intercept and slope means and variances
  for (i in 1:num_sub){
    sub_alpha[i] ~ normal(hyper_alpha_mean, hyper_alpha_sigma);
    sub_condition_slope[i] ~ normal(condition_slope_mean[sub_condition[i]], condition_slope_sigma[sub_condition[i]]);
    sub_inacc_slope[i] ~ normal(inacc_slope_mean, inacc_slope_sigma);
  }

// Likelihood for reaction time: Predicted by participant's intercept, their slope for accurate trials (based on condition), and their slope for inacc trials
  for (i in 1:num_sub){                           
    for (j in 1:num_trials){
      rt_mu[i,j] = sub_alpha[i] + sub_slope[i,trial_acc[i,j]] * trial[i,j];
      rt[i,j] ~ normal(rt_mu[i,j], rt_sigma);
    }
  }
}

generated quantities {
  real post_slope_diff; // difference between means for slopes for baseline vs dense or sparse conditions
  matrix[num_sub,num_trials] pred_rt; // matrix of predicted RTs for each subject/trial
  
  post_slope_diff = condition_slope_mean[1] - condition_slope_mean[2];
    
  for (i in 1:num_sub) {
    for (j in 1:num_trials) {
      pred_rt[i,j] = normal_rng(sub_alpha[i] + sub_slope[i,trial_acc[i,j]] * trial[i,j], rt_sigma); // predicted RT on jth trial for the ith subject
    }
  }
}
"


model_exponent_rt <- "
data {
  // Observed data
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_obs; // total number of observations across subjects
  int<lower=1> num_trials; // number of trials in exposure phase
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> sub_condition[num_sub]; // condition for each subject to be used as *indeces* to identify params for different conditions
  int<lower=1> id[num_obs]; // subject id for each observation
  int<lower=1> trial[num_obs]; // trial number predictor across all observations
  real<lower=0> rt[num_obs]; // reaction times across all observations
}

parameters{
  // Participant-level asymptote, gain, rate, and noise  
  real<upper=2000>              sub_asymptote[num_sub];
  real<lower=200, upper=20000>  sub_start[num_sub];
  real<lower=0, upper=50>       sub_rate[num_sub];
    
  real<lower=0>   rt_sigma2; // RT noise
  
  // Group-level asymptote, gain, and rate  
  real<upper=2000>             condition_asymptote_mean[num_conditions];
  real<lower=200, upper=2000>  condition_start_mean[num_conditions];
  real<lower=0, upper=50>      condition_rate_mean[num_conditions];
  
  real<lower=0>   condition_asymptote_sigma2[num_conditions];
  real<lower=0>   condition_start_sigma2[num_conditions];
  real<lower=0>   condition_rate_sigma2[num_conditions];

}

transformed parameters {
  real<lower=0> rt_sigma;

  real<lower=0>   condition_asymptote_sigma[num_conditions];
  real<lower=0>   condition_start_sigma[num_conditions];
  real<lower=0>   condition_rate_sigma[num_conditions];
   
  rt_sigma = sqrt(rt_sigma2);
   
  condition_asymptote_sigma = sqrt(condition_asymptote_sigma2);
  condition_start_sigma = sqrt(condition_start_sigma2);
  condition_rate_sigma = sqrt(condition_rate_sigma2);
  
}

model {
  // Latent RT
  real rt_mu[num_obs];
  
  rt_sigma2 ~ inv_gamma(.01,.01);
  
  // Group-level parameters
  for(c in 1:num_conditions) {
    condition_asymptote_mean[c] ~ normal( 400 , 1/.001 ) T[0 , 2000]; 
    condition_start_mean[c] ~     normal( 1000 , 1/.001 ) T[200, 2000];
    condition_rate_mean[c] ~      normal( 1, 1/.03 ) T[0, 50];
  
    condition_asymptote_sigma2[c] ~ inv_gamma(.01,.01);
    condition_start_sigma2[c] ~ inv_gamma(.01,.01);
    condition_rate_sigma2[c] ~ inv_gamma(.01,.01);
  }
  
  // Participant-level parameters
  
  for(i in 1:num_sub) {
    sub_asymptote[i] ~ normal( condition_asymptote_mean[sub_condition[i]], condition_asymptote_sigma[sub_condition[i]]);
    sub_start[i] ~ normal( condition_start_mean[sub_condition[i]], condition_start_sigma[sub_condition[i]]);
    sub_rate[i] ~ normal( condition_rate_mean[sub_condition[i]], condition_rate_sigma[sub_condition[i]]);
    
  }
  
  //Likelihood: Loop over observations
  for (i in 1:num_obs) {

    rt_mu[i] = sub_asymptote[id[i]] - (sub_asymptote[id[i]] - sub_start[id[i]]) * exp( (1 - trial[i])/sub_rate[id[i]] );

    rt[i] ~   normal( rt_mu[i] , rt_sigma );
    }
}

generated quantities {
  real condition_gain_mean[num_conditions];
  real sub_gain[num_sub];
  real pred_mu[num_obs];
  real pred_rt[num_obs]; 
  real log_lik[num_obs];
  real sub_slope_trial[num_obs];
  real slope_trial_base[num_trials];
  real slope_trial_dense[num_trials];
  real slope_mean_base;
  real slope_mean_dense;
  real slope_head_base;
  real slope_head_dense;
  real slope_tail_base;
  real slope_tail_dense;
  real slope_diff;
  real slope_head_diff;
  real slope_tail_diff;
  real asymptote_diff; 
  real gain_diff;
  real rate_diff;
  
  
  // Calculate gains
  for (c in 1:num_conditions) {
    condition_gain_mean[c] = condition_asymptote_mean[c] - condition_start_mean[c];
  }
  
  for (i in 1:num_sub) {
    sub_gain[i] = sub_asymptote[i] - sub_start[i];
  }
  
  // Calculate trial slopes
  for(t in 1:num_trials) {
    slope_trial_base[t] = (condition_gain_mean[1] * exp( (1 - trial[t])/condition_rate_mean[1] )) / condition_rate_mean[1];
    slope_trial_dense[t] = (condition_gain_mean[2] * exp( (1 - trial[t])/condition_rate_mean[2] )) / condition_rate_mean[2];
  }
  
  
  slope_mean_base = mean(slope_trial_base);
  slope_mean_dense = mean(slope_trial_dense);
  slope_head_base = mean( head(slope_trial_base, 40) );
  slope_head_dense = mean( head(slope_trial_dense, 40) );
  slope_tail_base = mean( tail(slope_trial_base, 40) );
  slope_tail_dense = mean( tail(slope_trial_dense, 40) );
  
  slope_diff = slope_mean_base - slope_mean_dense;
  slope_head_diff = slope_head_base - slope_head_dense;
  slope_tail_diff = slope_tail_base - slope_tail_dense;
  
  asymptote_diff = condition_asymptote_mean[1] - condition_asymptote_mean[2];
  gain_diff =      condition_gain_mean[1] - condition_gain_mean[2];
  rate_diff =      condition_rate_mean[1] - condition_rate_mean[2];
  
  for (i in 1:num_obs){ 
    pred_mu[i] = sub_asymptote[id[i]] - ( sub_asymptote[id[i]] - sub_start[id[i]] ) * exp( (1 - trial[i])/sub_rate[id[i]] );
    pred_rt[i] = normal_rng(pred_mu[i], rt_sigma );
    log_lik[i] = normal_lpdf(rt[i] | pred_mu[i], rt_sigma );
    sub_slope_trial[i] = (sub_gain[id[i]] * exp( (1 - trial[i])/sub_rate[id[i]] )) / sub_rate[id[i]];
  }
}
"


################################################
################### DATA #######################
################################################

# This section reads in and formats data so that it can be fit using rstan. 
# Note that the data for Experiment 1 is divided into two objects: exp1A, 
# which is the Dense Structure condition, and exp1B, which is the Sparse Structure condition.

setwd("LOCATION")

exp1A <- readRDS(file="Exposure - 1A.rds")
exp1B <- readRDS(file="Exposure - 1B.rds")
exp2 <- readRDS(file="Exposure - 2.rds")
exp3 <- readRDS(file="Exposure - 3 - All RTs.rds")

#Filter RTs in experiment 3
exp3 <- ddply(exp3, .(ID), rt.filter)

#Re-code IDs as integers starting at 1
exp1A$ID.num <- as.integer(as.factor(exp1A$ID))
exp1B$ID.num <- as.integer(as.factor(exp1B$ID))
exp2$ID.num <- as.integer(as.factor(exp2$ID))
exp3$ID.num <- as.integer(as.factor(exp3$ID))

#Re-code Conditions as integers starting at 1
exp1A$Condition.num <- as.integer(ifelse(exp1A$Condition=="Base Dense",1,2))
exp1B$Condition.num <- as.integer(ifelse(exp1B$Condition=="Base Sparse",1,2))
exp2$Condition.num <- as.integer(ifelse(exp2$Condition=="Base Dense",1,2))
exp3$Condition.num <- as.integer(ifelse(exp3$Condition=="Base Dense",1,2))

#Re-code Accuracies as integers starting at 1, in which 1 = acc, 2 = inacc
exp1A$Acc.num <- as.integer(ifelse(exp1A$Accuracy==1,1,2))
exp1B$Acc.num <- as.integer(ifelse(exp1B$Accuracy==1,1,2))
exp2$Acc.num <- as.integer(ifelse(exp2$Accuracy==1,1,2))

#Sort the dataframe by particpant number, then by trial number
#This is useful for generating a vector of participant conditions in which participants
#are in the same order as they are in the dataframe
exp1A <- exp1A[with(exp1A, order(ID.num, Running.Trial)),]
exp1B <- exp1B[with(exp1B, order(ID.num, Running.Trial)),]
exp2 <- exp2[with(exp2, order(ID.num, Running.Trial)),]
exp3 <- exp3[with(exp3, order(ID.num, Running.Trial)),]


#Data used to fit the models

#Note that the data are in the same format for experiments 1A, 1B, and 2, but slightly different for experiment 3. 
#This is because experiments 1A, 1B, and 2 had progressively strict limits on reaction time, and therefore: 
#(1) Have a sizable minority of inaccurate trials (~15%-20%), and (2) No outlier reaction times.
#In contrast, experiment 3 had no reaction time limit, and therefore has the 
#opposite pattern: (1) Almost no inaccurate trials (0.6%), and (2) Some outlier reaction times.

#Therefore, RT data used to fit models from experiments 1A, 1B, and 2 include all RTs, such that
#inaccurate trials are simply fit to a different distribution from accurate trials. In
#addition, accuracy data from these experiments are also used to fit a model for accuracy.
#In contrast, RT data used to fit models from experiment 3 include only accurate trial RTs, and
#inaccurate trials are not modeled. 

#In addition, the progressively strict RT limits in experiments 1A, 1B, and 2 meant that changes in RT
#over trials in these experiments were approximately linear. Therefore, these changes were modeled 
#using linear regression.
#In contrast, without RT limits in experiment 3, changes in RT showed a typical pattern of speeding up
#rapidly at first, and then asymptoting. Therefore, these changes were modeled by predicting RT as the
#outcome of an asymptotic function with intercept, rate, and asymptote parameters.

#Reaction time
dat_rt_exp1A <- gen.rt.data(exp1A)
dat_rt_exp1B <- gen.rt.data(exp1B)
dat_rt_exp2 <- gen.rt.data(exp2)
dat_rt_exp3 <- list(num_sub = length(unique(exp3$ID.num)), #number of participants
                    num_obs = nrow(exp3[!(is.na(exp3$RT.filter)),]), #total number of non-filtered RT observations across particpants
                    num_trials = length(unique(exp3$Running.Trial)), # number of trials in exposure phase
                    num_conditions = length(unique(exp3$Condition.num)), #number of conditions
                    sub_condition = as.integer(ddply(exp3, .(ID), summarise, Cond = unique(Condition.num))$Cond), #condition for each participant
                    id = exp3$ID.num[!is.na(exp3$RT.filter)], #vector of ids across observations for non-filtered RTs
                    trial = exp3$Running.Trial[!is.na(exp3$RT.filter)], #vector of trial numbers across observations for non-filtered RTs
                    #trial_log = log(exp3$Running.Trial[!is.na(exp3$RT.filter)]), #vector of log trial numbers across observations for non-filtered RTs
                    rt = exp3$RT.filter[!is.na(exp3$RT.filter)] #vector of RTs across observations for non-filtered RTs
                    #start_mean = mean(exp3$RT.filter[exp3$Running.Trial == 1], na.rm = T),
                    #end_mean = mean(exp3$RT.filter[exp3$Running.Trial == max(exp3$Running.Trial)], na.rm = T)
)


#Accuracy
dat_acc_exp1A <- gen.acc.data(exp1A)
dat_acc_exp1B <- gen.acc.data(exp1B)
dat_acc_exp2 <- gen.acc.data(exp2)

################################################
############# SIGNAL DETECTION #################
################################################

# Experiments 1-3 used tasks that afforded a test of implicit learning based on RT over
# trials. In contrast, Experiments 4 and 5 used 1-back tasks that did not afford such tests
# This section contains code to check whether participants in Experiments 4 and 5 were 
# on-task (i.e., performed well at detecting 1-back repetitions, assessed using signal
# detection indeces)

exp4 <- readRDS(file="Exposure - 4.rds")
exp5 <- readRDS(file="Exposure - 5.rds")

exp4_signal <- ddply(exp4, .(ID, Condition), extract_signal)
exp5_signal <- ddply(exp5, .(ID, Condition), extract_signal)

# Identify number of participants with inattentive performance (fewer hits than 
# false alarms). These participants are removed from analyses.
nrow(exp4_signal[exp4_signal$hit_fa <= 0,])
nrow(exp5_signal[exp5_signal$hit_fa <= 0,])

exp4_signal <- exp4_signal[exp4_signal$hit_fa > 0,]
exp5_signal <- exp5_signal[exp5_signal$hit_fa > 0,]

# Check that remaining participants were on-task (above chance
# hit - fa)
t.test(exp4_signal$hit_fa, mu = 0)
t.test(exp5_signal$hit_fa, mu = 0)

# Descriptive stats
mean(exp4_signal$hit_fa)
sd(exp4_signal$hit_fa)

mean(exp5_signal$hit_fa)
sd(exp5_signal$hit_fa)


######################################################
#################### FIT MODELS ######################
######################################################

niter <- 2000
burnin <- 1000
nchains <- 3

# The models take a long time to fit, so the commands that run the fits
# have been commented out. The fit objects have been saved as .rds files.
# The lines below read in these saved fit objects.
# Note: For masked peer review, only the fits that were ultimately used
# in the analyses have been made available on OSF, due to storage limits
# on masked (non-public) projects. If you are running this script while the
# project is still masked for peer review, the lines that read in fits that
# were not used in the analyses will give you an error.
# All fits will be made available pending publication. Any users can also 
# generate the fits themselves by un-commenting the lines that fit the models below.

############################# EXP 1A ################################

#fit_slope_rt_exp1A <- stan(model_code=model_slope_rt,
                       #data=dat_rt_exp1A,iter=niter+burnin,
                       #warmup=burnin,chains=nchains,
                       #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_rt_exp1A, file="Fits/Exposure/Fit_RT_Exp1A.rds")

fit_slope_rt_exp1A <- readRDS(file="Fits/Exposure/Fit_RT_Exp1A.rds")
traceplot(fit_slope_rt_exp1A, pars="condition_slope_mean")


#fit_slope_acc_exp1A <- stan(model_code=model_slope_acc,
                            #data=dat_acc_exp1A,iter=niter+burnin,
                            #warmup=burnin,chains=nchains,
                            #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_acc_exp1A, file="Fits/Exposure/Fit_Acc_Exp1A.rds")

fit_slope_acc_exp1A <- readRDS(file="Fits/Exposure/Fit_Acc_Exp1A.rds")
traceplot(fit_slope_acc_exp1A, pars="condition_slope_mean")

############################# EXP 1B ################################

#fit_slope_rt_exp1B <- stan(model_code=model_slope_rt,
                           #data=dat_rt_exp1B,iter=niter+burnin,
                           #warmup=burnin,chains=nchains,
                           #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_rt_exp1B, file="Fits/Exposure/Fit_RT_Exp1B.rds")

fit_slope_rt_exp1B <- readRDS(file="Fits/Exposure/Fit_RT_Exp1B.rds")
traceplot(fit_slope_rt_exp1B, pars="condition_slope_mean")

#fit_slope_acc_exp1B <- stan(model_code=model_slope_acc,
                            #data=dat_acc_exp1B,iter=niter+burnin,
                            #warmup=burnin,chains=nchains,
                            #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_acc_exp1B, file="Fits/Exposure/Fit_Acc_Exp1B.rds")

fit_slope_acc_exp1B <- readRDS(file="Fits/Exposure/Fit_Acc_Exp1B.rds")
traceplot(fit_slope_acc_exp1B, pars="condition_slope_mean")

############################# EXP 2 ################################

#fit_slope_rt_exp2 <- stan(model_code=model_slope_rt,
                          #data=dat_rt_exp2,iter=niter+burnin,
                          #warmup=burnin,chains=nchains,
                          #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_rt_exp2, file="Fits/Exposure/Fit_RT_Exp2.rds")

fit_slope_rt_exp2 <- readRDS(file="Fits/Exposure/Fit_RT_Exp2.rds")
traceplot(fit_slope_rt_exp2, pars="condition_slope_mean")

#fit_slope_acc_exp2 <- stan(model_code=model_slope_acc,
                           #data=dat_acc_exp2,iter=niter+burnin,
                           #warmup=burnin,chains=nchains,
                           #verbose = TRUE, control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_acc_exp2, file="Fits/Exposure/Fit_Acc_Exp2.rds")

fit_slope_acc_exp2 <- readRDS(file="Fits/Exposure/Fit_Acc_Exp2.rds")
traceplot(fit_slope_acc_exp2, pars="condition_slope_mean")

############################# EXP 3 ################################

# fit_exponent_rt_exp3 <- stan(model_code = model_exponent_rt,
#                               data=dat_rt_exp3, iter=niter+burnin,
#                               warmup=burnin, chains=nchains,
#                               control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_exponent_rt_exp3, file="Fits/Exposure/Fit_Exponent_RT_Exp3.rds")

fit_exponent_rt_exp3 <- readRDS("Fits/Exposure/Fit_Exponent_RT_Exp3.rds")
traceplot(fit_exponent_rt_exp3, pars=c("condition_rate_mean", "condition_asymptote_mean"))

######################################################
############ QUALITATIVE MODEL ASSESSMENT ############
######################################################
# As a sanity check, you can visualize whether a model fit does a reasonable
# job of capturing the data. This section contains an example of how
# to do so using data from Experiment 1.

# As described above in the Functions section, comb_pred_obs is a function that takes a model fitted to
# data from an experiment, and the object containing the data from that experiment as input. From the model, 
# the function extracts predicted outcome data from the posterior (the function only extracts predicted outcome 
# data from a subset of iterations sampled from the posterior, i.e., downsample_size, to keep this object from being too huge). 
# The object containing the actual data are used to organize the outcome data predicted from the model in the same
# way as the actual data (e.g., with each predicted datapoint matched up to the corresponding trial, and the corresponding
# participant ID/participant condition in the experiment)


# Example of plotting predicted against observed data, using predictions and observations 
# for the Dense condition in Experiment 1
obs_pred_exponent_exp3 <- obs_pred(exp3, fit_exponent_rt_exp3, RT.filter = TRUE)

# This graphs the predictions from the model with the data observed in the experiment
# The predictions are the lines with ribbons, and the data are the lines without ribbons.
obs_pred_exponent_exp3_plot <- ggplot(obs_pred_exponent_exp3, aes(x = Block, y = RT.filter)) +
  stat_summary(aes(color=Condition), geom="line", fun="mean") +
  stat_summary(aes(y = RT.pred, color = Condition), geom="line", fun="mean", linetype = "dashed") +
  ggtitle("Exp 1: Dense vs Base") +
  theme_bw() +
  theme(legend.position=c(.2,.9),
        legend.background=element_rect(fill="transparent"),
        plot.title=element_text(size=11),
        panel.grid=element_blank())

obs_pred_exponent_exp3_plot

######################################################
############### CONDITION DIFFERENCES ################
######################################################

# Exp1A
post_diff_rt_exp1A <- data.frame(as.matrix(fit_slope_rt_exp1A,pars = "post_slope_diff"))
post_diff_acc_exp1A <- data.frame(as.matrix(fit_slope_acc_exp1A,pars = "post_slope_diff"))
bayestestR::ci(post_diff_rt_exp1A, method="HDI", ci = .89)
bayestestR::ci(post_diff_acc_exp1A, method="HDI", ci = .89)

# Exp1B
post_diff_rt_exp1B <- data.frame(as.matrix(fit_slope_rt_exp1B,pars = "post_slope_diff"))
post_diff_acc_exp1B <- data.frame(as.matrix(fit_slope_acc_exp1B,pars = "post_slope_diff"))
bayestestR::ci(post_diff_rt_exp1B, method="HDI", ci = .89)
bayestestR::ci(post_diff_acc_exp1B, method="HDI", ci = .89)

# Exp2
post_diff_rt_exp2 <- data.frame(as.matrix(fit_slope_rt_exp2,pars = "post_slope_diff"))
post_diff_acc_exp2 <- data.frame(as.matrix(fit_slope_acc_exp2,pars = "post_slope_diff"))
bayestestR::ci(post_diff_rt_exp2, method="HDI", ci = .89)
bayestestR::ci(post_diff_acc_exp2, method="HDI", ci = .89)

# Exp3
# It is possible to look at the condition differences in the parameters
# of the exponential model - i.e., gain, asymptote, and rate. However, 
# interpretation of differences in any one of these parameters is ambiguous 
# because they are all needed to describe the shape of the curve for change
# in RT. Instead, condition differences are inferred from differences in slopes
# derived from the derivative of these curves below.

# # Asymptote difference
# post_diff_asymptote_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "asymptote_diff"))
# bayestestR::ci(post_diff_asymptote_exp3, method="HDI", ci = .89)
# length(post_diff_asymptote_exp3[post_diff_asymptote_exp3$asymptote_diff > 0,])/nrow(post_diff_asymptote_exp3)
# 
# # Gain difference
# post_diff_gain_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "gain_diff"))
# bayestestR::ci(post_diff_gain_exp3, method="HDI", ci = .89)
# length(post_diff_gain_exp3[post_diff_gain_exp3$gain_diff > 0,])/nrow(post_diff_gain_exp3)
# 
# # Rate difference
# post_diff_rate_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "rate_diff"))
# bayestestR::ci(post_diff_rate_exp3, method="HDI", ci = .89)
# length(post_diff_rate_exp3[post_diff_rate_exp3$rate_diff > 0,])/nrow(post_diff_rate_exp3)


# Slope differences
post_diff_slope_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "slope_diff"))
bayestestR::ci(post_diff_slope_exp3, method="HDI", ci = .89)
length(post_diff_slope_exp3[post_diff_slope_exp3$slope_diff > 0,])/nrow(post_diff_slope_exp3)

post_diff_slope_head_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "slope_head_diff"))
bayestestR::ci(post_diff_slope_head_exp3, method="HDI", ci = .89)
length(post_diff_slope_head_exp3[post_diff_slope_head_exp3$slope_head_diff > 0,])/nrow(post_diff_slope_head_exp3)

post_diff_slope_tail_exp3 <- data.frame(as.matrix(fit_exponent_rt_exp3, pars = "slope_tail_diff"))
bayestestR::ci(post_diff_slope_tail_exp3, method="HDI", ci = .89)
length(post_diff_slope_tail_exp3[post_diff_slope_tail_exp3$slope_tail_diff > 0,])/nrow(post_diff_slope_tail_exp3)

######################################################
######## GRAPHS: MODEL CONDITION DIFFERENCES #########
######################################################

#Plot distributions of condition differences with credible intervals highlighted
#Use plot_diff function defined in functions block above
plot_rt_diff_exp1A <- plot_diff(fit_slope_rt_exp1A, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_rt_diff_exp1B <- plot_diff(fit_slope_rt_exp1B, "post_slope_diff", conditions="Sparse vs Base", density.color=rgb(.8,0,.1))
plot_rt_diff_exp2 <- plot_diff(fit_slope_rt_exp2, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_rt_diff_exp3 <- plot_diff(fit_exponent_rt_exp3, "slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))

#png(file="Figures/Exposure Phase - Reaction Time Difference.png", width=1350*(2/3), height=700*(95/100), res=120, type="cairo")
grid.arrange(
  grobs = list(plot_rt_diff_exp1A, plot_rt_diff_exp1B, plot_rt_diff_exp2, plot_rt_diff_exp3),
  layout_matrix = rbind(c(1,2),
                        c(3,4))
)
#dev.off()

######################################################
################### GRAPHS: DATA #####################
######################################################

# Just the data - RT on accurate trials across trials
plot_data_exp1A <- plot_data(exp1A, "RT", rgb(0,.1,1), c(230, 380))
plot_data_exp1B <- plot_data(exp1B,"RT", rgb(.8,0,.1), c(230, 380))
plot_data_exp2 <- plot_data(exp2, "RT", rgb(0,.1,1), c(230, 380))
plot_data_exp3 <- plot_data(exp3, "RT.filter", rgb(0,.1,1), c(400, 900))

png(file="Figures/Exposure Phase - Data.png", width=700, height=1000*(2/3), res=120, type="cairo")
grid.arrange(
  grobs = list(plot_data_exp1A, plot_data_exp1B, plot_data_exp2, plot_data_exp3),
  layout_matrix = rbind(c(1,2),
                        c(3,4))
)
dev.off()

######################################################
############### GRAPHS: MODEL + DATA #################
######################################################

######## EMBED MODEL RESULTS IN OBSERVED DATA ########
plot_rt_embed_exp1A <- plot_diff_embed(fit_slope_rt_exp1A, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_rt_embed_exp1B <- plot_diff_embed(fit_slope_rt_exp1B, "post_slope_diff", conditions="Sparse vs Base", density.color=rgb(.8,0,.1))
plot_rt_embed_exp2 <- plot_diff_embed(fit_slope_rt_exp2, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_rt_embed_exp3 <- plot_diff_embed(fit_exponent_rt_exp3, "slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))

plot_data_embed_exp1A <- plot_data_exp1A +
  annotation_custom(ggplotGrob(plot_rt_embed_exp1A), xmin = 4, xmax = 8, 
                    ymin = 310, ymax = 385)
plot_data_embed_exp1B <- plot_data_exp1B +
  annotation_custom(ggplotGrob(plot_rt_embed_exp1B), xmin = 4, xmax = 8, 
                    ymin = 310, ymax = 385)
plot_data_embed_exp2 <- plot_data_exp2 +
  annotation_custom(ggplotGrob(plot_rt_embed_exp2), xmin = 4, xmax = 8, 
                    ymin = 310, ymax = 385)
plot_data_embed_exp3 <- plot_data_exp3 +
  annotation_custom(ggplotGrob(plot_rt_embed_exp3), xmin = 4, xmax = 8, 
                    ymin = 650, ymax = 915)

legend_data <- rbind(exp1A, exp1B)
legend_data$Condition <- factor(legend_data$Condition, levels = c("Dense", "Sparse", "Base Dense", "Base Sparse"))
legend <- ggplot(legend_data, aes(x = Condition, y = RT)) +
  stat_summary(aes(fill = Condition), geom = "bar") +
  scale_fill_manual(breaks = c("Dense", "Sparse", "Base Dense"), 
                    values = rgb(c(0, .8, 0, 0), c(.1, 0, 0, 0), c(1, .1, 0, 0), c(1, 1, .4, .4)),
                    name = "", labels = c("Category Exposure\nDense Structure", "Category Exposure\nSparse Structure", "Baseline Exposure\n")) +
  theme(legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
legend <- as_ggplot(get_legend(legend))


png(file="Figures/Exposure Phase - Data + Fit Diff.png", width=900, height=900, res=120, type="cairo")
grid.arrange(
  grobs = list(legend,
               plot_data_embed_exp1A, plot_data_embed_exp1B, 
               plot_data_embed_exp2, plot_data_embed_exp3),
  layout_matrix = rbind(c(NA, 1, 1, NA),
                        c(NA, 1, 1, NA),
                        c(NA, NA, NA, NA),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(2,2,3,3),
                        c(4,4,5,5),
                        c(4,4,5,5),
                        c(4,4,5,5),
                        c(4,4,5,5),
                        c(4,4,5,5),
                        c(4,4,5,5),
                        c(4,4,5,5))
)
dev.off()