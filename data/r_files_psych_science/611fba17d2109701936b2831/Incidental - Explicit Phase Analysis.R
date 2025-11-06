################################################
################ DESCRIPTION ###################
################################################

# Data come from a set of 4 experiments. In each experiment, participants completed an Explicit Phase 
# in which they learned two categories, "Flurps" and "Jalets", with conventional supervision 
# (i.e., categorized an exemplar, then received corrective feedback after each trial)
# In each experiment, the Explicit Phase occurred after an "Exposure Phase" in 
# one of two conditions: (1) Baseline, or  (2) Incidental exposure to categories.
# In Experiment 1, categories were either Dense or Sparse. In all subsequent experiments, categories 
# were dense

# Goal of analyses: Test how condition affects category learning across trials
# Specifically, test hypothesis that Incidental exposure to dense categories facilitates subsequent 
# category learning in comparison to Baseline, whereas exposure to sparse categories does not

#Data for each experiment are stored in a long- format dataframe. Columns include:
#ID: randomly assigned ID of the participant as an alphanumeric string
#Condition: combined exposure and category structure condition info
#Accuracy: 0 or 1
#Block: Block 1-3 of Explicit phase (or 1 in Experiment 3 only)
#Trial: Trial 1-20 within block of Explicit phase (or 1-12 in Experiment 3 only)
#Running.Trial: Trial 1-30 of Explicit phase (or 1-12 in Experiment 3 only)

# Analysis steps currently in script:
# Steps currently in script:
# (1) Define a set of new functions that will be used in the script
# (2) Define a set of hierarchical Bayesian models that will be used to analyze the data. In these models,
#     the outcome variable (categorization Accuracy) of a logistic regression is predicted by trial number. 
#     Across the models, the intercept and slope for each participant are each taken from either
#     a single distribution, or one of two distributions based on condition. When a parameter is taken from
#     one of two distributions based on condition, for each iteration, the model compares the Baseline and 
#     Incidental slope distributions. This comparison is used to compare whether changes Accuracy across 
#     trials vary across the two conditions.
# (3) Load and format Accuracy data from each experiment.
# (4) Fit the models to the data
# (5) For each experiment, compare the fits of the different models using leave-one-out cross-validation, and
#     identify the best-fitting model
# (6) From the best-fitting models, extract differences between the distributions for slopes in the two
#     conditions for each sample from the posterior. 
# (7) Evaluate differences between distributions between the two conditions for each
#     experiment.
# (8) Generate graphs, including graphs presented in the manuscript, and additional graphs that can be used to
#     explore the data, the predictions of the models, and comparisons between observed & predicted data.


################################################
################# PACKAGES #####################
################################################

library(tidyr)
library(plyr)
library(stringr)

library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpubr)

library(rstan)
library(bayestestR)
library(loo)

rstan_options(auto_write = TRUE)
options(mc.cores = 3)

################################################
################# FUNCTIONS ####################
################################################

# This section defines a number of functions that are used in the script
# to accomplish tasks such as formatting data from an experiment to use to fit an
# rstan model

############## FORMATTING DATA #################

# Generate object in which acc data used to fit models is in list
# format used as input for stan models
gen.explicit.data <- function(experiment, num_comparisons) {
  dat <- list(num_sub = length(unique(experiment$ID.num)), #number of participants
              num_trials = length(unique(experiment$Running.Trial)), #number of trials for each participant
              num_conditions =length(unique(experiment$Condition.num)), #number of conditions
              num_comparisons = num_comparisons, #number of comparisons between conditions (base vs dense only, or also base vs sparse)
              sub_condition = as.integer(ddply(experiment, .(ID), summarise, Cond = unique(Condition.num))$Cond), #condition for each participant
              trial = matrix(experiment$Running.Trial, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)),byrow=T), #matrix of trial numbers for each participant
              acc = matrix(experiment$Accuracy, nrow=length(unique(experiment$ID)), ncol=length(unique(experiment$Running.Trial)), byrow=T) #matrix of accuracies for each participant
  )
  return(dat)
}

####### EXTRACT MODEL-PREDICTED OUTCOMES #######

# Get the upper and lower values for credible intervals of posterior predicted mean accuracies 
# for each participant from a model.
ppd_acc_sub <- function(input.ppd.acc.sub) {
  ci_ppd <- data.frame(bayestestR::ci(input.ppd.acc.sub, method="ETI"))[,c("CI_low","CI_high")]
  return(ci_ppd)
}


# Function that calculates upper and lower limits of credible interval, used for plotting.
credible <- function(x) {
  data.frame(ymin=bayestestR::ci(x, ci=.1)$CI_low,
             ymax=bayestestR::ci(x, ci=.1)$CI_high)
}

# Generate graphs of posterior distributions of differences between conditions
plot_diff <- function(input.fit, parameter, conditions, density.color) {
  input.name <- deparse(substitute(input.fit))
  experiment <- str_replace_all(input.name, "fit_slope_|fit_int_", "")
  #experiment <- paste("(", experiment, ")", sep="")
  experiment <- str_replace_all(experiment, "exp", "Experiment ")
  type_diff <- ifelse(grepl("slope", input.name), "(slope)", "(intercept)")
  post_diff <- data.frame(as.matrix(input.fit, pars=parameter))
  names(post_diff) <- c("post_diff")
  density_diff <- with(density(post_diff$post_diff), data.frame(x, y))
  ci_diff <- bayestestR::ci(post_diff$post_diff, method="HDI")
  ggplot(post_diff, aes(x=post_diff)) +
    #geom_density(color=rgb(0,0,0,0), fill=density.color, size=1, alpha=.2) +
    geom_area(data= density_diff[density_diff$x>ci_diff$CI_low & density_diff$x<ci_diff$CI_high,], aes(x=x, y=y), fill = density.color, alpha=.8) +
    geom_density(color="black", fill=density.color, size=1, alpha=.1) +
    scale_x_continuous(name=paste("Category Learning Advantage for\nCategory Exposure", type_diff)) +
    #scale_y_continuous(name=paste(experiment, conditions, sep="\n")) +
    scale_y_continuous(name=experiment) +
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
  ci_diff <- bayestestR::ci(post_diff$post_diff, method="HDI")
  ggplot(post_diff, aes(x=post_diff)) +
    geom_density(color=rgb(0,0,0,0), fill=density.color, size=1, alpha=.1) +
    geom_area(data= density_diff[density_diff$x>ci_diff$CI_low & density_diff$x<ci_diff$CI_high,], aes(x=x, y=y), fill = density.color, alpha=.8) +
    scale_x_continuous(name=paste("Incidental Advantage"), breaks = breaks_extended(n=3)) +
    scale_y_continuous(name="") +
    geom_vline(xintercept=0, linetype="dashed", color="black", size = .5) +
    theme_bw() +
    theme(panel.grid=element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent"), 
          axis.title=element_text(size=8, face="bold"),
          axis.text=element_text(size=6),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

# Generate graphs of data
plot_data <- function(input.data, incidental.color, upper.limit = 1) {
  input.name <- deparse(substitute(input.data))
  experiment <- str_replace_all(input.name, "A|B", "")
  #experiment <- paste("(", experiment, ")", sep="")
  experiment <- str_replace_all(experiment, "exp", "Experiment ")
  ggplot(input.data, aes(x = factor(Block), y = Accuracy)) +
    stat_summary(aes(group = Condition, color = Condition), geom = "line", fun = "mean", position = position_dodge(width = .1), show.legend=F) +
    stat_summary(aes(group = Condition), geom = "linerange", fun.data = "mean_se", position = position_dodge(width = .1)) +
    stat_summary(aes(group = Condition, color = Condition), geom = "point", fun = "mean", position = position_dodge(width = .1), show.legend=F) +
    scale_x_discrete(name = "Block") +
    scale_y_continuous(name = paste(experiment, "Accuracy", sep = "\n"), breaks = seq(.4,1, by = .2)) +
    scale_color_manual(values = c(incidental.color, "grey")) +
    coord_cartesian(ylim = c(.4, upper.limit)) +
    geom_hline(yintercept = .5, linetype = "dashed") +
    theme_bw() +
    theme(panel.grid=element_blank(),
          axis.title=element_text(size=12, face="bold"),
          axis.text=element_text(size=8))
}

################################################
################## MODELS ######################
################################################

#Model in which intercepts and slopes are drawn from different distributions based on condition
model_intercept_slope = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> sub_condition[num_sub]; // condition for each subject
  int<lower=1> num_comparisons; // number of comparisons
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor
  int<lower=0,upper=1> acc[num_sub,num_trials]; // accuracies (binary integers - 0 or 1)
  }
parameters {
  real sub_alpha[num_sub];        // Intercept for each participant
  real sub_slope[num_sub]; // Slope for trial number for each particpant
  
  real condition_alpha_mean[num_conditions];  // means of the 2 distributions (one for each condition) for subject intercepts
  real<lower=0> condition_alpha_sigma2[num_conditions]; // variances of the 2 distributions for subject intercepts
  real condition_slope_mean[num_conditions];   // means of the 2 distributions for for subject trial slopes
  real<lower=0> condition_slope_sigma2[num_conditions];  // variances of the 2 distributions for subject trial slopes
  }

transformed parameters {
  real<lower=0> condition_alpha_sigma[num_conditions];
  real<lower=0> condition_slope_sigma[num_conditions];
  
  condition_alpha_sigma = sqrt(condition_alpha_sigma2);
  condition_slope_sigma = sqrt(condition_slope_sigma2);
  }

model {
// Priors for hyperparameters
  condition_alpha_mean ~ normal(0,1/.01);
  condition_slope_mean ~ normal(0,1/.01);
  
  condition_alpha_sigma2 ~ inv_gamma(.01,.01);
  condition_slope_sigma2 ~ inv_gamma(.01,.01);


// Priors for intercepts and slopes for each participant
// Use this to draw intercept and slope values from one of two distributions based on participant condition
  for (i in 1:num_sub){                           
    sub_alpha[i] ~ normal(condition_alpha_mean[sub_condition[i]], condition_alpha_sigma[sub_condition[i]]);
    sub_slope[i] ~ normal(condition_slope_mean[sub_condition[i]],condition_slope_sigma[sub_condition[i]]);

// Likelihood for accuracy: Predicted by participant's intercept, their slope for trial type, and their slope for trial num
    for (j in 1:num_trials){                         
    acc[i,j] ~ bernoulli_logit(sub_alpha[i] + sub_slope[i] * trial[i,j]);
    }
  }
}

generated quantities {
  real post_slope_diff[num_comparisons]; // difference between means of condition distributions for slopes
  real post_alpha_diff[num_comparisons]; // difference between means of condition distributions for intercepts
  matrix[num_sub,num_trials] log_lik; // matrix of log-likelihoods for each acc observation
  real ppd_mean_acc[num_sub]; // vector of predicted mean accuracies
  
  //Calculate difference between means of condition distributions for intercepts
  if(num_comparisons == 1){
    post_alpha_diff[1] = condition_alpha_mean[2] - condition_alpha_mean[1]; //Incidental Dense minus Baseline Dense
    post_slope_diff[1] = condition_slope_mean[2] - condition_slope_mean[1]; //Incidental Dense minus Baseline Dense
  }
  else {
    post_alpha_diff[1] = condition_alpha_mean[2] - condition_alpha_mean[1]; //Incidental Dense minus Baseline Dense
    post_alpha_diff[2] = condition_alpha_mean[4] - condition_alpha_mean[3]; //Incidental Sparse minus Baseline Sparse
    post_slope_diff[1] = condition_slope_mean[2] - condition_slope_mean[1]; //Incidental Dense minus Baseline Dense
    post_slope_diff[2] = condition_slope_mean[4] - condition_slope_mean[3]; //Incidental Sparse minus Baseline Sparse
  }

  for (i in 1:num_sub) {
    vector[num_trials] ppd_accuracies; //vector of predicted accuracies for each trial
    
    for (j in 1:num_trials) {
      log_lik[i,j] =  bernoulli_logit_lpmf(acc[i,j] | sub_alpha[i] + sub_slope[i] * trial[i,j] );
      ppd_accuracies[j] = bernoulli_logit_rng(sub_alpha[i] + sub_slope[i] * trial[i,j] ); //predicted acc on the jth trial for the ith subject
    }
    ppd_mean_acc[i] = mean(ppd_accuracies); //predicted mean acc for the ith subject
  }
}
"


#Model in which intercepts but not slopes are drawn from different distributions based on condition
model_intercept = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> num_comparisons; // number of comparisons
  int<lower=1> sub_condition[num_sub]; // condition for each subject
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor
  int<lower=0,upper=1> acc[num_sub,num_trials]; // accuracies (binary integers - 0 or 1)
  }
parameters {
  real sub_alpha[num_sub];        // Intercept for each participant
  real sub_slope[num_sub]; // Slope for trial number for each particpant
  
  real condition_alpha_mean[num_conditions];  // means of the 2 distributions (one for each condition) for subject intercepts
  real<lower=0> condition_alpha_sigma2[num_conditions]; // variances of the 2 distributions for subject intercepts
  
  real hyper_slope_mean;   // mean of the single distribution for for subject trial slopes
  real<lower=0> hyper_slope_sigma2;  // variance of the single distribution for subject trial slopes
  }

transformed parameters {
  real<lower=0> condition_alpha_sigma[num_conditions];
  real<lower=0> hyper_slope_sigma;
  
  condition_alpha_sigma = sqrt(condition_alpha_sigma2);
  hyper_slope_sigma = sqrt(hyper_slope_sigma2);
  }

model {
// Priors for hyperparameters
  condition_alpha_mean ~ normal(0,1/.01);
  hyper_slope_mean ~ normal(0,1/.01);
  
  condition_alpha_sigma2 ~ inv_gamma(.01,.01);
  hyper_slope_sigma2 ~ inv_gamma(.01,.01);


// Priors for intercepts and slopes for each participant
// Use this to draw intercept and slope values from one of two distributions based on participant condition
  for (i in 1:num_sub){                           
    sub_alpha[i] ~ normal(condition_alpha_mean[sub_condition[i]],condition_alpha_sigma[sub_condition[i]]);
    sub_slope[i] ~ normal(hyper_slope_mean,hyper_slope_sigma);

// Likelihood for accuracy: Predicted by participant's intercept, their slope for trial type, and their slope for trial num
    for (j in 1:num_trials){                         
    acc[i,j] ~ bernoulli_logit(sub_alpha[i] + sub_slope[i] * trial[i,j]);
    }
  }
}

generated quantities {
  real post_alpha_diff[num_comparisons]; // difference between means of condition distributions for intercepts
  matrix[num_sub,num_trials] log_lik; // matrix of log-likelihoods for each acc observation
  real ppd_mean_acc[num_sub]; // vector of predicted mean accuracies
  matrix[num_sub,num_trials] pred_prob_acc; // matrix of predicted probabilities of acc response for each subject/trial

  //Calculate difference between means of condition distributions for intercepts
  if(num_comparisons == 1){
    post_alpha_diff[1] = condition_alpha_mean[2] - condition_alpha_mean[1]; //Incidental Dense minus Baseline Dense
  }
  else {
    post_alpha_diff[1] = condition_alpha_mean[2] - condition_alpha_mean[1]; //Incidental Dense minus Baseline Dense
    post_alpha_diff[2] = condition_alpha_mean[4] - condition_alpha_mean[3]; //Incidental Sparse minus Baseline Sparse
  }  

  
  //Calculate log likelihoods, predicted mean accuracies, and predicted probabilities of a correct response on each trial
  for (i in 1:num_sub) {
    vector[num_trials] ppd_accuracies; //vector of predicted accuracies for each trial
    
    for (j in 1:num_trials) {
      log_lik[i,j] =  bernoulli_logit_lpmf(acc[i,j] | sub_alpha[i] + sub_slope[i] * trial[i,j] ); //log likelihood for the jth trial for the ith subject
      ppd_accuracies[j] = bernoulli_logit_rng(sub_alpha[i] + sub_slope[i] * trial[i,j] ); //predicted acc on the jth trial for the ith subject
      pred_prob_acc[i,j] = inv_logit(sub_alpha[i] + sub_slope[i] * trial[i,j]); // probability of acc response on the jth trial for the ith subject
    }
    ppd_mean_acc[i] = mean(ppd_accuracies); //predicted mean acc for the ith subject
  }
}
"

#Model in which slopes but not intercepts are drawn from different distributions based on condition
model_slope = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> num_conditions; // number of conditons
  int<lower=1> num_comparisons; // number of comparisons between conditions; 2 in exp1 (baseline vs dense and base vs sparse), 1 in other experiments
  int<lower=1> sub_condition[num_sub]; // condition for each subject
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor
  int<lower=0,upper=1> acc[num_sub,num_trials]; // accuracies (binary integers - 0 or 1)
  }
parameters {
  real alpha[num_sub];        // Intercept for each participant
  real sub_slope[num_sub]; // Slope for trial number for each particpant
  
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
    alpha[i] ~ normal(hyper_alpha,hyper_alpha_sigma);
    sub_slope[i] ~ normal(condition_slope_mean[sub_condition[i]],condition_slope_sigma[sub_condition[i]]);

// Likelihood for accuracy: Predicted by participant's intercept, their slope for trial type, and their slope for trial num
    for (j in 1:num_trials){                         
    acc[i,j] ~ bernoulli_logit(alpha[i] + sub_slope[i] * trial[i,j]);
    }
  }
}

generated quantities {
  real post_slope_diff[num_comparisons]; // difference between means of conditon distributions for slopes  
  matrix[num_sub,num_trials] log_lik;  // matrix of log-likelihoods for each acc observation
  real ppd_mean_acc[num_sub]; // vector of predicted mean accuracies
  matrix[num_sub,num_trials] pred_prob_acc; // matrix of predicted probabilities of acc response for each subject/trial
  
  //Calculate difference between means of condition distributions for slopes
  if(num_comparisons == 1){
    post_slope_diff[1] = condition_slope_mean[2] - condition_slope_mean[1];
  }
  else {
    post_slope_diff[1] = condition_slope_mean[2] - condition_slope_mean[1];
    post_slope_diff[2] = condition_slope_mean[4] - condition_slope_mean[3];
  }
  
  //Calculate log likelihoods, predicted mean accuracies, and predicted probabilities of a correct response on each trial 
  for (i in 1:num_sub) {
    vector[num_trials] ppd_accuracies; //vector of predicted accuracies for each trial
    
    for (j in 1:num_trials) {
      log_lik[i,j] =  bernoulli_logit_lpmf(acc[i,j] | alpha[i] + sub_slope[i] * trial[i,j] );
      pred_prob_acc[i,j] = inv_logit(alpha[i] + sub_slope[i] * trial[i,j]); // probability of acc response on the jth trial for the ith subject
      ppd_accuracies[j] = bernoulli_logit_rng(alpha[i] + sub_slope[i] * trial[i,j] ); //predicted acc on the jth trial for the ith subject
    }
    ppd_mean_acc[i] = mean(ppd_accuracies); //predicted mean acc for the ith subject
  }
}
"

#Model in which slopes and intercepts are drawn from the same distribution, regardless of condition
model_base = "
data {
  int<lower=1> num_sub; // number of subjects
  int<lower=1> num_trials; // number of trials
  int<lower=1> sub_condition[num_sub]; // condition for each subject
  matrix<lower=0>[num_sub,num_trials] trial; // trial number predictor
  int<lower=0,upper=1> acc[num_sub,num_trials]; // accuracies (binary integers - 0 or 1)
  }
parameters {
  real alpha[num_sub];        // Intercept for each participant
  real sub_slope[num_sub]; // Slope for trial number for each particpant
  
  real hyper_alpha;  // mean of the single distribution for subject intercepts
  real<lower=0> hyper_alpha_sigma2; // variance of the single distribution for subject intercepts
  real condition_slope_mean;   // mean of the single distribution for for subject trial slopes
  real<lower=0> condition_slope_sigma2;  // variance of the single distributions for subject trial slopes
  }

transformed parameters {
  real<lower=0> hyper_alpha_sigma;
  real<lower=0> condition_slope_sigma;
  
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
// Use this to draw intercepts from one distribution, and slopes from one distribution
  for (i in 1:num_sub){                           
    alpha[i] ~ normal(hyper_alpha,hyper_alpha_sigma);
    sub_slope[i] ~ normal(condition_slope_mean,condition_slope_sigma);

// Likelihood for accuracy: Predicted by participant's intercept, their slope for trial type, and their slope for trial num
    for (j in 1:num_trials){                         
    acc[i,j] ~ bernoulli_logit(alpha[i] + 
    sub_slope[i] * trial[i,j]);
    }
  }
}

generated quantities {
  matrix[num_sub,num_trials] log_lik;// matrix of log-likelihoods for each acc observation
  real ppd_mean_acc[num_sub]; // vector of predicted mean accuracies
  
  for (i in 1:num_sub) {
    vector[num_trials] ppd_accuracies; //vector of predicted accuracies for each trial
    
    for (j in 1:num_trials) {
      log_lik[i,j] =  bernoulli_logit_lpmf(acc[i,j] | alpha[i] + sub_slope[i] * trial[i,j] );
      ppd_accuracies[j] = bernoulli_logit_rng(alpha[i] + sub_slope[i] * trial[i,j] ); //predicted acc on the jth trial for the ith subject
    }
    ppd_mean_acc[i] = mean(ppd_accuracies); //predicted mean acc for the ith subject
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

exp1A <- readRDS(file="Explicit - 1A.rds")
exp1B <- readRDS(file="Explicit - 1B.rds")
exp2 <- readRDS(file="Explicit - 2.rds")
exp3 <- readRDS(file="Explicit - 3.rds")
exp4 <- readRDS(file="Explicit - 4.rds")
exp5 <- readRDS(file="Explicit - 5.rds")

#Fix some formatting to be consistent across experiments
exp2 <- droplevels(exp2[!(is.na(exp2$ID)),])
names(exp3)[names(exp3) == 'Trial'] <- 'Running.Trial'
exp4$Running.Trial <- rep(1:30, length(unique(exp4$ID)))
names(exp5)[names(exp5) == 'Trial'] <- 'Running.Trial'

#Combine experiments "1A" and "1B" (run simultaneously with same pool of participants)
exp1 <- rbind(exp1A, exp1B)

#Re-code IDs as integers starting at 1
exp1$ID.num <- as.integer(exp1$ID)
exp1A$ID.num <- as.integer(exp1A$ID)
exp1B$ID.num <- as.integer(exp1B$ID)
exp2$ID.num <- as.integer(exp2$ID)
exp3$ID.num <- as.integer(exp3$ID)
exp4$ID.num <- as.integer(as.factor(exp4$ID))
exp5$ID.num <- as.integer(as.factor(exp5$ID))

#Re-code Conditions as integers starting at 1
exp1$Condition.num <- as.integer(as.character(mapvalues(exp1$Condition, c("Base Dense","Dense","Base Sparse", "Sparse"), 1:4)))
exp1A$Condition.num <- as.integer(ifelse(exp1A$Condition=="Base Dense",1,2))
exp1B$Condition.num <- as.integer(ifelse(exp1B$Condition=="Base Sparse",1,2))
exp2$Condition.num <- as.integer(ifelse(exp2$Condition=="Base Dense",1,2))
exp3$Condition.num <- as.integer(ifelse(exp3$Condition=="Base Dense",1,2))
exp4$Condition.num <- as.integer(ifelse(exp4$Condition=="Base Dense",1,2))
exp5$Condition.num <- as.integer(ifelse(exp5$Condition=="Base Dense",1,2))

#Sort the dataframe by particpant number, then by trial number
#This is useful for generating a vector of participant conditions in which participants
#are in the same order as they are in the dataframe
exp1 <- exp1[with(exp1, order(ID.num, Running.Trial)),]
exp1A <- exp1A[with(exp1A, order(ID.num, Running.Trial)),]
exp1B <- exp1B[with(exp1B, order(ID.num, Running.Trial)),]
exp2 <- exp2[with(exp2, order(ID.num, Running.Trial)),]
exp3 <- exp3[with(exp3, order(ID.num, Running.Trial)),]
exp4 <- exp4[with(exp4, order(ID.num, Running.Trial)),]
exp5 <- exp5[with(exp5, order(ID.num, Running.Trial)),]


#Data used to fit the models. Use function defined above.
dat_exp1 <- gen.explicit.data(exp1, 2)
dat_exp2 <- gen.explicit.data(exp2, 1)
dat_exp3 <- gen.explicit.data(exp3, 1)
dat_exp4 <- gen.explicit.data(exp4, 1)
dat_exp5 <- gen.explicit.data(exp5, 1)

################################################
################ FIT MODELS ####################
################################################

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

niter <- 8000
burnin <- 1000
nchains <- 3

######################### COMPILE MODELS ############################

int_slope_acc_compiled <- stan_model(model_code = model_intercept_slope)
int_acc_compiled <- stan_model(model_code = model_intercept)
slope_acc_compiled <- stan_model(model_code = model_slope)
base_acc_compiled <- stan_model(model_code = model_base)

############################# EXP 1 ################################
#Intercept and slope
# fit_int_slope_exp1 <- sampling(object = int_slope_acc_compiled,
#                            data=dat_exp1,iter=niter+burnin,
#                            warmup=burnin,chains=3,
#                            control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_slope_exp1, file="Fits/Explicit/Fit_Int_Slope_Exp1.rds")

fit_int_slope_exp1 <- readRDS(file="Fits/Explicit/Fit_Int_Slope_Exp1.rds")

#Intercept only
# fit_int_exp1 <- sampling(object= int_acc_compiled,
#                          data=dat_exp1,iter=niter+burnin,
#                          warmup=burnin,chains=3,
#                          control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_exp1, file="Fits/Explicit/Fit_Int_Exp1.rds")

fit_int_exp1 <- readRDS(file="Fits/Explicit/Fit_Int_Exp1.rds")

#Slope only
# fit_slope_exp1 <- sampling(object = slope_acc_compiled,
#                            data=dat_exp1,iter=niter+burnin,
#                            warmup=burnin,chains=3,
#                            control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_slope_exp1, file="Fits/Explicit/Fit_Slope_Exp1.rds")

fit_slope_exp1 <- readRDS(file="Fits/Explicit/Fit_Slope_Exp1.rds")

#Baseline
# fit_base_exp1 <- sampling(object = base_acc_compiled,
#                           data=dat_exp1,iter=niter+burnin,
#                           warmup=burnin,chains=3,
#                           control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_base_exp1, file="Fits/Explicit/Fit_Base_Exp1.rds")

fit_base_exp1 <- readRDS(file="Fits/Explicit/Fit_Base_Exp1.rds")

############################# EXP 2 ################################

#Intercept and slope
# fit_int_slope_exp2 <- sampling(object = int_slope_acc_compiled,
#                                data = dat_exp2,iter=niter+burnin,
#                                warmup = burnin, chains=3,
#                                control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_slope_exp2, file="Fits/Explicit/Fit_Int_Slope_Exp2.rds")

fit_int_slope_exp2 <- readRDS(file="Fits/Explicit/Fit_Int_Slope_Exp2.rds")

#Intercept only
#fit_int_exp2 <- sampling(object = int_acc_compiled,
#data=dat_exp2,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_int_exp2, file="Fits/Explicit/Fit_Int_Exp2.rds")

fit_int_exp2 <- readRDS(file="Fits/Explicit/Fit_Int_Exp2.rds")

#Slope only
#fit_slope_exp2 <- sampling(object=slope_acc_compiled,
#data=dat_exp2,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_exp2, file="Fits/Explicit/Fit_Slope_Exp2.rds")

fit_slope_exp2 <- readRDS(file="Fits/Explicit/Fit_Slope_Exp2.rds")

#Baseline
#fit_base_exp2 <- sampling(object=base_acc_compiled,
#data=dat_exp2,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_base_exp2, file="Fits/Explicit/Fit_Base_Exp2.rds")

fit_base_exp2 <- readRDS(file="Fits/Explicit/Fit_Base_Exp2.rds")

############################# EXP 3 ################################

#Intercept and slope
# fit_int_slope_exp3 <- sampling(object = int_slope_acc_compiled,
#                                data=dat_exp3,iter=niter+burnin,
#                                warmup=burnin,chains=3,
#                                control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_slope_exp3, file="Fits/Explicit/Fit_Int_Slope_Exp3.rds")

fit_int_slope_exp3 <- readRDS(file="Fits/Explicit/Fit_Int_Slope_Exp3.rds")

#Intercept only
#fit_int_exp3 <- sampling(object=int_acc_compiled,
#data=dat_exp3,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_int_exp3, file="Fits/Explicit/Fit_Int_Exp3.rds")

fit_int_exp3 <- readRDS(file="Fits/Explicit/Fit_Int_Exp3.rds")

#Slope only
#fit_slope_exp3 <- sampling(object=slope_acc_compiled,
#data=dat_exp3,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_exp3, file="Fits/Explicit/Fit_Slope_Exp3.rds")

fit_slope_exp3 <- readRDS(file="Fits/Explicit/Fit_Slope_Exp3.rds")

#Baseline
#fit_base_exp3 <- sampling(object=base_acc_compiled,
#data=dat_exp3,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_base_exp3, file="Fits/Explicit/Fit_Base_Exp3.rds")

fit_base_exp3 <- readRDS(file="Fits/Explicit/Fit_Base_Exp3.rds")

############################# EXP 4 ################################

#Intercept and slope
# fit_int_slope_exp4 <- sampling(object=int_slope_acc_compiled,
#                                data=dat_exp4,iter=niter+burnin,
#                                warmup=burnin,chains=3,
#                                control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_slope_exp4, file="Fits/Explicit/Fit_Int_Slope_Exp4.rds")

fit_int_slope_exp4 <- readRDS(file="Fits/Explicit/Fit_Int_Slope_Exp4.rds")

#Intercept only
#fit_int_exp4 <- sampling(object=int_acc_compiled,
#data=dat_exp4,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_int_exp4, file="Fits/Explicit/Fit_Int_Exp4.rds")

fit_int_exp4 <- readRDS("Fits/Explicit/Fit_Int_Exp4.rds")

#Slope only
#fit_slope_exp4 <- sampling(object=slope_acc_compiled,
#data=dat_exp4,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_exp4, file="Fits/Explicit/Fit_Slope_Exp4.rds")

fit_slope_exp4 <- readRDS("Fits/Explicit/Fit_Slope_Exp4.rds")

#Baseline
#fit_base_exp4 <- sampling(object=base_acc_compiled,
#data=dat_exp4,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_base_exp4, file="Fits/Explicit/Fit_Base_Exp4.rds")

fit_base_exp4 <- readRDS("Fits/Explicit/Fit_Base_Exp4.rds")

############################# EXP 5 ################################

#Intercept and slope
# fit_int_slope_exp5 <- sampling(object = int_slope_acc_compiled,
#                                data=dat_exp5,iter=niter+burnin,
#                                warmup=burnin,chains=3,
#                                control = list(adapt_delta = 0.99, max_treedepth=15))
# saveRDS(fit_int_slope_exp5, file="Fits/Explicit/Fit_Int_Slope_Exp5.rds")

fit_int_slope_exp5 <- readRDS(file="Fits/Explicit/Fit_Int_Slope_Exp5.rds")

#Intercept only
#fit_int_exp5 <- sampling(object=int_acc_compiled,
#data=dat_exp5,iter=niter+burnin,
#warmup=burnin,chains=3,
# control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_int_exp5, file="Fits/Explicit/Fit_Int_Exp5.rds")

fit_int_exp5 <- readRDS("Fits/Explicit/Fit_Int_Exp5.rds")

#Slope only
#fit_slope_exp5 <- sampling(object = slope_acc_compiled,
#data=dat_exp5,iter=niter+burnin,
#warmup=burnin,chains=3,
# control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_slope_exp5, file="Fits/Explicit/Fit_Slope_Exp5.rds")

fit_slope_exp5 <- readRDS("Fits/Explicit/Fit_Slope_Exp5.rds")

#Baseline
#fit_base_exp5 <- sampling(object = base_acc_compiled,
#data=dat_exp5,iter=niter+burnin,
#warmup=burnin,chains=3,
#control = list(adapt_delta = 0.99, max_treedepth=15))
#saveRDS(fit_base_exp5, file="Fits/Explicit/Fit_Base_Exp5.rds")

fit_base_exp5 <- readRDS("Fits/Explicit/Fit_Base_Exp5.rds")


################################################
############### ASSESS MODELS ##################
################################################

############## COMPARE MODELS ##################

loo_base_exp1 <- loo(fit_base_exp1)
loo_int_exp1 <- loo(fit_int_exp1)
loo_slope_exp1 <- loo(fit_slope_exp1)
loo_int_slope_exp1 <- loo(fit_int_slope_exp1)


loo_base_exp2 <- loo(fit_base_exp2)
loo_int_exp2 <- loo(fit_int_exp2)
loo_slope_exp2 <- loo(fit_slope_exp2)
loo_int_slope_exp2 <- loo(fit_int_slope_exp2)

loo_base_exp3 <- loo(fit_base_exp3)
loo_int_exp3 <- loo(fit_int_exp3)
loo_slope_exp3 <- loo(fit_slope_exp3)
loo_int_slope_exp3 <- loo(fit_int_slope_exp3)

loo_base_exp4 <- loo(fit_base_exp4)
loo_int_exp4 <- loo(fit_int_exp4)
loo_slope_exp4 <- loo(fit_slope_exp4)
loo_int_slope_exp4 <- loo(fit_int_slope_exp4)

loo_base_exp5 <- loo(fit_base_exp5)
loo_int_exp5 <- loo(fit_int_exp5)
loo_slope_exp5 <- loo(fit_slope_exp5)
loo_int_slope_exp5 <- loo(fit_int_slope_exp5)

#Compare models
loo::loo_compare(loo_int_slope_exp1, loo_int_exp1, loo_slope_exp1, loo_base_exp1)
loo::loo_compare(loo_int_slope_exp2, loo_int_exp2, loo_slope_exp2, loo_base_exp2)
loo::loo_compare(loo_int_slope_exp3, loo_int_exp3, loo_slope_exp3, loo_base_exp3)
loo::loo_compare(loo_int_slope_exp4, loo_int_exp4, loo_slope_exp4, loo_base_exp4)
loo::loo_compare(loo_int_slope_exp5,  loo_int_exp5, loo_slope_exp5, loo_base_exp5)

######################################################
############ QUALITATIVE MODEL ASSESSMENT ############
######################################################
# As a sanity check, you can visualize whether a model fit does a reasonable
# job of capturing the data. This section contains an example of how
# to do so using data from Experiment 1.

# Extract predicted mean accuracies for each participant from model
# Each column is a participant; each row is an iteration
pred_acc_exp5 <- data.frame(as.matrix(fit_slope_exp5, pars="ppd_mean_acc"))
names(pred_acc_exp5) <- seq(1:ncol(pred_acc_exp5))

# Credible intervals for predicted mean accuracies for each participant
pred_acc_ci_exp5 <- adply(pred_acc_exp5, 2, ppd_acc_sub, .id="ID.num")

# Observed mean accuracies
obs_acc_exp5 <- ddply(exp5, .(ID.num), summarise, Accuracy=mean(Accuracy))

# Observed versus credible intervals
obs_pred_acc_exp5 <- merge(obs_acc_exp5, pred_acc_ci_exp5, by="ID.num")

# Plot observed mean accuracies for participants (points) against credible intervals 
# of predicted accuracies (lines) for participants
# Points should fall within lines
ggplot(obs_pred_acc_exp5, aes(x=ID.num, y=Accuracy)) +
  geom_point(color="blue", alpha=.6)+
  geom_linerange(aes(ymin=CI_low, ymax=CI_high), alpha=.4) +
  theme_bw()

################################################
############ CONDITION DIFFERENCES #############
################################################

########### INCIDENTAL VS BASELINE #############

# Extract samples from posterior for differences between incidental 
# and baseline conditions from best fitting model for each experiment 
# (slope for experiments 1, 2, and 4, intercept for experiment 3)
post_diff_exp1 <- data.frame(as.matrix(fit_slope_exp1, pars="post_slope_diff"))
post_diff_exp2 <- data.frame(as.matrix(fit_slope_exp2, pars="post_slope_diff"))
post_diff_exp3 <- data.frame(as.matrix(fit_int_exp3, pars="post_alpha_diff"))
post_diff_exp4 <- data.frame(as.matrix(fit_slope_exp4, pars="post_slope_diff"))
post_diff_exp5 <- data.frame(as.matrix(fit_slope_exp5, pars="post_slope_diff"))

names(post_diff_exp1) <- c("post_diff_dense", "post_diff_sparse")
names(post_diff_exp2) <- c("post_diff_dense")
names(post_diff_exp3) <- c("post_diff_dense")
names(post_diff_exp4) <- c("post_diff_dense")
names(post_diff_exp5) <- c("post_diff_dense")

#Calculate 89% highest density credible interval
bayestestR::ci(post_diff_exp1$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_exp1$post_diff_sparse, method="HDI", ci = .89)
bayestestR::ci(post_diff_exp2$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_exp3$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_exp4$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_exp5$post_diff_dense, method="HDI", ci = .89)

#Calculate the probability of differences greater than 0
length(post_diff_exp1$post_diff_dense[post_diff_exp1$post_diff_dense > 0])/length(post_diff_exp1$post_diff_dense)
length(post_diff_exp1$post_diff_sparse[post_diff_exp1$post_diff_sparse > 0])/length(post_diff_exp1$post_diff_sparse)
length(post_diff_exp2$post_diff_dense[post_diff_exp2$post_diff_dense > 0])/length(post_diff_exp2$post_diff_dense)
length(post_diff_exp3$post_diff_dense[post_diff_exp3$post_diff_dense > 0])/length(post_diff_exp3$post_diff_dense)
length(post_diff_exp4$post_diff_dense[post_diff_exp4$post_diff_dense > 0])/length(post_diff_exp4$post_diff_dense)
length(post_diff_exp5$post_diff_dense[post_diff_exp5$post_diff_dense > 0])/length(post_diff_exp5$post_diff_dense)

############## DENSE VS SPARSE ################

# Learnability of dense vs sparse categories in baseline condition in Experiment 1
post_slope_exp1 <- data.frame(as.matrix(fit_slope_exp1, pars=c("condition_slope_mean[1]", "condition_slope_mean[3]")))
names(post_slope_exp1) <- c("Base_Dense", "Base_Sparse")
post_slope_exp1$Diff <- post_slope_exp1$Base_Dense - post_slope_exp1$Base_Sparse
bayestestR::ci(post_slope_exp1$Diff, method="HDI")

################################################
## CONDITION DIFFERENCES: CHECKING INTERCEPTS ##
################################################

########### INCIDENTAL VS BASELINE #############

# For experiments in which model with slope differences was best fitting model, 
# check whether intercepts also varied by extracting samples from posterior for 
# differences in intercepts between incidental and baseline conditions from the 
# maximal model in which both intercepts and slopes could vary across conditions 
post_diff_int_exp1 <- data.frame(as.matrix(fit_int_slope_exp1, pars="post_alpha_diff"))
post_diff_int_exp2 <- data.frame(as.matrix(fit_int_slope_exp2, pars="post_alpha_diff"))

post_diff_int_exp4 <- data.frame(as.matrix(fit_int_slope_exp4, pars="post_alpha_diff"))
post_diff_int_exp5 <- data.frame(as.matrix(fit_int_slope_exp5, pars="post_alpha_diff"))

names(post_diff_int_exp1) <- c("post_diff_dense", "post_diff_sparse")
names(post_diff_int_exp2) <- c("post_diff_dense")
names(post_diff_int_exp4) <- c("post_diff_dense")
names(post_diff_int_exp5) <- c("post_diff_dense")

#Calculate 89% highest density credible interval
bayestestR::ci(post_diff_int_exp1$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_int_exp1$post_diff_sparse, method="HDI", ci = .89)
bayestestR::ci(post_diff_int_exp2$post_diff_dense, method="HDI", ci = .89)

bayestestR::ci(post_diff_int_exp4$post_diff_dense, method="HDI", ci = .89)
bayestestR::ci(post_diff_int_exp5$post_diff_dense, method="HDI", ci = .89)


######################################################
######## GRAPHS: MODEL CONDITION DIFFERENCES #########
######################################################

# Plot distributions of condition differences with credible intervals highlighted
# Use plot_diff function defined in functions block above
plot_diff_exp1A <- plot_diff(fit_slope_exp1, "post_slope_diff[1]", conditions="Dense vs Baseline", density.color=rgb(0,.1,1))
plot_diff_exp1B <- plot_diff(fit_slope_exp1, "post_slope_diff[2]", conditions="Sparse vs Baseline", density.color=rgb(.8,0,.1))
plot_diff_exp2 <- plot_diff(fit_slope_exp2, "post_slope_diff", conditions="Dense vs Baseline", density.color=rgb(0,.1,1))
plot_diff_exp3 <- plot_diff(fit_int_exp3, "post_alpha_diff", conditions="Dense vs Baseline", density.color=rgb(0,.1,1))
plot_diff_exp4 <- plot_diff(fit_slope_exp4, "post_slope_diff", conditions="Dense vs Baseline", density.color=rgb(0,.1,1))
plot_diff_exp5 <- plot_diff(fit_slope_exp5, "post_slope_diff", conditions="Dense vs Baseline", density.color=rgb(0,.1,1))

legend_data <- exp1
legend_data$Condition <- factor(legend_data$Condition, levels = c("Dense", "Sparse", "Base Dense", "Base Sparse"))
legend <- ggplot(legend_data, aes(x = Condition, y = Accuracy)) +
  stat_summary(aes(fill = Condition), geom = "bar") +
  scale_fill_manual(breaks = c("Dense", "Sparse"), 
                    values = rgb(c(0, .8, 0, 0), c(.1, 0, 0, 0), c(1, .1, 0, 0)),
                    name = "", labels = c("Dense Structure", "Sparse Structure")) +
  theme(legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
legend <- as_ggplot(get_legend(legend))


png(file="Figures/Explicit Phase - Incidental Learning Advantage.png", width=800, height=950, res=120, type="cairo")
grid.arrange(
  grobs = list(legend, plot_diff_exp1A, plot_diff_exp1B, plot_diff_exp2, plot_diff_exp3, plot_diff_exp4, plot_diff_exp5),
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
                        c(4,4,5,5),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7))
)
dev.off()

######################################################
################### GRAPHS: DATA #####################
######################################################

# Just the data, binned into blocks
plot_data_exp1A <- plot_data(exp1A, rgb(0,.1,1), upper.limit = 1)
plot_data_exp1B <- plot_data(exp1B, rgb(.8,0,.1), upper.limit = 1)
plot_data_exp2 <- plot_data(exp2, rgb(0,.1,1), upper.limit = 1)
plot_data_exp3 <- plot_data(exp3, rgb(0,.1,1), upper.limit = 1)
plot_data_exp4 <- plot_data(exp4, rgb(0,.1,1), upper.limit = 1)
plot_data_exp5 <- plot_data(exp5, rgb(0,.1,1), upper.limit = 1)

legend_data <- exp1
legend_data$Condition <- factor(legend_data$Condition, levels = c("Dense", "Sparse", "Base Dense", "Base Sparse"))
legend <- ggplot(legend_data, aes(x = Condition, y = Accuracy)) +
  stat_summary(aes(fill = Condition), geom = "bar") +
  scale_fill_manual(breaks = c("Dense", "Sparse", "Base Dense"), 
                    values = rgb(c(0, .8, 0, 0), c(.1, 0, 0, 0), c(1, .1, 0, 0), c(1, 1, .4, .4)),
                    name = "", labels = c("Category Exposure\nDense Structure", "Category Exposure\nSparse Structure", "Baseline Exposure\n")) +
  theme(legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
legend <- as_ggplot(get_legend(legend))

png(file="Figures/Explicit Phase - Data.png", width=700, height=950, res=120,  type="cairo")
grid.arrange(
  grobs = list(legend, plot_data_exp1A, plot_data_exp1B, plot_data_exp2, plot_data_exp3, plot_data_exp4, plot_data_exp5),
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
                        c(4,4,5,5),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7),
                        c(6,6,7,7))
)
dev.off()


######################################################
############### GRAPHS: MODEL + DATA #################
######################################################

######## EMBED MODEL RESULTS IN OBSERVED DATA ########
plot_diff_embed_exp1A <- plot_diff_embed(fit_slope_exp1, "post_slope_diff[1]", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_diff_embed_exp1B <- plot_diff_embed(fit_slope_exp1, "post_slope_diff[2]", conditions="Sparse vs Base", density.color=rgb(.8,0,.1))
plot_diff_embed_exp2 <- plot_diff_embed(fit_slope_exp2, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_diff_embed_exp3 <- plot_diff_embed(fit_int_exp3, "post_alpha_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_diff_embed_exp4 <- plot_diff_embed(fit_slope_exp4, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))
plot_diff_embed_exp5 <- plot_diff_embed(fit_slope_exp5, "post_slope_diff", conditions="Dense vs Base", density.color=rgb(0,.1,1))

plot_data_embed_exp1A <- plot_data(exp1A, rgb(0,.1,1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp1A), xmin = 1.75, xmax = 3.5, 
                    ymin = .78, ymax = 1.28)
plot_data_embed_exp1B <- plot_data(exp1B, rgb(.8,0,.1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp1B), xmin = 1.75, xmax = 3.5, 
                    ymin = .78, ymax = 1.28)
plot_data_embed_exp2 <- plot_data(exp2, rgb(0,.1,1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp2), xmin = 1.75, xmax = 3.5, 
                    ymin = .78, ymax = 1.28)
plot_data_embed_exp3 <- plot_data(exp3, rgb(0,.1,1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp3), xmin = .9, xmax = 1.57, 
                    ymin = .78, ymax = 1.28)
plot_data_embed_exp4 <- plot_data(exp4, rgb(0,.1,1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp4), xmin = 1.75, xmax = 3.5, 
                    ymin = .78, ymax = 1.28)
plot_data_embed_exp5 <- plot_data(exp5, rgb(0,.1,1), upper.limit = 1.25) +
  annotation_custom(ggplotGrob(plot_diff_embed_exp5), xmin = 1.75, xmax = 3.5, 
                    ymin = .78, ymax = 1.28)

legend_data <- rbind(exp1A, exp1B)
legend_data$Condition <- factor(legend_data$Condition, levels = c("Dense", "Sparse", "Base Dense", "Base Sparse"))
legend <- ggplot(legend_data, aes(x = Condition, y = RT)) +
  stat_summary(aes(fill = Condition), geom = "bar") +
  scale_fill_manual(breaks = c("Dense", "Sparse", "Base Dense"), 
                    values = rgb(c(0, .8, 0, 0), c(.1, 0, 0, 0), c(1, .1, 0, 0)),
                    name = "Exposure Condition", labels = c("Structured-Dense", "Structured-Sparse", "Unstructured")) +
  theme(legend.position="bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
legend <- as_ggplot(get_legend(legend))


png(file="Figures/Explicit Phase - Data + Fit Diff.png", width=675, height=900, res=120, type="cairo")
grid.arrange(
  grobs = list(legend, plot_data_embed_exp1A, plot_data_embed_exp1B, plot_data_embed_exp2, plot_data_embed_exp3,
               plot_data_embed_exp4, plot_data_embed_exp5),
  layout_matrix = rbind(c(NA,1,1,NA),
                        c(2,2,3,3),
                        c(4,4,5,5),
                        c(6,6,7,7))
)
dev.off()