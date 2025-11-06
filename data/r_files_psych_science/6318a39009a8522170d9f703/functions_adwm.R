# Convenience functions for 'Adaptive Dynamics in Working Memory'
library(circular)
library(CircStats)
library(data.table)

library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(patchwork)
library(Hmisc)

library(lme4)
library(lmerTest)
library(nlstools)
library(nls.multstart)

read_dt <- function(folder){
  # reads csv files from folder containing raw data
  # and strings them together, filters and computes 
  # relevant variables
  
  # input: folder (containing raw data)
  # returns: data (processed)
  
  raw_dt <- data.table()
  for(i in list.files(path=folder, pattern = "^subject-")){
    temp <- read.csv(paste0(folder, '/', i))
    temp <- data.table(temp)
    
    if(folder %in% c('./block_experiment')){
      if(nchar(i) == 13){
        sub_nr <- as.numeric(substr(i, 9, 9))
      } else if(nchar(i) == 14){
        sub_nr <- as.numeric(substr(i, 9, 10))
      } else if(nchar(i) == 15){
        sub_nr <- as.numeric(substr(i, 9, 11))
      }
      
      temp[, subject_nr := NULL]
      temp[, subject_nr := sub_nr]
    }
    
    raw_dt <- rbindlist(list(raw_dt, temp), fill=TRUE)
  }
  
  dt <- raw_dt
  raw_dt <- raw_dt[subject_nr!=999]

  dt[, error_angle := circular(error_angle, units='degrees')] # make circular
  dt[, error_angle := conversion.circular(error_angle)] # convert to rad
  dt[, error_angle := error_angle * 2] # to interval -pi -- pi
  
  # time-rounding with monitor refresh rate (100Hz)
  dt[, presentation_time := presentation_time + 5]
  dt <- dt[block_type != "'practice'"]
  
  # n-x analysis
  dt[, prev_duration := c(NA, presentation_time[2:length(presentation_time)-1])]
  dt[, prev_duration2 := c(NA, NA, presentation_time[3:length(presentation_time)-2])]
  
  dt[, prev_fp := c(NA, foreperiod[2:length(foreperiod)-1])]
  
  dt[, prev_error := c(NA, error_angle[2:length(error_angle)-1])]
  dt[, median_error := median(abs(prev_error)), 
         by=c('subject_nr', 'prev_duration', 'presentation_time')]
  dt[abs(prev_error) > median_error, prev_error_large := TRUE]
  dt[abs(prev_error) < median_error, prev_error_large := FALSE]
  
  keep <- c('subject_nr', 'block', 'block_type', 'trial', 'presentation_time',  
            'target_orientation', 'cue', 'response_orientation', 'error_angle', 
            'prev_duration', 'prev_duration2', 'prev_error', 'median_error',
            'prev_error_large', 'db_counter', 'session', 'rt', 'prev_fp')
  keep <- colnames(dt) %in% keep
  dt <- dt[, keep, with=FALSE]
  
  return(dt)
}

fit_encoding_rc <- function(data, conditions){
  # input: data (raw, trial-level), conditions (list)
  # returns: data (summary stats per condition), parameter estimates
  
  # compute precision for each subject and condition
  groups <- append(c('subject_nr', 'presentation_time'), conditions)
  
  summary_dt <- data[,.(k = mle.vonmises(error_angle)$kappa,
                        precision = 1/sd.circular(error_angle),
                        mean_rt = mean(rt),
                        .N),
                     by=groups]
  summary_dt <- summary_dt[, J := k* (besselI(k,1)/besselI(k,0)), # see v.d. Berg (2012)
                           by=groups]
  
  # generate standard deviation from uniform circular dist., with same number of
  # samples as in experiment per condition. Its reciprocal is subtracted from
  # precision to correct for random guessing
  sd_uniform <- c()
  sd_vec <- c()
  n <- summary_dt[, min(N)]
  for(i in 1:1000){
    sd_vec[i] <- sd.circular(rcircularuniform(n))
  }
  sd_uniform <- mean(sd_vec)
  
  summary_dt <- summary_dt[, precision := precision - 1/sd_uniform] # see Bays (2011)
  
  if(length(conditions) > 0){
    for(i in 1:length(conditions)){
      summary_dt <- summary_dt[!is.na(get(conditions[i]))]
    }
  }
  
  # fit rc-function for each subject and condition
  conditions <- append(c('subject_nr'), conditions)
  rc_fit <- summary_dt[, .(
  c = {
    if (inherits(try(
      ans <- coef(nls_multstart(precision ~ c*(1-exp(-presentation_time/tau)), 
                                data = .SD,
                                iter = 500,
                                start_lower = c(c = 0, tau = 1e-10),
                                start_upper = c(c = 50, tau = 1000),
                                supp_errors = 'Y',
                                na.action = na.omit,
                                lower = c(c = 0, tau = 0)))[[1]]), "try-error"))
      NA_real_ # catch errors in fitting and assign NA
    else
      ans
  }, 
  rate = {
    if (inherits(try(
      ans <- 1/coef(nls_multstart(precision ~ c*(1-exp(-presentation_time/tau)), 
                                  data = .SD,
                                  iter = 500,
                                  start_lower = c(c = 0, tau = 1e-10),
                                  start_upper = c(c = 50, tau = 1000),
                                  supp_errors = 'Y',
                                  na.action = na.omit,
                                  lower = c(c = 0, tau = 0)))[[2]]), "try-error"))
      NA_real_
    else
      ans
  }), 
  by=conditions]
  
  # only keep subjects with parameter estimates in all conditions
  #rc_fit <- rc_fit[c > 20, c := NA]
  
  rc_fit[, not_any_na := !anyNA(rate), by='subject_nr']
  rc_fit[, not_any_na := !anyNA(c), by='subject_nr']
  rc_fit <- rc_fit[not_any_na==TRUE]
  
  # compute normalized (per subject and if applicable, session) rate and capacity
  if('session' %in% conditions){
    norm_groups <- c('subject_nr', 'session')
  } else {
    norm_groups <- c('subject_nr')
  }
   
  rc_fit[, mean_rate := mean(rate), by=norm_groups]
  rc_fit[, rate_ratio := rate/mean_rate]
  
  rc_fit[, timescale := 1/rate]
  rc_fit[, mean_timescale := mean(timescale), by=norm_groups]
  rc_fit[, timescale_ratio := timescale/mean_timescale]
  
  rc_fit[, mean_c := mean(c), by=norm_groups]
  rc_fit[, c_ratio := c/mean_c]
  rc_fit <- rc_fit[mean_c < 50]
  
  return(list(summary_dt = summary_dt, 
              rc_fit = rc_fit))
}

plot_encoding_rc <- function(data, conditions, color_indices=c(1, 2, 3, 4), 
                             rc_data, plot_subjects=FALSE){
  # input: data (processed), conditions, color_indices (vector containing indexes), 
  # rc_data (parameter estimates)
  # returns (for each condition): curve plot, parameter estimates summary, 
  # individual subject plots (optional)
  
  # plots are constructed to split 1st condition into colors, and second
  # condition into panels
  
  # colours are defined by previous presentation time
  plot_colours <- brewer.pal(max(color_indices), 'RdBu')[color_indices]
  
  # First, plot RC curves
  # Add fitted RC curves
  # define encoding function: Resistor capacitor (RC) for step input
  rc_step_function <- function(t, c, tau){
    # t, time dimension
    # c, maximum capacity
    # tau, time-constant (encoding/forgetting speed)
    return(c*(1-exp(-t/tau)))
  }
  
  # Then plot empirical data
  data <- data[!is.na(get(conditions[1]))]
  data[, (conditions[1]) := as.factor(get(conditions[1]))]
  tmp <- append(c('subject_nr'), conditions)
  
  test_plot <- ggplot(data, 
                      aes_string(x='presentation_time',
                                 y='precision',
                                 group=conditions[1],
                                 color=conditions[1])) +
    scale_colour_manual(values = plot_colours,
                        name='n-1 presentation time (ms)') +
    stat_summary(fun.data='mean_sdl', geom='pointrange', size=0.75,
                 fun.args = list(1/data[, sqrt(unique(N)[1])])) +
    scale_x_continuous(name='Presentation time (ms)', 
                       breaks=c(0, 50, 200, 400)) +
    scale_y_continuous(name=expression(paste('Precision (', rad^{-1}, ')'))) +
    expand_limits(y=c(0, 3.5), x=0) +
    theme_cowplot() +
    theme(legend.justification = c(1, 0), 
          legend.position = c(1, 0.1))
  
  setkeyv(data, conditions[1])
  for(i in 1:length(data[, unique(get(conditions[1]))])){
    cond <- data[, unique(get(conditions[1]))][i]
    m <- nls_multstart(precision ~ c*(1-exp(-presentation_time/tau)), 
                  data = data[get(conditions[1])==cond],
                  iter = 500,
                  start_lower = c(c = 0, tau = 1e-10),
                  start_upper = c(c = 10, tau = 1000),
                  supp_errors = 'Y',
                  na.action = na.omit,
                  lower = c(c = 0, tau = 0))
    
    c <- m$m$getPars()[[1]]
    tau <- m$m$getPars()[[2]]
    
    test_plot <- test_plot + geom_function(fun=rc_step_function, 
                                           args=list(c, tau),
                                           color=plot_colours[i], size=1.25, 
                                           alpha=0.2)
  }
  
  if(length(conditions) > 1){
    test_plot <- test_plot + 
      facet_wrap(as.formula(paste("~", conditions[2])), nrow=1)
  }
  
  return(test_plot)
}

plot_parameter_summary <- function(rc_data, conditions,
                                   color_indices=c(1,2,3,4)){
  
  rc_data <- rc_data[!is.na(get(conditions[1]))]
  rc_data[, 'x' := get(conditions[1])]
  if(conditions[1] == 'prev_duration' | conditions[1] == 'prev_duration_cat'){
    rc_data[, 'x' := get(conditions[1])]
  }
  
  rc_data[, (conditions[1]) := as.factor(get(conditions[1]))]
  
  if('i' %in% colnames(rc_data)){
    rc_data <- rc_data[, N := .N, by=c('i', 'x')]
  } else {
    rc_data <- rc_data[, N := .N, by=c('x')]
  }
  

  # colours are defined by previous presentation time
  plot_colours <- brewer.pal(max(color_indices), 'RdBu')[color_indices]
  
  c_plot <- ggplot(data = rc_data, 
                   aes_string(x='x', y='c_ratio', group=1,
                              fill=conditions[1])) + 
    geom_hline(yintercept=1, color='lightgrey',
               size=1.5, linetype='dashed') +
    scale_fill_manual(values = plot_colours) +
    scale_y_continuous(breaks= c(0.5, 1., 1.5)) +
    #stat_summary(geom='line', color='black', size=1) +
    #geom_smooth(method='lm', se=FALSE, color='black') +
    stat_summary(geom='line') + 
    stat_summary(fun.data='mean_sdl', geom='pointrange', size=0.75,
                 shape=21, color='black',
                 fun.args = list(1/rc_data[1, sqrt(N)])) +
    ylab('normalized capacity') +
    ggtitle('Capacity') +
    expand_limits(y=c(0.5, 1.8), x=c(0)) +
    theme_cowplot() +
    theme(legend.position = 'None',
          axis.title.x = element_blank(),
          plot.title = element_text(hjust=0.5))
  
  if(length(conditions) == 2){
    c_plot <- c_plot + 
      facet_wrap(as.formula(paste("~", conditions[2])), 
                 nrow=length(conditions[2]))
  }
  
  rate_plot <- ggplot(data = rc_data, 
                      aes_string(x='x', y='rate_ratio', group=1,
                                 fill=conditions[1])) + 
    geom_hline(yintercept=1, color='lightgrey',
               size=1.5, linetype='dashed') +
    scale_fill_manual(values = plot_colours) +
    scale_y_continuous(breaks= c(0.5, 1., 1.5)) +
    #stat_summary(geom='line', color='black', size=1) +
    #geom_smooth(method='lm', se=FALSE, color='black') +
    stat_summary(geom='line') + 
    stat_summary(fun.data='mean_sdl', geom='pointrange', size=0.75,
                 shape=21, color='black', 
                 fun.args = list(1/rc_data[1, sqrt(N)])) +
    ylab('normalized speed') +
    ggtitle('Encoding speed') +
    expand_limits(y=c(0.5, 1.8), x=c(0)) +
    theme_cowplot() +
    theme(legend.position = 'None',
          axis.title.x = element_blank(),
          plot.title = element_text(hjust=0.5))
  
  if(length(conditions) == 2){
    rate_plot <- rate_plot + 
      facet_wrap(as.formula(paste("~", conditions[2])), 
                 nrow=length(conditions[2]))
  }
  
  parameter_plot <- c_plot + rate_plot
  
  return(parameter_plot)
}

bootstrap_adwm <- function(data, conditions, n_iter=10){
  groups <- append(c('subject_nr', 'presentation_time'), conditions)
  stats_dt <- data.table(coefficient_rate=numeric(),
                         t_value_rate=numeric(),
                         p_value_rate=numeric(),
                         dAIC_rate=numeric(),
                         dBIC_rate=numeric(),
                         BF10_rate=numeric(),
                         coefficient_capacity=numeric(),
                         t_value_capacity=numeric(),
                         p_value_capacity=numeric(),
                         dAIC_capacity=numeric(),
                         dBIC_capacity=numeric(),
                         BF10_capacity=numeric())
  
  for(i in 1:n_iter){
    message(cat('running bootstrap', i, '/', n_iter))
    # bootstrap data
    min_N <- data[, .N, by=groups]
    min_N <- min_N[, min(N)] - 1
    data_boot <- data[, .SD[sample(.N, min_N, replace=FALSE)],
                 by = groups]

    tryCatch(expr = {temp <- fit_encoding_rc(data_boot, conditions)},
             error = function(e){
               temp$summary_dt <- cbind(i=i, summary_dt)
               temp$summary_dt <- temp$summary_dt[FALSE,]
               
               temp$pars_dt <- cbind(i=i, pars_dt)
               temp$pars_dt <- temp$pars_dt[FALSE,]
             })
    
    # put summary stats and rc-parameters in data.table
    if(i == 1){ # if its the first iteration, create empty table
      summary_dt <- cbind(i=i, temp$summary_dt)
      summary_dt <- summary_dt[FALSE,]
      
      pars_dt <- cbind(i=i, temp$rc_fit)
      pars_dt <- pars_dt[FALSE,]
    }
      
    pars_dt <- rbindlist(list(pars_dt,
                              cbind(i=i, temp$rc_fit)))
    summary_dt <- rbindlist(list(summary_dt,
                              cbind(i=i, temp$summary_dt)))
    
    # Encoding speed
    # then fit linear models 
    f0_rate <- paste('rate_ratio', '~', 'c_ratio')
    lm0_rate <- lm(f0_rate, temp$rc_fit)
      
    f1_rate <- paste('rate_ratio', '~', 'c_ratio', '+', conditions[1])
    lm1_rate <- lm(f1_rate, temp$rc_fit)
      
    # and compute stats
    coefficient_rate <- lm1_rate$coefficients[[3]]
    t_value_rate <- summary(lm1_rate)[["coefficients"]][, "t value"][[3]]
    df_rate <- temp$rc_fit[, .N] - 2 - 1
    p_value_rate <- 2*pt(q=abs(t_value_rate), df=df_rate, lower.tail=FALSE)
    dAIC_rate <- AIC(lm0_rate) - AIC(lm1_rate)
    dBIC_rate <- BIC(lm0_rate) - BIC(lm1_rate)
    BF10_rate <- exp(dBIC_rate/2)

    # Capacity
    # then fit linear models 
    f0_capacity <- paste('c_ratio', '~', 'rate_ratio')
    lm0_capacity <- lm(f0_capacity, temp$rc_fit)
    
    f1_capacity <- paste('c_ratio', '~', 'rate_ratio', '+', conditions[1])
    lm1_capacity <- lm(f1_capacity, temp$rc_fit)
    
    # and compute stats
    coefficient_capacity <- lm1_capacity$coefficients[[3]]
    t_value_capacity <- summary(lm1_capacity)[["coefficients"]][, "t value"][[3]]
    df_capacity <- temp$rc_fit[, .N] - 2 - 1
    p_value_capacity <- 2*pt(q=abs(t_value_capacity), df=df_capacity, lower.tail=FALSE)
    dAIC_capacity <- AIC(lm0_capacity) - AIC(lm1_capacity)
    dBIC_capacity <- BIC(lm0_capacity) - BIC(lm1_capacity)
    BF10_capacity <- exp(dBIC_capacity/2)
      
    stats_dt <- rbindlist(list(stats_dt,
                               data.table(coefficient_rate=coefficient_rate,
                                            t_value_rate=t_value_rate,
                                            p_value_rate=p_value_rate,
                                            dAIC_rate=dAIC_rate,
                                            dBIC_rate=dBIC_rate,
                                            BF10_rate=BF10_rate,
                                          coefficient_capacity=coefficient_capacity,
                                            t_value_capacity=t_value_capacity,
                                            p_value_capacity=p_value_capacity,
                                            dAIC_capacity=dAIC_capacity,
                                            dBIC_capacity=dBIC_capacity,
                                            BF10_capacity=BF10_capacity)))
  }
  return(list(stats_dt=stats_dt, summary_dt=summary_dt, pars_dt=pars_dt))
}


# Residual analysis
residual_analysis <- function(data, conditions){
  residual_dt <- copy(data$summary_dt)
  rc_data <- copy(data$rc_fit)
  residual_dt[, mean_precision := mean(precision), 
              by=c('presentation_time', 'subject_nr')]
  residual_dt[, residual_precision := precision - mean_precision]
  setnames(residual_dt, conditions[1], 'condition')
  
  rc_residual_dt <- data.table(presentation_time = numeric(),
                               residual_precision = numeric(),
                               condition = numeric())
  
  pts <- rc_data[, unique(get(conditions[1]))]
  subjects <- rc_data[, unique(subject_nr)]
  m <- nls_multstart(precision ~ c*(1-exp(-presentation_time/tau)), 
                     data = residual_dt,
                     iter = 500,
                     start_lower = c(c = 0, tau = 1e-10),
                     start_upper = c(c = 10, tau = 1000),
                     supp_errors = 'Y',
                     na.action = na.omit,
                     lower = c(c = 0, tau = 0))
  mean_c <- m$m$getPars()[[1]]
  rc_mean <- rc_step_function(0:400, m$m$getPars()[[1]], 
                              m$m$getPars()[[2]])
  residual_dt[, c := mean_c]
  
  for(j in c(1:length(pts))){
    m <- nls_multstart(precision ~ rc_step_function(t=presentation_time, c=c, tau), 
                       data = residual_dt[condition == pts[j]],
                       iter = 500,
                       start_lower = c(tau = 1e-10),
                       start_upper = c(tau = 1000),
                       supp_errors = 'Y',
                       na.action = na.omit,
                       lower = c(tau = 0))
    rc_temp <- rc_step_function(0:400, mean_c, 
                                m$m$getPars()[[1]])
    rc_residual_dt <- rbindlist(list(rc_residual_dt, 
                                     data.table(
                                       presentation_time = 0:400,
                                       residual_precision = (rc_temp-rc_mean),
                                       condition = pts[j])))
    
  }
  
  # alternative
  mask_residuals_dt <- residual_dt[, .(precision = mean(precision)),
                                   by=c(append('presentation_time', 'condition'))]
  mask_residuals_dt[, precision := mean(precision), by='presentation_time']
  
  # depending on experiment, there's difference mismatches with expectations
  if(conditions[1] %in% c('prev_duration', 'prev_duration_cat')){
    mask_residuals_dt[, mismatch := abs(presentation_time - condition)]
  } else if(conditions[1] == 'block_type'){
    mask_residuals_dt[condition == "'short'", 
                      mismatch := abs(presentation_time - 50)]
    mask_residuals_dt[condition == "'long'", 
                      mismatch := abs(presentation_time - 400)]
  } else if(conditions[1] == 'cue'){
    mask_residuals_dt[condition == "FAST", 
                      mismatch := abs(presentation_time - 50)]
    mask_residuals_dt[condition == "SLOW", 
                      mismatch := abs(presentation_time - 400)]
  }
  
  mask_residuals_dt[, boost := -(mismatch - mean(mismatch))]
  
  # Here, we will optimize for scale across conditions
  true_dt <- residual_dt[, .(precision = mean(precision)), 
                         by=c('presentation_time', 'condition')]
  optim_data <- data.frame(
    precision=true_dt[, precision],
    base_precision=mask_residuals_dt[, precision],
    boost=mask_residuals_dt[, boost])
  
  scale_function <- function(data, scale){
    masked_precision <- data[, 'base_precision'] + data[, 'boost'] * scale
    abs_error <- abs(masked_precision - data[, 'precision'])
    return(mean(abs_error))
  }
  
  fit <- optim(par=c(0.005),
                 fn=scale_function,
                 data=optim_data,
                 lower=0.0005,
                 method='L-BFGS-B')
  fit_scale <- fit$par
  print(fit_scale)
  
  mask_residuals_dt[, masked_precision := precision + boost*fit_scale]
  mask_residuals_dt[, mean_precision := mean(masked_precision), by='presentation_time'] 
  mask_residuals_dt[, residual_precision := masked_precision - mean_precision]

  residual_summary <- list()
  residual_summary$data <- residual_dt
  residual_summary$rc <- rc_residual_dt
  residual_summary$alternative <- mask_residuals_dt
  
  return(residual_summary)
}

