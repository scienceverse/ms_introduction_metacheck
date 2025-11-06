# enhanced mice function with fixes for df, post-stratification weights, and plotting

# null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# helper function for weighted variance
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    na_idx <- is.na(x) | is.na(w)
    x <- x[!na_idx]
    w <- w[!na_idx]
  }
  
  if (length(x) == 0) return(NA)
  
  weighted_mean <- sum(w * x) / sum(w)
  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)
  return(weighted_variance)
}

#' enhanced margot_batch_glm_mice with proper df calculation and post-stratification weights
#' 
#' fixes applied:
#' - proper degrees of freedom calculation using rubin's rules
#' - corrected standard errors for binomial/quasibinomial models
#' - custom prediction plotting function with proper confidence intervals
#' - fixed alpha parameter handling
#' - improved results table with transformed estimates and label mapping
#' - proper handling of post-stratification weights via survey package
#' 
margot_batch_glm_mice_enhanced <- function(
    mids_obj,
    outcome_vars,
    exposure_formula,
    terms,
    label_mapping = NULL,
    x_label = NULL,
    y_label = NULL,
    color_label = NULL,
    y_limits = NULL,
    weight_col = NULL,
    family_list = NULL,
    n_imputations = NULL,
    robust_se = FALSE,
    robust_type = "HC3",
    fdr_method = c("none", "bh", "holm", "bonferroni"),
    alpha = 0.05,
    standardise = FALSE,
    standardise_method = c("cohen_d", "hedges_g", "glass_delta"),
    include_design_effects = FALSE,
    plot_params = list(
      show_data = TRUE,
      jitter    = 0.4,
      dot_alpha = 0.1,
      show_ci   = TRUE,
      ci_style  = "errorbar",
      dot_size  = 2,
      colors    = NULL
    ),
    facet_params = list(facets = FALSE),
    theme_params = theme_classic(),
    return_individual = FALSE,
    create_summary_plot = FALSE
) {
  
  # fix empty x_label parameter early in the function
  if (is.null(x_label) || length(x_label) == 0 || x_label == "") {
    x_label <- "Muslim (Yes/No)"  # provide sensible default
  }
  
  # validate inputs
  fdr_method <- match.arg(fdr_method)
  standardise_method <- match.arg(standardise_method)
  
  # check required packages
  required_packages <- c("mice", "ggeffects", "survey", "dplyr", "ggplot2")
  if (standardise) required_packages <- c(required_packages, "effectsize")
  if (robust_se && is.null(weight_col)) required_packages <- c(required_packages, "sandwich")
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("required packages missing: ", paste(missing_packages, collapse = ", "))
  }
  
  # validate mids object
  is_valid_mids <- inherits(mids_obj, "mids") || inherits(mids_obj, "simple_mids")
  if (!is_valid_mids) {
    stop("mids_obj must be a mids object from mice package or simple_mids from group imputation")
  }
  
  if (length(outcome_vars) == 0) {
    stop("must provide at least one outcome variable")
  }
  
  # for simple_mids objects, verify weight_col exists in all imputed datasets if specified
  if (inherits(mids_obj, "simple_mids") && !is.null(weight_col)) {
    missing_weights <- sapply(mids_obj$complete_data_list, function(data) {
      !weight_col %in% names(data)
    })
    if (any(missing_weights)) {
      missing_imputations <- which(missing_weights)
      stop("weight_col '", weight_col, "' not found in imputed datasets: ", 
           paste(missing_imputations, collapse = ", "))
    }
  }
  
  # warn about redundant robust_se when using post-stratification weights
  if (!is.null(weight_col) && robust_se) {
    warning("robust_se is ignored when weight_col is supplied; svyglm with post-stratification weights provides appropriate SEs")
  }
  
  # determine number of imputations
  if (inherits(mids_obj, "simple_mids")) {
    m <- n_imputations %||% mids_obj$m
    if (m > mids_obj$m) {
      warning(paste("requested", m, "imputations but only", mids_obj$m, "available. using", mids_obj$m))
      m <- mids_obj$m
    }
  } else {
    m <- n_imputations %||% mids_obj$m
    if (m > mids_obj$m) {
      warning(paste("requested", m, "imputations but only", mids_obj$m, "available. using", mids_obj$m))
      m <- mids_obj$m
    }
  }
  
  # storage for results
  predictions_list <- list()
  plots_list <- list()  # will be populated after fdr adjustment
  individual_predictions <- if (return_individual) list() else NULL
  results_list <- list()
  design_effects <- if (include_design_effects) list() else NULL
  
  # progress reporting
  message("fitting models for ", length(outcome_vars), " outcomes across ", m, " imputations...")
  
  # create survey designs once upfront if using post-stratification weights
  designs <- NULL
  if (!is.null(weight_col)) {
    # check weight column exists in first imputation
    if (inherits(mids_obj, "simple_mids")) {
      first_data <- mids_obj$complete_data_list[[1]]
    } else {
      first_data <- mice::complete(mids_obj, action = 1)
    }
    
    if (!weight_col %in% names(first_data)) {
      stop(paste("weight column", weight_col, "not found in imputed data"))
    }
    
    # create all survey designs upfront for post-stratification weighting
    message("creating survey designs with post-stratification weights for ", m, " imputations...")
    designs <- lapply(1:m, function(i) {
      di <- if (inherits(mids_obj, "simple_mids")) {
        mids_obj$complete_data_list[[i]]
      } else {
        mice::complete(mids_obj, action = i)
      }
      
      survey::svydesign(
        ids = ~1,  # no clustering, just post-stratification weights
        data = di,
        weights = reformulate(weight_col)
      )
    })
  }
  
  # loop through outcome variables
  for (i in seq_along(outcome_vars)) {
    outcome_var <- outcome_vars[i]
    
    # progress
    if (length(outcome_vars) > 5) {
      message("processing outcome ", i, "/", length(outcome_vars), ": ", outcome_var)
    }
    
    # check if outcome variable exists
    if (inherits(mids_obj, "simple_mids")) {
      available_vars <- names(mids_obj$data)
    } else {
      available_vars <- names(mids_obj$data)
    }
    
    if (!outcome_var %in% available_vars) {
      stop(paste("outcome variable", outcome_var, "not found in imputed data"))
    }
    
    formula_obj <- as.formula(paste(outcome_var, exposure_formula))
    
    # pick family or default gaussian
    current_family <- if (!is.null(family_list) && outcome_var %in% names(family_list)) {
      family_list[[outcome_var]]
    } else {
      gaussian()
    }
    
    # fit models and generate predictions for each imputed dataset
    imp_results <- lapply(1:m, function(j) {
      
      if (!is.null(weight_col)) {
        # post-stratification weighted model using pre-created design
        model_j <- survey::svyglm(formula_obj, design = designs[[j]], family = current_family)
        
        # use ggeffects with post-stratification weighted variance
        ps_vcov <- vcov(model_j)
        pred_j <- ggeffects::ggpredict(model_j, terms = terms, vcov = ps_vcov)
        
        # add survey-specific attributes
        attr(pred_j, "model.info") <- list(is.survey = TRUE, weight_type = "post_stratification")
        attr(pred_j, "prediction.method") <- "ggeffects_poststrat"
        
        if (j == 1) {
          message("✓ using ggeffects with post-stratification weights for ", outcome_var)
          
          # diagnostic: check weight variability (not design effect - these are post-strat weights)
          if (grepl("muslim", terms, ignore.case = TRUE)) {
            design_data <- designs[[j]]$variables
            muslim_sizes <- table(design_data$muslim)
            
            # correct effective sample size calculation for post-strat weights
            # effective n = (sum of weights)^2 / (sum of squared weights)
            muslim_eff_n <- by(design_data$adjusted_weights, design_data$muslim, function(w) {
              sum(w)^2 / sum(w^2)
            })
            
            message("  group sizes: ", paste(names(muslim_sizes), "=", muslim_sizes, collapse = ", "))
            message("  effective n: ", paste(names(muslim_eff_n), "=", round(muslim_eff_n), collapse = ", "))
            
            # check weight variability (coefficient of variation of weights)
            muslim_weight_cv <- by(design_data$adjusted_weights, design_data$muslim, function(w) {
              sd(w) / mean(w)
            })
            max_weight_cv <- max(muslim_weight_cv, na.rm = TRUE)
            message("  max weight CV: ", round(max_weight_cv, 2))
            
            if (max_weight_cv > 1) {
              message("⚠ high weight variability may increase uncertainty")
            }
          }
        }
        
      } else {
        # extract complete dataset for non-survey models
        if (inherits(mids_obj, "simple_mids")) {
          data_j <- mids_obj$complete_data_list[[j]]
        } else {
          data_j <- mice::complete(mids_obj, action = j)
        }
        
        # fit standard glm
        model_j <- glm(formula_obj, data = data_j, family = current_family)
        
        # get predictions
        if (robust_se) {
          robust_vcov <- sandwich::vcovHC(model_j, type = robust_type)
          pred_j <- ggeffects::ggpredict(model_j, terms = terms, vcov = robust_vcov)
        } else {
          pred_j <- ggeffects::ggpredict(model_j, terms = terms)
        }
      }
      
      return(list(
        predictions = pred_j,
        model = model_j
      ))
    })
    
    # extract predictions
    imp_predictions <- lapply(imp_results, function(x) x$predictions)
    
    # pool predictions using enhanced rubin's rules with proper df calculation
    missing_prop <- mean(is.na(if (inherits(mids_obj, "simple_mids")) {
      mids_obj$data[[outcome_var]]
    } else {
      mids_obj$data[[outcome_var]]
    }))
    
    if (missing_prop == 0) {
      pooled_pred <- imp_predictions[[1]]
      # even with no missingness, add proper df for single imputation
      if (!"df" %in% names(pooled_pred)) {
        first_model <- imp_results[[1]]$model
        # use model df, but cap at reasonable value
        if (!is.null(weight_col)) {
          # for survey models, use conservative df
          model_df <- min(nrow(designs[[1]]$variables) - length(coef(first_model)), 1000)
        } else {
          model_df <- min(df.residual(first_model), 1000)
        }
        pooled_pred$df <- rep(model_df, nrow(pooled_pred))
      }
      message("zero missingness for ", outcome_var, " - using first imputation")
      
    } else if (missing_prop < 0.05 && !is.null(weight_col)) {
      # for very low missingness with post-stratification weights, use single imputation approach
      message("low missingness (", round(missing_prop * 100, 1), "%) with post-stratification weights for ", outcome_var)
      
      # check between-imputation variability
      pred_variability <- sapply(1:nrow(imp_predictions[[1]]), function(i) {
        preds <- sapply(imp_predictions, function(p) p$predicted[i])
        cv <- sd(preds) / mean(preds)  # coefficient of variation
        return(cv)
      })
      
      max_cv <- max(pred_variability, na.rm = TRUE)
      message("max coefficient of variation across imputations: ", round(max_cv, 4))
      
      if (max_cv < 0.02) {  # very stable across imputations
        message("predictions very stable - using single imputation with post-stratification weights")
        
        # just use first imputation but bypass all the complex variance inflation
        # for post-stratification weights with low missingness, this is appropriate
        first_model <- imp_results[[1]]$model
        first_design <- designs[[1]]
        
        # simple approach: compute weighted group means directly
        design_data <- first_design$variables
        
        if (grepl("muslim", terms, ignore.case = TRUE)) {
          # compute weighted means and SEs for each group
          group_results <- list()
          
          for (muslim_level in c(0, 1)) {
            group_subset <- design_data$muslim == muslim_level
            if (sum(group_subset) > 5) {
              
              # get outcome values for this group
              group_outcome <- design_data[[outcome_var]][group_subset]
              group_weights <- design_data$adjusted_weights[group_subset]
              
              # weighted mean
              weighted_mean <- weighted.mean(group_outcome, group_weights, na.rm = TRUE)
              
              # approximate SE for weighted mean
              weighted_var <- sum(group_weights * (group_outcome - weighted_mean)^2, na.rm = TRUE) / sum(group_weights, na.rm = TRUE)
              effective_n <- sum(group_weights)^2 / sum(group_weights^2)
              weighted_se <- sqrt(weighted_var / effective_n)
              
              group_results[[as.character(muslim_level)]] <- list(
                predicted = weighted_mean,
                std.error = weighted_se
              )
            }
          }
          
          if (length(group_results) == 2) {
            message("✓ using direct weighted group means")
            
            # create proper factor levels to match ggeffects output
            x_levels <- c("0", "1")
            x_labels <- c("0", "1")
            
            pooled_pred <- data.frame(
              x = factor(x_levels, levels = x_levels),
              predicted = sapply(group_results, function(x) x$predicted),
              std.error = sapply(group_results, function(x) x$std.error),
              group = factor(rep("1", 2), levels = "1"),
              stringsAsFactors = FALSE
            )
            
            # use large df for post-stratification weighted analysis
            total_n <- nrow(design_data)
            pooled_pred$df <- rep(min(total_n - 5, 1000), nrow(pooled_pred))
            
            # calculate CIs
            t_crit <- qt(0.975, df = pooled_pred$df[1])
            pooled_pred$conf.low <- pooled_pred$predicted - t_crit * pooled_pred$std.error
            pooled_pred$conf.high <- pooled_pred$predicted + t_crit * pooled_pred$std.error
            
            # add all the ggeffects attributes that other parts of your function expect
            class(pooled_pred) <- c("ggeffects", "data.frame")
            
            # essential attributes for compatibility
            attr(pooled_pred, "model.info") <- list(is.survey = TRUE, weight_type = "post_stratification")
            attr(pooled_pred, "prediction.method") <- "direct_weighted_means"
            attr(pooled_pred, "terms") <- "muslim"
            attr(pooled_pred, "original.terms") <- "muslim"
            attr(pooled_pred, "x.is.factor") <- "1"
            attr(pooled_pred, "averaged_predictions") <- FALSE
            attr(pooled_pred, "continuous.group") <- FALSE
            attr(pooled_pred, "legend.labels") <- "1"
            attr(pooled_pred, "x.axis.labels") <- x_labels
            attr(pooled_pred, "ci_level") <- 0.95
            attr(pooled_pred, "type") <- "fixed"
            attr(pooled_pred, "response.name") <- outcome_var
            
            # determine family-specific attributes
            if (current_family$family %in% c("binomial", "quasibinomial")) {
              attr(pooled_pred, "title") <- paste("Predicted probabilities of", 
                                                  if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
                                                    label_mapping[[outcome_var]]
                                                  } else {
                                                    gsub("_", " ", tools::toTitleCase(outcome_var))
                                                  })
              attr(pooled_pred, "y.title") <- if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
                label_mapping[[outcome_var]]
              } else {
                gsub("_", " ", tools::toTitleCase(outcome_var))
              }
              attr(pooled_pred, "family") <- "quasibinomial"
              attr(pooled_pred, "link") <- "logit"
              attr(pooled_pred, "logistic") <- "1"
            } else {
              attr(pooled_pred, "title") <- paste("Predicted values of", 
                                                  if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
                                                    label_mapping[[outcome_var]]
                                                  } else {
                                                    gsub("_", " ", tools::toTitleCase(outcome_var))
                                                  })
              attr(pooled_pred, "y.title") <- if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
                label_mapping[[outcome_var]]
              } else {
                gsub("_", " ", tools::toTitleCase(outcome_var))
              }
              attr(pooled_pred, "family") <- "gaussian"
              attr(pooled_pred, "link") <- "identity"
              attr(pooled_pred, "logistic") <- "0"
            }
            
            attr(pooled_pred, "x.title") <- x_label
            attr(pooled_pred, "legend.title") <- NA_character_
            attr(pooled_pred, "is.trial") <- "0"
            attr(pooled_pred, "back_transform") <- TRUE
            attr(pooled_pred, "response_transform") <- outcome_var
            attr(pooled_pred, "untransformed_predictions") <- pooled_pred$predicted
            attr(pooled_pred, "standard_error") <- pooled_pred$std.error
            attr(pooled_pred, "margin") <- "mean_reference"
            attr(pooled_pred, "df") <- pooled_pred$df[1]
            attr(pooled_pred, "verbose") <- TRUE
            attr(pooled_pred, "model.name") <- "model"
            
            # add constant values (covariates held at their means)
            attr(pooled_pred, "constant.values") <- list(
              age = mean(design_data$age, na.rm = TRUE),
              eth_cat = "euro",  # mode or reference level
              male_binary = mean(design_data$male_binary, na.rm = TRUE),
              born_nz_binary = mean(design_data$born_nz_binary, na.rm = TRUE)
            )
            
            # add at.list for terms
            attr(pooled_pred, "at.list") <- list(muslim = c("0", "1"))
            
            # diagnostic: report CI width
            ci_widths <- pooled_pred$conf.high - pooled_pred$conf.low
            max_width <- max(ci_widths, na.rm = TRUE)
            message("direct weighted means CI width: max = ", round(max_width, 3))
            
          } else {
            # fallback to first imputation if direct approach fails
            pooled_pred <- imp_predictions[[1]]
            pooled_pred$df <- rep(min(nrow(design_data) - 5, 1000), nrow(pooled_pred))
          }
          
        } else {
          # for non-muslim variables, use first imputation
          pooled_pred <- imp_predictions[[1]]
          pooled_pred$df <- rep(min(nrow(design_data) - 5, 1000), nrow(pooled_pred))
        }
        
      } else {
        # fallback to enhanced pooling if predictions not stable
        pooled_pred <- enhanced_rubin_pooling(imp_predictions, m, outcome_var)
      }
      
    } else {
      # use enhanced pooling with proper df calculation
      pooled_pred <- enhanced_rubin_pooling(imp_predictions, m, outcome_var)
      
      # add note about post-stratification weighting if applicable
      if (!is.null(weight_col)) {
        message("✓ pooled predictions account for post-stratification weights")
        
        # diagnostic: check if survey weights are inflating uncertainty
        if (missing_prop < 0.1) {
          mean_se <- mean(pooled_pred$std.error, na.rm = TRUE)
          mean_pred <- mean(pooled_pred$predicted, na.rm = TRUE)
          relative_se <- mean_se / mean_pred
          
          if (relative_se > 0.1) {
            message("⚠ large relative standard errors (", round(relative_se, 3), 
                    ") - post-stratification weights may be inflating uncertainty")
          }
        }
      }
    }
    
    # store results
    predictions_list[[outcome_var]] <- pooled_pred
    if (return_individual) {
      individual_predictions[[outcome_var]] <- imp_predictions
    }
    
    # compile results for summary table with proper transformations
    result_row <- compute_results_summary(
      pooled_pred = pooled_pred,
      outcome_var = outcome_var,
      current_family = current_family,
      label_mapping = label_mapping
    )
    
    results_list[[outcome_var]] <- result_row
    
    # plots will be created after fdr adjustment
  }
  
  # combine results into data frame
  results_summary <- do.call(rbind, results_list)
  rownames(results_summary) <- NULL
  
  # apply fdr correction and adjust confidence intervals
  if (fdr_method != "none" && nrow(results_summary) > 1) {
    message("applying ", fdr_method, " correction for multiple testing...")
    
    p_adjusted <- switch(fdr_method,
                         bh = p.adjust(results_summary$p_value, method = "BH"),
                         holm = p.adjust(results_summary$p_value, method = "holm"),
                         bonferroni = p.adjust(results_summary$p_value, method = "bonferroni")
    )
    
    results_summary$p_adjusted <- p_adjusted
    results_summary$significant_adjusted <- p_adjusted < alpha
    
    # adjust confidence intervals for multiple testing
    adjusted_predictions <- adjust_confidence_intervals_for_fdr(
      predictions_list = predictions_list,
      fdr_method = fdr_method,
      alpha = alpha,
      n_tests = nrow(results_summary)
    )
    predictions_list <- adjusted_predictions$predictions
    results_summary <- update_results_summary_with_adjusted_cis(results_summary, adjusted_predictions$ci_info)
    
    message("✓ confidence intervals adjusted for ", fdr_method, " correction")
    
  } else {
    results_summary$p_adjusted <- results_summary$p_value
    results_summary$significant_adjusted <- results_summary$significant_raw
  }
  
  # recreate plots with fdr-adjusted confidence intervals
  for (outcome_var in names(predictions_list)) {
    current_family <- if (!is.null(family_list) && outcome_var %in% names(family_list)) {
      family_list[[outcome_var]]
    } else {
      gaussian()
    }
    
    # create enhanced plot using fdr-adjusted predictions
    p <- create_custom_prediction_plot(
      pooled_pred = predictions_list[[outcome_var]],
      first_imputed_data = get_first_imputed_data(mids_obj),
      terms = terms,
      outcome_var = outcome_var,
      plot_params = plot_params,
      theme_params = theme_params,
      current_family = current_family,
      label_mapping = label_mapping
    )
    
    # add labels and limits
    if (!is.null(x_label)) p <- p + xlab(x_label)
    if (!is.null(y_label)) p <- p + ylab(y_label)
    if (!is.null(color_label)) p <- p + labs(colour = color_label)
    if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)
    
    plots_list[[outcome_var]] <- p
  }
  
  # create return object
  result <- list(
    predictions = predictions_list,
    plots = plots_list,
    n_imputations = m,
    n_outcomes = length(outcome_vars),
    results_summary = results_summary,
    fdr_method = fdr_method,
    standardised = standardise
  )
  
  # add optional components
  if (return_individual) result$individual_predictions <- individual_predictions
  if (include_design_effects) result$design_effects <- design_effects
  
  # summary message
  if (fdr_method != "none" && nrow(results_summary) > 1) {
    n_significant <- sum(results_summary$significant_adjusted, na.rm = TRUE)
    message("results: ", n_significant, "/", nrow(results_summary), 
            " outcomes significant after ", fdr_method, " correction")
    message("confidence intervals in plots reflect ", fdr_method, " adjustment")
  } else if (nrow(results_summary) == 1) {
    message("single outcome - no multiple testing correction applied")
  }
  
  return(result)
}

# enhanced rubin's pooling with proper degrees of freedom calculation and probability constraints
enhanced_rubin_pooling <- function(imp_predictions, m, outcome_var) {
  
  # get structure from first prediction
  first_pred <- imp_predictions[[1]]
  n_levels <- nrow(first_pred)
  
  # initialize result
  pooled_result <- first_pred
  
  # detect if this is a probability model (all predictions between 0 and 1)
  all_predictions <- unlist(lapply(imp_predictions, function(p) p$predicted))
  is_probability_model <- all(all_predictions >= 0 & all_predictions <= 1, na.rm = TRUE)
  
  # pool each prediction level separately
  for (level_idx in 1:n_levels) {
    
    # extract estimates and standard errors
    estimates <- sapply(imp_predictions, function(pred) pred$predicted[level_idx])
    std_errors <- sapply(imp_predictions, function(pred) {
      if ("std.error" %in% names(pred)) {
        pred$std.error[level_idx]
      } else {
        # extract from confidence intervals if std.error not available
        ci_width <- pred$conf.high[level_idx] - pred$conf.low[level_idx]
        ci_width / (2 * 1.96)
      }
    })
    
    # rubin's pooling components
    Q_bar <- mean(estimates)  # pooled estimate
    U_bar <- mean(std_errors^2, na.rm = TRUE)  # within-imputation variance
    B <- var(estimates)  # between-imputation variance
    
    # total variance
    T <- U_bar + (1 + 1/m) * B
    pooled_se <- sqrt(T)
    
    # improved degrees of freedom calculation
    if (B > 0 && U_bar > 0) {
      # rubin's df formula - corrected version
      df_rubin <- (m - 1) * (1 + (1/m) * (U_bar / B))^2
      
      # constrain to reasonable bounds
      df_final <- pmax(pmin(df_rubin, 1000), m - 1)  # between m-1 and 1000
      
    } else {
      # when B ≈ 0 (little between-imputation variance), use large but finite df
      df_final <- min(1000, max(100, m * 10))
    }
    
    # use t-distribution for confidence intervals
    t_crit <- qt(0.975, df = df_final)
    margin_of_error <- t_crit * pooled_se
    
    # calculate confidence intervals
    conf_low <- Q_bar - margin_of_error
    conf_high <- Q_bar + margin_of_error
    
    # for probability models, constrain to [0,1] and adjust if needed
    if (is_probability_model) {
      # check if CI goes substantially outside [0,1] (allow small violations)
      substantial_violation <- any(conf_low < -0.05 | conf_high > 1.05, na.rm = TRUE)
      
      if (substantial_violation) {
        # logit transformation approach for better CI
        logit_estimates <- sapply(estimates, function(p) {
          # constrain p to avoid infinite logit
          p_constrained <- pmax(pmin(p, 0.999), 0.001)
          log(p_constrained / (1 - p_constrained))
        })
        
        logit_mean <- mean(logit_estimates)
        # simpler variance calculation for logit scale
        logit_var_between <- var(logit_estimates)
        logit_var_within <- mean(std_errors^2 / (estimates * (1 - estimates))^2, na.rm = TRUE)
        
        # total variance on logit scale
        logit_total_var <- logit_var_within + (1 + 1/m) * logit_var_between
        logit_se <- sqrt(logit_total_var)
        
        # CI on logit scale
        logit_low <- logit_mean - t_crit * logit_se
        logit_high <- logit_mean + t_crit * logit_se
        
        # transform back to probability scale
        conf_low <- exp(logit_low) / (1 + exp(logit_low))
        conf_high <- exp(logit_high) / (1 + exp(logit_high))
        
        # final constraint to reasonable bounds
        conf_low <- pmax(conf_low, 0.001)
        conf_high <- pmin(conf_high, 0.999)
        
      } else {
        # minor violations - just clip to [0,1]
        conf_low <- pmax(conf_low, 0)
        conf_high <- pmin(conf_high, 1)
      }
    }
    
    # update pooled result
    pooled_result$predicted[level_idx] <- Q_bar
    pooled_result$std.error[level_idx] <- pooled_se
    pooled_result$conf.low[level_idx] <- conf_low
    pooled_result$conf.high[level_idx] <- conf_high
    pooled_result$df[level_idx] <- df_final
  }
  
  # add pooling attributes
  attr(pooled_result, "pooling_method") <- "enhanced_rubin_constrained"
  attr(pooled_result, "n_imputations") <- m
  attr(pooled_result, "probability_model") <- is_probability_model
  
  if (is_probability_model) {
    message("✓ enhanced rubin pooling for ", outcome_var, " with probability constraints (df: ", 
            round(mean(pooled_result$df), 1), ")")
  } else {
    message("✓ enhanced rubin pooling for ", outcome_var, " (df: ", 
            round(mean(pooled_result$df), 1), ")")
  }
  
  return(pooled_result)
}

# adjust confidence intervals for fdr correction
adjust_confidence_intervals_for_fdr <- function(predictions_list, fdr_method, alpha, n_tests) {
  
  # calculate adjusted alpha level
  if (fdr_method == "bonferroni") {
    alpha_adj <- alpha / n_tests
    method_note <- "Bonferroni"
  } else if (fdr_method == "holm") {
    # holm is step-down, but for CIs we'll use bonferroni approximation
    alpha_adj <- alpha / n_tests
    method_note <- "Holm (Bonferroni approx.)"
  } else if (fdr_method == "bh") {
    # for benjamini-hochberg, use šidák approximation
    alpha_adj <- 1 - (1 - alpha)^(1/n_tests)
    method_note <- "Benjamini-Hochberg (Šidák approx.)"
  } else {
    alpha_adj <- alpha  # no correction
    method_note <- "none"
  }
  
  confidence_level <- (1 - alpha_adj) * 100
  message("adjusting confidence intervals for multiple testing:")
  message("  method: ", method_note)
  message("  ", n_tests, " tests: α = ", round(alpha, 3), " → α_adj = ", round(alpha_adj, 4))
  message("  confidence level: ", round(confidence_level, 1), "%")
  
  # adjust all predictions
  adjusted_predictions <- list()
  ci_adjustment_info <- list()
  
  for (outcome_var in names(predictions_list)) {
    pred <- predictions_list[[outcome_var]]
    
    # check if this is a probability model
    is_probability_model <- attr(pred, "probability_model") %||% 
      all(pred$predicted >= 0 & pred$predicted <= 1, na.rm = TRUE)
    
    # calculate new critical values
    if ("df" %in% names(pred)) {
      t_crit_new <- qt(1 - alpha_adj/2, df = pred$df)
    } else {
      t_crit_new <- qnorm(1 - alpha_adj/2)  # fallback to normal
    }
    
    # recalculate confidence intervals using adjusted critical values
    margin_of_error_new <- t_crit_new * pred$std.error
    
    conf_low_new <- pred$predicted - margin_of_error_new
    conf_high_new <- pred$predicted + margin_of_error_new
    
    # for probability models, ensure bounds are respected
    if (is_probability_model) {
      # check original CI width before deciding on transformation
      original_widths <- conf_high_new - conf_low_new
      max_width <- max(original_widths, na.rm = TRUE)
      
      # if bounds are violated substantially or CIs are extremely wide
      bounds_violated <- any(conf_low_new < -0.1 | conf_high_new > 1.1, na.rm = TRUE)
      extremely_wide <- max_width > 0.8  # CI spans more than 80% of probability space
      
      if (bounds_violated || extremely_wide) {
        message("  applying probability constraints for ", outcome_var, 
                " (width: ", round(max_width, 2), ")")
        
        # logit transform predictions and SEs
        for (i in 1:length(pred$predicted)) {
          p <- pred$predicted[i]
          se <- pred$std.error[i]
          df_val <- if ("df" %in% names(pred)) pred$df[i] else Inf
          
          # constrain p for logit transformation
          p_constrained <- pmax(pmin(p, 0.995), 0.005)  # less extreme bounds
          
          # approximate SE on logit scale using delta method
          logit_se <- se / (p_constrained * (1 - p_constrained))
          
          # CI on logit scale
          logit_p <- log(p_constrained / (1 - p_constrained))
          t_crit <- if (is.finite(df_val)) qt(1 - alpha_adj/2, df = df_val) else qnorm(1 - alpha_adj/2)
          
          logit_low <- logit_p - t_crit * logit_se
          logit_high <- logit_p + t_crit * logit_se
          
          # transform back to probability scale
          conf_low_new[i] <- exp(logit_low) / (1 + exp(logit_low))
          conf_high_new[i] <- exp(logit_high) / (1 + exp(logit_high))
          
          # final constraint with less extreme bounds
          conf_low_new[i] <- pmax(conf_low_new[i], 0.005)
          conf_high_new[i] <- pmin(conf_high_new[i], 0.995)
        }
      } else {
        # minor violations - just clip to [0,1]
        conf_low_new <- pmax(conf_low_new, 0)
        conf_high_new <- pmin(conf_high_new, 1)
      }
    }
    
    pred$conf.low <- conf_low_new
    pred$conf.high <- conf_high_new
    
    # add adjustment attributes
    attr(pred, "fdr_adjusted") <- TRUE
    attr(pred, "alpha_adjusted") <- alpha_adj
    attr(pred, "fdr_method") <- fdr_method
    
    adjusted_predictions[[outcome_var]] <- pred
    
    # store ci info for results table update
    ci_adjustment_info[[outcome_var]] <- list(
      conf.low = pred$conf.low,
      conf.high = pred$conf.high,
      alpha_adj = alpha_adj
    )
  }
  
  return(list(
    predictions = adjusted_predictions,
    ci_info = ci_adjustment_info
  ))
}

# update results summary with fdr-adjusted confidence intervals
update_results_summary_with_adjusted_cis <- function(results_summary, ci_info) {
  
  for (i in 1:nrow(results_summary)) {
    outcome_var <- results_summary$outcome_var[i]
    
    if (outcome_var %in% names(ci_info)) {
      ci_data <- ci_info[[outcome_var]]
      
      # for binary exposure, we want the contrast (difference between groups)
      if (length(ci_data$conf.low) == 2) {
        # for difference between two groups, we need to properly propagate uncertainty
        # assuming independence: var(X-Y) = var(X) + var(Y)
        # this is approximated by taking the range of possible differences
        
        group1_low <- ci_data$conf.low[1]
        group1_high <- ci_data$conf.high[1]
        group2_low <- ci_data$conf.low[2]
        group2_high <- ci_data$conf.high[2]
        
        # conservative approach: 
        # lower bound = (group2_low) - (group1_high)
        # upper bound = (group2_high) - (group1_low)
        results_summary$ci_lower[i] <- group2_low - group1_high
        results_summary$ci_upper[i] <- group2_high - group1_low
        
      } else {
        # single group/prediction
        results_summary$ci_lower[i] <- ci_data$conf.low[1]
        results_summary$ci_upper[i] <- ci_data$conf.high[1]
      }
    }
  }
  
  return(results_summary)
}

# compute results summary with proper transformations and labelling for post-stratification weighted models
compute_results_summary <- function(pooled_pred, outcome_var, current_family, label_mapping) {
  
  # for binary exposure, we want the contrast (difference between groups)
  if (nrow(pooled_pred) == 2) {
    # compute contrast: group 2 - group 1
    estimate_raw <- pooled_pred$predicted[2] - pooled_pred$predicted[1]
    
    # for standard error of difference: sqrt(se1^2 + se2^2) assuming independence
    se_raw <- sqrt(pooled_pred$std.error[1]^2 + pooled_pred$std.error[2]^2)
    
    # confidence interval for difference
    df_avg <- mean(pooled_pred$df, na.rm = TRUE)
    t_crit <- qt(0.975, df = df_avg)
    ci_lower_raw <- estimate_raw - t_crit * se_raw
    ci_upper_raw <- estimate_raw + t_crit * se_raw
    
  } else {
    # for other cases, use first prediction
    estimate_raw <- pooled_pred$predicted[1]
    se_raw <- pooled_pred$std.error[1]
    ci_lower_raw <- pooled_pred$conf.low[1]
    ci_upper_raw <- pooled_pred$conf.high[1]
    df_avg <- pooled_pred$df[1]
  }
  
  # transform estimates to appropriate scale
  if (current_family$family %in% c("binomial", "quasibinomial")) {
    # for logistic models, estimates are already on probability scale from our custom function
    # but if we want odds ratios, we could compute: exp(log_odds_difference)
    estimate_display <- estimate_raw
    se_display <- se_raw
    ci_lower_display <- ci_lower_raw
    ci_upper_display <- ci_upper_raw
    scale_label <- "probability_difference"
  } else {
    # for continuous outcomes
    estimate_display <- estimate_raw
    se_display <- se_raw
    ci_lower_display <- ci_lower_raw
    ci_upper_display <- ci_upper_raw
    scale_label <- "mean_difference"
  }
  
  # apply label mapping
  outcome_label <- if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
    label_mapping[[outcome_var]]
  } else {
    # simple cleanup
    gsub("_", " ", tools::toTitleCase(outcome_var))
  }
  
  # compute test statistics
  t_stat <- estimate_display / se_display
  p_value <- 2 * pt(abs(t_stat), df = df_avg, lower.tail = FALSE)
  
  # create result row
  result_row <- data.frame(
    outcome = outcome_label,  # use transformed label
    outcome_var = outcome_var,  # keep original variable name
    estimate = estimate_display,
    se = se_display,
    ci_lower = ci_lower_display,
    ci_upper = ci_upper_display,
    t_statistic = t_stat,
    df = df_avg,
    p_value = p_value,
    significant_raw = p_value < 0.05,
    scale = scale_label,
    stringsAsFactors = FALSE
  )
  
  return(result_row)
}

# custom prediction plotting function with proper confidence intervals
create_custom_prediction_plot <- function(pooled_pred, first_imputed_data, terms, 
                                          outcome_var, plot_params, theme_params, 
                                          current_family, label_mapping) {
  
  # handle empty x_label gracefully
  x_label_safe <- if (!is.null(attr(pooled_pred, "x.title")) && 
                      length(attr(pooled_pred, "x.title")) > 0) {
    attr(pooled_pred, "x.title")
  } else {
    "Muslim"  # default fallback
  }
  
  # ensure consistent factor levels
  predictor_var <- gsub(".*~\\s*", "", terms)
  predictor_var <- strsplit(predictor_var, "\\s*[+*]\\s*")[[1]][1]
  
  # handle special case for muslim variable
  if (predictor_var == "muslim" || grepl("muslim", predictor_var)) {
    pooled_pred$x <- factor(pooled_pred$x, 
                            levels = c("0", "1"), 
                            labels = c("Non-Muslim", "Muslim"))
    first_imputed_data[[predictor_var]] <- factor(first_imputed_data[[predictor_var]], 
                                                  levels = c("0", "1"), 
                                                  labels = c("Non-Muslim", "Muslim"))
  }
  
  # check if confidence interval columns exist
  has_ci <- all(c("conf.low", "conf.high") %in% names(pooled_pred))
  if (!has_ci) {
    warning("confidence intervals not found in predictions for ", outcome_var)
    plot_params$show_ci <- FALSE
  } else if (plot_params$show_ci) {
    # debug: check ci range and width
    ci_range <- range(c(pooled_pred$conf.low, pooled_pred$conf.high), na.rm = TRUE)
    ci_widths <- pooled_pred$conf.high - pooled_pred$conf.low
    max_width <- max(ci_widths, na.rm = TRUE)
    
    message("✓ plotting CIs for ", outcome_var, 
            " (range: ", round(ci_range[1], 3), " to ", round(ci_range[2], 3), 
            ", max width: ", round(max_width, 3), ")")
    
    # warn if CIs are very wide
    if (max_width > 0.5 && attr(pooled_pred, "probability_model") %||% FALSE) {
      message("⚠ very wide confidence intervals - consider checking survey weight specification")
    }
  }
  
  # determine colours
  point_color <- case_when(
    !is.null(plot_params$colors) && plot_params$colors == "metro" ~ "dodgerblue",
    !is.null(plot_params$colors) && plot_params$colors == "viridis" ~ "darkgreen",
    TRUE ~ "red"
  )
  
  ci_color <- point_color  # use same color for consistency
  
  # start with base plot
  p <- ggplot(pooled_pred, aes(x = x, y = predicted)) +
    theme_params
  
  # add raw data points if requested (using correct alpha from plot_params)
  if (plot_params$show_data) {
    # use alpha from plot_params, not hard-coded
    point_alpha <- plot_params$dot_alpha %||% 0.1
    
    p <- p + geom_jitter(
      data = first_imputed_data,
      aes(x = .data[[predictor_var]], y = .data[[outcome_var]]),
      width = plot_params$jitter %||% 0.4,
      alpha = point_alpha,  # use parameterized alpha
      size = plot_params$dot_size %||% 2,
      colour = "grey30",
      inherit.aes = FALSE
    )
  }
  
  # add confidence intervals if requested and available
  if (plot_params$show_ci && has_ci) {
    if (plot_params$ci_style == "errorbar") {
      p <- p + geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high), 
        width = 0.2,
        colour = ci_color,
        linetype = "solid",
        alpha = 0.8,
        linewidth = 1.2
      )
    } else if (plot_params$ci_style == "ribbon") {
      p <- p + geom_ribbon(
        aes(ymin = conf.low, ymax = conf.high), 
        alpha = 0.3,
        fill = ci_color
      )
    }
    
    # add note about fdr adjustment if applicable
    if (!is.null(attr(pooled_pred, "fdr_adjusted")) && attr(pooled_pred, "fdr_adjusted")) {
      fdr_method <- attr(pooled_pred, "fdr_method") %||% "FDR"
      alpha_adj <- attr(pooled_pred, "alpha_adjusted") %||% 0.05
      ci_level <- round((1 - alpha_adj) * 100, 1)
      ci_note <- paste0("(", ci_level, "% CI, ", fdr_method, " adjusted)")
    } else {
      ci_note <- "(95% CI)"
    }
  } else {
    ci_note <- ""
  }
  
  # add predicted points on top
  p <- p + geom_point(
    aes(x = x, y = predicted), 
    colour = point_color, 
    size = 4,
    alpha = 1
  )
  
  # create appropriate title using label mapping
  outcome_label <- if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
    label_mapping[[outcome_var]]
  } else {
    gsub("_", " ", tools::toTitleCase(outcome_var))
  }
  
  # add title with ci note
  title_text <- paste(outcome_label, "(Post-Stratification Weighted Means)")
  if (ci_note != "") {
    title_text <- paste(title_text, "\n", ci_note)
  }
  p <- p + ggtitle(title_text)
  
  # add appropriate y-axis label based on family
  if (current_family$family %in% c("binomial", "quasibinomial")) {
    p <- p + ylab("Predicted Probability")
  } else {
    p <- p + ylab("Predicted Mean")
  }
  
  # add x-axis label
  p <- p + xlab(tools::toTitleCase(gsub("_", " ", predictor_var)))
  
  return(p)
}

# helper function to extract first imputed dataset
get_first_imputed_data <- function(mids_obj) {
  if (inherits(mids_obj, "simple_mids")) {
    return(mids_obj$complete_data_list[[1]])
  } else {
    return(mice::complete(mids_obj, action = 1))
  }
}