# Population-standardised contrasts for multiply imputed survey data
# Version: 1.1.0
# Author: Joseph Bulbulia
# Date: 2025-01-12

# null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Population-Standardised Contrasts for Multiply Imputed Survey Data
#' 
#' @description
#' Quantifies group differences using survey-weighted generalised linear models 
#' fitted to multiply-imputed data. Implements the methodology described in 
#' Bulbulia et al. (2025) for population-representative inference with proper 
#' handling of missing data and complex survey designs.
#' 
#' The function:
#' 1. Fits survey-weighted GLMs to each imputed dataset
#' 2. Computes population-average predictions using empirical marginalization
#' 3. Pools contrasts across imputations using Rubin's rules
#' 4. Applies multiple testing corrections (optional)
#' 
#' @details
#' For each imputed dataset, builds a survey design object and fits:
#' Y = β₀ + β₁Muslim + β₂Age + β₃Ethnicity + β₄Gender + β₅BornNZ + ε
#' 
#' Population-standardised predictions are obtained via:
#' ggeffects::predict_response(model, terms = "Muslim", margin = "empirical", weights = weights(design))
#' 
#' This yields fully standardised marginal means that are pooled using mitools::MIcombine().
#' 
#' @param mids_obj A 'mids' object from mice with group-aware imputation
#' @param outcome_vars Character vector of outcome variable names
#' @param exposure_formula Formula for exposure and covariates (default: "~ muslim + age + eth_cat + male_binary + born_nz_binary")
#' @param terms Character string specifying terms for marginal predictions (default: "muslim")
#' @param weight_col Character string naming the post-stratification weight column
#' @param family_list Named list mapping outcomes to GLM families (gaussian/binomial/quasibinomial)
#' @param n_imputations Number of imputations to use (default: all available)
#' @param fdr_method Multiple testing correction: "none", "bonferroni", "holm", "bh" (default: "bonferroni")
#' @param alpha Family-wise error rate (default: 0.10)
#' @param label_mapping Named list for outcome label mapping in outputs
#' @param x_label Character string for x-axis label in plots
#' @param y_label Character string for y-axis label in plots
#' @param y_limits Numeric vector of length 2 for y-axis limits
#' @param robust_se Logical whether to use robust standard errors (ignored with survey weights)
#' @param robust_type Type of robust standard errors if robust_se = TRUE
#' @param standardise Logical whether to standardise effect sizes
#' @param standardise_method Method for standardisation: "cohen_d", "hedges_g", "glass_delta"
#' @param include_design_effects Logical whether to calculate design effects
#' @param plot_params List of plotting parameters (see details)
#' @param theme_params ggplot2 theme for plots (default: theme_classic())
#' @param return_individual Logical whether to return individual imputation results
#' 
#' @return List containing:
#' \itemize{
#'   \item predictions: Marginal predictions with population-weighted means
#'   \item plots: ggplot objects with adjusted confidence intervals
#'   \item results_summary: Pooled contrasts with p-values and significance indicators
#'   \item n_imputations: Number of imputations used
#'   \item n_outcomes: Number of outcomes analyzed
#'   \item fdr_method: Multiple testing method applied
#'   \item individual_predictions: If return_individual = TRUE, list of predictions per imputation
#' }
#' 
#' @references
#' Bulbulia, J. A., Afzali, M. U., Rasheed, A., Troughton, G., Yogeeswaran, K., & Sibley, C. G. (2025). 
#' They Are Us: Introducing the New Zealand Muslim Diversity Study. Manuscript in preparation.
#' 
#' Rubin, D. B. (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.
#' 
#' @import mice
#' @import survey  
#' @import ggeffects
#' @import mitools
#' @import dplyr
#' @import ggplot2
#' @importFrom stats pt qt family coef vcov weights p.adjust
#' @importFrom utils head
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Group-aware imputation
#' pred_matrix <- quickpred(data)
#' pred_matrix[, "muslim"] <- 1  # Muslim predicts all
#' pred_matrix["muslim", ] <- 0  # Muslim never imputed
#' mids_data <- mice(data, m = 20, predictorMatrix = pred_matrix)
#' 
#' # Run analysis
#' results <- margot_poststrat_contrast_mi(
#'   mids_obj = mids_data,
#'   outcome_vars = c("life_satisfaction", "belonging"),
#'   weight_col = "adjusted_weights",
#'   fdr_method = "bonferroni",
#'   alpha = 0.10
#' )
#' }
margot_poststrat_contrast_mi <- function(
    mids_obj,
    outcome_vars,
    exposure_formula = "~ muslim + age + eth_cat + male_binary + born_nz_binary",
    terms = "muslim",
    label_mapping = NULL,
    x_label = NULL,
    y_label = NULL,
    y_limits = NULL,
    weight_col = NULL,
    family_list = NULL,
    n_imputations = NULL,
    robust_se = FALSE,
    robust_type = "HC3",
    fdr_method = c("none", "bh", "holm", "bonferroni"),
    alpha = 0.10,
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
    theme_params = theme_classic(),
    return_individual = FALSE
) {
  
  # validate inputs
  fdr_method <- match.arg(fdr_method)
  standardise_method <- match.arg(standardise_method)
  
  # check required packages
  required_packages <- c("mice", "ggeffects", "survey", "mitools", "dplyr", "ggplot2")
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
  
  # warn about redundant robust_se when using post-stratification weights
  if (!is.null(weight_col) && robust_se) {
    message("note: robust_se is ignored when using post-stratification weights")
  }
  
  # determine number of imputations
  if (inherits(mids_obj, "simple_mids")) {
    m <- n_imputations %||% mids_obj$m
  } else {
    m <- n_imputations %||% mids_obj$m
  }
  
  if (m > mids_obj$m) {
    warning(paste("requested", m, "imputations but only", mids_obj$m, "available. using", mids_obj$m))
    m <- mids_obj$m
  }
  
  # storage for results
  predictions_list <- list()
  plots_list <- list()
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
        ids = ~1,  # no clustering
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
    
    # fit models for each imputed dataset
    fitlist <- vector("list", m)
    
    for (j in 1:m) {
      
      if (!is.null(weight_col)) {
        # post-stratification weighted model using pre-created design
        fitlist[[j]] <- survey::svyglm(formula_obj, design = designs[[j]], family = current_family)
        
        if (j == 1) {
          message("✓ using svyglm with post-stratification weights for ", outcome_var)
          
          # diagnostic: check weight variability
          if (grepl("muslim", terms, ignore.case = TRUE)) {
            design_data <- designs[[j]]$variables
            muslim_sizes <- table(design_data$muslim)
            
            # effective sample size calculation for post-strat weights
            muslim_eff_n <- by(design_data[[weight_col]], design_data$muslim, function(w) {
              sum(w)^2 / sum(w^2)
            })
            
            message("  group sizes: ", paste(names(muslim_sizes), "=", muslim_sizes, collapse = ", "))
            message("  effective n: ", paste(names(muslim_eff_n), "=", round(muslim_eff_n), collapse = ", "))
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
        fitlist[[j]] <- glm(formula_obj, data = data_j, family = current_family)
      }
    }
    
    # generate predictions using predict_response with empirical margins
    pred_m <- lapply(seq_along(fitlist), function(idx) {
      mod <- fitlist[[idx]]
      if (!is.null(weight_col)) {
        # get the data from the survey design which includes the weight column
        model_data <- designs[[idx]]$variables
        
        # for survey models, use predict_response with explicit weights
        # provide newdata explicitly to ensure weight column is available
        ggeffects::predict_response(
          mod, 
          terms = terms,
          margin = "empirical",
          newdata = model_data,
          weights = weight_col  # use weight column name
        )
      } else {
        # for standard models
        if (robust_se) {
          robust_vcov <- sandwich::vcovHC(mod, type = robust_type)
          ggeffects::predict_response(mod, terms = terms, vcov = robust_vcov)
        } else {
          ggeffects::predict_response(mod, terms = terms)
        }
      }
    })
    
    # validation message for first outcome
    if (!is.null(weight_col) && i == 1) {
      first_pred <- pred_m[[1]]
      if (length(first_pred$predicted) == 2) {
        diff_weighted <- abs(first_pred$predicted[2] - first_pred$predicted[1])
        message("  weighted contrast: ", round(diff_weighted, 4), 
                " (post-stratification weights applied)")
      }
    }
    
    # pool contrasts using mitools
    if (length(pred_m[[1]]$predicted) == 2) {  # binary exposure
      # extract contrasts (difference between groups)
      contrasts <- sapply(pred_m, function(x) diff(x$predicted))
      # variance of difference: var(X2 - X1) = var(X2) + var(X1)
      variances <- sapply(pred_m, function(x) x$std.error[1]^2 + x$std.error[2]^2)
      
      # prepare lists for MIcombine: results as list of coefficient vectors, variances as list of 1x1 matrices
      estimates_list <- lapply(contrasts, function(est) c(contrast = est))  # named for clarity
      vars_list <- lapply(variances, function(var) matrix(var, 1, 1, dimnames = list("contrast", "contrast")))
      
      # pool using mitools with separate results and variances arguments
      pooled_contrast <- tryCatch({
        mitools::MIcombine(results = estimates_list, variances = vars_list)
      }, error = function(e) {
        stop("error pooling contrasts with MIcombine for ", outcome_var, ":\n", 
             "  ", conditionMessage(e), "\n",
             "  contrasts: ", paste(round(contrasts, 4), collapse = ", "), "\n",
             "  variances: ", paste(round(variances, 6), collapse = ", "))
      })
      
      # extract pooled results
      pooled_estimate <- pooled_contrast$coefficients
      pooled_se <- sqrt(pooled_contrast$variance)
      pooled_df <- pooled_contrast$df
      
      # cap df at a reasonable maximum when between-imputation variance is near zero
      max_df <- nrow(designs[[1]]$variables) - 10
      if (!is.finite(pooled_df) || pooled_df > max_df) {
        original_df <- pooled_df
        pooled_df <- max_df
        message("  note: df capped at ", round(pooled_df), " (was ", 
                format(original_df, scientific = TRUE), " - between-imputation variance near zero)")
      }
      
      # create pooled prediction object for consistency with rest of function
      pooled_pred <- pred_m[[1]]  # use structure from first imputation
      
      # update with pooled marginal means
      marginal_means <- sapply(pred_m, function(x) x$predicted)
      pooled_pred$predicted <- rowMeans(marginal_means)
      
      # update standard errors using delta method approximation
      # for marginal means, pool variances accounting for between-imputation variance
      for (k in 1:2) {
        mean_k <- mean(marginal_means[k,])
        var_within <- mean(sapply(pred_m, function(x) x$std.error[k]^2))
        var_between <- var(marginal_means[k,])
        var_total <- var_within + (1 + 1/m) * var_between
        pooled_pred$std.error[k] <- sqrt(var_total)
      }
      
      # update confidence intervals
      t_crit <- qt(0.975, df = pooled_df)
      pooled_pred$conf.low <- pooled_pred$predicted - t_crit * pooled_pred$std.error
      pooled_pred$conf.high <- pooled_pred$predicted + t_crit * pooled_pred$std.error
      
      # note: not adding df column to maintain compatibility with downstream processing
      # df is stored in attributes if needed
      
      # add pooling attributes
      attr(pooled_pred, "pooling_method") <- "mitools_empirical_margins"
      attr(pooled_pred, "n_imputations") <- m
      attr(pooled_pred, "pooled_contrast") <- list(
        estimate = pooled_estimate,
        se = pooled_se,
        df = pooled_df
      )
      
      message("✓ pooled predictions for ", outcome_var, " using MIcombine with empirical margins")
      message("  contrast: ", round(pooled_estimate, 4), " (SE: ", round(pooled_se, 4), 
              ", df: ", round(pooled_df, 1), ")")
      
    } else {
      # for non-binary exposures, use standard pooling
      pooled_pred <- pool_predictions_standard(pred_m, m, outcome_var)
    }
    
    # check and constrain for probability models
    if (current_family$family %in% c("binomial", "quasibinomial")) {
      pooled_pred$predicted <- pmax(pmin(pooled_pred$predicted, 0.999), 0.001)
      pooled_pred$conf.low <- pmax(pooled_pred$conf.low, 0)
      pooled_pred$conf.high <- pmin(pooled_pred$conf.high, 1)
    }
    
    # store results
    predictions_list[[outcome_var]] <- pooled_pred
    if (return_individual) {
      individual_predictions[[outcome_var]] <- pred_m
    }
    
    # compile results for summary table
    result_row <- compute_results_summary(
      pooled_pred = pooled_pred,
      outcome_var = outcome_var,
      current_family = current_family,
      label_mapping = label_mapping,
      pooled_contrast = attr(pooled_pred, "pooled_contrast")
    )
    
    results_list[[outcome_var]] <- result_row
    
    # plots will be created after fdr adjustment
  }
  
  # combine results into data frame with explicit column ordering
  expected_cols <- c("outcome", "outcome_var", "estimate", "se", "ci_lower", 
                     "ci_upper", "t_statistic", "df", "p_value", 
                     "significant_raw", "scale")
  
  results_summary <- do.call(rbind, lapply(results_list, function(row) {
    # ensure all expected columns exist
    for (col in expected_cols) {
      if (!col %in% names(row)) {
        warning("Missing column ", col, " in results row for ", row$outcome_var)
        row[[col]] <- NA
      }
    }
    # return only expected columns in correct order
    row[expected_cols]
  }))
  rownames(results_summary) <- NULL
  
  # apply fdr correction and adjust confidence intervals
  if (fdr_method != "none" && nrow(results_summary) > 1) {
    message("applying ", fdr_method, " correction for multiple testing...")
    
    # check for any NA or invalid p-values
    if (any(is.na(results_summary$p_value))) {
      warning("NA p-values found in results_summary for outcomes: ",
              paste(results_summary$outcome_var[is.na(results_summary$p_value)], collapse = ", "))
      # replace NA with 1 (most conservative)
      results_summary$p_value[is.na(results_summary$p_value)] <- 1
    }
    
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


# Helper functions --------------------------------------------------------

# standard pooling for non-binary exposures
pool_predictions_standard <- function(pred_m, m, outcome_var) {
  
  # get structure from first prediction
  first_pred <- pred_m[[1]]
  n_levels <- nrow(first_pred)
  
  # initialize result
  pooled_result <- first_pred
  
  # pool each prediction level separately
  for (level_idx in 1:n_levels) {
    
    # extract estimates and standard errors
    estimates <- sapply(pred_m, function(pred) pred$predicted[level_idx])
    std_errors <- sapply(pred_m, function(pred) pred$std.error[level_idx])
    
    # rubin's pooling components
    Q_bar <- mean(estimates)  # pooled estimate
    U_bar <- mean(std_errors^2)  # within-imputation variance
    B <- var(estimates)  # between-imputation variance
    
    # total variance
    T <- U_bar + (1 + 1/m) * B
    pooled_se <- sqrt(T)
    
    # degrees of freedom
    if (B > 0) {
      df_rubin <- (m - 1) * (1 + (1/m) * (U_bar / B))^2
      df_final <- pmax(pmin(df_rubin, 1000), m - 1)
    } else {
      df_final <- min(1000, max(100, m * 10))
    }
    
    # confidence intervals
    t_crit <- qt(0.975, df = df_final)
    margin_of_error <- t_crit * pooled_se
    
    # update pooled result
    pooled_result$predicted[level_idx] <- Q_bar
    pooled_result$std.error[level_idx] <- pooled_se
    pooled_result$conf.low[level_idx] <- Q_bar - margin_of_error
    pooled_result$conf.high[level_idx] <- Q_bar + margin_of_error
    # store df in attributes instead of column to maintain compatibility
    if (level_idx == 1) {
      attr(pooled_result, "df") <- df_final
    }
  }
  
  attr(pooled_result, "pooling_method") <- "standard_rubin"
  attr(pooled_result, "n_imputations") <- m
  
  message("✓ standard pooling for ", outcome_var, " (df: ", 
          round(attr(pooled_result, "df"), 1), ")")
  
  return(pooled_result)
}

# compute results summary with pooled contrast information
compute_results_summary <- function(pooled_pred, outcome_var, current_family, 
                                    label_mapping, pooled_contrast = NULL) {
  
  # use pooled contrast if available (binary exposure)
  if (!is.null(pooled_contrast)) {
    # check pooled_contrast structure
    if (!all(c("estimate", "se", "df") %in% names(pooled_contrast))) {
      # try to extract from different structure
      if ("coefficients" %in% names(pooled_contrast)) {
        estimate_display <- pooled_contrast$coefficients
        se_display <- sqrt(pooled_contrast$variance)
        df_avg <- pooled_contrast$df
      } else {
        stop("Cannot extract values from pooled_contrast")
      }
    } else {
      estimate_display <- pooled_contrast$estimate
      se_display <- pooled_contrast$se
      df_avg <- pooled_contrast$df
    }
    
    # confidence interval for contrast
    t_crit <- qt(0.975, df = df_avg)
    ci_lower_display <- estimate_display - t_crit * se_display
    ci_upper_display <- estimate_display + t_crit * se_display
    
  } else {
    # fallback for non-binary exposures
    if (nrow(pooled_pred) == 2) {
      estimate_display <- pooled_pred$predicted[2] - pooled_pred$predicted[1]
      se_display <- sqrt(pooled_pred$std.error[1]^2 + pooled_pred$std.error[2]^2)
      # get df from attributes since we removed the column
      df_avg <- attr(pooled_pred, "df") %||% 1000
      t_crit <- qt(0.975, df = df_avg)
      ci_lower_display <- estimate_display - t_crit * se_display
      ci_upper_display <- estimate_display + t_crit * se_display
    } else {
      estimate_display <- pooled_pred$predicted[1]
      se_display <- pooled_pred$std.error[1]
      ci_lower_display <- pooled_pred$conf.low[1]
      ci_upper_display <- pooled_pred$conf.high[1]
      # get df from attributes since we removed the column
      df_avg <- attr(pooled_pred, "df") %||% 1000
    }
  }
  
  # scale labels
  if (current_family$family %in% c("binomial", "quasibinomial")) {
    scale_label <- "probability_difference"
  } else {
    scale_label <- "mean_difference"
  }
  
  # apply label mapping
  outcome_label <- if (!is.null(label_mapping) && outcome_var %in% names(label_mapping)) {
    label_mapping[[outcome_var]]
  } else {
    gsub("_", " ", tools::toTitleCase(outcome_var))
  }
  
  # compute test statistics
  t_stat <- estimate_display / se_display
  
  # cap df for p-value calculation to avoid numerical issues
  df_for_pval <- df_avg
  if (!is.finite(df_for_pval) || df_for_pval > 10000) {
    df_for_pval <- 10000
  }
  
  p_value <- 2 * pt(abs(t_stat), df = df_for_pval, lower.tail = FALSE)
  
  # ensure all values are scalars (not vectors)
  estimate_display <- as.numeric(estimate_display)[1]
  se_display <- as.numeric(se_display)[1]
  ci_lower_display <- as.numeric(ci_lower_display)[1]
  ci_upper_display <- as.numeric(ci_upper_display)[1]
  t_stat <- as.numeric(t_stat)[1]
  df_avg <- as.numeric(df_avg)[1]
  p_value <- as.numeric(p_value)[1]
  
  # create result row
  result_row <- data.frame(
    outcome = outcome_label,
    outcome_var = outcome_var,
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
    is_probability_model <- all(pred$predicted >= 0 & pred$predicted <= 1, na.rm = TRUE)
    
    # calculate new critical values
    df_val <- attr(pred, "df") %||% 1000
    t_crit_new <- qt(1 - alpha_adj/2, df = df_val)
    
    # recalculate confidence intervals using adjusted critical values
    margin_of_error_new <- t_crit_new * pred$std.error
    
    conf_low_new <- pred$predicted - margin_of_error_new
    conf_high_new <- pred$predicted + margin_of_error_new
    
    # for probability models, ensure bounds are respected
    if (is_probability_model) {
      conf_low_new <- pmax(conf_low_new, 0)
      conf_high_new <- pmin(conf_high_new, 1)
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

# custom prediction plotting function with proper confidence intervals
create_custom_prediction_plot <- function(pooled_pred, first_imputed_data, terms, 
                                          outcome_var, plot_params, theme_params, 
                                          current_family, label_mapping) {
  
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
    # check ci range and width
    ci_range <- range(c(pooled_pred$conf.low, pooled_pred$conf.high), na.rm = TRUE)
    ci_widths <- pooled_pred$conf.high - pooled_pred$conf.low
    max_width <- max(ci_widths, na.rm = TRUE)
    
    message("✓ plotting CIs for ", outcome_var, 
            " (range: ", round(ci_range[1], 3), " to ", round(ci_range[2], 3), 
            ", max width: ", round(max_width, 3), ")")
  }
  
  # determine colours
  point_color <- if (!is.null(plot_params$colors) && plot_params$colors == "metro") {
    "dodgerblue"
  } else if (!is.null(plot_params$colors) && plot_params$colors == "viridis") {
    "darkgreen"
  } else {
    "red"
  }
  
  ci_color <- point_color  # use same color for consistency
  
  # start with base plot
  p <- ggplot(pooled_pred, aes(x = x, y = predicted)) +
    theme_params
  
  # add raw data points if requested
  if (plot_params$show_data) {
    point_alpha <- plot_params$dot_alpha %||% 0.1
    
    p <- p + geom_jitter(
      data = first_imputed_data,
      aes(x = .data[[predictor_var]], y = .data[[outcome_var]]),
      width = plot_params$jitter %||% 0.4,
      alpha = point_alpha,
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