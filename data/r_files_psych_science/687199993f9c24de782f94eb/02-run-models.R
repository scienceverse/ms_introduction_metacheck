# 02-run-models.R
# template for running the muslim diversity study models
# this script assumes you have:
# 1. run 01-setup.R
# 2. have a mids object (multiply imputed data) available

# note: actual data not included for privacy
# this template shows how to run the analysis with your own data

# check that setup has been run
if (!exists("ANALYSIS_PARAMS")) {
  stop("Please run 01-setup.R first to load parameters and functions")
}

# ============================================================================
# LOAD DATA (PLACEHOLDER)
# ============================================================================
# in practice, you would load your mids object here
# the mids object should be created with group-aware imputation
# where muslim identity is used as an auxiliary variable

# example (not run):
# mids_data <- readRDS("path/to/your/imputed_data.rds")

# for this template, we'll create a message
if (!exists("mids_data")) {
  message("
  ========================================
  DATA LOADING TEMPLATE
  ========================================
  
  To run this analysis, you need a mids object with:
  
  1. All outcome variables listed in OUTCOME_DOMAINS
  2. Exposure variable: muslim (0/1)
  3. Covariates: age, eth_cat, male_binary, born_nz_binary
  4. Weight variable: adjusted_weights
  5. Group-aware imputation (muslim used as auxiliary variable)
  
  Example code to create mids object:
  
  # define predictor matrix for group-aware imputation
  pred_matrix <- quickpred(your_data)
  pred_matrix[, 'muslim'] <- 1  # muslim predicts all other variables
  pred_matrix['muslim', ] <- 0  # muslim is not imputed
  
  # run mice
  mids_data <- mice(
    your_data,
    m = 20,
    method = 'pmm',
    predictorMatrix = pred_matrix,
    seed = 123
  )
  
  ========================================
  ")
  stop("Please load your mids_data object before continuing")
}

# ============================================================================
# RUN MODELS BY DOMAIN
# ============================================================================

# initialize results storage
all_results <- list()

# function to run models for a single domain
run_domain_models <- function(domain_name, outcome_vars, mids_obj) {
  
  cat(sprintf("\n=== Running models for %s domain ===\n", domain_name))
  cat(sprintf("Number of outcomes: %d\n", length(outcome_vars)))
  
  # get families for this domain's outcomes
  domain_families <- OUTCOME_FAMILIES[names(OUTCOME_FAMILIES) %in% outcome_vars]
  
  # run the population-standardised contrast function
  results <- margot_poststrat_contrast_mi(
    mids_obj = mids_obj,
    outcome_vars = outcome_vars,
    exposure_formula = EXPOSURE_FORMULA,
    terms = ANALYSIS_PARAMS$exposure_var,
    weight_col = ANALYSIS_PARAMS$weight_col,
    family_list = domain_families,
    n_imputations = ANALYSIS_PARAMS$n_imputations,
    fdr_method = ANALYSIS_PARAMS$fdr_method,
    alpha = ANALYSIS_PARAMS$alpha,
    plot_params = ANALYSIS_PARAMS$plot_params,
    label_mapping = LABEL_MAPPING,
    x_label = "Muslim Status",
    theme_params = theme_classic()
  )
  
  # print summary
  cat("\nResults summary:\n")
  print(results$results_summary[, c("outcome", "estimate", "ci_lower", "ci_upper", 
                                    "p_value", "p_adjusted", "significant_adjusted")])
  
  return(results)
}

# run models for each domain
for (domain_name in names(OUTCOME_DOMAINS)) {
  
  tryCatch({
    results <- run_domain_models(
      domain_name = domain_name,
      outcome_vars = OUTCOME_DOMAINS[[domain_name]],
      mids_obj = mids_data
    )
    
    # store results with consistent naming
    all_results[[paste0("results_", domain_name)]] <- results
    
  }, error = function(e) {
    warning(sprintf("Error in %s domain: %s", domain_name, e$message))
  })
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

# create output directory if it doesn't exist
# use project_root if available, otherwise here::here()
if (exists("project_root")) {
  output_dir <- file.path(project_root, "output")
} else {
  output_dir <- here::here("output")
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# save individual domain results
cat("\n=== Saving results ===\n")
for (result_name in names(all_results)) {
  saveRDS(
    all_results[[result_name]], 
    file = file.path(output_dir, paste0(result_name, ".rds"))
  )
  cat(sprintf("Saved: %s\n", result_name))
}

# save combined results
saveRDS(all_results, file = file.path(output_dir, "all_results.rds"))
cat("Saved: all_results.rds\n")

# ============================================================================
# CREATE SUMMARY REPORT
# ============================================================================

# count significant results by domain
summary_stats <- data.frame(
  domain = character(),
  n_outcomes = integer(),
  n_significant_raw = integer(),
  n_significant_adjusted = integer(),
  stringsAsFactors = FALSE
)

for (result_name in names(all_results)) {
  domain <- gsub("results_", "", result_name)
  results_summary <- all_results[[result_name]]$results_summary
  
  summary_stats <- rbind(summary_stats, data.frame(
    domain = domain,
    n_outcomes = nrow(results_summary),
    n_significant_raw = sum(results_summary$significant_raw),
    n_significant_adjusted = sum(results_summary$significant_adjusted),
    stringsAsFactors = FALSE
  ))
}

cat("\n=== Analysis Summary ===\n")
print(summary_stats)

# save summary stats
write.csv(summary_stats, file = file.path(output_dir, "summary_statistics.csv"), 
          row.names = FALSE)

cat("\n=== Analysis complete ===\n")
cat("Results saved to:", output_dir, "\n")
cat("Next steps:\n")
cat("1. Run 03-create-tables.R to generate publication tables\n")
cat("2. Run 04-create-plots.R to generate figures\n")

