# run_analysis_example.R
# complete example workflow for the muslim diversity study analysis
# this script shows how to run the full analysis pipeline
# **author**: Joseph Bulbulia
# **contact**: joseph.bulbulia@vuw.ac.nz
# **date**: 2024-JUL-11

# ============================================================================
# SETUP
# ============================================================================

# clear environment
rm(list = ls())

# establish project root using multiple methods
# option 1: check environment variable
# option 2: if running from the project directory
# option 3: try to find the script and work backwards
# option 4: ask user to set manually

# Method 1: Check if project_root is already set (for interactive use)
if (exists("project_root") && dir.exists(project_root)) {
  message("Using existing project_root variable: ", project_root)
} else {
  # Method 2: Check for environment variable
  project_root <- Sys.getenv("MDS_PROJECT_PATH", "")
  
  if (project_root != "" && dir.exists(project_root)) {
    message("Using project root from MDS_PROJECT_PATH environment variable: ", project_root)
  } else if (file.exists("01-setup.R")) {
    # Method 3: We're already in the project root
    project_root <- getwd()
    message("Running from project root directory: ", project_root)
  } else {
    # Method 4: Try to find where this script is located and work from there
    script_path <- NULL
  
    # Try various methods to get the script path
    tryCatch({
      # Check if being sourced
      if (sys.nframe() > 0) {
        # Look for ofile in any frame
        for (i in seq_len(sys.nframe())) {
          if (exists("ofile", where = sys.frame(i))) {
            script_path <- get("ofile", sys.frame(i))
            break
          }
        }
      }
    }, error = function(e) NULL)
    
    # If not found and in interactive mode, try rstudioapi
    if (is.null(script_path) && interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
      tryCatch({
        script_path <- rstudioapi::getActiveDocumentContext()$path
        if (script_path == "") script_path <- NULL
      }, error = function(e) NULL)
    }
    
    if (!is.null(script_path) && file.exists(script_path)) {
      # Check if the script is in a known project structure
      script_dir <- dirname(script_path)
      possible_roots <- c(
        script_dir,  # script is in project root
        dirname(script_dir),  # script is in a subdirectory
        file.path(dirname(script_dir), "25-muslim-diversity-study-osf-code")  # common parent structure
      )
      
      for (possible_root in possible_roots) {
        if (file.exists(file.path(possible_root, "01-setup.R")) && 
            file.exists(file.path(possible_root, "run_analysis_example.R"))) {
          project_root <- possible_root
          message("Found project root at: ", project_root)
          break
        }
      }
    }
    
    # Method 5: If still not found, provide helpful error message
    if (is.null(project_root) || project_root == "") {
      stop("\nCould not automatically detect the project directory.\n",
           "Please use one of these methods:\n\n",
           "1. Set project_root variable before running:\n",
           "   project_root <- '/path/to/25-muslim-diversity-study-osf-code'\n",
           "   source('run_analysis_example.R')  # or paste the code\n\n",
           "2. Navigate to the project directory first:\n",
           "   setwd('/path/to/25-muslim-diversity-study-osf-code')\n",
           "   source('run_analysis_example.R')\n\n",
           "3. Set an environment variable:\n",
           "   Sys.setenv(MDS_PROJECT_PATH = '/path/to/25-muslim-diversity-study-osf-code')\n",
           "   source('/path/to/25-muslim-diversity-study-osf-code/run_analysis_example.R')\n\n",
           "4. Use the run_from_anywhere.R wrapper script\n")
    }
  }
}

# now we can source files using the project root
source(file.path(project_root, "01-setup.R"))

# ============================================================================
# DATA PREPARATION (EXAMPLE)
# ============================================================================

# note: actual participant data not included
# this section shows how to prepare your data

# option 1: load existing mids object
# mids_data <- readRDS("path/to/your/imputed_data.rds")

# option 2: create new imputation from raw data
prepare_data_for_analysis <- function(raw_data_path) {
  
  # load raw data
  # raw_data <- read.csv(raw_data_path)
  
  # for this example, we'll create synthetic data
  # replace this with your actual data loading
  source(file.path(project_root, "data-templates", "example_data_structure.R"))
  raw_data <- create_example_data_structure(n = 32000)
  
  # ensure muslim variable is properly coded
  raw_data$muslim <- as.integer(raw_data$muslim)
  
  # set up group-aware imputation
  imp_setup <- setup_group_aware_imputation(raw_data)
  
  # run mice with progress bar
  cat("\n=== Running multiple imputation ===\n")
  cat("This may take several minutes...\n")
  
  mids_data <- mice(
    raw_data,
    m = ANALYSIS_PARAMS$n_imputations,
    method = imp_setup$method,
    predictorMatrix = imp_setup$predictorMatrix,
    seed = 123,
    printFlag = TRUE,
    maxit = 5  # increase if needed for convergence
  )
  
  # check convergence
  cat("\n=== Checking imputation convergence ===\n")
  plot(mids_data)
  
  # save imputed data
  saveRDS(mids_data, "mids_data_example.rds")
  cat("\nImputed data saved to: mids_data_example.rds\n")
  
  return(mids_data)
}

# create example data (comment out and load your real data instead)
mids_data <- prepare_data_for_analysis()

# ============================================================================
# RUN MAIN ANALYSIS
# ============================================================================

cat("\n=== Running main analysis ===\n")
cat("This will fit models for all", sum(sapply(OUTCOME_DOMAINS, length)), "outcomes\n")

# source and run the models script
source(file.path(project_root, "02-run-models.R"))

# check that results were created
if (exists("all_results")) {
  cat("\n✓ Analysis completed successfully\n")
  cat("Number of domains analyzed:", length(all_results), "\n")
  
  # print summary of results
  for (domain in names(all_results)) {
    n_sig <- sum(all_results[[domain]]$results_summary$significant_adjusted)
    n_total <- nrow(all_results[[domain]]$results_summary)
    cat(sprintf("  %s: %d/%d significant after correction\n", 
                gsub("results_", "", domain), n_sig, n_total))
  }
}

# ============================================================================
# CREATE TABLES
# ============================================================================

cat("\n=== Creating publication tables ===\n")
source(file.path(project_root, "03-create-tables.R"))

# list created tables
tables_dir <- file.path(project_root, "output", "tables")
if (dir.exists(tables_dir)) {
  table_files <- list.files(tables_dir, pattern = "\\.docx$")
  cat("\nTables created:\n")
  for (f in table_files) {
    cat("  -", f, "\n")
  }
}

# ============================================================================
# CREATE PLOTS
# ============================================================================

cat("\n=== Creating figures ===\n")
source(file.path(project_root, "04-create-plots.R"))

# list created plots
plots_dir <- file.path(project_root, "output", "plots")
if (dir.exists(plots_dir)) {
  plot_files <- list.files(plots_dir, pattern = "\\.(pdf|png)$")
  cat("\nPlots created:\n")
  for (f in plot_files) {
    cat("  -", f, "\n")
  }
}

# ============================================================================
# OPTIONAL: CUSTOM ANALYSES
# ============================================================================

# example: create table for specific domains only
cat("\n=== Running custom analyses ===\n")

# 1. religion and discrimination only
religion_discrim_results <- list(
  results_religion = all_results$results_religion,
  results_discrimination = all_results$results_discrimination
)

religion_discrim_table <- create_master_summary(
  religion_discrim_results,
  font_size = 10,
  row_padding = 1
)

# save custom table
save_as_docx(
  religion_discrim_table,
  path = file.path(project_root, "output", "tables", "custom_religion_discrimination.docx")
)
cat("Created custom table: custom_religion_discrimination.docx\n")

# 2. create simplified plot for presentation
key_outcomes_plot <- create_specific_domain_plots(
  all_results,
  domains = c("religion", "life_reflective"),
  max_plots = 8,
  plot_ncol = 4
)

if (!is.null(key_outcomes_plot)) {
  ggsave(
    filename = file.path(project_root, "output", "plots", "key_outcomes_presentation.png"),
    plot = key_outcomes_plot$plots,
    width = 12,
    height = 6,
    dpi = 300
  )
  cat("Created presentation plot: key_outcomes_presentation.png\n")
}

# ============================================================================
# SESSION INFO
# ============================================================================

cat("\n=== Session Information ===\n")
session_info <- sessionInfo()
cat("\nR version:", session_info$R.version$version.string, "\n")
cat("\nKey package versions:\n")
key_packages <- c("mice", "survey", "ggeffects", "ggplot2", "flextable")
for (pkg in key_packages) {
  if (pkg %in% names(session_info$otherPkgs)) {
    cat(sprintf("  %s: %s\n", pkg, session_info$otherPkgs[[pkg]]$Version))
  }
}

# save session info
writeLines(capture.output(sessionInfo()), 
           file.path(project_root, "output", "session_info.txt"))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n")
cat("==================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==================================================\n")
cat("\nOutputs saved to:", file.path(project_root, "output"), "\n")
cat("\nKey files:\n")
cat("  - all_results.rds: Complete model results\n")
cat("  - tables/master_summary_table.docx: Main results table\n")
cat("  - plots/all_domains_combined.pdf: All outcomes visualization\n")
cat("  - session_info.txt: R session information\n")
cat("\nNext steps:\n")
cat("1. Review tables and plots for accuracy\n")
cat("2. Check significant findings against hypotheses\n")
cat("3. Prepare manuscript tables and figures\n")
cat("4. Consider sensitivity analyses\n")
cat("==================================================\n")