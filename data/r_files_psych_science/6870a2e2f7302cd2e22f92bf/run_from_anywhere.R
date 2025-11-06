# run_from_anywhere.R
# Simple wrapper to run the Muslim Diversity Study analysis from any location
# 
# Instructions:
# 1. Copy this file to wherever you want to run the analysis from
# 2. Edit the project_path below to point to your 25-muslim-diversity-study-osf-code folder
# 3. Run this script

# EDIT THIS PATH to point to your project folder
project_path <- "/path/to/25-muslim-diversity-study-osf-code"

# Verify the path exists
if (!dir.exists(project_path)) {
  stop("Project directory not found at: ", project_path, 
       "\nPlease edit this script and set project_path to the correct location.")
}

# Verify it's the right directory
if (!file.exists(file.path(project_path, "run_analysis_example.R"))) {
  stop("This doesn't appear to be the correct project directory.\n",
       "Could not find run_analysis_example.R in: ", project_path)
}

# Set the environment variable so the main script can find the project
Sys.setenv(MDS_PROJECT_PATH = project_path)

# Source the main analysis script
cat("Running Muslim Diversity Study analysis...\n")
cat("Project location:", project_path, "\n\n")

source(file.path(project_path, "run_analysis_example.R"))