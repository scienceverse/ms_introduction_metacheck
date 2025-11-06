# 04-create-plots.R
# functions and script for creating publication-ready plots
# extracted from the quarto manuscript code

# check that setup has been run
if (!exists("ANALYSIS_PARAMS")) {
  if (exists("project_root")) {
    source(file.path(project_root, "01-setup.R"))
  } else {
    source(here::here("01-setup.R"))
  }
}

# load results if not already loaded
if (!exists("all_results")) {
  # use project_root if available
  if (exists("project_root")) {
    output_dir <- file.path(project_root, "output")
  } else {
    output_dir <- here::here("output")
  }
  if (file.exists(file.path(output_dir, "all_results.rds"))) {
    all_results <- readRDS(file.path(output_dir, "all_results.rds"))
  } else {
    stop("Please run 02-run-models.R first to generate results")
  }
}

# ============================================================================
# PLOTTING FUNCTIONS
# ============================================================================

#' combine plots across multiple domains from all_results structure
#' 
#' @param all_results list of results objects from all domains
#' @param max_plots maximum total number of plots to include
#' @param plot_ncol number of columns in the plot layout
#' @param domain_subset optional subset of domains to include
combine_all_domain_plots <- function(all_results, max_plots = 35, plot_ncol = 5, 
                                     domain_subset = NULL) {
  
  # create name mapping for consistent labelling
  plot_name_mapping <- create_plot_name_mapping()
  
  # extract all plots from all domains
  all_plots <- list()
  all_plot_names <- character()
  domain_info <- character()
  
  # get domain names, optionally subset
  domain_names <- names(all_results)
  if (!is.null(domain_subset)) {
    domain_names <- domain_names[domain_names %in% domain_subset]
  }
  
  # clean up domain names for display (remove "results_" prefix)
  clean_domain_names <- stringr::str_remove(domain_names, "^results_")
  clean_domain_names <- stringr::str_to_title(clean_domain_names)
  
  # loop through each results object
  for (i in seq_along(domain_names)) {
    domain_key <- domain_names[i]
    results <- all_results[[domain_key]]
    clean_domain_name <- clean_domain_names[i]
    
    # get available plots from this domain
    if (!is.null(results$plots)) {
      domain_plots <- results$plots
      domain_plot_names <- names(domain_plots)
      
      # add to collection
      for (plot_name in domain_plot_names) {
        plot_obj <- domain_plots[[plot_name]]
        
        # create unique identifier
        unique_name <- paste0(clean_domain_name, ": ", plot_name)
        all_plots[[unique_name]] <- plot_obj
        all_plot_names <- c(all_plot_names, unique_name)
        domain_info <- c(domain_info, clean_domain_name)
      }
    }
  }
  
  # limit total plots if requested
  if (!is.null(max_plots) && length(all_plots) > max_plots) {
    all_plots <- all_plots[1:max_plots]
    all_plot_names <- all_plot_names[1:max_plots]
    domain_info <- domain_info[1:max_plots]
  }
  
  # check if we have any plots
  if (length(all_plots) == 0) {
    warning("No plots found in the provided results")
    return(NULL)
  }
  
  # create combined plot using patchwork
  combined_plot <- patchwork::wrap_plots(
    all_plots,
    ncol = plot_ncol,
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Muslim vs Non-Muslim Comparative Analysis Across All Domains",
    subtitle = paste0("Showing ", length(all_plots), " outcomes adjusted for age, ethnicity, gender, and NZ birth"),
    caption = "Note: Error bars represent 95% confidence intervals adjusted for multiple comparisons (Bonferroni)",
    tag_levels = NULL  # remove individual tags since we have many plots
  )
  
  # create summary info
  domain_summary <- table(domain_info)
  
  message("Combined plots from ", length(unique(domain_info)), " domains:")
  for (d in names(domain_summary)) {
    message("  ", d, ": ", domain_summary[d], " plots")
  }
  
  return(list(
    plots = combined_plot,
    plot_names = all_plot_names,
    domain_info = domain_info,
    n_plots = length(all_plots),
    n_domains = length(unique(domain_info))
  ))
}

#' create combined plot for specific domains only
#' 
#' @param all_results list of results objects from all domains
#' @param domains character vector of domain names to include
#' @param ... additional arguments passed to combine_all_domain_plots
create_specific_domain_plots <- function(all_results, domains, ...) {
  
  # validate domains
  if (is.null(domains) || length(domains) == 0) {
    stop("Please specify at least one domain")
  }
  
  # ensure domains have "results_" prefix
  domains <- ifelse(grepl("^results_", domains), domains, paste0("results_", domains))
  
  # check which domains exist
  available <- domains %in% names(all_results)
  if (!any(available)) {
    stop("None of the specified domains found in results")
  }
  
  if (!all(available)) {
    missing <- domains[!available]
    warning("Some domains not found: ", paste(missing, collapse = ", "))
  }
  
  # use only available domains
  selected_domains <- domains[available]
  
  # create combined analysis
  combined_results <- combine_all_domain_plots(
    all_results = all_results,
    domain_subset = selected_domains,
    ...
  )
  
  return(combined_results)
}

#' quick function to get domain overview
summarise_all_results <- function(all_results) {
  summary_df <- data.frame(
    domain = names(all_results),
    n_outcomes = sapply(all_results, function(x) x$n_outcomes),
    n_imputations = sapply(all_results, function(x) x$n_imputations),
    plot_names = sapply(all_results, function(x) {
      if (!is.null(x$plots)) {
        paste(names(x$plots), collapse = ", ")
      } else {
        "No plots"
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}

#' simple wrapper function for quick cross-domain plotting
#' 
#' @param all_results list of results objects from all domains
#' @param ncol number of columns in the plot layout
quick_cross_domain_plots <- function(all_results, ncol = 5) {
  # create the combined plot
  combined <- combine_all_domain_plots(
    all_results = all_results,
    plot_ncol = ncol
  )
  
  # return just the plot for quick viewing
  return(combined$plots)
}

#' create individual domain plots with custom layout
#' 
#' @param results single results object from one domain
#' @param plot_title custom title for the plot
#' @param plot_ncol number of columns
#' @param plot_nrow number of rows (if NULL, calculated automatically)
create_domain_plot <- function(results, 
                              plot_title = NULL,
                              plot_ncol = 3,
                              plot_nrow = NULL) {
  
  if (is.null(results$plots) || length(results$plots) == 0) {
    warning("No plots found in results object")
    return(NULL)
  }
  
  # get all plots
  plots_list <- results$plots
  n_plots <- length(plots_list)
  
  # calculate layout
  if (is.null(plot_nrow)) {
    plot_nrow <- ceiling(n_plots / plot_ncol)
  }
  
  # create combined plot
  combined <- patchwork::wrap_plots(
    plots_list,
    ncol = plot_ncol,
    nrow = plot_nrow,
    guides = "collect"
  )
  
  # add title if provided
  if (!is.null(plot_title)) {
    combined <- combined + 
      patchwork::plot_annotation(
        title = plot_title,
        subtitle = "Adjusted for age, ethnicity, gender, and born in NZ",
        tag_levels = "A"
      )
  }
  
  return(combined)
}

# ============================================================================
# CREATE PLOTS
# ============================================================================

# create output directory for plots
# use project_root if available
if (exists("project_root")) {
  plots_dir <- file.path(project_root, "output", "plots")
} else {
  plots_dir <- here::here("output", "plots")
}
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# 1. create combined plot across all domains
cat("\n=== Creating combined cross-domain plot ===\n")
all_plots <- quick_cross_domain_plots(all_results, ncol = 5)

if (!is.null(all_plots)) {
  ggsave(
    filename = file.path(plots_dir, "all_domains_combined.pdf"),
    plot = all_plots,
    width = ANALYSIS_PARAMS$plot_width,
    height = ANALYSIS_PARAMS$plot_height,
    dpi = 300
  )
  cat("Saved: all_domains_combined.pdf\n")
  
  # also save as PNG for easier viewing
  ggsave(
    filename = file.path(plots_dir, "all_domains_combined.png"),
    plot = all_plots,
    width = ANALYSIS_PARAMS$plot_width,
    height = ANALYSIS_PARAMS$plot_height,
    dpi = 300
  )
  cat("Saved: all_domains_combined.png\n")
}

# 2. create individual domain plots
cat("\n=== Creating individual domain plots ===\n")
for (result_name in names(all_results)) {
  domain <- gsub("results_", "", result_name)
  
  # create domain-specific plot
  domain_plot <- create_domain_plot(
    results = all_results[[result_name]],
    plot_title = paste0(stringr::str_to_title(domain), " Domain Outcomes"),
    plot_ncol = 3
  )
  
  if (!is.null(domain_plot)) {
    # save as PDF
    ggsave(
      filename = file.path(plots_dir, paste0("domain_", domain, ".pdf")),
      plot = domain_plot,
      width = 12,
      height = ceiling(length(all_results[[result_name]]$plots) / 3) * 3,
      dpi = 300
    )
    cat(sprintf("Saved: domain_%s.pdf\n", domain))
    
    # save as PNG
    ggsave(
      filename = file.path(plots_dir, paste0("domain_", domain, ".png")),
      plot = domain_plot,
      width = 12,
      height = ceiling(length(all_results[[result_name]]$plots) / 3) * 3,
      dpi = 300
    )
    cat(sprintf("Saved: domain_%s.png\n", domain))
  }
}

# 3. create subset plots for key domains
cat("\n=== Creating subset plots for key comparisons ===\n")

# religion and discrimination together
religion_discrim_plot <- create_specific_domain_plots(
  all_results,
  domains = c("religion", "discrimination"),
  max_plots = 20,
  plot_ncol = 4
)

if (!is.null(religion_discrim_plot)) {
  ggsave(
    filename = file.path(plots_dir, "religion_discrimination_combined.pdf"),
    plot = religion_discrim_plot$plots,
    width = 14,
    height = 10,
    dpi = 300
  )
  cat("Saved: religion_discrimination_combined.pdf\n")
}

# wellbeing domains (health, psychological, life outcomes)
wellbeing_plot <- create_specific_domain_plots(
  all_results,
  domains = c("health", "psychological", "life_reflective"),
  max_plots = 20,
  plot_ncol = 4
)

if (!is.null(wellbeing_plot)) {
  ggsave(
    filename = file.path(plots_dir, "wellbeing_combined.pdf"),
    plot = wellbeing_plot$plots,
    width = 30,
    height = 30,
    dpi = 300
  )
  cat("Saved: wellbeing_combined.pdf\n")
}

# 4. create summary overview
cat("\n=== Creating results summary ===\n")
summary_overview <- summarise_all_results(all_results)
write.csv(
  summary_overview,
  file = file.path(plots_dir, "plot_summary.csv"),
  row.names = FALSE
)
cat("Saved: plot_summary.csv\n")

# 5. save individual plots as separate files (optional)
if (ANALYSIS_PARAMS$save_plots) {
  cat("\n=== Saving individual plots ===\n")
  individual_dir <- file.path(plots_dir, "individual")
  if (!dir.exists(individual_dir)) {
    dir.create(individual_dir)
  }
  
  for (result_name in names(all_results)) {
    domain <- gsub("results_", "", result_name)
    results <- all_results[[result_name]]
    
    if (!is.null(results$plots)) {
      for (plot_name in names(results$plots)) {
        plot_obj <- results$plots[[plot_name]]
        
        # create clean filename
        clean_name <- gsub("[^A-Za-z0-9_-]", "_", plot_name)
        filename <- paste0(domain, "_", clean_name, ".png")
        
        ggsave(
          filename = file.path(individual_dir, filename),
          plot = plot_obj,
          width = 30,
          height = 30,
          dpi = 300
        )
      }
    }
  }
  cat("Individual plots saved to:", individual_dir, "\n")
}

cat("\n=== Plot creation complete ===\n")
cat("Plots saved to:", plots_dir, "\n")