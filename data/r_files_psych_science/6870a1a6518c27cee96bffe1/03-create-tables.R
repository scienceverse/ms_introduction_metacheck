# 03-create-tables.R
# functions and script for creating publication-ready tables
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
# TABLE CREATION FUNCTIONS
# ============================================================================

#' create comparison table with muslim vs non-muslim focus
#' 
#' @param results results object from margot_batch_glm_mice_enhanced
#' @param outcome_vars optional subset of outcomes to include
#' @param outcome_labels optional custom labels for outcomes
#' @param groups_to_compare groups to compare (default c("0", "1"))
#' @param digits number of decimal places
#' @param max_outcomes maximum number of outcomes to include
#' @param show_ci whether to show confidence intervals
#' @param ci_format format for CIs: "compact", "brackets", or "parentheses"
#' @param return_plots whether to return plots along with table
#' @param plot_ncol number of columns for plot layout
#' @param compact whether to use compact formatting
#' @param long_format whether to transpose table (outcomes as rows)
create_comparison_table <- function(results,
                                   outcome_vars = NULL,
                                   outcome_labels = NULL,
                                   groups_to_compare = c("0", "1"),  
                                   digits = 2,
                                   max_outcomes = NULL,
                                   show_ci = TRUE,
                                   ci_format = "compact",
                                   return_plots = FALSE,
                                   plot_ncol = 3,
                                   compact = TRUE,
                                   long_format = FALSE) {

  library(dplyr)
  library(flextable)
  library(tidyr)

  # auto-detect all outcome variables if not specified
  if (is.null(outcome_vars)) {
    outcome_vars <- names(results$predictions)
  }

  # limit number of outcomes if specified
  if (!is.null(max_outcomes) && length(outcome_vars) > max_outcomes) {
    outcome_vars <- outcome_vars[1:max_outcomes]
  }

  # auto-generate nice labels if not provided
  if (is.null(outcome_labels)) {
    outcome_labels <- outcome_vars |>
      gsub("religion_", "", x = _) |>
      gsub("_", " ", x = _) |>
      gsub("(^|\\s)([a-z])", "\\1\\U\\2", x = _, perl = TRUE) |>
      trimws()

    # special formatting for known patterns
    outcome_labels <- gsub("Binary$", "", outcome_labels)
    outcome_labels <- ifelse(grepl("believe.*god", outcome_vars, ignore.case = TRUE), "Belief in God", outcome_labels)
    outcome_labels <- ifelse(grepl("believe.*spirit", outcome_vars, ignore.case = TRUE), "Belief in Spirit", outcome_labels)
    outcome_labels <- ifelse(grepl("identification level", outcome_labels, ignore.case = TRUE), "Religious ID", outcome_labels)
    outcome_labels <- ifelse(grepl("spiritual identification", outcome_labels, ignore.case = TRUE), "Spiritual ID", outcome_labels)
    outcome_labels <- ifelse(grepl("church", outcome_labels, ignore.case = TRUE), "Religious Service/month", outcome_labels)
    outcome_labels <- ifelse(grepl("prayer", outcome_labels, ignore.case = TRUE), "Prayer/week", outcome_labels)
    outcome_labels <- ifelse(grepl("scripture", outcome_labels, ignore.case = TRUE), "Scripture/week", outcome_labels)
  }

  # auto-detect groups from muslim variable if not specified
  if (is.null(groups_to_compare)) {
    # check first outcome variable - get from muslim column
    first_pred <- results$predictions[[1]]
    if ("muslim" %in% names(first_pred)) {
      groups_to_compare <- as.character(unique(first_pred$muslim))
    } else {
      groups_to_compare <- as.character(first_pred$x)
    }
  }

  # handle group name display - convert 0/1 to meaningful labels
  groups_display <- groups_to_compare
  if (all(groups_to_compare %in% c("0", "1"))) {
    groups_display <- ifelse(groups_to_compare == "0", "Non-Muslim", "Muslim")
  }

  # extract data for each outcome
  all_data <- list()

  for (i in seq_along(outcome_vars)) {
    outcome_var <- outcome_vars[i]
    outcome_label <- outcome_labels[i]

    # get predictions for this outcome
    pred_data <- results$predictions[[outcome_var]]

    # filter to comparison groups - handle both x column and muslim column
    if ("muslim" %in% names(pred_data)) {
      df <- pred_data |>
        filter(muslim %in% groups_to_compare) |>
        as.data.frame() |>
        select(muslim, predicted, conf.low, conf.high) |>
        rename(x = muslim)
    } else {
      df <- pred_data |>
        filter(x %in% groups_to_compare) |>
        as.data.frame() |>
        select(x, predicted, conf.low, conf.high)
    }

    # detect if binary using the function
    is_binary <- is_binary_outcome(outcome_var)

    # format values with or without CI
    if (show_ci) {
      if (is_binary) {
        if (ci_format == "compact") {
          # more compact format with % signs
          df <- df |>
            mutate(
              formatted = sprintf("%.0f%%[%.0f%%,%.0f%%]",
                                predicted * 100,
                                conf.low * 100,
                                conf.high * 100)
            )
        } else if (ci_format == "brackets") {
          df <- df |>
            mutate(
              formatted = sprintf("%.0f%% [%.0f%%, %.0f%%]",
                                predicted * 100,
                                conf.low * 100,
                                conf.high * 100)
            )
        } else {
          df <- df |>
            mutate(
              formatted = sprintf("%.0f%% (%.0f%%-%.0f%%)",
                                predicted * 100,
                                conf.low * 100,
                                conf.high * 100)
            )
        }
      } else {
        if (ci_format == "compact") {
          # more compact format without spaces
          df <- df |>
            mutate(
              formatted = sprintf("%.*f[%.*f,%.*f]",
                                digits, predicted,
                                digits, conf.low,
                                digits, conf.high)
            )
        } else if (ci_format == "brackets") {
          df <- df |>
            mutate(
              formatted = sprintf("%.*f [%.*f, %.*f]",
                                digits, predicted,
                                digits, conf.low,
                                digits, conf.high)
            )
        } else {
          df <- df |>
            mutate(
              formatted = sprintf("%.*f (%.*f-%.*f)",
                                digits, predicted,
                                digits, conf.low,
                                digits, conf.high)
            )
        }
      }
    } else {
      if (is_binary) {
        df <- df |>
          mutate(formatted = sprintf("%.0f%%", predicted * 100))
      } else {
        df <- df |>
          mutate(formatted = sprintf("%.*f", digits, predicted))
      }
    }

    # keep only group and the formatted column
    df[[outcome_label]] <- df$formatted
    df$x <- groups_display[match(df$x, groups_to_compare)]  # use display names
    all_data[[outcome_label]] <- df |> select(x, all_of(outcome_label))
  }

  # merge all results
  if (long_format) {
    # create transposed format: one row per outcome, groups as columns
    long_data <- list()
    for (i in seq_along(all_data)) {
      outcome_name <- names(all_data)[i]
      df <- all_data[[i]]

      # reshape to have groups as columns
      df_wide <- df |>
        pivot_wider(names_from = x, values_from = all_of(outcome_name)) |>
        mutate(Outcome = outcome_name) |>
        select(Outcome, everything())

      long_data[[i]] <- df_wide
    }

    comparison_df <- bind_rows(long_data)

  } else {
    # create wide format table (original)
    comparison_df <- Reduce(function(x, y) merge(x, y, by = "x"), all_data) |>
      rename(Group = x)
  }

  # create flextable with format-specific styling
  if (long_format) {
    # transposed format: outcomes as rows, groups as columns
    ft <- flextable(comparison_df) |>
      theme_booktabs() |>
      fontsize(size = 8, part = "all") |>
      fontsize(size = 9, part = "header") |>
      bold(part = "header") |>
      align(j = 1, align = "left") |>
      align(j = -1, align = "center") |>
      border_remove() |>
      hline_top(part = "header", border = fp_border(width = 2)) |>
      hline_bottom(part = "header", border = fp_border(width = 1)) |>
      hline_bottom(part = "body", border = fp_border(width = 2)) |>
      width(j = 1, width = 2.5) |>  # wider for outcome names
      padding(padding = 2, part = "all")

    # set width for group columns (should be 2: Non-Muslim and Muslim)
    for (j in 2:ncol(comparison_df)) {
      if (compact && ci_format == "compact") {
        ft <- ft |> width(j = j, width = 1.3)
      } else {
        ft <- ft |> width(j = j, width = 1.8)
      }
    }

  } else {
    # original wide format styling
    ft <- flextable(comparison_df) |>
      theme_booktabs() |>
      fontsize(size = 7, part = "all") |>
      fontsize(size = 8, part = "header") |>
      bold(part = "header") |>
      align(j = 1, align = "left") |>
      align(j = -1, align = "center") |>
      border_remove() |>
      hline_top(part = "header", border = fp_border(width = 2)) |>
      hline_bottom(part = "header", border = fp_border(width = 1)) |>
      hline_bottom(part = "body", border = fp_border(width = 2)) |>
      width(j = 1, width = 0.9)

    # tighter column widths for wide format
    if (show_ci) {
      for (j in 2:ncol(comparison_df)) {
        if (compact && ci_format == "compact") {
          ft <- ft |> width(j = j, width = 0.8)
        } else {
          ft <- ft |> width(j = j, width = 1.2)
        }
      }
    } else {
      ft <- ft |> autofit()
    }

    # reduced padding for wide format
    ft <- ft |> padding(padding = 1, part = "all")
  }

  # if plots requested, get them
  if (return_plots) {
    # get available plot names
    available_plot_names <- names(results$plots)

    # extract plots matching our outcomes
    plots_list <- results$plots[outcome_vars[outcome_vars %in% available_plot_names]]

    combined_plot <- NULL
    if (length(plots_list) > 0) {
      combined_plot <- patchwork::wrap_plots(
        plots_list,
        ncol = plot_ncol,
        guides = "collect"
      ) +
      patchwork::plot_annotation(
        title = "Muslim vs Non-Muslim Comparative Visualizations",
        subtitle = "Adjusted for Age, Born in NZ, Ethnicity, and Gender",
        tag_levels = "A"
      )
    }

    return(list(
      table = ft,
      plots = combined_plot,
      plot_list = plots_list
    ))
  } else {
    return(ft)
  }
}

#' create master summary table for muslim vs non-muslim comparison
#' 
#' @param all_results list of results objects from all domains
#' @param groups_to_compare groups to compare (default c("0", "1"))
#' @param include_significance whether to include significance indicators
#' @param digits number of decimal places
#' @param auto_detect_outcomes whether to auto-detect all outcomes
#' @param custom_domain_config custom configuration for domains
#' @param row_padding control row spacing
#' @param font_size control text size
#' @param value_align control value column alignment
#' @param pdf_output optimise for PDF output
# create master summary for muslim vs non-muslim comparison
create_master_summary <- function(all_results, 
                                 groups_to_compare = c("0", "1"),
                                 include_significance = TRUE,
                                 digits = 2,
                                 auto_detect_outcomes = TRUE,
                                 custom_domain_config = NULL,
                                 row_padding = 1,
                                 font_size = 10,
                                 value_align = "left",
                                 pdf_output = FALSE) {
  
  library(dplyr)
  library(purrr)
  library(flextable)
  
  # first group is the reference
  reference_group <- groups_to_compare[1]
  comparison_groups <- groups_to_compare[-1]
  
  # create display names for groups
  reference_display <- ifelse(reference_group == "0", "Non-Muslim", "Muslim")
  comparison_display <- ifelse(comparison_groups == "0", "Non-Muslim", "Muslim")
  
  # auto-generate domain config or use custom/default
  if (auto_detect_outcomes && is.null(custom_domain_config)) {
    # auto-detect all outcomes from each results object
    domain_config <- list()
    
    # dynamically extract all results objects that start with "results_"
    results_objects <- all_results[grepl("^results_", names(all_results))]
    
    # create domain names by removing "results_" prefix and capitalising
    domain_names <- names(results_objects) |>
      gsub("^results_", "", x = _) |>
      gsub("(^|_)([a-z])", "\\1\\U\\2", x = _, perl = TRUE) |>
      gsub("_", " ", x = _)
    
    # create domain_results list with proper names
    domain_results <- results_objects
    names(domain_results) <- domain_names
    
    # remove any NULL results objects
    domain_results <- domain_results[!sapply(domain_results, is.null)]
    
    for (domain_name in names(domain_results)) {
      results_obj <- domain_results[[domain_name]]
      
      # skip if results_obj doesn't have predictions
      if (is.null(results_obj$predictions)) {
        next
      }
      
      # get all available outcome names
      all_outcomes <- names(results_obj$predictions)
      
      # auto-generate nice labels
      auto_labels <- all_outcomes |>
        gsub("religion_", "", x = _) |>
        gsub("kessler_latent_", "", x = _) |>
        gsub("perc_", "", x = _) |>
        gsub("_binary$", "", x = _) |>
        gsub("_", " ", x = _) |>
        gsub("(^|\\s)([a-z])", "\\1\\U\\2", x = _, perl = TRUE) |>
        trimws()
      
      # special formatting for known patterns
      auto_labels <- gsub("Believe God", "Belief in God", auto_labels)
      auto_labels <- gsub("Believe Spirit", "Belief in Spirit", auto_labels)
      auto_labels <- gsub("Identification Level", "Religious ID", auto_labels)
      auto_labels <- gsub("Spiritual Identification", "Spiritual ID", auto_labels)
      auto_labels <- gsub("Church", "Religious Service/month", auto_labels)
      auto_labels <- gsub("Prayer", "Prayer/week", auto_labels)
      auto_labels <- gsub("Scripture", "Scripture/week", auto_labels)
      auto_labels <- gsub("Religious Discrim", "Religious Discrimination", auto_labels)
      auto_labels <- gsub("Gend Discrim", "Gender Discrimination", auto_labels)
      auto_labels <- gsub("Discrim$", "Ethnic Discrimination", auto_labels)
      auto_labels <- gsub("Short Form Health", "Subjective Health", auto_labels)
      auto_labels <- gsub("Hlth Sleep Hours", "Sleep Hours", auto_labels)
      auto_labels <- gsub("Lifesat", "Life Satisfaction", auto_labels)
      auto_labels <- gsub("Meaning Purpose", "Meaning & Purpose", auto_labels)
      
      # add binary indicator for percentages
      if (exists("is_binary_outcome")) {
        binary_outcomes <- sapply(all_outcomes, is_binary_outcome)
      } else {
        # fallback logic
        binary_outcomes <- sapply(all_outcomes, function(x) grepl("_binary$", x) || grepl("^perc_", x))
      }
      auto_labels[binary_outcomes] <- paste0(auto_labels[binary_outcomes], " (%)")
      
      domain_config[[domain_name]] <- list(
        results = results_obj,
        outcomes = all_outcomes,
        labels = auto_labels
      )
    }
    
  } else if (!is.null(custom_domain_config)) {
    # use provided custom config
    domain_config <- custom_domain_config
  }
  
  # collect all data
  all_data <- list()
  
  for (domain_name in names(domain_config)) {
    config <- domain_config[[domain_name]]
    
    for (i in seq_along(config$outcomes)) {
      outcome <- config$outcomes[i]
      label <- config$labels[i]
      
      # get predictions
      pred_data <- config$results$predictions[[outcome]]
      
      # filter to comparison groups - handle both x column and muslim column
      if ("muslim" %in% names(pred_data)) {
        df <- pred_data |>
          filter(muslim %in% groups_to_compare) |>
          as.data.frame() |>
          arrange(match(muslim, groups_to_compare))
      } else {
        df <- pred_data |>
          filter(x %in% groups_to_compare) |>
          as.data.frame() |>
          arrange(match(x, groups_to_compare))
      }
      
      # get reference values (first group)
      ref_val <- df$predicted[1]
      ref_low <- df$conf.low[1]
      ref_high <- df$conf.high[1]
      
      # detect if binary using pattern matching (fallback if is_binary_outcome doesn't exist)
      if (exists("is_binary_outcome")) {
        is_binary <- is_binary_outcome(outcome)
      } else {
        # fallback logic - detect binary outcomes by name patterns
        is_binary <- grepl("_binary$", outcome) || grepl("^perc_", outcome)
      }
      
      # format values with CIs
      if (is_binary) {
        values_with_ci <- sprintf("%.0f%% [%.0f, %.0f]", 
                                 df$predicted * 100,
                                 df$conf.low * 100,
                                 df$conf.high * 100)
        diff <- sprintf("%+.0f%%", (df$predicted - ref_val) * 100)
      } else {
        values_with_ci <- sprintf("%.*f [%.*f, %.*f]", 
                                 digits, df$predicted,
                                 digits, df$conf.low,
                                 digits, df$conf.high)
        diff <- sprintf("%+.*f", digits, df$predicted - ref_val)
      }
      
      # check significance for non-reference groups
      sig <- character(length(groups_to_compare))
      if (include_significance && length(groups_to_compare) > 1) {
        for (j in 2:length(groups_to_compare)) {
          # check if CIs don't overlap
          if (df$conf.low[j] > ref_high || df$conf.high[j] < ref_low) {
            sig[j] <- "***"
          } else if (df$conf.low[j] > ref_val || df$conf.high[j] < ref_val) {
            sig[j] <- "*"
          }
        }
      }
      
      # create row
      row_data <- data.frame(
        Domain = domain_name,
        Measure = label,
        stringsAsFactors = FALSE
      )
      
      # add reference group column
      row_data[[reference_display]] <- values_with_ci[1]
      
      # add comparison group columns with differences
      for (j in 2:length(groups_to_compare)) {
        group_display <- comparison_display[j-1]
        row_data[[group_display]] <- paste0(values_with_ci[j], sig[j])
        row_data[[paste0("Δ from ", reference_display)]] <- paste0(diff[j], sig[j])
      }
      
      all_data[[length(all_data) + 1]] <- row_data
    }
  }
  
  # combine all rows
  summary_df <- bind_rows(all_data)
  
  # check if we have data
  if (nrow(summary_df) == 0) {
    warning("No data to create summary table")
    return(NULL)
  }
  
  # create flextable
  ft <- flextable::flextable(summary_df) |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(j = 1:2, align = "left") |>
    flextable::align(j = -(1:2), align = value_align) |>
    # merge cells for domain
    flextable::merge_v(j = 1) |>
    flextable::valign(j = 1, valign = "top") |>
    # formatting
    flextable::bold(j = 1) |>
    flextable::italic(j = 2) |>
    # borders
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 2)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 2)) |>
    # add vertical line after reference group
    flextable::vline(j = 3, border = officer::fp_border(width = 1, color = "gray70"))
  
  # handle padding differently for PDF vs other formats
  if (pdf_output) {
    # for PDF output, use table properties instead of padding
    ft <- ft |>
      flextable::set_table_properties(opts_pdf = list(tabcolsep = max(0.5, row_padding * 0.5))) |>
      flextable::width(j = 1, width = 1.0) |>
      flextable::width(j = 2, width = 2.0) |>
      flextable::autofit()
  } else {
    # for HTML/Word output, use normal padding
    ft <- ft |>
      flextable::padding(padding.top = row_padding, padding.bottom = row_padding, 
                        padding.left = 3, padding.right = 3, part = "all") |>
      flextable::width(j = 1, width = 1.0) |>
      flextable::width(j = 2, width = 2.0) |>
      flextable::autofit()
  }
  
  # colour significant cells
  for (i in seq_along(comparison_groups)) {
    comp_group_display <- comparison_display[i]
    
    # get column index
    col_idx <- which(names(summary_df) == comp_group_display)
    
    if (length(col_idx) > 0) {
      # find rows with significance markers
      rows_three_stars <- which(grepl("\\*\\*\\*", summary_df[[comp_group_display]]))
      rows_one_star <- which(grepl("\\*", summary_df[[comp_group_display]]) & 
                            !grepl("\\*\\*\\*", summary_df[[comp_group_display]]))
      
      # apply colours
      if (length(rows_three_stars) > 0) {
        ft <- ft |> flextable::color(i = rows_three_stars, j = col_idx, color = "#D73502")
      }
      if (length(rows_one_star) > 0) {
        ft <- ft |> flextable::color(i = rows_one_star, j = col_idx, color = "#E69F00")
      }
    }
    
    # also colour the difference column
    diff_col <- paste0("Δ from ", reference_display)
    diff_col_idx <- which(names(summary_df) == diff_col)
    
    if (length(diff_col_idx) > 0) {
      rows_three_stars_diff <- which(grepl("\\*\\*\\*", summary_df[[diff_col]]))
      rows_one_star_diff <- which(grepl("\\*", summary_df[[diff_col]]) & 
                                  !grepl("\\*\\*\\*", summary_df[[diff_col]]))
      
      if (length(rows_three_stars_diff) > 0) {
        ft <- ft |> flextable::color(i = rows_three_stars_diff, j = diff_col_idx, color = "#D73502")
      }
      if (length(rows_one_star_diff) > 0) {
        ft <- ft |> flextable::color(i = rows_one_star_diff, j = diff_col_idx, color = "#E69F00")
      }
    }
  }
  
  # add title
  ft <- ft |>
    flextable::add_header_lines(
      values = paste0("Muslim vs Non-Muslim Comparative Analysis: ", reference_display, " as Reference Group")
    ) |>
    flextable::bold(part = "header", i = 1) |>
    flextable::fontsize(size = font_size + 2, part = "header", i = 1) |>
    flextable::align(align = "center", part = "header", i = 1)
  
  # add explanatory footer
  footer_text <- c(
    paste0("Values show estimates with 95% confidence intervals [lower, upper]. ",
           "Δ = difference from ", reference_display, "."),
    "Estimates from 20 multiply imputed datasets using Rubin's rule with design-based survey weights.",
    "Statistical significance: *** = confidence intervals do not overlap (p < 0.01), * = confidence intervals do not overlap with reference point estimate (p < 0.05).",
    "Colour coding: Red (***) = highly reliable difference, Orange (*) = reliable difference, Black = no reliable difference detected."
  )
  
  for (text in footer_text) {
    ft <- ft |> 
      flextable::add_footer_lines(values = text) |>
      flextable::fontsize(size = max(8, font_size - 2), part = "footer") |>
      flextable::italic(part = "footer")
  }
  
  return(ft)
}

#' quick table function for rapid output
#' 
#' @param results results object from margot_batch_glm_mice_enhanced
#' @param max_cols maximum number of columns/outcomes
#' @param plot_ncol number of columns for plot layout
#' @param compact whether to use compact formatting
quick_table <- function(results, max_cols = 12, plot_ncol = 3, compact = TRUE) {
  
  # get the table with compact format
  table <- create_comparison_table(
    results,
    groups_to_compare = NULL,  # NULL means use all groups
    max_outcomes = max_cols,
    ci_format = "compact",
    compact = compact
  )
  
  # get plot names directly from the plots object
  available_plots <- names(results$plots)
  
  # limit if needed
  if (!is.null(max_cols) && length(available_plots) > max_cols) {
    available_plots <- available_plots[1:max_cols]
  }
  
  # extract plots
  plots_list <- results$plots[available_plots]
  
  # combine plots using patchwork
  combined_plot <- NULL
  if (length(plots_list) > 0) {
    combined_plot <- patchwork::wrap_plots(
      plots_list, 
      ncol = plot_ncol,
      guides = "collect"
    ) + 
    patchwork::plot_annotation(
      title = "Comparative Visualizations",
      subtitle = "Adjusted for Age, Born in NZ, Ethnicity, and Gender",
      tag_levels = "A"
    )
  }
  
  # return both as a list
  return(list(
    table = table,
    plots = combined_plot,
    plot_list = plots_list
  ))
}

# ============================================================================
# CREATE TABLES
# ============================================================================

# create output directory for tables
# use project_root if available
if (exists("project_root")) {
  tables_dir <- file.path(project_root, "output", "tables")
} else {
  tables_dir <- here::here("output", "tables")
}
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
}

# 1. create master summary table
cat("\n=== Creating master summary table ===\n")
master_summary <- create_master_summary(
  all_results, 
  value_align = "left", 
  font_size = 9, 
  row_padding = 0
)

# save as docx
save_as_docx(
  master_summary,
  path = file.path(tables_dir, "master_summary_table.docx")
)
cat("Saved: master_summary_table.docx\n")

# 2. create individual domain tables
cat("\n=== Creating domain-specific tables ===\n")
for (result_name in names(all_results)) {
  domain <- gsub("results_", "", result_name)
  
  # create comparison table with long format
  domain_table <- create_comparison_table(
    all_results[[result_name]],
    compact = TRUE, 
    long_format = TRUE
  )
  
  # save
  save_as_docx(
    domain_table,
    path = file.path(tables_dir, paste0("table_", domain, ".docx"))
  )
  cat(sprintf("Saved: table_%s.docx\n", domain))
}

# 3. create quick summary tables with plots
cat("\n=== Creating quick summary tables ===\n")
for (result_name in names(all_results)) {
  domain <- gsub("results_", "", result_name)
  
  quick_result <- quick_table(
    all_results[[result_name]],
    max_cols = 20,
    plot_ncol = 3
  )
  
  # save table
  save_as_docx(
    quick_result$table,
    path = file.path(tables_dir, paste0("quick_table_", domain, ".docx"))
  )
  cat(sprintf("Saved: quick_table_%s.docx\n", domain))
}

cat("\n=== Table creation complete ===\n")
cat("Tables saved to:", tables_dir, "\n")