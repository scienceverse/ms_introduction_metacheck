# helper functions for muslim diversity study analysis
# extracted from the main analysis code for modularity

# null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# function to check if a variable is binary based on its name
is_binary_outcome <- function(outcome_name) {
  # only these specific variables are binary
  binary_vars <- c(
    "religion_believe_god_binary",
    "religion_believe_spirit_binary"
  )
  
  return(outcome_name %in% binary_vars)
}

# helper function to extract first imputed dataset
get_first_imputed_data <- function(mids_obj) {
  if (inherits(mids_obj, "simple_mids")) {
    return(mids_obj$complete_data_list[[1]])
  } else {
    return(mice::complete(mids_obj, action = 1))
  }
}

# create plot name mapping for consistent labelling
create_plot_name_mapping <- function(results = NULL) {
  # common mappings based on your data
  mapping <- list(
    # religion mappings
    "religion_believe_god_binary" = "Belief in God",
    "religion_believe_spirit_binary" = "Belief in Spirit", 
    "religion_church" = "Monthly Religious Service Attendence",
    "religion_identification_level" = "Religious Identification Level",
    "religion_prayer" = "Weekly Prayer",
    "religion_scripture" = "Weekly Scripture",
    "religion_spiritual_identification" = "Spiritual Identification Level",
    
    # health mappings
    "hlth_sleep_hours" = "Sleep",
    "log_hours_exercise" = "Hours of Exercise (log)",
    "short_form_health" = "Short Form Health",
    
    # psych mappings
    "hlth_fatigue" = "Fatigue",
    "kessler_latent_anxiety" = "Anxiety",
    "kessler_latent_depression" = "Depression",
    "rumination" = "Rumination",
    
    # discrimination mappings
    "perc_age_discrim" = "Perceived Age Discrimination",
    "perc_discrim" = "Perceived Ethnic Discrimination",
    "perc_gend_discrim" = "Perceived Gender Discrimination",
    "perc_religious_discrim" = "Perceived Religious Discrimination",
    
    # social mappings
    "belong_accept" = "Belonging: Acceptance",
    "belong_beliefs" = "Belonging: Shared Beliefs",
    "belong_outsider_reversed" = "Belonging: Not an Outsider",
    "neighbourhood_community" = "Neighbourhood Community",
    "support_help" = "Support: Help Available",
    "support_noguidance_reversed" = "Support: Guidance Available",
    "support_turnto" = "Support: Someone to Turn To",
    
    # present-reflective mappings
    "forgiveness" = "Forgiveness",
    "perfectionism" = "Perfectionism",
    "self_esteem" = "Self-Esteem",
    "pwb_your_future_security" = "PWB: Future Security",
    "pwb_standard_living" = "PWB: Standard of Living",
    "pwb_your_health" = "PWB: Your Health",
    "pwb_your_relationships" = "PWB: Your Relationships",
    
    # life-reflective mappings
    "gratitude" = "Gratitude",
    "lifesat" = "Life Satisfaction",
    "meaning_purpose" = "Meaning & Purpose",
    "meaning_sense" = "Sense of Meaning"
  )
  
  return(mapping)
}

# function to transform labels for display
transform_label <- function(label, label_mapping = NULL, options = list()) {
  # coerce each flag to a single TRUE/FALSE
  remove_tx_prefix   <- isTRUE(options$remove_tx_prefix)
  remove_z_suffix    <- isTRUE(options$remove_z_suffix)
  remove_underscores <- isTRUE(options$remove_underscores)
  use_title_case     <- isTRUE(options$use_title_case)
  
  original_label <- label
  
  # 1) explicit mappings
  if (!is.null(label_mapping)) {
    for (pat in names(label_mapping)) {
      if (grepl(pat, label, fixed = TRUE)) {
        repl  <- label_mapping[[pat]]
        label <- gsub(pat, repl, label, fixed = TRUE)
      }
    }
  }
  
  # 2) strip trailing numeric-range suffix
  label <- sub(" - \\(.*\\]$", "", label)
  
  # 3) default transforms if unmapped
  if (identical(label, original_label)) {
    if (remove_tx_prefix)   label <- sub("^t[0-9]+_", "", label)
    if (remove_z_suffix)    label <- sub("_z$", "", label)
    if (remove_underscores) label <- gsub("_", " ", label, fixed = TRUE)
    
    if (use_title_case) {
      # convert to title case and enforce specific uppercase acronyms
      label <- tools::toTitleCase(label)
      label <- gsub("Nz",  "NZ",  label, fixed = TRUE)
      label <- gsub("Sdo", "SDO", label, fixed = TRUE)
      label <- gsub("Rwa", "RWA", label, fixed = TRUE)
    }
  }
  
  return(label)
}