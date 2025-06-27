# Load all required packages
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(purrr)     # Functional programming
library(openxlsx)  # Excel I/O

# Check for optional packages
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)  # Load ggplot2 if available
} else {
  warning("Package 'ggplot2' not found. Plotting functionality will not be available.")
}
if (!requireNamespace("fs", quietly = TRUE)) {
  stop("Package 'fs' is required for directory creation.")
}

# Automatically create output directories for both steps
fs::dir_create("Output_files/Step_1", recurse = TRUE)
fs::dir_create("Output_files/Step_2", recurse = TRUE)
fs::dir_create("Output_files/log", recurse = TRUE)  # Create log directory

# Create timestamp for log file
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path("Output_files/log", paste0("pipeline_log_", timestamp, ".txt"))

# Simple logging function that writes INFO messages to both console and file
# Warnings and messages will go to console as usual (not captured in log file)
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  
  # Write to console
  cat(formatted_msg, "\n")
  
  # Write to log file (only for INFO messages)
  if (level == "INFO") {
    cat(formatted_msg, "\n", file = log_file, append = TRUE)
  }
}

# Log initial setup
log_message("Logging system initialized", "INFO")
log_message(paste("Log file created:", log_file), "INFO")
log_message("Note: Warnings and messages will appear in console only", "INFO")
log_message(paste("Current working directory:", getwd()), "INFO")

# =============================================================================
# CENTRALIZED UTILITY FUNCTIONS
# =============================================================================

#' Generate standardized output file path
#' @param file_prefix File prefix
#' @param output_dir Output directory
#' @param extension File extension (default: ".csv")
#' @return Complete file path with date
generate_output_path <- function(file_prefix, output_dir, extension = ".csv") {
  file_date <- format(Sys.Date(), "%Y%m%d")
  file.path(output_dir, paste0(file_prefix, "_", file_date, extension))
}

#' Safe file existence check with informative error
#' @param file_path Path to check
#' @param context Context for error message
#' @return TRUE if file exists, stops with error if not
check_file_exists <- function(file_path, context = "file") {
  if (!file.exists(file_path)) {
    stop(sprintf("%s not found: %s", context, file_path))
  }
  TRUE
}

#' Safe directory creation with logging
#' @param dir_path Directory path to create
#' @param context Context for logging
#' @return TRUE if successful
ensure_directory <- function(dir_path, context = "directory") {
  if (!fs::dir_exists(dir_path)) {
    fs::dir_create(dir_path, recurse = TRUE)
    log_message(sprintf("Created %s: %s", context, dir_path))
  }
  TRUE
}

#' Standardized error handling wrapper
#' @param expr Expression to evaluate
#' @param error_msg Error message prefix
#' @param context Additional context
#' @return Result of expression or stops with formatted error
safe_execute <- function(expr, error_msg = "Operation failed", context = "") {
  tryCatch({
    expr
  }, error = function(e) {
    full_msg <- if (nchar(context) > 0) {
      sprintf("%s (%s): %s", error_msg, context, e$message)
    } else {
      sprintf("%s: %s", error_msg, e$message)
    }
    stop(full_msg)
  })
}

#' Standardized data splitting for Excel output
#' @param data Data frame to split
#' @param config Configuration list
#' @param type Either "pc" or "pp"
#' @return List of split data frames
split_data_for_output <- function(data, config, type = "pc") {
  split_var <- get_split_var(config, data, type)
  split(data, data[[split_var]], drop = TRUE)
}

#' Standardized Excel output with common patterns
#' @param data_list List of data frames
#' @param file_prefix File prefix
#' @param output_dir Output directory
#' @param context Context for logging
#' @return Invisibly returns output file path
write_standardized_excel <- function(data_list, file_prefix, output_dir, context = "data") {
  # Handle long sheet names
  data_list <- handle_long_sheet_names(data_list, file_prefix, output_dir)
  
  # Write Excel file
  output_file <- write_excel_outputs(data_list, file_prefix, output_dir)
  log_message(sprintf("Wrote %s Excel to %s", context, output_dir))
  
  invisible(output_file)
}

# Utility functions for grouping variable management
#' Get defensive grouping variables for PC data
#' @param config Configuration list
#' @param data Data frame to check against
#' @return Vector of present grouping variables
get_pc_group_vars <- function(config, data) {
  if ("pc_nontime_group_vars" %in% names(config) && length(config$pc_nontime_group_vars) > 0) {
    present_vars <- intersect(config$pc_nontime_group_vars, names(data))
    if (length(present_vars) > 0) {
      return(present_vars)
    }
  }
  if ("Cohort" %in% names(data)) {
    return("Cohort")
  }
  return("default_group")
}

#' Get defensive grouping variables for PP data
#' @param config Configuration list
#' @param data Data frame to check against
#' @return Vector of present grouping variables
get_pp_group_vars <- function(config, data) {
  if ("pp_group_vars" %in% names(config) && length(config$pp_group_vars) > 0 && 
      all(config$pp_group_vars %in% names(data))) {
    return(config$pp_group_vars)
  }
  if ("Cohort" %in% names(data)) {
    return("Cohort")
  }
  return("default_group")
}

#' Get primary split variable for data splitting
#' @param config Configuration list
#' @param data Data frame to check against
#' @param type Either "pc" or "pp"
#' @return Single variable name for splitting
get_split_var <- function(config, data, type = "pc") {
  if (type == "pc") {
    group_vars <- get_pc_group_vars(config, data)
  } else {
    group_vars <- get_pp_group_vars(config, data)
  }
  
  if (length(group_vars) > 0) {
    return(group_vars[1])
  }
  return("default_group")
}

#' Generate unit row for data frame columns
#' @param df Data frame
#' @return Vector of units matching column names
generate_unit_row <- function(df) {
  sapply(names(df), function(col) {
    if (grepl("Concentration", col)) "ng/mL"
    else if (grepl("Time", col, ignore.case = TRUE)) "h"
    else ""
  })
}

#' Handle long sheet names for Excel output
#' @param data_list Named list of data frames
#' @param file_prefix Prefix for mapping file
#' @param output_dir Output directory
#' @return List with potentially renamed data frames
handle_long_sheet_names <- function(data_list, file_prefix, output_dir) {
  sheet_names <- names(data_list)
  if (any(nchar(sheet_names) > 31)) {
    new_names <- paste0("G", seq_along(sheet_names))
    mapping <- data.frame(GroupID = new_names, OriginalName = sheet_names, stringsAsFactors = FALSE)
    names(data_list) <- new_names
    mapping_file <- file.path(output_dir, paste0(file_prefix, "_sheet_mapping_", format(Sys.Date(), "%Y%m%d"), ".txt"))
    write.table(mapping, file = mapping_file, sep = "\t", row.names = FALSE, quote = FALSE)
    log_message(paste0("Some group names exceeded 31 characters. Sheet names have been replaced with G1, G2, ... . See mapping file: ", mapping_file))
  }
  return(data_list)
}

#' Validate configuration structure
#' @param config Configuration list to validate
#' @return TRUE if valid, stops with error if invalid
validate_config <- function(config) {
  # Required fields
  required_fields <- c("input_dir", "output_dir", "column_map", "time_clean_rules",
                      "pc_time_group_vars", "pc_nontime_group_vars", "pp_group_vars",
                      "input_pc_file", "filter_analyte", "output_prefix_pc",
                      "input_pp_file", "input_pp_summary_file", "output_prefix_pp",
                      "bloq_value", "handle_bloq", "handle_zero", "apply_diagnostic_criteria",
                      "rsq_threshold", "auc_extrap_threshold", "single_group_dataset_PC", "single_group_dataset_pp")
  
  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    stop("Missing required configuration fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # Validate specific field types
  if (!is.character(config$input_dir)) stop("input_dir must be a character string")
  if (!is.character(config$output_dir)) stop("output_dir must be a character string")
  if (!is.list(config$column_map)) stop("column_map must be a list")
  if (!is.list(config$time_clean_rules)) stop("time_clean_rules must be a list")
  if (!is.character(config$pc_time_group_vars)) stop("pc_time_group_vars must be a character vector")
  if (!is.character(config$pc_nontime_group_vars)) stop("pc_nontime_group_vars must be a character vector")
  if (!is.character(config$pp_group_vars)) stop("pp_group_vars must be a character vector")
  if (!is.logical(config$filter_analyte)) stop("filter_analyte must be a logical value")
  if (!is.logical(config$apply_diagnostic_criteria)) stop("apply_diagnostic_criteria must be a logical value")
  if (!is.numeric(config$rsq_threshold)) stop("rsq_threshold must be a numeric value")
  if (!is.numeric(config$auc_extrap_threshold)) stop("auc_extrap_threshold must be a numeric value")
  if (!is.logical(config$single_group_dataset_PC)) stop("single_group_dataset_PC must be a logical value")
  if (!is.logical(config$single_group_dataset_pp)) stop("single_group_dataset_pp must be a logical value")
  
  # Validate specific values
  if (!config$handle_bloq %in% c("0", "NA")) stop("handle_bloq must be '0' or 'NA'")
  if (!config$handle_zero %in% c("exclude", "adjust")) stop("handle_zero must be 'exclude' or 'adjust'")
  
  log_message("Configuration validation passed")
  return(TRUE)
}

#' Initialize pipeline environment
#' @param config Configuration list
#' @return TRUE if successful
initialize_pipeline <- function(config) {
  # Validate configuration
  validate_config(config)
  
  # Create output directories
  fs::dir_create("Output_files/Step_1", recurse = TRUE)
  fs::dir_create("Output_files/Step_2", recurse = TRUE)
  fs::dir_create("Output_files/log", recurse = TRUE)
  
  log_message("Pipeline environment initialized successfully")
  return(TRUE)
}

