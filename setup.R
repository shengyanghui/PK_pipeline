# Load all required packages
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(purrr)     # Functional programming
library(openxlsx)  # Excel I/O

# Check for optional packages
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)  # Load ggplot2 if available
} else {
  log_message("Package 'ggplot2' not found. Plotting functionality will not be available.", "WARNING")
}
if (!requireNamespace("fs", quietly = TRUE)) {
  stop("Package 'fs' is required for directory creation.")
}


# Create interim directory
fs::dir_create("Interim")

# Automatically create output directories for both steps
fs::dir_create(paste0(config$output_dir,"Step_1"), recurse = TRUE)
fs::dir_create(paste0(config$output_dir,"Step_2"), recurse = TRUE)
fs::dir_create(paste0(config$output_dir,"log"), recurse = TRUE)  # Create log directory

# Create timestamp for log file
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(paste0(config$output_dir,"log"), paste0("pipeline_log_", timestamp, ".txt"))

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
  # Method 1: Use simplified configuration (RECOMMENDED)
  # Build grouping variables from primary + additional
  primary_var <- if (!is.null(config$primary_group_var)) config$primary_group_var else "Cohort"
  additional_vars <- if (!is.null(config$additional_group_vars)) config$additional_group_vars else character(0)
  all_vars <- c(primary_var, additional_vars)
  
  # Check which of these variables are present in the data
  present_vars <- intersect(all_vars, names(data))
  if (length(present_vars) > 0) {
    return(present_vars)
  }
  
  # Method 2: Legacy fallback (for backward compatibility)
  if ("pc_nontime_group_vars" %in% names(config) && length(config$pc_nontime_group_vars) > 0) {
    present_vars <- intersect(config$pc_nontime_group_vars, names(data))
    if (length(present_vars) > 0) {
      return(present_vars)
    }
  }
  
  # Method 3: Final fallback
  return("default_group")
}

#' Get defensive grouping variables for PP data
#' @param config Configuration list
#' @param data Data frame to check against
#' @return Vector of present grouping variables
get_pp_group_vars <- function(config, data) {
  # Method 1: Use simplified configuration (RECOMMENDED)
  # Build grouping variables from primary + additional
  primary_var <- if (!is.null(config$primary_group_var)) config$primary_group_var else "Cohort"
  additional_vars <- if (!is.null(config$additional_group_vars)) config$additional_group_vars else character(0)
  all_vars <- c(primary_var, additional_vars)
  
  # Check which of these variables are present in the data
  present_vars <- intersect(all_vars, names(data))
  if (length(present_vars) > 0) {
    return(present_vars)
  }
  
  # Method 2: Legacy fallback (for backward compatibility)
  if ("pp_group_vars" %in% names(config) && length(config$pp_group_vars) > 0) {
    present_vars <- intersect(config$pp_group_vars, names(data))
    if (length(present_vars) > 0) {
      return(present_vars)
    }
  }
  
  # Method 3: Final fallback
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

#' Get all time-related grouping variables for PC data
#' @param config Configuration list
#' @param data Data frame to check against
#' @return Vector of present time grouping variables
get_pc_time_group_vars <- function(config, data) {
  # Method 1: Use simplified configuration (RECOMMENDED)
  # Build time grouping variables from primary + additional
  primary_var <- if (!is.null(config$primary_time_var)) config$primary_time_var else "Time"
  additional_vars <- if (!is.null(config$additional_time_vars)) config$additional_time_vars else character(0)
  all_vars <- c(primary_var, additional_vars)
  
  # Check which of these variables are present in the data
  present_vars <- intersect(all_vars, names(data))
  if (length(present_vars) > 0) {
    return(present_vars)
  }
  
  # Method 2: Legacy fallback (for backward compatibility)
  if ("pc_time_group_vars" %in% names(config) && length(config$pc_time_group_vars) > 0) {
    present_vars <- intersect(config$pc_time_group_vars, names(data))
    if (length(present_vars) > 0) {
      return(present_vars)
    }
  }
  
  # Method 3: Final fallback
  return(character(0))
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

#' Match parameter names to units using a lookup table
#' @param parameters Character vector of parameter names
#' @param lookup_table Data frame with columns 'Preferred' and 'Unit'
#' @return Character vector of units (same order as parameters)
match_parameter_units <- function(parameters, lookup_table) {
  # Ensure lookup_table has correct columns
  stopifnot(all(c("Preferred", "Unit") %in% names(lookup_table)))
  units <- lookup_table$Unit[match(parameters, lookup_table$Preferred)]
  # Replace NA with empty string for unmatched parameters
  units[is.na(units)] <- ""
  return(units)
}

#' Get id_cols for pivot_wider for PC data
#' @param config Configuration list
#' @param data Cleaned PC data frame
#' @return Character vector of id_cols for pivot_wider
get_pc_id_cols <- function(config, data) {
  split_var <- get_split_var(config, data, "pc")
  non_time_group_vars <- setdiff(config$pc_nontime_group_vars, split_var)
  id_cols <- c(non_time_group_vars, config$pc_time_group_vars)
  return(id_cols)
}

#' Transpose data to wide format by subject
#' @param df Data frame
#' @param id_cols Character vector of id columns (grouping variables)
#' @param subject_col Name of the subject column
#' @param value_col Name of the value column
#' @param arrange_cols Optional: columns to arrange by after pivoting
#' @return Wide-format data frame (one column per subject)
transpose_wide_by_subject <- function(df, id_cols, subject_col, value_col, arrange_cols = NULL) {
  wide_df <- df |>
    tidyr::pivot_wider(
      id_cols = all_of(id_cols),
      names_from = {{subject_col}},
      values_from = {{value_col}}
    )
  if (!is.null(arrange_cols)) {
    wide_df <- wide_df |> dplyr::arrange(across(all_of(arrange_cols)))
  }
  return(wide_df)
}

#' Transpose Phoenix data for each group
#' @param df Data frame containing Phoenix parameters
#' @param lookup_table Data frame with parameter unit mappings
#' @param config Configuration list
#' @return Wide-format data frame with parameters as rows and subjects as columns
transpose_phx_data <- function(df, lookup_table, config) {
  group_vars <- get_pp_group_vars(config, df)
  # Identify all parameter columns (exclude Subject and group vars only)
  all_param_cols <- setdiff(
    names(df), 
    c("Subject", group_vars)
  )
  # Identify PK parameter columns (those used in summary statistics)
  pk_param_cols <- setdiff(
    names(df), 
    c("Subject", group_vars, 
      "Dose", "Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG",
      "Lambda_z", "Lambda_z_intercept", "Lambda_z_lower", "Lambda_z_upper", "N_Samples",
      "Rsq", "Corr_XY", "AUMC_%Extrap_obs", "AUC_TAU_%Extrap")
  )
  # Order: PK parameters first, then the rest
  other_param_cols <- setdiff(all_param_cols, pk_param_cols)
  param_cols_ordered <- c(pk_param_cols, other_param_cols)
  # Prepare long format
  df_long <- df |>
    select(Subject, all_of(group_vars), all_of(param_cols_ordered)) |>
    mutate(across(all_of(param_cols_ordered), as.character)) |>
    tidyr::pivot_longer(
      cols = all_of(param_cols_ordered),
      names_to = "Parameter",
      values_to = "Estimate"
    )
  # Use utility to pivot wider: all group_vars + Parameter as id_cols
  id_cols <- c(group_vars, "Parameter")
  df_wide <- transpose_wide_by_subject(
    df_long,
    id_cols = id_cols,
    subject_col = "Subject",
    value_col = "Estimate"
  )
  # Add Unit column by matching Parameter to lookup_table
  df_wide$Unit <- match_parameter_units(df_wide$Parameter, lookup_table)
  df_wide <- dplyr::relocate(df_wide, Unit, .after = "Parameter")
  # Arrange columns: group_vars, Parameter, Unit, then all Subject columns (in order)
  subject_cols <- setdiff(names(df_wide), c(group_vars, "Parameter", "Unit"))
  df_wide <- df_wide[, c(group_vars, "Parameter", "Unit", subject_cols)] |>
    arrange(across(all_of(group_vars)))
  return(df_wide)
}

#' Calculate Adaptive Plot Dimensions
#'
#' Calculates optimal PDF dimensions based on the number of facets/panels.
#' Used for both PC and PP plots to ensure proper sizing.
#' @param n_facets Number of facets/panels in the plot
#' @param base_width Base width for single plot (default: 7)
#' @param base_height Base height for single plot (default: 6)
#' @return List with width and height dimensions
calculate_plot_dimensions <- function(n_facets, base_width = 7, base_height = 6) {
  stopifnot(is.numeric(n_facets), n_facets > 0,
            is.numeric(base_width), base_width > 0,
            is.numeric(base_height), base_height > 0)
  
  if (n_facets <= 2) {
    # 1-2 facets: arrange horizontally
    pdf_width <- base_width * n_facets
    pdf_height <- base_height
  } else if (n_facets <= 4) {
    # 3-4 facets: 2x2 grid
    pdf_width <- base_width * 2
    pdf_height <- base_height * 2
  } else if (n_facets <= 6) {
    # 5-6 facets: 3x2 grid
    pdf_width <- base_width * 3
    pdf_height <- base_height * 2
  } else if (n_facets <= 9) {
    # 7-9 facets: 3x3 grid
    pdf_width <- base_width * 3
    pdf_height <- base_height * 3
  } else {
    # 10+ facets: 4x3 grid (maximum reasonable size)
    pdf_width <- base_width * 4
    pdf_height <- base_height * 3
  }
  
  return(list(width = pdf_width, height = pdf_height))
}

# Source all utility scripts so their functions are available globally
source(file.path(dir.pkpipeline, "clean_data.R"))
source(file.path(dir.pkpipeline, "phoenix_nca_utils.R"))
source(file.path(dir.pkpipeline, "stats_utils.R"))
source(file.path(dir.pkpipeline, "io_utils.R"))

