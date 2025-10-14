# =============================================================================
# PK PIPELINE CONFIGURATION FILE
# =============================================================================
# Review and update every line before running a new analysis
# This file contains all configuration settings for the PK analysis pipeline
#
# IMPORTANT: This file uses generic placeholder names that must be replaced
# with your actual study-specific values:
# - File names and paths (sections 1 & 4)
# - Column names in column_map (section 2)  
# - Analyte names and output prefixes (section 4)
# - BLOQ values and thresholds (section 5)
# - Any other study-specific parameters
#
# Look for comments with "replace with actual" for required customizations.
# =============================================================================

# =============================================================================
# 1. FILE AND DIRECTORY SETTINGS
# =============================================================================
  
# Directory for input files
input_dir <- "input/"

# Directory for output files  
output_dir <- "output/"

# =============================================================================
# 2. DATA MAPPING AND CLEANING SETTINGS
# =============================================================================
  
# Rules for column name mappings (comment out if not applicable or already in desired name)
column_map <- c(
  Subject = "SUBJECT_ID",    # Variable for Subject ID (replace with actual column name)
  Time = "TIME_POINT",       # Variable for Time points character (replace with actual column name)
  Analyte = "ANALYTE",       # Variable for Analyte (replace with actual column name)
  Concentration = "CONCENTRATION", # Variable for Concentration (replace with actual column name)
  Cohort = "COHORT"          # Variable for Cohort/Arm (replace with actual column name)
)

# Rules to clean time points
time_clean_rules <- list(
  c(" hr EOS/ET", ""),
  c(" hr", ""),
  c("Predose", "0")
)
  
# =============================================================================
# 3. GROUPING VARIABLE CONFIGURATION (SIMPLIFIED & RECOMMENDED)
# =============================================================================
# RECOMMENDED APPROACH: Use the simplified variables below
# The functions will automatically combine primary + additional variables

# Primary grouping variables (automatically derived from column_map if NULL)
primary_group_var <- NULL         # NULL = auto-detect from column_map, or specify manually (e.g., "Cohort")
primary_time_var <- NULL          # NULL = auto-detect from column_map, or specify manually (e.g., "Time")

# Additional grouping variables (optional - add any extra variables you need)
additional_time_vars <- c("VISIT")      # Additional time-related variables (e.g., "Period", "Day")
additional_group_vars <- c()            # Additional non-time grouping variables (e.g., "Treatment", "Dose_Group")

# =============================================================================
# 4. INPUT FILE SETTINGS
# =============================================================================

# PC (Concentration) Data Settings
input_pc_file <- "input/pc/concentration_data.csv"
filter_analyte <- TRUE                    # Whether to filter by analyte in PC step
analyte_name <- "ANALYTE_NAME"           # Analyte to filter for PC step (replace with actual name)
output_prefix_pc <- "STUDY_PREFIX_PC"    # Output prefix for PC step (replace with actual prefix)

# Phoenix Parameter (PP) Data Settings  
input_pp_file <- "input/pp/phoenix_parameters.csv"
input_pp_summary_file <- "input/pp/summary_table.csv"
summary_time_points <- c("24")           # Time points to pull from summary file
output_prefix_pp <- "STUDY_PREFIX_PP"    # Output prefix for PP step (replace with actual prefix)
import_summary_table <- TRUE             # Whether to import summary table

# =============================================================================
# 5. DATA PROCESSING SETTINGS
# =============================================================================

# BLOQ (Below Limit of Quantification) Handling
bloq_value <- "BLOQ_VALUE"               # BLOQ identifier in raw data (replace with actual value)
handle_bloq <- "0"                       # Method: '0' or 'NA'
handle_zero <- "exclude"                 # Zero handling for geometric stats: 'exclude' or 'adjust'

# Diagnostic Exclusion Criteria
apply_diagnostic_criteria <- TRUE        # Whether to apply exclusion criteria
rsq_threshold <- 0.8                     # Rsq threshold for exclusion
auc_extrap_threshold <- 20               # AUC%Extrap threshold for exclusion

# Single Group Dataset Handling
single_group_dataset_PC <- FALSE         # Add 'default_group' if all PC grouping vars missing
single_group_dataset_pp <- FALSE         # Add 'default_group' if all PP grouping vars missing

# =============================================================================
# 6. OUTPUT AND SUMMARY SETTINGS
# =============================================================================

# Summary Statistics Settings
summary_digits <- 3                      # Number of digits for summary statistics
summary_rounding_method <- "round"       # Rounding method: 'round' or 'signif'

# Plotting Settings
pk_plot_parameters <- c("Cmax", "AUC_last", "AUC_inf_obs", "Half_life")

# =============================================================================
# 7. LEGACY CONFIGURATION (AUTO-GENERATED - DO NOT EDIT MANUALLY)
# =============================================================================
# These are maintained for backward compatibility only
# The functions now prioritize the simplified approach above

pc_time_group_vars <- c("Time", "VISIT")     # Auto-generated: primary_time_var + additional_time_vars
pc_nontime_group_vars <- c("Cohort")         # Auto-generated: primary_group_var + additional_group_vars  
pp_group_vars <- c("Cohort")                 # Auto-generated: primary_group_var + additional_group_vars

# =============================================================================
# CONFIGURATION OBJECT CREATION
# =============================================================================

config <- list(
  # File and Directory Settings
  input_dir = input_dir,
  output_dir = output_dir,
  
  # Data Mapping and Cleaning
  column_map = column_map,
  time_clean_rules = time_clean_rules,
  
  # Grouping Variable Configuration
  primary_group_var = primary_group_var,
  primary_time_var = primary_time_var,
  additional_time_vars = additional_time_vars,
  additional_group_vars = additional_group_vars,
  
  # Input File Settings
  input_pc_file = input_pc_file,
  filter_analyte = filter_analyte,
  analyte_name = analyte_name,
  output_prefix_pc = output_prefix_pc,
  input_pp_file = input_pp_file,
  input_pp_summary_file = input_pp_summary_file,
  summary_time_points = summary_time_points,
  output_prefix_pp = output_prefix_pp,
  import_summary_table = import_summary_table,
  
  # Data Processing Settings
  bloq_value = bloq_value,
  handle_bloq = handle_bloq,
  handle_zero = handle_zero,
  apply_diagnostic_criteria = apply_diagnostic_criteria,
  rsq_threshold = rsq_threshold,
  auc_extrap_threshold = auc_extrap_threshold,
  single_group_dataset_PC = single_group_dataset_PC,
  single_group_dataset_pp = single_group_dataset_pp,
  
  # Output and Summary Settings
  summary_digits = summary_digits,
  summary_rounding_method = summary_rounding_method,
  pk_plot_parameters = pk_plot_parameters,
  
  # Legacy Configuration (Auto-generated)
  pc_time_group_vars = pc_time_group_vars,
  pc_nontime_group_vars = pc_nontime_group_vars,
  pp_group_vars = pp_group_vars
)

# =============================================================================
# 8. CONFIGURATION PROCESSING AND AUTO-GENERATION
# =============================================================================
# This section processes the configuration and auto-generates legacy variables

#' Derive Primary Variables from Column Map
#' 
#' Automatically detects primary grouping variables from the column_map
#' if they are not explicitly specified in the configuration.
#' 
#' @param config Configuration list
#' @return Updated configuration list with derived primary variables
derive_primary_vars <- function(config) {
  # Auto-detect primary group variable if not specified
  if (is.null(config$primary_group_var) && "column_map" %in% names(config)) {
    group_candidates <- c("Cohort", "Treatment", "Group", "Arm")
    for (candidate in group_candidates) {
      if (candidate %in% names(config$column_map)) {
        config$primary_group_var <- candidate
        break
      }
    }
  }
  
  # Auto-detect primary time variable if not specified
  if (is.null(config$primary_time_var) && "column_map" %in% names(config)) {
    time_candidates <- c("Time", "TIME", "Timepoint", "TPT")
    for (candidate in time_candidates) {
      if (candidate %in% names(config$column_map)) {
        config$primary_time_var <- candidate
        break
      }
    }
  }
  
  return(config)
}

#' Auto-Generate Legacy Grouping Variables
#' 
#' Generates legacy grouping variables from the simplified configuration
#' to maintain backward compatibility with existing functions.
#' 
#' @param config Configuration list with primary variables set
#' @return Updated configuration list with legacy variables
auto_generate_legacy_vars <- function(config) {
  if (!is.null(config$primary_time_var) && !is.null(config$primary_group_var)) {
    # Generate legacy grouping variables
    config$pc_time_group_vars <- c(config$primary_time_var, config$additional_time_vars)
    config$pc_nontime_group_vars <- c(config$primary_group_var, config$additional_group_vars)
    config$pp_group_vars <- c(config$primary_group_var, config$additional_group_vars)
  }
  return(config)
}

# =============================================================================
# 9. CONFIGURATION FINALIZATION
# =============================================================================

# Apply auto-detection and generation
config <- derive_primary_vars(config)
config <- auto_generate_legacy_vars(config)

# =============================================================================
# 10. PIPELINE PATH SETTINGS
# =============================================================================

# Specify path for pipeline code (update as needed)
dir.pkpipeline <- "path/to/pipeline_code"

# =============================================================================
# END OF CONFIGURATION FILE
# =============================================================================
