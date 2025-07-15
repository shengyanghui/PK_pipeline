# Step 2: Phoenix Output Processing and Summary Generation
# ========================================================

# NOTE: This script expects setup.R and data_utils.R to be sourced already (i.e., run via run_pipeline.R)

log_message("Step 2 started: Phoenix Output Processing and Summary Generation")

# Process Phoenix WinNonlin output files
phx_data <- safe_execute(
  process_phoenix_output(
    file.path(config$input_dir, config$input_pp_file),
    file.path(config$input_dir, config$input_pp_summary_file),
    config
  ),
  "Failed to process Phoenix output"
)

# Extract unit lookup table from processed data
lt <- attr(phx_data, "unit_lookup")

# Generate summary statistics for Phoenix parameters
df_pp_summary <- safe_execute(
  generate_PP_summary(phx_data, config, lookup_table = lt),
  "Failed to generate PP summary"
)

# Round all PK parameters to 3 significant digits
# Identify PK parameter columns (exclude grouping variables, Subject, and diagnostic columns)
pk_param_cols <- setdiff(
  names(phx_data), 
  c("Subject", get_pp_group_vars(config, phx_data), 
  "Dose", "Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG")
)

# Round PK parameters to 3 significant digits
phx_data <- phx_data |>
  mutate(across(all_of(pk_param_cols), ~signif(., 3)))

# Helper function to transpose phx_data for each group
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
      values_to = "Value"
    )
  # Use utility to pivot wider: all group_vars + Parameter as id_cols
  id_cols <- c(group_vars, "Parameter")
  df_wide <- transpose_wide_by_subject(
    df_long,
    id_cols = id_cols,
    subject_col = "Subject",
    value_col = "Value"
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

# Split data by grouping variable for multi-sheet Excel output using centralized function
phx_data_list <- split_data_for_output(phx_data, config, "pp")
# Transpose each item in phx_data_list
phx_data_list <- lapply(phx_data_list, transpose_phx_data, lookup_table = lt, config = config)
pp_summary_list <- split_data_for_output(df_pp_summary, config, "pp")

# Write Phoenix parameter outputs using centralized function
write_standardized_excel(
  phx_data_list,
  paste0(config$output_prefix_pp, "_parameters"),
  "Output_files/Step_2/",
  "Phoenix parameter"
)

write_standardized_excel(
  pp_summary_list,
  paste0(config$output_prefix_pp, "_summary"),
  "Output_files/Step_2/",
  "Phoenix summary"
)

log_message("Step 2 completed.")