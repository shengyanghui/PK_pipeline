# Step 2: Phoenix Output Processing and Summary Generation
# ========================================================

log_message("Step 2 started: Phoenix Output Processing and Summary Generation")

# Initialize environment and load packages
source(file.path(dir.pkpipeline,"phoenix_utils.R"))
source(file.path(dir.pkpipeline,"stats_utils.R"))
source(file.path(dir.pkpipeline,"io_utils.R"))

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

# Split data by grouping variable for multi-sheet Excel output using centralized function
phx_data_list <- split_data_for_output(phx_data, config, "pp")
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