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

# Write processed dataset to interim folder
saveRDS(phx_data, 
        file = paste0("./Interim/", config$output_prefix_pp, "_phx_data.rds"))

# Generate summary statistics for Phoenix parameters
df_pp_summary <- safe_execute(
  generate_PP_summary(phx_data, config, lookup_table = lt),
  "Failed to generate PP summary"
)


# Split data by grouping variable for multi-sheet Excel output using centralized function
phx_data_list <- split_data_for_output(phx_data, config, "pp")
# Transpose each item in phx_data_list
phx_data_list <- lapply(phx_data_list, transpose_phx_data, lookup_table = lt, config = config)

# Read non-numeric parameter list from file
non_numeric_param_file <- file.path("misc", "non_numeric_parameters.txt")
if (file.exists(non_numeric_param_file)) {
  non_numeric_params <- trimws(readLines(non_numeric_param_file))
} else {
  non_numeric_params <- c("EX", "RG")
}

# Apply rounding to phx_data_list just before output, using config
phx_digits <- if (!is.null(config$summary_digits)) config$summary_digits else 3
phx_rounding_method <- if (!is.null(config$summary_rounding_method)) config$summary_rounding_method else "round"
phx_rounding_fn <- if (phx_rounding_method == "signif") signif else round
phx_data_list <- lapply(phx_data_list, function(df) {
  subject_cols <- setdiff(names(df), c(get_pp_group_vars(config, df), "Parameter", "Unit"))
  # Apply rounding only to rows where Parameter is not in non_numeric_params
  df[subject_cols] <- t(apply(df, 1, function(row) {
    if (row["Parameter"] %in% c(non_numeric_params, "Tmax", "Tlast", "Tmin")) {
      row[subject_cols]
    } else {
      suppressWarnings(as.character(phx_rounding_fn(as.numeric(row[subject_cols]), phx_digits)))
    }
  }))
  return(df)
})
# After rounding, reorder rows so EX and RG are always first
phx_data_list <- lapply(phx_data_list, function(df) {
  if ("Parameter" %in% names(df)) {
    ex_rows <- which(df$Parameter == "EX")
    rg_rows <- which(df$Parameter == "RG")
    other_rows <- setdiff(seq_len(nrow(df)), c(ex_rows, rg_rows))
    df <- df[c(ex_rows, rg_rows, other_rows), , drop = FALSE]
  }
  df
})
pp_summary_list <- split_data_for_output(df_pp_summary, config, "pp")

# Sort each summary sheet by all grouping variables and then Parameter if multiple grouping variables exist
pp_group_vars <- get_pp_group_vars(config, df_pp_summary)
if (length(pp_group_vars) > 1) {
  pp_summary_list <- lapply(
    pp_summary_list,
    function(df) {
      arrange(df, across(all_of(pp_group_vars)))
    }
  )
}

# Write Phoenix parameter outputs using centralized function
write_standardized_excel(
  phx_data_list,
  paste0(config$output_prefix_pp, "_parameters"),
  paste0(config$output_dir,"Step_2/"),
  "Phoenix parameter"
)

write_standardized_excel(
  pp_summary_list,
  paste0(config$output_prefix_pp, "_summary"),
  paste0(config$output_dir,"Step_2/"),
  "Phoenix summary"
)

log_message("Step 2 completed.")