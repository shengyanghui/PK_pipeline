# Step 1: Raw Data Cleaning and PC Summary Generation
# ====================================================

# NOTE: This script expects setup.R and data_utils.R to be sourced already (i.e., run via run_pipeline.R)

log_message("Step 1 started: Raw Data Cleaning and PC Summary Generation")

# Load and clean raw concentration data
raw_data <- safe_execute(
  load_raw_data(config$input_pc_file, config),
  "Failed to load raw data"
)
cleaned_data <- safe_execute(
  clean_pk_data(raw_data, config),
  "Failed to clean PK data"
)

# Filter for the specified analyte (if configured and enabled)
if (isTRUE(config$filter_analyte) &&
    !is.null(config$analyte_name) &&
    "Analyte" %in% names(cleaned_data)) {
  cleaned_data <- cleaned_data |> filter(Analyte == config$analyte_name)
  log_message(sprintf("Filtered data for analyte: %s", config$analyte_name))
} else if (!isTRUE(config$filter_analyte)) {
  log_message("Analyte filtering skipped (filter_analyte = FALSE)")
}

# Generate summary statistics for concentration data
summary_stats <- safe_execute(
  generate_PC_summary(cleaned_data, config),
  "Failed to generate PC summary"
)

# Get id_cols for pivot_wider using centralized utility
id_cols <- get_pc_id_cols(config, cleaned_data)

# Split data for Excel output using centralized function
summary_stats_list <- split_data_for_output(summary_stats, config, "pc")

data_wide_list <- cleaned_data |>
  split_data_for_output(config, "pc") |>
  map(~ transpose_wide_by_subject(
    .x,
    id_cols = id_cols,
    subject_col = Subject,
    value_col = Concentration,
    arrange_cols = id_cols
  ))

# Write Excel outputs using centralized function
write_standardized_excel(
  summary_stats_list,
  paste0(config$output_prefix_pc, "_summary"),
  "Output_files/Step_1/",
  "summary statistics"
)

write_standardized_excel(
  data_wide_list,
  paste0(config$output_prefix_pc, "_wide"),
  "Output_files/Step_1/",
  "wide-format"
)

# Prepare cleaned output using centralized functions
present_nontime_vars <- intersect(config$pc_nontime_group_vars, names(cleaned_data))
data_cleaned_out <- cleaned_data |> 
  select(Subject, all_of(present_nontime_vars), contains("Time"), Concentration)

write_csv_output(
  data_cleaned_out,
  file_prefix = config$output_prefix_pc,
  unit_row = generate_unit_row(data_cleaned_out),
  output_dir = "Output_files/Step_1/"
)
log_message("Wrote cleaned data CSV to Output_files/Step_1/")

# ---- Set BLOQ values before first measurable concentration to 0 and output ----
bloq_value <- config$bloq_value
data_bloq0 <- cleaned_data |>
  arrange(!!!rlang::syms(config$pc_nontime_group_vars), Subject, Time) |>
  group_by(!!!rlang::syms(config$pc_nontime_group_vars), Subject) |>
  mutate(
    is_bloq = Concentration == bloq_value,
    all_bloq = all(is_bloq),
    first_measurable_idx = which(!is.na(suppressWarnings(as.numeric(Concentration))) &
                                 Concentration != bloq_value &
                                 Concentration != "")[1],
    Concentration_BLOQ0 = case_when(
      all_bloq ~ 0,
      is_bloq & row_number() < first_measurable_idx ~ 0,
      is_bloq ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(Concentration))
    )
  ) |>
  ungroup() |>
  select(-is_bloq, -all_bloq, -first_measurable_idx)

# Prepare BLOQ0 output using centralized functions
present_nontime_vars_bloq0 <- intersect(config$pc_nontime_group_vars, names(data_bloq0))
data_bloq0_out <- data_bloq0 |> 
  select(Subject, all_of(present_nontime_vars_bloq0), contains("Time"), Concentration_BLOQ0) |> 
  rename(Concentration = Concentration_BLOQ0)

write_csv_output(
  data_bloq0_out,
  file_prefix = paste0(config$output_prefix_pc, "_BLOQ0"),
  output_dir = "Output_files/Step_1/",
  unit_row = generate_unit_row(data_bloq0_out)
)
log_message(paste(
  "BLOQ0-adjusted data saved to:",
  generate_output_path(paste0(config$output_prefix_pc, "_BLOQ0"), "Output_files/Step_1/")
))

log_message("Step 1 completed.")
