# stats_utils.R
# Statistical analysis utilities for PK pipeline
#
# NOTE: This script expects setup.R to be sourced already (i.e., run via run_pipeline.R)

#' Generate PC Summary
#'
#' Generates summary statistics for cleaned PC data, including geometric statistics.
#' @param cleaned_data Data frame of cleaned PK data
#' @param config List of configuration options (must specify pc_time_group_vars and pc_nontime_group_vars for PC data)
#' @return List of summary statistics split by Cohort
#' @export
generate_PC_summary <- function(cleaned_data, config) {
  stopifnot(is.data.frame(cleaned_data), is.list(config))
  # Extract configuration settings
  bloq_value   <- config$bloq_value
  handle_bloq  <- config$handle_bloq  # e.g., "0" or "NA"
  handle_zeros <- config$handle_zero  # "exclude" or "adjust"
  # Get grouping variables from config
  if (!("pc_nontime_group_vars" %in% names(config) && "pc_time_group_vars" %in% names(config))) {
    stop("You must specify both pc_nontime_group_vars and pc_time_group_vars in config.")
  }
  group_vars <- c(config$pc_nontime_group_vars, config$pc_time_group_vars)
  # Ensure all required grouping variables exist in the data
  missing_vars <- setdiff(group_vars, names(cleaned_data))
  if (length(missing_vars) > 0) {
    stop("Missing grouping variables in data: ", paste(missing_vars, collapse = ", "))
  }
  # Replace BLOQ values with user-specified value (0 or NA)
  cleaned_data <- cleaned_data |>
    mutate(Concentration = ifelse(Concentration == bloq_value, handle_bloq, Concentration)) |>
    mutate(Concentration = as.numeric(Concentration))
  # Use generic summary utility
  summary_stats <- summarize_stats_generic(
    cleaned_data,
    value_col = "Concentration",
    group_vars = group_vars,
    handle_zeros = handle_zeros,
    digits = 2
  )
  return(summary_stats)
}

#' Generate PP Summary
#'
#' Generates summary statistics for Phoenix data, including geometric statistics.
#' @param phx_data Data frame from process_phoenix_output
#' @param config List of configuration options
#' @param lookup_table Unit lookup table from process_phoenix_output
#' @return Data frame of summary statistics with a 'Unit' column for each parameter
generate_PP_summary <- function(phx_data, config, lookup_table) {
  stopifnot(is.data.frame(phx_data), is.list(config))
  group_vars <- get_pp_group_vars(config, phx_data)
  missing_vars <- setdiff(group_vars, names(phx_data))
  if (length(missing_vars) > 0) {
    stop("Missing grouping variables in data: ", paste(missing_vars, collapse = ", "))
  }
  exclude_cols <- c("Subject", group_vars, "Dose","Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG",
                    "Lambda_z", "Lambda_z_intercept", "Lambda_z_lower", "Lambda_z_upper","N_Samples",
                    "Rsq", "Corr_XY","AUMC_%Extrap_obs", "AUC_TAU_%Extrap")
  param_cols <- base::setdiff(names(phx_data), exclude_cols)
  param_cols_ordered <- param_cols[order(match(param_cols, names(phx_data)))]
  if (isTRUE(config$apply_diagnostic_criteria)) {
    rg_affected_params <- c("Half_life", "AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F", "MRT_inf_obs", "AUMC_inf_obs")
    ex_affected_params <- c("AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F", "MRT_inf_obs", "AUMC_inf_obs")
    phx_data <- phx_data |>
      mutate(
        across(any_of(rg_affected_params), ~ if_else(RG == "Fail", NA_real_, .)),
        across(any_of(ex_affected_params), ~ if_else(EX == "Fail", NA_real_, .))
      )
  }
  phx_long <- phx_data |>
    tidyr::pivot_longer(
      cols = all_of(param_cols_ordered),
      names_to = "Parameter",
      values_to = "Value"
    )
  phx_long$Unit <- match_parameter_units(as.character(phx_long$Parameter), lookup_table)
  phx_long$Parameter <- factor(phx_long$Parameter, levels = param_cols_ordered)
  # Use generic summary utility for arithmetic stats
  phx_summary <- summarize_stats_generic(
    phx_long,
    value_col = "Value",
    group_vars = c(group_vars, "Parameter", "Unit"),
    handle_zeros = "exclude",
    digits = 3
  )
  # Drop geometric columns from arithmetic summary to avoid .x/.y duplicates
  phx_summary <- phx_summary %>% select(-Geo.Mean, -Geo.SD, -Geo.CV)
  # Use generic summary utility for geometric stats (with zero-handling from config)
  geo_stats <- summarize_stats_generic(
    phx_long,
    value_col = "Value",
    group_vars = c(group_vars, "Parameter", "Unit"),
    handle_zeros = config$handle_zero,
    digits = 3
  ) |>
    select(-N, -Mean, -SD, -Min, -Median, -Max, -CV) # keep only geometric columns
  # Merge
  full_summary <- full_join(phx_summary, geo_stats, by = c(group_vars, "Parameter", "Unit")) |>
    arrange(Parameter)
  full_summary$Parameter <- as.character(full_summary$Parameter)
  return(full_summary)
}

#' Apply Exclusion Criteria
#'
#' Applies diagnostic exclusion criteria to Phoenix data and adds flag variables.
#' @param data Data frame
#' @param config List of configuration options
#' @return Data frame with exclusion flags added
apply_exclusion_criteria <- function(data, config) {
  stopifnot(is.data.frame(data), is.list(config))
  if (!isTRUE(config$apply_diagnostic_criteria)) {
    return(data)
  }
  # Use explicit thresholds from config
  rsq_threshold <- config$rsq_threshold
  auc_extrap_threshold <- config$auc_extrap_threshold
  
  # Add Rsq_adjusted exclusion flag (EX)
  data <- data |>
    mutate(RG = case_when(
      Rsq_adjusted < rsq_threshold ~ "Fail",
      is.na(Rsq_adjusted) ~ "Fail",
      TRUE ~ "Pass"
    ))
  
  # Add AUC%Extrap_obs exclusion flag (RG)
  data <- data |>
    mutate(EX = case_when(
      `AUC_%Extrap_obs` > auc_extrap_threshold ~ "Fail",
      is.na(`AUC_%Extrap_obs`) ~ "Fail",
      TRUE ~ "Pass"
    ))
  
  return(data)
}