# Source setup.R to ensure utility functions are available
if (exists("dir.pkpipeline")) {
  source(file.path(dir.pkpipeline, "setup.R"))
}

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
  
  # Prepare adjusted concentration for geometric stats
  cleaned_data <- cleaned_data |>
    mutate(Conc_adj = case_when(handle_zeros == "adjust" & Concentration == 0 ~ Concentration + 1e-6,
                                handle_zeros == "exclude"& Concentration == 0 ~ NA_real_,
                                TRUE ~ Concentration))
  
  # Summarize data by user-defined grouping variables
  summary_stats <- cleaned_data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      # Arithmetic statistics (always include zeros)
      N        = sum(!is.na(Concentration)),
      Mean     = mean(Concentration, na.rm = TRUE)|>round(digits = 2),
      SD       = sd(Concentration, na.rm = TRUE)|>round(digits = 2),
      Min      = min(Concentration, na.rm = TRUE),
      Median   = median(Concentration, na.rm = TRUE),
      Max      = max(Concentration, na.rm = TRUE),
      `CV (%)` = (SD / Mean * 100)|>round(digits = 2),
      
      # Geometric statistics (adjusted as needed, na.rm handles zeros)
      Geo.Mean   = exp(mean(log(Conc_adj), na.rm = TRUE))|>round(digits = 2),
      Geo.SD     = exp(sd(log(Conc_adj), na.rm = TRUE))|>round(digits = 2),
      `Geo.CV (%)` = (sqrt(exp(var(log(Conc_adj), na.rm = TRUE)) - 1) * 100)|>round(digits = 2),
      
      .groups = "drop"
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
  
  # Get grouping variables using centralized function for consistency
  group_vars <- get_pp_group_vars(config, phx_data)
  
  # Ensure all required grouping variables exist in the data
  missing_vars <- setdiff(group_vars, names(phx_data))
  if (length(missing_vars) > 0) {
    stop("Missing grouping variables in data: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Exclude grouping variables, Subject, and priority columns from parameter columns
  # Subject, grouping vars, and diagnostic parameters are not included in summary stats
  exclude_cols <- c("Subject", group_vars, "Dose","Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG",
                    "Lambda_z", "Lambda_z_intercept", "Lambda_z_lower", "Lambda_z_upper")
  param_cols <- base::setdiff(names(phx_data), exclude_cols)
  
  # Get parameter columns in the same order as they appear in phx_data
  param_cols_ordered <- param_cols[order(match(param_cols, names(phx_data)))]
  
  # Apply diagnostic exclusion criteria filtering if enabled
  if (isTRUE(config$apply_diagnostic_criteria)) {
    # Define parameters affected by each criterion
    rg_affected_params <- c("Half_life", "AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F")
    ex_affected_params <- c("AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F")
    
    # Filter out parameters based on diagnostic criteria
    phx_data <- phx_data |>
      mutate(
        # RG = Fail excludes Half_life, AUC_inf_obs, AUC_inf_obs/Dose, Vd_obs/F, CL_obs/F
        across(
          any_of(rg_affected_params),
          ~ if_else(RG == "Fail", NA_real_, .)
        ),
        # EX = Fail excludes AUC_inf_obs, AUC_inf_obs/Dose, Vd_obs/F, CL_obs/F
        across(
          any_of(ex_affected_params),
          ~ if_else(EX == "Fail", NA_real_, .)
        )
      )
  }
  
  # Always create a Unit column for each parameter using the lookup table
  phx_long <- phx_data |>
    tidyr::pivot_longer(
      cols = all_of(param_cols_ordered),
      names_to = "Parameter",
      values_to = "Value"
    ) |>
    dplyr::left_join(lookup_table[, c("Preferred", "Unit")], by = c("Parameter" = "Preferred"))
  
  # Create factor for Parameter to maintain the order from phx_data
  phx_long$Parameter <- factor(phx_long$Parameter, levels = param_cols_ordered)
  
  phx_summary <- phx_long |>
    group_by(across(all_of(c(
      group_vars, "Parameter", "Unit"
    )))) |>
    summarise(
      N = sum(!is.na(Value)),
      Mean = if(N > 0) mean(Value, na.rm = TRUE) |> signif(3) else NA_real_,
      SD = if(N > 1) sd(Value, na.rm = TRUE) |> signif(3) else NA_real_,
      Min = if(N > 0) min(Value, na.rm = TRUE) |> signif(3) else NA_real_,
      Median = if(N > 0) median(Value, na.rm = TRUE) |> signif(3) else NA_real_,
      Max = if(N > 0) max(Value, na.rm = TRUE) |> signif(3) else NA_real_,
      CV = if(N > 0 && !is.na(Mean) && Mean != 0) (SD / Mean * 100) |> signif(3) else NA_real_,
      .groups = "drop"
    )
  
  geo_stats <- phx_long |>
    mutate(
      Value_adj = case_when(
        config$handle_zero == "adjust" & Value == 0 ~ Value + 1e-6,
        config$handle_zero == "exclude" & Value == 0 ~ NA_real_,
        TRUE ~ Value
      )
    ) |>
    filter(!is.na(Value_adj), Value_adj > 0) |>
    group_by(across(all_of(c(
      group_vars, "Parameter", "Unit"
    )))) |>
    summarise(
      N_geo = n(),
      Geo.Mean = if(N_geo > 0) exp(mean(log(Value_adj), na.rm = TRUE)) |> signif(3) else NA_real_,
      Geo.CV = if(N_geo > 1) (sqrt(exp(var(log(Value_adj), na.rm = TRUE)) - 1) * 100) |> signif(3) else NA_real_,
      .groups = "drop"
    )
  
  full_summary <- full_join(phx_summary, geo_stats, by = c(group_vars, "Parameter", "Unit")) |>
    arrange(Parameter)
  
  # Convert Parameter back to character to avoid factor issues in output
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