# stats_utils.R
# Statistical analysis utilities for PK pipeline
#
# NOTE: This script expects setup.R to be sourced already (i.e., run via run_pipeline.R)

#' Generic summary statistics (arithmetic and geometric)
#' @param data Data frame
#' @param value_col Name of value column (string)
#' @param group_vars Character vector of grouping variables
#' @param handle_zeros How to handle zeros for geometric stats ('exclude' or 'adjust')
#' @param digits Rounding digits (default 2)
#' @param rounding_fn Rounding function (default: round)
summarize_stats_generic <- function(data, value_col, group_vars, handle_zeros = "exclude", digits = 2, rounding_fn = round) {
  data <- data |>
    mutate(
      value = .data[[value_col]],
      value_adj = case_when(
        handle_zeros == "adjust" & value == 0 ~ value + 1e-6,
        handle_zeros == "exclude" & value == 0 ~ NA_real_,
        TRUE ~ value
      )
    )
  # Always compute arithmetic summary for all groups with at least one non-NA value
  arith_data <- data |>
    group_by(across(all_of(group_vars))) |>
    filter(any(!is.na(value))) |>
    ungroup()
  arith_summary <- arith_data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      N = sum(!is.na(value)),
      Mean = mean(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      Median = median(value, na.rm = TRUE),
      Max = max(value, na.rm = TRUE),
      CV = (SD / Mean * 100),
      .groups = "drop"
    )

  # Apply rounding with special handling for Tmax, Tlast, and Tlag
  if ("Parameter" %in% group_vars) {
    # Apply different rounding for Tmax vs other parameters
    arith_summary <- arith_summary |>
      mutate(
        across(where(is.numeric), ~ ifelse(grepl("^T", Parameter, ignore.case = TRUE), round(., 3), rounding_fn(., digits)))
      )
  } else {
    # Apply standard rounding when no Parameter column, excluding columns named with 'Time'
    arith_summary <- arith_summary |>
      mutate(across(where(is.numeric) & !matches("Time"), ~ rounding_fn(., digits)))
  }

  # For geometric stats, only compute for groups with at least one non-NA and all non-negative value_adj
  if (handle_zeros == "exclude") {
    geom_data <- data |>
      group_by(across(all_of(group_vars))) |>
      filter(any(!is.na(value_adj)) && all(value_adj[!is.na(value_adj)] >= 0)) |>
      ungroup()
  } else {
    geom_data <- data |>
      group_by(across(all_of(group_vars))) |>
      filter(any(!is.na(value_adj))) |>
      ungroup()
  }
  if (nrow(geom_data) > 0) {
    geom_summary <- geom_data |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        Geo.Mean = exp(mean(log(value_adj), na.rm = TRUE)),
        Geo.CV = (sqrt(exp(var(log(value_adj), na.rm = TRUE)) - 1) * 100),
        Geo.SD = exp(sd(log(value_adj), na.rm = TRUE)),
        .groups = "drop"
      )
    
    # Apply rounding with special handling for Tmax
    if ("Parameter" %in% group_vars) {
      # Apply different rounding for Tmax vs other parameters
      geom_summary <- geom_summary |>
        mutate(
          across(where(is.numeric), ~ ifelse(grepl("^T", Parameter, ignore.case = TRUE), round(., 3), rounding_fn(., digits)))
        )
    } else {
      # Apply standard rounding when no Parameter column, excluding columns named with 'Time'
      geom_summary <- geom_summary |>
        mutate(across(where(is.numeric) & !matches("Time"), ~ rounding_fn(., digits)))
    }
  } else {
    geom_summary <- arith_summary |>
      mutate(Geo.Mean = NA, Geo.SD = NA, Geo.CV = NA) |>
      select(all_of(group_vars), Geo.Mean, Geo.SD, Geo.CV) |> filter(FALSE)
  }

  # Log filtered-out groups for geometric stats (only for handle_zeros == 'exclude')
  if (handle_zeros == "exclude") {
    # Find groups in arith_summary not in geom_summary
    missing_geom <- dplyr::anti_join(arith_summary, geom_summary, by = group_vars)
    if (nrow(missing_geom) > 0) {
      for (i in seq_len(nrow(missing_geom))) {
        group_info <- as.list(missing_geom[i, group_vars, drop=FALSE])
        # Find the original data for this group
        group_data <- dplyr::semi_join(data, missing_geom[i, group_vars, drop=FALSE], by = group_vars)
        reason <- NULL
        if (all(is.na(group_data$value_adj))) {
          reason <- "all values are NA"
        } else if (any(group_data$value_adj[!is.na(group_data$value_adj)] < 0)) {
          reason <- "contains negative values"
        }
        log_message(sprintf(
          "Filtered out group for geometric stats: %s (reason: %s)",
          paste(sprintf("%s=%s", names(group_info), group_info), collapse=", "),
          reason
        ))
      }
    }
  }

  # Left join geometric stats to arithmetic stats
  summary <- dplyr::left_join(arith_summary, geom_summary, by = group_vars)
  return(summary)
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
  digits <- if (!is.null(config$summary_digits)) config$summary_digits else 2
  rounding_method <- if (!is.null(config$summary_rounding_method)) config$summary_rounding_method else "round"
  rounding_fn <- if (rounding_method == "signif") signif else round
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
    digits = digits,
    rounding_fn = rounding_fn
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
  digits <- if (!is.null(config$summary_digits)) config$summary_digits else 3
  rounding_method <- if (!is.null(config$summary_rounding_method)) config$summary_rounding_method else "round"
  rounding_fn <- if (rounding_method == "signif") signif else round
  phx_long <- phx_data |>
    tidyr::pivot_longer(
      cols = all_of(param_cols_ordered),
      names_to = "Parameter",
      values_to = "Estimate"
    )
  phx_long$Unit <- match_parameter_units(as.character(phx_long$Parameter), lookup_table)
  phx_long$Parameter <- factor(phx_long$Parameter, levels = param_cols_ordered)
  # Use generic summary utility for summary stats
  full_summary <- summarize_stats_generic(
    phx_long,
    value_col = "Estimate",
    group_vars = c(group_vars, "Parameter", "Unit"),
    handle_zeros = "exclude",
    digits = digits,
    rounding_fn = rounding_fn
  ) |> arrange(Parameter)
  full_summary$Parameter <- as.character(full_summary$Parameter)
  return(full_summary)
}
