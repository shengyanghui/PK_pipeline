#' Clean PK Data
#'
#' Removes subjects with only BLOQ or missing values, cleans time and cohort labels.
#' If none of the grouping variables are present and config$single_group_dataset_PC is TRUE, adds a 'Cohort' column with value 'default_group'.
#' If none of the grouping variables are present and config$single_group_dataset_PC is FALSE, throws an error.
#' @param raw_data Data frame of raw PK data
#' @param config List of configuration options
#' @return Cleaned data frame
clean_pk_data <- function(raw_data, config) {
  stopifnot(is.data.frame(raw_data))
  stopifnot(is.list(config))
  bloq_value <- config$bloq_value
  
  # Get grouping variables from config
  time_group_vars <- if ("pc_time_group_vars" %in% names(config)) config$pc_time_group_vars else stop("'pc_time_group_vars' must be specified in config.")
  nontime_group_vars <- if ("pc_nontime_group_vars" %in% names(config)) config$pc_nontime_group_vars else stop("'pc_nontime_group_vars' must be specified in config.")

  # Ensure all time-related grouping variables exist
  missing_time_vars <- setdiff(time_group_vars, names(raw_data))
  if (length(missing_time_vars) > 0) {
    stop("The following time-related grouping variables are missing in the data: ", paste(missing_time_vars, collapse = ", "))
  }

  # Check non-time grouping variables for single-group logic
  missing_nontime_vars <- setdiff(nontime_group_vars, names(raw_data))
  if (length(nontime_group_vars) > 0 && length(missing_nontime_vars) == length(nontime_group_vars)) {
    # All non-time grouping variables are missing
    if (isTRUE(config$single_group_dataset_PC)) {
      raw_data$Cohort <- 'default_group'
      log_message("All non-time grouping variables missing. Added 'Cohort' column with value 'default_group'.")
    } else {
      stop("All non-time grouping variables are missing in the data and single_group_dataset_PC is FALSE. Please check your data or configuration.")
    }
  } else if (length(missing_nontime_vars) > 0) {
    log_message(paste("Missing non-time grouping variables in data:", paste(missing_nontime_vars, collapse = ", ")), "WARNING")
  }
  
  cleaned <- raw_data |>
    group_by(Subject) |>
    # remove Subjects taking placebo (no drug detected at all time points)
    filter(any(Concentration != bloq_value & Concentration != "", na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      Time = clean_timepoints(Time, config$time_clean_rules)
    )
  return(cleaned)
}

#' Clean Timepoints
#'
#' Applies a set of regex rules to clean timepoint labels.
#' @param time_col Vector of time labels
#' @param rules List of find/replace rules
#' @return Numeric vector of cleaned timepoints
clean_timepoints <- function(time_col, rules) {
  stopifnot(is.vector(time_col))
  stopifnot(is.list(rules))
  for (rule in rules) {
    time_col <- gsub(pattern = rule[1], replacement = rule[2], x = time_col, fixed = T)
  }
  result <- suppressWarnings(as.numeric(time_col))
  if (any(is.na(result) & !is.na(time_col))) {
    log_message("Some timepoints could not be converted to numeric.", "WARNING")
  }
  return(result)
}