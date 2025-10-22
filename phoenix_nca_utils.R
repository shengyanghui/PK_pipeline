# Load column name mapping lookup table
colnames_lookup <- read.csv(file.path(dir.pkpipeline, "misc/Column_name_lookup_table_ver2.csv"))

# Create mapping vectors for column renaming
colnames_vec <- setNames(colnames_lookup$Preferred, colnames_lookup$Phoenix_out)  # original -> preferred
rename_vec <- setNames(colnames_lookup$Phoenix_out, colnames_lookup$Preferred)    # preferred -> original

#' Process Phoenix Output
#'
#' Reads and processes Phoenix output and summary table, applies exclusion criteria if enabled.
#' Builds a lookup table for parameter units after renaming.
#' @param phoenix_file Path to Phoenix output CSV
#' @param summary_table_file Path to summary table CSV
#' @param config List of configuration options
#' @return Data frame with processed Phoenix data and a lookup table for units as attribute
process_phoenix_output <- function(phoenix_file, summary_table_file, config) {
  stopifnot(is.character(phoenix_file),
            is.character(summary_table_file),
            is.list(config))
  phx_raw <- tryCatch({
    read.csv(phoenix_file,
             stringsAsFactors = FALSE,
             check.names = FALSE)
  }, error = function(e)
    stop("Failed to read Phoenix file: ", phoenix_file, "\n", e$message))
  
  # Extract unit row and data rows
  unit_row <- phx_raw[1, ]
  data_rows <- phx_raw[-1, ]
  names(unit_row) <- colnames(phx_raw)
  names(data_rows) <- colnames(phx_raw)
  
  # Build lookup table: Original, Preferred, Unit
  original_names <- colnames(phx_raw)
  preferred_names <- vapply(original_names, function(nm) {
    val <- colnames_vec[nm]
    if (!is.na(val))
      val
    else
      nm
  }, character(1))
  units <- as.character(unit_row)
  lookup_table <- data.frame(
    Original = original_names,
    Preferred = preferred_names,
    Unit = units,
    stringsAsFactors = FALSE
  )
  
  # Get grouping variables using centralized function
  group_vars <- get_pp_group_vars(config, data_rows)
  
  # Check for missing grouping variables in PP data
  missing_vars <- setdiff(group_vars, names(data_rows))
  if (length(missing_vars) == length(group_vars)) {
    if (isTRUE(config$single_group_dataset_pp)) {
      data_rows[[group_vars]] <- 'default_group'
      log_message(paste0("All PP grouping variables missing. Added '", group_vars, "' column with value 'default_group'."), "INFO")
    } else {
      stop("All PP grouping variables are missing in the data and single_group_dataset_pp is FALSE. Please check your PP data or configuration.")
    }
  } else if (length(missing_vars) > 0) {
    log_message(paste("Missing PP grouping variables in data:", paste(missing_vars, collapse = ", ")), "WARNING")
  }
  
  # Process Phoenix data
  phx_data <- data_rows |>
    mutate(Subject = as.character(Subject)) |>
    rename(any_of(rename_vec)) |>
    select(-contains(c("pred", "points"))) |>
    mutate(across(-c(Subject, any_of(group_vars)), as.numeric))

  # Only import summary table and pull concentration data if enabled
  if (isTRUE(config$import_summary_table)) {
    # Read and process summary table
    summary_raw <- tryCatch({
      read.csv(summary_table_file,
               stringsAsFactors = FALSE,
               check.names = FALSE)
    }, error = function(e)
      stop(
        "Failed to read summary table: ",
        summary_table_file,
        "\n",
        e$message
      ))
    summary_units <- as.character(summary_raw[1, ])
    names(summary_units) <- colnames(summary_raw)

    # Get time points to pull from config, default to "24" if not set
    time_points <- if (!is.null(config$summary_time_points)) config$summary_time_points else "24"

    # Check for missing grouping variables in summary data and add if needed
    summary_data_rows <- summary_raw[-1, ]
    missing_vars_summary <- setdiff(group_vars, names(summary_data_rows))
    if (length(missing_vars_summary) == length(group_vars)) {
      if (isTRUE(config$single_group_dataset_pp)) {
        summary_data_rows[[group_vars]] <- 'default_group'
        log_message(paste0("All PP grouping variables missing in summary table. Added '", group_vars, "' column with value 'default_group'."), "INFO")
      }
    }

    # Pull concentration at specified time point(s)
    conc_data_list <- lapply(time_points, function(tp) {
      summary_data_rows |>
        mutate(Subject = as.character(Subject)) |>
        filter(Time == tp) |>
        select(Subject, any_of(group_vars), Concentration) |>
        mutate(Concentration = as.numeric(Concentration))
    })
    names(conc_data_list) <- paste0("C", time_points)

    # Add CXX columns for each time point
    for (i in seq_along(time_points)) {
      cname <- paste0("C", time_points[i])
      conc_data <- conc_data_list[[i]]
      join_vars <- c("Subject", group_vars)
      phx_data <- phx_data |>
        left_join(conc_data, by = join_vars) |>
        rename(!!cname := Concentration)
      # Add to lookup table
      lookup_table <- rbind(
        lookup_table,
        data.frame(
          Original = "Concentration",
          Preferred = cname,
          Unit = summary_units["Concentration"],
          stringsAsFactors = FALSE
        )
      )
    }

    # Add Cmax/CXX columns for each time point
    for (i in seq_along(time_points)) {
      cname <- paste0("C", time_points[i])
      cmax_cname <- paste0("Cmax/C", time_points[i])
      phx_data <- phx_data |>
        mutate(!!cmax_cname := ifelse(is.na(Cmax) | is.na(.data[[cname]]) | .data[[cname]] == 0, NA_real_, Cmax / .data[[cname]]))
      # Add to lookup table
      lookup_table <- rbind(
        lookup_table,
        data.frame(
          Original = "Cmax/CXX",
          Preferred = cmax_cname,
          Unit = "",
          stringsAsFactors = FALSE
        )
      )
    }
    lookup_table$Unit[lookup_table$Unit == "NA"] <- ""
  }

  attr(phx_data, "unit_lookup") <- lookup_table
  
  # Apply exclusion criteria if enabled
  if (isTRUE(config$apply_diagnostic_criteria)) {
    phx_data <- apply_exclusion_criteria(phx_data, config)
  }
  
  # Reorder columns: Subject, grouping variables, diagnostic columns, and the rest
  priority_cols <- c("Subject", group_vars, "Dose" ,"Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG")
  all_cols <- names(phx_data)
  existing_priority_cols <- priority_cols[priority_cols %in% all_cols]
  remaining_cols <- all_cols[!all_cols %in% existing_priority_cols]
  final_col_order <- c(existing_priority_cols, remaining_cols)
  
  # Reorder the data frame
  phx_data <- phx_data |> select(all_of(final_col_order))
  
  return(phx_data)
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

