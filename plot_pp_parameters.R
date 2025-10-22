# plot_pp_parameters.R
# Plot key PK parameters by grouping variable using ggplot2 and save to PDF

# NOTE: This script expects setup.R and data_utils.R to be sourced already (i.e., run via run_pipeline.R)

# Construct the expected Phoenix parameter data file path (Step 2 output)
default_output_dir <- paste0(config$output_dir,"Step_2/")
file_date <- format(Sys.Date(), "%Y%m%d")
# We'll regenerate the long-format data in-memory from phx_data (Step 2)

# Defensive: check if phx_data exists, otherwise reload
if (!exists("phx_data")) {
  phx_data <- safe_execute(
    process_phoenix_output(
      file.path(config$input_dir, config$input_pp_file),
      file.path(config$input_dir, config$input_pp_summary_file),
      config
    ),
    "Failed to process Phoenix output (for plotting)"
  )
  lt <- attr(phx_data, "unit_lookup")
} else {
  if (!exists("lt")) lt <- attr(phx_data, "unit_lookup")
}

# Get grouping variables
group_vars <- get_pp_group_vars(config, phx_data)

# Prepare long-format data for plotting
exclude_cols <- c("Subject", group_vars, "Dose", "Rsq_adjusted", "AUC_%Extrap_obs", "Span", "EX", "RG",
                  "Lambda_z", "Lambda_z_intercept", "Lambda_z_lower", "Lambda_z_upper", "N_Samples",
                  "Rsq", "Corr_XY", "AUMC_%Extrap_obs", "AUC_TAU_%Extrap")
param_cols <- setdiff(names(phx_data), exclude_cols)
pp_long <- phx_data |>
  tidyr::pivot_longer(
    cols = all_of(param_cols),
    names_to = "Parameter",
    values_to = "Estimate"
  )

# Filter for selected PK parameters (from config)
plot_parameters <- if (!is.null(config$pk_plot_parameters)) config$pk_plot_parameters else c("Cmax", "AUClast", "AUC_inf_obs", "Half_life")
pp_long <- pp_long |> dplyr::filter(Parameter %in% plot_parameters)

# Convert grouping variables to factors for plotting
for (g in group_vars) {
  if (g %in% names(pp_long)) pp_long[[g]] <- as.factor(pp_long[[g]])
}

# Convert Estimate to numeric (suppress warnings for non-numeric rows)
pp_long$Estimate <- suppressWarnings(as.numeric(pp_long$Estimate))

# Apply EX and RG diagnostic filter before plotting, only if columns exist in pp_long
rg_affected_params <- c("Half_life", "AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F", "MRT_inf_obs", "AUMC_inf_obs")
ex_affected_params <- c("AUC_inf_obs", "AUC_inf_obs/Dose", "Vd_obs/F", "CL_obs/F", "MRT_inf_obs", "AUMC_inf_obs")

has_EX <- "EX" %in% names(pp_long)
has_RG <- "RG" %in% names(pp_long)

removed_rows <- data.frame()

if (has_EX || has_RG) {
  # Apply filter: set Estimate to NA if fails EX or RG for affected parameters
  pp_long <- pp_long |>
    dplyr::rowwise() |>
    dplyr::mutate(
      removed_reason = dplyr::case_when(
        Parameter %in% rg_affected_params & has_RG & RG == "Fail" ~ "RG",
        Parameter %in% ex_affected_params & has_EX & EX == "Fail" ~ "EX",
        TRUE ~ NA_character_
      ),
      Estimate = ifelse(!is.na(removed_reason), NA, Estimate)
    ) |>
    dplyr::ungroup()

  # Log removals
  removed_rows <- pp_long |> dplyr::filter(!is.na(removed_reason))
  if (nrow(removed_rows) > 0) {
    for (i in seq_len(nrow(removed_rows))) {
      row <- removed_rows[i, ]
      log_message(sprintf(
        "Removed data point: Subject=%s, %s=%s, Parameter=%s due to failing %s diagnostic criteria.",
        as.character(row$Subject),
        group_vars[1],
        as.character(row[[group_vars[1]]]),
        as.character(row$Parameter),
        as.character(row$removed_reason)
      ), "INFO")
    }
  }
}

# Output file
pdf_file <- file.path(default_output_dir, paste0(config$output_prefix_pp, "_PK_parameter_plots_", file_date, ".pdf"))

# Plot for each parameter
if (length(plot_parameters) == 0) stop("No PK parameters specified for plotting.")

# Calculate adaptive PDF dimensions based on number of facets
if (length(group_vars) >= 2) {
  # Count unique values in the second grouping variable for faceting
  n_facets <- length(unique(pp_long[[group_vars[2]]]))
  
  # Calculate optimal dimensions using helper function
  plot_dims <- calculate_plot_dimensions(n_facets, base_width = 4, base_height = 4)
  pdf_width <- plot_dims$width
  pdf_height <- plot_dims$height
  
  log_message(sprintf("Using adaptive PDF dimensions: %.1f x %.1f inches for %d facets", 
                      pdf_width, pdf_height, n_facets), "INFO")
} else {
  # Single grouping variable - use standard dimensions
  pdf_width <- 4
  pdf_height <- 4
  log_message("Using standard PDF dimensions: 4 x 4 inches (single grouping variable)", "INFO")
}

# Open PDF device with adaptive dimensions
pdf(pdf_file, width = pdf_width, height = pdf_height)
for (param in plot_parameters) {
  df_param <- pp_long |> dplyr::filter(Parameter == param)
  if (nrow(df_param) == 0) next
  # Get unit for this parameter from lookup table (lt)
  unit_val <- NA
  if (exists("lt") && !is.null(lt) && "Preferred" %in% names(lt) && "Unit" %in% names(lt)) {
    unit_val <- lt$Unit[match(param, lt$Preferred)]
    if (is.na(unit_val)) unit_val <- ""
  } else {
    unit_val <- ""
  }
  ylab_str <- if (nzchar(unit_val)) paste0(param, " (", unit_val, ")") else param
  
  # Create base plot
  p <- ggplot(df_param, aes(x = !!sym(group_vars[1]), y = Estimate)) +
    geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7, color = "blue") +
    geom_boxplot(aes(group = !!sym(group_vars[1])), outlier.shape = NA, alpha = 0.3, fill = NA) +
    labs(title = paste(param, "by", group_vars[1]),
         x = group_vars[1],
         y = ylab_str) +
    theme_bw()
  
  # Add faceting if second grouping variable exists
  if (length(group_vars) >= 2) {
    p <- p + facet_wrap(as.formula(paste("~", group_vars[2])), scales = "fixed") +
      theme(
        panel.spacing = unit(2, "lines"),  # Increase space between facet panels
        strip.text = element_text(size = 12)  # Ensure strip text is readable
      )
  }
  # Check for long x-axis labels and adjust angle/margin if needed
  x_labels <- unique(as.character(df_param[[group_vars[1]]]))
  if (any(nchar(x_labels) > 30)) {
    p <- p + theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(10, 40, 10, 10, unit = "pt")
    )
  }
  print(p)
}
dev.off()

log_message(paste("PK parameter plots saved to:", pdf_file), "INFO") 