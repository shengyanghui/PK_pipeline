# plot_pc_data.R
# Plot cleaned PC data from Step 1 output using ggplot2 and save to PDF
# Creates both linear and log scale plots

# NOTE: This script expects setup.R and data_utils.R to be sourced already (i.e., run via run_pipeline.R)

# Function to create PK concentration plot
create_pk_plot <- function(data, use_log_scale = TRUE, facet_row_var = NULL, facet_col_var = NULL, facet_var = NULL, use_facet_grid = TRUE) {
  # Create base plot
  p <- data |>
    ggplot() +
    geom_point(aes(x = Time, y = Concentration, color = factor(subject_index))) +
    geom_line(aes(x = Time, y = Concentration, group = Subject, color = factor(subject_index))) +
    scale_color_brewer(palette = "Set3") +  # Use Set3 palette (up to 12 colors)
    theme_dark() +  # Use dark background for better Set3 contrast
    guides(color = "none") # Remove the legend
  
  # Add y-axis scale
  if (use_log_scale) {
    p <- p + scale_y_log10()
  }
  
  # Add appropriate faceting and labels
  if (use_facet_grid) {
    p <- p + facet_grid(as.formula(paste(facet_row_var, "~", facet_col_var))) +
      labs(title = paste("PK Concentration vs. Time by", facet_row_var, "and", facet_col_var),
           y = ifelse(use_log_scale, "Concentration (log10)", "Concentration"),
           x = "Time (h)")
  } else {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = 3) +
      labs(title = paste("PK Concentration vs. Time by", facet_var),
           y = ifelse(use_log_scale, "Concentration (log10)", "Concentration"),
           x = "Time (h)")
  }
  
  return(p)
}

# Construct the expected cleaned PC data file path
default_output_dir <- paste0(config$output_dir,"Step_1")
file_date <- format(Sys.Date(), "%Y%m%d")
cleaned_file <- generate_output_path(config$output_prefix_pc, default_output_dir)
pdf_file_log <- file.path(default_output_dir, paste0(config$output_prefix_pc, "_log_", file_date, ".pdf"))
pdf_file_linear <- file.path(default_output_dir, paste0(config$output_prefix_pc, "_linear_", file_date, ".pdf"))

safe_execute(check_file_exists(cleaned_file, "Cleaned PC data file"), "Cleaned PC data file not found")

# Read in the cleaned PC data (skip unit row if present)
data_raw <- safe_execute(read.csv(cleaned_file, stringsAsFactors = FALSE), "Failed to read cleaned PC data")
# If the first row is not numeric in Concentration, treat as unit row and remove
if (!suppressWarnings(is.numeric(data_raw$Concentration[1]))) {
  data <- data_raw[-1, ]
} else {
  data <- data_raw
}

# Convert columns to appropriate types
cols_to_num <- c("Time", "Concentration")
for (col in cols_to_num) {
  if (col %in% names(data)) {
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
    if (all(is.na(data[[col]]))) {
      log_message(paste("Column", col, "could not be converted to numeric"), "WARNING")
    }
  }
}
if ("Subject" %in% names(data)) data$Subject <- as.factor(data$Subject)
if ("Cohort" %in% names(data)) data$Cohort <- as.factor(data$Cohort)

# Get grouping variables using centralized function
present_group_vars <- get_pc_group_vars(config, data)

# Determine faceting variables from config (defensive, not hard-coded)
if (length(present_group_vars) >= 2) {
  # Use top 2 grouping variables for facet_grid
  facet_row_var <- present_group_vars[1]
  facet_col_var <- present_group_vars[2]
  use_facet_grid <- TRUE
} else if (length(present_group_vars) == 1) {
  # Use single grouping variable for facet_wrap
  facet_var <- present_group_vars[1]
  use_facet_grid <- FALSE
} else {
  stop("No suitable non-time grouping variables found for faceting.")
}

# Assign a within-group (panel) index to each Subject for color recycling
if (use_facet_grid) {
  data <- data %>%
    group_by(!!sym(facet_row_var), !!sym(facet_col_var)) %>%
    mutate(subject_index = as.integer(factor(Subject))) %>%
    ungroup()
} else {
  data <- data %>%
    group_by(!!sym(facet_var)) %>%
    mutate(subject_index = as.integer(factor(Subject))) %>%
    ungroup()
}

# Create log scale plot
p_log <- create_pk_plot(data, use_log_scale = TRUE, facet_row_var, facet_col_var, facet_var, use_facet_grid)

# Create linear scale plot
p_linear <- create_pk_plot(data, use_log_scale = FALSE, facet_row_var, facet_col_var, facet_var, use_facet_grid)

base_width <- 3
base_height <- 2.5

# Calculate adaptive PDF dimensions based on number of facets
if (use_facet_grid) {
  # Count unique values in the second grouping variable for faceting
  n_facets <- length(unique(data[[facet_col_var]])) * length(unique(data[[facet_row_var]]))
  pdf_width <- base_width * length(unique(data[[facet_col_var]]))
  pdf_height <- base_height * length(unique(data[[facet_row_var]]))
} else {
  # For facet_wrap, count unique values in the single grouping variable
  n_facets <- length(unique(data[[facet_var]]))
  # Calculate optimal dimensions using helper function
  plot_dims <- calculate_plot_dimensions(n_facets, base_width, base_height)
  pdf_width <- plot_dims$width
  pdf_height <- plot_dims$height
}

log_message(sprintf("Using adaptive PDF dimensions: %.1f x %.1f inches for %d facets", 
                    pdf_width, pdf_height, n_facets), "INFO")

# Save log scale plot to PDF
pdf(pdf_file_log, width = pdf_width, height = pdf_height)
print(p_log)
dev.off()

# Save linear scale plot to PDF
pdf(pdf_file_linear, width = pdf_width, height = pdf_height)
print(p_linear)
dev.off()

log_message(paste("Log scale plot saved to:", pdf_file_log), "INFO")
log_message(paste("Linear scale plot saved to:", pdf_file_linear), "INFO") 