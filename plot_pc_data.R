# plot_pc_data.R
# Plot cleaned PC data from Step 1 output using ggplot2 and save to PDF

# NOTE: This script expects setup.R and data_utils.R to be sourced already (i.e., run via run_pipeline.R)

# Construct the expected cleaned PC data file path
default_output_dir <- "Output_files/Step_1/"
file_date <- format(Sys.Date(), "%Y%m%d")
cleaned_file <- generate_output_path(config$output_prefix_pc, default_output_dir)
pdf_file <- file.path(default_output_dir, paste0(config$output_prefix_pc, "_", file_date, ".pdf"))

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

# Create base plot
p <- data |>
  ggplot() +
  geom_point(aes(x = Time, y = Concentration, color = factor(subject_index))) +
  geom_line(aes(x = Time, y = Concentration, group = Subject, color = factor(subject_index))) +
  scale_y_log10() +
  scale_color_brewer(palette = "Set3") +  # Use Set3 palette (up to 12 colors)
  theme_dark() +  # Use dark background for better Set3 contrast
  guides(color = "none") # Remove the legend

# Add appropriate faceting
if (use_facet_grid) {
  p <- p + facet_grid(as.formula(paste(facet_row_var, "~", facet_col_var))) +
    labs(title = paste("PK Concentration vs. Time by", facet_row_var, "and", facet_col_var),
         y = "Concentration (log10)",
         x = "Time (h)")
} else {
  p <- p + facet_wrap(as.formula(paste("~", facet_var)), ncol = 3) +
    labs(title = paste("PK Concentration vs. Time by", facet_var),
         y = "Concentration (log10)",
         x = "Time (h)")
}

# Save plot to PDF
pdf(pdf_file, width = 10, height = 7)
print(p)
dev.off()

log_message(paste("Plot saved to:", pdf_file), "INFO") 