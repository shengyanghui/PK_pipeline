#' Load Raw Data
#'
#' Loads raw data from a CSV file and applies column mappings from config.
#' @param file_name Name of the file to load (relative to input_dir)
#' @param config List of configuration options
#' @return Data frame of loaded and renamed data
load_raw_data <- function(file_name, config) {
  stopifnot(is.character(file_name), is.list(config))
  file_path <- paste0(config$input_dir, file_name)
  data <- tryCatch({
    read.csv(file_path)
  }, error = function(e) stop("Failed to read file: ", file_path, "\n", e$message))
  data <- data |> rename(any_of(config$column_map))
  return(data)
}

library(openxlsx)

# Helper to apply Pass/Fail formatting to EX and RG rows in a worksheet
detect_and_color_pass_fail <- function(wb, sheet, df) {
  # Find which rows are EX or RG (Parameter column)
  param_col <- which(names(df) == "Parameter")
  if (length(param_col) == 0) return()
  ex_rg_rows <- which(df$Parameter %in% c("EX", "RG"))
  if (length(ex_rg_rows) == 0) return()
  # Find subject columns (exclude group vars, Parameter, Unit)
  subject_cols <- setdiff(seq_along(df), c(param_col, which(names(df) == "Unit"), which(names(df) %in% get_pp_group_vars(config, df))))
  # Offset by 1 for header row
  for (row in ex_rg_rows) {
    for (col in subject_cols) {
      cell_val <- df[row, col]
      if (is.na(cell_val)) next
      if (cell_val == "Pass") {
        addStyle(wb, sheet, createStyle(fontColour = "#006100"), rows = row + 1, cols = col, gridExpand = FALSE, stack = TRUE)
      } else if (cell_val == "Fail") {
        addStyle(wb, sheet, createStyle(fontColour = "#9C0006"), rows = row + 1, cols = col, gridExpand = FALSE, stack = TRUE)
      }
    }
  }
}

#' Write Excel Outputs (Multi-sheet)
#'
#' Writes a list of data frames to an Excel file, each as a separate sheet. Creates output directory if needed.
#' @param data_list Named list of data frames to write (sheet names = list names)
#' @param file_prefix Prefix for output file name
#' @param output_dir Output directory (default 'Output_files/')
#' @param unit_row Optional unit row to prepend to each sheet
#' @return Invisibly returns output file path
write_excel_outputs <- function(data_list, 
                               file_prefix, 
                               output_dir = "Output_files/",
                               unit_row = NULL) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir, recurse = TRUE)
  file_date <- format(Sys.Date(), "%Y%m%d")
  output_file <- paste0(output_dir, file_prefix, "_", file_date, ".xlsx")
  if (!is.null(unit_row)) {
    data_list <- lapply(data_list, function(x) rbind(unit_row, x))
  }
  wb <- createWorkbook()
  for (sheet in names(data_list)) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, data_list[[sheet]], colNames = TRUE, na.string = "N/A")
    # Apply Pass/Fail coloring for EX and RG rows
    detect_and_color_pass_fail(wb, sheet, data_list[[sheet]])
  }
  tryCatch({
    saveWorkbook(wb, output_file, overwrite = TRUE)
  }, error = function(e) stop("Failed to write Excel file: ", output_file, "\n", e$message))
  invisible(output_file)
}

#' Write CSV Output (Single Data Frame)
#'
#' Writes a single data frame to a CSV file. Creates output directory if needed.
#' @param df Data frame to write
#' @param file_prefix Prefix for output file name
#' @param output_dir Output directory (default 'Output_files/')
#' @param unit_row Optional unit row to prepend
#' @return Invisibly returns output file path
write_csv_output <- function(df, 
                            file_prefix, 
                            output_dir = "Output_files/",
                            unit_row = NULL) {
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir, recurse = TRUE)
  file_date <- format(Sys.Date(), "%Y%m%d")
  output_file <- paste0(output_dir, file_prefix, "_", file_date, ".csv")
  if (!is.null(unit_row)) {
    df <- rbind(unit_row, df)
  }
  tryCatch({
    write.csv(df, output_file, row.names = FALSE, na = "N/A")
  }, error = function(e) stop("Failed to write CSV file: ", output_file, "\n", e$message))
  invisible(output_file)
}

#' @deprecated Use write_excel_outputs or write_csv_output instead.
write_outputs <- function(...) {
  stop("write_outputs() is deprecated. Use write_excel_outputs() or write_csv_output() instead.")
}