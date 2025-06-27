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
  tryCatch({
    openxlsx::write.xlsx(
      data_list,
      file = output_file,
      colNames = TRUE,
      na.string = "N/A"
    )
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