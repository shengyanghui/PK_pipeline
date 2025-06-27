# Review and update every line before running a new analysis

config <- list(
  # =====================
  # Custom Inputs for Each Analysis 
  # =====================
  
  # Directory for input files
  input_dir = "Input_files/",
  
  # Directory for output files
  output_dir = "Output_files/",
  
  # Rules for column name mappings (comment out if not applicable or already in desired name)
  column_map = c(
    Subject = "SUBJID",    # Variable for Subject ID (or any first-level sort variable)
    Time = "TPT",          # Variable for Time points character
    Analyte = "TEST",      # Variable for Analyte
    Concentration = "RESULT", # Variable for Concentration
    Cohort = "COHORT"     # Variable for Cohort (Arm)
  ),
  
  # Rules to clean time points
  time_clean_rules = list(
    c(" hr EOS/ET", ""),
    c(" hr", ""),
    c("Predose", "0")
  ),
  
  # Grouping variables for PC data analysis (REQUIRED)
  # Specify time-related grouping variables (e.g., Time, VISIT)
  pc_time_group_vars = c("Time", "VISIT"),

  # Specify non-time grouping variables (e.g., Cohort, Treatment)
  pc_nontime_group_vars = c("Cohort"),

  # (DEPRECATED) pc_group_vars is replaced by pc_time_group_vars and pc_nontime_group_vars
  # pc_group_vars = c("Cohort", "Time", "VISIT"),
  
  # Grouping variables for Phoenix parameter analysis
  # If all grouping variables are missing in the data, a 'Cohort' column with value 'default_group' will be added automatically.
  pp_group_vars = c("Cohort"),
  
  # Input file for PC (concentration) step
  input_pc_file = "ASC47-101_PK_05MAY2025_PROD.csv",
  
  # Whether to filter by analyte in PC step (set FALSE if input file is already single-analyte)
  filter_analyte = TRUE,
  
  # Analyte to filter for PC step
  analyte_name = "GLC02-306",
  
  # Output prefix for PC step (used for all PC output files)
  output_prefix_pc = "ASC47_GLC02-306_PC",
  
  # Input file for Phoenix parameters (PP step)
  input_pp_file = "ASC47_PP_20250605.csv",
  
  # Input summary table for Phoenix (PP step)
  input_pp_summary_file = "ASC47_Summary Table.csv",
  
  # Character vector of time points to pull from summary file for Phoenix output (e.g., c("24"), or c("24", "48"))
  # If NULL or not set, defaults to "24" for backward compatibility
  summary_time_points = c("24"),
  
  # Output prefix for PP step (used for all PP output files)
  output_prefix_pp = "ASC47_PP",
  
  # BLOQ identifier in raw data
  bloq_value = "BLLOQ",
  
  # Method of BLOQ-handling when calculating PC summary stats: set to '0' or 'NA'
  handle_bloq = "0",
  
  # Method of zero-handling when calculating geometric summary stats: 'exclude' or 'adjust'
  handle_zero = "exclude",
  
  # Whether to apply diagnostic exclusion criteria (TRUE/FALSE)
  apply_diagnostic_criteria = TRUE,
  
  # Rsq threshold for exclusion (single value, user must set)
  rsq_threshold = 0.8,
  
  # AUC%Extrap threshold for exclusion (single value, user must set)
  auc_extrap_threshold = 20,
  
  # Whether to treat the PC dataset as a single group if all grouping variables are missing.
  # If TRUE, adds Cohort = 'default_group' and continues. If FALSE, throws an error if all grouping variables are missing in the PC step.
  single_group_dataset_PC = FALSE,
  
  # Whether to treat the PP dataset as a single group if all grouping variables are missing.
  # If TRUE, adds Cohort = 'default_group' and continues. If FALSE, throws an error if all grouping variables are missing in the PP step.
  single_group_dataset_pp = FALSE,
  
  # Whether to import summary table and pull concentration data in Phoenix step
  import_summary_table = TRUE
)

# specify path for pipeline code ====
dir.pkpipeline<-"C:/Users/yanghui.sheng/Documents/Ascletis_CodeBase/R/Pk_pipeline"




