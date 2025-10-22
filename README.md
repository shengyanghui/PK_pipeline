# PK Data Processing Pipeline

A modular R pipeline for pharmacokinetic (PK) data: raw data cleaning, Phoenix WinNonlin output processing, summary statistics, and reporting.

## Features
- Clean and map raw PK concentration data
- Process Phoenix WinNonlin outputs and (optionally) summary tables
- Generate arithmetic and geometric summary statistics
- Visualize the distributions of key PK parameters using boxplots with adaptive sizing
- Apply diagnostic exclusion criteria (Rsq, AUC%Extrap)
- Export results to Excel/CSV (multi-sheet)
- Comprehensive logging

## Quick Start

1. **Install required packages:**
   ```r
   install.packages(c("dplyr", "tidyr", "purrr", "openxlsx", "fs"))
   ```

2. **Configure your analysis:**  
   Edit `data_utils.R` for input/output files, column mappings, grouping variables, and settings.

3. **Run the pipeline:**
   ```r
   source("run_pipeline.R")  # Main entry point: runs all steps in order
   ```

   - `run_pipeline.R` will source `setup.R` and `data_utils.R` first to initialize the environment, then run all pipeline steps in sequence.
   - **Do not source `setup.R` or any utility/config scripts in step scripts.**
   - All utility functions are available globally after initialization.

## Data & Configuration

- **All configuration is handled in `data_utils.R`.**
  - File paths, column mappings, grouping variables, BLOQ/zero handling, summary digits, rounding method, and diagnostic criteria are set here.
  - Review and update the config list for each analysis.
  - **New simplified approach:**
    - Use `primary_group_var` and `primary_time_var` for main grouping variables (auto-detected from column_map if NULL)
    - Add `additional_time_vars` and `additional_group_vars` for extra grouping variables
    - Legacy variables (`pc_time_group_vars`, `pc_nontime_group_vars`, `pp_group_vars`) are auto-generated for backward compatibility
    - `summary_digits`: Number of digits for summary statistics (applies to both arithmetic and geometric stats, and to parameter outputs).
    - `summary_rounding_method`: Rounding method for summary statistics (`"round"` or `"signif"`).
- **Required columns:** `Subject`, `Time`, `Concentration`, `Cohort` (or mapped equivalents)
- **Grouping variables:**  
  - **Simplified approach (recommended):** Use `primary_group_var` + `additional_group_vars` and `primary_time_var` + `additional_time_vars`
  - **Legacy approach:** `pc_time_group_vars`, `pc_nontime_group_vars` for PC data; `pp_group_vars` for PP data
- **Summary table:** Optional. If enabled, must contain `Subject`, `Time`, `Concentration` (and grouping variables). Controlled by `import_summary_table` in config.
- **Non-numeric parameter list:**
  - The file `misc/non_numeric_parameters.txt` contains a list of parameter names (one per line, e.g., `EX`, `RG`) that should always be treated as non-numeric in output. You can edit this file to add or remove parameters as needed.
- **Config options:**  
  Set in `data_utils.R` (see in-file comments for details)
  - `import_summary_table`: Set to `FALSE` to skip importing the summary table and pulling CXX/Cmax/CXX columns in the Phoenix step. Only the main Phoenix output will be processed.

## Outputs

- **Step 1 (PC Data):**  
  - `Output_files/Step_1/`: Cleaned CSV, wide-format Excel, summary Excel, BLOQ0 CSV
- **Step 2 (Pharmacokinetic Parameters):**  
  - `Output_files/Step_2/`: Individual values Excel, summary Excel (with units), CXX and Cmax/CXX columns (if `import_summary_table = TRUE`)
  - **EX and RG rows always appear first** in parameter output sheets.
  - **Conditional formatting:** In Excel outputs, cells with value `Pass` in EX or RG rows are colored green, and `Fail` are colored red (font color only).
  - **Non-numeric parameters:** Any parameter listed in `misc/non_numeric_parameters.txt` is never rounded and is displayed as-is in the output.
  - **Summary statistics:** Arithmetic stats are always shown for all groups; geometric stats only if valid (blank otherwise)
- **Logging:**  
  - `Output_files/log/`: Timestamped log files
  - **Filtered-out groups for geometric stats** are logged with the reason (all NA or contains negative values).

## File Structure

```
Pk_pipeline/
├── run_pipeline.R        # Main entry point: runs all steps
├── setup.R               # Environment, config, and utility initialization
├── main_step1.R          # Step 1: Data cleaning & PC summary
├── main_step2.R          # Step 2: Phoenix processing & summary
├── data_utils.R          # Configuration
├── clean_data.R          # Data cleaning functions
├── phoenix_nca_utils.R   # Phoenix NCA output processing
├── stats_utils.R         # Statistical analysis
├── io_utils.R            # Input/output utilities (Excel/CSV writing, conditional formatting)
├── plot_pc_data.R        # Plotting script
├── misc/                 # Supporting files
│   ├── Column_name_lookup_table_ver2.csv  # Column name mapping for Phoenix output
│   └── non_numeric_parameters.txt         # List of non-numeric parameters (e.g., EX, RG)
├── Output_files/
│   ├── Step_1/           # Step 1 outputs
│   ├── Step_2/           # Step 2 outputs
│   └── log/              # Log files
└── Input_files/          # Raw data files
```

## Troubleshooting

- Ensure all required packages are installed
- Check file paths and column mappings in `data_utils.R`
- Verify grouping variables exist in your data and match your config
- Review log files in `Output_files/log/` for details
- If you see errors about grouping or arranging, check that your config grouping variables match the columns in your data

## Project Template

For new projects:
```
YourProject/
├── Input_files/
├── data_utils.R
├── run_pipeline.R   # source("setup.R"); source("main_step1.R"); ...
```

## Function Tree

```
Pk_pipeline/
├── setup.R
│   ├── log_message()                # Logging to console and file
│   ├── generate_output_path()       # Standardized output file path
│   ├── check_file_exists()          # File existence check
│   ├── ensure_directory()           # Safe directory creation
│   ├── safe_execute()               # Standardized error handling
│   ├── split_data_for_output()      # Data splitting for Excel output
│   ├── write_standardized_excel()   # Standardized Excel output
│   ├── get_pc_group_vars()          # Defensive grouping for PC data (enhanced with simplified config support)
│   ├── get_pp_group_vars()          # Defensive grouping for PP data (enhanced with simplified config support)
│   ├── get_pc_time_group_vars()     # Time grouping variables for PC data (NEW)
│   ├── get_split_var()              # Primary split variable
│   ├── validate_config()            # Config validation
│   ├── initialize_pipeline()        # Pipeline environment setup
│   ├── match_parameter_units()      # Parameter-to-unit matching
│   ├── get_pc_id_cols()             # id_cols for PC pivot_wider
│   ├── transpose_wide_by_subject()  # Wide-format by subject
│   ├── transpose_phx_data()         # Transpose Phoenix data for each group (MOVED from main_step2.R)
│   ├── summarize_stats_generic()    # Generic summary stats
│   ├── calculate_plot_dimensions()  # Calculate adaptive PDF dimensions based on number of facets
│   └── (sources all utility scripts)
├── clean_data.R
│   └── clean_pk_data()              # Clean PK data, handle grouping, BLOQ, etc.
├── phoenix_nca_utils.R
│   ├── process_phoenix_output()     # Read/process Phoenix NCA output, build unit lookup
│   └── apply_exclusion_criteria()   # Apply diagnostic exclusion criteria (Rsq, AUC%Extrap)
├── stats_utils.R
│   ├── generate_PC_summary()        # PC summary stats (arithmetic/geometric)
│   └── generate_PP_summary()        # PP summary stats (arithmetic/geometric)
├── io_utils.R
│   ├── load_raw_data()              # Load raw data with column mapping
│   ├── write_excel_outputs()        # Write list of data frames to Excel
│   ├── write_csv_output()           # Write single data frame to CSV
│   └── write_outputs() [deprecated] # (Deprecated) Use write_excel_outputs/write_csv_output
├── main_step1.R
│   └── (pipeline logic only; no new functions)
├── main_step2.R
│   └── (pipeline logic only; no new functions)
├── plot_pc_data.R
│   └── (pipeline logic only; no new functions)
├── data_utils.R
│   ├── config (list)                # All analysis configuration (restructured with simplified approach)
│   ├── derive_primary_vars()        # Auto-detect primary grouping variables from column_map (NEW)
│   └── auto_generate_legacy_vars()  # Generate legacy variables for backward compatibility (NEW)
```