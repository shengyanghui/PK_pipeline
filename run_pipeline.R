# PK Pipeline Main Entry Point
# ============================
# This script initializes the environment, loads configuration, and runs all pipeline steps in order.
# Usage: source("run_pipeline.R")

# 1. Load analysis configuration (edit data_utils.R for your project settings)
source("data_utils.R")

# 2. Initialize environment, logging, and load all utility functions
source(file.path(dir.pkpipeline, "setup.R"))

# 3. Step 1: Clean raw data and generate PC summary statistics and wide-format outputs
source(file.path(dir.pkpipeline, "main_step1.R"))

# 4. (Optional) Step 1b: Plot cleaned PC data (requires ggplot2)
source(file.path(dir.pkpipeline, "plot_pc_data.R"))

# 5. Step 2: Process Phoenix output and generate PP summary statistics
source(file.path(dir.pkpipeline, "main_step2.R"))