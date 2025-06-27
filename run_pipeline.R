source("setup.R")
# Source utility functions (but not other modules yet)
source("data_utils.R")

# Run step 1 
source(file.path(dir.pkpipeline,"main_step1.R"))

# Run step 1b
source(file.path(dir.pkpipeline,"plot_pc_data.R"))

# Run step 2 
source(file.path(dir.pkpipeline,"main_step2.R"))