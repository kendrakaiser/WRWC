# ----------------------------------------------------------------------------- #
# Complete set of data generation and modeling components
# Kendra Kaiser
# July 14, 2020
# Calls modeling scripts to download data, predict temperature, streamflow 
# characteristics and associated water rights shutoff dates
# ----------------------------------------------------------------------------- # 

# Set input parameters and directories in global environment for each model run
cd <<- '~/Desktop/Data/WRWC'
# set prediction year
pred.yr <<- 2020
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'feb1'
# set end date for AgriMet Data download
end_date <<- '2020-10-01'

# Relative file paths 
fig_dir <<- '~/github/WRWC/figures'
input_dir <<- file.path(cd, 'input')
data_dir <<- file.path(cd, 'data')

# ---- Run Model code

source('~/github/WRWC/code/packages.R')
source('~/github/WRWC/code/data_scraping.R')
source('~/github/WRWC/code/temperature_models.R')

# sets input file name and runs model code depending on model run date 
if (run_date == 'feb1'){
  input <<- 'all_dat_feb.csv'
  source('~/github/WRWC/code/streamflow_model_feb.R')
} else if (run_date == 'march1'){
  input <<- 'all_dat_mar.csv'
  source('~/github/WRWC/code/streamflow_model_march.R')
} else if (run_date == 'april1'){
  input <<- 'all_dat_apr.csv'
  source('~/github/WRWC/code/streamflow_model_april.R')
}

source('~/github/WRWC/code/streamflow_simulation.R')


# knit Model Results Rmd
source(file.path("code", "ModelOutput.Rmd"))