# ----------------------------------------------------------------------------- #
# Complete set of data generation and modeling components
# Kendra Kaiser
# July 14, 2020
# Calls modeling scripts to download data, predict temperature, streamflow 
# characteristics and associated water rights shutoff dates
# ----------------------------------------------------------------------------- # 

# Set input parameters and directories in global environment for each model run

# GitHub File Paths
git_dir <<- '~/github/WRWC'
fig_dir <<- file.path(git_dir, 'figures')
input_dir <<- file.path(git_dir, 'input')

# Local File Paths
cd <<- '~/Desktop/Data/WRWC'
data_dir <<- file.path(cd, 'data')

# set prediction year
pred.yr <<- 2019
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'april1'
# set end date for AgriMet Data download
end_date <<- '2020-10-01'




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
#fig_dir_mo <<- file.path(fig_dir, 'March')
#source('~/github/WRWC/ModelOutput.Rmd')
