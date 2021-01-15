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
pred.yr <<- 2020
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'feb1'
# set end date for AgriMet Data download
end_date <<- '2021-01-01'
# info for model run report
author = "Kendra Kaiser"
todays_date = "01/14/2021"

# ---- Run Model code

source(file.path(git_dir, 'code/packages.R'))
source(file.path(git_dir, 'code/data_scraping.R'))
source(file.path(git_dir, 'code/temperature_models.R'))

# sets input file name and runs model code depending on model run date 
if (run_date == 'feb1'){
  input <<- 'all_dat_feb.csv'
  source(file.path(git_dir, 'code/streamflow_model_feb.R'))
  fig_dir_mo <<- 'figures/February'
} else if (run_date == 'march1'){
  input <<- 'all_dat_mar.csv'
  source(file.path(git_dir, 'code/streamflow_model_march.R'))
  fig_dir_mo <<- 'figures/March'
} else if (run_date == 'april1'){
  input <<- 'all_dat_apr.csv'
  source(file.path(git_dir, 'code/streamflow_model_april.R'))
  fig_dir_mo <<- 'figures/April'
}

source(file.path(git_dir, 'code/streamflow_simulation.R'))

# knit Model Results PDF
rmarkdown::render(file.path(git_dir, 'ModelOutput.Rmd'), params = list(fig_dir_mo = fig_dir_mo, author = author, todays_date=todays_date))
