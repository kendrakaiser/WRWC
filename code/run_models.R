# ----------------------------------------------------------------------------- #
# Complete set of data generation and modeling components
# Kendra Kaiser
# July 14, 2020
# Calls modeling scripts to download data, predict temperature, streamflow 
# characteristics and associated water rights shutoff dates
# ----------------------------------------------------------------------------- # 

# define input parameters in global environment
cd <<- '~/Desktop/Data/WRWC'
pred.yr <<- 2020
#set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'feb1'
# set date for AgriMet Data download
end <<- '2020-10-01'

fig_dir = '~/github/WRWC/figures'

source(file.path("code", "data_scraping.R"))
source(file.path("code", "temperature_model.R"))

#change depending on model run date ("streamflow_model_feb.R", "streamflow_model_march.R", "streamflow_model_april.R")
if (run_date == 'feb1'){
  input <<- 'all_dat_feb.csv'
  source(file.path("code", "streamflow_model_feb.R"))
} else if (run_date == 'march1'){
  input <<- 'all_dat_mar.csv'
  source(file.path("code", "streamflow_model_march.R"))
} else if (run_date == 'april1'){
  input <<- 'all_dat_mar.csv'
  source(file.path("code", "streamflow_model_april.R"))
}


source(file.path("code", "streamflow_simulation.R"))


# knit Model Results Rmd
source(file.path("code", "ModelOutput.Rmd"))