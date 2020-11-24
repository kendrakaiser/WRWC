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

source(file.path("code", "data_scraping.R"))
source(file.path("code", "temperature_model.R"))
source(file.path("code", "streamflow_model.R"))
source(file.path("code", "streamflow_simulation.R"))

#run water_rights_model