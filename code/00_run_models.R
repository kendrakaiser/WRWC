# ----------------------------------------------------------------------------- #
# Complete set of data generation and modeling components
# Kendra Kaiser
# July 14, 2020
# Calls modeling scripts to download data, predict temperature, streamflow 
# characteristics and associated water rights shutoff dates
# ----------------------------------------------------------------------------- # 

# Set input parameters and directories in global environment for each model run
# GitHub File Path
git_dir=getwd()
#cd=getwd()

# Output file paths
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github CHECK THIS - necessary?

# set end date for AgriMet Data download
end_date <<-Sys.Date() #as.Date("2021-02-01") replace when testing historical time frame
#TODO: auto select based on date
# set prediction year
pred.yr <<- 2024
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'feb1'

# info for model run report - move to RMD
author <<- "Kendra Kaiser"
todays_date <<- "02/01/2023"


# ---- Run Model code

source(file.path(git_dir, 'code/01_packages.R'))

run_info<- sessionInfo()
#writeLines(capture.output(sessionInfo()), file.path(cd, "sessionInfo.txt"))

# sets input/output file directories and selects model params and models depending on model run date 
# ------------------------------------------------------------------------------
if (run_date == 'feb1'){
  input_data <<- 'alldat_feb.csv'
  fig_dir_mo <<- file.path(fig_dir,'February')
  fig_dir_mo_rmd <<- './figures/February'

  vol.summary <<-'feb_vol_summary.csv'
  cm.summary <<- 'feb_cm_summary.csv'
  
  vol_sum <<- 'feb_vol_summary.rdata'
  vol_mods <<- 'feb_vol_mods.rdata'
  cm_sum <<- 'feb_cm_summary.rdata'
  cm_mods <<- 'feb_cm_mods.rdata'
  wr_params <<- 'feb_wr_vars.rdata'
  
} else if (run_date == 'march1'){
  input_data <<- 'alldat_mar.csv'
  fig_dir_mo <<- file.path(fig_dir,'March')
  fig_dir_mo_rmd <<- './figures/March'

  vol.summary <<-'mar_vol_summary.csv'
  cm.summary <<- 'mar_cm_summary.csv'
  
  vol_sum <<- 'mar_vol_summary.rdata'
  vol_mods <<- 'mar_vol_mods.rdata'
  cm_sum <<- 'mar_cm_summary.rdata'
  cm_mods <<- 'mar_cm_mods.rdata'
  wr_params <<- 'mar_wr_vars.rdata'
  
} else if (run_date == 'april1'){
  input_data <<- 'alldat_apr.csv'
  fig_dir_mo <<- file.path(fig_dir,'April')
  fig_dir_mo_rmd <<- './figures/April'

  vol.summary <<-'apr_vol_summary.csv'
  cm.summary <<- 'apr_cm_summary.csv'
  
  vol_sum <<- 'apr_vol_summary.rdata'
  vol_mods <<- 'apr_vol_mods.rdata'
  cm_sum <<- 'apr_cm_summary.rdata'
  cm_mods <<- 'apr_cm_mods.rdata'
  wr_params <<- 'apr_wr_vars.rdata'
}


# ------------------------------------------------------------------------------
# Compile Data Based on Run Date
# ------------------------------------------------------------------------------
source(file.path(git_dir,'code/005_db_update.R'))
source(file.path(git_dir, 'code/02_data_scraping.R'))
source(file.path(git_dir, 'code/03_temperature_models.R')) 
source(file.path(git_dir, 'code/04_data_integration.R'))  

# Create Streamflow Models 
#-------------------------------------------------------------------------------
#TODO: 1) use sys.date to determine if these need to be run, 2) save mod files directly to db and have them read in here
#this only needs to be run after 10-1, the models will stay the same through the prediction season & take a long time to run
suppressWarnings(source(file.path(git_dir, 'code/05_streamflow_models.R')))# warning messages are expected and okay

# Make the Irrigation Season Streamflow Predictions
source(file.path(git_dir, 'code/06_streamflow_predictions.R'))

# Simulate the Irrigation Season Hydrograph
source(file.path(git_dir, 'code/07_streamflow_simulation.R'))

# Develop curtailment models and make curtailment date predictions - not tested yet
#source(file.path(git_dir, 'code/08_curtailment_predictions.R'))

# manage data and push necessary outputs to db
source(file.path(git_dir, 'code/09_data_management.R'))

#TODO: move knit results to shiny so users can click - "download report"
# knit Model Results PDF
#detach(package:plyr) #plyr interferes with a grouping function needed for plotting
#params_list = list(fig_dir_mo_rmd = fig_dir_mo_rmd, set_author = author, 
 #                  todays_date=todays_date, data_dir = data_dir, 
  #                 git_dir = git_dir, input_data = input_data, run_date=run_date)

# update the Rmd to work with figures in the shiny folder and create a "make pdf" button
# knit PDF - if it doesn't work you can open the 'ModelOutputv2.Rmd' and press 'knit'
#rmarkdown::render(file.path(git_dir, 'ModelOutputv2.Rmd'), params = params_list, 
     #output_file = file.path(git_dir, paste0("ModelOutput-", end_date, ".pdf")))

