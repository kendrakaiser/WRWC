# ----------------------------------------------------------------------------- #
# Complete set of data generation and modeling components
# Kendra Kaiser
# July 14, 2020
# Calls modeling scripts to download data, predict temperature, streamflow 
# characteristics and associated water rights shutoff dates
# ----------------------------------------------------------------------------- # 

# Set input parameters and directories in global environment for each model run

# GitHub File Path
git_dir <<- '~/github/WRWC'
# Local File Path
cd <<- '~/Desktop/WRWC'

# set prediction year
pred.yr <<- 2023
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'april1'

# info for model run report
author <<- "Kendra Kaiser"

todays_date <<- "04/01/2023"

# Output file paths - do not change
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github CHECK THIS - necessary?
data_dir <<- file.path(cd, 'data') # local

# set end date for AgriMet Data download
end_date <<-as.Date("2023-04-01")# Sys.Date() #as.Date("2021-02-01") replace when testing historical time frame

# ---- Run Model code

source(file.path(git_dir, 'code/01_packages.R'))

run_info<- sessionInfo()
writeLines(capture.output(sessionInfo()), file.path(cd, "sessionInfo.txt"))

source(file.path(git_dir, 'code/01_data_scraping.R'))
source(file.path(git_dir, 'code/03_temperature_models.R')) 

# sets input/output file directories and selects model params and models depending on model run date 
# ------------------------------------------------------------------------------
if (run_date == 'feb1'){
  input_data <<- 'alldat_feb.csv'
  fig_dir_mo <<- file.path(fig_dir,'February')
  fig_dir_mo_rmd <<- './figures/February'
  model_out <<-  file.path(cd, 'February_output')
  
  vol.vars <<-'feb_vol_vars.csv'
  cm.vars <<- 'feb_cm_vars.csv'
  
  vol_params <<- 'feb_vol_vars.rdata'
  vol_mods <<- 'feb_vol_mods.rdata'
  cm_params <<- 'feb_cm_vars.rdata'
  cm_mods <<- 'feb_cm_mods.rdata'
  wr_params <<- 'feb_wr_vars.rdata'
  
} else if (run_date == 'march1'){
  input_data <<- 'alldat_mar.csv'
  fig_dir_mo <<- file.path(fig_dir,'March')
  fig_dir_mo_rmd <<- './figures/March'
  model_out <<-  file.path(cd, 'March_output')
  
  vol.vars <<-'mar_vol_vars.csv'
  cm.vars <<- 'mar_cm_vars.csv'
  
  vol_params <<- 'mar_vol_vars.rdata'
  vol_mods <<- 'mar_vol_mods.rdata'
  cm_params <<- 'mar_cm_vars.rdata'
  cm_mods <<- 'mar_cm_mods.rdata'
  wr_params <<- 'mar_wr_vars.rdata'
  
} else if (run_date == 'april1'){
  input_data <<- 'alldat_apr.csv'
  fig_dir_mo <<- file.path(fig_dir,'April')
  fig_dir_mo_rmd <<- './figures/April'
  model_out <<-  file.path(cd, 'April_output')
  
  vol.vars <<-'apr_vol_vars.csv'
  cm.vars <<- 'apr_cm_vars.csv'
  
  vol_params <<- 'apr_vol_vars.rdata'
  vol_mods <<- 'apr_vol_mods.rdata'
  cm_params <<- 'apr_cm_vars.rdata'
  cm_mods <<- 'apr_cm_mods.rdata'
  wr_params <<- 'apr_wr_vars.rdata'
}


# ------------------------------------------------------------------------------
# Compile Data Based on Run Date
# ------------------------------------------------------------------------------
source(file.path(git_dir, 'code/04_data_integration.R'))  

# Create Streamflow Models 
#-------------------------------------------------------------------------------
suppressWarnings(source(file.path(git_dir, 'code/05_streamflow_models.R')))# warning messages are expected and okay

# Make the Irrigation Season Streamflow Predictions
source(file.path(git_dir, 'code/06_streamflow_predictions.R'))

# Remove unnecessary variables in the environment
rm.all.but(c("cd", "pred.yr", "run_date", "git_dir", "fig_dir", "input_dir", 
             "data_dir", "input_data", "fig_dir_mo_rmd", "fig_dir_mo",  "author",  "todays_date", "end_date", 
             "model_out", "streamflow_data_out"))

# Simulate the Irrigation Season Hydrograph
source(file.path(git_dir, 'code/07_streamflow_simulation.R'))

# Develop curtailment models and make curtailment date predictions
# Retiring this 'code/curtailment_model.R')))
source(file.path(git_dir, 'code/08_curtailment_predictions.R'))


# knit Model Results PDF
detach(package:plyr) #plyr interferes with a grouping function needed for plotting
params_list = list(fig_dir_mo_rmd = fig_dir_mo_rmd, set_author = author, 
                   todays_date=todays_date, data_dir = data_dir, 
                   git_dir = git_dir, input_data = input_data, run_date=run_date)

# knit PDF - if it doesn't work you can open the 'ModelOutputv2.Rmd' and press 'knit'
rmarkdown::render(file.path(git_dir, 'ModelOutputv2.Rmd'), params = params_list, 
     output_file = file.path(git_dir, paste0("ModelOutput-", end_date, ".pdf")))




