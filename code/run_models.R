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
pred.yr <<- 2020
# set run date for pulling swe data 'feb1', 'march1', 'april1'
run_date <<- 'april1'

# info for model run report
author <<- "Kendra Kaiser"
todays_date <<- "03/30/2021"

# Output file paths - do not change
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github CHECK THIS - necessary?
data_dir <<- file.path(cd, 'data') # local

# set end date for AgriMet Data download
end_date <<- Sys.Date()

# ---- Run Model code

source(file.path(git_dir, 'code/packages.R'))

run_info<- sessionInfo()
writeLines(capture.output(sessionInfo()), file.path(cd, "sessionInfo.txt"))

source(file.path(git_dir, 'code/data_scraping.R'))
source(file.path(git_dir, 'code/download_agrimet.R'))
source(file.path(git_dir, 'code/temperature_models.R'))

# sets input/output file directories and selects model params and models depending on model run date 
if (run_date == 'feb1'){
  input <<- 'all_dat_feb.csv'
  fig_dir_mo <<- file.path(git_dir,'figures/February')
  model_out <<-  file.path(cd, 'February_output')
  
  vol.vars <<-'feb_vol_vars.csv'
  cm.vars <<- 'feb_cm_vars.csv'
  
  vol_params <<- 'feb_vol_vars.rdata'
  vol_mods <<- 'feb_vol_mods.rdata'
  cm_params <<- 'feb_cm_vars.rdata'
  cm_mods <<- 'feb_cm_mods.rdata'
  
} else if (run_date == 'march1'){
  input <<- 'all_dat_mar.csv'
  fig_dir_mo <<- 'figures/March'
  model_out <<-  file.path(cd, 'March_output')
  
  vol.vars <<-'mar_vol_vars.csv'
  cm.vars <<- 'mar_cm_vars.csv'
  
  vol_params <<- 'mar_vol_vars.rdata'
  vol_mods <<- 'mar_vol_mods.rdata'
  cm_params <<- 'mar_cm_vars.rdata'
  cm_mods <<- 'mar_cm_mods.rdata'
  
} else if (run_date == 'april1'){
  input <<- 'all_dat_apr.csv'
  fig_dir_mo <<- file.path(git_dir,'figures/April')
  model_out <<-  file.path(cd, 'April_output')
  
  vol.vars <<-'apr_vol_vars.csv'
  cm.vars <<- 'apr_cm_vars.csv'
  
  vol_params <<- 'apr_vol_vars.rdata'
  vol_mods <<- 'apr_vol_mods.rdata'
  cm_params <<- 'apr_cm_vars.rdata'
  cm_mods <<- 'apr_cm_mods.rdata'
}

source(file.path(git_dir, 'code/streamflow_models.R'))

vol.params <<- list.load(file.path(data_dir, vol_params))
vol.mods <<- list.load(file.path(data_dir, vol_mods))
cm.params <<- list.load(file.path(data_dir,cm_params))
cm.mods <<- list.load(file.path(data_dir, cm_mods))

source(file.path(git_dir, 'code/streamflow_predictions.R'))


rm.all.but(c("cd", "pred.yr", "run_date", "git_dir", "fig_dir", "input_dir", 
             "data_dir", "input", "fig_dir_mo", "author", "todays_date", "model_out"))

source(file.path(git_dir, 'code/streamflow_simulation.R'))

# knit Model Results PDF
detach(package:plyr) #plyr interferes with a grouping function needed for plotting
params_list = list(fig_dir_mo = fig_dir_mo, set_author = author, 
                   todays_date=todays_date, data_dir = data_dir, 
                   git_dir = git_dir, input = input)
rmarkdown::render(file.path(git_dir, 'ModelOutputv2.Rmd'), params = params_list)



