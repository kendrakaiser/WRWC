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
# set end date for AgriMet Data download
end_date <<- '2021-08-16'
# info for model run report
author <<- "Kendra Kaiser"
todays_date <<- "03/30/2021"

# Output file paths - do not change
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github
data_dir <<- file.path(cd, 'data') # local

# ---- Run Model code

source(file.path(git_dir, 'code/packages.R'))

run_info<- sessionInfo()
writeLines(capture.output(sessionInfo()), file.path(cd, "sessionInfo.txt"))

source(file.path(git_dir, 'code/data_scraping.R'))
#source(file.path(git_dir, 'code/download_agrimet.R'))
source(file.path(git_dir, 'code/temperature_models.R'))

# sets input file name and runs model code depending on model run date 
if (run_date == 'feb1'){
  input <<- 'all_dat_feb.csv'
  fig_dir_mo <<- file.path(git_dir,'figures/February')
  source(file.path(git_dir, 'code/streamflow_model_feb.R'))
  model_out <<-  file.path(cd, 'February_output')
  
} else if (run_date == 'march1'){
  input <<- 'all_dat_mar.csv'
  fig_dir_mo <<- 'figures/March'
  model_out <<-  file.path(cd, 'March_output')
  source(file.path(git_dir, 'code/streamflow_model_march.R'))
  
} else if (run_date == 'april1'){
  input <<- 'all_dat_apr.csv'
  fig_dir_mo <<- file.path(git_dir,'figures/April')
  model_out <<-  file.path(cd, 'April_output')
  source(file.path(git_dir, 'code/streamflow_model_april.R'))
}

rm.all.but(c("cd", "pred.yr", "run_date", "git_dir", "fig_dir", "input_dir", 
             "data_dir", "input", "fig_dir_mo", "author", "todays_date", "model_out"))

source(file.path(git_dir, 'code/streamflow_simulation.R'))

# knit Model Results PDF
detach(package:plyr) #plyr interferes with a grouping function needed for plotting
params_list = list(fig_dir_mo = fig_dir_mo, set_author = author, 
                   todays_date=todays_date, data_dir = data_dir, 
                   git_dir = git_dir, input = input)
rmarkdown::render(file.path(git_dir, 'ModelOutputv2.Rmd'), params = params_list)



