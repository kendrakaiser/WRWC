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
#git_dir="/home/sam/Documents/R\ Workspace/WRWC_cronTask"

# Output file paths
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github necessary for 08

# TODO simdate v.s. run date
# set end date 
#end_date <<-Sys.Date() #as.Date("2021-02-01") replace when testing historical time frame
end_date=as.Date("2025-02-21")


model_n=10

# ------------------------------------------------------------------------------
# Run Model
# ------------------------------------------------------------------------------
source(file.path(git_dir, 'code/01_packages.R'))
source(file.path(git_dir, 'code/init_db.R'))
# set prediction year
pred.yr <<- year(end_date)

run_info<- sessionInfo()
#writeLines(capture.output(sessionInfo()), file.path(git_dir, "sessionInfo.txt"))

# sets output figure directories based on model run date 
# ------------------------------------------------------------------------------
if (month(end_date) == 2){
  
  fig_dir_mo <<- file.path(fig_dir,'February')
  fig_dir_mo_rmd <<- './figures/February'
  
} else if (month(end_date) == 3){
  
  fig_dir_mo <<- file.path(fig_dir,'March')
  fig_dir_mo_rmd <<- './figures/March'
  
} else if (month(end_date) == 4){
  fig_dir_mo <<- file.path(fig_dir,'April')
  fig_dir_mo_rmd <<- './figures/April'
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

# Develop curtailment models and make curtailment date predictions 
# TODO: test with previous years hydrographs to see how accurate
source(file.path(git_dir, 'code/08_curtailment_predictions.R'))

# manage data and push necessary outputs to db
source(file.path(git_dir, 'code/09_data_management.R'))
