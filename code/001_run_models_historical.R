#iterator for running historical models

#initialize
# Set input parameters and directories in global environment for each model run
# GitHub File Path
git_dir=getwd()
#git_dir="/home/sam/Documents/R\ Workspace/WRWC_cronTask"

# Output file paths
fig_dir <<- file.path(git_dir, 'figures') # github
input_dir <<- file.path(git_dir, 'input') # github necessary for 08

source(file.path(git_dir, 'code/01_packages.R'))
source(file.path(git_dir, 'code/init_db.R'))
try({
 source(file.path(git_dir,'code/005_db_update.R'))
})

model_n=10
refitModelToToday=T
reuseMonthlyModels=F

displayModelResults=T

wtLowFlow=F #unweighted run done on 12/30/2025
# and what about validity/implementation of  BIC of weighted regression?

hindCast=F
#if hindCast, whole dataset to date is used, only excluding forecast year

runDates=c(#as.Date(apply(expand.grid(2005:2025,c("03","04","05"),c("01")),MARGIN=1,FUN=paste,collapse="-"))-1,
          as.Date(apply(expand.grid(2005:2025,c("02","03","04"),c("01")),MARGIN=1,FUN=paste,collapse="-"))
          )

#runDates=as.Date("2025-04-01")
# runDates=c(seq.Date(from=as.Date("2024-02-01"),to=as.Date("2024-04-30"),by="day")
#            # seq.Date(from=as.Date("2024-02-01"),to=as.Date("2024-04-30"),by="day")
# )

runDates=as.Date(c("2025/04/26","2025/04/27","2025/04/28","2025/04/29","2025/04/30"))

for( dateIndex in 1:length(runDates)){
  end_date=runDates[dateIndex]
  print(end_date)
  # ------------------------------------------------------------------------------
  # Run Model
  # ------------------------------------------------------------------------------
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
  # source(file.path(git_dir, 'code/02_data_scraping.R'))
  # source(file.path(git_dir, 'code/03_temperature_models.R')) 
  # source(file.path(git_dir, 'code/04_data_integration.R'))  
  source(file.path(git_dir, 'code/0234_makeVarFunction.R'))  
  
  suppressWarnings(source(file.path(git_dir, 'code/05_streamflow_models.R')))# warning messages are expected and okay
  
  
  # Make the Irrigation Season Streamflow Predictions
  source(file.path(git_dir, 'code/06_streamflow_predictions.R'))
  
  # Simulate the Irrigation Season Hydrograph
  source(file.path(git_dir, 'code/07_streamflow_simulation.R'))
  
  source(file.path(git_dir, 'code/08_curtailment_predictions.R'))
  
  # manage data and push necessary outputs to db
  source(file.path(git_dir, 'code/09_data_management.R'))
  
  # bxpData=todayData$allVar[,c("bwh.irr_vol","sc.irr_vol","cc.irr_vol","bws.irr_vol","wateryear")]
  # bxpData=reshape(bxpData,direction="long", v.names="irrVol", times=c("bwh.irr_vol","sc.irr_vol","cc.irr_vol","bws.irr_vol"),varying=list(c("bwh.irr_vol","sc.irr_vol","cc.irr_vol","bws.irr_vol")))
  # boxplot(bxpData$irrVol/1000~bxpData$time, main=end_date)
  # points(1:4,output.vol,pch="*",cex=3)
  # 
  print(output.vol)
}


