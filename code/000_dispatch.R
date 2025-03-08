#overall dispatch script, called by cron
runTask=T   #go/nogo flag
wd="/home/sam/Documents/R\ Workspace/WRWC_cronTask"
setwd(wd)

logFile=file(file.path(wd,"log.txt"),open="a")
sink(file=logFile,append=T,type="output")
sink(file=logFile,append=T,type="message")
#run as:
#Rscript ~/Documents/R\ Workspace/WRWC_cronTask/code/000_dispatch.R

#crontab entry
#0 5 * * * /usr/sbin/rtcwake -m no -u -t $(date -d '+1 day 05:00' +\%s)

try(
  if(runTask){
    print("")
    print("----------------------------------------------------------------------------------------------")
    print(paste("Run initiated at",Sys.time()))
    
    source("~/Documents/R Workspace/WRWC_cronTask/code/00_run_models.R")
    
  }
)
sink(type="message")
sink()#end sink
