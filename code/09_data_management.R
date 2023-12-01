# ------------------------------------------------------------------------------
# Data Management for Wood River Model 
# Kendra Kaiser
# August, 2022
# ------------------------------------------------------------------------------

# Pivot Longer
#might not need to re-read these in because they are already in the environment - need to test and clean that up throughout

# Predicted mean april-june temperature
aj_pred.temps<- read.csv(file.path(data_dir,'aj_pred.temps.csv'))
aj_pred.temps.long <- aj_pred.temps %>% pivot_longer(cols = everything(), names_to = "site", values_to = "aj.mean.t")
write.csv(aj_pred.temps.long, file.path(data_dir,'aj_pred_temps_long.csv'), row.names=FALSE)

var<-read.csv(file.path(model_out,'all_vars.csv'))
all_vars_long <- var %>% pivot_longer(cols = -c('wateryear'), names_to = c("site", "variable"), names_sep="[.]", values_to = "value")
write.csv(all_vars_long, file.path(data_dir,'all_vars_long.csv'), row.names=FALSE)


bwh.flow.simLong<- read.csv(file.path(model_out, "BWH.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
bws.flow.simLong<- read.csv(file.path(model_out, "BWS.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
cc.flow.simLong<- read.csv(file.path(model_out, "CC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
sc.flow.simLong <- read.csv(file.path(model_out, "SC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")

write.csv(bwh.flow.simLong, file.path(data_dir,'bwh.flow.simLong.csv'), row.names=FALSE)
write.csv(bws.flow.simLong, file.path(data_dir,'bws.flow.simLong.csv'), row.names=FALSE)
write.csv(cc.flow.simLong, file.path(data_dir,'cc.flow.simLong.csv'), row.names=FALSE)
write.csv(sc.flow.simLong, file.path(data_dir,'sc.flow.simLong.csv'), row.names=FALSE)


volumes<-read.csv(file.path(model_out,"vol.sample.csv"))
colnames(volumes)<-c("bwh.vol", "bws.vol","cc.vol", "sc.vol") #this has already been read in via the streamflow simulation script; re-consider re-reading it in
volumes.sampleLong<- volumes %>% pivot_longer(everything(), names_to = "site_name", values_to = "vol_af") 
#add run date?
vol.bws<- volumes.sampleLong %>% filter(site_name == 'bws.vol')


write.csv(volumes.sampleLong, file.path(data_dir,'volumes.sampleLong.csv'), row.names=FALSE)

curt.sampleLong<- read.csv(file.path(model_out,"curt.sample.csv"))





writeSummaryStats=function(x,site.metric,simDate,runDate=Sys.Date()){

  simDate=as.Date(simDate)

  
  if(length(strsplit(site.metric,"\\.")[[1]])!=2){
    stop(paste0("Invalid site.metric ",site.metric))
  }
  
  site=strsplit(site.metric,"\\.")[[1]][1]
  metric=strsplit(site.metric,"\\.")[[1]][2]
  
  
  x.stats=boxplot.stats(x)
  
  statDF=data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="n",value=x.stats$n)
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="min",value=x.stats$stats[[1]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="lower_hinge",value=x.stats$stats[[2]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="med",value=x.stats$stats[[3]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="upper_hinge",value=x.stats$stats[[4]]))
  statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="max",value=x.stats$stats[[5]]))
  if(length(x.stats$out)>0){
    statDF=rbind(statDF,data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,stat="outlier",value=x.stats$out))
  }
  #return(statDF)
  
  dbWriteTable(conn,"summarystatistics",statDF,append=T)
}



makeBoxplotData=function(dbdf=dbGetQuery(conn,"SELECT * FROM summarystatistics;")){
  groups=unique(dbdf[c("site","metric","simdate","rundate")])
  bpData=list(stats=matrix(nrow=5,ncol=nrow(groups)),n=rep(NA,nrow(groups)),out=vector(),group=vector(),names=vector())
  for(i in 1:nrow(groups)){
    thisName=paste0(groups[i,"site"],".",groups[i,"metric"],"_simDate:",groups[i,"simdate"],"_runDate:",groups[i,"rundate"])
    bpData$names[i]=thisName
    thisData=merge(groups[i,],dbdf)
    bpData$stats[,i]=c(thisData$value[thisData$stat==c("min")],
                       thisData$value[thisData$stat==c("lower_hinge")],
                       thisData$value[thisData$stat==c("med")],
                       thisData$value[thisData$stat==c("upper_hinge")],
                       thisData$value[thisData$stat==c("max")])
    
    bpData$n[i]=thisData$value[thisData$stat==c("n")]
    
    outliers=thisData$value[thisData$stat==c("outlier")]
    bpData$out=c(bpData$out,outliers)
    bpData$group=c(bpData$group,rep(i,length(outliers)))
    
  }
  
  return(bpData)
  #boxplat wants:
  # Value
  # List with the following components:
  # stats	
  #  a matrix, each column contains the extreme of the lower whisker, the lower hinge, the median, the upper hinge and 
  # the extreme of the upper whisker for one group/plot. If all the inputs have the same class attribute, so will this component.
  # n	
  #  a vector with the number of (non-NA) observations in each group.
  # conf	
  #  a matrix where each column contains the lower and upper extremes of the notch.
  # out	
  #  the values of any data points which lie beyond the extremes of the whiskers.
  # group	
  #  a vector of the same length as out whose elements indicate to which group the outlier belongs.
  # names	
  #  a vector of names for the groups.
  
}
  
  



