# ------------------------------------------------------------------------------
# Data Management for Wood River Model 
# Kendra Kaiser
# August, 2022
# ------------------------------------------------------------------------------

#TODO: split the names to two variables make the dates come through 
#write to database, but overright it every day
pred.intervals<- pi %>% pivot_longer(everything(), names_to = "sitePI", values_to = "dailyFlow")

volumes.sampleLong<- vol.sample %>% pivot_longer(everything(), names_to = "site_name", values_to = "vol_af") 

#add run date?
#vol.bws<- volumes.sampleLong %>% filter(site_name == 'bws.vol')

#curt.sampleLong<- curt.sample

# Function to calculate summary statistics from model runs that can be used to generate irrigation season volume boxplots
writeSummaryStats=function(x,site.metric,simDate,runDate=Sys.Date()){
'x:x is the sample for which summary stats will be written to db'
  simDate=as.Date(simDate)
  runDate=as.Date(runDate)

  
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

#writeSummaryStats(c(runif(1000),10,11),"poodle.dog",simDate=Sys.Date()-1)  

# push model output to database - is in log form
writeSummaryStats(x=exp(vol.sample$bwh.irr_vol), site.metric="bwh.irr_vol",simDate=end_date)
writeSummaryStats(x=exp(vol.sample$bws.irr_vol), site.metric="bws.irr_vol",simDate=end_date)
writeSummaryStats(x=exp(vol.sample$cc.irr_vol), site.metric="cc.irr_vol",simDate=end_date)
writeSummaryStats(x=exp(vol.sample$sc.irr_vol), site.metric="sc.irr_vol",simDate=end_date)

# TODO: test/ is this working?
# Write daily prediction interval data to db for generating timeseries forecast figures
dbWriteTable(conn,"predictionintervals",pi,overwrite=T)
#bGetQuery(conn,"SELECT * FROM predictionintervals;")


# pull data from db to generate data in the format necessary to create boxplot -- this will be moved to shiny side
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
  #boxplot wants:
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

# SAMPLE code to show how to make boxplot from data stored in db - mmove to shiny
bxpList=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'bwh' AND metric = 'irr_vol' AND simdate='2023-10-01';"))
bxp(bxpList)


