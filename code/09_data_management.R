# ------------------------------------------------------------------------------
# Data Management for Wood River Model 
# Kendra Kaiser
# August, 2022
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Write model structure to database
# ------------------------------------------------------------------------------

writeModel=function(model,modelName,enddate=end_date){
  moddate=as.Date(paste0(year(enddate), "/", month(enddate), "/", 1))
  rundate=Sys.Date()
  
  writeModelDF=data.frame(modelname=modelName,
                          moddate=moddate,rundate=rundate,
                          modelcoefjson=toJSON(data.frame(model$coefficients)),
                          modeldatajson=toJSON(model$model),
                          modelpredictors=toJSON(model$predictors)
                          
  )
  
  dbExecute(conn,paste0("DELETE FROM volumemodels WHERE modelname = '",modelName,
                        "' AND rundate = '",rundate,"' AND moddate = '",moddate,"';"))
  
  dbWriteTable(conn,"volumemodels",writeModelDF,append=T)
  
}    

mapply(writeModel,vol_models,names(vol_models))


# ------------------------------------------------------------------------------
# write the prediction intervals for daily streamflow output (cfs)
# ------------------------------------------------------------------------------
pi_date=pi
pi_date$date=as.Date(rownames(pi))
dbWriteTable(conn,"predictionintervals",pi_date,overwrite=T)

#dbGetQuery(conn,"SELECT * FROM predictionintervals;")


# ------------------------------------------------------------------------------
# write Curtailment model output
# ------------------------------------------------------------------------------
#curt.sampleLong<- curt.sample

# ------------------------------------------------------------------------------
# Volume model output, seasonal AF in logged units
#TODO update how the logged output is managed
writeVolModelOutput=function(x,site.metric,simDate,runDate=Sys.Date()){
  'x:x is the sample for which model output will be written to db'
  simDate=as.Date(simDate)
  runDate=as.Date(runDate)
  
  
  if(length(strsplit(site.metric,"\\.")[[1]])!=2){
    stop(paste0("Invalid site.metric ",site.metric))
  }
  
  site=strsplit(site.metric,"\\.")[[1]][1]
  metric=strsplit(site.metric,"\\.")[[1]][2]
  
  modelOutputDF=data.frame(site=site,metric=metric,rundate=runDate,simdate=simDate,value=x)
  
  #dont allow duplicate entries (same run day and same simualted day)
  # dbExecute(conn,paste0("DELETE FROM forecastvolumes WHERE site = '",site,"' AND metric = '",metric,
  #                       "' AND rundate = '",runDate,"' AND simdate = '",simDate,"';"))
  # 
  # dbWriteTable(conn,"forecastvolumes",modelOutputDF,append=T)
  # 
  # 

  #dont allow duplicate entries (same run day and same simualted day)
  dbExecute(conn,paste0("DELETE FROM forecastvolumes WHERE site = '",site,"' AND metric = '",metric,
                        "' AND rundate = '",runDate,"' AND simdate = '",simDate,"';"))
  
  dbExecute(conn,paste0("INSERT INTO forecastvolumes (site, metric, rundate, simdate, values) VALUES ('",site,"', '",metric,"', '",runDate,"', '",simDate,
                        "', '{",paste(x,collapse=","),"}');"
                        )
            )
  
  
}

writeVolModelOutput(x=vol.sample$bwh.irr_vol, site.metric="bwh.irr_vol",simDate=end_date)
writeVolModelOutput(x=vol.sample$bws.irr_vol, site.metric="bws.irr_vol",simDate=end_date)
writeVolModelOutput(x=vol.sample$cc.irr_vol, site.metric="cc.irr_vol",simDate=end_date)
writeVolModelOutput(x=vol.sample$sc.irr_vol, site.metric="sc.irr_vol",simDate=end_date)

#pulling data back out:
#lastForecasts=dbGetQuery(conn,"SELECT DISTINCT ON (simdate) forecastvolumes.* FROM forecastvolumes ORDER BY simdate, rundate DESC;")
# unique(lastForecasts$rundate)

#dbExecute(conn,"CREATE INDEX rundate_idx ON forecastvolumes (rundate);")
# Write summary statistics for predicted irrigation season volumes
# ------------------------------------------------------------------------------

#------------ THIS (below) is to be depreciated, replaced bu writeVolModelOutput above
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
  
  dbExecute(conn,paste0("DELETE FROM summarystatistics WHERE site = '",site,"' AND metric = '",metric,
                        "' AND rundate = '",runDate,"' AND simdate = '",simDate,"';"))
  
  dbWriteTable(conn,"summarystatistics",statDF,append=T)
}

# push model output to database
writeSummaryStats(x=vol.sample$bwh.irr_vol, site.metric="bwh.irr_vol",simDate=end_date)
writeSummaryStats(x=vol.sample$bws.irr_vol, site.metric="bws.irr_vol",simDate=end_date)
writeSummaryStats(x=vol.sample$cc.irr_vol, site.metric="cc.irr_vol",simDate=end_date)
writeSummaryStats(x=vol.sample$sc.irr_vol, site.metric="sc.irr_vol",simDate=end_date)


# ------------------------------------------------------------------------------
#Sample code to show how to make bxplt from db
# pull data from db to generate data in the format necessary to create boxplot -- this will be moved to shiny side
# makeBoxplotData=function(dbdf=dbGetQuery(conn,"SELECT * FROM summarystatistics;")){
#   groups=unique(dbdf[c("site","metric","simdate","rundate")])
#   bpData=list(stats=matrix(nrow=5,ncol=nrow(groups)),n=rep(NA,nrow(groups)),out=vector(),group=vector(),names=vector())
#   for(i in 1:nrow(groups)){
#     thisName=paste0(groups[i,"site"],".",groups[i,"metric"],"_simDate:",groups[i,"simdate"],"_runDate:",groups[i,"rundate"])
#     bpData$names[i]=thisName
#     thisData=merge(groups[i,],dbdf, all=F)
#     thisData$ssid=NULL#drop this column so unique works
#     thisData=unique(thisData) # drop duplicate records (from multiple model runs on the same day)
#     #if there are still multiple model runs on the same days(simdate and rundate), warn but proceed
#     if(length(thisData$value[thisData$stat==c("min")])!=1 ){
#       warning("multiple model outputs found for: \n",paste(capture.output(print(groups[i,])),collapse="\n"))
#     }
#     bpData$stats[,i]=c(mean(thisData$value[thisData$stat==c("min")]),
#                        mean(thisData$value[thisData$stat==c("lower_hinge")]),
#                        mean(thisData$value[thisData$stat==c("med")]),
#                        mean(thisData$value[thisData$stat==c("upper_hinge")]),
#                        mean(thisData$value[thisData$stat==c("max")]))
#     
#     bpData$n[i]=mean(thisData$value[thisData$stat==c("n")])
#     
#     outliers=thisData$value[thisData$stat==c("outlier")]
#     bpData$out=c(bpData$out,outliers)
#     bpData$group=c(bpData$group,rep(i,length(outliers)))
#     
#   }
#   
#   return(bpData)
# }
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



# SAMPLE code to show how to make boxplot from data stored in db - mmove to shiny
# bxpList=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM summarystatistics WHERE site= 'bwh' AND metric = 'irr_vol' AND simdate='2023-10-01';"))
# bxp(bxpList)


# ------------------------------------------------------------------------------
# Calculate Exceedance Probabilities and write to db
# ------------------------------------------------------------------------------

exceed.probs<- function(vols, probs){
  'calculate exceedance probabilities of model output
  p=m/(n+1)
  vols: numeric of volumes
  probs: array of probabilities to calculate'
  
  n=length(vols) 
  m=round(probs*(n+1))
  #rank the sampled volumes
  ranks<- rank(vols)
  # find the index of which volume goes with each exceedance
  ix=match(m, ranks)
  # find the volume of each exceedance
  ex.vols=vols[ix]
  return(ex.vols)
}

# Exceedance probs from NWRFC
prb<- c(0.1, 0.25, 0.5, 0.75, 0.9)
# Calculate exceedance probabilities and save table with labels
ex.vols<- round(apply(vol.sample, 2, exceed.probs, prb)) %>% as.data.frame()
ex.vols$Exceedance <- c('90%', '75%', '50%', '25%', '10%') 
ex.vols<- ex.vols%>% relocate(Exceedance)

my_table_theme <- ttheme_default(core=list(fg_params = list(col = c("red","darkorange","green3","deepskyblue", "blue3"), col=NA)))
#grid.table(ex.vols, theme = my_table_theme, rows = NULL)

# Write exceedance prob to db

dbWriteTable(conn,"exceednaceprobabilities",ex.vols,overwrite=T)


