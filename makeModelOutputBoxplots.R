# ------------------------------------------------------------------------------
#Sample code to show how to make bxplt from db
# pull data from db to generate data in the format necessary to create boxplot -- this will be moved to shiny side
makeBoxplotData=function(dbdf=dbGetQuery(conn,"SELECT * FROM summarystatistics;")){
  groups=unique(dbdf[c("site","metric","simdate","rundate")])
  bpData=list(stats=matrix(nrow=5,ncol=nrow(groups)),n=rep(NA,nrow(groups)),out=vector(),group=vector(),names=vector())
  for(i in 1:nrow(groups)){
    thisName=paste0(groups[i,"site"],".",groups[i,"metric"],"_simDate:",groups[i,"simdate"],"_runDate:",groups[i,"rundate"])
    bpData$names[i]=thisName
    thisData=merge(groups[i,],dbdf, all=F)
    thisData$ssid=NULL#drop this column so unique works
    thisData=unique(thisData) # drop duplicate records (from multiple model runs on the same day)
    #if there are still multiple model runs on the same days(simdate and rundate), warn but proceed
    if(length(thisData$value[thisData$stat==c("min")])!=1 ){
      warning("multiple model outputs found for: \n",paste(capture.output(print(groups[i,])),collapse="\n"))
    }
    bpData$stats[,i]=c(mean(thisData$value[thisData$stat==c("min")]),
                       mean(thisData$value[thisData$stat==c("lower_hinge")]),
                       mean(thisData$value[thisData$stat==c("med")]),
                       mean(thisData$value[thisData$stat==c("upper_hinge")]),
                       mean(thisData$value[thisData$stat==c("max")]))

    bpData$n[i]=mean(thisData$value[thisData$stat==c("n")])

    outliers=thisData$value[thisData$stat==c("outlier")]
    bpData$out=c(bpData$out,outliers)
    bpData$group=c(bpData$group,rep(i,length(outliers)))

  }

  return(bpData)
}
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


dbGetQuery(conn,"SELECT * FROM summarystatistics LIMIT 100;")
dbGetQuery(conn,"SELECT DISTINCT(simdate) FROM summarystatistics WHERE rundate = '2024-09-27' ORDER BY simdate;")

dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site, metric) * FROM summarystatistics ORDER BY simdate, site, metric, ssid DESC;")

dbGetQuery(conn, "SELECT * FROM 
           (SELECT DISTINCT ON (simdate, site, metric, stat) * FROM summarystatistics ORDER BY simdate, site, metric, stat, ssid DESC) AS lastRun 
           WHERE site= 'bwh' AND metric = 'irr_vol'  LIMIT 100;")

# SAMPLE code to show how to make boxplot from data stored in db - mmove to shiny
bxpList=makeBoxplotData(dbGetQuery(conn,"SELECT * FROM 
           (SELECT DISTINCT ON (simdate, site, metric, stat) * FROM summarystatistics ORDER BY simdate, site, metric, stat, ssid DESC) AS lastRun 
           WHERE site= 'bwh' AND metric = 'irr_vol' AND simdate >= '2024-04-01' AND simdate <='2024-05-01' ORDER BY simdate;"))
bxp(bxpList)
