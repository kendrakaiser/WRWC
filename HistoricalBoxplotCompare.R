q_daily=dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site) site, simdate, rundate, values FROM forecastvolumes WHERE site= 'bwh' 
               AND simdate >= '2024-02-01' AND simdate <= '2024-04-30' AND rundate = '2025-04-06' ORDER BY simdate DESC, site, rundate DESC;")
q_daily=q_daily[order(q_daily$simdate),]

pgArrayToNumeric=function(x){
  f=function(one_x){
    return(as.numeric(unlist(strsplit(gsub("[\\{\\}]","",one_x),","))))
  }
  return(lapply(x,f))
}

q_daily_all=pgArrayToNumeric(q_daily$values)
q_daily$medianQ=sapply(q_daily_all,median)
boxplot(x=q_daily_all,names=q_daily$simdate,ylim=c(0,600),las=2, main="models refitted daily")



q_monthly=dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site) site, simdate, rundate, values FROM forecastvolumes WHERE site= 'bwh' 
               AND simdate >= '2024-02-01' AND simdate <= '2024-04-30' AND rundate = '2025-04-05' ORDER BY simdate DESC, site, rundate DESC;")
nrow(q_monthly)
q_monthly=q_monthly[order(q_monthly$simdate),]

pgArrayToNumeric=function(x){
  f=function(one_x){
    return(as.numeric(unlist(strsplit(gsub("[\\{\\}]","",one_x),","))))
  }
  return(lapply(x,f))
}

q_monthly_all=pgArrayToNumeric(q_monthly$values)
q_monthly$medianQ=sapply(q_monthly_all,median)
boxplot(x=q_monthly_all,names=q_monthly$simdate,ylim=c(0,600),las=2,main = "models not refitted daily")
#hist(unlist(qs[1]))