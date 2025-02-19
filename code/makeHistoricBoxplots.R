febQ=dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site) site, simdate, rundate, values FROM forecastvolumes WHERE site= 'bwh' AND simdate >= '2021-02-01' ORDER BY simdate DESC, site, rundate DESC;")
febQ=febQ[order(febQ$simdate),]

pgArrayToNumeric=function(x){
  f=function(one_x){
    return(as.numeric(unlist(strsplit(gsub("[\\{\\}]","",one_x),","))))
  }
  return(lapply(x,f))
}


qs=pgArrayToNumeric(febQ$values)
febQ$medianQ=sapply(qs,median)
boxplot(x=qs,names=febQ$simdate,ylim=c(0,1000),las=2)
hist(unlist(qs[1]))
