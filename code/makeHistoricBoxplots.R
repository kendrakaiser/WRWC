febQ=dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site) site, simdate, rundate, values FROM forecastvolumes WHERE site= 'bwh' AND simdate >= '2025-02-01' ORDER BY simdate DESC, site, rundate DESC;")
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
#hist(unlist(qs[1]))


dbModels=dbGetQuery(conn,"SELECT * FROM volumemodels WHERE rundate = (SELECT MAX(rundate) FROM volumemodels);")
fromJSON(dbModels$modelcoefjson[1])
boxplot(fromJSON(dbModels$modeldatajson[1])$bwh.snow_covered_area)
fromJSON(dbModels$modelpredictors[1])


wsh=st_read(conn,query="SELECT locations.locationid, locations.name, watersheds.geometry FROM watersheds LEFT JOIN locations ON watersheds.outflowlocationid = locations.locationid WHERE locations.name IN ('BIG WOOD RIVER AT HAILEY','BIG WOOD RIVER AT STANTON CROSSING', 
'SILVER CREEK AT SPORTSMAN ACCESS', 'CAMAS CREEK NR BLAINE ID');")


#units fuckery:
#1 mm * 1 meter^2 = 1000000 mm^3
#1 kg/m^2 = 1000 g/m^2
#1000 g/m2 =  1000000 mg/m^2
