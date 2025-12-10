source(file.path(getwd(), 'code/init_db.R'))

q25=dbGetQuery(conn,"SELECT DISTINCT ON (simdate, site) site, simdate, rundate, values FROM forecastvolumes WHERE site= 'bwh' 
               AND simdate >= '2024-02-01' AND simdate <='2024-05-01' ORDER BY simdate DESC, site, rundate DESC;")
q25=q25[order(q25$simdate),]
max(q25$simdate)

pgArrayToNumeric=function(x){
  f=function(one_x){
    return(as.numeric(unlist(strsplit(gsub("[\\{\\}]","",one_x),","))))
  }
  return(lapply(x,f))
}

qs=pgArrayToNumeric(q25$values)
q25$medianQ=sapply(qs,median)
boxplot(x=qs,names=q25$simdate,ylim=c(0,600),las=2, main="bwh no snodas")
#hist(unlist(qs[1]))


dbModels=dbGetQuery(conn,"SELECT * FROM volumemodels WHERE rundate = (SELECT MAX(rundate) FROM volumemodels);")
fromJSON(dbModels$modelcoefjson[1])
fromJSON(dbModels$modelpredictors[1])


wsh=st_read(conn,query="SELECT locations.locationid, locations.name, watersheds.geometry FROM watersheds LEFT JOIN locations ON watersheds.outflowlocationid = locations.locationid WHERE locations.name IN ('BIG WOOD RIVER AT HAILEY','BIG WOOD RIVER AT STANTON CROSSING', 
'SILVER CREEK AT SPORTSMAN ACCESS', 'CAMAS CREEK NR BLAINE ID');")


#units fuckery:
#1 mm * 1 meter^2 = 1000000 mm^3
#1 kg/m^2 = 1000 g/m^2
#1000 g/m2 =  1000000 mg/m^2
# 

# dbExecute(conn,"DELETE FROM volumemodels WHERE (moddate >= '2024-02-01' AND moddate <='2024-05-01');")
# dbExecute(conn,"DELETE FROM centermassmodels WHERE (moddate >= '2024-02-01' AND moddate <='2024-05-01');")
# dbExecute(conn,"DELETE FROM forecastvolumes WHERE (simdate >= '2024-02-01' AND simdate <='2024-05-01');")

