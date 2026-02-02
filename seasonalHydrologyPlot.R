git_dir=getwd()
source(file.path(git_dir, 'code/01_packages.R'))
source(file.path(git_dir, 'code/init_db.R'))
library(lubridate)

#seasonality and hydrologic characterization plot
dbGetQuery(conn,"SELECT * FROM metrics;")

#sites=c('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )


makeplotData=function(site){
  
  avg_flow=dbGetQuery(conn,paste0("SELECT AVG(value) AS streamflow, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'flow'
                          AND name = '",site,"'
                          GROUP BY location_name, j_day
                    ORDER BY j_day;"))
  
  avg_swe_total= dbGetQuery(conn,paste0("SELECT AVG(value) AS swe_total, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name, data.locationid FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'swe_total'
                          AND name = '",site,"'
                          GROUP BY location_name, j_day, data.locationid
                          ORDER BY j_day;"))
  
  #dbGetQuery(conn,"SELECT ST_SRID(geometry) FROM watersheds WHERE outflowlocationid = '140';")
  
  wshedAreaKm=dbGetQuery(conn,paste0("SELECT ST_AREA(watersheds.geometry) AS m2 FROM locations
                        LEFT JOIN watersheds ON locations.locationid = watersheds.outflowlocationid
                        WHERE locations.name = '",site,"';"))$m2 / (1000 * 1000)
  
  avg_swe_total$swe_mm=1000*avg_swe_total$swe_total/wshedAreaKm
  
  plotDF=merge(avg_flow,avg_swe_total)
  plotDF=plotDF[plotDF$j_day<=365,]
  plotDF$plotDate=ymd("1999-12-31")+plotDF$j_day
  
  
  wy_d1=ymd("2000-10-01")-ymd("2000-01-01")+1
  
  plotDF$wy_j_day=plotDF$j_day-wy_d1
  plotDF$wy_j_day[plotDF$wy_j_day < 0]=plotDF$wy_j_day[plotDF$wy_j_day < 0]+365
  plotDF=plotDF[order(plotDF$wy_j_day),]
  
  return(plotDF)
}

#plot matrix.......
layout(matrix(1:4,nrow=2,byrow=T))
axis_label_cex=0.9

#sites=c('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
site='BIG WOOD RIVER AT HAILEY'
plotDF=makeplotData(site)

fom_days=unique(floor_date(plotDF$plotDate, unit = "month")-ymd("2000-01-01"))+1

bwh_maxQ=max(plotDF$streamflow)

#par(oma=c(0,0,0,0))
par(mar=c(3,5,4,4))
plot(plotDF$streamflow~plotDF$wy_j_day,type="l",col="blue",ylim=c(0,bwh_maxQ*1.2),
     main = site,xlab="",ylab="",
     axes=F,lwd=2)
#axis.Date(side=1,x=plotDF$plotDate,format="%m-%d")
axis(side=1,at=plotDF$wy_j_day[plotDF$j_day %in% fom_days],
     labels=month(ymd("2000-01-01")+fom_days,label=T)
)
axis(side=4,col="blue",lwd=2)

par(new=T)
plot(plotDF$swe_mm~plotDF$wy_j_day,type="l",axes=F,xlab="",ylab="",lwd=2,ylim=c(0,max(plotDF$swe_mm)*1.1))
axis(side=2,lwd=2)
#mtext("Average Streamflow (cfs)",side=4,line=2.5,cex=axis_label_cex)
mtext("Average SWE across watershed (mm)",side=2,line=2.5,cex=axis_label_cex)
#mtext("Month of wateryear",side=1,line=2.5,cex=axis_label_cex)

legend(x="topleft",legend=c('Streamflow',"SWE"),lty=1,lwd=2,col=c("blue","black"),bty="n")



site='BIG WOOD RIVER AT STANTON CROSSING'
plotDF=makeplotData(site)

fom_days=unique(floor_date(plotDF$plotDate, unit = "month")-ymd("2000-01-01"))+1

par(mar=c(3,4,4,5))
plot(plotDF$streamflow~plotDF$wy_j_day,type="l",col="blue",ylim=c(0,bwh_maxQ*1.2),
     main = site,xlab="",ylab="",
     axes=F,lwd=2)
#axis.Date(side=1,x=plotDF$plotDate,format="%m-%d")
axis(side=1,at=plotDF$wy_j_day[plotDF$j_day %in% fom_days],
     labels=month(ymd("2000-01-01")+fom_days,label=T)
)
axis(side=4,col="blue",lwd=2)

par(new=T)
plot(plotDF$swe_mm~plotDF$wy_j_day,type="l",axes=F,xlab="",ylab="",lwd=2,ylim=c(0,max(plotDF$swe_mm)*1.1))
axis(side=2,lwd=2)
mtext("Average Streamflow (cfs)",side=4,line=2.5,cex=axis_label_cex)
#mtext("Average SWE across watershed (mm)",side=2,line=2.5,cex=axis_label_cex)
#mtext("Month of wateryear",side=1,line=2.5,cex=axis_label_cex)




site='CAMAS CREEK NR BLAINE ID'
plotDF=makeplotData(site)

fom_days=unique(floor_date(plotDF$plotDate, unit = "month")-ymd("2000-01-01"))+1

bwh_maxQ=max(plotDF$streamflow)

par(mar=c(5,5,2,4))
plot(plotDF$streamflow~plotDF$wy_j_day,type="l",col="blue",ylim=c(0,max(plotDF$streamflow)*1.2),
     main = site,xlab="",ylab="",
     axes=F,lwd=2)
#axis.Date(side=1,x=plotDF$plotDate,format="%m-%d")
axis(side=1,at=plotDF$wy_j_day[plotDF$j_day %in% fom_days],
     labels=month(ymd("2000-01-01")+fom_days,label=T)
)
axis(side=4,col="blue",lwd=2)

par(new=T)
plot(plotDF$swe_mm~plotDF$wy_j_day,type="l",axes=F,xlab="",ylab="",lwd=2,ylim=c(0,max(plotDF$swe_mm)*1.1))
axis(side=2,lwd=2)
#mtext("Average Streamflow (cfs)",side=4,line=2.5,cex=axis_label_cex)
mtext("Average SWE across watershed (mm)",side=2,line=2.5,cex=axis_label_cex)
mtext("Month of wateryear",side=1,line=2.5,cex=axis_label_cex)





site='SILVER CREEK AT SPORTSMAN ACCESS'
plotDF=makeplotData(site)

fom_days=unique(floor_date(plotDF$plotDate, unit = "month")-ymd("2000-01-01"))+1

bwh_maxQ=max(plotDF$streamflow)

par(mar=c(5,4,2,5))
plot(plotDF$streamflow~plotDF$wy_j_day,type="l",col="blue",ylim=c(0,max(plotDF$streamflow)*1.2),
     main = site,xlab="",ylab="",
     axes=F,lwd=2)
#axis.Date(side=1,x=plotDF$plotDate,format="%m-%d")
axis(side=1,at=plotDF$wy_j_day[plotDF$j_day %in% fom_days],
     labels=month(ymd("2000-01-01")+fom_days,label=T)
)
axis(side=4,col="blue",lwd=2)

par(new=T)
plot(plotDF$swe_mm~plotDF$wy_j_day,type="l",axes=F,xlab="",ylab="",lwd=2,ylim=c(0,max(plotDF$swe_mm)*1.1))
axis(side=2,lwd=2)
mtext("Average Streamflow (cfs)",side=4,line=2.5,cex=axis_label_cex)
#mtext("Average SWE across watershed (mm)",side=2,line=2.5,cex=axis_label_cex)
mtext("Month of wateryear",side=1,line=2.5,cex=axis_label_cex)




# #snotel metrics:
# 
# avg_daily_precip= dbGetQuery(conn,"SELECT AVG(value) AS daily_precip, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
#                           LEFT JOIN locations ON data.locationid = locations.locationid
#                           WHERE qcstatus = 'true' AND metric = 'daily precip'
#                           GROUP BY location_name, j_day;")
# 
# avg_swe= dbGetQuery(conn,"SELECT AVG(value) AS swe, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
#                           LEFT JOIN locations ON data.locationid = locations.locationid
#                           WHERE qcstatus = 'true' AND metric = 'swe'
#                           GROUP BY location_name, j_day;")
# 
# plot(avg_swe$swe~avg_swe$j_day)
