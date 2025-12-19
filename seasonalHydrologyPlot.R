git_dir=getwd()
source(file.path(git_dir, 'code/01_packages.R'))
source(file.path(git_dir, 'code/init_db.R'))

#seasonality and hydrologic characterization plot
dbGetQuery(conn,"SELECT * FROM metrics;")
#('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )

avg_flow=dbGetQuery(conn,"SELECT AVG(value) AS streamflow, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'flow'
                          AND name IN ('BIG WOOD RIVER AT HAILEY')
                          GROUP BY location_name, j_day
                    ORDER BY j_day;")

avg_swe_total= dbGetQuery(conn,"SELECT AVG(value) AS swe_total, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name, data.locationid FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'swe_total'
                          AND name IN ('BIG WOOD RIVER AT HAILEY')
                          GROUP BY location_name, j_day, data.locationid
                          ORDER BY j_day;")

#dbGetQuery(conn,"SELECT ST_SRID(geometry) FROM watersheds WHERE outflowlocationid = '140';")

wshedAreaKm=dbGetQuery(conn,"SELECT ST_AREA(watersheds.geometry) AS m2 FROM locations
                        LEFT JOIN watersheds ON locations.locationid = watersheds.outflowlocationid
                        WHERE locations.name ='BIG WOOD RIVER AT HAILEY';")$m2 / (1000 * 1000)

avg_swe_total$swe_mm=1000*avg_swe_total$swe_total/wshedAreaKm

par(mar=c(5,4,4,4))
plot(avg_flow$streamflow~avg_flow$j_day,type="l",col="blue",xlab = "day of year", ylab="average streamflow (cfs)", main = "BIG WOOD RIVER AT HAILEY")
par(new=T)
plot(avg_swe_total$swe_mm~avg_swe_total$j_day,type="l",axes=F,xlab="",ylab="")
axis(side=4)
mtext("average snodas swe for watershed (mm)",side=4,line=2.5)


#snotel metrics:

avg_daily_precip= dbGetQuery(conn,"SELECT AVG(value) AS daily_precip, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'daily precip'
                          GROUP BY location_name, j_day;")

avg_swe= dbGetQuery(conn,"SELECT AVG(value) AS swe, EXTRACT(doy FROM datetime) AS j_day, locations.name AS location_name FROM data 
                          LEFT JOIN locations ON data.locationid = locations.locationid
                          WHERE qcstatus = 'true' AND metric = 'swe'
                          GROUP BY location_name, j_day;")

plot(avg_swe$swe~avg_swe$j_day)
