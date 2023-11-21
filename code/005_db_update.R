#db updater
source(file.path(git_dir, 'code/init_db.R'))
source(file.path(git_dir, 'code/01_packages.R'))
source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) 
source(paste0(git_dir,"/code/fxn_get_snow.R")) 
source(paste0(git_dir,"/code/fxn_SNODASR_functions.R")) 

dbExecute(conn,"CREATE OR REPLACE FUNCTION wateryear(datetime timestamp without time zone) RETURNS integer AS $$
              SELECT CASE WHEN ( EXTRACT(month FROM datetime)) >= 10  THEN EXTRACT(year FROM datetime) +1 
                                      ELSE EXTRACT(year FROM datetime) 
                        END
                $$
            LANGUAGE SQL;")


# snodas=rbind(grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="snow_covered_area"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="liquid_precip"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="swe_total"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="runoff_total")
# )
# 



updateDbData=function(metric,location,days,sourceName,rebuildInvalidData=F){ #location can be locationID or locationName
  
  #if rebuildInvalidData=T, returned dataset will include invalid data
  
  ###test args for debug
  # metric="streamflow"
  # location="BIG WOOD RIVER AT HAILEY"
  # days=seq.Date(as.Date("2021-01-01"),as.Date("2024-01-02"),by="day")
  # sourceName="USGS"
  # rebuildInvalidData=F
  ###
  
  
  days=as.Date(days)
  
  #metric check
  if(is.na(suppressWarnings({ as.numeric(metric)}))){  #metric is not numeric
    metricID = dbGetQuery(conn, paste0("SELECT metricid FROM metrics WHERE name = '",metric,"';"))$metricid
    if(length(metricID)==0){  #metric is not found in db
      print("Metrics in database:")
      print(dbGetQuery(conn,"SELECT * FROM METRICS"),max=1000)
      stop(message=paste0("metric `",metric,"` not found." )  )
    }
  } else{ #metric is or can be coerced to numeric
    metricID=as.numeric(metric)
    metric=dbGetQuery(conn,paste0("SELECT name FROM metrics WHERE metricID = '",metricID,"';"))$name
  }
  
  #location check
  if(is.na(suppressWarnings({ as.numeric(location)}))){  #location is not numeric
    locationID = dbGetQuery(conn, paste0("SELECT locationid FROM locations WHERE name = '",location,"';"))$locationid
    if(length(locationID)==0){  #location is not found in db
      print(paste0("locations in database with metric `",metric,"`:"))
      locs=dbGetQuery(conn, "SELECT locationid, name, metrics FROM locationattributes")
      locs=locs[grep(metricName, locs$metrics),c(1,2)]
      print(locs,max=1000)
      stop(message=paste0("location `",location,"` not found.")  )
    }
  } else{ #location is or can be coerced to numeric
    locationID=as.numeric(location)
  }
  
  
  #if rebuild
  if(rebuildInvalidData){
    dbExecute(conn,paste0("DELETE FROM data WHERE metricid = '",metricID,"' AND locationid = '",locationID,"' AND datetime::date IN ('",paste(days,collapse="', '"),"') AND qcstatus = 'false';"))
  }
  
  #get existing data from db:
  #changed to just get a few columns instead of whole dataset
  dataInDb=dbGetQuery(conn,paste0("SELECT dataid, datetime, qcstatus FROM data WHERE metricid = '",metricID,"' AND locationid = '",locationID,"' AND datetime::date IN ('",paste(days,collapse="', '"),"') ORDER BY datetime;"))
  
  #if rebuild:
  # if(rebuildInvalidData){ #drop records w/ qc=F to trigger rebuild
  #   dataInDb=dataInDb[dataInDb$qcstatus==TRUE,]
  # }
  
  #check for complete dataset
  if(!all(days %in% dataInDb$datetime)){ #data for some days is not in database    
    missingDays=days[!days %in% dataInDb$datetime] 
    
    
    ###################--------------- data sourcing functions -------------------------
    
    sourceUSGS=function(metricID, locationID, days){
      print("sourcing USGS data...")
      
      ##db knows about these as source_site_id
      # bwb = 13139510  #  Bullion Bridge, Big Wood at Hailey
      # bws = 13140800  #  Stanton Crossing, Big Wood
      # cc  = 13141500  #  Camas Creek near Blaine
      # sc  = 13150430  #  Silver Creek near Picabo
      # bwr = 13142500  #  Big Wood below Magic near Richfield
      # mr  = 13142000  #  Magic Reservoir storage in acre - feet, impt for carry-over
      # bwbr = 13140335 # Big Wood at S Broadford Bridge Nr Bellevue, data only goes back to 2017
      # bwk = 13135500  #  Big Wood nr Ketchum, goes to 2011
      # usgs_sites = c(bwb, bws, cc, sc, bwr) #  put all sites in one vector
      
      thisLocation_sourceID=dbGetQuery(conn,paste0("SELECT source_site_id FROM locations WHERE locationid = '",locationID,"';"))$source_site_id
      
      pCode = "00060" # USGS code for streamflow
      #     sCode = "00054" # USGS code for reservoir storage (acre-feet) -- not currently used?
      
      # Dataframe with information about sites and period of record, uv = instantaneous value
      # site_info<- whatNWISdata(sites= thisLocation_sourceID, parameterCd = pCode, outputDataTypeCd ='uv') 
      # min_date=site_info$begin_date
      # max_date=site_info$end_date
      # startDate = max(min_date,min(days))
      # endDate = min(max_date,max(days))
      # if(endDate<startDate){
      #   endDate=startDate
      # }
      
      
      # Dowload data from this location
      streamflow <- readNWISdv(siteNumbers = thisLocation_sourceID, parameterCd = pCode, startDate = min(days), endDate = max(days) ) %>% renameNWISColumns() %>% data.frame
      
      
      if(!all(days %in% streamflow$Date)){
        addDays=days[!days %in% streamflow$Date]
        streamflow=rbind(streamflow,data.frame(agency_cd="USGS",site_no=thisLocation_sourceID,Date=addDays,Flow=NA,Flow_cd="bad"))
      }
      
      streamflow$qcStatus=T
      streamflow$qcStatus[is.na(streamflow$Flow)]=F
      streamflow$Flow[streamflow$qcStatus==F]=-999
      
      
      #write to db
      dbWriteData(metric="streamflow",value=streamflow$Flow,datetime=streamflow$Date,locationID=locationID,sourceName="USGS",qcStatus=streamflow$qcStatus)
      
    }
    
    sourceSnotel=function(metricID, locationID, days){
      #db knows internal snotel source location ids:
      thisLocation_sourceID=dbGetQuery(conn,paste0("SELECT source_site_id FROM locations WHERE locationid = '",locationID,"';"))$source_site_id
      
      snotel_data = snotel_download(thisLocation_sourceID, path = tempdir(), internal = TRUE)
      #or all locations?  Test time...
      
    }
    
    #call appropriate source function:
    if(sourceName == "USGS"){
      sourceUSGS(metricID,locationID,missingDays)
    }
    if(sourceName == "snotel"){
      sourceSnotel(metricID,locationID,missingDays)
    }
    
    #try to source the missing data
    #dbSourceNewData(metric=metric,locationID=locationID,days=missingDays)
    
    #get dataset again after adding new data
    #dataInDb=dbGetQuery(conn,paste0("SELECT * FROM data WHERE metricid = '",metricID,"' AND locationid = '",locationID,"' AND datetime::date IN ('",paste(days,collapse="', '"),"') ORDER BY datetime;"))
  }
  
  # if(!rebuildInvalidData){  #strip qc=F data out of returned dataset
  #   dataInDb=dataInDb[dataInDb$qcstatus==TRUE,]
  # }
  
  #return(dataInDb)
}

updateDbData(metric="USGS", location="BIG WOOD RIVER AT HAILEY", days=seq.Date(as.Date("1996-01-01"),as.Date("2023-11-20"),by="day"),sourceName="USGS")


updateDbData(metric="streamflow", location="BIG WOOD RIVER AT HAILEY", days=seq.Date(as.Date("1996-01-01"),as.Date("2023-11-20"),by="day"),sourceName="USGS")
