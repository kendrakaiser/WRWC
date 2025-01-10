#db updater
#git_dir <<- '~/github/WRWC'
#sam's computer: 
#git_dir <<- getwd()

source(file.path(git_dir, 'code/init_db.R'))
source(file.path(git_dir, 'code/01_packages.R'))
source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) 
source(paste0(git_dir,"/code/fxn_SNODASR_functions.R")) 
source(file.path(git_dir, 'code/fxn_grabAgriMetData.R'))

# dbExecute(conn,"CREATE OR REPLACE FUNCTION wateryear(datetime timestamp without time zone) RETURNS integer AS $$
#               SELECT CASE WHEN ( EXTRACT(month FROM datetime)) >= 10  THEN EXTRACT(year FROM datetime) +1 
#                                       ELSE EXTRACT(year FROM datetime) 
#                         END
#                 $$
#             LANGUAGE SQL;")





###########------------------ snodas data getter function --------------------------------
# average snow pack temperature; parameter is not included in files prior to 21 February 2004.
### ---------------- Grab and/or Process SNODAS Data ------------------------ ###
# Wrapper function that pulls data from db if it exists or downloads and processes SNODAS data

update_ws_snow=function(ws_ids, dates, metric, rebuildAllMissingData=F){
  
  
  ###########-----------------functions and definitions------------------------------------------
  
  #data frame of snodas-derived metric names and units
  # add additional metrics here
  allMetrics=data.frame(metric=c("swe_total", "runoff_total", "snow_covered_area", "liquid_precip"),
                        units=c("meters", "meters", "km2", "mm")
  )
  
  #####-------functions for each metric-----------
  # each new derived metric must be defined here, and must have a function created to calculate it
  # the function is then matched to the metric in the 'switch' statement in the main function
  
  extract_ws_swe <- function(ws_id, ws_geoms, d){ 
    
    # geometries of sub watershed to use to extract metrics of interest
    ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
    ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
    ## --- extract all SNODAS values wanted 
    out_img<-extract.SNODAS.subset(d, values_wanted='SWE', extent=ws_extent, write_file = FALSE) 
    #convert images to raster
    out_rast=rast(out_img[[1]])
    
    ## --- extract values from all relevant parameters and modify into metrics ---#
    out_swe<-terra::extract(out_rast, vect(ws_geom_tr))
    #hist(out_swe[,2])
    tot_swe<-sum(out_swe[,2],na.rm=T)
    # convert to meters using scale factor from user guide
    tot_swe_m=tot_swe/1000 
    
    # add error catch to make sure there is data in here
    return(tot_swe_m)
  }
  #Extract total 24 hour melt (m)
  extract_ws_runoff <- function(ws_id, ws_geoms, d){ 
    # geometries of sub watershed to use to extract metrics of interest
    ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
    ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
    ## --- extract all SNODAS values wanted 
    out_img<-extract.SNODAS.subset(d, values_wanted='Runoff', extent=ws_extent, write_file = FALSE) 
    #convert images to raster
    out_rast=rast(out_img[[1]])
    
    ## --- extract values from all relevant parameters and modify into metrics ---#
    out_runoff<-terra::extract(out_rast, vect(ws_geom_tr))
    #hist(out_swe[,2])
    tot_runoff<-sum(out_runoff[,2],na.rm=T) 
    
    # convert integer to meters based on scale factor
    tot_runoff_m=tot_runoff/100000 
    
    return(tot_runoff_m)
    
  }
  # snow covered area (km2)
  extract_ws_sca <- function(ws_id, ws_geoms, d){ 
    # geometries of sub watershed to use to extract metrics of interest
    ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
    ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
    ## --- extract all SNODAS values wanted 
    out_img<-extract.SNODAS.subset(d, values_wanted='Depth', extent=ws_extent, write_file = FALSE) 
    #convert images to raster 
    out_rast=rast(out_img[[1]])
    
    ## --- extract values from all relevant parameters and modify into metrics ---#
    # extract and divide by scale factor to get into meters
    out_depth<-terra::extract(out_rast, vect(ws_geom_tr))/1000
    # ID which pixels have more than 0.15m of snow and sum
    sca = sum(out_depth[,2] > 0.15, na.rm=T)
    
    # add error catch to make sure there is data in here -- doesnt work yet
    #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
    #  , error = function(e) {message('Dataframe is EMPTY')
    #    print(e)})
    return(sca)
  }
  # liquid precip, 24hr total (mm)
  extract_ws_precip <- function(ws_id, ws_geoms, d){ 
    # geometries of sub watershed to use to extract metrics of interest
    ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
    ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
    ## --- extract all SNODAS values wanted 
    out_img<-extract.SNODAS.subset(d, values_wanted='P_Liquid', extent=ws_extent, write_file = FALSE) 
    #convert images to raster 
    out_rast=rast(out_img[[1]])
    
    ## --- extract values from all relevant parameters and modify into metrics ---#
    # extract and divide by scale factor to get into meters
    out_p<-terra::extract(out_rast, vect(ws_geom_tr))
    # convert to kg/m2 (mm) using scale factor from user guide
    tot_p<-sum(out_p[,2],na.rm=T)/10 
    
    # add error catch to make sure there is data in here -- doesnt work yet
    #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
    #  , error = function(e) {message('Dataframe is EMPTY')
    #    print(e)})
    return(tot_p)
  }
  
  
  #----------------------worker function for doing actual snodasing-------------------------
  grab_ws_snow_worker = function(ws_ids, d, metric, metricDefinitions=allMetrics){
    
    ###---------------- remove snodas source files directory to start clean ------------------###
    unlink("./SNODAS",recursive=T)
    
    ##for debug:
    #ws_ids = c(140,167)
    #d=as.Date("2001-04-03")
    #metric="swe_total"
    #metricDefinitions=snodasMetrics
    
    
    
    #####obsolete, handled in wrapper function instead to consolidate querys
    # #look in db to see if the data exists, return it if it does:
    # dbData=dbGetQuery(conn, paste0("SELECT * FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",d,"' AND metric = '",metric,"';"))
    # if(all(ws_ids %in% dbData$locationid)){
    #   return(dbData[,c("metric","value","datetime","locationid")])
    # }
    #if doesn't exist in database pull all data for that date / date range & calculate metrics
    
    ##--------------------------------------------------------------------------#
    ## --- Download the full SNODAS dataset
    dlWorked=tryCatch(
      download.SNODAS(d, parallel = FALSE, overwrite=TRUE, masked=TRUE),
      error=function(e){
        print(e)
        print("snodas download failed, proceeding to next date...")
        return(F)
      }
    )
    if(is.null(dlWorked)){dlWorked=T} #above trycatch returns null (from downloas.SNODAS()) if it worked.  Reassign as T
    
    ws_geoms=st_read(conn, query=paste0("SELECT outflowlocationid, geometry FROM watersheds WHERE outflowlocationid IN ('",
                                        paste(ws_ids, collapse= "', '"),"');"))
    
    if(dlWorked){# if the download worked, have a go with the processing and metric calculation.  If not, save the effort
      
      ## --- pull spatial geometry(s) from location ID and transform it to extent --- #
      
      #since we have downloaded the SNODAS data, calculate all metrics regardless of what was asked for:
      for(addMetric in metricDefinitions$metric){
        
        #simple way to match 'metric' arg to this whole function to a specific function for calculating that metric.  
        #For example, if metric = 'SWE_total', this will run sapply(ws_ids, extract_ws_swe, ws_geoms) , and return the result as SNODAS_values.
        #we need to add more functions for other  metrics of interest, and add the metrics to the check above (knownMetrics)
        SNODAS_values=switch(addMetric,
                             swe_total= sapply(ws_ids, extract_ws_swe, ws_geoms=ws_geoms, d=d),
                             runoff_total= sapply(ws_ids, extract_ws_runoff, ws_geoms=ws_geoms, d=d),
                             snow_covered_area= sapply(ws_ids, extract_ws_sca, ws_geoms=ws_geoms, d=d),
                             liquid_precip = sapply(ws_ids, extract_ws_precip, ws_geoms=ws_geoms, d=d)
        )
        
        
        #write new values to database
        dbWriteData(metric = addMetric, value = SNODAS_values, datetime = d, locationID = ws_ids, sourceName = "snodas", units = metricDefinitions$units[metricDefinitions$metric==addMetric],addMetric = T)
      }
    } else { #download did not work, write placeholder data to db
      if(Sys.Date()-d>7){  #only mark data as unavailable if it is more than a week 'old'.  Prevents marking data as qc=F when it may become available in the future
        for(addMetric in metricDefinitions$metric){
          
          SNODAS_fail_value=-999
          
          #write new values to database
          dbWriteData(metric = addMetric, value = SNODAS_fail_value, datetime = d, locationID = ws_ids, sourceName = "snodas", units = metricDefinitions$units[metricDefinitions$metric==addMetric],addMetric = T, qcStatus=F)
        }
      }
    }
    
    ###---------------- remove snodas source files directory ------------------###
    unlink("./SNODAS",recursive=T)
    
    #everything has been calculated an written to db. Return to the users dataframe
    #dbData=dbGetQuery(conn, paste0("SELECT metric, value, datetime, locationid, qcstatus FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",d,"' AND metric = '", metric,"';"))
    #dbData_bad=dbData[dbData$qcstatus=="FALSE",]
    #if(nrow(dbData_bad)>=1){
    #  print("Data with qcstatus=F not returned:")
    #  print(dbData_bad)
    #}
    #return(dbData[dbData$qcstatus=="TRUE",]) #return the same values that were written to db
  }
  
  ########################## get snodas process -----------------------------------
  
  if(rebuildAllMissingData){
    dbExecute(conn,"DELETE FROM data WHERE dataid IN (SELECT dataid FROM data LEFT JOIN batches ON data.batchid = batches.batchid WHERE batches.source = 'snodas' AND data.qcstatus = FALSE);")
  }
  dates=as.Date(dates)
  
  
  
  #known metrics - to throw an error if a metric is not understood
  if(!metric %in% allMetrics$metric){
    stop("metric ",metric," can not be interpreted  >grab_ws_snow( metric )")
  }
  
  
  
  #######main process - start by getting all relevant data:
  dataInDb=dbGetQuery(conn, paste0("SELECT metric, value, datetime::date, locationid, qcstatus FROM data WHERE metric = '",metric,
                                   "' AND datetime::date IN ('",paste(dates,collapse="', '"),"') AND locationid IN ('",
                                   paste(ws_ids,collapse="', '"),"');"))
  
  
  
  if(nrow(dataInDb) < length(dates)*length(ws_ids) ){ #if not all data is in database, identify days with incomplete data (days without all locationids)
    
    if(nrow(dataInDb)>=1){# at least some existing data found, grab days with complete data
      dateLocationCount=aggregate(dataInDb$locationid~dataInDb$datetime,FUN=length)  # get count of locations per day in existing data
      datesToRun=dates[!dates %in% dateLocationCount$`dataInDb$datetime`]   #identify days not in database at all
      datesToRun=c(datesToRun,dateLocationCount$`dataInDb$datetime`[dateLocationCount$`dataInDb$locationid`<length(ws_ids)])  #also identify days w/o all locations for each day
      existingData=dataInDb[dataInDb$datetime %in% dateLocationCount$`dataInDb$datetime`[dateLocationCount$`dataInDb$locationid`==length(ws_ids)],]  #data frame for all days with complete data (1 record for each day*locationid)
      
    } else{  # no existing data, define dates to run
      datesToRun=dates
      existingData=data.frame(metric=character(0),value=numeric(0),datetime=as.Date(character(0)),locationid=numeric(0)) #placeholder empty data frame
    }
    
    moreData=data.frame(metric=character(0),value=numeric(0),datetime=as.Date(character(0)),locationid=numeric(0)) #data frame for holding newly generated data
    
    datesToRun=datesToRun[order(datesToRun)]
    pb=txtProgressBar(max=length(datesToRun),style=3) 
    i=1
    
    for(d in as.list(datesToRun)){  #run snodas worker function for days without complete data
      print(d)
      grab_ws_snow_worker(ws_ids=ws_ids, d=d, metric=metric, metricDefinitions = allMetrics)
      setTxtProgressBar(pb,value=i)
      writeLines("")
      i=i+1
      gc()
    }
    
    
    # allData=rbind(existingData,moreData)
    # allData=allData[order(allData$datetime),]
    # 
    # allData_bad=allData[allData$qcstatus=="FALSE",]
    # 
    # if(nrow(allData_bad>=1)){
    #   print("Data with qcstatus=F not returned:")
    #   print(allData_bad)
    # }
    # return(allData[allData$qcstatus==TRUE,])
    
  } else { ##### all data was retrieved from db query, no need to call worker function
    
    # dataInDb_bad=dataInDb[dataInDb$qcstatus=="FALSE",]
    # if(nrow(dataInDb_bad>=1)){
    #   print("Data with qcstatus=F not returned:")
    #   print(dataInDb_bad)
    # }
    # 
    # return(dataInDb[dataInDb$qcstatus==TRUE,])
  }
  
}

###############-------------generalized update db function ------------------------------
updateDbData=function(metric,location,days,sourceName,rebuildInvalidData=F){ #location can be locationID or locationName
  
  #if rebuildInvalidData=T, functon will drop and attempt to overwrite invalid data
  
  days=as.Date(days)
  days=unique(days)
  
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
  if(!all(days %in% as.Date(dataInDb$datetime))){ #data for some days is not in database    
    missingDays=days[!days %in% as.Date(dataInDb$datetime)]
    #print(paste("adding data for missing days:",paste(missingDays, collapse=", ")))
    
    
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
      
      streamflowindb=dbGetQuery(conn,paste0("SELECT * FROM data WHERE locationid = '",locationID,"' AND metricid = '",metricID,
                                            "' AND ( datetime IN ('",paste(days,collapse="', '"),"') OR qcdetails = 'P' );"))
      
      
      days=unique(c(days, streamflowindb$datetime))
      days=days[order(days)]
      

      # Download data from this location
      streamflow <- readNWISdv(siteNumbers = thisLocation_sourceID, parameterCd = pCode, startDate = min(days), endDate = max(days) ) %>% renameNWISColumns() %>% data.frame
      
      dupDays=streamflowindb$datetime[duplicated(streamflowindb$datetime)]
      
      allStreamflow=merge(streamflow,streamflowindb,by.x="Date",by.y="datetime",all.x=T, all.y=T)
      
      allStreamflow$flowMatch=allStreamflow$Flow==allStreamflow$value
      
      problemDays=allStreamflow$Date[!allStreamflow$flowMatch]
      
      remDays=c(dupDays,problemDays)
      remDays=remDays[complete.cases(remDays)]
      
      if(length(remDays)>0){
        dbExecute(conn,paste0("DELETE FROM data WHERE locationid = '",locationID,"' AND metricid = '",metricID,
                              "' AND datetime IN ('",paste(remDays,collapse="', '"),"');")
        )
      }
      if(!all(days %in% streamflow$Date)){
        addDays=days[!days %in% streamflow$Date]
        streamflow=rbind(streamflow,data.frame(agency_cd="USGS",site_no=thisLocation_sourceID,Date=addDays,Flow=NA,Flow_cd="bad"))
      }
      
      streamflow$qcStatus=T
      streamflow$qcStatus[is.na(streamflow$Flow)]=F
      streamflow$Flow[streamflow$qcStatus==F]=-999
      
      
      #write to db
      dbWriteData(metric="flow",value=streamflow$Flow,datetime=streamflow$Date,locationID=locationID,sourceName="USGS",qcStatus=streamflow$qcStatus,qcDetails=streamflow$Flow_cd)
      
    }
    
    sourceSnotel=function(metricID, locationID, missingDays){
      #for debug:
      # metricID=6
      # locationID=c(170)
      
      
      #db knows internal snotel source location ids:
      thisLocation_sourceID=dbGetQuery(conn,paste0("SELECT source_site_id FROM locations WHERE locationid = '",locationID,"';"))$source_site_id
      
      snotel_data = snotel_download(thisLocation_sourceID, path = tempdir(), internal = TRUE)
      snotel_data=snotel_data[snotel_data$date %in% missingDays,]
      
      snotel_data = snotel_data[,c("date","snow_water_equivalent","temperature_mean")]
      
      
      
      #process and write swe
      snotel_data$sweQC=TRUE
      snotel_data$snow_water_equivalent[is.na(snotel_data$snow_water_equivalent)]=-999
      snotel_data$sweQC[snotel_data$snow_water_equivalent==-999]=FALSE
      dbWriteData(metric="swe",value=snotel_data$snow_water_equivalent,datetime=snotel_data$date,locationID=locationID,sourceName = "snotel",qcStatus = snotel_data$sweQC)
      
      #process and write mean daily temperature
      snotel_data$meanTQC=TRUE
      snotel_data$temperature_mean[is.na(snotel_data$temperature_mean)]=-999
      snotel_data$meanTQC[snotel_data$temperature_mean==-999]=FALSE
      dbWriteData(metric="mean daily temperature",value=snotel_data$temperature_mean,datetime=snotel_data$date,locationID=locationID,sourceName = "snotel",qcStatus = snotel_data$meanTQC)
      
    }
    
    sourceAgriMet=function(metricID, locationID, days){
      #for debug:
      # locationID=180 #picabo
      # metricID=15
      # days=seq.Date(as.Date("1992-01-01"),as.Date("1992-01-04"),by="day")
      # # 
      thisLocation_sourceID=dbGetQuery(conn,paste0("SELECT source_site_id FROM locations WHERE locationid = '",locationID,"';"))$source_site_id
      
      #p codes: OB = temperature; PI= precip incrimental; PC = precip cumulative; SI = Incremental solar radiation; SQ = cumulative solar radiation
      am_data=getAgriMet.data(site_id=thisLocation_sourceID, timescale="hourly", DayBgn = as.Date(min(days)), DayEnd = as.Date(max(days)), pCodes=c("OB"))
      obCol=grep("OB$",names(am_data))
      print(paste("renaming",names(am_data)[obCol],"to: OB"))
      names(am_data)[obCol]="OB"
      
      am_data$datetime=as.POSIXct(am_data$`DATE       TIME`,format="%m/%d/%Y %H:%M")
      am_data$airT=as.numeric(am_data$OB)
      
      #drop na records
      am_data=am_data[complete.cases(am_data),]
      
      stillMissingDays=days[!days %in% as.Date(am_data$datetime)]
      #print(stillMissingDays)
      if(length(stillMissingDays>0)){ ## add placeholder invalid data for missing days
        am_data=rbind(am_data,data.frame(`DATE       TIME`="", OB="", datetime=stillMissingDays,airT=-999,check.names = F))
      }
      am_data$airTQC=T
      am_data$airT[is.na(am_data$airT)]=-999
      am_data$airT[am_data$airT< -33]=-999
      am_data$airTQC[am_data$airT== -999]=F
      
      
      dbWriteData(metric = "air temperature", value=am_data$airT,datetime=am_data$datetime,locationID=locationID,sourceName="AgriMet",qcStatus=am_data$airTQC)
      
    }
    
    #call appropriate source function:
    if(sourceName == "USGS"){
      sourceUSGS(metricID,locationID,missingDays)
    }
    if(sourceName == "snotel"){
      if( (Sys.Date()-max(missingDays)<=1) | rebuildInvalidData ){ # current data is not here - run rebuild function (or, if rebuild invalid data, run anyway)
        sourceSnotel(metricID,locationID, missingDays) 
      }
    }
    if(sourceName=="AgriMet"){
      sourceAgriMet(metricID, locationID, missingDays)
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



#############---------------update snodas data------------------------
#first call will build all metrics, other calls should be unnecessary (but are quick)
update_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="snow_covered_area")
update_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="liquid_precip")
update_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="swe_total")
update_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="runoff_total")




##########-----------update streamflow data ------------------
updateDbData(metric="flow", location="BIG WOOD RIVER AT HAILEY", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS")
updateDbData(metric="flow", location="BIG WOOD RIVER AT STANTON CROSSING", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS")
updateDbData(metric="flow", location="SILVER CREEK AT SPORTSMAN ACCESS", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS")
updateDbData(metric="flow", location="CAMAS CREEK NR BLAINE ID", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS")


###########------------------update snotel data-----------------
#snotel data getter function writes SWE and mean daily temperature, no need to call for both
#date does not matter for snotel_download function
updateDbData(metric="swe", location="chocolate gulch", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="galena", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="galena summit", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="hyndman", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="lost-wood divide", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="dollarhide summit", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="camas creek divide", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="soldier r.s.", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="garfield r.s.", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="swede peak", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="stickney mill", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")
updateDbData(metric="swe", location="bear canyon", days=seq.Date(as.Date("2000-01-01"),Sys.Date(),by="day"),sourceName="snotel")

#dbGetQuery(conn,"SELECT data.locationid, locations.name, min(datetime) FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE metric = 'swe' GROUP BY data.locationid, locations.name;")


###########-----------update AgriMet data------------------
#dbGetQuery(conn, "SELECT DISTINCT metric FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE locations.locationid=181;")

updateDbData(metric="air temperature", location="Picabo AgriMet station", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="AgriMet")
updateDbData(metric="air temperature", location="Fairfield AgriMet station", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="AgriMet")
#dbGetQuery(conn,"SELECT data.locationid, locations.name, min(datetime) FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE metric = 'air temperature' GROUP BY data.locationid, locations.name;")

######################-------------------- update db materialized views

# dbExecute(conn, "CREATE MATERIALIZED VIEW locationattributes AS SELECT ROW_NUMBER() OVER(), met.locationid, met.name, met.locationgeometry, met.metrics, wsh.wshedareakm, wsh.watershedgeometry FROM
#                       (SELECT DISTINCT locations.name, locations.locationid, locations.geometry AS locationgeometry, STRING_AGG(DISTINCT(data.metric), ',') AS metrics FROM locations LEFT JOIN data ON locations.locationid = data.locationid GROUP BY locations.locationid) met
#                      LEFT JOIN 
#                       (SELECT locations.locationid, watersheds.geometry AS watershedgeometry, ST_AREA(watersheds.geometry)/1000000 AS wshedareakm FROM locations LEFT JOIN watersheds ON locations.locationid = watersheds.outflowlocationid) wsh
#                       ON met.locationid = wsh.locationid;")

dbExecute(conn,"REFRESH MATERIALIZED VIEW locationattributes;")

# 6 1982-06-01 05:00:00 49.10    picabo     6 1982 1982

# dbExecute(conn,"CREATE MATERIALIZED VIEW daily_air_temperature AS SELECT datetime::date AS date, AVG(value) AS day_mean_t, locations.sitenote as site_name, EXTRACT(MONTH FROM datetime) AS month, EXTRACT(YEAR FROM datetime) as y, wateryear(datetime) as wy
#            FROM data LEFT JOIN locations ON data.locationid = locations.locationid 
#            WHERE metric = 'air temperature' AND qcstatus = 'true'
#            GROUP BY date, site_name, month, y, wy;")


dbExecute(conn,"REFRESH MATERIALIZED VIEW daily_air_temperature;")



# dbExecute(conn, "CREATE MATERIALIZED VIEW snodasdata AS SELECT data.metric, data.value, data.datetime, 
#           data.metricid, data.locationid, data.qcstatus, data.qcdetails
#           FROM data LEFT JOIN batches ON data.batchid = batches.batchid WHERE batches.source = 'snodas' AND qcstatus=TRUE;")
# 

dbExecute(conn, "REFRESH MATERIALIZED VIEW snodasdata")

# dbExecute(conn,"CREATE OR REPLACE FUNCTION wateryear(datetime timestamp without time zone) RETURNS integer AS $$
#               SELECT CASE WHEN ( EXTRACT(month FROM datetime)) >= 10  THEN EXTRACT(year FROM datetime) +1 
#                                       ELSE EXTRACT(year FROM datetime) 
#                         END
#                 
#                 $$
#             LANGUAGE SQL;")






