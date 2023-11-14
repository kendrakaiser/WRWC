# Download Clip and Process SNODAS data
# March 09, 2023
# Sam Carlson, Kendra Kaiser

# load libaries
library(curl)
library(terra)
library(R.utils)
library(raster)
library(sf)
library(terra)
library(RPostgres)

# what is this script doing that isn't being done elsewhere now?
# kek think about other snodas metrics to calculate; confirm IDs of relevant pourpoints; 

#to run on sam's computer:
#git_dir=getwd()

source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) #tools to connect and write to database
source(paste0(git_dir,"/code/fxn_SNODASR_functions.R")) 
conn=scdbConnect() #connect to database

#all flow locations
dbGetQuery(conn, "SELECT distinct data.locationid, name FROM data left join locations on data.locationid = locations.locationid WHERE metric = 'flow'")


#data frame of snodas-derived metric names and units
# add additional metrics here
snodasMetrics=data.frame(metric=c("swe_total", "runoff_total", "snow_covered_area", "liquid_precip"),
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
# average snow pack temperature; parameter is not included in files prior to 21 February 2004.
# extract_ws_snowT <- function(ws_id, ws_geoms, d){ 
#   # geometries of sub watershed to use to extract metrics of interest
#   ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
#   ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
#   ## --- extract all SNODAS values wanted 
#   out_img<-extract.SNODAS.subset(d, values_wanted='T_Mean', extent=ws_extent, write_file = FALSE) 
#   #convert images to raster
#   out_rast=rast(out_img[[1]])
#   
#   ## --- extract values from all relevant parameters and modify into metrics ---#
#   out_snow_temp<-terra::extract(out_rast, vect(ws_geom_tr))
#   #hist(out_swe[,2])
#   avg_snow_temp<-mean(out_snow_temp[,2], na.rm=TRUE) 
#   
#   # convert from Kelvin to C
#   avg_snow_tempC= avg_snow_temp - 273.15
#   
#   # add error catch to make sure there is data in here -- doesnt work yet
#   #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
#   #  , error = function(e) {message('Dataframe is EMPTY')
#   #    print(e)})
#   return(avg_snow_tempC)
# }


#######worker function for doing actual snodasing
grab_ws_snow_worker = function(ws_ids, d, metric, metricDefinitions=allMetrics){
  
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
  dbData=dbGetQuery(conn, paste0("SELECT metric, value, datetime, locationid, qcstatus FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",d,"' AND metric = '", metric,"';"))
  dbData_bad=dbData[dbData$qcstatus=="FALSE",]
  if(nrow(dbData_bad)>=1){
    print("Data with qcstatus=F not returned:")
    print(dbData_bad)
  }
  return(dbData[dbData$qcstatus=="TRUE",]) #return the same values that were written to db
}

### ---------------- Grab and/or Process SNODAS Data ------------------------ ###
# Wrapper function that pulls data from db if it exists or downloads and processes SNODAS data

grab_ws_snow=function(ws_ids, dates, metric, allMetrics=snodasMetrics, rebuildAllMissingData=F){
  
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
      moreData=rbind(moreData,grab_ws_snow_worker(ws_ids=ws_ids, d=d, metric=metric, metricDefinitions = allMetrics))
      setTxtProgressBar(pb,value=i)
      writeLines("")
      i=i+1
      gc()
    }
    
    
    allData=rbind(existingData,moreData)
    allData=allData[order(allData$datetime),]
    
    allData_bad=allData[allData$qcstatus=="FALSE",]
    
    if(nrow(allData_bad>=1)){
      print("Data with qcstatus=F not returned:")
      print(allData_bad)
    }
    return(allData[allData$qcstatus==TRUE,])
    
  } else { ##### all data was retrieved from db query, no need to call worker function
    
    dataInDb_bad=dataInDb[dataInDb$qcstatus=="FALSE",]
    if(nrow(dataInDb_bad>=1)){
      print("Data with qcstatus=F not returned:")
      print(dataInDb_bad)
    }
    
    return(dataInDb[dataInDb$qcstatus==TRUE,])
  }
  
}


#simple test:
#grab_ws_snow(ws_ids = 140, dates=as.Date("2023-08-12"),metric="runoff_total")
#date range
#rt=grab_ws_snow(ws_ids = 140, dates=seq.Date(from=as.Date("2021-01-01"),to=as.Date("2021-07-01"),by="day"),metric="runoff_total")

#multiple locations
#grab_ws_snow(ws_ids = c(167,144), dates=as.Date("2023-01-1"),metric="runoff_total")

#day seq
#grab_ws_snow(ws_ids = 140, dates=seq.Date(from=as.Date("2021-08-01"),to=as.Date("2021-08-05"),by="day"),metric="runoff_total")

#day w/ missing data:
#grab_ws_snow(ws_ids = 140, dates=as.Date("1980-08-04"),metric="runoff_total")

#The masked files span 30 September 2003 to the present, and the unmasked files span 09
#December 2009 to the present at a daily resolution

date_seq=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day")
grab_ws_snow(ws_ids=c(140,167,144,141),dates=date_seq,metric="runoff_total")

grab_ws_snow(ws_ids=c(140,167,144,141),dates=as.Date("2005-08-02"),metric="runoff_total")


#slow query, but should return all snodas-sourced data
#allSnodasData=dbGetQuery(conn,"SELECT * FROM data LEFT JOIN batches ON data.batchid = batches.batchid WHERE batches.source = 'snodas';")
#unique(allSnodasData$datetime)

#save locally for data exploration, likely remove later
#write.csv(allSnodasData, file.path(data_dir, 'allSnodasData.csv'), row.names=FALSE)
