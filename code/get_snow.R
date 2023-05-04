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

# kek think about other snodas metrics to calculate; confirm IDs of relevant pourpoints; 

#to run on sam's computer:
#source("~/Documents/R Workspace/snodasr/SNODASR_functions.R")


#source("~/github/SNODASR-main/R/extract.SNODAS.subset.R")
#source("~/github/SNODASR-main/R/download.SNODAS.R")
source(paste0(git_dir,"/code/dbIntakeTools.R")) #tools to connect and write to database
conn=scdbConnect() #connect to database


#data frame of snodas-derived metric names and units
# add additional metrics here
snodasMetrics=data.frame(metric=c("swe_total", "runoff_total", "snow_temp_avg", "snow_covered_area", "liguid_precip"),
                         units=c("meters", "meters", "celcius", "km2", "mm")
)

#### ------------ Define Metrics and associated functions ------------------ ###
# each new derived metric must be defined here, and must have a function created to calculate it
# the function is then matched to the metric in the 'switch' statement in the main function

# Extract swe (m) from extent
extract_ws_swe <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='SWE', extent=ws_extent, write_file = FALSE) 
  #convert images to raster
  out_rast=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_swe<-terra::extract(out_rast, vect(ws_geom_tr))
  #hist(out_swe[,2])
  tot_swe<-sum(out_swe[,2])
  # convert to meters using scale factor from user guide
  tot_swe_m=tot_swe/1000 

  # add error catch to make sure there is data in here
  return(tot_swe_m3)
}
#Extract total 24 hour melt (m)
extract_ws_runoff <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='Runoff', extent=ws_extent, write_file = FALSE) 
  #convert images to raster
  out_rast=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_runoff<-terra::extract(out_rast, vect(ws_geom_tr))
  #hist(out_swe[,2])
  tot_runoff<-sum(out_runoff[,2]) 
  
  # convert integer to meters based on scale factor
  tot_runoff_m=tot_runoff/100000 
  
  # add error catch to make sure there is data in here -- doesnt work yet
  #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
          #  , error = function(e) {message('Dataframe is EMPTY')
          #    print(e)})
  return(tot_runoff_m3)
}
# average snow pack temperature; parameter is not included in files prior to 21 February 2004.
extract_ws_snowT <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='T_Mean', extent=ws_extent, write_file = FALSE) 
  #convert images to raster
  out_rast=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_snow_temp<-terra::extract(out_rast, vect(ws_geom_tr))
  #hist(out_swe[,2])
  avg_snow_temp<-mean(out_snow_temp[,2], na.rm=TRUE) 
  
  # convert from Kelvin to C
  avg_snow_tempC= avg_snow_temp - 273.15
  
  # add error catch to make sure there is data in here -- doesnt work yet
  #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
  #  , error = function(e) {message('Dataframe is EMPTY')
  #    print(e)})
  return(avg_snow_tempC)
}
# snow covered area (km2)
extract_ws_sca <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='Depth', extent=ws_extent, write_file = FALSE) 
  #convert images to raster 
  out_rast=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  # extract and divide by scale factor to get into meters
  out_depth<-terra::extract(out_rast, vect(ws_geom_tr))/1000
  # ID which pixels have more than 0.15m of snow and sum
  sca = sum(out_depth[,2] > 0.15)
  
  # add error catch to make sure there is data in here -- doesnt work yet
  #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
  #  , error = function(e) {message('Dataframe is EMPTY')
  #    print(e)})
  return(sca)
}
# liquid precip, 24hr total (mm)
extract_ws_precip <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='P_Liquid', extent=ws_extent, write_file = FALSE) 
  #convert images to raster 
  out_rast=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  # extract and divide by scale factor to get into meters
  out_p<-terra::extract(out_rast, vect(ws_geom_tr))
  # convert to kg/m2 (mm) using scale factor from user guide
  tot_p<-sum(out_p[,2])/10 

  # add error catch to make sure there is data in here -- doesnt work yet
  #tryCatch( {if(nrow(tot_runoff_m3) == 0)}
  #  , error = function(e) {message('Dataframe is EMPTY')
  #    print(e)})
  return(tot_p)
}

### ---------------- Grab and/or Process SNODAS Data ------------------------ ###
# Wrapper function that pulls data from db if it exists or downloads and processes SNODAS data

grab_ws_snow = function(ws_ids, date, metric, metricDefinitions=snodasMetrics){
  
  ##for debug:
  #ws_ids = c(140,167)
  #date=as.Date("2023-04-03")
  #metric="SWE_total"
  
  #known metrics - to throw an error if a metric is not understood
 
  if(!metric %in% metricDefinitions$metric){
    stop("metric ",metric," can not be interpreted  >grab_ws_snow( metric )")
  }
  
  #look in db to see if the data exists, return it if it does:
  dbData=dbGetQuery(conn, paste0("SELECT * FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",date,"' AND metric = '",metric,"';"))
  if(all(ws_ids %in% dbData$locationid)){
    return(dbData[,c("metric","value","datetime","locationid")])
  }
  #if doesn't exist in database pull all data for that date / date range & calculate metrics
  
  ##--------------------------------------------------------------------------#
  ## --- Download the full SNODAS dataset
  download.SNODAS(date, parallel = T, overwrite=T)
  
  ## --- pull spatial geometry(s) from location ID and transform it to extent --- #
  ws_geoms=st_read(conn, query=paste0("SELECT outflowlocationid, geometry FROM watersheds WHERE outflowlocationid IN ('",
                                      paste(ws_ids, collapse= "', '"),"');"))
  
  
  #since we have downloaded the SNODAS data, calculate all metrics regardless of what was asked for:
  for(addMetric in metricDefinitions$metric){
    
    #simple way to match 'metric' arg to this whole function to a specific function for calculating that metric.  
    #For example, if metric = 'SWE_total', this will run sapply(ws_ids, extract_ws_swe, ws_geoms) , and return the result as SNODAS_values.
    #we need to add more functions for other  metrics of interest, and add the metrics to the check above (knownMetrics)
    SNODAS_values=switch(addMetric,
                         SWE_total=sapply(ws_ids, extract_ws_swe, ws_geoms=ws_geoms, date=date)#,
                         # insert other metric e.g. temp=sapply(ws_ids, extract_ws_temp, ws_geoms=ws_geoms, date=date)
                         )
    
    
    #write new values to database
    dbWriteData(metric = addMetric, value = SNODAS_values, datetime = date, locationID = ws_ids, sourceName = "snodas", units = metricDefinitions$units[metricDefinitions$metric==addMetric],addMetric = T)
  }
  
  ###---------------- remove snodas source files directory ------------------###
  unlink("./SNODAS",recursive=T)
  
  #everything has been calculated an written to db. Return to the users dataframe
  dbData=dbGetQuery(conn, paste0("SELECT metric, value, datetime, locationid FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",date,"' AND metric = '",addMetric,"';"))
  return(dbData) #return the same values that were written to db
}


#I have run it for these dates, so they will return a value quickly (no snodasing needed).  
#Change a date and it will have to dl snodas data and process it before returning a value
#functions should behave the same either way


#simple test:
#swe_totals=grab_ws_snow(ws_ids = 140, date=as.Date("2023-04-12"),metric="SWE_total")


#multiple locations
#swe_totals=grab_ws_snow(ws_ids = c(140,167), date=as.Date("2023-04-15"),metric="SWE_total")

#grab_ws_snow does not work across dates, but we can make a wrapper for it, or just apply:
#date_seq<-seq(as.Date("2021-10-01"), as.Date("2021-10-11"), by= "day")
#swe_totals=do.call(rbind,lapply(date_seq,grab_ws_snow,ws_ids=c(140,167),metric="SWE_total"))
