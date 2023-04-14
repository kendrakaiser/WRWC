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

#to run on sam's coimputer:
source("~/Documents/R Workspace/snodasr/SNODASR_functions.R")


#source("~/github/SNODASR-main/R/extract.SNODAS.subset.R")
#source("~/github/SNODASR-main/R/download.SNODAS.R")
#library(SNODASR)



#df of snodas-derived metric names and units
snodasMetrics=data.frame(metric=c("SWE_total"),
                         units=c("cubic meters")
)
#each new derived metric must be defined here, and must have a function created to calculate it
#the function is then matched to the metric in the 'switch' statement in the main function


# I added some more functions for easy writing to the db:
source(paste0(getwd(),"/code/dbIntakeTools.R"))
conn=scdbConnect()
#^this^ replaces init_db, but it does pollute the workspace with too many functions.
# we can hide them if we want as follows:
.ghost = function(){return("boo")}
#note that there is no .ghost function in the workspace, but i t still works
.ghost()


#source("~/github/WRWC/code/init_db.R") # /Documents


extract_ws_swe <- function(ws_id, ws_geoms, date){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='SWE', extent=ws_extent, write_file = FALSE) 
  #convert images to raster
  out_rast=rast(out_img[[1]])
  
  #is there another function to extract data by elevation band??
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_swe<-terra::extract(out_rast, vect(ws_geom_tr))
  #hist(out_swe[,2])
  tot_swe<-sum(out_swe[,2]) ## this needs to be modified
  
  #From a quick look at snodas user guide, I believe SWE is in mm.  Might as well convert to a reasonable unit to stuff in the db
  
  tot_swe_m=tot_swe/1000 # convert to meters
  
  # raster pixels are aproxxamitly 929*673, or more precisley, 625129 meters^2 per pixel on average
  
  tot_swe_m3=tot_swe_m*625129
  
  return(tot_swe_m3)
}

# write functions for other metrics of interest


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
  
  
  
  
  
  #since we have downloaded the SNODAS data, calulate all metrics regardless of what was asked for:
  for(addMetric in metricDefinitions$metric){
    
    #simple way to match 'metric' arg to this whole function to a specific function for calculating that metric.  
    #For example, if metric = 'SWE_total', this will run sapply(ws_ids, extract_ws_swe, ws_geoms) , and return the result as SNODAS_values.
    #however, if metric = 'Poodle_density', it will run calculatePoodleDensity()
    #we need to add more functions for other  metrics of interest, and add the metrics to the check above (knownMetrics)
    SNODAS_values=switch(addMetric,
                         SWE_total=sapply(ws_ids, extract_ws_swe, ws_geoms=ws_geoms, date=date)#,
                         #Poodle_density=calculatePoodleDensity()
                         )
    
    
    #write new values to database
    dbWriteData(metric = addMetric, value = SNODAS_values, datetime = date, locationID = ws_ids, sourceName = "snodas", units = metricDefinitions$units[metricDefinitions$metric==addMetric],addMetric = T)
  }
  
  ########################remove snodas source files directory
  ################ this is consistent with the default in snodasr functions
  unlink("./SNODAS",recursive=T)
  
  #everything has been calculated an written to db.  Return to the users  dataframe to return
  dbData=dbGetQuery(conn, paste0("SELECT metric, value, datetime, locationid FROM data WHERE data.locationid IN ('",paste(ws_ids,collapse="', '"),"') AND datetime::date = '",date,"' AND metric = '",addMetric,"';"))
  return(dbData) #return the same values that were written to db
}


#I have run it for these dates, so they will return a value quickly (no snodasing needed).  
#Change a date and it will have to dl snodas data and process it before returning a value
#functions should behave the same either way


#simple test:
swe_totals=grab_ws_snow(ws_ids = 140, date=as.Date("2023-01-12"),metric="SWE_total")


#multiple locations
swe_totals=grab_ws_snow(ws_ids = c(140,167), date=as.Date("2023-01-15"),metric="SWE_total")

#grab_ws_snow does not work across dates, but we can make a wrapper for it, or just apply:
date_seq<-seq(as.Date("2021-10-01"), as.Date("2021-10-11"), by= "day")
swe_totals=do.call(rbind,lapply(date_seq,grab_ws_snow,ws_ids=c(140,167),metric="SWE_total"))
