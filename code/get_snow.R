# Download Clip and Process SNODAS data
# March 09, 2023
# Sam Carlson, Kendra Kaiser

# load libaries

source("~/github/SNODASR-main/R/extract.SNODAS.subset.R")
source("~/github/SNODASR-main/R/download.SNODAS.R")

library(SNODASR)
library(curl)
library(terra)
library(R.utils)

source("~/github/WRWC/code/init_db.R") # /Documents

boundingbox <- dbGetQuery(conn, "SELECT ST_EXTENT(watersheds.geometry) FROM watersheds;") 

extract_ws_swe <- function(ws_id, ws_geoms){ 
  # geometries of sub watershed to use to extract metrics of interest
  ws_geom_tr= st_transform(ws_geoms[ws_geoms$outflowlocationid == ws_id,], crs=st_crs(4326))
  ws_extent = matrix(st_bbox(ws_geom_tr), nrow=2)
  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted='SWE', extent=ws_extent, write_file = FALSE) 
  #convert images to raster
  out_rast=rast(out_img[[1]])
  
  #is there another function to extract data by elevation band??
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_swe<-terra::extract(out_rast, ws_geom_tr)
  hist(out_swe[,2])
  tot_swe<-sum(out_swe[,2]) ## this needs to be modified
  
  return(tot_swe)
}

# insert in other metrics of interest

#download by largest extent, then use extract w diff watersheds
grab_ws_snow = function(ws_id, date, param, metric){ #need to make work for both single date and sequence
  
  #look in db to see if the metric exists
    #return metric
  
  #if doesn't exist in database pull all data for that date / date range & calculate metrics
  
  ##--------------------------------------------------------------------------#
  ## --- Download the full SNODAS dataset
  download.SNODAS(date)

  ## --- pull spatial geometry from location ID and transform it to extent --- #
  ws_ids = c(140,167)#c()
  ws_geoms=st_read(conn, query=paste0("SELECT outflowlocationid, geometry FROM watersheds WHERE outflowlocationid IN ('",
                                      paste(ws_ids, collapse= "', '"),"');"))
 
  ws_tot_swe<-sapply(ws_ids, extract_ws_swe, ws_geoms)
  
  #write new metrics to database
  #return metric of interest
  
  return(tot_swe) #consider returning out_img for data vis
}

# Run functions to download and extract relevant data

date_seq<-seq(as.Date("2021-10-01"), as.Date("2022-09-30"), by= "day")
full_extent = grab_geom(161)
date = as.Date("2023-04-11")
param='SWE'

out<- grab_ws_snow(full_extent, date_seq, param ='SWE')

plot(out_img)
#plotfull_extent, add=TRUE)



