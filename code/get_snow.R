# Download Clip and Process SNODAS data
# March 09, 2023
# Sam Carlson, Kendra Kaiser

# load libaries
devtools::install_github('marinosr/SNODASR')

library(SNODASR)
library(curl)
library(terra)
library(R.utils)

source("~/github/WRWC/code/init_db.R") # /Documents

boundingbox <- dbGetQuery(conn, "SELECT ST_EXTENT(watersheds.geometry) FROM watersheds;") 

%>% st_transform(crs=st_crs(4326))

#download by largest extent, then use extract w diff watersheds
grab_ws_snow = function(ws_id, date, param, metric){ #need to make work for both single date and sequence
  
  #look in db to see if the metric exists
    #return metric
  
  #if doesn't exist in database pull all data for that date / date range & calculate metrics
  
  ##--------------------------------------------------------------------------#
  ## --- Download the full SNODAS dataset
  download.SNODAS(date)

  ## --- pull spatial geometry from location ID and transform it to extent --- #
  ws_ids = c(140,161)#c()
  ws_geoms=st_read(conn, query=paste0("SELECT watershedgeometry FROM locationattributes WHERE locationid IN ('",
                                      paste(ws_ids, collapse= "', '"),"');"))
  
  # geometries of sub watersheds to use to extract metrics of interest
  ws_geoms_tr= st_transform(ws_geoms, crs=st_crs(4326))
  # create full extent of combined watershed area
  ws_extent=matrix(st_bbox(ws_geom_tr), nrow=2)

  ## --- extract all SNODAS values wanted 
  out_img<-extract.SNODAS.subset(date, values_wanted=c('SWE', 'Runoff'), extent=ws_extent, write_file = FALSE) 
  
  #convert images to rasters -- NEED TO MODIFY TO MULT RASTERS
  out_img=rast(out_img[[1]])
  
  ## --- extract values from all relevant parameters and modify into metrics ---#
  out_swe<-terra::extract(out_img, ws_geoms_tr) ## MODIFY for stack of rasters
  tot_swe<-c(date_ts, sum(out_swe$X2023.03.16)) ## this needs to be modified
  
  #write new metrics to database
  #return metric of interest
  
  return(tot_swe) #consider returning out_img for data vis
}

# Run functions to download and extract relevant data

date_seq<-seq(as.Date("2021-10-01"), as.Date("2022-09-30"), by= "day")
full_extent = grab_geom(161)
date = as.Date("2023-04-11")

out<- grab_ws_snow(full_extent, date_seq, param ='SWE')

plot(out_img)
plotfull_extent, add=TRUE)



