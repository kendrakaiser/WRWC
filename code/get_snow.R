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

# function to pull spatial geometry from location ID and transform it 
grab_geom = function(ws_id){
  ws_geom=st_read(conn, query=paste0("SELECT watershedgeometry FROM locationattributes WHERE locationid=",
                                   ws_id, ";"))
  ws_geom_tr= st_transform(ws_geom, crs=st_crs(4326))
  return(ws_geom_tr)
}

#download by largest extent, then use extract w diff watersheds
grab_ws_snow = function(ws_geom_trs, date_seq, param){ #need to make work for both single date and sequence
  
  #look in db to see if the metric exists
    #return values
  
  #if doesnt exist in database pull all data for that date / date range & calculate metrics
  
  
  # pull extent of watershed
  extent=matrix(st_bbox(ws_geom_trs), nrow=2)
  
  ## Download the SNODAS dataset
  download.SNODAS(date_seq)
  ## extract SNODAS
  out_img<-extract.SNODAS.subset(date_seq, values_wanted='SWE', extent=baseline_extent, write_file = FALSE) 
  
  #convert image to raster
  out_img=rast(out_img[[1]])
  
  #extract values
  out_swe<-terra::extract(out_img, baseline_geomt)
  tot_swe<-c(date_ts, sum(out_swe$X2023.03.16)) ## this needs to be modified
  
  #write new metrics to database
  #return metric of interest
  
  return(tot_swe) #consider returning out_img for data vis
}

# Run functions to download and extract relevant data

date_seq<-seq(as.Date("2021-10-01"), as.Date("2022-09-30"), by= "day")
full_extent = grab_geom(161)

out<- grab_ws_snow(full_extent, date_seq, param ='SWE')

plot(out_img)
plotfull_extent, add=TRUE)



