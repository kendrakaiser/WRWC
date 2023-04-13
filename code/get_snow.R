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

# create function to pull ws geometry from location ID 
hai_id=140
ws_id =161

# function to pull spatial data
grab_geom = function(ws_id){
  ws_geom=st_read(conn, query=paste0("SELECT watershedgeometry FROM locationattributes WHERE locationid=",
                                   ws_id, ";"))
  return(ws_geom)
}

baseline_geom= grab_geom(161)
baseline_geomt=st_transform(baseline_geom, crs=st_crs(4326))
plot(baseline_geomt)

date_seq<-seq(as.Date("2022-10-01"), as.Date("2023-09-30"), by= "day")

#download by largest extent, then use extract w diff watersheds
grab_ws_swe = function(ws_geom_trs, date_seq){ #need to make work for both single date and sequence
  
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
  
  return(tot_swe) #consider returning out_img for data vis
}

yr_out<- grab_ws_swe(baseline_geomt, date_seq)

plot(out_img)
plot(baseline_geomt, add=TRUE)



