# Download Clip and Process SNODAS data
# March 09, 2023
# Sam Carlson, Kendra Kaiser

# load libaries
devtools::install_github('marinosr/SNODASR')

library(SNODASR)
library(curl)
library(terra)
library(R.utils)

source("~/Documents/github/WRWC/code/init_db.R")

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
plot(baseline_geom)

# pull extent of watershed
baseline_geomt=st_transform(baseline_geom, crs=st_crs(4326))
baseline_extent=matrix(st_bbox(baseline_geomt), nrow=2)

download.SNODAS(as.Date("2023-03-16"))
## extract SNODAS
out_img<-extract.SNODAS.subset(as.Date("2023-03-16"), values_wanted='SWE', extent=baseline_extent, write_file = FALSE) 

out_img=rast(out_img[[1]])
plot(out_img)
plot(baseline_geomt, add=TRUE)

#extract values
out_swe<-terra::extract(out_img, baseline_geomt)
sum(out_swe$X2023.03.16)

