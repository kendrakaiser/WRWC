# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June, 19, 2020
# ------------------------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)
library(snotelr)
library(XML)
library(httr)
library(dplyr)
library(devtools)
devtools::install_github(repo = "rhlee12/RNRCS", subdir = "/RNRCS/", force =TRUE)
library(RNRCS)
library(plyr)
library(readr)
library(lubridate)
library(lfstat)

# set data directory for saving data
cd ='~/Desktop/Data/WRWC'
# set date for AgriMet Data download
end = '2020-09-15'

# ------------------------------------------------------------------------------
# USGS Gages
bwb = 13139510  #  Bullion Bridge, Big Wood at Hailey
bws = 13140800  #  Stanton Crossing, Big Wood
cc  = 13141500  #  Camas Creek near Blaine
sc  = 13150430  #  Silver Creek near Picabo
bwr = 13142500  #  Big Wood below Magic near Richfield
mr  = 13142000  #  Magic Reservoir storage in acre - feet, impt for carry-over
bwbr = 13140335 # Big Wood at S Broadford Bridge Nr Bellevue, data only goes back to 2017
bwk = 13135500  #  Big Wood nr Ketchum, goes to 2011
usgs_sites = c(bwb, bws, cc, sc, bwr) #  put all sites in one vector

pCode = "00060" # USGS code for streamflow
sCode = "00054" # USGS code for reservoir storage (acre-feet)
# Dataframe with information about sites and period of record
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') # uv is an alias for instantaneous values
res_info <- whatNWISdata(sites= mr, parameterCd = sCode)

# Merge data from all sites into one dataframe
streamflow_data <- readNWISuv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = site_info$begin_date, endDate = site_info$end_date) %>% renameNWISColumns() %>% data.frame

res_data <- readNWISuv(siteNumbers = mr, parameterCd = sCode, startDate = res_info$begin_date[2], endDate = res_info$end_date[2]) %>% renameNWISColumns() %>% data.frame

# Save flow data as a csv
write.csv(streamflow_data, file.path(cd,'streamflow_data.csv'))
write.csv(site_info, file.path(cd,'usgs_sites.csv'))

#save a figure that shows YTD streamflow over WY average and CV

# Retrieve Snotel data ----------------------------------------------------

# SNOTEL Sites ----
cg = 895 #  Chocolate Gulch (0301)
g  = 489 #  Galena (0101)
gs = 490 #  Galena Summit (0101)
hc = 537 #  Hyndman Creek (0402)
lwd= 601 #  Lost-Wood Divide (Trail Creek 0304)
ds = 450 #  Dollarhide Summit (Placer/Warm Springs Creek 0201)
cd = 382 #  Camas Creek Divide (Sheep/Camas Creek 0101)
sr = 769 #  Soldier R.S (Upper Soldier Creek 0303)
ga = 492 #  Garfield R.S. (Upper Muldoon Creek 0301)
sp = 805 #  Swede Peak (Upper Muldoon Creek 0301)
snotel_sites = c(cg, g, gs, hc, lwd, ds, cd, sr, ga, sp)
snotel_abrv <- c("cg", "g", "gs", "hc", "lwd", "ds", "cd", "sr", "ga", "sp")

# Download snotel data ----
snotel_data = snotel_download(snotel_sites, path = '~/Desktop/Data/WRWC/', internal = TRUE )

# Pull out snotel site information
snotel_site_info<-data.frame('id'=NA, 'start'=NA, 'end'=NA, 'lat'=NA, 'long'=NA, 'elev'=NA, 'description'=NA, 'site_name'=NA, 'huc8'=NA)
snotel_site_info$start <-as.Date(NA)
snotel_site_info$end <-as.Date(NA)
for (i in 1:length(snotel_sites)){
  snotel_site_info[i,'id'] <- snotel_sites[i] 
  snotel_site_info[i,'start'] <- snotel_data$start[snotel_data$site_id == snotel_sites[i]][1]
  snotel_site_info[i,'end'] <- snotel_data$end[snotel_data$site_id == snotel_sites[i]][1]
  snotel_site_info[i,'lat'] <- snotel_data$latitude[snotel_data$site_id == snotel_sites[i]][1]
  snotel_site_info[i,'long'] <- snotel_data$longitude[snotel_data$site_id == snotel_sites[i]][1]
  snotel_site_info[i,'elev'] <- snotel_data$elev[snotel_data$site_id == snotel_sites[i]][1]
  snotel_site_info[i,'description'] <- snotel_data$description[snotel_data$site_id == snotel_sites[i]][1]
}
snotel_site_info$site_name <- c('chocolate gulch', 'galena', 'galena summit', 'hyndman creek', 'lost wood divide', 'dollarhide summit', 'camas creek', 'soldier', 'garfield', 'swede peak')
snotel_site_info$abv<- snotel_abrv
snotel_site_info$huc8 <- c(219,219,219,219,219,219,220,220,221,221)
# remove unecessary columns from snotel data frame
snotel_data_out = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))

snotel_data_out$date <- as.Date(snotel_data_out$date, format = "%Y-%m-%d")
snotel_data_out$mo <- month(snotel_data_out$date)
snotel_data_out$wy <- water_year(snotel_data_out$date, origin='usgs')
snotel_data_out$day <- day(snotel_data_out$date)

#subset April 1 data for model
wy<- year(seq(as.Date("1979-04-01"),as.Date(end),"year"))
april1swe<- array(data=NA, dim=c(length(wy), length(snotel_sites)+1))
colnames(april1swe)<- c('year', snotel_abrv)
april1swe[,1]<- wy

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == unique(snotel_data_out$site_name)[i] & snotel_data_out$mo == 4 & snotel_data_out$day ==1,]
  april1swe[,i+1]<- sub$snow_water_equivalent

  }




# save snotel data as a csv
write.csv(snotel_data_out, file.path(cd,'snotel_data.csv'))
write.csv(snotel_site_info, file.path(cd,'snotel_sites.csv'))


# Download NRCS ET Agrimet data ----
# OB = Air temperature
# PC = precipitation, cumulative (units?)
# SI = hourly soloar radiation (units?)
# SQ = solar radiation, cumulative ??
# additional: soil temp, humidity, vapor pressure

# Download Fairfield Data ----------------------------------------
# start: 06/25/1987; site number: 3108

# create increments to download data
tenYearDates=c(as.character(seq.Date(from=as.Date("1987-06-25"), to=as.Date(end), by="10 years")), end)
fafi = data.frame()

# download site data in ten year incremenets to prevent timing out server, will produce warning, that's okay
for (i in 2:length(tenYearDates)){
  tempDF=getAgriMet.data(site_id="FAFI", timescale="hourly", DayBgn = tenYearDates[i-1], DayEnd=tenYearDates[i], pCodes=c("OB", "PC","SI", "SQ")) 
  fafi=plyr::rbind.fill(fafi, tempDF)
}
#rename columns
colnames(fafi)<- c("date_time", "fafi_t", "fafi_pc", "fafi_si", "fafi_sq")
# update format of dates
fafi$date_time<- as.POSIXct(fafi$date_time, format ='%m/%d/%Y %H:%M')
fafi$mo <- month(fafi$date_time)
fafi$y <- year(fafi$date_time)
#this is not working
#fafi$wy <- water_year(fafi$date_time, origin='usgs')
# update format of values
for(i in 2:4) {
  fafi[, i]<- as.numeric(as.character(fafi[, i]))
}

fafi<-fafi[-1,] #remove first bad data value
fafiT<-fafi[,1:2]
fafiT[,3]<- 'fairfield'
fafiT<-cbind(fafiT, fafi[,6:7])
colnames(fafiT) <- c("date_time","t", "site_name", 'month', 'y')

# Download Picabo Data --------------------------------------
# start: 1982-06-01; site number: 7040; 
# Has SWE 1993-2002 and 2005-2017 - but can't be used predictively since it is no longer available

# create increments to download data
tenYearDates_p=c(as.character(seq.Date(from=as.Date("1982-06-01"), to=as.Date(end), by="10 years")), end)
pici =data.frame()

# download site data in ten year incremenets to prevent timing out server
for (i in 2:length(tenYearDates)){
  tempDF=getAgriMet.data(site_id="PICI", timescale="hourly", DayBgn = tenYearDates_p[i-1], DayEnd=tenYearDates_p[i], pCodes=c("OB", "PC")) 
  pici=plyr::rbind.fill(pici, tempDF)
}
# rename columns 
colnames(pici)<- c("date_time", "pici_t", "pici_pc")
# update format of dates
pici$date_time<- as.POSIXct(pici$date_time, format ='%m/%d/%Y %H:%M')
pici$mo <- month(pici$date_time)
pici$y <- year(pici$date_time)
#not working
#pici$wy <- water_year(pici$date_time, origin='usgs')

# update format of values
pici[, 2]<- as.numeric(as.character(pici[, 2]))
pici[, 3]<- as.numeric(as.character(pici[, 3]))
piciT<- pici [,1:2]
piciT[, 3]<-'picabo'
piciT<- cbind(piciT, pici[,4:5])
colnames(piciT)<- c("date_time","t", "site_name", 'month', 'y')

# Download Richmond Data - can't do this through AgriMet because Idaho Power Data ----
# start 3/27/2014 - 01-2020 daily temp; site number 7673

ichi=getAgriMet.data(site_id="ICHI", timescale="hourly", DayBgn = "2014-01-01", DayEnd="2020-02-01", pCodes=c("OB", "PC"))


# Merge AgriMet Data

# Merge & save AgriMet Data ---------------------------------
agri_metT <- rbind(piciT, fafiT)
write.csv(agri_metT, file.path(cd,'agri_metT.csv'))

agri_met<- full_join(pici, fafi, by='date_time')
write.csv(agri_met, file.path(cd,'agri_met.csv'))

# Additional Data Sources ----
# National Operational Hydrologic Remote Sensing Center data - max SWE at 17 locations?
# Reservoir Data
