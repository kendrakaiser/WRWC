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
devtools::install_github(repo = "rhlee12/RNRCS", subdir = "/RNRCS/")
library(RNRCS)

#set date for AgriMet Data download
end = '2020-07-01'

# USGS Gages
bwb = 13139510 #  Bullion Bridge, Big Wood at Hailey
bws = 13140800 #  Stanton Crossing, Big Wood
cc  = 13141500 #  Camas Creek near Blaine
sc  = 13150430 #  Silver Creek near Picabo
usgs_sites = c(bwb, bws, cc, sc) #  put all sites in one vector

pCode <- "00060" # USGS code for streamflow

# Dataframe with information about sites and period of record
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') # uv is an alias for instantaneous values

# Merge data from all sites into one dataframe
streamflow_data <- readNWISuv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = site_info$begin_date, endDate = site_info$end_date) %>% renameNWISColumns() %>% data.frame

# Save flow data as a csv
write.csv(streamflow_data, '~/Desktop/Data/WRWC/streamflow_data.csv')
write.csv(site_info, '~/Desktop/Data/WRWC/usgs_sites.csv')

#save a figure that shows YTD streamflow over WY average and CV

# -------- Retrieve Snotel data -------- #

# SNOTEL Sites
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

# Download snotel data
snotel_data = snotel_download(snotel_sites, path = '~/Desktop/Data/WRWC/', internal = TRUE )

# Pull out snotel site information
snotel_site_info<-data.frame('id'=NA, 'start'=NA, 'end'=NA, 'lat'=NA, 'long'=NA, 'elev'=NA, 'description'=NA)
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

# remove unecessary columns from snotel data frame
snotel_data_out = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))

# save snotel data as a csv
write.csv(snotel_data_out, '~/Desktop/Data/WRWC/snotel_data.csv')
write.csv(snotel_site_info, '~/Desktop/Data/WRWC/snotel_sites.csv')

# save a figure that shows YTD SWE over WY average and CV for each site



# -------- Download NRCS ET Agrimet data -------- #
# OB = Air temperature
# PC = precipitation, cumulative
# additional: soil temp, humidity, vapor pressure

# Fairfield; start: 06/25/1987; site number: 3108
fafi=getAgriMet.data(site_id="FAFI", timescale="hourly", DayBgn = "1990-07-01", DayEnd="2020-07-01", pCodes=c("OB", "PC","SI")) 
colnames(fafi)<- c("date_time", "fafi_t", "fafi_pc", "fafi_si", "fafi_sq")
fafi$date_time<- as.POSIXct(fafi$date_time, format ='%m/%d/%Y %H:%M')
#something aint right w the conversion
fafi[, 2]<- as.numeric(fafi[, 2])

write.csv(fafi, '~/Desktop/Data/WRWC/fafi.csv')

#Picabo; start: start: 1982-06-01; site number: 7040; does have SWE 1993-2002 and 2005-2017 - cant be used predictively since it is no longer available
pici=getAgriMet.data(site_id="PICI", timescale="hourly", DayBgn = "1990-06-01", DayEnd="2020-07-01", pCodes=c("OB", "PC"))
colnames(pici)<- c("date_time", "pici_t", "pici_pc")
pici$date_time<- as.POSIXct(pici$date_time, format ='%m/%d/%Y %H:%M')

write.csv(pici, '~/Desktop/Data/WRWC/pici.csv')

#Richmond; start 3/27/2014 - 01-2020 daily temp; site number 7673
ichi=getAgriMet.data(site_id="ICHI", timescale="hourly", DayBgn = "2014-01-01", DayEnd="2020-02-01", pCodes=c("OB", "PC"))

# Merge AgriMet Data
agri_met<- full_join(fafi, pici, by='date_time')
# Save AgriMet data as csv



# National Operational Hydrologic Remote Sensing Center data - max SWE at 17 locations?

# Reservoir Data
