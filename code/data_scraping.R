# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June, 19, 2020
# ------------------------------------------------------------------------------

library(dataRetrieval)
library(tidyverse)
library(snotelr)

# USGS Gages
bwb = 13139510 #  Bullion Bridge, Big Wood at Hailey
bws = 13140800 #  Stanton Crossing, Big Wood
cc  = 13141500 #  Camas Creek near Blaine
sc  = 13150430 #  Silver Creek near Picabo

usgs_sites = c(bwb, bws, cc, sc) #  put all sites in one vector
pCode <- "00060" #  USGS code for streamflow

# Dataframe with information about sites and period of record
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') # uv is an alias for instantaneous values

# Merge data from all sites into one dataframe
flowdata <- readNWISuv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = site_info$begin_date, endDate = site_info$end_date) %>% renameNWISColumns() %>% data.frame

#save flow data as a csv
#save a figure that shows YTD streamflow over WY average and CV

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

snotel_data = snotel_download(snotel_sites, path = '~/Desktop/Data/WRWC/', internal = TRUE )
#clean unecessary columns

#save snotel data as a csv
#save a figure that shows YTD SWE over WY average and CV for each site

#  Additional Data

#NRCS ET Agrimet data

#National Operational Hydrologic Remote Sensing Center data - max SWE at 17 locations?

#Reservoir Data
