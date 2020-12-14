# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June, 19, 2020
# ------------------------------------------------------------------------------

source(file.path("code", "packages.R"))

# set data directory for saving data
cd ='~/Desktop/Data/WRWC'
# set date for AgriMet Data download
end = '2020-09-15'

#import and manage diversion data
bw.div <- read.csv(file.path(cd, 'bwr_diversion_data_1987_2019_111620.csv'))
bw.div$Date <- as.Date(bw.div$Date, format = "%m/%d/%y")
bw.div$year<- format(bw.div$Date, "%Y")
bw.div[is.na(bw.div)] <- 0
bw.div$Osborn.24 <- as.numeric(bw.div$Osborn.24) #something is making this a character

#summarize by location
bw.div$abv.h <-rowSums(cbind(bw.div$Tom.P2, bw.div$Lewis.1, bw.div$Ketchum.2, 
                                 bw.div$McCoy.3, bw.div$Peters.17C, bw.div$Hiawatha.22,
                                 bw.div$Osborn.24, bw.div$Cove.33), na.rm = TRUE)
#everything other than HICE
bw.div$abv.s <-rowSums(cbind(bw.div$WRVID.45, bw.div$Bannon.49, 
                                 bw.div$Glendale.50, bw.div$Baseline.55, bw.div$Brown.57F1, 
                                 bw.div$Brown.57F2, bw.div$Black.61, bw.div$Graf.62,
                                 bw.div$Uhrig.63, bw.div$Flood.64), na.rm=TRUE)
bw.div.gage<- bw.div %>% select(c(Date, abv.h, abv.s))
#summarize by year
bw.div.sum<- bw.div %>% select(-c(Date)) %>% group_by(year) %>% summarise_each(funs(sum))
bw.div.tot<- bw.div.sum %>% select(c(year, abv.h, abv.s))
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

# Dataframe with information about sites and period of record, uv = instantaneous value
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') 
site_info$abv <- c("bwb", "bws", "cc", "bwr", 'sc')
site_info <- site_info %>% select(site_no, station_nm, dec_lat_va, dec_long_va, alt_va, huc_cd, begin_date, end_date, count_nu, abv)

# Dowload data from all sites into one dataframe
streamflow_data <- readNWISdv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = min(site_info$begin_date), endDate = site_info$end_date) %>% renameNWISColumns() %>% data.frame

#Re-format dates and pull out month /day/ water year
streamflow_data$Date <- as.Date(streamflow_data$Date, format = "%Y-%m-%d")
streamflow_data$mo <- month(streamflow_data$Date)
streamflow_data$wy <- as.numeric(as.character(water_year(streamflow_data$Date, origin='usgs')))
streamflow_data$day <- day(streamflow_data$Date)
# Cleanup Streamdlow data frame and join relevant site information
streamflow_data <- streamflow_data %>% select(-agency_cd) %>% inner_join(site_info, by ="site_no") 
# add diversion data to streamflow dataframe
streamflow_data <- inner_join(streamflow_data, bw.div.gage, by = 'Date')
streamflow_data$bwb.nat.q[streamflow_data$abv == 'bwb'] <- streamflow_data$Flow[streamflow_data$abv == 'bwb'] + streamflow_data$abv.h[streamflow_data$abv == 'bwb']
streamflow_data$bws.nat.q[streamflow_data$abv == 'bws'] <- streamflow_data$Flow[streamflow_data$abv == 'bws'] + streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']
streamflow_data$bw.div[streamflow_data$abv == 'bws'] <- streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']

#Download reservoir data and site information
res_info <- whatNWISdata(sites= mr, parameterCd = sCode)
res_data <- readNWISuv(siteNumbers = mr, parameterCd = sCode, startDate = res_info$begin_date[2], endDate = res_info$end_date[2]) %>% renameNWISColumns() %>% data.frame

# ----------------------------------------------------------------------------------
# calculate hydrologic metrics for each year for each station
# winter "baseflow" (wb), Apr - Sept total Volume (vol), and center of mass (cm)
stream.id<-c("bwb","bws","cc","sc")
years = min(streamflow_data$wy):max(streamflow_data$wy)
metrics<-data.frame(matrix(ncol = 13, nrow= length(years)))
names(metrics)<-c("year","bwb.wq","bwb.vol","bwb.cm","bws.wq", "bws.vol","bws.cm","cc.wq","cc.vol","cc.cm", "sc.wq","sc.vol", "sc.cm")
metrics$year<- years

nat.cm<-data.frame(matrix(ncol = 1, nrow= length(years)))
names(nat.cm)<-c("year")
nat.cm$year<- years

# calculate winter baseflow, annual irrigation season volume and center of mass
for(i in 1:length(stream.id)){
  sub <- streamflow_data %>% filter(abv == unique(abv)[i])
  
  for (y in 1: length(years)){
    #average winter flow
    sub1<- sub %>% filter(wy == years[y] & (mo >= 10 | mo < 4))
    wq <- mean(sub1$Flow)
    
    #total april-september flow
    sub2<- sub %>% filter(wy == years[y] & between(mo, 4, 9)) 
    vol<- sum(sub2$Flow)*1.98 #convert from cfs to ac-ft
    
    #center of mass between April 1 and July 31
    sub3<- sub %>% filter(wy == years[y] & between(mo, 4, 7)) 
    sub3$doy <- yday(as.Date(sub3$Date))
    cm <- sum(sub3$doy * sub3$Flow)/sum(sub3$Flow)
    
    metrics[y,(((i-1)*3)+2)]<- wq
    metrics[y,(((i-1)*3)+3)]<- vol
    metrics[y,(((i-1)*3)+4)]<- cm
  }
}

# calculate center of mass for "natural flow"
for(i in 1:length(stream.id)){
  sub <- streamflow_data %>% filter(abv == unique(abv)[i])
  
  if(unique(sub$abv) == 'bwb'){
    sub$nat.flow <- sub$Flow + sub$abv.h
  } else if (unique(sub$abv) == 'bws'){
    sub$nat.flow = sub$Flow + sub$abv.s + sub$abv.h
  } else {next}
  
  cm = matrix(ncol = 1, nrow= length(years))
  for (y in 1: length(years)){
  #center of mass between April 1 and July 31
  sub3<- sub %>% filter(wy == years[y] & between(mo, 4, 7)) 
  sub3$doy <- yday(as.Date(sub3$Date))
  cm[y] <- sum(sub3$doy * sub3$nat.flow)/sum(sub3$nat.flow)
  }
  nat.cm<- cbind(nat.cm, cm)
}
colnames(nat.cm)<- c('year', 'bwb.cm.nat', 'bws.cm.nat')

#save a figure that shows YTD streamflow over WY average and CV

# Retrieve Snotel sites ----------------------------------------------------

# SNOTEL Sites 
cg = 895 #  Chocolate Gulch (0301)
g  = 489 #  Galena (0101)
gs = 490 #  Galena Summit (0101)
hc = 537 #  Hyndman Creek (0402)
lwd= 601 #  Lost-Wood Divide (Trail Creek 0304)
ds = 450 #  Dollarhide Summit (Placer/Warm Springs Creek 0201)
ccd = 382 #  Camas Creek Divide (Sheep/Camas Creek 0101)
sr = 769 #  Soldier R.S (Upper Soldier Creek 0303)
ga = 492 #  Garfield R.S. (Upper Muldoon Creek 0301)
sp = 805 #  Swede Peak (Upper Muldoon Creek 0301)
snotel_sites = c(cg, g, gs, hc, lwd, ds, ccd, sr, ga, sp)
snotel_abrv <- c("cg.swe", "g.swe", "gs.swe", "hc.swe", "lwd.swe", "ds.swe", "ccd.swe", "sr.swe", "ga.swe", "sp.swe")

# Download snotel data ----
snotel_data = snotel_download(snotel_sites, path = '~/Desktop/Data/WRWC/', internal = TRUE )

# Pull out snotel site information ----
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
snotel_site_info$site_name <- c('chocolate gulch', 'galena', 'galena summit', 'hyndman', 'lost-wood divide', 'dollarhide summit', 'camas creek divide', 'soldier r.s.', 'garfield r.s.', 'swede peak')
snotel_site_info$abv<- snotel_abrv
snotel_site_info$huc8 <- c(219,219,219,219,219,219,220,220,221,221)
# remove unecessary columns from snotel data frame
snotel_data_out = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))

snotel_data_out$site_name<-trimws(snotel_data_out$site_name)
snotel_data_out$date <- as.Date(snotel_data_out$date, format = "%Y-%m-%d")
snotel_data_out$mo <- month(snotel_data_out$date)
snotel_data_out$wy <- as.numeric(as.character(water_year(snotel_data_out$date, origin='usgs')))
snotel_data_out$day <- day(snotel_data_out$date)

# subset April 1 data for model 
# TODO: update this to pull Feb1 and Mar1 as well
wy<- year(seq(as.Date("1979-04-01"),as.Date(end),"year"))
april1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(april1swe)<- c('year', snotel_abrv)
april1swe[,1]<- wy
april1swe<-as.data.frame(april1swe)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 4 & snotel_data_out$day ==1,]
  start<- min(sub$wy)
  april1swe[which(april1swe$year == start) : which(april1swe$year == year(end)),i+1]<- sub$snow_water_equivalent
  }

# Save snotel data as csvs -----
write.csv(april1swe, file.path(cd, 'april1swe.csv'))
write.csv(snotel_data_out, file.path(cd,'snotel_data.csv'))
write.csv(snotel_site_info, file.path(cd,'snotel_sites.csv'))

# Save flow data as csvs ------
metrics <- metrics %>% inner_join(nat.cm, by= "year")

write.csv(streamflow_data, file.path(cd,'streamflow_data.csv'))
write.csv(metrics, file.path(cd,'metrics.csv'))
write.csv(site_info, file.path(cd,'usgs_sites.csv'))

# Merge april 1 swe and streamflow metrics & diversion data ----
bw.div.tot$year<- as.integer(bw.div.tot$year)
allApril1<- april1swe %>% inner_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") 
# 'natural' flow is the volume at the gage plus the volume from upstream diversions
allApril1$bwb.vol.nat <- allApril1$bwb.vol + allApril1$abv.h
allApril1$bws.vol.nat <- allApril1$bws.vol + allApril1$abv.s + allApril1$abv.h
allApril1$bws.loss <- allApril1$bws.vol.nat - allApril1$bwb.vol
write.csv(allApril1, file.path(cd,'all_April1.csv'))

# Download NRCS ET Agrimet data ----
# OB = Air temperature
# PC = precipitation, cumulative (units?)
# SI = hourly solar radiation (units?)
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
# Has SWE 1993-2002 and 2005-2017 - but can't be used predicatively since it is no longer available

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

# Download Richmond Data - can't do this through AgriMet because it's Idaho Power Data
# start 3/27/2014 - 01-2020 daily temp; site number 7673
# ichi=getAgriMet.data(site_id="ICHI", timescale="hourly", DayBgn = "2014-01-01", DayEnd="2020-02-01", pCodes=c("OB", "PC"))


# Merge & save AgriMet Data ---------------------------------
agri_metT <- rbind(piciT, fafiT)
write.csv(agri_metT, file.path(cd,'agri_metT.csv'))

agri_met<- full_join(pici, fafi, by='date_time')
write.csv(agri_met, file.path(cd,'agri_met.csv'))

# Additional Data Sources ----
# National Operational Hydrologic Remote Sensing Center data - max SWE at 17 locations?
# Reservoir Data
