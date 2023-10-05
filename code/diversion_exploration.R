# Diversion data analysis
# Kendra Kaiser
# Thursday Oct 5th, 2023

# ------------------------------------------------------------------------------     
# import and manage diversion data
# ------------------------------------------------------------------------------
cfs_to_AF<- 1.983

bw.div <- read.csv(file.path(input_dir, 'bwr_diversion_data_1987_2019_111620.csv'))
names(bw.div)[1]<- 'Date'
bw.div$Date <- as.Date(bw.div$Date, format = "%m/%d/%y")
bw.div$year<- format(bw.div$Date, "%Y")
bw.div[is.na(bw.div)] <- 0
bw.div$Osborn.24 <- as.numeric(bw.div$Osborn.24) #something is making this a character

sc.div <- read.csv(file.path(input_dir, 'sc_diversiondata_1987_2019_121620.csv'))
names(sc.div)[1]<- 'Date'
sc.div$Date <- as.Date(sc.div$Date, format = "%m/%d/%y")
sc.div$year<- format(sc.div$Date, "%Y")
sc.div$sc.div<- rowSums(sc.div[,2:11], na.rm = TRUE)

sc.div.daily<- sc.div %>% dplyr::select(c(Date, sc.div))
sc.div.sum<- sc.div %>% dplyr::select(-c(Date)) %>% group_by(year) %>% dplyr::summarise(across(everything(), sum))
sc.div.tot <- data.frame(matrix(nrow=dim(sc.div.sum)[1], ncol =2))

colnames(sc.div.tot)<- c("year", "sc.div")
sc.div.tot$year <- as.integer(sc.div.sum$year)
sc.div.tot$sc.div <- rowSums(sc.div.sum[,2:11], na.rm = TRUE)

#summarize by location
bw.div$abv.h <-rowSums(cbind(bw.div$Tom.P2, bw.div$Lewis.1, bw.div$Ketchum.2, 
                             bw.div$McCoy.3, bw.div$Peters.17C, bw.div$Hiawatha.22,
                             bw.div$Osborn.24, bw.div$Cove.33), na.rm = TRUE)
#everything other than HICE
bw.div$abv.s <-rowSums(cbind(bw.div$WRVID.45, bw.div$Bannon.49, 
                             bw.div$Glendale.50, bw.div$Baseline.55, bw.div$Brown.57F1, 
                             bw.div$Brown.57F2, bw.div$Black.61, bw.div$Graf.62,
                             bw.div$Uhrig.63, bw.div$Flood.64), na.rm=TRUE)
bw.div.gage<- bw.div %>% dplyr::select(c(Date, abv.h, abv.s))
#summarize by year
bw.div.sum<- bw.div %>% dplyr::select(-c(Date)) %>% group_by(year) %>% dplyr::summarise(across(everything(), sum))
bw.div.tot<- bw.div.sum %>% dplyr::select(c(year, abv.h, abv.s)) %>% as.data.frame()

bw.div.tot.AF<- as.numeric(bw.div.tot$year) %>% as.data.frame()
bw.div.tot.AF$abv.h <- bw.div.tot$abv.h*cfs_to_AF
bw.div.tot.AF$abv.s <- bw.div.tot$abv.s*cfs_to_AF
#

priorities <- read.csv(file.path(input_dir, 'ABV_PRIORITY_2020.csv'))
priorities$wr.vol.af<- priorities$TOTAL * cfs_to_AF
priorities$wr.date<- as.Date(paste(priorities$year_prior, priorities$mo, priorities$date, sep="-"))

wr <- read.csv(file.path(input_dir, 'WD37_Irrigation rights Big Wood Above Magic.csv'))
total_af<-  sum(wr$Overall.Max.Diversion.Volume.af., na.rm = TRUE)


# ------------------------------------------------------------------------------
# USGS Gages
# ------------------------------------------------------------------------------
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
site_info <- site_info %>% dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, alt_va, huc_cd, begin_date, end_date, count_nu, abv)

# Dowload data from all sites into one dataframe
streamflow_data <- readNWISdv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = min(site_info$begin_date), endDate = max(site_info$end_date)) %>% renameNWISColumns() %>% data.frame

#Re-format dates and pull out month /day/ water year
streamflow_data$Date <- as.Date(streamflow_data$Date, format = "%Y-%m-%d")
streamflow_data$mo <- month(streamflow_data$Date)
streamflow_data$wy <- as.numeric(as.character(waterYear(streamflow_data$Date, numeric=TRUE)))
streamflow_data$day <- day(streamflow_data$Date)
# Cleanup Streamflow data frame and join relevant site information
streamflow_data <- streamflow_data %>% dplyr::select(-agency_cd) %>% inner_join(site_info, by ="site_no") 

#-------------------------------------------------------------------------------
# add diversion data to streamflow dataframe
#-------------------------------------------------------------------------------

streamflow_data <- full_join(streamflow_data, bw.div.gage, by = 'Date')
streamflow_data <- full_join(streamflow_data, sc.div.daily, by = 'Date')
#streamflow_data <- streamflow_data[-c(58886:61156), ]
#make NAs equal to zero 
streamflow_data[,17:19][is.na(streamflow_data[,17:19])] <- 0 

streamflow_data$bwb.nat.q[streamflow_data$abv == 'bwb'] <- streamflow_data$Flow[streamflow_data$abv == 'bwb'] + streamflow_data$abv.h[streamflow_data$abv == 'bwb']
streamflow_data$bws.nat.q[streamflow_data$abv == 'bws'] <- streamflow_data$Flow[streamflow_data$abv == 'bws'] + streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']
streamflow_data$bw.div[streamflow_data$abv == 'bws'] <- streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']
streamflow_data$sc.nat[streamflow_data$abv == 'sc'] <- streamflow_data$Flow[streamflow_data$abv == 'sc'] + streamflow_data$sc.div[streamflow_data$abv == 'sc']

# metrics using "natural" flows
stream.id<-c("bwb","bws","cc","sc")
years = min(streamflow_data$wy):max(streamflow_data$wy)

nat.cm<-data.frame(matrix(ncol = 1, nrow= length(years)))
names(nat.cm)<-c("year")
nat.cm$year<- years

# calculate center of mass for "natural flow"
for(i in 1:length(stream.id)){
  sub <- streamflow_data %>% filter(abv == unique(abv)[i]) %>% filter(wy < 2020)
  
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
metrics <- metrics %>% full_join(nat.cm, by= "year")

# Merge April 1 SWE and streamflow metrics ----
bw.div.tot$year<- as.integer(bw.div.tot$year)

if (run_date == 'feb1'){
  alldat<- feb1swe %>% full_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
} else if (run_date == 'march1'){
  alldat<- mar1swe %>% full_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
} else if (run_date == 'april1'){
  alldat<- april1swe %>% full_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
}

# 'natural' flow is the volume at the gage plus the volume from upstream diversions
alldat$bwb.vol.nat <- alldat$bwb.vol + alldat$abv.h
alldat$bws.vol.nat <- alldat$bws.vol + alldat$abv.s + alldat$abv.h
alldat$bws.loss <- alldat$bws.vol.nat - alldat$bwb.vol
alldat$sc.vol.nat<- alldat$sc.vol + alldat$sc.div
