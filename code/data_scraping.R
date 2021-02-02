# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June, 19, 2020
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------     
# import and manage diversion data
# ------------------------------------------------------------------------------
bw.div <- read.csv(file.path(input_dir, 'bwr_diversion_data_1987_2019_111620.csv'))
bw.div$Date <- as.Date(bw.div$Date, format = "%m/%d/%y")
bw.div$year<- format(bw.div$Date, "%Y")
bw.div[is.na(bw.div)] <- 0
bw.div$Osborn.24 <- as.numeric(bw.div$Osborn.24) #something is making this a character

sc.div <- read.csv(file.path(input_dir, 'sc_diversiondata_1987_2019_121620.csv'))
sc.div$Date <- as.Date(sc.div$Date, format = "%m/%d/%y")
sc.div$year<- format(sc.div$Date, "%Y")
sc.div$sc.div<- rowSums(sc.div[,2:11], na.rm = TRUE)

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
bw.div.tot<- bw.div.sum %>% dplyr::select(c(year, abv.h, abv.s))

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
streamflow_data$wy <- as.numeric(as.character(water_year(streamflow_data$Date, origin='usgs')))
streamflow_data$day <- day(streamflow_data$Date)
# Cleanup Streamflow data frame and join relevant site information
streamflow_data <- streamflow_data %>% dplyr::select(-agency_cd) %>% inner_join(site_info, by ="site_no") 

# add diversion data to streamflow dataframe
streamflow_data <- full_join(streamflow_data, bw.div.gage, by = 'Date')
streamflow_data <- full_join(streamflow_data, sc.div %>% dplyr::select(c(Date, sc.div)), by = 'Date')

streamflow_data$bwb.nat.q[streamflow_data$abv == 'bwb'] <- streamflow_data$Flow[streamflow_data$abv == 'bwb'] + streamflow_data$abv.h[streamflow_data$abv == 'bwb']
streamflow_data$bws.nat.q[streamflow_data$abv == 'bws'] <- streamflow_data$Flow[streamflow_data$abv == 'bws'] + streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']
streamflow_data$bw.div[streamflow_data$abv == 'bws'] <- streamflow_data$abv.s[streamflow_data$abv == 'bws'] + streamflow_data$abv.h[streamflow_data$abv == 'bws']
streamflow_data$sc.nat[streamflow_data$abv == 'sc'] <- streamflow_data$Flow[streamflow_data$abv == 'sc'] + streamflow_data$sc.div[streamflow_data$abv == 'sc']

#Download reservoir data and site information
#res_info <- whatNWISdata(sites= mr, parameterCd = sCode)
#res_data <- readNWISuv(siteNumbers = mr, parameterCd = sCode, startDate = res_info$begin_date[2], endDate = res_info$end_date[2]) %>% renameNWISColumns() %>% data.frame

# ----------------------------------------------------------------------------------
# calculate hydrologic metrics for each year for each station
# winter "baseflow" (wb), Apr - Sept total Volume (vol), and center of mass (cm)
# ------------------------------------------------------------------------------
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
  sub <- streamflow_data %>% filter(abv == stream.id[i])
  
  for (y in 1: length(years)){
    #average winter flow
    sub1<- sub %>% filter(wy == years[y] & (mo >= 10 | mo < 2))
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

# ------------------------------------------------------------------------------
# Retrieve Snotel Data 
# ------------------------------------------------------------------------------
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

# Download Snotel data ----
snotel_data = snotel_download(snotel_sites, path = data_dir, internal = TRUE )

# Pull out Snotel site information ----
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
# remove unnecessary columns from Snotel data frame
snotel_data_out = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))
snotel_data_out$site_name<-trimws(snotel_data_out$site_name)
snotel_data_out$date <- as.Date(snotel_data_out$date, format = "%Y-%m-%d")
snotel_data_out$mo <- month(snotel_data_out$date)
snotel_data_out$wy <- as.numeric(as.character(water_year(snotel_data_out$date, origin='usgs')))
snotel_data_out$day <- day(snotel_data_out$date)

# subset April 1 data for model 
wy<- seq(1979, year(end_date))
april1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(april1swe)<- c('year', snotel_abrv)
april1swe[,1]<- wy
april1swe<-as.data.frame(april1swe)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 4 & snotel_data_out$day ==1,]
  april1swe[which(april1swe$year == min(sub$wy)) : which(april1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
}

# subset March 1 data for model 
mar1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(mar1swe)<- c('year', snotel_abrv)
mar1swe[,1]<- wy
mar1swe<-as.data.frame(mar1swe)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 3 & snotel_data_out$day ==1,]
  mar1swe[which(mar1swe$year == min(sub$wy)) : which(mar1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
}

# subset February 1 data for model 
feb1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(feb1swe)<- c('year', snotel_abrv)
feb1swe[,1]<- wy
feb1swe<-as.data.frame(feb1swe)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 2 & snotel_data_out$day ==1,]
  feb1swe[which(feb1swe$year == min(sub$wy)) : which(feb1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
}

# ------------------------------------------------------------------------------
# Save Data
# ------------------------------------------------------------------------------

# Save Snotel data as csvs -----
write.csv(april1swe, file.path(data_dir, 'april1swe.csv'))
write.csv(mar1swe, file.path(data_dir, 'mar1swe.csv'))
write.csv(feb1swe, file.path(data_dir, 'feb1swe.csv'))

write.csv(snotel_data_out, file.path(data_dir,'snotel_data.csv'))
write.csv(snotel_site_info, file.path(data_dir,'snotel_sites.csv'))

# Save flow data as csvs ------
metrics <- metrics %>% full_join(nat.cm, by= "year")

write.csv(streamflow_data, file.path(data_dir,'streamflow_data.csv'))
write.csv(metrics, file.path(data_dir,'metrics.csv'))
write.csv(site_info, file.path(data_dir,'usgs_sites.csv'))

# Merge April 1 SWE and streamflow metrics & diversion data ----
bw.div.tot$year<- as.integer(bw.div.tot$year)

if (run_date == 'feb1'){
  alldat<- feb1swe %>% full_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
} else if (run_date == 'march1'){
  alldat<- mar1swe %>% inner_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
} else if (run_date == 'april1'){
  alldat<- april1swe %>% inner_join(metrics, by ="year") %>% full_join(bw.div.tot, by ="year") %>% full_join(sc.div.tot, by="year")
}

# 'natural' flow is the volume at the gage plus the volume from upstream diversions
alldat$bwb.vol.nat <- alldat$bwb.vol + alldat$abv.h
alldat$bws.vol.nat <- alldat$bws.vol + alldat$abv.s + alldat$abv.h
alldat$bws.loss <- alldat$bws.vol.nat - alldat$bwb.vol
alldat$sc.vol.nat<- alldat$sc.vol + alldat$sc.div

if (run_date == 'feb1'){
  filename = 'all_dat_feb.csv'
} else if (run_date == 'march1'){
  filename = 'all_dat_mar.csv'
} else if (run_date == 'april1'){
  filename = 'all_dat_apr.csv'
}

write.csv(alldat, file.path(data_dir,filename))


# Additional Data Sources ----
# National Operational Hydrologic Remote Sensing Center data - max SWE at 17 locations?
# Reservoir Data
# Groundwater Data

# -------------- Create Summary Figures ----
# These are generated in the ModelOutput.Rmd
# streamflow_data$Date <- as.Date(streamflow_data$Date)
# streamflow_data$doy <- yday(streamflow_data$Date)
# q<- streamflow_data %>% filter(abv != 'bwr')
# #Calculate day of water-year
# water_year_begin <- ymd('1987-10-01')-1
# #deal with leap years
# q$doWY<- ((q$doy - yday(water_year_begin)) %% ifelse(leap_year(year(q$Date)), 366, 365)) +1
# 
# # Summary stats
# detach(package:plyr) #plyr interferes with a group_by
# data <- group_by(q, doWY) %>% filter(abv == 'bwb') %>% mutate(meanQ=mean(Flow), maxQ=max(Flow), minQ=min(Flow))
# # Plot
# png(filename = file.path(fig_dir,"bwb_historicQ.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="quartz") 
# 
# ggplot(data=data, mapping=aes(x=doWY))+
#   labs(title = "Big Wood River at Hailey")+
#   geom_line(mapping=aes(y=meanQ), color='red')+
#   geom_ribbon(mapping=aes(ymin=minQ, ymax=maxQ), alpha=.1)+
#   xlab('DOY')+
#   ylab('Flow (cfs)')+
#   theme_bw()
# dev.off()
# 
# # Summary stats
# data <- group_by(q, doWY) %>% filter(abv == 'bws') %<>% mutate(meanQ=mean(Flow), maxQ=max(Flow), minQ=min(Flow))
# # Plot
# png(filename = file.path(fig_dir,"bws_historicQ.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="quartz") 
# 
# ggplot(data=data, mapping=aes(x=doWY))+
#   labs(title = "Big Wood River at Stanton Crossing") +
#   geom_line(mapping=aes(y=meanQ), color='red')+
#   geom_ribbon(mapping=aes(ymin=minQ, ymax=maxQ), alpha=.1)+
#   xlab('DOY')+
#   ylab('Flow (cfs)')+
#   theme_bw()
# dev.off()
# 
# # Summary stats
# data <- group_by(q, doWY) %>% filter(abv == 'cc') %>% mutate(meanQ=mean(Flow), maxQ=max(Flow), minQ=min(Flow))
# # Plot
# png(filename = file.path(fig_dir,"cc_historicQ.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="quartz") 
# 
# ggplot(data=data, mapping=aes(x=doWY))+
#   labs(title = "Camas Creek") +
#   geom_line(mapping=aes(y=meanQ), color='red')+
#   geom_ribbon(mapping=aes(ymin=minQ, ymax=maxQ), alpha=.1)+
#   xlab('DOY')+
#   ylab('Flow (cfs)')+
#   theme_bw()
# dev.off()
# 
# # Summary stats
# data <- group_by(q, doWY) %>% filter(abv == 'sc') %>% mutate(meanQ=mean(Flow), maxQ=max(Flow), minQ=min(Flow))
# # Plot
# png(filename = file.path(fig_dir,"sc_historicQ.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600, type ="quartz") 
# 
# ggplot(data=data, mapping=aes(x=doWY))+
#   labs(title = "Silver Creek") +
#   geom_line(mapping=aes(y=meanQ), color='red')+
#   geom_ribbon(mapping=aes(ymin=minQ, ymax=maxQ), alpha=.1)+
#   xlab('DOY')+
#   ylab('Flow (cfs)')+
#   theme_bw()
# 
# dev.off()