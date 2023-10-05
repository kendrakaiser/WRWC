# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June 19, 2020
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# USGS Gage
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

# ----------------------------------------------------------------------------------
# calculate hydrologic metrics for each year for each station
# winter "baseflow" (wb), Apr - Sept total Volume (vol), and center of mass (cm)
# ------------------------------------------------------------------------------
stream.id<-c("bwb","bws","cc","sc")
years = min(streamflow_data$wy):max(streamflow_data$wy)
metrics<-data.frame(matrix(ncol = 13, nrow= length(years)))
names(metrics)<-c("year","bwb.wq","bwb.vol","bwb.cm","bws.wq", "bws.vol","bws.cm","cc.wq","cc.vol","cc.cm", "sc.wq","sc.vol", "sc.cm")
metrics$year<- years

# calculate winter baseflow, annual irrigation season volume and center of mass
for(i in 1:length(stream.id)){
  sub <- streamflow_data %>% filter(abv == stream.id[i])
  
  for (y in 1: length(years)){
    #average winter flow
    sub1<- sub %>% filter(wy == years[y] & (mo >= 10 | mo < 2))
    wq <- mean(sub1$Flow, na.rm = TRUE)
    
    #total april-september flow in AF
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
sm = 792 # Stickney Mill
bc = 320 # Bear Canyon
snotel_sites = c(cg, g, gs, hc, lwd, ds, ccd, sr, ga, sp, sm, bc)
snotel_abrv <- c("cg.swe", "g.swe", "gs.swe", "hc.swe", "lwd.swe", "ds.swe", "ccd.swe", "sr.swe", "ga.swe", "sp.swe", "sm.swe", "bc.swe")

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
snotel_site_info$site_name <- c('chocolate gulch', 'galena', 'galena summit', 'hyndman', 'lost-wood divide', 'dollarhide summit', 'camas creek divide', 'soldier r.s.', 'garfield r.s.', 'swede peak', "stickney mill", "bear canyon")
snotel_site_info$abv<- snotel_abrv
snotel_site_info$huc8 <- c(219,219,219,219,219,219,220,220,221,221,303,101)
# remove unnecessary columns from Snotel data frame
snotel_data_out = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))
snotel_data_out$site_name<-trimws(snotel_data_out$site_name)
snotel_data_out$date <- as.Date(snotel_data_out$date, format = "%Y-%m-%d")
snotel_data_out$mo <- month(snotel_data_out$date)
snotel_data_out$wy <- as.numeric(as.character(waterYear(snotel_data_out$date, numeric=TRUE)))
snotel_data_out$day <- day(snotel_data_out$date)

# subset April 1 data for model 
wy<- seq(1979, year(end_date))
april1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(april1swe)<- c('year', snotel_abrv)
april1swe[,1]<- wy
april1swe<-as.data.frame(april1swe)

today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 4 & snotel_data_out$day ==1,]
  april1swe[which(april1swe$year == min(sub$wy)) : which(april1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 5){
  today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 4 & sub2$day ==30 & sub2$wy == (pred.yr-1)]}
}

april1swe[length(wy), 1:length(snotel_sites)+1]<- today_swe

# subset March 1 data for model 
mar1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(mar1swe)<- c('year', snotel_abrv)
mar1swe[,1]<- wy
mar1swe<-as.data.frame(mar1swe)

today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 3 & snotel_data_out$day ==1,]
  mar1swe[which(mar1swe$year == min(sub$wy)) : which(mar1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 4){
    today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 3 & sub2$day ==31 & sub2$wy == (pred.yr-1)]}
  }

# Update the March 1 SWE with the current swe
mar1swe[length(wy), 1:length(snotel_sites)+1]<- today_swe

# subset February 1 data for model 
feb1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(feb1swe)<- c('year', snotel_abrv)
feb1swe[,1]<- wy
feb1swe<-as.data.frame(feb1swe)

today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$mo == 2 & snotel_data_out$day ==1,]
  feb1swe[which(feb1swe$year == min(sub$wy)) : which(feb1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel_data_out[snotel_data_out$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 3){
    today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 2 & sub2$day ==28 & sub2$wy == (pred.yr-1)]}
  #today_swe[i]<-snotel_data_out$snow_water_equivalent[snotel_data_out$site_name == snotel_site_info$site_name[i] & snotel_data_out$date == max(snotel_data_out$date)-1]
}
# Update the Feb 1 SWE with the current swe
feb1swe[length(wy), 1:length(snotel_sites)+1]<- today_swe

# ------------------------------------------------------------------------------
# Save Data
# ------------------------------------------------------------------------------

# Save Snotel data as csvs -----
write.csv(april1swe, file.path(data_dir, 'april1swe.csv'), row.names=FALSE)
write.csv(mar1swe, file.path(data_dir, 'mar1swe.csv'), row.names=FALSE)
write.csv(feb1swe, file.path(data_dir, 'feb1swe.csv'), row.names=FALSE)

write.csv(snotel_data_out, file.path(data_dir,'snotel_data.csv'), row.names=FALSE)
write.csv(snotel_site_info, file.path(data_dir,'snotel_sites.csv'), row.names=FALSE)

# Save flow data as csvs ------
metrics <- metrics %>% full_join(nat.cm, by= "year")

write.csv(streamflow_data, file.path(data_dir,'streamflow_data.csv'), row.names=FALSE)
write.csv(metrics, file.path(data_dir,'metrics.csv'), row.names=FALSE)
write.csv(site_info, file.path(data_dir,'usgs_sites.csv'), row.names=FALSE)

# Merge April 1 SWE and streamflow metrics & diversion data ----
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

if (run_date == 'feb1'){
  filename = 'alldat_feb.csv'
} else if (run_date == 'march1'){
  filename = 'alldat_mar.csv'
} else if (run_date == 'april1'){
  filename = 'alldat_apr.csv'
}

write.csv(alldat, file.path(data_dir,filename), row.names=FALSE)

wq<- alldat %>% select("year", "bwb.wq", "bws.wq", "cc.wq", "sc.wq") %>% pivot_longer(!year, names_to = "site", values_to = "winterFlow")

# Boxplots of Historic Conditions
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")
wq_box<- ggplot(wq %>% filter(year < pred.yr), aes(x=factor(site), y=winterFlow))+
  geom_boxplot(alpha=0.8)+
  theme_bw()+
  xlab("USGS Site")+
  ylab("Average Nov-Jan Winter Flow (cfs)")+
  geom_point(data = wq %>% filter(year == pred.yr),  aes(x=factor(site), y=winterFlow), color="blue", size=3, shape=15)+
  scale_x_discrete(labels= sitelabs)

png(filename = file.path(fig_dir,"wq_box.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(wq_box)
dev.off()
