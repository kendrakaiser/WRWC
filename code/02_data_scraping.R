# ------------------------------------------------------------------------------
# Download and aggregate all available data for the Wood River Predictive Model
# Kendra Kaiser
# June 19, 2020
# ------------------------------------------------------------------------------
#tools to connect and write to database
source(file.path(git_dir, 'code/init_db.R'))
source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) 
source(paste0(git_dir,"/code/fxn_get_snow.R")) 
source(paste0(git_dir,"/code/fxn_SNODASR_functions.R")) 
#connect to database
conn=scdbConnect() 

# ------------------------------------------------------------------------------
# USGS Gages
# ------------------------------------------------------------------------------
# bwh = 13139510  #  Bullion Bridge, Big Wood at Hailey
# bws = 13140800  #  Stanton Crossing, Big Wood
# cc  = 13141500  #  Camas Creek near Blaine
# sc  = 13150430  #  Silver Creek near Picabo
# bwr = 13142500  #  Big Wood below Magic near Richfield
# mr  = 13142000  #  Magic Reservoir storage in acre - feet, impt for carry-over
# bwbr = 13140335 # Big Wood at S Broadford Bridge Nr Bellevue, data only goes back to 2017
# bwk = 13135500  #  Big Wood nr Ketchum, goes to 2011
# usgs_sites = c(bwh, bws, cc, sc, bwr) #  put all sites in one vector
# 
# pCode = "00060" # USGS code for streamflow
# sCode = "00054" # USGS code for reservoir storage (acre-feet)
# 
# # Dataframe with information about sites and period of record, uv = instantaneous value
# site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') 
# site_info$abv <- c("bwh", "bws", "cc", "bwr", 'sc')
# site_info <- site_info %>% dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, alt_va, huc_cd, begin_date, end_date, count_nu, abv)
# 
# # Dowload data from all sites into one dataframe
# streamflow <- readNWISdv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = min(site_info$begin_date), endDate = max(site_info$end_date)) %>% renameNWISColumns() %>% data.frame
# 
# #Re-format dates and pull out month /day/ water year
# streamflow$Date <- as.Date(streamflow$Date, format = "%Y-%m-%d")
# streamflow$mo <- month(streamflow$Date)
# streamflow$wy <- as.numeric(as.character(waterYear(streamflow$Date, numeric=TRUE)))
# streamflow$day <- day(streamflow$Date)
# # Cleanup Streamflow data frame and join relevant site information
# streamflow <- streamflow %>% dplyr::select(-agency_cd) %>% inner_join(site_info, by ="site_no") 
# 

streamflow_db=rbind(getWriteData(metric="streamflow", location="BIG WOOD RIVER AT HAILEY", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS"),
                    getWriteData(metric="streamflow", location="BIG WOOD RIVER AT STANTON CROSSING", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS"),
                    getWriteData(metric="streamflow", location="SILVER CREEK AT SPORTSMAN ACCESS", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS"),
                    getWriteData(metric="streamflow", location="CAMAS CREEK NR BLAINE ID", days=seq.Date(as.Date("1986-12-01"),Sys.Date(),by="day"),sourceName="USGS")
)
streamflow_db=streamflow_db[,c("locationid","value","datetime")]

streamflow_db = merge(streamflow_db,
                      dbGetQuery(conn, paste0("SELECT locationid, sitenote AS abv FROM locations WHERE locationid IN ('",paste(unique(streamflow_db$locationid),collapse="', '"),"');"))
)

streamflow_db$wateryear=as.numeric(as.character(waterYear(streamflow_db$datetime, numeric=TRUE)))
streamflow_db$mo=month(streamflow_db$datetime)




# ----------------------------------------------------------------------------------
# THESE METRICS will be replaced with SQL queries!
#calculate hydrologic metrics for each year for each station 
# winter "baseflow" (wb), Apr - Sept total Volume (vol), and center of mass (cm)
# ------------------------------------------------------------------------------
stream.id<-c("bwh","bws","cc","sc")
years = min(streamflow_db$wateryear):max(streamflow_db$wateryear)
metrics<-data.frame(matrix(ncol = 17, nrow= length(years)))
names(metrics)<-c("wateryear","bwh.wq","bwh.irr_vol","bwh.cm", "bwh.tot_vol", "bws.wq", "bws.irr_vol","bws.cm","bws.tot_vol","cc.wq","cc.irr_vol","cc.cm", "cc.tot_vol", "sc.wq","sc.irr_vol", "sc.cm","sc.tot_vol")
metrics$wateryear<- years

# calculate winter baseflow, annual irrigation season volume and center of mass
for(i in 1:length(stream.id)){
  sub <- streamflow_db %>% filter(abv == stream.id[i])
  
  for (y in 1: length(years)){
    #average winter flow
    sub1<- sub %>% filter(wateryear == years[y] & (mo >= 10 | mo < 2))
    wq <- mean(sub1$value, na.rm = TRUE)
    
    #total april-september flow in AF
    sub2<- sub %>% filter(wateryear == years[y] & between(mo, 4, 9)) 
    irr_vol<- sum(sub2$value)*1.98 #convert from cfs to ac-ft
    
    #total april-september flow in AF
    subv2<- sub %>% filter(wateryear == years[y])
    tot_vol<- sum(subv2$value)*1.98 #convert from cfs to ac-ft
    
    #center of mass between April 1 and July 31
    sub3<- sub %>% filter(wateryear == years[y] & between(mo, 4, 7)) 
    sub3$doy <- yday(as.Date(sub3$datetime))
    cm <- sum(sub3$doy * sub3$value)/sum(sub3$value)
    
    metrics[y,(((i-1)*4)+2)]<- wq
    metrics[y,(((i-1)*4)+3)]<- irr_vol
    metrics[y,(((i-1)*4)+4)]<- cm
    metrics[y,(((i-1)*4)+5)]<- tot_vol
  }
}

# add variable for last years streamflow -- total water year flow 
metrics$bwh.ly_vol[2:length(years)]<- metrics$bwh.tot_vol[1:length(years)-1]
metrics$bws.ly_vol[2:length(years)]<- metrics$bws.tot_vol[1:length(years)-1]
metrics$cc.ly_vol[2:length(years)]<- metrics$cc.tot_vol[1:length(years)-1]
metrics$sc.ly_vol[2:length(years)]<- metrics$sc.tot_vol[1:length(years)-1]

# ------------------------------------------------------------------------------
# Retrieve Snotel Data 
# ------------------------------------------------------------------------------
# SNOTEL Sites ----
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
snotel = subset(snotel_data, select = -c(network, state, start, end, latitude, longitude, elev, county, description))
snotel$site_name<-trimws(snotel$site_name)
snotel$date <- as.Date(snotel$date, format = "%Y-%m-%d")
snotel$mo <- month(snotel$date)
snotel$wy <- as.numeric(as.character(waterYear(snotel$date, numeric=TRUE)))
snotel$day <- day(snotel$date)

# subset SWE data by month for model ----
wy<- seq(1979, year(end_date))

#--- subset April 1 data for model
april1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(april1swe)<- c('wateryear', snotel_abrv)
april1swe[,1]<- wy
april1swe<-as.data.frame(april1swe)

today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel[snotel$site_name == snotel_site_info$site_name[i] & snotel$mo == 4 & snotel$day ==1,]
  april1swe[which(april1swe$year == min(sub$wy)) : which(april1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel[snotel$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 5){
    today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 4 & sub2$day ==30 & sub2$wy == (pred.yr-1)]}
}
# Update the April 1 SWE with the current swe
april1swe[length(wy), 1:length(snotel_sites)+1]<- today_swe

#--- subset March 1 data for model 
mar1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(mar1swe)<- c('year', snotel_abrv)
mar1swe[,1]<- wy
mar1swe<-as.data.frame(mar1swe)
today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel[snotel$site_name == snotel_site_info$site_name[i] & snotel$mo == 3 & snotel$day ==1,]
  mar1swe[which(mar1swe$year == min(sub$wy)) : which(mar1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel[snotel$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 4){
    today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 3 & sub2$day ==31 & sub2$wy == (pred.yr-1)]}
}
# Update the March 1 SWE with the current swe
mar1swe[length(wy), 1:length(snotel_sites)+1]<- today_swe

#--- subset February 1 data for model 
feb1swe<- matrix(data=NA, nrow=length(wy), ncol=length(snotel_sites)+1)
colnames(feb1swe)<- c('year', snotel_abrv)
feb1swe[,1]<- wy
feb1swe<-as.data.frame(feb1swe)

today_swe<- matrix(data=NA, nrow=1, ncol=length(snotel_sites))
colnames(today_swe)<- c(snotel_abrv)

for (i in 1:length(snotel_sites)) {
  sub<- snotel[snotel$site_name == snotel_site_info$site_name[i] & snotel$mo == 2 & snotel$day ==1,]
  feb1swe[which(feb1swe$year == min(sub$wy)) : which(feb1swe$year == max(sub$wy)),i+1]<- sub$snow_water_equivalent
  sub2<- snotel[snotel$site_name == snotel_site_info$site_name[i],]
  if (month(end_date) < 3){
    today_swe[i]<- sub2$snow_water_equivalent[sub2$date == end_date-1]
  } else {today_swe[i] <- sub2$snow_water_equivalent[sub2$mo == 2 & sub2$day ==28 & sub2$wy == (pred.yr-1)]}
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

write.csv(snotel, file.path(data_dir,'snotel_data.csv'), row.names=FALSE)
write.csv(snotel_site_info, file.path(data_dir,'snotel_sites.csv'), row.names=FALSE)

# Save flow data as csvs ------
write.csv(streamflow_db, file.path(data_dir,'streamflow_data.csv'), row.names=FALSE) # should we remove this now that it is all in the db?
write.csv(metrics, file.path(data_dir,'metrics.csv'), row.names=FALSE)
#write.csv(site_info, file.path(data_dir,'usgs_sites.csv'), row.names=FALSE)

print('Streamflow Data Saved')

#------------------------------------------------------------------------------
# Get SNODAS from Database 
#------------------------------------------------------------------------------
#FEBRUARY
winterSums_feb=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_feb<-pivot_wider(data=winterSums_feb[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#MARCH
winterSums_mar=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 3
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_march<-pivot_wider(data=winterSums_mar[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#APRIL
winterSums_apr=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 4
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_april<-pivot_wider(data=winterSums_apr[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")




# integrate SNODAS with USGS and SNOTEL
#TODO: THESE NEED CHANGED ONCE USGS and SNOTEL are updated
#April data for modeling
alldat <- feb1swe %>% full_join(metrics, by ="wateryear") %>% 
  merge(snodas_feb, by= 'wateryear')

filename = 'alldat_feb.csv' 

#April data for modeling
alldat <- mar1swe %>% full_join(metrics, by ="wateryear") %>% 
  merge(snodas_march, by= 'wateryear')

filename = 'alldat_mar.csv'

#April data for modeling
alldat <- april1swe %>% full_join(metrics, by ="wateryear") %>% 
  merge(snodas_april, by= 'wateryear')

filename = 'alldat_apr.csv'



# -----------------------------------------------------------------------------
# EXPORT Streamflow, Snotel, and Snodas Data
# -----------------------------------------------------------------------------
write.csv(alldat, file.path(data_dir,filename), row.names=FALSE)
print("All streamflow and snow data saved")

#------------------------------------------------------------------------------
#TODO: move these figures to seperate script
wq<- alldat %>% select("year", "bwh.wq", "bws.wq", "cc.wq", "sc.wq") %>% pivot_longer(!year, names_to = "site", values_to = "winterFlow")

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

# AGRIMET DATA
# -----------------------------------------------------------------------------
source(file.path(git_dir, 'code/fxn_grabAgriMetData.R'))

# Download NRCS ET Agrimet data ----
# OB = Air temperature
# PC = precipitation, cumulative (units?)
# SI = hourly solar radiation (units?)
# SQ = solar radiation, cumulative ??
# additional: soil temp, humidity, vapor pressure

# Download Fairfield Data ----------------------------------------
# start: 06/25/1987; site number: 3108
# create increments to download data
fiveYearDates=c(seq.Date(from=as.Date("1987-06-25"), to=as.Date(end_date), by="5 years"), end_date)
fafi = data.frame()

# download site data in ten year increments to prevent timing out server, will produce warning, that's okay
for (i in 2:length(fiveYearDates)){
  tempDF=getAgriMet.data(site_id="FAFI", timescale="hourly", DayBgn = fiveYearDates[i-1], DayEnd=fiveYearDates[i], 
                         pCodes=c("OB", "PC","SI", "SQ"))
  fafi=plyr::rbind.fill(fafi, tempDF)
  rm(tempDF)
}

# rename columns
colnames(fafi)<- c("date_time", "fafi_t", "fafi_pc", "fafi_si", "fafi_sq")
# update format of dates
fafi$date_time<- as.POSIXct(fafi$date_time, format ='%m/%d/%Y %H:%M')
fafi$mo <- month(fafi$date_time)
fafi$y <- year(fafi$date_time)
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
fiveYearDates_p=c(seq.Date(from=as.Date("1982-06-01"), to=end_date, by="5 years"),end_date) 
pici = data.frame()

# download site data in ten year increments to prevent timing out server
for (i in 2:length(fiveYearDates_p)){
  tempDF=getAgriMet.data(site_id="PICI", timescale="hourly", DayBgn = fiveYearDates_p[i-1], DayEnd=fiveYearDates_p[i], pCodes=c("OB", "PC")) 
  pici=plyr::rbind.fill(pici, tempDF)
}
# rename columns 
colnames(pici)<- c("date_time", "pici_t", "pici_pc")
# update format of dates
pici$date_time<- as.POSIXct(pici$date_time, format ='%m/%d/%Y %H:%M')
pici$mo <- month(pici$date_time)
pici$y <- year(pici$date_time)
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
agrimet <- rbind(piciT, fafiT) #long form agrimet data for database
agrimet$wy[!is.na(agrimet$date_time)]<- as.numeric(as.character(waterYear(agrimet$date_time[!is.na(agrimet$date_time)], numeric=TRUE)))
agri_met<- merge(pici[,1:3], fafi, by='date_time') #wide for modeling
agri_met$wy[!is.na(agri_met$date_time)]<- as.numeric(as.character(waterYear(agri_met$date_time[!is.na(agri_met$date_time)], numeric=TRUE)))

# saving to local directory
write.csv(agrimet, file.path('~/agri_metT.csv'), row.names = FALSE)

# > head(agrimet)
# date_time              t  site_name month    y   wy
# 1 1982-06-01 00:00:00 53.35    picabo     6 1982 1982
# 2 1982-06-01 01:00:00 53.74    picabo     6 1982 1982
# 3 1982-06-01 02:00:00 52.64    picabo     6 1982 1982
# 4 1982-06-01 03:00:00 51.54    picabo     6 1982 1982
# 5 1982-06-01 04:00:00 49.89    picabo     6 1982 1982


