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
# USGS Gages Data & Metrics
# calculate hydrologic metrics for each year for each station 
# winter "baseflow" (wb), Apr - Sept irrigation volume (irr_vol), total Volume (tot_vol), and center of mass (cm)
# ------------------------------------------------------------------------------

#Average Winter Flow 
avgBaseflow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, AVG(value) AS wq, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2) 
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
#snodas_feb<-pivot_wider(data=winterSums_feb[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#Total April-September flow in AF [1.98 #convert from cfs to ac-ft]
irr_AF=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS irr_vol, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10) 
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

#Total water year flow in AF [1.98 #convert from cfs to ac-ft]
tot_AF=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS tot_vol, data.locationid, name, sitenote
          FROM data LEFT JOIN locations ON data.locationid = locations.locationid
          WHERE metric = 'streamflow' AND qcstatus = 'true'
          GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

tot_AF$wy1<-tot_AF$wateryear-1
totAF<- merge(tot_AF, tot_AF[,c('wateryear', "tot_vol", 'locationid', 'metric')], by.x=c('wy1', 'locationid', 'metric'), by.y=c('wateryear','locationid', 'metric'), suffixes = c('', 'ly'))




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

#center of mass between April 1 and July 31
cm_dat=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS flow, data.locationid, name, sitenote,  EXTRACT(doy FROM datetime) AS doy 
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 8) 
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote, doy) ORDER BY wateryear;")


years<- min(cm_dat$wateryear):max(cm_dat$wateryear)
cm<-data.frame(matrix(ncol = 3, nrow= length(years)*4)) %>% `colnames<-`(c('wateryear', 'sitenote', 'cm'))
cm$wateryear<- rep(years, times=4)
cm$sitenote<- rep(unique(cm_dat$sitenote), each=length(years))

for(sitename in unique(cm$sitenote)){
  sub <- cm_dat %>% filter(sitenote == sitename)
  for (wy in unique(cm$wateryear)){
    ix= which(cm$wateryear== wy & cm$sitenote == sitename) 
    cm$cm[ix]<- sum(sub$doy * sub$flow)/sum(sub$flow)
  }}

print('Streamflow Metrics Complete')
# ------------------------------------------------------------------------------
# Retrieve Snotel Data 
# ------------------------------------------------------------------------------
# TODO need the name of the snoteldata set, are the snotel shorthand sitenames under sitenote?
#FEBRUARY TESTER
winterSWE_feb=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS swe, snotel_data.locationid, name, sitenote
           FROM snotel_data LEFT JOIN locations ON snotel_data.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2
           GROUP BY(wateryear, snotel_data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
swe_feb<-pivot_wider(data=winterSums_feb[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")




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


