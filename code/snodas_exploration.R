#explore snow data

#import relevant data
snodas<-read.csv(file.path(data_dir, 'allSnodasData.csv'))
snotel<-read.csv(file.path(data_dir, 'snotel_data.csv'))
streamflow<-read.csv(file.path(data_dir, 'streamflow_data.csv'))
allDat<-read.csv(file.path(data_dir, 'alldat_feb.csv'))

#modify df to subset and plot
snodas$year <- year(snodas$datetime)
snodas$mo<- month(snodas$datetime)
snodas$day<- day(snodas$datetime)

vol <- allDat %>% dplyr ::select (year, bwb.vol, bws.vol, cc.vol, sc.vol) %>% pivot_longer(cols=c('bwb.vol', 'bws.vol', 'cc.vol', 'sc.vol'), names_to = 'site', values_to = 'volume')
vol<- vol[!is.na(vol$volume),]
vol<- vol[vol$volume>0,]
siteIDs<-t(matrix(c(140, 'BIG WOOD RIVER AT HAILEY', 'bwb.vol',167,'CAMAS CREEK NR BLAINE ID', 'cc.vol',  144, 'SILVER CREEK AT SPORTSMAN ACCESS', 'sc.vol', 141, 'BIG WOOD RIVER AT STANTON CROSSING', 'bws.vol'), nrow=3, ncol=4))
colnames(siteIDs)<-c('locationid', 'name', 'site')
vol<-merge(vol, siteIDs, by='site')

#subset timeseries data to plot 
sno_april1<- snodas[snodas$mo == 4 & snodas$day ==1,] %>% dplyr::select (metric, value, locationid,year)

snovol<- merge(vol,sno_april1, by=c('year', 'locationid'))

snovol.swe<- subset(snovol, metric=='swe_total' & site != 'bws.vol' & site != 'sc.vol')
#filter(metric=='swe_total') %>% filter(site )
snovol.bws<- subset(snovol, metric=='SWE_total')

ggplot(snovol.swe, aes(x=value, y=volume, color=site)) + geom_point() + 
  theme_bw()

ggplot(snovol.bws, aes(x=value, y=volume)) + geom_point() + 
  theme_bw()
