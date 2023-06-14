#explore snow data

#import relevant data
snodas<-read.csv(file.path(data_dir, 'allSnodasData.csv'))
snotel<-read.csv(file.path(data_dir, 'snotel_data.csv'))
streamflow<-read.csv(file.path(data_dir, 'streamflow_data.csv'))
allDat<-read.csv(file.path(data_dir, 'all_dat_apr.csv'))

#modify df to subset and plot
snodas$year <- year(snodas$datetime)
snodas$mo<- month(snodas$datetime)
snodas$day<- day(snodas$datetime)

vol <- allDat %>% dplyr ::select (year, bwb.vol, bws.vol, cc.vol, sc.vol) %>% pivot_longer(cols=c('bwb.vol', 'bws.vol', 'cc.vol', 'sc.vol'), names_to = 'site', values_to = 'volume')
vol<- vol[!is.na(vol$volume),]
vol<- vol[vol$volume>0,]
siteIDs<-t(matrix(c(140, 'BIG WOOD RIVER AT HAILEY', 'bwb.vol', 'bwb', 167,'CAMAS CREEK NR BLAINE ID', 'cc.vol', 'cc', 144, 'SILVER CREEK AT SPORTSMAN ACCESS', 'sc.vol', 'sc', 141, 'BIG WOOD RIVER AT STANTON CROSSING', 'bws.vol', 'bws'), nrow=4, ncol=4))
colnames(siteIDs)<-c('locationid', 'name', 'site', 'site.s')
vol<-merge(vol, siteIDs, by='site')

# PIVOT snodas data to merge into the allDat frame
sno.wide<- snodas[,c(1:3,5)] %>% pivot_wider(names_from = c(metric, locationid), names_glue = "{metric}.{locationid}", values_from = value) 
#modify df to merge
sno.wide$year <- year(sno.wide$datetime)
sno.wide$mo<- month(sno.wide$datetime)
sno.wide$day<- day(sno.wide$datetime)

pTemp<-sno.wide[,c(1,5,9,14,15, 19,20)]
pTemp$wy<- as.numeric(as.character(waterYear(pTemp$datetime, numeric=TRUE)))
n.yrs<- unique(sno.wide$year)

for (i in 1:length(n.yrs)){
  sub1<- pTemp %>% filter(wy == n.yrs[i] & (mo >= 10 | mo < 4)) %>% as.data.frame() 
  p.wint[i, 2:5]<- sub1 %>% dplyr::select(c(2:5)) %>% colSums() %>% t()  %>% as.data.frame() 
  p.wint$wy[i] <-n.yrs[i]
}


sno.wide.apr<- sno.wide[sno.wide$mo == 4 & sno.wide$day ==1,] %>% dplyr::select(-c(datetime, mo, day))

allDat <- merge(allDat, sno.wide.apr, by= 'year')


#subset timeseries data to plot 
sno_april1<- snodas[snodas$mo == 4 & snodas$day ==1,] %>% dplyr::select (metric, value, locationid,year)

snovol<- merge(vol,sno_april1, by=c('year', 'locationid'))

#addnl subsetting, change to tidy verse or cleaner way, there is a prob w snodas data having two versions of swe 
snovol.swe<- subset(snovol, metric=='swe_total' & site != 'bws.vol' & site != 'sc.vol')
snovol.bws<- subset(snovol, metric=='swe_total' & site == 'bws.vol')
snovol.sca<- subset(snovol, metric=='snow_covered_area')# & site != 'bws.vol')
snovol.runoff<- subset(snovol, metric=='runoff_total' & site != 'bws.vol')

# plot swe versus total summer volume
ggplot(snovol.swe, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF') +xlab('SWE')
  # plot SNOTEL data over top of this 
  
ggplot(snovol.sca, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF')  +xlab('Snow Covered Area')

#runoff needs to be cumulative? does it actually makes sense?
ggplot(snovol.runoff, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF')  +xlab('Runoff')

ggplot(snovol.bws, aes(x=value, y=volume/1000)) + geom_point() + 
  theme_bw()+ylab('Irrigation Season Vol KAF')  +xlab('Snow Covered Area')


sno.runoff<- subset(snodas, metric=='runoff_total' & locationid == 140)
