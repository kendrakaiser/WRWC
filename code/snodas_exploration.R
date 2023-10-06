#Data analysis and management on snow data and relationships 
# Need to modify now that SNODAS is incorp into data_scraping

### ---------------------------------------------------------------------------------
# Plotting for data exploration
### ---------------------------------------------------------------------------------
vol <- alldat %>% dplyr ::select (year, bwb.vol, bws.vol, cc.vol, sc.vol) %>% pivot_longer(cols=c('bwb.vol', 'bws.vol', 'cc.vol', 'sc.vol'), names_to = 'site', values_to = 'volume')
vol<- vol[!is.na(vol$volume),]
vol<- vol[vol$volume>0,]
vol<-merge(vol, siteIDs, by='site')

#subset and merge timeseries data to plot 
sno_april1<- snodas[snodas$mo == 4 & snodas$day ==1,] %>% dplyr::select (metric, value, locationid,year)
snovol<- merge(vol, sno_april1, by=c('year', 'locationid'))

#addnl subsetting, change to tidy verse or cleaner way, there is a prob w snodas data having two versions of swe 
snovol.swe<- subset(snovol, metric=='swe_total' & site != 'sc.vol')
snovol.sca<- subset(snovol, metric=='snow_covered_area')# & site != 'bws.vol')

# plot swe versus total summer volume
ggplot(snovol.swe, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF') +xlab('SWE')
  # plot SNOTEL data over top of this 

# SCA v.s. vol -- april 1 sca above haily doesnt tell us much ...
ggplot(snovol.sca, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF')  +xlab('Snow Covered Area')

# subset to precip and runoff
p.spr.long<- p.spring %>% pivot_longer(!wy, names_to = c("metric", "locationid"), names_sep="\\.", values_to = "value")
p.spr.long$metric<- "liquid_precip_spr"
p.wint.long<- p.wint %>% pivot_longer(!wy, names_to = c("metric", "locationid"), names_sep="\\.", values_to = "value")
p.wint.long$metric<- "liquid_precip_wint"
runoff.long<- runoff.apr %>% pivot_longer(!wy, names_to = c("metric", "locationid"), names_sep="\\.", values_to = "value")
p.run<- rbind(p.spr.long, p.wint.long, runoff.long) %>% subset(wy >2003)

# --- Precip and runoff 
snop<- merge(vol, p.run, by=c('year', 'locationid'))

snop.p.sp<- subset(snop, metric=='liquid_precip_spr')# & site != 'sc.vol')
snop.p.wint<- subset(snop, metric=='liquid_precip_wint')
snop.run<- subset(snop, metric=='runoff_total')

ggplot(snop.p.sp, aes(x=value/1000, y=volume/1000, color=site)) + geom_point() + 
  theme_bw() +ylab('Irrigation Season Vol KAF')  +xlab('Spring Precip')

ggplot(snop.p.wint, aes(x=value/1000, y=volume/1000, color=site)) + geom_point() + 
  theme_bw()+ylab('Irrigation Season Vol KAF')  +xlab('Winter Precip')

ggplot(snop.run, aes(x=value, y=volume/1000, color=site)) + geom_point() + 
  theme_bw()+ylab('Irrigation Season Vol KAF')  +xlab('SRunoff')

sno.pre<- subset(snodas, metric=='liquid_precip')
snop.p.bw<- subset(sno.pre, locationid == '140') %>% subset(locationid == '140')
ggplot(snop.p.bw, aes(x=datetime, y=value)) + geom_point()

