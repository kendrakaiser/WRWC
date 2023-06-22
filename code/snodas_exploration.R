#explore snow data and relationships 

#tools to connect and write to database
library(RPostgres)
# GitHub File Path
git_dir <<- '~/github/WRWC'
# Local File Path
cd <<- '~/Desktop/WRWC'
data_dir <<- file.path(cd, 'data') # local

source(paste0(git_dir,"/code/dbIntakeTools.R")) 
source(paste0(git_dir,"/code/SNODASR_functions.R")) 
source(file.path(git_dir, 'code/packages.R'))

#connect to database
conn=scdbConnect() 
#update the snodas data when necessary
dbExecute(conn, "REFRESH MATERIALIZED VIEW snodasdata") 

### Data Import -----------------------------------------------------------###
#import relevant data
snodas<-dbGetQuery(conn,"SELECT * FROM snodasdata WHERE qcstatus = 'TRUE';")
#snodas<-read.csv(file.path(data_dir, 'allSnodasData.csv'))
snotel<-read.csv(file.path(data_dir, 'snotel_data.csv'))
streamflow<-read.csv(file.path(data_dir, 'streamflow_data.csv'))
allDat<-read.csv(file.path(data_dir, 'all_dat_mar.csv')) ## will need to check the naming convention here and make sure automation with full script works

### Data Munging -----------------------------------------------------------###
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
sno.wide$yearog <- year(sno.wide$datetime)
sno.wide$mo<- month(sno.wide$datetime)
sno.wide$day<- day(sno.wide$datetime)
sno.wide$year<- as.numeric(as.character(waterYear(sno.wide$datetime, numeric=TRUE)))
n.yrs<- unique(sno.wide$year)

#subset snodas data to calculate cumulative values
pTemp<-sno.wide[,c(1,5,9,12,17,18,19,21)] # need to use reg expressions to do this correctly
p.wint<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(p.wint) <- c('year', "liquid_precip.140.wint", "liquid_precip.167.wint", "liquid_precip.144.wint", "liquid_precip.141.wint")
p.spring<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(p.spring) <- c('year', "liquid_precip.140.spr", "liquid_precip.167.spr", "liquid_precip.144.spr", "liquid_precip.141.spr")
runoffTemp<-sno.wide[,c(1,3,7,13,15, 18,19,21)]
runoff.sub<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(runoff.sub) <- colnames(runoffTemp)[c(8, 2:5)]

# UPDATE THIS SECTION TO RUN FOR EACH MONTH ------

#function to sum data for a given range of months
cuml.snodas<-function(in_array, out_array, start_mo, end_mo){
  for (i in 1:length(n.yrs)){
    sub1<- in_array %>% filter(year == n.yrs[i] & (mo >= start_mo | mo < end_mo)) %>% as.data.frame() 
    out_array$year[i] <-n.yrs[i]
    out_array[i,2:5]<- sub1 %>% dplyr::select(c(2:5)) %>% colSums() %>% t()  %>% as.data.frame() 
  }
  return(out_array)
}

#calculate seasonal totals 
p.wint<- cuml.snodas(pTemp, p.wint, 10, 3)
#p.spring<- cuml.snodas(pTemp, p.spring, 4, 7)
runoff.sub<- cuml.snodas(runoffTemp, runoff.sub, 10, 3)

##### ----- COMPILE ALL NEW DATA for modeling ---------------
sno.wide.sub<- sno.wide[sno.wide$mo == 3 & sno.wide$day ==1,] %>% dplyr::select(-c(datetime, mo, day))
allDat <- merge(allDat, sno.wide.sub[,c(1,3,5,7,9,10,13,15,18)], by= 'year')
allDat <- allDat %>% merge(p.wint, by= 'year')%>% merge(runoff.sub, by= 'year') #%>% merge(p.spring, by= 'year')
write.csv(allDat, file.path(data_dir, 'alldat_mar.csv'), row.names=FALSE)

### ---------


#### Plotting for data exploration

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

