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
dbExecuteQuery(conn, "REFRESH MATERIALIZED VIEW snodasdata") 

### Data Import -----------------------------------------------------------###
#import relevant data
#snodas<-dbGetQuery(conn,"SELECT * FROM snodasdata;")
snodas<-read.csv(file.path(data_dir, 'allSnodasData.csv'))
snotel<-read.csv(file.path(data_dir, 'snotel_data.csv'))
streamflow<-read.csv(file.path(data_dir, 'streamflow_data.csv'))
allDat<-read.csv(file.path(data_dir, 'all_dat_apr.csv'))

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
sno.wide$year <- year(sno.wide$datetime)
sno.wide$mo<- month(sno.wide$datetime)
sno.wide$day<- day(sno.wide$datetime)
sno.wide$wy<- as.numeric(as.character(waterYear(sno.wide$datetime, numeric=TRUE)))
n.yrs<- unique(sno.wide$year)

pTemp<-sno.wide[,c(1,5,9,12,17, 18,19,21)] # need to use reg expressions to do this correctly
p.wint<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(p.wint) <- colnames(pTemp)[c(8, 2:5)]
p.spring<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(p.spring) <- colnames(pTemp)[c(8, 2:5)]

runoffTemp<-sno.wide[,c(1,3,7,13,15, 18,19,21)]
runoff.apr<-as.data.frame(array(data=NA, dim=c(length(n.yrs), 5)))
colnames(runoff.apr) <- colnames(runoffTemp)[c(8, 2:5)]

cuml.snodas<-function(in_array, out_array, start_mo, end_mo){
  for (i in 1:length(n.yrs)){
    sub1<- in_array %>% filter(wy == n.yrs[i] & (mo >= start_mo | mo < end_mo)) %>% as.data.frame() 
    out_array$wy[i] <-n.yrs[i]
    out_array[i,2:5]<- sub1 %>% dplyr::select(c(2:5)) %>% colSums() %>% t()  %>% as.data.frame() 
  }
  return(out_array)
}

p.wint<- cuml.snodas(pTemp, p.wint, 10, 4)
p.spring<- cuml.snodas(pTemp, p.spring, 4, 7)
runoff.apr<- cuml.snodas(runoffTemp, runoff.apr, 10, 4)


sno.wide.apr<- sno.wide[sno.wide$mo == 4 & sno.wide$day ==1,] %>% dplyr::select(-c(datetime, mo, day))
allDat <- merge(allDat, sno.wide.apr, by= 'year')


#subset timeseries data to plot 
sno_april1<- snodas[snodas$mo == 4 & snodas$day ==1,] %>% dplyr::select (metric, value, locationid,year)

snovol<- merge(vol,sno_april1, by=c('year', 'locationid'))

#addnl subsetting, change to tidy verse or cleaner way, there is a prob w snodas data having two versions of swe 
snovol.swe<- subset(snovol, metric=='swe_total' & site != 'sc.vol')
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
