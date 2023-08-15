# Recession Analysis
# Kendra Kaiser
# August 15, 2023

#tools to connect and write to database
library(RPostgres)
# GitHub File Path
git_dir <<- '~/github/WRWC'
# Local File Path
cd <<- '~/Desktop/WRWC'
data_dir <<- file.path(cd, 'data') # local

#source(paste0(git_dir,"/code/dbIntakeTools.R")) 
source(file.path(git_dir, 'code/packages.R'))

#connect to database
#conn=scdbConnect() 

### Data Import -----------------------------------------------------------###
#import relevant data
# streamflow<-dbGetQuery(conn,"SELECT * FROM xx) update to pull streamflow from db?
streamflow<-read.csv(file.path(data_dir, 'streamflow_data.csv'))

# Calculate decrease in cfs per day; 
decreaseQ <- function(site_no, wy){
  qdiff<- diff(streamflow$Flow[streamflow$site_no == site_no]) %>% 
    as.data.frame() %>% `colnames<-` (c('qdiff'))
  # add these vars through tidy verse to clean up
  qdiff$date<- streamflow$Date[streamflow$site_no == site_no][-1]
  qdiff$wy<- streamflow$wy[streamflow$site_no == site_no][-1]
  qdiff$mo<- streamflow$mo[streamflow$site_no == site_no][-1]
  qdiff$q<-streamflow$Flow[streamflow$site_no == site_no][-1]
  qdiff_sub<- qdiff[qdiff$wy == wy & qdiff$mo > 3 & qdiff$mo < 10 ,]
  qseq <- qdiff_sub$qdiff
  decr <- sapply(seq_along(qseq),function(x){all(qseq[x:(x+7)]<0)})
  firstdecr <- min(which(decr))
  recessionQ<- qdiff_sub[firstdecr:length(qseq),]
  return(recessionQ)
}
rq<- decreaseQ('13139510', 2022)

# fit a & b; 4-5

