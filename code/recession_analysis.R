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
fig_dir <<- file.path(git_dir, 'figures')

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
  qdiff$date<- streamflow$Date[streamflow$site_no == site_no][-1]%>% as.Date()
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

mod<- nls(I(-qdiff ~ a*(q^b)), data=rq, start = list(a=1, b=0.5))

plot(-qdiff~q,data=rq)
curve(0.0069*x^1.277,add=T)
# fit a & b; 4-5


predR=function(iQ,timesteps,m){
  getNextQ=function(thisQ,m){
    nextQ= thisQ-coefficients(m)['a']*thisQ^coefficients(m)['b']
    return(nextQ)
  }
  recession=data.frame(predQ=iQ,timestep=1:timesteps)
  for(t in 2:timesteps){
    thisQ=getNextQ(recession$predQ[t-1],m)
    recession$predQ[t]=thisQ
  }
  return(recession)
}

rq$predQ<- predR(rq$q[1], timesteps = nrow(rq), mod)$predQ

##

png(filename = file.path(fig_dir, "bwh_recession.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

plot(rq$date, rq$predQ, pch=16, xlab="Date", ylab="Big Wood at Hailey (cfs)")
points(rq$date, rq$q,pch=19, col='blue')

dev.off()



