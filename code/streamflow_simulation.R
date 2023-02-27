# ---------------------------------------------------------------------------- #
# WRWC Streamflow simulation
# Kendra Kaiser
# November 24, 2020
# Stochastic model to simulate Big Wood, Camas and Silver Creeks
# from April 1 - September 30
# Uses modeled temperature data from linear random effects model
# Uses multivariate models of natural streamflow and diversions

ns<-5000  #Number of simulations
dates<-seq(as.Date(paste(pred.yr,"-04-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")

# ------------------------------------------------------------------------------
# Import data
# volume & bootstraps for each gage
# natural flow and diversion records from which we will select the runoff timing
#streamflow_data<-read.csv(file.path(data_dir,"streamflow_data.csv"))
streamflow_data$year <- year(streamflow_data$Date)

bwb.wy<-streamflow_data[streamflow_data$abv == 'bwb',]
bws.wy<-streamflow_data[streamflow_data$abv == 'bws',]
cc.wy<-streamflow_data[streamflow_data$abv == 'cc',]
sc.wy<-streamflow_data[streamflow_data$abv == 'sc',]

# distributions and diversion hydrographs 
#vol.sample<-read.csv(file.path(model_out,"vol.sample.csv")) #ac-ft
colnames(vol.sample)<-c("bwb.vol", "bws.vol","cc.vol", "sc.vol")
#Example figures for presentation
#plot(dates, bwb.wy$Flow[bwb.wy$wy == 2006][183:365], xlab="Date", ylab ="Flow (cfs)", type='l', col="black", ylim=c(0,6650))
#lines(dates,bwb.wy$Flow[bwb.wy$wy == 2014][183:365], lwd=1, col="black")
#lines(dates,bwb.wy$Flow[bwb.wy$wy == 2013][183:365], lwd=1, col="black")
#lines(dates,bwb.wy$Flow[bwb.wy$wy == 2019][183:365], lwd=1, col="blue")
#lines(dates,bwb.wy$Flow[bwb.wy$wy == 1998][183:365], lwd=1, col="red")
#tst=cumsum(bwb.wy$Flow[bwb.wy$wy == 2019][183:365])
#plot(dates, tst, xlab="Date", ylab ="Cumulative Flow (cfs)", type='l')

# ------------------------------------------------------------------------------
# Create arrays to store outputs of stochastic simulations
#

cc.flow.s<-bwb.flow.s<-bws.flow.s<-sc.flow.s<-
  data.frame(array(NA,c(183,ns)))

rownames(cc.flow.s)<-rownames(bwb.flow.s)<-rownames(bws.flow.s)<-
  rownames(sc.flow.s)<-dates

# ------------------------------------------------------------------------------
# Simulations
#
sim.flow <- function(nat.wy, vol){
  "
  nat.wy: natural flow (or regulated flow depending on location) (cfs)
  vol: total volume from bootstrap sample (ac-ft)
  "
  pred <- nat.wy*vol/(sum(nat.wy)*1.98)
  return (pred)
}

for(k in 1:ns){ 
  # Simulate flow supply at the four gages
  year<-CMyear.sample[k,1] # year sample
  vol<-vol.sample[k,] # volume sample

  bwb<- bwb.wy[bwb.wy$wy == year, "Flow"][183:365]
  bws<- bws.wy[bws.wy$wy == year, "Flow"][183:365]
  sc<- sc.wy[sc.wy$wy == year, "Flow"][183:365]
  cc <- cc.wy[cc.wy$wy == year, "Flow"][183:365]
  
  bwb.flow.s[,k]<-sim.flow(bwb, exp(vol$bwb.vol))
  bws.flow.s[,k]<-sim.flow(bws, exp(vol$bws.vol))
  sc.flow.s[,k]<-sim.flow(sc, exp(vol$sc.vol))
  cc.flow.s[,k]<-sim.flow(cc, exp(vol$cc.vol))
}

#matplot(bwb.flow.s, type='l',col = "gray90" )
#matplot(sc.flow.s, type='l',col = "gray90" )

# ------------------------------------------------------------------------------
# Calculate prediction Intervals
numpreds<- 16 # reg *4
pi<-data.frame(array(NA,c(183,numpreds)))
meanQ<-data.frame(array(NA,c(183,4)))
medQ<-data.frame(array(NA,c(183,4)))

pred.int<-function(location){
  lo<-apply(location,1,quantile,0.05, na.rm=TRUE)
  hi<-apply(location,1,quantile,0.95, na.rm=TRUE)
  meanQ<-apply(location,1,mean, na.rm=TRUE)
  medQ<-apply(location,1,median, na.rm=TRUE)
  
  return(cbind(lo, hi, meanQ, medQ))
}

colnames(pi)<- c("bwb.low", "bwb.hi", "bwb.mean", "bwb.med","bws.low", "bws.hi", "bws.mean", "bws.med", 
                  "sc.low", "sc.hi", "sc.mean", "sc.med","cc.low", "cc.hi", "cc.mean", "cc.med")
pi[,1:4] <-as.data.frame(pred.int(bwb.flow.s))
pi[,5:8] <-as.data.frame(pred.int(bws.flow.s))
pi[,9:12]<-as.data.frame(pred.int(sc.flow.s))
pi[,13:16]<-as.data.frame(pred.int(cc.flow.s))

### Average Annual streamflow with maximum and minimum daily flows
q = read.csv(file.path(data_dir,'streamflow_data.csv'))
q$Date <- as.Date(q$Date)
q$doy <- yday(q$Date)
q<- q %>% filter(abv != 'bwr')

#Calculate day of water-year
water_year_begin <- ymd('1987-10-01')-1
#deal with leap years
q$doWY<- ((q$doy - yday(water_year_begin)) %% ifelse(leap_year(year(q$Date)), 366, 365)) +1


data <- q %>% filter(abv == 'bwb') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(Flow, na.rm=TRUE))

minBW=min(pi[,1], pi[,5],data$meanQ[457:639])
maxBW=max(pi[,2], pi[,6],data$meanQ[457:639])
# Big Wood @ Hailey
png(filename = file.path(fig_dir_mo, "BWB_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,3], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Natural Streamflow at Hailey", ylim=c(minBW, maxBW))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,1], pi[,2], rev(pi[,1])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[457:639],lwd=1.5,col="black")
lines(dates,pi[,3],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= bwb.wy[bwb.wy$wy == pred.yr, "bwb.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Summary stats
data <- q %>% filter(abv == 'bws') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(Flow, na.rm=TRUE))
# Big Wood @ Stanton
png(filename = file.path(fig_dir_mo, "BWS_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,7], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Streamflow at Stanton Crossing", ylim=c(minBW, maxBW))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,5], pi[,6], rev(pi[,5])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[196:378],lwd=1.5,col="black")
lines(dates,pi[,7],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= bws.wy[bws.wy$wy == pred.yr, "bws.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Summary stats
data <- q %>% filter(abv == 'sc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(Flow, na.rm=TRUE))

# Silver Creek
png(filename = file.path(fig_dir_mo, "SC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,11], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Silver Creek Natural Streamflow Simulation", ylim=c(min(pi[,9]), max(pi[,10])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,9], pi[,10], rev(pi[,9])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[91:273],lwd=1.5,col="black")
lines(dates,pi[,11],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= sc.wy[sc.wy$wy == pred.yr, "sc.nat"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Summary stats 
data <- q %>% filter(abv == 'cc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(Flow))
mincc=min(pi[,13], data$meanQ[91:273])
maxcc=max(pi[,14], data$meanQ[91:273])
# Camas Creek
png(filename = file.path(fig_dir_mo, "CC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates,pi[,15], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Camas Creek Streamflow Simulation", ylim=c(mincc, maxcc))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,13], pi[,14], rev(pi[,13])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[91:273],lwd=1.5,col="black")
lines(dates,pi[,15],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= cc.wy[cc.wy$wy == pred.yr, "Flow"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()
# ------------------------------------------------------------------------------
# Save Output

paste0("ModelOutput-", end_date, ".pdf")

write.csv(bwb.flow.s, file.path(model_out, paste0("BWB.sim-",end_date,".csv")), row.names=dates, col.names = TRUE)
write.csv(bws.flow.s, file.path(model_out, paste0("BWS.sim-",end_date,".csv")), row.names=dates, col.names = TRUE)
write.csv(cc.flow.s, file.path(model_out, paste0("CC.sim-", end_date,".csv")), row.names=dates, col.names = TRUE)
write.csv(sc.flow.s, file.path(model_out, paste0("SC.sim-", end_date,".csv")), row.names=dates, col.names = TRUE)

# ------------------------------------------------------------------------------
# Identify when curtailments will occur based on mean simulated streamflow

# Big Wood Water Rights -- should this be using flow at stanton, 
# or cumulative flow going into Magic?

# big wood stanton mean simulated flow 
wr.cutoffs<- data.frame("date" = array(NA,c(183)))
wr.cutoffs$date<- dates
wr.cutoffs$doy<- yday(dates)
wr.cutoffs$bws.meanQ<-pi[,"bwb.mean"]


# find the date of the most senior wr that is above the "current" flow
cutoff<- function(flow, wr_array){
  cut.wr<- wr_array$priority.date[min(which(wr_array$cuml.cfs > flow))]
  return(cut.wr)
  }
  
wr.cutoffs$cut.wr<-Reduce(c, lapply(wr.cutoffs$bws.meanQ, cutoff, wr.sub))
wr.cutoffs$cut.wr[1:which(wr.cutoffs$bws.meanQ == max(wr.cutoffs$bws.meanQ))]<-NA #ignore cutoffs before peak
wr.cutoffs$ymo<- zoo::as.yearmon(wr.cutoffs$cut.wr)
wr.cutoffs<- wr.cutoffs[complete.cases(wr.cutoffs),]

ggplot(wr.cutoffs, aes(x=date, y=bws.meanQ, color= as.factor(ymo))) + 
  geom_point()+
  theme_bw()+
  scale_y_continuous(n.breaks=8)+
  scale_x_date(date_breaks = "weeks" , date_labels = "%m/%d")+
  scale_color_viridis(discrete = TRUE, option = "turbo", direction=-1)+  
  scale_fill_discrete(name = "Water Right")+
  ylab("Average Simulated Streamflow @ Hailey (cfs)")+
  xlab("Date")

#the big problem / question here is that the old water rights shouldnt get turned off, so I'm wondering which data should be used here??
# Silver Creek 



