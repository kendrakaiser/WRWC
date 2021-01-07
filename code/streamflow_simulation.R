# ---------------------------------------------------------------------------- #
# WRWC Streamflow simulation
# Kendra Kaiser
# November 24, 2020
# Stochastic model to simulate Big Wood, Camas and Silver Creeks
# from April 1 - September 30
# Uses modeled temperature data from linear random effects model
# Uses multivariate models of natural streamflow and diversions


cd <<- '~/Desktop/Data/WRWC'
fig_dir = '~/github/WRWC/figures' 
#set run date for pulling & saving data
run_date = 'feb1'

pred.yr <<- 2019 #Simyear
ns<-5000  #Number of simulations

dates<-seq(as.Date(paste(pred.yr,"-04-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")

# ------------------------------------------------------------------------------
# Import data
# mean ET from each basin -- or from FAFI / PICO ???
# volume & bootstraps for each gage
# natural flow and diversion records from which we will select the runoff timing
streamflow<-read.csv(file.path(cd,"streamflow_data.csv"))
streamflow$year <- year(streamflow$Date)

bwb.wy<-streamflow[streamflow$abv == 'bwb',]
bws.wy<-streamflow[streamflow$abv == 'bws',]
cc.wy<-streamflow[streamflow$abv == 'cc',]
sc.wy<-streamflow[streamflow$abv == 'sc',]

# distributions and diversion hydrographs 
cm.year<-read.csv(file.path(cd,"CMyear.sample.csv"))
if (run_date == 'feb1'){
  filename = "vol.sample.feb.csv"
} else if (run_date == 'march1'){
  filename = "vol.sample.march.csv"
} else if (run_date == 'april1'){
  filename = "vol.sample.april.csv"
}

volumes<-read.csv(file.path(cd,filename)) #ac-ft

# ------------------------------------------------------------------------------
# Create arrays to store outputs of stochastic simulations
#

cc.flow.s<-bwb.flow.s<-bws.flow.s<-sc.flow.s<-bw.div.s<-sc.div.s<-
  data.frame(array(NA,c(183,ns)))

rownames(cc.flow.s)<-rownames(bwb.flow.s)<-rownames(bws.flow.s)<-
  rownames(sc.flow.s)<-rownames(bw.div.s)<-rownames(sc.div.s)<-dates

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
  # Simulate natural flow supply at the four gages and total diversions
  year<-cm.year[k,1] # year sample
  vol<-volumes[k,] # volume sample

  bwb<- bwb.wy[bwb.wy$wy == year, "bwb.nat.q"][183:365]
  bws<- bws.wy[bws.wy$wy == year, "bws.nat.q"][183:365]
  bw.div<- bws.wy[bws.wy$wy == year, "bw.div"][183:365]
  sc<- sc.wy[sc.wy$wy == year, "sc.nat"][183:365]
  sc.div <- sc.wy[sc.wy$wy == year, "sc.div"][183:365]
  cc <- cc.wy[cc.wy$wy == year, "Flow"][183:365]
  
  bwb.flow.s[,k]<-sim.flow(bwb, vol$bwb)
  bws.flow.s[,k]<-sim.flow(bws, vol$bws)
  bw.div.s[,k]<-sim.flow(bw.div, vol$div)
  sc.flow.s[,k]<-sim.flow(sc, vol$sc)
  sc.div.s[,k]<-sim.flow(sc.div, vol$sc.div)
  cc.flow.s[,k]<-sim.flow(cc, vol$cc)
}

#matplot(bwb.flow.s, type='l',col = "gray90" )
#matplot(sc.flow.s, type='l',col = "gray90" )

# ------------------------------------------------------------------------------
# Calculate prediction Intervals
numpreds<- 18 # nat+reg + div *3
pi<-data.frame(array(NA,c(183,numpreds)))
meanQ<-data.frame(array(NA,c(183,4)))

pred.int<-function(location){
  lo<-apply(location,1,quantile,0.05, na.rm=TRUE)
  hi<-apply(location,1,quantile,0.95, na.rm=TRUE)
  meanQ<-apply(location,1,mean, na.rm=TRUE)
  
  return(cbind(lo, hi, meanQ))
}

colnames(pi)<- c("bwb.low", "bwb.hi", "bwb.mean", "bws.low", "bws.hi", "bws.mean", 
                 "div.low", "div.hi", "div.mean", "sc.low", "sc.hi", "sc.mean",
                 "sc.div.low", "sc.div.hi", "sc.div.mean","cc.low", "cc.hi", "cc.mean")
pi[,1:3] <-as.data.frame(pred.int(bwb.flow.s))
pi[,4:6] <-as.data.frame(pred.int(bws.flow.s))
pi[,7:9]<-as.data.frame(pred.int(bw.div.s))
pi[,10:12]<-as.data.frame(pred.int(sc.flow.s))
pi[,13:15]<-as.data.frame(pred.int(sc.div.s))
pi[,16:18]<-as.data.frame(pred.int(cc.flow.s))

# Plot Simulation Results
# Big Wood @ Hailey
png(filename = file.path(fig_dir, "BigWood_Hailey_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(dates, pi[,3], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Natural Streamflow at Hailey", ylim=c(min(pi[,1]), max(pi[,2])))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,1], pi[,2], rev(pi[,1])), 
        col = "gray90", border = NA)
lines(dates,pi[,3],lwd=2.5,col="blue")
#flow= bwb.wy[bwb.wy$wy == pred.yr, "bwb.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()


# Big Wood @ Stanton
png(filename = file.path(fig_dir, "BigWood_Stanton_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(dates, pi[,6], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood Streamflow at Stanton Crossing", ylim=c(min(pi[,4]), max(pi[,5])))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,4], pi[,5], rev(pi[,4])), 
        col = "gray90", border = NA)
lines(dates,pi[,6],lwd=2.5,col="blue")
#flow= bws.wy[bws.wy$wy == pred.yr, "bws.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Silver Creek
png(filename = file.path(fig_dir, "Silver_creek_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(dates,pi[,12], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Silver Creek Natural Streamflow Simulation", ylim=c(min(pi[,10]), max(pi[,11])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,10], pi[,11], rev(pi[,10])), 
        col = "gray90", border = NA)
lines(dates,pi[,12],lwd=2.5,col="blue")
#flow= sc.wy[sc.wy$wy == pred.yr, "sc.nat"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Camas Creek
png(filename = file.path(fig_dir, "Camas_creek_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(dates,pi[,18], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Camas Creek Streamflow Simulation", ylim=c(min(pi[,16]), max(pi[,17])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,16], pi[,17], rev(pi[,16])), 
        col = "gray90", border = NA)
lines(dates,pi[,18],lwd=2.5,col="blue")
dev.off()
# ------------------------------------------------------------------------------
# Save Output
write.csv(bwb.flow.s, "BigWoodBullion.nat.sim.csv", row.names=dates)
write.csv(bws.flow.s, "BigWoodStanton.nat.sim.csv", row.names=dates)
write.csv(bw.div.s, "BigWoodStanton.div.sim.csv", row.names=dates)

write.csv(cc.flow.s, "CamasCreek.flow.sim.csv", row.names=dates)

write.csv(sc.flow.s, "SilverCreek.nat.sim.csv", row.names=dates)
write.csv(sc.div.s, "SilverCreek.div.sim.csv", row.names=dates)

