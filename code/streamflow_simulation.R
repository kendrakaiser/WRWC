# ---------------------------------------------------------------------------- #
# WRWC Streamflow simulation
# Kendra Kaiser
# November 24, 2020
# Stochastic model to simulate Big Wood, Camas and Silver Creks
# from April 1 - September 30
# Uses modeled temperature data from linear random effects model
# uses random diversion based on predicted diversion
# Reach gain

cd <<- '~/Desktop/Data/WRWC'
pred.yr <<- 2020 #Simyear
ns<-5000  #Number of simulations

dates<-seq(as.Date(paste(Simyear,"-04-01",sep="")),as.Date(paste(Simyear,"-09-30",sep="")),"day")

# ------------------------------------------------------------------------------
# Import data
# mean ET from each basin -- or from FAFI / PICO
# volume & bootstraps for each gage
# natural flow and diversion records from which we will select the runoff timing
# distributions and diversion hydrographs. 
# Random values of runoff years (based on CM) and runoff volumes. * alreadsy selected where?
#

cm.year<-read.csv("CMyear.sample.csv")
volumes<-read.csv("flow.sample.csv")
Div.year<-read.csv("Diversion.years.csv")


# ------------------------------------------------------------------------------
# Create arrays to store outputs of stochastic simulations
#

cc.vol.s<-cc.flow.s<-bwb.vol.s<-bwb.flow.s<-bws.vol.s<-bws.flow.s<-sc.vol.s<-
  sc.flow.s<-data.frame(array(NA,c(183,ns)))

rownames(cc.vol.s)<-rownames(cc.flow.s)<-rownames(bwb.vol.s)<-rownames(bwb.flow.s)
  <-rownames(bws.vol.s)<-rownames(bws.flow.s)<-rownames(sc.vol.s)<-
  rownames(sc.flow.s)<-dates

# ------------------------------------------------------------------------------
# Simulations
#
sim.flow <- function(year, nat.wy, vol){
  "
  year: selected cm.year from bootstrap sample
  nat.wy: bootstraped set of predictions for natural flow (or regulated flow depending on location)
  vol: selected flow from bootstrap sample

  "
  pred <- nat.wy[analogWY]*vol/sum(nat.wy[analogWY])*1.98
  return (pred)
}

for(k in 1:ns){ 
  # Calculate natural flow supply at the four gages
  year<-cm.year[k,1]
  vol<-volumes[k,] #flow sample

  bwb.nat.s[,k]<-sim.flow(year, bwb.nat.wy, vol$bwb)
  bws.nat.s[,k]<-sim.flow(year, bws.nat.wy, vol$bws)
  sc.nat.s[,k]<-sim.flow(year, sc.nat.wy, vol$sc)
  cc.nat.s[,k]<-sim.flow(year, cc.nat.wy, vol$cc)
  
  # Calculate diversions at the four gages
  yearDiv<-div.year[k,1]
  
  bwb.div.s[,k] <- bwb.div.iy[yearDiv]
  bws.div.s[,k]<- bws.div.iy[yearDiv]
  sc.div.s[,k]<- sc.div.iy[yearDiv]
}

# ------------------------------------------------------------------------------
# Calculate prediction Intervals
numpreds<- 6 # nat+reg + div *2
pi<-data.frame(array(NA,c(183,numpreds)))
meanQ<-data.frame(array(NA,c(183,4)))

pred.int<-(location){
  lo<-apply(location,1,quantile,0.05)
  hi<-apply(location,1,quantile,0.95)
  meanQ<-apply(location,1,mean)
  return(lo, hi, meanQ)
}

pi[,"cc"] <-pred.int(cc.nat.s)

# ------------------------------------------------------------------------------
# Save Output
write.csv(bwb.nat.s, "BigWoodBullion.nat.sim.csv", row.names=dates)
write.csv(bwb.vol.s, "BigWoodBullion.vol.sim.csv", row.names=dates)
write.csv(bwb.div.s, "BigWoodBullion.div.sim.csv", row.names=dates)

write.csv(bws.nat.s, "BigWoodStanton.nat.sim.csv", row.names=dates)
write.csv(bws.vol.s, "BigWoodStanton.vol.sim.csv", row.names=dates)
write.csv(bws.div.s, "BigWoodStanton.div.sim.csv", row.names=dates)

write.csv(cc.nat.s, "CamasCreek.nat.sim.csv", row.names=dates)
write.csv(cc.vol.s, "CamasCreek.vol.sim.csv", row.names=dates)
write.csv(cc.div.s, "CamasCreek.div.sim.csv", row.names=dates)

write.csv(sc.nat.s, "SilverCreek.nat.sim.csv", row.names=dates)
write.csv(sc.vol.s, "SilverCreek.vol.sim.csv", row.names=dates)
write.csv(sc.div.s, "SilverCreek.div.sim.csv", row.names=dates)

