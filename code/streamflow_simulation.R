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

for(k in 1:ns){ 
  # First calculate natural flow supply at the four gages
  year<-cm.year[k,1]
  vol<-volumes[k,] #flow sample
  
  bwb.nat.s[,k]<-bwb.nat.wy[analog wy]* vol$bwb /sum(bwb.nat.wy[analog wy])*1.98
  bws.nat.s[,k]<-bws.nat.wy[analog wy]* vol$bws /sum(bws.nat.wy[analog wy])*1.98
  sc.nat.s[,k]<-sc.nat.wy[analog wy]* vol$sc /sum(sc.nat.wy[analog wy])*1.98
  cc.nat.s[,k]<-cc.nat.wy[analog wy]* vol$cc /sum(cc.nat.wy[analog wy])*1.98
   
  bwb.vol.s[,k]<-bwb.vol.wy[analog wy]* vol$bwb /sum(bwb.vol.wy[analog wy])*1.98
  bws.vol.s[,k]<-bws.vol.wy[analog wy]* vol$bws /sum(bws.vol.wy[analog wy])*1.98
  sc.vol.s[,k]<-sc.vol.wy[analog wy]* vol$sc /sum(sc.vol.wy[analog wy])*1.98
  cc.vol.s[,k]<-cc.vol.wy[analog wy]* vol$cc /sum(cc.vol.wy[analog wy])*1.98
  
  yearDiv<-div.year[k,1]
  
  bwb.div.s[,k] <- bwb.div.iy[analogwy]
  bws.div.s[,k]<- bws.div.iy[analogwy]
  sc.div.s[,k]<- sc.div.iy[analogwy]
}

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

