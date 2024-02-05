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
# timeseries of flow at each gage 

#Irrigation Season April-September streamflow in cfs
irr_cfs=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, datetime, metric, value, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10) 
           ORDER BY datetime;")

bwh.wy<-irr_cfs[irr_cfs$sitenote == "bwh", ]
bws.wy<-irr_cfs[irr_cfs$sitenote == "bws", ]
cc.wy<-irr_cfs[irr_cfs$sitenote == "cc", ]
sc.wy<-irr_cfs[irr_cfs$sitenote == "sc", ]

colnames(vol.sample)<-c("bwh.irr_vol", "bws.irr_vol","cc.irr_vol", "sc.irr_vol")

# ------------------------------------------------------------------------------
# Create arrays to store outputs of stochastic simulations

cc.flow.s<-bwh.flow.s<-bws.flow.s<-sc.flow.s<-
  data.frame(array(NA,c(183,ns)))

rownames(cc.flow.s)<-rownames(bwh.flow.s)<-rownames(bws.flow.s)<-
  rownames(sc.flow.s)<-dates

# ------------------------------------------------------------------------------
# Simulations
#
sim.flow <- function(irr.seas.flow, vol){
  "
  irr.seas.flow: streamflow from irrigation season of analog water year
  vol: total volume from bootstrap sample (ac-ft)
  "
  pred <- irr.seas.flow*vol/(sum(irr.seas.flow)*1.98)
  return (pred)
}

# select water years from CM sample and normalize by vol samples, 

for(k in 1:ns){ 
  # Simulate flow supply at the four gages
  year<-CMyear.sample[k] # year sample
  vol<-vol.sample[k,] # volume sample

  bwh<- bwh.wy[bwh.wy$wateryear == year, "value"]
  bws<- bws.wy[bws.wy$wateryear == year, "value"]
  sc<- sc.wy[sc.wy$wateryear == year, "value"]
  cc <- cc.wy[cc.wy$wateryear == year, "value"]
  
  bwh.flow.s[,k]<-sim.flow(bwh, exp(vol$bwh.irr_vol))
  bws.flow.s[,k]<-sim.flow(bws, exp(vol$bws.irr_vol))
  sc.flow.s[,k]<-sim.flow(sc, exp(vol$sc.irr_vol))
  cc.flow.s[,k]<-sim.flow(cc, exp(vol$cc.irr_vol))
}
# TODO: add an error catch if the output is NA

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

colnames(pi)<- c("bwh.low", "bwh.hi", "bwh.mean", "bwh.med","bws.low", "bws.hi", "bws.mean", "bws.med", 
                  "sc.low", "sc.hi", "sc.mean", "sc.med","cc.low", "cc.hi", "cc.mean", "cc.med")
pi[,1:4] <-as.data.frame(pred.int(bwh.flow.s))
pi[,5:8] <-as.data.frame(pred.int(bws.flow.s))
pi[,9:12]<-as.data.frame(pred.int(sc.flow.s))
pi[,13:16]<-as.data.frame(pred.int(cc.flow.s))
rownames(pi)<-dates


# ------------------------------------------------------------------------------
# Save Output -- Remove?

#paste0("ModelOutput-", end_date, ".pdf")

#write.csv(bwh.flow.s, file.path(model_out, paste0("BWH.sim-",end_date,".csv")), row.names=dates, col.names = TRUE)
#write.csv(bws.flow.s, file.path(model_out, paste0("BWS.sim-",end_date,".csv")), row.names=dates, col.names = TRUE)
#write.csv(cc.flow.s, file.path(model_out, paste0("CC.sim-", end_date,".csv")), row.names=dates, col.names = TRUE)
#write.csv(sc.flow.s, file.path(model_out, paste0("SC.sim-", end_date,".csv")), row.names=dates, col.names = TRUE)

