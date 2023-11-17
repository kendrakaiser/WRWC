# ------------------------------------------------------------------------------
# Data Management for Wood River Model 
# Kendra Kaiser
# August, 2022
# ------------------------------------------------------------------------------

# Pivot Longer
#might not need to re-read these in because they are already in the environment - need to test and clean that up throughout

# Predicted mean april-june temperature
aj_pred.temps<- read.csv(file.path(data_dir,'aj_pred.temps.csv'))
aj_pred.temps.long <- aj_pred.temps %>% pivot_longer(cols = everything(), names_to = "site", values_to = "aj.mean.t")
write.csv(aj_pred.temps.long, file.path(data_dir,'aj_pred_temps_long.csv'), row.names=FALSE)

var<-read.csv(file.path(model_out,'all_vars.csv'))
all_vars_long <- var %>% pivot_longer(cols = -c('wateryear'), names_to = c("site", "variable"), names_sep="[.]", values_to = "value")
write.csv(all_vars_long, file.path(data_dir,'all_vars_long.csv'), row.names=FALSE)


bwh.flow.simLong<- read.csv(file.path(model_out, "BWH.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
bws.flow.simLong<- read.csv(file.path(model_out, "BWS.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
cc.flow.simLong<- read.csv(file.path(model_out, "CC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
sc.flow.simLong <- read.csv(file.path(model_out, "SC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")

write.csv(bwh.flow.simLong, file.path(data_dir,'bwh.flow.simLong.csv'), row.names=FALSE)
write.csv(bws.flow.simLong, file.path(data_dir,'bws.flow.simLong.csv'), row.names=FALSE)
write.csv(cc.flow.simLong, file.path(data_dir,'cc.flow.simLong.csv'), row.names=FALSE)
write.csv(sc.flow.simLong, file.path(data_dir,'sc.flow.simLong.csv'), row.names=FALSE)


volumes<-read.csv(file.path(model_out,"vol.sample.csv"))
colnames(volumes)<-c("bwh.vol", "bws.vol","cc.vol", "sc.vol") #this has already been read in via the streamflow simulation script; re-consider re-reading it in
volumes.sampleLong<- volumes %>% pivot_longer(everything(), names_to = "site_name", values_to = "vol_af") 
write.csv(volumes.sampleLong, file.path(data_dir,'volumes.sampleLong.csv'), row.names=FALSE)

curt.sampleLong<- read.csv(file.path(model_out,"curt.sample.csv"))



