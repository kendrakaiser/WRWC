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
all_vars_long <- var %>% pivot_longer(cols = -c('year'), names_to = "variable", values_to = "value")
write.csv(all_vars_long, file.path(data_dir,'all_vars_long.csv'), row.names=FALSE)


bwh.flow.simLong<- read.csv(file.path(model_out, "BWB.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
bws.flow.simLong<- read.csv(file.path(model_out, "BWS.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
cc.flow.simLong<- read.csv(file.path(model_out, "CC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")
sc.flow.simLong <- read.csv(file.path(model_out, "SC.sim.csv")) %>% pivot_longer(cols = -c(1), names_to = "simulation", values_to = "dailyFlow")

