# ---------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# January 18th, 2021
#
# Linear models are run to predict total April-September streamflow volume and 
# center of mass based on average winter flows, current SWE and predicted temperature; 
# 
# This model was informed by the statistical tools developed for the Henry's Fork 
# Foundation by Rob VanKirk
# TODO: NEED TO ERROR CHECK THAT pred.params.vol are NOT NA
# -----------------------------------------------------------------------------  

# Import Data ------------------------------------------------------------------  
# var= read.csv(file.path(data_dir,'var_feb.csv'))
# usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
temp.ran = read.csv(file.path(data_dir,'aj_pred.temps.csv'))

#TODO: why change naming conventions here? change for consistency?
# Load the models and parameters from all the models 
vol.params <<- list.load(file.path(data_dir, vol_params))
vol.mods <<- list.load(file.path(data_dir, vol_mods))
cm.params <<- list.load(file.path(data_dir,cm_params))
cm.mods <<- list.load(file.path(data_dir, cm_mods))

# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beginning of year in accordance with my calculation of cm, consider changing to day of wy
wy<-seq(as.Date(paste(pred.yr,"-01-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")
wy<-data.frame(wy,1:length(wy))
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------  
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),3))
rownames(output.vol)<-stream.id
output.vol<-output.vol[-4,]
colnames(output.vol)<-c("Winter Vol\n% of mean", "Pred. Vol (KAF)", "Pred. Vol \n% of mean")
rownames(output.vol)<-c("Big Wood Hailey","Big Wood Stanton","Camas Creek","Silver Creek")

pred.params.vol<-array(NA,c(4,4))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma", "low.log.vol", "upp.log.vol")

# center of mass
output.cm<-data.frame(array(NA,c(5,4)))
rownames(output.cm)<-stream.id
output.cm<-output.cm[-4,]
colnames(output.cm)<-c("SWE % of mean","cm","cm-mean","cm.date") #"Hist Nov-Jan Temp","Nov-Jan Temps",

pred.params.cm<-array(NA,c(4,2))
rownames(pred.params.cm)<-c("bwb.cm","bws.cm","cc.cm","sc.cm")
colnames(pred.params.cm)<-c("cm","sigma")

# summary stats
mod_sum<-data.frame(array(NA,c(6,2)))
colnames(mod_sum)<-c("Vol Adj-R2", "CM Adj-R2")
rownames(mod_sum)<-c("bwb","bws","cc","sc", "bw.div", "sc.div")

# ------------------------------------------------------------------------------  
#
# April-Sept Volume Predictions
# an additional function can be made to clean these up now that they are all automated
# ------------------------------------------------------------------------------ # 

modOut<- function(mod, pred.dat, wq.cur, wq, vol, hist.swe, lastQ){
  '
  mod:     input model
  pred.dat: data.frame of prediction variables
  wq.cur:   this years winter baseflow
  wq:       array of historic winter flows (e.g. hist$cc.wq)
  vol:      array of historic april-sept volumes  (hist$cc.vol)
  hist.swe: mean(arrays of historic SWE from ws snotel sites) #mean(hist$ccd+hist$sr, na.rm=T)
  lastQ:    last years summer streamflow volume (ac-ft) #var$cc.vol[var$year == pred.yr-1] 
  '
  pred.params.vol<-array(NA,c(1,4))
  output.vol<-array(NA,c(1,3))
  
  meanSWE <- mean(hist.swe, trim=0, na.rm=TRUE)
  sig<-summary(mod)$sigma
  pred.params.vol[1,2]<-sig #^2/2 #lognormal residuals
  #predict this years total volume at 95 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=TRUE,interval="prediction",level=0.95)
  pred.params.vol[1,1]<-predictions$fit[1] #mean prediction 
  pred.params.vol[1,3]<-predictions$fit[2] #lower prediction interval
  pred.params.vol[1,4]<-predictions$fit[3] #upper prediction interval
  #ADD ERROR CHECK HERE IF NA then STOP
  
  #This years percent of mean winter flow
  output.vol[1,1]<-round(wq.cur/mean(wq, na.rm=TRUE)*100,0)
  
  # back-transformation of log-transformed data to expected value in original units; 183 is the number of days between April-Sept and 1.98 converts back to cfs
  output.vol[1,2]<-round(exp(predictions$fit[1])/1000,0) #+sig^2/2 , with lognormal residuals
  #Division by long-term mean to generate % of average volume
  output.vol[1,3]<-round(exp(predictions$fit[1])/mean(vol, na.rm=TRUE) *100,0) # +sig^2/2 , with lognormal residuals
  
  return(list(output.vol, pred.params.vol))
}

# --------------------------------------------------
# Subset Big Wood Variables
hist <- var[var$year < pred.yr,] %>% dplyr::select(c(bwb.vol, vol.params$bwh$vars)) %>% filter(complete.cases(.))
swe_cols <- hist %>% dplyr::select(contains('swe'))

#Prediction Data
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(vol.params$bwh$vars) 

# Big Wood at Hailey Model output
mod_sum[1,1]<-summary(vol.mods$bwh_mod)$adj.r.squared
mod_out<- modOut(vol.mods$bwh_mod, pred.dat, var$bwb.wq[var$year == pred.yr], var$bwb.wq[var$year < pred.yr], hist$bwb.vol, mean(colMeans(swe_cols, na.rm=T)), var$bwb.vol[var$year == pred.yr-1])
#these could be formatted differently to be saved to the gloabl env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]

# --------------------------------------------------
# Subset Big Wood at Stanton Winter flows, Snotel from Galena & Galena Summit, Hyndman
hist <- var[var$year < pred.yr,] %>% dplyr::select(c(bws.vol, vol.params$bws$vars)) %>% filter(complete.cases(.))
swe_cols <- hist %>% dplyr::select(contains('swe'))

#  bws Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(vol.params$bws$vars) 

# Big Wood at Stanton Flow Model output 
mod_sum[2,1]<-summary(vol.mods$bws_mod)$adj.r.squared
mod_out<- modOut(vol.mods$bws_mod, pred.dat, var$bws.wq[var$year == pred.yr], var$bws.wq[var$year < pred.yr], hist$bws.vol, mean(colMeans(swe_cols, na.rm=T)), var$bws.vol[var$year == pred.yr-1])
output.vol[2,] <- mod_out[[1]] #prediction plus sigma^2
mod_out[[2]][,2]<- mod_out[[2]][,2]^2 #manually lognormalizing the sigma
pred.params.vol[2,] <- mod_out[[2]]

# --------------------------------------------------
# Subset Silver Creek 
hist <- var[var$year < pred.yr,] %>% dplyr::select(c(sc.vol, vol.params$sc$vars)) %>% filter(complete.cases(.))
swe_cols <- hist %>% dplyr::select(contains('swe'))

# SC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(vol.params$sc$vars) 

# Silver Creek Model output
mod_sum[4,1]<-summary(vol.mods$sc_mod)$adj.r.squared
mod_out<- modOut(vol.mods$sc_mod, pred.dat, var$sc.wq[var$year == pred.yr], var$sc.wq[var$year < pred.yr], hist$sc.vol, mean(colMeans(swe_cols, na.rm=T)), var$sc.vol[var$year == pred.yr-1])
output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]

# --------------------------------------------------
# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek divide was not included in model selection 
hist <- var[var$year < pred.yr,] %>% dplyr::select(cc.vol, vol.params$cc$vars) %>% filter(complete.cases(.))
swe_cols <- hist %>% dplyr::select(contains('swe'))

#CC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(vol.params$cc$vars)

# Camas Creek Model output
mod_sum[3,1]<-summary(vol.mods$cc_mod)$adj.r.squared
mod_out<- modOut(vol.mods$cc_mod, pred.dat, var$cc.wq[var$year == pred.yr], var$cc.wq[var$year < pred.yr], hist$cc.vol, mean(colMeans(swe_cols, na.rm=T)), var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]


# ------------------------------------------------------------------------------  
#
# Center of Mass Predictions
#
# ------------------------------------------------------------------------------ # 
modOutcm<- function(mod.cm, pred.dat, hist.temps, cur.temps, hist.cm, pred.swe, hist.swe){
  '
  mod.cm:           input model
  pred.dat:         data.frame of prediction variables
  cur.temps:        numeric of winter temperatures
  hist.temps:       historic temperature data
  hist.cm:          historic cm
  pred.swe:         current swe used for prediction
  hist.swe:         historic swe
  '
  pred.params.cm<-array(NA,c(1,2))
  output.cm<-data.frame(percSWE=integer(), predCM=integer(), diffCM=integer(), dateCM=character())
  sig<-summary(mod.cm)$sigma
  
  predictions<-predict(mod.cm,newdata=pred.dat)
  
  pred.params.cm[1,1]<-mean(predictions)
  pred.params.cm[1,2]<-sqrt(sig^2+var(predictions)) 
  if (is.na(pred.params.cm[1,2])){pred.params.cm[1,2]<-sig}
  
  #output.cm[1,1]<-mean(apply(hist.temps, MARGIN=2, mean)) 
  #output.cm[1,2]<-as.list(round(cur.temps,3))

  output.cm[1,1]<-  if (length(grep('swe',names(mod.cm$coefficients))) >0) {
    round(mean(as.matrix(pred.swe), na.rm=TRUE)/mean(as.matrix(hist.swe), na.rm =TRUE),3)*100
    } else {'No SWE Param'}
    #round(sum(pred.swe, na.rm=TRUE)/mean(as.matrix(hist.swe), na.rm =T),3) 
  output.cm[1,2]<-round(mean(predictions),0) 
  output.cm[1,3]<-round(mean(predictions)-mean(hist.cm,na.rm=TRUE),0) 
  output.cm[1,4]<-format(wy$Date[wy$day==round(mean(predictions, na.rm=TRUE),0)],"%b-%d")
  
  return(list(output.cm, pred.params.cm))
}

# Big Wood at Hailey center of mass
sub_params<- cm.params$bwh$vars[-grep('aj', cm.params$bwh$vars)]
aj_params<-cm.params$bwh$vars[grep('aj', cm.params$bwh$vars)]
hist <- var[var$year < pred.yr,] %>% dplyr::select(bwb.cm, cm.params$bwh$vars) %>% filter(complete.cases(.))

# Prediction Data with modeled temperature data
pred.data<-var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::slice(rep(1:n(), 5000))
pred.data[aj_params] <- temp.ran[aj_params]

# Big Wood Hailey Model output
mod_sum[1,2]<-summary(cm.mods$bwh_cm.mod)$adj.r.squared
mod_out<- modOutcm(cm.mods$bwh_cm.mod, pred.data, hist%>% dplyr::select(contains('nj')), 
                   (var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$bwb.cm, var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('swe')), 
                   hist%>% dplyr::select(contains('swe')))
output.cm[1,] <- mod_out[[1]]
pred.params.cm[1,] <- mod_out[[2]]

# --------------------
# Big Wood at Stanton
# 
sub_params<- cm.params$bws$vars[-grep('aj', cm.params$bws$vars)]
aj_params<-cm.params$bws$vars[grep('aj', cm.params$bws$vars)]
hist <- var[var$year < pred.yr,] %>% dplyr::select(bws.cm, cm.params$bws$vars) %>% filter(complete.cases(.))

# Prediction Data with modeled temperature data
pred.data<-var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
pred.data[aj_params] <- temp.ran[aj_params]

# Big Wood Stanton Model output
mod_sum[2,2]<-summary(cm.mods$bws_cm.mod)$adj.r.squared
mod_out<- modOutcm(cm.mods$bws_cm.mod, pred.data, hist%>% dplyr::select(contains('nj')), 
                   (var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$bws.cm, var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('swe')), 
                   hist%>% dplyr::select(contains('swe')))
output.cm[2,] <- mod_out[[1]]
pred.params.cm[2,] <- mod_out[[2]]

# --------------------
# Silver Creek Center of Mass
#
# added 'if' statement here because March SC CM doesn't use aj temperatures
if (is.integer(grep('aj', cm.params$sc$vars))){ 
  sub_params<- cm.params$sc$vars[-grep('aj', cm.params$sc$vars)]
  aj_params<-cm.params$sc$vars[grep('aj', cm.params$sc$vars)]
  # Prediction Data with modeled temperature data
  pred.data<-var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
  pred.data[aj_params] <- temp.ran[aj_params]
} else {
  sub_params<- cm.params$sc$vars
  pred.data<-var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params))
}

hist <- var[var$year < pred.yr,] %>% dplyr::select(sc.cm, cm.params$sc$vars) %>% filter(complete.cases(.))

# Silver Creek  Model output
mod_sum[4,2]<-summary(cm.mods$sc_cm.mod)$adj.r.squared
mod_out<- modOutcm(cm.mods$sc_cm.mod, pred.data, hist%>% dplyr::select(contains('nj')), 
                   (var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$sc.cm, var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('swe')), 
                   hist%>% dplyr::select(contains('swe')))
output.cm[4,] <- mod_out[[1]]
pred.params.cm[4,] <- mod_out[[2]]

# --------------------
# Camas Creek Center of Mass

sub_params<- cm.params$cc$vars[-grep('aj', cm.params$cc$vars)]
aj_params<-cm.params$cc$vars[grep('aj', cm.params$cc$vars)]
hist <- var[var$year < pred.yr,] %>% dplyr::select(cc.cm, cm.params$cc$vars) %>% filter(complete.cases(.))

#Prediction Data with modeled temperature data
pred.data<-var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
pred.data[aj_params] <- temp.ran[aj_params]

# Camas Creek Model output
mod_sum[3,2]<-summary(cm.mods$cc_cm.mod)$adj.r.squared
mod_out<- modOutcm(cm.mods$cc_cm.mod, pred.data, hist%>% dplyr::select(contains('nj')), 
                   (var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$cc.cm, var[var$year == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('swe')), 
                   hist%>% dplyr::select(contains('swe')))
output.cm[3,] <- mod_out[[1]]
pred.params.cm[3,] <- mod_out[[2]]

### Save model outputs 
# --------------------
png(file.path(fig_dir_mo,"pred.volumes.png"), height = 25*nrow(output.vol), width = 130*ncol(t(output.vol[,2:3])))
grid.table(t(output.vol[,2:3]))
dev.off()

png(file.path(fig_dir_mo,"pred.cm.png"), height = 30*nrow(output.vol), width = 90*ncol(output.vol))
grid.table(output.cm)
dev.off()

write.csv(output.vol, file.path(model_out,"pred.output.vol.csv"),row.names=T)
write.csv(pred.params.vol, file.path(model_out,"pred.params.vol.csv"),row.names=T)
write.csv(output.cm, file.path(model_out,"pred.output.cm.csv"),row.names=T)
write.csv(pred.params.cm, file.path(model_out,"pred.params.cm.csv"),row.names=T)

# ------------------------------------------------------------------------------
# Draw a sample of volumes and water years with similar timing
# ------------------------------------------------------------------------------
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume and center of mass (timing)
# between each gage

# calculate correlations between flow conditions across the basins
flow.data = var[var$year >= 1997 & var$year < 2022,] %>% dplyr::select(bwb.vol, 
      bwb.cm, bws.vol, bws.cm, cc.vol, cc.cm, sc.vol, sc.cm) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(cbind(flow.data[c(1,3,5,7)],flow.data[c(2,4,6,8)]),use="pairwise.complete")
#pred.params.vol[2,2]<-0.2
# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol[,1:2], pred.params.cm)
outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (ac-ft)
vol.sample<-data.frame(mvrnorm(n=5000,mu=(pred.params.vol[,1]),Sigma=cov.mat[1:4,1:4]))
colnames(vol.sample)<-c("Big Wood Hailey", "Big Wood Stanton","Camas Creek","Silver Creek")
write.csv(exp(vol.sample), file.path(model_out,paste0("vol.sample-", end_date,".csv")),row.names=F)

# save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir_mo,"correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()

# save output from correlations
write.csv(cov.mat, file.path(model_out,"cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(model_out,"pred.pars.csv"),row.names=T)

# ------------------------------------------------------------------------------
# Calculate Exceedance Probabilities
# ------------------------------------------------------------------------------

exceed.probs<- function(vols, probs){
  'calculate exceedance probabilities of model output
  p=m/(n+1)
  vols: numeric of volumes
  probs: array of probabilities to calculate'
  
  n=length(vols) 
  m=round(probs*(n+1))
  #rank the sampled volumes
  ranks<- rank(vols)
  # find the index of which volume goes with each exceedance
  ix=match(m, ranks)
  # find the actual volume of each exceedance
  ex.vols=exp(vols[ix])
  return(ex.vols)
}

# Exceedance probs from NWRFC
prb<- c(0.1, 0.25, 0.5, 0.75, 0.9)
# Calculate exceedance probabilities and save table with labels
ex.vols<- round(apply(vol.sample, 2, exceed.probs, prb)/1000) %>% as.data.frame()
ex.vols$Exceedance <- c('90%', '75%', '50%', '25%', '10%') 
ex.vols<- ex.vols%>% relocate(Exceedance)

my_table_theme <- ttheme_default(core=list(fg_params = list(col = c("red","darkorange","green3","deepskyblue", "blue3"), col=NA)))
png(file.path(fig_dir_mo,"ex.vols.png"), height = 30*nrow(ex.vols), width = 130*ncol(ex.vols))
grid.table(ex.vols, theme = my_table_theme, rows = NULL)
dev.off()

#saveRDS(round(ex.vols,0), file.path(fig_dir_mo,"ex.vols.rds"))
ex.vols3 <- ex.vols %>%pivot_longer(!Exceedance, names_to="site", values_to="value")
ex.vols3$t<- "Predicted"
# ------------------------------------------------------------------------------
# Plot boxplots of total annual flow from each model
# ------------------------------------------------------------------------------
# Subset for plotting
vol.hist<- as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(bwb.vol, bws.vol, cc.vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist","Camas Creek Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/1000
vol.hist$t<- "Historic"
vol.hist.sm<-as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(sc.vol)) %>% `colnames<-`(c("Silver Creek Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000
vol.hist.sm$t<- "Historic"

vol.pred <-as.data.frame(exp(vol.sample)/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
vol.pred$t<- "Predicted"

vol.big<- rbind(vol.hist, vol.pred[vol.pred$site != "Silver Creek",])
vol.sm<- rbind(vol.hist.sm, vol.pred[vol.pred$site == "Silver Creek",])
vol.cc<- rbind(vol.hist[vol.hist$site == "Camas Creek Hist",], vol.pred[vol.pred$site == "Camas Creek",])
vol.cc$t <- factor(vol.cc$t)
vol.big$t <- factor(vol.big$t)
vol.sm$t <- factor(vol.sm$t)
vol.big$site<-factor(vol.big$site,levels = c("Big Wood Hailey Hist","Big Wood Hailey", "Big Wood Stanton Hist", "Big Wood Stanton", "Camas Creek Hist", "Camas Creek" ), ordered = TRUE)
vol.sm$site<-factor(vol.sm$site,levels = c("Silver Creek Hist","Silver Creek"), ordered = TRUE)
vol.cc$site<-factor(vol.cc$site,levels = c("Camas Creek Hist", "Camas Creek" ), ordered = TRUE)


colfunc<-colorRampPalette(c("red","darkorange","green3","deepskyblue", "blue3"))

# Plot boxplots of total annual flow from each model with exceedance probabilities
  
p<-ggplot(vol.big, aes(x=site, y=value, fill=t), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 50),1))+

  geom_point(data=ex.vols3[ex.vols3$site !="Silver Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=15)+
  scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  theme_bw(base_size = 14)+
  ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
  labs(fill="", color="Exceedance")+
  xlab("")+
  ylab("Irrigation Season Volume (KAF)") 

png(filename = file.path(fig_dir_mo,"sampled_volumes.png"),
    width = 6.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(p)
dev.off()

saveRDS(p, file.path(fig_dir_mo,"sampled_volumes.rds"))

ps<- ggplot(vol.sm, aes(x=site, y=value, fill=site), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.sm$value, na.rm=TRUE), by = 10),1))+
  geom_point(data=ex.vols3[ex.vols3$site =="Silver Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=15)+
  scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  
  theme_bw(base_size = 18)+
  theme(legend.position="none") +
  ggtitle("") +
  labs(fill="", color="Exceedance")+
  xlab("")+
  ylab("Irrigation Volume (KAF)")

png(filename = file.path(fig_dir_mo,"sampled_sc_vol.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(ps)
dev.off()

saveRDS(ps, file = file.path(fig_dir_mo, paste0("sampled_sc_vol-", end_date, ".rds")))

pc<- ggplot(vol.cc, aes(x=site, y=value, fill=site), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.cc$value, na.rm=TRUE), by = 20),1))+
  geom_point(data=ex.vols3[ex.vols3$site =="Camas Creek",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=15)+
  scale_color_manual(values=c("red","darkorange","green3","deepskyblue", "blue3"))+
  
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("") +
  labs(fill="", color="Exceedance")+
  xlab("")+
  ylab("Irrigation Volume (KAF)")

png(filename = file.path(fig_dir_mo,"sampled_cc_vol.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(pc)
dev.off()


# ------------------------------------------------------------------------------
# Create distribution and draw samples of CENTER of MASS & Volume
# ------------------------------------------------------------------------------
# Draw sample of years with similar center of mass (timing)
cm.data = var[var$year >= 1997 & var$year < pred.yr,]
cm.data = cm.data %>% dplyr::select(year, bwb.cm, bws.cm,cc.cm, sc.cm) 
cm.data$prob<-NA

# pmvnorm calculates the distribution function of the multivariate normal distribution
for(i in 1:dim(cm.data)[1]){
  vec<-cm.data[i,2:5] # center of mass at each site for a given year
  cm.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-pred.params.cm[,2], # use the mean or location specific
                           upper=as.numeric(vec)+pred.params.cm[,2],mean=pred.params.cm[,1],sigma=cov.mat[5:8,5:8])[1] #need to adjust this to have the upper and lower limits be wider? e.g right now the upper and lowers are only one day off of the original??
}
cm.data$prob<-cm.data$prob/sum(cm.data$prob)
# create normal distribution of years 
CMyear.sample<-sample(cm.data$year,5000,replace=TRUE) 
# Of the 5000 replicates show the percentage that each year represents
cm_sum<-as.data.frame(summary(as.factor(CMyear.sample))/5000)
colnames(cm_sum)<- c("% of sample")
cm_sum<- cm_sum*100
png(file.path(fig_dir_mo,"CM_summary.png"), height = 50*nrow(cm_sum), width = 200*ncol(cm_sum))
grid.table(cm_sum)
dev.off()
CMyear.sample<-as.data.frame(CMyear.sample)
#save the probabilities for use in the simulation
write.csv(CMyear.sample, file.path(model_out, paste0("CMyear.sample-", end_date,".csv")),row.names=F)

# this version makes the probabilities based on pvnorm distribution, 
# this is not used in the simulations, but is useful for reference
CMyear.sample.prob<-sample(cm.data$year,5000,replace=TRUE, prob=cm.data$prob) 
cm_prob<-as.data.frame(summary(as.factor(CMyear.sample.prob))/5000)
colnames(cm_prob)<- c("% of sample")
cm_prob<- cm_prob*100
png(file.path(fig_dir_mo,"CM_summary_prob.png"), height = 50*nrow(cm_prob), width = 200*ncol(cm_prob))
grid.table(cm_prob)
dev.off()


# Draw sample of years with similar volume for comparison -- 
#TODO: these are too dependent on how the pmvnorm bounds are defined, need to come up with something else
# of note - the bounds are +/- 1 in log scale
vol.data = var[var$year >1996 & var$year < pred.yr,]%>% dplyr::select(year, bwb.vol, bws.vol, cc.vol, sc.vol) 
vol.data$prob<-NA

b = seq(0.20, 1.05, .05) 
std.bwh<- matrix()
s.means<-matrix(0,1,4)
# pmvnorm calculates the distribution function of the multivariate normal distribution
for(j in b){
  vol.diff<-matrix(0,1,4)
  for(i in 1:dim(vol.data)[1]){
    vec<-log(vol.data[i,2:5])
    vol.data$prob[i]<-pmvnorm(lower=as.numeric(vec-j),  # +/- standard error (pred.params.vol$sigma) would return effectively one year
                              upper=as.numeric(vec+j), 
                              mean=pred.params.vol[,1],corr=cor.mat[1:4,1:4])[1]
    vol.diff<- rbind(vol.diff, exp(as.numeric(vec+j)) - exp(as.numeric(vec-j)))
  }
  s.means<- rbind(s.means, colMeans(vol.diff[2:19,]))
  vol.sample.prob<-sample(vol.data$year, 5000, replace=TRUE, prob=vol.data$prob) 
  year.samp<- as.data.frame(vol.sample.prob) %>% `colnames<-`('year')
  vol.sampl<-left_join(year.samp, vol.data)
  std.bwh<- rbind(std.bwh, sd(vol.sampl$bwb.vol))
}
s.means<-s.means[2:19,]
std.bwh<-std.bwh[2:19]
plot(b, std.bwh)
plot(std.bwh, s.means[,1]/1000)

vol_prob<-as.data.frame(summary(as.factor(vol.sample.prob))/5000)*100
colnames(vol_prob)<- c("% of sample")

png(file.path(fig_dir_mo,"vol_prob.png"), height = 50*nrow(vol_prob), width = 200*ncol(vol_prob))
grid.table(vol_prob)
dev.off()

# do a sensitivity analysis on the value (0.5) in pmvnorm


