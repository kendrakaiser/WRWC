# Figures and tables for data analysis and model output from wood river streamflow forecasting

#------------------------------------------------------------------------------
# Historic condition data
#TODO: update
 
wq<- alldat %>% select("year", "bwh.wq", "bws.wq", "cc.wq", "sc.wq") %>% pivot_longer(!year, names_to = "site", values_to = "winterFlow")

# Boxplots of Historic Conditions
sitelabs<- c( "Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")
wq_box<- ggplot(wq %>% filter(year < pred.yr), aes(x=factor(site), y=winterFlow))+
  geom_boxplot(alpha=0.8)+
  theme_bw()+
  xlab("USGS Site")+
  ylab("Average Nov-Jan Winter Flow (cfs)")+
  geom_point(data = wq %>% filter(year == pred.yr),  aes(x=factor(site), y=winterFlow), color="blue", size=3, shape=15)+
  scale_x_discrete(labels= sitelabs)

png(filename = file.path(fig_dir,"wq_box.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(wq_box)
dev.off()

#------------------------------------------------------------------------------

#model output version
vol.sample<-read.csv(file.path(model_out,"vol.sample.csv"))
#database input version
volumes<-vol.sample
colnames(volumes)<-c("bwh.vol", "bws.vol","cc.vol", "sc.vol") #this has already been read in via the streamflow simulation script; re-consider re-reading it in
volumes.sampleLong<- volumes %>% pivot_longer(everything(), names_to = "site_name", values_to = "vol_af") 

#model output
var<-read.csv(file.path(model_out,'all_vars.csv'))
#database input
all_vars_long <- var %>% pivot_longer(cols = -c('year'), names_to = "variable", values_to = "value")
write.csv(all_vars_long, file.path(data_dir,'all_vars_long.csv'), row.names=FALSE)
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
  # find the volume of each exceedance
  ex.vols=vols[ix]
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
vol.hist<- as.data.frame(var[var$year < pred.yr ,] %>% dplyr::select(c(bwh.vol, bws.vol, cc.vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist","Camas Creek Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/1000
vol.hist$t<- "Historic"
vol.hist.sm<-as.data.frame(var[var$year < pred.yr,] %>% dplyr::select(c(sc.vol)) %>% `colnames<-`(c("Silver Creek Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000
vol.hist.sm$t<- "Historic"

vol.pred <-as.data.frame(vol.sample/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
vol.pred <- volumes.sampleLong/1000 # site_name vol_af
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



#-------------------------------------------------------------------------------
### Streamflow Simulation Figures
#-------------------------------------------------------------------------------

### Average Annual streamflow with maximum and minimum daily flows
streamflow_data$Date <- as.Date(streamflow_data$datetime)
streamflow_data$doy <- yday(streamflow_data$Date)

#Calculate day of water-year
water_year_begin <- ymd('1987-10-01')-1
#deal with leap years
streamflow_data$doWY<- ((streamflow_data$doy - yday(water_year_begin)) %% ifelse(leap_year(year(streamflow_data$Date)), 366, 365)) +1


data <- streamflow_data %>% filter(abv == 'bwh') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))

minBW=min(pi[,1], pi[,5],data$meanQ[457:639])
maxBW=max(pi[,2], pi[,6],data$meanQ[457:639])
# Big Wood @ Hailey
png(filename = file.path(fig_dir_mo, "bwh_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,3], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Big Wood River Streamflow at Hailey", ylim=c(minBW, maxBW))
polygon(c(dates[1], dates, rev(dates)), c(pi[1,1], pi[,2], rev(pi[,1])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[457:639],lwd=1.5,col="black")
lines(dates,pi[,3],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= bwh.wy[bwh.wy$wy == pred.yr, "bwh.nat.q"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Big Wood @ Stanton
data <- streamflow_data %>% filter(abv == 'bws') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))
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

# Silver Creek
data <- streamflow_data %>% filter(abv == 'sc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))

# Silver Creek
png(filename = file.path(fig_dir_mo, "SC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,11], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Silver Creek Streamflow Simulation", ylim=c(min(pi[,9]), max(pi[,10])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,9], pi[,10], rev(pi[,9])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[91:273],lwd=1.5,col="black")
lines(dates,pi[,11],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
#flow= sc.wy[sc.wy$wy == pred.yr, "sc.nat"]
#lines(dates,flow[183:365], lwd=2, col="green")
dev.off()

# Camas Creek

data <- streamflow_data %>% filter(abv == 'cc') %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))
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


#Multiple Water Years plotted on one figure
plot(dates, bwh.wy$value[bwh.wy$wy == 2006][183:365], xlab="Date", ylab ="Flow (cfs)", type='l', col="black", ylim=c(0,6650))
lines(dates,bwh.wy$value[bwh.wy$wy == 2014][183:365], lwd=1, col="black")
lines(dates,bwh.wy$value[bwh.wy$wy == 2013][183:365], lwd=1, col="black")
lines(dates,bwh.wy$value[bwh.wy$wy == 2019][183:365], lwd=1, col="blue")
lines(dates,bwh.wy$value[bwh.wy$wy == 1998][183:365], lwd=1, col="red")


#matplot(bwh.flow.s, type='l',col = "gray90" )
#matplot(sc.flow.s, type='l',col = "gray90" )