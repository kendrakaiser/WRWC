# Figures and tables for data analysis and model output from wood river streamflow forecasting

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#test
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

# Calculate exceedance probabilities and save table with labels (vol.sample is from database as logged AF)
ex.vols<- round(apply(exp(vol.sample)/1000, 2, exceed.probs, prb)) %>% as.data.frame()
ex.vols$Exceedance <- c('90%', '75%', '50%', '25%', '10%') 
ex.vols<- ex.vols%>% relocate(Exceedance)

my_table_theme <- ttheme_default(core=list(fg_params = list(col = c("red","darkorange","green3","deepskyblue", "blue3"), col=NA)))
png(file.path(fig_dir_mo,"ex.vols.png"), height = 30*nrow(ex.vols), width = 130*ncol(ex.vols))
grid.table(ex.vols, theme = my_table_theme, rows = NULL)
dev.off()

saveRDS(ex.vols, "ex.vols.rds")
ex.vols3 <- ex.vols %>%pivot_longer(!Exceedance, names_to="site", values_to="value")
ex.vols3$t<- "Predicted"

# ------------------------------------------------------------------------------
# Plot boxplots of total annual flow from each model
# ------------------------------------------------------------------------------
# Subset for plotting
vol.hist<- as.data.frame(var[var$wateryear < pred.yr ,] %>% dplyr::select(c(bwh.irr_vol, bws.irr_vol, cc.irr_vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist","Camas Creek Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/1000
vol.hist$t<- "Historic"

vol.hist.sm<-as.data.frame(var[var$wateryear < pred.yr,] %>% dplyr::select(c(sc.irr_vol)) %>% `colnames<-`(c("Silver Creek Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000
vol.hist.sm$t<- "Historic"

vol.pred <-as.data.frame(exp(vol.sample)/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
#vol.pred <- volumes.sampleLong/1000 # site_name vol_af
vol.pred$t<- "Predicted"

vol.sm<- rbind(vol.hist.sm, vol.pred[vol.pred$site == "sc.irr_vol",])
vol.sm$t <- factor(vol.sm$t)
vol.sm$site<-factor(vol.sm$site,levels = c("Silver Creek Hist","sc.irr_vol"), ordered = TRUE)

vol.cc<- rbind(vol.hist[vol.hist$site == "Camas Creek Hist",], vol.pred[vol.pred$site == "Camas Creek",])
vol.cc$t <- factor(vol.cc$t)

vol.big<- rbind(vol.hist, vol.pred[vol.pred$site != "sc.irr_vol",])
vol.big$t <- factor(vol.big$t)
vol.big$site<-factor(vol.big$site,levels = c("Big Wood Hailey Hist","bwh.irr_vol", "Big Wood Stanton Hist", "bws.irr_vol", "Camas Creek Hist", "cc.irr_vol"), ordered = TRUE)

vol.cc$site<-factor(vol.cc$site,levels = c("Camas Creek Hist", "Camas Creek" ), ordered = TRUE)


colfunc<-colorRampPalette(c("red","darkorange","green3","deepskyblue", "blue3"))

# Plot boxplots of total annual flow from each model with exceedance probabilities

p<-ggplot(vol.big, aes(x=site, y=value, fill=t), alpha=0.6) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values=c("royalblue3", "grey90")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 50),1))+
  
  geom_point(data=ex.vols3[ex.vols3$site !="sc.irr_vol",], aes(x=site, y=value, color=as.factor(Exceedance)), size=2, shape=15)+
  scale_color_manual(values=c("blue3", "deepskyblue", "green3","darkorange","red"))+
  theme_bw(base_size = 14)+
  ggtitle("Historic & Modeled Irrigation Season Volumes (April-Sept.)") +
  labs(fill="", color="Exceedance")+
  xlab("")+
  ylab("Irrigation Season Volume (KAF)") 

png(filename = file.path("sampled_volumes.png"),
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

png(filename = file.path("sampled_sc_vol.png"),
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

# This one works  --- need to replicate for all sites

sc.wy$Date <- as.Date(sc.wy$datetime)
sc.wy$doy <- yday(sc.wy$Date)

#Calculate day of water-year
water_year_begin <- ymd('1987-10-01')-1
#deal with leap years
sc.wy$doWY<- ((sc.wy$doy - yday(water_year_begin)) %% ifelse(leap_year(year(sc.wy$Date)), 366, 365)) +1

data <- sc.wy %>% group_by(doWY) %>% dplyr::mutate(meanQ=mean(value, na.rm=TRUE))

png(filename = file.path("SC_Simulation.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(dates, pi[,11], type="n", xlab="Date", ylab ="Flow (cfs)",
     main = "Silver Creek Streamflow Simulation", ylim=c(min(pi[,9]), max(pi[,10])))
polygon(c(dates[1], dates, rev(dates) ), c(pi[1,9], pi[,10], rev(pi[,9])), 
        col = "gray90", border = NA)
lines(dates,data$meanQ[1:183],lwd=1.5,col="black")
lines(dates,pi[,11],lwd=2.5,col="blue")
legend("topright", inset=.02, legend=c("Historic Avg.", "Avg Simulation"),
       col=c("black", "blue"), lty=1:1, lwd=1:2.5,cex=0.8, box.lty=0)
dev.off()

options(scipen = 999)


#-------------------------------------------------------------------------------
### Model variable plots with current conditions
#TODO: make these work automatically!
# 1) select variables from current model - working
# 2) make boxplots of each variable in the model - grouping by values that are 
# similar would reduce the number of plots, but seems like a facet wrap would work better so each can be on it's own scale?
#-------------------------------------------------------------------------------

# Silver Creek
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(volModels_db$sc_mod$vars) %>% filter(complete.cases(.))
#SC Prediction Data 
pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(volModels_db$sc_mod$vars)
pred.dat$lwd.log_swe<-exp(pred.dat$lwd.log_swe) #we dont have logged swe anymore correct?

boxplot(hist[,1])
stripchart(pred.dat[,1], pch = 19, col = 4,vertical = TRUE, add = TRUE) 
boxplot(hist[,c(4:5)])
stripchart(pred.dat[,c(4:5)], pch = 19, col = 4,vertical = TRUE, add = TRUE) 
boxplot(hist[,3])
stripchart(pred.dat[,3], pch = 19, col = 4,vertical = TRUE, add = TRUE) 

#CC historic data
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(volModels_db$cc_mod$vars) %>% filter(complete.cases(.))
#hist$ga.log_swe<- exp(hist$ga.log_swe)
#CC Prediction Data 
pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(volModels_db$cc_mod$vars)
#pred.dat$ga.log_swe<-exp(pred.dat$ga.log_swe)

boxplot(hist[,c(2:4)])
stripchart(pred.dat[,c(2:4)], pch = 19, col = 4,vertical = TRUE, add = TRUE) 
boxplot(hist[,1])
stripchart(pred.dat[,1], pch = 19, col = 4,vertical = TRUE, add = TRUE) 

boxplot(hist[,5:6])
stripchart(pred.dat[,5:6], pch = 19, col = 4,vertical = TRUE, add = TRUE) 

boxplot(hist[,7:9])
stripchart(pred.dat[,7:9], pch = 19, col = 4,vertical = TRUE, add = TRUE) 

# ------------------------------------------------------------------------------
# Plot Year to date streamflow on top of historic traces
# TODO : make these interactive to toggle between variables for comparison
# automatically shows the 3 statistically closest water years wrt timing ('cm_prob')
# provide option to look at three years with similar baseflow? 
# individual plots will be better than the facet wrap? allow user to select which basin
# ------------------------------------------------------------------------------
flow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, datetime, metric, value AS flow, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' ORDER BY datetime;")

#seq dates starting with the beginning of water year
flow <- flow %>% mutate(wyF=factor(wateryear)) %>%
  mutate(plot_date=as.Date(paste0(ifelse(month(datetime) < 10, pred.yr, pred.yr-1),
                                  "-", month(datetime), "-", day(datetime))))

flow_hist<- flow[flow$wateryear < pred.yr,]
flow_ytd<- flow[flow$wateryear == pred.yr,]

# New facet label names for supp variable
site.labs <- c("Big Wood Hailey", "Big Wood Stanton", "Camas Creek", "Silver Creek")
names(site.labs) <- c("bwh", "bws", "cc", "sc")

#FIGURE 1 facet wrap of all sites
ggplot(flow, aes(x = plot_date, y = flow, group = wateryear)) +
  geom_line(color = 'lightgrey', lwd = 0.5)+
  geom_line(data=flow_ytd,  aes(x = plot_date, y = flow, group = wateryear), lwd = 0.5, color='dodgerblue2')+
  facet_wrap(~sitenote, scales = "free", labeller = labeller(sitenote = site.labs)) +
  scale_x_date(date_labels = "%b")+
  theme_bw()+
  theme(strip.background=element_rect(fill="white"))


#single plot subset example with bwh
flow_bwh<- flow_hist[flow_hist$sitenote== 'bwh',] 
flow_bwh_ytd<- flow_ytd[flow_ytd$sitenote== 'bwh',] 

ggplot(flow_bwh, aes(x = plot_date, y = flow, group = wateryear)) +
  geom_line(lwd = 0.5, color='lightgrey')+
  geom_line(data=flow_bwh_ytd,  aes(x = plot_date, y = flow, group = wateryear), lwd = 0.5, color='dodgerblue2')+
  ylab("Big Wood Hailey Streamflow (cfs)")+
  xlab("Day of Year")+
  scale_x_date(date_labels = "%b")+
  theme_bw()+
  ylim(50,5000)




#These do NOT WORK 
# #Multiple Water Years plotted on one figure
# plot(dates, bwh.wy$value[bwh.wy$wy == 2006][183:365], xlab="Date", ylab ="Flow (cfs)", type='l', col="black", ylim=c(0,6650))
# lines(dates,bwh.wy$value[bwh.wy$wy == 2014][183:365], lwd=1, col="black")
# lines(dates,bwh.wy$value[bwh.wy$wy == 2013][183:365], lwd=1, col="black")
# lines(dates,bwh.wy$value[bwh.wy$wy == 2019][183:365], lwd=1, col="blue")
# lines(dates,bwh.wy$value[bwh.wy$wy == 1998][183:365], lwd=1, col="red")
# 
# 
# #matplot(bwh.flow.s, type='l',col = "gray90" )
# #matplot(sc.flow.s, type='l',col = "gray90" )



# # TEMP data and MODEL FIGURES
# #TODO: update with new naming conventions
# 
# #---------------
# #TODO: All of these figures will have to be updated using either new format
#
# #plot all data
# png(filename = file.path(fig_dir,"SpringTemps.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600) 
# ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()
# dev.off()
# 
# ggplot(tdata[tdata$site == "fairfield" | tdata$site == "picabo",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()
# ggplot(tdata[tdata$site == "camas creek divide" | tdata$site == "chocolate gulch",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()
# 
# #plot all data
# png(filename = file.path(fig_dir,"SummerTemps.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600) 
# ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=sum.tempF, color=site)) +geom_point()
# dev.off()
# 
# wt<-ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=wint.tempF, color=site)) +geom_point()
# 
# png(filename = file.path(fig_dir,"WinterTemps.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600) 
# print(wt)
# dev.off()
# 
# 
# subT<-tdata[tdata$site != "fairfield" & tdata$site != "picabo",]
# 
# wt_box<- ggplot(subT%>% filter(wateryear < pred.yr), aes(x=reorder(factor(site), nj.tempF, na.rm = TRUE), y=nj.tempF))+
#   geom_boxplot(alpha=0.8)+
#   theme_bw()+
#   xlab("Snotel Site")+
#   ylab("Average Nov-Jan Temperature (F)")+
#   geom_point(data = subT %>% filter(wateryear == pred.yr), aes(reorder(factor(site), nj.tempF, na.rm = TRUE), y=nj.tempF), color="blue", size=3, shape=15)+
#   coord_flip()
# 
# png(filename = file.path(fig_dir,"NovJanT_box.png"),
#     width = 5.5, height = 5.5,units = "in", pointsize = 12,
#     bg = "white", res = 600) 
# print(wt_box)
# dev.off()
# #---------------
# 
# # TODO: make a plot that represents the model better
# #plot the observed versus fitted 
# png(filename = file.path(fig_dir,"ModeledTemps.png"),
#     width = 5.5, height = 3.5,units = "in", pointsize = 12,
#     bg = "white", res = 600) 
# 
# ggplot(input, aes(x=aj_tempf, y=fitted, color=sitenote)) + 
#   geom_abline(intercept=0,lty=1)+
#   geom_point()+
#   xlim(2, 11.5)+
#   ylim(2,11.5) +
#   xlab('Observed Mean April - June Temperature (F)') +
#   ylab('Predicted Mean April - June Temperature (F)') +
#   theme_bw()
# #+geom_line(color='red', data = predicted_df, aes(y=kwh_pred.fit/1000, x=MonthlyQ/1000))
# dev.off()
# 
# ggplot(input, aes(x=spring.tempF, y=fitted)) + geom_point()+
#   geom_smooth(method = "lm", se = FALSE)+
#   xlim(2, 11.5)+
#   ylim(2,11.5) +
#   xlab('Observed Mean April - June Temperature (F)') +
#   ylab('Predicted Mean April - June Temperature (F)')+
#   theme_bw()



#----------------------
# Relationship between Big Wood at Hailey Irrigation flow and Silver Creek

ggplot(var, aes(x=bwh.irr_vol/1000, y=sc.irr_vol/1000))+
  geom_abline(intercept=24.17,slope=.06781, lty=1, color="lightgrey")+
  geom_point()+
  theme_bw()+
  xlab("Big Wood Hailey Apr-Oct Volume (KAF)")+
  ylab("Silver Creek Apr-Oct Volume (KAF)")

tst<-lm(var$sc.irr_vol ~ var$bwh.irr_vol)
summary(tst)


