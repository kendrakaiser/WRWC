# ---------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# January 18th, 2021
#
# Linear models to predict total April-September streamflow volume and center of mass,
# based on average winter flows, current SWE and predicted spring temperature; 
# model selection explored in 'streamflow_model_exploration.R' using BIC.
# Linear model are also used to predict total diversions in the Big Wood and Silver Creek
#
# This model was informed by the statistical tools developed for the Henry's Fork 
# Foundation by Rob VanKirk
# -----------------------------------------------------------------------------  
rm.all.but(c("cd", "pred.yr", "run_date", "git_dir", "fig_dir", "input_dir", 
             "data_dir", "input", "fig_dir_mo", "author", "todays_date"))
# Import Data ------------------------------------------------------------------  
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
swe_q = read.csv(file.path(data_dir,input))
swe_q[swe_q == 0] <- NA # change zeros to a value so lm works
temps = read.csv(file.path(data_dir, 'ajTemps.csv'))
var = swe_q %>% dplyr::select(-X) %>% inner_join(temps, by ="year") %>% dplyr::select(-X)
var$div <- var$abv.h + var$abv.s
curtailments = read.csv(file.path(input_dir,'historic_shutoff_dates_071520.csv'))
var$bws.vol[var$year == 1996] <- NA #only a partial year of data

write.csv(var, file.path(cd,'March_output/all_vars.csv'))

temp.ran = read.csv(file.path(data_dir,'aj_pred.temps.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beginning of year in accordance with my calculation of cm, consider changin to day of wy
wy<-seq(as.Date(paste(pred.yr,"-01-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")
wy<-data.frame(wy,1:length(wy))
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------  
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),7))
rownames(output.vol)<-stream.id
output.vol<-output.vol[-4,]
colnames(output.vol)<-c("Winter Vol\n% of mean", "Pred. Vol \nKAF", "Pred. Vol \n% of mean", "90% exc. \nKAF", "90% exc. \n% of mean", "Prev Year \nVol KAF", "Prev Year \n% of mean Volume")
rownames(output.vol)<-c("Big Wood Hailey","Big Wood Stanton","Camas Creek","Silver Creek")

pred.params.vol<-array(NA,c(4,2))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma")

# summary stats
mod_sum<-data.frame(array(NA,c(6,1)))
colnames(mod_sum)<-c("Vol Adj-R2")
rownames(mod_sum)<-c("bwb","bws","cc","sc", "bw.div", "sc.div")

# ------------------------------------------------------------------------------  
#
# April-Sept Volume Predictions
#
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
  pred.params.vol<-array(NA,c(1,2))
  output.vol<-array(NA,c(1,7))
  
  meanSWE <- mean(hist.swe, trim=0, na.rm=TRUE)
  sig<-summary(mod)$sigma
  pred.params.vol[1,2]<-sig
  #predict this years total volume at 95 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=TRUE,interval="prediction",level=0.95)
  pred.params.vol[1,1]<-mean(predictions$fit, na.rm=TRUE)
  #This years percent of mean winter flow
  output.vol[1,1]<-round(wq.cur/mean(wq, na.rm=TRUE),2)*100 
  #percent of mean SWE
  #output.vol[1,2]<-round(sum(swe, na.rm=TRUE)/meanSWE,3) *100
  # back-transformation of log-transformed data to expected value in original units, with lognormal residuals; 183 is the number of days between April-Sept and 1.98 converts back to cfs
  output.vol[1,2]<-round(exp(predictions$fit[1]+sig^2/2),0) /1000
  #Division by long-term mean to generate % of average volume, with lognormal residuals
  output.vol[1,3]<-round(exp(predictions$fit[1]+sig^2/2)/mean(vol, na.rm=TRUE),3) *100
  
  #this years total volume at 80 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=TRUE,interval="prediction",level=0.9)
  #bottom of 90% CI (statisticians) converted ac-ft
  output.vol[1,4]<-round(exp(predictions$fit[2]),0)/1000 #(1.98*183)
  # 90% exceedance flow as a percent of long-term mean
  output.vol[1,5]<-round(exp(predictions$fit[2])/mean(vol, na.rm=TRUE),3) *100
  output.vol[1,6]<-round(lastQ,0) # last years volume in ac-ft
  output.vol[1,7]<-round(lastQ/mean(vol, na.rm=TRUE),3)*100 # Last years percent of average historic volume
  return(list(output.vol, pred.params.vol))
}

# --------------------------------------------------
# Big Wood Hailey variables  
# vol natural is only correct prior to 2020 because of diversion data
hist <- var[var$year < 2020,] %>% dplyr::select(bwb.vol.nat, gs.swe) 
# linear model
bwb_mod<-lm(log(bwb.vol.nat)~ log(gs.swe), data=hist) 
mod_sum[1,1]<-summary(bwb_mod)$adj.r.squared
# March 1 bwb Prediction Data
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(gs.swe) 

# Big Wood at Hailey Model output
mod_out<- modOut(bwb_mod, pred.dat, var$bwb.wq[var$year == pred.yr], var$bwb.wq[var$year < pred.yr], hist$bwb.vol.nat, hist$gs.swe, var$bwb.vol.nat[var$year == pred.yr-1])
#these could be formatted differently to be saved to the global env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir,"March/BWB_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bwb_mod))
plot(var$bwb.vol.nat[var$year < 2020]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \n April-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < 2020 & var$year > 1996,] %>% dplyr::select(bws.vol.nat, bws.wq, gs.swe) 
# Big Wood at Stanton linear model
bws_mod<-lm(log(bws.vol.nat)~ bws.wq+ gs.swe, data=hist) 
mod_sum[2,1]<-summary(bws_mod)$adj.r.squared
# March 1 BWS Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(bws.wq, gs.swe) 

# Big Wood at Stanton Natural Flow Model output
mod_out<- modOut(bws_mod, pred.dat, var$bws.wq[var$year == pred.yr], hist$bws.wq, hist$bws.vol.nat, hist$gs.swe, var$bws.vol.nat[var$year == pred.yr-1])
output.vol[2,] <- mod_out[[1]]
pred.params.vol[2,] <- mod_out[[2]]

#Plot modeled bws data for visual evaluation 
png(filename = file.path(fig_dir,"March/BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bws_mod))
plot(var$bws.vol.nat[var$year < 2020 & var$year > 1996]/1000,c(fits)/1000, lwd=2, xlim=c(0,730), ylim=c(0,730), xlab="Observed", ylab="Predicted",main="Big Wood at Stanton \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Swede Peak
hist <- var[var$year < 2020,] %>% dplyr::select(sc.vol.nat, sc.wq, sp.swe, hc.swe) 
# Silver Creek linear model, note mixture of SWE from Big Wood and Little Wood basins
sc_mod<-lm(log(sc.vol.nat)~ sp.swe + hc.swe + sc.wq, data=hist)
mod_sum[4,1]<-summary(sc_mod)$adj.r.squared

# April 1 SC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(sc.wq, sp.swe, hc.swe) 
# Silver Creek Model output
mod_out<- modOut(sc_mod, pred.dat, var$sc.wq[var$year == pred.yr], var$sc.wq[var$year < pred.yr], hist$sc.vol.nat, c(hist$sp.swe,hist$hc.swe), var$sc.vol.nat[var$year == pred.yr-1])
output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]

# Plot sc modeled data for visual evaluation 
png(filename = file.path(fig_dir,"March/SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(sc_mod))
plot(hist$sc.vol.nat[complete.cases(hist)]/100,c(fits)/100, lwd=2, xlim=c(310,720), ylim=c(310,720), xlab="Observed", ylab="Predicted", main="Silver Creek \nApril-Sept Streamflow Vol (100 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek divide was not included in model selection 
hist <- var[var$year < pred.yr,] %>% dplyr::select(cc.vol, cc.wq, ccd.swe) #vol is in ac-ft
# Camas Creek linear model
cc_mod<-lm(log(cc.vol)~log(cc.wq)+ccd.swe, data=hist) 
mod_sum[3,1]<-summary(cc_mod)$adj.r.squared
# February 1 CC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(cc.wq, ccd.swe)
# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, var$cc.wq[var$year == pred.yr], hist$cc.wq, hist$cc.vol, hist$ccd.swe, var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

#Plot CC modeled data for visual evaluation 
png(filename = file.path(fig_dir,"March/CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(cc_mod))
plot(var$cc.vol[complete.cases(hist)]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()


### Save model outputs for simulation runs 

png(file.path(fig_dir,"March/pred.volumes.png"), height = 30*nrow(output.vol), width = 90*ncol(output.vol))
grid.table(output.vol[,1:5])
dev.off()

write.csv(output.vol, file.path(cd,"March_output/pred.output.vol.csv"),row.names=T)
write.csv(pred.params.vol, file.path(cd,"March_output/pred.params.vol.csv"),row.names=T)

# --------------------
# Apr-Sept diversion predictions
#

pred.params.div<-array(NA,c(1,2))
colnames(pred.params.div)<-c("log.vol","sigma")
rownames(pred.params.div)<-c("sc.div")

# Total Big Wood Diversions ----

# draw from random normal distribution
var.div<- var$div[!is.na(var$div)]
bw.div.sample<-sample(var.div,5000,replace=TRUE) 

# Silver Creek Diversions ----
hist <- var[var$year>1993 & var$year < 2020,] %>% dplyr::select(sc.div, sc.wq, g.swe, lwd.swe, t.f, t.p) 
hist$temps<- rowMeans(cbind(hist$t.f, hist$t.p), na.rm=TRUE)
# linear model 
sc.div_mod<-lm(log(var$sc.div[var$year>1993 & var$year < 2020]) ~ log(sc.wq)+ temps+log(g.swe)+log(lwd.swe), data=hist) 
mod_sum[6,1] <- summary(sc.div_mod)$adj.r.squared 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% dplyr::select(sc.wq, g.swe, lwd.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.cc

# Model output
preds.div<-predict(sc.div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[1,1]<-preds.div$fit[1]
pred.params.div[1,2]<-mean(preds.div$se.fit)

png(filename = file.path(fig_dir,"March/SC_Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
fits<-fitted(sc.div_mod)
plot(var$sc.div[var$year>1993 & var$year < 2020],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(3300, 8200), ylim=c(3300, 8200))
abline(0,1,col="gray50",lty=1)
dev.off()


mod_sum<- round(mod_sum, 3)
png(file.path(fig_dir,"March/model_summary.png"), height = 25*nrow(mod_sum), width = 110*ncol(mod_sum))
grid.table(mod_sum)
dev.off()


# --------------------
# Draw a sample of volumes and water years with similar timing
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume at each gage

# check correlations between flow conditions across the basins
flow.data = var[var$year >= 1997  & var$year < 2020,] %>% dplyr::select(bwb.vol.nat, bws.vol.nat, cc.vol, sc.vol.nat, sc.div) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(flow.data, use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol, pred.params.div)
outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (ac-ft)
vol.pars<-rbind(pred.params.vol, pred.params.div)
vol.sample<-mvrnorm(n=5000,mu=(vol.pars[,1]),Sigma=cov.mat)
colnames(vol.sample)<-c("bwb.nat","bws.nat","cc","sc.nat", "sc.div")
#Add the big wood diversion sample
vol.sample2<- cbind(vol.sample, log(bw.div.sample))
colnames(vol.sample2)<-c("Big Wood Hailey", "Big Wood Stanton","Camas Creek","Silver Creek", "Silver Creek Div", "Big Wood Div")
write.csv(exp(vol.sample2), file.path(cd,"March_output/vol.sample.csv"),row.names=F)

#save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir,"March/correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()
# save output from correlations
write.csv(cov.mat, file.path(cd,"March_output/cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(cd,"March_output/pred.pars.csv"),row.names=T)

# Subset for plotting
vol.hist<- as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(bwb.vol.nat, bws.vol.nat, cc.vol)) %>% `colnames<-`(c("Big Wood Hailey Hist", "Big Wood Stanton Hist","Camas Creek Hist")) %>%pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist$value<-vol.hist$value/10000
vol.hist.sm<-as.data.frame(var[var$year < 2020,] %>% dplyr::select(c(sc.vol.nat, sc.div, div)) %>% `colnames<-`(c("Silver Creek Hist", "Silver Creek Div Hist", "Big Wood Div Hist")) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") )
vol.hist.sm$value<-vol.hist.sm$value/1000
colnames(vol.sample2)<-c("Big Wood Hailey Pred", "Big Wood Stanton Pred","Camas Creek Pred", "Silver Creek Pred", "Silver Creek Div Pred", "Big Wood Div Pred")

vol.pred <-as.data.frame(exp(vol.sample2[,1:3])/10000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
vol.pred.sm <- as.data.frame(exp(vol.sample2[,4:6])/1000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value")

vol.big<- rbind(vol.hist, vol.pred)
vol.sm<- rbind(vol.hist.sm, vol.pred.sm)

mo_fig_dir = file.path(fig_dir, 'March')

# Plot boxplots of total annual flow from each model
png(filename = file.path(mo_fig_dir,"sampled_volumes.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

vol.big %>%
  ggplot(aes(x=site, y=value, fill=site)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("grey90","blue", "grey90","blue", "grey90","blue")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.big$value, na.rm=TRUE), by = 10),1))+
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Sampled Irrigation Season Volumes") +
  xlab("")+
  ylab("Irrigation Season Volume (10,000 ac-ft)")
dev.off()

print(file.path(mo_fig_dir,"sampled_volumes.png"))

png(filename = file.path(mo_fig_dir,"sampled_sc_diversions.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

vol.sm %>%
  ggplot(aes(x=site, y=value, fill=site)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("grey90","blue", "grey90","blue", "grey90","blue")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(breaks = round(seq(0, max(vol.sm$value, na.rm=TRUE), by = 10),1))+
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("") +
  xlab("")+
  ylab("Irrigation Volume (1,000 ac-ft)")
dev.off()

# Draw sample of years with similar center of mass (timing)
cm.data = var[var$year >= 1997 & var$year < 2020,]
cm.data = cm.data %>% dplyr::select(year, bwb.cm.nat, bws.cm.nat,cc.cm, sc.cm) 

# create normal distribution of years 
CMyear.sample<-sample(cm.data$year,5000,replace=TRUE) 
#save the probabilities for use in the simulation
write.csv(CMyear.sample, file.path(cd,"March_output/CMyear.sample.csv"),row.names=F)

# Create distribution of most similar analog years (by volume) for reference
# TODO: add cm and total WY flow probabilities - this will require individual cm models


