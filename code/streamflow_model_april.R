# ---------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# October 27th, 2020
# Linear models to predict total April-September streamflow volume and center of mass 
# based on average winter flows, current SWE and temperature; model selection explored
# 'streamflow_model_xploration.R' using BIC
#
# This model was informed by the statstical tools developed for the Henry's Fork 
# Foundation by Rob VanKirk
# -----------------------------------------------------------------------------  

source(file.path("~/github/WRWC/code", "packages.R"))

rm(list=ls())

cd = '~/Desktop/Data/WRWC'
fig_dir = file.path(cd, "figures")
pred.yr <- 2019

# Import Data ------------------------------------------------------------------  
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(cd,'usgs_sites.csv'))
swe_q = read.csv(file.path(cd,'all_dat.csv'))
swe_q[swe_q == 0] <- 0.00001 # change zeros to a value so lm works
temps = read.csv(file.path(cd, 'ajTemps.csv'))
var = swe_q %>% select(-X) %>% inner_join(temps, by ="year") %>% select(-X)
var$div <- var$abv.h + var$abv.s
curtailments = read.csv(file.path(cd,'historic_shutoff_dates_071520.csv'))

write.csv(var, file.path(cd,'April1_vars.csv'))

temp.ran = read.csv(file.path(cd,'aj_pred.temps.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beginning of year in accordance with my calculation of cm, consider changin to day of wy
wy<-seq(as.Date("2019-01-01"),as.Date("2019-09-30"),"day") #update to automate based on new.yr
wy<-data.frame(wy,1:273)
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------  
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),8))
rownames(output.vol)<-stream.id
output.vol<-output.vol[-4,]
colnames(output.vol)<-c("Predictors Vol % of mean","Predictors swe % of mean", "Pred. Vol (cfs)", "Pred. Vol % of mean", "90% exc. cfs", "90% exc. % or mean", "Prev Year Vol (cfs)", "Prev Year % of mean Volume")

pred.params.vol<-array(NA,c(4,2))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma")

# center of mass
output.cm<-data.frame(array(NA,c(5,5)))
rownames(output.cm)<-stream.id
output.cm<-output.cm[-4,]
colnames(output.cm)<-c("temp-mean","% of mean","cm","cm-mean","cm.date")

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
#
# ------------------------------------------------------------------------------ # 

modOut<- function(mod, pred.dat, wq, vol, meanSWE, lastQ){
  '
  mod:     input model
  pred.dat: data.frame of prediction variables
  wq:       array of historic winter flows (e.g. hist$cc.wq)
  vol:      array of historic april-sept volumes  (hist$cc.vol)
  meanSWE:  mean(arrays of historic SWE from ws snotel sites) #mean(hist$ccd+hist$sr, na.rm=T)
  lastQ:    last years summer streamflow volume (ac-ft) #var$cc.vol[var$year == pred.yr-1] 
  '
  pred.params.vol<-array(NA,c(1,2))
  output.vol<-array(NA,c(1,8))
  
  sig<-summary(mod)$sigma
  pred.params.vol[1,2]<-sig
  #predict this years total volume at 95 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.vol[1,1]<-mean(predictions$fit, na.rm=T)
  #This years percent of mean winter flow
  output.vol[1,1]<-round(pred.dat[1,1]/mean(wq, na.rm=T),3) 
  #percent of mean SWE
  output.vol[1,2]<-round(sum(pred.dat[1,2:3])/meanSWE,3) 
  # back-transformation of log-transformed data to expected value in original units, with lognormal residuals; 183 is the number of days between April-Sept and 1.98 converts back to cfs
  output.vol[1,3]<-round(exp(predictions$fit[1]+sig^2/2)/(1.98*183),0) 
  #Division by long-term mean to generate % of average volume, with lognormal residuals
  output.vol[1,4]<-round(exp(predictions$fit[1]+sig^2/2)/mean(vol, na.rm=T),3) 
  
  #this years total volume at 80 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.8)
  #bottom of 80% CI (statisticians) converted to cfs
  output.vol[1,5]<-round(exp(predictions$fit[2])/(1.98*183),0) 
  # 90% exceedance flow as a percent of long-term mean
  output.vol[1,6]<-round(exp(predictions$fit[2])/mean(vol, na.rm=T),3) 
  output.vol[1,7]<-round(lastQ/(1.98*183),0) # last years volume in cfs
  output.vol[1,8]<-round(lastQ/mean(vol, na.rm=T),3) # Last years percent of average historic volume
  return(list(output.vol, pred.params.vol))
}

# --------------------------------------------------
# Subset Big Wood Winter flows, Snotel from  Galena & Galena Summit, Hyndman
hist <- var[var$year < pred.yr,] %>% select(bwb.vol.nat, g.swe, gs.swe, hc.swe) 
# Big Wood at Hailey linear model
bwb_mod<-lm(log(bwb.vol.nat)~ g.swe+ log(gs.swe)+ hc.swe, data=hist) 
mod_sum[1,1]<-summary(bwb_mod)$adj.r.squared

#April 1 bwb Prediction Data
pred.dat<-var[var$year == pred.yr,] %>% select(g.swe, gs.swe, hc.swe) 

# Big Wood at Hailey Model output
mod_out<- modOut(bwb_mod, pred.dat, var$bwb.wq[var$year < pred.yr], hist$bwb.vol.nat, mean(hist$g.swe,  hist$gs.swe,  hist$hc.swe, trim=0, na.rm=T), var$bwb.vol.nat[var$year == pred.yr-1])
#these could be formatted differently to be saved to the gloabl env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir,"BWB_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bwb_mod))
plot(var$bwb.vol.nat[var$year < pred.yr]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Big Wood at Stanton Winter flows, Snotel from Galena & Galena Summit, Hyndman
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.vol.nat, bws.wq, g.swe, gs.swe, hc.swe) 
# Big Wood at Stanton linear model
bws_mod<-lm(log(bws.vol.nat)~bws.wq+ log(g.swe) + log(gs.swe)+ log(hc.swe), data=hist) 
mod_sum[2,1]<-summary(bws_mod)$adj.r.squared

#April 1 bws Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(bws.wq, g.swe, gs.swe, hc.swe) 

# Big Wood at Stanton Natural Flow Model output
mod_out<- modOut(bws_mod, pred.dat, hist$bws.wq, hist$bws.vol.nat, mean(hist$g.swe,  hist$gs.swe,  hist$hc.swe, trim=0, na.rm=T), var$bws.vol.nat[var$year == pred.yr-1])
output.vol[2,] <- mod_out[[1]]
pred.params.vol[2,] <- mod_out[[2]]

#Plot modeled bws data for visual evaluation 
png(filename = file.path(fig_dir,"BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bws_mod))
plot(var$bws.vol.nat[var$year < pred.yr & var$year > 1996]/1000,c(fits)/1000, lwd=2, xlim=c(0,730), ylim=c(0,730), xlab="Observed", ylab="Predicted",main="Big Wood at Stanton \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.vol.nat, sc.wq, ga.swe, sp.swe, cg.swe, hc.swe) 
# Silver Creek linear model, note mixture of SWE from Big Wood and Little Wood basins
sc_mod<-lm(log(sc.vol.nat)~ sc.wq + ga.swe+ sp.swe + cg.swe+log(hc.swe), data=hist)
mod_sum[4,1]<-summary(sc_mod)$adj.r.squared

# April 1 SC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(sc.wq, ga.swe, g.swe, hc.swe, bwb.wq) 
# Silver Creek Model output
mod_out<- modOut(sc_mod, pred.dat, hist$sc.wq, hist$sc.vol.nat, mean(hist$ga.swe,  hist$g.swe,  hist$hc.swe, trim=0, na.rm=T), var$sc.vol.nat[var$year == pred.yr-1])
output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]

# Plot sc modeled data for visual evaluation 
png(filename = file.path(fig_dir,"SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(sc_mod))
plot(hist$sc.vol.nat[complete.cases(hist)]/100,c(fits)/100, lwd=2, xlim=c(310,720), ylim=c(310,720), xlab="Observed", ylab="Predicted", main="Silver Creek \nApril-Sept Streamflow Vol (100 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek divide was not included in model selection 
hist <- var[var$year < pred.yr,] %>% select(cc.vol, cc.wq, ccd.swe, sr.swe) #vol is in ac-ft
# Camas Creek linear model
cc_mod<-lm(log(cc.vol)~log(cc.wq)+sr.swe+ccd.swe, data=hist) 
mod_sum[3,1]<-summary(cc_mod)$adj.r.squared

#April 1 CC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(cc.wq, ccd.swe, sr.swe)

# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, hist$cc.wq, hist$cc.vol, mean(hist$sr.swe, na.rm=T), var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

#Plot CC modeled data for visual evaluation 
png(filename = file.path(fig_dir,"CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(cc_mod))
plot(var$cc.vol[complete.cases(hist)]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()


# ------------------------------------------------------------------------------  
#
# Center of Mass Predictions
#
# ------------------------------------------------------------------------------ # 
modOutcm<- function(mod.cm, pred.dat, pred.dat.temps, hist.temps, hist.cm, pred.swe, hist.swe){
  '
  mod.cm:           input model
  pred.dat:         data.frame of prediction variables
  pred.data.temps:  vector of modeled temperature data 
  hist.temps:       concatenated vectors of historic temperature data
  hist.cm:          historic cm
  '
  pred.params.cm<-array(NA,c(1,2))
  output.cm<-array(NA,c(1,5))
  sig<-summary(mod.cm)$sigma
  
  predictions<-predict(mod.cm,newdata=pred.dat)
  
  pred.params.cm[1,1]<-mean(predictions)
  pred.params.cm[1,2]<-sqrt(sig^2+var(predictions)) 
  
  output.cm[1,1]<-round(mean(pred.dat.temps)-mean(hist.temps,na.rm=T),3)
  output.cm[1,2]<-round(sum(pred.swe)/mean(as.matrix(hist.swe), na.rm =T),3) 
  output.cm[1,3]<-round(mean(predictions),0) 
  output.cm[1,4]<-round(mean(predictions)-mean(hist.cm,na.rm=T),0) 
  output.cm[1,5]<-format(wy$Date[wy$day==round(mean(predictions),0)],"%b-%d")
  
  return(list(output.cm, pred.params.cm))
}

# Big Wood at hailey center of mass
hist <- var[var$year < pred.yr,] %>% select(bwb.cm.nat, bwb.wq, g.swe, hc.swe, t.g, t.gs, t.lw, cg.swe, gs.swe) 
hist$temps <-rowMeans(cbind(hist$t.g, hist$t.gs, hist$t.lw), na.rm=TRUE)
# BW Hailey center of mass linear model
bwb_mod.cm <-lm(bwb.cm.nat ~ log(bwb.wq) + g.swe+ hc.swe+ temps+ log(cg.swe)+log(gs.swe), data=hist)
mod_sum[1,2]<-summary(bwb_mod.cm)$adj.r.squared 

# April 1 Prediction Data with modeled temperature data
params<-var[var$year == pred.yr,] %>% select(bwb.wq, g.swe, hc.swe, cg.swe, gs.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.bwh

# Big Wood Hailey Model output
mod_out<- modOutcm(bwb_mod.cm, pred.dat, pred.dat$temps, c(hist$t.g,hist$t.gs,hist$t.lw), 
                   hist$bwb.cm.nat, params[2:5], cbind(hist[,3:4], hist[,8:9]))
output.cm[1,] <- mod_out[[1]]
pred.params.cm[1,] <- mod_out[[2]]

#Plot modeled data for visual evaluation 
png(filename = file.path(fig_dir,"BWB_CMmodelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-fitted(bwb_mod.cm)
plot(var$bwb.cm.nat[complete.cases(hist)],c(fits), lwd=2, xlab="Observed", ylab="Predicted", main="Big Wood at Hailey \n Center of Mass", xlim=c(144, 165), ylim=c(144,165))
abline(0,1,col="gray50",lty=1)
dev.off()
# --------------------
# Big Wood at Stanton
# 
hist <- var[var$year < pred.yr,] %>% select(bws.cm.nat, lwd.swe, cg.swe, hc.swe, t.cg, t.g, t.hc, t.lw) 
hist$temps <-rowMeans(cbind(hist$t.g, hist$t.g, hist$t.hc), na.rm=TRUE)
# BWS linear model - update temperature options if time allows, dropped from 0.934 to 0.62 with the lme temp
bws_mod.cm <-lm(bws.cm.nat ~ lwd.swe +log(cg.swe)+log(hc.swe) + temps, data=hist)
mod_sum[2,2]<-summary(bws_mod.cm)$adj.r.squared 

# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% select(lwd.swe, cg.swe, hc.swe) 
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.bws

# BWS Model output
mod_out<- modOutcm(bws_mod.cm, pred.dat, pred.dat$temps, c(hist$t.cg,hist$t.g,hist$t.lw), 
                   hist$bws.cm.nat, params, hist[,2:4])
output.cm[2,] <- mod_out[[1]]
pred.params.cm[2,] <- mod_out[[2]]

#Plot modeled data for visual evaluation 
png(filename = file.path(fig_dir,"BWS_CMmodelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-fitted(bws_mod.cm)
plot(var$bws.cm.nat[complete.cases(hist)],c(fits), lwd=2, xlab="Observed", ylab="Predicted", main="Big Wood at Stanton \n Center of Mass", xlim=c(140, 165), ylim=c(140,165))
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------
# Subset Silver Creek Winter flows, Snotel from Chocolate Gulch, Hyndaman, Lost Wood Div. & Swede Peak
#
hist <- var[var$year < pred.yr,] %>% select(sc.cm, sc.wq, cg.swe, hc.swe, lwd.swe, sp.swe, t.cg, t.gs) 
hist$temps <-rowMeans(cbind(hist$t.cg, hist$t.gs), na.rm=TRUE)
# Silver Creek linear model
# note here that this includes swe from the big wood and the little wood 
sc_mod.cm<-lm(sc.cm~ cg.swe+ hc.swe+ lwd.swe+temps+log(sp.swe)+log(sc.wq), data=hist) 
mod_sum[4,2]<-summary(sc_mod.cm)$adj.r.squared 

# April 1 SC Prediction Data 
params<-var[var$year == pred.yr,] %>% select(sc.wq, cg.swe, hc.swe, lwd.swe, sp.swe) 
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.sc

# SC Model output
mod_out<- modOutcm(sc_mod.cm, pred.dat, pred.dat$temps, c(hist$t.cg,hist$t.gs), 
                   hist$sc.cm, params[2:5], hist[,3:6])
output.cm[4,] <- mod_out[[1]]
pred.params.cm[4,] <- mod_out[[2]]

#Plot modeled data for visual evaluation 
png(filename = file.path(fig_dir,"SC_CMmodelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-fitted(sc_mod.cm)
plot(var$sc.cm[complete.cases(hist)],c(fits), lwd=2, xlab="Observed", ylab="Predicted", main="Silver Creek Center of Mass", xlim=c(139, 181), ylim=c(139,181))
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------
# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek 
# divide & temperature from Fairfield agrimet site
hist <- var[var$year < pred.yr,] %>% select(cc.cm, ccd.swe, sr.swe, t.f) 
# Camas Creek linear model
cc_mod.cm<-lm(cc.cm~ccd.swe + sr.swe+ t.f, data=hist) 
mod_sum[3,2]<-summary(cc_mod.cm)$adj.r.squared 

# April 1 CC Prediction Data 
params<- var[var$year == pred.yr,] %>% select(ccd.swe, sr.swe) 
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$t.f<- temp.ran$aj.temps.cc

# Camas Creek Model output
mod_out<- modOutcm(cc_mod.cm, pred.dat, pred.dat$t.f, hist$t.f, hist$cc.cm, params, hist[,2:3])
output.cm[3,] <- mod_out[[1]]
pred.params.cm[3,] <- mod_out[[2]]

#Plot modeled data for visual evaluation 
png(filename = file.path(fig_dir,"CC_CMmodelFit.png"),
width = 5.5, height = 5.5,units = "in", pointsize = 12,
bg = "white", res = 600, type ="quartz") 

fits<-fitted(cc_mod.cm)
plot(var$cc.cm[complete.cases(hist)],c(fits), lwd=2, xlab="Observed", ylab="Predicted", 
     main="Camas Creek Center of Mass", xlim=c(110, 138), ylim=c(110,138))
abline(0,1,col="gray50",lty=1)
dev.off()

### Save model outputs for simulation runs (is this actually used later?)

write.csv(output.vol, file.path(cd,"pred.output.vol.csv"),row.names=T)
write.csv(pred.params.vol, file.path(cd,"pred.params.vol.csv"),row.names=T)
write.csv(output.cm, file.path(cd,"pred.output.cm.csv"),row.names=T)
write.csv(pred.params.cm, file.path(cd,"pred.params.cm.csv"),row.names=T)

# --------------------
# Apr-Sept diversion & reach gain predictions
#

pred.params.div<-array(NA,c(2,2))
colnames(pred.params.div)<-c("log.vol","sigma")
rownames(pred.params.div)<-c("bw.div", "sc.div")

# Above Hailey -----
png(filename = file.path(fig_dir,"Div.abv.Hailey.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(var$year, var$abv.h, xlab="Year", ylab="Diversions (ac-ft)")
dev.off()

plot(var$year[var$year >=2000], var$abv.h[var$year >=2000])
hist <- var[var$year >=2000 & var$year < pred.yr,] %>% select(abv.h, g.swe, hc.swe, t.cg, t.lw) 
# linear model 
div_mod.h<-lm(abv.h~ g.swe+hc.swe+t.cg+t.lw, data=hist) 
mod_sum[4,1]<-summary(div_mod.h)$adj.r.squared 
# April 1 Prediction Data 
pred.dat<- var[var$year == pred.yr,] %>% select(g.swe, hc.swe, t.cg, t.lw) 
# Model output
preds.div<-predict(div_mod.h,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
#preds.params.div[1,1]<-preds.div$fit[1]
#preds.params.div[1,2]<-preds.div$se.fit
#preds.params.div.[1,3]<-cor(dat$Total.Div,dat$Reach.Gain)

png(filename = file.path(fig_dir,"Div.abv.Hailey_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-fitted(div_mod.h)
plot(var$abv.h[var$year >=2000 & var$year < pred.yr],c(fits), xlab="Observed", 
     ylab="Predicted", xlim=c(5200, 11350), ylim=c(5200, 11350))
abline(0,1,col="gray50",lty=1)
dev.off()

# Above Stanton Crossing the lm does really poorly (0.36) - draw randomly or use a lm that includes natural flow estimate
png(filename = file.path(fig_dir,"Div.abv.Stanton.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(var$year, var$abv.s, xlab="Year", ylab="Diversions (ac-ft)")
dev.off()

mod_sum[5,1]<-0.36
# losses between Hailey and Stanton
png(filename = file.path(fig_dir,"Losses.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
plot(var$year, var$bws.loss, xlab="Year", ylab="Annual Losses (ac-ft)")
dev.off()

plot(var$year[var$year >=2000], var$bws.loss[var$year >=2000])

hist <- var[var$year >=2000 & var$year < pred.yr,] %>% select(bws.loss, g.swe, hc.swe, t.g, t.cg, t.gs) 
# linear model 
bws.loss_mod<-lm(bws.loss~ g.swe+hc.swe+t.cg+ t.gs+t.gs, data=hist) 
mod_sum[6,1]<-summary(bws.loss_mod)$adj.r.squared 
# April 1 Prediction Data 
pred.dat<- var[var$year == pred.yr,] %>% select(g.swe,hc.swe,t.cg, t.gs,t.gs) 
# Model output
preds.div<-predict(bws.loss_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

png(filename = file.path(fig_dir,"Losses_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
fits<-fitted(bws.loss_mod)
plot(var$bws.loss[var$year >=2000 & var$year < pred.yr],c(fits), xlab="Observed", 
     ylab="Predicted", xlim=c(-73000, -39900), ylim=c(-73000, -39900))
abline(0,1,col="gray50",lty=1)
dev.off()

# Total Big Wood Diversions ----

hist <- var[var$year >=1997 & var$year < pred.yr,] %>% select(div, bws.wq, cg.swe, hc.swe) 
# linear model 
div_mod<-lm(log(var$div[var$year >=1997 & var$year < pred.yr]) ~ log(cg.swe)+log(hc.swe)+log(bws.wq), data=hist) 
mod_sum[5,1] <- summary(div_mod)$adj.r.squared 
# April 1 Prediction Data 
pred.dat<- var[var$year == pred.yr,] %>% select(bws.wq, cg.swe, hc.swe) 
# Model output
preds.div<-predict(div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[1,1]<-preds.div$fit[1]
pred.params.div[1,2]<-preds.div$se.fit

png(filename = file.path(fig_dir,"Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
fits<-fitted(div_mod)
plot(var$div[var$year >=1997 & var$year < pred.yr],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(32500, 58800), ylim=c(32500, 58800))
abline(0,1,col="gray50",lty=1)
dev.off()

# Silver Creek Diversions ----
# g.swe, t.cg, t.gs,t.hc, log.cg, log.lwd
hist <- var[var$year>1993 & var$year < pred.yr,] %>% select(sc.div, g.swe, cg.swe, lwd.swe, t.cg, t.gs, t.hc) 
hist$temps<- rowMeans(cbind(hist$t.cg, hist$t.gs, hist$t.hc), na.rm=TRUE)
# linear model 
sc.div_mod<-lm(log(var$sc.div[var$year>1993 & var$year < pred.yr]) ~ g.swe+ temps+log(cg.swe)+log(lwd.swe), data=hist) 
mod_sum[6,1] <- summary(sc.div_mod)$adj.r.squared #not gonna work 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% select(g.swe, cg.swe, lwd.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.scd

# Model output
preds.div<-predict(sc.div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[2,1]<-preds.div$fit[1]
pred.params.div[2,2]<-mean(preds.div$se.fit)

png(filename = file.path(fig_dir,"SilverCreek_Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
fits<-fitted(sc.div_mod)
plot(var$sc.div[var$year>1993 & var$year < pred.yr],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(3300, 8200), ylim=c(3300, 8200))
abline(0,1,col="gray50",lty=1)
dev.off()


mod_sum<- round(mod_sum, 3)
png(file.path(fig_dir,"model_summary.png"), height = 25*nrow(mod_sum), width = 100*ncol(mod_sum))
grid.table(mod_sum)
dev.off()


# --------------------
# Draw a sample of volumes and water years with similar timing
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume and center of mass (timing)
# between each gage

# check correlations between flow conditions across the basins
flow.data = var[var$year >= 1997,] %>% select(bwb.vol.nat, bwb.cm.nat, bws.vol.nat, bws.cm.nat, cc.vol, cc.cm, sc.vol, sc.cm, div, sc.div) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(cbind(flow.data[c(1,3,5,7,9,10)],flow.data[c(2,4,6,8)]),use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol, pred.params.div, pred.params.cm)
outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (ac-ft)
vol.pars<-rbind(pred.params.vol, pred.params.div)
vol.sample<-mvrnorm(n=5000,mu=(vol.pars[,1]),Sigma=cov.mat[1:6,1:6])
colnames(vol.sample)<-c("bwb.nat","bws.nat","cc","sc", "div", "sc.div")
write.csv(exp(vol.sample), file.path(cd,"vol.sample.april.csv"),row.names=F)

#save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir,"correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()
# save output from correlations
write.csv(cov.mat, file.path(cd,"cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(cd,"pred.pars.csv"),row.names=T)

# Plot boxplots of total annual flow from each model -> modelOutput.Rmd
library(viridis)
png(filename = file.path(fig_dir,"sampled_volumes.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

as.data.frame(exp(vol.sample)/10000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") %>%
  ggplot(aes(x=site, y=value, fill=site)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Sampled Irrigation Season Volumes") +
  xlab("")+
  ylab("Irrigation Season Volume (10,000 ac-ft)")
dev.off()

# Draw sample of years with similar center of mass (timing)
cm.data = var[var$year >= 1997 & var$year < pred.yr,]
cm.data = cm.data %>% select(year, bwb.cm.nat, bws.cm.nat,cc.cm, sc.cm) 
cm.data$prob<-NA

# pmvnorm calculates the distribution function of the multivariate normal distribution
for(i in 1:dim(cm.data)[1]){
  vec<-cm.data[i,2:5]
  cm.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-0.75,
                          upper=as.numeric(vec)+0.75,mean=pred.params.cm[,1],sigma=cov.mat[6:9,6:9])[1]
}
cm.data$prob<-cm.data$prob/sum(cm.data$prob)
# create normal distribution of years 
CMyear.sample<-sample(cm.data$year,5000,replace=TRUE) 
# Of the 5000 replicates show the percentage that each year represents
cm_sum<-as.data.frame(summary(as.factor(CMyear.sample))/5000)
colnames(cm_sum)<- c("% of sample")
cm_sum<- cm_sum*100
png(file.path(fig_dir,"CM_summary.png"), height = 50*nrow(cm_sum), width = 200*ncol(cm_sum))
grid.table(cm_sum)
dev.off()

#save the probabilities for use in the simulation
write.csv(CMyear.sample, file.path(cd,"CMyear.sample.csv"),row.names=F)

# this version makes the probabilities based on pvnorm distribution, 
# this is not used in the simulations, but is useful for reference
CMyear.sample.prob<-sample(cm.data$year,5000,replace=TRUE, prob=cm.data$prob) 
cm_prob<-as.data.frame(summary(as.factor(CMyear.sample.prob))/5000)
colnames(cm_prob)<- c("% of sample")
cm_prob- cm_prob*100
png(file.path(fig_dir,"CM_summary_prob.png"), height = 50*nrow(cm_prob), width = 200*ncol(cm_prob))
grid.table(cm_prob)
dev.off()



# --------------------
# Curtailment predictions
#

curt_sub<- curtailments %>% select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") #%>% subset(subbasin == 'bw_ab_magic')#, water_right_cat =="A")

curt_sub$subbasin<-factor(curt_sub$subbasin)
curt_sub$water_right_cat<-factor(curt_sub$water_right_cat)

curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% select(year, subbasin, water_right_cat, shut_off_julian, div, sc.div)

curt_mod <- lm(shut_off_julian ~ subbasin + div + sc.div, data = curt)
summary(curt_mod)

# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% select(g.swe, cg.swe, lwd.swe)

