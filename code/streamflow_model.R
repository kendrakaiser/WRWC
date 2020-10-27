# ----------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# July 14, 2020
# Linear model to predict total streamflow volume and center of mass based on 
# calculated baseflow, current SWE and predicted temperature 
# ----------------------------------------------------------------------------- # 

library(MASS)
library(plotrix)
library(mvtnorm)
library(tidyverse)

rm(list=ls())
cd = '~/Desktop/Data/WRWC'

pred.yr <- 2020

# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(cd,'usgs_sites.csv'))
swe_q = read.csv(file.path(cd,'all_April1.csv'))
swe_q[swe_q == 0] <- 0.00001 # change zeros to a value so lm works
temps = read.csv(file.path(cd, 'ajTemps.csv'))
var = swe_q %>% select(-X) %>% inner_join(temps, by ="year") %>% select(-X)

temp.ran = read.csv(file.path(cd,'rand.apr.jun.temp.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
# ------------------------------------------------------------------------------ # 
# Create sequence of non-leap year dates
wy<-seq(as.Date("2018-10-01"),as.Date("2019-09-30"),"day") #update to automate based on new.yr
wy<-data.frame(wy,1:365)
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------ # 
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),8))
rownames(output.vol)<-stream.id
colnames(output.vol)<-c("cfs % of mean","swe % of mean", "ask RVK fit", "ask RVK fit", "ask RVK fit", "ask RVK fit", "ask RVK fit", "Prev Year % of mean Volume")
pred.params.vol<-array(NA,c(5,2))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","bwr.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma")

# center of mass
output.cm<-data.frame(array(NA,c(5,5)))
rownames(output.cm)<-stream.id
colnames(output.cm)<-c("temp-mean","% of mean","cm","cm-mean","cm.date")
pred.params.cm<-array(NA,c(5,2))
rownames(pred.params.cm)<-c("bwb.cm","bws.cm","cc.cm","bwr.cm","sc.cm")
colnames(pred.params.cm)<-c("cm","sigma")

swe.new<-data.frame(t(c(rep(NA,10))))
names(swe.new)<-c("cg", "g", "gs", "hc", "lwd", "ds", "ccd", "sr", "ga", "sp")

# summary stats
mod_sum<-data.frame(array(NA,c(4,2)))
colnames(mod_sum)<-c("Vol Adj-R2", "CM ADj-R2")
rownames(mod_sum)<-c("bwb","bws","cc","sc")

# ------------------------------------------------------------------------------ # 
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
  # back-transformation of log-transformed data to expected value in original units, with lognormal   residuals; 183 is the number of days between April-Sept and 1.98 converts back to cfs
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

# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek divide was not included in model selection
hist <- var[var$year < pred.yr,] %>% select(cc.vol, cc.wq, ccd.swe, sr.swe) 
# Camas Creek linear model
cc_mod<-lm(log(cc.vol)~log(cc.wq)+sr.swe+ccd.swe, data=hist) 
mod_sum[3,1]<-summary(cc_mod)$adj.r.squared

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("cc.wq","ccd.swe","sr.swe")
pred.dat$cc.wq<- var$cc.wq[var$year == pred.yr] #this years base flow
pred.dat$ccd.swe<- var$ccd.swe[var$year == pred.yr]
pred.dat$sr.swe<- var$sr.swe[var$year == pred.yr] # current April 1 SWE

# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, hist$cc.wq, hist$cc.vol, mean(hist$sr.swe, na.rm=T), var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]


# --------------------------------------------------
# Subset Big Wood Winter flows, Snotel from  Galena & Galena Summit, Hyndman
hist <- var[var$year < pred.yr,] %>% select(bwb.vol, bwb.wq, g.swe, gs.swe, hc.swe) 
# Big Wood at Hailey linear model
bwb_mod<-lm(log(bwb.vol)~log(bwb.wq)+log(g.swe)+ log(gs.swe)+ log(hc.swe), data=hist) 
mod_sum[1,1]<-summary(bwb_mod)$adj.r.squared

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,6)))
names(pred.dat)<-c("bwb.wq","cg.swe","g.swe","gs.swe","hc.swe","lwd.swe")
pred.dat$bwb.wq<- var$bwb.wq[var$year == pred.yr] #this years base flow
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$g.swe<- var$g.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$gs.swe<- var$gs.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$hc.swe<- var$hc.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$lwd.swe<- var$lwd.swe[var$year == pred.yr] # current April 1 SWE

# Big Wood at Hailey Model output
mod_out<- modOut(bwb_mod, pred.dat, hist$bwb.wq, hist$bwb.vol, mean(hist$g.swe,  hist$gs.swe,  hist$hc.swe, trim=0, na.rm=T), var$bwb.vol[var$year == pred.yr-1])
#these could be formatted differntely to be saved to the gloabl env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]

# --------------------------------------------------
# Subset Big Wood at Stanton Winter flows, Snotel from Chocolate Gulch, Galena & Galena Summit, Hyndman, Lost-Wood Divide and Dollarhide
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.vol, bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
# Big Wood at Stanton linear model
bws_mod<-lm(log(bws.vol)~bws.wq+g.swe+ log(gs.swe)+ log(hc.swe), data=hist) 
mod_sum[2,1]<-summary(bws_mod)$adj.r.squared

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,6)))
names(pred.dat)<-c("bws.wq","cg.swe","g.swe","gs.swe","hc.swe","lwd.swe")
pred.dat$bws.wq<- var$bws.wq[var$year == pred.yr] #this years base flow
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$g.swe<- var$g.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$gs.swe<- var$gs.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$hc.swe<- var$hc.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$lwd.swe<- var$lwd.swe[var$year == pred.yr] # current April 1 SWE

# Big Wood at Stanton Model output
mod_out<- modOut(bws_mod, pred.dat, hist$bws.wq, hist$bws.vol, mean(hist$cg.swe,  hist$g.swe,  hist$gs.swe,  hist$hc.swe,  hist$lwd.swe, trim=0, na.rm=T), var$bws.vol[var$year == pred.yr-1])
#these could be formatted differntely to be saved to the gloabl env. within the function
output.vol[2,] <- mod_out[[1]]
pred.params.vol[2,] <- mod_out[[2]]


# --------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.vol, sc.wq, ga.swe, sp.swe) 
# Silver Creek linear model -- note no log tranformations here, difference in travel times?
sc_mod<-lm(sc.vol~sc.wq+ga.swe+sp.swe, data=hist) 
mod_sum[4,1]<-summary(sc_mod)$adj.r.squared

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("sc.wq","ga.swe","sp.swe")
pred.dat$sc.wq<- var$sc.wq[var$year == pred.yr] #this years base flow
pred.dat$ga.swe<- var$ga.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$sp.swe<- var$sp.swe[var$year == pred.yr] # current April 1 SWE

# Silver Creek Model output
mod_out<- modOut(sc_mod, pred.dat, hist$sc.wq, hist$sc.vol, mean(hist$ga.swe+hist$sp.swe, na.rm=T), var$cc.vol[var$year == pred.yr-1])
output.vol[5,] <- mod_out[[1]]
pred.params.vol[5,] <- mod_out[[2]]

# ------------------------------------------------------------------------------ # 
#
# Center of Mass Predictions
#
# ------------------------------------------------------------------------------ # 

# Subset Camas Creek Winter flows, Snotel & Temperatures from Soldier Ranger Station, camas creek divide 
hist <- var[var$year < pred.yr,] %>% select(cc.cm, ccd.swe, t.sr) 
# Camas Creek linear model
cc_mod.cm<-lm(log(cc.cm)~log(ccd.swe) + t.sr, data=hist) 
mod_sum[3,2]<-summary(cc_mod.cm)$adj.r.squared 


#big wood at hailey
hist <- var[var$year < pred.yr,] %>% select(bwb.cm, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
#linear model
bwb_mod.cm <-lm(log(bwb.cm) ~ g.swe+t.cg+ t.g+t.gs+t.hc+t.lw +log(cg.swe)+log(hc.swe), data=hist)
mod_sum[1,2]<-summary(bwb_mod.cm)$adj.r.squared #r2 at 0.947


#big wood at stanton
hist <- var[var$year < pred.yr,] %>% select(bws.cm, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
#linear model
bws_mod.cm <-lm(log(bws.cm) ~ g.swe + t.cg+ t.g +t.lw +log(cg.swe)+log(gs.swe)+log(hc.swe), data=hist)
mod_sum[2,2]<-summary(bws_mod.cm)$adj.r.squared #r2 at 0.914


# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.cm, sc.wq, ga.swe, sp.swe, t.sp) 
# Silver Creek linear model -- note no log tranformations here, difference in travel times?
sc_mod.cm<-lm(sc.cm~log(sc.wq)+ga.swe+log(sp.swe), data=hist) 
mod_sum[4,2]<-summary(sc_mod.cm)$adj.r.squared #r2 0.134 this does terribly ... 
