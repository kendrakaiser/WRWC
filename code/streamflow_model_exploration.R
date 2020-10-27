# ----------------------------------------------------------------------------- #
# Predictive Streamflow Model Exploration for the Wood River Water Collaborative
# Kendra Kaiser
# October 1, 2020
# Exploration of linear models to predict total streamflow volume and center of mass based on 
# calculated baseflow, current SWE and predicted temperature 
# ----------------------------------------------------------------------------- # 

library(MASS)
library(plotrix)
library(mvtnorm)
library(tidyverse)

rm(list=ls())

cd = '~/Desktop/Data/WRWC'
source("aictable.R")

pred.yr <- 2020

# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
#q = read.csv(file.path(cd,'streamflow_data.csv'))
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
#volumes
output.vol<-array(NA,c(length(stream.id),8))
rownames(output.vol)<-stream.id
colnames(output.vol)<-c("cfs % of mean","swe % of mean", "ask RVK fit", "ask RVK fit", "ask RVK fit", "ask RVK fit", "ask RVK fit", "Prev Year % of mean Volume")

pred.params.vol<-array(NA,c(5,2))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","bwr.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma")

#center of mass
output.cm<-data.frame(array(NA,c(5,5)))
rownames(output.cm)<-stream.id
colnames(output.cm)<-c("temp-mean","% of mean","cm","cm-mean","cm.date")

pred.params.cm<-array(NA,c(5,2))
rownames(pred.params.cm)<-c("bwb.cm","bws.cm","cc.cm","bwr.cm","sc.cm")
colnames(pred.params.cm)<-c("cm","sigma")

swe.new<-data.frame(t(c(rep(NA,10))))
names(swe.new)<-c("cg", "g", "gs", "hc", "lwd", "ds", "ccd", "sr", "ga", "sp")

# ------------------------------------------------------------------------------ # 
#
# April-Sept Volume Predictions
#
# ------------------------------------------------------------------------------ # 

modOut<- function(mod, pred.dat, wq, vol, meanSWE, lastQ){
  '
  mod:     input model
  pred.dat: data.frame of prediction variables
  i:        watershed iteration for saving output
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
  output.vol[1,1]<-round(pred.dat[1,1]/mean(wq),3) 
  #percent of mean SWE
  output.vol[1,2]<-round(sum(pred.dat[1,2:3])/meanSWE,3) 
  # back-transformation of log-transformed data to expected value in original units, with lognormal   residuals; 183 is the number of days between April-Sept and 1.98 converts back to cfs
  output.vol[1,3]<-round(exp(predictions$fit[1]+sig^2/2)/(1.98*183),0) 
  #Division by long-term mean to generate % of average volume, with lognormal residuals
  output.vol[1,4]<-round(exp(predictions$fit[1]+sig^2/2)/mean(vol),3) 
  
  #this years total volume at 80 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.8)
  #bottom of 80% CI (statisticians) converted to cfs
  output.vol[1,5]<-round(exp(predictions$fit[2])/(1.98*183),0) 
  # 90% exceedance flow as a percent of long-term mean
  output.vol[1,6]<-round(exp(predictions$fit[2])/mean(vol),3) 
  output.vol[1,7]<-round(lastQ/(1.98*183),0) # last years volume in cfs
  output.vol[1,8]<-round(lastQ/mean(vol),3) # Last years percent of average historic volume
  return(list(output.vol, pred.params.vol))
}

# Subset Camas Creek Winter flows, Snotel from Camas Creek Divide and Soldier Ranger Station
hist <- var[var$year < pred.yr,] %>% select(cc.vol, cc.wq, ccd.swe, sr.swe) 
# Camas Creek linear model
cc_mod<-lm(log(cc.vol)~log(cc.wq)+log(ccd.swe+sr.swe), data=hist) 

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("cc.wq","ccd.swe","sr.swe")
pred.dat$cc.wq<- var$cc.wq[var$year == pred.yr] #this years base flow
pred.dat$ccd.swe<- var$ccd.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$sr.swe<- var$sr.swe[var$year == pred.yr] # current April 1 SWE

# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, hist$cc.wq, hist$cc.vol, mean(hist$ccd.swe+hist$sr.swe, na.rm=T), var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

# Subset Big Wood Winter flows, Snotel from Chocolate Gulch, Galena & Galena Summit, Hyndman, Lost-Wood Divide and Dollarhide
hist <- var[var$year < pred.yr,] %>% select(bwb.vol, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
# Big Wood at Hailey linear model
bwb_mod<-lm(log(bwb.vol)~log(bwb.wq)+log(cg.swe+ g.swe+ gs.swe+ hc.swe+ lwd.swe), data=hist) 

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
mod_out<- modOut(bwb_mod, pred.dat, hist$bwb.wq, hist$bwb.vol, mean(hist$cg.swe,  hist$g.swe,  hist$gs.swe,  hist$hc.swe,  hist$lwd.swe, trim=0, na.rm=T), var$bwb.vol[var$year == pred.yr-1])
#these could be formatted differntely to be saved to the gloabl env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]




