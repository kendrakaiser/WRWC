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
# Camas Creek Winter flows, Snotel from Camas Creek Divide and Soldier Ranger Station
i=1
#function(pred_site, pred_year, wq, swe_Sites) this as a function would be nice - but would need to figure out how to give a variable number of snotel sites
# site_name, vol, wq, swe sites

hist <- var[var$year < pred.yr,] %>% select(cc.vol, cc.wq, ccd.swe, sr.swe) #subset historical data out
cc_mod<-lm(log(cc.vol)~log(cc.wq)+log(ccd.swe+sr.swe), data=hist) 
sig<-summary(cc_mod)$sigma
pred.params[i,2]<-sig

#April 1 data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("cc.wq","ccd.swe","sr.swe")
pred.dat$cc.wq<- var$cc.wq[var$year == pred.yr] #this years base flow
pred.dat$ccd.swe<- var$ccd.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$sr.swe<- var$sr.swe[var$year == pred.yr] # current April 1 SWE

#predict this years total volume at 95 % confidence
preds.cc<-predict(cc_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
pred.params[i,1]<-mean(preds.cc$fit, na.rm=T)

output.vol[i,1]<-round(pred.dat[1,1]/mean(hist$cc.wq),3) #This years percent of mean winter flow
output.vol[i,2]<-round(sum(pred.dat[1,2:3])/mean(hist$ccd+hist$sr, na.rm=T),3) #percent of mean SWE
output.vol[i,3]<-round(exp(preds.cc$fit[1]+sig^2/2)/(1.98*183),0) # TODO: RVK what is this?
output.vol[i,4]<-round(exp(preds.cc$fit[1]+sig^2/2)/mean(hist$cc.vol),3) # TODO: RVK what is this?

#predict this years total volume at 80 % confidence
preds.cc<-predict(cc_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.8)

output.vol[i,5]<-round(exp(preds.cc$fit[2])/(1.98*183),0) # TODO: RVK what is this?
output.vol[i,6]<-round(exp(preds.cc$fit[2])/mean(hist$cc.vol),3) # TODO: RVK what is this?

lastQ<-var$cc.vol[var$year == pred.yr-1] 
output.vol[i,7]<-round(lastQ/(1.98*183),0) # TODO: RVK what is this?
output.vol[i,8]<-round(lastQ/mean(hist$cc.vol),3) # Last years percent of historic volume ...










