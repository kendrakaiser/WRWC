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

rm(list=ls())

source("aictable.R")

cd = '~/Desktop/Data/WRWC'


# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE and Modeled Temperature Data
q = read.csv(file.path(cd,'streamflow_data.csv'))
usgs_sites = read.csv(file.path(cd,'usgs_sites.csv'))
swe = read.csv(file.path(cd,'april1swe.csv'))
temp = read.csv(file.path(cd,'rand.apr.jun.temp.csv'))

new.yr<-swe$year[dim(swe)[1]]+1

# ------------------------------------------------------------------------------ # 
# Calculate winter flow for each station, perhaps move this to the data scraping script
# currently set to calculate on the previous year
stream.id<-c("bwb","bws","cc","bwr","sc")
wint.flows<-c(rep(NA,5))
names(wint.flows)<-stream.id
for(i in 1:length(stream.id)){
  sub <- q[q$site_no == usgs_sites$site_no[usgs_sites$abr == stream.id[i]] & q$wy == 2019,] # new.yr
  rownames(sub) <- seq(length=nrow(sub))
  end <-min(sub$X[sub$mo == 4 & sub$day == 2])
  wint.flows[i]<- mean(sub$Flow_Inst[1] : sub$Flow_Inst[which(sub$X == end)])
}

# Create sequence of non-leap year dates
wy<-seq(as.Date("2018-10-01"),as.Date("2019-09-30"),"day") #update to automate based on new.yr
wy<-data.frame(wy,1:365)
colnames(wy)<-c("Date","day")


# ------------------------------------------------------------------------------ # 
# Setup output arrays
# ------------------------------------------------------------------------------ # 
output<-array(NA,c(length(stream.id),8))
rownames(output)<-stream.id
colnames(output)<-c(rep(c("cfs","% of mean"),4))

pred.params<-array(NA,c(5,2))
rownames(pred.params)<-c("bwb.vol","bws.vol","cc.vol","bwr.vol","sc.vol")
colnames(pred.params)<-c("log.vol","sigma")

# ------------------------------------------------------------------------------ # 
#
# April-Sept Volume Predictions
#
# ------------------------------------------------------------------------------ # 
bwb_mod<-lm(log(HL.Vol)~log(HL.Base)+log(I(CC+WE+BB+IP)))
sig<-summary(HLmod)$sigma

pred.params[i,2]<-sig
