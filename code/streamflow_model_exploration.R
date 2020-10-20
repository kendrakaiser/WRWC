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

# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
#q = read.csv(file.path(cd,'streamflow_data.csv'))
usgs_sites = read.csv(file.path(cd,'usgs_sites.csv'))
swe = read.csv(file.path(cd,'all_April1.csv'))
temps = read.csv(file.path(cd, 'ajTemps.csv'))
var = swe %>% select(-X) %>% inner_join(temps, by ="year") %>% select(-X)

temp.ran = read.csv(file.path(cd,'rand.apr.jun.temp.csv'))
new.yr<-swe$year[dim(swe)[1]]+1

# ------------------------------------------------------------------------------ # 
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
