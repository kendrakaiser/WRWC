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
# Predicted temperatures for each subbasin
# Year and April 1 SWE 

# Streamflow Data
q = read.csv(file.path(cd,'streamflow_data.csv'))




# Calculate baseflows for each station
stream.id<-c("bwb","bws","cc","bwr","sc")
base.flows<-c(rep(NA,5))
names(base.flows)<-stream.id


# Create sequence of non-leap year dates
wy<-seq(as.Date("2018-10-01"),as.Date("2019-09-30"),"day") #update to take user input
wy<-data.frame(wy,1:365)
colnames(wy)<-c("Date","day")