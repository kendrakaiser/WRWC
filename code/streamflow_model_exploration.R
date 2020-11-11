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
library(leaps)

rm(list=ls())
cd = '~/Desktop/Data/WRWC'

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
# Evaluate alternative model combinations for April-Sept Volume Predictions
# ------------------------------------------------------------------------------ # 

#camas creek
hist <- var[var$year < pred.yr,] %>% select(cc.wq, ccd.swe, sr.swe) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.sum <- log(hist$ccd.swe+hist$sr.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(log(var$cc.vol[var$year < pred.yr])~., data=hist, nbest=1, nvmax=4)

#big wood at hailey
hist <- var[var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(log(var$bwb.vol[var$year < pred.yr])~., data=hist, nbest=1, nvmax=8)


#big wood at stanton
hist <- var[var$year < pred.yr,] %>% select(bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.wq <- log(hist$bws.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to explore models
regsubsets.out<-regsubsets(log(var$bws.vol[var$year < pred.yr])~., data=hist, nbest=1, nvmax=8)

# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.vol, sc.wq, ga.swe, sp.swe, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.hc<- log(hist$hc.swe)
hist$log.bbwq<- log(hist$bwb.wq)

#hist$log.ga<- log(hist$ga.swe)
#hist$log.sum<- log(hist$ga.swe+hist$sp.swe)

# Silver Creek regsubsets 
regsubsets.out<-regsubsets(log(sc.vol[var$year < pred.yr])~., data=hist, nbest=3, nvmax=5)
# 0.83 sc.wq, sp.swe, g.swe, log cg lowest BIC
regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")


# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 

#camas creek
hist <- var[var$year < pred.yr,] %>% select(cc.wq, ccd.swe, sr.swe, t.ccd, t.sr) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.sum <- log(hist$ccd.swe+hist$sr.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$cc.cm[var$year < pred.yr]~., data=hist, nbest=3, nvmax=5)

#big wood at hailey
hist <- var[var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(log(var$bwb.cm[var$year < pred.yr])~., data=hist, nbest=3, nvmax=8)


#big wood at stanton
hist <- var[var$year < pred.yr,] %>% select(bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
hist$log.cg<- log(hist$cg.swe)
hist$log.g<- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to explore models
regsubsets.out<-regsubsets(log(var$bws.cm[var$year < pred.yr])~., data=hist, nbest=2, nvmax=8)

# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.cm, sc.wq, ga.swe, sp.swe, t.ga, t.sp) 
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.ga<- log(hist$ga.swe)
hist$log.sum<- log(hist$ga.swe+hist$sp.swe)

# Silver Creek regsubsets 
regsubsets.out<-regsubsets(log(sc.cm[var$year < pred.yr])~., data=hist, nbest=3, nvmax=8)

regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")

