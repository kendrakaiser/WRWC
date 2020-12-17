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

pred.yr <- 2019

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

#Big Wood at hailey, 'natural' flow
hist <- var[var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(log(var$bwh.vol.nat[var$year < pred.yr])~., data=hist, nbest=1, nvmax=8)
#g.swe, hc.swe, log.gs lowest bic and 0.84

# -------------------------------------------------------------
# Big Wood at Stanton, 'natural' flow
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.wq <- log(hist$bws.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to explore models
regsubsets.out<-regsubsets(log(var$bws.vol.nat[var$year < pred.yr & var$year > 1996])~., data=hist, nbest=1, nvmax=8)
#lowest BIC is bws.wq + log.hc
# highest r2 with the next lowest bic is bws.wq + log(g, gs, hc)

# -------------------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.vol, sc.wq, ga.swe, sp.swe, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.hc<- log(hist$hc.swe)
hist$log.bbwq<- log(hist$bwb.wq)

# Silver Creek regsubsets 
regsubsets.out<-regsubsets(sc.vol[var$year < pred.yr]~., data=hist, nbest=3, nvmax=5)
# 0.83 sc.wq, sp.swe, g.swe, log cg.swe lowest BIC w log(sc.vol)
#0.82 sc.wq, sp.swe, log(cg.swe) BIC=-35 in both ... compare the two

# -------------------------------------------------------------
# camas creek
hist <- var[var$year < pred.yr,] %>% select(cc.wq, ccd.swe, sr.swe) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.sum <- log(hist$ccd.swe+hist$sr.swe)

regsubsets.out<-regsubsets(log(var$cc.vol[var$year < pred.yr])~., data=hist, nbest=1, nvmax=4)

# use regsubsets to plot the results
regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
quartz(title="R2 v BIC",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")

# -------------------------------------------------------------
# Diversions above Hailey
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$abv.h[var$year >= 1997 & var$year < pred.yr]~., data=hist, nbest=3, nvmax=8)
# cg.swe, g.swe, hc.swe, t.cg, t.g, t.gs, t.lw, log.lwd, r2=.65 post 1997, no trend with time

# Diversions above Stanton Crossing
plot(var$year, var$abv.s)
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% select(bwb.wq, bws.wq,, bwb.vol.nat, bws.vol.nat, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
regsubsets.out<-regsubsets(var$abv.s[var$year >= 1997 & var$year < pred.yr]~., data=hist, nbest=3, nvmax=8)
# these all do really poorly, and no trends in the data at all, so pull randomly

# Losses Between Hailey and Stanton Crossing
plot(var$year, var$bws.loss)
plot(var$year[var$year >=2000], var$bws.loss[var$year >=2000])
hist <- var[var$year >= 2000 & var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$bws.loss[var$year >= 2000 & var$year < pred.yr]~., data=hist, nbest=2, nvmax=8)
# t.cg, t.gs, lowest BIC, R2 == 0.55
# g.swe, hc.swe, t.g, t.cg, t.gs highest R2 and third lowest BIC

# Total Diversions
hist <- var[var$year >= 1997 & var$year < 2020,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<-hist %>% select(-cg.swe, -hc.swe, -bwb.wq)
regsubsets.out<-regsubsets(var$div[var$year >= 1997 & var$year < 2020]~., data=hist, nbest=3, nvmax=8)


# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 

# Big wood at Hailey
hist <- var[var$year < pred.yr,] %>% select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(log(var$bwb.cm.nat[var$year < pred.yr])~., data=hist, nbest=3, nvmax=8)
#g.swe+t.cg+ t.g+t.gs+t.hc+t.lw +log(cg.swe)+log(hc.swe) natural cm
# g.swe, hc.swe, t.cg, t.lw

# -------------------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
hist$log.cg<- log(hist$cg.swe)
hist$log.g<- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
#use regsubsets to explore models
regsubsets.out<-regsubsets(log(var$bws.cm.nat[var$year < pred.yr & var$year > 1996])~., data=hist, nbest=2, nvmax=8)
#bws.cm ~ g.swe + t.cg+ t.g +t.lw +log(cg.swe)+log(gs.swe)+log(hc.swe) w. 2020 
#'natural' center of mass: lwd.swe, t.cg, t.g, t.hc, t.lw, log.cg log.hc 

# -------------------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.cm, sc.wq, ga.swe, sp.swe, t.ga, t.sp, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, t.p) 
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.ga<- log(hist$ga.swe)
hist$log.sum<- log(hist$ga.swe+hist$sp.swe)

# Silver Creek regsubsets 
regsubsets.out<-regsubsets(log(sc.cm[var$year < pred.yr])~., data=hist, nbest=3, nvmax=8)
#BIC is much lower for: cg.swe, hc.swe, lwd.swe, t.cg, t.gs, log(sp.swe), log(sc.wq) (r2=0.72!)

# -------------------------------------------------------------
# Camas Creek
hist <- var[var$year < pred.yr,] %>% select(cc.wq, ccd.swe, sr.swe, t.ccd, t.sr, t.f) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.sum <- log(hist$ccd.swe+hist$sr.swe)
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$cc.cm[var$year < pred.yr]~., data=hist, nbest=3, nvmax=5)
#between two best r2 (0.51) the lower BIC includes ccd.swe, sr.swe, t.f


regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
quartz(title="BIC v R2",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")

