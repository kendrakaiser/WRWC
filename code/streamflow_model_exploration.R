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
library(leaps) #regsubsets
library(rlist) #list.save
library(caret) #loocv
library(erer) #write.list to csv
library(ggcorrplot) 

rm(list=ls())
data_dir = '~/Desktop/WRWC/data' #local
git_dir <<- '~/github/WRWC'
fig_dir = '~/github/WRWC/figures' 

fig_dir_mo <<- file.path(git_dir,'figures/April')
mo_data = 'all_dat_apr.csv'
mo_vars ='mod_apr_vars.csv'
mo_Rvars ='mod_apr_vars.rdata'
pred.yr <- 2020

mo_cm.vars ='mod_apr_cm_vars.csv'
mo_cm.Rvars ='mod_apr_cm_vars.rdata'

# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
#q = read.csv(file.path(cd,'streamflow_data.csv'))
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
swe_q = read.csv(file.path(data_dir,mo_data))
swe_q[swe_q == 0] <- NA # change zeros to a value so lm works

temps = read.csv(file.path(data_dir, 'ajTemps.csv'))
wint.temps = read.csv(file.path(data_dir, 'wintTemps.csv')) #average temps, november - march
nj.temps = read.csv(file.path(data_dir, 'njTemps.csv'))
nf.temps = read.csv(file.path(data_dir, 'nfTemps.csv'))

var = swe_q %>% dplyr::select(-X) %>% inner_join(temps, by ="year") %>% dplyr::select(-X)
stream.id<-unique(as.character(usgs_sites$abv))
swe_cols<-c(2:12)
t_cols<-c(35:46)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")
# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for April-Sept Volume Predictions
# ------------------------------------------------------------------------------ # 

#Big Wood at hailey actual flow, preforms better with linear swe data
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bwb.vol, bwb.wq, all_of(swe_cols))
#vol<- var$bwb.vol[var$year < pred.yr & var$year >= min(hist$year)]
#hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year")[,-c(1)] %>% filter(complete.cases(.))

#use regsubsets to assess the results
regsubsets.out<-regsubsets(log(hist$bwb.vol)~., data=hist[,-1], nbest=1, nvmax=8)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

bwh_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit the regression model and use LOOCV to evaluate performance
form<- paste("log(bwb.vol)~ ", paste(bwh_sum$vars, collapse=" + "), sep = "")
bwh_sum$lm<-summary(lm(form, data=hist))$adj.r.squared
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
#view summary of LOOCV
bwh_sum$loocv<- model$results
print(bwh_sum)
print(summary(lm(form, data=hist)))

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir_mo, "BWH_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()
#check residuals
mod.red<- resid(model)

# calculate the correlations
r <- round(cor(hist[bwh_sum$vars], use="complete.obs"),2)
print(r)
ggcorrplot(r)
# -------------------------------------------------------------
# Big Wood at Stanton, actual flow, preforms better with linear swe data
hist <- var[var$year < pred.yr & var$year > 1996,] %>% dplyr::select(year, bws.vol, bws.wq, all_of(swe_cols)) 
hist$log.wq <- log(hist$bws.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year")[,-1] %>% filter(complete.cases(.)) #remove year, n-j temps preform better than full winter temps

#use regsubsets to explore models
regsubsets.out<-regsubsets(log(hist$bws.vol)~., data=hist[,-c(1)], nbest=1, nvmax=8) #remove bws.vol
# April 1 lowest BIC is bws.wq + log.hc
# April 1 highest r2 with the next lowest bic is bws.wq + log(g, gs, hc)
# Feb 1 log.wq, log.cg
# Mar 1 bws.wq gs.swe
reg_sum<- summary(regsubsets.out) #summary of regsubsets to pull info from
vars<-reg_sum$which[which.min(reg_sum$bic),] #sT/F of variables
bws_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2=reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
print(bws_sum)

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(bws.vol)~ ", paste(bws_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bws_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
bws_sum$loocv<- model$results

png(filename = file.path(fig_dir_mo, "BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Stanton Crossing \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Subset Silver Creek Winter flows
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, sc.vol, sc.wq, all_of(swe_cols)) 
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.hc<- log(hist$hc.swe)
hist$log.bbwq<- log(hist$bwb.wq)
hist<- merge(hist, nj.temps, by = "year")[,-c(1)] %>% filter(complete.cases(.)) #remove year,

# Silver Creek regsubsets 
regsubsets.out<-regsubsets(log(hist$sc.vol)~., data=hist[,-1], nbest=3, nvmax=5)
# Feb 1 ga.swe, sp.swe, bwb.wq, hc.swe
# Mar 1 sc.wq sp.swe, hc.swe
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]
sc_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2=reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(sc.vol)~ ", paste(sc_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
sc_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
sc_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(sc_sum)

png(filename = file.path(fig_dir_mo, "SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()
# -------------------------------------------------------------
# camas creek
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, cc.vol, cc.wq, all_of(swe_cols)) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.sp <- log(hist$sp.swe)
hist$log.cg<- log(hist$cg.swe)
hist$log.hc<- log(hist$hc.swe)
hist$log.bbwq<- log(hist$bwb.wq)
hist<- merge(hist, nj.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #remove year,

regsubsets.out<-regsubsets(log(hist$cc.vol)~., data=hist[,-1], nbest=1, nvmax=4)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

cc_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(cc.vol)~ ", paste(cc_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
cc_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
cc_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(cc_sum)

png(filename = file.path(fig_dir_mo, "CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

#compile all model details into one list to export
mod_sum<- list(bwh = bwh_sum, bws = bws_sum, sc = sc_sum, cc = cc_sum)
write.list(mod_sum, file.path(data_dir, mo_vars))
list.save(mod_sum, file.path(data_dir, mo_Rvars))

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
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
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
hist <- var[var$year >= 1997 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, bws.wq, bwb.vol.nat, bws.vol.nat, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
regsubsets.out<-regsubsets(var$abv.s[var$year >= 1997 & var$year < pred.yr]~., data=hist, nbest=3, nvmax=8)
# these all do really poorly, and no trends in the data at all, so pull randomly

# Losses Between Hailey and Stanton Crossing
plot(var$year, var$bws.loss)
plot(var$year[var$year >=2000], var$bws.loss[var$year >=2000])
hist <- var[var$year >= 2000 & var$year < pred.yr,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
#use regsubsets to plot the results
regsubsets.out<-regsubsets(var$bws.loss[var$year >= 2000 & var$year < pred.yr]~., data=hist, nbest=2, nvmax=8)
# t.cg, t.gs, lowest BIC, R2 == 0.55
# g.swe, hc.swe, t.g, t.cg, t.gs highest R2 and third lowest BIC

# Total Diversions
hist <- var[var$year >= 1997 & var$year < 2020,] %>% dplyr::select(bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, year) 
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<-hist %>% dplyr::select(-cg.swe, -hc.swe, -bwb.wq)
regsubsets.out<-regsubsets(var$div[var$year >= 1997 & var$year < 2020]~., data=hist, nbest=3, nvmax=8)

# Feb 1 Max R2 of 0.13 -- wont work
# Mar 1 log gs r2 0.27

# Silver Creek Diversions
hist <- var[var$year < 2020,] %>% dplyr::select(sc.wq, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.cg, t.g, t.gs, t.hc, t.lw, t.f, year) 
hist$log.scwq <- log(hist$sc.wq)
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)

regsubsets.out<-regsubsets(log(var$sc.div[var$year < 2020])~., data=hist, nbest=3, nvmax=8)

# g.swe, t.cg, t.gs,t.hc, log.cg, log.lwd
# log(div) g.swe, t.cg, t.gs, t.hc, log.cg, loglwd

# Feb 1 t.f, log.sc.wq, log.g, log.lwd
# Mar 1 g.swe, t.cg, t.f, log.sc.wq, log bwb.wq, log.hc.swe


# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 

# Big wood at Hailey
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bwb.cm, bwb.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, ds.swe, ccd.swe, sr.swe, ga.swe, sp.swe, sm.swe, bc.swe)
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #remove year,

regsubsets.out<-regsubsets(log(hist$bwb.cm)~., data=hist[,-1], nbest=1, nvmax=4)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

bwh.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(bwb.cm)~ ", paste(bwb.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bwh.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
bwh.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(bwh.cm_sum)

png(filename = file.path(fig_dir_mo, "bwb.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr & var$year > 1996,] %>% dplyr::select(year, bws.cm, bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, ds.swe, ccd.swe, sr.swe, ga.swe, sp.swe, sm.swe, bc.swe)
hist$log.cg<- log(hist$cg.swe)
hist$log.g<- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #remove year,

regsubsets.out<-regsubsets(log(hist$bws.cm)~., data=hist[,-1], nbest=1, nvmax=4)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

bws.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(bws.cm)~ ", paste(bws.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bws.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
bws.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(bws.cm_sum)

png(filename = file.path(fig_dir_mo, "bws.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Garfield Ranger Station and Swede Peak
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, sc.cm, sc.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, ds.swe, ccd.swe, sr.swe, ga.swe, sp.swe, sm.swe, bc.swe)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.ga<- log(hist$ga.swe)
hist<- merge(hist, nj.temps, by = "year") [,-c(1,4,5,6,7,8)] %>% filter(complete.cases(.)) #remove year,

regsubsets.out<-regsubsets(log(hist$sc.cm)~., data=hist[,-1], nbest=1, nvmax=4)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

sc.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(sc.cm)~ ", paste(sc.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
sc.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
sc.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(sc.cm_sum)

png(filename = file.path(fig_dir_mo, "sc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()
# -------------------------------------------------------------
# Camas Creek
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, cc.cm, cc.wq, g.swe, gs.swe, hc.swe, lwd.swe, ds.swe, ccd.swe, sr.swe, ga.swe, sp.swe, sm.swe, bc.swe) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.ga<- log(hist$ga.swe)
hist<- merge(hist, nj.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #remove year,

regsubsets.out<-regsubsets(log(hist$cc.cm)~., data=hist[,-1], nbest=1, nvmax=4)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

cc.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(cc.cm)~ ", paste(cc.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
cc.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
cc.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(cc.cm_sum)

png(filename = file.path(fig_dir_mo, "cc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

#compile all model details into one list to export
mod_cm.sum<- list(bwh = bwh.cm_sum, bws = bws.cm_sum, sc = sc.cm_sum, cc = cc.cm_sum)
write.list(mod_cm.sum, file.path(data_dir, mo_cm.vars))
list.save(mod_cm.sum, file.path(data_dir, mo_cm.Rvars))



regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
png(filename = file.path(fig_dir, "Regsubsets_example.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
#quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
dev.off()

quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")

rs<-summary(regsubsets.out)
quartz(title="BIC v R2",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")


# curtailments
curtailments = read.csv(file.path(cd,'historic_shutoff_dates_071520.csv'))
#bigwood A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# div, bwb.vol.nat, t (0.67)

#bigwood B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# div, bwb.wq, bwb.vol.nat, t (0.89)

#bigwood c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
#  bwb.vol.nat, t (0.87)

# bigwood bl magic A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.wq bwb.vol.nat t.g t.lw (0.35)
# div t (0.300)

#bigwood bl magic B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.vol.nat t (0.44)

#bigwood bl magic c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(shut_off_julian, div, bwb.wq, bwb.vol.nat, t)
# bwb.wq bwb.vol.nat t (0.47)

# sc A
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.cm ga.swe, sp.swe, cg.swe, g.swe (0.29)

# sc B
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.div, ga.swe (0.68)

# sc c
curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw')
curt <- curt_sub  %>% inner_join(var, by = 'year')  %>% dplyr::select(shut_off_julian, sc.div, sc.cm, sc.wq, ga.swe, sp.swe, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe, t.sp, t.g, t.gs, t.lw)
curt$t <- rowMeans(cbind(curt$t.sp, curt$t.g, curt$t.gs, curt$t.lw), na.rm=TRUE)
curt <- curt %>% dplyr::select(-c(t.sp, t.g, t.gs, t.lw))
# sc.div, ga.swe, cg.swe, lwd.swe, t (0.83)


#use regsubsets to plot the results
regsubsets.out<-regsubsets(shut_off_julian~., data=curt, nbest=3, nvmax=5)
regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
quartz(title="Adjusted R^2",10,10)
plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
quartz(title="BIC",10,10)
plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
rs<-summary(regsubsets.out)
quartz(title="R2 v BIC",10,10)
plot(rs$bic, rs$adjr2, xlab="BIC", ylab="adj R2")