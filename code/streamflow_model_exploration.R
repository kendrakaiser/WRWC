# ----------------------------------------------------------------------------- #
# Predictive Streamflow Model Exploration for the Wood River Water Collaborative
# Kendra Kaiser
# October 1, 2020
# Exploration of linear models to predict total streamflow volume and center of mass based on 
# calculated baseflow, current SWE and predicted temperature 
# ----------------------------------------------------------------------------- # 

# evaluate nvmax for each model and associated change in adj r2 - 
# particularly for the cm -- at what point is it overfitted?

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

fig_dir_mo <<- file.path(git_dir,'figures/February')
mo_data = 'all_dat_feb.csv'
mo_vars ='mod_feb_vars.csv'
mo_Rvars ='mod_feb_vars.rdata'
pred.yr <- 2020

mo_cm.vars ='mod_feb_cm_vars.csv'
mo_cm.Rvars ='mod_feb_cm_vars.rdata'

# Import Data ------------------------------------------------------------------ # 
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
#q = read.csv(file.path(cd,'streamflow_data.csv'))
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
swe_q = read.csv(file.path(data_dir,mo_data))
swe_q[swe_q == 0] <- NA # change zeros to a value so lm works

spring.temps = read.csv(file.path(data_dir, 'sprTemps.csv'))
wint.temps = read.csv(file.path(data_dir, 'wintTemps.csv')) #average temps, november - march
nj.temps = read.csv(file.path(data_dir, 'njTemps.csv'))
nf.temps = read.csv(file.path(data_dir, 'nfTemps.csv'))
fm.temps = read.csv(file.path(data_dir, 'fmTemps.csv'))

var = swe_q %>% dplyr::select(-X) %>% inner_join(spring.temps, by ="year") 
stream.id<-unique(as.character(usgs_sites$abv))
swe_cols<-c(2:12)
t_cols<-c(35:46)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")
#------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for April-Sept Volume Predictions
#------------------------------------------------------------------------------ # 
 
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

# -----
# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 

# Big wood at Hailey
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bwb.cm, bwb.wq, all_of(swe_cols))
hist$log.wq <- log(hist$bwb.wq)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year") 
hist<- merge(hist,spring.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #add in predicted april-june temps and remove year, 

regsubsets.out<-regsubsets(hist$bwb.cm~., data=hist[,-1], nbest=1, nvmax=6)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

bwh.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("bwb.cm~ ", paste(bwh.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bwh.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
bwh.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(bwh.cm_sum)
print(summary(lm(form, data=hist)))

png(filename = file.path(fig_dir_mo, "bwh.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Hailey Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr & var$year > 1996,] %>% dplyr::select(year, bws.cm, bws.wq, all_of(swe_cols))
hist$log.cg<- log(hist$cg.swe)
hist$log.g<- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist<- merge(hist, nj.temps, by = "year")
hist<- merge(hist,spring.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #add in predicted april-june temps and remove year, 

regsubsets.out<-regsubsets(hist$bws.cm~., data=hist[,-1], nbest=1, nvmax=6)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

bws.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("bws.cm~ ", paste(bws.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bws.cm_sum$lm<-summary(lm(form, data=hist) )$adj.r.squared
#view summary of LOOCV
bws.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(bws.cm_sum)

png(filename = file.path(fig_dir_mo, "bws.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Stanton Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Subset Silver Creek Winter flows
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, sc.cm, sc.wq, bwb.wq, bws.wq, all_of(swe_cols)) #,
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist$log.sp <- log(hist$sp.swe)
hist$log.wq <- log(hist$sc.wq)
hist$log.ga<- log(hist$ga.swe)
hist<- merge(hist, nj.temps, by = "year")
hist<- merge(hist, spring.temps, by = "year") [,-c(1)]%>% filter(complete.cases(.)) #add in predicted april-june temps and remove year

regsubsets.out<-regsubsets(hist$sc.cm~., data=hist[,-1], nbest=1, nvmax=6)
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]

sc.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("sc.cm~ ", paste(sc.cm_sum$vars, collapse=" + "), sep = "")
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
sc.cm_sum$lm<-summary(lm(form, data=hist))$adj.r.squared
#view summary of LOOCV
sc.cm_sum$loocv<- model$results
#add in error catch if model doesnt work ...
print(sc.cm_sum)

png(filename = file.path(fig_dir_mo, "sc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek Center of Mass (doy)")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Camas Creek
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, cc.cm, cc.wq, all_of(swe_cols)) 
hist$log.wq <- log(hist$cc.wq)
hist$log.ccd <- log(hist$ccd.swe)
hist$log.sr <- log(hist$sr.swe)
hist$log.cg<- log(hist$cg.swe)
hist$log.g <- log(hist$g.swe)
hist$log.gs<- log(hist$gs.swe)
hist$log.hc <- log(hist$hc.swe)
hist$log.lwd <- log(hist$lwd.swe)
hist$log.sp <- log(hist$sp.swe)
hist$log.ga<- log(hist$ga.swe)
hist<- merge(hist, nj.temps, by = "year")
hist<- merge(hist,spring.temps, by = "year") [,-c(1)] %>% filter(complete.cases(.)) #add in predicted april-june temps and remove year

regsubsets.out<-regsubsets(log(hist$cc.cm)~., data=hist[,-1], nbest=1, nvmax=6)
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
plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek Center of Mass (doy)")
abline(0,1,col="gray50",lty=1)
dev.off()

#compile all model details into one list to export
mod_cm.sum<- list(bwh = bwh.cm_sum, bws = bws.cm_sum, sc = sc.cm_sum, cc = cc.cm_sum)
write.list(mod_cm.sum, file.path(data_dir, mo_cm.vars))
list.save(mod_cm.sum, file.path(data_dir, mo_cm.Rvars))



# -------------------------------------------------------------
# Evaluation of residuals 
#https://drsimonj.svbtle.com/visualising-residuals
#library(broom)
#bwb.m<- model %>% augment()

#ggplot(bwb.m, aes(x = "hc.swe", y = "log(bwb.vol)")) +
 #   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  #  geom_segment(aes(xend = hc.swe, yend = .fitted), alpha = .2) +  # Note `.fitted`
   # geom_point(aes(alpha = abs(.std.resid))) +  # Note `.resid`
    #guides(alpha = FALSE) +
    #geom_point(aes(y = .fitted), shape = 1) +  # Note `.fitted`
    #theme_bw()



