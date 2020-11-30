# ---------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# October 27th, 2020
# Linear models to predict total April-September streamflow volume and center of mass 
# based on average winter flows, current SWE and temperature; model selection explored
# 'streamflow_model_xploration.R' using BIC
#
# This model was informed by the statstical tools developed for the Henry's Fork 
# Foundation by Rob VanKirk
# -----------------------------------------------------------------------------  
#+ setup, echo= FALSE
source(file.path("code", "packages.R"))

rm(list=ls())

cd = '~/Desktop/Data/WRWC'
pred.yr <- 2020

# Import Data ------------------------------------------------------------------  
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(cd,'usgs_sites.csv'))
swe_q = read.csv(file.path(cd,'all_April1.csv'))
swe_q[swe_q == 0] <- 0.00001 # change zeros to a value so lm works
temps = read.csv(file.path(cd, 'ajTemps.csv'))
var = swe_q %>% select(-X) %>% inner_join(temps, by ="year") %>% select(-X)

temp.ran = read.csv(file.path(cd,'rand.apr.jun.temp.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beinning of year in accordance with my calculation of cm, consider changin to day of wy
wy<-seq(as.Date("2019-01-01"),as.Date("2019-09-30"),"day") #update to automate based on new.yr
wy<-data.frame(wy,1:273)
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------  
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),8))
rownames(output.vol)<-stream.id
output.vol<-output.vol[-4,]
colnames(output.vol)<-c("Predictors Vol % of mean","Predictors swe % of mean", "Pred. Vol (cfs)", "Pred. Vol % of mean", "90% exc. cfs", "90% exc. % or mean", "Prev Year Vol (cfs)", "Prev Year % of mean Volume")

pred.params.vol<-array(NA,c(4,2))
rownames(pred.params.vol)<-c("bwb.vol","bws.vol","cc.vol","sc.vol")
colnames(pred.params.vol)<-c("log.vol","sigma")

# center of mass
output.cm<-data.frame(array(NA,c(5,5)))
rownames(output.cm)<-stream.id
output.cm<-output.cm[-4,]
colnames(output.cm)<-c("temp-mean","% of mean","cm","cm-mean","cm.date")

pred.params.cm<-array(NA,c(4,2))
rownames(pred.params.cm)<-c("bwb.cm","bws.cm","cc.cm","sc.cm")
colnames(pred.params.cm)<-c("cm","sigma")

swe.new<-data.frame(t(c(rep(NA,10))))
names(swe.new)<-c("cg", "g", "gs", "hc", "lwd", "ds", "ccd", "sr", "ga", "sp")

# summary stats
mod_sum<-data.frame(array(NA,c(4,2)))
colnames(mod_sum)<-c("Vol Adj-R2", "CM ADj-R2")
rownames(mod_sum)<-c("bwb","bws","cc","sc")

# ------------------------------------------------------------------------------  
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

#April 1 CC data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("cc.wq","ccd.swe","sr.swe")
pred.dat$cc.wq<- var$cc.wq[var$year == pred.yr] #this years base flow
pred.dat$ccd.swe<- var$ccd.swe[var$year == pred.yr]
pred.dat$sr.swe<- var$sr.swe[var$year == pred.yr] # current April 1 SWE

# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, hist$cc.wq, hist$cc.vol, mean(hist$sr.swe, na.rm=T), var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

#Plot CC modeled data for visual evaluation 
png(filename = "CC_modelFit.png",
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(cc_mod))
plot(var$cc.vol[6:32]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Big Wood Winter flows, Snotel from  Galena & Galena Summit, Hyndman
hist <- var[var$year < pred.yr,] %>% select(bwb.vol, bwb.wq, g.swe, gs.swe, hc.swe) 
# Big Wood at Hailey linear model
bwb_mod<-lm(log(bwb.vol)~log(bwb.wq)+log(g.swe)+ log(gs.swe)+ log(hc.swe), data=hist) 
mod_sum[1,1]<-summary(bwb_mod)$adj.r.squared

#April 1 bwb data to use for prediction 
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
#these could be formatted differently to be saved to the gloabl env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]


#Plot bwb modeled data for visual evaluation 
png(filename = "BWB_modelFit.png",
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bwb_mod))
plot(var$bwb.vol[1:32]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()
# --------------------------------------------------
# Subset Big Wood at Stanton Winter flows, Snotel from Chocolate Gulch, Galena & Galena Summit, Hyndman, Lost-Wood Divide and Dollarhide
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.vol, bws.wq, cg.swe, g.swe, gs.swe, hc.swe, lwd.swe) 
# Big Wood at Stanton linear model
bws_mod<-lm(log(bws.vol)~bws.wq+ g.swe+ log(gs.swe)+ log(hc.swe), data=hist) 
mod_sum[2,1]<-summary(bws_mod)$adj.r.squared

#April 1 bws data to use for prediction 
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

output.vol[2,] <- mod_out[[1]]
pred.params.vol[2,] <- mod_out[[2]]

#Plot modeled bws data for visual evaluation 
png(filename = "BWS_modelFit.png",
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bws_mod))
plot(var$bws.vol[10:32]/1000,c(fits)/1000, lwd=2, xlim=c(0,715), ylim=c(0,715), xlab="Observed", ylab="Predicted",main="Big Wood at Stanton \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Swede Peak
# sc.wq, sp.swe, g.swe, log cg
hist <- var[var$year < pred.yr,] %>% select(sc.vol, sc.wq, sp.swe, g.swe, cg.swe) 
# Silver Creek linear model, note mixture of SWE from Big Wood and Little Wood basins
sc_mod<-lm(log(sc.vol)~sc.wq+sp.swe + g.swe + log(cg.swe), data=hist)  #look at fit w.o g,swe
mod_sum[4,1]<-summary(sc_mod)$adj.r.squared

#April 1 sc data to use for prediction 
pred.dat<-data.frame(array(NA,c(1,4)))
names(pred.dat)<-c("sc.wq","sp.swe","g.swe","cg.swe")
pred.dat$sc.wq<- var$sc.wq[var$year == pred.yr] #this years base flow
pred.dat$sp.swe<- var$g.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$g.swe<- var$sp.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr]

# Silver Creek Model output
mod_out<- modOut(sc_mod, pred.dat, hist$sc.wq, hist$sc.vol, mean(hist$sp.swe,  hist$g.swe,  hist$cg.swe, trim=0, na.rm=T), var$sc.vol[var$year == pred.yr-1])

output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]


#Plot sc modeled data for visual evaluation 
png(filename = "SC_modelFit.png",
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-fitted(sc_mod)
plot(var$sc.vol[1:32]/1000,c(fits)/1000, lwd=2, xlim=c(22,65), ylim=c(22,65), xlab="Observed", ylab="Predicted", main="Silver Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()
# ------------------------------------------------------------------------------  
#
# Center of Mass Predictions
#
# ------------------------------------------------------------------------------ # 
modOutcm<- function(mod.cm, pred.dat, pred.dat.temps, hist.temps, hist.cm){
  '
  mod.cm:           input model
  pred.dat:         data.frame of prediction variables
  pred.data.temps:  concatenated vectors of temperature data used in predictions
  hist.temps:       concatenated vectors of historic temperature data
  hist.cm:          historic cm
  '
  pred.params.cm<-array(NA,c(1,2))
  output.cm<-array(NA,c(1,5))
  
  predictions<-predict(mod.cm,newdata=pred.dat)
  
  pred.params.cm[1,1]<-mean(predictions)
  pred.params.cm[1,2]<-summary(mod.cm)$sigma
  #sqrt(sig^2+var(predictions)) - this was in RVK code, but only one prediction, so not sure how to take the variance of it?
  
  output.cm[1,1]<-round(mean(pred.dat.temps)-mean(hist.temps,na.rm=T),3)
  output.cm[1,2]<-round(sum(pred.dat.temps)/mean(hist.temps, na.rm =T),3) 
  output.cm[1,3]<-round(mean(predictions),0) 
  output.cm[1,4]<-round(mean(predictions)-mean(hist.cm,na.rm=T),0) 
  output.cm[1,5]<-format(wy$Date[wy$day==round(mean(predictions),0)],"%b-%d")
  
  return(list(output.cm, pred.params.cm))
}

# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek 
# divide & temperature from Fairfield agrimet site
hist <- var[var$year < pred.yr,] %>% select(cc.cm, ccd.swe, sr.swe, t.f) 
# Camas Creek linear model
cc_mod.cm<-lm(cc.cm~ccd.swe + sr.swe+ t.f, data=hist) 
mod_sum[3,2]<-summary(cc_mod.cm)$adj.r.squared 

#April 1 cc data for prediction 
pred.dat<-data.frame(array(NA,c(1,3)))
names(pred.dat)<-c("ccd.swe","sr.swe","t.f")
pred.dat$ccd.swe<- var$ccd.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$sr.swe<- var$sr.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$t.f<- var$t.f[var$year == pred.yr] 

# Camas Creek Model output
mod_out<- modOutcm(cc_mod.cm, pred.dat, pred.dat$t.f, hist$t.f, hist$cc.cm)

output.cm[3,] <- mod_out[[1]]
pred.params.cm[3,] <- mod_out[[2]]

#Plot sc modeled data for visual evaluation 
#png(filename = "CC_CMmodelFit.png",
    #width = 5.5, height = 5.5,units = "in", pointsize = 12,
    #bg = "white", res = 600, type ="quartz") 

#fits<-fitted(cc_mod.cm)
#plot(var$cc.cm[6:32],c(fits), lwd=2, xlab="Observed", ylab="Predicted", main="Camas Creek Center of Mass")
#abline(0,1,col="gray50",lty=1)
#dev.off()


#big wood at hailey
hist <- var[var$year < pred.yr,] %>% select(bwb.cm, cg.swe, g.swe, hc.swe, t.cg, t.g, t.gs, t.hc, t.lw) 
#bwb linear model
bwb_mod.cm <-lm(bwb.cm ~ g.swe+t.cg+ t.g+t.gs+t.hc+t.lw +log(cg.swe)+log(hc.swe), data=hist)
mod_sum[1,2]<-summary(bwb_mod.cm)$adj.r.squared 

#April 1 bwh data for prediction 
pred.dat<-data.frame(array(NA,c(1,8)))
names(pred.dat)<-c("g.swe", "cg.swe","hc.swe", "t.cg", "t.g","t.gs","t.hc","t.lw")
pred.dat$g.swe<- var$g.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr]
pred.dat$hc.swe<- var$hc.swe[var$year == pred.yr]
pred.dat$t.cg<- var$t.cg[var$year == pred.yr] 
pred.dat$t.g<- var$t.g[var$year == pred.yr] 
pred.dat$t.gs<- var$t.gs[var$year == pred.yr] 
pred.dat$t.hc<- var$t.hc[var$year == pred.yr] 
pred.dat$t.lw<- var$t.lw[var$year == pred.yr] 

# Big Wood Hailey Model output
mod_out<- modOutcm(bwb_mod.cm, pred.dat, c(pred.dat$t.cg,pred.dat$t.g,pred.dat$t.gs,pred.dat$t.hc,pred.dat$t.lw), c(hist$t.cg,hist$t.g,hist$t.gs,hist$t.hc,hist$t.lw), hist$bwb.cm)

output.cm[1,] <- mod_out[[1]]
pred.params.cm[1,] <- mod_out[[2]]

# --------------------
# Big wood at stanton
# 
hist <- var[var$year < pred.yr,] %>% select(bws.cm, cg.swe, g.swe, gs.swe, hc.swe, t.cg, t.g, t.lw) 
# BWS linear model
bws_mod.cm <-lm(bws.cm ~ g.swe + t.cg+ t.g +t.lw +log(cg.swe)+log(gs.swe)+log(hc.swe), data=hist)
mod_sum[2,2]<-summary(bws_mod.cm)$adj.r.squared 

# April 1 BWS data for prediction 
pred.dat<-data.frame(array(NA,c(1,11)))
names(pred.dat)<-c("g.swe", "cg.swe","hc.swe","gs.swe", "t.cg", "t.g","t.lw")
pred.dat$g.swe<- var$g.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr]
pred.dat$hc.swe<- var$hc.swe[var$year == pred.yr]
pred.dat$gs.swe<- var$gs.swe[var$year == pred.yr]
pred.dat$t.cg<- var$t.cg[var$year == pred.yr] 
pred.dat$t.g<- var$t.g[var$year == pred.yr] 
pred.dat$t.lw<- var$t.lw[var$year == pred.yr] 

# BWS Model output
mod_out<- modOutcm(bws_mod.cm, pred.dat, c(pred.dat$t.cg,pred.dat$t.g,pred.dat$t.lw), c(hist$t.cg,hist$t.g,hist$t.lw), hist$bws.cm)

output.cm[2,] <- mod_out[[1]]
pred.params.cm[2,] <- mod_out[[2]]

# --------------------
# Subset Silver Creek Winter flows, Snotel from Chocolate Gulch, Hyndaman, Lost Wood Div. & Swede Peak
#
hist <- var[var$year < pred.yr,] %>% select(sc.cm, sc.wq, cg.swe, hc.swe, lwd.swe, t.cg, t.gs, sp.swe) 
# Silver Creek linear model
# note here that this includes swe from the big wood and the little wood 
sc_mod.cm<-lm(sc.cm~ cg.swe+ hc.swe+ lwd.swe+t.cg+t.gs+log(sp.swe)+log(sc.wq), data=hist) 
mod_sum[4,2]<-summary(sc_mod.cm)$adj.r.squared 
mod_sum<- round(mod_sum, 3)
# April 1 SC data for prediction 
pred.dat<-data.frame(array(NA,c(1,7)))
names(pred.dat)<-c("cg.swe", "hc.swe","lwd.swe","sp.swe", "t.cg", "t.gs","sc.wq")
pred.dat$cg.swe<- var$cg.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$hc.swe<- var$hc.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$lwd.swe<- var$lwd.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$sp.swe<- var$sp.swe[var$year == pred.yr] # current April 1 SWE
pred.dat$t.cg<- var$t.cg[var$year == pred.yr] 
pred.dat$t.gs<- var$t.gs[var$year == pred.yr] 
pred.dat$sc.wq<- var$sc.wq[var$year == pred.yr] 

# SC Model output
mod_out<- modOutcm(sc_mod.cm, pred.dat, c(pred.dat$t.cg,pred.dat$t.gs), c(hist$t.cg,hist$t.gs), hist$sc.cm)

output.cm[4,] <- mod_out[[1]]
pred.params.cm[4,] <- mod_out[[2]]

### Save model outputs for simulation runs

write.csv(output.vol, file.path(cd,"pred.output.vol.csv"),row.names=T)
write.csv(pred.params.vol, file.path(cd,"pred.params.vol.csv"),row.names=T)
write.csv(output.cm, file.path(cd,"pred.output.cm.csv"),row.names=T)
write.csv(pred.params.cm, file.path(cd,"pred.params.cm.csv"),row.names=T)

# --------------------
# Apr-Sept diversion & reach gain predictions
#



# --------------------
# Draw random water years
#

# check correlations between flow conditions across the basins
flow.data = var[var$year >= 1997,]
flow.data= flow.data %>% select(bwb.vol, bwb.cm, bws.vol, bws.cm, cc.vol, cc.cm, sc.vol, sc.cm) 
# make this an output table?
cor.mat1< - round(cor(flow.data,use="pairwise.complete"),2)  

# check correlations between natural flow and diversions

#
pred.pars<-rbind(pred.params.vol,pred.params.cm)
cor.mat<-cor(cbind(log(flow.data[c(1,3,5,7)]),flow.data[c(2,4,6,8)]),use="pairwise.complete")

outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# output for simulations?
write.csv(cov.mat, file.path(cd,"cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(cd,"pred.pars.csv"),row.names=T)


# Draw flow volumes using mvnorm (ac-ft)
flow.sample<-mvrnorm(n=5000,mu=(pred.params.vol[,1]),Sigma=cov.mat[1:4,1:4])
colnames(flow.sample)<-c("bwb","bws","cc","sc")

write.csv(exp(flow.sample), file.path(cd,"flow.sample.csv"),row.names=F)

# Draw center of mass timing
cm.data = var[var$year >= 1997,]
cm.data = cm.data %>% select(year, bwb.cm, bws.cm,cc.cm, sc.cm) 
cm.data$prob<-NA

for(i in 1:dim(cm.data)[1]){
  vec<-cm.data[i,2:5]
  cm.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-0.5,
                          upper=as.numeric(vec)+0.5,mean=pred.params.cm[,1],sigma=cov.mat[5:8,5:8])[1]
}


cm.data$prob<-cm.data$prob/sum(cm.data$prob)

CMyear.sample<-sample(cm.data$year,5000,replace=TRUE,prob=cm.data$prob)

hist(CMyear.sample,breaks=seq(1997,2020,1))

summary(as.factor(CMyear.sample))/5000

write.csv(CMyear.sample, file.path(cd,"CMyear.sample.csv"),row.names=F)

#options(knitr.duplicate.label = "allow")
#rmarkdown::render("/Users/kek25/Documents/GitRepos/WRWC/code/streamflow_model.R")

#ExceedanceProbabilities
#excProb(x, threshold=0, random=FALSE, template=NULL, templateIdCol=NULL,nuggetInPrediction=TRUE)
