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

rm.all.but(c("cd", "pred.yr", "run_date", "git_dir", "fig_dir", "input_dir", "data_dir", "input"))

# Import Data ------------------------------------------------------------------  
# Streamflow, April 1 SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
swe_q = read.csv(file.path(data_dir, input))
swe_q[swe_q == 0] <- 0.00001 # change zeros to a value so lm works
temps = read.csv(file.path(data_dir, 'ajTemps.csv'))
var = swe_q %>% select(-X) %>% inner_join(temps, by ="year") %>% select(-X)
var$div <- var$abv.h + var$abv.s
curtailments = read.csv(file.path(input_dir,'historic_shutoff_dates_071520.csv'))

write.csv(var, file.path(cd,'February_output/all_vars.csv'))

temp.ran = read.csv(file.path(data_dir,'aj_pred.temps.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beginning of year in accordance with my calculation of cm, consider changin to day of wy
wy<-seq(as.Date(paste(pred.yr,"-01-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")
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

# summary stats
mod_sum<-data.frame(array(NA,c(6,1)))
colnames(mod_sum)<-c("Vol Adj-R2")
rownames(mod_sum)<-c("bwb","bws","cc","sc", "bw.div", "sc.div")

# ------------------------------------------------------------------------------  
#
# April-Sept Volume Predictions
#
# ------------------------------------------------------------------------------ # 

modOut<- function(mod, pred.dat, wq, vol, swe, lastQ){
  '
  mod:     input model
  pred.dat: data.frame of prediction variables
  wq:       array of historic winter flows (e.g. hist$cc.wq)
  vol:      array of historic april-sept volumes  (hist$cc.vol)
  swe:      array of current swe in model
  meanSWE:  mean(arrays of historic SWE from ws snotel sites) #mean(hist$ccd+hist$sr, na.rm=T)
  lastQ:    last years summer streamflow volume (ac-ft) #var$cc.vol[var$year == pred.yr-1] 
  '
  pred.params.vol<-array(NA,c(1,2))
  output.vol<-array(NA,c(1,8))
  
  meanSWE <- mean(swe, trim=0, na.rm=T)
  sig<-summary(mod)$sigma
  pred.params.vol[1,2]<-sig
  #predict this years total volume at 95 % confidence
  predictions<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.vol[1,1]<-mean(predictions$fit, na.rm=T)
  #This years percent of mean winter flow
  output.vol[1,1]<-round(pred.dat[1,1]/mean(wq, na.rm=T),3) 
  #percent of mean SWE
  output.vol[1,2]<-round(sum(swe)/meanSWE,3) 
  # back-transformation of log-transformed data to expected value in original units, with lognormal residuals; 183 is the number of days between April-Sept and 1.98 converts back to cfs
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

# --------------------------------------------------
# Big Wood Hailey variables
hist <- var[var$year < pred.yr,] %>% select(bwb.vol.nat, gs.swe) 
# linear model
bwb_mod<-lm(log(bwb.vol.nat)~ log(gs.swe), data=hist) 
mod_sum[1,1]<-summary(bwb_mod)$adj.r.squared
#Feb 1 bwb Prediction Data
pred.dat<-var[var$year == pred.yr,] %>% select(gs.swe) 

# Big Wood at Hailey Model output
mod_out<- modOut(bwb_mod, pred.dat, var$bwb.wq[var$year < pred.yr], hist$bwb.vol.nat, hist$gs.swe, var$bwb.vol.nat[var$year == pred.yr-1])
#these could be formatted differently to be saved to the global env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir,"February/BWB_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bwb_mod))
plot(var$bwb.vol.nat[var$year < pred.yr]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \n April-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr & var$year > 1996,] %>% select(bws.vol.nat, bws.wq, cg.swe) 
# Big Wood at Stanton linear model
bws_mod<-lm(log(bws.vol.nat)~ log(bws.wq)+ log(cg.swe), data=hist) 
mod_sum[2,1]<-summary(bws_mod)$adj.r.squared
# Feb 1 BWS Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(bws.wq, cg.swe) 

# Big Wood at Stanton Natural Flow Model output
mod_out<- modOut(bws_mod, pred.dat, hist$bws.wq, hist$bws.vol.nat, hist$cg.swe, var$bws.vol.nat[var$year == pred.yr-1])
output.vol[2,] <- mod_out[[1]]
pred.params.vol[2,] <- mod_out[[2]]

#Plot modeled bws data for visual evaluation 
png(filename = file.path(fig_dir,"February/BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(bws_mod))
plot(var$bws.vol.nat[var$year < pred.yr & var$year > 1996]/1000,c(fits)/1000, lwd=2, xlim=c(0,730), ylim=c(0,730), xlab="Observed", ylab="Predicted",main="Big Wood at Stanton \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Silver Creek Winter flows, Snotel from Swede Peak
hist <- var[var$year < pred.yr,] %>% select(sc.vol.nat, bwb.wq, ga.swe, sp.swe, hc.swe) 
# Silver Creek linear model, note mixture of SWE from Big Wood and Little Wood basins
sc_mod<-lm(log(sc.vol.nat)~ ga.swe+ sp.swe + hc.swe + bwb.wq, data=hist)
mod_sum[4,1]<-summary(sc_mod)$adj.r.squared

# April 1 SC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(bwb.wq, ga.swe, sp.swe, hc.swe) 
# Silver Creek Model output
mod_out<- modOut(sc_mod, pred.dat, var$sc.wq[var$year < pred.yr], hist$sc.vol.nat, c(hist$ga.swe,hist$sp.swe,hist$hc.swe), var$sc.vol.nat[var$year == pred.yr-1])
output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]

# Plot sc modeled data for visual evaluation 
png(filename = file.path(fig_dir,"February/SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(sc_mod))
plot(hist$sc.vol.nat[complete.cases(hist)]/100,c(fits)/100, lwd=2, xlim=c(310,720), ylim=c(310,720), xlab="Observed", ylab="Predicted", main="Silver Creek \nApril-Sept Streamflow Vol (100 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# --------------------------------------------------
# Subset Camas Creek Winter flows, Snotel from Soldier Ranger Station, camas creek divide was not included in model selection 
hist <- var[var$year < pred.yr,] %>% select(cc.vol, cc.wq, ccd.swe) #vol is in ac-ft
# Camas Creek linear model
cc_mod<-lm(log(cc.vol)~log(cc.wq)+ccd.swe, data=hist) 
mod_sum[3,1]<-summary(cc_mod)$adj.r.squared
# February 1 CC Prediction Data 
pred.dat<-var[var$year == pred.yr,] %>% select(cc.wq, ccd.swe)
# Camas Creek Model output
mod_out<- modOut(cc_mod, pred.dat, hist$cc.wq, hist$cc.vol, hist$ccd.swe, var$cc.vol[var$year == pred.yr-1])
output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

#Plot CC modeled data for visual evaluation 
png(filename = file.path(fig_dir,"February/CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

fits<-exp(fitted(cc_mod))
plot(var$cc.vol[complete.cases(hist)]/1000,c(fits)/1000, lwd=2, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()


### Save model outputs for simulation runs (is this actually used later?)

write.csv(output.vol, file.path(cd,"pred.output.vol.csv"),row.names=T)
write.csv(pred.params.vol, file.path(cd,"pred.params.vol.csv"),row.names=T)

# --------------------
# Apr-Sept diversion predictions
#

pred.params.div<-array(NA,c(1,2))
colnames(pred.params.div)<-c("log.vol","sigma")
rownames(pred.params.div)<-c("sc.div")

# Total Big Wood Diversions ----

# draw from random normal distribution
# create normal distribution of years 
bw.div.sample<-sample(var$div,5000,replace=TRUE) 


# Silver Creek Diversions ----
hist <- var[var$year>1993 & var$year < pred.yr,] %>% select(sc.div, sc.wq, g.swe, lwd.swe, t.f, t.p) 
hist$temps<- rowMeans(cbind(hist$t.f, hist$t.p), na.rm=TRUE)
# linear model 
sc.div_mod<-lm(log(var$sc.div[var$year>1993 & var$year < pred.yr]) ~ log(sc.wq)+ temps+log(g.swe)+log(lwd.swe), data=hist) 
mod_sum[6,1] <- summary(sc.div_mod)$adj.r.squared 
# April 1 Prediction Data 
params<- var[var$year == pred.yr,] %>% select(sc.wq, g.swe, lwd.swe)
pred.dat<- params %>% slice(rep(1:n(), 5000))
pred.dat$temps<- temp.ran$aj.temps.cc
# Model output
preds.div<-predict(sc.div_mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)

pred.params.div[1,1]<-preds.div$fit[1]
pred.params.div[1,2]<-mean(preds.div$se.fit)

png(filename = file.path(fig_dir,"February/SC_Diversions_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
fits<-fitted(sc.div_mod)
plot(var$sc.div[var$year>1993 & var$year < pred.yr],exp(c(fits)), xlab="Observed", 
     ylab="Predicted", xlim=c(3300, 8200), ylim=c(3300, 8200))
abline(0,1,col="gray50",lty=1)
dev.off()


mod_sum<- round(mod_sum, 3)
png(file.path(fig_dir,"February/model_summary.png"), height = 25*nrow(mod_sum), width = 110*ncol(mod_sum))
grid.table(mod_sum)
dev.off()


# --------------------
# Draw a sample of volumes and water years with similar timing
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume at each gage

# check correlations between flow conditions across the basins
flow.data = var[var$year >= 1997,] %>% select(bwb.vol.nat, bws.vol.nat, cc.vol, sc.vol.nat, sc.div) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(flow.data, use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol, pred.params.div)
outer.prod<-as.matrix(pred.pars[,2])%*%t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (ac-ft)
vol.pars<-rbind(pred.params.vol, pred.params.div)
vol.sample<-mvrnorm(n=5000,mu=(vol.pars[,1]),Sigma=cov.mat)
colnames(vol.sample)<-c("bwb.nat","bws.nat","cc","sc.nat", "sc.div")
write.csv(exp(vol.sample), file.path(cd,"February_output/vol.sample.csv"),row.names=F)

#save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir,"February/correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()

# save output from correlations
write.csv(cov.mat, file.path(cd,"February_output/cov.mat.csv"),row.names=T)
write.csv(pred.pars, file.path(cd,"February_output/pred.pars.csv"),row.names=T)

# Plot boxplots of total annual flow from each model -> modelOutput.Rmd
png(filename = file.path(fig_dir,"February/sampled_volumes.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 

as.data.frame(exp(vol.sample)/10000) %>% pivot_longer(everything(),  names_to = "site", values_to = "value") %>%
  ggplot(aes(x=site, y=value, fill=site)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  theme_bw()+
  ggtitle("Sampled Irrigation Season Volumes") +
  xlab("")+
  ylab("Irrigation Season Volume (10,000 ac-ft)")
dev.off()

# Draw sample of years with similar center of mass (timing)
cm.data = var[var$year >= 1997 & var$year < pred.yr,]
cm.data = cm.data %>% select(year, bwb.cm.nat, bws.cm.nat,cc.cm, sc.cm) 

# create normal distribution of years 
CMyear.sample<-sample(cm.data$year,5000,replace=TRUE) 
#save the probabilities for use in the simulation
write.csv(CMyear.sample, file.path(cd, "February_output/CMyear.sample.csv"),row.names=F)


# --------------------
# Curtailment predictions
#


