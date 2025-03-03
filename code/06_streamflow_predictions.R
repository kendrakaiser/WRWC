# ---------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# January 18th, 2021
#
# Linear models are run to predict total April-September streamflow volume and 
# center of mass based on average winter flows, current SWE and predicted temperature; 
# 
# This model was informed by the statistical tools developed for the Henry's Fork 
# Foundation by Rob VanKirk
# -----------------------------------------------------------------------------  

# Import Data ------------------------------------------------------------------  
stream.id<-c("bwh", "bws", "cc", "sc") #automate to no be hard coded

# ------------------------------------------------------------------------------  
# Create sequence of non-leap year dates, changed to start at the beginning of year in accordance with my calculation of cm, consider changing to day of wy
wy<-seq(as.Date(paste(pred.yr,"-01-01",sep="")),as.Date(paste(pred.yr,"-09-30",sep="")),"day")
wy<-data.frame(wy,1:length(wy))
colnames(wy)<-c("Date","day")

# ------------------------------------------------------------------------------  
# Setup output arrays
# ------------------------------------------------------------------------------ # 

# volumes
output.vol<-array(NA,c(length(stream.id),1))
colnames(output.vol)<-c("Pred. Vol (KAF)")
rownames(output.vol)<-c("Big Wood Hailey","Big Wood Stanton","Camas Creek","Silver Creek")

pred.params.vol<-array(NA,c(4,4))
rownames(pred.params.vol)<-c("bwh.irr_vol","bws.irr_vol","cc.irr_vol","sc.irr_vol")
colnames(pred.params.vol)<-c("irr_vol","sigma", "low.irr_vol", "upp.irr_vol")

# center of mass
output.cm<-data.frame(array(NA,c(4,3)))
rownames(output.cm)<-stream.id
colnames(output.cm)<-c("cm","cm-mean","cm.date") #"Hist Nov-Jan Temp","Nov-Jan Temps",

pred.params.cm<-array(NA,c(4,2))
rownames(pred.params.cm)<-c("bwh.cm","bws.cm","cc.cm","sc.cm")
colnames(pred.params.cm)<-c("cm","sigma")

# summary stats
mod_sum<-data.frame(array(NA,c(4,2)))
colnames(mod_sum)<-c("Irr Vol Adj-R2", "CM Adj-R2")
rownames(mod_sum)<-c("Big Wood Hailey","Big Wood Stanton","Camas Creek","Silver Creek")

# ------------------------------------------------------------------------------  
# April-Sept Volume Predictions
# an additional function can be made to clean these up now that they are all automated
# ------------------------------------------------------------------------------ # 

modOut<- function(mod, pred.dat){
  '
  mod:     input model
  pred.dat: data.frame of prediction variables
  '
# Test Data
 #  mod<- vol_models$bwh_mod
 #  pred.dat<- var[var$wateryear == pred.yr,] %>% dplyr::select(vol_mod_sum$bwh$vars)
 # hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(bwh.irr_vol, vol_mod_sum$bwh$vars) %>% filter(complete.cases(.))
 #  vol<- hist$bwh.irr_vol

  pred.params.vol<-array(NA,c(1,4))
  
  #predict this years total volume at 90 % confidence
  predictions<-predict(mod, newdata=pred.dat, se.fit=TRUE, interval="prediction",level=0.90)

  pred.params.vol[1,1]<-predictions$fit[1] #mean prediction 
  pred.params.vol[1,2]<-summary(mod)$sigma 
  pred.params.vol[1,3]<-predictions$fit[2] #lower prediction interval
  pred.params.vol[1,4]<-predictions$fit[3] #upper prediction interval

  output.vol<-predictions$fit[1]
  
  return(list(output.vol, pred.params.vol))
}

# --------------------------------------------------
#bwh Prediction Data
pred.dat = current_data[,names(current_data) %in% c('wateryear',vol_models$bwh_mod$vars)]
# Big Wood at Hailey Model output
mod_sum[1,1]<-summary(vol_models$bwh_mod)$adj.r.squared
mod_out<- modOut(vol_models$bwh_mod, pred.dat)
vol_models$bwh_mod$predictors=pred.dat

#these could be formatted differently to be saved to the global env. within the function
output.vol[1,] <- mod_out[[1]]
pred.params.vol[1,] <- mod_out[[2]]


# --------------------------------------------------
#  bws Prediction Data 
pred.dat = current_data[,names(current_data) %in% c('wateryear',vol_models$bws_mod$vars)]

# Big Wood at Stanton Flow Model output 
mod_sum[2,1]<-summary(vol_models$bws_mod)$adj.r.squared
mod_out<- modOut(vol_models$bws_mod, pred.dat)
vol_models$bws_mod$predictors=pred.dat

output.vol[2,] <- mod_out[[1]] 
pred.params.vol[2,] <- mod_out[[2]]

# --------------------------------------------------
# SC Prediction Data 
pred.dat = current_data[,names(current_data) %in% c('wateryear',vol_models$sc_mod$vars)]

# Silver Creek Model output
mod_sum[4,1]<-summary(vol_models$sc_mod)$adj.r.squared
mod_out<- modOut(vol_models$sc_mod, pred.dat)
vol_models$sc_mod$predictors=pred.dat

output.vol[4,] <- mod_out[[1]]
pred.params.vol[4,] <- mod_out[[2]]

# --------------------------------------------------
#CC Prediction Data 
pred.dat = current_data[,names(current_data) %in% c('wateryear',vol_models$cc_mod$vars)]

# Camas Creek Model output
mod_sum[3,1]<-summary(vol_models$cc_mod)$adj.r.squared
mod_out<- modOut(vol_models$cc_mod, pred.dat)
vol_models$cc_mod$predictors=pred.dat

output.vol[3,] <- mod_out[[1]]
pred.params.vol[3,] <- mod_out[[2]]

#error catch for NA in pred params
#TODO: Move this into the function and add model name into error
try(if(any(is.na(pred.params.vol))) stop("NA in Predicted Parameters"))

tryCatch({is.integer(pred.params.vol)}, error = function(e) {message("error:\n", "NA in Predicted Parameters")})

any(is.na(pred.params.vol))
# ------------------------------------------------------------------------------  
#
# Center of Mass Predictions
#
# ------------------------------------------------------------------------------ # 
modOutcm<- function(mod.cm, pred.dat, hist.temps, cur.temps, hist.cm){
  '
  mod.cm:           input model
  pred.dat:         data.frame of prediction variables
  cur.temps:        numeric of winter temperatures
  hist.temps:       historic temperature data
  hist.cm:          historic cm
  '
  
  pred.params.cm<-array(NA,c(1,2))
  output.cm<-data.frame(predCM=integer(), diffCM=integer(), dateCM=character())
  sig<-summary(mod.cm)$sigma
  
  predictions<-predict(mod.cm,newdata=pred.dat)
  
  pred.params.cm[1,1]<-mean(predictions)
  pred.params.cm[1,2]<-sqrt(sig^2+var(predictions)) 
  if (is.na(pred.params.cm[1,2])){pred.params.cm[1,2]<-sig}
  
  cm_predict=round(mean(predictions,na.rm=T))
  
  #limit cm predictions to the observed range
  if(all(cm_predict>hist.cm)){
    cm_predict=round(max(hist.cm, na.rm = T))
    pred.params.cm[1,1]=round(max(hist.cm, na.rm = T))
  }

  if(all(cm_predict<hist.cm)){
    cm_predict=round(min(hist.cm, na.rm=T))
    pred.params.cm[1,1]=round(min(hist.cm, na.rm=T))
  }

  output.cm[1,1]<-cm_predict
  output.cm[1,2]<-cm_predict-mean(hist.cm) 
  output.cm[1,3]<-format(wy$Date[wy$day==cm_predict],"%b-%d")
  
  return(list(output.cm, pred.params.cm))
}

#-------------------------------------------------------------------------------
# Big Wood at Hailey center of mass
#cm_models$bwh_cm_mod
sub_params<- cm_models$bwh_cm_mod$vars[-grep('aj', cm_models$bwh_cm_mod$vars)]
aj_params<-cm_models$bwh_cm_mod$vars[grep('aj', cm_models$bwh_cm_mod$vars)]
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(bwh.cm, cm_models$bwh_cm_mod$vars) %>% filter(complete.cases(.))

# Prediction Data with modeled temperature data
pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::slice(rep(1:n(), 5000))
pred.dat[aj_params] <- aj.pred.temps[aj_params]

# Big Wood Hailey Model output
mod_sum[1,2]<-summary(cm_models$bwh_cm_mod)$adj.r.squared
mod_out<- modOutcm(cm_models$bwh_cm_mod, pred.dat, hist%>% dplyr::select(contains('nj')), 
                   (var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$bwh.cm)
output.cm[1,] <- mod_out[[1]]
pred.params.cm[1,] <- mod_out[[2]]

cm_models$bwh_cm.mod$predictors=pred.dat

# --------------------
# Big Wood at Stanton
#cm_models$bws_cm_mod
sub_params<- cm_models$bws_cm_mod$vars[-grep('aj', cm_models$bws_cm_mod$vars)]
aj_params<-cm_models$bws_cm_mod$vars[grep('aj', cm_models$bws_cm_mod$vars)]
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(bws.cm, cm_models$bws_cm_mod$vars) %>% filter(complete.cases(.))

# Prediction Data with modeled temperature data
pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
pred.dat[aj_params] <- aj.pred.temps[aj_params]

# Big Wood Stanton Model output
mod_sum[2,2]<-summary(cm_models$bws_cm_mod)$adj.r.squared
mod_out<- modOutcm(cm_models$bws_cm_mod, pred.dat, hist%>% dplyr::select(contains('nj')), 
                   (var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$bws.cm)
output.cm[2,] <- mod_out[[1]]
pred.params.cm[2,] <- mod_out[[2]]

cm_models$bws_cm.mod$predictors=pred.dat

# --------------------
# Silver Creek Center of Mass
#cm_models$sc_cm_mod
# added 'if' statement here because March SC CM doesn't use aj temperatures
if (any(grepl('aj', cm_models$sc_cm_mod$vars))){ 
  sub_params<- cm_models$sc_cm_mod$vars[-grep('aj', cm_models$sc_cm_mod$vars)]
  aj_params<-cm_models$sc_cm_mod$vars[grep('aj', cm_models$sc_cm_mod$vars)]
  # Prediction Data with modeled temperature data
  pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
  pred.dat[aj_params] <- aj.pred.temps[aj_params]
} else {
  sub_params<- cm_models$sc_cm_mod$vars
  pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params))
}

hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(sc.cm, cm_models$sc_cm_mod$vars) %>% filter(complete.cases(.))

# Silver Creek CM Model output
mod_sum[4,2]<-summary(cm_models$sc_cm_mod)$adj.r.squared
mod_out<- modOutcm(cm_models$sc_cm_mod, pred.dat, hist%>% dplyr::select(contains('nj')), 
                   (var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$sc.cm)
output.cm[4,] <- mod_out[[1]]
pred.params.cm[4,] <- mod_out[[2]]
cm_models$sc_cm.modpredictors=pred.dat

# --------------------
# Camas Creek Center of Mass
#cm_models$cc_cm_mod
sub_params<- cm_models$cc_cm_mod$vars[-grep('aj', cm_models$cc_cm_mod$vars)]
aj_params<-cm_models$cc_cm_mod$vars[grep('aj', cm_models$cc_cm_mod$vars)]
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(cc.cm, cm_models$cc_cm_mod$vars) %>% filter(complete.cases(.))

#Prediction Data with modeled temperature data
pred.dat<-var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% slice(rep(1:n(), 5000))
pred.dat[aj_params] <- aj.pred.temps[aj_params]
# pred.dat$ga.aj_t <-4.1
# pred.dat$sm.aj_t<- 8.5

# Camas Creek Model output
mod_sum[3,2]<-summary(cm_models$cc_cm_mod)$adj.r.squared
mod_out<- modOutcm(cm_models$cc_cm_mod, pred.dat, hist %>% dplyr::select(contains('nj')), 
                   (var[var$wateryear == pred.yr,] %>% dplyr::select(all_of(sub_params)) %>% dplyr::select(contains('nj'))), 
                   hist$cc.cm)
output.cm[3,] <- mod_out[[1]]
pred.params.cm[3,] <- mod_out[[2]]

cm_models$cc_cm.mod$predictors=pred.dat


#write.csv(output.vol, file.path(model_out,"pred.output.vol.csv"),row.names=T)
#write.csv(pred.params.vol, file.path(model_out,"pred.params.vol.csv"),row.names=T)
#write.csv(output.cm, file.path(model_out,"pred.output.cm.csv"),row.names=T)
#write.csv(pred.params.cm, file.path(model_out,"pred.params.cm.csv"),row.names=T)

# ------------------------------------------------------------------------------
# Draw a sample of volumes and water years with similar timing
# ------------------------------------------------------------------------------
# These samples are drawn from multivariate normal distributions which are 
# created from the correlation between total volume and center of mass (timing)
# between each gage

# calculate correlations between flow conditions across the basins
flow.data = var[var$wateryear >= 1997 & var$wateryear < pred.yr,] %>% dplyr::select(bwh.irr_vol, 
      bwh.cm, bws.irr_vol, bws.cm, cc.irr_vol, cc.cm, sc.irr_vol, sc.cm) 

# calculate correlations between gages' total volume, diversions and center of mass
cor.mat<-cor(cbind(flow.data[c(1,3,5,7)],flow.data[c(2,4,6,8)]),use="pairwise.complete")

# create covariance matrix by multiplying by each models standard error
pred.pars<-rbind(pred.params.vol[,1:2], pred.params.cm)
outer.prod<-as.matrix(pred.pars[,2]) %*% t(as.matrix(pred.pars[,2]))
cov.mat<-cor.mat*outer.prod

# Draw flow volumes using multivariate normal distribution (logged; ac-ft)
vol.sample<-data.frame(mvrnorm(n=5000,mu=(pred.params.vol[,1]),Sigma=cov.mat[1:4,1:4]))
colnames(vol.sample)<-c("Big Wood Hailey", "Big Wood Stanton","Camas Creek","Silver Creek")

# save correlation matrix for model details report
cor.mat.out<-as.data.frame(round(cor.mat,2))
png(file.path(fig_dir_mo,"correlation_matrix.png"), height = 25*nrow(cor.mat.out), width = 80*ncol(cor.mat.out))
grid.table(cor.mat.out)
dev.off()

########### transform outputs from log to linear space ##############
output.vol=exp(output.vol)/1000
vol.sample=exp(vol.sample)/1000

### Save model outputs 
# --------------------
png(file.path(fig_dir_mo,"pred.volumes.png"), height = 25*nrow(output.vol), width = 130*ncol(t(output.vol)))
grid.table(t(output.vol))
dev.off()

png(file.path(fig_dir_mo,"pred.cm.png"), height = 30*nrow(output.vol), width = 90*ncol(output.vol))
grid.table(output.cm)
dev.off()



# ------------------------------------------------------------------------------
# Create distribution and draw samples of CENTER of MASS & Volume
# ------------------------------------------------------------------------------
# Draw sample of years with similar center of mass (timing)
cm.data = var[var$wateryear >= 1997 & var$wateryear < pred.yr,]
cm.data = cm.data %>% dplyr::select(wateryear, bwh.cm, bws.cm,cc.cm, sc.cm) 
cm.data$prob<-NA

samp.sd.cm<- c(sd(cm.data$bwh.cm[cm.data$wateryear < pred.yr]), sd(cm.data$bws.cm[cm.data$wateryear < pred.yr]), sd(cm.data$cc.cm[cm.data$wateryear < pred.yr]), sd(cm.data$sc.cm[cm.data$wateryear < pred.yr]))
var.fore.cm<- pred.params.cm[,2] + samp.sd.cm

# pmvnorm calculates the distribution function of the multivariate normal distribution
for(i in 1:dim(cm.data)[1]){
  vec<-cm.data[i,2:5] # center of mass at each site for a given year
  cm.data$prob[i]<-pmvnorm(lower=as.numeric(vec)-(var.fore.cm), # use the location specific std @ 95% CI
                           upper=as.numeric(vec)+(var.fore.cm),mean=pred.params.cm[,1],sigma=cov.mat[5:8,5:8])[1] #need to adjust this to have the upper and lower limits be wider? e.g right now the upper and lowers are only one day off of the original??
  }

# create normal distribution of years 
CMyear.sample<-sample(cm.data$wateryear,5000,replace=TRUE, prob=cm.data$prob) 
# Of the 5000 replicates show the percentage that each year represents
cm_prob<-as.data.frame(summary(as.factor(CMyear.sample))/5000)
colnames(cm_prob)<- c("% of sample")
cm_prob<- cm_prob*100

cm_prob$year = rownames(cm_prob)

dbWriteTable(conn,"cm_prob", cm_prob, overwrite=TRUE)

png(file.path(fig_dir_mo,"CM_summary_prob.png"), height = 50*nrow(cm_prob), width = 200*ncol(cm_prob))
grid.table(cm_prob)
dev.off()

#save the probabilities for use in the simulation
#write.csv(CMyear.sample, file.path(model_out, paste0("CMyear.sample-", end_date,".csv")),row.names=F)


# TESTING
# Draw sample of years with similar volume for comparison -- 
vol.data = var[var$wateryear >1996 & var$wateryear < pred.yr,]%>% dplyr::select(wateryear, bwh.irr_vol, bws.irr_vol, cc.irr_vol, sc.irr_vol) 
vol.data$prob<-NA

#TODO: Write this up in a methods section
samp.sd<- c(sd(log(var$bwh.irr_vol)), sd(log(var$bws.irr_vol)), sd(log(var$cc.irr_vol)), sd(log(var$sc.irr_vol)))
var.fore<- pred.params.vol[,2] + samp.sd

# pmvnorm calculates the distribution function of the multivariate normal distribution
#for(i in 1:dim(vol.data)[1]){
 #   vec<-log(vol.data[i,2:5])
    
  #  vol.data$prob[i]<-pmvnorm(lower=as.numeric(vec-var.fore),  # pred.params.vol[,2] +/- standard error (pred.params.vol$sigma) would return effectively one year
   #                           upper=as.numeric(vec+var.fore), #
    #                          mean=pred.params.vol[,1],corr=cor.mat[1:4,1:4])[1]
    #vol.diff<- rbind(vol.diff, exp(as.numeric(vec+var.fore)) - exp(as.numeric(vec-j)))
#}

# this part is broken
#vol_prob<-as.data.frame(summary(as.factor(vol.sample))/5000)*100
#colnames(vol_prob)<- c("% of sample")

#png(file.path(fig_dir_mo,"vol_prob.png"), height = 50*nrow(vol_prob), width = 200*ncol(vol_prob))
#grid.table(vol_prob)
#dev.off()

#--------------
# additional variables to summarize in comparison to previous years

#, wq.cur, wq, vol, lastQ
# var$bwh.wq[var$wateryear == pred.yr], var$bwh.wq[var$wateryear < pred.yr], hist$bwh.irr_vol, var$bwh.irr_vol[var$wateryear == pred.yr-1]
# wq.cur:   this years winter baseflow
# wq:       array of historic winter flows (e.g. hist$cc.wq)
# vol:      array of historic april-sept volumes  (hist$cc.vol)
# lastQ:    last years summer streamflow volume (ac-ft) #var$cc.vol[var$wateryear == pred.yr-1]
#wq.cur<-var$bwh.wq[var$wateryear == pred.yr]
#wq<-var$bwh.wq[var$wateryear < pred.yr]

#This years percent of mean winter flow
#output.vol[1,1]<-round(wq.cur/mean(wq, na.rm=TRUE)*100,0)
#Division by long-term mean to generate % of average volume
#output.vol[1,3]<-round(predictions$fit[1]/mean(vol, na.rm=TRUE) *100,0) 
#swe_cols <- hist %>% dplyr::select(contains('swe'))
# lastQ<- var$cc.irr_vol[var$wateryear == pred.yr-1]
#output.cm

