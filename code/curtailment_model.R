# ---------------------------------------------------------------------------- #
# WRWC Curtailment Date Model
# Kendra Kaiser
# December, 16 2020

cd <<- '~/Desktop/Data/WRWC'

volumes<-read.csv(file.path(model_out,"vol.sample.csv")) #ac-ft
curtailments<- read.csv(file.path(input_dir,"historic_shutoff_dates_071520.csv"))
var<-read.csv(file.path(model_out,'all_vars.csv'))

key <- unique(curtailments[c("subbasin", 'water_right_cat')])

for (i in 1:dim(key)[1]){
curt<- curtailments %>% filter(subbasin == key[i,1] & water_right_cat == key[i,2]) %>% select(year, shut_off_julian)
plot(var$bwb.vol[var$year <2020]/1000, curt$shut_off_julian[curt$year >= 1988])
}

# Import Data ------------------------------------------------------------------ # 
# Streamflow, Current SWE, historic and Modeled Temperature Data
usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
swe_q = read.csv(file.path(data_dir,input))
swe_q[swe_q == 0] <- NA # change zeros to a value so lm works
swe_q<-swe_q[!(names(swe_q) %in% c("bwb.cm.nat","bws.cm.nat","abv.h","abv.s","sc.div","bwb.vol.nat","bws.vol.nat","bws.loss","sc.vol.nat"))]

spring.temps = read.csv(file.path(data_dir, 'sprTemps.csv'))
wint.temps = read.csv(file.path(data_dir, 'wintTemps.csv')) #average temps, november - march
nj.temps = read.csv(file.path(data_dir, 'njTemps.csv'))
nf.temps = read.csv(file.path(data_dir, 'nfTemps.csv'))
fm.temps = read.csv(file.path(data_dir, 'fmTemps.csv'))

var = swe_q %>% dplyr::select(-X) %>% inner_join(spring.temps, by ="year") %>% inner_join(nj.temps, by ="year")
# sp.test<-apply(var, MARGIN=2, shapiro.test) 
# normailze parameters that have a shapiro.test() < 0.05
var$log.cg.swe <- log(var$cg.swe)
var$log.g.swe <- log(var$g.swe)
var$log.gs.swe <- log(var$gs.swe)
var$log.hc.swe <- log(var$hc.swe)
var$log.lwd.swe <- log(var$lwd.swe)
var$log.ga.swe <- log(var$ga.swe)
var$log.bc.swe <- log(var$bc.swe)
var<-var[,!(names(var) %in% c('cg.swe', 'g.swe','gs.swe','hc.swe', 'lwd.swe','ga.swe','bc.swe', 'nj.t.sr', 'aj.t.sr'))]

swe_cols<-grep('swe', colnames(var))
t_cols<-grep('.t.', colnames(var))
wint_t_cols<-grep('nj.t', colnames(var))
vol_cols<- grep('vol', colnames(var))

stream.id<-unique(as.character(usgs_sites$abv))

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

# -----------------Water Right Curtailment Models -----------------------------#
basins<-unique(curtailments$subbasin)
wr_cat<- unique(curtailments$water_right_cat)
figNames<-expand.grid(basin, wr_cat)

mod_dev<- function(water_right, subbasin){
  pred.params.curt <-array(NA,c(1,4))
  fitFigName<- paste(subbasin, water_right, ".png", sep='')
    
  curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% 
    subset(water_right) %>% subset(subbasin) %>% dplyr::select(-c(subbasin, water_right_cat))

  curt <- var %>% dplyr::select(year, bwb.vol, bwb.wq, all_of(swe_cols),all_of(wint_t_cols)) %>% 
    inner_join(curt_sub, by = 'year') %>% filter(complete.cases(.))
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(shut_off_julian~., data=curt[,-c(1)], nbest=1, nvmax=12)}, 
         error= function(e) {print(c("Curtailment model did not work", subbasin))}) #error catch
  reg_sum<- summary(regsubsets.out)
  rm(regsubsets.out)

  vars<-reg_sum$which[which.min(reg_sum$bic),]
  mod_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

  #fit the regression model and use LOOCV to evaluate performance
  form<- paste("shut_off_julian~ ", paste(mod_sum$vars, collapse=" + "), sep = "")
  mod<-lm(form, data=curt)
  mod_sum$lm<-summary(mod)$adj.r.squared
  #save summary of LOOCV
  model <- train(as.formula(form), data = curt, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results

  # Prediction Data 
  pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(mod_sum$vars)

  # Model output
  preds.curt<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.curt[1,1]<-summary(mod)$adj.r.squared 
  pred.params.curt[1,2]<-mean(preds.curt$fit) # mean of predicted
  pred.params.curt[1,3]<-mean(preds.curt$se.fit)



  #Plot Big Wood at Hailey modeled data for visual evaluation 
  png(filename = file.path(fig_dir_mo, fitFigName),
     width = 5.5, height = 5.5,units = "in", pointsize = 12,
     bg = "white", res = 600) 

    plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted")
    abline(0,1,col="gray50",lty=1)
  dev.off()
  return(list(pred.params.curt)) # is there something else we need here?
}


