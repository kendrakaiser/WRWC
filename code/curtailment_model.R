# ---------------------------------------------------------------------------- #
# WRWC Curtailment Date Models
# Kendra Kaiser
# December, 16 2020
# Linear regression models of specific curtailment dates in the Big Wood Basin
# ---------------------------------------------------------------------------- #

# Import Data -----------------------------------------------------------------# 
volumes<-read.csv(file.path(model_out,"vol.sample.csv")) #ac-ft
curtailments<- read.csv(file.path(input_dir,"historic_shutoff_dates_071520.csv"))
var<-read.csv(file.path(model_out,'all_vars.csv')) %>% dplyr::select(-X) 
nat_cols<-grep('nat', colnames(var))
var<- var %>% dplyr::select(-c(all_of(nat_cols), "div", "sc.div", "abv.s", "abv.h", "bws.loss")) 

swe_cols<-grep('swe', colnames(var))
t_cols<-grep('.t.', colnames(var))
wint_t_cols<-grep('nj.t', colnames(var))
vol_cols<- grep('vol', colnames(var))
wq_cols<- grep('wq', colnames(var))

key <- unique(curtailments[c("subbasin", 'water_right_cat')])

usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
stream.id<-unique(as.character(usgs_sites$abv))

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

# -----------------Water Right Curtailment Models -----------------------------#
basins<-unique(curtailments$subbasin)
wr_cat<- unique(curtailments$water_right_cat)
curtNames<-expand.grid(basins, wr_cat)

water_right= wr_cat[i]
subws= basins[j]

# function to develop model and predict curtailment dates for each water right
mod_dev<- function(water_right, subws){
  pred.params.curt <-array(NA,c(1,4))
  fitFigName<- paste(subws, water_right, ".png", sep='')
    
  curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% 
    subset(water_right_cat == water_right) %>% subset(subbasin == subws) %>% dplyr::select(-c(subbasin, water_right_cat))

  curt <- var %>% dplyr::select(year, all_of(vol_cols),all_of(wq_cols), all_of(swe_cols),all_of(wint_t_cols)) %>% 
    inner_join(curt_sub, by = 'year') %>% filter(complete.cases(.))
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(shut_off_julian~., data=curt[,-c(1)], nbest=1, nvmax=8)}, 
         error= function(e) {print(c("Curtailment model did not work", subws))}) #error catch
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
  pred.params.curt[1,1]<-round(summary(mod)$adj.r.squared,2) 
  pred.params.curt[1,2]<-round(mod_sum$loocv$Rsquared,2)
  pred.params.curt[1,3]<-round(mean(preds.curt$fit),1) # mean of predicted
  pred.params.curt[1,4]<-round(mean(preds.curt$se.fit),1)

  #make the max prediction doy 275
  if (pred.params.curt[1,2] > 275){pred.params.curt[1,2] =275}
  model$pred$pred[model$pred$pred > 275] = 275

  #Plot Big Wood at Hailey modeled data for visual evaluation 
  png(filename = file.path(fig_dir_mo, fitFigName),
     width = 5.5, height = 5.5,units = "in", pointsize = 12,
     bg = "white", res = 600) 

    plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted")
    abline(0,1,col="gray50",lty=1)
  dev.off()
  return(list(pred.params.curt)) # is there something else we need here?
}

# pre-deinfe arrays to store output
wr_mod_out <-array(NA,c(9,4))
rownames(wr_mod_out)<- paste(curtNames[,1], curtNames[,2], sep="")
colnames(wr_mod_out)<- c("Adj R2", "LOOCV R2", "Curt.Doy", "Error")

# run all water rights through model dev and prediction function
for(i in 1:length(wr_cat)){
  for(j in 1:length(basins)){
    wr_name<- paste(basins[j], wr_cat[i], sep="")
    wr_mod_out[wr_name,]<- mod_dev(wr_cat[i], basins[j])[[1]]
  }
}

# save output
png(file.path(fig_dir_mo,"r2s_wr.png"), height = 25*nrow(wr_mod_out), width = 80*ncol(wr_mod_out))
grid.table(wr_mod_out)
dev.off()

