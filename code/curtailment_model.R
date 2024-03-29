# ---------------------------------------------------------------------------- #
# WRWC Curtailment Date Models
# Kendra Kaiser
# December, 16 2020
# Linear regression models of specific curtailment dates in the Big Wood Basin
# ---------------------------------------------------------------------------- #

# Import Data -----------------------------------------------------------------# 
volumes<-read.csv(file.path(model_out,"vol.sample.csv")) #ac-ft
curtailments<- read.csv(file.path(input_dir,"historic_shutoff_dates_042022.csv"))
curtailments$shut_off_date<- as.Date(curtailments$shut_off_date, format="%m/%d/%y")
curtailments$shut_off_julian <- yday(curtailments$shut_off_date)
var<-read.csv(file.path(model_out,'all_vars.csv'))
var$bw.div <- var$abv.h + var$abv.s

#check on full run
#nat_cols<-grep('nat', colnames(var))
#var<- var %>% dplyr::select(-c(all_of(nat_cols), "div", "sc.div", "abv.s", "abv.h", "bws.loss")) 
curtailments$wr_name<- paste(curtailments$subbasin, curtailments$water_right_cat, sep='')

swe_cols<-grep('swe', colnames(var))
t_cols<-grep('.t.', colnames(var))
wint_t_cols<-grep('nj.t', colnames(var))
vol_cols<- grep('vol', colnames(var))
wq_cols<- grep('wq', colnames(var))

key <- unique(curtailments[c("subbasin", 'water_right_cat')])
div.names<- c("abv.h", "abv.s", "sc.div", "bw.div")
div_cols<- match(div.names, colnames(var))

usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
pred.vols<- as.data.frame(t(output.vol[,2]))*1000
colnames(pred.vols)<- c('bwb.vol','bws.vol', 'cc.vol', 'sc.vol') 

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

# Look at a few dates
water_right= key[6,2]
subws= key[6,1]

curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% 
  subset(water_right_cat == water_right) %>% subset(subbasin == subws) %>% dplyr::select(-c(subbasin, water_right_cat, wr_name))

curt <- var %>% dplyr::select(year, all_of(vol_cols),all_of(wq_cols), all_of(swe_cols),all_of(wint_t_cols), all_of(div_cols)) %>% 
  inner_join(curt_sub, by = 'year') %>% filter(complete.cases(.))



# -----------------Diversion Volume Models

mod_div<- function(div.name){
  pred.params.div <-array(NA,c(1,5))
  hist <- var %>% dplyr::select(c(div.name), year, all_of(vol_cols),all_of(wq_cols), all_of(swe_cols),all_of(wint_t_cols)) %>% filter(complete.cases(.))
 
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(hist[,c(div.name)]~., data=hist[,-c(1,2)], nbest=1, nvmax=8)}, 
           error= function(e) {print(c("Diversion model did not work", div.name))}) #error catch
  reg_sum<- summary(regsubsets.out)
  rm(regsubsets.out)
  
  vars<-reg_sum$which[which.min(reg_sum$bic),]
  mod_sum<-list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
  
  #fit the regression model and use LOOCV to evaluate performance
  form<- paste(div.name,"~", paste(mod_sum$vars, collapse=" + "), sep = "")
  mod<-lm(form, data=hist)
  mod_sum$lm<-summary(mod)$adj.r.squared
  #save summary of LOOCV
  model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results
  
  # Prediction Data 
  pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(mod_sum$vars)
  #if predicted flows are in the model, add them here
  if (length(grep('vol', mod_sum$vars)) > 0){
    pred.var<- mod_sum$vars[grep('vol', mod_sum$vars)]
    pred.dat[pred.var]<- pred.vols[pred.var]
  }
  
  # Model output
  preds.div<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.div[1,1]<-round(summary(mod)$adj.r.squared,2) 
  pred.params.div[1,2]<-round(mod_sum$loocv$Rsquared,2)
  pred.params.div[1,3]<-round(mod_sum$loocv$RMSE)
  pred.params.div[1,4]<-round(preds.div$fit,1) 
  pred.params.div[1,5]<-round(preds.div$se.fit,1)
  
  plt_name=paste(run_date, div.name, sep= " ")
  fitFigName<- paste(div.name, ".png", sep='')
  
  # Plot Big Wood at Hailey modeled data for visual evaluation 
  png(filename = file.path(fig_dir_mo, fitFigName),
      width = 5.5, height = 5.5,units = "in", pointsize = 12,
      bg = "white", res = 600) 
  
  plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted", main=plt_name)
  abline(0,1,col="gray50",lty=1)
  dev.off()
  return(list(pred.params.div, mod_sum$vars)) # is there something else we need here?
}
# initialize arrays to store output
div_mod_out <-data.frame(array(NA,c(length(div.names),5)))
rownames(div_mod_out)<- div.names
colnames(div_mod_out)<- c("Adj R2", "LOOCV R2", "RMSE", "Diversion", "+/-")
div_vars <-vector(mode = "list", length = length(div.names))

#for (i in 1:length(div.names)) {
 # mod_out<- mod_div(div.names[i])
  #div_mod_out[div.names[i],]<- mod_out[[1]]
  #div_vars[i]<- mod_out[2]
#}

# -----------------Water Right Curtailment Models -----------------------------#
basins<-unique(curtailments$subbasin)
wr_cat<- unique(curtailments$water_right_cat)
curtNames<-expand.grid(basins, wr_cat, stringsAsFactors = FALSE)



# function to develop model and predict curtailment dates for each water right
mod_dev<- function(water_right, subws){
  pred.params.curt <-array(NA,c(1,6))
  fitFigName<- paste(subws, water_right, ".png", sep='')
    
  curt_sub<- curtailments %>% dplyr::select(-c(water_right_date,shut_off_date)) %>% 
    subset(water_right_cat == water_right) %>% subset(subbasin == subws) %>% dplyr::select(-c(subbasin, water_right_cat, wr_name))

  curt <- var %>% dplyr::select(year, all_of(vol_cols),all_of(wq_cols), all_of(swe_cols),all_of(wint_t_cols)) %>% 
    inner_join(curt_sub, by = 'year') %>% filter(complete.cases(.))
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(shut_off_julian~., data=curt[,-c(1)], nbest=1, nvmax=4)}, 
         error= function(e) {print(c("Curtailment model did not work", subws))}) #error catch
  reg_sum<- summary(regsubsets.out)
  rm(regsubsets.out)

  vars<-reg_sum$which[which.min(reg_sum$bic),]
  mod_sum<-list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

  #fit the regression model and use LOOCV to evaluate performance
  form<- paste("shut_off_julian~ ", paste(mod_sum$vars, collapse=" + "), sep = "")
  mod<-lm(form, data=curt)
  mod_sum$lm<-summary(mod)$adj.r.squared
  #save summary of LOOCV
  model <- train(as.formula(form), data = curt, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results

  # Prediction Data 
  pred.dat<-var[var$year == pred.yr,] %>% dplyr::select(mod_sum$vars)
  div.vars<- match(div.names, mod_sum$vars)
  #if predicted flows are in the model, add them here
  if (length(grep('vol', mod_sum$vars)) > 0){
    pred.var<- mod_sum$vars[grep('vol', mod_sum$vars)]
    pred.dat[pred.var]<- pred.vols[pred.var]
  } 
  if (length(div.vars[!is.na(div.vars)])>0){
    pred.div<- mod_sum$vars[div.vars[!is.na(div.vars)]]
    pred.dat[pred.div]<- div_mod_out[pred.div, "Diversion"]
  }

  # Model output
  preds.curt<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.curt[1,1]<-round(summary(mod)$adj.r.squared,2) 
  pred.params.curt[1,2]<-round(mod_sum$loocv$Rsquared,2)
  pred.params.curt[1,3]<-round(mod_sum$loocv$RMSE)
  pred.params.curt[1,4]<-round(preds.curt$fit[1]) 
  pred.params.curt[1,5]<-round(preds.curt$se.fit,1)
  pred.params.curt[1,6]<-round(preds.curt$fit[2]) 
  
  # Make the max prediction doy 275
  if (pred.params.curt[1,4] > 275){pred.params.curt[1,4] =275}
  model$pred$pred[model$pred$pred > 275] = 275

  plt_name=paste(run_date, subws, water_right, sep= " ")
  # Plot  modeled data for visual evaluation 
  png(filename = file.path(fig_dir_mo, fitFigName),
     width = 5.5, height = 5.5,units = "in", pointsize = 12,
     bg = "white", res = 600) 

    plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted", main=plt_name)
    abline(0,1,col="gray50",lty=1)
  dev.off()
  return(list(pred.params.curt, mod_sum$vars)) # is there something else we need here?
}

# initialize arrays to store output
wr_mod_out <-data.frame(array(NA,c(9,6)))
rownames(wr_mod_out)<- paste(key[,1], key[,2], sep="")
colnames(wr_mod_out)<- c("Adj R2", "LOOCV R2", "RMSE", "Curt Day", "Day +/-", "Lower Curt")
wr_vars <-vector(mode = "list", length = 9)
names(wr_vars)<- paste(key[,1], key[,2], sep="")

# run all water rights through model dev and prediction function
for(i in 1:dim(key)[1]){
  wr_name<- paste(key[i,1], key[i,2], sep="")
  mod_out<- mod_dev(key[i,2], key[i,1])
  wr_mod_out[wr_name,]<- mod_out[[1]]
  wr_vars[wr_name]<- mod_out[2]
}

# Silver Creek A only has two values 9/31 and 10/1 - the model isn't that useful

# Save model fit output  
png(file.path(fig_dir_mo,"r2s_wr.png"), height = 25*nrow(wr_mod_out), width = 80*ncol(wr_mod_out))
grid.table(wr_mod_out[,1:3])
dev.off()

# Save model parameters
list.save(wr_vars, file.path(fig_dir_mo, wr_params))

# Curtailment Summary ------
# couldnt get pivot wider to work ...
# tst<-pivot_wider(curtailments, names_from = c(subbasin, water_right_cat), values_from = shut_off_julian)
julian_curt<- data.frame(matrix(ncol = 10, nrow = length(min(curtailments$year):max(curtailments$year))))
colnames(julian_curt)<- c("year", "bw_ab_magicA", "bw_ab_magicB", "bw_ab_magicC", "bw_bl_magicA", "bw_bl_magicB", "bw_bl_magicC", "sc_lwA",  "sc_lwB", "sc_lwC")
julian_curt$year <- min(curtailments$year):max(curtailments$year)

julian_curt$bw_ab_magicA <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$bw_ab_magicB <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$bw_ab_magicC <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 

julian_curt$bw_bl_magicA <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$bw_bl_magicB <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$bw_bl_magicC <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 


julian_curt$sc_lwA<- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_lwB <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_lwC <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 


# Curtailment Correlations ------
# calculate correlations between locations
curt.cor.mat<-cor(julian_curt[-1], use="pairwise.complete")
# make the water rights model output same order
wr_out<- wr_mod_out[match(rownames(curt.cor.mat), rownames(wr_mod_out)),]

# create covariance matrix by multiplying by each models standard error
curt.outer.prod<-as.matrix(wr_out[,5])%*%t(as.matrix(wr_out[,5]))
curt.cov.mat<-curt.cor.mat*curt.outer.prod
# Draw curtailment dates using multivariate normal distribution
curt.sample<-data.frame(mvrnorm(n=5000,mu=(as.matrix(wr_out[,4])),Sigma=curt.cov.mat))
#TODO export in long format for DB
curt.sample[curt.sample>275] = 275
colnames(curt.sample)<-c("Big Wood abv Magic '83", "Big Wood abv Magic '84", "Big Wood abv Magic '86", "Big Wood blw Magic '83", "Big Wood blw Magic '84", "Big Wood blw Magic '86", "Silver Creek '83",  "Silver Creek '84", "Silver Creek '86")
write.csv(curt.sample, file.path(model_out,"curt.sample.csv"),row.names=F)

# Plot boxplots of predicted curtailment dates from each model
curt.samp.fig<- curt.sample %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
curt.samp.fig$yr<-as.factor(rep(c(1983, 1984, 1986),15000))
#curt.samp.fig$loc<-rep(c(1,4,7,2,5,8,3,6,9),5000)

cs<- ggplot(curt.samp.fig,
            aes(x=site, y=as.Date(value, origin=as.Date(paste(pred.yr,"-01-01",sep=""))), fill=yr)) +
  theme_bw()+
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_y_date(date_breaks = "1 week", date_labels = "%b %d")+
  scale_x_discrete(labels = wrap_format(10)) +
  theme(legend.position="none") +
  ggtitle("Sampled Curtailment Dates") +
  xlab("")+
  ylab("Curtailment Date")

png(filename = file.path(fig_dir_mo,"sampled_curtailments.png"),
    width = 6.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
    print(cs)
dev.off()

curt_hist<- ggplot(curtailments, 
                   aes(x=wr_name, y=as.Date(shut_off_julian, origin=as.Date(paste(pred.yr,"-01-01",sep=""))), fill=water_right_cat))+
  theme_bw()+
  geom_boxplot()+
  geom_jitter(alpha=0.6)+
  scale_fill_viridis(discrete = TRUE, alpha=0.4) +
  scale_y_date(date_breaks = "1 week", date_labels = "%b %d")+
  scale_x_discrete(labels = wrap_format(10))  +
  ggtitle("Historic Curtailment Dates") +
  theme(legend.position="none") +
  xlab("")+
  ylab("Curtailment Date")
png(filename = file.path(fig_dir_mo,"hist_curtailments.png"),
    width = 6.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
  print(curt_hist)
dev.off()
# change from day of year to date for the table
wr_tbl<-wr_out[,4:5]
wr_tbl[,1]<- as.Date(wr_tbl[,1], origin=as.Date(paste(pred.yr,"-01-01",sep="")), format='%m/%d')

rownames(wr_tbl)<- c('Big Wood Above Magic 3-24-83', 'Big Wood Above Magic 10-14-84', 'Big Wood Above Magic 6-01-86', 'Big Wood Below Magic 83',  'Big Wood Below Magic 84', 'Big Wood Below Magic 86', 'Silver Creek 83', 'Silver Creek 84', 'Silver Creek 86')

png(file.path(fig_dir_mo,"wr_preds.png"), height = 25*nrow(wr_mod_out), width = 80*ncol(wr_mod_out))
grid.table(wr_tbl)
dev.off()
