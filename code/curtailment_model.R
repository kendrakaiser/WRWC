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
#check on full run
#nat_cols<-grep('nat', colnames(var))
#var<- var %>% dplyr::select(-c(all_of(nat_cols), "div", "sc.div", "abv.s", "abv.h", "bws.loss")) 

swe_cols<-grep('swe', colnames(var))
t_cols<-grep('.t.', colnames(var))
wint_t_cols<-grep('nj.t', colnames(var))
vol_cols<- grep('vol', colnames(var))
wq_cols<- grep('wq', colnames(var))

key <- unique(curtailments[c("subbasin", 'water_right_cat')])

usgs_sites = read.csv(file.path(data_dir,'usgs_sites.csv'))
stream.id<-unique(as.character(usgs_sites$abv))
pred.vols<- as.data.frame(t(output.vol[,3]))
colnames(pred.vols)<- c('bwb.vol','bws.vol', 'cc.vol', 'sc.vol') 

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

# -----------------Water Right Curtailment Models -----------------------------#
basins<-unique(curtailments$subbasin)
wr_cat<- unique(curtailments$water_right_cat)
curtNames<-expand.grid(basins, wr_cat, stringsAsFactors = FALSE)

water_right= wr_cat[1]
subws= basins[3]

# function to develop model and predict curtailment dates for each water right
mod_dev<- function(water_right, subws){
  pred.params.curt <-array(NA,c(1,5))
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
  #if predicted flows are in the model, add them here
  if (length(grep('vol', mod_sum$vars)) > 0){
    pred.var<- mod_sum$vars[grep('vol', mod_sum$vars)]
    pred.dat[pred.var]<- pred.vols[pred.var]
  }

  # Model output
  preds.curt<-predict(mod,newdata=pred.dat,se.fit=T,interval="prediction",level=0.95)
  pred.params.curt[1,1]<-round(summary(mod)$adj.r.squared,2) 
  pred.params.curt[1,2]<-round(mod_sum$loocv$Rsquared,2)
  pred.params.curt[1,3]<-round(mod_sum$loocv$RMSE)
  pred.params.curt[1,4]<-round(mean(preds.curt$fit),1) # mean of predicted
  pred.params.curt[1,5]<-round(mean(preds.curt$se.fit),1)
  
  # Make the max prediction doy 275
  if (pred.params.curt[1,4] > 275){pred.params.curt[1,4] =275}
  model$pred$pred[model$pred$pred > 275] = 275

  plt_name=paste(run_date, subws, water_right, sep= " ")
  # Plot Big Wood at Hailey modeled data for visual evaluation 
  png(filename = file.path(fig_dir_mo, fitFigName),
     width = 5.5, height = 5.5,units = "in", pointsize = 12,
     bg = "white", res = 600) 

    plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted", main=plt_name)
    abline(0,1,col="gray50",lty=1)
  dev.off()
  return(list(pred.params.curt, mod_sum$vars)) # is there something else we need here?
}

# initialize arrays to store output
wr_mod_out <-data.frame(array(NA,c(9,5)))
rownames(wr_mod_out)<- paste(curtNames[,1], curtNames[,2], sep="")
colnames(wr_mod_out)<- c("Adj R2", "LOOCV R2", "RMSE", "Curt Day", "Day +/-")
wr_vars <-vector(mode = "list", length = 9)
names(wr_vars)<- paste(curtNames[,1], curtNames[,2], sep="")

# run all water rights through model dev and prediction function
#for(i in 1:length(wr_cat)){
 # for(j in 1:length(basins)){
  #  wr_name<- paste(basins[j], wr_cat[i], sep="")
   # mod_out<- mod_dev(wr_cat[i], basins[j])
  #  wr_mod_out[wr_name,]<- mod_out[[1]]
  #  wr_vars[wr_name]<- mod_out[2]
#  }
#}

for(i in 1:dim(curtNames)[1]){
    wr_name<- paste(curtNames[i,1], curtNames[i,2], sep="")
    mod_out<- mod_dev(curtNames[i,2], curtNames[i,1])
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
colnames(julian_curt)<- c("year", "uwr_a", "lwr_a", "sc_a", "uwr_b", "lwr_b","sc_b", "uwr_c",  "lwr_c",  "sc_c")
julian_curt$year <- min(curtailments$year):max(curtailments$year)
julian_curt$uwr_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_a <- curtailments %>% subset(water_right_cat =="A") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 

julian_curt$uwr_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_b <- curtailments %>% subset(water_right_cat =="B") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 

julian_curt$uwr_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_ab_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$lwr_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'bw_bl_magic') %>% dplyr::select(shut_off_julian) 
julian_curt$sc_c <- curtailments %>% subset(water_right_cat =="C") %>% subset(subbasin == 'sc_lw') %>% dplyr::select(shut_off_julian) 


# Curtailment Correlations ------
# calculate correlations between locations
curt.cor.mat<-cor(julian_curt[-1], use="pairwise.complete")
# create covariance matrix by multiplying by each models standard error
curt.outer.prod<-as.matrix(wr_mod_out[,5])%*%t(as.matrix(wr_mod_out[,5]))
curt.cov.mat<-curt.cor.mat*curt.outer.prod

# Draw curtailment dates using multivariate normal distribution
curt.sample<-data.frame(mvrnorm(n=5000,mu=(as.matrix(wr_mod_out[,4])),Sigma=curt.cov.mat))

curt.sample[curt.sample>275] = 275
colnames(curt.sample)<-c("Big Wood abv Magic '83", "Big Wood blw Magic 83", "Silver Creek '83", "Big Wood abv Magic '84", "Big Wood blw Magic '84", "Silver Creek '84", "Big Wood abv Magic '86", "Big Wood blw Magic '86", "Silver Creek '86")
write.csv(curt.sample, file.path(model_out,"curt.sample.csv"),row.names=F)

# Plot boxplots of predicted curtailment dates from each model
curt.samp.fig<- curt.sample %>% pivot_longer(everything(),  names_to = "site", values_to = "value")
curt.samp.fig$yr<-as.factor(rep(c(1983, 1983, 1983, 1984, 1984, 1984, 1986, 1986, 1986),5000))
curt.samp.fig$loc<-rep(c(1,4,7,2,5,8,3,6,9),5000)

cs<- ggplot(curt.samp.fig,
            aes(x=reorder(site, loc), y=as.Date(value, origin=as.Date(paste(pred.yr,"-01-01",sep=""))), fill=yr)) +
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

# change from day of year to date for the table
wr_tbl<-wr_mod_out[,4:5]
wr_tbl[,1]<- as.Date(wr_tbl[,1], origin=as.Date(paste(pred.yr,"-01-01",sep="")), format='%m/%d')
rownames(wr_tbl)<- c('Big Wood Above Magic 3-24-83', 'Big Wood Below Magic 83', 'Silver Creek 83', 'Big Wood Above Magic 10-14-84', 'Big Wood Below Magic 84', 'Silver Creek 84', 'Big Wood Above Magic 6-01-86', 'Big Wood Below Magic 86', 'Silver Creek 86')

png(file.path(fig_dir_mo,"wr_preds.png"), height = 25*nrow(wr_mod_out), width = 80*ncol(wr_mod_out))
grid.table(wr_tbl)
dev.off()
