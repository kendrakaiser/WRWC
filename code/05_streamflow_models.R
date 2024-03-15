# ----------------------------------------------------------------------------- #
# Predictive Streamflow Model Selection for the Wood River Water Collaborative
# Kendra Kaiser
# October 1, 2020
#
# Multiple linear regression models to predict total irrigation season streamflow 
# volume (Acre-Feet) and center of mass based on calculated baseflow, current SWE, 
# winter and predicted spring temperature 
# ----------------------------------------------------------------------------- # 

defaultW <- getOption("warn") 
options(warn = -1) 

#------------------------------------------------------------------------------ # 
# Import & Compile Data                                                        -# 
#------------------------------------------------------------------------------ # 

# ID columns for subsetting
swe_cols<-grep("(?!swe_)_?swe", colnames(var), perl = TRUE, value = TRUE)
wint_t_cols<-grep('nj_t', colnames(var))
aj_t_cols<-grep('aj_t', colnames(var))
irr_vol_cols<- grep('irr_vol', colnames(var))
tot_vol_cols<- grep('tot_vol', colnames(var))
snodas_cols<- c(grep('wint', colnames(var)), grep('snow', colnames(var)), grep('swe_total', colnames(var)))
wq_cols<- grep('wq', colnames(var))
runoff_cols<- grep('runoff', colnames(var))
#par(mar=c(1, 1, 1, 1))
#pairs(c(var[swe_cols[1:8]], var[snodas_cols[1:10]]))

#------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for April-Sept Volume Predictions
#------------------------------------------------------------------------------ # 
#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#function to make an easier to read df with some info on the models
getRegModelSummary=function(regSum=reg_sum,f_fitDF=fitDF, response="iv" ){
  outDF=data.frame(form=character(),r2=numeric(),bic=numeric(),aicc=numeric())
  for(i in 1:nrow(regSum$which)){
    thisRegSumWhich=regSum$which[i,]
    addDF=data.frame(form=deparse1(reformulate(names(thisRegSumWhich)[thisRegSumWhich][-1], response=response)),
                     r2=regSum$rsq[i],
                     bic=regSum$bic[i],
                     aicc=AICc(lm(reformulate(names(thisRegSumWhich)[thisRegSumWhich][-1], response=response), data=f_fitDF ))
    )
    outDF=rbind(outDF,addDF)
  }
  return(outDF)
}

vol_model<-function(site, sites, max_var){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  site_vars<- grep(paste(sites, collapse="|"), colnames(var))
  hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
              all_of(swe_cols), all_of(wint_t_cols), -all_of(c(tot_vol_cols, runoff_cols))) %>% filter(complete.cases(.))
  name<- paste0(site, ".irr_vol")
  #id column names that should be removed from the modeling set
  irr_vols<- colnames(hist)[grep('irr_vol', colnames(hist))]
  vol_col<-grep(name, colnames(hist))
  cms<- colnames(hist)[grep('cm', colnames(hist))]
  
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(log(hist[[vol_col]])~., 
            data=hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE], 
            nbest=1, nvmax=max_var, really.big=T)}, 
            error= function(e) {print(paste(site,"volume model did not work"))})
  reg_sum<- summary(regsubsets.out)
  rm(regsubsets.out)
  
  fitDF_linear=cbind(data.frame(iv=hist[[vol_col]],hist[, !names(hist) %in% c("wateryear", irr_vols, cms)]))
  
  allModels_linear=getRegModelSummary(regSum=reg_sum,f_fitDF=fitDF_linear, response="iv" )
  
  allModels_linear$cv_meanAbsError_kaf=0
  allModels_linear$cv_worstError_kaf=0
  for(m in 1:nrow(allModels_linear)){
    #k=nrow(fitDF_linear) # loocv
    k=10  #k-fold cv
    thisModel=lm(allModels_linear$form[m],fitDF_linear)
    errors_kaf=numeric()
    for(i in 1:1000){
      holdout=sample(1:nrow(fitDF_linear),nrow(fitDF_linear)/k)
      lko_lm=lm(reformulate(names(thisModel$coefficients)[-1],response="iv"),fitDF_linear[-holdout,])
      lko_iv=predict(lko_lm, newdata=fitDF_linear[holdout,])
      errors_kaf=c(errors_kaf,fitDF_linear[holdout,"iv"]/1000-lko_iv/1000)
    }
    #hist(errors_kaf)
    allModels_linear$cv_meanAbsError_kaf[m]=mean(abs(errors_kaf))
    allModels_linear$cv_worstError_kaf[m]=max(abs(errors_kaf))
  }
  
  # which formula has the lowest AICC
  form<-gsub("iv", name, allModels_linear$form[which.min(allModels_linear$aicc)])

  mod_sum<- as.list(allModels_linear[which.min(allModels_linear$aicc),])
  mod_sum$form <-form
  vrs<- unlist(strsplit(form, "\\s*[~]\\s*"))[[2]]
  mod_sum$vars<-unlist(strsplit(vrs, "\\s*\\+\\s*"))
  mod<-lm(form, data=hist)
  
  #put coefficients into DF to save across runs
  coef<- signif(mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))
  
  #save summary of LOOCV
  model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results
  #check residuals mod.red<- resid(model)
  
  # calculate the correlations
  #r <- round(cor(hist[bwh_sum$vars], use="complete.obs"),2)
  #ggcorrplot(r)
  
  #Plot modeled data for visual evaluation 
  fig_name = paste0(site, ".vol_modelFit.png")
  png(filename = file.path(fig_dir_mo, fig_name),
      width = 5.5, height = 5.5,units = "in", pointsize = 12,
      bg = "white", res = 600) 
  
  plot(model$pred$obs/1000, model$pred$pred/1000, pch=19, 
       xlab="Observed Irrigation Season KAF", ylab="Predicted Irrigation Season KAF")
  abline(0,1,col="gray50",lty=1)
  dev.off()
  
  return(list(mod_sum, mod, coef))
}

# Create Volume Models for each USGS gage
bwh_vol_mod<- vol_model("bwh", "bwh", 10)
bws_vol_mod<- vol_model("bws", c("bws"), 10)
cc_vol_mod<- vol_model("cc", c("bwh", "cc\\."), 10)
sc_vol_mod<- vol_model("sc", c("bwh", "sc"), 10)


# EXPORT VOL MODEL DETAILS
# ----------------------
vol_mod_sum<- list(bwh = bwh_vol_mod[[1]], bws = bws_vol_mod[[1]], sc = sc_vol_mod[[1]], cc = cc_vol_mod[[1]])
vol_models<- list(bwh_mod = bwh_vol_mod[[2]], bws_mod = bws_vol_mod[[2]], sc_mod = sc_vol_mod[[2]], cc_mod = cc_vol_mod[[2]])
#vol_coef<- cbind(bwh_vol_mod[[3]], bws_vol_mod[[3]], sc_vol_mod[[3]], cc_vol_mod[[3]])

#write.csv(vol_coef, file.path(model_out,'vol_coeff.csv'), row.names = FALSE)
#write.list(vol_mod_sum, file.path(data_dir, vol.summary)) #.csv

#list.save(vol_mod_sum, file.path(data_dir, vol_sum)) #.Rdata summary stats
#list.save(vol_models, file.path(data_dir, vol_mods)) #actual model structure

# Pull out R2 for summary stats
r2s<- data.frame(matrix(ncol = 3, nrow = 4))
colnames(r2s)<-c("AdjR2", "Loocv R2", "MAE")
rownames(r2s)<-c("BWH", "BWS", "SC", "CC")
r2s[,1]<- round(c(bwh_vol_mod[[1]]$true.r2, bws_vol_mod[[1]]$true.r2, sc_vol_mod[[1]]$true.r2,cc_vol_mod[[1]]$true.r2)*100, 2)
r2s[,2]<- round(c(bwh_vol_mod[[1]]$loocv$Rsquared, bws_vol_mod[[1]]$loocv$Rsquared,sc_vol_mod[[1]]$loocv$Rsquared,cc_vol_mod[[1]]$loocv$Rsquared)*100, 2)
r2s[,3]<- round(c(bwh_vol_mod[[1]]$loocv$MAE, bws_vol_mod[[1]]$loocv$MAE, sc_vol_mod[[1]]$loocv$MAE, cc_vol_mod[[1]]$loocv$MAE), 2)

png(file.path("r2s.png"), height = 25*nrow(r2s), width = 80*ncol(r2s))
grid.table(r2s)
dev.off()


# ---------------------------------------------------------------------------- # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ---------------------------------------------------------------------------- # 
cm_model<-function(site, sites, max_var){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  site_vars<- grep(paste(sites, collapse="|"), colnames(var))
  hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
                all_of(swe_cols), all_of(aj_t_cols), -all_of(c(tot_vol_cols, runoff_cols, irr_vol_cols))) %>% filter(complete.cases(.))
  name<- paste0(site, ".cm")
  #id column names that should be removed from the modeling set
  vol_col<-grep(name, colnames(hist))
  irr_vols<- colnames(hist)[grep('irr_vol', colnames(hist))]
  cms<- colnames(hist)[grep('cm', colnames(hist))]
  
  #use regsubsets to assess the results
  tryCatch({regsubsets.out<-regsubsets(hist[[vol_col]]~., 
            data=hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE], 
            nbest=1, nvmax=max_var, really.big=T)}, 
           error= function(e) {print(paste(site,"center of mass model did not work"))}) #error catch
  reg_sum<- summary(regsubsets.out)
  rm(regsubsets.out)
  # which set of variables have the lowest BIC
  vars<-reg_sum$which[which.min(reg_sum$bic),]
  #vars<-reg_sum$which[which.max(reg_sum$adjr2),]
  mod_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
  
  #fit the regression model and use LOOCV to evaluate performance
  form<- paste(paste(name, "~ "), paste(mod_sum$vars, collapse=" + "), sep = "")
  
  mod<-lm(form, data=hist)
  mod_sum$lm<-summary(mod)$adj.r.squared
  
  #put coefficients into DF to save across runs
  coef<- signif(mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))
  
  #save summary of LOOCV
  model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results
  
  #Save model result figures
  fig_name = paste0(site, ".cm_modelFit.png")
  png(filename = file.path(fig_dir_mo, fig_name),
      width = 5.5, height = 5.5,units = "in", pointsize = 12,
      bg = "white", res = 600) 
  plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed CM", ylab="Predicted CM")
  abline(0,1,col="gray50",lty=1)
  dev.off()
  
  print(paste(name, "model complete"))
  
  return(list(mod_sum, model, coef))
}

# Create Center of Mass Models for each site
bwh_cm_mod<- cm_model("bwh", "bwh", 9)
bws_cm_mod<- cm_model("bws", "bws", 9)
sc_cm_mod<- cm_model("sc", c("bwh","sc"), 9)
cc_cm_mod<- cm_model("cc", "cc", 9)

# ---------------------------------------------------------------------------- # 
### EXPORT Center of Mass MODEL DETAILS
# ----------------------------------------------------------------------------
#compile all model details into one list to export
cm_mod_sum<- list(bwh = bwh_cm_mod[[1]], bws = bws_cm_mod[[1]], sc = sc_cm_mod[[1]], cc = cc_cm_mod[[1]])
cm_models<- list(bwh_cm.mod = bwh_cm_mod[[2]], bws_cm.mod = bws_cm_mod[[2]], sc_cm.mod = sc_cm_mod[[2]], cc_cm.mod = cc_cm_mod[[2]])

#write.list(cm_mod_sum, file.path(data_dir, cm.summary))

#list.save(cm_mod_sum, file.path(data_dir, cm_sum))
#list.save(cm_models, file.path(data_dir, cm_mods))

r2s_cm<- data.frame(matrix(ncol = 2, nrow = 4))
colnames(r2s_cm)<-c("AdjR2", "Loocv R2")
rownames(r2s_cm)<-c("BWH", "BWS", "SC", "CC")

r2s_cm[,1]<- round(c(bwh_cm_mod[[1]]$adjr2, bws_cm_mod[[1]]$adjr2, sc_cm_mod[[1]]$adjr2, cc_cm_mod[[1]]$adjr2)*100, 2)
r2s_cm[,2]<- round(c(bwh_cm_mod[[1]]$loocv$Rsquared, bws_cm_mod[[1]]$loocv$Rsquared, sc_cm_mod[[1]]$loocv$Rsquared, cc_cm_mod[[1]]$loocv$Rsquared)*100, 2)

png(file.path(fig_dir_mo,"r2s_cm.png"), height = 25*nrow(r2s_cm), width = 80*ncol(r2s_cm))
grid.table(r2s_cm)
dev.off()


