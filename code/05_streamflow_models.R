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
var<- read.csv(file.path(model_out,'all_vars.csv'))

swe_cols<-grep("(?!swe_)_?swe", colnames(var), perl = TRUE, value = TRUE)

wint_t_cols<-grep('nj_t', colnames(var))
aj_t_cols<-grep('aj_t', colnames(var))
irr_vol_cols<- grep('irr_vol', colnames(var))
tot_vol_cols<- grep('tot_vol', colnames(var))
snodas_cols<- c(grep('wint', colnames(var)), grep('runoff', colnames(var)), grep('snow', colnames(var)), grep('swe_total', colnames(var)))

#par(mar=c(1, 1, 1, 1))
#pairs(c(var[swe_cols[1:8]], var[snodas_cols[1:10]]))

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")


#------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for April-Sept Volume Predictions
#------------------------------------------------------------------------------ # 
vol_model<-function(site, sites, max_var){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  site_vars<- grep(paste(sites, collapse="|"), colnames(var))
  hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
              all_of(swe_cols), all_of(wint_t_cols), -all_of(tot_vol_cols)) %>% filter(complete.cases(.))
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
  # which set of variables have the lowest BIC
  vars<-reg_sum$which[which.min(reg_sum$bic),]
  #vars<-reg_sum$which[which.max(reg_sum$adjr2),]
  mod_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
 
   #fit the regression model and use LOOCV to evaluate performance
  form<- paste(paste("log(", name, ")~ "), paste(mod_sum$vars, collapse=" + "), sep = "")
  
  mod<-lm(form, data=hist)
  mod_sum$lm<-summary(mod)$adj.r.squared
  
  #put coefficients into DF to save across runs
  coef<- signif(mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))
  
  #save summary of LOOCV
  model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
  mod_sum$loocv<- model$results
  #check residuals mod.red<- resid(model)
  
  #linear model of logged prediction v.s. observed for a more accurate r2
  pred<-exp(model$pred$pred)/1000
  obs<- exp(model$pred$obs)/1000
  r2<- lm(pred ~obs)
  mod_sum$true.r2<-summary(r2)$adj.r.squared
  
  # calculate the correlations
  #r <- round(cor(hist[bwh_sum$vars], use="complete.obs"),2)
  #ggcorrplot(r)
  
  return(list(mod_sum, model, coef))
}

# Create Volume Models for each USGS gage
bwh_output<- vol_model("bwh", "bwh", 9)
bws_output<- vol_model("bws", c("bwh", "bws"), 9)
cc_output<- vol_model("cc", c("bwh", "cc"), 9)
sc_output<- vol_model("sc", c("bwh", "sc"), 9)

#TODO: update to make these from model function output

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir_mo, "BWH_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

#Save Big Wood at Stanton
png(filename = file.path(fig_dir_mo, "BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Stanton Crossing \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

#Save Model fit Silver Creek
png(filename = file.path(fig_dir_mo, "SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

#Save figure of model results
png(filename = file.path(fig_dir_mo, "CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()


# EXPORT VOL MODEL DETAILS
#TODO: update all these structures using output from new model function
# ----------------------
# compile all model details into one list to export
mod_sum<- list(bwh = bwh_sum, bws = bws_sum, sc = sc_sum, cc = cc_sum)
vol_models<- list(bwh_mod = bwh_mod, bws_mod = bws_mod, sc_mod = sc_mod, cc_mod = cc_mod)
mod_coef<- cbind(bwh_coef, bws_coef, sc_coef, cc_coef)

write.csv(mod_coef, file.path(model_out,'mod_coeff.csv'), row.names = FALSE)
write.list(mod_sum, file.path(data_dir, vol.vars))

list.save(mod_sum, file.path(data_dir, vol_params))
list.save(vol_models, file.path(data_dir, vol_mods))

r2s<- data.frame(matrix(ncol = 3, nrow = 4))
colnames(r2s)<-c("AdjR2", "Loocv R2", "MAE")
rownames(r2s)<-c("BWH", "BWS", "SC", "CC")
r2s[,1]<- round(c(mod_sum$bwh$true.r2, mod_sum$bws$true.r2,mod_sum$sc$true.r2,mod_sum$cc$true.r2)*100, 2)
r2s[,2]<- round(c(mod_sum$bwh$loocv$Rsquared, mod_sum$bws$loocv$Rsquared,mod_sum$sc$loocv$Rsquared,mod_sum$cc$loocv$Rsquared)*100, 2)
r2s[,3]<- round(c(exp(mod_sum$bwh$loocv$MAE), exp(mod_sum$bws$loocv$MAE), exp(mod_sum$sc$loocv$MAE), exp(mod_sum$cc$loocv$MAE)), 2)

png(file.path(fig_dir_mo,"r2s.png"), height = 25*nrow(r2s), width = 80*ncol(r2s))
grid.table(r2s)
dev.off()

# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 
cm_model<-function(site, sites, max_var){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  site_vars<- grep(paste(sites, collapse="|"), colnames(var))
  hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
                all_of(swe_cols), all_of(aj_t_cols), -all_of(irr_vol_cols), -all_of(tot_vol_cols)) %>% filter(complete.cases(.))
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
  
  return(list(mod_sum, model, coef))
}
# Big Wood at Hailey
bwh_cm_out<- cm_model("bwh", "bwh", 9)
bwh_cm_out<- cm_model("bws", "bws", 9)
#bwh.wq, bws.wq, 
sc_cm_out<- cm_model("sc", c("bwh","sc"), 9)

#Save model results
png(filename = file.path(fig_dir_mo, "bwh.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Hailey Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Big Wood at Stanton
# Save figure of model results
png(filename = file.path(fig_dir_mo, "bws.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Stanton Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Silver Creek Center of Mass
# Save figure of model results
png(filename = file.path(fig_dir_mo, "sc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek Center of Mass (doy)")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Camas Creek Center of Mass
# Save figure of model results 
png(filename = file.path(fig_dir_mo, "cc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek Center of Mass (doy)")
abline(0,1,col="gray50",lty=1)
dev.off()


### EXPORT Center of Mass MODEL DETAILS
# -----------------------------------------------------------------------------
#compile all model details into one list to export
mod_cm.sum<- list(bwh = bwh.cm_sum, bws = bws.cm_sum, sc = sc.cm_sum, cc = cc.cm_sum)
cm_models<- list(bwh_cm.mod = bwh_cm.mod, bws_cm.mod = bws_cm.mod, sc_cm.mod = sc_cm.mod, cc_cm.mod = cc_cm.mod)

write.list(mod_cm.sum, file.path(data_dir, cm.vars))

list.save(mod_cm.sum, file.path(data_dir, cm_params))
list.save(cm_models, file.path(data_dir, cm_mods))

r2s_cm<- data.frame(matrix(ncol = 2, nrow = 4))
colnames(r2s_cm)<-c("AdjR2", "Loocv R2")
rownames(r2s_cm)<-c("BWH", "BWS", "SC", "CC")
r2s_cm[,1]<- round(c(mod_cm.sum$bwh$adjr2, mod_cm.sum$bws$adjr2,mod_cm.sum$sc$adjr2,mod_cm.sum$cc$adjr2)*100, 2)
r2s_cm[,2]<- round(c(mod_cm.sum$bwh$loocv$Rsquared, mod_cm.sum$bws$loocv$Rsquared,mod_cm.sum$sc$loocv$Rsquared,mod_cm.sum$cc$loocv$Rsquared)*100, 2)

png(file.path(fig_dir_mo,"r2s_cm.png"), height = 25*nrow(r2s_cm), width = 80*ncol(r2s_cm))
grid.table(r2s_cm)
dev.off()

options(warn = defaultW)

# -------------------------------------------------------------
# Evaluation of residuals 
#https://drsimonj.svbtle.com/visualising-residuals
#library(broom)
#bwh.m<- model %>% augment()

#ggplot(bwh.m, aes(x = "hc.swe", y = "log(bwh.vol)")) +
 #   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  #  geom_segment(aes(xend = hc.swe, yend = .fitted), alpha = .2) +  # Note `.fitted`
   # geom_point(aes(alpha = abs(.std.resid))) +  # Note `.resid`
    #guides(alpha = FALSE) +
    #geom_point(aes(y = .fitted), shape = 1) +  # Note `.fitted`
    #theme_bw()



