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

swe_cols<-grep('swe', colnames(var))
t_cols<-grep('.t.', colnames(var))
wint_t_cols<-grep('nj.t', colnames(var))
aj_t_cols<-grep('aj.t', colnames(var))
vol_cols<- grep('vol', colnames(var))
snodas_cols<- c(grep('wint', colnames(var)), grep('runoff', colnames(var)), grep('snow', colnames(var)), grep('swe_total', colnames(var)))

#par(mar=c(1, 1, 1, 1))
#pairs(c(var[swe_cols[1:8]], var[snodas_cols[1:10]]))

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")
nv_max=9

#------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for April-Sept Volume Predictions
#------------------------------------------------------------------------------ # 

# Big Wood at Hailey
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bwb.vol, bwb.wq, 
              all_of(swe_cols), all_of(wint_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.))

#use regsubsets to assess the results
tryCatch({regsubsets.out<-regsubsets(log(hist$bwb.vol)~., data=hist[,-c(1)], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Big Wood Hailey Vol model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
#vars<-reg_sum$which[which.max(reg_sum$adjr2),]
bwh_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit the regression model and use LOOCV to evaluate performance
form<- paste("log(bwb.vol)~ ", paste(bwh_sum$vars, collapse=" + "), sep = "")
#pairs(var[bwh_sum$vars])
bwh_mod<-lm(form, data=hist)
bwh_sum$lm<-summary(bwh_mod)$adj.r.squared

#put coefficients into DF to save across runs
bwh_coef<- signif(bwh_mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))

#save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bwh_sum$loocv<- model$results
#bwh_sum

#linear model of logged prediction v.s. observed for a more accurate r2
pred<-exp(model$pred$pred)/1000
obs<- exp(model$pred$obs)/1000
bwh_r2<- lm(pred ~obs)
bwh_sum$true.r2<-summary(bwh_r2)$adj.r.squared

#check residuals
mod.red<- resid(model)
#hist(mod.red)
#shapiro.test(mod.red)

#Plot Big Wood at Hailey modeled data for visual evaluation 
png(filename = file.path(fig_dir_mo, "BWH_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Hailey \nApril-Sept Streamflow Vol (1000 ac-ft)")
abline(0,1,col="gray50",lty=1)
dev.off()

# calculate the correlations
r <- round(cor(hist[bwh_sum$vars], use="complete.obs"),2)
#ggcorrplot(r)

# -------------------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bws.vol, bws.wq, 
                  all_of(swe_cols), all_of(wint_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.))

#use regsubsets to explore models
tryCatch({regsubsets.out<-regsubsets(log(hist$bws.vol)~., data=hist[,-c(1)], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Big Wood Stanton Vol model did not work")}) #error catch
reg_sum<- summary(regsubsets.out) #summary of regsubsets to pull info from
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),] #T/F of variables
#vars3<-coef(regsubsets.out, 5)
bws_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2=reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(bws.vol)~ ", paste(bws_sum$vars, collapse=" + "), sep = "")
bws_mod<-lm(form, data=hist)
bws_sum$lm<-summary(bws_mod)$adj.r.squared

#put coefficients into DF to save across runs
bws_coef<- signif(bws_mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))

#save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bws_sum$loocv<- model$results
#bws_sum

#linear model of logged prediction v.s. observed for a more accurate r2
pred<-exp(model$pred$pred)/1000
obs<- exp(model$pred$obs)/1000
bws_r2<- lm(pred ~obs)
bws_sum$true.r2<-summary(bws_r2)$adj.r.squared

#check residuals
mod.red<- resid(model)
#hist(mod.red)
#shapiro.test(mod.red)

#Save Model fit figure
png(filename = file.path(fig_dir_mo, "BWS_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood at Stanton Crossing \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Silver Creek
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, sc.vol, sc.wq, bwb.wq, 
             all_of(swe_cols), all_of(wint_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.)) 

# Silver Creek regsubsets 
tryCatch({regsubsets.out<-regsubsets(log(hist$sc.vol)~., data=hist[,-1], nbest=3, nvmax=nv_max)}, 
         error= function(e) {print("Silver Creek Vol model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
sc_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2=reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(sc.vol)~ ", paste(sc_sum$vars, collapse=" + "), sep = "")
sc_mod<-lm(form, data=hist)
sc_sum$lm<-summary(sc_mod)$adj.r.squared

#put coefficients into DF to save across runs
sc_coef<- signif(sc_mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))

#Save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
sc_sum$loocv<- model$results
#sc_sum

#linear model of logged prediction v.s. observed for a more accurate r2
pred<-exp(model$pred$pred)/1000
obs<- exp(model$pred$obs)/1000
sc_r2<- lm(pred ~obs)
sc_sum$true.r2<-summary(sc_r2)$adj.r.squared

#check residuals
mod.red<- resid(model)
#hist(mod.red)
#shapiro.test(mod.red)

#Save Model fit figure
png(filename = file.path(fig_dir_mo, "SC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Camas creek
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, cc.vol, cc.wq, bwb.wq,
            all_of(swe_cols), all_of(wint_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.)) 

#select parameters
tryCatch({regsubsets.out<-regsubsets(log(hist$cc.vol)~., data=hist[,-1], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Camas Creek Vol model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
cc_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("log(cc.vol)~ ", paste(cc_sum$vars, collapse=" + "), sep = "")
cc_mod<-lm(form, data=hist)
cc_sum$lm<-summary(cc_mod)$adj.r.squared

cc_coef<- signif(cc_mod$coefficients, 2) %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))

#save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
cc_sum$loocv<- model$results

#linear model of logged prediction v.s. observed for a more accurate r2
pred<-exp(model$pred$pred)/1000
obs<- exp(model$pred$obs)/1000
cc_r2<- lm(pred ~obs)
cc_sum$true.r2<-summary(cc_r2)$adj.r.squared

#check residuals
mod.red<- resid(model)
#hist(mod.red)
#shapiro.test(mod.red)

#Save figure of model results
png(filename = file.path(fig_dir_mo, "CC_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
    plot(exp(model$pred$obs)/1000, exp(model$pred$pred)/1000, pch=19, xlab="Observed", ylab="Predicted",main="Camas Creek \nApril-Sept Streamflow Vol (1000 ac-ft)")
    abline(0,1,col="gray50",lty=1)
dev.off()


# EXPORT VOL MODEL DETAILS
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
# ----------------------
# use regsubsets to plot the results
#regsubets.res<-cbind(regsubsets.out$size,regsubsets.out$adjr2, regsubsets.out$bic)
#quartz(title="Adjusted R^2",10,10)
#plot(regsubsets.out, scale = "adjr2", main="Adjusted R^2 For the best model of a given size")
#quartz(title="BIC",10,10)
#plot(regsubsets.out, scale = "bic", main="BIC For the best model of a given size")
#quartz(title="R2 v BIC",10,10)
#plot(reg_sum$bic, reg_sum$adjr2, xlab="BIC", ylab="adj R2")

# ------------------------------------------------------------------------------ # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ------------------------------------------------------------------------------ # 

# Big Wood at Hailey
# TODO: data isn't going through 2022 -- there is no runoff total for 140, 167, 144??
# this doesn't like to run efficiently 
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bwb.cm, bwb.wq, 
                  all_of(swe_cols),  all_of(snodas_cols), all_of(aj_t_cols)) %>% filter(complete.cases(.)) 

tryCatch({regsubsets.out<-regsubsets(hist$bwb.cm~., data=hist[,-1], nbest=1, nvmax=nv_max, really.big=TRUE)}, 
         error= function(e) {print("Big Wood Hailey CM model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
bwh.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("bwb.cm~ ", paste(bwh.cm_sum$vars, collapse=" + "), sep = "")
bwh_cm.mod<-lm(form, data=hist) 
bwh.cm_sum$lm<-summary(bwh_cm.mod)$adj.r.squared
#Save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bwh.cm_sum$loocv<- model$results
#bwh.cm_sum

#Save model results
png(filename = file.path(fig_dir_mo, "bwh.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Hailey Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Big Wood at Stanton
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, bws.cm, bws.wq,
                  all_of(swe_cols), all_of(aj_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.)) 

#select Parameters
tryCatch({regsubsets.out<-regsubsets(hist$bws.cm~., data=hist[,-1], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Big Wood Stanton CM model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
bws.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("bws.cm~ ", paste(bws.cm_sum$vars, collapse=" + "), sep = "")
bws_cm.mod<-lm(form, data=hist) 
bws.cm_sum$lm<-summary(bws_cm.mod)$adj.r.squared

#Save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
bws.cm_sum$loocv<- model$results

# Save figure of model results
png(filename = file.path(fig_dir_mo, "bws.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Big Wood Stanton Center of Mass")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Silver Creek Center of Mass
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, sc.cm, sc.wq, bwb.wq, bws.wq, 
         all_of(swe_cols),all_of(aj_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.)) 

# Select and Save Parameters
tryCatch({regsubsets.out<- regsubsets(hist$sc.cm~., data=hist[,-1], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Silver Creek CM model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
vars<-reg_sum$which[which.min(reg_sum$bic),]
sc.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("sc.cm~ ", paste(sc.cm_sum$vars, collapse=" + "), sep = "")
sc_cm.mod<- lm(form, data=hist)
sc.cm_sum$lm<-summary(sc_cm.mod)$adj.r.squared
#Save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
sc.cm_sum$loocv<- model$results
#sc.cm_sum

# Save figure of model results
png(filename = file.path(fig_dir_mo, "sc.cm_modelFit.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
plot(model$pred$obs, model$pred$pred, pch=19, xlab="Observed", ylab="Predicted",main="Silver Creek Center of Mass (doy)")
abline(0,1,col="gray50",lty=1)
dev.off()

# -------------------------------------------------------------
# Camas Creek Center of Mass
hist <- var[var$year < pred.yr,] %>% dplyr::select(year, cc.cm, cc.wq, all_of(swe_cols), 
                                 all_of(aj_t_cols), all_of(snodas_cols)) %>% filter(complete.cases(.)) 

# Select and Save model parameters
tryCatch({regsubsets.out<-regsubsets(hist$cc.cm~., data=hist[,-1], nbest=1, nvmax=nv_max)}, 
         error= function(e) {print("Camas Creek CM model did not work")}) #error catch
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

vars<-reg_sum$which[which.min(reg_sum$bic),]
cc.cm_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2= reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])

#fit a regression model and use LOOCV to evaluate performance
form<- paste("cc.cm~ ", paste(cc.cm_sum$vars, collapse=" + "), sep = "")
cc_cm.mod<-lm(form, data=hist) 
cc.cm_sum$lm<-summary(cc_cm.mod)$adj.r.squared

#Save summary of LOOCV
model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
cc.cm_sum$loocv<- model$results
#cc.cm_sum
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
#bwb.m<- model %>% augment()

#ggplot(bwb.m, aes(x = "hc.swe", y = "log(bwb.vol)")) +
 #   geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  #  geom_segment(aes(xend = hc.swe, yend = .fitted), alpha = .2) +  # Note `.fitted`
   # geom_point(aes(alpha = abs(.std.resid))) +  # Note `.resid`
    #guides(alpha = FALSE) +
    #geom_point(aes(y = .fitted), shape = 1) +  # Note `.fitted`
    #theme_bw()



