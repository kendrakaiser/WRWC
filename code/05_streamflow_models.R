
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
cm_cols <- grep('cm', colnames(var))
#par(mar=c(1, 1, 1, 1))

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

vol_model<-function(site, sites, max_var, min_var=2, pred.year = pred.yr, volVars=var, usePredTypes=c("wq", "ly_vol","liquid_precip","snow_covered_area","swe_total","swe","nj_t")){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  offSiteTypes=c("swe","nj_t")
  
  # site= "bwh"
  # sites= "bwh"
  # max_var = 10
  responseName=paste0(site,".irr_vol")
  # classify predictors into groups based on naming convention (prevents duplicates from each type)
  predTypeNames=sub(".*\\.","",names(volVars)) %in% usePredTypes
  siteNames=grepl(paste(sites, collapse="|"), colnames(volVars))
  onSiteNames=names(volVars)[predTypeNames & siteNames]
  #variables that are not specific to the watershed (SWE & temp)
  offSiteTypes=offSiteTypes[offSiteTypes %in% usePredTypes]
  offSiteNames=names(volVars)[sub(".*\\.","",names(volVars))  %in% offSiteTypes]   
  # select historic values
  volVars=volVars[volVars$wateryear < pred.year,]
  #recombine all possible terms
  modelDF= cbind(volVars[,responseName],
                 volVars[,c(onSiteNames, offSiteNames)])
  
  names(modelDF)[1]=responseName
  modelDF=modelDF[complete.cases(modelDF),]
  
  # fastest comprehensive search (still way too slow)
  # with leaps / BIC to identify best model
  getLeapFormulas=function(leapList){
    leapList$label=leapList$label[-1] #drop '(Intercept)' from terms list
    forms=character()
    for(i in 1:nrow(leapList$which)){
      varNames=leapList$label[leapList$which[i,]]
      varNames=varNames[order(varNames)] #order the variables so simplify avoidance of fitting replicate models
      forms = c(forms,paste(names(modelDF)[1]," ~ ", paste(varNames,collapse=" + ")))
    }
    return(forms)
  }
  
  
  ############################# log response ################################
  unloggedResponse=modelDF[,1]
  
  modelDF[,1]=log(modelDF[,1])
  names(modelDF)[1]=paste0("log.",names(modelDF)[1])
  
  #best combn of one of each `type` of variable (conveniently defined by suffix)
  predTypes=as.factor(sub(".*\\.","",names(modelDF)[-1]))
  
  predTypeList=list()
  for(i in 1:length(levels(predTypes))){
    predTypeList[[i]]=names(modelDF)[-1][predTypes==levels(predTypes)[i]]
    
  }
  names(predTypeList)=levels(predTypes)
  predTypes = expand.grid(predTypeList[])
  rm(predTypeList)
  
  typeModels=paste(names(modelDF)[1],"~",apply(predTypes, 1, paste, collapse=' + '))
  
  modelCompareDF=data.frame(formula=character(),
                            n=numeric(),
                            r2=numeric(),
                            BIC=numeric()
                            #max_vif=numeric()
  )
  
  for(globalFormula in typeModels){
    global_lm=lm(globalFormula,data=modelDF,na.action=na.fail)
    
    #leaps + fit method (faster)
    thisVars=names(coefficients(global_lm))[-1]
    leapList=leaps(x=modelDF[,thisVars],y=modelDF[,1],method="r2",nbest=1,names=thisVars)
    forms=getLeapFormulas(leapList)
    for(f in forms){
      if(!f %in% modelCompareDF$formula){ #this sub model has not been tested, proceed...
        
        thisModel=lm(f,data=modelDF)
        
        addDF=data.frame(formula=f,
                         n=length(thisModel$coefficients)-1, #just count vars, not intercept
                         r2=summary(thisModel)$r.squared,
                         BIC=BIC(thisModel)
                         #max_vif=getMaxVif(thisModel)
        )
        modelCompareDF=rbind(modelCompareDF,addDF)
      }
    }
  } 
  

  modelCompareDF=modelCompareDF[modelCompareDF$n <= max_var,]
  modelCompareDF=modelCompareDF[modelCompareDF$n >= min_var,]
  modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  #print(head(modelCompareDF))
  bestByType=lm(modelCompareDF$formula[1],data=modelDF,na.action=na.fail)
  #summary(bestByType)
  #check_model(bestByType)
  #vif(bestByType)
  #dredge(bestByType,rank="BIC",extra="R^2") #check that model selected is better than subsets as per BIC
  #plot(exp(bestByType$fitted.values)~exp(bestByType$model$log.bwh.irr_vol))
  #summary(lm(exp(bestByType$fitted.values)~exp(bestByType$model$log.bwh.irr_vol))) # r2 of fitted model in real (unlogged) space
  
  
  mod_sum=bestByType
  mod_sum$form <- paste(responseName," ~ ", paste(names(bestByType$coefficients)[-1],collapse=" + "))
  mod_sum$vars<-names(bestByType$coefficients)[-1]
  
  #Plot (LOOCV) modeled data for visual evaluation 
  # fig_name = paste0(site, ".vol_modelFit.png")
  # png(filename = file.path(fig_dir_mo, fig_name),
  #     width = 5.5, height = 5.5,units = "in", pointsize = 12,
  #     bg = "white", res = 600) 
  # 
  # plot(model$pred$obs/1000, model$pred$pred/1000, pch=19, 
  #      xlab="Observed Irrigation Season KAF", ylab="Predicted Irrigation Season KAF")
  # abline(0,1,col="gray50",lty=1)
  # dev.off()
  
  return(mod_sum)
}

# Create Volume Models for each USGS gage
bwh_vol_mod<- vol_model("bwh", "bwh", 10)
bws_vol_mod<- vol_model("bws", c("bws", "bwh"), 10)
cc_vol_mod<- vol_model("cc", c("bwh", "cc\\."), 10)
sc_vol_mod<- vol_model("sc", c("bwh", "sc"), 10)


# EXPORT VOL MODEL DETAILS
# ----------------------
vol_models<- list(bwh_mod = bwh_vol_mod, bws_mod = bws_vol_mod, sc_mod = sc_vol_mod, cc_mod = cc_vol_mod)


# ---------------------------------------------------------------------------- # 
# Evaluate alternative model combinations for Center of Mass Predictions
# ---------------------------------------------------------------------------- # 
cm_model<-function(site, sites, max_var){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  site_vars<- grep(paste(sites, collapse="|"), colnames(var))
  #remove variables from predictor set that don't have data for this year
  pred.avail<- var [var$wateryear == pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars), all_of(swe_cols), 
                                                                 all_of(wint_t_cols), -all_of(c(tot_vol_cols, runoff_cols, irr_vol_cols, cm_cols)))# %>% filter(complete.cases(.))
  na.vars<- names(pred.avail)[is.na(pred.avail[1,])] 
  
  hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
                                                          all_of(swe_cols), all_of(wint_t_cols), all_of(aj_t_cols), 
                                                          -all_of(c(tot_vol_cols, runoff_cols, irr_vol_cols))) %>% filter(complete.cases(.))
  hist<- hist[,!names(hist) %in% na.vars] #remove variables from the dataset that do not have data for this year
  
  
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
  fitDF=cbind(data.frame(cm=hist[[vol_col]],hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE]))
  allModelSummary=getRegModelSummary(reg_sum,f_fitDF = fitDF,response="cm")
  
  form<-gsub("cm", name, allModelSummary$form[which.min(allModelSummary$aicc)])
  
  # pulling variable names out so they can be subset in 06
  mod_sum<- as.list(allModelSummary[which.min(allModelSummary$aicc),])
  mod_sum$form <-form
  vrs<- unlist(strsplit(form, "\\s*[~]\\s*"))[[2]]
  mod_sum$vars<-unlist(strsplit(vrs, "\\s*\\+\\s*"))
  
  # run model
  # mod<-lm(form, data=hist)
  
  # # which set of variables have the lowest BIC
  # vars<-reg_sum$which[which.min(reg_sum$bic),]
  # #vars<-reg_sum$which[which.max(reg_sum$adjr2),]
  # mod_sum<- list(vars = names(vars)[vars==TRUE][-1], adjr2 = reg_sum$adjr2[which.min(reg_sum$bic)], bic=reg_sum$bic[which.min(reg_sum$bic)])
  # 
  #fit the regression model and use LOOCV to evaluate performance
  # form<- paste(paste(name, "~ "), paste(mod_sum$vars, collapse=" + "), sep = "")
  # 
  mod<-lm(form, data=hist)
  mod_sum$adjr2<-summary(mod)$adj.r.squared
  
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
bwh_cm_mod<- cm_model("bwh", "bwh", 6)
bws_cm_mod<- cm_model("bws", "bws", 6)
sc_cm_mod<- cm_model("sc", c("bwh","sc"),6)
cc_cm_mod<- cm_model("cc", "cc\\.", 6)

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


