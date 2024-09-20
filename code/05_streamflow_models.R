
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

vol_model<-function(site, sites, max_var, pred.year = pred.yr, volVars=var, usePredTypes=c("wq", "ly_vol","liquid_precip","snow_covered_area","swe_total","swe","nj_t")){
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  offSiteTypes=c("swe","nj_t")
  
  # site= "bwh"
  # sites= "bwh"
  # max_var = 10

  responseName=paste0(site,".irr_vol")
  
  predTypeNames=sub(".*\\.","",names(volVars)) %in% usePredTypes
  siteNames=grepl(paste(sites, collapse="|"), colnames(volVars))
  onSiteNames=names(volVars)[predTypeNames & siteNames]
  
  offSiteTypes=offSiteTypes[offSiteTypes %in% usePredTypes]
  offSiteNames=names(volVars)[sub(".*\\.","",names(volVars))  %in% offSiteTypes]   
  
  volVars=volVars[volVars$wateryear < pred.year,]
  
  modelDF= cbind(volVars[,responseName],
                 volVars[,c(onSiteNames, offSiteNames)]
  )
  names(modelDF)[1]=responseName
  
  modelDF=modelDF[complete.cases(modelDF),]
  
  # 
  # getMaxVif=function(thisModel){
  #   if(length(thisModel$coefficients)>2){
  #     return(max(vif(thisModel)))
  #   } else {
  #     return(1)
  #   }
  # }
  
  # allModels=combn(names(modelDF)[-1],max_var,FUN=paste,collapse=" + ")
  # am=allModels
  # 
  # allModels=am[1:100]
  # 
  # modelCompareDF=data.frame(formula=character(length=length(allModels)),
  #                           n=numeric(length=length(allModels)),
  #                           r2=numeric(length=length(allModels)),
  #                           BIC=numeric(length=length(allModels)),
  #                           max_vif=numeric(length=length(allModels))
  # )
  # 
  # 
  # for(i in 1:length(allModels)){
  #   print(i)
  #   try({
  #     bestVars_vif=vif_select(df=modelDF,response = name,preference_order = sample(names(modelDF[,-1]) ),max_vif=5)
  #     mod_vif=lm( formula=paste(name,"~",paste(bestVars_vif,collapse="+")), data=modelDF, na.action=na.fail)
  #     #summary(mod_vif)
  #     mod_best=get.models(dredge(mod_vif, rank="BIC",extra="R^2"),subset=1)[[1]]
  #     modelCompareDF$formula[i]=paste(name,"~", paste(names(mod_best$coefficients[-1]),collapse=" + "))
  #     modelCompareDF$n=length(mod_best$coefficients)
  #     modelCompareDF$r2=summary(mod_best)$r.squared
  #     modelCompareDF$BIC=BIC(mod_best)
  #     modelCompareDF$max_vif=getMaxVif(mod_best)
  #     
  #   })
  # }
  # 
  # modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  # head(modelCompareDF)
  
  
  # system.time({
  #   modelSearchIter=100
  #   
  #   modelCompareDF=data.frame(formula=rep("",modelSearchIter),
  #                             n=rep(0,modelSearchIter),
  #                             r2=rep(0,modelSearchIter),
  #                             BIC=rep(0,modelSearchIter),
  #                             max_vif=rep(0,modelSearchIter)
  #   )
  #   
  #   
  #   for(i in 1:modelSearchIter){
  #     print(i)
  #     try({
  #       bestVars_vif=vif_select(df=modelDF,response = name,preference_order = sample(names(modelDF[,-1]) ),max_vif=5)
  #       mod_vif=lm( formula=paste(name,"~",paste(bestVars_vif,collapse="+")), data=modelDF, na.action=na.fail)
  #       #summary(mod_vif)
  #       mod_best=get.models(dredge(mod_vif, rank="BIC",extra="R^2"),subset=1)[[1]]
  #       modelCompareDF$formula[i]=paste(name,"~", paste(names(mod_best$coefficients[-1]),collapse=" + "))
  #       modelCompareDF$n[i]=length(mod_best$coefficients)
  #       modelCompareDF$r2[i]=summary(mod_best)$r.squared
  #       modelCompareDF$BIC[i]=BIC(mod_best)
  #       modelCompareDF$max_vif[i]=getMaxVif(mod_best)
  #       
  #     })
  #   }
  # })
  # modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  # head(modelCompareDF)
  # #best model from a sample run:
  # # formula                                                                   n        r2      BIC     max_vif
  # # 156 bwh.irr_vol ~ bwh.liquid_precip + bwh.wq + cg.nj_t + g.nj_t + sr.swe  6    0.7399056 495.9586 3.794114
  # 
  
  
  
  #fastest comprehensive search (still way too slow)
  #with leaps / BIC to identify best model
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
  
  # modelSearchIter=20000
  # modelCompareDF=data.frame(formula=character(),
  #                           n=numeric(),
  #                           r2=numeric(),
  #                           BIC=numeric(),
  #                           max_vif=numeric(),
  #                           iter=numeric()
  # )
  # 
  # testedVarCombns=character()
  # 
  # for(i in 1:modelSearchIter){
  #   
  #   try({
  #     bestVars_vif=vif_select(df=modelDF,response = name,preference_order = sample(names(modelDF[,-1]) ),max_vif=4)
  #     bestVars_vif_label=paste(bestVars_vif[order(bestVars_vif)],collapse="+")#order doesn't matter after this, putting in alphabetical to simplify duplicate avoidance
  #     
  #     if(!bestVars_vif_label %in% testedVarCombns){ #this set of terms has not been tested, proceed...
  #       testedVarCombns=c(testedVarCombns,bestVars_vif_label)
  #       
  #       leapList=leaps(x=modelDF[,bestVars_vif],y==modelDF[,1],method="r2",nbest=1,names=bestVars_vif)
  #       forms=getLeapFormulas(leapList)
  #       for(f in forms){
  #         if(!f %in% modelCompareDF$formula){ #this sub model has not been tested, proceed...
  #           
  #           thisModel=lm(f,data=modelDF)
  #           
  #           addDF=data.frame(formula=f,
  #                            n=length(thisModel$coefficients),
  #                            r2=summary(thisModel)$r.squared,
  #                            BIC=BIC(thisModel),
  #                            max_vif=getMaxVif(thisModel),
  #                            iter=i
  #           )
  #           if(addDF$BIC < min(modelCompareDF$BIC) ){
  #             print(paste0("iter:",i,", Model:",addDF$f," (BIC:",round(addDF$BIC,2),", R^2:",round(addDF$r2,2),")"))
  #             modelCompareDF=rbind(modelCompareDF,
  #                                  addDF
  #             )
  #           }
  #         }
  #       } 
  #     } 
  #     
  #   })
  # }
  # 
  # 
  # 
  # modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  # head(modelCompareDF)
  # bestByType=lm(modelCompareDF$formula[1],data=modelDF)
  # summary(bestByType)
  # check_model(bestByType)
  # vif(bestByType)
  #best models from a sample run of 100000:
  # formula                                                                                                   n        r2      BIC  max_vif
  # 124648                            bwh.irr_vol  ~  sr.swe + g.nj_t + bwh.liquid_precip + bwh.wq + cg.nj_t 6 0.7399056 495.9586 3.794114
  # 174691                            bwh.irr_vol  ~  cg.nj_t + bwh.wq + sr.swe + bwh.liquid_precip + g.nj_t 6 0.7399056 495.9586 3.794114
  # 135201          bwh.irr_vol  ~  sr.swe + g.swe + g.nj_t + ds.nj_t + cg.nj_t + bwh.wq + bwh.liquid_precip 8 0.8082814 496.0522 4.349935
  # 144318  bwh.irr_vol  ~  bwh.swe_total + bwh.liquid_precip + sr.swe + cg.nj_t + bwh.wq + ds.nj_t + g.nj_t 8 0.8042109 496.4513 4.415109
  # 174632         bwh.irr_vol  ~  cg.nj_t + gs.swe + sr.swe + ds.nj_t + g.nj_t + bwh.liquid_precip + bwh.wq 8 0.8012352 496.7379 4.449210
  # 1108418        bwh.irr_vol  ~  ds.nj_t + g.nj_t + gs.swe + sr.swe + cg.nj_t + bwh.liquid_precip + bwh.wq 8 0.8012352 496.7379 4.449210
  
  
  ############################# log response ################################
  unloggedResponse=modelDF[,1]
  
  modelDF[,1]=log(modelDF[,1])
  names(modelDF)[1]=paste0("log.",names(modelDF)[1])
  
  #new idea, - best combn of one of each `type` of variable (conveniently defined by suffix)
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
    
    #dredge method (slow)
    # thisModel=get.models(dredge(global_lm,rank="BIC"),subset=1)[[1]]
    # thisVars=names(coefficients((thisModel)))[-1]
    # thisVars=thisVars[order(thisVars)]
    # thisForm=paste(name,"~",paste(thisVars,collapse="+"))
    # if(!thisForm %in% modelCompareDF$formula){
    #   addDF=data.frame(formula=thisForm,
    #                    n=length(thisModel$coefficients),
    #                    r2=summary(thisModel)$r.squared,
    #                    BIC=BIC(thisModel),
    #                    max_vif=getMaxVif(thisModel)
    #   )
    #   modelCompareDF=rbind(modelCompareDF,addDF)
    # }
    
    
    #leaps + fit method (faster)
    thisVars=names(coefficients(global_lm))[-1]
    leapList=leaps(x=modelDF[,thisVars],y=modelDF[,1],method="r2",nbest=1,names=thisVars)
    forms=getLeapFormulas(leapList)
    for(f in forms){
      if(!f %in% modelCompareDF$formula){ #this sub model has not been tested, proceed...
        
        thisModel=lm(f,data=modelDF)
        
        addDF=data.frame(formula=f,
                         n=length(thisModel$coefficients),
                         r2=summary(thisModel)$r.squared,
                         BIC=BIC(thisModel)
                         #max_vif=getMaxVif(thisModel)
        )
        modelCompareDF=rbind(modelCompareDF,addDF)
      }
    }
  } 
  
  modelCompareDF=modelCompareDF[modelCompareDF$n <= max_var,]
  modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  #head(modelCompareDF)
  bestByType=lm(modelCompareDF$formula[1],data=modelDF,na.action=na.fail)
  #summary(bestByType)
  #check_model(bestByType)
  #vif(bestByType)
  #dredge(bestByType,rank="BIC",extra="R^2") #check that model selected is better than subsets as per BIC
  
  #plot(exp(bestByType$fitted.values)~exp(bestByType$model$log.bwh.irr_vol))
  
  #summary(lm(exp(bestByType$fitted.values)~exp(bestByType$model$log.bwh.irr_vol))) # r2 of fitted model in real (unlogged) space
  
  # 
  # getMI=function(varName,responseVar=hist[,name],dataset=hist){
  #   mi=cmi.pw(responseVar,dataset[,varName])$mi
  #   return(mi)
  # }
  # 
  # #as per Cheng et al 2022:
  # VIF_threshold=5
  # varSelectDF=data.frame(varName=names(hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE]),mi=0)
  # 
  # selectedTerms=character()
  # varSelectDF$mi=sapply(varSelectDF$varName,FUN=getMI)
  # varSelectDF=varSelectDF[order(varSelectDF$mi,decreasing=T),]
  # 
  # for(i in 1:nrow(varSelectDF)){
  #   
  #   varSelectLM=lm(as.formula( paste(name,"~",paste(paste(selectedTerms,collapse="+"),varSelectDF$varName[i],sep="+")) ),data=hist)
  #   #print(summary(varSelectLM))
  #   if(length(coefficients(varSelectLM))>2){
  #     thisVif=max(vif(varSelectLM))
  #     #print(vif(varSelectLM))
  #   } else { thisVif = 1 }
  #   
  #   if(thisVif<VIF_threshold){
  #     selectedTerms=c(selectedTerms,varSelectDF$varName[i])
  #   }
  #   
  # }
  # varSelectLM=lm(as.formula( paste(name,"~",paste(selectedTerms,collapse="+")) ),data=hist, na.action=na.fail)
  # summary(varSelectLM)
  # vif(varSelectLM)
  # BIC(varSelectLM)
  # check_model(varSelectLM)
  # dredge(varSelectLM,extra="R^2",rank="BIC")[1,]
  # summary(get.models(dredge(varSelectLM,extra="R^2",rank="BIC"),subset=1)[[1]])
  # 
  # 
  # 
  # #modified from cheng et al, MI with residuals after each selection
  # varSelectDF=data.frame(varName=names(hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE]),mi=0)
  # selectedTerms=character()
  # varSelectDF$mi=sapply(varSelectDF$varName,FUN=getMI)
  # varSelectDF=varSelectDF[order(varSelectDF$mi,decreasing=T),]
  # 
  # while(nrow(varSelectDF)>=1){
  #   #fit new model adding predictor (based on MI w/ residuals)
  #   varSelectLM=lm(as.formula( paste(name,"~",paste(paste(selectedTerms,collapse="+"),varSelectDF$varName[1],sep="+")) ),data=hist)
  #   #print(summary(varSelectLM))
  #   
  #   if(length(coefficients(varSelectLM))>2){
  #     thisVif=max(vif(varSelectLM))
  #     #print(vif(varSelectLM))
  #   } else { thisVif = 1 }
  #   
  #   #if VIF meets standard, add term to selectedTerms (and keep previously built lm)
  #   if(thisVif<VIF_threshold){
  #     selectedTerms=c(selectedTerms,varSelectDF$varName[1])
  #   } else { #else create lm w/out this term
  #     varSelectLM=lm(as.formula( paste(name,"~",paste(selectedTerms,collapse="+")) ),data=hist)
  #   }
  #   #this term has been tested, remove from select df
  #   varSelectDF=varSelectDF[-1,]
  #   
  #   #recalculate MI on residuals and sort
  #   varSelectDF$mi=sapply(varSelectDF$varName,FUN=getMI,responseVar=varSelectLM$residuals)
  #   varSelectDF=varSelectDF[order(varSelectDF$mi,decreasing=T),]
  #   
  # }
  # 
  # varSelectLM=lm(as.formula( paste(name,"~",paste(selectedTerms,collapse="+")) ),data=hist,na.action = na.fail)
  # summary(varSelectLM)
  # vif(varSelectLM)
  # BIC(varSelectLM)
  # check_model(varSelectLM)
  # 
  # dredge(varSelectLM,extra="R^2",rank="BIC")[1,]
  # 
  # summary(get.models(dredge(varSelectLM,extra="R^2",rank="BIC"),subset=1)[[1]])
  # 
  # 
  
  
  ###---- old ###################
  # fitDF_linear=cbind(data.frame(iv=hist[[vol_col]],hist[, !names(hist) %in% c("wateryear", irr_vols, cms)]))
  # 
  # allModelDF=data.frame(n=numeric(),form=character(),r2=numeric(),max_vif=numeric())
  # 
  # #use regsubsets to assess the results  ##can this be replaced with leaps()? looks like a simpler interface to the same thing
  # tryCatch({regsubsets.out<-regsubsets(hist[[vol_col]]~., 
  #                                      data=hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE], 
  #                                      nbest=1, nvmax=max_var, really.big=T
  #                                      #,method="forward"
  # )}, 
  # error= function(e) {print(paste(site,"volume model did not work"))})
  # reg_sum<- summary(regsubsets.out)
  # rm(regsubsets.out)
  # 
  # fitDF_linear=cbind(data.frame(iv=hist[[vol_col]],hist[, !names(hist) %in% c("wateryear", irr_vols, cms)]))
  # 
  # allModels_linear=getRegModelSummary(regSum=reg_sum,f_fitDF=fitDF_linear, response="iv" )
  # 
  # bestModel_aicc=lm(allModels_linear$form[which.min(allModels_linear$aicc)],fitDF_linear)
  # summary(bestModel_aicc)
  # 
  # bestVars_vif=vif_select(df=hist[, !names(hist) %in% c("wateryear", cms)],response="bwh.irr_vol",preference_order = names(bestModel_aicc$coefficients[-1]),max_vif = 5)
  # mod_vif=lm(hist[[vol_col]]~.,data=hist[,names(hist) %in% c(vol_col,bestVars_vif)])
  # summary(mod_vif)
  # vif(mod_vif)
  # check_model(mod_vif)
  # 
  # allModels_linear$cv_meanAbsError_kaf=0
  # allModels_linear$cv_worstError_kaf=0
  # allModels_linear$maxVif=0
  # for(m in 1:nrow(allModels_linear)){
  #   #k=nrow(fitDF_linear) # loocv
  #   k=5  #k-fold cv
  #   thisModel=lm(allModels_linear$form[m],fitDF_linear)
  #   if(m>2){
  #     maxVif=max(vif(thisModel))
  #   } else {
  #     maxVif = 1
  #   }
  #   allModels_linear$maxVif[m]=maxVif
  #   errors_kaf=numeric()
  #   for(i in 1:1000){
  #     holdout=sample(1:nrow(fitDF_linear),round(nrow(fitDF_linear)/k))
  #     lko_lm=lm(reformulate(names(thisModel$coefficients)[-1],response="iv"),fitDF_linear[-holdout,])
  #     lko_iv=predict(lko_lm, newdata=fitDF_linear[holdout,])
  #     errors_kaf=c(errors_kaf,fitDF_linear[holdout,"iv"]/1000-lko_iv/1000)
  #   }
  #   #hist(errors_kaf)
  #   allModels_linear$cv_meanAbsError_kaf[m]=mean(abs(errors_kaf))
  #   allModels_linear$cv_worstError_kaf[m]=max(abs(errors_kaf))
  # }
  # allModels_linear
  
  # which formula has the lowest AICC
  #form<-gsub("iv", name, allModels_linear$form[which.min(allModels_linear$aicc)])
  # pulling variable names out so they can be subset in 06
  
  # mod_sum<- as.list(allModels_linear[which.min(allModels_linear$aicc),])
  # mod_sum$form <-form
  # vrs<- unlist(strsplit(form, "\\s*[~]\\s*"))[[2]]
  # mod_sum$vars<-unlist(strsplit(vrs, "\\s*\\+\\s*"))
  # 
  
  mod_sum=bestByType
  mod_sum$form <- paste(responseName," ~ ", paste(names(bestByType$coefficients)[-1],collapse=" + "))
  mod_sum$vars<-names(bestByType$coefficients)[-1]
  
  # # run model
  # mod<-lm(form, data=hist)
  # mod_sum$adjr2<-summary(mod)$adj.r.squared

  
  #put coefficients into DF to save across runs --- removed rounding signif(mod$coefficients, 2)
  #coef<- mod$coefficients %>% as.data.frame() %>% tibble::rownames_to_column()  %>% `colnames<-`(c('params', 'coef'))
  
  #save summary of LOOCV
  #model <- train(as.formula(form), data = hist, method = "lm", trControl = ctrl)
  #mod_sum$loocv<- model$results
  #check residuals mod.red<- resid(model)
  
  # calculate the correlations
  #r <- round(cor(hist[bwh_sum$vars], use="complete.obs"),2)
  #ggcorrplot(r)
  
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
# formula, vars and summary stats
#vol_mod_sum<- list(bwh = bwh_vol_mod[[1]], bws = bws_vol_mod[[1]], sc = sc_vol_mod[[1]], cc = cc_vol_mod[[1]])
#lm and coefficients
vol_models<- list(bwh_mod = bwh_vol_mod, bws_mod = bws_vol_mod, sc_mod = sc_vol_mod, cc_mod = cc_vol_mod)
#vol_coef<- cbind(bwh_vol_mod[[3]], bws_vol_mod[[3]], sc_vol_mod[[3]], cc_vol_mod[[3]])
#write.csv(vol_coef, file.path(model_out,'vol_coeff.csv'), row.names = FALSE)

# Pull out R2 for summary stats --- just save table from Sams code

# r2s<- data.frame(matrix(ncol = 3, nrow = 4))
# colnames(r2s)<-c("AdjR2", "Loocv R2", "MAE")
# rownames(r2s)<-c("BWH", "BWS", "SC", "CC")
# r2s[,1]<- round(c(bwh_vol_mod[[1]]$true.r2, bws_vol_mod[[1]]$true.r2, sc_vol_mod[[1]]$true.r2,cc_vol_mod[[1]]$true.r2)*100, 2)
# r2s[,2]<- round(c(bwh_vol_mod[[1]]$loocv$Rsquared, bws_vol_mod[[1]]$loocv$Rsquared,sc_vol_mod[[1]]$loocv$Rsquared,cc_vol_mod[[1]]$loocv$Rsquared)*100, 2)
# r2s[,3]<- round(c(bwh_vol_mod[[1]]$loocv$MAE, bws_vol_mod[[1]]$loocv$MAE, sc_vol_mod[[1]]$loocv$MAE, cc_vol_mod[[1]]$loocv$MAE), 2)
# 
# png(file.path("r2s.png"), height = 25*nrow(r2s), width = 80*ncol(r2s))
# grid.table(r2s)
# dev.off()


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


