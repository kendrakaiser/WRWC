
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


#specify the cross-validation method
#ctrl <- trainControl(method = "LOOCV")

######functions##########
refitModel=function(model,newData=todayData$allVar){
  modelForm=formula(model)
  responseName=names(model$model)[1] #first column of $model within model object is response
  if(grepl("log.",responseName,fixed=T)){
    unlogTermName=gsub("log.","",responseName)
    newData$logResponse=log(newData[,names(newData)==unlogTermName])
    names(newData)[names(newData)=="logResponse"]=responseName
  }
  newModel=lm(formula=modelForm,data=newData)
  
  # #pass along added elements to model object
  # newModel$form=model$form
  # newModel$vars=model$vars
  newModel$vars=names(newModel$coefficients)[-1]
  return(newModel)
}


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

# get formatted formulas out of a leap returned list
getLeapFormulas=function(leapList,responseVarName){
  leapList$label=leapList$label[-1] #drop '(Intercept)' from terms list
  forms=character()
  for(i in 1:nrow(leapList$which)){
    varNames=leapList$label[leapList$which[i,]]
    varNames=varNames[order(varNames)] #order the variables to simplify avoidance of fitting replicate models
    forms = c(forms,paste( responseVarName," ~ ", paste(varNames,collapse=" + ")) )
  }
  return(forms)
}

vol_model<-function(site, sites, max_var, min_var=2, forceVars=NULL, pred.year = pred.yr, volVars=firstOfMonthData$allVar){
  
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  usePredTypes=c("wq", "ly_vol","snow_covered_area","swe_total","swe","nj_t")
  #dropped "liquid_precip"
  offSiteTypes=c("swe","nj_t")
  
  #forceVars="bwh.wq"
  # site= "bwh"
  # sites= "bwh"
  # max_var = 10
  # 
  responseName=paste0(site,".irr_vol")
  # classify predictors into groups based on naming convention (prevents duplicates from each type)
  predTypeNames=sub(".*\\.","",names(volVars)) %in% usePredTypes
  siteNames=grepl(paste(sites, collapse="|"), colnames(volVars))
  onSiteNames=names(volVars)[predTypeNames & siteNames]
  
  #variables that are not specific to the watershed (SWE & temp)
  offSiteTypes=offSiteTypes[offSiteTypes %in% usePredTypes]
  offSiteNames=names(volVars)[sub(".*\\.","",names(volVars))  %in% offSiteTypes]  
  
  # save current values to check for presence in predictor dataset
  currentVars=volVars[volVars$wateryear==pred.year,]

  badVars=names(currentVars)[is.na(currentVars)] # no special treatment for aj_t in vol models
  
  # select historic values
  volVars=volVars[volVars$wateryear < pred.year,]
  
  #recombine all possible terms
  modelDF=volVars[,c(onSiteNames, offSiteNames)]
  
  #drop vars which cant be used as predictors
  modelDF=modelDF[,!names(modelDF) %in% badVars]
  
  #add response as first column
  modelDF= cbind(volVars[,responseName],
                 modelDF)
  
  names(modelDF)[1]=responseName
  modelDF=modelDF[complete.cases(modelDF),]
  
  
  
  ############################# log response ################################
  unloggedResponse=modelDF[,1]
  modelDF[,1]=log(modelDF[,1])
  names(modelDF)[1]=paste0("log.",names(modelDF)[1])
  responseName=names(modelDF)[1]
  #best combn of one of each `type` of variable (conveniently defined by suffix)
  predTypes=as.factor(sub(".*\\.","",names(modelDF)[-1]))
  
  predTypeList=list()
  for(i in 1:length(levels(predTypes))){
    predTypeList[[i]]=names(modelDF)[-1][predTypes==levels(predTypes)[i]]
    
  }
  names(predTypeList)=levels(predTypes)
  predTypes = expand.grid(predTypeList[])
  rm(predTypeList)
  
  useForce=F
  if(!is.null(forceVars)){
    if(!is.na(forceVars)){
      if(any(forceVars==predTypes)){
        useForce=T
      } else {
        print(paste("forced var",forceVars,"not found in dataset"))
      }
    }
  }
  
  typeModels=paste(names(modelDF)[1],"~",apply(predTypes, 1, paste, collapse=' + '))
  
  dfLength=nrow(predTypes)*ncol(predTypes)
  
  modelCompareDF=data.frame(formula=character(dfLength),
                            n=numeric(dfLength),
                            r2=numeric(dfLength),
                            BIC=numeric(dfLength),
                            #max_vif=numeric(dfLength),
                            AICc=numeric(dfLength)
  )
  
  
  i=1 
  #profvis({
  for(globalFormula in typeModels){
    
    global_lm=lm(globalFormula,data=modelDF,na.action=na.fail)
    
    #leaps + fit method (faster)
    thisVars=names(coefficients(global_lm))[-1]
    leapList=leaps(x=modelDF[,thisVars],y=modelDF[,1],method="r2",nbest=1,names=thisVars)
    forms=getLeapFormulas(leapList,responseVarName=names(modelDF)[1])
    
    if(useForce){
      forms=forms[grepl(forceVars,forms,fixed=T)]
    }
    
    formLength=sapply(strsplit(forms,"\\+"),length)
    forms=forms[formLength>min_var & formLength<max_var]
    for(f in forms){
      if(!f %in% modelCompareDF$formula){ #this sub model has not been tested, proceed...
        thisModel=lm(f,data=modelDF)
        
        modelCompareDF$formula[i]=f
        modelCompareDF$n[i]=length(thisModel$coefficients)-1
        modelCompareDF$r2[i]=summary(thisModel)$r.squared
        modelCompareDF$BIC[i]=BIC(thisModel)
        modelCompareDF$AICc[i]=AICc(thisModel)
        
        i=i+1
      }
    }
  } 
  #})
  
  
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
  
  return(mod_sum)
}


cm_model<-function(site, sites, max_var, min_var=2, pred.year = pred.yr, cmVars=firstOfMonthData$allVar){
  
  'site: site name as string
   sites: list of sites with relevant variables for prediction 
   max_var: max number of variables  
  '
  usePredTypes=c("wq", "ly_vol","snow_covered_area","swe_total","swe","nj_t","aj_t")
  #dropped: "liquid_precip"
  offSiteTypes=c("swe","nj_t","aj_t")
  
  # site= "bws"
  # sites= "bws"
  # max_var = 6
  
  responseName=paste0(site,".cm")
  # classify predictors into groups based on naming convention (prevents duplicates from each type)
  predTypeNames=sub(".*\\.","",names(cmVars)) %in% usePredTypes
  siteNames=grepl(paste(sites, collapse="|"), colnames(cmVars))
  onSiteNames=names(cmVars)[predTypeNames & siteNames]
  #variables that are not specific to the watershed (SWE & temp)
  offSiteTypes=offSiteTypes[offSiteTypes %in% usePredTypes]
  offSiteNames=names(cmVars)[sub(".*\\.","",names(cmVars))  %in% offSiteTypes]   
  
  # save current values to check for presence in predictor dataset
  currentVars=cmVars[cmVars$wateryear==pred.year,]
  badVars=names(currentVars)[is.na(currentVars) & !grepl("aj_t",names(currentVars),fixed = T)] # leave all aj_t in dataset
  
  
  # select historic values
  cmVars=cmVars[cmVars$wateryear < pred.year,]
  
  #recombine all useable terms
  
  modelDF=cmVars[,c(onSiteNames, offSiteNames)]
  #drop vars which cant be used as predictors
  modelDF=modelDF[,!names(modelDF) %in% badVars]
  
  #add response as first col
  modelDF= cbind(cmVars[,responseName],
                 modelDF)
  
  names(modelDF)[1]=responseName
  modelDF=modelDF[complete.cases(modelDF),]
  
  
  predTypes=as.factor(sub(".*\\.","",names(modelDF)[-1]))
  
  predTypeList=list()
  for(i in 1:length(levels(predTypes))){
    predTypeList[[i]]=names(modelDF)[-1][predTypes==levels(predTypes)[i]]
    
  }
  names(predTypeList)=levels(predTypes)
  predTypes = expand.grid(predTypeList[])
  rm(predTypeList)
  
  typeModels=paste(names(modelDF)[1],"~",apply(predTypes, 1, paste, collapse=' + '))
  
  dfLength=nrow(predTypes)*ncol(predTypes)
  
  modelCompareDF=data.frame(formula=character(dfLength),
                            n=numeric(dfLength),
                            r2=numeric(dfLength),
                            BIC=numeric(dfLength)
                            #max_vif=numeric(dfLength),
                            #AICc=numeric(dfLength)
  )
  
  
  i=1 
  #profvis({
  for(globalFormula in typeModels){
    
    global_lm=lm(globalFormula,data=modelDF,na.action=na.fail)
    
    #leaps + fit method (faster)
    thisVars=names(coefficients(global_lm))[-1]
    leapList=leaps(x=modelDF[,thisVars],y=modelDF[,1],method="r2",nbest=1,names=thisVars)
    forms=getLeapFormulas(leapList,responseVarName=names(modelDF)[1])
    formLength=sapply(strsplit(forms,"\\+"),length)
    forms=forms[formLength>min_var & formLength<max_var]
    for(f in forms){
      if(!f %in% modelCompareDF$formula){ #this sub model has not been tested, proceed...
        thisModel=lm(f,data=modelDF)
        
        modelCompareDF$formula[i]=f
        modelCompareDF$n[i]=length(thisModel$coefficients)-1
        modelCompareDF$r2[i]=summary(thisModel)$r.squared
        modelCompareDF$BIC[i]=BIC(thisModel)
        
        i=i+1
      }
    }
  } 
  #})
  
  modelCompareDF=modelCompareDF[modelCompareDF$n <= max_var,]
  modelCompareDF=modelCompareDF[modelCompareDF$n >= min_var,]
  modelCompareDF=modelCompareDF[order(modelCompareDF[,"BIC"]),]
  #print(head(modelCompareDF))
  bestByType=lm(modelCompareDF$formula[1],data=modelDF,na.action=na.fail)
  
  mod_sum=bestByType
  mod_sum$form <- paste(responseName," ~ ", paste(names(bestByType$coefficients)[-1],collapse=" + "))
  mod_sum$vars<-names(bestByType$coefficients)[-1]
  
  return(mod_sum)
}




#######check if new volume models are needed------------
volModelJSON=dbGetQuery(conn,paste("SELECT DISTINCT ON (modelname) * FROM volumemodels WHERE EXTRACT(month from moddate) = ",format.Date(end_date,"%m"),"
                                   AND EXTRACT(year from moddate) = ",format.Date(end_date,"%Y"),"
                                   ORDER BY modelname, rundate DESC;" ))

########note:::
#there is the potential for some weird behavior here if the models are not re-fit for each simulation.
#the point of this is to avoid re running the selection process, and rely on the terms already selected
#But, it is probably best not to blindly trust model coefficients taken from the db 
#without a way to be more specific about which model is grabbed 
#currently uses the latest rundate for the correct moddate

if(all(
  reuseMonthlyModels,
  c("bwh_mod", "bws_mod", "sc_mod",  "cc_mod" ) %in% volModelJSON$modelname,
  format.Date(volModelJSON$rundate,"%m-%Y") == format.Date(Sys.Date(),"%m-%Y")# check that all models are from this month and year, 
  #dont reuse older models when re running older simulations
)){#Reuse Volume models
  bwh_vol_mod=lm(log.bwh.irr_vol~., data=fromJSON(volModelJSON[volModelJSON$modelname=="bwh_mod",]$modeldatajson ))
  bws_vol_mod=lm(log.bws.irr_vol~., data=fromJSON(volModelJSON[volModelJSON$modelname=="bws_mod",]$modeldatajson ))
  cc_vol_mod=lm(log.cc.irr_vol~., data=fromJSON(volModelJSON[volModelJSON$modelname=="cc_mod",]$modeldatajson ))
  sc_vol_mod=lm(log.sc.irr_vol~., data=fromJSON(volModelJSON[volModelJSON$modelname=="sc_mod",]$modeldatajson ))
  
} else { # reselect terms
  # Create Volume Models for each site gage
  print("Selecting new volume models....")
  bwh_vol_mod<- vol_model("bwh", "bwh", model_n,forceVars="bwh.wq")
  bws_vol_mod<- vol_model("bws", c("bws", "bwh"), model_n,forceVars="bwh.wq")
  cc_vol_mod<- vol_model("cc", c("bwh", "cc\\."), model_n)
  sc_vol_mod<- vol_model("sc", c("bwh", "sc"), model_n,forceVars="bwh.wq")
}


firstOfMonthVolModels<- list(bwh_mod = bwh_vol_mod, bws_mod = bws_vol_mod, sc_mod = sc_vol_mod, cc_mod = cc_vol_mod)

if(refitModelToToday){ #refit models to latest data (no reselection of terms)
  bwh_vol_mod=refitModel(bwh_vol_mod)
  bws_vol_mod=refitModel(bws_vol_mod)
  sc_vol_mod=refitModel(sc_vol_mod)
  cc_vol_mod=refitModel(cc_vol_mod)
  vol_models=list(bwh_mod = bwh_vol_mod, bws_mod = bws_vol_mod, sc_mod = sc_vol_mod, cc_mod = cc_vol_mod)
} else {
  bwh_vol_mod=refitModel(bwh_vol_mod,newData = firstOfMonthData$allVar)
  bws_vol_mod=refitModel(bws_vol_mod,newData = firstOfMonthData$allVar)
  sc_vol_mod=refitModel(sc_vol_mod,newData = firstOfMonthData$allVar)
  cc_vol_mod=refitModel(cc_vol_mod,newData = firstOfMonthData$allVar)
  vol_models=list(bwh_mod = bwh_vol_mod, bws_mod = bws_vol_mod, sc_mod = sc_vol_mod, cc_mod = cc_vol_mod)
  
}




#######check if new center of mass models are needed------------
cmModelJSON=dbGetQuery(conn,paste("SELECT DISTINCT ON (modelname) * FROM centermassmodels WHERE EXTRACT(month from moddate) = ",format.Date(end_date,"%m"),"
                                   AND EXTRACT(year from moddate) = ",format.Date(end_date,"%Y"),"
                                   ORDER BY modelname, rundate DESC;" ))



if(all(
  reuseMonthlyModels,
  c("bwh_cm_mod", "bws_cm_mod", "sc_cm_mod",  "cc_cm_mod" ) %in% cmModelJSON$modelname,
  format.Date(cmModelJSON$rundate,"%m-%Y") == format.Date(Sys.Date(),"%m-%Y")# check that all models are from this month and year, 
  #dont reuse older models when re running older simulations
)){#Reuse CM models
  bwh_cm_mod=lm(bwh.cm~., data=fromJSON(cmModelJSON[cmModelJSON$modelname=="bwh_cm_mod",]$modeldatajson ))
  bws_cm_mod=lm(bws.cm~., data=fromJSON(cmModelJSON[cmModelJSON$modelname=="bws_cm_mod",]$modeldatajson ))
  sc_cm_mod=lm(sc.cm~., data=fromJSON(cmModelJSON[cmModelJSON$modelname=="sc_cm_mod",]$modeldatajson ))
  cc_cm_mod=lm(cc.cm~., data=fromJSON(cmModelJSON[cmModelJSON$modelname=="cc_cm_mod",]$modeldatajson ))
  
} else { #select new CM models
  # Create Center of Mass Models for each site
  print("Selecting new center of mass models....")
  bwh_cm_mod<- cm_model("bwh", "bwh", max_var=6)
  bws_cm_mod<- cm_model("bws", "bws", max_var=6)
  sc_cm_mod<- cm_model("sc", c("bwh","sc"),max_var=6)
  cc_cm_mod<- cm_model("cc", "cc\\.", max_var=6)
}


#compile all model details into one list to export
firstOfMonthCmModels= list(bwh_cm_mod = bwh_cm_mod, bws_cm_mod = bws_cm_mod, sc_cm_mod = sc_cm_mod, cc_cm_mod = cc_cm_mod)

if(refitModelToToday){
  bwh_cm_mod=refitModel(bwh_cm_mod)
  bws_cm_mod=refitModel(bws_cm_mod)
  sc_cm_mod=refitModel(sc_cm_mod)
  cc_cm_mod=refitModel(cc_cm_mod)
  cm_models=list(bwh_cm_mod = bwh_cm_mod, bws_cm_mod = bws_cm_mod, sc_cm_mod = sc_cm_mod, cc_cm_mod = cc_cm_mod)
} else {
  bwh_cm_mod=refitModel(bwh_cm_mod,newData=firstOfMonthData$allVar)
  bws_cm_mod=refitModel(bws_cm_mod,newData=firstOfMonthData$allVar)
  sc_cm_mod=refitModel(sc_cm_mod,newData=firstOfMonthData$allVar)
  cc_cm_mod=refitModel(cc_cm_mod,newData=firstOfMonthData$allVar)
  cm_models=list(bwh_cm_mod = bwh_cm_mod, bws_cm_mod = bws_cm_mod, sc_cm_mod = sc_cm_mod, cc_cm_mod = cc_cm_mod)
}


#sapply(cm_models,summary)

getr2=function(model){
  return(summary(model)$r.squared)
}

r2s_cm=data.frame(name=names(cm_models),
                  r2=sapply(cm_models,getr2))


# png(file.path(fig_dir_mo,"r2s_cm.png"), height = 25*nrow(r2s_cm), width = 80*ncol(r2s_cm))
# grid.table(r2s_cm)
# dev.off()


