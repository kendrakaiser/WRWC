#run the model scripts through 04, then run this stuff

#################---------- data wrangling copied from 05_streamflow, simplified to bws for this example ------------
library(MuMIn)

swe_cols<-grep("(?!swe_)_?swe", colnames(var), perl = TRUE, value = TRUE)
wint_t_cols<-grep('nj_t', colnames(var))
aj_t_cols<-grep('aj_t', colnames(var))
irr_vol_cols<- grep('irr_vol', colnames(var))
tot_vol_cols<- grep('tot_vol', colnames(var))
snodas_cols<- c(grep('wint', colnames(var)), grep('snow', colnames(var)), grep('swe_total', colnames(var)))
wq_cols<- grep('wq', colnames(var))
runoff_cols<- grep('runoff', colnames(var))


# bwh_vol_mod<- vol_model("bwh", "bwh", 9)
# bws_vol_mod<- vol_model("bws", c("bws"), 9)
# cc_vol_mod<- vol_model("cc", c("bwh", "cc\\."), 9)
# sc_vol_mod<- vol_model("sc", c("bwh", "sc"), 9)

site="bws" #this site
sites="bws" # used for variable selection

site_vars<- grep(paste(sites, collapse="|"), colnames(var))
hist <- var[var$wateryear < pred.yr,] %>% dplyr::select(wateryear, all_of(site_vars),
                                                        all_of(swe_cols), all_of(wint_t_cols), -all_of(c(tot_vol_cols, runoff_cols))) %>% filter(complete.cases(.))
name<- paste0(site, ".irr_vol")
#id column names that should be removed from the modeling set
irr_vols<- colnames(hist)[grep('irr_vol', colnames(hist))]
vol_col<-grep(name, colnames(hist))
cms<- colnames(hist)[grep('cm', colnames(hist))]

########################------------Model selection, starting with the functions from 05_streamflow
max_var=15 # I set this pretty high to test the model selection criteria

#regsubsets is stupid fast for what it does - the other approach I was considering takes a few hours to run.
tryCatch({regsubsets.out<-regsubsets(log(hist[[vol_col]])~., 
                                     data=hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE], 
                                     nbest=1, nvmax=max_var, really.big=T)}, 
         error= function(e) {print(paste(site,"volume model did not work"))})
reg_sum<- summary(regsubsets.out)
rm(regsubsets.out)

#get the fitted df 
fitDF=cbind(data.frame(log_iv=log(hist[[vol_col]]),hist[, !names(hist) %in% c("wateryear", irr_vols, cms)]))

#function to make an easier to read df with some iinfo on the models
getRegModelSummary=function(regSum=reg_sum,f_fitDF=fitDF, response="log_iv" ){
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

#run the function
allModels_log=getRegModelSummary()
#look at allModels_log
#  My takeaways here - BIC absolutely does not work for model selection.  
#  This must be at least partially because of the small dataset
#AICc is a bit better (at least it indicates that some models are overfit!) but I still dont blindly trust it


#moving on from the bic and aicc selection metrics, I want to think about the cross validation metrics in real terms (actual prediction error in KAF)
#there are some issues with doing the cross validation this way - it would be better to cross validate the model selection
#but regsubsets is so fast i dont want to fuck with that part

#stupid (and slow) loop for cross validation.  Note that it can do leav one out (loocv) or k-fold.  
#Not really sure why but I think k-fold is a better test of predictive power, and i choose k=10 for basically no reason at all
allModels_log$cv_meanAbsError_kaf=0
allModels_log$cv_worstError_kaf=0
for(m in 1:nrow(allModels_log)){
  #k=nrow(fitDF) # loocv
  k=10  #k-fold cv
  thisModel=lm(allModels_log$form[m],fitDF)
  errors_kaf=numeric()
  
  for(i in 1:1000){
    holdout=sample(1:nrow(fitDF),nrow(fitDF)/k)
    lko_lm=lm(reformulate(names(thisModel$coefficients)[-1],response="log_iv"),fitDF[-holdout,])
    lko_log_iv=predict(lko_lm, newdata=fitDF[holdout,])
    errors_kaf=c(errors_kaf,exp(fitDF[holdout,"log_iv"])/1000-exp(lko_log_iv)/1000)
  }
  #hist(errors_kaf)
  allModels_log$cv_meanAbsError_kaf[m]=mean(abs(errors_kaf))
  allModels_log$cv_worstError_kaf[m]=max(abs(errors_kaf))
  
}

#Now look at allModels_log - it has the mean error in real units, and the max error from the cross validation set
#this max error is intended to indicate model behavior when faced with an unusual year beyond the bounds of the fitted dataset
#with a high number of terms, there will be some predictions with very high max errors
#I think that this is because of spurious correlations and I think it indicates the danger of over-fitting
#The exact values will vary every run due to the random sampling in the loop
#my takeaways at this point - not sure how to select a good model, but the big ones are probably overfitted


#BUT, After looking at a bunch of plots, I am questioning the need to log-transform the response var.  Here is the same shit unlogged:
tryCatch({l_regsubsets.out<-regsubsets(hist[[vol_col]]~., 
                                     data=hist[, !names(hist) %in% c("wateryear", irr_vols, cms), drop = FALSE], 
                                     nbest=1, nvmax=max_var, really.big=T)}, 
         error= function(e) {print(paste(site,"volume model did not work"))})
l_reg_sum<- summary(l_regsubsets.out)
rm(l_regsubsets.out)

fitDF_linear=cbind(data.frame(iv=hist[[vol_col]],hist[, !names(hist) %in% c("wateryear", irr_vols, cms)]))


allModels_linear=getRegModelSummary(regSum=l_reg_sum,f_fitDF=fitDF_linear, response="iv" )

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

#compare allModels_linear to allModels_log
#r2, bic, and aic are not comparable, but the error terms are in real units and are comparable
#in terms of mean error, the linear models are as good or, in some cases, better!
#in terms of max error, they are way less scary - because there is less potential for a small (combination of) change(s) in the variables
# to cause a massive change in the response

#also, bic is still useless, aicc is better but not exactly good

#Morals of the story: 
# - I think we should consider fitting unlogged kaf
# - I think that the models with 6-8 parameters are likely to be as good as any
# - I think that we can cautiously use AICc to help with model selection




#stepwise aicc selection - doesn't appear to be useful just yet
# thisModel=lm(iv~1,data=fitDF_linear)
# for(i in 1:10){
#   print(summary(thisModel))
#   print(AICc(thisModel))
#   a1=add1(object=thisModel,scope=lm(iv~.,data=fitDF_linear))
#   thisModel=lm(reformulate(c(names(thisModel$coefficients)[-1],rownames(a1)[which.min(a1$RSS)]),response="iv"),data=fitDF_linear)
# }



