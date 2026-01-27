#hindcasting accuracy plots#
#December 9, 2025

#source('code/init_db.R')
#sed directory if needed
git_dir=getwd() #set to correct directory 
source(file.path(git_dir, 'code/01_packages.R'))
source(file.path(git_dir, 'code/init_db.R'))

pgArrayToNumeric=function(x){
  f=function(one_x){
    return(as.numeric(unlist(strsplit(gsub("[\\{\\}]","",one_x),","))))
  }
  return(lapply(x,f))
}

#### set things here #####

dbGetQuery(conn,"SELECT DISTINCT(rundate) FROM forecastvolumes ORDER BY rundate DESC")
# weighted backcasting runs: rundate 2025-12-19
# unweighted backcasting runs: rundate 2025-12-30

dbGetQuery(conn,"SELECT DISTINCT(site) FROM forecastvolumes;")
site = "sc"
month = "04"
endOfMonth = F #model runs for first day or last day


if(endOfMonth){
  d_char=">"
} else {
  d_char = "="
}


#from forecastvolumes
#rundate is hardcoded to the day I did all the hindcasting...
#this one uses the flags set above....
back_vols=dbGetQuery(conn,paste0("SELECT site, metric, rundate, simdate, extract(year FROM simdate) AS wateryear, values FROM forecastvolumes 
                    WHERE rundate = '2025-12-30'
                    AND metric = 'irr_vol'
                    AND extract(day FROM simdate) ", d_char," '1'
                    AND extract(month FROM simdate) = '",month,"'
                    AND site = '",site,"'
                     ;"))

#or just set things directly in the query
# back_vols=dbGetQuery(conn,"SELECT site, metric, rundate, simdate, extract(year FROM simdate) AS wateryear, values FROM forecastvolumes 
#                     WHERE rundate = '2025-12-09'
#                     AND metric = 'irr_vol'
#                     AND extract(day FROM simdate) = '1'
#                     AND extract(month FROM simdate) = '04'
#                     AND site = 'bwh'
#                      ;")

vols=pgArrayToNumeric(back_vols$values)
names(vols) = back_vols$wateryear

back_vols$med_KAF=sapply(vols,median) #median prediction volume

#### set lower and upper quantile definitions for plot here ###
back_vols$lq_95=sapply(vols,quantile, probs = 0.025)
back_vols$uq_95=sapply(vols,quantile, probs = 0.975)

back_vols$lq_90=sapply(vols,quantile, probs = 0.05)
back_vols$uq_90=sapply(vols,quantile, probs = 0.95)

back_vols$lq_50=sapply(vols,quantile, probs = 0.25)
back_vols$uq_50=sapply(vols,quantile, probs = 0.75)

back_vols$values=NULL
head(back_vols)

obs_vols=dbGetQuery(conn,paste0("SELECT * FROM (
            SELECT wateryear(datetime) AS wateryear, COUNT(DISTINCT EXTRACT(doy FROM datetime)) AS days_in_record,  
            SUM(value)*1.98 AS irr_vol, 
            data.locationid, name AS sitename, sitenote AS site
            FROM data LEFT JOIN locations ON data.locationid = locations.locationid
            WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
            AND sitenote = '",site,"'
            GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear
          ) as histvols WHERE days_in_record > 180;"))  # complete record is 183 days



obs_vols$obs_KAF=obs_vols$irr_vol/1000
head(obs_vols)
hist(obs_vols$obs_KAF)

volPerformance=merge(back_vols,obs_vols)
head(volPerformance)

lowFlowCutoff= median(volPerformance$obs_KAF)
volPerformance$lowFlow=volPerformance$obs_KAF<lowFlowCutoff


plot(volPerformance$obs_KAF,volPerformance$med_KAF, pch=16,cex=1.5,
     main = paste0("2005 - 2025 Model Performance for ",unique(volPerformance$sitename)," \n (month=",month,", end of month=",endOfMonth,")"), cex.main = 1,
     xlab = "Observed irrigation season volume (KAF)",
     ylab = "Predicted irrigation season volume (KAF)")
abline(a=0,b=1)
for(y in volPerformance$wateryear){
  #95% interval
  lines(x=rep(volPerformance$obs_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$lq_95[volPerformance$wateryear==y],volPerformance$uq_95[volPerformance$wateryear==y]),
        col="lightgrey",lwd=3)
  
  #90% interval
  lines(x=rep(volPerformance$obs_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$lq_90[volPerformance$wateryear==y],volPerformance$uq_90[volPerformance$wateryear==y]),
        col="darkgray",lwd=3)
  
  #50% interval
  lines(x=rep(volPerformance$obs_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$lq_50[volPerformance$wateryear==y],volPerformance$uq_50[volPerformance$wateryear==y]),
        col="darkblue",lwd=3)
}

points(volPerformance$obs_KAF,volPerformance$med_KAF, pch=16,cex=1.5)

volPerformance$absErr=abs(volPerformance$med_KAF-volPerformance$obs_KAF)
median(volPerformance$absErr) #wt: 3.2354, unwt: 3.6748
median(volPerformance$absErr[volPerformance$lowFlow]) #wt: 3.2334, unwt: 4.34139

volPerformance$pctErr=100 * (volPerformance$absErr/volPerformance$obs_KAF)
median(volPerformance$pctErr)
median(volPerformance$pctErr[volPerformance$lowFlow])



# getQuantile=function(yr,obs,preds=vols){
#   preds=preds[[as.character(yr)]]
#   pred_quantile=sum(obs>preds)/length(preds)
#   return(pred_quantile)
# }
# 
# volPerformance$quantileOfPrediction=100*mapply(getQuantile,yr=volPerformance$wateryear,obs=volPerformance$obs_KAF)
# 
# plot(volPerformance$quantileOfPrediction~volPerformance$obs_KAF)
# 

#forecasted center of mass is not stored in db

