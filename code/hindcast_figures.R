#hindcasting accuracy plots#
#December 9, 2025

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

#todo: add model run note to forecastvolumes table 
dbGetQuery(conn,"SELECT DISTINCT(rundate) FROM forecastvolumes ORDER BY rundate DESC")
# weighted backcasting runs: rundate 2025-12-19
# unweighted backcasting runs: rundate 2025-12-30

dbGetQuery(conn,"SELECT DISTINCT(site) FROM forecastvolumes;")
site = "bwh"
month = "04"
endOfMonth = F #model runs for first day or last day

if(endOfMonth){
  d_char=">"
} else {
  d_char = "="
}


#from forecastvolumes
#rundate is hardcoded to the day I did all the hindcasting...
#this one uses the parameters set above....
back_vols=dbGetQuery(conn,paste0("SELECT site, metric, rundate, simdate, extract(year FROM simdate) AS wateryear, values FROM forecastvolumes 
                    WHERE rundate = '2025-12-19'
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
#exceedence in inverse of quantile
back_vols$exceedence_025=sapply(vols,quantile, probs = 0.975)
back_vols$exceedence_05=sapply(vols,quantile, probs = 0.95)
back_vols$exceedence_25=sapply(vols,quantile, probs = 0.75)
back_vols$exceedence_75=sapply(vols,quantile, probs = 0.25)
back_vols$exceedence_95=sapply(vols,quantile, probs = 0.05)
back_vols$exceedence_975=sapply(vols,quantile, probs = 0.025)

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



obs_vols$actual_KAF=obs_vols$irr_vol/1000
head(obs_vols)
hist(obs_vols$actual_KAF)

volPerformance=merge(back_vols,obs_vols)
head(volPerformance)

#lowFlowCutoff= median(volPerformance$actual_KAF)
#volPerformance$lowFlow=volPerformance$actual_KAF<lowFlowCutoff

write.csv(volPerformance,"BWH_VolModelPerformance.csv")

plot(volPerformance$actual_KAF,volPerformance$med_KAF, pch=16,cex=1.5,
     main = paste0("2005 - 2025 Model Performance for ",unique(volPerformance$sitename)#," \n (month=",month,", end of month=",endOfMonth,"
                   ), cex.main = 1,
     xlim=c(50,650),ylim=c(50,650),
     xlab = "Observed irrigation season volume (KAF)",
     ylab = "Predicted irrigation season volume (KAF)")
abline(a=0,b=1)

legend(x="topleft",legend=c("25% - 75% exceedence probability","5% - 95% exceedence probability", "2.5% to 97.5% exceedence probability"),
       lty=1,lwd=3,col=c("lightgrey","darkgrey","darkblue"),bty="n",bg=NULL)

for(y in volPerformance$wateryear){
  #95% interval
  lines(x=rep(volPerformance$actual_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$exceedence_025[volPerformance$wateryear==y],volPerformance$exceedence_975[volPerformance$wateryear==y]),
        col="lightgrey",lwd=3)
  
  #90% interval
  lines(x=rep(volPerformance$actual_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$exceedence_05[volPerformance$wateryear==y],volPerformance$exceedence_95[volPerformance$wateryear==y]),
        col="darkgray",lwd=3)
  
  #50% interval
  lines(x=rep(volPerformance$actual_KAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$exceedence_25[volPerformance$wateryear==y],volPerformance$exceedence_75[volPerformance$wateryear==y]),
        col="darkblue",lwd=3)
}

points(volPerformance$actual_KAF,volPerformance$med_KAF, pch=16,cex=1.5)

volPerformance$absErr=abs(volPerformance$med_KAF-volPerformance$actual_KAF)
median(volPerformance$absErr) #wt: 3.2354, unwt: 3.6748
quantile(volPerformance$absErr,0.95)

median(volPerformance$absErr[volPerformance$lowFlow]) #wt: 3.2334, unwt: 4.34139

volPerformance$pctErr=100 * (volPerformance$absErr/volPerformance$actual_KAF)
median(volPerformance$pctErr)
median(volPerformance$pctErr[volPerformance$lowFlow])



getQuantile=function(yr,obs,preds=vols){
  preds=preds[[as.character(yr)]]
  pred_quantile=sum(obs>preds)/length(preds)
  return(pred_quantile)
}

volPerformance$exceedence_of_actual=100*(1-mapply(getQuantile,yr=volPerformance$wateryear,obs=volPerformance$actual_KAF))

plot(volPerformance$exceedence_of_actual~volPerformance$actual_KAF)


#forecasted center of mass is not stored in db

