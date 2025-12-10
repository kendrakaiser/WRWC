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
#this one uses the flags set above....
back_vols=dbGetQuery(conn,paste0("SELECT site, metric, rundate, simdate, extract(year FROM simdate) AS wateryear, values FROM forecastvolumes 
                    WHERE rundate = '2025-12-09'
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

back_vols$med_kAF=sapply(vols,median) #median prediction volume

back_vols$lq_kAF=sapply(vols,quantile, probs = 0.10) #lower quantile
back_vols$uq_kAF=sapply(vols,quantile, probs = 0.90) #upper quantile


back_vols$values=NULL
head(back_vols)

obs_vols=dbGetQuery(conn,paste0("SELECT * FROM (
            SELECT wateryear(datetime) AS wateryear, COUNT(DISTINCT EXTRACT(doy FROM datetime)) AS days_in_record,  
            SUM(value)*1.98 AS irr_vol, 
            data.locationid, name AS sitename, sitenote AS site
            FROM data LEFT JOIN locations ON data.locationid = locations.locationid
            WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
            AND locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
            GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear
          ) as histvols WHERE days_in_record > 180;"))  # complete record is 183 days



obs_vols$obs_kAF=obs_vols$irr_vol/1000
head(obs_vols)

volPerformance=merge(back_vols,obs_vols)
head(volPerformance)

plot(volPerformance$obs_kAF,volPerformance$med_kAF, 
     main = paste0("2005 - 2025 Model Performance for ",unique(volPerformance$sitename)," \n (month=",month,", end of month=",endOfMonth,")"), cex.main = 1,
     xlab = "Observed irrigation season volume (kAF)",
     ylab = "Predicted irrigation season volume (kAF)")
abline(a=0,b=1)
for(y in volPerformance$wateryear){
  lines(x=rep(volPerformance$obs_kAF[volPerformance$wateryear==y],2),
        y=c(volPerformance$lq_kAF[volPerformance$wateryear==y],volPerformance$uq_kAF[volPerformance$wateryear==y])
  )
}



getQuantile=function(yr,obs,preds=vols){
  preds=preds[[as.character(yr)]]
  pred_quantile=sum(obs>preds)/length(preds)
  return(pred_quantile)
}

volPerformance$quantileOfPrediction=100*mapply(getQuantile,yr=volPerformance$wateryear,obs=volPerformance$obs_kAF)

plot(volPerformance$quantileOfPrediction~volPerformance$obs_kAF)


#forecasted center of mass is not stored in db

