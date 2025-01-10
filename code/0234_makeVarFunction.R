#function for getting var df depending on date
makeDatasets=function(date){
  
  end_date=date
  # ------------------------------------------------------------------------------
  # Download and aggregate all available data for the Wood River Predictive Model
  # Kendra Kaiser
  # June 19, 2020
  # ------------------------------------------------------------------------------
  #tools to connect and write to database
  source(file.path(git_dir, 'code/init_db.R')) #terra sf RPostgres #R.utils raster
  source(paste0(git_dir,"/code/fxn_dbIntakeTools.R")) 
  source(paste0(git_dir,"/code/fxn_get_snow.R")) 
  source(paste0(git_dir,"/code/fxn_SNODASR_functions.R")) 
  #connect to database
  #conn=scdbConnect() # done in init_db.R
  
  # ------------------------------------------------------------------------------
  # USGS Gages Data & Metrics
  # calculate hydrologic metrics for each year for each station 
  # winter "baseflow" (wb), Apr - Sept irrigation volume (irr_vol), total Volume (tot_vol), and center of mass (cm)
  # ------------------------------------------------------------------------------
  source(file.path(git_dir,'code/fxn_baseflowA.r'))
  #Average Winter flow - doesnt work properly bc needs filtered, retaining so code doesnt break
  # avgBaseflow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, AVG(value) AS wq, data.locationid, name, sitenote
  #            FROM data LEFT JOIN locations ON data.locationid = locations.locationid
  #            WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2) 
  #            GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
  
  #pull winter flow to do filter on
  wint_flow=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, datetime, metric, value AS flow, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 11 OR EXTRACT(month FROM datetime) < 2)
           AND datetime::date <= '",end_date,"'::date
           AND locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
           ORDER BY datetime;")
  )
  
  bf_wrapper=function(wy, lid, wint_flow_df){
    thisQ=wint_flow_df[wint_flow_df$wateryear == wy & wint_flow_df$locationid == lid,"flow"]
    bf=baseflowA(thisQ)
    return(mean(bf$bf))
  }
  
  
  avgBaseflow=unique(wint_flow[,c("wateryear","metric","locationid","name","sitenote")])
  
  avgBaseflow$wq = mapply(bf_wrapper, wy=avgBaseflow$wateryear, lid=avgBaseflow$locationid, MoreArgs = list(wint_flow_df=wint_flow))
  
  # pivot data wider
  baseflow<-pivot_wider(data=avgBaseflow[,c("wateryear","wq","sitenote")],names_from = c(sitenote),values_from = c(wq), names_glue = "{sitenote}.wq")
  
  #Total Irrigation Season April-September flow in AF [1.98 #convert from cfs to ac-ft]
  irr_AF=dbGetQuery(conn,paste0("SELECT * FROM (
            SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS irr_vol, data.locationid, name, sitenote, COUNT(DISTINCT(dataid)) AS days_in_record
            FROM data LEFT JOIN locations ON data.locationid = locations.locationid
            WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
            AND datetime::date <= '",end_date,"'::date
            AND locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
            GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear
          ) as histvols WHERE days_in_record > 180;"))  # complete record is 183 days
  
  #TODO  DO WE NEED A SIMILAR CATCH FOR cases (current year) where tot_af and cm_dat return an incomplete dataset?
  
  # pivot data wider
  irr_vol<-pivot_wider(data=irr_AF[,c("wateryear","irr_vol","sitenote")],names_from = c(sitenote),values_from = c(irr_vol), names_glue = "{sitenote}.irr_vol")
  
  #Total Water Year volume in AF [1.98 #convert from cfs to ac-ft]
  tot_AF=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS tot_vol, data.locationid, name, sitenote
          FROM data LEFT JOIN locations ON data.locationid = locations.locationid
          WHERE metric = 'flow' AND qcstatus = 'true' AND datetime::date <= '",end_date,"'::date
          AND locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
          GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  
  tot_AF$wy1<-tot_AF$wateryear-1
  totAF<- merge(tot_AF, tot_AF[,c('wateryear', "tot_vol", 'locationid', 'metric')], by.x=c('wy1', 'locationid', 'metric'), 
                by.y=c('wateryear','locationid', 'metric'), suffixes = c('', '_ly')) %>% dplyr::rename(ly_vol = tot_vol_ly)
  
  # pivot data wider
  tot_vol<-pivot_wider(data=totAF[,c("wateryear","tot_vol","ly_vol","sitenote")], names_from = c(sitenote), values_from = c(tot_vol, ly_vol), names_glue = "{sitenote}.{.value}")
  
  #CENTER of MASS between April 1 and July 31
  cm_dat=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS flow, data.locationid, name, sitenote,  EXTRACT(doy FROM datetime) AS doy 
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'flow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 8)
           AND datetime::date <= '",end_date,"'::date
           AND locations.name IN ('BIG WOOD RIVER AT HAILEY', 'BIG WOOD RIVER AT STANTON CROSSING', 'CAMAS CREEK NR BLAINE ID', 'SILVER CREEK AT SPORTSMAN ACCESS' )
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote, doy) ORDER BY wateryear;"))
  
  years<- min(cm_dat$wateryear):max(cm_dat$wateryear)
  cm<-data.frame(matrix(ncol = 3, nrow= length(years)*4)) %>% `colnames<-`(c('wateryear', 'sitenote', 'cm'))
  cm$wateryear<- rep(years, times=4)
  cm$sitenote<- rep(unique(cm_dat$sitenote), each=length(years))
  
  for(sitename in unique(cm$sitenote)){
    sub <- cm_dat %>% dplyr::filter(sitenote == sitename)
    for (wy in unique(cm$wateryear)){
      sub2 <- sub %>% filter(wateryear == wy)
      ix= which(cm$wateryear== wy & cm$sitenote == sitename) 
      cm$cm[ix]<- sum(sub2$doy * sub2$flow)/sum(sub2$flow)
    }}
  
  cm_wide<- cm %>% pivot_wider(names_from = sitenote, values_from = cm, names_glue = "{sitenote}.cm" )
  
  print('flow Metrics Complete')
  
  # ------------------------------------------------------------------------------
  # Retrieve Snotel Data 
  # ------------------------------------------------------------------------------
  #FEBRUARY
  winterSWE_feb=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 2)
           AND metric = 'swe' AND qcstatus = 'true' AND datetime::date <= '",end_date,"'::date
           ORDER BY wateryear;"))
  # pivot data wider
  swe_feb<-pivot_wider(data=winterSWE_feb[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")
  
  #March
  winterSWE_mar=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 3)
           AND metric = 'swe' AND qcstatus = 'true' AND datetime::date <= '",end_date,"'::date
           ORDER BY wateryear;"))
  # pivot data wider
  swe_mar<-pivot_wider(data=winterSWE_mar[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")
  
  #APRIL
  winterSWE_apr=dbGetQuery(conn,paste0("SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 4)
           AND metric = 'swe' AND qcstatus = 'true' AND datetime::date <= '",end_date,"'::date
           ORDER BY wateryear;"))
  # pivot data wider
  swe_apr<-pivot_wider(data=winterSWE_apr[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")
  
  
  # SNOTEL Sites ----
  #cg = 895 #  Chocolate Gulch (0301)
  #g  = 489 #  Galena (0101)
  #gs = 490 #  Galena Summit (0101)
  #hc = 537 #  Hyndman Creek (0402)
  #lwd= 601 #  Lost-Wood Divide (Trail Creek 0304)
  #ds = 450 #  Dollarhide Summit (Placer/Warm Springs Creek 0201)
  #ccd = 382 #  Camas Creek Divide (Sheep/Camas Creek 0101)
  #sr = 769 #  Soldier R.S (Upper Soldier Creek 0303)
  #ga = 492 #  Garfield R.S. (Upper Muldoon Creek 0301)
  #sp = 805 #  Swede Peak (Upper Muldoon Creek 0301)
  #sm = 792 # Stickney Mill
  #bc = 320 # Bear Canyon
  
  #------------------------------------------------------------------------------
  # Get SNODAS from Database 
  #------------------------------------------------------------------------------
  #FEBRUARY
  winterSums_feb=dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric, name, sitenote) wateryear(datetime) AS wateryear, max(datetime) AS datetime, metric,
           sum(value) as value, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
           WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2) 
           AND METRIC = 'liquid_precip'
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  #^^^^ this gets the sum for liquid precip (all liquid precip for the period)
  
  winterSums_feb=rbind(winterSums_feb,
                       dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric) wateryear(datetime) AS wateryear, datetime, metric, value, locations.locationid, locations.name, locations.sitenote 
                 FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
                 WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2)
                 AND metric != 'liquid_precip'
                 ORDER BY locationid, metric, wateryear, datetime DESC;"))
  )###^^This adds the latest data within the period for runoff_total, snow_covered_area, swe_total
  
  # pivot data wider
  snodas_feb<-pivot_wider(data=winterSums_feb[,c("wateryear","metric","value","sitenote")],names_from = c(sitenote, metric),values_from = c(value),names_sep=".")
  
  #MARCH
  winterSums_mar=dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric, name, sitenote) wateryear(datetime) AS wateryear, max(datetime) AS datetime, metric,
           sum(value) as value, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
           WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 3) 
           AND METRIC = 'liquid_precip'
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  #^^^^ this gets the sum for liquid precip (all liquid precip for the period)
  
  winterSums_mar=rbind(winterSums_mar,
                       dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric) wateryear(datetime) AS wateryear, datetime, metric, value, locations.locationid, locations.name, locations.sitenote 
                 FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
                 WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 3)
                 AND metric != 'liquid_precip'
                 ORDER BY locationid, metric, wateryear, datetime DESC;"))
  )###^^This adds the latest data within the period for runoff_total, snow_covered_area, swe_total
  
  # pivot data wider
  snodas_march<-pivot_wider(data=winterSums_mar[,c("wateryear","metric","value","sitenote")],names_from = c(sitenote, metric),values_from = c(value),names_sep=".")
  
  #APRIL
  winterSums_apr=dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric, name, sitenote) wateryear(datetime) AS wateryear, max(datetime) AS datetime, metric,
           sum(value) as value, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
           WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 4) 
           AND METRIC = 'liquid_precip'
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  #^^^^ this gets the sum for liquid precip (all liquid precip for the period)
  
  winterSums_apr=rbind(winterSums_apr,
                       dbGetQuery(conn,paste0("SELECT DISTINCT ON (wateryear, locationid, metric) wateryear(datetime) AS wateryear, datetime, metric, value, locations.locationid, locations.name, locations.sitenote 
                 FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
                 WHERE datetime::date <= '",end_date,"'::date AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 4)
                 AND metric != 'liquid_precip'
                 ORDER BY locationid, metric, wateryear, datetime DESC;"))
  )###^^This adds the latest data within the period for runoff_total, snow_covered_area, swe_total
  
  # pivot data wider
  snodas_april<-pivot_wider(data=winterSums_apr[,c("wateryear","metric","value","sitenote")],names_from = c(sitenote, metric),values_from = c(value),names_sep=".")
  
  
  
  ######var compiles data for the first day of each month, while current_data compiles data for tme most recent day <= end_day
  #------ generate dataset as of current (really end_date) for prediction
  #Grab current SWE
  currentSWE=dbGetQuery(conn,paste0("SELECT DISTINCT ON (locationid) datetime, wateryear(datetime) AS wateryear, metric, value AS swe, locations.locationid, locations.name, locations.sitenote 
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'swe' AND qcstatus = true AND datetime::date <= '",end_date,"'::date 
           ORDER BY locationid, datetime DESC;"))  #returns the latest value w/ DISTINCT ON (locationid) and ORDER BY datetime
  
  currentSWE=pivot_wider(data=currentSWE[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")
  
  currentSnodas=dbGetQuery(conn,paste0("SELECT DISTINCT ON (locationid, metric, name, sitenote) wateryear(datetime) AS wateryear, max(datetime) AS datetime, metric,
           sum(value) as value, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
           WHERE datetime::date <= '",end_date,"'::date
           AND METRIC = 'liquid_precip'
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY locationid, metric, name, sitenote, datetime DESC;"))
  #^^^^ this gets the sum for liquid precip (all liquid precip for the period)
  
  currentSnodas=rbind(currentSnodas,
                      dbGetQuery(conn,paste0("SELECT DISTINCT ON (locationid, metric) wateryear(datetime) AS wateryear, datetime, metric, value, locations.locationid, locations.name, locations.sitenote 
                 FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid 
                 WHERE datetime::date <= '",end_date,"'::date 
                 AND metric != 'liquid_precip'
                 ORDER BY locationid, metric, datetime DESC;"))
  )###^^This adds the latest data within the period for runoff_total, snow_covered_area, swe_total
  
  # pivot data wider
  currentSnodas<-pivot_wider(data=currentSnodas[,c("wateryear","metric","value","sitenote")],names_from = c(sitenote, metric),values_from = c(value),names_sep=".")
  
  current_data<- currentSWE %>% merge(baseflow, by= 'wateryear', all=F) %>% merge(currentSnodas, by= 'wateryear', all=F) %>% merge(tot_vol, by= 'wateryear', all=F)
  
  #------------------------------------------------------------------------------
  # Integrate & EXPORT Data: USGS SNOTEL SNODAS
  #------------------------------------------------------------------------------
  
  #February data for modeling
  alldat_feb <- swe_feb %>% merge(baseflow, by= 'wateryear', all=T) %>% merge(tot_vol, by= 'wateryear', all=TRUE) %>% 
    merge(irr_vol, by= 'wateryear', all=TRUE) %>% merge(cm_wide, by= 'wateryear', all=TRUE) %>% 
    merge(snodas_feb, by= 'wateryear', all=T)
  
  alldat_mar <- swe_mar %>% merge(baseflow, by= 'wateryear', all=T) %>% merge(tot_vol, by= 'wateryear', all=T) %>% 
    merge(irr_vol, by= 'wateryear', all=TRUE) %>% merge(cm_wide, by= 'wateryear', all=TRUE) %>% 
    merge(snodas_march, by= 'wateryear', all=T)
  
  #write.csv(alldat, file.path(data_dir,input_data), row.names=FALSE)
  
  #April data for modeling
  alldat_april <- swe_apr %>% merge(baseflow, by= 'wateryear',all=T) %>% merge(tot_vol, by= 'wateryear', all=TRUE) %>% 
    merge(irr_vol, by= 'wateryear', all=TRUE) %>% merge(cm_wide, by= 'wateryear', all=TRUE) %>% 
    merge(snodas_april, by= 'wateryear',all=T)
  
  #write.csv(alldat, file.path(data_dir,input_data), row.names=FALSE)
  
  print("All flow and snow data saved")
  
  
  
  
  
  # ----------------------------------------------------------------------------- #
  # Predictive Temperature Model for the Wood River Water Collaborative
  # Kendra Kaiser
  # December 21, 2020
  # Creates Mixed-effects regression model to predict upcoming year temperature
  # based on subsets of basins that preformed best in lm of center of mass etc
  # ----------------------------------------------------------------------------- # 
  
  # Pull AGRIMET Data from database
  # -----------------------------------------------------------------------------
  agrimet<- dbGetQuery(conn, paste0("SELECT date AS date_time, day_mean_t AS temperature_mean, site_name AS site_name, month AS mo, y AS y, wy AS wy 
                                  FROM daily_air_temperature
                                  WHERE date::date <= '",end_date,"'::date;"))
  #renamed agrimet columns to match snotel for calculations
  
  #remove values that are erroneous
  agrimet$temperature_mean[agrimet$temperature_mean < -90] <- NA
  agrimet$temperature_mean[agrimet$temperature_mean > 130] <- NA
  
  
  #Mean Nov- Jan Temp
  snotel.nj_temp=dbGetQuery(conn,paste0("SELECT count(value) AS n_obs, wateryear(datetime) AS wateryear, metric, avg(value) AS nj_tempf, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'mean daily temperature' AND qcstatus = 'true' AND sitenote != 'ccd'
           AND (EXTRACT(month FROM datetime) >= 11 OR EXTRACT(month FROM datetime) <= 1) 
           AND datetime::date <= '",end_date,"'::date
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  
  #Also, ccd has 83 obs in 2017...  including it here drops the whole year later, which kinda sucks considering how many other temperatures are available
  
  snotel.nj_temp=snotel.nj_temp[snotel.nj_temp$n_obs>=86,-1] #all 2024 data has at most 86 observations, so if I use 88 as a threshold 2024 is excluded entirely.  
  
  
  snotel.aj_temp=dbGetQuery(conn,paste0("SELECT count(value) AS n_obs, wateryear(datetime) AS wateryear, metric, avg(value) AS aj_tempf, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'mean daily temperature' AND qcstatus = 'true' AND 
           (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) <= 6)
           AND datetime::date <= '",end_date,"'::date
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;"))
  
  snotel.aj_temp=snotel.aj_temp[snotel.aj_temp$n_obs>=90,-1]    # much less missing data in this period
  
  
  
  # Analyze and Predict temperature trend ----
  # Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993
  
  first.yr<-1988
  last.yr<-pred.yr
  nyrs<-last.yr-first.yr+1
  site.key <- c(as.character(unique(agrimet$site_name))) #as.character(unique(snotel.aj_temp$sitenote)), 
  #create a dataframe to store avg. Apr/Jun Temp for each site for period of record
  tdata<-data.frame(array(NA,c(length(site.key)*nyrs,5)))
  
  
  elev<- structure(list(sitenote = c("lwd", "bc", "sr", "g", "gs", "hc", "ds", "ga", "sp", 
                                     "sm", "ccd", "cg", "picabo", "fairfield"), elev = c(7900L, 7900L, 5740L, 7470L, 
                                                                                         8780L, 7620L, 8420L, 6560L, 7640L, 7430L,5710L, 6310L, 4900L, 5038L)), 
                   class = "data.frame", row.names = c(NA,-14L))
  #tdata$elev<-rep(elev$elev, each=nyrs)
  
  #summer temp july-sept, winter temp NDJFM
  colnames(tdata)<-c("wateryear","sitenote","aj_t","nj_t", "sum_t")
  tdata$wateryear<-rep(first.yr:last.yr,length(site.key))
  tdata$sitenote<-rep(site.key, each=nyrs)
  
  #calculate average agrimet seasonal temperatures for every year
  for(i in 1:2){ 
    for (y in first.yr:last.yr){
      #subset to indv. site and year
      sub<- na.omit(agrimet[agrimet$site_name == site.key[i] & agrimet$wy==y,])
      #average april - june temps
      #if length is greater than 95% of the desired period calculate the mean
      if (length(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"]) > 88) {
        aj.mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=TRUE)
      } else (aj.mean.temp <- NA)
      #average summer temps July Aug Sept
      if (length(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"]) > 88) {
        sum.mean.temp <- mean(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"], na.rm=TRUE)
      } else (sum.mean.temp <- NA)
      #average nov-jan temps
      if (length(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"]) > 88) {
        nj.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"], na.rm=TRUE)
      } else (nj.mean.temp <- NA)
      
      #save to tdata table
      tdata$aj_t[tdata$wateryear == y & tdata$site == site.key[i]] <- aj.mean.temp
      tdata$nj_t[tdata$wateryear == y & tdata$site == site.key[i]] <- nj.mean.temp
      tdata$sum_t[tdata$wateryear == y & tdata$site == site.key[i]] <- sum.mean.temp
    }
  }
  
  #transform temperature dataframe for main model
  tdata.wide <- tdata[c(1:5)] %>% pivot_wider(names_from = sitenote, values_from = c("aj_t","nj_t", "sum_t"), names_glue = "{sitenote}.{.value}" )
  nj.snot <-snotel.nj_temp %>% dplyr::select(wateryear, nj_tempf, sitenote) %>% pivot_wider(names_from = sitenote, values_from = nj_tempf, names_glue = "{sitenote}.nj_t" )
  aj.snot<-snotel.aj_temp %>% dplyr::select(wateryear, aj_tempf, sitenote) %>% pivot_wider(names_from = sitenote, values_from = aj_tempf, names_glue = "{sitenote}.aj_t" )
  
  
  #compile data into one wide table
  all.temp.dat <- nj.snot %>% merge(aj.snot , by= 'wateryear', all=TRUE) %>% 
    merge(tdata.wide, by= 'wateryear', all=TRUE) 
  
  ##### add temp data to current_data
  
  current_data=merge(current_data,all.temp.dat, by='wateryear',all=F)
  
  #write.csv(all.temp.dat, file.path(data_dir,"temp_dat.csv"), row.names=FALSE)
  
  #-------------------------------------------------------------------------------
  # Bootstrap spring temperature predictions for each site
  #TODO need to test a better model - this one sucks
  #-------------------------------------------------------------------------------
  # the soldier ranger station has some missing years that cut the record short - consider removing
  input<- snotel.aj_temp %>% merge(elev, by="sitenote")  %>% dplyr::select(wateryear, aj_tempf, sitenote, elev) %>% filter(complete.cases(.))
  lr.elev<- lm(aj_tempf~  elev+wateryear+sitenote, data=input)
  summary(lr.elev)
  input$fitted<- predict(lr.elev)
  
  # covarience matrix (var-cov matrix) - diagonal the variance of temp at each site, the rest are the correlations
  sp.agri<- tdata.wide %>% dplyr :: select (wateryear, picabo.aj_t, fairfield.aj_t) 
  
  sp.temps <- aj.snot %>% merge(sp.agri, by= 'wateryear') %>% filter(complete.cases(.))
  
  site.cov<- cov(sp.temps[-1], use="complete.obs")
  # calculate the correlations
  r <- as.data.frame(round(cor(sp.temps[-1], use="complete.obs"),2))
  
  # data for prediction
  all_site.key<-c(as.character(unique(snotel.aj_temp$sitenote)), site.key)
  new.data<-data.frame(array(NA,c(length(all_site.key),4)))
  colnames(new.data)<-c("wateryear","sitenote", "elev", "aj_tempf")
  new.data$wateryear<-rep(last.yr+1,length(all_site.key))
  new.data$sitenote<-(all_site.key)
  new.data$elev<-elev$elev
  
  #predict the mean april-june temperature at each site
  new.data$aj_tempf[1:12]<-predict(lr.elev, new.data[1:12,])
  # use the mean of fairfield and picabo - has no trend and decreases strength of lm
  new.data$aj_tempf[14]<- mean(tdata.wide$fairfield.aj_t, na.rm = TRUE)
  new.data$aj_tempf[13]<- mean(tdata.wide$picabo.aj_t, na.rm = TRUE)
  
  # Draw stream temperatures using multivariate normal distribution
  nboot<-5000
  aj.pred.temps<- data.frame(mvrnorm(nboot, new.data$aj_tempf, site.cov) )
 
  
  # Data Integration
  # merge data from data scraping and temp models
  # Kendra Kaiser
  # 09-15-2023
  
  # Import & Compile Data -------------------------------------------------------# 
  # Streamflow, Current SWE, historic Temperature Data
  
  # input data name 'alldat_mo.csv'
  if (month(end_date) == 2){
    swe_q  <<- alldat_feb
  } else if (month(end_date) == 3){
    swe_q  <<- alldat_mar
  } else if (month(end_date) == 4){
    swe_q  <<- alldat_april
  }
  
  swe_q[swe_q == 0] <- 0.01 # change zeros to a value so lm works 
  
  # combine discharge & SWE with temp data
  var = swe_q %>% merge(all.temp.dat, by ="wateryear", all=TRUE)
  
  outputList=list(var=var,aj.pred.temps=aj.pred.temps,current_data=current_data)
  
  return(outputList)
}
