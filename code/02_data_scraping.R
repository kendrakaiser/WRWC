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
conn=scdbConnect() 

# ------------------------------------------------------------------------------
# USGS Gages Data & Metrics
# calculate hydrologic metrics for each year for each station 
# winter "baseflow" (wb), Apr - Sept irrigation volume (irr_vol), total Volume (tot_vol), and center of mass (cm)
# ------------------------------------------------------------------------------
source(file.path(git_dir,'code/fxn_baseflowA.r'))
#Average Winter Flow - doesnt work properly bc needs filtered, retaining so code doesnt break
# avgBaseflow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, AVG(value) AS wq, data.locationid, name, sitenote
#            FROM data LEFT JOIN locations ON data.locationid = locations.locationid
#            WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2) 
#            GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

#pull winter flow to do filter on
wint_flow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, datetime, metric, value AS flow, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 11 OR EXTRACT(month FROM datetime) < 2)
                     ORDER BY datetime;")

bf_wrapper=function(wy, lid, wint_flow_df){
  thisQ=wint_flow_df[wint_flow_df$wateryear == wy & wint_flow_df$locationid == lid,"flow"]
  bf=baseflowA(thisQ)
  return(mean(bf$bf))
}

avgBaseflow=unique(wint_flow[,c("wateryear","metric","locationid","name","sitenote")])

avgBaseflow$wq = mapply(bf_wrapper, wy=avgBaseflow$wateryear, lid=avgBaseflow$locationid, MoreArgs = list(wint_flow_df=wint_flow))

# pivot data wider
baseflow<-pivot_wider(data=avgBaseflow[,c("wateryear","wq","sitenote")],names_from = c(sitenote),values_from = c(wq), names_glue = "{sitenote}.wq")

#Total Irrigation Season April-September streamflow in AF [1.98 #convert from cfs to ac-ft]
irr_AF=dbGetQuery(conn," SELECT * FROM (SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS irr_vol, data.locationid, name, sitenote, COUNT(DISTINCT( dataid)) AS days_in_record
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 10)
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear) as histvols WHERE days_in_record > 180;")  # complete record is 183 days

#TODO  DO WE NEED A SIMILAR CATCH FOR cases (current year) where tot_af and cm_dat return an incomplete dataset?

# pivot data wider
irr_vol<-pivot_wider(data=irr_AF[,c("wateryear","irr_vol","sitenote")],names_from = c(sitenote),values_from = c(irr_vol), names_glue = "{sitenote}.irr_vol")

#Total Water Year volume in AF [1.98 #convert from cfs to ac-ft]
tot_AF=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS tot_vol, data.locationid, name, sitenote
          FROM data LEFT JOIN locations ON data.locationid = locations.locationid
          WHERE metric = 'streamflow' AND qcstatus = 'true'
          GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

tot_AF$wy1<-tot_AF$wateryear-1
totAF<- merge(tot_AF, tot_AF[,c('wateryear', "tot_vol", 'locationid', 'metric')], by.x=c('wy1', 'locationid', 'metric'), 
              by.y=c('wateryear','locationid', 'metric'), suffixes = c('', '_ly')) %>% dplyr::rename(ly_vol = tot_vol_ly)

# pivot data wider
tot_vol<-pivot_wider(data=totAF[,c("wateryear","tot_vol","ly_vol","sitenote")], names_from = c(sitenote), values_from = c(tot_vol, ly_vol), names_glue = "{sitenote}.{.value}")

#CENTER of MASS between April 1 and July 31
cm_dat=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, SUM(value)*1.98 AS flow, data.locationid, name, sitenote,  EXTRACT(doy FROM datetime) AS doy 
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' AND (EXTRACT(month FROM datetime) >= 4 AND EXTRACT(month FROM datetime) < 8) 
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote, doy) ORDER BY wateryear;")

years<- min(cm_dat$wateryear):max(cm_dat$wateryear)
cm<-data.frame(matrix(ncol = 3, nrow= length(years)*4)) %>% `colnames<-`(c('wateryear', 'sitenote', 'cm'))
cm$wateryear<- rep(years, times=4)
cm$sitenote<- rep(unique(cm_dat$sitenote), each=length(years))

for(sitename in unique(cm$sitenote)){
  sub <- cm_dat %>% filter(sitenote == sitename)
  for (wy in unique(cm$wateryear)){
    sub2 <- sub %>% filter(wateryear == wy)
    ix= which(cm$wateryear== wy & cm$sitenote == sitename) 
    cm$cm[ix]<- sum(sub2$doy * sub2$flow)/sum(sub2$flow)
  }}

cm_wide<- cm %>% pivot_wider(names_from = sitenote, values_from = cm, names_glue = "{sitenote}.cm" )

print('Streamflow Metrics Complete')

# ------------------------------------------------------------------------------
# Retrieve Snotel Data 
# ------------------------------------------------------------------------------
#FEBRUARY
winterSWE_feb=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 2)
           AND metric = 'swe' AND qcstatus = 'true'
           ORDER BY wateryear;")
# pivot data wider
swe_feb<-pivot_wider(data=winterSWE_feb[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")

#March
winterSWE_mar=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 3)
           AND metric = 'swe' AND qcstatus = 'true'
           ORDER BY wateryear;")
# pivot data wider
swe_mar<-pivot_wider(data=winterSWE_mar[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")

#APRIL
winterSWE_apr=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, value AS swe, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid WHERE 
           (EXTRACT(day FROM datetime) = 1 AND EXTRACT(month FROM datetime) = 4)
           AND metric = 'swe' AND qcstatus = 'true'
           ORDER BY wateryear;")
# pivot data wider
swe_apr<-pivot_wider(data=winterSWE_apr[,c("wateryear","metric","swe","sitenote")],names_from = c(sitenote, metric),values_from = c(swe),names_sep=".")

#TODO - set this to query the SWE based on 
#Grab Todays SWE
todaySWE_=dbGetQuery(conn,"SELECT DISTINCT ON (locationid) datetime, wateryear(datetime) AS wateryear, metric AS swe, value, locations.locationid, locations.name 
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'swe' AND qcstatus = true ORDER BY locationid, datetime DESC;")

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
winterSums_feb=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 2
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_feb<-pivot_wider(data=winterSums_feb[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#MARCH
winterSums_mar=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 3
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_march<-pivot_wider(data=winterSums_mar[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#APRIL
winterSums_apr=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, sum(value) AS winterSum, snodasdata.locationid, name, sitenote
           FROM snodasdata LEFT JOIN locations ON snodasdata.locationid = locations.locationid WHERE EXTRACT(month FROM datetime) >= 10 OR EXTRACT(month FROM datetime) < 4
           GROUP BY(wateryear, snodasdata.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")
# pivot data wider
snodas_april<-pivot_wider(data=winterSums_apr[,c("wateryear","metric","wintersum","sitenote")],names_from = c(sitenote, metric),values_from = c(wintersum),names_sep=".")

#TODO - set this to query the SNODAS for specific simulation date

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

print("All streamflow and snow data saved")
