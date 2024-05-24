# Groundwater analysis for IDWR BWGWMA Technical Committee
# Kendra Kaiser, May 8, 2024

# The goal here is to evaluate if/how baseflow calculations represent groundwater levels
# to see if it might be a useful metric for management purposes

# I'm starting with winter baseflows (Nov-Jan) as that reflects the overall 
# state of the basin after the irrigation season and prior to snowmelt

library(sf)
library(DBI)
source(file.path(git_dir, 'code/init_db.R')) 
#connect to database
conn=scdbConnect() 

git_dir=getwd()
#gw.idwr<- read_csv(file.path(git_dir, 'input/gw-idwr.csv')) 

#calculate baseflow values for the entire year
baseflow=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, datetime, metric, value AS flow, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'streamflow' AND qcstatus = 'true' 
                     ORDER BY datetime;")

bf_wrapper=function(wy, lid, wint_flow_df){
  thisQ=wint_flow_df[wint_flow_df$wateryear == wy & wint_flow_df$locationid == lid,"flow"]
  bf=baseflowA(thisQ)
  return(mean(bf$bf))
}

avgBaseflow=unique(baseflow[,c("wateryear","metric","locationid","name","sitenote")])

avgBaseflow$wq = mapply(bf_wrapper, wy=avgBaseflow$wateryear, lid=avgBaseflow$locationid, MoreArgs = list(wint_flow_df=baseflow))


# Pull GW data out of database
out<-dbGetQuery(conn,paste0("SELECT metric, value, datetime, data.locationid, locations.name FROM
                       data LEFT JOIN locations ON data.locationid = locations.locationid
                       WHERE data.metric IN ('depth to groundwater') AND data.qcstatus = 'true';"))
out$mo<- month(out$datetime)

#select only winter months for comparison to winter baseflow (11-1)
winter.gw <- out[out$mo>10 | out$mo <2,]
winter.gw$wateryear<- waterYear(winter.gw$datetime)

#average the flows across those months based on water year
wint.avg.gw <- winter.gw %>%
  group_by(name, wateryear) %>%
  summarise(average_value = mean(value))

# Transform WIDE
wint.avg.gwW<- wint.avg.gw %>%
  pivot_wider(names_from = name, values_from = average_value)


# SUBSET all the WRWC model variables to only winter baseflow (wq)
# this will only run after the model has been run through #04 which produces var
bq<-var[,c("wateryear", "cc.wq", "sc.wq", "bwh.wq", "bws.wq")]
wint.bq.gw<- merge(bq, wint.avg.gwW, by="wateryear")
irr_vol<-var[,c("wateryear", "cc.irr_vol", "sc.irr_vol", "bwh.irr_vol", "bws.irr_vol")]
sp.swe<- var[,c("wateryear","sp.swe")]

# Transform LONG
bq.long<- bq %>% pivot_longer(cols=c(2:5), names_to ="site",  values_to="wq") %>% mutate(site = sub("\\..*", "", site)) 
irr_vol.long<- irr_vol %>% pivot_longer(cols=c(2:5), names_to = "site", values_to="irr_vol") %>% mutate(site = sub("\\..*", "", site)) 


wint.bq.gw.long<- merge(bq.long, wint.avg.gw, by="wateryear")
vols_bq<- merge(wint.bq.gw.long, irr_vol.long, by=c("wateryear", "site"))
vols_bq<- merge(vols_bq, sp.swe, by=c("wateryear"))


# Wells since 1987
# c("IDWR-Greyhawk Village Well", "USGS-Labrador North Well", "USGS Stocker Creek Well", "USGS-Baseline Well")

sc.baseline<-vols_bq %>% filter(name %in% c("USGS-Baseline Well")) %>% filter(site %in% c("sc"))

sc.bq<-lm(sc.baseline$wq~sc.baseline$average_value) #Adj R2 86%

# comparison of baseflow, irrigation season volume and snowpack
ggplot(sc.baseline, aes(x = wq, y = average_value)) +
  geom_point() +
  labs(x = "Silver Creek Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Baseline Well")+
  theme_bw()+
  scale_y_reverse()+
  scale_size_continuous(name = "Irrigation Season Volume (KAF)")+
  geom_smooth(method='lm', formula= y~x)

# comparison of baseflow, irrigation season volume and snowpack
ggplot(sc.baseline, aes(x = wq, y = sp.swe, size=irr_vol)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Swede Peak SWE")+
  theme_bw()
  scale_size_continuous(name = "Irrigation Season Volume (KAF)")

# comparison of baseflow, irrigation season volume and snowpack
ggplot(sc.baseline, aes(x = wq, y = sp.swe, color = site, size=irr_vol)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Swede Peak SWE")+
  theme_bw()+
  scale_size_continuous(name = "Irrigation Season Volume (KAF)", range = c(1, 9), breaks = c(80000, 150000, 210000, 230000, 400000))

# Comparison of average winter groundwater levels and baseflows by site
ggplot(wint, aes(x = wq, y = average_value, color = site)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Baseline Well")+
  theme_bw()+
  scale_y_reverse()

simplot<-vols_bq %>% filter(name %in% c("USGS-Simplot Cutoff Rd South Well"))

ggplot(simplot, aes(x = wq, y = average_value, color = site)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Simplot Well")+
  theme_bw()+
  scale_y_reverse()


plot.bq <-vols_bq %>% filter(name %in% c("USGS-Labrador North Well"))
# Comparison of average winter groundwater levels and baseflows by site
ggplot(plot.bq, aes(x = wq, y = average_value, color = site)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Labrador North Well")+
  theme_bw()+
  scale_y_reverse()


plot.bq<-vols_bq %>% filter(name %in% c("USGS-Labrador South Well"))

# Comparison of average winter groundwater levels and baseflows by site
ggplot(plot.bq, aes(x = wq, y = average_value, color = site)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Labrador South Well")+
  theme_bw()+
  scale_y_reverse()



# Comparison of average winter groundwater levels and baseflows by site
ggplot(plot.bq, aes(x = wq, y = average_value, color = site)) +
  geom_point() +
  labs(x = "Average Winter Baseflow (cfs)", y = "Average Winter Depth to Groundwater (ft) \n USGS Labrador North Well")+
  theme_bw()+
  scale_y_reverse()

