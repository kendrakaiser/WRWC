# Groundwater analysis for IDWR BWGWMA Technical Committee
# Kendra Kaiser, May 8, 2024

# The goal here is to evaluate if/how baseflow calculations represent groundwater levels
# to see if it might be a useful metric for management purposes

# I'm starting with winter baseflows (Nov-Jan) as that reflects the overall 
# state of the basin after the irrigation season and prior to snowmelt

library(sf)
library(DBI)

git_dir=getwd()
gw.idwr<- read_csv(file.path(git_dir, 'input/gw-idwr.csv')) 
# need to incorperate this below and/or get the USGS baseline/ picabo wells into the db


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

# Transform LONG
bq.long<- bq %>% pivot_longer(cols=c(2:5), names_to = "site", values_to="wq")
wint.bq.gw.long<- merge(bq.long, wint.avg.gw, by="wateryear")

#terrible figure
ggplot(wint.bq.gw.long, aes(x = wq, y = average_value, color = name)) +
  geom_point() +
  labs(x = "Baseflow", y = "Groundwater Level")

#TODO: make some alt figures that are actually useful



