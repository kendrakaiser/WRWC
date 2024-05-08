# Groundwater analysis for IDWR BWGW Technical Committee

library(sf)
library(DBI)



# GW analysis for IDWR Tech 
out<-dbGetQuery(conn,paste0("SELECT metric, value, datetime, data.locationid, locations.name FROM
                       data LEFT JOIN locations ON data.locationid = locations.locationid
                       WHERE data.metric IN ('depth to groundwater') AND data.qcstatus = 'true';"))
out$mo<- month(out$datetime)

winter.gw <- out[out$mo>10 | out$mo <2,]
winter.gw$wateryear<- waterYear(winter.gw$datetime)

wint.avg.gw <- winter.gw %>%
  group_by(name, wateryear) %>%
  summarise(average_value = mean(value))

wint.avg.gwW<- wint.avg.gw %>%
  pivot_wider(names_from = name, values_from = average_value)

bq<-var[,c("wateryear", "cc.wq", "sc.wq", "bwh.wq", "bws.wq")]
wint.bq.gw<- merge(bq, wint.avg.gwW, by="wateryear")