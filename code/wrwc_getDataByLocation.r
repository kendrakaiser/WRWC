library(sf)
library(DBI)

#location attributes, note the 'metrics' column.  You can also see all the locations in the app map.
locationAttributes=st_read(conn,query = "SELECT locationid, name, metrics, locationgeometry FROM locationattributes;")

#find sites within 'distance' meters of site 'targetLID' (locationID)
distance=1000 #meters
targetLID=10 #location id

xy=st_coordinates(locationAttributes[locationAttributes$locationid==targetLID,])
nearLocations=dbGetQuery(conn,sqlInterpolate(conn,"SELECT * FROM locations WHERE ST_DWITHIN(locations.geometry, ST_POINT(?x,?y, 26911),?dist);",
                                             x=xy[1],y=xy[2],dist=distance))

out<-dbGetQuery(conn,paste0("SELECT metric, value, datetime, data.locationid, locations.name FROM
                       data LEFT JOIN locations ON data.locationid = locations.locationid
                       WHERE data.locationid IN ('",
                       paste(nearLocations$locationid,collapse="', '"),
                       "') AND data.metric IN ('flow','depth to groundwater') AND data.qcstatus = 'true';"))
