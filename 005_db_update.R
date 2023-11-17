#db updater

dbExecute(conn,"CREATE OR REPLACE FUNCTION wateryear(datetime timestamp without time zone) RETURNS integer AS $$
              SELECT CASE WHEN ( EXTRACT(month FROM datetime)) >= 10  THEN EXTRACT(year FROM datetime) +1 
                                      ELSE EXTRACT(year FROM datetime) 
                        END
                
                $$
            LANGUAGE SQL;")


# snodas=rbind(grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="snow_covered_area"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="liquid_precip"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="swe_total"),
#              grab_ws_snow(ws_ids=c(140,167,144,141),dates=seq.Date(from=as.Date("2003-09-30"),to=Sys.Date(),by="day"),metric="runoff_total")
# )
# 
