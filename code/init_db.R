# run functions and load packages to connect to database

library(RPostgres) #db
#library(DBI)
library(sf)

scdbConnect=function(){
  conn=dbConnect(RPostgres::Postgres(),
                 host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                 port="25060",
                 dbname="silvercreekdb" ,
                 user="dbread",
                 password="dbread"
  )
  return(conn)
}

conn=scdbConnect()

