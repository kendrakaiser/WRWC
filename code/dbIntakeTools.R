
scdbConnect=function(readOnly=T){
  if(readOnly){
    conn=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbread",
                   password=Sys.getenv("scdb_readPass")
    )
  } else {
    conn=dbConnect(RPostgres::Postgres(),
                   host="silvercreekdb-do-user-12108041-0.b.db.ondigitalocean.com",
                   port="25060",
                   dbname="silvercreekdb" ,
                   user="dbwrite",
                   password=Sys.getenv("scdb_pass")
    )
  }
  return(conn)
}

escapeSingleQuote=function(df){
  df[]=lapply(df,function (X) {gsub( "'","''",X)})
  return(df)
}

writeLog=function(x,basePath='C:/Users/sam/Dropbox/NIFA Project/DB_Intake/',logFileName='_log.txt'){
  if(!file.exists(paste0(basePath,logFileName))){
    write(paste("Log created on",Sys.time()),file=paste0(basePath,logFileName),append=F)
  }
  write(paste(Sys.time(),">>",x),file=paste0(basePath,logFileName),append=T)
}

dbWriteBatch=function(batch,notes=" ",include=T,dbHandle=conn){
  if(!batch %in% dbGetQuery(dbHandle,"SELECT batchID FROM batches;")$batchid){# not an id, assume it's a name
    
    batchRecord=dbGetQuery(conn,paste0("SELECT source FROM batches WHERE batches.source = '",batch,"';")) # get records (if present) for this batch name
    if(nrow(batchRecord)==0){ #batch is not present in table, write new record
      datetime=Sys.time()
      dbExecute(dbHandle,paste0("INSERT INTO batches (source, importdatetime, notes, include) VALUES ('",
                                batch,"', '",
                                datetime,"', '",
                                notes,"', '",
                                include,"');"))
    }
    batchID=dbGetQuery(dbHandle,paste0("SELECT batchid FROM batches WHERE batches.source = '",batch,"';"))$batchid[1]
    
  } else{
    batchid=source
  }
  return(batchID)
}

dbWriteData=function(metric,value,datetime,locationID,sourceName,units="",isPrediction=F,simnumber=0,addMetric=F,dbHandle=conn){
  if(is.data.frame(value)){value=value[,1]} #strip extra attributes
  
  #first check batches, locations, and metrics
  
  ##############check/write batch:---------------
  batchID=dbWriteBatch(sourceName)
  
  
  ##############check for missing locations:-------------
  existingLocationIDs=dbGetQuery(dbHandle,"SELECT locationid FROM locations;")$locationid
  missingLocations=unique(locationID[!locationID %in% existingLocationIDs])
  if(length(missingLocations)>0){
    stop(paste("locationID(s)",missingLocations,"not present in locations table \n"))
  }
  
  
  ##########check/write metric:--------------
  metricDone=F
  metricIsNumeric=suppressWarnings( {!is.na(as.numeric(metric))} ) #is metric numeric?
  
  if(length(metric)>1){
    print(paste("metrics:",metric))
    stop("Only metric allowed per dbWriteData function call")
  }
  
  if (metricIsNumeric) {
    metricName=dbGetQuery(dbHandle,paste0("SELECT name FROM metrics WHERE metrics.metricid = '",metric,"';"))$name
    if(length(metricName)==1){ #found metric by id, all good
      metricID=metric 
      metricDone=T
    } else if(addMetric ){ #if metricIsNumeric and not already present, and addMetric=T, no good.
      stop(paste("Named metric and units required to add new metric record"))
    } 
    
  }
  
  if(!metricIsNumeric) {#if metric is not numeric
    #look for metric by name, get id
    metricID=dbGetQuery(dbHandle,paste0("SELECT metricid FROM metrics WHERE metrics.name = '",metric,"' AND metrics.isprediction = '",isPrediction,"';"))$metricid
    metricName=metric 
    if(length(metricID)==1){#got it!
      metricDone=T
    } else if(addMetric){#if no record of metric, and addMetric = T
      #check for units...
      if(units==""){ #if !metricIsNumeric and addMetric=T and no units given, no good
        stop(paste("Named metric and units required to add new metric record"))
      } 
      
      #go ahead and add it, and get the new id back
      dbExecute(dbHandle,paste0("INSERT INTO metrics (name, units, isprediction) VALUES ('",metric,"', '",
                                units,"', '",
                                isPrediction,"');"))
      metricID=dbGetQuery(dbHandle,paste0("SELECT metricid FROM metrics WHERE metrics.name = '",metric,"';"))$metricid
      metricDone=T
    }
    
  } 
  
  if(!metricDone){ #if the above lines did not define a metric
    print(paste("Metric", metric, "not found or added.  Known metrics:"))
    print(dbGetQuery(dbHandle,"SELECT * FROM metrics;"))
    stop("correct metric definition or set addMetric=T to add new metric")
  } 
  
  ########write data:--------------
  
  
  writeMe=data.frame(metric=metricName,value=value,datetime=datetime,metricid=metricID,locationid=locationID,batchid=batchID,simnumber=simnumber)
  writeMe$value=as.numeric(writeMe$value)
  writeMe=writeMe[complete.cases(writeMe$value),]
  
  potentialDups=dbGetQuery(conn, paste0("SELECT metricid, metric, value, datetime, locationid, simnumber FROM data WHERE metricid = '",metricID,
                          "' AND locationid IN ('",paste(locationID,collapse="', '"),"') AND  datetime IN ('",
                          paste(datetime,collapse="', '"),"');"))

  potentialDups=rbind(potentialDups,writeMe[,c("metricid","metric","value","datetime","locationid","simnumber")])

  moreDups=T
  while(moreDups){
    notDups=potentialDups[!duplicated(potentialDups) & !duplicated(potentialDups,fromLast = T),]
    if(nrow(notDups)==nrow(potentialDups)){
      moreDups=F
    } else {
      potentialDups=notDups
    }
  }

  if(nrow(notDups)>=1){
    writeMe=merge(notDups,writeMe)
    dbAppendTable(conn, name="data", value=writeMe)
    
  }
  #one at a time (for debug)
  #  for(i in 1:nrow(writeMe)){
  #    query=paste0("INSERT INTO data (metric, value, datetime, metricid, locationid, batchid) VALUES ('",
  #                 writeMe$metricName[i],"', '",
  #                 writeMe$value[i],"', '",
  #                 writeMe$datetime[i],"', '",
  #                 writeMe$metricid[i],"', '",
  #                 writeMe$locationid[i],"', '",
  #                 writeMe$batchid[i],"');")
  #    print(query)
  #    dbExecute(dbHandle,query)
  #  }
  
  #delete not unique:
  #slow as hell, replaced with while loop above
  #dbRemoveDuplicates(table="data",idCol = "dataid", uniqueCols = c("metric","value","datetime","metricid","locationid","simnumber"))
}

dbGetLocationID=function(source_site_id,sourceNote){
  location=dbGetQuery(conn,paste0("SELECT locationid, name, sitenote FROM locations WHERE locations.sourcenote='",sourceNote,"'
                                  AND locations.source_site_id = '",source_site_id,"';"))
  if(nrow(location)==1){
    return(location$locationid)
  } else {
    print("Location unclear.  Records found:")
    print(location)
    stop("Clarify or add location")
  }
}

parseIntakeFile=function(intakeFileName,metric='auto',basePath='C:/Users/sam/Dropbox/NIFA Project/DB_Intake/',siteIndexFile="_siteIndex.csv",logFileName='_log.txt'){
  
  skip=0
  done=F
  while(!done){
    
    rawFile=read.csv(paste0(basePath,intakeFileName),header=F,skip=skip)
    if( all( nchar( rawFile[1,] )>0 ) ){
      rawFile=read.csv(paste0(basePath,intakeFileName),header=T,skip=skip,blank.lines.skip = T)
      done=T
    } else { skip=skip+1 }
    
    if(skip>100){
      writeLog(paste('error reading',intakeFileName))
      done=T
      rawFile=""
    }
  }
  
  writeLog(paste('Read file',intakeFileName,'with names',paste(names(rawFile),collapse = ', ')))
  
  dateColIdx=which(grepl("Date",names(rawFile)))
  names(rawFile)[dateColIdx]="Date"
  
  siteIndex=read.csv(paste0(basePath,siteIndexFile))
  
  thisSite=paste0(siteIndex$metric[siteIndex$fileName==intakeFileName],"_",siteIndex$site[siteIndex$fileName==intakeFileName])
  
  formFile=data.frame(Date=as.POSIXlt(rawFile$Date,format="%m/%d/%y %I:%M:%S %p",tz="MST"),site=thisSite)
  
  if(metric=='auto'){
    
    if(sum(grepl("Temp",names(rawFile)))==1){
      tempColIDX=which(grepl("Temp",names(rawFile)))
      writeLog(paste("interpreted",names(rawFile)[tempColIDX],'as Water Temperature (c)'))
      names(rawFile)[tempColIDX]="WaterTemp"
      formFile$WaterTemp=rawFile[,tempColIDX]
    }
    
    if(sum(grepl("DO",names(rawFile)))==1){
      DOColIDX=which(grepl("DO",names(rawFile)))
      writeLog(paste("interpreted",names(rawFile)[DOColIDX],'as DO (mg/L)'))
      names(rawFile)[DOColIDX]="DO"
      formFile$DO=rawFile[DOColIDX]
      
    }
  }
  return(formFile)
}

dbWritePoints=function(writeDF,locationNameCol="Name",sourceNoteCol="",siteNoteCol="siteNote",source_siteIDCol="ID",srid=26911){ #requires an sf object
  #remove Z coord to simplify coordinate retrieval
  writeDF=st_zm(writeDF)
  
  cleanDF=data.frame(writeDF)#strip sf attribute
  cleanDF$geom=NULL
  
  #rename or create by name function
  renameCol=function(df,oldName,newName){
    if(oldName %in% names(df)){
      df$temp=df[,names(df)==oldName]
    } else {
      df$temp = oldName
    }
    names(df)[names(df)=="temp"] = newName
    return(df)
  }
  
  cleanDF=renameCol(cleanDF,locationNameCol,"name")
  cleanDF=renameCol(cleanDF,sourceNoteCol,"sourcenote")
  cleanDF=renameCol(cleanDF,siteNoteCol,"sitenote")
  cleanDF=renameCol(cleanDF,source_siteIDCol,"source_site_id")
  
  cleanDF=escapeSingleQuote(cleanDF)
  
  cleanDF$point="0 0"
  
  #works with a single line only, checks validity of each record before writing
  for (i in 1:nrow(cleanDF)){
    writeLine=cleanDF[i,]
    writeLine$point=paste0(st_coordinates(writeDF$geom[i])[1]," ",st_coordinates(writeDF$geom[i])[2])
    
    matchingRecordsByID=dbGetQuery(conn,paste0("SELECT * FROM locations WHERE source_site_id = '",writeLine$source_site_id,"';"))
    if(nrow(matchingRecordsByID>0)){
      print(">This record:")
      print(writeLine)
      print(">>Matching records found:")
      print(matchingRecordsByID)
      print(">>This record not added.")
      print("")
      print("")
    } else {
      
      # print(writeLine)
      
      query=paste0("INSERT INTO locations (name, geometry, sourcenote, sitenote, source_site_id) VALUES ('",
                   writeLine$name,"', ",
                   "'SRID=",srid,";POINT(",writeLine$point,")', '",
                   writeLine$sourcenote,"', '",
                   writeLine$sitenote,"', '",
                   writeLine$source_site_id,"');" )
      
      #print(query)
      
      dbExecute(conn,query)
      
    }
    
  }
  dbRemoveDuplicates(table="locations",idCol = "locationid", uniqueCols = c("name","geometry","sourcenote","source_site_id","sitenote"))
  #return(writeDF)
}

dbRemoveDuplicates=function(table, idCol, uniqueCols){
  
  rmID=dbGetQuery(conn, paste0("SELECT ",paste0("a.",idCol)," FROM ", table, " a, ", table, " b WHERE ",
                               paste0("a.",uniqueCols, " = b.", uniqueCols, collapse = " AND "), " AND a.",
                               idCol," > b.",idCol,";"))[,1]
  
  if(length(rmID)>0){
    
    dbExecute(conn, paste0("DELETE FROM ", table, " WHERE ", table, ".", idCol, " IN ('",paste0(rmID,collapse="', '"),"');"))
    
  }
  return(paste("Removed",length(rmID),"duplicate(s) from table: ", table))
  #return(rmID)
}

