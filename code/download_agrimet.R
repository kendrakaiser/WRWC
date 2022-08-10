# Download Agrimet Data separately
# Kendra Kaiser
# January 27, 2020
# ------------------------------------------------------------------------------

source(file.path(git_dir, 'code/grabAgriMetData.R'))

# Download NRCS ET Agrimet data ----
# OB = Air temperature
# PC = precipitation, cumulative (units?)
# SI = hourly solar radiation (units?)
# SQ = solar radiation, cumulative ??
# additional: soil temp, humidity, vapor pressure

# Download Fairfield Data ----------------------------------------
# start: 06/25/1987; site number: 3108

# create increments to download data
fiveYearDates=c(seq.Date(from=as.Date("1987-06-25"), to=as.Date(end_date), by="5 years"), end_date)
fafi = data.frame()

# download site data in ten year increments to prevent timing out server, will produce warning, that's okay
for (i in 2:length(fiveYearDates)){
  tempDF=getAgriMet.data(site_id="FAFI", timescale="hourly", DayBgn = fiveYearDates[i-1], DayEnd=fiveYearDates[i], 
                         pCodes=c("OB", "PC","SI", "SQ"))
  fafi=plyr::rbind.fill(fafi, tempDF)
  rm(tempDF)
}

# The following Warning message is okay : In (function (..., deparse.level = 1)  : 
# number of columns of result is not a multiple of vector length (arg 14042)

# rename columns
colnames(fafi)<- c("date_time", "fafi_t", "fafi_pc", "fafi_si", "fafi_sq")
# update format of dates
fafi$date_time<- as.POSIXct(fafi$date_time, format ='%m/%d/%Y %H:%M')
fafi$mo <- month(fafi$date_time)
fafi$y <- year(fafi$date_time)
# update format of values
for(i in 2:4) {
  fafi[, i]<- as.numeric(as.character(fafi[, i]))
}
fafi<-fafi[-1,] #remove first bad data value
fafiT<-fafi[,1:2]
fafiT[,3]<- 'fairfield'
fafiT<-cbind(fafiT, fafi[,6:7])
colnames(fafiT) <- c("date_time","t", "site_name", 'month', 'y')

# Download Picabo Data --------------------------------------
# start: 1982-06-01; site number: 7040; 
# Has SWE 1993-2002 and 2005-2017 - but can't be used predicatively since it is no longer available

# create increments to download data
fiveYearDates_p=c(seq.Date(from=as.Date("1982-06-01"), to=end_date, by="5 years"),end_date) 
pici = data.frame()

# download site data in ten year increments to prevent timing out server
for (i in 2:length(fiveYearDates_p)){
  tempDF=getAgriMet.data(site_id="PICI", timescale="hourly", DayBgn = fiveYearDates_p[i-1], DayEnd=fiveYearDates_p[i], pCodes=c("OB", "PC")) 
  pici=plyr::rbind.fill(pici, tempDF)
}
# rename columns 
colnames(pici)<- c("date_time", "pici_t", "pici_pc")
# update format of dates
pici$date_time<- as.POSIXct(pici$date_time, format ='%m/%d/%Y %H:%M')
pici$mo <- month(pici$date_time)
pici$y <- year(pici$date_time)
# update format of values
pici[, 2]<- as.numeric(as.character(pici[, 2]))
pici[, 3]<- as.numeric(as.character(pici[, 3]))
piciT<- pici [,1:2]
piciT[, 3]<-'picabo'
piciT<- cbind(piciT, pici[,4:5])
colnames(piciT)<- c("date_time","t", "site_name", 'month', 'y')

# Download Richmond Data - can't do this through AgriMet because it's Idaho Power Data
# start 3/27/2014 - 01-2020 daily temp; site number 7673
# ichi=getAgriMet.data(site_id="ICHI", timescale="hourly", DayBgn = "2014-01-01", DayEnd="2020-02-01", pCodes=c("OB", "PC"))

# Merge & save AgriMet Data ---------------------------------
agri_metT <- rbind(piciT, fafiT)
agri_metT$wy[!is.na(agri_metT$date_time)]<- as.numeric(as.character(waterYear(agri_metT$date_time[!is.na(agri_metT$date_time)], numeric=TRUE)))

agri_met<- full_join(pici, fafi, by='date_time')
agri_met$wy[!is.na(agri_met$date_time)]<- as.numeric(as.character(waterYear(agri_met$date_time[!is.na(agri_met$date_time)], numeric=TRUE)))

# saving to local directory
write.csv(agri_metT, file.path(data_dir,'agri_metT.csv'), row.names = FALSE)
