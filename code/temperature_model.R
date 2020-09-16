# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# September 19, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# ----------------------------------------------------------------------------- # 

# load packages ----

# import data ----
cd = '~/Desktop/Data/WRWC'
snotel = read.csv(file.path(cd,'snotel_data.csv'))
agrimet = read.csv(file.path(cd,'agri_met.csv'))

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-2020
nyrs<-last.yr-first.yr+1
site.key = as.character(unique(snotel$site_name))

#create a dataframe to store avg. Apr/Jun Temp for each site for period of record
tdata<-data.frame(array(NA,c(length(site.key)*nyrs,3)))
colnames(tdata)<-c("year","site","Apr.Jun.tempF")
tdata$year<-rep(first.yr:last.yr,length(site.key))
tdata$site<-rep(site.key,each=nyrs)

#calculate average april/jun temperature for every year
for(i in 1:length(site.key)){
  for (y in first.yr:last.yr){
    sub<- snotel[snotel$site_name == site.key[i] & snotel$wy==y, ] #subset to indv. site and year
    #average april - june temps
    mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=T)
    #save to tdata table
    tdata$Apr.Jun.tempF[tdata$year == y & tdata$site == site.key[i]] <- mean.temp
  }
}

