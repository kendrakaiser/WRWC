# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# December 21, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# based on subsets of basins that preformed best in lm of center of mass etc
# ----------------------------------------------------------------------------- # 

# import data ----
#agrimet = read.csv(file.path(data_dir,'agri_metT.csv'))

#connect to database
conn=scdbConnect() 

# Pull AGRIMET Data from database
# -----------------------------------------------------------------------------
# TODO: automate pulling the averaged agrimet data from db - test code aint workin
agrimet<- dbGetQuery(conn, "SELECT * FROM daily_air_temperature;")

#agrimet_nj<- dbGetQuery(conn, "SELECT wateryear(datetime) AS wateryear, metric, avg(value) AS nj_tempF 
 #   FROM data LEFT JOIN locations ON data.locationid = locations.locationid
  #  WHERE metric = 'temperature mean' AND qcstatus = 'true' AND 
   # (EXTRACT(month FROM datetime) >= 11 OR EXTRACT(month FROM datetime) < 2)
    #GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

# saving to local directory
write.csv(agrimet, file.path('~/agri_metT.csv'), row.names = FALSE)

#renaming agrimet columns to match snotel for calculations
colnames(agrimet)<- c("date_time","temperature_mean", "site_name", "mo", "y", "wy")
agrimet$date_time<- as.Date(agrimet$date_time)
#remove values that are erroneous
agrimet$temperature_mean[agrimet$temperature_mean < -90] <- NA
agrimet$temperature_mean[agrimet$temperature_mean > 150] <- NA

#TODO NEED to add IF statement that reqiures the record length to be at least 88 
#Mean Nov- Jan Temp
snotel.nj_temp=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, avg(value) AS nj_tempF, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'mean daily temperature' AND qcstatus = 'true' AND 
           (EXTRACT(month FROM datetime) >= 11 OR EXTRACT(month FROM datetime) < 2)
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

snotel.aj_temp=dbGetQuery(conn,"SELECT wateryear(datetime) AS wateryear, metric, avg(value) AS aj_tempF, data.locationid, name, sitenote
           FROM data LEFT JOIN locations ON data.locationid = locations.locationid
           WHERE metric = 'mean daily temperature' AND qcstatus = 'true' AND 
           (EXTRACT(month FROM datetime) >= 4 OR EXTRACT(month FROM datetime) <= 6)
           GROUP BY(wateryear, data.locationid, metric, locations.name, locations.sitenote) ORDER BY wateryear;")

#TODO make a SQL query for agrimet data so that its all streamlined??

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-pred.yr
nyrs<-last.yr-first.yr+1
site.key <- c(as.character(unique(agrimet$site_name))) #as.character(unique(snotel.aj_temp$sitenote)), 
#create a dataframe to store avg. Apr/Jun Temp for each site for period of record
tdata<-data.frame(array(NA,c(length(site.key)*nyrs,5)))

#TODO: Need to check the elevations to confirm correct
elev<- structure(list(sitenote = c("lwd", "bc", "sr", "g", "gs", "hc", "ds", "ga", "sp", 
                "sm", "ccd", "cg", "picabo", "fairfield"), elev = c(1923L, 1740L, 1750L, 2408L, 
                2566L, 2277L, 1999L, 2323L, 2408L, 2265L,2676L, 2329L, 1536L, 1494L)), 
               class = "data.frame", row.names = c(NA, 
                                                                                                                                                       -14L))
#tdata$elev<-rep(elev$elev, each=nyrs)

#summer temp july-sept, winter temp NDJFM
colnames(tdata)<-c("wateryear","sitenote","aj_t","nj_t", "sum_t")
tdata$wateryear<-rep(first.yr:last.yr,length(site.key))
tdata$sitenote<-rep(site.key, each=nyrs)

#calculate average agrimet seasonal temperatures for every year
for(i in 1:2){ 
  for (y in first.yr:last.yr){
    #subset to indv. site and year
      sub<- na.omit(agrimet[agrimet$site_name == site.key[i] & agrimet$wy==y,])
    #average april - june temps
    #if length is greater than 95% of the desired period calculate the mean
    if (length(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"]) > 88) {
        aj.mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=TRUE)
        } else (aj.mean.temp <- NA)
    #average summer temps July Aug Sept
    if (length(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"]) > 88) {
      sum.mean.temp <- mean(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"], na.rm=TRUE)
      } else (sum.mean.temp <- NA)
    #average nov-jan temps
    if (length(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"]) > 88) {
       nj.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"], na.rm=TRUE)
        } else (nj.mean.temp <- NA)
  
    #save to tdata table
    tdata$aj_t[tdata$wateryear == y & tdata$site == site.key[i]] <- aj.mean.temp
    tdata$nj_t[tdata$wateryear == y & tdata$site == site.key[i]] <- nj.mean.temp
    tdata$sum_t[tdata$wateryear == y & tdata$site == site.key[i]] <- sum.mean.temp
  }
}

#transform temperature dataframe for main model
tdata.wide <- tdata[c(1:5)] %>% pivot_wider(names_from = sitenote, values_from = c("aj_t","nj_t", "sum_t"), names_glue = "{sitenote}.{.value}" )
nj.snot <-snotel.nj_temp %>% dplyr::select(wateryear, nj_tempf, sitenote) %>% pivot_wider(names_from = sitenote, values_from = nj_tempf, names_glue = "{sitenote}.nj_t" )
aj.snot<-snotel.aj_temp %>% dplyr::select(wateryear, aj_tempf, sitenote) %>% pivot_wider(names_from = sitenote, values_from = aj_tempf, names_glue = "{sitenote}.aj_t" )


#compile data into one wide table
all.temp.dat <- aj.snot %>% merge(nj.snot , by= 'wateryear') %>% 
  merge(tdata.wide, by= 'wateryear') 

write.csv(all.temp.dat, file.path(data_dir,"temp_dat.csv"), row.names=FALSE)

#---------------
#TODO: All of these figures will have to be updated using either new format
#from above if static or db format if we are going to have them on the website

#plot all data
png(filename = file.path(fig_dir,"SpringTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()
dev.off()

ggplot(tdata[tdata$site == "fairfield" | tdata$site == "picabo",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()
ggplot(tdata[tdata$site == "camas creek divide" | tdata$site == "chocolate gulch",], aes(x=wateryear, y=spring.tempF, color=site)) +geom_point()

#plot all data
png(filename = file.path(fig_dir,"SummerTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=sum.tempF, color=site)) +geom_point()
dev.off()

wt<-ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=wateryear, y=wint.tempF, color=site)) +geom_point()

png(filename = file.path(fig_dir,"WinterTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(wt)
dev.off()


subT<-tdata[tdata$site != "fairfield" & tdata$site != "picabo",]

wt_box<- ggplot(subT%>% filter(wateryear < pred.yr), aes(x=reorder(factor(site), nj.tempF, na.rm = TRUE), y=nj.tempF))+
  geom_boxplot(alpha=0.8)+
  theme_bw()+
  xlab("Snotel Site")+
  ylab("Average Nov-Jan Temperature (F)")+
  geom_point(data = subT %>% filter(wateryear == pred.yr), aes(reorder(factor(site), nj.tempF, na.rm = TRUE), y=nj.tempF), color="blue", size=3, shape=15)+
  coord_flip()

png(filename = file.path(fig_dir,"NovJanT_box.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 
print(wt_box)
dev.off()
#---------------

#-------------------------------------------------------------------------------
# Bootstrap spring temperature predictions for each site
#TODO need to test a better model
#-------------------------------------------------------------------------------
# the soldier ranger station has some missing years that cut the record short - consider removing
input<- snotel.aj_temp %>% merge(elev, by="sitenote")  %>% dplyr::select(wateryear, aj_tempf, sitenote, elev) %>% filter(complete.cases(.))
lr.elev<- lm(aj_tempf~  elev+wateryear+sitenote, data=input)
summary(lr.elev)

input$fitted<- predict(lr.elev)
#lr.elev<- lm(spring.tempF~  poly(elev,2) +year, data=input)
#el <- as.data.frame(seq(from=1700, to=2700, by=30))
#y <- as.data.frame(seq(from=1988, to=2021, by=1))
#predicted_df <- data.frame(t_pred = predict(lr.elev, el, y, interval = 'confidence', level=0.95), elev=el, year=y)


# TODO: make a plot that represents the model better
#plot the observed versus fitted 
png(filename = file.path(fig_dir,"ModeledTemps.png"),
    width = 5.5, height = 3.5,units = "in", pointsize = 12,
    bg = "white", res = 600) 

ggplot(input, aes(x=spring.tempF, y=fitted, color=site)) + 
  geom_abline(intercept=0,lty=1)+
  geom_point()+
  xlim(2, 11.5)+
  ylim(2,11.5) +
  xlab('Observed Mean April - June Temperature (F)') +
  ylab('Predicted Mean April - June Temperature (F)') +
  theme_bw()
  #+geom_line(color='red', data = predicted_df, aes(y=kwh_pred.fit/1000, x=MonthlyQ/1000))
dev.off()
  
ggplot(input, aes(x=spring.tempF, y=fitted)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  xlim(2, 11.5)+
  ylim(2,11.5) +
  xlab('Observed Mean April - June Temperature (F)') +
  ylab('Predicted Mean April - June Temperature (F)')+
  theme_bw()

# polynomial fit - come back to later
#lr.elev<-lm(spring.tempF~  poly(elev,2) +year, data=input) #polynomial helps 
#ix <- sort(input$spring.tempF,index.return=T)$ix
#geom_smooth(method="lm", formula=y~x+(x^2))
#stat_smooth(method = lm,  data = input, formula = y ~ poly(x, 2) + z)
#adding in fairfield and picabo throws off the regression - it effectively does poorly everywhere

# covarience matrix (var-cov matrix) - diagonal the variance of temp at each site, the rest are the correlations
site.cov<- cov(spring.tdata[-1], use="complete.obs")
# calculate the correlations
r <- as.data.frame(round(cor(spring.tdata[-1], use="complete.obs"),2))

# data for prediction
new.data<-data.frame(array(NA,c(length(site.key),4)))
colnames(new.data)<-c("wateryear","site", "elev", "spr.tempF")
new.data$wateryear<-rep(last.yr+1,length(site.key))
new.data$site<-(site.key)
new.data$elev<-elev

#predict the mean april-june temperature at each site
new.data$spr.tempF[1:12]<-predict(lr.elev, new.data[1:12,])
# use the mean of fairfield and picabo - has no trend and decreases strength of lm
new.data$spr.tempF[13]<- mean(tdata$spring.tempF[tdata$site == "fairfield"], na.rm = TRUE)
new.data$spr.tempF[14]<- mean(tdata$spring.tempF[tdata$site == "picabo"], na.rm = TRUE)

# Draw stream temperatures using multivariate normal distribution
nboot<-5000
aj.pred.temps<- mvrnorm(nboot, new.data$spr.tempF, site.cov)
write.csv(aj.pred.temps, file.path(data_dir, 'aj_pred.temps.csv'), row.names=FALSE)




