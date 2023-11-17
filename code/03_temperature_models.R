# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# December 21, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# based on subsets of basins that preformed best in lm of center of mass etc
# ----------------------------------------------------------------------------- # 

# import data ----
#snotel_data = read.csv(file.path(data_dir,'snotel_data.csv'))
#agrimet = read.csv(file.path(data_dir,'agri_metT.csv'))

#renaming agrimet columns to match snotel for calculations
colnames(agrimet)<- c("date_time","temperature_mean", "site_name", "mo", "y", "wy")
agrimet$date_time<- as.Date(agrimet$date_time)
#remove values that are erroneous
agrimet$temperature_mean[agrimet$temperature_mean < -90] <- NA
agrimet$temperature_mean[agrimet$temperature_mean > 150] <- NA

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-pred.yr
nyrs<-last.yr-first.yr+1
site.key <- c(as.character(unique(snotel$site_name)), as.character(unique(agrimet$site_name)))
elev<-c(1923,1740,1750,2408,2566,2277,1999,2323,2408,2265,2676,2329,1536,1494)
#create a dataframe to store avg. Apr/Jun Temp for each site for period of record
tdata<-data.frame(array(NA,c(length(site.key)*nyrs,7)))

#summer temp july-sept, winter temp NDJFM
colnames(tdata)<-c("wateryear","site","spring.tempF", "sum.tempF", "wint.tempF", "nj.tempF", "nf.tempF")
tdata$wateryear<-rep(first.yr:last.yr,length(site.key))
tdata$site<-rep(site.key, each=nyrs)
tdata$elev<-rep(elev, each=nyrs)

#calculate average Snotel seasonal temperatures for every year
for(i in 1:14){ #hard coded this in after adding agrimet sites to site.key list
  for (y in first.yr:last.yr){
    #subset to indv. site and year
    if (i<13){
      sub<- snotel[snotel$site_name == site.key[i] & snotel$wy==y, ]
    } else if (i>=13){
      sub<- na.omit(agrimet[agrimet$site_name == site.key[i] & agrimet$wy==y, ])}
    #average april - june temps
    #if length is greater than 95% of the desired period calculate the mean
    if (length(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"]) > 88) {
        aj.mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=TRUE)
        } else (aj.mean.temp <- NA)
    #average summer temps July Aug Sept
    if (length(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"]) > 88) {
      sum.mean.temp <- mean(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"], na.rm=TRUE)
      } else (sum.mean.temp <- NA)
    #average winter temps
    if (length(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2 | sub$mo == 3, "temperature_mean"]) > 149) {
      wint.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2 | sub$mo ==3, "temperature_mean"], na.rm=TRUE)
      } else (wint.mean.temp <- NA)
    #average nov-jan temps
    if (length(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"]) > 88) {
       nj.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"], na.rm=TRUE)
        } else (nj.mean.temp <- NA)
    #average nov-feb temps
    if (length(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2, "temperature_mean"]) > 119) {
      nf.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2, "temperature_mean"], na.rm=TRUE)
      } else (nf.mean.temp <- NA)
    #average feb-march temps
    if (length(sub[sub$mo == 2 | sub$mo ==3, "temperature_mean"]) > 58) {
      fm.mean.temp <- mean(sub[sub$mo == 2 | sub$mo ==3, "temperature_mean"], na.rm=TRUE)
      } else (fm.mean.temp <- NA)
    
    #save to tdata table
    tdata$spring.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- aj.mean.temp #april-june
    tdata$sum.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- sum.mean.temp
    tdata$wint.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- wint.mean.temp
    tdata$nj.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- nj.mean.temp
    tdata$nf.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- nf.mean.temp
    tdata$fm.tempF[tdata$wateryear == y & tdata$site == site.key[i]] <- fm.mean.temp
  }
}

#transform temperature dataframe for main model
spring.tdata <-pivot_wider(tdata[,1:3], names_from = site, values_from = spring.tempF)
sum.tdata<-pivot_wider(tdata[,c(1,2,4)], names_from = site, values_from = sum.tempF)
wint.tdata<-pivot_wider(tdata[,c(1,2,5)], names_from = site, values_from = wint.tempF)
nj.tdata<-pivot_wider(tdata[,c(1,2,6)], names_from = site, values_from = nj.tempF)
nf.tdata<-pivot_wider(tdata[,c(1,2,7)], names_from = site, values_from = nf.tempF)
fm.tdata<-pivot_wider(tdata[,c(1,2,9)], names_from = site, values_from = fm.tempF)

#figure out a cleaner way to assign these
colnames(spring.tdata)<-c("wateryear", "aj_t.cg","aj_t.ccd", "aj_t.sr", "aj_t.bc","aj_t.ds","aj_t.g","aj_t.ga", "aj_t.hc", "aj_t.lw", "aj_t.sm", "aj_t.gs", "aj_t.sp","aj_t.p", "aj_t.f")
colnames(sum.tdata)<-c("wateryear", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")
colnames(wint.tdata)<-c("wateryear", "wint_t.cg","wint_t.ccd", "wint_t.sr", "wint_t.bc","wint_t.ds","wint_t.g","wint_t.ga", "wint_t.hc", "wint_t.lw", "wint_t.sm", "wint_t.gs", "wint_t.sp","wint_t.p", "wint_t.f")
colnames(nj.tdata)<-c("wateryear", "nj_t.cg","nj_t.ccd", "nj_t.sr", "nj_t.bc","nj_t.ds","nj_t.g","nj_t.ga", "nj_t.hc", "nj_t.lw", "nj_t.sm", "nj_t.gs", "nj_t.sp","nj_t.p", "nj_t.f")
colnames(nf.tdata)<-c("wateryear", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")
colnames(fm.tdata)<-c("wateryear", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")

spring.tdata[spring.tdata == "NaN"]<- NA
nj.tdata[nj.tdata== "NaN"]<- NA

write.csv(spring.tdata, file.path(data_dir, 'sprTemps.csv'), row.names=FALSE)
write.csv(sum.tdata, file.path(data_dir, 'sumTemps.csv'), row.names=FALSE)
write.csv(wint.tdata, file.path(data_dir, 'wintTemps.csv'), row.names=FALSE)
write.csv(nj.tdata, file.path(data_dir, 'njTemps.csv'), row.names=FALSE)
write.csv(nf.tdata, file.path(data_dir, 'nfTemps.csv'), row.names=FALSE)
write.csv(fm.tdata, file.path(data_dir, 'fmTemps.csv'), row.names=FALSE)


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


# have 500 predictions of the upcoming years temperature for each site -- make sure that the notation (aj.site) is consitent with the model selection
# linear regression using elevation alone
input <- tdata[tdata$site != "fairfield" & tdata$site != "picabo",] %>% filter(complete.cases(.))
lr.elev<- lm(spring.tempF~  elev+year, data=input)
#lr.elev<- lm(spring.tempF~  poly(elev,2) +year, data=input)
input$fitted<- predict(lr.elev)
#el <- as.data.frame(seq(from=1700, to=2700, by=30))
#y <- as.data.frame(seq(from=1988, to=2021, by=1))
#predicted_df <- data.frame(t_pred = predict(lr.elev, el, y, interval = 'confidence', level=0.95), elev=el, year=y)

summary(lr.elev)

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
