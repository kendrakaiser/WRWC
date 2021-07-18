# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# December 21, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# based on subsets of basins that preformed best in lm of center of mass etc
# ----------------------------------------------------------------------------- # 

# import data ----
data_out = file.path(cd, 'data')

snotel = read.csv(file.path(data_out,'snotel_data.csv'))
agrimet = read.csv(file.path(input_dir,'agri_metT.csv'))

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-pred.yr
nyrs<-last.yr-first.yr+1
site.key <- c(as.character(unique(snotel$site_name)), as.character(unique(agrimet$site_name)))
elev<-c(1923,1740,1750,2408,2566,2277,1999,2323,2408,2265,2676,2329,1536,1494)
#create a dataframe to store avg. Apr/Jun Temp for each site for period of record
tdata<-data.frame(array(NA,c(length(site.key)*nyrs,6)))
#summer temp july-sept, winter temp NDJFM
colnames(tdata)<-c("year","site","spring.tempF", "sum.tempF", "wint.tempF", "nj.tempF")
tdata$year<-rep(first.yr:last.yr,length(site.key))
tdata$site<-rep(site.key, each=nyrs)
tdata$elev<-rep(elev, each=nyrs)

#calculate average Snotel april/jun temperature for every year
for(i in 1:12){ #hard coded this in after adding agrimet sites to site.key list
  for (y in first.yr:last.yr){
    sub<- snotel[snotel$site_name == site.key[i] & snotel$wy==y, ] #subset to indv. site and year
    #average april - june temps
    mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=T)
    #average summer temps July Aug Sept
    sum.mean.temp <- mean(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "temperature_mean"], na.rm=T)
    #average winter temps
    wint.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2 | sub$mo ==3 , "temperature_mean"], na.rm=T)
    #average nov-jan temps
    nj.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "temperature_mean"], na.rm=T)
    #save to tdata table
    tdata$spring.tempF[tdata$year == y & tdata$site == site.key[i]] <- mean.temp
    tdata$sum.tempF[tdata$year == y & tdata$site == site.key[i]] <- sum.mean.temp
    tdata$wint.tempF[tdata$year == y & tdata$site == site.key[i]] <- wint.mean.temp
    tdata$nj.tempF[tdata$year == y & tdata$site == site.key[i]] <- nj.mean.temp
  }
}
#calculate Agrimet average seasonal temperatures for every year
for(i in 13:14){# these values could be ∆ to not be hard coded
  for (y in first.yr:last.yr){
    sub<- na.omit(agrimet[agrimet$site_name == site.key[i] & agrimet$y==y, ]) #subset to indv. site and year
    #average april - june temps
    mean.temp <- mean(sub[sub$month == 4 | sub$month ==5 | sub$month ==6 , "t"], na.rm=T)
    #average summer temps July Aug Sept
    sum.mean.temp <- mean(sub[sub$mo == 7 | sub$mo ==8 | sub$mo ==9 , "t"], na.rm=T)
    #average winter temps
    wint.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1 | sub$mo ==2 | sub$mo ==3 , "t"], na.rm=T)
    #average nov-jan temps
    nj.mean.temp <- mean(sub[sub$mo == 11 | sub$mo ==12 | sub$mo ==1, "t"], na.rm=T)
    
    #save to tdata table
    tdata$spring.tempF[tdata$year == y & tdata$site == site.key[i]] <- mean.temp
    tdata$sum.tempF[tdata$year == y & tdata$site == site.key[i]] <- sum.mean.temp
    tdata$wint.tempF[tdata$year == y & tdata$site == site.key[i]] <- wint.mean.temp
    tdata$nj.tempF[tdata$year == y & tdata$site == site.key[i]] <- nj.mean.temp
  }
}

#transform temperature dataframe for main model
spring.tdata <-pivot_wider(tdata[,1:3], names_from = site, values_from = spring.tempF)
sum.tdata<-pivot_wider(tdata[,c(1,2,4)], names_from = site, values_from = sum.tempF)
wint.tdata<-pivot_wider(tdata[,c(1,2,5)], names_from = site, values_from = wint.tempF)
nj.tdata<-pivot_wider(tdata[,c(1,2,6)], names_from = site, values_from = nj.tempF)
colnames(spring.tdata)<-c("year", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")
colnames(sum.tdata)<-c("year", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")
colnames(wint.tdata)<-c("year", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")
colnames(nj.tdata)<-c("year", "t.cg","t.ccd", "t.sr", "t.bc","t.ds","t.g","t.ga", "t.hc", "t.lw", "t.sm", "t.gs", "t.sp","t.p", "t.f")


write.csv(spring.tdata, file.path(data_out, 'sprTemps.csv'))
write.csv(sum.tdata, file.path(data_out, 'sumTemps.csv'))
write.csv(wint.tdata, file.path(data_out, 'wintTemps.csv'))
write.csv(nj.tdata, file.path(data_out, 'njTemps.csv'))

snotel_abrv <- c("cg", "g", "gs", "hc", "lwd", "ds", "ccd", "sr", "ga", "sp")
#plot all data
png(filename = file.path(fig_dir,"SpringTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=year, y=spring.tempF, color=site)) +geom_point()
dev.off()

#plot all data
png(filename = file.path(fig_dir,"SummerTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=year, y=sum.tempF, color=site)) +geom_point()
dev.off()

png(filename = file.path(fig_dir,"WinterTemps.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
ggplot(tdata[tdata$site != "fairfield" & tdata$site != "picabo",], aes(x=year, y=wint.tempF, color=site)) +geom_point()
dev.off()

# generate data for prediction
new.data<-data.frame(array(NA,c(length(site.key),5)))
colnames(new.data)<-c("year","site", "wint.tempF", "spr.tempF","sum.tempF")
new.data$year<-rep(last.yr+1,length(site.key))
new.data$site<-(site.key)
new.data$elev<-elev
  
nboots<-2000
nboot<-5000

# All sites winter --------------------------------------------------------------------------
tdata.sno<- tdata[tdata$site != "fairfield" & tdata$site != "picabo",]
#not so sure about this .. come back to decide how it would be used (e.g. summer temp goes into irrigation est)
trend.reml<-lme(fixed=wint.tempF ~ year +elev, random=~1+year|site, correlation = corAR1(), data=tdata.sno, method="REML",na.action=na.omit)
summary(trend.reml)

# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]

# Bootstrap to estimate variance on new prediction, based on
# fixed-effects covariance matrix
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1, 2121))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site) #standard error
sum.temps.all<-rnorm(nboot,mean=pred,sd=se.pred)

plot(tdata.sno$year,tdata.sno$wint.tempF,xlab="Year",xlim=c(first.yr,(last.yr+1)),
     ylab="Temperature (F)",main="Mean Winter Temperature")
lines(first.yr:(last.yr+1),c(fits,pred),lwd=3)

# site combinations needed for multivariate models

# BWH --------------------------------------------------------------------------
# t.g +t.gs +t.lw
tdata.bwb<- tdata[tdata$site %in% c("galena","galena summit", "lost-wood divide"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.bwb, method="REML",na.action=na.omit)
summary(trend.reml)

# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]

# Bootstrap to estimate variance on new prediction, based on
# fixed-effects covariance matrix
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.bwh<-rnorm(nboot,mean=pred,sd=se.pred)

# BWS --------------------------------------------------------------------------
# t.cg+ t.g + t.hc+ t.lw
tdata.bws<- tdata[tdata$site %in% c("chocolate gulch","galena", "hyndman", "lost-wood divide"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.bws, method="REML",na.action=na.omit)
# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]
# Bootstrap 
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.bws<-rnorm(nboot,mean=pred,sd=se.pred)

# SC --------------------------------------------------------------------------
# t.cg+t.gs
tdata.sc<- tdata[tdata$site %in% c("chocolate gulch","galena summit"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.sc, method="REML",na.action=na.omit)
# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]
# Bootstrap 
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.sc<-rnorm(nboot,mean=pred,sd=se.pred)

# CC --------------------------------------------------------------------------
# t.f
tdata.cc<- tdata[tdata$site %in% c("fairfield", "picabo"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.cc, method="REML",na.action=na.omit)
# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]
# Bootstrap 
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.cc<-rnorm(nboot,mean=pred,sd=se.pred)

# SC Diversions ----------------------------------------------------------------
# t.cg, t.gs, t.hc
tdata.scd<- tdata[tdata$site %in% c("chocolate gulch","galena summit", "hyndman"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.scd, method="REML",na.action=na.omit)
# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]
# Bootstrap 
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.scd<-rnorm(nboot,mean=pred,sd=se.pred)


tdata.curt<- tdata[tdata$site %in% c("lost-wood divide" ,"galena summit", "galena", "swede peak"),]
trend.reml<-lme(fixed=spring.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.curt, method="REML",na.action=na.omit)

# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]
# Bootstrap 
mu<-trend.reml$coef$fixed
sig<-trend.reml$var
rand.coefs<-mvrnorm(nboots,mu,sig)
var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)
se.pred<-sqrt(var.est+var.site)
aj.temps.curt<-rnorm(nboot,mean=pred,sd=se.pred)




aj.pred.temps<-cbind(aj.temps.bwh, aj.temps.bws, aj.temps.sc, aj.temps.cc, aj.temps.scd, aj.temps.curt)
write.csv(aj.pred.temps, file.path(data_out, 'aj_pred.temps.csv'))


