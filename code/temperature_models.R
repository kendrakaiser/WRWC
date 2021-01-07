# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# December 21, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# based on subsets of basins that preformed best in lm of center of mass etc
# ----------------------------------------------------------------------------- # 

# load packages ----
library(ggplot2)
library(nlme) #need for lme 
library(MASS) #need for mvrnorm
library(plotrix) #need for CI
library(tidyr)

# import data ----
#cd = '~/Desktop/Data/WRWC'
#fig_dir = '~/Desktop/Data/WRWC/figures' 
data_out = file.path(cd, 'data')

snotel = read.csv(file.path(data_out,'snotel_data.csv'))
agrimet = read.csv(file.path(data_out,'agri_metT.csv'))

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-2020
nyrs<-last.yr-first.yr+1
site.key <- c(as.character(unique(snotel$site_name)), as.character(unique(agrimet$site_name)))

#create a dataframe to store avg. Apr/Jun Temp for each site for period of record
tdata<-data.frame(array(NA,c(length(site.key)*nyrs,3)))
colnames(tdata)<-c("year","site","Apr.Jun.tempF")
tdata$year<-rep(first.yr:last.yr,length(site.key))
tdata$site<-rep(site.key,each=nyrs)

#calculate average Snotel april/jun temperature for every year
for(i in 1:10){ #hard coded this in after adding agrimet sites to site.key list
  for (y in first.yr:last.yr){
    sub<- snotel[snotel$site_name == site.key[i] & snotel$wy==y, ] #subset to indv. site and year
    #average april - june temps
    mean.temp <- mean(sub[sub$mo == 4 | sub$mo ==5 | sub$mo ==6 , "temperature_mean"], na.rm=T)
    #save to tdata table
    tdata$Apr.Jun.tempF[tdata$year == y & tdata$site == site.key[i]] <- mean.temp
  }
}
#calculate Agrimet average april/jun temperature for every year
for(i in 11:12){# these values could be ∆ to not be hard coded
  for (y in first.yr:last.yr){
    sub<- na.omit(agrimet[agrimet$site_name == site.key[i] & agrimet$y==y, ]) #subset to indv. site and year
    #average april - june temps
    mean.temp <- mean(sub[sub$month == 4 | sub$month ==5 | sub$month ==6 , "t"], na.rm=T)
    #save to tdata table
    tdata$Apr.Jun.tempF[tdata$year == y & tdata$site == site.key[i]] <- mean.temp
  }
}

#transform temperature dataframe for main model
tdata.wide <-pivot_wider(tdata, names_from = site, values_from = Apr.Jun.tempF)
colnames(tdata.wide)<-c("year", "t.cg","t.ccd", "t.sr", "t.ds","t.g","t.ga", "t.hc", "t.lw", "t.gs", "t.sp","t.p", "t.f")

snotel_abrv <- c("cg", "g", "gs", "hc", "lwd", "ds", "ccd", "sr", "ga", "sp")
write.csv(tdata.wide, file.path(data_out, 'ajTemps.csv'))

#plot all data
png(filename = file.path(fig_dir,"Temperatures.png"),
    width = 5.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, type ="quartz") 
ggplot(tdata, aes(x=year, y=Apr.Jun.tempF, color=site)) +geom_point()
dev.off()

# generate data for prediction
new.data<-data.frame(array(NA,c(length(site.key),3)))
colnames(new.data)<-c("year","site","Apr.Jun.tempF")
new.data$year<-rep(last.yr+1,length(site.key))
new.data$site<-(site.key)

nboots<-2000
nboot<-5000
# ssite combinations needed for multivariate models

# BWH --------------------------------------------------------------------------
# t.g +t.gs +t.lw
tdata.bwb<- tdata[tdata$site %in% c("galena","galena summit", "lost-wood divide"),]
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.bwb, method="REML",na.action=na.omit)
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
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.bws, method="REML",na.action=na.omit)
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
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.sc, method="REML",na.action=na.omit)
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
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.cc, method="REML",na.action=na.omit)
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
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata.scd, method="REML",na.action=na.omit)
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


aj.pred.temps<-cbind(aj.temps.bwh, aj.temps.bws, aj.temps.sc, aj.temps.cc, aj.temps.scd)
write.csv(aj.pred.temps, file.path(data_out, 'aj_pred.temps.csv'))


