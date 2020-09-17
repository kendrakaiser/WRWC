# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# September 19, 2020
# Creates Mixed-effects regression model to predict upcoming year temperature
# ----------------------------------------------------------------------------- # 

# load packages ----
library(ggplot2)
library(nlme) #need for lme 
library(MASS) #need for mvrnorm
library(plotrix) #need for CI
# import data ----
cd = '~/Desktop/Data/WRWC'
snotel = read.csv(file.path(cd,'snotel_data.csv'))
agrimet = read.csv(file.path(cd,'agri_met.csv'))

# Analyze and Predict temperature trend ----
# Starts in water year 1988 for common period of record.Camas comes in 1992 and chocolate gulch in 1993

first.yr<-1988
last.yr<-2017
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

#plot all data
ggplot(tdata, aes(x=year, y=Apr.Jun.tempF, color=site)) +geom_point()

# -----
# Linear mixed effects model
# ---

trend<-lme(fixed=Apr.Jun.tempF ~ (year), random=~1+(year)|site, correlation = corAR1(), data=tdata, method="ML",na.action=na.omit)
null<-lme(fixed=Apr.Jun.tempF~1, random=~1+year|site,correlation=corAR1(),data=tdata,method="ML",na.action=na.omit)
anova(null,trend) # p-Value is significant ( <.0001)

#m odel is fit by maximizing the restricted log-likelihood (ask vanKirk why*)
trend.reml<-lme(fixed=Apr.Jun.tempF ~ year, random=~1+year|site, correlation = corAR1(), data=tdata, method="REML",na.action=na.omit)
summary(trend.reml)

# generate data for prediction
new.data<-data.frame(array(NA,c(length(site.key),3)))
colnames(new.data)<-c("year","site","Apr.Jun.tempF")
new.data$year<-rep(last.yr+1,length(site.key))
new.data$site<-(site.key)

# predict this years temperature
pred<-predict(trend.reml,new.data,0:1)$predict.fixed[1]
fits<-fitted(trend.reml,0:1)[(1:nyrs),1]

# ----
# Bootstrap to estimate variance on new prediction, based on fixed-effects covariance matrix
# ---
mu<-trend.reml$coef$fixed
sig<-trend.reml$var

nboots<-2000
rand.coefs<-mvrnorm(nboots,mu,sig)

var.est<-var(rand.coefs%*%c(1,last.yr+1))
var.site<-var(summary(trend.reml)$coeff$random$site[,1])/length(site.key)

se.pred<-sqrt(var.est+var.site)
#bootstrap results
nboot<-5000
apr.jun.temps<-rnorm(nboot,mean=pred,sd=se.pred)

write.csv(apr.jun.temps,"rand.apr.jun.temp.csv",row.names=F)

# ----
# Plot results
# ___

png(filename = "Apr_Jun_ModTemp.png",
    width = 7.5, height = 5.5,units = "in", pointsize = 12,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    antialias="cleartype")

plot(tdata$year,tdata$Apr.Jun.tempF,type="n",xlab="Year",xlim=c(first.yr,(last.yr+1)),
     ylab="Temperature (F)",main="Mean April-June Temperature Big Wood Watershed SnoTel Sites",ylim=c(0,15))
abline(h=seq(0,20,5),col="gray85",lty=2)
abline(v=seq(1990,last.yr+1,5),col="gray85",lty=2)

for(i in 1:length(site.key)){
  lines(tdata$year[tdata$site==site.key[i]],
        tdata$Apr.Jun.tempF[tdata$site==site.key[i]],col="gray50",lty=2)
  abline(lm(tdata$Apr.Jun.tempF[tdata$site==site.key[i]]~tdata$year[tdata$site==site.key[i]]),
         col="gray50",lty=1)
}
lines(first.yr:(last.yr+1),c(fits,pred),lwd=3)
plotCI(last.yr+1,pred,pch=18,cex=2,col=1,uiw=2*se.pred,add=T,lwd=2)

legend("bottomright",bg="white",box.col=1, 
       lwd = c(1,1,3,2),pch=c(NA_integer_,NA_integer_,NA_integer_,18),pt.cex=c(0,0,0,2), col=c("gray50","gray50",1,1),lty=c(2,1,1,1),
       legend = c("Individual site data","Individual site trends","Watershed mean trend","2021 prediction & 95% CI"))

dev.off()
