# ------------------------------------------------------------------------------
# Exploration of Snow Covered Area Data from Landsat 5
# Kendra Kaiser
# August 15, 2020
# ------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

# set data directory for saving data
cd ='~/Desktop/Data/WRWC'

# Import Landsat Derived Snow Covered Area, reformat data into usable format
sca_file<- read.csv('~/Desktop/Data/WRWC/SnowCoveredArea_ts_84-13.csv', header = TRUE)
dm<- as.matrix(str_split(gsub("\\[|]", "", gsub("[{nd=\\d+}{pixel_qa}]", "", as.character(sca_file[,2]))), ", "))
sca<- as.data.frame(matrix(as.numeric(unlist(dm)), ncol=3, byrow = TRUE))
colnames(sca)<- c("sca", "date", "obs_pixels")
sca$date<- as.Date(as.character(sca$date), format="%Y%m%d")
sca$month <-month(sca$date)
# percent of watershed that has "visible" data
sca$obs_sqkm <- sca$obs_pixels*900/(1000*1000) 
sca$perc_obs <- sca$obs_sqkm/5483.936

sca$sca_sqkm <- sca$sca*900/(1000*1000) #convert to sqkm
sca$perc_sca <- sca$sca_sqkm/5483.936 #divide by area of ws to get percent sca

# this doesn't really work as a metric of the amount of data because of combining "clear" with "snow" pixles making the total snow pixels higher than the "observed" pixels; I can update the GEE code, and the observed will be quite similar to the percent snow 
sca$perc_obs_sca <- sca$obs_sqkm/sca$sca_sqkm

sca_gt5 <- sca[sca$perc_obs >= 0.5,]
## save a figure that shows YTD SWE over WY average and CV for each site

#Import Snotel Data
snotel<-read.csv(file.path(cd,'snotel_data.csv'))
snotel$date <- as.Date(as.character(snotel$date))

#join all snow data by date
snows <- left_join(sca, snotel, by = "date")
snows_gt5 = snows[snows$perc_obs >= .5, ]

plot(snows$snow_water_equivalent[snows$perc_obs > .5], snows$perc_sca[snows$perc_obs > .5]*100, xlab="SWE (inches)", ylab="Snow Covered Area (%)", las=1)


ggplot(sca_gt5, aes(x=month)) + geom_histogram(color="black", binwidth = 1)
ggplot(sca, aes(x=month)) + geom_histogram(color="black", binwidth = 1)
snows_gt5 = snows[snows$perc_obs >= .5, ]
snows_swe0 <- snows_gt5[snows_gt5$snow_water_equivalent == 0,]
uniqeSWE0SCAgt5<-unique(snows_swe0$date)

snows_swe <- snows_gt5[snows_gt5$snow_water_equivalent > 0,]
uniqeSWESCAgt5<-unique(snows_swe$date)
uniqeDate<-unique(snows$date)

custom.col <- c("#999999","#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C","#56B4E9", "#4E84C4", "#293352")

ggplot(data=snows_gt5, aes(x=snow_water_equivalent, y=perc_sca*100, color=site_name, size=perc_obs))+geom_point()+xlab("SWE (in.)") + ylab("Snow Covered Area (%)") + scale_color_manual(values = custom.col)

ggplot(data=snows, aes(x=snow_water_equivalent, y=perc_sca*100, color=site_name, size=perc_obs))+geom_point()+xlab("SWE (in.)") + ylab("Snow Covered Area (%)") + scale_color_manual(values = custom.col)
    