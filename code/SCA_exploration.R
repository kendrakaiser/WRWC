# ------------------------------------------------------------------------------
# Exploration of Snow Covered Area Data from Landsat 5
# Kendra Kaiser
# August 15, 2020
# ------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)

# set data directory for saving data
cd ='~/Desktop/Data/WRWC'

# Import Landsat Derived Snow Covered Area, reformat data into usable format
sca_file<- read.csv('~/Desktop/Data/WRWC/SnowCoveredArea_ts_84-90.csv', header = TRUE)
dm<- as.matrix(str_split(gsub("\\[|]", "", gsub("[{nd=\\d+}{pixel_qa}]", "", as.character(sca_file[,2]))), ", "))
sca<- as.data.frame(matrix(as.numeric(unlist(dm)), ncol=3, byrow = TRUE))
colnames(sca)<- c("sca", "date", "obs_pixels")
sca$date<- as.Date(as.character(sca$date), format="%Y%m%d")

# percent of watershed that has "visible" data
sca$obs_sqkm <- sca$obs_pixels*900/(1000*1000) 
sca$perc_obs <- sca$obs_sqkm/5483.55

sca$sca_sqkm <- sca$sca*900/(1000*1000) #convert to sqkm
sca$perc_sca <- sca$sca_sqkm/5483.55 #divide by area of ws to get percent sca

sca$perc_obs_sca <- sca$sca_sqkm/sca$obs_sqkm

sca_gt5 <- sca[sca$perc_obs > 0.5,]
## save a figure that shows YTD SWE over WY average and CV for each site

#Import Snotel Data
snotel<-read.csv(file.path(cd,'snotel_data.csv'))
snotel$date <- as.Date(as.character(snotel$date))

#join all snow data by date
snows <- left_join(sca, snotel, by = "date")

plot(snows$snow_water_equivalent[snows$perc_obs > .5], snows$perc_sca[snows$perc_obs > .5]*100, xlab="SWE (inches)", ylab="Snow Covered Area (%)", las=1)

##ggplot(data=snows,  )