# Data Download and figure generation for Blaine Co. Flood Risk
# May 27, 2021

library(tidyverse)
library(dataRetrieval)
library(hydrostats)

bwh_data <- readNWISdv(siteNumbers = 13139510, parameterCd = "00060", startDate = "1915-06-30", endDate = "2021-05-26") %>% renameNWISColumns() %>% data.frame
annual_peak<-readNWISpeak(13139510)
Q=bwh_data$Flow
n = length(Q)
r = n + 1 - rank(Q)  # highest Q has rank r = 1
T = (n + 1)/r

# Set up x axis tick positions and labels
Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100)
y = -log(-log(1 - 1/T))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)

# Fit a line by method of moments, along with 95% confidence intervals
KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
QTtick = mean(Q) + KTtick*sd(Q) 
nQ = length(Q)
se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
LB = QTtick - qt(0.975, nQ - 1)*se
UB = QTtick + qt(0.975, nQ - 1)*se
max = max(UB)
Qmax = max(QTtick)

# Plot peak flow series with Gumbel axis
plot(y, Q,
     ylab = "Peak Flow (cfs)",
     xaxt = "n", xlab = "Time (years)",
     ylim = c(0, 3500),
     xlim = c(xmin, xmax),
     pch = 21, bg = "red",
     main ="Annual Peak Flows"
)  
par(cex = 0.65)
axis(1, at = ytick, labels = as.character(xtlab))


bwh<- read.csv('~/Desktop/WRWC/data/BW_Hailey_historic.csv')
bwh$Date<- as.POSIXct.Date(as.Date(bwh$Date, format = ("%m/%d/%Y")))
plot(bwh$Date, bwh$Q, type='l')

floods<-high.spells(bwh, quant = 0.9, threshold = 2000,
            ind.days = 5, duration = TRUE, volume = TRUE, plot = TRUE, ignore.zeros = FALSE,
            ctf.threshold = 0.1, ann.stats = TRUE, ann.stats.only = FALSE, inter.flood = FALSE,
            hydro.year=FALSE)


# ------------------------------------------------------------------------------
# Auto Download USGS Gages
# ------------------------------------------------------------------------------

bwb = 13139510  #  Bullion Bridge, Big Wood at Hailey
bws = 13140800  #  Stanton Crossing, Big Wood
bwr = 13142500  #  Big Wood below Magic near Richfield
mr  = 13142000  #  Magic Reservoir storage in acre - feet, impt for carry-over
bwbr = 13140335 # Big Wood at S Broadford Bridge Nr Bellevue, data only goes back to 2017
bwk = 13135500  #  Big Wood nr Ketchum, goes to 2011
usgs_sites = c(bwb, bws, bwk) #  put all sites in one vector

pCode = "00060" # USGS code for streamflow
sCode = "00054" # USGS code for reservoir storage (acre-feet)

# Dataframe with information about sites and period of record, uv = instantaneous value
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') 
site_info$abv <- c("bwb", "bws", "bwk")
site_info <- site_info %>% dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, alt_va, huc_cd, begin_date, end_date, count_nu, abv)

# Dowload data from all sites into one dataframe
streamflow_data <- readNWISdv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = min(site_info$begin_date), endDate = max(site_info$end_date)) %>% renameNWISColumns() %>% data.frame

#Re-format dates and pull out month /day/ water year
streamflow_data$Date <- as.Date(streamflow_data$Date, format = "%Y-%m-%d")
streamflow_data$mo <- month(streamflow_data$Date)
streamflow_data$wy <- as.numeric(as.character(water_year(streamflow_data$Date, origin='usgs')))
streamflow_data$day <- day(streamflow_data$Date)
# Cleanup Streamflow data frame and join relevant site information
streamflow_data <- streamflow_data %>% dplyr::select(-agency_cd) %>% inner_join(site_info, by ="site_no") 
