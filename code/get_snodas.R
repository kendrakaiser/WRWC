# download and process SNODAS data
# March 16th 2023
# Kendra Kaiser

#devtools::install_github('marinosr/SNODASR')
source('~/github/SNODASR-main/R/download.SNODAS.R')
source('~/github/SNODASR-main/R/extract.SNODAS.subset.R')
wy_start<- as.Date(paste0(pred.yr-1, "-10-01"))
date_range<- seq(wy_start, end_date, by = "day")
# as written this will get all data for the full year, 
# it would be more ideal to get the data since the last run date and append the new data
sdas<- download.SNODAS(dates = date_range)

donde=matrix(c(-116, -113, 43, 44), nrow=2, byrow = TRUE)
sdas<- extract.SNODAS.subset(dates = date_range, values_wanted=c('SWE', 'Depth', 'Runoff', 'T_Mean'), extent=donde)
