# Download Clip and Process SNODAS data
# March 09, 2023
# Sam Carlson, Kendra Kaiser

# load libaries
library(curl)
library(terra)

##SNODAS, Version 1: 

#data_dir= 

# Function to Download Daily SNODAS data using FTP

getSnodasFile=function(dataDate=as.Date("2021-04-20"),
                       workingDirectory=getwd(),
                       baseURL="ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02158/masked/"){
  
  workingDirectory = paste0(workingDirectory, "/tmp")
  
  dir.create(workingDirectory)
  
  #compile file name
  addURL=paste0(format.Date(dataDate,"%Y/%m_%b"),
                "/SNODAS_",
                format.Date(dataDate,"%Y%m%d"),
                ".tar")
  
  fileName= paste0(workingDirectory, "/", format.Date(dataDate,"%Y%m%d"), ".tar")
  
  # combine file name to directory and download to working directory
  curl_download(paste0(baseURL,addURL), fileName)
  
  #create subfolder for extracted files
  dir.create(paste0(workingDirectory, "/ext"))
  untar(fileName, exdir=paste0(workingDirectory, "/ext"))
  
  gzFiles=list.files(paste0(workingDirectory, "/ext"))
  
  #come back to this
  sweFile=gzFiles[grepl(pattern=".*34tS.*\\.dat.*", gzFiles)]
  sweFiles=gzFiles[grepl(pattern=".*34tS.*", gzFiles)]
}

dat<- getSnodasFile()


# save processed data 

# dump datadir