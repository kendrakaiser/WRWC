## packages.R
# All packages needed to run WRWC scripts
# Once you have installed the packages locally, you can comment out the install packages lines with a #

#install.packages(c('MASS', 'plotrix', 'mvtnorm', 'tidyverse', 'dataRetrieval', 
#'snotelr', 'XML', 'httr', 'plyr', 'dplyr', 'devtools', 'readr', 'lubridate', 
#'lfstat', 'gridExtra', 'varhandle', 'viridis', 'ggplot2', 'nlme', 'tidyr', 
#'knitr', 'fasstr', 'stringr','kableExtra', 'leaps', 'rlist', 'caret', 'erer', 
#'ggcorrplot', 'scales', 'terra', 'sf', 'RPostgres', 'R.utils', 'raster', 'ggnewscale'))

#remotes::install_github("USGS-R/smwrBase") 
# load packages

library(tidyverse)

#"dplyr","plyr","devtools","knitr","httr"
if (!require("pacman")) install.packages("pacman")
pacman::p_load("MASS","mvtnorm","tidyverse","tidyr","dataRetrieval", "dplyr","snotelr","lubridate","gridExtra","ggplot2","nlme",
               "fasstr","stringr","leaps","caret","RPostgres","raster", "terra", "sf", "MuMIn", "R.utils","selectr","jsonlite",
               "ggnewscale","viridis")

#'Water Year
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#'
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the water year.
#' @note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#' @seealso 
#Flip for production/manual
#'\code{\link[lubridate]{year}}
#\code{year} (in lubridate package)
#' @export
#' @keywords manip
#' @examples
#'
#'library(smwrData)
#'data(QW05078470)
#'## Return an ordered factor
#'waterYear(QW05078470$DATES)
waterYear <- function(x, numeric=FALSE) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated verion
  ##    2010Feb17 DLLorenz Added option to return numerics
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}



fillNullWithLastGoodValue=function(df,dataDate=end_date,maxDayDiff=7){
  
  for(r in 1:nrow(df)){
    if(any(is.na(df[r,]))){
      for(naName in names(df)[is.na(df[r,])]){
        site=strsplit(naName,".",fixed=T)[[1]][1]
        metric=strsplit(naName,".",fixed=T)[[1]][2]
        prevValue=dbGetQuery(conn,paste0("SELECT datetime, wateryear(datetime) AS wateryear, metric, value, data.locationid, name, sitenote
             FROM data LEFT JOIN locations ON data.locationid = locations.locationid
             WHERE datetime <= '",dataDate,"' AND metric = '",metric,"' AND qcstatus = 'true' AND sitenote = '",site,"'
                        ORDER BY datetime desc LIMIT 1;"))
        
        if(length(prevValue$datetime)>0){
          if((dataDate - as.Date(prevValue$datetime))<7){
            df[r,names(df)==naName] = prevValue$value
          }
        } 
      }
    }
  }
  
  return(df)
  
  
}
