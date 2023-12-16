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
library(MASS)
library(plotrix)
library(mvtnorm) #multivariate distributions
library(tidyverse)
library(tidyr)
library(dataRetrieval) #USGS data
library(snotelr) #NRCS data
library(XML)
library(httr)
library(plyr)
library(dplyr)
library(devtools)
library(readr)
library(lubridate) #date transformation
#library(lfstat) #water year
library(gridExtra)
#library(RNRCS)
library(varhandle)
library(viridis) # color scheme
library(ggplot2) # plotting
library(nlme) 
library(knitr)
library(fasstr)
library(stringr)
library(leaps) #regsubsets
library(rlist) #list.save
library(caret) #loocv
library(erer) #write.list to csv
library(ggcorrplot) # colored correlation plot, not in use
library(scales) # wrap text of figure labels
#library(smwrBase)
library(ggnewscale) # plot two scatter plots with different scales on the same plot
library(RPostgres) #connect to db

library(sf)
library(curl)
library(terra)
library(R.utils)
library(raster)







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