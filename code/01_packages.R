## packages.R
# All packages needed to run WRWC scripts
# Once you have installed the packages locally, you can comment out the install packages lines with a #

 #install.packages(c('MASS', 'plotrix', 'mvtnorm', 'tidyverse', 'dataRetrieval', 
 #'snotelr', 'XML', 'httr', 'plyr', 'dplyr', 'devtools', 'readr', 'lubridate', 
 #'lfstat', 'gridExtra', 'varhandle', 'viridis', 'ggplot2', 'nlme', 'tidyr', 
 #'knitr', 'fasstr', 'stringr','kableExtra', 'leaps', 'rlist', 'caret', 'erer', 
 #'ggcorrplot', 'scales'))

remotes::install_github("USGS-R/smwrBase") 
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
library(smwrBase)
library(ggnewscale) # plot two scatter plots with different scales on the same plot
