## packages.R
# All packages needed to run WRWC scripts
# Once you have installed the packages locally, you can comment out the install packages lines with a #

# install.packages(c('MASS', 'plotrix', 'mvtnorm', 'tidyverse', 'dataRetrieval', 
# 'snotelr', 'XML', 'httr', 'plyr', 'dplyr', 'devtools', 'readr', 'lubridate', 
# 'lfstat', 'gridExtra', 'varhandle', 'viridis', 'ggplot2', 'nlme', 'tidyr', 
# 'knitr', 'fasstr', 'stringr','kableExtra'))

# load packages
library(MASS)
library(plotrix)
library(mvtnorm)
library(tidyverse)
library(dataRetrieval) #USGS data
library(snotelr) #NRCS data
library(XML)
library(httr)
library(plyr)
library(dplyr)
library(devtools)
library(readr)
library(lubridate) #date transformation
library(lfstat) #water year
library(gridExtra)
#library(RNRCS)
library(varhandle)
library(viridis)
library(ggplot2)
library(nlme) 
library(tidyr)
library(knitr)
library(fasstr)
library(stringr)
library(rlist) #list.save
library(caret) #loocv
