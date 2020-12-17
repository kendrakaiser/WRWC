## packages.R
# All packages needed to run WRWC scripts

# load packages

library(MASS)
library(plotrix)
library(mvtnorm)
library(tidyverse)

library(dataRetrieval)
library(tidyverse)
library(snotelr)
library(XML)
library(httr)
library(dplyr)
library(devtools)
library(plyr)
library(readr)
library(lubridate)
library(lfstat)

devtools::install_github(repo = "rhlee12/RNRCS", subdir = "/RNRCS/", force =TRUE)
library(RNRCS)

