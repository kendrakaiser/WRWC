# ----------------------------------------------------------------------------- #
# Predictive Temperature Model for the Wood River Water Collaborative
# Kendra Kaiser
# July 14, 2020
# Creates Mixed-effects rRegression model to predict upcoming year temperature
# ----------------------------------------------------------------------------- # 

# load packages ----

# import data ----
cd = '~/Desktop/Data/WRWC'
snotel = read.csv(file.path(cd,'snotel_data.csv'))
agrimet = read.csv(file.path(cd,'agri_met.csv'))

# Analyze and Predict temperature trend ----
# Starts in water year XXXX for common period of record.
