# ----------------------------------------------------------------------------- #
# Predictive Streamflow Model for the Wood River Water Collaborative
# Kendra Kaiser
# July 14, 2020
# ----------------------------------------------------------------------------- # 

# load packages ----

# import data ----
cd = '~/Desktop/Data/WRWC'
streamflow = read.csv(file.path(cd,'streamflow_data.csv'))
snotel = read.csv(file.path(cd,'snotel_data.csv'))
agrimet = read.csv(file.path(cd,'agri_met.csv'))

# Analyze and Predict temperature trend ----
# Mixed-effects regression of temp. vs. year that predicts temperature for the upcoming year. 
# Starts in water year XXXX for common period of record.
