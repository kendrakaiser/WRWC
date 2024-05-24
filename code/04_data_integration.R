# Data Integration
# merge data from data scraping and temp models
# Kendra Kaiser
# 09-15-2023

# Import & Compile Data -------------------------------------------------------# 
# Streamflow, Current SWE, historic Temperature Data

# input data name 'alldat_mo.csv'
if (month(end_date) == 2){
  swe_q  <<- alldat_feb
} else if (month(end_date) == 3){
  swe_q  <<- alldat_mar
} else if (month(end_date) == 4){
  swe_q  <<- alldat_april
}

swe_q[swe_q == 0] <- 0.01 # change zeros to a value so lm works 

# combine discharge & SWE with temp data
var = swe_q %>% merge(all.temp.dat, by ="wateryear", all=TRUE)

